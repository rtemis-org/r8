//! Rust-native values: the values of a type whose codec (`R8TypeVTable`) a
//! package registered, held as an `ALTLIST` over the codec's state
//! (`src/native.c`), and the value functions of `R8Api` (`src/view.c`).

use std::ffi::{CStr, c_char, c_void};
use std::panic::{AssertUnwindSafe, catch_unwind};
use std::sync::atomic::{AtomicPtr, Ordering};

use extendr_api::SEXP;
use extendr_api::prelude::*;
use extendr_ffi::{
    R_xlen_t, Rf_allocVector, Rf_protect, Rf_unprotect, SET_STRING_ELT, SEXPTYPE, VECTOR_ELT,
};
use rtemis_r8::abi::{ERROR, Error, OK, Sexp, Status, TypeVTable};
use rtemis_r8::ir::TypeDef;
use rtemis_r8::name::QualifiedName;
use rtemis_r8::rules::TypeLookup;

use crate::construct::mkchar;
use crate::declare::registry;
use crate::rview::{sym_names, xlen};

static WRAP: AtomicPtr<()> = AtomicPtr::new(std::ptr::null_mut());
static STATE: AtomicPtr<()> = AtomicPtr::new(std::ptr::null_mut());
static PARTS: AtomicPtr<()> = AtomicPtr::new(std::ptr::null_mut());
static MEMORY: AtomicPtr<()> = AtomicPtr::new(std::ptr::null_mut());
static PROTECT: AtomicPtr<()> = AtomicPtr::new(std::ptr::null_mut());
static SYMBOLS: AtomicPtr<()> = AtomicPtr::new(std::ptr::null_mut());

/// Receives the functions of `native.c` from `R_init_R8()`.
///
/// # Safety
///
/// `wrap`, `state`, `parts`, `memory`, `protect`, and `symbols` are
/// `r8_native_wrap()`, `r8_native_state()`, `r8_native_parts()`,
/// `r8_native_memory()`, `r8_protect()`, and `r8_native_symbols()`.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn r8_set_native_functions(
    wrap: *mut (),
    state: *mut (),
    parts: *mut (),
    memory: *mut (),
    protect: *mut (),
    symbols: *mut (),
) {
    WRAP.store(wrap, Ordering::Release);
    STATE.store(state, Ordering::Release);
    PARTS.store(parts, Ordering::Release);
    MEMORY.store(memory, Ordering::Release);
    PROTECT.store(protect, Ordering::Release);
    SYMBOLS.store(symbols, Ordering::Release);
}

/// The parts of a Rust-native value that `r8_size()` adds up: the bytes of
/// its state and the cached fields that are not views of the state.
///
/// @param x An R8 value.
/// @return List: `state` and `fields`, or `NULL` for an R-native value.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_native_parts(x: Robj) -> Robj {
    let f = PARTS.load(Ordering::Acquire);
    assert!(!f.is_null(), "R8 is loaded through R_init_R8()");
    let f: unsafe extern "C" fn(SEXP) -> SEXP = unsafe { std::mem::transmute(f) };
    unsafe { Robj::from_sexp(f(x.get())) }
}

/// The bytes of the states of Rust-native values reachable from R, and the
/// level at which R8 requests a collection.
///
/// @return Double: `c(live, threshold)`.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_native_memory() -> Robj {
    let f = MEMORY.load(Ordering::Acquire);
    assert!(!f.is_null(), "R8 is loaded through R_init_R8()");
    let f: unsafe extern "C" fn() -> SEXP = unsafe { std::mem::transmute(f) };
    unsafe { Robj::from_sexp(f()) }
}

/// The native symbols of R8's C routines that R calls through `.Call()`:
/// `invoke`, which calls an impl function, and `exports`, which fetches a
/// package's exports table (`src/view.c`).
///
/// @return List: `invoke` and `exports`.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_native_symbols() -> Robj {
    let f = SYMBOLS.load(Ordering::Acquire);
    assert!(!f.is_null(), "R8 is loaded through R_init_R8()");
    let f: unsafe extern "C" fn() -> SEXP = unsafe { std::mem::transmute(f) };
    unsafe { Robj::from_sexp(f()) }
}

extendr_module! {
    mod native;
    fn r8_native_parts;
    fn r8_native_memory;
    fn r8_native_symbols;
}

/// The value over `state` with the attributes of `like`, the assembled
/// list of the value's fields; the value owns the state.
pub(crate) fn wrap(vt: *const TypeVTable, state: *mut c_void, like: SEXP) -> SEXP {
    wrap_from(vt, state, like, unsafe { extendr_ffi::R_NilValue }, -1)
}

/// `wrap()` for a value derived from the Rust-native value `from`, whose
/// read fields, except field `skip`, the new value keeps.
pub(crate) fn wrap_from(
    vt: *const TypeVTable,
    state: *mut c_void,
    like: SEXP,
    from: SEXP,
    skip: R_xlen_t,
) -> SEXP {
    let f = WRAP.load(Ordering::Acquire);
    assert!(!f.is_null(), "R8 is loaded through R_init_R8()");
    let f: unsafe extern "C" fn(*const TypeVTable, *mut c_void, SEXP, SEXP, R_xlen_t) -> SEXP =
        unsafe { std::mem::transmute(f) };
    unsafe { f(vt, state, like, from, skip) }
}

/// A state built by the codec from `state` with the field `name` replaced
/// by `value`, validated and in its stored form.
pub(crate) fn with_field(
    vt: *const TypeVTable,
    state: *const c_void,
    name: &str,
    value: SEXP,
) -> Result<*mut c_void, String> {
    let codec = unsafe { &*vt };
    let f = codec
        .with_field
        .ok_or_else(|| "the codec has no `with_field`".to_string())?;
    let field = std::ffi::CString::new(name).map_err(|e| e.to_string())?;
    let mut new_state: *mut c_void = std::ptr::null_mut();
    let mut err = Error {
        code: [0; 64],
        message: [0; 1024],
    };
    let status = unsafe {
        f(
            state,
            field.as_ptr(),
            value.cast(),
            &mut new_state,
            &mut err,
        )
    };
    if status != OK || new_state.is_null() {
        return Err(error_text(&err));
    }
    Ok(new_state)
}

/// The state and codec of a Rust-native value; `None` for any other
/// object.
pub(crate) fn state_of(x: SEXP) -> Option<(*const c_void, *const TypeVTable)> {
    let f = STATE.load(Ordering::Acquire);
    assert!(!f.is_null(), "R8 is loaded through R_init_R8()");
    let f: unsafe extern "C" fn(SEXP, *mut *const TypeVTable) -> *const c_void =
        unsafe { std::mem::transmute(f) };
    let mut vt: *const TypeVTable = std::ptr::null();
    let state = unsafe { f(x, &mut vt) };
    (!state.is_null()).then_some((state, vt))
}

/// The codec's error as text: its message followed by its code in brackets.
fn error_text(err: &Error) -> String {
    let text = |b: &[c_char]| {
        let bytes: Vec<u8> = b
            .iter()
            .take_while(|&&c| c != 0)
            .map(|&c| c as u8)
            .collect();
        String::from_utf8_lossy(&bytes).into_owned()
    };
    format!("{} [{}]", text(&err.message), text(&err.code))
}

/// A state built by the codec from the fields of `fields`, a list in
/// declaration order whose fields are validated and in their stored forms.
pub(crate) fn from_fields(vt: *const TypeVTable, fields: SEXP) -> Result<*mut c_void, String> {
    let codec = unsafe { &*vt };
    let f = codec
        .from_fields
        .ok_or_else(|| "the codec has no `from_fields`".to_string())?;
    let items: Vec<Sexp> = (0..xlen(fields))
        .map(|i| unsafe { VECTOR_ELT(fields, i as R_xlen_t) }.cast())
        .collect();
    let mut state: *mut c_void = std::ptr::null_mut();
    let mut err = Error {
        code: [0; 64],
        message: [0; 1024],
    };
    let status = unsafe { f(items.as_ptr(), items.len(), &mut state, &mut err) };
    if status != OK || state.is_null() {
        return Err(error_text(&err));
    }
    Ok(state)
}

/// Fills `err` with `code` and `message`, truncated to fit.
fn set_error(err: *mut Error, code: &str, message: &str) {
    if err.is_null() {
        return;
    }
    let fill = |to: &mut [c_char], s: &str| {
        let n = s.len().min(to.len() - 1);
        for (d, b) in to.iter_mut().zip(&s.as_bytes()[..n]) {
            *d = *b as c_char;
        }
        to[n] = 0;
    };
    let e = unsafe { &mut *err };
    fill(&mut e.code, code);
    fill(&mut e.message, message);
}

/// The text of a NUL-terminated string from C.
fn c_text(p: *const c_char) -> Option<String> {
    (!p.is_null()).then(|| unsafe { CStr::from_ptr(p) }.to_string_lossy().into_owned())
}

/// `R8Api.new_value`: wraps a state of a registered Rust-native struct as
/// an R8 value, taking ownership of the state, also on error.
///
/// # Safety
///
/// `type_name` is NUL-terminated or NULL; `state` was built by the type's
/// codec; `value` and `err` are writable or NULL.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn r8_api_new_value(
    type_name: *const c_char,
    state: *mut c_void,
    value: *mut Sexp,
    err: *mut Error,
) -> Status {
    let result = catch_unwind(AssertUnwindSafe(|| new_value(type_name, state, value)));
    match result {
        Ok(Ok(())) => OK,
        Ok(Err((code, message))) => {
            set_error(err, code, &message);
            ERROR
        }
        Err(_) => {
            set_error(err, "panic", "R8 panicked while wrapping the state");
            ERROR
        }
    }
}

fn new_value(
    type_name: *const c_char,
    state: *mut c_void,
    value: *mut Sexp,
) -> Result<(), (&'static str, String)> {
    let name = c_text(type_name);
    let vt = name.as_deref().and_then(crate::exports::codec);
    let Some(vt) = vt else {
        // The state is unowned without a codec; nothing can drop it.
        return Err((
            "unknown_type",
            format!(
                "`{}` is not a registered Rust-native type",
                name.unwrap_or_default()
            ),
        ));
    };
    let drop_state = || {
        if let Some(d) = unsafe { (*vt).drop } {
            unsafe { d(state) };
        }
    };
    if value.is_null() || state.is_null() {
        drop_state();
        return Err(("invalid_argument", "`state` or `value` is NULL".to_string()));
    }
    let q = match QualifiedName::parse(name.as_deref().unwrap_or_default()) {
        Ok(q) => q,
        Err(e) => {
            drop_state();
            return Err(("unknown_type", e.to_string()));
        }
    };
    let docs = match registry().lock() {
        Ok(d) => d,
        Err(_) => {
            drop_state();
            return Err(("registry", "the registry is unavailable".to_string()));
        }
    };
    let Some(TypeDef::Struct(def)) = (&*docs as &dyn TypeLookup).find(&q) else {
        drop_state();
        return Err(("unknown_type", format!("`{q}` is not a registered struct")));
    };
    let names: Vec<String> = def.fields.iter().map(|f| f.name.to_string()).collect();
    let version = def.version;
    // The registry is released before R allocates: a jump out of an
    // allocation skips no Rust frame that holds the lock.
    drop(docs);
    let qname = q.to_string();
    let args = BuildArgs {
        vt,
        state,
        qname: &qname,
        names: &names,
        version,
    };
    let mut out: SEXP = unsafe { extendr_ffi::R_NilValue };
    let mut err = Error {
        code: [0; 64],
        message: [0; 1024],
    };
    let status = protect(
        build_value,
        std::ptr::from_ref(&args).cast_mut().cast(),
        &mut out,
        &mut err,
    );
    if status != OK {
        // The value takes the state only when the build returns.
        drop_state();
        return Err(("r_error", "R could not allocate the value".to_string()));
    }
    unsafe { *value = out.cast() };
    Ok(())
}

/// The arguments of `build_value()`, borrowed from `new_value()`.
struct BuildArgs<'a> {
    vt: *const TypeVTable,
    state: *mut c_void,
    qname: &'a str,
    names: &'a [String],
    version: u32,
}

/// Builds a Rust-native value under `r8_protect()`; it holds only borrows,
/// so a jump out of it skips no destructor.
extern "C" fn build_value(p: *mut c_void) -> SEXP {
    let a = unsafe { &*p.cast::<BuildArgs>() };
    unsafe {
        let like = template(a.qname, a.names, a.version);
        let v = wrap(a.vt, a.state, like);
        Rf_unprotect(1);
        v
    }
}

/// `R8Api.protect` (`r8_protect()` in `view.c`).
fn protect(
    f: extern "C" fn(*mut c_void) -> SEXP,
    data: *mut c_void,
    value: &mut SEXP,
    err: &mut Error,
) -> Status {
    let p = PROTECT.load(Ordering::Acquire);
    assert!(!p.is_null(), "R8 is loaded through R_init_R8()");
    let p: unsafe extern "C" fn(
        extern "C" fn(*mut c_void) -> SEXP,
        *mut c_void,
        *mut SEXP,
        *mut Error,
    ) -> Status = unsafe { std::mem::transmute(p) };
    unsafe { p(f, data, value, err) }
}

/// A list of the field names `names` with the attributes of a value of the
/// struct `qname`, the template of a Rust-native value; returned protected.
/// It allocates no Rust memory, so it runs inside `r8_protect()`.
///
/// # Safety
///
/// The caller unprotects the result.
pub(crate) unsafe fn template(qname: &str, names: &[String], version: u32) -> SEXP {
    unsafe {
        let like = Rf_protect(Rf_allocVector(SEXPTYPE::VECSXP, names.len() as R_xlen_t));
        let n = Rf_protect(Rf_allocVector(SEXPTYPE::STRSXP, names.len() as R_xlen_t));
        for (k, f) in names.iter().enumerate() {
            SET_STRING_ELT(n, k as R_xlen_t, mkchar(f));
        }
        extendr_ffi::Rf_setAttrib(like, sym_names(), n);
        crate::construct::set_value_attributes(like, qname, version, None);
        Rf_unprotect(1);
        like
    }
}

/// A state built by the codec's `decode` from a document of the type;
/// `None` when the codec has no `decode`.
pub(crate) fn decode(vt: *const TypeVTable, document: &str) -> Option<Result<*mut c_void, String>> {
    let f = unsafe { &*vt }.decode?;
    let mut state: *mut c_void = std::ptr::null_mut();
    let mut err = Error {
        code: [0; 64],
        message: [0; 1024],
    };
    let status = unsafe { f(document.as_ptr(), document.len(), &mut state, &mut err) };
    Some(if status != OK || state.is_null() {
        Err(error_text(&err))
    } else {
        Ok(state)
    })
}

/// `R8Api.value_state`: borrows the state of a value of the type
/// `type_name`.
///
/// # Safety
///
/// `type_name` is NUL-terminated or NULL; `state` and `err` are writable or
/// NULL.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn r8_api_value_state(
    value: Sexp,
    type_name: *const c_char,
    state: *mut *const c_void,
    err: *mut Error,
) -> Status {
    let result = catch_unwind(AssertUnwindSafe(|| {
        let name = c_text(type_name).unwrap_or_default();
        let Some((s, vt)) = state_of(value.cast()) else {
            return Err((
                "not_native",
                format!("the value is not a Rust-native `{name}`"),
            ));
        };
        let held = c_text(unsafe { (*vt).type_name }).unwrap_or_default();
        if held != name {
            return Err((
                "type_mismatch",
                format!("the value is a `{held}`, not a `{name}`"),
            ));
        }
        if state.is_null() {
            return Err(("invalid_argument", "`state` is NULL".to_string()));
        }
        unsafe { *state = s };
        Ok(())
    }));
    match result {
        Ok(Ok(())) => OK,
        Ok(Err((code, message))) => {
            set_error(err, code, &message);
            ERROR
        }
        Err(_) => {
            set_error(err, "panic", "R8 panicked while reading the state");
            ERROR
        }
    }
}

/// The version, declaration hash, and registering package of the
/// Rust-native type `name`, which `Serialized_state` records with the
/// state's bytes: `list(version, hash, package)`, or `NULL` when the type
/// is not registered.
///
/// # Safety
///
/// `type_name` is NUL-terminated; called on R's main thread.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn r8_native_meta(type_name: *const c_char) -> Sexp {
    let nil = unsafe { extendr_ffi::R_NilValue }.cast();
    let result = catch_unwind(AssertUnwindSafe(|| -> Option<Sexp> {
        let name = c_text(type_name)?;
        let q = QualifiedName::parse(&name).ok()?;
        let docs = registry().lock().ok()?;
        let lookup: &dyn TypeLookup = &*docs;
        let Some(TypeDef::Struct(def)) = lookup.find(&q) else {
            return None;
        };
        let hash = rtemis_r8::codec::declaration_hash(&q, lookup);
        let package = crate::exports::codec_package(&name).unwrap_or_default();
        unsafe {
            let out = Rf_protect(Rf_allocVector(SEXPTYPE::VECSXP, 3));
            extendr_ffi::SET_VECTOR_ELT(out, 0, extendr_ffi::Rf_ScalarInteger(def.version as i32));
            extendr_ffi::SET_VECTOR_ELT(out, 1, scalar_string(&hash));
            extendr_ffi::SET_VECTOR_ELT(out, 2, scalar_string(&package));
            Rf_unprotect(1);
            Some(out.cast())
        }
    }));
    result.ok().flatten().unwrap_or(nil)
}

/// A character vector of the one string `s`.
///
/// # Safety
///
/// Called on R's main thread; the result is unprotected.
unsafe fn scalar_string(s: &str) -> SEXP {
    unsafe {
        let x = Rf_protect(Rf_allocVector(SEXPTYPE::STRSXP, 1));
        SET_STRING_ELT(x, 0, mkchar(s));
        Rf_unprotect(1);
        x
    }
}

/// The codec that reads a state recorded as a value of `type_name` at
/// `version` with the declaration hash `hash`; NULL, with `err` filled,
/// when no package has registered it or its declaration differs.
///
/// # Safety
///
/// `type_name` and `hash` are NUL-terminated; `err` is writable.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn r8_native_resolve(
    type_name: *const c_char,
    version: i32,
    hash: *const c_char,
    err: *mut Error,
) -> *const TypeVTable {
    let result = catch_unwind(AssertUnwindSafe(
        || -> Result<*const TypeVTable, (&'static str, String)> {
            let name = c_text(type_name).unwrap_or_default();
            let Some(vt) = crate::exports::codec(&name) else {
                return Err((
                    "codec_missing",
                    format!("no loaded package registers the codec of `{name}`"),
                ));
            };
            let q = QualifiedName::parse(&name).map_err(|e| ("unknown_type", e.to_string()))?;
            let docs = registry()
                .lock()
                .map_err(|_| ("registry", "the registry is unavailable".to_string()))?;
            let lookup: &dyn TypeLookup = &*docs;
            let Some(TypeDef::Struct(def)) = lookup.find(&q) else {
                return Err((
                    "unknown_type",
                    format!("`{name}` is not a registered struct"),
                ));
            };
            if def.version as i32 != version {
                return Err((
                    "version_mismatch",
                    format!(
                        "the value was saved at version {version} of `{name}`, and version {} is registered",
                        def.version
                    ),
                ));
            }
            let recorded = c_text(hash).unwrap_or_default();
            if rtemis_r8::codec::declaration_hash(&q, lookup) != recorded {
                return Err((
                    "declaration_changed",
                    format!(
                        "the declaration of `{name}` differs from the one the value was saved with, at the same version"
                    ),
                ));
            }
            Ok(vt)
        },
    ));
    match result {
        Ok(Ok(vt)) => vt,
        Ok(Err((code, message))) => {
            set_error(err, code, &message);
            std::ptr::null()
        }
        Err(_) => {
            set_error(err, "panic", "R8 panicked while resolving a codec");
            std::ptr::null()
        }
    }
}
