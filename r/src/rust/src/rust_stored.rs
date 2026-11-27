//! Rust-stored values: the values of a type whose codec (`R8Codec`) a
//! package registered, held as an `ALTLIST` over the codec's state
//! (`src/rust_stored.c`), and the value functions of `R8Api` (`src/view.c`).

use std::ffi::{CStr, c_char, c_void};
use std::panic::{AssertUnwindSafe, catch_unwind};
use std::sync::atomic::{AtomicPtr, Ordering};

use extendr_api::SEXP;
use extendr_api::prelude::*;
use extendr_ffi::{R_xlen_t, SEXPTYPE, VECTOR_ELT};
use rtemis_r8::abi::{Codec, ERROR, Error, OK, Sexp, Status};
use rtemis_r8::name::QualifiedName;
use rtemis_r8::rules::TypeLookup;

use crate::declare::registry;
use crate::rview::xlen;

static WRAP: AtomicPtr<()> = AtomicPtr::new(std::ptr::null_mut());
static FIELDS_READ: AtomicPtr<()> = AtomicPtr::new(std::ptr::null_mut());
static GIVE: AtomicPtr<()> = AtomicPtr::new(std::ptr::null_mut());
static STATE: AtomicPtr<()> = AtomicPtr::new(std::ptr::null_mut());
static PARTS: AtomicPtr<()> = AtomicPtr::new(std::ptr::null_mut());
static MEMORY: AtomicPtr<()> = AtomicPtr::new(std::ptr::null_mut());
static PROTECT: AtomicPtr<()> = AtomicPtr::new(std::ptr::null_mut());
static SYMBOLS: AtomicPtr<()> = AtomicPtr::new(std::ptr::null_mut());
static TAKE: AtomicPtr<()> = AtomicPtr::new(std::ptr::null_mut());

/// Receives the functions of `rust_stored.c` from `R_init_R8()`.
///
/// # Safety
///
/// `wrap`, `fields_read`, `give`, `state`, `parts`, `memory`, `protect`,
/// `symbols`, and `take` are `r8_rust_stored_wrap()`,
/// `r8_rust_stored_fields_read()`, `r8_rust_stored_give()`,
/// `r8_rust_stored_state()`, `r8_rust_stored_parts()`,
/// `r8_rust_stored_memory()`, `r8_protect()`, `r8_native_symbols()`, and
/// `r8_rust_stored_take()`.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn r8_set_rust_stored_functions(
    wrap: *mut (),
    fields_read: *mut (),
    give: *mut (),
    state: *mut (),
    parts: *mut (),
    memory: *mut (),
    protect: *mut (),
    symbols: *mut (),
    take: *mut (),
) {
    WRAP.store(wrap, Ordering::Release);
    FIELDS_READ.store(fields_read, Ordering::Release);
    GIVE.store(give, Ordering::Release);
    STATE.store(state, Ordering::Release);
    PARTS.store(parts, Ordering::Release);
    MEMORY.store(memory, Ordering::Release);
    PROTECT.store(protect, Ordering::Release);
    SYMBOLS.store(symbols, Ordering::Release);
    TAKE.store(take, Ordering::Release);
}

/// Takes the state back from `x`, a value that [`wrap()`] made
/// (`r8_rust_stored_take()`).
fn take(x: SEXP) {
    let f = TAKE.load(Ordering::Acquire);
    assert!(!f.is_null(), "R8 is loaded through R_init_R8()");
    let f: unsafe extern "C" fn(SEXP) = unsafe { std::mem::transmute(f) };
    unsafe { f(x) }
}

/// The parts of a Rust-stored value that `r8size()` adds up: the bytes of
/// its state and the cached fields that are not views of the state.
///
/// @param x An R8 value.
/// @return List: `state` and `fields`, or `NULL` for an R-stored value.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_rust_stored_parts(x: Robj) -> Robj {
    let f = PARTS.load(Ordering::Acquire);
    assert!(!f.is_null(), "R8 is loaded through R_init_R8()");
    let f: unsafe extern "C" fn(SEXP) -> SEXP = unsafe { std::mem::transmute(f) };
    unsafe { Robj::from_sexp(f(x.get())) }
}

/// The bytes of the states of Rust-stored values reachable from R, and the
/// level at which R8 requests a collection.
///
/// @return Double: `c(live, threshold)`.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_rust_stored_memory() -> Robj {
    let f = MEMORY.load(Ordering::Acquire);
    assert!(!f.is_null(), "R8 is loaded through R_init_R8()");
    let f: unsafe extern "C" fn() -> SEXP = unsafe { std::mem::transmute(f) };
    unsafe { Robj::from_sexp(f()) }
}

/// The native symbols of R8's C routines that R calls through `.Call()`
/// (`r8_native_symbols()` in `src/view.c`).
///
/// @return List: The symbols by name, such as `invoke`, which calls an impl
///   function, and `exports`, which fetches a package's exports table.
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
    mod rust_stored;
    fn r8_rust_stored_parts;
    fn r8_rust_stored_memory;
    fn r8_format_size;
    fn r8_native_symbols;
}

/// A size in bytes in IEC binary units to three significant digits, as the
/// Rust core formats it (`rtemis_r8::text::format_size()`).
///
/// @param bytes Numeric: A size in bytes.
/// @return Character: The size, such as `"7.63 MiB"`.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_format_size(bytes: f64) -> String {
    rtemis_r8::text::format_size(bytes.max(0.0) as u64)
}

/// The value of the identity `id` over `state`; the value owns the state.
pub(crate) fn wrap(vt: *const Codec, state: *mut c_void, id: SEXP) -> SEXP {
    wrap_from(vt, state, id, unsafe { extendr_ffi::R_NilValue }, -1)
}

/// `wrap()` for a value derived from the Rust-stored value `from`, whose
/// read fields, except field `skip`, the new value keeps.
pub(crate) fn wrap_from(
    vt: *const Codec,
    state: *mut c_void,
    id: SEXP,
    from: SEXP,
    skip: R_xlen_t,
) -> SEXP {
    let f = WRAP.load(Ordering::Acquire);
    assert!(!f.is_null(), "R8 is loaded through R_init_R8()");
    let f: unsafe extern "C" fn(*const Codec, *mut c_void, SEXP, SEXP, R_xlen_t) -> SEXP =
        unsafe { std::mem::transmute(f) };
    unsafe { f(vt, state, id, from, skip) }
}

/// A state built by the codec from `state` with the field `name` replaced
/// by `value`, validated and in its stored form.
pub(crate) fn with_field(
    vt: *const Codec,
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
    let mut err = blank();
    let status = unsafe {
        f(
            state,
            field.as_ptr(),
            value.cast(),
            &mut new_state,
            &mut err,
        )
    };
    codec_returned();
    if status != OK || new_state.is_null() {
        return Err(error_text(&err));
    }
    // A replacement keeps the variant of an enum's state.
    if let Some(v) = state_variant(vt, state) {
        check_variant(vt, new_state, &v)?;
    }
    Ok(new_state)
}

/// The state and codec of a Rust-stored value; `None` for any other
/// object.
pub(crate) fn state_of(x: SEXP) -> Option<(*const c_void, *const Codec)> {
    let f = STATE.load(Ordering::Acquire);
    assert!(!f.is_null(), "R8 is loaded through R_init_R8()");
    let f: unsafe extern "C" fn(SEXP, *mut *const Codec) -> *const c_void =
        unsafe { std::mem::transmute(f) };
    let mut vt: *const Codec = std::ptr::null();
    let state = unsafe { f(x, &mut vt) };
    (!state.is_null()).then_some((state, vt))
}

/// The fields of a Rust-stored value that a stream gave and that has no state;
/// `None` for any other object.
fn fields_read(x: SEXP) -> Option<SEXP> {
    let f = FIELDS_READ.load(Ordering::Acquire);
    assert!(!f.is_null(), "R8 is loaded through R_init_R8()");
    let f: unsafe extern "C" fn(SEXP) -> SEXP = unsafe { std::mem::transmute(f) };
    let fields = unsafe { f(x) };
    (!fields.is_null()).then_some(fields)
}

/// Builds the state of a Rust-stored value that a stream gave, once its fields
/// are validated; a value that has its state, or whose type has no codec, stays
/// as it is. An error is its code and message.
pub(crate) fn build(x: SEXP) -> Result<(), (&'static str, String)> {
    let Some(fields) = fields_read(x) else {
        return Ok(());
    };
    let name = crate::value::type_name(x).unwrap_or_default();
    let q = QualifiedName::parse(&name).map_err(|e| ("unknown_type", e.to_string()))?;
    let docs = registry().snapshot();
    let lookup: &dyn TypeLookup = &*docs;
    if lookup.find(&q).is_none() {
        return Err((
            "unknown_type",
            format!("`{q}` is not declared; load the package that declares it"),
        ));
    }
    let Some(vt) = crate::exports::codec(&name).map_err(|m| ("declaration_changed", m))? else {
        return Ok(());
    };
    let variant = crate::value::variant(x);
    let id =
        crate::value::id(x).ok_or(("state_absent", "the object is not a value".to_string()))?;
    let registered = crate::value::identity(&q, variant.as_deref(), lookup)?;
    let names = |id: SEXP| unsafe { Robj::from_sexp(crate::value::names_of(id)) };
    if !crate::value::current(id) || names(id) != names(registered) {
        return Err((
            "declaration_changed",
            format!("the declaration of `{q}` differs from the one the value was saved with"),
        ));
    }
    let errors = crate::construct::with_cache(|cache| {
        let v = crate::rview::RView::Whole(x);
        rtemis_r8::validate::validate_access(v, &q, lookup, cache)
    });
    if let Some(e) = errors.first() {
        return Err((
            "invalid_value",
            format!("the saved `{q}` is invalid: {}", e.message),
        ));
    }
    let state = match variant.as_deref() {
        None => from_fields(vt, fields),
        Some(v) => from_variant(vt, v, fields),
    }
    .map_err(|m| ("codec", m))?;
    let f = GIVE.load(Ordering::Acquire);
    assert!(!f.is_null(), "R8 is loaded through R_init_R8()");
    let f: unsafe extern "C" fn(SEXP, *const Codec, *mut c_void) =
        unsafe { std::mem::transmute(f) };
    unsafe { f(x, vt, state) };
    Ok(())
}

/// An empty error report, for a callee to fill.
fn blank() -> Error {
    Error {
        code: [0; 64],
        message: [0; 1024],
    }
}

/// The text of a buffer of an error report.
fn report_text(b: &[c_char]) -> String {
    let bytes: Vec<u8> = b
        .iter()
        .take_while(|&&c| c != 0)
        .map(|&c| c as u8)
        .collect();
    String::from_utf8_lossy(&bytes).into_owned()
}

/// The codec's error as text: its message followed by its code in brackets.
fn error_text(err: &Error) -> String {
    format!("{} [{}]", report_text(&err.message), report_text(&err.code))
}

/// `r8_drop_unwind()` of `view.c`, which drops the jump of R that a package's
/// `R8Api.protect` caught, set by `R_init_R8()`.
static DROP_UNWIND: std::sync::atomic::AtomicPtr<()> =
    std::sync::atomic::AtomicPtr::new(std::ptr::null_mut());

/// Receives `r8_drop_unwind()` from `R_init_R8()`.
///
/// # Safety
///
/// `drop` is that function.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn r8_set_drop_unwind(drop: *mut ()) {
    DROP_UNWIND.store(drop, std::sync::atomic::Ordering::Release);
}

/// After a codec function that R8 calls from a Rust frame: a jump of R that the
/// codec's `R8Api.protect` caught does not continue through Rust frames, so it
/// is dropped, and the call reports it as the error `unwind`.
fn codec_returned() {
    let f = DROP_UNWIND.load(std::sync::atomic::Ordering::Acquire);
    if !f.is_null() {
        let f: unsafe extern "C" fn() = unsafe { std::mem::transmute::<*mut (), _>(f) };
        unsafe { f() };
    }
}

/// A state built by the codec from the fields of `fields`, a list in
/// declaration order whose fields are validated and in their stored forms.
pub(crate) fn from_fields(vt: *const Codec, fields: SEXP) -> Result<*mut c_void, String> {
    let codec = unsafe { &*vt };
    let f = codec
        .from_fields
        .ok_or_else(|| "the codec has no `from_fields`".to_string())?;
    let items: Vec<Sexp> = (0..xlen(fields))
        .map(|i| unsafe { VECTOR_ELT(fields, i as R_xlen_t) }.cast())
        .collect();
    let mut state: *mut c_void = std::ptr::null_mut();
    let mut err = blank();
    let status = unsafe { f(items.as_ptr(), items.len(), &mut state, &mut err) };
    codec_returned();
    if status != OK || state.is_null() {
        return Err(error_text(&err));
    }
    Ok(state)
}

/// Fills `err` with `code` and `message`, truncated to fit.
pub(crate) fn set_error(err: *mut Error, code: &str, message: &str) {
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

/// Whether a codec table is an enum's: it sets both enum members.
pub(crate) fn is_enum_codec(vt: *const Codec) -> bool {
    let codec = unsafe { &*vt };
    codec.variant.is_some() && codec.from_variant.is_some()
}

/// A state built by the codec's `from_reader` from the value at `root`,
/// which `reader` lends.
pub(crate) fn from_reader(
    vt: *const Codec,
    reader: &rtemis_r8::abi::Reader,
    root: rtemis_r8::abi::Node,
) -> Result<*mut c_void, String> {
    let f = unsafe { &*vt }
        .from_reader
        .ok_or_else(|| "the codec has no `from_reader`".to_string())?;
    let mut state: *mut c_void = std::ptr::null_mut();
    let mut err = blank();
    let status = unsafe { f(reader, root, &mut state, &mut err) };
    codec_returned();
    if status != OK || state.is_null() {
        return Err(error_text(&err));
    }
    Ok(state)
}

/// The variant of a state of an enum's codec; `None` for a struct's codec.
pub(crate) fn state_variant(vt: *const Codec, state: *const c_void) -> Option<String> {
    if !is_enum_codec(vt) {
        return None;
    }
    let f = unsafe { &*vt }.variant?;
    c_text(unsafe { f(state) })
}

/// A state built by an enum's codec from the fields of `fields`, a list in
/// the declaration order of the variant `variant`, validated and in their
/// stored forms. The state's variant is checked against `variant`.
pub(crate) fn from_variant(
    vt: *const Codec,
    variant: &str,
    fields: SEXP,
) -> Result<*mut c_void, String> {
    if !is_enum_codec(vt) {
        return Err("the codec has no `variant` and `from_variant`".to_string());
    }
    let f = unsafe { &*vt }
        .from_variant
        .expect("checked by is_enum_codec");
    let name = std::ffi::CString::new(variant).map_err(|e| e.to_string())?;
    let items: Vec<Sexp> = (0..xlen(fields))
        .map(|i| unsafe { VECTOR_ELT(fields, i as R_xlen_t) }.cast())
        .collect();
    let mut state: *mut c_void = std::ptr::null_mut();
    let mut err = blank();
    let status = unsafe {
        f(
            name.as_ptr(),
            items.as_ptr(),
            items.len(),
            &mut state,
            &mut err,
        )
    };
    codec_returned();
    if status != OK || state.is_null() {
        return Err(error_text(&err));
    }
    check_variant(vt, state, variant)?;
    Ok(state)
}

/// Checks that a new state of an enum's codec has the variant `expected`;
/// on a mismatch, drops the state and returns the error `codec_variant`.
pub(crate) fn check_variant(
    vt: *const Codec,
    state: *mut c_void,
    expected: &str,
) -> Result<(), String> {
    let found = state_variant(vt, state);
    if found.as_deref() == Some(expected) {
        return Ok(());
    }
    if let Some(d) = unsafe { (*vt).drop } {
        unsafe { d(state) };
    }
    Err(format!(
        "the codec built a state of the variant `{}` for the variant `{expected}` [codec_variant]",
        found.unwrap_or_default()
    ))
}

/// `R8Api.new_value`: wraps a state of a registered Rust-stored struct or
/// enum, which the caller's module built, as an R8 value. R8 owns the state
/// once it returns `OK`; an enum value takes the variant that the codec's
/// `variant` names.
///
/// # Safety
///
/// `type_name` is NUL-terminated or NULL; `state` was built by `module`;
/// `value` and `err` are writable or NULL.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn r8_api_new_value(
    type_name: *const c_char,
    module: *const c_void,
    state: *mut c_void,
    value: *mut Sexp,
    err: *mut Error,
) -> Status {
    report(
        err,
        "R8 panicked while wrapping the state",
        AssertUnwindSafe(|| new_value(type_name, module, state, value)),
    )
}

/// Runs an API function: an error fills `err`, as does a panic, with the
/// code `panic` and the message `on_panic`.
fn report<C: AsRef<str>>(
    err: *mut Error,
    on_panic: &str,
    f: impl FnOnce() -> Result<(), (C, String)> + std::panic::UnwindSafe,
) -> Status {
    match catch_unwind(f) {
        Ok(Ok(())) => OK,
        Ok(Err((code, message))) => {
            set_error(err, code.as_ref(), &message);
            ERROR
        }
        Err(_) => {
            set_error(err, "panic", on_panic);
            ERROR
        }
    }
}

fn new_value(
    type_name: *const c_char,
    module: *const c_void,
    state: *mut c_void,
    value: *mut Sexp,
) -> Result<(), (&'static str, String)> {
    if value.is_null() || state.is_null() {
        return Err(("invalid_argument", "`state` or `value` is NULL".to_string()));
    }
    let name = c_text(type_name).unwrap_or_default();
    let Some(vt) = crate::exports::codec(&name).map_err(|m| ("declaration_changed", m))? else {
        return Err((
            "unknown_type",
            format!("`{name}` is not a registered Rust-stored type"),
        ));
    };
    if unsafe { (*vt).module } != module {
        return Err((
            "other_module",
            format!(
                "the codec of `{name}` is in the compiled code of another package, which builds its states"
            ),
        ));
    }
    let q = QualifiedName::parse(&name).map_err(|e| ("unknown_type", e.to_string()))?;
    let docs = registry().snapshot();
    let lookup: &dyn TypeLookup = &*docs;
    if lookup.find(&q).is_none() {
        return Err(("unknown_type", format!("`{q}` is not a registered type")));
    }
    let variant = state_variant(vt, state);
    let jumped = || ("r_error", "R could not allocate the value".to_string());
    let id = guarded(|| crate::value::identity(&q, variant.as_deref(), lookup))
        .ok_or_else(jumped)?
        .map_err(|_| {
            (
                "codec_variant",
                format!(
                    "the state's variant `{}` is not a variant of `{q}`",
                    variant.as_deref().unwrap_or_default()
                ),
            )
        })?;
    // The value takes the state only when the wrapper returns.
    let out = guarded(|| wrap(vt, state, id)).ok_or_else(jumped)?;
    // The caller drops the state of a value that fails, so the value gives
    // it back.
    if let Err(e) = checked(out, &q, lookup) {
        take(out);
        return Err(e);
    }
    unsafe { *value = out.cast() };
    Ok(())
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

/// `R8Api.pin`: gives R8 the declarations, a record of `len` bytes at `record`,
/// that `module` was compiled against.
///
/// # Safety
///
/// `record` points to `len` readable bytes; `err` is writable or NULL; called
/// on R's main thread.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn r8_api_pin(
    module: *const c_void,
    record: *const u8,
    len: usize,
    err: *mut Error,
) -> Status {
    report(
        err,
        "R8 panicked while reading the declarations",
        AssertUnwindSafe(|| {
            if module.is_null() || record.is_null() {
                return Err((
                    "invalid_argument",
                    "`module` or `record` is NULL".to_string(),
                ));
            }
            crate::pins::add(module, unsafe { std::slice::from_raw_parts(record, len) })
        }),
    )
}

/// The outcome of `f`, which may call R functions that jump, run by
/// `R8Api.protect`: `None` when R jumped out of it, which leaks what `f`
/// allocated in Rust. A panic in `f` continues in the caller.
pub(crate) fn guarded<T>(f: impl FnMut() -> T) -> Option<T> {
    guarded_report(f).ok()
}

/// [`guarded()`], with the message of the R error that ended `f`.
pub(crate) fn guarded_report<T>(mut f: impl FnMut() -> T) -> Result<T, String> {
    struct Run<'a, T> {
        f: &'a mut dyn FnMut() -> T,
        out: Option<std::thread::Result<T>>,
    }
    extern "C" fn run<T>(p: *mut c_void) -> SEXP {
        let r = unsafe { &mut *p.cast::<Run<T>>() };
        r.out = Some(catch_unwind(AssertUnwindSafe(|| (r.f)())));
        unsafe { extendr_ffi::R_NilValue }
    }
    let mut r = Run {
        f: &mut f,
        out: None,
    };
    let mut value: SEXP = unsafe { extendr_ffi::R_NilValue };
    let mut err = blank();
    let status = protect(
        run::<T>,
        std::ptr::from_mut(&mut r).cast(),
        &mut value,
        &mut err,
    );
    match r.out.filter(|_| status == OK) {
        Some(Ok(v)) => Ok(v),
        Some(Err(p)) => std::panic::resume_unwind(p),
        None => Err(report_text(&err.message)),
    }
}

/// Whether values of the type `q` are validated when Rust code makes them
/// (`rtemis_r8::validate::constrains()`), kept per registry generation.
fn constrained(q: &QualifiedName, lookup: &dyn TypeLookup) -> bool {
    use std::collections::HashMap;
    use std::sync::Mutex;
    static KEPT: Mutex<Option<(u64, HashMap<String, bool>)>> = Mutex::new(None);
    let now = crate::construct::GENERATION.load(Ordering::Relaxed);
    let mut kept = KEPT.lock().unwrap_or_else(|e| e.into_inner());
    if kept.as_ref().is_none_or(|(g, _)| *g != now) {
        *kept = Some((now, HashMap::new()));
    }
    let (_, by_name) = kept.as_mut().expect("set above");
    *by_name
        .entry(q.to_string())
        .or_insert_with(|| rtemis_r8::validate::constrains(q, lookup))
}

/// Validates `x`, a value of the type `q` that Rust code made, when its type
/// constrains it: every problem, as `invalid_value` with their text, or an R
/// error raised while a field was read, such as the build of a replaced
/// `subtype()` field.
fn checked(
    x: SEXP,
    q: &QualifiedName,
    lookup: &dyn TypeLookup,
) -> Result<(), (&'static str, String)> {
    if !constrained(q, lookup) {
        return Ok(());
    }
    let errors = guarded_report(|| unsafe {
        extendr_ffi::Rf_protect(x);
        let found = crate::construct::with_cache(|cache| {
            rtemis_r8::validate::validate_access(crate::rview::RView::Whole(x), q, lookup, cache)
        });
        extendr_ffi::Rf_unprotect(1);
        found
    })
    .map_err(|m| ("invalid_value", m))?;
    if errors.is_empty() {
        return Ok(());
    }
    let mut errors = errors;
    for e in &mut errors {
        if let Some(p) = &e.problem {
            e.message = p.sentence(
                &e.segments,
                e.found.as_ref(),
                e.suggestion.as_deref(),
                &rtemis_r8::problem::Neutral,
            );
        }
    }
    Err((
        "invalid_value",
        format!(
            "the value of `{q}` is invalid: {}",
            crate::construct::problems_text(&errors)
        ),
    ))
}

/// `R8Api.value_of_fields`: builds a value of a registered struct, or of a
/// variant of an enum, from the list of its fields, which is not validated;
/// the codec of a Rust-stored type builds its state.
///
/// # Safety
///
/// `type_name` and `declaration_hash` are NUL-terminated or NULL, as is
/// `variant`, NULL for a struct; `fields` is a protected list that nothing
/// else refers to; `value` and `err` are writable or NULL; called on R's main
/// thread.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn r8_api_value_of_fields(
    type_name: *const c_char,
    variant: *const c_char,
    declaration_hash: *const c_char,
    fields: Sexp,
    value: *mut Sexp,
    err: *mut Error,
) -> Status {
    report(
        err,
        "R8 panicked while building the value",
        AssertUnwindSafe(|| {
            if value.is_null() || type_name.is_null() || declaration_hash.is_null() {
                return Err((
                    "invalid_argument",
                    "`type_name`, `declaration_hash`, or `value` is NULL".to_string(),
                ));
            }
            let name = c_text(type_name).unwrap_or_default();
            let q = QualifiedName::parse(&name).map_err(|e| ("unknown_type", e.to_string()))?;
            let docs = registry().snapshot();
            if docs.find(&q).is_none() {
                return Err(("unknown_type", format!("`{q}` is not a registered type")));
            }
            let hash = c_text(declaration_hash).unwrap_or_default();
            if *crate::construct::registry_declaration_hash(&q, &*docs) != *hash {
                return Err((
                    "declaration_changed",
                    format!(
                        "the registered declaration of `{q}` differs from the one that the caller was compiled against; reinstall the caller's package"
                    ),
                ));
            }
            let x = value_of_fields(&q, c_text(variant).as_deref(), fields.cast(), &*docs, true)?;
            checked(x, &q, &*docs)?;
            unsafe { *value = x.cast() };
            Ok(())
        }),
    )
}

/// The value of the type `q`, of the variant `variant` for an enum, over
/// `fields`, the list of its fields in declaration order and in their stored
/// forms, which are not validated: a value over the list itself, or, for a
/// type whose codec is registered, a Rust-stored value of the state that the
/// codec builds from them. R allocates under `guarded()` with `guard`, for a
/// caller that R must not jump out of, and otherwise in the caller's frame.
pub(crate) fn value_of_fields(
    q: &QualifiedName,
    variant: Option<&str>,
    fields: SEXP,
    lookup: &dyn TypeLookup,
    guard: bool,
) -> Result<SEXP, (&'static str, String)> {
    let jumped = || ("r_error", "R could not allocate the value".to_string());
    let identity = || crate::value::identity(q, variant, lookup);
    let id = if guard {
        guarded(identity).ok_or_else(jumped)??
    } else {
        identity()?
    };
    let n = xlen(crate::value::names_of(id));
    let is_list = unsafe { extendr_ffi::TYPEOF(fields) } == SEXPTYPE::VECSXP
        && unsafe { extendr_ffi::ALTREP(fields) } == 0
        && crate::rview::attrib_count(fields) == 0;
    if !is_list || xlen(fields) != n {
        return Err((
            "invalid_argument",
            format!("`fields` is the list of the {n} fields of `{q}`, without attributes"),
        ));
    }
    if !crate::value::fit(fields) {
        return Err(("too_deep", rtemis_r8::access::too_deep().message));
    }
    let Some(vt) = crate::exports::codec(&q.to_string()).map_err(|m| ("declaration_changed", m))?
    else {
        return if guard {
            guarded(|| crate::value::new(fields, id)).ok_or_else(jumped)
        } else {
            Ok(crate::value::new(fields, id))
        };
    };
    let state = match variant {
        None => from_fields(vt, fields),
        Some(v) => from_variant(vt, v, fields),
    }
    .map_err(|m| ("codec", m))?;
    if !guard {
        return Ok(wrap(vt, state, id));
    }
    guarded(|| wrap(vt, state, id)).ok_or_else(|| {
        if let Some(d) = unsafe { (*vt).drop } {
            unsafe { d(state) };
        }
        jumped()
    })
}

/// `R8Api.with_fields`: the value of `value` with the fields of `fields`, a
/// named list, replaced, as `value[names] <- fields` gives it.
///
/// # Safety
///
/// `value` and `fields` are protected; `new_value` and `err` are writable or
/// NULL; called on R's main thread.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn r8_api_with_fields(
    value: Sexp,
    fields: Sexp,
    new_value: *mut Sexp,
    err: *mut Error,
) -> Status {
    report(
        err,
        "R8 panicked while replacing the fields",
        AssertUnwindSafe(|| {
            if new_value.is_null() {
                return Err(("invalid_argument", "`new_value` is NULL".to_string()));
            }
            let out = guarded(|| crate::construct::with_fields(value.cast(), fields.cast()))
                .ok_or_else(|| ("r_error", "R raised an error".to_string()))??;
            unsafe { *new_value = out.cast() };
            Ok(())
        }),
    )
}

/// `R8Api.value_state`: lends the state of a value of the type `type_name` to
/// the caller's module.
///
/// # Safety
///
/// `type_name` is NUL-terminated or NULL; `state` and `err` are writable or
/// NULL; called on R's main thread.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn r8_api_value_state(
    value: Sexp,
    type_name: *const c_char,
    module: *const c_void,
    state: *mut *const c_void,
    err: *mut Error,
) -> Status {
    report(
        err,
        "R8 panicked while reading the state",
        AssertUnwindSafe(|| lend(value, type_name, module, state, false)),
    )
}

/// `R8Api.upcast_state`: lends the state of a value of the type `type_name`,
/// or, for a value of a struct that extends it, the state that its state
/// embeds.
///
/// # Safety
///
/// As for [`r8_api_value_state`].
#[unsafe(no_mangle)]
pub unsafe extern "C" fn r8_api_upcast_state(
    value: Sexp,
    type_name: *const c_char,
    module: *const c_void,
    state: *mut *const c_void,
    err: *mut Error,
) -> Status {
    report(
        err,
        "R8 panicked while reading the state",
        AssertUnwindSafe(|| lend(value, type_name, module, state, true)),
    )
}

/// Lends the state of `value`, a value of the type `type_name` or, with
/// `upcast`, of a struct that extends it, to `module`: the codec of the
/// value has the same module and, for another type, embeds the state of
/// `type_name`. `not_lent` reports any other value of the type, or of a
/// registered subtype with `upcast`, whose fields the module may read, and
/// `pins::check()`'s codes one whose fields it may not; `type_mismatch` any
/// other object. The state of a value read
/// before its codec was registered is built first; a value whose state cannot
/// be built is the codec's error.
fn lend(
    value: Sexp,
    type_name: *const c_char,
    module: *const c_void,
    state: *mut *const c_void,
    upcast: bool,
) -> Result<(), (std::borrow::Cow<'static, str>, String)> {
    if state.is_null() || type_name.is_null() {
        return Err((
            "invalid_argument".into(),
            "`type_name` or `state` is NULL".to_string(),
        ));
    }
    let x: SEXP = value.cast();
    if let Some(o) = crate::value::other(x) {
        return Err((
            "declaration_changed".into(),
            format!("the value was {}", o.message()),
        ));
    }
    if state_of(x).is_none() {
        guarded(|| build(x))
            .ok_or_else(|| {
                (
                    "r_error".into(),
                    "R could not allocate the value".to_string(),
                )
            })?
            .map_err(|(code, message)| (code.into(), message))?;
    }
    let name = unsafe { CStr::from_ptr(type_name) };
    if let Some((s, vt)) = state_of(x) {
        let codec = unsafe { &*vt };
        if codec.module == module && !module.is_null() {
            let lent = if unsafe { CStr::from_ptr(codec.type_name) } == name {
                s
            } else {
                match codec.upcast {
                    Some(f) if upcast => unsafe { f(s, type_name) },
                    _ => std::ptr::null(),
                }
            };
            if !lent.is_null() {
                unsafe { *state = lent };
                return Ok(());
            }
        }
    }
    let name = name.to_string_lossy();
    let held = crate::value::type_name(x).unwrap_or_default();
    let is_one = held == name
        || (upcast
            && match (QualifiedName::parse(&held), QualifiedName::parse(&name)) {
                (Ok(h), Ok(n)) => {
                    let docs = registry().snapshot();
                    let lookup: &dyn TypeLookup = &*docs;
                    lookup.is_subtype(&h, &n) && !lookup.is_stale(&h)
                }
                _ => false,
            });
    Err(if is_one {
        match crate::pins::check(module, &name) {
            Ok(()) => (
                "not_lent".into(),
                format!("the state of the `{held}` is not this module's to read"),
            ),
            Err((code, message)) => (code.into(), message),
        }
    } else if held.is_empty() {
        (
            "type_mismatch".into(),
            format!("the object is not a `{name}`"),
        )
    } else {
        (
            "type_mismatch".into(),
            format!("the value is a `{held}`, not a `{name}`"),
        )
    })
}
