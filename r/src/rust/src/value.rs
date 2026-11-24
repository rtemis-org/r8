//! R8 values as R objects (`src/value.c`): the identity of a value, and a value
//! over the list of its fields.

use std::borrow::Cow;
use std::collections::HashMap;
use std::sync::atomic::{AtomicPtr, Ordering};
use std::sync::{Mutex, OnceLock};

use extendr_api::prelude::*;
use extendr_api::{Error, Result, SEXP};
use extendr_ffi::{
    R_NilValue, Rf_allocVector, Rf_protect, Rf_unprotect, SET_STRING_ELT, SEXPTYPE, VECTOR_ELT,
};
use rtemis_r8::ir::{TypeDef, VariantKind};
use rtemis_r8::name::QualifiedName;
use rtemis_r8::rules::{DocumentSet, TypeLookup};
use rtemis_r8::update::Effect;
use rtemis_r8::validate::ValueError;

use crate::construct::{GENERATION, mkchar};
use crate::rview::string_elt;

#[allow(improper_ctypes)]
unsafe extern "C" {
    fn Rf_shallow_duplicate(x: SEXP) -> SEXP;
}

static IDENTITY: AtomicPtr<()> = AtomicPtr::new(std::ptr::null_mut());
static HELD: AtomicPtr<()> = AtomicPtr::new(std::ptr::null_mut());
static ID: AtomicPtr<()> = AtomicPtr::new(std::ptr::null_mut());
static FIELDS: AtomicPtr<()> = AtomicPtr::new(std::ptr::null_mut());
static VALUE: AtomicPtr<()> = AtomicPtr::new(std::ptr::null_mut());
static FIT: AtomicPtr<()> = AtomicPtr::new(std::ptr::null_mut());
static VIEWED: AtomicPtr<()> = AtomicPtr::new(std::ptr::null_mut());

/// The elements of an identity (`value.c`).
const ID_TYPE: isize = 0;
const ID_VARIANT: isize = 1;
const ID_NAMES: isize = 2;
const ID_DECLARATION: isize = 3;
const ID_RECORD: isize = 5;
const ID_STAMP: isize = 6;

/// Receives the functions of `value.c` from `R_init_R8()`.
///
/// # Safety
///
/// `identity`, `held`, `id`, `fields`, `value`, `fit`, and `viewed` are
/// `r8_identity()`, `r8_held_identity()`, `r8_value_id()`, `r8_value_fields()`,
/// `r8_value_new()`, `r8_fields_fit()`, and `r8_field_viewed()`.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn r8_set_value_functions(
    identity: *mut (),
    held: *mut (),
    id: *mut (),
    fields: *mut (),
    value: *mut (),
    fit: *mut (),
    viewed: *mut (),
) {
    IDENTITY.store(identity, Ordering::Release);
    HELD.store(held, Ordering::Release);
    ID.store(id, Ordering::Release);
    FIELDS.store(fields, Ordering::Release);
    VALUE.store(value, Ordering::Release);
    FIT.store(fit, Ordering::Release);
    VIEWED.store(viewed, Ordering::Release);
}

fn loaded(f: &AtomicPtr<()>) -> *mut () {
    let f = f.load(Ordering::Acquire);
    assert!(!f.is_null(), "R8 is loaded through R_init_R8()");
    f
}

/// The identity of `x`; `None` for an object that is not a value.
pub(crate) fn id(x: SEXP) -> Option<SEXP> {
    let f: unsafe extern "C" fn(SEXP) -> SEXP = unsafe { std::mem::transmute(loaded(&ID)) };
    let id = unsafe { f(x) };
    (id != unsafe { R_NilValue }).then_some(id)
}

/// Whether `x` is a value.
pub(crate) fn is_value(x: SEXP) -> bool {
    id(x).is_some()
}

/// The qualified type name of the value `x`.
pub(crate) fn type_name<'a>(x: SEXP) -> Option<Cow<'a, str>> {
    string_elt(unsafe { VECTOR_ELT(id(x)?, ID_TYPE) }, 0)
}

/// The type name of the value `x`, of the registered declaration, as a
/// `CHARSXP`.
pub(crate) fn usable_type(x: SEXP) -> Option<SEXP> {
    let id = id(x).filter(|&id| current(id))?;
    Some(unsafe { extendr_ffi::STRING_ELT(VECTOR_ELT(id, ID_TYPE), 0) })
}

/// The variant of the enum value `x`.
pub(crate) fn variant<'a>(x: SEXP) -> Option<Cow<'a, str>> {
    let v = unsafe { VECTOR_ELT(id(x)?, ID_VARIANT) };
    if v == unsafe { R_NilValue } {
        None
    } else {
        string_elt(v, 0)
    }
}

/// The names of the fields of the values of the identity `id`.
pub(crate) fn names_of(id: SEXP) -> SEXP {
    unsafe { VECTOR_ELT(id, ID_NAMES) }
}

/// The list from which the fields of `x` are read: the fields of an R-stored
/// value, and `x` itself for any other object.
pub(crate) fn fields(x: SEXP) -> SEXP {
    let f: unsafe extern "C" fn(SEXP) -> SEXP = unsafe { std::mem::transmute(loaded(&FIELDS)) };
    unsafe { f(x) }
}

/// The vector that `x` views, for a view that a read of a field gave (`field.c`),
/// and `x` for any other object.
pub(crate) fn viewed(x: SEXP) -> SEXP {
    let f: unsafe extern "C" fn(SEXP) -> SEXP = unsafe { std::mem::transmute(loaded(&VIEWED)) };
    unsafe { f(x) }
}

/// Whether the values and lists in `fields`, the list of a value's fields, nest
/// no deeper than a value may.
pub(crate) fn fit(fields: SEXP) -> bool {
    let f: unsafe extern "C" fn(SEXP) -> i32 = unsafe { std::mem::transmute(loaded(&FIT)) };
    unsafe { f(fields) != 0 }
}

/// The value of the identity `id` over `fields`, a protected list without
/// attributes that nothing else writes; unprotected.
pub(crate) fn new(fields: SEXP, id: SEXP) -> SEXP {
    let f: unsafe extern "C" fn(SEXP, SEXP) -> SEXP =
        unsafe { std::mem::transmute(loaded(&VALUE)) };
    unsafe { f(fields, id) }
}

/// The class vector of a value of the type `q` and, for an enum, the variant
/// `variant`: the variant as `Type@Variant`, the type, its ancestors through
/// `extends`, the nearest first, and `r8_value`.
fn class(q: &QualifiedName, variant: Option<&str>, lookup: &dyn TypeLookup) -> Vec<String> {
    let mut out: Vec<String> = variant.iter().map(|v| format!("{q}@{v}")).collect();
    out.push(q.to_string());
    out.extend(lookup.ancestors(q).iter().map(|a| a.to_string()));
    out.push("r8_value".to_string());
    out
}

/// The text that names the declaration of the registered type `q` in an
/// identity: its hash and, for a struct that extends another, its ancestors,
/// each after `<`.
pub(crate) fn declaration(q: &QualifiedName, lookup: &dyn TypeLookup) -> String {
    let mut out = crate::construct::registry_declaration_hash(q, lookup).to_string();
    for a in lookup.ancestors(q) {
        out.push('<');
        out.push_str(&a.to_string());
    }
    out
}

/// The field names of a value of the type `def`, of the variant `variant` for
/// an enum; `None` when the enum declares no such variant.
pub(crate) fn field_names<'a>(
    def: &'a TypeDef,
    variant: Option<&str>,
    lookup: &'a dyn TypeLookup,
) -> Option<Vec<&'a str>> {
    fn names(fs: &[rtemis_r8::ir::FieldDef]) -> Vec<&str> {
        fs.iter().map(|f| f.name.as_str()).collect()
    }
    match def {
        TypeDef::Struct(s) => Some(names(&s.fields)),
        TypeDef::Enum(e) => {
            let v = e
                .variants
                .iter()
                .find(|v| Some(v.name.as_str()) == variant)?;
            match &v.kind {
                VariantKind::Unit => Some(Vec::new()),
                VariantKind::Struct { fields, .. } => Some(names(fields)),
                VariantKind::Newtype { of } => match lookup.find(of) {
                    Some(TypeDef::Struct(s)) => Some(names(&s.fields)),
                    _ => None,
                },
            }
        }
    }
}

/// The identity of the values of the registered type `q`, of the variant
/// `variant` for an enum; an error is a code and a message. The session holds
/// it (`identities` in `value.c`), so it needs no protection. R allocates a new
/// one in the caller's frame.
pub(crate) fn identity(
    q: &QualifiedName,
    variant: Option<&str>,
    lookup: &dyn TypeLookup,
) -> std::result::Result<SEXP, (&'static str, String)> {
    /// The address of an identity, which R keeps for the session.
    #[derive(Clone, Copy)]
    struct Held(SEXP);
    // SAFETY: R8's native functions run on R's main thread only; the cache is
    // never read from another thread.
    unsafe impl Send for Held {}
    type ByVariant = Vec<(Box<str>, Held)>;
    static CACHE: OnceLock<Mutex<(u64, HashMap<QualifiedName, ByVariant>)>> = OnceLock::new();
    let m = CACHE.get_or_init(|| Mutex::new((0, HashMap::new())));
    let g = GENERATION.load(Ordering::Relaxed);
    let v = variant.unwrap_or("");
    {
        let mut guard = m.lock().unwrap_or_else(|e| e.into_inner());
        if guard.0 != g {
            guard.1.clear();
            guard.0 = g;
        }
        let held = guard
            .1
            .get(q)
            .and_then(|of| of.iter().find(|(n, _)| **n == *v));
        if let Some((_, id)) = held {
            return Ok(id.0);
        }
    }
    let def = lookup
        .find(q)
        .ok_or_else(|| ("unknown_type", format!("`{q}` is not a registered type")))?;
    let names = field_names(def, variant, lookup)
        .ok_or_else(|| ("unknown_variant", format!("`{q}` has no variant `{v}`")))?;
    let cls = class(q, variant, lookup);
    let cls: Vec<&str> = cls.iter().map(String::as_str).collect();
    // The vectors are R allocations, from which R may jump: no lock is held.
    let record = || rtemis_r8::canonical::declaration_text(q, lookup);
    let id = new_identity(
        q,
        variant,
        &names,
        &cls,
        &declaration(q, lookup),
        &record,
        true,
    );
    set_stamp(id, g, true);
    let mut guard = m.lock().unwrap_or_else(|e| e.into_inner());
    if guard.0 == g {
        let of = guard.1.entry(q.clone()).or_default();
        of.push((v.into(), Held(id)));
    }
    Ok(id)
}

/// An identity of the parts given (`r8_identity()` in `value.c`).
fn new_identity(
    q: &QualifiedName,
    variant: Option<&str>,
    names: &[&str],
    class: &[&str],
    declaration: &str,
    record: &dyn Fn() -> String,
    registered: bool,
) -> SEXP {
    let text = std::ffi::CString::new(declaration).unwrap_or_default();
    let qname = q.to_string();
    unsafe {
        let held: unsafe extern "C" fn(SEXP, SEXP, SEXP) -> SEXP =
            std::mem::transmute(loaded(&HELD));
        let f: unsafe extern "C" fn(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, i32) -> SEXP =
            std::mem::transmute(loaded(&IDENTITY));
        let ty = Rf_protect(strings(&[&qname]));
        let var = Rf_protect(match variant {
            Some(v) => strings(&[v]),
            None => R_NilValue,
        });
        let names = Rf_protect(strings(names));
        let cls = Rf_protect(strings(class));
        let sym = extendr_ffi::Rf_install(text.as_ptr());
        // The text is written for a declaration that the session does not hold.
        let record = Rf_protect(if held(ty, var, sym) == R_NilValue {
            symbols(&record())
        } else {
            R_NilValue
        });
        let id = f(ty, var, names, cls, sym, record, i32::from(registered));
        Rf_unprotect(5);
        id
    }
}

/// The bytes of a symbol of a declaration's text; R admits 10,000.
const SYMBOL_BYTES: usize = 8000;

/// A declaration's text as a list of symbols, which R writes once per stream,
/// unprotected. The text is JSON, and its characters outside ASCII are written
/// as escapes, so that it has one form in every encoding.
///
/// # Safety
///
/// Called on R's main thread.
unsafe fn symbols(text: &str) -> SEXP {
    let mut ascii = String::with_capacity(text.len());
    for c in text.chars() {
        if c.is_ascii() {
            ascii.push(c);
        } else {
            let mut units = [0u16; 2];
            for u in c.encode_utf16(&mut units) {
                ascii.push_str(&format!("\\u{u:04x}"));
            }
        }
    }
    let parts: Vec<std::ffi::CString> = ascii
        .as_bytes()
        .chunks(SYMBOL_BYTES)
        .map(|b| std::ffi::CString::new(b).unwrap_or_default())
        .collect();
    unsafe {
        let out = Rf_protect(Rf_allocVector(SEXPTYPE::VECSXP, parts.len() as isize));
        for (i, p) in parts.iter().enumerate() {
            extendr_ffi::SET_VECTOR_ELT(out, i as isize, extendr_ffi::Rf_install(p.as_ptr()));
        }
        Rf_unprotect(1);
        out
    }
}

/// The text of a symbol.
fn symbol_text<'a>(sym: SEXP) -> Cow<'a, str> {
    unsafe { std::ffi::CStr::from_ptr(extendr_ffi::R_CHAR(extendr_ffi::PRINTNAME(sym))) }
        .to_string_lossy()
}

fn set_stamp(id: SEXP, generation: u64, current: bool) {
    unsafe {
        let stamp = extendr_ffi::REAL(VECTOR_ELT(id, ID_STAMP));
        *stamp = generation as f64;
        *stamp.add(1) = f64::from(u8::from(current));
    }
}

/// Whether the identity `id` is of the registered declaration of its type, or
/// of one whose differences have no part in a value. It allocates no R memory; the identity holds the answer until the registry
/// changes.
pub(crate) fn current(id: SEXP) -> bool {
    let g = GENERATION.load(Ordering::Relaxed);
    unsafe {
        let stamp = extendr_ffi::REAL(VECTOR_ELT(id, ID_STAMP));
        if *stamp == g as f64 {
            return *stamp.add(1) == 1.0;
        }
    }
    let name = string_elt(unsafe { VECTOR_ELT(id, ID_TYPE) }, 0).unwrap_or_default();
    let is = QualifiedName::parse(&name).is_ok_and(|q| {
        let docs = crate::declare::registry().snapshot();
        let lookup: &dyn TypeLookup = &*docs;
        if lookup.find(&q).is_none() {
            return false;
        }
        let (is, was) = (
            declaration(&q, lookup),
            symbol_text(unsafe { VECTOR_ELT(id, ID_DECLARATION) }),
        );
        // The ancestors follow the hash.
        let after = |text: &str| {
            text.find('<')
                .map_or(String::new(), |i| text[i..].to_string())
        };
        is == was
            || after(&is) == after(&was)
                && made_under(id).is_some_and(|made| {
                    rtemis_r8::update::changes(&q, &*made, lookup)
                        .iter()
                        .all(|c| c.effect == Effect::None)
                })
    });
    set_stamp(id, g, is);
    is
}

/// [`current`] for R8's C code; 1 or 0.
///
/// # Safety
///
/// `id` is an identity; called on R's main thread.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn r8_identity_current(id: SEXP) -> i32 {
    std::panic::catch_unwind(|| i32::from(current(id))).unwrap_or(0)
}

thread_local! {
    /// Whether a value of another declaration than the registered one reads as
    /// one of its type, for what looks at values.
    static LOOKING: std::cell::Cell<bool> = const { std::cell::Cell::new(false) };
}

/// The outcome of `f`, which looks at values: those of other declarations read
/// as of their types. `None` when R jumped out of `f`.
pub(crate) fn looking<T>(f: impl FnMut() -> T) -> Option<T> {
    let before = LOOKING.replace(true);
    let out = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        crate::rust_stored::guarded(f)
    }));
    LOOKING.set(before);
    out.unwrap_or_else(|p| std::panic::resume_unwind(p))
}

thread_local! {
    /// Whether a comparison met a value of another declaration than the
    /// registered one, in a field that names no type.
    static MET: std::cell::Cell<Option<SEXP>> = const { std::cell::Cell::new(None) };
}

/// Records that `x` was read as a value of the type that it names.
pub(crate) fn meet(x: SEXP) {
    if id(x).is_some_and(|id| !current(id)) {
        MET.set(Some(x));
    }
}

/// The outcome of `f`, a comparison, and a value of another declaration that it
/// met.
pub(crate) fn meeting<T>(f: impl FnOnce() -> T) -> (T, Option<SEXP>) {
    MET.set(None);
    let out = f();
    (out, MET.take())
}

/// Whether the value `x` is one to use: of the registered declaration of its
/// type.
pub(crate) fn usable(x: SEXP) -> bool {
    id(x).is_some_and(|id| LOOKING.get() || current(id))
}

/// The declarations that a value was made under: of its type and of every type
/// that it reaches; `None` when this R8 does not read them.
pub(crate) fn made_under(id: SEXP) -> Option<std::sync::Arc<DocumentSet>> {
    type Read = HashMap<usize, Option<std::sync::Arc<DocumentSet>>>;
    static READ: OnceLock<Mutex<Read>> = OnceLock::new();
    let mut held = READ
        .get_or_init(|| Mutex::new(HashMap::new()))
        .lock()
        .unwrap_or_else(|e| e.into_inner());
    held.entry(id as usize)
        .or_insert_with(|| {
            let record = unsafe { VECTOR_ELT(id, ID_RECORD) };
            let text: String = (0..crate::rview::xlen(record))
                .map(|i| symbol_text(unsafe { VECTOR_ELT(record, i as isize) }))
                .collect();
            let name = symbol_text(unsafe { VECTOR_ELT(id, ID_DECLARATION) });
            let hash = name.split('<').next().unwrap_or_default();
            rtemis_r8::update::read_declarations(&text, hash).map(std::sync::Arc::new)
        })
        .clone()
}

/// What differs between the declaration that a value was made under and the
/// registered one.
pub(crate) struct Other {
    q: QualifiedName,
    changes: Vec<rtemis_r8::update::Change>,
    /// Whether R8 reads the declaration that the value was made under.
    read: bool,
}

impl Other {
    /// Whether the value's fields mean the same under both.
    pub(crate) fn keeps_meaning(&self) -> bool {
        self.read && self.changes.iter().all(|c| c.effect < Effect::Meaning)
    }

    /// What differs, as a message goes on after "the value was" or "one".
    fn what(&self) -> String {
        const SHOWN: usize = 4;
        let q = &self.q;
        if !self.read {
            return format!(
                "made under another declaration of `{q}` than the registered one, which this version of R8 does not read"
            );
        }
        // The changes that keep `r8_update()` from converting the value first.
        let mut found: Vec<&rtemis_r8::update::Change> = self.changes.iter().collect();
        found.retain(|c| c.effect > Effect::None);
        found.sort_by_key(|c| std::cmp::Reverse(c.effect));
        let mut lines: Vec<String> = found
            .iter()
            .take(SHOWN)
            .map(|c| {
                if c.type_name == q.to_string() {
                    c.message.clone()
                } else {
                    format!("{} (in `{}`)", c.message, c.type_name)
                }
            })
            .collect();
        if found.len() > SHOWN {
            lines.push(format!("and {} more", found.len() - SHOWN));
        }
        format!(
            "made under another declaration of `{q}` than the registered one: {}",
            lines.join("; ")
        )
    }

    /// Why `r8_update()` does not convert the value.
    fn reason(&self) -> &'static str {
        match self.changes.iter().find(|c| c.effect == Effect::Meaning) {
            None => "its declaration does not read",
            Some(c) => match c.code {
                "field_added" => "it lacks a field without a default",
                "field_removed" => "it holds a field that was removed",
                "variant_removed" => "its variant was removed",
                "version_changed" => "another version states that the fields mean something else",
                _ => "its fields may mean something else",
            },
        }
    }

    /// What differs, and whether `r8_update()` converts the value.
    pub(crate) fn message(&self) -> String {
        if self.keeps_meaning() {
            format!(
                "{}; r8_update() gives its value under the registered one",
                self.what()
            )
        } else {
            format!(
                "{}; r8_update() does not convert it: {}",
                self.what(),
                self.reason()
            )
        }
    }
}

/// What differs for the value `x` of a registered type; `None` for one of the
/// registered declaration and for one whose type is not registered.
pub(crate) fn other(x: SEXP) -> Option<Other> {
    let id = id(x).filter(|&id| !current(id))?;
    let q = QualifiedName::parse(&type_name(x)?).ok()?;
    let docs = crate::declare::registry().snapshot();
    let lookup: &dyn TypeLookup = &*docs;
    lookup.find(&q)?;
    let made = made_under(id);
    let mut changes = match &made {
        Some(m) => rtemis_r8::update::changes(&q, &**m, lookup),
        None => Vec::new(),
    };
    // A removed variant matters to its values.
    if let Some(def) = lookup.find(&q)
        && field_names(def, variant(x).as_deref(), lookup).is_none()
    {
        for c in &mut changes {
            if c.code == "variant_removed" && c.type_name == q.to_string() {
                c.effect = Effect::Meaning;
            }
        }
    }
    let name = symbol_text(unsafe { VECTOR_ELT(id, ID_DECLARATION) });
    let before: Vec<&str> = name.split('<').skip(1).collect();
    let after: Vec<String> = lookup.ancestors(&q).iter().map(|a| a.to_string()).collect();
    if made.is_some() && before != after {
        let text = |of: &[String]| match of {
            [] => "none".to_string(),
            _ => of.join(", "),
        };
        let before: Vec<String> = before.iter().map(|s| s.to_string()).collect();
        changes.push(rtemis_r8::update::Change {
            code: "ancestors_changed",
            type_name: q.to_string(),
            message: format!(
                "the ancestors of `{q}` were {}, and are {}",
                text(&before),
                text(&after)
            ),
            effect: Effect::Form,
        });
    }
    Some(Other {
        q,
        changes,
        read: made.is_some(),
    })
}

/// What differs between the declaration that a value was made under and the
/// registered one.
///
/// @param x An R8 value.
/// @return List: `message`, character; `changes`, character, what differs;
///   `keeps_meaning`, logical, whether `r8_update()` converts the value; or
///   `NULL` for a value of the registered declaration and for one whose type is
///   not registered.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_value_other(x: Robj) -> Robj {
    match other(unsafe { x.get() }) {
        None => ().into(),
        Some(o) => {
            let changes: Vec<&str> = o.changes.iter().map(|c| c.message.as_str()).collect();
            list!(
                message = o.message(),
                changes = changes,
                keeps_meaning = o.keeps_meaning()
            )
            .into()
        }
    }
}

/// Why a value is not updated.
enum Refused {
    Deep(String),
    Other(String),
    Invalid(String, Vec<ValueError>),
}

impl From<Error> for Refused {
    fn from(e: Error) -> Self {
        Refused::Other(e.to_string())
    }
}

/// The value of `x` under the registered declarations, when it or a value that
/// it holds was made under others; `None` for an object that holds none.
fn updated(
    x: SEXP,
    lookup: &DocumentSet,
    levels: u32,
) -> std::result::Result<Option<Robj>, Refused> {
    if levels == 0 {
        return Err(Refused::Deep(format!(
            "it nests deeper than {} levels",
            rtemis_r8::validate::MAX_NESTING
        )));
    }
    let Some(id) = id(x) else {
        if crate::rview::sexptype(x) != SEXPTYPE::VECSXP || unsafe { extendr_ffi::ALTREP(x) } != 0 {
            return Ok(None);
        }
        let mut out: Option<Robj> = None;
        for i in 0..crate::rview::xlen(x) as isize {
            if let Some(v) = updated(unsafe { VECTOR_ELT(x, i) }, lookup, levels - 1)? {
                let list =
                    out.get_or_insert_with(|| unsafe { Robj::from_sexp(Rf_shallow_duplicate(x)) });
                unsafe { extendr_ffi::SET_VECTOR_ELT(list.get(), i, v.get()) };
            }
        }
        return Ok(out);
    };
    let name = type_name(x).unwrap_or_default();
    let q = QualifiedName::parse(&name).map_err(|e| Refused::Other(e.to_string()))?;
    let variant = variant(x);
    let Some(def) = lookup.find(&q) else {
        return Err(Refused::Other(format!(
            "`{q}` is not a registered type; load the package that declares it"
        )));
    };
    let is_current = current(id);
    if let Some(o) = other(x)
        && !o.keeps_meaning()
    {
        return Err(Refused::Other(format!(
            "it was {}; {}",
            o.what(),
            o.reason()
        )));
    }
    let Some(names) = field_names(def, variant.as_deref(), lookup) else {
        return Err(Refused::Other(format!(
            "`{q}` has no variant `{}`",
            variant.unwrap_or_default()
        )));
    };
    let held = names_of(id);
    let from = fields(x);
    let mut changed = !is_current;
    let mut values: Vec<Robj> = Vec::with_capacity(names.len());
    for (k, n) in names.iter().enumerate() {
        let at = |i: usize| string_elt(held, i).is_some_and(|h| h == *n);
        let len = crate::rview::xlen(held);
        let i = if k < len && at(k) {
            Some(k)
        } else {
            (0..len).find(|&i| at(i))
        };
        let v = match i {
            Some(i) => unsafe { VECTOR_ELT(from, i as isize) },
            None => unsafe { R_NilValue },
        };
        values.push(match updated(v, lookup, levels - 1)? {
            Some(u) => {
                changed = true;
                u
            }
            None => unsafe { Robj::from_sexp(v) },
        });
    }
    if !changed {
        return Ok(None);
    }
    let list: Robj = List::from_names_and_values(&names, values)?.into();
    let xs = unsafe { list.get() };
    let v = variant.as_deref().unwrap_or("");
    let errors = crate::construct::with_cache(|cache| {
        let draft = crate::rview::RView::Draft(xs, v);
        rtemis_r8::validate::validate_access(draft, &q, lookup, cache)
    });
    if !errors.is_empty() {
        return Err(Refused::Invalid(q.to_string(), errors));
    }
    Ok(Some(crate::construct::assemble(xs, &q, v, lookup)?))
}

/// The value of `x` under the registered declarations of its type and of the
/// types that it holds.
///
/// @param x An R8 value, or a list that holds some.
/// @return List: `value`; or `message` and, for a list that nests too deep, its
///   code, or `errors` and `of`, the type, why the value
///   is not converted.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_updated_value(x: Robj) -> Result<List> {
    crate::interrupt::interruptible(unsafe { x.get() }, || {
        let xs = unsafe { x.get() };
        let docs = crate::declare::registry().snapshot();
        // A field read of a Rust-stored value may raise an R error.
        let read = crate::rust_stored::guarded(|| {
            updated(xs, &docs, 2 * rtemis_r8::validate::MAX_NESTING)
        });
        Ok(match read {
            None => list!(message = "its fields do not read"),
            Some(Ok(None)) => list!(value = x),
            Some(Ok(Some(v))) => list!(value = v),
            Some(Err(Refused::Deep(message))) => list!(message = message, code = "too_deep"),
            Some(Err(Refused::Other(message))) => list!(message = message),
            Some(Err(Refused::Invalid(ty, errors))) => {
                list!(errors = crate::values::errors_list(&errors), of = ty)
            }
        })
    })
}

/// The SHA-256 of a raw vector.
///
/// @param x Raw.
/// @return Raw: 32 bytes.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_sha256(x: Raw) -> Raw {
    Raw::from_bytes(&rtemis_r8::canonical::sha256(x.as_slice()))
}

extendr_module! {
    mod value;
    fn r8_sha256;
    fn r8_value_other;
    fn r8_updated_value;
}

/// The identity of a declaration that is not the registered one, for the
/// tests.
#[cfg(feature = "oracle")]
pub(crate) fn other_identity(
    q: &QualifiedName,
    variant: Option<&str>,
    names: &[&str],
    class: &[&str],
    declaration: &str,
) -> SEXP {
    new_identity(q, variant, names, class, declaration, &String::new, false)
}

/// A character vector, unprotected.
///
/// # Safety
///
/// Called on R's main thread.
pub(crate) unsafe fn strings(items: &[&str]) -> SEXP {
    unsafe {
        let out = Rf_protect(Rf_allocVector(SEXPTYPE::STRSXP, items.len() as isize));
        for (i, s) in items.iter().enumerate() {
            SET_STRING_ELT(out, i as isize, mkchar(s));
        }
        Rf_unprotect(1);
        out
    }
}
