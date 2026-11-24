//! The check of a typed generic's arguments and result at each call.
//!
//! A generic or trait method that declares the types of its parameters or
//! its return type has a signature, built from the registry at its first
//! call and again after the registry changes. The signature belongs to the
//! method's key, the R symbol `Trait|method` that the generic's body holds;
//! symbols live for the session, so their addresses identify methods.
//!
//! The C routines of `view.c` force the arguments, in C frames, and call the
//! `extern "C"` tests here, which read R objects and neither allocate R
//! memory nor raise an R error, so no borrow of the signatures is held where
//! R can jump. A test passes a value that is visibly one of its type: an
//! unclassed scalar within its bounds, a value whose `r8_type` is the
//! declared type or one that the validator has admitted under the declared
//! `subtype()` or `dyn()`, or an object of a declared S3 class. Every other
//! value goes to the validator (`r8_check_args`, `r8_check_result`).
//! spec: R8/class-system#checks-at-a-call

use std::cell::RefCell;
use std::collections::HashMap;
use std::ffi::{CStr, CString};
use std::hash::{BuildHasherDefault, Hasher};
use std::os::raw::c_int;
use std::rc::Rc;
use std::sync::atomic::Ordering;

use extendr_api::prelude::*;
use extendr_api::{Error, Result};
use extendr_ffi::{
    INTEGER, LENGTH, LOGICAL, PRINTNAME, R_CHAR, R_NaString, REAL, Rf_install, SEXP, SEXPTYPE,
    STRING_ELT, VECTOR_ELT,
};
use rtemis_r8::ir::{DoubleType, FieldType, IntegerType, ReturnType, TypeRef};
use rtemis_r8::name::QualifiedName;
use rtemis_r8::problem::Segment;
use rtemis_r8::rules::{DocumentSet, TypeLookup};
use rtemis_r8::validate::{ValueError, validate_field_access};

use crate::construct::{GENERATION, normalize, problems, with_cache};
use crate::rview::*;

/// The outcome of a test: the value passes as it is, the validator decides,
/// or the value passes as an integer or as a double. `view.c` holds the same
/// numbers.
const PASS: c_int = 0;
const CHECK: c_int = 1;
const TO_INTEGER: c_int = 2;
const TO_DOUBLE: c_int = 3;
/// A result whose type is `Self`, which `view.c` tests.
const SELF: c_int = 4;
/// No signature: the trait is not registered, or the signature changed.
const ABSENT: c_int = -1;

/// The test of one field type.
enum Test {
    Logical,
    Integer(IntegerType),
    Double(DoubleType),
    /// A string, one of `values` when they are declared.
    Character(Option<Box<[Box<str>]>>),
    /// A value of exactly the named type.
    Type(Box<str>),
    /// A value of a type admitted before, under `subtype()` or `dyn()`.
    Admitted(RefCell<Vec<Box<str>>>),
    /// An object whose class attribute holds the class.
    Class(Box<str>),
    Option(Box<Test>),
    /// Every value goes to the validator.
    Check,
}

impl Test {
    fn of(ty: &FieldType) -> Test {
        match ty {
            FieldType::Logical => Test::Logical,
            FieldType::Integer(t) => Test::Integer(t.clone()),
            FieldType::Double(t) => Test::Double(t.clone()),
            FieldType::Character(t) if t.pattern.is_none() => Test::Character(t.values.clone()),
            FieldType::Named { name } => Test::Type(name.to_string().into()),
            FieldType::Subtype { .. } | FieldType::Dyn { .. } => {
                Test::Admitted(RefCell::new(Vec::new()))
            }
            FieldType::HostClass { host, class, .. } if &**host == "r" => {
                Test::Class(class.clone())
            }
            FieldType::Option { inner } => Test::Option(Box::new(Test::of(inner))),
            _ => Test::Check,
        }
    }

    fn run(&self, x: SEXP) -> c_int {
        match self {
            Test::Option(_) if is_null(x) => PASS,
            Test::Option(t) => t.run(x),
            Test::Logical => {
                let ok = is_scalar(x, SEXPTYPE::LGLSXP) && unsafe { *LOGICAL(x) } != i32::MIN;
                outcome(ok, PASS)
            }
            Test::Integer(t) => {
                if is_scalar(x, SEXPTYPE::INTSXP) {
                    let v = unsafe { *INTEGER(x) };
                    outcome(t.admits(i64::from(v)), PASS)
                } else if is_scalar(x, SEXPTYPE::REALSXP) {
                    let v = unsafe { *REAL(x) };
                    let whole = v.fract() == 0.0 && v.abs() <= f64::from(i32::MAX);
                    outcome(whole && t.admits(v as i64), TO_INTEGER)
                } else {
                    CHECK
                }
            }
            Test::Double(t) => {
                if is_scalar(x, SEXPTYPE::REALSXP) {
                    let v = unsafe { *REAL(x) };
                    // A negative zero is stored as zero.
                    let negative_zero = v == 0.0 && v.is_sign_negative();
                    outcome(
                        !rtemis_r8::access::is_na_real(v) && !negative_zero && t.admits(v),
                        PASS,
                    )
                } else if is_scalar(x, SEXPTYPE::INTSXP) {
                    let v = unsafe { *INTEGER(x) };
                    outcome(v != i32::MIN && t.admits(f64::from(v)), TO_DOUBLE)
                } else {
                    CHECK
                }
            }
            Test::Character(values) => {
                if !is_scalar(x, SEXPTYPE::STRSXP) {
                    return CHECK;
                }
                let c = unsafe { STRING_ELT(x, 0) };
                if c == unsafe { R_NaString } {
                    return CHECK;
                }
                let bytes = char_bytes(c);
                let ok = match values {
                    // An ASCII string is UTF-8 in every encoding R marks.
                    None => bytes.is_ascii(),
                    Some(v) => v.iter().any(|s| s.is_ascii() && s.as_bytes() == bytes),
                };
                outcome(ok, PASS)
            }
            Test::Type(name) => outcome(type_of(x) == Some(name.as_bytes()), PASS),
            Test::Admitted(names) => {
                let ok = type_of(x).is_some_and(|t| {
                    names
                        .try_borrow()
                        .is_ok_and(|n| n.iter().any(|a| a.as_bytes() == t))
                });
                outcome(ok, PASS)
            }
            Test::Class(class) => outcome(inherits(x, class), PASS),
            Test::Check => CHECK,
        }
    }

    /// Records the type of `x`, which the validator admitted.
    fn admit(&self, x: SEXP) {
        match self {
            Test::Option(t) => t.admit(x),
            Test::Admitted(names) => {
                if let Some(t) = type_of(x).and_then(|t| std::str::from_utf8(t).ok()) {
                    let mut names = names.borrow_mut();
                    if !names.iter().any(|a| &**a == t) {
                        names.push(t.into());
                    }
                }
            }
            _ => {}
        }
    }
}

fn outcome(ok: bool, pass: c_int) -> c_int {
    if ok { pass } else { CHECK }
}

/// A vector of `kind` of one element, without attributes, held in memory.
fn is_scalar(x: SEXP, kind: SEXPTYPE) -> bool {
    sexptype(x) == kind
        && unsafe { extendr_ffi::ALTREP(x) } == 0
        && xlen(x) == 1
        && attrib_count(x) == 0
}

/// The bytes of a CHARSXP.
fn char_bytes<'a>(c: SEXP) -> &'a [u8] {
    unsafe { std::slice::from_raw_parts(R_CHAR(c).cast::<u8>(), LENGTH(c) as usize) }
}

/// The type name of the value `x`, of the registered declaration.
fn type_of<'a>(x: SEXP) -> Option<&'a [u8]> {
    crate::value::usable_type(x).map(char_bytes)
}

/// A typed parameter: its position among the forced parameters, its name,
/// its type, and its test.
struct Slot {
    at: usize,
    name: Box<str>,
    ty: FieldType,
    test: Test,
}

/// What a method returns.
enum Returned {
    /// Any value.
    Any,
    /// A value of the type of the first dispatched argument.
    SelfType,
    /// A value of the type.
    Type(FieldType, Test),
}

/// The signature of a generic or trait method.
struct Signature {
    generation: u64,
    /// The symbols of the typed and the dispatched parameters, in the order
    /// of the formals.
    forced: Vec<SEXP>,
    /// The positions among `forced` of the dispatched parameters.
    dispatched: Vec<c_int>,
    slots: Vec<Slot>,
    returned: Returned,
    /// The return types that methods declare, by the key of their tuple.
    narrowed: Vec<(Box<str>, FieldType, Test)>,
}

/// A hash of an address: one multiplication.
#[derive(Default)]
struct AddressHasher(u64);

impl Hasher for AddressHasher {
    fn write(&mut self, bytes: &[u8]) {
        for &b in bytes {
            self.write_usize(b as usize);
        }
    }
    fn write_usize(&mut self, x: usize) {
        self.0 = (self.0 ^ (x as u64)).wrapping_mul(0x9E37_79B9_7F4A_7C15);
    }
    fn finish(&self) -> u64 {
        self.0 ^ (self.0 >> 32)
    }
}

type Signatures = HashMap<usize, Rc<Signature>, BuildHasherDefault<AddressHasher>>;

thread_local! {
    // R calls these functions on one thread.
    static SIGNATURES: RefCell<Signatures> = RefCell::new(HashMap::default());
}

/// The parts of a key, `Trait|method`.
fn key_parts<'a>(key: SEXP) -> Option<(QualifiedName, &'a str)> {
    let text = unsafe { CStr::from_ptr(R_CHAR(PRINTNAME(key))) }
        .to_str()
        .ok()?;
    let (tr, method) = text.split_once('|')?;
    Some((QualifiedName::parse(tr).ok()?, method))
}

/// The key of an impl's tuple of positions, as R's `tuple_key()` writes it.
fn tuple_key<'a>(positions: impl Iterator<Item = &'a TypeRef>) -> String {
    positions
        .map(|p| match p {
            TypeRef::HostClass { class, .. } => format!("\"{class}\""),
            p => p.to_string(),
        })
        .collect::<Vec<_>>()
        .join(",")
}

/// The return types that the impls of the trait `q` declare for `method`,
/// with the key of each impl's tuple.
fn narrowed<'a>(
    docs: &'a DocumentSet,
    q: &QualifiedName,
    method: &str,
) -> Vec<(String, &'a FieldType)> {
    rtemis_r8::ir::dispatch::impls_by_namespace(docs, q)
        .into_iter()
        .filter_map(|(_, imp)| {
            let own = imp.methods.iter().find(|i| i.name().as_str() == method)?;
            let positions = std::iter::once(&imp.target).chain(&imp.type_args);
            Some((tuple_key(positions), own.return_type()?))
        })
        .collect()
}

fn build(key: SEXP, generation: u64) -> Option<Signature> {
    let (q, method) = key_parts(key)?;
    let docs = crate::declare::registry().snapshot();
    let tr = docs.trait_of(&q)?;
    let m = tr.methods.iter().find(|m| m.name.as_str() == method)?;
    let dispatched = m.dispatched();
    let mut forced = Vec::new();
    let mut at = Vec::new();
    let mut slots = Vec::new();
    for (i, p) in m.params.iter().enumerate() {
        let is_dispatched = dispatched.contains(&i);
        if !is_dispatched && p.ty().is_none() {
            continue;
        }
        if is_dispatched {
            at.push(forced.len() as c_int);
        }
        if let Some(ty) = p.ty() {
            slots.push(Slot {
                at: forced.len(),
                name: p.name().into(),
                ty: ty.clone(),
                test: Test::of(ty),
            });
        }
        // The generic's formals installed the symbol.
        let name = CString::new(p.name()).ok()?;
        forced.push(unsafe { Rf_install(name.as_ptr()) });
    }
    let returned = match &m.return_type {
        None => Returned::Any,
        Some(ReturnType::SelfType(_)) => Returned::SelfType,
        Some(ReturnType::Type(t)) => Returned::Type((**t).clone(), Test::of(t)),
    };
    let narrowed = narrowed(&docs, &q, method)
        .into_iter()
        .map(|(k, t)| (k.into(), t.clone(), Test::of(t)))
        .collect();
    Some(Signature {
        generation,
        forced,
        dispatched: at,
        slots,
        returned,
        narrowed,
    })
}

/// The signature of `key` for the registry as it stands. A signature is
/// built outside the borrow of the table.
fn signature(key: SEXP) -> Option<Rc<Signature>> {
    let generation = GENERATION.load(Ordering::Relaxed);
    let address = key as usize;
    let held = SIGNATURES.with(|s| {
        let all = s.try_borrow().ok()?;
        all.get(&address)
            .filter(|sig| sig.generation == generation)
            .map(Rc::clone)
    });
    if held.is_some() {
        return held;
    }
    let built = build(key, generation).map(Rc::new);
    SIGNATURES.with(|s| {
        if let Ok(mut all) = s.try_borrow_mut() {
            match &built {
                Some(sig) => {
                    all.insert(address, Rc::clone(sig));
                }
                None => {
                    all.remove(&address);
                }
            }
        }
    });
    built
}

/// Writes the symbols of the parameters that a call of the method `key`
/// forces, in the order of the formals, to `out`, which holds `capacity`;
/// their number, which may exceed `capacity`, or -1 without a signature.
///
/// # Safety
///
/// `key` is a symbol and `out` points at `capacity` slots.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn r8_signature_forced(key: SEXP, out: *mut SEXP, capacity: c_int) -> c_int {
    let Some(sig) = signature(key) else {
        return ABSENT;
    };
    for (i, s) in sig.forced.iter().take(capacity.max(0) as usize).enumerate() {
        unsafe { *out.add(i) = *s };
    }
    sig.forced.len() as c_int
}

/// Tests the values of the parameters `symbols`, which
/// `r8_signature_forced()` wrote, against their types: the outcome of each in
/// `outcomes`, and the positions of the dispatched ones in `dispatched`, which
/// holds three, with their number in `n_dispatched`. Returns `PASS` when
/// every value passes as it is, `CHECK` when the validator decides some, the
/// largest conversion otherwise, and `ABSENT` when the signature is not the
/// one whose symbols were written, as after a declaration that forcing an
/// argument ran.
///
/// # Safety
///
/// `key` is a symbol; `symbols`, `values`, and `outcomes` point at `n` slots,
/// and `dispatched` at three.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn r8_signature_test(
    key: SEXP,
    symbols: *const SEXP,
    values: *const SEXP,
    n: c_int,
    outcomes: *mut c_int,
    dispatched: *mut c_int,
    n_dispatched: *mut c_int,
) -> c_int {
    let Some(sig) = signature(key) else {
        return ABSENT;
    };
    let n = n.max(0) as usize;
    let given = unsafe { std::slice::from_raw_parts(symbols, n) };
    if sig.forced.as_slice() != given {
        return ABSENT;
    }
    unsafe {
        for i in 0..n {
            *outcomes.add(i) = PASS;
        }
        for (k, at) in sig.dispatched.iter().take(3).enumerate() {
            *dispatched.add(k) = *at;
        }
        *n_dispatched = sig.dispatched.len().min(3) as c_int;
    }
    let mut check = false;
    let mut convert = PASS;
    for slot in &sig.slots {
        let o = slot.test.run(unsafe { *values.add(slot.at) });
        unsafe { *outcomes.add(slot.at) = o };
        check |= o == CHECK;
        convert = convert.max(o);
    }
    if check { CHECK } else { convert }
}

/// Tests a result against the return type of the impl whose tuple key is
/// `tuple`, a CHARSXP, or else of the method `key`: an outcome, or `SELF` for
/// a method that returns `Self`.
///
/// # Safety
///
/// `key` is a symbol and `tuple` a CHARSXP.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn r8_signature_result(key: SEXP, tuple: SEXP, value: SEXP) -> c_int {
    let Some(sig) = signature(key) else {
        return ABSENT;
    };
    if !sig.narrowed.is_empty() {
        let t = char_bytes(tuple);
        if let Some((_, _, test)) = sig.narrowed.iter().find(|(k, _, _)| k.as_bytes() == t) {
            return test.run(value);
        }
    }
    match &sig.returned {
        Returned::Any => PASS,
        Returned::SelfType => SELF,
        Returned::Type(_, test) => test.run(value),
    }
}

fn no_signature() -> Error {
    Error::Other("the method's trait is not registered".to_string())
}

/// The errors of `x` against `ty`, at the parameter `name` or at the value
/// itself.
fn errors_of(x: SEXP, ty: &FieldType, docs: &DocumentSet, name: Option<&str>) -> Vec<ValueError> {
    let mut errors = with_cache(|cache| validate_field_access(RView::Whole(x), ty, docs, cache));
    if is_null(x) {
        // An absent value, which the message names.
        for e in &mut errors {
            e.found = Some(rtemis_r8::rules::Shown::Absent);
        }
    }
    if let Some(name) = name {
        for e in &mut errors {
            e.path = format!("/{name}{}", e.path);
            e.segments.insert(0, Segment::Field(name.to_string()));
        }
    }
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
    errors
}

/// Validates the arguments of a call that the tests did not pass.
///
/// @param key The method's key, a symbol.
/// @param values List: The values of the forced parameters.
/// @return `NULL` when every value is in its stored form; a list, per
///   parameter the value in its stored form or `NULL`; or the problems, of
///   class `r8_problems`.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_check_args(key: Robj, values: Robj) -> Result<Robj> {
    let sig = signature(unsafe { key.get() }).ok_or_else(no_signature)?;
    let list = unsafe { values.get() };
    let docs = crate::declare::registry().snapshot();
    let checked: Vec<(&Slot, SEXP)> = sig
        .slots
        .iter()
        .map(|s| (s, unsafe { VECTOR_ELT(list, s.at as isize) }))
        .filter(|(s, x)| s.test.run(*x) == CHECK)
        .collect();
    let errors: Vec<ValueError> = checked
        .iter()
        .flat_map(|(s, x)| errors_of(*x, &s.ty, &docs, Some(&s.name)))
        .collect();
    if !errors.is_empty() {
        return problems(&errors);
    }
    let mut out: Vec<Robj> = vec![Robj::from(()); sig.forced.len()];
    let mut changed = false;
    for (s, x) in checked {
        s.test.admit(x);
        if let Some(r) = normalize(x, &s.ty, &*docs)? {
            out[s.at] = r;
            changed = true;
        }
    }
    Ok(if changed {
        List::from_values(out).into()
    } else {
        ().into()
    })
}

/// Validates a result that the test did not pass.
///
/// @param key The method's key, a symbol.
/// @param tuple Character: The tuple key of the impl that returned it.
/// @param value The result.
/// @return `NULL` when the result is in its stored form; a list of the
///   result in its stored form; or the problems, of class `r8_problems`.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_check_result(key: Robj, tuple: &str, value: Robj) -> Result<Robj> {
    let sig = signature(unsafe { key.get() }).ok_or_else(no_signature)?;
    let own = sig.narrowed.iter().find(|(k, _, _)| &**k == tuple);
    let (ty, test) = match (own, &sig.returned) {
        (Some((_, t, test)), _) | (None, Returned::Type(t, test)) => (t, test),
        _ => return Ok(().into()),
    };
    let x = unsafe { value.get() };
    let docs = crate::declare::registry().snapshot();
    let errors = errors_of(x, ty, &docs, None);
    if !errors.is_empty() {
        return problems(&errors);
    }
    test.admit(x);
    Ok(match normalize(x, ty, &*docs)? {
        Some(r) => List::from_values([r]).into(),
        None => ().into(),
    })
}

/// The stored form of a constant default of a parameter, which its
/// declaration has checked.
///
/// @param x The default.
/// @param type The parameter's type, a field type or an R8 type object.
/// @return The value.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_stored_form(x: Robj, r#type: Robj) -> Result<Robj> {
    let ty = crate::fieldtype::field_type(unsafe { r#type.get() })
        .map_err(|e| Error::Other(e.to_string()))?;
    let docs = crate::declare::registry().snapshot();
    Ok(normalize(unsafe { x.get() }, &ty, &*docs)?.unwrap_or(x))
}

/// The notation of a field type (`rtemis_r8::ir::notation`).
///
/// @param type List: The field type in its IR form.
/// @return Character.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_type_notation(r#type: Robj) -> Result<String> {
    let ty: FieldType = crate::irde::from_r(unsafe { r#type.get() })
        .map_err(|e| Error::Other(format!("the type does not read: {e}")))?;
    Ok(rtemis_r8::ir::notation::notation(&ty))
}

/// The return types that the methods of a generic or trait method declare.
///
/// @param trait_name Character: The qualified trait name.
/// @param method Character: The method.
/// @return List: `tuple`, the tuple keys, and `notation`, the notations.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_method_returns(trait_name: &str, method: &str) -> Result<List> {
    let q = QualifiedName::parse(trait_name).map_err(|e| Error::Other(e.to_string()))?;
    let docs = crate::declare::registry().snapshot();
    let (tuple, ty): (Vec<String>, Vec<String>) = narrowed(&docs, &q, method)
        .into_iter()
        .map(|(k, t)| (k, rtemis_r8::ir::notation::notation(t)))
        .unzip();
    Ok(list!(tuple = tuple, notation = ty))
}

extendr_module! {
    mod signature;
    fn r8_type_notation;
    fn r8_method_returns;
    fn r8_check_args;
    fn r8_check_result;
    fn r8_stored_form;
}
