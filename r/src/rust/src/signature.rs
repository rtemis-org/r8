//! The check of a typed generic's or function's arguments and result at each
//! call.
//!
//! A generic or trait method that declares the types of its parameters or
//! its return type, and a function of `new_function()`, have a signature,
//! built from the registry at their first call and again after the registry
//! changes. The signature belongs to the key, the R symbol `Trait|method` that
//! the generic's body holds or the qualified name `ns::f` of the function;
//! symbols live for the session, so their addresses identify methods.
//!
//! The C routines of `view.c` force the arguments, in C frames, and call the
//! `extern "C"` tests here, which read R objects and neither allocate R
//! memory nor raise an R error, so no borrow of the signatures is held where
//! R can jump. A test passes a value that is visibly one of its type: an
//! unclassed scalar within its bounds, a vector of logicals, integers, or
//! doubles without attributes whose elements are all values of its element
//! type (`VectorTest`), a value whose `r8_type` is the
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
use rtemis_r8::ir::{
    DoubleType, FieldDef, FieldType, IntegerType, ParamDef, ReturnType, RuleDef, TypeRef,
};
use rtemis_r8::name::QualifiedName;
use rtemis_r8::problem::Segment;
use rtemis_r8::rules::{DocumentSet, TypeLookup};
use rtemis_r8::validate::{ValueError, validate_field_access};

use crate::construct::{GENERATION, normalize, problems, with_cache};

#[allow(improper_ctypes)]
unsafe extern "C" {
    fn Rf_ScalarReal(x: f64) -> SEXP;
}
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
    /// A vector of logicals, integers, or doubles.
    Vector(VectorTest),
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
            FieldType::Vec {
                element,
                min_length,
                max_length,
                unique,
            } => {
                let Some(element) = Element::of(element) else {
                    return Test::Check;
                };
                Test::Vector(VectorTest {
                    element,
                    min_length: *min_length,
                    max_length: *max_length,
                    unique: *unique,
                })
            }
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
            Test::Vector(v) => outcome(v.admits(x), PASS),
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

/// The element type of a [`VectorTest`], with the scan of its elements.
enum Element {
    Logical,
    /// Integers from `lo` to `hi`, both included, within the integer domain.
    IntegerRange {
        lo: i32,
        hi: i32,
    },
    /// Doubles within bounds, each included or excluded, finite unless
    /// `infinite`; an absent bound is an infinity, included.
    DoubleRange {
        lo: f64,
        hi: f64,
        lo_excluded: bool,
        hi_excluded: bool,
        infinite: bool,
    },
    /// Integers of a set of admitted values, read element by element.
    Integer(IntegerType),
    /// Doubles of a set of admitted values, or with `NaN` admitted, read
    /// element by element.
    Double(DoubleType),
}

impl Element {
    fn of(ty: &FieldType) -> Option<Element> {
        Some(match ty {
            FieldType::Logical => Element::Logical,
            FieldType::Integer(t) if t.values.is_none() => {
                // The domain is -2147483647 to 2147483647; an excluded bound
                // is the included one next to it.
                let lo = t.lower.map_or(-i64::from(i32::MAX), |b| {
                    i64::from(b.value) + i64::from(b.exclusive)
                });
                let hi = t.upper.map_or(i64::from(i32::MAX), |b| {
                    i64::from(b.value) - i64::from(b.exclusive)
                });
                let lo = lo.max(-i64::from(i32::MAX));
                let hi = hi.min(i64::from(i32::MAX));
                if lo > hi {
                    // No integer is admitted.
                    Element::IntegerRange { lo: 1, hi: 0 }
                } else {
                    Element::IntegerRange {
                        lo: lo as i32,
                        hi: hi as i32,
                    }
                }
            }
            FieldType::Integer(t) => Element::Integer(t.clone()),
            FieldType::Double(t) if t.values.is_none() && !t.nan => Element::DoubleRange {
                lo: t.lower.map_or(f64::NEG_INFINITY, |b| b.value),
                hi: t.upper.map_or(f64::INFINITY, |b| b.value),
                lo_excluded: t.lower.is_some_and(|b| b.exclusive),
                hi_excluded: t.upper.is_some_and(|b| b.exclusive),
                infinite: t.infinite,
            },
            FieldType::Double(t) => Element::Double(t.clone()),
            _ => return None,
        })
    }
}

/// The test of `vec(T)` of a logical, integer, or double `T`: a vector of
/// `T`'s type without attributes, whose elements R holds as one array, of the
/// declared lengths, each element a value of `T`. A vector of distinct
/// elements of more than one element, a vector that needs a conversion (an
/// integer vector for doubles, or a negative zero), and an ALTREP vector
/// without an array go to the validator.
struct VectorTest {
    element: Element,
    min_length: Option<u64>,
    max_length: Option<u64>,
    unique: bool,
}

/// The bits of a negative zero.
const NEGATIVE_ZERO: u64 = 0x8000_0000_0000_0000;

/// The elements a scan reads between its tests of the answer so far, so that
/// the loop within a block has no branch.
const BLOCK: usize = 1024;

impl VectorTest {
    fn admits(&self, x: SEXP) -> bool {
        let kind = match self.element {
            Element::Logical => SEXPTYPE::LGLSXP,
            Element::IntegerRange { .. } | Element::Integer(_) => SEXPTYPE::INTSXP,
            Element::DoubleRange { .. } | Element::Double(_) => SEXPTYPE::REALSXP,
        };
        if sexptype(x) != kind || attrib_count(x) != 0 {
            return false;
        }
        let n = xlen(x);
        let len = n as u64;
        if self.min_length.is_some_and(|m| len < m)
            || self.max_length.is_some_and(|m| len > m)
            || (self.unique && n > 1)
        {
            return false;
        }
        // An empty vector has no array to read.
        if n == 0 {
            return true;
        }
        match &self.element {
            Element::Logical => data::<i32>(x).is_some_and(|s| all_in(s, |v| v != i32::MIN)),
            &Element::IntegerRange { lo, hi } => {
                data::<i32>(x).is_some_and(|s| all_in(s, |v| (v >= lo) & (v <= hi)))
            }
            &Element::DoubleRange {
                lo,
                hi,
                lo_excluded,
                hi_excluded,
                infinite,
            } => data::<f64>(x).is_some_and(|s| match (lo_excluded, hi_excluded, infinite) {
                (false, false, false) => doubles_in::<false, false, false>(s, lo, hi),
                (false, false, true) => doubles_in::<false, false, true>(s, lo, hi),
                (false, true, false) => doubles_in::<false, true, false>(s, lo, hi),
                (false, true, true) => doubles_in::<false, true, true>(s, lo, hi),
                (true, false, false) => doubles_in::<true, false, false>(s, lo, hi),
                (true, false, true) => doubles_in::<true, false, true>(s, lo, hi),
                (true, true, false) => doubles_in::<true, true, false>(s, lo, hi),
                (true, true, true) => doubles_in::<true, true, true>(s, lo, hi),
            }),
            Element::Integer(t) => {
                data::<i32>(x).is_some_and(|s| s.iter().all(|&v| t.admits(i64::from(v))))
            }
            Element::Double(t) => data::<f64>(x).is_some_and(|s| {
                s.iter().all(|&v| {
                    !rtemis_r8::access::is_na_real(v) && v.to_bits() != NEGATIVE_ZERO && t.admits(v)
                })
            }),
        }
    }
}

/// Whether `ok` holds for every element of `s`: a block at a time, each
/// block without a branch, so that the compiler can read it in vector
/// registers.
#[inline(always)]
fn all_in<T: Copy>(s: &[T], ok: impl Fn(T) -> bool) -> bool {
    s.chunks(BLOCK)
        .all(|b| b.iter().fold(true, |all, &v| all & ok(v)))
}

/// Whether every element of `s` lies within `lo` and `hi`, each excluded by
/// its flag, is finite unless `INFINITE`, and is no negative zero. A `NaN`,
/// `NA` among them, fails every comparison.
fn doubles_in<const LO_EXCLUDED: bool, const HI_EXCLUDED: bool, const INFINITE: bool>(
    s: &[f64],
    lo: f64,
    hi: f64,
) -> bool {
    all_in(s, |v| {
        let above = if LO_EXCLUDED { v > lo } else { v >= lo };
        let below = if HI_EXCLUDED { v < hi } else { v <= hi };
        above & below & (INFINITE | v.is_finite()) & (v.to_bits() != NEGATIVE_ZERO)
    })
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
    /// The rules of a function over its typed parameters, and those
    /// parameters as fields.
    rules: Vec<RuleDef>,
    scope: Vec<FieldDef>,
    /// The rules compiled, with the registry they were compiled against, and a
    /// list of the typed parameters, preserved, which holds a call's values while
    /// the rules read them.
    compiled: Vec<std::sync::Arc<rtemis_r8::rules::CompiledRule>>,
    docs: Option<std::sync::Arc<DocumentSet>>,
    held: SEXP,
}

impl Drop for Signature {
    fn drop(&mut self) {
        if !is_null(self.held) {
            unsafe { extendr_ffi::R_ReleaseObject(self.held) };
        }
    }
}

impl Signature {
    /// Whether the values `values` of the forced parameters satisfy every rule,
    /// read in place; `false` also where a rule has no answer, which the validator
    /// then gives. Allocates no R memory and raises no R error.
    fn rules_hold(&self, values: &[SEXP]) -> bool {
        let Some(docs) = &self.docs else { return false };
        if self.compiled.len() != self.rules.len() {
            return false;
        }
        for (k, s) in self.slots.iter().enumerate() {
            unsafe { extendr_ffi::SET_VECTOR_ELT(self.held, k as isize, values[s.at]) };
        }
        let view = RView::Draft(self.held, "");
        let holds = self.compiled.iter().all(|r| {
            matches!(
                r.check_access(view, &self.scope, &**docs),
                Ok(rtemis_r8::rules::Outcome::Holds)
            )
        });
        for k in 0..self.slots.len() {
            unsafe { extendr_ffi::SET_VECTOR_ELT(self.held, k as isize, extendr_ffi::R_NilValue) };
        }
        holds
    }
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
pub(crate) fn tuple_key<'a>(positions: impl Iterator<Item = &'a TypeRef>) -> String {
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

/// The qualified name of a function's key, `ns::f`.
fn function_key(key: SEXP) -> Option<QualifiedName> {
    let text = unsafe { CStr::from_ptr(R_CHAR(PRINTNAME(key))) }
        .to_str()
        .ok()?;
    if text.contains('|') {
        return None;
    }
    QualifiedName::parse(text).ok()
}

fn build(key: SEXP, generation: u64) -> Option<Signature> {
    let docs = crate::declare::registry().snapshot();
    if let Some(q) = function_key(key) {
        let f = docs.function_of(&q)?;
        let mut sig = build_params(&f.params, &[], &f.return_type, generation)?;
        with_rules(&mut sig, &f.rules, f.rule_scope(), &docs);
        return Some(sig);
    }
    let (q, method) = key_parts(key)?;
    let tr = docs.trait_of(&q)?;
    let m = tr.methods.iter().find(|m| m.name.as_str() == method)?;
    let mut sig = build_params(&m.params, &m.dispatched(), &m.return_type, generation)?;
    with_rules(&mut sig, &m.rules, m.rule_scope(), &docs);
    sig.narrowed = narrowed(&docs, &q, method)
        .into_iter()
        .map(|(k, t)| (k.into(), t.clone(), Test::of(t)))
        .collect();
    Some(sig)
}

/// Gives a signature the rules over its typed parameters, compiled against `docs`.
fn with_rules(
    sig: &mut Signature,
    rules: &[RuleDef],
    scope: Vec<FieldDef>,
    docs: &std::sync::Arc<DocumentSet>,
) {
    if rules.is_empty() {
        return;
    }
    sig.compiled = rules
        .iter()
        .filter_map(|r| {
            rtemis_r8::rules::CompiledRule::compile(r, &scope, &**docs)
                .ok()
                .map(std::sync::Arc::new)
        })
        .collect();
    sig.rules = rules.to_vec();
    sig.scope = scope;
    sig.held = held_list(&sig.slots);
    sig.docs = Some(docs.clone());
}

/// The signature of parameters, of which those at `dispatched` dispatch, and of
/// a return type.
fn build_params(
    params: &[ParamDef],
    dispatched: &[usize],
    return_type: &Option<ReturnType>,
    generation: u64,
) -> Option<Signature> {
    let mut forced = Vec::new();
    let mut at = Vec::new();
    let mut slots = Vec::new();
    for (i, p) in params.iter().enumerate() {
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
    let returned = match return_type {
        None => Returned::Any,
        Some(ReturnType::SelfType(_)) => Returned::SelfType,
        Some(ReturnType::Type(t)) => Returned::Type((**t).clone(), Test::of(t)),
    };
    Some(Signature {
        generation,
        forced,
        dispatched: at,
        slots,
        returned,
        narrowed: Vec::new(),
        rules: Vec::new(),
        scope: Vec::new(),
        compiled: Vec::new(),
        docs: None,
        held: unsafe { extendr_ffi::R_NilValue },
    })
}

/// A named list of the typed parameters, preserved for the signature's life.
fn held_list(slots: &[Slot]) -> SEXP {
    let names: Vec<&str> = slots.iter().map(|s| &*s.name).collect();
    let mut list: Robj = List::from_values(names.iter().map(|_| Robj::from(()))).into();
    let _ = list.set_names(names);
    let x = unsafe { list.get() };
    unsafe { extendr_ffi::R_PreserveObject(x) };
    x
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
    unwound(ABSENT, || {
        let Some(sig) = signature(key) else {
            return ABSENT;
        };
        for (i, s) in sig.forced.iter().take(capacity.max(0) as usize).enumerate() {
            unsafe { *out.add(i) = *s };
        }
        sig.forced.len() as c_int
    })
}

/// The value of `f`, or `fallback` when `f` panics: a panic does not unwind
/// across the C routines of a call, and the call takes the validator's path,
/// whose functions report it as an error.
fn unwound(fallback: c_int, f: impl FnOnce() -> c_int) -> c_int {
    std::panic::catch_unwind(std::panic::AssertUnwindSafe(f)).unwrap_or(fallback)
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
    unwound(CHECK, || unsafe {
        signature_test(key, symbols, values, n, outcomes, dispatched, n_dispatched)
    })
}

/// The body of [`r8_signature_test()`].
unsafe fn signature_test(
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
    // The rules of a function read the values in place; one that fails, or has
    // no answer, goes to the validator, which reports it.
    if !check && !sig.rules.is_empty() {
        let given = unsafe { std::slice::from_raw_parts(values, n) };
        check = !sig.rules_hold(given);
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
    unwound(CHECK, || {
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
    })
}

fn no_signature() -> Error {
    Error::Other("the method's trait is not registered".to_string())
}

/// The errors of `x` against `ty`, at the parameter `name` or at the value
/// itself.
pub(crate) fn errors_of(
    x: SEXP,
    ty: &FieldType,
    docs: &DocumentSet,
    name: Option<&str>,
) -> Vec<ValueError> {
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
    crate::interrupt::interruptible(unsafe { values.get() }, || {
        let sig = signature(unsafe { key.get() }).ok_or_else(no_signature)?;
        let docs = crate::declare::registry().snapshot();
        match validated(&sig, unsafe { values.get() }, &docs)? {
            Err(errors) => problems(&errors),
            Ok(out) if out.iter().all(Option::is_none) => Ok(().into()),
            Ok(out) => Ok(List::from_values(
                out.into_iter().map(|v| v.unwrap_or_else(|| Robj::from(()))),
            )
            .into()),
        }
    })
}

/// The validation of the values of the forced parameters, `list`, whose tests
/// did not pass: the errors, or for each forced parameter its value in its
/// stored form where that differs from the value given.
fn validated(
    sig: &Signature,
    list: SEXP,
    docs: &DocumentSet,
) -> Result<std::result::Result<Vec<Option<Robj>>, Vec<ValueError>>> {
    let checked: Vec<(&Slot, SEXP)> = sig
        .slots
        .iter()
        .map(|s| (s, unsafe { VECTOR_ELT(list, s.at as isize) }))
        .filter(|(s, x)| s.test.run(*x) == CHECK)
        .collect();
    let errors: Vec<ValueError> = checked
        .iter()
        .flat_map(|(s, x)| errors_of(*x, &s.ty, docs, Some(&s.name)))
        .collect();
    if !errors.is_empty() {
        return Ok(Err(errors));
    }
    let mut out: Vec<Option<Robj>> = vec![None; sig.forced.len()];
    for (s, x) in checked {
        s.test.admit(x);
        out[s.at] = normalize(x, &s.ty, docs)?;
    }
    if !sig.rules.is_empty() {
        let values: Vec<SEXP> = sig
            .slots
            .iter()
            .map(|s| match &out[s.at] {
                Some(r) => unsafe { r.get() },
                None => unsafe { VECTOR_ELT(list, s.at as isize) },
            })
            .collect();
        let names: Vec<&str> = sig.slots.iter().map(|s| &*s.name).collect();
        let errors = rule_errors(sig, &values, &names, docs);
        if !errors.is_empty() {
            return Ok(Err(errors));
        }
    }
    Ok(Ok(out))
}

/// The checks of the calls of a typed function that R8's Rust makes through
/// the function's compiled body: the checks that `checked_call()` and
/// `function_result()` of `view.c` make at a call from R, over values in a
/// list that holds them.
pub(crate) struct CallChecks(Rc<Signature>);

/// The checks of the calls of the function whose key is the symbol `key`, its
/// qualified name; `None` when it has no signature.
pub(crate) fn call_checks(key: SEXP) -> Option<CallChecks> {
    function_key(key)?;
    signature(key).map(CallChecks)
}

impl CallChecks {
    /// The number of the forced parameters, which are the function's typed
    /// parameters in the order of the declaration.
    pub(crate) fn forced(&self) -> usize {
        self.0.forced.len()
    }

    /// Whether the arguments in the first elements of `list`, one for each
    /// forced parameter, pass their types and the function's rules; each passes
    /// in its stored form, which replaces it in `list`. A call whose arguments
    /// do not pass goes to the function's closure, which reports the errors.
    pub(crate) fn arguments(&self, list: SEXP) -> bool {
        let sig = &self.0;
        let at = |i: usize| unsafe { VECTOR_ELT(list, i as isize) };
        let mut check = false;
        for s in &sig.slots {
            let x = at(s.at);
            match s.test.run(x) {
                PASS => {}
                TO_INTEGER => unsafe {
                    let v = extendr_ffi::Rf_ScalarInteger(*REAL(x) as i32);
                    extendr_ffi::SET_VECTOR_ELT(list, s.at as isize, v);
                },
                TO_DOUBLE => unsafe {
                    let v = Rf_ScalarReal(f64::from(*INTEGER(x)));
                    extendr_ffi::SET_VECTOR_ELT(list, s.at as isize, v);
                },
                _ => check = true,
            }
        }
        if !check && !sig.rules.is_empty() {
            let values: Vec<SEXP> = (0..sig.forced.len()).map(at).collect();
            check = !sig.rules_hold(&values);
        }
        if !check {
            return true;
        }
        let docs = crate::declare::registry().snapshot();
        match validated(sig, list, &docs) {
            Ok(Ok(out)) => {
                for (i, v) in out.iter().enumerate() {
                    if let Some(v) = v {
                        unsafe { extendr_ffi::SET_VECTOR_ELT(list, i as isize, v.get()) };
                    }
                }
                true
            }
            _ => false,
        }
    }

    /// Checks the result in the element `at` of `list` against the return
    /// type: `Ok` with the result in its stored form, which replaces it in
    /// `list`, or the problems that refuse it, of class `r8_problems`, or the
    /// text of an error of the check.
    pub(crate) fn result(&self, list: SEXP, at: usize) -> std::result::Result<(), Robj> {
        let (ty, test) = match &self.0.returned {
            Returned::Type(t, test) => (t, test),
            Returned::Any => return Ok(()),
            Returned::SelfType => return Err(Robj::from("a typed function returns no `Self`")),
        };
        let x = unsafe { VECTOR_ELT(list, at as isize) };
        let stored = match test.run(x) {
            PASS => return Ok(()),
            TO_INTEGER => unsafe { extendr_ffi::Rf_ScalarInteger(*REAL(x) as i32) },
            TO_DOUBLE => unsafe { Rf_ScalarReal(f64::from(*INTEGER(x))) },
            _ => {
                let docs = crate::declare::registry().snapshot();
                let errors = errors_of(x, ty, &docs, None);
                if !errors.is_empty() {
                    return Err(problems(&errors).unwrap_or_else(|e| Robj::from(e.to_string())));
                }
                test.admit(x);
                match normalize(x, ty, &*docs) {
                    Ok(None) => return Ok(()),
                    Ok(Some(r)) => {
                        unsafe { extendr_ffi::SET_VECTOR_ELT(list, at as isize, r.get()) };
                        return Ok(());
                    }
                    Err(e) => return Err(Robj::from(e.to_string())),
                }
            }
        };
        unsafe { extendr_ffi::SET_VECTOR_ELT(list, at as isize, stored) };
        Ok(())
    }
}

/// The violations of a function's rules by the values of its typed
/// parameters, `values` by the names `names`.
fn rule_errors(
    sig: &Signature,
    values: &[SEXP],
    names: &[&str],
    docs: &DocumentSet,
) -> Vec<ValueError> {
    let held = named(values, names);
    let view = RView::Draft(unsafe { held.get() }, "");
    let mut errors = Vec::new();
    with_cache(|cache| {
        for (ri, r) in sig.rules.iter().enumerate() {
            let Some(rule) = cache.compiled(r, &sig.scope, docs) else {
                continue;
            };
            let found = rule.check_access(view, &sig.scope, docs);
            match found {
                Ok(rtemis_r8::rules::Outcome::Holds) => {}
                Ok(rtemis_r8::rules::Outcome::Violated(paths)) => {
                    for p in paths.into_iter().take(rtemis_r8::validate::DIAGNOSED) {
                        let d = rule.diagnose_access(view, &sig.scope, docs, &p);
                        errors.push(ValueError {
                            code: "rule_violation",
                            path: rtemis_r8::problem::pointer(&p),
                            segments: p,
                            rule: r.id.clone(),
                            rule_index: Some(ri),
                            message: r.message.clone().unwrap_or_default(),
                            diagnosis: Some(rtemis_r8::validate::diagnosis(r, d)),
                            ..Default::default()
                        });
                    }
                }
                Err(e) => {
                    let mut d = rtemis_r8::validate::diagnosis(
                        r,
                        rule.diagnose_access(view, &sig.scope, docs, &[]),
                    );
                    d.operands.clear();
                    errors.push(ValueError {
                        code: "rule_error",
                        rule: r.id.clone(),
                        rule_index: Some(ri),
                        message: e,
                        diagnosis: Some(d),
                        ..Default::default()
                    });
                }
            }
        }
    });
    errors
}

/// A named list of `values`.
fn named(values: &[SEXP], names: &[&str]) -> Robj {
    let list = List::from_values(values.iter().map(|&v| unsafe { Robj::from_sexp(v) }));
    let mut out: Robj = list.into();
    let _ = out.set_names(names.iter().copied());
    out
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
fn r8_check_result(key: Robj, tuple: crate::utf8::Utf8, value: Robj) -> Result<Robj> {
    crate::interrupt::interruptible(unsafe { value.get() }, || {
        let tuple: &str = &tuple;
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
fn r8_method_returns(trait_name: crate::utf8::Utf8, method: crate::utf8::Utf8) -> Result<List> {
    let trait_name: &str = &trait_name;
    let method: &str = &method;
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
