//! Construction of R8 values from R objects, in place.
//!
//! `r8_new_value()` validates the fields of a value through [`RView`], which
//! reads the R objects without copying, and assembles the value from them. A
//! field already in its stored form is kept by reference; a field in another
//! accepted form is converted to the stored form: an integer vector for a
//! double field, a `Date` stored as integers, a `POSIXct` in another time
//! zone or below microsecond precision, a factor whose levels differ from
//! the declared levels, a `btree_map()` in unsorted order, a tibble, and the
//! others. The stored forms are listed in `values.rs`.
//!
//! Compiled rules and patterns are kept between constructions in a
//! [`RuleCache`], cleared when a declaration changes the registry.

use std::collections::HashMap;
use std::ffi::{CStr, c_char, c_int};
use std::panic::{AssertUnwindSafe, catch_unwind};
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Mutex, OnceLock};

use extendr_api::SEXP;
use extendr_api::prelude::*;
use extendr_api::{Error, Result};
use extendr_ffi::{
    INTEGER, LOGICAL, R_xlen_t, REAL, Rf_allocVector, Rf_duplicate, Rf_mkCharLenCE, Rf_protect,
    Rf_setAttrib, Rf_unprotect, SET_STRING_ELT, SET_VECTOR_ELT, SEXPTYPE, STRING_ELT, VECTOR_ELT,
    cetype_t,
};
use rtemis_r8::access::Access;
use rtemis_r8::ir::{FieldDef, FieldType, TypeDef, VariantKind};
use rtemis_r8::name::QualifiedName;
use rtemis_r8::rules::TypeLookup;
use rtemis_r8::validate::{RuleCache, ValueError, validate_access};

use crate::declare::registry;
use crate::rview::*;

/// Incremented by every change to the registry.
pub(crate) static GENERATION: AtomicU64 = AtomicU64::new(0);

/// Records a change to the registry, which invalidates compiled rules.
pub(crate) fn registry_changed() {
    GENERATION.fetch_add(1, Ordering::Relaxed);
}

/// Runs `f` with the rule cache of the current registry.
///
/// `f` reads R objects, from which R may jump: an element method of an ALTREP
/// vector raises an error, or the user interrupts it. The cache is taken for
/// the call and the lock released, so a jump leaves an empty cache and no
/// lock held, where a held lock would block every later call.
pub(crate) fn with_cache<T>(f: impl FnOnce(&mut RuleCache) -> T) -> T {
    static CACHE: OnceLock<Mutex<(u64, RuleCache)>> = OnceLock::new();
    let m = CACHE.get_or_init(|| Mutex::new((0, RuleCache::new())));
    let g = GENERATION.load(Ordering::Relaxed);
    let mut cache = {
        let mut held = m.lock().unwrap_or_else(|e| e.into_inner());
        if held.0 != g {
            held.1.clear();
            held.0 = g;
        }
        std::mem::take(&mut held.1)
    };
    let out = f(&mut cache);
    let mut held = m.lock().unwrap_or_else(|e| e.into_inner());
    if held.0 == g {
        held.1 = cache;
    }
    out
}

/// An R object kept between calls; R runs these calls on one thread.
struct Kept(Robj);
// SAFETY: R8's native functions run on R's main thread only.
unsafe impl Send for Kept {}

/// The declared defaults decoded to R, by type, variant (`""` for a
/// struct), and field index.
type Defaults = HashMap<QualifiedName, HashMap<Box<str>, Vec<Option<Kept>>>>;

/// The R form of the declared default `d` of the field `k` of the type `q`,
/// of its variant `variant` for an enum (`""` for a struct), whose type is
/// `ft`: decoded from the registry once per registry generation and shared
/// by every value that takes it. A value that modifies the field copies it
/// first, since the cache holds a reference. The key names the field, not
/// the address of its declaration, which a replaced declaration can reuse.
pub(crate) fn default_r(
    d: &rtemis_r8::tree::Tree,
    ft: &FieldType,
    lookup: &dyn TypeLookup,
    field: (&QualifiedName, &str, usize),
) -> Result<SEXP> {
    static CACHE: OnceLock<Mutex<(u64, Defaults)>> = OnceLock::new();
    let (q, variant, k) = field;
    let m = CACHE.get_or_init(|| Mutex::new((0, HashMap::new())));
    let g = GENERATION.load(Ordering::Relaxed);
    {
        let mut guard = m.lock().unwrap_or_else(|e| e.into_inner());
        if guard.0 != g {
            guard.1.clear();
            guard.0 = g;
        }
        if let Some(Some(r)) = guard
            .1
            .get(q)
            .and_then(|v| v.get(variant))
            .and_then(|fields| fields.get(k))
        {
            return Ok(unsafe { r.0.get() });
        }
    }
    // Decoding allocates R objects, from which R may jump: no lock is held.
    let r = crate::values::decode_field(rtemis_r8::tree::TreeRef::new(d), ft, lookup)?;
    let s = unsafe { r.get() };
    let mut guard = m.lock().unwrap_or_else(|e| e.into_inner());
    let slots = guard
        .1
        .entry(q.clone())
        .or_default()
        .entry(variant.into())
        .or_default();
    if slots.len() <= k {
        slots.resize_with(k + 1, || None);
    }
    slots[k] = Some(Kept(r));
    Ok(s)
}

/// The declaration hash of the registered type `q`
/// (`canonical::declaration_hash()`), computed once per registry change. The
/// cache is valid only for `lookup` the registry: a caller with another
/// lookup computes the hash directly.
pub(crate) fn registry_declaration_hash(
    q: &QualifiedName,
    lookup: &dyn TypeLookup,
) -> std::sync::Arc<str> {
    static CACHE: Hashes = OnceLock::new();
    registry_hash(&CACHE, q, || {
        rtemis_r8::canonical::declaration_hash(q, lookup)
    })
}

/// The value declaration hash of a registered type, which documents hold, kept
/// as [`registry_declaration_hash`] keeps its own.
pub(crate) fn registry_value_hash(
    q: &QualifiedName,
    lookup: &dyn TypeLookup,
) -> std::sync::Arc<str> {
    static CACHE: Hashes = OnceLock::new();
    registry_hash(&CACHE, q, || {
        rtemis_r8::canonical::value_declaration_hash(q, lookup)
    })
}

/// The hashes of registered types, at a generation of the registry.
type Hashes = OnceLock<Mutex<(u64, HashMap<QualifiedName, std::sync::Arc<str>>)>>;

fn registry_hash(
    cache: &Hashes,
    q: &QualifiedName,
    hash: impl FnOnce() -> String,
) -> std::sync::Arc<str> {
    let m = cache.get_or_init(|| Mutex::new((0, HashMap::new())));
    let mut guard = m.lock().unwrap_or_else(|e| e.into_inner());
    let g = GENERATION.load(Ordering::Relaxed);
    if guard.0 != g {
        guard.1.clear();
        guard.0 = g;
    }
    if let Some(h) = guard.1.get(q) {
        return h.clone();
    }
    let h: std::sync::Arc<str> = hash().into();
    guard.1.insert(q.clone(), h.clone());
    h
}

fn fail(message: impl Into<String>) -> Error {
    Error::Other(message.into())
}

const NA_INT: i32 = i32::MIN;
const NA_REAL: f64 = f64::from_bits(0x7FF0_0000_0000_07A2);

/// Wraps a newly allocated object, which extendr then protects.
fn own(x: SEXP) -> Robj {
    unsafe { Robj::from_sexp(x) }
}

pub(crate) fn mkchar(s: &str) -> SEXP {
    unsafe { Rf_mkCharLenCE(s.as_ptr() as *const _, s.len() as i32, cetype_t::CE_UTF8) }
}

/// A character vector of `items`.
fn character(items: &[&str]) -> Robj {
    unsafe {
        let v = Rf_protect(Rf_allocVector(SEXPTYPE::STRSXP, items.len() as R_xlen_t));
        for (i, s) in items.iter().enumerate() {
            SET_STRING_ELT(v, i as R_xlen_t, mkchar(s));
        }
        let r = own(v);
        Rf_unprotect(1);
        r
    }
}

fn set_attr(x: &Robj, name: SEXP, value: SEXP) {
    unsafe {
        Rf_setAttrib(x.get(), name, value);
    }
}

fn unwrap_option(ft: &FieldType) -> &FieldType {
    match ft {
        FieldType::Option { inner, .. } => inner,
        other => other,
    }
}

/// Whether every value of a date-time vector is a whole number of
/// microseconds, as the stored form holds it, and no value is `NaN`.
fn micros_exact(x: SEXP) -> bool {
    let mut exact = true;
    for_reals(x, 0, xlen(x), |_, v| {
        exact &= if v.is_nan() {
            is_na_real(v)
        } else {
            ((v * 1e6).round() as i64) as f64 / 1e6 == v
        };
    });
    exact
}

/// Whether `x` is an atomic vector in the stored form of the scalar type
/// `inner`, with `extra` other attributes.
fn atomic_ok(x: SEXP, inner: &FieldType, extra: usize) -> bool {
    let n = attrib_count(x);
    let t = sexptype(x);
    match inner {
        FieldType::Logical => t == SEXPTYPE::LGLSXP && n == extra,
        FieldType::Integer(_) => t == SEXPTYPE::INTSXP && n == extra,
        FieldType::Double(_) => t == SEXPTYPE::REALSXP && n == extra,
        FieldType::Character(_) => t == SEXPTYPE::STRSXP && n == extra,
        FieldType::Date => t == SEXPTYPE::REALSXP && class_is(x, &["Date"]) && n == extra + 1,
        FieldType::Posixct => {
            t == SEXPTYPE::REALSXP
                && class_is(x, &["POSIXct", "POSIXt"])
                && attr_string(x, sym_tzone()).as_deref() == Some("UTC")
                && n == extra + 2
                && micros_exact(x)
        }
        _ => false,
    }
}

/// An atomic vector in the stored form of the scalar type `inner`, from
/// elements of atomic vectors.
fn build_atomic(inner: &FieldType, items: &[(SEXP, usize)]) -> Robj {
    let n = items.len() as R_xlen_t;
    let number = number_elt;
    unsafe {
        let v = match inner {
            FieldType::Logical => {
                let v = Rf_protect(Rf_allocVector(SEXPTYPE::LGLSXP, n));
                for (k, &(x, i)) in items.iter().enumerate() {
                    *LOGICAL(v).add(k) = if sexptype(x) == SEXPTYPE::LGLSXP {
                        int_elt(x, i)
                    } else {
                        NA_INT
                    };
                }
                v
            }
            FieldType::Integer(_) => {
                let v = Rf_protect(Rf_allocVector(SEXPTYPE::INTSXP, n));
                for (k, &(x, i)) in items.iter().enumerate() {
                    *INTEGER(v).add(k) = number(x, i).map_or(NA_INT, |d| d as i32);
                }
                v
            }
            FieldType::Character(_) => {
                let v = Rf_protect(Rf_allocVector(SEXPTYPE::STRSXP, n));
                for (k, &(x, i)) in items.iter().enumerate() {
                    SET_STRING_ELT(v, k as R_xlen_t, STRING_ELT(x, i as R_xlen_t));
                }
                v
            }
            FieldType::Posixct => {
                let v = Rf_protect(Rf_allocVector(SEXPTYPE::REALSXP, n));
                for (k, &(x, i)) in items.iter().enumerate() {
                    *REAL(v).add(k) = match number(x, i) {
                        Some(s) if !s.is_nan() => ((s * 1e6).round() as i64) as f64 / 1e6,
                        _ => NA_REAL,
                    };
                }
                v
            }
            _ => {
                let v = Rf_protect(Rf_allocVector(SEXPTYPE::REALSXP, n));
                for (k, &(x, i)) in items.iter().enumerate() {
                    *REAL(v).add(k) = match sexptype(x) {
                        SEXPTYPE::REALSXP => real_elt(x, i),
                        _ => number(x, i).unwrap_or(NA_REAL),
                    };
                }
                v
            }
        };
        let r = own(v);
        Rf_unprotect(1);
        match inner {
            FieldType::Date => set_attr(&r, sym_class(), character(&["Date"]).get()),
            FieldType::Posixct => {
                set_attr(&r, sym_class(), character(&["POSIXct", "POSIXt"]).get());
                set_attr(&r, sym_tzone(), character(&["UTC"]).get());
            }
            _ => {}
        }
        r
    }
}

fn elements(x: SEXP) -> Vec<(SEXP, usize)> {
    (0..xlen(x)).map(|i| (x, i)).collect()
}

/// The stored R form of a computed value of the field type `ft`: a scalar
/// as a length-one vector, an array as a vector of the element type.
pub(crate) fn scalar_to_r(v: &rtemis_r8::rules::Scalar, ft: &FieldType) -> Result<Robj> {
    use rtemis_r8::rules::Scalar;
    let inner = unwrap_option(ft);
    let (element, items): (&FieldType, Vec<&Scalar>) = match (inner, v) {
        (_, Scalar::Null) => return Ok(().into()),
        (FieldType::Vec { element, .. }, Scalar::Array(a)) => {
            (unwrap_option(element), a.iter().collect())
        }
        (_, Scalar::Array(_)) => {
            return Err(fail("a computed array for a field that is not a vector"));
        }
        (t, s) => (t, vec![s]),
    };
    let n = items.len() as R_xlen_t;
    unsafe {
        let x = match element {
            FieldType::Logical => {
                let x = Rf_protect(Rf_allocVector(SEXPTYPE::LGLSXP, n));
                for (i, s) in items.iter().enumerate() {
                    *LOGICAL(x).add(i) = match s {
                        Scalar::Bool(b) => i32::from(*b),
                        _ => NA_INT,
                    };
                }
                x
            }
            FieldType::Integer(_) => {
                let x = Rf_protect(Rf_allocVector(SEXPTYPE::INTSXP, n));
                for (i, s) in items.iter().enumerate() {
                    *INTEGER(x).add(i) = match s {
                        Scalar::Num(d) if d.is_finite() => *d as i32,
                        _ => NA_INT,
                    };
                }
                x
            }
            FieldType::Character(_) => {
                let x = Rf_protect(Rf_allocVector(SEXPTYPE::STRSXP, n));
                for (i, s) in items.iter().enumerate() {
                    if let Scalar::Str(t) = s {
                        SET_STRING_ELT(x, i as R_xlen_t, mkchar(t));
                    } else {
                        SET_STRING_ELT(x, i as R_xlen_t, extendr_ffi::R_NaString);
                    }
                }
                x
            }
            _ => {
                let x = Rf_protect(Rf_allocVector(SEXPTYPE::REALSXP, n));
                for (i, s) in items.iter().enumerate() {
                    *REAL(x).add(i) = match (element, s) {
                        (FieldType::Date, Scalar::Date(d)) => *d as f64,
                        (FieldType::Posixct, Scalar::DateTime(m)) => *m as f64 / 1e6,
                        (_, Scalar::Num(d)) => *d,
                        _ => NA_REAL,
                    };
                }
                x
            }
        };
        let r = own(x);
        Rf_unprotect(1);
        match element {
            FieldType::Date => set_attr(&r, sym_class(), character(&["Date"]).get()),
            FieldType::Posixct => {
                set_attr(&r, sym_class(), character(&["POSIXct", "POSIXt"]).get());
                set_attr(&r, sym_tzone(), character(&["UTC"]).get());
            }
            _ => {}
        }
        Ok(r)
    }
}

/// The stored form of an atomic vector of the scalar type `inner`, or
/// `None` when `x` is in it.
fn atomic(x: SEXP, inner: &FieldType) -> Option<Robj> {
    (!atomic_ok(x, inner, 0)).then(|| build_atomic(inner, &elements(x)))
}

/// The stored form of a valid value `x` of the field type `ft`, or `None`
/// when `x` is in it.
pub(crate) fn normalize(x: SEXP, ft: &FieldType, lookup: &dyn TypeLookup) -> Result<Option<Robj>> {
    if is_null(x) {
        return Ok(None);
    }
    Ok(match ft {
        FieldType::Option { inner, .. } => return normalize(x, inner, lookup),
        FieldType::Logical
        | FieldType::Integer(_)
        | FieldType::Double(_)
        | FieldType::Character(_)
        | FieldType::Date
        | FieldType::Posixct => atomic(x, ft),
        FieldType::Raw
        | FieldType::HostClass { .. }
        | FieldType::Named { .. }
        | FieldType::Dyn { .. }
        | FieldType::Subtype { .. } => None,
        FieldType::Vec { element, .. } => {
            let inner = unwrap_option(element);
            if is_scalar(inner) {
                atomic(x, inner)
            } else {
                list_items(x, element, lookup)?
            }
        }
        FieldType::Factor { levels, ordered } => factor(x, levels.as_deref(), *ordered),
        FieldType::BtreeMap { value, .. } => map(x, value, false, lookup)?,
        FieldType::IndexMap { value, .. } => map(x, value, true, lookup)?,
        FieldType::Json => json(x),
        FieldType::Matrix { element, .. } | FieldType::Array { element, .. } => {
            dense(x, unwrap_option(element))
        }
        FieldType::MatrixOf { of } => match lookup.find(of) {
            Some(TypeDef::Struct(s)) => match s.fields.first() {
                Some(f) => dense(x, unwrap_option(&f.ty)),
                None => None,
            },
            _ => None,
        },
        FieldType::SparseMatrix { .. } => sparse(x)?,
        FieldType::Table { columns, .. } => table(x, columns)?,
        FieldType::TableOf { of } => match lookup.find(of) {
            Some(TypeDef::Struct(s)) => table(x, &s.fields)?,
            _ => None,
        },
    })
}

/// A list whose items are normalized one by one.
fn list_items(x: SEXP, element: &FieldType, lookup: &dyn TypeLookup) -> Result<Option<Robj>> {
    let n = xlen(x);
    let mut changed: Vec<(usize, Robj)> = Vec::new();
    for i in 0..n {
        let item = unsafe { VECTOR_ELT(x, i as R_xlen_t) };
        if let Some(r) = normalize(item, element, lookup)? {
            changed.push((i, r));
        }
    }
    if changed.is_empty() && attrib_count(x) == 0 {
        return Ok(None);
    }
    unsafe {
        let v = Rf_protect(Rf_allocVector(SEXPTYPE::VECSXP, n as R_xlen_t));
        for i in 0..n {
            SET_VECTOR_ELT(v, i as R_xlen_t, VECTOR_ELT(x, i as R_xlen_t));
        }
        for (i, r) in &changed {
            SET_VECTOR_ELT(v, *i as R_xlen_t, r.get());
        }
        let r = own(v);
        Rf_unprotect(1);
        Ok(Some(r))
    }
}

fn factor(x: SEXP, declared: Option<&[Box<str>]>, ordered: bool) -> Option<Robj> {
    let class: &[&str] = if ordered {
        &["ordered", "factor"]
    } else {
        &["factor"]
    };
    let levels = attr(x, sym_levels());
    let current: Vec<String> = strings(levels)
        .into_iter()
        .map(|l| l.map(|s| s.into_owned()).unwrap_or_default())
        .collect();
    let same_levels = declared.is_none_or(|d| {
        d.len() == current.len() && d.iter().zip(&current).all(|(a, b)| **a == **b)
    });
    if same_levels && class_is(x, class) && attrib_count(x) == 2 {
        return None;
    }
    let map: Vec<i32> = match declared {
        Some(d) => {
            let index: std::collections::HashMap<&str, i32> = d
                .iter()
                .enumerate()
                .map(|(k, l)| (&**l, k as i32 + 1))
                .collect();
            current
                .iter()
                .map(|l| index.get(l.as_str()).copied().unwrap_or(NA_INT))
                .collect()
        }
        None => (1..=current.len() as i32).collect(),
    };
    let n = xlen(x);
    unsafe {
        let v = Rf_protect(Rf_allocVector(SEXPTYPE::INTSXP, n as R_xlen_t));
        let out = std::slice::from_raw_parts_mut(INTEGER(v), n);
        for_ints(x, 0, n, |k, c| {
            out[k] = c
                .checked_sub(1)
                .and_then(|i| usize::try_from(i).ok())
                .and_then(|i| map.get(i).copied())
                .unwrap_or(NA_INT);
        });
        let r = own(v);
        Rf_unprotect(1);
        let lv: Vec<&str> = match declared {
            Some(d) => d.iter().map(|l| &**l).collect(),
            None => current.iter().map(String::as_str).collect(),
        };
        set_attr(&r, sym_levels(), character(&lv).get());
        set_attr(&r, sym_class(), character(class).get());
        Some(r)
    }
}

/// A `btree_map()` (`ordered = false`), stored with its keys sorted, as a named
/// atomic vector for scalar values and a named list otherwise; or an
/// `index_map()`, stored as a named list in its order.
fn map(x: SEXP, value: &FieldType, ordered: bool, lookup: &dyn TypeLookup) -> Result<Option<Robj>> {
    let names = attr(x, sym_names());
    let keys: Vec<String> = strings(names)
        .into_iter()
        .map(|k| {
            k.map(|s| s.into_owned())
                .unwrap_or_else(|| "NA".to_string())
        })
        .collect();
    let mut order: Vec<usize> = (0..keys.len()).collect();
    if !ordered {
        order.sort_by(|&a, &b| keys[a].as_bytes().cmp(keys[b].as_bytes()));
    }
    let sorted = order.iter().enumerate().all(|(i, &k)| i == k);
    let inner = unwrap_option(value);
    let atomic_store = !ordered && is_scalar(inner);
    let input_list = sexptype(x) == SEXPTYPE::VECSXP;
    let sorted_keys: Vec<&str> = order.iter().map(|&k| keys[k].as_str()).collect();
    if atomic_store {
        if !input_list && sorted && atomic_ok(x, inner, 1) {
            return Ok(None);
        }
        let items: Vec<(SEXP, usize)> = order
            .iter()
            .map(|&k| {
                if input_list {
                    (unsafe { VECTOR_ELT(x, k as R_xlen_t) }, 0)
                } else {
                    (x, k)
                }
            })
            .collect();
        let r = build_atomic(inner, &items);
        set_attr(&r, sym_names(), unsafe { character(&sorted_keys).get() });
        return Ok(Some(r));
    }
    // A named list, its items normalized; an atomic input becomes a list of
    // length-one vectors.
    let n = order.len();
    let mut items: Vec<Robj> = Vec::with_capacity(n);
    let mut changed = !input_list || !sorted || attrib_count(x) != 1;
    for &k in &order {
        let item = if input_list {
            let e = unsafe { VECTOR_ELT(x, k as R_xlen_t) };
            match normalize(e, value, lookup)? {
                Some(r) => {
                    changed = true;
                    r
                }
                None => unsafe { Robj::from_sexp(e) },
            }
        } else if RView::Elem(x, k).is_absent() {
            ().into()
        } else {
            build_atomic(inner, &[(x, k)])
        };
        items.push(item);
    }
    if !changed {
        return Ok(None);
    }
    unsafe {
        let v = Rf_protect(Rf_allocVector(SEXPTYPE::VECSXP, n as R_xlen_t));
        for (i, r) in items.iter().enumerate() {
            SET_VECTOR_ELT(v, i as R_xlen_t, r.get());
        }
        let r = own(v);
        Rf_unprotect(1);
        set_attr(&r, sym_names(), character(&sorted_keys).get());
        Ok(Some(r))
    }
}

/// A `type_json()` value: numbers stored as doubles, atomic values without
/// attributes, and lists with their names only.
fn json(x: SEXP) -> Option<Robj> {
    match sexptype(x) {
        SEXPTYPE::LGLSXP | SEXPTYPE::STRSXP | SEXPTYPE::REALSXP if attrib_count(x) == 0 => None,
        SEXPTYPE::LGLSXP => Some(build_atomic(&FieldType::Logical, &[(x, 0)])),
        SEXPTYPE::STRSXP => Some(build_atomic(
            &FieldType::Character(Default::default()),
            &[(x, 0)],
        )),
        SEXPTYPE::REALSXP | SEXPTYPE::INTSXP => Some(build_atomic(
            &FieldType::Double(Default::default()),
            &[(x, 0)],
        )),
        SEXPTYPE::VECSXP => {
            let n = xlen(x);
            let names = attr(x, sym_names());
            let extra = attrib_count(x) - usize::from(!is_null(names));
            let changed: Vec<(usize, Robj)> = (0..n)
                .filter_map(|i| json(unsafe { VECTOR_ELT(x, i as R_xlen_t) }).map(|r| (i, r)))
                .collect();
            if changed.is_empty() && extra == 0 {
                return None;
            }
            unsafe {
                let v = Rf_protect(Rf_allocVector(SEXPTYPE::VECSXP, n as R_xlen_t));
                for i in 0..n {
                    SET_VECTOR_ELT(v, i as R_xlen_t, VECTOR_ELT(x, i as R_xlen_t));
                }
                for (i, r) in &changed {
                    SET_VECTOR_ELT(v, *i as R_xlen_t, r.get());
                }
                let r = own(v);
                Rf_unprotect(1);
                if !is_null(names) {
                    set_attr(&r, sym_names(), names);
                }
                Some(r)
            }
        }
        _ => None,
    }
}

/// A matrix or array: its data in the stored form of `inner`, `dim`, and
/// `dimnames` without names.
fn dense(x: SEXP, inner: &FieldType) -> Option<Robj> {
    let dn = attr(x, sym_dimnames());
    let has_dn = !is_null(dn);
    let dn_named = has_dn && !is_null(attr(dn, sym_names()));
    if !dn_named && atomic_ok(x, inner, 1 + usize::from(has_dn)) {
        return None;
    }
    let r = build_atomic(inner, &elements(x));
    set_attr(&r, sym_dim(), attr(x, sym_dim()));
    if has_dn {
        unsafe {
            let n = xlen(dn);
            let v = Rf_protect(Rf_allocVector(SEXPTYPE::VECSXP, n as R_xlen_t));
            for i in 0..n {
                SET_VECTOR_ELT(v, i as R_xlen_t, VECTOR_ELT(dn, i as R_xlen_t));
            }
            set_attr(&r, sym_dimnames(), v);
            Rf_unprotect(1);
        }
    }
    Some(r)
}

/// A `dgCMatrix`, stored without dimnames or cached factorizations.
fn sparse(x: SEXP) -> Result<Option<Robj>> {
    let dimnames = attr(x, unsafe { extendr_ffi::Rf_install(c"Dimnames".as_ptr()) });
    let factors = attr(x, unsafe { extendr_ffi::Rf_install(c"factors".as_ptr()) });
    let plain_dimnames = sexptype(dimnames) == SEXPTYPE::VECSXP
        && (0..xlen(dimnames)).all(|i| is_null(unsafe { VECTOR_ELT(dimnames, i as R_xlen_t) }));
    if plain_dimnames && xlen(factors) == 0 {
        return Ok(None);
    }
    let slot = |n: &std::ffi::CStr| unsafe {
        Robj::from_sexp(attr(x, extendr_ffi::Rf_install(n.as_ptr())))
    };
    let f = R!("R8:::new_dgc")?;
    let r = f
        .as_function()
        .ok_or_else(|| fail("R8:::new_dgc"))?
        .call(pairlist!(slot(c"i"), slot(c"p"), slot(c"x"), slot(c"Dim")))?;
    Ok(Some(r))
}

/// A table, stored as a `data.frame` of its own with the compact row names
/// `c(NA, -n)` and its columns in their stored forms: the columns of a data frame
/// as they are, and copies of those of a data.table, whose functions change a
/// column in place. A list of its own keeps `setDT()` of the data frame given from
/// the value's.
fn table<C: rtemis_r8::ir::Column>(x: SEXP, columns: &[C]) -> Result<Option<Robj>> {
    let n = table_rows(x).map_err(|f| fail(f.message))? as i32;
    let names = attr(x, sym_names());
    let k = xlen(x);
    let mut changed: Vec<(usize, Robj)> = Vec::new();
    for i in 0..k {
        let col = unsafe { VECTOR_ELT(x, i as R_xlen_t) };
        let name = string_elt(names, i);
        let ft = match columns
            .iter()
            .find(|c| Some(c.field().name.as_str()) == name.as_deref())
        {
            Some(c) => c.field().ty.clone(),
            None => match infer_column(col) {
                Some(t) => t,
                None => continue,
            },
        };
        let inner = unwrap_option(&ft);
        let r = match inner {
            FieldType::Factor { levels, ordered } => factor(col, levels.as_deref(), *ordered),
            _ => atomic(col, inner),
        };
        if let Some(r) = r {
            changed.push((i, r));
        }
    }
    let by_reference = inherits(x, "data.table");
    unsafe {
        let v = Rf_protect(Rf_allocVector(SEXPTYPE::VECSXP, k as R_xlen_t));
        for i in 0..k {
            if changed.iter().any(|(j, _)| *j == i) {
                continue;
            }
            let col = VECTOR_ELT(x, i as R_xlen_t);
            let own = if by_reference {
                Rf_duplicate(col)
            } else {
                crate::value::viewed(col)
            };
            SET_VECTOR_ELT(v, i as R_xlen_t, own);
        }
        for (i, r) in &changed {
            SET_VECTOR_ELT(v, *i as R_xlen_t, r.get());
        }
        let r = own(v);
        Rf_unprotect(1);
        // data.table's setnames() and set() change the vector of names in place.
        let own_names = Rf_protect(Rf_duplicate(names));
        set_attr(&r, sym_names(), own_names);
        Rf_unprotect(1);
        let rn = Rf_protect(Rf_allocVector(SEXPTYPE::INTSXP, 2));
        *INTEGER(rn) = NA_INT;
        *INTEGER(rn).add(1) = -n;
        set_attr(&r, sym_row_names(), rn);
        Rf_unprotect(1);
        set_attr(&r, sym_class(), character(&["data.frame"]).get());
        Ok(Some(r))
    }
}

pub(crate) use crate::values::errors_list;

/// Assembles a value of the type `q` from validated fields: each field in
/// declaration order, in its stored form, an absent field with a default
/// given its default.
pub(crate) fn assemble(
    fields: SEXP,
    q: &QualifiedName,
    variant: &str,
    lookup: &dyn TypeLookup,
) -> Result<Robj> {
    let def = lookup
        .find(q)
        .ok_or_else(|| fail(format!("`{q}` is not registered")))?;
    let (defs, variant): (&[FieldDef], Option<&str>) = match def {
        TypeDef::Struct(s) => (&s.fields, None),
        TypeDef::Enum(e) => {
            let kind = e
                .variants
                .iter()
                .find(|v| v.name.as_str() == variant)
                .map(|v| &v.kind);
            let defs: &[FieldDef] = match kind {
                Some(VariantKind::Struct { fields, .. }) => fields,
                Some(VariantKind::Newtype { of }) => match lookup.find(of) {
                    Some(TypeDef::Struct(s)) => &s.fields,
                    _ => &[],
                },
                _ => &[],
            };
            (defs, Some(variant))
        }
    };
    let n = defs.len();
    // Objects created here, kept protected until they are in the value.
    let mut created: Vec<Robj> = Vec::new();
    unsafe {
        let out = Rf_protect(Rf_allocVector(SEXPTYPE::VECSXP, n as R_xlen_t));
        for (k, f) in defs.iter().enumerate() {
            let x = list_member_at(fields, k, f.name.as_str()).unwrap_or(nil());
            let value = if is_null(x) {
                match &f.default {
                    Some(d) => default_r(d, &f.ty, lookup, (q, variant.unwrap_or(""), k))?,
                    None => nil(),
                }
            } else {
                match normalize(x, &f.ty, lookup)? {
                    Some(r) => {
                        let s = r.get();
                        created.push(r);
                        s
                    }
                    None => crate::value::viewed(x),
                }
            };
            SET_VECTOR_ELT(out, k as R_xlen_t, value);
        }
        let id = crate::value::identity(q, variant, lookup).map_err(|(_, m)| fail(m))?;
        // A Rust-stored type's value is the codec's state behind an ALTLIST
        // (`rust_stored.rs`).
        let r = match (
            def,
            crate::exports::codec(&q.to_string())
                .map_err(|m| fail(format!("{m} [declaration_changed]")))?,
        ) {
            (TypeDef::Struct(_), Some(vt)) => {
                let state = crate::rust_stored::from_fields(vt, out).map_err(fail)?;
                own(crate::rust_stored::wrap(vt, state, id))
            }
            (TypeDef::Enum(_), Some(vt)) => {
                let name = variant.unwrap_or_default();
                let state = crate::rust_stored::from_variant(vt, name, out).map_err(fail)?;
                own(crate::rust_stored::wrap(vt, state, id))
            }
            _ => own(crate::value::new(out, id)),
        };
        Rf_unprotect(1);
        drop(created);
        Ok(r)
    }
}

/// Constructs a value: the fields of `fields`, a named list, form a value of
/// the struct `type_name`, or of the variant `variant` of the enum
/// `type_name`. The fields are validated in place and kept by reference
/// where they are in their stored forms.
///
/// Returns the value, or `list(errors = <problems>)` with the class
/// `r8_problems`.
///
/// @param fields List: The fields, by name.
/// @param type_name Character: The qualified type name.
/// @param variant Character: The enum variant, or `""` for a struct.
/// @return The value or the problems.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_new_value(
    fields: Robj,
    type_name: crate::utf8::Utf8,
    variant: crate::utf8::Utf8,
) -> Result<Robj> {
    crate::interrupt::interruptible(unsafe { fields.get() }, || {
        let type_name: &str = &type_name;
        let variant: &str = &variant;
        let q = QualifiedName::parse(type_name).map_err(|e| fail(e.to_string()))?;
        let docs = registry().snapshot();
        if docs.find(&q).is_none() {
            return problems(&[ValueError {
                code: "type_unregistered",
                path: String::new(),
                rule: None,
                rule_index: None,
                message: format!("`{q}` is not registered"),
                diagnosis: None,
                ..Default::default()
            }]);
        }
        let x = unsafe { fields.get() };
        let errors =
            with_cache(|cache| validate_access(RView::Draft(x, variant), &q, &*docs, cache));
        if !errors.is_empty() {
            return problems(&errors);
        }
        assemble(x, &q, variant, &*docs)
    })
}

/// `list(errors = <problems>)` with the class `r8_problems`, which no R8
/// value carries.
pub(crate) fn problems(errors: &[ValueError]) -> Result<Robj> {
    let mut r: Robj = list!(errors = errors_list(errors)).into();
    r.set_class(["r8_problems"])?;
    Ok(r)
}

/// Replaces the field `name` of the R8 value `x` with `value`, as
/// `replace_fields()` states.
///
/// @param x An R8 value.
/// @param name Character: The field name.
/// @param value The new field value.
/// @return The new value, the problems, or `NULL`.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_set_field(x: Robj, name: crate::utf8::Utf8, value: Robj) -> Result<Robj> {
    crate::interrupt::interruptible(unsafe { value.get() }, || {
        let name: &str = &name;
        replace_fields(&x, &[(name, unsafe { value.get() })])
    })
}

/// Replaces the fields `names` of the R8 value `x` with the elements of
/// `values`, as `replace_fields()` states.
///
/// @param x An R8 value.
/// @param names The field names.
/// @param values The new field values.
/// @return The new value or the problems; `NULL` when `names` are not distinct
///   fields, or `values` is not a plain list of one value per name, with the
///   names of the fields or none.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_set_fields(x: Robj, names: Robj, values: Robj) -> Result<Robj> {
    crate::interrupt::interruptible(unsafe { values.get() }, || {
        let (ns, vs) = unsafe { (names.get(), values.get()) };
        let fields: Option<Vec<_>> = strings(ns).into_iter().collect();
        let Some(fields) = fields.filter(|_| sexptype(ns) == SEXPTYPE::STRSXP) else {
            return Ok(().into());
        };
        let distinct = fields
            .iter()
            .enumerate()
            .all(|(i, f)| !fields[..i].contains(f));
        let given = attr(vs, sym_names());
        let named = is_null(given)
            || strings(given).len() == fields.len()
                && strings(given)
                    .iter()
                    .zip(&fields)
                    .all(|(g, f)| g.as_ref() == Some(f));
        if !distinct
            || !named
            || sexptype(vs) != SEXPTYPE::VECSXP
            || !is_null(attr(vs, sym_class()))
            || xlen(vs) != fields.len()
        {
            return Ok(().into());
        }
        let changes: Vec<(&str, SEXP)> = fields
            .iter()
            .enumerate()
            .map(|(i, f)| (&**f, unsafe { VECTOR_ELT(vs, i as R_xlen_t) }))
            .collect();
        replace_fields(&x, &changes)
    })
}

/// A new value of the R8 value `x` with each field of `changes` replaced: the
/// fields are validated against their types, and the type's rules that read
/// any of them run once, on the new value. The other fields are shared with
/// `x`.
///
/// Returns the new value; `list(errors = <problems>)`, of every field, with
/// the class `r8_problems`; or `NULL` when a name is not a field.
fn replace_fields(x: &Robj, changes: &[(&str, SEXP)]) -> Result<Robj> {
    match replaced(x, changes)? {
        Replaced::Value(v) => Ok(v),
        Replaced::Problems(errors) => problems(&errors),
        Replaced::NotField => Ok(().into()),
    }
}

/// The outcome of [`replaced`].
enum Replaced {
    Value(Robj),
    Problems(Vec<ValueError>),
    /// A name is not a field.
    NotField,
}

/// The value of `value`, an R8 value, with the fields of `fields`, a named
/// list, replaced, unprotected; an error is a code and a message.
pub(crate) fn with_fields(
    value: SEXP,
    fields: SEXP,
) -> std::result::Result<SEXP, (&'static str, String)> {
    let names: Option<Vec<_>> = strings(attr(fields, sym_names())).into_iter().collect();
    let names = names
        .filter(|n| sexptype(fields) == SEXPTYPE::VECSXP && n.len() == xlen(fields))
        .ok_or_else(|| ("invalid_argument", "`fields` is a named list".to_string()))?;
    let changes: Vec<(&str, SEXP)> = names
        .iter()
        .enumerate()
        .map(|(i, f)| (&**f, unsafe { VECTOR_ELT(fields, i as R_xlen_t) }))
        .collect();
    match replaced(&own(value), &changes).map_err(|e| ("invalid_argument", e.to_string()))? {
        Replaced::Value(v) => Ok(unsafe { v.get() }),
        Replaced::Problems(errors) => {
            let lines: Vec<String> = errors
                .iter()
                .map(|e| {
                    let rule = match (&e.rule, e.rule_index) {
                        (Some(id), _) => format!("the rule `{id}` is violated"),
                        (None, Some(i)) => format!("rule {} is violated", i + 1),
                        (None, None) => return e.message.clone(),
                    };
                    if e.message.is_empty() {
                        rule
                    } else {
                        format!("{rule}: {}", e.message)
                    }
                })
                .collect();
            Err(("invalid_value", lines.join("; ")))
        }
        Replaced::NotField => Err((
            "unknown_field",
            format!("the value has none of the fields {}", names.join(", ")),
        )),
    }
}

/// [`replace_fields`], its outcome as Rust values.
fn replaced(x: &Robj, changes: &[(&str, SEXP)]) -> Result<Replaced> {
    let xs = unsafe { x.get() };
    let type_name = crate::value::type_name(xs).ok_or_else(|| fail("not an R8 value"))?;
    let q = QualifiedName::parse(&type_name).map_err(|e| fail(e.to_string()))?;
    let variant = crate::value::variant(xs);
    let docs = registry().snapshot();
    let lookup: &dyn TypeLookup = &*docs;
    let def = lookup
        .find(&q)
        .ok_or_else(|| fail(format!("`{q}` is not registered")))?;
    let (fields, rules): (&[FieldDef], &[rtemis_r8::ir::RuleDef]) = match def {
        TypeDef::Struct(s) => (&s.fields, &s.rules),
        TypeDef::Enum(e) => {
            let v = variant.as_deref().unwrap_or_default();
            match e
                .variants
                .iter()
                .find(|x| x.name.as_str() == v)
                .map(|x| &x.kind)
            {
                Some(VariantKind::Struct { fields, rules }) => (fields, rules),
                Some(VariantKind::Newtype { of }) => match lookup.find(of) {
                    Some(TypeDef::Struct(s)) => (&s.fields, &s.rules),
                    _ => (&[], &[]),
                },
                _ => (&[], &[]),
            }
        }
    };
    // A value made under another declaration, whose fields have other positions
    // or meanings.
    if let Some(o) = crate::value::other(xs) {
        return Ok(Replaced::Problems(vec![ValueError {
            code: "declaration_changed",
            message: format!("The value was {}.", o.message()),
            ..Default::default()
        }]));
    }
    if lookup.is_stale(&q) {
        let parent = lookup.ancestors(&q).first().map(|p| p.to_string());
        return Ok(Replaced::Problems(vec![ValueError {
            code: "type_stale",
            message: format!(
                "`{q}` was declared against an earlier declaration of `{}`; declare `{q}` again, or reload or reinstall the package that declares it.",
                parent.unwrap_or_default()
            ),
            ..Default::default()
        }]));
    }
    // Each field's new value: validated, in its stored form, or its default.
    let mut created: Vec<Robj> = Vec::new();
    let mut stored: Vec<(usize, SEXP)> = Vec::with_capacity(changes.len());
    let mut errors: Vec<ValueError> = Vec::new();
    for &(name, vs) in changes {
        let Some(k) = fields.iter().position(|f| f.name.as_str() == name) else {
            return Ok(Replaced::NotField);
        };
        let f = &fields[k];
        let at = format!("/{}", name.replace('~', "~0").replace('/', "~1"));
        if is_null(vs) {
            match &f.default {
                Some(d) => {
                    let at = (&q, variant.as_deref().unwrap_or(""), k);
                    stored.push((k, default_r(d, &f.ty, lookup, at)?));
                }
                None if matches!(f.ty, FieldType::Option { .. }) => stored.push((k, nil())),
                None => {
                    let segments = vec![rtemis_r8::problem::Segment::Field(name.to_string())];
                    let problem = rtemis_r8::problem::Problem::Required;
                    let message =
                        problem.sentence(&segments, None, None, &rtemis_r8::problem::Neutral);
                    errors.push(ValueError {
                        code: "missing_field",
                        path: at,
                        message,
                        segments,
                        problem: Some(problem),
                        ..Default::default()
                    });
                }
            }
            continue;
        }
        let mut found = with_cache(|cache| {
            rtemis_r8::validate::validate_field_access(RView::Whole(vs), &f.ty, lookup, cache)
        });
        if !found.is_empty() {
            for e in &mut found {
                e.path = format!("{at}{}", e.path);
                e.segments
                    .insert(0, rtemis_r8::problem::Segment::Field(name.to_string()));
                if let Some(p) = &e.problem {
                    e.message = p.sentence(
                        &e.segments,
                        e.found.as_ref(),
                        e.suggestion.as_deref(),
                        &rtemis_r8::problem::Neutral,
                    );
                }
            }
            errors.append(&mut found);
            continue;
        }
        if !errors.is_empty() {
            continue;
        }
        let s = match normalize(vs, &f.ty, lookup)? {
            Some(r) => {
                let s = unsafe { r.get() };
                created.push(r);
                s
            }
            None => crate::value::viewed(vs),
        };
        stored.push((k, s));
    }
    if !errors.is_empty() {
        return Ok(Replaced::Problems(errors));
    }
    let id = crate::value::identity(&q, variant.as_deref(), lookup).map_err(|(_, m)| fail(m))?;
    // A Rust-stored value: the codec builds the new state, one field at a
    // time, and the new value reads its fields from it on demand, keeping the
    // fields the old value has read, except the replaced ones; the rules then
    // read the fields they name. The old value's fields are not read.
    if crate::rust_stored::state_of(xs).is_some() {
        let mut value = x.clone();
        for ((name, _), (k, s)) in changes.iter().zip(&stored) {
            let from = unsafe { value.get() };
            let (state, vt) = crate::rust_stored::state_of(from)
                .ok_or_else(|| fail("a Rust-stored value has a state"))?;
            let new_state = crate::rust_stored::with_field(vt, state, name, *s).map_err(fail)?;
            value = own(crate::rust_stored::wrap_from(
                vt,
                new_state,
                id,
                from,
                *k as R_xlen_t,
            ));
        }
        drop(created);
        let errors = reading_rule_errors(rules, fields, changes, unsafe { value.get() }, lookup);
        if !errors.is_empty() {
            return Ok(Replaced::Problems(errors));
        }
        return Ok(Replaced::Value(value));
    }
    let n = xlen(xs);
    let out = unsafe {
        let out = Rf_protect(Rf_allocVector(SEXPTYPE::VECSXP, n as R_xlen_t));
        let held = crate::value::fields(xs);
        for i in 0..n {
            SET_VECTOR_ELT(out, i as R_xlen_t, VECTOR_ELT(held, i as R_xlen_t));
        }
        for &(k, s) in &stored {
            SET_VECTOR_ELT(out, k as R_xlen_t, s);
        }
        let r = own(crate::value::new(out, id));
        Rf_unprotect(1);
        r
    };
    drop(created);
    let errors = reading_rule_errors(rules, fields, changes, unsafe { out.get() }, lookup);
    if !errors.is_empty() {
        return Ok(Replaced::Problems(errors));
    }
    Ok(Replaced::Value(out))
}

/// The violations of the rules of `rules` that read any field of `changes`,
/// on the value `os` whose fields are `fields`. The rules come compiled from the
/// rule cache, each with the fields it reads, and read only the fields they
/// name.
fn reading_rule_errors(
    rules: &[rtemis_r8::ir::RuleDef],
    fields: &[FieldDef],
    changes: &[(&str, SEXP)],
    os: SEXP,
    lookup: &dyn TypeLookup,
) -> Vec<ValueError> {
    // The rules that read a field, compiled once per registry and kept in
    // the rule cache, each with the fields it reads.
    let mut errors = Vec::new();
    if !rules.is_empty() {
        with_cache(|cache| {
            for (ri, r) in rules.iter().enumerate() {
                let Some(rule) = cache.compiled(r, fields, lookup) else {
                    // A rule that does not compile, which a change to a type
                    // it reads can cause, is reported by the writes of the
                    // fields its expressions name.
                    let named = |l: &rtemis_r8::ir::Logic| {
                        rtemis_r8::rules::reads(l)
                            .iter()
                            .any(|x| changes.iter().any(|(n, _)| x == n))
                    };
                    if !named(&r.invariant) && !r.when.as_ref().is_some_and(named) {
                        continue;
                    }
                    errors.push(ValueError {
                        code: "rule_invalid",
                        path: String::new(),
                        rule: r.id.clone(),
                        rule_index: Some(ri),
                        message: format!("rule {ri} does not compile; check the declaration"),
                        diagnosis: None,
                        ..Default::default()
                    });
                    continue;
                };
                if !changes.iter().any(|(n, _)| rule.reads_field(n)) {
                    continue;
                }
                match rule.check_access(RView::Whole(os), fields, lookup) {
                    Ok(rtemis_r8::rules::Outcome::Holds) => {}
                    Ok(rtemis_r8::rules::Outcome::Violated(paths)) => {
                        for (k, p) in paths.into_iter().enumerate() {
                            let diagnosis = (k < rtemis_r8::validate::DIAGNOSED).then(|| {
                                rtemis_r8::validate::diagnosis(
                                    r,
                                    rule.diagnose_access(RView::Whole(os), fields, lookup, &p),
                                )
                            });
                            errors.push(ValueError {
                                code: "rule_violation",
                                path: rtemis_r8::problem::pointer(&p),
                                segments: p,
                                rule: rule.id.clone(),
                                rule_index: Some(ri),
                                message: rule.message.clone().unwrap_or_default(),
                                diagnosis,
                                ..Default::default()
                            });
                        }
                    }
                    Err(e) => errors.push(ValueError {
                        code: "rule_error",
                        path: String::new(),
                        rule: rule.id.clone(),
                        rule_index: Some(ri),
                        message: e,
                        diagnosis: None,
                        ..Default::default()
                    }),
                }
            }
        });
    }
    errors
}

/// The ancestors of the registered type `type_name` through `extends`, the
/// nearest first, and whether the type is stale.
///
/// @param type_name Character: The qualified type name.
/// @return List: `ancestors`, character; `stale`, logical; `parent`, the
///   type's parent or `""`.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_type_ancestors(type_name: crate::utf8::Utf8) -> Result<List> {
    let type_name: &str = &type_name;
    let q = QualifiedName::parse(type_name).map_err(|e| fail(e.to_string()))?;
    let docs = registry().snapshot();
    let parent = match docs.find(&q) {
        Some(TypeDef::Struct(s)) => s.extends.as_ref().map(|p| p.to_string()),
        _ => None,
    };
    let ancestors: Vec<String> = docs.ancestors(&q).iter().map(|a| a.to_string()).collect();
    Ok(list!(
        ancestors = ancestors,
        stale = docs.is_stale(&q),
        parent = parent.unwrap_or_default()
    ))
}

/// The number of fields of the struct `ancestor` when the registered type
/// `type_name` is a subtype of it and not stale; -1 otherwise. The fields
/// are the first of a value of `type_name`, which R8's C routine `upcast`
/// (`src/view.c`) reads.
///
/// # Safety
///
/// Both names are NUL-terminated.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn r8_inherited_fields(
    type_name: *const c_char,
    ancestor: *const c_char,
) -> c_int {
    catch_unwind(AssertUnwindSafe(|| {
        let name = |p| QualifiedName::parse(unsafe { CStr::from_ptr(p) }.to_str().ok()?).ok();
        let (ty, base) = (name(type_name)?, name(ancestor)?);
        let docs = registry().snapshot();
        let lookup: &dyn TypeLookup = &*docs;
        match lookup.find(&base) {
            Some(TypeDef::Struct(s)) if lookup.is_subtype(&ty, &base) && !lookup.is_stale(&ty) => {
                c_int::try_from(s.fields.len()).ok()
            }
            _ => None,
        }
    }))
    .ok()
    .flatten()
    .unwrap_or(-1)
}

/// The value of the struct `type_name` over `fields`, a list of its fields
/// (`rust_stored::value_of_fields()`). NULL, with `err` filled, when there is
/// none.
///
/// # Safety
///
/// `type_name` is NUL-terminated, `fields` is a protected list that nothing
/// else refers to, and `err` is writable; called on R's main thread.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn r8_value_of_fields(
    type_name: *const c_char,
    fields: SEXP,
    err: *mut rtemis_r8::abi::Error,
) -> SEXP {
    let result = catch_unwind(AssertUnwindSafe(|| -> std::result::Result<SEXP, String> {
        let name = unsafe { CStr::from_ptr(type_name) }
            .to_str()
            .map_err(|e| e.to_string())?;
        let q = QualifiedName::parse(name).map_err(|e| e.to_string())?;
        let docs = registry().snapshot();
        crate::rust_stored::value_of_fields(&q, None, fields, &*docs, false)
            .map_err(|(_, message)| message)
    }));
    match result {
        Ok(Ok(v)) => v,
        Ok(Err(message)) => {
            crate::rust_stored::set_error(err, "upcast", &message);
            std::ptr::null_mut()
        }
        Err(_) => {
            crate::rust_stored::set_error(err, "panic", "R8 panicked while upcasting a value");
            std::ptr::null_mut()
        }
    }
}

extendr_module! {
    mod construct;
    fn r8_new_value;
    fn r8_set_field;
    fn r8_set_fields;
    fn r8_type_ancestors;
}
