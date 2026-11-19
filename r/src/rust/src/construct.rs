//! Construction of R8 values from R objects, in place.
//!
//! `r8_new_value()` validates the fields of a value through [`RView`], which
//! reads the R objects without copying, and assembles the value from them. A
//! field already in its stored form is kept by reference; a field in another
//! accepted form is converted to the stored form: an integer vector for a
//! double field, a `Date` stored as integers, a `POSIXct` in another time
//! zone or below microsecond precision, a factor whose levels differ from
//! the declared levels, a `hashmap()` in unsorted order, a tibble, and the
//! others. The stored forms are listed in `values.rs`.
//!
//! Compiled rules and patterns are kept between constructions in a
//! [`RuleCache`], cleared when a declaration changes the registry.

use std::collections::HashMap;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Mutex, OnceLock};

use extendr_api::SEXP;
use extendr_api::prelude::*;
use extendr_api::{Error, Result};
use extendr_ffi::{
    INTEGER, LOGICAL, R_xlen_t, REAL, Rf_allocVector, Rf_mkCharLenCE, Rf_protect, Rf_setAttrib,
    Rf_unprotect, SET_STRING_ELT, SET_VECTOR_ELT, SEXPTYPE, STRING_ELT, VECTOR_ELT, cetype_t,
};
use rtemis_r8::access::Access;
use rtemis_r8::ir::{FieldDef, FieldType, TypeDef, VariantShape};
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
pub(crate) fn with_cache<T>(f: impl FnOnce(&mut RuleCache) -> T) -> T {
    static CACHE: OnceLock<Mutex<(u64, RuleCache)>> = OnceLock::new();
    let m = CACHE.get_or_init(|| Mutex::new((0, RuleCache::new())));
    let mut guard = m.lock().unwrap_or_else(|e| e.into_inner());
    let g = GENERATION.load(Ordering::Relaxed);
    if guard.0 != g {
        guard.1.clear();
        guard.0 = g;
    }
    f(&mut guard.1)
}

/// An R object kept between calls; R runs these calls on one thread.
struct Kept(Robj);
// SAFETY: R8's native functions run on R's main thread only.
unsafe impl Send for Kept {}

/// The R form of the declared default `d` of a field of `ft`, decoded from
/// the IR document once per registry and shared by every value that takes
/// it. A value that modifies the field copies it first, since the cache
/// holds a reference.
pub(crate) fn default_r(
    d: &serde_json::Value,
    ft: &FieldType,
    lookup: &dyn TypeLookup,
) -> Result<SEXP> {
    static CACHE: OnceLock<Mutex<(u64, HashMap<usize, Kept>)>> = OnceLock::new();
    let m = CACHE.get_or_init(|| Mutex::new((0, HashMap::new())));
    let mut guard = m.lock().unwrap_or_else(|e| e.into_inner());
    let g = GENERATION.load(Ordering::Relaxed);
    if guard.0 != g {
        guard.1.clear();
        guard.0 = g;
    }
    let key = d as *const serde_json::Value as usize;
    if let Some(r) = guard.1.get(&key) {
        return Ok(unsafe { r.0.get() });
    }
    let r = crate::values::decode_field(d, ft, lookup)?;
    let s = unsafe { r.get() };
    guard.1.insert(key, Kept(r));
    Ok(s)
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

fn mkchar(s: &str) -> SEXP {
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
    let s = unsafe { std::slice::from_raw_parts(REAL(x), xlen(x)) };
    s.iter().all(|&v| {
        if v.is_nan() {
            is_na_real(v)
        } else {
            ((v * 1e6).round() as i64) as f64 / 1e6 == v
        }
    })
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
    let number = |x: SEXP, i: usize| -> Option<f64> {
        match sexptype(x) {
            SEXPTYPE::REALSXP => {
                let d = unsafe { *REAL(x).add(i) };
                (!is_na_real(d)).then_some(d)
            }
            SEXPTYPE::INTSXP | SEXPTYPE::LGLSXP => {
                let v = unsafe { *INTEGER(x).add(i) };
                (v != NA_INT).then_some(f64::from(v))
            }
            _ => None,
        }
    };
    unsafe {
        let v = match inner {
            FieldType::Logical => {
                let v = Rf_protect(Rf_allocVector(SEXPTYPE::LGLSXP, n));
                for (k, &(x, i)) in items.iter().enumerate() {
                    *LOGICAL(v).add(k) = if sexptype(x) == SEXPTYPE::LGLSXP {
                        *LOGICAL(x).add(i)
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
                        SEXPTYPE::REALSXP => *REAL(x).add(i),
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
        FieldType::Raw | FieldType::R { .. } | FieldType::Named { .. } | FieldType::Dyn { .. } => {
            None
        }
        FieldType::Vec { element, .. } => {
            let inner = unwrap_option(element);
            if is_scalar(inner) {
                atomic(x, inner)
            } else {
                list_items(x, element, lookup)?
            }
        }
        FieldType::Factor { levels, ordered } => factor(x, levels.as_deref(), *ordered),
        FieldType::Hashmap { value, .. } => map(x, value, false, lookup)?,
        FieldType::OrderedMap { value, .. } => map(x, value, true, lookup)?,
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

fn factor(x: SEXP, declared: Option<&[String]>, ordered: bool) -> Option<Robj> {
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
    let same_levels = declared.is_none_or(|d| d == current.as_slice());
    if same_levels && class_is(x, class) && attrib_count(x) == 2 {
        return None;
    }
    let codes = ints(x);
    let map: Vec<i32> = match declared {
        Some(d) => current
            .iter()
            .map(|l| {
                d.iter()
                    .position(|x| x == l)
                    .map_or(NA_INT, |k| k as i32 + 1)
            })
            .collect(),
        None => (1..=current.len() as i32).collect(),
    };
    unsafe {
        let v = Rf_protect(Rf_allocVector(SEXPTYPE::INTSXP, codes.len() as R_xlen_t));
        for (k, &c) in codes.iter().enumerate() {
            *INTEGER(v).add(k) = if c == NA_INT {
                NA_INT
            } else {
                map[(c - 1) as usize]
            };
        }
        let r = own(v);
        Rf_unprotect(1);
        let lv: Vec<&str> = match declared {
            Some(d) => d.iter().map(String::as_str).collect(),
            None => current.iter().map(String::as_str).collect(),
        };
        set_attr(&r, sym_levels(), character(&lv).get());
        set_attr(&r, sym_class(), character(class).get());
        Some(r)
    }
}

/// A `hashmap()` (`ordered = false`), stored with its keys sorted, as a named
/// atomic vector for scalar values and a named list otherwise; or an
/// `ordered_map()`, stored as a named list in its order.
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

/// A table, stored as a `data.frame` with the compact row names
/// `c(NA, -n)` and its columns in their stored forms.
fn table(x: SEXP, columns: &[FieldDef]) -> Result<Option<Robj>> {
    let n = table_rows(x).map_err(|f| fail(f.message))? as i32;
    let names = attr(x, sym_names());
    let k = xlen(x);
    let mut changed: Vec<(usize, Robj)> = Vec::new();
    for i in 0..k {
        let col = unsafe { VECTOR_ELT(x, i as R_xlen_t) };
        let name = string_elt(names, i);
        let ft = match columns
            .iter()
            .find(|c| Some(c.name.as_str()) == name.as_deref())
        {
            Some(c) => c.ty.clone(),
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
    let rn = attrib_raw(x, sym_row_names());
    let compact = sexptype(rn) == SEXPTYPE::INTSXP && ints(rn) == [NA_INT, -n];
    if changed.is_empty() && compact && class_is(x, &["data.frame"]) && attrib_count(x) == 3 {
        return Ok(None);
    }
    unsafe {
        let v = Rf_protect(Rf_allocVector(SEXPTYPE::VECSXP, k as R_xlen_t));
        for i in 0..k {
            SET_VECTOR_ELT(v, i as R_xlen_t, VECTOR_ELT(x, i as R_xlen_t));
        }
        for (i, r) in &changed {
            SET_VECTOR_ELT(v, *i as R_xlen_t, r.get());
        }
        let r = own(v);
        Rf_unprotect(1);
        set_attr(&r, sym_names(), names);
        let rn = Rf_protect(Rf_allocVector(SEXPTYPE::INTSXP, 2));
        *INTEGER(rn) = NA_INT;
        *INTEGER(rn).add(1) = -n;
        set_attr(&r, sym_row_names(), rn);
        Rf_unprotect(1);
        set_attr(&r, sym_class(), character(&["data.frame"]).get());
        Ok(Some(r))
    }
}

pub(crate) fn errors_list(errors: &[ValueError]) -> List {
    let code: Vec<String> = errors.iter().map(|e| e.code.to_string()).collect();
    let path: Vec<String> = errors.iter().map(|e| e.path.clone()).collect();
    let rule: Vec<String> = errors
        .iter()
        .map(|e| e.rule.as_ref().map(|r| r.to_string()).unwrap_or_default())
        .collect();
    let message: Vec<String> = errors.iter().map(|e| e.message.clone()).collect();
    list!(code = code, path = path, rule = rule, message = message)
}

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
    let (defs, version, variant): (&[FieldDef], u32, Option<&str>) = match def {
        TypeDef::Struct(s) => (&s.fields, s.version, None),
        TypeDef::Enum(e) => {
            let shape = e
                .variants
                .iter()
                .find(|v| v.name.as_str() == variant)
                .map(|v| &v.shape);
            let defs: &[FieldDef] = match shape {
                Some(VariantShape::Struct { fields, .. }) => fields,
                Some(VariantShape::Newtype { of }) => match lookup.find(of) {
                    Some(TypeDef::Struct(s)) => &s.fields,
                    _ => &[],
                },
                _ => &[],
            };
            (defs, e.version, Some(variant))
        }
    };
    let n = defs.len();
    // Objects created here, kept protected until they are in the value.
    let mut created: Vec<Robj> = Vec::new();
    unsafe {
        let out = Rf_protect(Rf_allocVector(SEXPTYPE::VECSXP, n as R_xlen_t));
        let names = Rf_protect(Rf_allocVector(SEXPTYPE::STRSXP, n as R_xlen_t));
        for (k, f) in defs.iter().enumerate() {
            SET_STRING_ELT(names, k as R_xlen_t, mkchar(f.name.as_str()));
            let x = list_member_at(fields, k, f.name.as_str()).unwrap_or(nil());
            let value = if is_null(x) {
                match &f.default {
                    Some(d) => default_r(d, &f.ty, lookup)?,
                    None => nil(),
                }
            } else {
                match normalize(x, &f.ty, lookup)? {
                    Some(r) => {
                        let s = r.get();
                        created.push(r);
                        s
                    }
                    None => x,
                }
            };
            SET_VECTOR_ELT(out, k as R_xlen_t, value);
        }
        Rf_setAttrib(out, sym_names(), names);
        let qname = q.to_string();
        let class = Rf_protect(Rf_allocVector(SEXPTYPE::STRSXP, 2));
        let qchar = mkchar(&qname);
        SET_STRING_ELT(class, 0, qchar);
        SET_STRING_ELT(class, 1, mkchar("r8_value"));
        Rf_setAttrib(out, sym_class(), class);
        let ty = Rf_protect(Rf_allocVector(SEXPTYPE::STRSXP, 1));
        SET_STRING_ELT(ty, 0, qchar);
        Rf_setAttrib(out, sym_r8_type(), ty);
        let ver = Rf_protect(Rf_allocVector(SEXPTYPE::INTSXP, 1));
        *INTEGER(ver) = version as i32;
        Rf_setAttrib(out, crate::rview::sym_r8_version(), ver);
        let mut protected = 5;
        if let Some(v) = variant {
            let s = Rf_protect(Rf_allocVector(SEXPTYPE::STRSXP, 1));
            SET_STRING_ELT(s, 0, mkchar(v));
            Rf_setAttrib(out, sym_r8_variant(), s);
            protected += 1;
        }
        let r = own(out);
        Rf_unprotect(protected);
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
fn r8_new_value(fields: Robj, type_name: &str, variant: &str) -> Result<Robj> {
    let q = QualifiedName::parse(type_name).map_err(|e| fail(e.to_string()))?;
    let docs = registry()
        .lock()
        .map_err(|_| fail("the registry is unavailable"))?;
    let x = unsafe { fields.get() };
    let errors = with_cache(|cache| validate_access(RView::Draft(x, variant), &q, &*docs, cache));
    if !errors.is_empty() {
        return problems(&errors);
    }
    assemble(x, &q, variant, &*docs)
}

/// `list(errors = <problems>)` with the class `r8_problems`, which no R8
/// value carries.
pub(crate) fn problems(errors: &[ValueError]) -> Result<Robj> {
    let mut r: Robj = list!(errors = errors_list(errors)).into();
    r.set_class(["r8_problems"])?;
    Ok(r)
}

/// Replaces the field `name` of the R8 value `x` with `value`, returning a
/// new value: the field is validated against its type, and the type's rules
/// that read it run on the new value. The other fields and the attributes
/// are shared with `x`.
///
/// Returns the new value; `list(errors = <problems>)` with the class
/// `r8_problems`; or `NULL` when `name` is not a field.
///
/// @param x An R8 value.
/// @param name Character: The field name.
/// @param value The new field value.
/// @return The new value, the problems, or `NULL`.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_set_field(x: Robj, name: &str, value: Robj) -> Result<Robj> {
    let xs = unsafe { x.get() };
    let type_name = attr_string(xs, sym_r8_type()).ok_or_else(|| fail("not an R8 value"))?;
    let q = QualifiedName::parse(&type_name).map_err(|e| fail(e.to_string()))?;
    let variant = attr_string(xs, sym_r8_variant());
    let docs = registry()
        .lock()
        .map_err(|_| fail("the registry is unavailable"))?;
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
                .map(|x| &x.shape)
            {
                Some(VariantShape::Struct { fields, rules }) => (fields, rules),
                Some(VariantShape::Newtype { of }) => match lookup.find(of) {
                    Some(TypeDef::Struct(s)) => (&s.fields, &s.rules),
                    _ => (&[], &[]),
                },
                _ => (&[], &[]),
            }
        }
    };
    let Some(k) = fields.iter().position(|f| f.name.as_str() == name) else {
        return Ok(().into());
    };
    let f = &fields[k];
    let vs = unsafe { value.get() };
    let at = format!("/{}", name.replace('~', "~0").replace('/', "~1"));
    // The field's new value: validated, in its stored form, or its default.
    let mut created: Option<Robj> = None;
    let stored = if is_null(vs) {
        match &f.default {
            Some(d) => default_r(d, &f.ty, lookup)?,
            None if matches!(f.ty, FieldType::Option { .. }) => nil(),
            None => {
                return problems(&[ValueError {
                    code: "missing_field",
                    path: at,
                    rule: None,
                    message: format!("field `{name}` is required"),
                }]);
            }
        }
    } else {
        let mut errors = with_cache(|cache| {
            rtemis_r8::validate::validate_field_access(RView::Whole(vs), &f.ty, lookup, cache)
        });
        if !errors.is_empty() {
            for e in &mut errors {
                e.path = format!("{at}{}", e.path);
            }
            return problems(&errors);
        }
        match normalize(vs, &f.ty, lookup)? {
            Some(r) => {
                let s = unsafe { r.get() };
                created = Some(r);
                s
            }
            None => vs,
        }
    };
    let n = xlen(xs);
    let out = unsafe {
        let out = Rf_protect(Rf_allocVector(SEXPTYPE::VECSXP, n as R_xlen_t));
        for i in 0..n {
            SET_VECTOR_ELT(out, i as R_xlen_t, VECTOR_ELT(xs, i as R_xlen_t));
        }
        SET_VECTOR_ELT(out, k as R_xlen_t, stored);
        for sym in [
            sym_names(),
            sym_class(),
            sym_r8_type(),
            crate::rview::sym_r8_version(),
            sym_r8_variant(),
        ] {
            let a = attr(xs, sym);
            if !is_null(a) {
                Rf_setAttrib(out, sym, a);
            }
        }
        let r = own(out);
        Rf_unprotect(1);
        r
    };
    drop(created);
    // The rules that read the field.
    let reading: Vec<&rtemis_r8::ir::RuleDef> = rules
        .iter()
        .filter(|r| {
            rtemis_r8::rules::reads(&r.invariant)
                .iter()
                .any(|x| x == name)
                || r.when
                    .as_ref()
                    .is_some_and(|w| rtemis_r8::rules::reads(w).iter().any(|x| x == name))
        })
        .collect();
    if !reading.is_empty() {
        let os = unsafe { out.get() };
        let mut errors = Vec::new();
        for r in reading {
            match rtemis_r8::rules::CompiledRule::compile(r, fields, lookup) {
                Ok(rule) => match rule.check_access(RView::Whole(os), fields, lookup) {
                    Ok(rtemis_r8::rules::Outcome::Holds) => {}
                    Ok(rtemis_r8::rules::Outcome::Violated(paths)) => {
                        for p in paths {
                            errors.push(ValueError {
                                code: "rule_violation",
                                path: p,
                                rule: Some(rule.id.clone()),
                                message: rule.message.clone(),
                            });
                        }
                    }
                    Err(e) => errors.push(ValueError {
                        code: "rule_error",
                        path: String::new(),
                        rule: Some(rule.id.clone()),
                        message: e,
                    }),
                },
                Err(_) => errors.push(ValueError {
                    code: "rule_invalid",
                    path: String::new(),
                    rule: Some(r.id.clone()),
                    message: format!("rule `{}` does not compile; check the declaration", r.id),
                }),
            }
        }
        if !errors.is_empty() {
            return problems(&errors);
        }
    }
    Ok(out)
}

extendr_module! {
    mod construct;
    fn r8_new_value;
    fn r8_set_field;
}
