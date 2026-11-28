//! Values of named tables: the columns of a value, its identity, and the
//! operations on its columns and its rows that keep a valid value valid.
//!
//! spec: R8/raven#values

use std::collections::{HashMap, HashSet};

use extendr_api::SEXP;
use extendr_api::prelude::*;
use extendr_api::{Error, Result};
use extendr_ffi::{
    DATAPTR_OR_NULL, INTEGER, INTEGER_ELT, LOGICAL, LOGICAL_ELT, R_xlen_t, REAL, REAL_ELT,
    Rf_allocVector, Rf_duplicate, Rf_protect, Rf_unprotect, SET_STRING_ELT, SET_VECTOR_ELT,
    SEXPTYPE, STRING_ELT, VECTOR_ELT,
};
use rtemis_r8::ir::{FieldType, TableDef, TypeDef};
use rtemis_r8::name::QualifiedName;
use rtemis_r8::progress::{self, tick, tick_block};
use rtemis_r8::rules::{NotChosen, TypeLookup, choose_rows};
use rtemis_r8::validate::{ValueError, validate_access, validate_field_access};

use crate::construct::{atomic, factor, problems, with_cache};
use crate::declare::registry;
use crate::rview::{
    MAX_ROWS, RView, attr, inherits, is_null, list_rows, sexptype, string_elt, strings, sym_names,
    too_many_rows, xlen,
};

#[allow(improper_ctypes)]
unsafe extern "C" {
    fn SHALLOW_DUPLICATE_ATTRIB(to: SEXP, from: SEXP);
}

thread_local! {
    /// The threads that the current call of a table function may use.
    static THREADS: std::cell::Cell<usize> = const { std::cell::Cell::new(1) };
}

/// The threads that the current call may use.
fn threads() -> usize {
    THREADS.get()
}

/// `f` with `n` threads for the current call.
fn with_threads<T>(n: i32, f: impl FnOnce() -> T) -> T {
    let before = THREADS.replace(usize::try_from(n).unwrap_or(1).max(1));
    let out = f();
    THREADS.set(before);
    out
}

/// The threads of a table function by default: half of the logical cores, at
/// most 2 where `R CMD check` limits the cores (`_R_CHECK_LIMIT_CORES_`), and
/// at most `OMP_THREAD_LIMIT`.
/// @return Integer: The threads.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_default_threads() -> i32 {
    let cores = std::thread::available_parallelism().map_or(1, |n| n.get());
    let mut n = (cores / 2).max(1);
    let limited = std::env::var("_R_CHECK_LIMIT_CORES_")
        .is_ok_and(|v| !v.is_empty() && !v.eq_ignore_ascii_case("false"));
    if limited {
        n = n.min(2);
    }
    if let Some(max) = std::env::var("OMP_THREAD_LIMIT")
        .ok()
        .and_then(|v| v.trim().parse::<usize>().ok())
        .filter(|&m| m >= 1)
    {
        n = n.min(max);
    }
    i32::try_from(n).unwrap_or(1)
}

fn fail(m: impl Into<String>) -> Error {
    Error::Other(m.into())
}

/// The type of the values without declared columns, `R8::Raven`.
fn raven() -> QualifiedName {
    QualifiedName::parse("R8::Raven").expect("a name")
}

fn own(x: SEXP) -> Robj {
    unsafe { Robj::from_sexp(x) }
}

/// The names of a list, each once by the checks of the caller.
fn names_of(x: SEXP) -> Vec<String> {
    strings(attr(x, sym_names()))
        .into_iter()
        .map(|s| s.map(|s| s.into_owned()).unwrap_or_default())
        .collect()
}

/// A named list without other attributes.
fn named_list(columns: &[SEXP], names: &[&str]) -> Robj {
    unsafe {
        let out = Rf_protect(Rf_allocVector(SEXPTYPE::VECSXP, columns.len() as R_xlen_t));
        for (i, c) in columns.iter().enumerate() {
            SET_VECTOR_ELT(out, i as R_xlen_t, *c);
        }
        let r = own(out);
        Rf_unprotect(1);
        let n = Rf_protect(crate::value::strings(names));
        extendr_ffi::Rf_setAttrib(out, sym_names(), n);
        Rf_unprotect(1);
        r
    }
}

/// The value of the table `q` over `columns`, a list of columns in their stored
/// forms, the declared ones first, whose names are `names`.
///
/// # Safety
///
/// On R's main thread, with a protected list that nothing else writes.
pub(crate) unsafe fn value(
    q: &QualifiedName,
    columns: SEXP,
    names: &[&str],
    facts: SEXP,
    lookup: &dyn TypeLookup,
) -> Result<Robj> {
    unsafe {
        let n = xlen(columns);
        let list = Rf_protect(Rf_allocVector(SEXPTYPE::VECSXP, n as R_xlen_t));
        for i in 0..n as R_xlen_t {
            SET_VECTOR_ELT(list, i, VECTOR_ELT(columns, i));
        }
        let names = Rf_protect(crate::value::strings(names));
        let id = crate::value::table_identity(q, names, facts, lookup);
        let out = id.map(|id| {
            let id = Rf_protect(id);
            let x = own(crate::value::new(list, id));
            Rf_unprotect(1);
            x
        });
        Rf_unprotect(2);
        out.map_err(|(_, m)| fail(m))
    }
}

/// The columns of `names` by their place in a value of the table `def`: the
/// declared ones in declaration order, then the others in the order given.
fn placed(def: &TableDef, names: &[&str]) -> Vec<usize> {
    let at: HashMap<&str, usize> = names.iter().enumerate().map(|(i, n)| (*n, i)).collect();
    let mut out: Vec<usize> = def
        .columns
        .iter()
        .filter_map(|c| at.get(c.name.as_str()).copied())
        .collect();
    let declared: HashSet<&str> = def.columns.iter().map(|c| c.name.as_str()).collect();
    out.extend((0..names.len()).filter(|&i| !declared.contains(names[i])));
    out
}

/// The facts of `facts`, a named list or `NULL`, of the columns of `names` that
/// `def` does not declare; `NULL` where none is left.
fn facts_of(facts: SEXP, def: &TableDef, names: &[&str]) -> Robj {
    if is_null(facts) || xlen(facts) == 0 {
        return ().into();
    }
    let held: HashSet<&str> = names.iter().copied().collect();
    let keys = names_of(facts);
    let (mut cols, mut kept) = (Vec::new(), Vec::new());
    for (i, k) in keys.iter().enumerate() {
        let of = unsafe { VECTOR_ELT(facts, i as R_xlen_t) };
        if held.contains(k.as_str())
            && !def.columns.iter().any(|c| c.name.as_str() == k)
            && sexptype(of) == SEXPTYPE::VECSXP
            && xlen(of) > 0
        {
            cols.push(of);
            kept.push(k.as_str());
        }
    }
    if kept.is_empty() {
        return ().into();
    }
    named_list(&cols, &kept)
}

/// The facts of `facts`, a named list or `NULL`, without those of `names`.
fn facts_without(facts: SEXP, names: &HashSet<&str>) -> Robj {
    if is_null(facts) {
        return ().into();
    }
    let keys = names_of(facts);
    let kept: Vec<usize> = (0..keys.len())
        .filter(|&i| !names.contains(keys[i].as_str()))
        .collect();
    let of: Vec<SEXP> = kept
        .iter()
        .map(|&i| unsafe { VECTOR_ELT(facts, i as R_xlen_t) })
        .collect();
    let names: Vec<&str> = kept.iter().map(|&i| keys[i].as_str()).collect();
    named_list(&of, &names)
}

/// The value of the table `q` over the validated columns of `fields`, a named
/// list: each column in its stored form, and a vector of its own with
/// `own_vectors`, the declared columns first.
pub(crate) fn assemble(
    fields: SEXP,
    q: &QualifiedName,
    def: &TableDef,
    facts: SEXP,
    own_vectors: bool,
    lookup: &dyn TypeLookup,
) -> Result<Robj> {
    let given = names_of(fields);
    let given: Vec<&str> = given.iter().map(String::as_str).collect();
    let order = placed(def, &given);
    let mut held: Vec<Robj> = Vec::with_capacity(order.len());
    for &i in &order {
        let col = unsafe { VECTOR_ELT(fields, i as R_xlen_t) };
        let ty = match def.columns.iter().find(|c| c.name.as_str() == given[i]) {
            Some(c) => Some(c.ty.clone()),
            None => crate::rview::infer_column(col),
        };
        let inner = match &ty {
            Some(FieldType::Option { inner, .. }) => Some(inner.as_ref()),
            other => other.as_ref(),
        };
        let stored = match inner {
            Some(FieldType::Factor { levels, ordered }) => {
                factor(col, levels.as_deref(), *ordered, own_vectors)
            }
            Some(t) => atomic(col, t, own_vectors),
            None => return Err(fail(format!("column `{}` has no type", given[i]))),
        };
        held.push(match stored {
            Some(r) => r,
            None if own_vectors => unsafe {
                let r = own(Rf_protect(Rf_duplicate(col)));
                Rf_unprotect(1);
                r
            },
            None => own(crate::value::viewed(col)),
        });
    }
    let names: Vec<&str> = order.iter().map(|&i| given[i]).collect();
    let facts = facts_of(facts, def, &names);
    let list = List::from_values(&held);
    unsafe { value(q, list.get(), &names, facts.get(), lookup) }
}

/// The value of the table `q` of the columns of `fields`, whose names are
/// `names`, or the problems of one.
pub(crate) fn checked(
    fields: SEXP,
    names: &[String],
    q: &QualifiedName,
    def: &TableDef,
    facts: SEXP,
    lookup: &dyn TypeLookup,
) -> Result<std::result::Result<Robj, Vec<ValueError>>> {
    let cols: Vec<SEXP> = (0..xlen(fields) as R_xlen_t)
        .map(|i| unsafe { VECTOR_ELT(fields, i) })
        .collect();
    let names: Vec<&str> = names.iter().map(String::as_str).collect();
    let draft = named_list(&cols, &names);
    let errors = with_cache(|cache| {
        validate_access(RView::Draft(unsafe { draft.get() }, ""), q, lookup, cache)
    });
    if !errors.is_empty() {
        return Ok(Err(errors));
    }
    assemble(unsafe { draft.get() }, q, def, facts, false, lookup).map(Ok)
}

/// A table value, its type, and the registry.
struct Held {
    q: QualifiedName,
    docs: std::sync::Arc<rtemis_r8::rules::DocumentSet>,
    names: Vec<String>,
    columns: SEXP,
    facts: SEXP,
}

impl Held {
    /// The table value `x`; the text of what refuses another object.
    fn of(x: SEXP) -> std::result::Result<Held, String> {
        let name = crate::value::type_name(x).ok_or("the object is not an R8 value")?;
        let q = QualifiedName::parse(&name).map_err(|e| e.to_string())?;
        let docs = registry().snapshot();
        if !matches!(docs.find(&q), Some(TypeDef::Table(_))) {
            return Err(format!("`{q}` is not a table"));
        }
        if let Some(o) = crate::value::other(x) {
            return Err(format!("the value was {}", o.message()));
        }
        Ok(Held {
            q,
            docs,
            names: names_of(x),
            columns: crate::value::fields(x),
            facts: crate::value::table_facts(x),
        })
    }

    fn def(&self) -> &TableDef {
        match self.docs.find(&self.q) {
            Some(TypeDef::Table(t)) => t,
            _ => unreachable!("checked by `of`"),
        }
    }

    fn column(&self, i: usize) -> SEXP {
        unsafe { VECTOR_ELT(self.columns, i as R_xlen_t) }
    }
}

/// A value of the table `type_name` of the columns of `columns`, a named list of
/// vectors, or the problems of one.
///
/// @param columns List: The columns by name.
/// @param type_name Character: The qualified type name.
/// @param own Logical: Copy each vector, for columns that a data.table holds.
/// @param facts List or `NULL`: The units and descriptions of extra columns.
/// @return The value or the problems.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_table_new(
    columns: Robj,
    type_name: crate::utf8::Utf8,
    own: bool,
    facts: Robj,
) -> Result<Robj> {
    crate::interrupt::interruptible(unsafe { columns.get() }, || {
        let q = QualifiedName::parse(&type_name).map_err(|e| fail(e.to_string()))?;
        let docs = registry().snapshot();
        let Some(TypeDef::Table(def)) = docs.find(&q) else {
            return problems(&[ValueError {
                code: "type_unregistered",
                message: format!("`{q}` is not registered"),
                ..Default::default()
            }]);
        };
        let xs = unsafe { columns.get() };
        let errors = with_cache(|cache| validate_access(RView::Draft(xs, ""), &q, &*docs, cache));
        if !errors.is_empty() {
            return problems(&errors);
        }
        assemble(xs, &q, def, unsafe { facts.get() }, own, &*docs)
    })
}

/// Whether the columns of `names` hold each required column of `def` and each
/// that a rule reads.
fn satisfies(def: &TableDef, names: &HashSet<&str>) -> bool {
    def.columns
        .iter()
        .all(|c| !c.required || names.contains(c.name.as_str()))
        && def.rules.iter().all(|r| {
            let mut read = rtemis_r8::rules::reads(&r.invariant);
            if let Some(w) = &r.when {
                read.extend(rtemis_r8::rules::reads(w));
            }
            read.iter().all(|n| {
                names.contains(n.as_str()) || !def.columns.iter().any(|c| c.name.as_str() == n)
            })
        })
}

/// The facts of a value that becomes a raven: its own, and the units and
/// descriptions that its type declares.
fn all_facts(h: &Held, names: &[&str]) -> Result<Robj> {
    let mut keys: Vec<String> = Vec::new();
    let mut of: Vec<Robj> = Vec::new();
    for n in names {
        let Some(c) = h.def().columns.iter().find(|c| c.name.as_str() == *n) else {
            continue;
        };
        let (mut k, mut v): (Vec<&str>, Vec<Robj>) = (Vec::new(), Vec::new());
        if let Some(u) = &c.unit {
            k.push("unit");
            v.push(u.to_string().into());
        }
        if !c.description.is_empty() {
            k.push("description");
            v.push(c.description.to_string().into());
        }
        if !k.is_empty() {
            let mut l: Robj = List::from_values(v).into();
            l.set_names(k)?;
            keys.push(n.to_string());
            of.push(l);
        }
    }
    if !is_null(h.facts) {
        for (i, k) in names_of(h.facts).into_iter().enumerate() {
            keys.push(k);
            of.push(own(unsafe { VECTOR_ELT(h.facts, i as R_xlen_t) }));
        }
    }
    if keys.is_empty() {
        return Ok(().into());
    }
    let mut r: Robj = List::from_values(of).into();
    r.set_names(keys)?;
    Ok(r)
}

/// The value of the columns at `at`, 0-based, over the columns of `columns`: of
/// the type of `h` when they satisfy it, and otherwise a raven.
fn with_columns(h: &Held, at: &[usize], columns: &dyn Fn(usize) -> SEXP) -> Result<Robj> {
    let names: Vec<&str> = at.iter().map(|&i| h.names[i].as_str()).collect();
    let set: HashSet<&str> = names.iter().copied().collect();
    let keeps = satisfies(h.def(), &set);
    let facts = if keeps {
        own(h.facts)
    } else {
        all_facts(h, &names)?
    };
    let (q, def) = if keeps {
        (h.q.clone(), h.def())
    } else {
        let q = raven();
        match h.docs.find(&q) {
            Some(TypeDef::Table(t)) => (q, t),
            _ => return Err(fail("`R8::Raven` is not registered")),
        }
    };
    let order = placed(def, &names);
    let list: Vec<SEXP> = order.iter().map(|&k| columns(at[k])).collect();
    let names: Vec<&str> = order.iter().map(|&k| names[k]).collect();
    let facts = facts_of(unsafe { facts.get() }, def, &names);
    let list = named_list(&list, &names);
    unsafe { value(&q, list.get(), &names, facts.get(), &*h.docs) }
}

/// The 0-based positions of the `TRUE` of a logical vector; `None` where one is
/// missing.
fn set_positions(given: &[i32]) -> Option<Vec<u32>> {
    let (mut set, mut missing) = (0usize, false);
    for &v in given {
        set += usize::from(v != 0);
        missing |= v == i32::MIN;
    }
    if missing {
        return None;
    }
    // Each position is written, and kept where the value is set.
    let mut out = vec![0u32; set + 1];
    let mut at = 0usize;
    for (k, &v) in given.iter().enumerate() {
        out[at] = k as u32;
        at += usize::from(v != 0);
    }
    out.truncate(set);
    Some(out)
}

/// The elements that `indx` asks for of the numbers of `v`, for `x[indx]` of a view
/// of `v` (`field.c`): R gives positions from 1, and a missing one and one past the
/// end are missing elements. NULL for another vector, which R extracts.
///
/// # Safety
///
/// On R's main thread, with protected vectors.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn r8_extract(v: SEXP, indx: SEXP) -> SEXP {
    const NA_DOUBLE: u64 = rtemis_r8::abi::NA_DOUBLE_BITS;
    unsafe {
        let t = sexptype(v);
        let (p, q) = (DATAPTR_OR_NULL(v), DATAPTR_OR_NULL(indx));
        let numbers = matches!(t, SEXPTYPE::REALSXP | SEXPTYPE::INTSXP | SEXPTYPE::LGLSXP);
        if !numbers || p.is_null() || q.is_null() {
            return std::ptr::null_mut();
        }
        let (n, m) = (xlen(v), xlen(indx));
        let out = Rf_allocVector(t, m as R_xlen_t);
        // The 0-based position of element `k` of the index, or `n` for none.
        macro_rules! take {
            ($from:expr, $to:expr, $na:expr) => {{
                let (from, to) = ($from, $to);
                if sexptype(indx) == SEXPTYPE::INTSXP {
                    let at = std::slice::from_raw_parts(q.cast::<i32>(), m);
                    for (k, &i) in at.iter().enumerate() {
                        let i = i as u32 as usize;
                        *to.add(k) = if i.wrapping_sub(1) < n && i <= i32::MAX as usize {
                            *from.add(i - 1)
                        } else {
                            $na
                        };
                    }
                } else {
                    let at = std::slice::from_raw_parts(q.cast::<f64>(), m);
                    for (k, &i) in at.iter().enumerate() {
                        *to.add(k) = if i >= 1.0 && i < n as f64 + 1.0 {
                            *from.add(i as usize - 1)
                        } else {
                            $na
                        };
                    }
                }
            }};
        }
        match t {
            SEXPTYPE::REALSXP => take!(p.cast::<f64>(), REAL(out), f64::from_bits(NA_DOUBLE)),
            SEXPTYPE::INTSXP => take!(p.cast::<i32>(), INTEGER(out), i32::MIN),
            _ => take!(p.cast::<i32>(), LOGICAL(out), i32::MIN),
        }
        out
    }
}

/// The least or the greatest of the numbers of `v`, for `min()` and `max()` of a view
/// of `v`: as doubles into `out`, and 1. 0 for a vector without elements, with a
/// missing one, or without an array, which R reads. The result is exact, so that it
/// is R's.
///
/// # Safety
///
/// On R's main thread; `out` is writable.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn r8_extreme(v: SEXP, greatest: i32, out: *mut f64) -> i32 {
    let p = unsafe { DATAPTR_OR_NULL(v) };
    let n = xlen(v);
    if p.is_null() || n == 0 {
        return 0;
    }
    const LANES: usize = 16;
    let found = match sexptype(v) {
        SEXPTYPE::REALSXP => {
            let x = unsafe { std::slice::from_raw_parts(p.cast::<f64>(), n) };
            let sign = if greatest != 0 { -1.0 } else { 1.0 };
            let mut least = [f64::INFINITY; LANES];
            let mut nan = false;
            let (chunks, rest) = x.as_chunks::<LANES>();
            for &v in rest {
                nan |= v.is_nan();
                least[0] = if sign * v < least[0] {
                    sign * v
                } else {
                    least[0]
                };
            }
            for c in chunks {
                for (l, &v) in least.iter_mut().zip(c) {
                    nan |= v.is_nan();
                    *l = if sign * v < *l { sign * v } else { *l };
                }
            }
            // Which of the two zeros is the least is R's to say.
            (!nan)
                .then(|| sign * least.iter().copied().fold(f64::INFINITY, f64::min))
                .filter(|x| *x != 0.0)
        }
        SEXPTYPE::INTSXP => {
            let x = unsafe { std::slice::from_raw_parts(p.cast::<i32>(), n) };
            let (mut low, mut high) = (i32::MAX, i32::MIN);
            for &v in x {
                low = low.min(v);
                high = high.max(v);
            }
            // `NA` is the least integer.
            (low != i32::MIN).then_some(f64::from(if greatest != 0 { high } else { low }))
        }
        _ => None,
    };
    match found {
        Some(x) => {
            unsafe { *out = x };
            1
        }
        None => 0,
    }
}

/// A number as R writes it.
fn number_text(x: f64) -> String {
    match rtemis_r8::rules::Shown::number(x) {
        rtemis_r8::rules::Shown::Number(t) => t,
        _ => x.to_string(),
    }
}

/// What an index asks for, or why it asks for nothing.
type Asked = std::result::Result<Vec<u32>, String>;

/// The 0-based positions that the index `i` asks for among `n` of `what`:
/// positive or negative whole numbers, or a logical vector of length `n`.
fn positions(i: SEXP, n: usize, what: &str) -> Asked {
    let len = xlen(i);
    let count = |n: usize| format!("{n} {what}{}", if n == 1 { "" } else { "s" });
    if sexptype(i) == SEXPTYPE::LGLSXP {
        if len != n {
            return Err(format!(
                "a logical index has one value per {what}; it has {len}, and there are {}",
                count(n)
            ));
        }
        let p = unsafe { DATAPTR_OR_NULL(i) };
        if !p.is_null() {
            let given = unsafe { std::slice::from_raw_parts(p.cast::<i32>(), len) };
            if let Some(out) = set_positions(given) {
                return Ok(out);
            }
        }
        let mut out = Vec::with_capacity(n);
        for k in 0..len {
            match unsafe { LOGICAL_ELT(i, k as R_xlen_t) } {
                0 => {}
                v if v == i32::MIN => {
                    return Err(format!("element {} of the index is NA", k + 1));
                }
                _ => out.push(k as u32),
            }
        }
        return Ok(out);
    }
    let doubles = match sexptype(i) {
        SEXPTYPE::INTSXP if !inherits(i, "factor") => false,
        SEXPTYPE::REALSXP => true,
        _ => {
            return Err(format!(
                "a {what} is asked for by position, or with a logical vector"
            ));
        }
    };
    let p = unsafe { DATAPTR_OR_NULL(i) };
    if !doubles && !p.is_null() {
        // The common case, positive positions, in one pass.
        let given = unsafe { std::slice::from_raw_parts(p.cast::<i32>(), len) };
        let top = u32::try_from(n).unwrap_or(u32::MAX);
        if given.iter().all(|&v| (v as u32).wrapping_sub(1) < top) {
            return Ok(given.iter().map(|&v| v as u32 - 1).collect());
        }
    }
    let mut out: Vec<u32> = Vec::with_capacity(len);
    let mut drop: Vec<bool> = Vec::new();
    for k in 0..len {
        let p: i64 = if doubles {
            let d = unsafe { REAL_ELT(i, k as R_xlen_t) };
            if d.is_nan() {
                return Err(format!("element {} of the index is NA", k + 1));
            }
            if d.is_infinite() || d.fract() != 0.0 {
                return Err(format!(
                    "element {} of the index, {}, is not a whole number",
                    k + 1,
                    number_text(d)
                ));
            }
            if d.abs() > n as f64 {
                return Err(format!(
                    "the index asks for {what} {}; there are {}",
                    number_text(d),
                    count(n)
                ));
            }
            d as i64
        } else {
            match unsafe { INTEGER_ELT(i, k as R_xlen_t) } {
                i32::MIN => return Err(format!("element {} of the index is NA", k + 1)),
                v => i64::from(v),
            }
        };
        if p == 0 {
            return Err("positions begin at 1; the index holds 0".to_string());
        }
        if p.unsigned_abs() > n as u64 {
            return Err(format!(
                "the index asks for {what} {p}; there are {}",
                count(n)
            ));
        }
        if p < 0 {
            if !out.is_empty() {
                return Err("an index is all positive or all negative".to_string());
            }
            if drop.is_empty() {
                drop = vec![false; n];
            }
            drop[p.unsigned_abs() as usize - 1] = true;
        } else {
            if !drop.is_empty() {
                return Err("an index is all positive or all negative".to_string());
            }
            out.push((p - 1) as u32);
        }
    }
    if !drop.is_empty() {
        out = (0..n as u32).filter(|&k| !drop[k as usize]).collect();
    }
    Ok(out)
}

/// The 0-based positions of the columns that `j` asks for: names, or what
/// [`positions`] reads.
fn column_positions(j: SEXP, names: &[String]) -> Asked {
    if sexptype(j) != SEXPTYPE::STRSXP {
        let at = positions(j, names.len(), "column")?;
        let mut seen = HashSet::new();
        if let Some(p) = at.iter().find(|p| !seen.insert(**p)) {
            return Err(format!(
                "the index asks for column `{}` twice; the columns of a table have distinct names",
                names[*p as usize]
            ));
        }
        return Ok(at);
    }
    let index: HashMap<&str, u32> = names
        .iter()
        .enumerate()
        .map(|(i, n)| (n.as_str(), i as u32))
        .collect();
    let mut out = Vec::with_capacity(xlen(j));
    let mut seen = HashSet::new();
    for k in strings(j) {
        let k = k.ok_or_else(|| "a column name is NA".to_string())?;
        let Some(&p) = index.get(k.as_ref()) else {
            let near = rtemis_r8::name::nearest(&k, names.iter().map(String::as_str));
            return Err(match near {
                Some(n) => format!("`{k}` is not a column; the nearest column is `{n}`"),
                None if names.len() <= 8 => {
                    format!(
                        "`{k}` is not a column; the columns are `{}`",
                        names.join("`, `")
                    )
                }
                None => format!("`{k}` is not a column of the {}", names.len()),
            });
        };
        if !seen.insert(p) {
            return Err(format!(
                "the index asks for column `{k}` twice; the columns of a table have distinct names"
            ));
        }
        out.push(p);
    }
    Ok(out)
}

fn refused(message: String) -> Robj {
    let mut r: Robj = list!(message = message).into();
    let _ = r.set_class(["r8_refused"]);
    r
}

/// The 0-based positions of the rows of the table value `x` where the rule
/// `rule`, a list of one `rule()`, is true.
fn chosen(h: &Held, x: SEXP, rule: SEXP) -> Asked {
    let one = xlen(rule) == 1 && inherits(unsafe { VECTOR_ELT(rule, 0) }, "r8_rule");
    if !one {
        return Err("a list in the place of the rows holds one rule()".to_string());
    }
    let rules = crate::reader::rules(rule, "/rules").map_err(|e| e.message)?;
    let [rule] = rules.as_slice() else {
        return Err("rows are chosen by one rule".to_string());
    };
    if rule.when.is_some() {
        return Err(
            "a rule that chooses rows is one condition, without `when`; join the parts with `&&`"
                .to_string(),
        );
    }
    let found = choose_rows(RView::Whole(x), h.def(), &rule.invariant, &*h.docs);
    found.map_err(|e| match e {
        NotChosen::Rule(errors) => format!(
            "the rule does not fit the table: {}",
            errors
                .iter()
                .map(|e| {
                    let mut text = crate::fieldtype::in_r(&e.message);
                    if e.code == "rule_field" {
                        let name = text.split('`').nth(1).unwrap_or_default().to_string();
                        text.push_str(&format!(
                            "; if `{name}` is an R variable, write `val({name})`"
                        ));
                    }
                    text
                })
                .collect::<Vec<_>>()
                .join("; ")
        ),
        NotChosen::Row(e) => format!(
            "the rule has no value in row {}: {}{}",
            e.row + 1,
            e.message,
            match e.count {
                1 => String::new(),
                n => format!(
                    "; nor in {} other row{}",
                    n - 1,
                    if n == 2 { "" } else { "s" }
                ),
            }
        ),
        NotChosen::Table(m) => m,
    })
}

/// The elements between two ticks of a walk.
const WALK: usize = 1 << 14;

/// The cells that a second thread takes part in gathering from.
const CELLS_FOR_THREADS: usize = 1 << 20;

/// A column of numbers to gather: its elements, and the new vector's.
#[derive(Clone, Copy)]
enum Numbers {
    Doubles(*const f64, *mut f64),
    Integers(*const i32, *mut i32),
}

// SAFETY: a thread reads elements that no thread writes, and writes a vector that
// no other thread has.
unsafe impl Send for Numbers {}
unsafe impl Sync for Numbers {}

impl Numbers {
    /// # Safety
    ///
    /// The positions lie within the column, and the new vector has one element
    /// for each.
    unsafe fn gather(self, at: &[u32]) {
        unsafe {
            for (b, part) in at.chunks(WALK).enumerate() {
                tick_block();
                let lo = b * WALK;
                match self {
                    Numbers::Doubles(from, to) => {
                        for (i, &k) in part.iter().enumerate() {
                            *to.add(lo + i) = *from.add(k as usize);
                        }
                    }
                    Numbers::Integers(from, to) => {
                        for (i, &k) in part.iter().enumerate() {
                            *to.add(lo + i) = *from.add(k as usize);
                        }
                    }
                }
            }
        }
    }
}

/// The elements `at` of each vector of `columns`, with its attributes. The
/// positions lie within the vectors. Columns of numbers that R holds as arrays are
/// gathered by two threads when they are many.
fn gathered(columns: &[SEXP], at: &[u32]) -> Vec<Robj> {
    let m = at.len();
    let mut out = Vec::with_capacity(columns.len());
    let mut numbers: Vec<Numbers> = Vec::new();
    for &x in columns {
        let t = sexptype(x);
        unsafe {
            let v = Rf_protect(Rf_allocVector(t, m as R_xlen_t));
            let p = if t == SEXPTYPE::STRSXP {
                std::ptr::null()
            } else {
                DATAPTR_OR_NULL(x)
            };
            match t {
                SEXPTYPE::REALSXP if !p.is_null() => {
                    numbers.push(Numbers::Doubles(p.cast(), REAL(v)));
                }
                SEXPTYPE::INTSXP if !p.is_null() => {
                    numbers.push(Numbers::Integers(p.cast(), INTEGER(v)));
                }
                SEXPTYPE::LGLSXP if !p.is_null() => {
                    numbers.push(Numbers::Integers(p.cast(), LOGICAL(v)));
                }
                _ => {
                    for (i, &k) in at.iter().enumerate() {
                        if i % 1024 == 0 {
                            tick_block();
                        }
                        let (i, k) = (i as R_xlen_t, k as R_xlen_t);
                        match t {
                            SEXPTYPE::REALSXP => *REAL(v).offset(i) = REAL_ELT(x, k),
                            SEXPTYPE::INTSXP => *INTEGER(v).offset(i) = INTEGER_ELT(x, k),
                            SEXPTYPE::LGLSXP => *LOGICAL(v).offset(i) = LOGICAL_ELT(x, k),
                            _ => SET_STRING_ELT(v, i, STRING_ELT(x, k)),
                        }
                    }
                }
            }
            SHALLOW_DUPLICATE_ATTRIB(v, x);
            out.push(own(v));
            Rf_unprotect(1);
        }
    }
    let one = |ns: &[Numbers]| ns.iter().for_each(|n| unsafe { n.gather(at) });
    if threads() > 1 && numbers.len() > 1 && numbers.len() * m >= CELLS_FOR_THREADS {
        let (a, b) = numbers.split_at(numbers.len() / 2);
        std::thread::scope(|s| {
            // A platform without threads, such as webR, gathers in one.
            let other = std::thread::Builder::new().spawn_scoped(s, || one(a));
            one(b);
            if other.is_err() {
                one(a);
            }
        });
    } else {
        one(&numbers);
    }
    out
}

/// The rows `i` and the columns `j` of the table value `x`, each an index or
/// `NULL` for all of them.
///
/// @param x A value of a table.
/// @param i,j The indices; `i` is a list of one `rule()` for the rows where it is
///   true.
/// @param threads Integer: The threads that the gathering of rows may use.
/// @return The value, or an object of class `r8_refused` or `r8_problems`.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_table_get(x: Robj, i: Robj, j: Robj, threads: i32) -> Result<Robj> {
    with_threads(threads, || table_get(x, i, j))
}

fn table_get(x: Robj, i: Robj, j: Robj) -> Result<Robj> {
    crate::interrupt::interruptible(unsafe { x.get() }, || {
        let h = match Held::of(unsafe { x.get() }) {
            Ok(h) => h,
            Err(m) => return Ok(refused(m)),
        };
        let (is, js) = unsafe { (i.get(), j.get()) };
        let cols: Vec<usize> = if is_null(js) {
            (0..h.names.len()).collect()
        } else {
            match column_positions(js, &h.names) {
                Ok(at) => at.into_iter().map(|p| p as usize).collect(),
                Err(m) => return Ok(refused(m)),
            }
        };
        if is_null(is) {
            return with_columns(&h, &cols, &|k| h.column(k));
        }
        let n = list_rows(h.columns) as usize;
        let by_rule = sexptype(is) == SEXPTYPE::VECSXP;
        let asked = if by_rule {
            chosen(&h, unsafe { x.get() }, is)
        } else {
            positions(is, n, "row")
        };
        let at = match asked {
            Ok(at) => at,
            Err(m) => return Ok(refused(m)),
        };
        // A row asked for twice repeats the values of a unique column.
        let unique: Vec<&str> = h
            .def()
            .columns
            .iter()
            .filter(|c| c.unique && cols.iter().any(|&k| h.names[k] == c.name.as_str()))
            .map(|c| c.name.as_str())
            .collect();
        if !unique.is_empty() && !by_rule {
            let mut seen = vec![false; n];
            if let Some(&r) = at
                .iter()
                .find(|&&r| std::mem::replace(&mut seen[r as usize], true))
            {
                return Ok(refused(format!(
                    "the index asks for row {} twice, and the values of column `{}` are distinct; Raven(x)[i, ] gives the rows without the type",
                    r + 1,
                    unique[0]
                )));
            }
        }
        let from: Vec<SEXP> = cols.iter().map(|&k| h.column(k)).collect();
        let rows = gathered(&from, &at);
        let place: HashMap<usize, usize> = cols.iter().enumerate().map(|(i, &k)| (k, i)).collect();
        with_columns(&h, &cols, &|k| unsafe { rows[place[&k]].get() })
    })
}

/// The table value `x` with the columns of `changes` replaced, added, or, for
/// `NULL`, removed, or the problems of it. The other columns are shared, and a
/// replaced one has no unit and no description but its type's.
pub(crate) fn replaced(
    x: SEXP,
    q: &QualifiedName,
    def: &TableDef,
    changes: &[(&str, SEXP)],
    lookup: &dyn TypeLookup,
) -> Result<std::result::Result<Robj, Vec<ValueError>>> {
    let h = match Held::of(x) {
        Ok(h) => h,
        Err(m) => {
            let mut c = m.chars();
            let first: String = c.next().into_iter().flat_map(char::to_uppercase).collect();
            return Ok(Err(vec![ValueError {
                code: "declaration_changed",
                message: format!("{first}{}", c.as_str()),
                ..Default::default()
            }]));
        }
    };
    let mut names: Vec<&str> = h.names.iter().map(String::as_str).collect();
    let mut cols: Vec<SEXP> = (0..names.len()).map(|i| h.column(i)).collect();
    for (k, v) in changes {
        let at = names.iter().position(|n| n == k);
        match (at, is_null(*v)) {
            (Some(i), true) => {
                names.remove(i);
                cols.remove(i);
            }
            (Some(i), false) => cols[i] = *v,
            (None, true) => {
                return Ok(Err(vec![ValueError {
                    code: "unknown_column",
                    path: format!("/{k}"),
                    message: format!("`{k}` is not a column of the table"),
                    ..Default::default()
                }]));
            }
            (None, false) => {
                names.push(k);
                cols.push(*v);
            }
        }
    }
    if !def.open
        && let Some((k, _)) = changes
            .iter()
            .find(|(k, v)| !is_null(*v) && !def.columns.iter().any(|c| c.name.as_str() == *k))
    {
        let of: Vec<String> = def.columns.iter().map(|c| c.name.to_string()).collect();
        return Ok(Err(vec![ValueError {
            code: "unknown_column",
            path: format!("/{k}"),
            message: format!(
                "`{k}` is not a column of the table, whose columns are `{}`",
                of.join("`, `")
            ),
            ..Default::default()
        }]));
    }
    // What changed is checked with what its rules read, and the length of the
    // table; the other columns are valid.
    let changed: HashSet<&str> = changes.iter().map(|(k, _)| *k).collect();
    let mut rules = Vec::new();
    let mut read: HashSet<String> = HashSet::new();
    for r in &def.rules {
        let mut of = rtemis_r8::rules::reads(&r.invariant);
        if let Some(w) = &r.when {
            of.extend(rtemis_r8::rules::reads(w));
        }
        if of.iter().any(|n| changed.contains(n.as_str())) {
            read.extend(of);
            rules.push(r.clone());
        }
    }
    let part = |n: &str| changed.contains(n) || read.contains(n);
    let columns: Vec<_> = def
        .columns
        .iter()
        .filter(|c| part(c.name.as_str()))
        .cloned()
        .collect();
    // A column that did not change gives the length.
    let keep = names.iter().position(|n| !changed.contains(*n));
    let mut pick: Vec<usize> = keep.into_iter().collect();
    pick.extend((0..names.len()).filter(|&i| part(names[i]) && Some(i) != keep));
    let draft = named_list(
        &pick.iter().map(|&i| cols[i]).collect::<Vec<_>>(),
        &pick.iter().map(|&i| names[i]).collect::<Vec<_>>(),
    );
    let mut columns = columns;
    if let Some(k) = keep
        && !part(names[k])
        && let Some(c) = def.columns.iter().find(|c| c.name.as_str() == names[k])
    {
        // The type of the column that gives the length, without its constraints,
        // which it satisfies.
        let mut c = c.clone();
        c.unique = false;
        columns.push(c);
    }
    let ty = FieldType::Table {
        columns: columns.into_boxed_slice(),
        open: def.open,
        rules: rules.into_boxed_slice(),
    };
    let errors = with_cache(|cache| {
        validate_field_access(RView::Draft(unsafe { draft.get() }, ""), &ty, lookup, cache)
    });
    if !errors.is_empty() {
        return Ok(Err(errors));
    }
    let list = named_list(&cols, &names);
    // The unit and the description of an extra column are those of its vector.
    let facts = facts_without(h.facts, &changed);
    assemble(
        unsafe { list.get() },
        q,
        def,
        unsafe { facts.get() },
        false,
        lookup,
    )
    .map(Ok)
}

/// The table value `x` with the columns `from` named `to`, or the problems of it. A
/// value that keeps its type keeps the order of its declared columns; without a
/// column that the type requires, it is a raven, in the order it has. The facts of a
/// column go with it.
///
/// @param x A value of a table.
/// @param from The columns.
/// @param to Character: The new names.
/// @return The value, or an object of class `r8_refused` or `r8_problems`.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_table_rename(x: Robj, from: Robj, to: Robj) -> Result<Robj> {
    crate::interrupt::interruptible(unsafe { x.get() }, || renamed(&x, &from, &to))
}

/// [`r8_table_rename`].
fn renamed(x: &Robj, from: &Robj, to: &Robj) -> Result<Robj> {
    let h = match Held::of(unsafe { x.get() }) {
        Ok(h) => h,
        Err(m) => return Ok(refused(m)),
    };
    let at = match column_positions(unsafe { from.get() }, &h.names) {
        Ok(at) => at,
        Err(m) => return Ok(refused(m)),
    };
    let given = strings(unsafe { to.get() });
    if given.len() != at.len() {
        return Ok(refused(format!(
            "`to` has one name for each column of `from`; it has {}, and `from` {}",
            given.len(),
            at.len()
        )));
    }
    let mut names = h.names.clone();
    for (&k, name) in at.iter().zip(&given) {
        let name = name.as_deref().unwrap_or_default();
        if !rtemis_r8::access::is_column_name(name) {
            return Ok(refused(format!(
                "`{name}` is not a name of a column: a name is not empty and does not begin with `$`"
            )));
        }
        names[k as usize] = name.to_string();
    }
    let mut seen = HashSet::new();
    if let Some(k) = names.iter().find(|n| !seen.insert(n.as_str())) {
        return Ok(refused(format!(
            "two columns are named `{k}`; the columns of a table have distinct names"
        )));
    }
    // The facts by the new names, those of the type included.
    let old: Vec<&str> = h.names.iter().map(String::as_str).collect();
    let mut facts = all_facts(&h, &old)?;
    if !is_null(unsafe { facts.get() }) {
        let keys: Vec<String> = names_of(unsafe { facts.get() })
            .into_iter()
            .map(|k| {
                let i = old.iter().position(|o| *o == k);
                i.map_or(k, |i| names[i].clone())
            })
            .collect();
        facts.set_names(keys)?;
    }
    let refs: Vec<&str> = names.iter().map(String::as_str).collect();
    let set: HashSet<&str> = refs.iter().copied().collect();
    let keeps = satisfies(h.def(), &set)
        && (h.def().open
            || refs
                .iter()
                .all(|n| h.def().columns.iter().any(|c| c.name.as_str() == *n)));
    let (q, def) = if keeps {
        (h.q.clone(), h.def())
    } else {
        let q = raven();
        match h.docs.find(&q) {
            Some(TypeDef::Table(t)) => (q, t),
            _ => return Err(fail("`R8::Raven` is not registered")),
        }
    };
    // A column that takes a declared name is checked as that column.
    let takes = at.iter().any(|&k| {
        def.columns
            .iter()
            .any(|c| c.name.as_str() == names[k as usize])
    });
    if takes {
        return match checked(h.columns, &names, &q, def, unsafe { facts.get() }, &*h.docs)? {
            Ok(v) => Ok(v),
            Err(errors) => problems(&errors),
        };
    }
    let order = placed(def, &refs);
    let list: Vec<SEXP> = order.iter().map(|&k| h.column(k)).collect();
    let refs: Vec<&str> = order.iter().map(|&k| refs[k]).collect();
    let facts = facts_of(unsafe { facts.get() }, def, &refs);
    let list = named_list(&list, &refs);
    unsafe { value(&q, list.get(), &refs, facts.get(), &*h.docs) }
}

/// The key of a missing cell, above every other.
const LAST: u64 = u64::MAX;

/// A key of each cell of `x`, in whose order the cells are: numbers, dates, and
/// date-times by value, logicals from `FALSE`, strings by code point, and factors by
/// level; a missing cell and `NaN` last, also where the others descend.
fn sort_keys(x: SEXP, descending: bool) -> Vec<u64> {
    let n = xlen(x);
    let mut out: Vec<u64> = Vec::with_capacity(n);
    match sexptype(x) {
        SEXPTYPE::REALSXP => {
            let key = |v: f64| {
                if v.is_nan() {
                    return LAST;
                }
                // Adding 0 makes one zero of the two.
                let bits = (v + 0.0).to_bits();
                if bits >> 63 == 1 {
                    !bits
                } else {
                    bits | 1 << 63
                }
            };
            let p = unsafe { DATAPTR_OR_NULL(x) };
            if p.is_null() {
                out.extend((0..n).map(|i| key(unsafe { REAL_ELT(x, i as R_xlen_t) })));
            } else {
                let s = unsafe { std::slice::from_raw_parts(p.cast::<f64>(), n) };
                out.extend(s.iter().map(|&v| key(v)));
            }
        }
        SEXPTYPE::STRSXP => return text_keys(x, descending),
        _ => {
            let key = |v: i32| {
                if v == i32::MIN {
                    LAST
                } else {
                    (i64::from(v) - i64::from(i32::MIN)) as u64
                }
            };
            let p = unsafe { DATAPTR_OR_NULL(x) };
            if p.is_null() {
                let logical = sexptype(x) == SEXPTYPE::LGLSXP;
                out.extend((0..n as R_xlen_t).map(|i| {
                    key(if logical {
                        unsafe { LOGICAL_ELT(x, i) }
                    } else {
                        unsafe { INTEGER_ELT(x, i) }
                    })
                }));
            } else {
                let s = unsafe { std::slice::from_raw_parts(p.cast::<i32>(), n) };
                out.extend(s.iter().map(|&v| key(v)));
            }
        }
    }
    if descending {
        for k in out.iter_mut().filter(|k| **k != LAST) {
            // No key but that of a missing cell is the greatest.
            *k = LAST - 1 - *k;
        }
    }
    out
}

/// The places of the texts of a character vector by the addresses that R keeps them
/// at: one object for each text of one encoding.
struct Places {
    /// An address and one more than its place; an address of 0 is free.
    held: Vec<(usize, u32)>,
    shift: u32,
    used: usize,
}

impl Places {
    fn new() -> Places {
        Places {
            held: vec![(0, 0); 1 << 10],
            shift: 64 - 10,
            used: 0,
        }
    }

    /// The slot of `key`: its own, or the free one where it goes.
    fn slot(&self, key: usize) -> usize {
        // An address is a multiple of 8; the product spreads its bits.
        let mut at = ((key as u64 >> 3).wrapping_mul(0x9e37_79b9_7f4a_7c15) >> self.shift) as usize;
        while self.held[at].0 != key && self.held[at].0 != 0 {
            at = (at + 1) & (self.held.len() - 1);
        }
        at
    }

    /// The place of `key`, which `new` gives where there is none.
    fn place(&mut self, key: usize, new: impl FnOnce() -> u32) -> u32 {
        let at = self.slot(key);
        if self.held[at].0 == key {
            return self.held[at].1;
        }
        let place = new();
        self.held[at] = (key, place);
        self.used += 1;
        if self.used * 2 > self.held.len() {
            let old = std::mem::take(&mut self.held);
            self.held = vec![(0, 0); old.len() * 4];
            self.shift -= 2;
            for (k, v) in old.into_iter().filter(|(k, _)| *k != 0) {
                let at = self.slot(k);
                self.held[at] = (k, v);
            }
        }
        place
    }
}

/// The keys of a character vector, by the code points of its texts: each text is
/// ranked once.
fn text_keys(x: SEXP, descending: bool) -> Vec<u64> {
    let n = xlen(x);
    let mut places = Places::new();
    let mut texts: Vec<std::borrow::Cow<'_, str>> = Vec::new();
    let mut of: Vec<u32> = Vec::with_capacity(n);
    let p = unsafe { DATAPTR_OR_NULL(x) };
    for i in 0..n {
        if i % 1024 == 0 {
            tick_block();
        }
        let c = if p.is_null() {
            unsafe { STRING_ELT(x, i as R_xlen_t) }
        } else {
            unsafe { *p.cast::<SEXP>().add(i) }
        };
        of.push(places.place(c as usize, || match string_elt(x, i) {
            Some(t) => {
                texts.push(t);
                texts.len() as u32
            }
            // A missing cell.
            None => 0,
        }));
    }
    // What every text begins with says nothing of their order.
    let first = texts.first().map_or(&[][..], |t| t.as_bytes());
    let common = texts.iter().fold(first.len(), |c, t| {
        let t = t.as_bytes();
        (0..c.min(t.len()))
            .find(|&i| t[i] != first[i])
            .unwrap_or(c.min(t.len()))
    });
    // The eight bytes that follow, as a number, decide most comparisons.
    let rest = |k: u32| &texts[k as usize].as_bytes()[common..];
    let mut order: Vec<(u64, u32)> = (0..texts.len() as u32)
        .map(|k| {
            let t = rest(k);
            let mut head = [0u8; 8];
            let m = t.len().min(8);
            head[..m].copy_from_slice(&t[..m]);
            (u64::from_be_bytes(head), k)
        })
        .collect();
    let after = |k: u32| rest(k).get(8..).unwrap_or(&[]);
    order.sort_unstable_by(|a, b| {
        tick();
        a.0.cmp(&b.0).then_with(|| after(a.1).cmp(after(b.1)))
    });
    let mut rank = vec![LAST; texts.len() + 1];
    let mut at = 0u64;
    for (i, &(head, k)) in order.iter().enumerate() {
        // One text in two encodings has one rank.
        if i > 0 {
            let (h, j) = order[i - 1];
            if h != head || rest(j) != rest(k) {
                at += 1;
            }
        }
        rank[k as usize + 1] = at;
    }
    if descending {
        for r in &mut rank[1..] {
            *r = at - *r;
        }
    }
    of.into_iter().map(|k| rank[k as usize]).collect()
}

/// The rows above which a second thread takes part in an order.
const ROWS_FOR_THREADS: usize = 1 << 19;

/// Orders the positions `at` by `keys`, and leaves those of one key in the order they
/// are in.
fn sort_by_keys(at: &mut Vec<u32>, keys: &[u64]) {
    let Some(&first) = at.first() else { return };
    let first = keys[first as usize];
    let differ = keys.iter().fold(0u64, |d, &k| d | (k ^ first));
    // Passes over the positions, of which each reads a key far from the last, serve
    // keys that differ in few bits. For others, the keys are moved along.
    let passes = (0..64)
        .step_by(16)
        .filter(|s| (differ >> s) as u16 != 0)
        .count();
    if at.len() < 1 << 16 || passes <= 2 {
        sort_part(at, keys);
        return;
    }
    let mut pairs: Vec<(u64, u32)> = at.iter().map(|&k| (keys[k as usize], k)).collect();
    let mut spare = vec![(0u64, 0u32); pairs.len()];
    let bounds = by_first_digit(&mut pairs, &mut spare);
    // The buckets of the first digit are apart, and go to the threads by halves.
    let half = match bounds.iter().position(|b| *b >= pairs.len() / 2) {
        Some(i) if threads() > 1 && pairs.len() >= ROWS_FOR_THREADS => bounds[i],
        _ => pairs.len(),
    };
    let (low, high) = pairs.split_at_mut(half);
    let (spare_low, spare_high) = spare.split_at_mut(half);
    let each = |from: usize, part: &mut [(u64, u32)], spare: &mut [(u64, u32)]| {
        let end = from + part.len();
        for w in bounds.windows(2).filter(|w| w[0] >= from && w[1] <= end) {
            let (a, b) = (w[0] - from, w[1] - from);
            sort_bucket(&mut part[a..b], &mut spare[a..b]);
        }
    };
    let started = high.is_empty()
        || std::thread::scope(|s| {
            // A platform without threads, such as webR, orders in one.
            let other = std::thread::Builder::new()
                .spawn_scoped(s, || each(half, &mut *high, &mut *spare_high));
            each(0, &mut *low, &mut *spare_low);
            other.is_ok()
        });
    if high.is_empty() {
        each(0, low, spare_low);
    } else if !started {
        each(half, high, spare_high);
    }
    for (to, (_, k)) in at.iter_mut().zip(pairs) {
        *to = k;
    }
}

/// Moves pairs to the buckets of the highest bits in which their keys differ, in the
/// order they are in, and gives the bounds of the buckets.
fn by_first_digit(pairs: &mut [(u64, u32)], spare: &mut [(u64, u32)]) -> Vec<usize> {
    let first = pairs.first().map_or(0, |p| p.0);
    let differ = pairs.iter().fold(0u64, |d, p| d | (p.0 ^ first));
    if differ == 0 {
        return vec![0, pairs.len()];
    }
    let bits = (usize::BITS - pairs.len().leading_zeros()).clamp(8, 16) - 3;
    let shift = (64 - differ.leading_zeros()).saturating_sub(bits);
    let mask = (1u64 << bits) - 1;
    let digit = |k: u64| ((k >> shift) & mask) as usize;
    let mut bounds = vec![0usize; (1 << bits) + 1];
    pairs.iter().for_each(|p| bounds[digit(p.0) + 1] += 1);
    for i in 1..bounds.len() {
        bounds[i] += bounds[i - 1];
    }
    let mut next = bounds.clone();
    for p in pairs.iter() {
        let to = &mut next[digit(p.0)];
        spare[*to] = *p;
        *to += 1;
    }
    pairs.copy_from_slice(spare);
    bounds
}

/// Orders pairs by their keys, and leaves those of one key in the order they are in.
fn sort_bucket(pairs: &mut [(u64, u32)], spare: &mut [(u64, u32)]) {
    tick_block();
    if pairs.len() < 1 << 10 {
        pairs.sort_by_key(|p| p.0);
        return;
    }
    if pairs.windows(2).all(|w| w[0].0 <= w[1].0) {
        return;
    }
    for w in by_first_digit(pairs, spare).windows(2) {
        sort_bucket(&mut pairs[w[0]..w[1]], &mut spare[w[0]..w[1]]);
    }
}

/// Orders positions by their keys, as [`sort_by_keys`], in one thread: by one pass
/// per 16 bits in which the keys differ.
fn sort_part(at: &mut Vec<u32>, keys: &[u64]) {
    const BITS: u32 = 16;
    // The counts of a pass cost more than the comparisons of a short vector.
    if at.len() < 1 << 12 {
        at.sort_by_key(|&k| keys[k as usize]);
        return;
    }
    let first = keys[at[0] as usize];
    let differ = at.iter().fold(0u64, |d, &k| d | (keys[k as usize] ^ first));
    let mut next = vec![0u32; at.len()];
    for shift in (0..64).step_by(BITS as usize) {
        if (differ >> shift) as u16 == 0 {
            continue;
        }
        let digit = |k: u32| (keys[k as usize] >> shift) as u16 as usize;
        let mut count = vec![0u32; 1 << BITS];
        at.iter().for_each(|&k| count[digit(k)] += 1);
        let mut from = 0u32;
        for c in &mut count {
            from += std::mem::replace(c, from);
        }
        for &k in at.iter() {
            let c = &mut count[digit(k)];
            next[*c as usize] = k;
            *c += 1;
        }
        std::mem::swap(at, &mut next);
        progress::call();
    }
}

/// The rows of the table value `x` in the order of the columns `by`, the first
/// foremost; rows of one key stay in the order they are in.
///
/// @param x A value of a table.
/// @param by The columns.
/// @param descending The columns of `by` that descend.
/// @param threads Integer: The threads that the order and the gathering may use.
/// @return The value, or an object of class `r8_refused`.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_table_sort(x: Robj, by: Robj, descending: Robj, threads: i32) -> Result<Robj> {
    with_threads(threads, || table_sort(x, by, descending))
}

fn table_sort(x: Robj, by: Robj, descending: Robj) -> Result<Robj> {
    crate::interrupt::interruptible(unsafe { x.get() }, || {
        let h = match Held::of(unsafe { x.get() }) {
            Ok(h) => h,
            Err(m) => return Ok(refused(m)),
        };
        let of = |j: &Robj| column_positions(unsafe { j.get() }, &h.names);
        let (by, down) = match (of(&by), of(&descending)) {
            (Ok(b), Ok(d)) => (b, d),
            (Err(m), _) | (_, Err(m)) => return Ok(refused(m)),
        };
        if let Some(k) = down.iter().find(|k| !by.contains(k)) {
            return Ok(refused(format!(
                "`{}` descends, and is not among the columns of `by`",
                h.names[*k as usize]
            )));
        }
        let n = list_rows(h.columns) as usize;
        let mut at: Vec<u32> = (0..n as u32).collect();
        for k in by.iter().rev() {
            let keys = sort_keys(h.column(*k as usize), down.contains(k));
            progress::call();
            sort_by_keys(&mut at, &keys);
            progress::call();
        }
        let cols: Vec<usize> = (0..h.names.len()).collect();
        let from: Vec<SEXP> = cols.iter().map(|&k| h.column(k)).collect();
        let rows = gathered(&from, &at);
        with_columns(&h, &cols, &|k| unsafe { rows[k].get() })
    })
}

/// The columns of the table value `x`.
///
/// @param x A value of a table.
/// @return List: `name`, `type`, `unit`, `description`, and `declared`.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_table_columns(x: Robj) -> Result<List> {
    let h = Held::of(unsafe { x.get() }).map_err(fail)?;
    let def = h.def();
    let v = RView::Whole(unsafe { x.get() });
    let n = h.names.len();
    let mut ty = Vec::with_capacity(n);
    let mut unit: Vec<Option<String>> = Vec::with_capacity(n);
    let mut description: Vec<Option<String>> = Vec::with_capacity(n);
    let mut declared = Vec::with_capacity(n);
    for (i, k) in h.names.iter().enumerate() {
        let found = def.columns.iter().find(|c| c.name.as_str() == k);
        declared.push(found.is_some());
        match found {
            Some(c) => {
                ty.push(rtemis_r8::ir::notation::notation(&c.ty));
                unit.push(c.unit.as_deref().map(str::to_string));
                description.push(Some(c.description.to_string()).filter(|d| !d.is_empty()));
            }
            None => {
                let t = crate::rview::infer_column(h.column(i));
                let of = match &t {
                    Some(FieldType::Option { inner, .. }) => Some(inner.as_ref()),
                    other => other.as_ref(),
                };
                // The type of an extra column has no constraints.
                ty.push(match of {
                    Some(FieldType::Double(_)) => "double".to_string(),
                    Some(t) => rtemis_r8::ir::notation::notation(t),
                    None => String::new(),
                });
                let f = rtemis_r8::access::Access::column_facts(v, k).unwrap_or_default();
                unit.push(f.unit);
                description.push(f.description);
            }
        }
    }
    let text = |x: Vec<Option<String>>| -> Robj {
        x.into_iter()
            .map(|s| s.map_or_else(Rstr::na, Rstr::from))
            .collect::<Strings>()
            .into()
    };
    let mut out = List::from_values([
        h.names.clone().into(),
        ty.into(),
        text(unit),
        text(description),
        Robj::from(declared),
    ]);
    out.set_names(["name", "type", "unit", "description", "declared"])?;
    Ok(out)
}

/// The table value `x` with the facts `facts` of its extra columns.
///
/// @param x A value of a table.
/// @param facts List: Per column, a list with `unit` and `description`.
/// @return The value.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_table_set_facts(x: Robj, facts: Robj) -> Result<Robj> {
    let h = Held::of(unsafe { x.get() }).map_err(fail)?;
    let names: Vec<&str> = h.names.iter().map(String::as_str).collect();
    let facts = facts_of(unsafe { facts.get() }, h.def(), &names);
    unsafe { value(&h.q, h.columns, &names, facts.get(), &*h.docs) }
}

/// The rows of the table values `xs`, of one type and with the same columns.
///
/// @param xs List of values.
/// @return The value, or an object of class `r8_refused` or `r8_problems`.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_table_bind(xs: List) -> Result<Robj> {
    crate::interrupt::interruptible_if(true, || bound(&xs))
}

/// The rows of the table values `xs`.
fn bound(xs: &List) -> Result<Robj> {
    let mut held = Vec::with_capacity(xs.len());
    for x in xs.values() {
        match Held::of(unsafe { x.get() }) {
            Ok(h) => held.push(h),
            Err(m) => return Ok(refused(m)),
        }
    }
    let Some(first) = held.first() else {
        return Ok(refused("there is no value to bind".to_string()));
    };
    for (k, h) in held.iter().enumerate().skip(1) {
        if h.q != first.q {
            return Ok(refused(format!(
                "value {} is a `{}`, and the first a `{}`; Raven() gives values of one type",
                k + 1,
                h.q,
                first.q
            )));
        }
        if h.names != first.names {
            return Ok(refused(format!(
                "value {} has the columns `{}`, and the first `{}`",
                k + 1,
                h.names.join("`, `"),
                first.names.join("`, `")
            )));
        }
        let same: bool = {
            let (a, b) = (own(h.facts), own(first.facts));
            a == b
        };
        if !same {
            return Ok(refused(format!(
                "the units or descriptions of the columns of value {} differ from those of the first",
                k + 1
            )));
        }
        for (i, n) in first.names.iter().enumerate() {
            let (a, b) = (first.column(i), h.column(i));
            if sexptype(a) != sexptype(b) || attr_text(a, "class") != attr_text(b, "class") {
                return Ok(refused(format!(
                    "column `{n}` of value {} is of another type than that of the first",
                    k + 1
                )));
            }
            if attr_text(a, "levels") != attr_text(b, "levels") {
                return Ok(refused(format!(
                    "the levels of factor `{n}` of value {} differ from those of the first",
                    k + 1
                )));
            }
        }
    }
    let rows: usize = held.iter().map(|h| list_rows(h.columns) as usize).sum();
    if rows as u64 > MAX_ROWS {
        return Ok(refused(too_many_rows(rows as u64).message));
    }
    let mut out: Vec<Robj> = Vec::with_capacity(first.names.len());
    for i in 0..first.names.len() {
        let parts: Vec<SEXP> = held.iter().map(|h| h.column(i)).collect();
        out.push(append(&parts, rows));
    }
    let names: Vec<&str> = first.names.iter().map(String::as_str).collect();
    let list = named_list(
        &out.iter().map(|c| unsafe { c.get() }).collect::<Vec<_>>(),
        &names,
    );
    if first.def().columns.iter().any(|c| c.unique) {
        let names: Vec<String> = first.names.clone();
        return match checked(
            unsafe { list.get() },
            &names,
            &first.q,
            first.def(),
            first.facts,
            &*first.docs,
        )? {
            Ok(v) => Ok(v),
            Err(errors) => problems(&errors),
        };
    }
    unsafe { value(&first.q, list.get(), &names, first.facts, &*first.docs) }
}

fn attr_text(x: SEXP, name: &str) -> Vec<Option<String>> {
    let name = std::ffi::CString::new(name).unwrap_or_default();
    let a = attr(x, unsafe { extendr_ffi::Rf_install(name.as_ptr()) });
    (0..xlen(a))
        .map(|i| string_elt(a, i).map(|s| s.into_owned()))
        .collect()
}

/// The elements of `parts`, vectors of one type, one after another.
fn append(parts: &[SEXP], n: usize) -> Robj {
    let t = sexptype(parts[0]);
    unsafe {
        let out = Rf_protect(Rf_allocVector(t, n as R_xlen_t));
        let mut at = 0usize;
        for &x in parts {
            let m = xlen(x);
            for k in 0..m {
                if k % 1024 == 0 {
                    tick_block();
                }
                let (i, j) = ((at + k) as R_xlen_t, k as R_xlen_t);
                match t {
                    SEXPTYPE::REALSXP => *REAL(out).add(at + k) = REAL_ELT(x, j),
                    SEXPTYPE::INTSXP => *INTEGER(out).add(at + k) = INTEGER_ELT(x, j),
                    SEXPTYPE::LGLSXP => *LOGICAL(out).add(at + k) = LOGICAL_ELT(x, j),
                    _ => SET_STRING_ELT(out, i, STRING_ELT(x, j)),
                }
            }
            at += m;
        }
        SHALLOW_DUPLICATE_ATTRIB(out, parts[0]);
        let r = own(out);
        Rf_unprotect(1);
        r
    }
}

/// The R vector of a computed column of the table value `x`; a date-time
/// takes the time zone of the first date-time column that its expression
/// reads, or `"UTC"`.
fn computed_vector(c: &rtemis_r8::rules::calc::Computed, x: &Held) -> Robj {
    use rtemis_r8::rules::calc::Data;
    match &c.data {
        Data::Logical(v) => Logicals::from_values(v.iter().map(|b| match b {
            Some(true) => Rbool::true_value(),
            Some(false) => Rbool::false_value(),
            None => Rbool::na_value(),
        }))
        .into(),
        Data::Integer(v) => {
            Integers::from_values(v.iter().map(|i| i.map_or(Rint::na(), Rint::from))).into()
        }
        Data::Double(v) => Robj::from(v.as_slice()),
        Data::Character(v) => Strings::from_values(v.iter().map(|t| match t {
            Some(t) => Rstr::from(t.as_str()),
            None => Rstr::na(),
        }))
        .into(),
        Data::Date(v) => {
            let mut d: Robj = Doubles::from_values(
                v.iter()
                    .map(|t| t.map_or(Rfloat::na(), |t| Rfloat::from(t as f64))),
            )
            .into();
            let _ = d.set_class(["Date"]);
            d
        }
        Data::DateTime(v) => {
            let mut d: Robj = Doubles::from_values(
                v.iter()
                    .map(|t| t.map_or(Rfloat::na(), |t| Rfloat::from(t as f64 / 1e6))),
            )
            .into();
            let _ = d.set_class(["POSIXct", "POSIXt"]);
            let zone = c
                .zone_of
                .as_deref()
                .and_then(|name| x.names.iter().position(|n| n == name))
                .and_then(|k| {
                    let z = attr(x.column(k), crate::rview::sym_tzone());
                    string_elt(z, 0).map(|s| s.into_owned())
                })
                .unwrap_or_else(|| "UTC".to_string());
            let _ = d.set_attrib("tzone", zone);
            d
        }
    }
}

/// A message of an expression of `names` that computes no column.
fn not_computed(e: rtemis_r8::rules::calc::NotComputed, names: &[String], summary: bool) -> String {
    use rtemis_r8::rules::calc::NotComputed as N;
    match e {
        N::Rule { at, errors } => format!(
            "`{}` does not fit the table: {}",
            names[at],
            errors
                .iter()
                .map(|e| {
                    let mut text = crate::fieldtype::in_r(&e.message);
                    if e.code == "rule_field" {
                        let name = text.split('`').nth(1).unwrap_or_default().to_string();
                        text.push_str(&format!(
                            "; if `{name}` is an R variable, write `val({name})`"
                        ));
                    }
                    text
                })
                .collect::<Vec<_>>()
                .join("; ")
        ),
        N::Row { at, error } => format!(
            "`{}` has no value in {} {}: {}{}",
            names[at],
            if summary { "group" } else { "row" },
            error.row + 1,
            error.message,
            match error.count {
                1 => String::new(),
                n => format!(
                    "; nor in {} other {}{}",
                    n - 1,
                    if summary { "group" } else { "row" },
                    if n == 2 { "" } else { "s" }
                ),
            }
        ),
        N::Key(m) => format!("`by`: {m}"),
        N::Call {
            at,
            function,
            message,
            ..
        } => format!("`{}` calls `{function}()`: {message}", names[at]),
        N::Table(m) => m,
    }
}

/// The columns that expressions compute over the table value `x`.
///
/// @param x The table value.
/// @param exprs List: The expressions, as `calc()` holds them, by the names
///   of the columns.
/// @param by Character: The key columns, or a vector of length 0.
/// @param summary Logical: One value for each group.
/// @param threads Integer: The threads that the aggregates of a column may use.
/// @param functions List: The typed functions that the expressions call, by
///   their qualified names.
/// @param callers List: R8's `call_typed()`, `call_settled()`, and the function
///   that reports the calls done to the progress hook, or `NULL`, as `call`,
///   `settle`, and `report`; or `NULL` without functions.
/// @return A list of `first`, the first row of each group from 1 or `NULL`
///   without keys, `columns`, the computed vectors, and `units`, or an
///   `r8_refused`, which for an error of a call holds `call`, the function,
///   the column, and the first row of the group, and `failed`, the condition,
///   and for an error of the progress hook holds `hook`, its condition. A jump
///   of R that a compiled body caught continues instead.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_table_calc(
    x: Robj,
    exprs: List,
    by: Robj,
    summary: bool,
    threads: i32,
    functions: Robj,
    callers: Robj,
) -> Result<Robj> {
    // A call of R may jump, which the protection of a long walk carries past
    // the frames of Rust.
    let calls = !functions.is_null() && functions.len() > 0;
    crate::interrupt::interruptible_if(calls || crate::interrupt::long(unsafe { x.get() }), || {
        let h = match Held::of(unsafe { x.get() }) {
            Ok(h) => h,
            Err(m) => return Ok(refused(m)),
        };
        let names: Vec<String> = exprs
            .names()
            .map(|n| n.map(str::to_string).collect())
            .unwrap_or_default();
        let mut logic = Vec::with_capacity(exprs.len());
        for (i, (_, e)) in exprs.iter().enumerate() {
            match crate::irde::from_r::<rtemis_r8::ir::Logic>(unsafe { e.get() }) {
                Ok(l) => logic.push(l),
                Err(m) => return Ok(refused(format!("`{}` does not read: {}", names[i], m.0))),
            }
        }
        let keys: Vec<String> = strings(unsafe { by.get() })
            .into_iter()
            .map(|k| k.map(|k| k.into_owned()).unwrap_or_default())
            .collect();
        let keys: Vec<&str> = keys.iter().map(String::as_str).collect();
        let refs: Vec<&rtemis_r8::ir::Logic> = logic.iter().collect();
        let host = crate::typed_calls::RFunctions::new(
            &h.names,
            h.columns,
            &h.docs,
            &functions,
            &callers,
            logic.len(),
        );
        let out = rtemis_r8::rules::calc::computed(
            RView::Whole(unsafe { x.get() }),
            h.def(),
            &keys,
            &refs,
            &*h.docs,
            &host,
            summary,
            usize::try_from(threads).unwrap_or(1).max(1),
        );
        // A held jump continues once the frames return, with no call of R
        // before it (`interruptible_if()`).
        if host.jumped() {
            return Ok(().into());
        }
        if let Some(hook) = host.hook_failed.borrow_mut().take() {
            let mut r: Robj = list!(message = "the progress hook failed", hook = hook).into();
            let _ = r.set_class(["r8_refused"]);
            return Ok(r);
        }
        let out = match out {
            Ok(o) => o,
            Err(rtemis_r8::rules::calc::NotComputed::Call {
                at,
                function,
                first_row,
                message,
                ..
            }) => {
                let failed = host.failed.borrow_mut().take().unwrap_or_else(|| ().into());
                let mut r: Robj = list!(
                    message = message,
                    call = list!(
                        function = function,
                        column = names[at].clone(),
                        first_row = first_row.map(|r| r as i32 + 1)
                    ),
                    failed = failed
                )
                .into();
                let _ = r.set_class(["r8_refused"]);
                return Ok(r);
            }
            Err(e) => return Ok(refused(not_computed(e, &names, summary))),
        };
        let columns: Vec<Robj> = out.columns.iter().map(|c| computed_vector(c, &h)).collect();
        let units: Vec<Option<String>> = out.columns.iter().map(|c| c.unit.clone()).collect();
        let first: Robj = match &out.groups {
            Some(g) => g
                .first
                .iter()
                .map(|&r| r as i32 + 1)
                .collect::<Vec<i32>>()
                .into(),
            None => ().into(),
        };
        let units: Robj = Strings::from_values(units.iter().map(|u| match u {
            Some(u) => Rstr::from(u.as_str()),
            None => Rstr::na(),
        }))
        .into();
        Ok(list!(
            first = first,
            columns = List::from_values(columns),
            units = units
        )
        .into())
    })
}

/// The plan of a call over the columns of a table, as `r8_table_calc()` would
/// run it, without evaluating a row or calling a typed function: the table is
/// read, the keys checked, the groups numbered, and the expressions compiled.
///
/// @param x The table.
/// @param exprs List: The expressions, by the names of the columns.
/// @param by Character: The key columns.
/// @param call List: `name`, the table function; `label`; `summary`,
///   logical; and `threads`, an integer.
/// @return A list of `nodes`, or an `r8_refused`.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_table_plan(x: Robj, exprs: List, by: Robj, call: List) -> Result<Robj> {
    crate::interrupt::interruptible_if(crate::interrupt::long(unsafe { x.get() }), || {
        let h = match Held::of(unsafe { x.get() }) {
            Ok(h) => h,
            Err(m) => return Ok(refused(m)),
        };
        let names: Vec<String> = exprs
            .names()
            .map(|n| n.map(str::to_string).collect())
            .unwrap_or_default();
        let mut logic = Vec::with_capacity(exprs.len());
        for (i, (_, e)) in exprs.iter().enumerate() {
            match crate::irde::from_r::<rtemis_r8::ir::Logic>(unsafe { e.get() }) {
                Ok(l) => logic.push(l),
                Err(m) => return Ok(refused(format!("`{}` does not read: {}", names[i], m.0))),
            }
        }
        let keys: Vec<String> = strings(unsafe { by.get() })
            .into_iter()
            .map(|k| k.map(|k| k.into_owned()).unwrap_or_default())
            .collect();
        let keys: Vec<&str> = keys.iter().map(String::as_str).collect();
        let refs: Vec<&rtemis_r8::ir::Logic> = logic.iter().collect();
        let get = |name: &str| call.dollar(name).unwrap_or_else(|_| Robj::from(()));
        let summary = get("summary").as_bool().unwrap_or(true);
        let threads = get("threads")
            .as_integer()
            .map(|n| usize::try_from(n).unwrap_or(1).max(1))
            .unwrap_or(1);
        let function = get("name").as_str().unwrap_or("r8map").to_string();
        let label = get("label").as_str().unwrap_or("").to_string();
        let planned = match rtemis_r8::rules::calc::planned(
            RView::Whole(unsafe { x.get() }),
            h.def(),
            &keys,
            &refs,
            &*h.docs,
            summary,
            threads,
        ) {
            Ok(p) => p,
            Err(e) => return Ok(refused(not_computed(e, &names, summary))),
        };
        let columns: Vec<&str> = names.iter().map(String::as_str).collect();
        let plan = rtemis_r8::plan::table_plan(
            &rtemis_r8::plan::TableCall {
                function: &function,
                label: &label,
                table: &h.q,
                by: &keys,
                columns: &columns,
                exprs: &refs,
                summary,
                threads,
            },
            &planned,
            &*h.docs,
            &|f| crate::message::in_rust(&f.to_string()),
        );
        crate::irser::to_r(&plan).map_err(|e| Error::Other(e.to_string()))
    })
}

/// The plan of a map of the typed function `typed` over the items of a list
/// or a vector, whose calls a loop of R makes.
///
/// @param name Character: The table function, `"r8map"`.
/// @param typed Character: The typed function's qualified name.
/// @param items Integer: The items.
/// @return A list of `nodes`.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_list_plan(name: &str, typed: &str, items: i32) -> Result<Robj> {
    let q = QualifiedName::parse(typed).map_err(|e| Error::Other(e.to_string()))?;
    let docs = registry().snapshot();
    let plan = rtemis_r8::plan::list_plan(
        &rtemis_r8::plan::ListCall {
            function: name,
            typed: &q,
            items: u64::try_from(items).unwrap_or(0),
            in_rust: crate::message::in_rust(typed),
        },
        &*docs,
    );
    crate::irser::to_r(&plan).map_err(|e| Error::Other(e.to_string()))
}

/// The text of a plan: its Mermaid flowchart, its headline, or the text of
/// each stage.
///
/// @param nodes List: The plan's nodes.
/// @param form Character \{"mermaid", "headline", "stages"\}: The text.
/// @return Character: One string, or one for each stage.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_plan_text(nodes: Robj, form: &str) -> Result<Robj> {
    let plan = rtemis_r8::plan::Plan {
        nodes: crate::irde::from_r(unsafe { nodes.get() }).map_err(|e| Error::Other(e.0))?,
    };
    match form {
        "mermaid" => Ok(rtemis_r8::plan::mermaid(&plan).into()),
        "headline" => Ok(rtemis_r8::plan::headline(&plan, "R").into()),
        "stages" => Ok(Strings::from_values(rtemis_r8::plan::stage_texts(&plan)).into()),
        other => Err(Error::Other(format!("no text `{other}` of a plan"))),
    }
}

/// The expressions of a map of `f` over `columns`: `f` with `col()` set to
/// each column.
///
/// @param f The expression, as `calc()` holds it.
/// @param columns Character: The columns.
/// @return A list of the expressions, named by the columns, or an
///   `r8_refused`.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_calc_mapped(f: Robj, columns: Robj) -> Robj {
    let f = match crate::irde::from_r::<rtemis_r8::ir::Logic>(unsafe { f.get() }) {
        Ok(l) => l,
        Err(m) => return refused(m.0),
    };
    let names: Vec<String> = strings(unsafe { columns.get() })
        .into_iter()
        .map(|k| k.map(|k| k.into_owned()).unwrap_or_default())
        .collect();
    let refs: Vec<&str> = names.iter().map(String::as_str).collect();
    match rtemis_r8::rules::calc::mapped(&f, &refs) {
        Ok(exprs) => {
            let items: Vec<Robj> = exprs
                .iter()
                .map(|e| crate::irser::to_r(e).unwrap_or_else(|e| Robj::from(e.to_string())))
                .collect();
            let mut out: Robj = List::from_values(items).into();
            let _ = out.set_names(names);
            out
        }
        Err(e) => refused(crate::fieldtype::in_r(&e.message)),
    }
}

extendr_module! {
    mod table;
    fn r8_table_new;
    fn r8_table_get;
    fn r8_table_columns;
    fn r8_table_set_facts;
    fn r8_table_rename;
    fn r8_table_sort;
    fn r8_table_bind;
    fn r8_table_calc;
    fn r8_default_threads;
    fn r8_calc_mapped;
    fn r8_table_plan;
    fn r8_list_plan;
    fn r8_plan_text;
}
