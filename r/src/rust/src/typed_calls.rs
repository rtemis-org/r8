//! The calls of typed functions in the expressions of table functions: the
//! host of `rtemis_r8::rules::calc::Functions` in R.
//!
//! The evaluator gives the calls of one function at once, one for each group.
//! A function whose body a package registered as compiled code
//! (`#[r8::function]`, `crate::exports`) is called through that body from
//! here: the cells of each argument of `vec(T)` as an R vector, the
//! arguments checked as a call from R checks them, and the result checked
//! against the return type (`crate::signature::CallChecks`). A call that these
//! checks or the body refuse runs through the function's closure, which raises
//! the error of a direct call. The calls of a function whose body is R code run
//! through its closure, in R8's `call_typed()`, one loop for the calls of a
//! function.
//!
//! spec: R8/raven#summaries

use std::cell::RefCell;
use std::collections::HashMap;

use extendr_api::SEXP;
use extendr_api::prelude::*;
use extendr_ffi::{
    INTEGER, LOGICAL, R_xlen_t, REAL, Rf_allocVector, Rf_protect, Rf_unprotect, SET_STRING_ELT,
    SET_VECTOR_ELT, SEXPTYPE, STRING_ELT, VECTOR_ELT,
};
use rtemis_r8::abi::{CallInputs, OK, Sexp};
use rtemis_r8::name::QualifiedName;
use rtemis_r8::rules::calc::{Argument, Call, CallFailed, CallValue, Data};
use rtemis_r8::rules::{DocumentSet, TypeLookup};

use crate::rview::{attr, inherits, is_null, sexptype, string_elt, xlen};

#[allow(improper_ctypes)]
unsafe extern "C" {
    fn Rf_copyMostAttrib(from: SEXP, to: SEXP);
}

/// The host of the calls of typed functions in the expressions of a table
/// function: the table's columns, the registered bodies in compiled code, and
/// each function's closure by its qualified name, which `caller`, R8's
/// `call_typed()`, calls, giving the values or the condition of an error.
pub(crate) struct RFunctions<'h> {
    names: &'h [String],
    columns: SEXP,
    docs: &'h DocumentSet,
    closures: HashMap<String, Robj>,
    caller: Option<Function>,
    /// The condition of the call that is an error.
    pub(crate) failed: RefCell<Option<Robj>>,
}

impl<'h> RFunctions<'h> {
    /// The closures of `functions`, a list named by qualified names, over the
    /// columns `columns`, a list named `names`, of a table that `docs` declares.
    pub(crate) fn new(
        names: &'h [String],
        columns: SEXP,
        docs: &'h DocumentSet,
        functions: &Robj,
        caller: &Robj,
    ) -> RFunctions<'h> {
        let mut closures = HashMap::new();
        if let Ok(l) = List::try_from(functions.clone()) {
            for (name, f) in l.iter() {
                closures.insert(name.to_string(), f);
            }
        }
        RFunctions {
            names,
            columns,
            docs,
            closures,
            caller: caller.as_function(),
            failed: RefCell::new(None),
        }
    }

    /// The column `name` of the table.
    fn column(&self, name: &str) -> std::result::Result<SEXP, String> {
        self.names
            .iter()
            .position(|n| n == name)
            .map(|k| unsafe { VECTOR_ELT(self.columns, k as R_xlen_t) })
            .ok_or_else(|| format!("`{name}` is not a column of the table"))
    }

    /// The time zone of the column `name`, for the cells of a date-time.
    fn zone(&self, name: Option<&str>) -> Option<String> {
        name.and_then(|z| self.column(z).ok())
            .and_then(|c| string_elt(attr(c, crate::rview::sym_tzone()), 0).map(|s| s.into_owned()))
    }
}

/// The cells of the vector `x` in the rows `rows`, with the attributes of `x`
/// but its names and dimensions, such as the levels of a factor and the time
/// zone of a date-time; unprotected.
fn rows_of(x: SEXP, rows: &[u32]) -> std::result::Result<SEXP, String> {
    let t = sexptype(x);
    if !matches!(
        t,
        SEXPTYPE::REALSXP | SEXPTYPE::INTSXP | SEXPTYPE::LGLSXP | SEXPTYPE::STRSXP
    ) {
        return Err("a column of a call holds logicals, numbers, or strings".to_string());
    }
    unsafe {
        let out = Rf_protect(Rf_allocVector(t, rows.len() as R_xlen_t));
        // The cells in place where R holds them as one array, and through R's
        // accessors otherwise.
        match t {
            SEXPTYPE::REALSXP => copy_rows(x, rows, REAL(out), |x, r| extendr_ffi::REAL_ELT(x, r)),
            SEXPTYPE::INTSXP => {
                copy_rows(x, rows, INTEGER(out), |x, r| extendr_ffi::INTEGER_ELT(x, r))
            }
            SEXPTYPE::LGLSXP => {
                copy_rows(x, rows, LOGICAL(out), |x, r| extendr_ffi::LOGICAL_ELT(x, r))
            }
            _ => {
                for (i, &r) in rows.iter().enumerate() {
                    SET_STRING_ELT(out, i as R_xlen_t, STRING_ELT(x, r as R_xlen_t));
                }
            }
        }
        Rf_copyMostAttrib(x, out);
        Rf_unprotect(1);
        Ok(out)
    }
}

/// Copies the elements `rows` of `x` to `to`.
///
/// # Safety
///
/// `to` holds `rows.len()` elements, and every row is an element of `x`.
unsafe fn copy_rows<T: Copy>(
    x: SEXP,
    rows: &[u32],
    to: *mut T,
    element: impl Fn(SEXP, R_xlen_t) -> T,
) {
    let to = unsafe { std::slice::from_raw_parts_mut(to, rows.len()) };
    match crate::rview::data::<T>(x) {
        Some(from) => {
            for (t, &r) in to.iter_mut().zip(rows) {
                *t = from[r as usize];
            }
        }
        None => {
            for (t, &r) in to.iter_mut().zip(rows) {
                *t = element(x, r as R_xlen_t);
            }
        }
    }
}

/// The cells of `data` in the rows `rows`, as an R vector; a date-time takes
/// the time zone `zone`.
fn cells_of(data: &Data, rows: &[u32], zone: Option<String>) -> Robj {
    let at = |r: &u32| *r as usize;
    match data {
        Data::Logical(v) => Logicals::from_values(rows.iter().map(|r| match v[at(r)] {
            Some(true) => Rbool::true_value(),
            Some(false) => Rbool::false_value(),
            None => Rbool::na_value(),
        }))
        .into(),
        Data::Integer(v) => {
            Integers::from_values(rows.iter().map(|r| v[at(r)].map_or(Rint::na(), Rint::from)))
                .into()
        }
        Data::Double(v) => Doubles::from_values(rows.iter().map(|r| Rfloat::from(v[at(r)]))).into(),
        Data::Character(v) => Strings::from_values(rows.iter().map(|r| match &v[at(r)] {
            Some(t) => Rstr::from(t.as_str()),
            None => Rstr::na(),
        }))
        .into(),
        Data::Date(v) => {
            let mut d: Robj = Doubles::from_values(
                rows.iter()
                    .map(|r| v[at(r)].map_or(Rfloat::na(), |t| Rfloat::from(t as f64))),
            )
            .into();
            let _ = d.set_class(["Date"]);
            d
        }
        Data::DateTime(v) => {
            let mut d: Robj = Doubles::from_values(
                rows.iter()
                    .map(|r| v[at(r)].map_or(Rfloat::na(), |t| Rfloat::from(t as f64 / 1e6))),
            )
            .into();
            let _ = d.set_class(["POSIXct", "POSIXt"]);
            let _ = d.set_attrib("tzone", zone.unwrap_or_else(|| "UTC".to_string()));
            d
        }
    }
}

/// A constant of a call as an R value: an integer for a whole number that R
/// wrote as one, and `NA` for `null`.
fn constant_of(c: &rtemis_r8::ir::Logic) -> std::result::Result<Robj, String> {
    use rtemis_r8::ir::{Logic, Number};
    Ok(match c {
        Logic::Null => Robj::from(Rbool::na_value()),
        Logic::Bool(b) => Robj::from(*b),
        Logic::String(t) => Robj::from(&**t),
        Logic::Number(Number::F64(x)) => Robj::from(*x),
        Logic::Number(n) => match n.as_i64().and_then(|i| i32::try_from(i).ok()) {
            Some(i) => Robj::from(i),
            None => Robj::from(n.as_f64()),
        },
        _ => return Err("a constant of a call is one value".to_string()),
    })
}

/// The value that a call gives, one element of a vector.
fn call_value(s: SEXP) -> std::result::Result<CallValue, String> {
    use CallValue as V;
    // `NULL` is the absent value of a return type of `option()`.
    if is_null(s) {
        return Ok(V::Missing);
    }
    if xlen(s) != 1 {
        return Err(format!(
            "the function gives {} values, and a call gives one",
            xlen(s)
        ));
    }
    unsafe {
        let real = |s: SEXP| match sexptype(s) {
            SEXPTYPE::REALSXP => Some(extendr_ffi::REAL_ELT(s, 0)).filter(|x| !x.is_nan()),
            SEXPTYPE::INTSXP => Some(extendr_ffi::INTEGER_ELT(s, 0))
                .filter(|&i| i != extendr_ffi::R_NaInt)
                .map(f64::from),
            _ => None,
        };
        // A factor, a date, and a date-time carry a class; a value without one
        // is of its storage type.
        let classed = !is_null(attr(s, crate::rview::sym_class()));
        if classed && inherits(s, "factor") {
            let code = extendr_ffi::INTEGER_ELT(s, 0);
            if code == extendr_ffi::R_NaInt {
                return Ok(V::Missing);
            }
            return string_elt(attr(s, crate::rview::sym_levels()), (code - 1) as usize)
                .map(|t| V::Text(t.into_owned()))
                .ok_or_else(|| "the factor's code is outside its levels".to_string());
        }
        if classed && inherits(s, "Date") {
            return Ok(real(s).map_or(V::Missing, |d| V::Date(d.floor() as i64)));
        }
        if classed && inherits(s, "POSIXct") {
            return Ok(real(s).map_or(V::Missing, |t| V::DateTime((t * 1e6).round() as i64)));
        }
        Ok(match sexptype(s) {
            SEXPTYPE::REALSXP | SEXPTYPE::INTSXP => real(s).map_or(V::Missing, V::Number),
            SEXPTYPE::LGLSXP => match extendr_ffi::LOGICAL_ELT(s, 0) {
                b if b == extendr_ffi::R_NaInt => V::Missing,
                b => V::Logical(b != 0),
            },
            SEXPTYPE::STRSXP => string_elt(s, 0).map_or(V::Missing, |t| V::Text(t.into_owned())),
            _ => return Err("the function gives a value that is not a logical, a number, a string, a date, or a date-time".to_string()),
        })
    }
}

/// How the calls through a compiled body end.
enum Native {
    /// Every call gave its value.
    Done,
    /// The call after those that gave their values did not pass its checks,
    /// or its body returned an error.
    Refused,
    /// No package registered a compiled body of the function, or the body
    /// takes other parameters than the call gives.
    Absent,
}

/// What an argument of the calls of a compiled body is: the cells of a
/// column, cells that the evaluator computed, or the same value in every
/// call, a constant or a default.
#[derive(Clone, Copy)]
enum Given {
    Column(SEXP),
    Cells,
    Fixed,
}

impl RFunctions<'_> {
    /// The arguments of one call as a list of R values by parameter name.
    fn arguments(&self, args: &[(&str, Argument<'_>)]) -> std::result::Result<Robj, String> {
        let mut names: Vec<&str> = Vec::with_capacity(args.len());
        let mut values: Vec<Robj> = Vec::with_capacity(args.len());
        for (p, a) in args {
            let v = match a {
                // The closure evaluates the default it declares.
                Argument::Default(_) => continue,
                Argument::Column { name, rows } => {
                    let x = rows_of(self.column(name)?, rows)?;
                    unsafe { Robj::from_sexp(x) }
                }
                Argument::Cells { data, rows, zone } => cells_of(data, rows, self.zone(*zone)),
                Argument::Constant(c) => constant_of(c)?,
            };
            names.push(p);
            values.push(v);
        }
        List::from_names_and_values(names, values)
            .map(Robj::from)
            .map_err(|e| e.to_string())
    }

    /// The values of the calls `calls` of the function `name` through its
    /// closure, whose places start at `offset`.
    fn through_closure(
        &self,
        name: &str,
        calls: &[Call<'_>],
        offset: usize,
    ) -> std::result::Result<Vec<CallValue>, CallFailed> {
        let fail = |at: usize, message: String| CallFailed {
            at: offset + at,
            message,
        };
        let (Some(f), Some(caller)) = (self.closures.get(name), &self.caller) else {
            return Err(fail(
                0,
                format!("`{name}()` is not a function of the expression"),
            ));
        };
        let mut given: Vec<Robj> = Vec::with_capacity(calls.len());
        for (at, args) in calls.iter().enumerate() {
            given.push(self.arguments(args).map_err(|m| fail(at, m))?);
        }
        let out = caller
            .call(pairlist!(f.clone(), List::from_values(given)))
            .map_err(|e| fail(0, format!("the call ended: {e}")))?;
        if out.inherits("r8_call_failed") {
            let message = out
                .dollar("message")
                .ok()
                .and_then(|m| m.as_str().map(str::to_string))
                .unwrap_or_default();
            let at = out
                .dollar("at")
                .ok()
                .and_then(|a| a.as_integer())
                .map_or(0, |a| (a.max(1) - 1) as usize);
            *self.failed.borrow_mut() = Some(out);
            return Err(fail(at, message));
        }
        let values = List::try_from(out).map_err(|e| fail(0, e.to_string()))?;
        values
            .values()
            .enumerate()
            .map(|(at, v)| call_value(unsafe { v.get() }).map_err(|m| fail(at, m)))
            .collect()
    }

    /// The values of the calls `calls` of `function` through the compiled
    /// body that its package registered, appended to `out`, whose length is
    /// the place of the first call, until a call that its checks or the body
    /// refuse.
    fn through_body(
        &self,
        function: &QualifiedName,
        calls: &[Call<'_>],
        out: &mut Vec<CallValue>,
    ) -> std::result::Result<Native, CallFailed> {
        let offset = out.len();
        let Ok(qname) = std::ffi::CString::new(function.to_string()) else {
            return Ok(Native::Absent);
        };
        // A body is registered by the symbol of the function's qualified name.
        let key = unsafe { extendr_ffi::Rf_install(qname.as_ptr()) };
        let (Some((body, n)), Some(checks), Some(def)) = (
            crate::exports::native_body(key),
            crate::signature::call_checks(key),
            self.docs.function_of(function),
        ) else {
            return Ok(Native::Absent);
        };
        let Some(first) = calls.first() else {
            return Ok(Native::Done);
        };
        // Every parameter is typed, in the order of the declaration, which
        // the body takes.
        if n != def.params.len()
            || checks.forced() != n
            || first.len() != n
            || first
                .iter()
                .zip(&def.params)
                .any(|((p, _), d)| *p != d.name())
        {
            return Ok(Native::Absent);
        }
        let fail = |at: usize, message: String| CallFailed {
            at: offset + at,
            message,
        };
        // Two lists, protected while `kept` lives: `fixed`, the constants and
        // defaults, the same for every call, and `list`, the arguments of a
        // call, then its result.
        let kept: [Robj; 2] = unsafe {
            [
                Robj::from_sexp(Rf_allocVector(SEXPTYPE::VECSXP, n as R_xlen_t)),
                Robj::from_sexp(Rf_allocVector(SEXPTYPE::VECSXP, n as R_xlen_t + 1)),
            ]
        };
        let (fixed, list) = unsafe { (kept[0].get(), kept[1].get()) };
        let mut given = Vec::with_capacity(n);
        for (i, (_, a)) in first.iter().enumerate() {
            given.push(match a {
                Argument::Column { name, .. } => {
                    Given::Column(self.column(name).map_err(|m| fail(0, m))?)
                }
                Argument::Cells { .. } => Given::Cells,
                Argument::Constant(c) => {
                    let v = constant_of(c).map_err(|m| fail(0, m))?;
                    unsafe { SET_VECTOR_ELT(fixed, i as R_xlen_t, v.get()) };
                    Given::Fixed
                }
                Argument::Default(d) => {
                    let Some(ty) = def.params[i].ty() else {
                        return Ok(Native::Absent);
                    };
                    let v = crate::values::decode_field(
                        rtemis_r8::tree::TreeRef::new(d),
                        ty,
                        self.docs,
                    )
                    .map_err(|e| fail(0, e.to_string()))?;
                    unsafe { SET_VECTOR_ELT(fixed, i as R_xlen_t, v.get()) };
                    Given::Fixed
                }
            });
        }
        let inputs = CallInputs {
            size: std::mem::size_of::<CallInputs>() as u32,
            flags: 0,
            seed: 0,
        };
        // The body fills the report only when it returns an error.
        let mut err = crate::rust_stored::blank();
        let mut argv: Vec<Sexp> = vec![std::ptr::null_mut(); n];
        for (k, args) in calls.iter().enumerate() {
            rtemis_r8::progress::tick();
            for (i, (_, a)) in args.iter().enumerate() {
                let v = match (given[i], a) {
                    (Given::Column(x), Argument::Column { rows, .. }) => {
                        rows_of(x, rows).map_err(|m| fail(k, m))?
                    }
                    (Given::Cells, Argument::Cells { data, rows, zone }) => {
                        let v = cells_of(data, rows, self.zone(*zone));
                        unsafe { SET_VECTOR_ELT(list, i as R_xlen_t, v.get()) };
                        continue;
                    }
                    (Given::Fixed, _) => unsafe { VECTOR_ELT(fixed, i as R_xlen_t) },
                    _ => return Ok(Native::Refused),
                };
                unsafe { SET_VECTOR_ELT(list, i as R_xlen_t, v) };
            }
            if !checks.arguments(list) {
                return Ok(Native::Refused);
            }
            for (i, a) in argv.iter_mut().enumerate() {
                *a = unsafe { VECTOR_ELT(list, i as R_xlen_t) }.cast();
            }
            let mut result: Sexp = unsafe { extendr_ffi::R_NilValue }.cast();
            let status = unsafe { body(&inputs, argv.as_ptr(), n, &mut result, &mut err) };
            if status == OK {
                unsafe { SET_VECTOR_ELT(list, n as R_xlen_t, result.cast()) };
            }
            crate::rust_stored::returned_to_rust();
            if status != OK || !checks.result(list, n) {
                return Ok(Native::Refused);
            }
            let v =
                call_value(unsafe { VECTOR_ELT(list, n as R_xlen_t) }).map_err(|m| fail(k, m))?;
            out.push(v);
        }
        Ok(Native::Done)
    }
}

impl rtemis_r8::rules::calc::Functions for RFunctions<'_> {
    fn call(
        &self,
        function: &QualifiedName,
        calls: &[Call<'_>],
    ) -> std::result::Result<Vec<CallValue>, CallFailed> {
        let name = function.to_string();
        let mut out = Vec::with_capacity(calls.len());
        while out.len() < calls.len() {
            let k = out.len();
            match self.through_body(function, &calls[k..], &mut out)? {
                Native::Done => {}
                // The closure raises the error of a direct call, or, where
                // it admits what the checks here refuse, gives the value.
                Native::Refused => {
                    let at = out.len();
                    out.extend(self.through_closure(&name, &calls[at..=at], at)?);
                }
                Native::Absent => {
                    out.extend(self.through_closure(&name, &calls[k..], k)?);
                }
            }
        }
        Ok(out)
    }
}
