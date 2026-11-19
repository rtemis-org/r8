//! The value-access trait over R objects, read in place.
//!
//! [`RView`] implements `rtemis_r8::access::Access` for R objects: it reads
//! numbers, strings, and attributes through R's C API without copying, so
//! validating a vector of 10^6 doubles is one loop over R's own array. Form
//! problems carry the codes of the R form (`r_type`, `r_length`,
//! `na_value`, `r_attributes`, and the others); the validator in `rtemis-r8`
//! checks the declared constraints.
//!
//! A handle holds a raw `SEXP`, which is valid while the `.Call` that
//! received it runs: the arguments of a `.Call` are protected for its
//! duration, and every object a handle reaches is reachable from them.

use std::borrow::Cow;
use std::ffi::CStr;
use std::sync::atomic::{AtomicPtr, AtomicUsize, Ordering};

use extendr_api::SEXP;
use extendr_ffi::{
    DATAPTR_OR_NULL, INTEGER, INTEGER_GET_REGION, LENGTH, LOGICAL, LOGICAL_GET_REGION, R_CHAR,
    R_NaString, R_NilValue, R_compute_identical, R_xlen_t, RAW, REAL, REAL_GET_REGION,
    Rf_getAttrib, Rf_install, SEXPTYPE, STRING_ELT, TYPEOF, VECTOR_ELT, XLENGTH,
};
use rtemis_r8::access::{Access, Codes, Dense, Dimnames, Factor, Form, Read, Sparse, Table};
use rtemis_r8::ir::{EnumDef, FieldDef, FieldType};
use rtemis_r8::name::QualifiedName;
use rtemis_r8::rules::TypeLookup;
use serde_json::Value;

// ------------------------------------------------------------- attributes

static ATTRIB_COUNT: AtomicPtr<()> = AtomicPtr::new(std::ptr::null_mut());
static ATTRIB_RAW: AtomicPtr<()> = AtomicPtr::new(std::ptr::null_mut());

/// Receives the attribute functions of `attrib.c` from `R_init_R8()`.
///
/// # Safety
///
/// `count` and `raw` are `r8_attrib_count()` and `r8_attrib_raw()`.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn r8_set_attrib_functions(count: *mut (), raw: *mut ()) {
    ATTRIB_COUNT.store(count, Ordering::Release);
    ATTRIB_RAW.store(raw, Ordering::Release);
}

/// The number of attributes of `x`.
pub(crate) fn attrib_count(x: SEXP) -> usize {
    let f = ATTRIB_COUNT.load(Ordering::Acquire);
    assert!(!f.is_null(), "R8 is loaded through R_init_R8()");
    let f: unsafe extern "C" fn(SEXP) -> R_xlen_t = unsafe { std::mem::transmute(f) };
    unsafe { f(x) as usize }
}

/// The attribute `name` of `x` as stored, without the expansion of compact
/// row names that `Rf_getAttrib()` applies.
pub(crate) fn attrib_raw(x: SEXP, name: SEXP) -> SEXP {
    let f = ATTRIB_RAW.load(Ordering::Acquire);
    assert!(!f.is_null(), "R8 is loaded through R_init_R8()");
    let f: unsafe extern "C" fn(SEXP, SEXP) -> SEXP = unsafe { std::mem::transmute(f) };
    unsafe { f(x, name) }
}

/// An installed symbol, looked up once.
macro_rules! symbol {
    ($name:literal) => {{
        static S: AtomicUsize = AtomicUsize::new(0);
        let mut s = S.load(Ordering::Relaxed);
        if s == 0 {
            let c: &CStr = $name;
            s = unsafe { Rf_install(c.as_ptr()) } as usize;
            S.store(s, Ordering::Relaxed);
        }
        s as SEXP
    }};
}

pub(crate) fn sym_names() -> SEXP {
    symbol!(c"names")
}
pub(crate) fn sym_class() -> SEXP {
    symbol!(c"class")
}
pub(crate) fn sym_dim() -> SEXP {
    symbol!(c"dim")
}
pub(crate) fn sym_dimnames() -> SEXP {
    symbol!(c"dimnames")
}
pub(crate) fn sym_levels() -> SEXP {
    symbol!(c"levels")
}
pub(crate) fn sym_tzone() -> SEXP {
    symbol!(c"tzone")
}
pub(crate) fn sym_row_names() -> SEXP {
    symbol!(c"row.names")
}
pub(crate) fn sym_r8_type() -> SEXP {
    symbol!(c"r8_type")
}
pub(crate) fn sym_r8_version() -> SEXP {
    symbol!(c"r8_version")
}
pub(crate) fn sym_r8_variant() -> SEXP {
    symbol!(c"r8_variant")
}
fn sym_dim_cap() -> SEXP {
    symbol!(c"Dim")
}
fn sym_p() -> SEXP {
    symbol!(c"p")
}
fn sym_i() -> SEXP {
    symbol!(c"i")
}
fn sym_x() -> SEXP {
    symbol!(c"x")
}

pub(crate) fn nil() -> SEXP {
    unsafe { R_NilValue }
}

pub(crate) fn is_null(x: SEXP) -> bool {
    x == nil()
}

pub(crate) fn attr(x: SEXP, name: SEXP) -> SEXP {
    unsafe { Rf_getAttrib(x, name) }
}

pub(crate) fn sexptype(x: SEXP) -> SEXPTYPE {
    unsafe { TYPEOF(x) }
}

pub(crate) fn xlen(x: SEXP) -> usize {
    if is_null(x) {
        0
    } else {
        unsafe { XLENGTH(x) as usize }
    }
}

/// A string element of a character vector, `None` for `NA`. A string that
/// is not valid UTF-8 is converted with replacement characters.
pub(crate) fn string_elt<'a>(x: SEXP, i: usize) -> Option<Cow<'a, str>> {
    unsafe {
        let c = STRING_ELT(x, i as R_xlen_t);
        if c == R_NaString {
            return None;
        }
        let bytes = std::slice::from_raw_parts(R_CHAR(c) as *const u8, LENGTH(c) as usize);
        Some(match std::str::from_utf8(bytes) {
            Ok(s) => Cow::Borrowed(s),
            Err(_) => Cow::Owned(String::from_utf8_lossy(bytes).into_owned()),
        })
    }
}

/// The strings of a character vector, `NA` as `None`.
pub(crate) fn strings<'a>(x: SEXP) -> Vec<Option<Cow<'a, str>>> {
    if sexptype(x) != SEXPTYPE::STRSXP {
        return Vec::new();
    }
    (0..xlen(x)).map(|i| string_elt(x, i)).collect()
}

pub(crate) fn inherits(x: SEXP, class: &str) -> bool {
    let c = attr(x, sym_class());
    sexptype(c) == SEXPTYPE::STRSXP
        && (0..xlen(c)).any(|i| string_elt(c, i).as_deref() == Some(class))
}

/// The class vector of `x` is exactly `want`.
pub(crate) fn class_is(x: SEXP, want: &[&str]) -> bool {
    let c = attr(x, sym_class());
    sexptype(c) == SEXPTYPE::STRSXP
        && xlen(c) == want.len()
        && want
            .iter()
            .enumerate()
            .all(|(i, w)| string_elt(c, i).as_deref() == Some(*w))
}

/// The one string of a length-one character attribute.
pub(crate) fn attr_string<'a>(x: SEXP, name: SEXP) -> Option<Cow<'a, str>> {
    let a = attr(x, name);
    (sexptype(a) == SEXPTYPE::STRSXP && xlen(a) == 1).then(|| string_elt(a, 0))?
}

/// `R_IsNA()`: a `NaN` whose low word is 1954.
#[inline]
pub(crate) fn is_na_real(x: f64) -> bool {
    x.is_nan() && (x.to_bits() as u32) == 1954
}

const NA_INT: i32 = i32::MIN;

fn reals<'a>(x: SEXP) -> &'a [f64] {
    unsafe { std::slice::from_raw_parts(REAL(x), xlen(x)) }
}

pub(crate) fn ints<'a>(x: SEXP) -> &'a [i32] {
    unsafe { std::slice::from_raw_parts(INTEGER(x), xlen(x)) }
}

fn logicals<'a>(x: SEXP) -> &'a [i32] {
    unsafe { std::slice::from_raw_parts(LOGICAL(x), xlen(x)) }
}

/// The elements of a vector as one array, where R holds them as one: an
/// ordinary vector, or an ALTREP vector whose data are materialized.
fn data<'a, T>(x: SEXP) -> Option<&'a [T]> {
    let p = unsafe { DATAPTR_OR_NULL(x) };
    (!p.is_null()).then(|| unsafe { std::slice::from_raw_parts(p.cast::<T>(), xlen(x)) })
}

/// Calls `f` with elements `start..end` of a double vector, reading an
/// ALTREP vector without materialized data by region, so that it stays
/// unmaterialized. A short region read ends the calls early.
fn for_reals(x: SEXP, start: usize, end: usize, mut f: impl FnMut(usize, f64)) {
    if let Some(s) = data::<f64>(x) {
        for (i, &v) in s[start..end].iter().enumerate() {
            f(start + i, v);
        }
        return;
    }
    let mut buf = [0f64; 4096];
    let mut i = start;
    while i < end {
        let got = unsafe {
            REAL_GET_REGION(
                x,
                i as R_xlen_t,
                buf.len().min(end - i) as R_xlen_t,
                buf.as_mut_ptr(),
            )
        } as usize;
        if got == 0 {
            break;
        }
        for (k, &v) in buf[..got].iter().enumerate() {
            f(i + k, v);
        }
        i += got;
    }
}

/// Calls `f` with elements `start..end` of an integer or logical vector,
/// reading an ALTREP vector without materialized data by region.
fn for_ints(x: SEXP, start: usize, end: usize, mut f: impl FnMut(usize, i32)) {
    if let Some(s) = data::<i32>(x) {
        for (i, &v) in s[start..end].iter().enumerate() {
            f(start + i, v);
        }
        return;
    }
    let logical = sexptype(x) == SEXPTYPE::LGLSXP;
    let mut buf = [0i32; 4096];
    let mut i = start;
    while i < end {
        let want = buf.len().min(end - i) as R_xlen_t;
        let got = unsafe {
            if logical {
                LOGICAL_GET_REGION(x, i as R_xlen_t, want, buf.as_mut_ptr())
            } else {
                INTEGER_GET_REGION(x, i as R_xlen_t, want, buf.as_mut_ptr())
            }
        } as usize;
        if got == 0 {
            break;
        }
        for (k, &v) in buf[..got].iter().enumerate() {
            f(i + k, v);
        }
        i += got;
    }
}

fn r_type(message: impl Into<String>) -> Form {
    Form::new("r_type", message)
}

fn unwrap_option(ft: &FieldType) -> &FieldType {
    match ft {
        FieldType::Option { inner, .. } => inner,
        other => other,
    }
}

pub(crate) fn is_scalar(ft: &FieldType) -> bool {
    matches!(
        ft,
        FieldType::Logical
            | FieldType::Integer(_)
            | FieldType::Double(_)
            | FieldType::Character(_)
            | FieldType::Date
            | FieldType::Posixct
    )
}

/// The attributes an atomic vector of `ft` may carry.
fn allowed_attributes(ft: &FieldType) -> &'static [fn() -> SEXP] {
    match ft {
        FieldType::Date => &[sym_class],
        FieldType::Posixct => &[sym_class, sym_tzone],
        _ => &[],
    }
}

/// Rejects attributes of `x` beyond `allowed`, naming them.
fn plain(x: SEXP, allowed: &[fn() -> SEXP]) -> Read<()> {
    let n = attrib_count(x);
    if n == 0 {
        return Ok(());
    }
    let present = allowed.iter().filter(|s| !is_null(attr(x, s()))).count();
    if n <= present {
        return Ok(());
    }
    let names = r_attribute_names(x);
    let allowed_names: Vec<String> = allowed
        .iter()
        .map(|s| unsafe {
            let p = extendr_ffi::PRINTNAME(s());
            CStr::from_ptr(R_CHAR(p)).to_string_lossy().into_owned()
        })
        .collect();
    let extra: Vec<String> = names
        .into_iter()
        .filter(|a| !allowed_names.contains(a))
        .collect();
    Err(Form::new(
        "r_attributes",
        format!(
            "the value carries the attributes {extra:?}, which this field's type does not hold; r8_conform() removes them"
        ),
    ))
}

/// The names of the attributes of `x`, through R's `attributes()`; used on
/// the error path only.
fn r_attribute_names(x: SEXP) -> Vec<String> {
    use extendr_api::prelude::*;
    let robj = unsafe { Robj::from_sexp(x) };
    let Ok(f) = R!("base::attributes") else {
        return Vec::new();
    };
    match f.as_function().map(|f| f.call(pairlist!(robj))) {
        Some(Ok(a)) => a
            .names()
            .map(|n| n.map(str::to_string).collect())
            .unwrap_or_default(),
        _ => Vec::new(),
    }
}

/// The R type and class an atomic vector of the scalar type `ft` has.
fn atomic_kind(x: SEXP, ft: &FieldType) -> Read<()> {
    let t = sexptype(x);
    let numeric = matches!(t, SEXPTYPE::REALSXP | SEXPTYPE::INTSXP);
    let ok = match ft {
        FieldType::Logical => t == SEXPTYPE::LGLSXP,
        FieldType::Integer(_) | FieldType::Double(_) => numeric,
        FieldType::Character(_) => t == SEXPTYPE::STRSXP,
        FieldType::Date => numeric && inherits(x, "Date"),
        FieldType::Posixct => numeric && inherits(x, "POSIXct"),
        _ => false,
    };
    if ok {
        return Ok(());
    }
    Err(r_type(match ft {
        FieldType::Logical => "expected a logical vector",
        FieldType::Integer(_) => "expected an integer vector",
        FieldType::Double(_) => "expected a numeric vector",
        FieldType::Character(_) => "expected a character vector",
        FieldType::Date => "expected a Date",
        FieldType::Posixct => "expected a POSIXct",
        _ => "expected an atomic vector",
    }))
}

fn date_days(d: f64) -> Read<i64> {
    if d.fract() != 0.0 {
        return Err(Form::new(
            "date_fraction",
            format!("a Date of {d} days has a fraction of a day"),
        ));
    }
    Ok(d as i64)
}

fn whole(d: f64) -> Read<i64> {
    if d.fract() == 0.0 && d.abs() <= 2_147_483_647.0 {
        Ok(d as i64)
    } else {
        Err(r_type(format!(
            "{d} is not a whole number in the integer range"
        )))
    }
}

/// The element of a numeric vector as a double, `None` for `NA`.
fn number_elt(x: SEXP, i: usize) -> Option<f64> {
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
}

// ------------------------------------------------------------------ handle

/// A handle into an R object.
#[derive(Debug, Clone, Copy)]
pub enum RView<'a> {
    /// An R object.
    Whole(SEXP),
    /// Element `i` of an atomic vector.
    Elem(SEXP, usize),
    /// Row `i` of a data.frame.
    Row(SEXP, usize),
    /// An absent member.
    Missing,
    /// The fields of a value under construction, a named list, and the
    /// variant for an enum (`""` for a struct).
    Draft(SEXP, &'a str),
}

impl<'a> RView<'a> {
    /// The R object the view reads, shared: a whole object or a list
    /// element.
    pub(crate) fn robj(self) -> extendr_api::Result<extendr_api::Robj> {
        match self {
            RView::Whole(x) | RView::Draft(x, _) => Ok(unsafe { extendr_api::Robj::from_sexp(x) }),
            RView::Elem(x, i) if sexptype(x) == SEXPTYPE::VECSXP => {
                Ok(unsafe { extendr_api::Robj::from_sexp(VECTOR_ELT(x, i as R_xlen_t)) })
            }
            _ => Err(extendr_api::Error::Other(
                "a computed field names a value that is not an R object".to_string(),
            )),
        }
    }

    fn sexp(self) -> Option<SEXP> {
        match self {
            RView::Whole(x) | RView::Draft(x, _) => Some(x),
            _ => None,
        }
    }

    fn found(self) -> &'static str {
        match self {
            RView::Whole(x) => match sexptype(x) {
                SEXPTYPE::NILSXP => "NULL",
                SEXPTYPE::LGLSXP => "a logical vector",
                SEXPTYPE::INTSXP => "an integer vector",
                SEXPTYPE::REALSXP => "a numeric vector",
                SEXPTYPE::STRSXP => "a character vector",
                SEXPTYPE::VECSXP => "a list",
                SEXPTYPE::RAWSXP => "a raw vector",
                _ => "an R object",
            },
            RView::Elem(..) => "a vector element",
            RView::Row(..) => "a table row",
            RView::Missing => "nothing",
            RView::Draft(..) => "a list",
        }
    }

    /// A scalar: a length-one atomic vector of `ft` without other attributes
    /// than `ft` allows, not `NA`; or an element of a vector.
    fn scalar(self, ft: &FieldType) -> Read<(SEXP, usize)> {
        match self {
            RView::Whole(x) => {
                plain(x, allowed_attributes(ft))?;
                if xlen(x) != 1 {
                    return Err(Form::new(
                        "r_length",
                        format!("expected one value, found {}", xlen(x)),
                    ));
                }
                atomic_kind(x, ft)?;
                if RView::Elem(x, 0).is_absent() {
                    return Err(Form::new(
                        "na_value",
                        "NA is not a value of this field; an absent value is declared option()",
                    ));
                }
                Ok((x, 0))
            }
            RView::Elem(x, i) => {
                atomic_kind(x, ft)?;
                Ok((x, i))
            }
            _ => Err(Form::mismatch("a value", self.found())),
        }
    }
}

impl<'a> Access<'a> for RView<'a> {
    fn is_absent(self) -> bool {
        match self {
            RView::Missing => true,
            RView::Whole(x) | RView::Draft(x, _) => is_null(x),
            RView::Row(..) => false,
            RView::Elem(x, i) => unsafe {
                match sexptype(x) {
                    SEXPTYPE::REALSXP => is_na_real(*REAL(x).add(i)),
                    SEXPTYPE::INTSXP => *INTEGER(x).add(i) == NA_INT,
                    SEXPTYPE::LGLSXP => *LOGICAL(x).add(i) == NA_INT,
                    SEXPTYPE::STRSXP => STRING_ELT(x, i as R_xlen_t) == R_NaString,
                    SEXPTYPE::VECSXP => is_null(VECTOR_ELT(x, i as R_xlen_t)),
                    _ => false,
                }
            },
        }
    }

    fn logical(self) -> Read<bool> {
        let (x, i) = self.scalar(&FieldType::Logical)?;
        Ok(unsafe { *LOGICAL(x).add(i) } != 0)
    }

    fn integer(self) -> Read<i64> {
        let (x, i) = self.scalar(&FieldType::Integer(Default::default()))?;
        match sexptype(x) {
            SEXPTYPE::INTSXP => Ok(i64::from(unsafe { *INTEGER(x).add(i) })),
            _ => whole(unsafe { *REAL(x).add(i) }),
        }
    }

    fn double(self) -> Read<f64> {
        let (x, i) = self.scalar(&FieldType::Double(Default::default()))?;
        Ok(number_elt(x, i).unwrap_or(f64::NAN))
    }

    fn character(self) -> Read<Cow<'a, str>> {
        let (x, i) = self.scalar(&FieldType::Character(Default::default()))?;
        Ok(string_elt(x, i).unwrap_or(Cow::Borrowed("NA")))
    }

    fn date(self) -> Read<i64> {
        let (x, i) = self.scalar(&FieldType::Date)?;
        date_days(number_elt(x, i).unwrap_or(f64::NAN))
    }

    fn datetime(self) -> Read<i64> {
        let (x, i) = self.scalar(&FieldType::Posixct)?;
        Ok((number_elt(x, i).unwrap_or(f64::NAN) * 1e6).round() as i64)
    }

    fn raw(self) -> Read<()> {
        match self.sexp() {
            Some(x) if sexptype(x) == SEXPTYPE::RAWSXP => plain(x, &[]),
            _ => Err(r_type("expected a raw vector")),
        }
    }

    fn bytes(self) -> Read<Cow<'a, [u8]>> {
        self.raw()?;
        let x = self.sexp().unwrap_or(nil());
        let n = xlen(x);
        if n == 0 {
            return Ok(Cow::Borrowed(&[]));
        }
        Ok(Cow::Borrowed(unsafe {
            std::slice::from_raw_parts(RAW(x), n)
        }))
    }

    fn json(self) -> Read<()> {
        match self.sexp() {
            Some(x) => json_form(x, ""),
            None => Err(Form::new(
                "r_json",
                "a JSON value in R is a list, a length-one atomic vector, or NULL",
            )),
        }
    }

    fn runtime(self) -> Read<()> {
        Ok(())
    }

    fn vec(self, element: &FieldType) -> Read<usize> {
        let Some(x) = self.sexp() else {
            return Err(Form::mismatch("a vector", self.found()));
        };
        let inner = unwrap_option(element);
        if is_scalar(inner) {
            plain(x, allowed_attributes(inner))?;
            atomic_kind(x, inner)?;
            return Ok(xlen(x));
        }
        if sexptype(x) != SEXPTYPE::VECSXP {
            return Err(r_type("expected an unnamed list"));
        }
        plain(x, &[])?;
        Ok(xlen(x))
    }

    fn item(self, i: usize) -> Self {
        match self.sexp() {
            Some(x) if sexptype(x) == SEXPTYPE::VECSXP => {
                RView::Whole(unsafe { VECTOR_ELT(x, i as R_xlen_t) })
            }
            Some(x) => RView::Elem(x, i),
            None => RView::Missing,
        }
    }

    fn f64_slice(self) -> Option<&'a [f64]> {
        let x = self.sexp()?;
        (sexptype(x) == SEXPTYPE::REALSXP)
            .then(|| data(x))
            .flatten()
    }

    fn i32_slice(self) -> Option<&'a [i32]> {
        let x = self.sexp()?;
        (sexptype(x) == SEXPTYPE::INTSXP).then(|| data(x)).flatten()
    }

    fn each_double_in(self, start: usize, end: usize, mut f: impl FnMut(usize, Read<Option<f64>>)) {
        let Some(x) = self.sexp() else { return };
        match sexptype(x) {
            SEXPTYPE::REALSXP => for_reals(x, start, end, |i, d| {
                f(i, Ok((!is_na_real(d)).then_some(d)))
            }),
            SEXPTYPE::INTSXP => for_ints(x, start, end, |i, v| {
                f(i, Ok((v != NA_INT).then_some(f64::from(v))))
            }),
            _ => (start..end).for_each(|i| f(i, Err(r_type("expected a numeric vector")))),
        }
    }

    fn each_integer_in(
        self,
        start: usize,
        end: usize,
        mut f: impl FnMut(usize, Read<Option<i64>>),
    ) {
        let Some(x) = self.sexp() else { return };
        match sexptype(x) {
            SEXPTYPE::INTSXP => for_ints(x, start, end, |i, v| {
                f(i, Ok((v != NA_INT).then_some(i64::from(v))))
            }),
            SEXPTYPE::REALSXP => for_reals(x, start, end, |i, d| {
                f(
                    i,
                    if d.is_nan() {
                        Ok(None)
                    } else {
                        whole(d).map(Some)
                    },
                )
            }),
            _ => (start..end).for_each(|i| f(i, Err(r_type("expected an integer vector")))),
        }
    }

    fn each_logical_in(
        self,
        start: usize,
        end: usize,
        mut f: impl FnMut(usize, Read<Option<bool>>),
    ) {
        let Some(x) = self.sexp() else { return };
        if sexptype(x) == SEXPTYPE::LGLSXP {
            for_ints(x, start, end, |i, v| {
                f(i, Ok((v != NA_INT).then_some(v != 0)))
            });
        }
    }

    fn each_character_in(
        self,
        start: usize,
        end: usize,
        mut f: impl FnMut(usize, Read<Option<Cow<'a, str>>>),
    ) {
        let Some(x) = self.sexp() else { return };
        if sexptype(x) == SEXPTYPE::STRSXP {
            for i in start..end.min(xlen(x)) {
                f(i, Ok(string_elt(x, i)));
            }
        }
    }

    fn each_date_in(self, start: usize, end: usize, mut f: impl FnMut(usize, Read<Option<i64>>)) {
        let Some(x) = self.sexp() else { return };
        match sexptype(x) {
            SEXPTYPE::REALSXP => for_reals(x, start, end, |i, d| {
                f(
                    i,
                    if d.is_nan() {
                        Ok(None)
                    } else {
                        date_days(d).map(Some)
                    },
                )
            }),
            SEXPTYPE::INTSXP => for_ints(x, start, end, |i, v| {
                f(i, Ok((v != NA_INT).then_some(i64::from(v))))
            }),
            _ => {}
        }
    }

    fn each_datetime_in(
        self,
        start: usize,
        end: usize,
        mut f: impl FnMut(usize, Read<Option<i64>>),
    ) {
        let Some(x) = self.sexp() else { return };
        match sexptype(x) {
            SEXPTYPE::REALSXP => for_reals(x, start, end, |i, s| {
                f(i, Ok((!s.is_nan()).then(|| (s * 1e6).round() as i64)))
            }),
            SEXPTYPE::INTSXP => for_ints(x, start, end, |i, v| {
                f(i, Ok((v != NA_INT).then_some(i64::from(v) * 1_000_000)))
            }),
            _ => {}
        }
    }

    fn map(self, value: &FieldType, _ordered: bool) -> Read<usize> {
        let Some(x) = self.sexp() else {
            return Err(Form::mismatch("a map", self.found()));
        };
        let names = attr(x, sym_names());
        if sexptype(names) != SEXPTYPE::STRSXP || xlen(names) != xlen(x) {
            return Err(r_type("a map is a named list or a named vector"));
        }
        if sexptype(x) != SEXPTYPE::VECSXP {
            let inner = unwrap_option(value);
            if !is_scalar(inner) {
                return Err(r_type("a map of non-scalar values is a named list"));
            }
            atomic_kind(x, inner)?;
        }
        Ok(xlen(x))
    }

    fn each_entry(self, mut f: impl FnMut(usize, Read<(Cow<'a, str>, Self)>)) {
        let Some(x) = self.sexp() else { return };
        let names = attr(x, sym_names());
        if sexptype(names) != SEXPTYPE::STRSXP {
            return;
        }
        let list = sexptype(x) == SEXPTYPE::VECSXP;
        for i in 0..xlen(x).min(xlen(names)) {
            let k = string_elt(names, i).unwrap_or(Cow::Borrowed("NA"));
            let v = if list {
                RView::Whole(unsafe { VECTOR_ELT(x, i as R_xlen_t) })
            } else {
                RView::Elem(x, i)
            };
            f(i, Ok((k, v)));
        }
    }

    fn factor(self, _declared: Option<&[String]>, ordered: bool) -> Read<Factor<'a>> {
        let Some(x) = self.sexp() else {
            return Err(r_type("expected a factor"));
        };
        if sexptype(x) != SEXPTYPE::INTSXP || !inherits(x, "factor") {
            return Err(r_type("expected a factor"));
        }
        plain(x, &[sym_levels, sym_class])?;
        if inherits(x, "ordered") != ordered {
            return Err(r_type(if ordered {
                "expected an ordered factor"
            } else {
                "expected an unordered factor"
            }));
        }
        let lv = strings(attr(x, sym_levels()));
        if lv.iter().any(Option::is_none) {
            return Err(Form::new("r_factor", "a factor has no NA level"));
        }
        let levels: Vec<Cow<'a, str>> = lv.into_iter().flatten().collect();
        let codes = ints(x);
        let k = levels.len() as i32;
        if codes.iter().any(|&c| c != NA_INT && !(1..=k).contains(&c)) {
            return Err(Form::new("r_factor", "a factor's codes index its levels"));
        }
        Ok(Factor {
            levels,
            codes: Codes::I32(codes),
            problems: Vec::new(),
        })
    }

    fn dense(self, element: &FieldType) -> Read<Dense<'a, Self>> {
        let Some(x) = self.sexp() else {
            return Err(r_type("expected a matrix or an array"));
        };
        let dim = attr(x, sym_dim());
        if sexptype(dim) != SEXPTYPE::INTSXP {
            return Err(r_type("expected a matrix or an array"));
        }
        plain(x, &[sym_dim, sym_dimnames])?;
        atomic_kind(x, unwrap_option(element))?;
        let shape: Vec<u64> = ints(dim).iter().map(|&d| d.max(0) as u64).collect();
        let mut problems = Vec::new();
        let dn = attr(x, sym_dimnames());
        if !is_null(dn) {
            let ok = sexptype(dn) == SEXPTYPE::VECSXP
                && xlen(dn) == shape.len()
                && shape.iter().enumerate().all(|(i, &len)| {
                    let names = unsafe { VECTOR_ELT(dn, i as R_xlen_t) };
                    is_null(names)
                        || (sexptype(names) == SEXPTYPE::STRSXP && xlen(names) as u64 == len)
                });
            if !ok {
                problems.push(
                    Form::new(
                        "matrix_form",
                        "`dimnames` holds, per dimension, null or one string per index",
                    )
                    .at("/dimnames"),
                );
            }
        }
        let dimnames: Option<Dimnames<'a>> = (!is_null(dn) && sexptype(dn) == SEXPTYPE::VECSXP)
            .then(|| {
                (0..xlen(dn))
                    .map(|i| {
                        let names = unsafe { VECTOR_ELT(dn, i as R_xlen_t) };
                        (!is_null(names)).then(|| {
                            strings(names)
                                .into_iter()
                                .map(|s| s.unwrap_or(Cow::Borrowed("NA")))
                                .collect()
                        })
                    })
                    .collect()
            });
        Ok(Dense {
            shape,
            data: RView::Whole(x),
            len: xlen(x),
            dimnames,
            problems,
        })
    }

    fn sparse(self, _element: &FieldType) -> Read<Sparse<Self>> {
        let Some(x) = self.sexp() else {
            return Err(r_type("expected a dgCMatrix (package Matrix)"));
        };
        if !inherits(x, "dgCMatrix") {
            return Err(r_type("expected a dgCMatrix (package Matrix)"));
        }
        let slot = |s: SEXP, t: SEXPTYPE| {
            let v = attr(x, s);
            (sexptype(v) == t).then_some(v)
        };
        let (Some(dim), Some(p), Some(i), Some(xs)) = (
            slot(sym_dim_cap(), SEXPTYPE::INTSXP),
            slot(sym_p(), SEXPTYPE::INTSXP),
            slot(sym_i(), SEXPTYPE::INTSXP),
            slot(sym_x(), SEXPTYPE::REALSXP),
        ) else {
            return Err(Form::new(
                "sparse_form",
                "a dgCMatrix holds the slots Dim, p, i, and x",
            ));
        };
        let d = ints(dim);
        if d.len() != 2 {
            return Err(
                Form::new("matrix_form", "a sparse matrix has two dimensions").at("/shape"),
            );
        }
        let shape = [d[0].max(0) as u64, d[1].max(0) as u64];
        let (p, idx) = (ints(p), ints(i));
        let nnz = xlen(xs);
        let well_formed = p.len() as u64 == shape[1] + 1
            && p.first() == Some(&0)
            && p.last().map(|&v| v as i64) == Some(nnz as i64)
            && p.windows(2).all(|w| w[0] <= w[1])
            && idx.len() == nnz
            && p.windows(2).all(|w| {
                let col = &idx[w[0] as usize..w[1] as usize];
                col.iter().all(|&r| r >= 0 && (r as u64) < shape[0])
                    && col.windows(2).all(|q| q[0] < q[1])
            });
        if !well_formed {
            return Err(Form::new(
                "sparse_form",
                "compressed sparse column form: `indptr` has ncol + 1 non-decreasing offsets from 0 to the number of values, and each column's row `indices` increase and lie below nrow",
            ));
        }
        Ok(Sparse {
            shape,
            data: RView::Whole(xs),
            len: nnz,
            indptr: p.iter().map(|&v| v as u64).collect(),
            indices: idx.iter().map(|&v| v as u64).collect(),
        })
    }

    fn table(self, _columns: &[FieldDef], _open: bool) -> Read<Table<'a, Self>> {
        let Some(x) = self.sexp() else {
            return Err(r_type("expected a data.frame"));
        };
        if sexptype(x) != SEXPTYPE::VECSXP || !inherits(x, "data.frame") {
            return Err(r_type("expected a data.frame"));
        }
        let rows = table_rows(x)?;
        let names = attr(x, sym_names());
        let columns = (0..xlen(x))
            .map(|i| {
                (
                    string_elt(names, i).unwrap_or(Cow::Borrowed("NA")),
                    RView::Whole(unsafe { VECTOR_ELT(x, i as R_xlen_t) }),
                )
            })
            .collect();
        Ok(Table {
            rows,
            columns,
            problems: Vec::new(),
        })
    }

    fn extra_column_type(self, name: &str) -> Read<FieldType> {
        let col = match self.member(name) {
            RView::Whole(c) => c,
            _ => return Err(r_type(format!("column `{name}` is absent"))),
        };
        infer_column(col).ok_or_else(|| {
            r_type(format!("column `{name}` holds no column type: logical, integer, double, character, factor, Date, or POSIXct"))
                .at(format!("/{}", name.replace('~', "~0").replace('/', "~1")))
        })
    }

    fn row(self, i: usize) -> Self {
        match self.sexp() {
            Some(x) => RView::Row(x, i),
            None => RView::Missing,
        }
    }

    fn record(self, name: &QualifiedName) -> Read<()> {
        match self {
            RView::Draft(..) => Ok(()),
            RView::Whole(x)
                if sexptype(x) == SEXPTYPE::VECSXP
                    && inherits(x, "r8_value")
                    && inherits(x, &name.to_string()) =>
            {
                Ok(())
            }
            _ => Err(r_type(format!("expected a value of `{name}`"))),
        }
    }

    fn member(self, name: &str) -> Self {
        match self {
            RView::Row(x, i) => match list_member(x, name) {
                Some(col) if sexptype(col) != SEXPTYPE::VECSXP => RView::Elem(col, i),
                Some(col) => RView::Whole(unsafe { VECTOR_ELT(col, i as R_xlen_t) }),
                None => RView::Missing,
            },
            _ => match self.sexp().and_then(|x| list_member(x, name)) {
                Some(v) => RView::Whole(v),
                None => RView::Missing,
            },
        }
    }

    fn member_at(self, index: usize, name: &str) -> Self {
        match self {
            RView::Whole(x) | RView::Draft(x, _) => match list_member_at(x, index, name) {
                Some(v) => RView::Whole(v),
                None => RView::Missing,
            },
            _ => self.member(name),
        }
    }

    fn members(self) -> Vec<Cow<'a, str>> {
        match self.sexp() {
            Some(x) if sexptype(x) == SEXPTYPE::VECSXP => strings(attr(x, sym_names()))
                .into_iter()
                .flatten()
                .collect(),
            _ => Vec::new(),
        }
    }

    fn variant(self, name: &QualifiedName, _def: &EnumDef) -> Read<Cow<'a, str>> {
        match self {
            RView::Draft(_, v) => Ok(Cow::Borrowed(v)),
            RView::Whole(x) if inherits(x, "r8_value") && inherits(x, &name.to_string()) => {
                attr_string(x, sym_r8_variant()).ok_or_else(|| {
                    Form::new(
                        "missing_tag",
                        format!("a `{name}` value names its variant in `r8_variant`"),
                    )
                })
            }
            _ => Err(r_type(format!("expected a value of `{name}`"))),
        }
    }

    fn implementor(
        self,
        _trait: &QualifiedName,
        _lookup: &dyn TypeLookup,
    ) -> Result<(QualifiedName, Option<String>), String> {
        let ty = self.sexp().and_then(|x| attr_string(x, sym_r8_type()));
        ty.and_then(|t| QualifiedName::parse(&t).ok())
            .map(|q| (q, None))
            .ok_or_else(|| "a dyn() field holds an R8 value".to_string())
    }

    fn same(self, other: Self) -> bool {
        match (self, other) {
            (RView::Whole(a), RView::Whole(b)) => a == b,
            (RView::Elem(a, i), RView::Elem(b, j)) => a == b && i == j,
            _ => false,
        }
    }

    fn identical(self, other: Self) -> bool {
        match (self.sexp(), other.sexp()) {
            // Flags 16: identical()'s defaults.
            (Some(a), Some(b)) => unsafe {
                R_compute_identical(a, b, 16) == extendr_ffi::Rboolean::TRUE
            },
            _ => false,
        }
    }

    fn to_value(self, ft: &FieldType, lookup: &dyn TypeLookup) -> Value {
        match self {
            RView::Missing | RView::Row(..) => Value::Null,
            RView::Whole(x) | RView::Draft(x, _) => crate::values::encode_field(x, ft, lookup),
            RView::Elem(x, i) => elem_value(x, i, unwrap_option(ft)),
        }
    }
}

/// The JSON form of element `i` of an atomic vector, by the element's type.
fn elem_value(x: SEXP, i: usize, ft: &FieldType) -> Value {
    if RView::Elem(x, i).is_absent() {
        return Value::Null;
    }
    match (ft, sexptype(x)) {
        (FieldType::Factor { .. }, SEXPTYPE::INTSXP) => {
            let code = ints(x)[i];
            strings(attr(x, sym_levels()))
                .get((code - 1) as usize)
                .cloned()
                .flatten()
                .map_or(Value::Null, |s| Value::from(s.into_owned()))
        }
        (FieldType::Date, _) => number_elt(x, i).map_or(Value::Null, |d| {
            Value::from(rtemis_r8::codec::date_text(d as i64))
        }),
        (FieldType::Posixct, _) => number_elt(x, i).map_or(Value::Null, |s| {
            Value::from(rtemis_r8::codec::datetime_text((s * 1e6).round() as i64))
        }),
        (_, SEXPTYPE::LGLSXP) => Value::Bool(logicals(x)[i] != 0),
        (_, SEXPTYPE::STRSXP) => {
            string_elt(x, i).map_or(Value::Null, |s| Value::from(s.into_owned()))
        }
        (_, SEXPTYPE::INTSXP | SEXPTYPE::REALSXP) => {
            let d = number_elt(x, i).unwrap_or(f64::NAN);
            if d.is_nan() {
                Value::from("NaN")
            } else if d.is_infinite() {
                Value::from(if d > 0.0 { "Infinity" } else { "-Infinity" })
            } else if d.fract() == 0.0 && d.abs() < 9.007_199_254_740_992e15 {
                Value::from(d as i64)
            } else {
                serde_json::Number::from_f64(d).map_or(Value::Null, Value::Number)
            }
        }
        _ => Value::Null,
    }
}

/// Whether string `i` of a character vector is `name`, compared as bytes.
fn name_is(names: SEXP, i: usize, name: &str) -> bool {
    unsafe {
        let c = STRING_ELT(names, i as R_xlen_t);
        c != R_NaString
            && LENGTH(c) as usize == name.len()
            && std::slice::from_raw_parts(R_CHAR(c) as *const u8, name.len()) == name.as_bytes()
    }
}

/// The member `name` of a named list.
pub(crate) fn list_member(x: SEXP, name: &str) -> Option<SEXP> {
    list_member_at(x, usize::MAX, name)
}

/// The member `name` of a named list, looked for at `hint` first.
pub(crate) fn list_member_at(x: SEXP, hint: usize, name: &str) -> Option<SEXP> {
    if sexptype(x) != SEXPTYPE::VECSXP {
        return None;
    }
    let names = attr(x, sym_names());
    if sexptype(names) != SEXPTYPE::STRSXP {
        return None;
    }
    let n = xlen(names);
    let i = if hint < n && name_is(names, hint, name) {
        Some(hint)
    } else {
        (0..n).find(|&i| name_is(names, i, name))
    };
    i.map(|i| unsafe { VECTOR_ELT(x, i as R_xlen_t) })
}

/// The number of rows of a data.frame with automatic row names: the compact
/// form `c(NA, -n)` or `c(NA, n)`, or `1:n`.
pub(crate) fn table_rows(x: SEXP) -> Read<u64> {
    let rn = attrib_raw(x, sym_row_names());
    if is_null(rn) {
        return Ok(0);
    }
    if sexptype(rn) == SEXPTYPE::INTSXP {
        let s = ints(rn);
        if s.len() == 2 && s[0] == NA_INT {
            return Ok(s[1].unsigned_abs() as u64);
        }
        if s.iter().enumerate().all(|(i, &v)| v == i as i32 + 1) {
            return Ok(s.len() as u64);
        }
    }
    Err(Form::new(
        "r_row_names",
        "a table has automatic row names; r8_conform() resets them",
    ))
}

/// The column type of an extra column of an open table, from its R class;
/// cells may be missing, so the type is `option()` of the class's type.
pub(crate) fn infer_column(col: SEXP) -> Option<FieldType> {
    let base = if inherits(col, "factor") {
        FieldType::Factor {
            levels: None,
            ordered: inherits(col, "ordered"),
        }
    } else if inherits(col, "Date") {
        FieldType::Date
    } else if inherits(col, "POSIXct") {
        FieldType::Posixct
    } else {
        match sexptype(col) {
            SEXPTYPE::LGLSXP => FieldType::Logical,
            SEXPTYPE::INTSXP => FieldType::Integer(Default::default()),
            SEXPTYPE::REALSXP => FieldType::Double(Default::default()),
            SEXPTYPE::STRSXP => FieldType::Character(Default::default()),
            _ => return None,
        }
    };
    Some(FieldType::Option {
        inner: Box::new(base),
        resolve: Default::default(),
    })
}

/// The JSON form of a `type_json()` value in R: `NULL`, a length-one atomic
/// vector that is not `NA` and is finite, or a list of such values.
fn json_form(x: SEXP, suffix: &str) -> Read<()> {
    let fail = |m: &str| Err(Form::new("r_json", m).at(suffix.to_string()));
    match sexptype(x) {
        SEXPTYPE::NILSXP => Ok(()),
        SEXPTYPE::VECSXP => {
            let names = attr(x, sym_names());
            for i in 0..xlen(x) {
                let s = match string_elt_opt(names, i) {
                    Some(k) => format!("{suffix}/{}", k.replace('~', "~0").replace('/', "~1")),
                    None => format!("{suffix}/{i}"),
                };
                json_form(unsafe { VECTOR_ELT(x, i as R_xlen_t) }, &s)?;
            }
            Ok(())
        }
        SEXPTYPE::LGLSXP | SEXPTYPE::INTSXP | SEXPTYPE::REALSXP | SEXPTYPE::STRSXP => {
            if xlen(x) != 1 {
                return fail(
                    "a JSON array in R is an unnamed list; an atomic value has length one",
                );
            }
            let bad = match sexptype(x) {
                SEXPTYPE::REALSXP => !reals(x)[0].is_finite(),
                _ => RView::Elem(x, 0).is_absent(),
            };
            if bad {
                return fail("NA and non-finite numbers have no JSON form");
            }
            Ok(())
        }
        _ => fail("a JSON value in R is a list, a length-one atomic vector, or NULL"),
    }
}

fn string_elt_opt<'a>(names: SEXP, i: usize) -> Option<Cow<'a, str>> {
    (sexptype(names) == SEXPTYPE::STRSXP && i < xlen(names)).then(|| string_elt(names, i))?
}
