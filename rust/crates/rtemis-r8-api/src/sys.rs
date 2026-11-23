//! The entry points of R's C API that the crate calls, all in the API index
//! of Writing R Extensions. The package that links the crate links R: the
//! symbols are bound when R loads the package's shared object, and against
//! `R.dll` on Windows.

#![allow(non_snake_case, non_upper_case_globals, non_camel_case_types)]

use std::ffi::{c_char, c_double, c_int, c_void};

/// An R object, as R's `SEXP`.
pub type SEXP = *mut c_void;
/// R's vector length, `R_xlen_t`.
pub type R_xlen_t = isize;

pub const LGLSXP: c_int = 10;
pub const INTSXP: c_int = 13;
pub const REALSXP: c_int = 14;
pub const STRSXP: c_int = 16;
pub const VECSXP: c_int = 19;
pub const RAWSXP: c_int = 24;
/// `CE_UTF8` of `cetype_t`.
pub const CE_UTF8: c_int = 1;

unsafe extern "C" {
    pub static mut R_NilValue: SEXP;
    pub static mut R_NamesSymbol: SEXP;
    pub static mut R_NaString: SEXP;
    pub static R_NaInt: c_int;
    pub static R_NaReal: c_double;

    pub fn Rf_allocVector(t: c_int, n: R_xlen_t) -> SEXP;
    pub fn Rf_protect(x: SEXP) -> SEXP;
    pub fn Rf_unprotect(n: c_int);
    pub fn TYPEOF(x: SEXP) -> c_int;
    pub fn Rf_xlength(x: SEXP) -> R_xlen_t;
    pub fn DATAPTR_OR_NULL(x: SEXP) -> *const c_void;
    pub fn REAL(x: SEXP) -> *mut c_double;
    pub fn INTEGER(x: SEXP) -> *mut c_int;
    pub fn LOGICAL(x: SEXP) -> *mut c_int;
    pub fn RAW(x: SEXP) -> *mut u8;
    pub fn REAL_GET_REGION(x: SEXP, i: R_xlen_t, n: R_xlen_t, buf: *mut c_double) -> R_xlen_t;
    pub fn INTEGER_GET_REGION(x: SEXP, i: R_xlen_t, n: R_xlen_t, buf: *mut c_int) -> R_xlen_t;
    pub fn LOGICAL_GET_REGION(x: SEXP, i: R_xlen_t, n: R_xlen_t, buf: *mut c_int) -> R_xlen_t;
    pub fn RAW_GET_REGION(x: SEXP, i: R_xlen_t, n: R_xlen_t, buf: *mut u8) -> R_xlen_t;
    pub fn REAL_ELT(x: SEXP, i: R_xlen_t) -> c_double;
    pub fn INTEGER_ELT(x: SEXP, i: R_xlen_t) -> c_int;
    pub fn LOGICAL_ELT(x: SEXP, i: R_xlen_t) -> c_int;
    pub fn STRING_ELT(x: SEXP, i: R_xlen_t) -> SEXP;
    pub fn SET_STRING_ELT(x: SEXP, i: R_xlen_t, v: SEXP);
    pub fn VECTOR_ELT(x: SEXP, i: R_xlen_t) -> SEXP;
    pub fn SET_VECTOR_ELT(x: SEXP, i: R_xlen_t, v: SEXP) -> SEXP;
    pub fn Rf_mkCharLenCE(s: *const c_char, n: c_int, enc: c_int) -> SEXP;
    pub fn Rf_translateCharUTF8(x: SEXP) -> *const c_char;
    pub fn Rf_ScalarReal(x: c_double) -> SEXP;
    pub fn Rf_ScalarInteger(x: c_int) -> SEXP;
    pub fn Rf_ScalarLogical(x: c_int) -> SEXP;
    pub fn Rf_getAttrib(x: SEXP, name: SEXP) -> SEXP;
    pub fn Rf_setAttrib(x: SEXP, name: SEXP, v: SEXP) -> SEXP;
    pub fn Rf_cons(car: SEXP, cdr: SEXP) -> SEXP;
    pub fn CAR(x: SEXP) -> SEXP;
    pub fn CDR(x: SEXP) -> SEXP;
    pub fn TAG(x: SEXP) -> SEXP;
    pub fn SETCDR(x: SEXP, y: SEXP) -> SEXP;
    pub fn SET_TAG(x: SEXP, y: SEXP);
    pub fn R_PreserveObject(x: SEXP);
    pub fn R_MakeExternalPtr(p: *mut c_void, tag: SEXP, prot: SEXP) -> SEXP;
    pub fn R_ExternalPtrAddr(x: SEXP) -> *mut c_void;
    pub fn R_ClearExternalPtr(x: SEXP);
    pub fn R_RegisterCFinalizerEx(x: SEXP, f: unsafe extern "C" fn(SEXP), onexit: c_int);
    pub fn R_GetCCallable(package: *const c_char, name: *const c_char) -> *const c_void;
    pub fn R_RegisterCCallable(package: *const c_char, name: *const c_char, f: *const c_void);
}
