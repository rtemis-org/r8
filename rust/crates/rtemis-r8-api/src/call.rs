//! Calls of impl functions: the conversion of arguments from R
//! ([`FromR`]) and of results to R ([`IntoR`]), the receiver `x`
//! ([`Receiver`]), `...` ([`Dots`]), and [`method`], which the functions
//! that `#[r8::methods]` generates call.

use std::ffi::c_void;
use std::ops::Deref;
use std::panic::{AssertUnwindSafe, catch_unwind};

use rtemis_r8::abi::{self, ERROR, OK, Status, UNWIND};

use crate::api::{self, Error, Jumped, Result};
use crate::buffer::{Buffer, Element};
use crate::field::Field;
use crate::keep;
use crate::native::{Native, panic_text};
use crate::sys::{self, SEXP};

/// An argument of an impl function, converted from its R value.
pub trait FromR: Sized {
    /// The argument from `x`; `name` names the parameter in errors.
    ///
    /// # Safety
    ///
    /// On R's main thread, with a valid `x`.
    unsafe fn from_arg(x: SEXP, name: &str) -> Result<Self>;
}

fn invalid(name: &str, e: Error) -> Error {
    Error::new("invalid_argument", format!("`{name}`: {}", e.message))
}

macro_rules! from_field {
    ($($t:ty),*) => {$(
        impl FromR for $t {
            unsafe fn from_arg(x: SEXP, name: &str) -> Result<Self> {
                unsafe { <$t as Field>::from_r(x) }.map_err(|e| invalid(name, e))
            }
        }
    )*};
}
from_field!(bool, i32, String);

impl FromR for f64 {
    unsafe fn from_arg(x: SEXP, name: &str) -> Result<Self> {
        // An integer argument reads as a double, as R's arithmetic does.
        if unsafe { sys::TYPEOF(x) } == sys::INTSXP && unsafe { sys::Rf_xlength(x) } == 1 {
            let v = unsafe { sys::INTEGER_ELT(x, 0) };
            if v == i32::MIN {
                return Err(invalid(name, Error::new("na_value", "the integer is NA")));
            }
            return Ok(f64::from(v));
        }
        let v = unsafe { <f64 as Field>::from_r(x) }.map_err(|e| invalid(name, e))?;
        if v.to_bits() == unsafe { sys::R_NaReal }.to_bits() {
            return Err(invalid(name, Error::new("na_value", "the double is NA")));
        }
        Ok(v)
    }
}

impl<T: FromR> FromR for Option<T> {
    unsafe fn from_arg(x: SEXP, name: &str) -> Result<Self> {
        if x == unsafe { sys::R_NilValue } {
            return Ok(None);
        }
        unsafe { T::from_arg(x, name) }.map(Some)
    }
}

impl<T: Element> FromR for Buffer<T> {
    unsafe fn from_arg(x: SEXP, name: &str) -> Result<Self> {
        unsafe { Buffer::from_r(x) }.map_err(|e| invalid(name, e))
    }
}

/// The arguments R passed as `...`, a list.
#[derive(Debug, Clone, Copy)]
pub struct Dots(pub SEXP);

impl Dots {
    /// The number of arguments.
    pub fn len(&self) -> usize {
        unsafe { sys::Rf_xlength(self.0) as usize }
    }

    /// Whether `...` was empty.
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

impl FromR for Dots {
    unsafe fn from_arg(x: SEXP, _name: &str) -> Result<Self> {
        Ok(Dots(x))
    }
}

/// The receiver `x` of a method of a Rust-native type: the state of a
/// Rust-native value, borrowed, or a value built with `from_fields` from an
/// R-native value of the type, such as `serialize(version = 2)` reads back.
pub enum Receiver<'a, T> {
    /// The state of a Rust-native value.
    Borrowed(&'a T),
    /// A value built from an R-native value's fields.
    Built(T),
}

impl<T> Deref for Receiver<'_, T> {
    type Target = T;
    fn deref(&self) -> &T {
        match self {
            Receiver::Borrowed(t) => t,
            Receiver::Built(t) => t,
        }
    }
}

impl<T: Native> Receiver<'_, T> {
    /// The receiver from `x`; an object of another type returns
    /// `type_mismatch`.
    ///
    /// # Safety
    ///
    /// On R's main thread, with a valid `x` that stays protected while the
    /// receiver lives.
    pub unsafe fn from_arg(x: SEXP) -> Result<Self> {
        let a = api::api()?;
        let mut state: *const c_void = std::ptr::null();
        let mut err = abi::Error {
            code: [0; 64],
            message: [0; 1024],
        };
        let status = match a.value_state {
            Some(f) => unsafe { f(x, T::TYPE_NAME.as_ptr(), &mut state, &mut err) },
            None => ERROR,
        };
        if status == OK {
            return Ok(Receiver::Borrowed(unsafe { &*state.cast::<T>() }));
        }
        let e = Error::from_abi(&err);
        if e.code != "not_native" || !is_r_native::<T>(x) {
            return Err(Error::new(
                "type_mismatch",
                format!("`x` is not a `{}`", T::NAME),
            ));
        }
        let n = unsafe { sys::Rf_xlength(x) } as usize;
        let fields: Vec<SEXP> = (0..n)
            .map(|i| unsafe { sys::VECTOR_ELT(x, i as isize) })
            .collect();
        unsafe { T::from_fields(&fields) }.map(Receiver::Built)
    }
}

/// Whether `x` is an R-native value of `T`: a list whose `r8_type` names
/// `T`.
fn is_r_native<T: Native>(x: SEXP) -> bool {
    unsafe {
        if sys::TYPEOF(x) != sys::VECSXP {
            return false;
        }
        let ty = api::protect(&|| sys::Rf_getAttrib(x, symbol_r8_type()));
        let Ok(ty) = ty else { return false };
        if sys::TYPEOF(ty) != sys::STRSXP || sys::Rf_xlength(ty) != 1 {
            return false;
        }
        let p = sys::Rf_translateCharUTF8(sys::STRING_ELT(ty, 0));
        std::ffi::CStr::from_ptr(p).to_bytes() == T::NAME.as_bytes()
    }
}

fn symbol_r8_type() -> SEXP {
    unsafe extern "C" {
        fn Rf_install(name: *const std::ffi::c_char) -> SEXP;
    }
    unsafe { Rf_install(c"r8_type".as_ptr()) }
}

/// A result of an impl function, converted to its R value.
pub trait IntoR {
    /// The R value; a jump out of an allocation is kept for R8 to continue.
    fn into_r(self) -> std::result::Result<SEXP, Jumped>;
}

fn alloc<F: Fn() -> SEXP>(f: &F) -> std::result::Result<SEXP, Jumped> {
    api::protect(f)
}

impl IntoR for f64 {
    fn into_r(self) -> std::result::Result<SEXP, Jumped> {
        alloc(&|| unsafe { sys::Rf_ScalarReal(self) })
    }
}

impl IntoR for i32 {
    fn into_r(self) -> std::result::Result<SEXP, Jumped> {
        alloc(&|| unsafe { sys::Rf_ScalarInteger(self) })
    }
}

impl IntoR for bool {
    fn into_r(self) -> std::result::Result<SEXP, Jumped> {
        alloc(&|| unsafe { sys::Rf_ScalarLogical(i32::from(self)) })
    }
}

impl IntoR for String {
    fn into_r(self) -> std::result::Result<SEXP, Jumped> {
        self.as_str().into_r()
    }
}

impl IntoR for &str {
    fn into_r(self) -> std::result::Result<SEXP, Jumped> {
        let n = i32::try_from(self.len()).map_err(|_| {
            Jumped::Error(Error::new("too_long", "the string is longer than R allows"))
        })?;
        alloc(&|| unsafe {
            let v = sys::Rf_protect(sys::Rf_allocVector(sys::STRSXP, 1));
            sys::SET_STRING_ELT(
                v,
                0,
                sys::Rf_mkCharLenCE(self.as_ptr().cast(), n, sys::CE_UTF8),
            );
            sys::Rf_unprotect(1);
            v
        })
    }
}

impl IntoR for () {
    fn into_r(self) -> std::result::Result<SEXP, Jumped> {
        Ok(unsafe { sys::R_NilValue })
    }
}

impl<T: IntoR> IntoR for Option<T> {
    fn into_r(self) -> std::result::Result<SEXP, Jumped> {
        match self {
            Some(v) => v.into_r(),
            None => Ok(unsafe { sys::R_NilValue }),
        }
    }
}

impl<T: IntoR, E: Into<Error>> IntoR for std::result::Result<T, E> {
    fn into_r(self) -> std::result::Result<SEXP, Jumped> {
        match self {
            Ok(v) => v.into_r(),
            Err(e) => Err(Jumped::Error(e.into())),
        }
    }
}

unsafe extern "C" fn drop_buffer<T: Element>(x: SEXP) {
    let p = unsafe { sys::R_ExternalPtrAddr(x) };
    if !p.is_null() {
        let _ = catch_unwind(AssertUnwindSafe(|| {
            drop(unsafe { Box::from_raw(p.cast::<Buffer<T>>()) })
        }));
        unsafe { sys::R_ClearExternalPtr(x) };
    }
}

impl<T: Element> IntoR for Buffer<T> {
    /// A borrowed buffer returns its R vector; an owned one, a view whose
    /// owner, an external pointer, holds the buffer until R collects it.
    fn into_r(self) -> std::result::Result<SEXP, Jumped> {
        let boxed = Box::into_raw(Box::new(self));
        let owner = alloc(&|| unsafe {
            let o = sys::Rf_protect(sys::R_MakeExternalPtr(
                boxed.cast(),
                sys::R_NilValue,
                sys::R_NilValue,
            ));
            sys::R_RegisterCFinalizerEx(o, drop_buffer::<T>, 1);
            sys::Rf_unprotect(1);
            o
        });
        let owner = match owner {
            Ok(o) => o,
            Err(j) => {
                // The finalizer was not registered, so the box is still Rust's.
                drop(unsafe { Box::from_raw(boxed) });
                return Err(j);
            }
        };
        let b = unsafe { &*boxed };
        b.to_r(owner).map_err(Jumped::Error)
    }
}

/// A Rust-native value returned by an impl function, wrapped by R8.
pub struct Value<T: Native>(pub T);

impl<T: Native> IntoR for Value<T> {
    fn into_r(self) -> std::result::Result<SEXP, Jumped> {
        let a = api::api().map_err(Jumped::Error)?;
        let Some(f) = a.new_value else {
            return Err(Jumped::Error(Error::new(
                "no_api",
                "R8's API has no `new_value`",
            )));
        };
        let mut value: SEXP = unsafe { sys::R_NilValue };
        let mut err = abi::Error {
            code: [0; 64],
            message: [0; 1024],
        };
        // R8 takes the state, also on error.
        let state = Box::into_raw(Box::new(self.0));
        if unsafe { f(T::TYPE_NAME.as_ptr(), state.cast(), &mut value, &mut err) } != OK {
            return Err(Jumped::Error(Error::from_abi(&err)));
        }
        Ok(value)
    }
}

/// An R object passed through unchanged; the impl function keeps it
/// protected or returns it at once.
#[derive(Debug, Clone, Copy)]
pub struct Sexp(pub SEXP);

impl IntoR for Sexp {
    fn into_r(self) -> std::result::Result<SEXP, Jumped> {
        Ok(self.0)
    }
}

impl FromR for Sexp {
    unsafe fn from_arg(x: SEXP, _name: &str) -> Result<Self> {
        Ok(Sexp(x))
    }
}

/// Runs an impl function for R8: checks the number of arguments, runs
/// `body`, which converts the arguments, calls the method under
/// `catch_unwind`, and converts the result, and reports `R8_OK`,
/// `R8_ERROR`, or the `R8_UNWIND` of a jump R8 continues.
///
/// # Safety
///
/// Called from an impl function with R8's arguments.
pub unsafe fn method(
    args: *const abi::Sexp,
    nargs: usize,
    n_params: usize,
    result: *mut abi::Sexp,
    err: *mut abi::Error,
    body: impl FnOnce(&[SEXP]) -> std::result::Result<SEXP, Jumped>,
) -> Status {
    keep::drain();
    if nargs != n_params {
        Error::new(
            "arity",
            format!("the method takes {n_params} arguments, and received {nargs}"),
        )
        .fill(err);
        return ERROR;
    }
    let a: &[SEXP] = if nargs == 0 {
        &[]
    } else {
        unsafe { std::slice::from_raw_parts(args, nargs) }
    };
    match catch_unwind(AssertUnwindSafe(|| body(a))) {
        Ok(Ok(v)) => {
            unsafe { *result = v };
            OK
        }
        Ok(Err(Jumped::Error(e))) => {
            e.fill(err);
            ERROR
        }
        Ok(Err(Jumped::Unwind(e))) => {
            e.fill(err);
            UNWIND
        }
        Err(p) => {
            Error::new("panic", panic_text(&*p)).fill(err);
            ERROR
        }
    }
}

impl From<Error> for Jumped {
    fn from(e: Error) -> Self {
        Jumped::Error(e)
    }
}
