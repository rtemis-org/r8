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
use crate::rust_stored::{RustStored, module, panic_text};
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

/// The receiver `x` of a method of a Rust-stored type, or the value of a
/// parameter `&T`: the state of a Rust-stored value, borrowed, or a value
/// built from the fields of one whose state R8 does not lend
/// ([`RustStored::from_value`]).
pub enum Receiver<'a, T> {
    /// The state of a Rust-stored value, or the ancestor's state that it
    /// embeds.
    Borrowed(&'a T),
    /// A value built from a value's fields.
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

impl<T: RustStored> Receiver<'_, T> {
    /// The receiver from `x`, a value of `T` or of a struct that extends it,
    /// to which the method for `T` applies; an object of another type returns
    /// `type_mismatch`.
    ///
    /// # Safety
    ///
    /// On R's main thread, with a valid `x` that stays protected while the
    /// receiver lives.
    pub unsafe fn from_arg(x: SEXP) -> Result<Self> {
        unsafe { Self::lent(x, "x", api::api()?.upcast_state) }
    }

    /// The value of the argument `name`, a parameter `&T` of a method: a
    /// value of `T` exactly; an object of another type returns
    /// `type_mismatch`, naming the argument.
    ///
    /// # Safety
    ///
    /// On R's main thread, with a valid `x` that stays protected while the
    /// value lives.
    pub unsafe fn from_named_arg(x: SEXP, name: &str) -> Result<Self> {
        unsafe { Self::lent(x, name, api::api()?.value_state) }
    }

    /// The value of `T` in `x` through `lend`, which lends its state or
    /// reports a value whose fields are read instead.
    unsafe fn lent(x: SEXP, name: &str, lend: Option<abi::StateFn>) -> Result<Self> {
        let mut state: *const c_void = std::ptr::null();
        let mut err = abi::Error {
            code: [0; 64],
            message: [0; 1024],
        };
        let status = match lend {
            Some(f) => unsafe { f(x, T::TYPE_NAME.as_ptr(), module(), &mut state, &mut err) },
            None => ERROR,
        };
        if status == OK {
            return Ok(Receiver::Borrowed(unsafe { &*state.cast::<T>() }));
        }
        if Error::from_abi(&err).code != "not_lent" {
            return Err(Error::new(
                "type_mismatch",
                format!("`{name}` is not a `{}`", T::NAME),
            ));
        }
        // A field read of a value of another package's codec may raise an R
        // error.
        let n = unsafe { sys::Rf_xlength(x) };
        let fields = (0..n)
            .map(|i| api::protect(&|| unsafe { sys::VECTOR_ELT(x, i) }))
            .collect::<std::result::Result<Vec<SEXP>, Jumped>>()
            .map_err(Jumped::into_error)?;
        unsafe { T::from_value(x, &fields) }.map(Receiver::Built)
    }
}

/// The variant of an R-stored enum value `x`, its `r8_variant` attribute.
///
/// # Safety
///
/// On R's main thread, with a valid `x`.
#[doc(hidden)]
pub unsafe fn r_variant(x: SEXP) -> Result<String> {
    unsafe extern "C" {
        fn Rf_install(name: *const std::ffi::c_char) -> SEXP;
    }
    unsafe {
        let v = api::protect(&|| sys::Rf_getAttrib(x, Rf_install(c"r8_variant".as_ptr())))
            .map_err(api::Jumped::into_error)?;
        if sys::TYPEOF(v) != sys::STRSXP || sys::Rf_xlength(v) != 1 {
            return Err(Error::new(
                "no_variant",
                "the enum value has no `r8_variant`",
            ));
        }
        let p = sys::Rf_translateCharUTF8(sys::STRING_ELT(v, 0));
        Ok(std::ffi::CStr::from_ptr(p).to_string_lossy().into_owned())
    }
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

/// The result of `R8::PartialOrd`'s `partial_cmp()`: `-1L`, `0L`, or `1L`,
/// and `NA` when the values are not ordered, as the R method returns.
impl IntoR for Option<std::cmp::Ordering> {
    fn into_r(self) -> std::result::Result<SEXP, Jumped> {
        let o = match self {
            Some(o) => o as i32,
            None => unsafe { sys::R_NaInt },
        };
        alloc(&|| unsafe { sys::Rf_ScalarInteger(o) })
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

impl<T: Element> IntoR for Vec<T> {
    /// The vector as an owned [`Buffer`], which R reads as a view: the
    /// result of `R8::Shape`'s `dim()`, among others.
    fn into_r(self) -> std::result::Result<SEXP, Jumped> {
        Buffer::from(self).into_r()
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

/// A Rust-stored value returned by an impl function, wrapped by R8, of a type
/// whose codec this package's exports hold.
pub struct Value<T: RustStored>(pub T);

impl<T: RustStored> IntoR for Value<T> {
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
        let state = Box::into_raw(Box::new(self.0));
        let status = unsafe {
            f(
                T::TYPE_NAME.as_ptr(),
                module(),
                state.cast(),
                &mut value,
                &mut err,
            )
        };
        if status != OK {
            // R8 owns the state only once it has wrapped it.
            drop(unsafe { Box::from_raw(state) });
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
