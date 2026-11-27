//! Parameters borrowed from their R values for one call: slices of vectors
//! and strings ([`Borrowed`]). A borrowed argument holds no token and leaves
//! the caller's vector as it is; the call's frame keeps the R value alive while
//! the compiled code reads it.
//! spec: R8/class-system#rust-impls-and-rust-stored-types-rtemis-r8-api

use std::ffi::c_int;

use rtemis_r8::derive::Encode;

use crate::api::{self, Error, Result};
use crate::buffer::Element;
use crate::field::ElementType;
use crate::signature::ParamType;
use crate::sys::{self, R_xlen_t, SEXP};

/// A parameter type that a compiled function or method borrows from its R
/// value for the call: `[f64]`, `[i32]`, `[u8]`, `[bool]`, or `str`, which the
/// function takes as `&[f64]` and the others.
#[diagnostic::on_unimplemented(
    message = "`&{Self}` is not a borrowed parameter of an R8 function",
    note = "a borrowed parameter is `&[f64]`, `&[i32]`, `&[u8]`, `&[bool]`, `&str`, or an `Option` of one"
)]
pub trait Borrowed {
    /// What holds the borrowed form for the call: the R vector's own
    /// elements, or a copy where R holds them otherwise.
    type Holder;

    /// The R8 type of the parameter.
    fn param_type() -> ParamType;

    /// The holder of the argument `x`; `name` names the parameter in errors.
    ///
    /// # Safety
    ///
    /// On R's main thread, with a valid `x`, which the caller keeps alive while
    /// the holder lives.
    unsafe fn hold(x: SEXP, name: &str) -> Result<Self::Holder>;

    /// The borrowed form.
    fn lend(h: &Self::Holder) -> &Self;
}

/// The elements of an R vector: its own array, or a copy of its regions where
/// R holds no array, as for a compact sequence.
pub enum Slice<T: 'static> {
    /// The elements of the R vector, which the call's frame keeps alive.
    R(*const T, usize),
    /// A copy.
    Owned(Vec<T>),
}

impl<T> Slice<T> {
    fn get(&self) -> &[T] {
        match self {
            // A vector of no elements holds no array, whose pointer the slice
            // does not take.
            Slice::R(_, 0) => &[],
            Slice::R(p, n) => unsafe { std::slice::from_raw_parts(*p, *n) },
            Slice::Owned(v) => v,
        }
    }
}

fn invalid(name: &str, e: Error) -> Error {
    Error::new("invalid_argument", format!("`{name}`: {}", e.message))
}

/// The elements of the R vector `x` of `T`'s type.
unsafe fn slice_of<T: Element>(x: SEXP, name: &str) -> Result<Slice<T>> {
    if unsafe { sys::TYPEOF(x) } != T::SEXPTYPE {
        return Err(invalid(
            name,
            Error::new("type_mismatch", "the R vector has another type"),
        ));
    }
    let len = unsafe { sys::Rf_xlength(x) } as usize;
    if len == 0 {
        return Ok(Slice::R(std::ptr::null(), 0));
    }
    let ptr = unsafe { sys::DATAPTR_OR_NULL(x) }.cast::<T>();
    if !ptr.is_null() {
        return Ok(Slice::R(ptr, len));
    }
    let mut v: Vec<T> = Vec::with_capacity(len);
    let out = v.spare_capacity_mut().as_mut_ptr().cast::<T>();
    const BLOCK: usize = 4096;
    let mut start = 0;
    while start < len {
        let n = BLOCK.min(len - start);
        let got = std::cell::Cell::new(0 as R_xlen_t);
        // A region read of a third party's ALTREP class may raise an R error.
        api::protect(&|| {
            got.set(unsafe { T::region(x, start as R_xlen_t, n as R_xlen_t, out.add(start)) });
            unsafe { sys::R_NilValue }
        })
        .map_err(api::Jumped::into_error)?;
        if got.get() as usize != n {
            return Err(invalid(
                name,
                Error::new(
                    "short_read",
                    "the R vector returned fewer elements than its length",
                ),
            ));
        }
        start += n;
    }
    unsafe { v.set_len(len) };
    Ok(Slice::Owned(v))
}

macro_rules! borrowed_slice {
    ($($t:ty),*) => {$(
        impl Borrowed for [$t] {
            type Holder = Slice<$t>;
            fn param_type() -> ParamType {
                ParamType::Typed(<$t as ElementType>::buffer_type())
            }
            unsafe fn hold(x: SEXP, name: &str) -> Result<Self::Holder> {
                unsafe { slice_of::<$t>(x, name) }
            }
            fn lend(h: &Self::Holder) -> &Self {
                h.get()
            }
        }
    )*};
}
borrowed_slice!(f64, i32, u8);

/// R's logicals are 32 bits wide, so a slice of `bool` is a copy, which the
/// check of the call has kept free of `NA`.
impl Borrowed for [bool] {
    type Holder = Vec<bool>;
    fn param_type() -> ParamType {
        ParamType::Typed(<crate::buffer::Logical as ElementType>::buffer_type())
    }
    unsafe fn hold(x: SEXP, name: &str) -> Result<Self::Holder> {
        let s = unsafe { slice_of::<crate::buffer::Logical>(x, name) }?;
        s.get()
            .iter()
            .map(|v| match v.0 {
                0 => Ok(false),
                i32::MIN => Err(invalid(
                    name,
                    Error::new("na_value", "the logical vector holds NA"),
                )),
                _ => Ok(true),
            })
            .collect()
    }
    fn lend(h: &Self::Holder) -> &Self {
        h
    }
}

/// The text of a string: the bytes of R's string where it is ASCII or R marks
/// it as UTF-8, or a translation.
pub enum Text {
    /// The bytes of the R string, which the call's frame keeps alive.
    R(*const u8, usize),
    /// A translation into UTF-8.
    Owned(String),
}

unsafe extern "C" {
    // R's C API since R 2.7.0.
    fn Rf_getCharCE(x: SEXP) -> c_int;
    fn LENGTH(x: SEXP) -> c_int;
}

impl Borrowed for str {
    type Holder = Text;
    fn param_type() -> ParamType {
        ParamType::Typed(<String as Encode>::field_type())
    }
    unsafe fn hold(x: SEXP, name: &str) -> Result<Self::Holder> {
        if unsafe { sys::TYPEOF(x) } != sys::STRSXP || unsafe { sys::Rf_xlength(x) } != 1 {
            return Err(invalid(
                name,
                Error::new("type_mismatch", "the argument is not one string"),
            ));
        }
        let c = unsafe { sys::STRING_ELT(x, 0) };
        if c == unsafe { sys::R_NaString } {
            return Err(invalid(name, Error::new("na_value", "the string is NA")));
        }
        let p = unsafe { sys::R_CHAR(c) }.cast::<u8>();
        let n = unsafe { LENGTH(c) } as usize;
        let bytes = unsafe { std::slice::from_raw_parts(p, n) };
        // An ASCII string, and one that R marks as UTF-8, are their bytes.
        if bytes.is_ascii() || unsafe { Rf_getCharCE(c) } == sys::CE_UTF8 {
            if std::str::from_utf8(bytes).is_ok() {
                return Ok(Text::R(p, n));
            }
            return Err(invalid(
                name,
                Error::new("not_utf8", "the string is not valid UTF-8"),
            ));
        }
        let p = std::cell::Cell::new(std::ptr::null::<std::ffi::c_char>());
        // A translation allocates, which may raise an R error.
        api::protect(&|| {
            p.set(unsafe { sys::Rf_translateCharUTF8(c) });
            unsafe { sys::R_NilValue }
        })
        .map_err(api::Jumped::into_error)?;
        let s = unsafe { std::ffi::CStr::from_ptr(p.get()) };
        Ok(Text::Owned(s.to_string_lossy().into_owned()))
    }
    fn lend(h: &Self::Holder) -> &Self {
        match h {
            Text::R(p, n) => {
                // `hold()` checked the bytes.
                unsafe { std::str::from_utf8_unchecked(std::slice::from_raw_parts(*p, *n)) }
            }
            Text::Owned(s) => s,
        }
    }
}

/// The holder of an optional borrowed argument: `None` for `NULL`.
///
/// # Safety
///
/// As [`Borrowed::hold()`].
#[doc(hidden)]
pub unsafe fn optional_hold<B: Borrowed + ?Sized>(
    x: SEXP,
    name: &str,
) -> Result<Option<B::Holder>> {
    if x == unsafe { sys::R_NilValue } {
        return Ok(None);
    }
    unsafe { B::hold(x, name) }.map(Some)
}

/// The R8 type of an optional borrowed parameter.
#[doc(hidden)]
pub fn optional_type<B: Borrowed + ?Sized>() -> ParamType {
    B::param_type().optional()
}
