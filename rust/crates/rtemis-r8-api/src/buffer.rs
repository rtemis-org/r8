//! `Buffer<T>`, the field type of a large vector in a Rust-stored struct:
//! immutable, read as `&[T]`, cloned in O(1), and either borrowed from an R
//! vector, which a [`Token`] keeps alive, or owned by Rust.

use std::ffi::c_int;
use std::ops::Deref;
use std::sync::Arc;

use rtemis_r8::abi;

use crate::api::{self, Error, Result};
use crate::keep::Token;
use crate::sys::{self, R_xlen_t, SEXP};

/// A logical in R's representation: 0, 1, or `NA` (`i32::MIN`).
#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Logical(pub i32);

impl Logical {
    /// R's `NA`.
    pub const NA: Logical = Logical(i32::MIN);

    /// The logical as a `bool`, `None` for `NA`.
    pub fn get(self) -> Option<bool> {
        (self.0 != i32::MIN).then_some(self.0 != 0)
    }
}

impl From<bool> for Logical {
    fn from(b: bool) -> Self {
        Logical(i32::from(b))
    }
}

/// An element type of a [`Buffer`]: its R vector type, its region reader,
/// and its view in R8's API.
pub trait Element: Copy + Send + Sync + 'static {
    /// The R vector type (`REALSXP`, `INTSXP`, `LGLSXP`, or `RAWSXP`).
    const SEXPTYPE: c_int;
    /// Reads elements `i..i + n` of `x` into `buf`, returning how many it
    /// read.
    ///
    /// # Safety
    ///
    /// `x` is a vector of `SEXPTYPE`, `buf` holds `n` elements, on R's main
    /// thread.
    unsafe fn region(x: SEXP, i: R_xlen_t, n: R_xlen_t, buf: *mut Self) -> R_xlen_t;
    /// The array of the R vector `x`, which R allocated.
    ///
    /// # Safety
    ///
    /// `x` is a vector of `SEXPTYPE` that R allocated, not ALTREP, on R's main
    /// thread.
    unsafe fn data_mut(x: SEXP) -> *mut Self;
    /// A view of `n` elements at `data`, which `owner` keeps alive.
    ///
    /// # Safety
    ///
    /// `data` holds `n` elements that `owner` keeps alive; `value` and `err`
    /// are writable.
    unsafe fn view(
        api: &abi::Api,
        owner: SEXP,
        data: *const Self,
        n: usize,
        value: *mut SEXP,
        err: *mut abi::Error,
    ) -> abi::Status;
    /// The elements of an array node that R8 lends through `r`: numbers for
    /// a double or an integer, logicals, or a base64 string for bytes.
    fn from_node<'a>(
        r: &crate::reader::Reader<'a>,
        n: crate::reader::Node<'a>,
    ) -> Result<Vec<Self>>;
}

/// A vector of `len` elements of `T` filled in one block by `fill`, whose
/// items have the layout of `U`.
fn block<T: Copy, U>(len: usize, fill: impl FnOnce(&mut [U]) -> Result<()>) -> Result<Vec<T>> {
    const { assert!(std::mem::size_of::<T>() == std::mem::size_of::<U>()) };
    let mut out: Vec<T> = Vec::with_capacity(len);
    // `fill` writes every item or fails; on failure the vector is dropped
    // without reading its items, which are plain numbers.
    let items = unsafe { std::slice::from_raw_parts_mut(out.as_mut_ptr().cast::<U>(), len) };
    fill(items)?;
    unsafe { out.set_len(len) };
    Ok(out)
}

macro_rules! element {
    ($t:ty, $sexptype:expr, $region:ident, $data:ident, $view:ident, $vt:ty, $from_node:expr) => {
        impl Element for $t {
            const SEXPTYPE: c_int = $sexptype;
            unsafe fn region(x: SEXP, i: R_xlen_t, n: R_xlen_t, buf: *mut Self) -> R_xlen_t {
                unsafe { sys::$region(x, i, n, buf.cast()) }
            }
            unsafe fn data_mut(x: SEXP) -> *mut Self {
                unsafe { sys::$data(x) }.cast()
            }
            unsafe fn view(
                api: &abi::Api,
                owner: SEXP,
                data: *const Self,
                n: usize,
                value: *mut SEXP,
                err: *mut abi::Error,
            ) -> abi::Status {
                match api.$view {
                    Some(f) => unsafe { f(owner, data.cast::<$vt>(), n, value, err) },
                    None => abi::ERROR,
                }
            }
            fn from_node<'a>(
                r: &crate::reader::Reader<'a>,
                n: crate::reader::Node<'a>,
            ) -> Result<Vec<Self>> {
                ($from_node)(r, n)
            }
        }
    };
}

element!(
    f64,
    sys::REALSXP,
    REAL_GET_REGION,
    REAL,
    view_doubles,
    f64,
    |r: &crate::reader::Reader<'a>, n: crate::reader::Node<'a>| {
        block::<f64, f64>(r.length(n), |out| r.doubles(n, 0, out))
    }
);
element!(
    i32,
    sys::INTSXP,
    INTEGER_GET_REGION,
    INTEGER,
    view_integers,
    i32,
    |r: &crate::reader::Reader<'a>, n: crate::reader::Node<'a>| {
        block::<i32, i32>(r.length(n), |out| r.integers(n, 0, out))
    }
);
element!(
    Logical,
    sys::LGLSXP,
    LOGICAL_GET_REGION,
    LOGICAL,
    view_logicals,
    i32,
    |r: &crate::reader::Reader<'a>, n: crate::reader::Node<'a>| {
        block::<Logical, i32>(r.length(n), |out| r.logicals(n, 0, out))
    }
);
element!(
    u8,
    sys::RAWSXP,
    RAW_GET_REGION,
    RAW,
    view_bytes,
    u8,
    |r: &crate::reader::Reader<'a>, n: crate::reader::Node<'a>| {
        r.string(n).map(rtemis_r8::access::base64_decode)
    }
);

enum Backing<T> {
    /// Elements of an R vector, which the token keeps alive.
    R {
        token: Arc<Token>,
        ptr: *const T,
        len: usize,
    },
    /// Elements Rust owns.
    Owned(Arc<[T]>),
}

/// An immutable vector of `T` in a Rust-stored struct: borrowed from an R
/// vector with a data pointer, or owned by Rust. It reads as `&[T]` on any
/// thread and clones in O(1).
pub struct Buffer<T: Element> {
    backing: Backing<T>,
}

// R does not move or change a vector that a token keeps alive, and the
// buffer only reads it.
unsafe impl<T: Element> Send for Buffer<T> {}
unsafe impl<T: Element> Sync for Buffer<T> {}

impl<T: Element> Clone for Buffer<T> {
    fn clone(&self) -> Self {
        Buffer {
            backing: match &self.backing {
                Backing::R { token, ptr, len } => Backing::R {
                    token: Arc::clone(token),
                    ptr: *ptr,
                    len: *len,
                },
                Backing::Owned(a) => Backing::Owned(Arc::clone(a)),
            },
        }
    }
}

impl<T: Element> Deref for Buffer<T> {
    type Target = [T];
    fn deref(&self) -> &[T] {
        match &self.backing {
            Backing::R { ptr, len, .. } => {
                if *len == 0 {
                    &[]
                } else {
                    unsafe { std::slice::from_raw_parts(*ptr, *len) }
                }
            }
            Backing::Owned(a) => a,
        }
    }
}

impl<T: Element + std::fmt::Debug> std::fmt::Debug for Buffer<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_list().entries(self.iter()).finish()
    }
}

impl<T: Element + PartialEq> PartialEq for Buffer<T> {
    fn eq(&self, other: &Self) -> bool {
        **self == **other
    }
}

impl<T: Element> From<Vec<T>> for Buffer<T> {
    fn from(v: Vec<T>) -> Self {
        Buffer {
            backing: Backing::Owned(v.into()),
        }
    }
}

impl<T: Element> From<&[T]> for Buffer<T> {
    fn from(v: &[T]) -> Self {
        Buffer {
            backing: Backing::Owned(v.into()),
        }
    }
}

/// Elements read per region call when a vector has no data pointer.
const BLOCK: usize = 4096;

impl<T: Element> Buffer<T> {
    /// The buffer of an R vector of `T`: borrowed when the vector has a data
    /// pointer, and filled block by block from its regions otherwise, as
    /// for a compact sequence, which stays compact.
    ///
    /// # Safety
    ///
    /// On R's main thread, with a valid `x`.
    pub(crate) unsafe fn from_r(x: SEXP) -> Result<Self> {
        if unsafe { sys::TYPEOF(x) } != T::SEXPTYPE {
            return Err(Error::new("type_mismatch", "the R vector has another type"));
        }
        let len = unsafe { sys::Rf_xlength(x) } as usize;
        let ptr = unsafe { sys::DATAPTR_OR_NULL(x) }.cast::<T>();
        if !ptr.is_null() || len == 0 {
            let token = unsafe { Token::keep(x) }?;
            return Ok(Buffer {
                backing: Backing::R {
                    token: Arc::new(token),
                    ptr,
                    len,
                },
            });
        }
        let mut v: Vec<T> = Vec::with_capacity(len);
        let out = v.spare_capacity_mut().as_mut_ptr().cast::<T>();
        let mut start = 0;
        while start < len {
            let n = BLOCK.min(len - start);
            let got = std::cell::Cell::new(0 as R_xlen_t);
            // A region read of a third party's ALTREP class may raise an R
            // error.
            api::protect(&|| {
                got.set(unsafe { T::region(x, start as R_xlen_t, n as R_xlen_t, out.add(start)) });
                unsafe { sys::R_NilValue }
            })
            .map_err(api::Jumped::into_error)?;
            if got.get() as usize != n {
                return Err(Error::new(
                    "short_read",
                    "the R vector returned fewer elements than its length",
                ));
            }
            start += n;
        }
        unsafe { v.set_len(len) };
        Ok(v.into())
    }

    /// The buffer as an R vector: the borrowed vector itself, or a view of
    /// the owned elements that `owner` keeps alive. Neither copies.
    pub(crate) fn to_r(&self, owner: SEXP) -> Result<SEXP> {
        match &self.backing {
            Backing::R { token, .. } => Ok(token.get()),
            Backing::Owned(a) => {
                let api = api::api()?;
                let mut value: SEXP = unsafe { sys::R_NilValue };
                let mut err = api::blank();
                if unsafe { T::view(api, owner, a.as_ptr(), a.len(), &mut value, &mut err) }
                    != abi::OK
                {
                    return Err(Error::from_abi(&err));
                }
                Ok(value)
            }
        }
    }

    /// The memory Rust owns for the buffer, as this holder's share of it: a
    /// buffer that `n` clones share counts `1/n` of its bytes in each, as
    /// the clones are counted when R8 wraps a value, so that values that
    /// share one buffer, such as a value and the value with a field replaced,
    /// count it about once. A borrowed vector is R's, which R accounts for.
    pub fn owned_bytes(&self) -> usize {
        match &self.backing {
            Backing::R { .. } => 0,
            Backing::Owned(a) => std::mem::size_of_val::<[T]>(a) / Arc::strong_count(a).max(1),
        }
    }
}
