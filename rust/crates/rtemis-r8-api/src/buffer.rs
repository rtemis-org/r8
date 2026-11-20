//! `Buffer<T>`, the field type of a large vector in a Rust-native struct:
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
}

macro_rules! element {
    ($t:ty, $sexptype:expr, $region:ident, $view:ident, $vt:ty) => {
        impl Element for $t {
            const SEXPTYPE: c_int = $sexptype;
            unsafe fn region(x: SEXP, i: R_xlen_t, n: R_xlen_t, buf: *mut Self) -> R_xlen_t {
                unsafe { sys::$region(x, i, n, buf.cast()) }
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
        }
    };
}

element!(f64, sys::REALSXP, REAL_GET_REGION, view_doubles, f64);
element!(i32, sys::INTSXP, INTEGER_GET_REGION, view_integers, i32);
element!(Logical, sys::LGLSXP, LOGICAL_GET_REGION, view_logicals, i32);
element!(u8, sys::RAWSXP, RAW_GET_REGION, view_bytes, u8);

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

/// An immutable vector of `T` in a Rust-native struct: borrowed from an R
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
                let mut err = abi::Error {
                    code: [0; 64],
                    message: [0; 1024],
                };
                if unsafe { T::view(api, owner, a.as_ptr(), a.len(), &mut value, &mut err) }
                    != abi::OK
                {
                    return Err(Error::from_abi(&err));
                }
                Ok(value)
            }
        }
    }

    /// The memory Rust owns for the buffer; a borrowed vector is R's, which
    /// R accounts for.
    pub fn owned_bytes(&self) -> usize {
        match &self.backing {
            Backing::R { .. } => 0,
            Backing::Owned(a) => std::mem::size_of_val::<[T]>(a),
        }
    }

    /// The elements as bytes in the host's order, for a codec's state.
    pub(crate) fn as_bytes(&self) -> &[u8] {
        let s: &[T] = self;
        unsafe { std::slice::from_raw_parts(s.as_ptr().cast::<u8>(), std::mem::size_of_val(s)) }
    }

    /// An owned buffer of `len` elements read from `bytes` in the host's
    /// order.
    pub(crate) fn from_bytes(bytes: &[u8], len: usize) -> Result<Self> {
        let size = std::mem::size_of::<T>();
        if bytes.len() != len * size {
            return Err(Error::new(
                "bad_state",
                "the state's bytes do not hold the vector",
            ));
        }
        let mut v: Vec<T> = Vec::with_capacity(len);
        unsafe {
            std::ptr::copy_nonoverlapping(bytes.as_ptr(), v.as_mut_ptr().cast::<u8>(), bytes.len());
            v.set_len(len);
        }
        Ok(v.into())
    }
}
