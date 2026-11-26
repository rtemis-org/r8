//! R8's API table (`R8Api`), errors, and calls of R functions that can jump.

use std::ffi::{c_char, c_void};
use std::sync::atomic::{AtomicPtr, Ordering};

use rtemis_r8::abi::{self, ABI_VERSION, OK, Status, UNWIND};

use crate::sys::{self, SEXP};

/// An error an impl function or a codec returns to R8: a stable code, such
/// as `zero_divisor`, and a message that says what went wrong and how to fix
/// it. R raises it as an error whose message ends with the code in
/// brackets.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Error {
    /// The stable code.
    pub code: String,
    /// The message.
    pub message: String,
}

impl Error {
    /// An error of `code` with `message`.
    pub fn new(code: impl Into<String>, message: impl Into<String>) -> Self {
        Error {
            code: code.into(),
            message: message.into(),
        }
    }

    /// Writes the error into R8's fixed buffers, truncated to fit.
    pub(crate) fn fill(&self, err: *mut abi::Error) {
        if err.is_null() {
            return;
        }
        let e = unsafe { &mut *err };
        fill(&mut e.code, &self.code);
        fill(&mut e.message, &self.message);
    }

    /// The error R8 filled into `err`.
    pub(crate) fn from_abi(err: &abi::Error) -> Self {
        Error::new(text(&err.code), text(&err.message))
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} [{}]", self.message, self.code)
    }
}

impl std::error::Error for Error {}

/// The result of an impl function or a conversion.
pub type Result<T> = std::result::Result<T, Error>;

fn fill(to: &mut [c_char], s: &str) {
    // Truncation keeps whole UTF-8 characters.
    let mut n = s.len().min(to.len() - 1);
    while !s.is_char_boundary(n) {
        n -= 1;
    }
    for (d, b) in to.iter_mut().zip(&s.as_bytes()[..n]) {
        *d = *b as c_char;
    }
    to[n] = 0;
}

fn text(b: &[c_char]) -> String {
    let bytes: Vec<u8> = b
        .iter()
        .take_while(|&&c| c != 0)
        .map(|&c| c as u8)
        .collect();
    String::from_utf8_lossy(&bytes).into_owned()
}

/// An empty error report, for R8 to fill.
pub(crate) fn blank() -> abi::Error {
    abi::Error {
        code: [0; 64],
        message: [0; 1024],
    }
}

static API: AtomicPtr<abi::Api> = AtomicPtr::new(std::ptr::null_mut());

/// R8's API table of ABI version `ABI_VERSION`, fetched with
/// `R_GetCCallable("R8", "r8_api")` on first use. A package using the crate
/// imports R8, so R8 is loaded first.
pub(crate) fn api() -> Result<&'static abi::Api> {
    let p = API.load(Ordering::Acquire);
    if !p.is_null() {
        return Ok(unsafe { &*p });
    }
    let f = unsafe { sys::R_GetCCallable(c"R8".as_ptr(), c"r8_api".as_ptr()) };
    if f.is_null() {
        return Err(Error::new("no_api", "R8 provides no `r8_api`; update R8"));
    }
    let f: unsafe extern "C" fn(u32) -> *const abi::Api = unsafe { std::mem::transmute(f) };
    let a = unsafe { f(ABI_VERSION) };
    if a.is_null() {
        return Err(Error::new(
            "abi_version",
            format!("R8 provides no API of ABI version {ABI_VERSION}; update R8"),
        ));
    }
    // A table of this ABI version holds at least the fields read here.
    if (unsafe { (*a).size } as usize) < abi::API_MIN_SIZE {
        return Err(Error::new(
            "abi_size",
            "R8's API table is older than this crate; update R8",
        ));
    }
    API.store(a.cast_mut(), Ordering::Release);
    Ok(unsafe { &*a })
}

/// The outcome of a call through [`protect`] when R jumped: R8 keeps the
/// jump and continues it once the impl function returns.
#[derive(Debug)]
pub enum Jumped {
    /// R jumped (an error or an interrupt).
    Unwind(Error),
    /// R8 refused the call.
    Error(Error),
}

/// The code of an [`Error`] that stands for a jump of R that `R8Api.protect`
/// caught: a codec function or an impl that returns it reports `R8_UNWIND`,
/// and R8 continues the jump from a C frame.
pub(crate) const UNWIND_CODE: &str = "unwind";

impl Jumped {
    /// The error, for code that carries a [`crate::Result`]: a jump keeps the
    /// code [`UNWIND_CODE`], which the function R8 called reports as
    /// `R8_UNWIND`.
    pub(crate) fn into_error(self) -> Error {
        match self {
            Jumped::Unwind(_) => {
                Error::new(UNWIND_CODE, "R raised a condition inside R8Api.protect")
            }
            Jumped::Error(e) => e,
        }
    }
}

/// Calls `f`, which may call R functions that jump, through
/// `R8Api.protect`. A jump skips the rest of `f`, so `f` owns nothing that
/// needs dropping: it borrows, and returns an R object.
pub(crate) fn protect<F: Fn() -> SEXP>(f: &F) -> std::result::Result<SEXP, Jumped> {
    extern "C" fn trampoline<F: Fn() -> SEXP>(data: *mut c_void) -> SEXP {
        let f = unsafe { &*data.cast::<F>() };
        f()
    }
    let a = api().map_err(Jumped::Error)?;
    let Some(p) = a.protect else {
        return Err(Jumped::Error(Error::new(
            "no_protect",
            "R8's API has no `protect`",
        )));
    };
    let mut value: abi::Sexp = unsafe { sys::R_NilValue };
    let mut err = blank();
    let status: Status = unsafe {
        p(
            trampoline::<F>,
            std::ptr::from_ref(f).cast_mut().cast(),
            &mut value,
            &mut err,
        )
    };
    match status {
        OK => Ok(value),
        UNWIND => Err(Jumped::Unwind(Error::from_abi(&err))),
        _ => Err(Jumped::Error(Error::from_abi(&err))),
    }
}
