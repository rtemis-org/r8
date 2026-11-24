//! The strings of R objects as Rust text. R marks a string as UTF-8, Latin-1,
//! bytes, or in the native encoding, and checks none; a string of invalid bytes
//! taken for a `&str` ends the process where its characters are walked.

use std::borrow::Cow;
use std::ops::Deref;

use extendr_api::prelude::*;
use extendr_api::{Error, Result, SEXP};
use extendr_ffi::{LENGTH, R_CHAR, R_xlen_t, SEXPTYPE, STRING_ELT, cetype_t};

use crate::rview::{sexptype, xlen};

#[allow(improper_ctypes)]
unsafe extern "C" {
    fn Rf_getCharCE(c: SEXP) -> cetype_t;
    fn Rf_translateCharUTF8(c: SEXP) -> *const std::ffi::c_char;
    fn vmaxget() -> *mut std::ffi::c_void;
    fn vmaxset(top: *const std::ffi::c_void);
}

/// The bytes of a `CHARSXP`.
fn bytes_of<'a>(c: SEXP) -> &'a [u8] {
    unsafe { std::slice::from_raw_parts(R_CHAR(c).cast::<u8>(), LENGTH(c) as usize) }
}

/// The text of a `CHARSXP`: its own bytes where they are UTF-8, translated from
/// Latin-1 or from the native encoding; the error is the first byte, counted from
/// one, that is no text.
pub(crate) fn text<'a>(c: SEXP) -> std::result::Result<Cow<'a, str>, usize> {
    let bytes = bytes_of(c);
    if bytes.is_ascii() {
        // SAFETY: ASCII is UTF-8.
        return Ok(Cow::Borrowed(unsafe {
            std::str::from_utf8_unchecked(bytes)
        }));
    }
    let marked = unsafe { Rf_getCharCE(c) };
    let checked = || match std::str::from_utf8(bytes) {
        Ok(s) => Ok(Cow::Borrowed(s)),
        Err(e) => Err(e.valid_up_to() + 1),
    };
    if marked == cetype_t::CE_UTF8 || marked == cetype_t::CE_BYTES {
        return checked();
    }
    if marked == cetype_t::CE_NATIVE && checked().is_ok() {
        return checked();
    }
    if marked == cetype_t::CE_NATIVE && native_is_utf8() {
        return checked();
    }
    // R_alloc() memory, released here.
    unsafe {
        let top = vmaxget();
        let p = Rf_translateCharUTF8(c);
        let out = std::ffi::CStr::from_ptr(p).to_string_lossy().into_owned();
        vmaxset(top);
        Ok(Cow::Owned(out))
    }
}

/// Whether the native encoding of the session is UTF-8, asked once.
fn native_is_utf8() -> bool {
    static IS: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *IS.get_or_init(|| {
        R!("base::l10n_info()[['UTF-8']]")
            .ok()
            .and_then(|x| x.as_bool())
            .unwrap_or(true)
    })
}

/// What to do about a string that is no text.
pub(crate) const ADVICE: &str = "read it with its encoding, such as readLines(file, encoding = \"latin1\"), and convert it with enc2utf8()";

/// The first string of a character vector, or of its names, that is no text: its
/// position, counted from one, and the position of the byte.
pub(crate) fn invalid(x: SEXP) -> Option<(usize, usize)> {
    if sexptype(x) != SEXPTYPE::STRSXP {
        return None;
    }
    (0..xlen(x)).find_map(|i| {
        let c = unsafe { STRING_ELT(x, i as R_xlen_t) };
        text(c).err().map(|at| (i + 1, at))
    })
}

/// A one-string argument of a routine.
pub(crate) struct Utf8(String);

impl Deref for Utf8 {
    type Target = str;

    fn deref(&self) -> &str {
        &self.0
    }
}

fn argument(x: SEXP, i: usize) -> Result<String> {
    let c = unsafe { STRING_ELT(x, i as R_xlen_t) };
    if c == unsafe { extendr_ffi::R_NaString } {
        return Err(Error::Other("a string is NA".to_string()));
    }
    text(c).map(Cow::into_owned).map_err(|at| {
        Error::Other(format!(
            "a string is not valid UTF-8 at byte {at}; {ADVICE}"
        ))
    })
}

impl TryFrom<&Robj> for Utf8 {
    type Error = Error;

    fn try_from(x: &Robj) -> Result<Self> {
        let x = unsafe { x.get() };
        if sexptype(x) != SEXPTYPE::STRSXP || xlen(x) != 1 {
            return Err(Error::Other("an argument is one string".to_string()));
        }
        argument(x, 0).map(Utf8)
    }
}

/// A character argument of a routine.
pub(crate) struct Utf8s(Vec<String>);

impl Deref for Utf8s {
    type Target = [String];

    fn deref(&self) -> &[String] {
        &self.0
    }
}

impl TryFrom<&Robj> for Utf8s {
    type Error = Error;

    fn try_from(x: &Robj) -> Result<Self> {
        let x = unsafe { x.get() };
        if sexptype(x) != SEXPTYPE::STRSXP {
            return Err(Error::Other("an argument is character".to_string()));
        }
        (0..xlen(x))
            .map(|i| argument(x, i))
            .collect::<Result<_>>()
            .map(Utf8s)
    }
}

/// The one string of `x`.
pub(crate) fn one(x: &Robj) -> Option<String> {
    Utf8::try_from(x).ok().map(|s| s.0)
}

/// The strings of `x`, `None` for `NA` and for one that is no text.
pub(crate) fn all(x: &Robj) -> Option<Vec<Option<String>>> {
    let s = unsafe { x.get() };
    (sexptype(s) == SEXPTYPE::STRSXP).then(|| {
        crate::rview::strings(s)
            .into_iter()
            .map(|v| v.map(Cow::into_owned))
            .collect()
    })
}

/// The names of `x`.
pub(crate) fn names(x: &Robj) -> Option<Vec<String>> {
    let names = crate::rview::attr(unsafe { x.get() }, crate::rview::sym_names());
    (sexptype(names) == SEXPTYPE::STRSXP).then(|| {
        crate::rview::strings(names)
            .into_iter()
            .map(|v| v.map(Cow::into_owned).unwrap_or_default())
            .collect()
    })
}
