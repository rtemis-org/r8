//! A safe view of the reader R8 lends a codec (`R8Reader`): the value R8 has
//! read from a document and validated, from which `RustStored::from_reader` and
//! `RustStoredEnum::from_reader` build a state without JSON text.

use std::ffi::{CStr, c_char};

use std::marker::PhantomData;

use rtemis_r8::abi;

use crate::api::{Error, Result};

/// A part of the value a [`Reader`] lends, valid as long as the reader; only
/// the reader makes one.
#[derive(Clone, Copy)]
pub struct Node<'a> {
    raw: abi::Node,
    reader: PhantomData<&'a abi::Reader>,
}

/// The reader R8 lends for one call; its nodes and the strings it lends live
/// as long as it does.
pub struct Reader<'a> {
    raw: &'a abi::Reader,
    kind: unsafe extern "C" fn(*mut std::ffi::c_void, abi::Node) -> u32,
}

/// An error that a reader function reported.
fn reported(e: &abi::Error) -> Error {
    let text = |b: &[c_char]| {
        let bytes: Vec<u8> = b
            .iter()
            .take_while(|&&c| c != 0)
            .map(|&c| c as u8)
            .collect();
        String::from_utf8_lossy(&bytes).into_owned()
    };
    Error::new(text(&e.code), text(&e.message))
}

fn blank() -> abi::Error {
    abi::Error {
        code: [0; 64],
        message: [0; 1024],
    }
}

fn missing(what: &str) -> Error {
    Error::new("reader", format!("the reader has no `{what}`"))
}

impl<'a> Reader<'a> {
    /// The reader at `raw`, whose functions R8 sets.
    ///
    /// # Safety
    ///
    /// `raw` is a reader R8 lends for the current call.
    pub(crate) unsafe fn new(raw: *const abi::Reader) -> Result<Self> {
        let raw = unsafe { raw.as_ref() }.ok_or_else(|| missing("table"))?;
        if (raw.size as usize) < std::mem::size_of::<abi::Reader>() {
            return Err(Error::new(
                "reader",
                format!(
                    "the reader has size {}; this codec reads {}",
                    raw.size,
                    std::mem::size_of::<abi::Reader>()
                ),
            ));
        }
        let kind = raw.kind.ok_or_else(|| missing("kind"))?;
        Ok(Reader { raw, kind })
    }

    /// The node R8 passes as a value's root.
    pub(crate) fn root(&self, raw: abi::Node) -> Node<'a> {
        Node {
            raw,
            reader: PhantomData,
        }
    }

    fn node(&self, raw: abi::Node) -> Node<'a> {
        self.root(raw)
    }

    /// The node's kind, an `abi::NODE_*` constant.
    pub fn kind(&self, n: Node<'a>) -> u32 {
        unsafe { (self.kind)(self.raw.context, n.raw) }
    }

    /// The node is absent or null.
    pub fn is_none(&self, n: Node<'a>) -> bool {
        matches!(self.kind(n), abi::NODE_ABSENT | abi::NODE_NULL)
    }

    /// An object's member `name`; absent when the object has none.
    pub fn field(&self, n: Node<'a>, name: &CStr) -> Node<'a> {
        self.node(match self.raw.field {
            Some(f) => unsafe { f(self.raw.context, n.raw, name.as_ptr()) },
            None => std::ptr::null(),
        })
    }

    /// An array's item `i`; absent past its end.
    pub fn item(&self, n: Node<'a>, i: usize) -> Node<'a> {
        self.node(match self.raw.item {
            Some(f) => unsafe { f(self.raw.context, n.raw, i) },
            None => std::ptr::null(),
        })
    }

    /// An array's or an object's length.
    pub fn length(&self, n: Node<'a>) -> usize {
        self.raw
            .length
            .map_or(0, |f| unsafe { f(self.raw.context, n.raw) })
    }

    /// A logical: 0, 1, or `abi::NA_INTEGER`.
    pub fn logical(&self, n: Node<'a>) -> Result<i32> {
        let f = self.raw.logical.ok_or_else(|| missing("logical"))?;
        let (mut out, mut e) = (0, blank());
        match unsafe { f(self.raw.context, n.raw, &mut out, &mut e) } {
            abi::OK => Ok(out),
            _ => Err(reported(&e)),
        }
    }

    /// A number; null reads as R's `NA_real_`.
    pub fn number(&self, n: Node<'a>) -> Result<f64> {
        let f = self.raw.number.ok_or_else(|| missing("number"))?;
        let (mut out, mut e) = (0.0, blank());
        match unsafe { f(self.raw.context, n.raw, &mut out, &mut e) } {
            abi::OK => Ok(out),
            _ => Err(reported(&e)),
        }
    }

    /// A string, lent for the call.
    pub fn string(&self, n: Node<'a>) -> Result<&'a str> {
        let f = self.raw.string.ok_or_else(|| missing("string"))?;
        let (mut p, mut len, mut e) = (std::ptr::null(), 0usize, blank());
        match unsafe { f(self.raw.context, n.raw, &mut p, &mut len, &mut e) } {
            abi::OK => {
                let bytes = unsafe { std::slice::from_raw_parts(p.cast::<u8>(), len) };
                std::str::from_utf8(bytes).map_err(|e| Error::new("not_utf8", e.to_string()))
            }
            _ => Err(reported(&e)),
        }
    }

    /// Items `start..start + out.len()` of an array of numbers.
    pub fn doubles(&self, n: Node<'a>, start: usize, out: &mut [f64]) -> Result<()> {
        let f = self.raw.doubles.ok_or_else(|| missing("doubles"))?;
        let mut e = blank();
        match unsafe {
            f(
                self.raw.context,
                n.raw,
                start,
                out.len(),
                out.as_mut_ptr(),
                &mut e,
            )
        } {
            abi::OK => Ok(()),
            _ => Err(reported(&e)),
        }
    }

    /// Items `start..start + out.len()` of an array of whole numbers.
    pub fn integers(&self, n: Node<'a>, start: usize, out: &mut [i32]) -> Result<()> {
        let f = self.raw.integers.ok_or_else(|| missing("integers"))?;
        let mut e = blank();
        match unsafe {
            f(
                self.raw.context,
                n.raw,
                start,
                out.len(),
                out.as_mut_ptr(),
                &mut e,
            )
        } {
            abi::OK => Ok(()),
            _ => Err(reported(&e)),
        }
    }

    /// Items `start..start + out.len()` of an array of logicals.
    pub fn logicals(&self, n: Node<'a>, start: usize, out: &mut [i32]) -> Result<()> {
        let f = self.raw.logicals.ok_or_else(|| missing("logicals"))?;
        let mut e = blank();
        match unsafe {
            f(
                self.raw.context,
                n.raw,
                start,
                out.len(),
                out.as_mut_ptr(),
                &mut e,
            )
        } {
            abi::OK => Ok(()),
            _ => Err(reported(&e)),
        }
    }
}
