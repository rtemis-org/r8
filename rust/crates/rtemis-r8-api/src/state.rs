//! The codec's state bytes and the R values kept beside them: a writer over
//! R8's `R8Writer`, and a reader over what `read_state` receives. Scalars and lengths are
//! little-endian; a [`Buffer`](crate::Buffer)'s elements are written in the
//! host's byte order, which every platform R8 builds for is little-endian.

use rtemis_r8::abi;

use crate::api::{Error, Result};
use crate::sys::{self, SEXP};

/// Writes a state's bytes, and keeps its R values, through R8's writer.
pub struct StateWriter {
    writer: *mut abi::Writer,
}

impl StateWriter {
    pub(crate) fn new(writer: *mut abi::Writer) -> Self {
        StateWriter { writer }
    }

    /// Writes `bytes`, in one call to R8's writer.
    pub fn write(&mut self, bytes: &[u8]) -> Result<()> {
        let w = unsafe { &*self.writer };
        let Some(f) = w.write else {
            return Err(Error::new("no_writer", "R8's writer has no `write`"));
        };
        if unsafe { f(w.context, bytes.as_ptr(), bytes.len()) } != abi::OK {
            return Err(Error::new("write_failed", "R8's writer refused the bytes"));
        }
        Ok(())
    }

    /// Keeps the R value `x` beside the state's bytes, and writes its
    /// position among the kept values into them.
    pub(crate) fn keep(&mut self, x: SEXP) -> Result<()> {
        let w = unsafe { &*self.writer };
        let Some(f) = w.keep else {
            return Err(Error::new("no_writer", "R8's writer keeps no R value"));
        };
        let mut index = 0usize;
        if unsafe { f(w.context, x, &mut index) } != abi::OK {
            return Err(Error::new(
                "write_failed",
                "R8's writer refused the R value",
            ));
        }
        self.write(&(index as u64).to_le_bytes())
    }
}

/// Reads a state's bytes in order, and the R values kept beside them.
pub struct StateReader<'a> {
    bytes: &'a [u8],
    kept: SEXP,
}

impl<'a> StateReader<'a> {
    pub(crate) fn new(bytes: &'a [u8], kept: SEXP) -> Self {
        StateReader { bytes, kept }
    }

    /// The kept R value whose position the next bytes hold; R protects it
    /// during `read_state`.
    pub(crate) fn kept(&mut self) -> Result<SEXP> {
        let i = u64::from_le_bytes(self.take_array()?);
        let n = match unsafe { sys::TYPEOF(self.kept) } {
            sys::VECSXP => (unsafe { sys::Rf_xlength(self.kept) }) as u64,
            _ => 0,
        };
        if i >= n {
            return Err(Error::new(
                "bad_state",
                format!("the state keeps {n} R values, and its bytes name value {i}"),
            ));
        }
        Ok(unsafe { sys::VECTOR_ELT(self.kept, i as sys::R_xlen_t) })
    }

    /// The next `n` bytes.
    pub fn take(&mut self, n: usize) -> Result<&'a [u8]> {
        if n > self.bytes.len() {
            return Err(Error::new("bad_state", "the state's bytes end early"));
        }
        let (head, rest) = self.bytes.split_at(n);
        self.bytes = rest;
        Ok(head)
    }

    /// The next `N` bytes as an array.
    pub fn take_array<const N: usize>(&mut self) -> Result<[u8; N]> {
        let b = self.take(N)?;
        Ok(b.try_into().expect("take returns N bytes"))
    }

    /// Whether every byte has been read.
    pub fn is_done(&self) -> bool {
        self.bytes.is_empty()
    }
}
