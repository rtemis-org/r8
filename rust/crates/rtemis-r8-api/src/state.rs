//! The codec's state bytes: a writer over R8's `R8Writer`, and a reader
//! over the bytes `read_state` receives. Scalars and lengths are
//! little-endian; a [`Buffer`](crate::Buffer)'s elements are written in the
//! host's byte order, which every platform R8 builds for is little-endian.

use rtemis_r8::abi;

use crate::api::{Error, Result};

/// Writes a state's bytes through R8's writer.
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
}

/// Reads a state's bytes in order.
pub struct StateReader<'a> {
    bytes: &'a [u8],
}

impl<'a> StateReader<'a> {
    pub(crate) fn new(bytes: &'a [u8]) -> Self {
        StateReader { bytes }
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
