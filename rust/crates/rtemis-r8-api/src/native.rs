//! `Native`: a Rust-native struct, whose values R8 holds as the codec's
//! state; `codec::<T>()`, the codec table R8 calls; and the `extern "C"`
//! functions behind it.

use std::ffi::{CStr, c_char, c_void};
use std::panic::{AssertUnwindSafe, catch_unwind};

use rtemis_r8::abi::{self, ERROR, OK, Status};

use crate::api::{Error, Result};
use crate::keep;
use crate::state::{StateReader, StateWriter};
use crate::sys::SEXP;

/// A struct whose values R8 holds in Rust. `#[derive(R8)]` with
/// `#[r8(native)]` implements it from the struct's fields, each a
/// [`Field`](crate::Field).
pub trait Native: rtemis_r8::derive::R8 + Send + Sync + Sized + 'static {
    /// The qualified type name, NUL-terminated.
    const TYPE_NAME: &'static CStr;

    /// The value from its fields, R values in declaration order that R8 has
    /// validated and converted to their stored forms.
    ///
    /// # Safety
    ///
    /// On R's main thread, with valid R objects.
    unsafe fn from_fields(fields: &[SEXP]) -> Result<Self>;

    /// The field `name` as an R value; `owner` keeps the state alive, for a
    /// view of its memory.
    fn get_field(&self, owner: SEXP, name: &str) -> Result<SEXP>;

    /// A value with the field `name` replaced by `value`, which R8 has
    /// validated; the unchanged fields share their buffers.
    ///
    /// # Safety
    ///
    /// On R's main thread, with a valid `value`.
    unsafe fn with_field(&self, name: &str, value: SEXP) -> Result<Self>;

    /// Writes the state's bytes.
    fn write_state(&self, out: &mut StateWriter) -> Result<()>;

    /// The value from bytes `write_state` wrote.
    fn read_state(input: &mut StateReader) -> Result<Self>;

    /// The memory Rust owns for the value.
    fn owned_bytes(&self) -> usize;
}

/// A table R8 reads through a pointer: static data without interior
/// mutability, so it is shared across threads.
#[repr(transparent)]
pub struct Shared<T>(pub T);

unsafe impl<T> Sync for Shared<T> {}

/// The codec table of `T`.
pub const fn codec<T: Native>() -> Shared<abi::TypeVTable> {
    Shared(abi::TypeVTable {
        size: std::mem::size_of::<abi::TypeVTable>() as u32,
        type_name: T::TYPE_NAME.as_ptr(),
        from_fields: Some(from_fields::<T>),
        decode: Some(decode::<T>),
        encode: Some(encode::<T>),
        write_state: Some(write_state::<T>),
        read_state: Some(read_state::<T>),
        get_field: Some(get_field::<T>),
        with_field: Some(with_field::<T>),
        drop: Some(drop_state::<T>),
        bytes: Some(bytes::<T>),
    })
}

/// The text of a panic's payload.
pub(crate) fn panic_text(p: &(dyn std::any::Any + Send)) -> String {
    p.downcast_ref::<&str>()
        .map(|s| s.to_string())
        .or_else(|| p.downcast_ref::<String>().cloned())
        .unwrap_or_else(|| "a panic without a message".to_string())
}

/// Runs `f` for R8: on the main thread, after the queued releases; an
/// `Err` fills `err` and returns `R8_ERROR`, and a panic does the same with
/// the code `panic`.
pub(crate) fn guard(err: *mut abi::Error, f: impl FnOnce() -> Result<()>) -> Status {
    keep::drain();
    match catch_unwind(AssertUnwindSafe(f)) {
        Ok(Ok(())) => OK,
        Ok(Err(e)) => {
            e.fill(err);
            ERROR
        }
        Err(p) => {
            Error::new("panic", panic_text(&*p)).fill(err);
            ERROR
        }
    }
}

fn state<'a, T>(p: *const c_void) -> &'a T {
    unsafe { &*p.cast::<T>() }
}

unsafe extern "C" fn from_fields<T: Native>(
    fields: *const abi::Sexp,
    n: usize,
    out: *mut *mut c_void,
    err: *mut abi::Error,
) -> Status {
    guard(err, || {
        let f: &[SEXP] = if n == 0 {
            &[]
        } else {
            unsafe { std::slice::from_raw_parts(fields, n) }
        };
        let v = unsafe { T::from_fields(f) }?;
        unsafe { *out = Box::into_raw(Box::new(v)).cast() };
        Ok(())
    })
}

unsafe extern "C" fn decode<T: Native>(
    json: *const u8,
    len: usize,
    out: *mut *mut c_void,
    err: *mut abi::Error,
) -> Status {
    guard(err, || {
        let bytes = unsafe { std::slice::from_raw_parts(json, len) };
        let text =
            std::str::from_utf8(bytes).map_err(|e| Error::new("decode_failed", e.to_string()))?;
        let v = T::from_document(text).map_err(|errors| {
            let m: Vec<String> = errors
                .iter()
                .map(|e| format!("{} at {}", e.message, e.path))
                .collect();
            Error::new("decode_failed", m.join("; "))
        })?;
        unsafe { *out = Box::into_raw(Box::new(v)).cast() };
        Ok(())
    })
}

unsafe extern "C" fn encode<T: Native>(
    s: *const c_void,
    w: *mut abi::Writer,
    err: *mut abi::Error,
) -> Status {
    guard(err, || {
        let doc = state::<T>(s).to_document().map_err(|errors| {
            let m: Vec<String> = errors
                .iter()
                .map(|e| format!("{} at {}", e.message, e.path))
                .collect();
            Error::new("encode_failed", m.join("; "))
        })?;
        StateWriter::new(w).write(doc.as_bytes())
    })
}

unsafe extern "C" fn write_state<T: Native>(
    s: *const c_void,
    w: *mut abi::Writer,
    err: *mut abi::Error,
) -> Status {
    guard(err, || state::<T>(s).write_state(&mut StateWriter::new(w)))
}

unsafe extern "C" fn read_state<T: Native>(
    bytes: *const u8,
    len: usize,
    out: *mut *mut c_void,
    err: *mut abi::Error,
) -> Status {
    guard(err, || {
        let b: &[u8] = if len == 0 {
            &[]
        } else {
            unsafe { std::slice::from_raw_parts(bytes, len) }
        };
        let mut r = StateReader::new(b);
        let v = T::read_state(&mut r)?;
        if !r.is_done() {
            return Err(Error::new(
                "bad_state",
                "the state's bytes continue past the value",
            ));
        }
        unsafe { *out = Box::into_raw(Box::new(v)).cast() };
        Ok(())
    })
}

unsafe extern "C" fn get_field<T: Native>(
    s: *const c_void,
    owner: abi::Sexp,
    field: *const c_char,
    value: *mut abi::Sexp,
    err: *mut abi::Error,
) -> Status {
    guard(err, || {
        let name = unsafe { CStr::from_ptr(field) }
            .to_str()
            .map_err(|e| Error::new("bad_field", e.to_string()))?;
        let v = state::<T>(s).get_field(owner, name)?;
        unsafe { *value = v };
        Ok(())
    })
}

unsafe extern "C" fn with_field<T: Native>(
    s: *const c_void,
    field: *const c_char,
    value: abi::Sexp,
    out: *mut *mut c_void,
    err: *mut abi::Error,
) -> Status {
    guard(err, || {
        let name = unsafe { CStr::from_ptr(field) }
            .to_str()
            .map_err(|e| Error::new("bad_field", e.to_string()))?;
        let v = unsafe { state::<T>(s).with_field(name, value) }?;
        unsafe { *out = Box::into_raw(Box::new(v)).cast() };
        Ok(())
    })
}

unsafe extern "C" fn drop_state<T: Native>(s: *mut c_void) {
    keep::drain();
    // A panic in a field's drop stops here; `drop` returns no status.
    let _ = catch_unwind(AssertUnwindSafe(|| {
        drop(unsafe { Box::from_raw(s.cast::<T>()) })
    }));
}

unsafe extern "C" fn bytes<T: Native>(s: *const c_void) -> usize {
    catch_unwind(AssertUnwindSafe(|| {
        std::mem::size_of::<T>() + state::<T>(s).owned_bytes()
    }))
    .unwrap_or(0)
}
