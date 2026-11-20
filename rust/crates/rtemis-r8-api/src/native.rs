//! `Native` and `NativeEnum`: a Rust-native struct or enum, whose values R8
//! holds as the codec's state; `codec::<T>()` and `enum_codec::<T>()`, the
//! codec tables R8 calls; `Codec`, which both kinds implement; and the
//! `extern "C"` functions behind them.

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

/// An enum whose values, unit variants included, R8 holds in Rust.
/// `#[derive(R8)]` with `#[r8(native)]` implements it from the enum's
/// variants: a struct variant's fields are each a [`Field`](crate::Field),
/// and a newtype variant's struct implements [`Native`].
pub trait NativeEnum: rtemis_r8::derive::R8 + Send + Sync + Sized + 'static {
    /// The qualified type name, NUL-terminated.
    const TYPE_NAME: &'static CStr;

    /// The value of the variant `variant` from its fields, R values in the
    /// variant's declaration order that R8 has validated and converted to
    /// their stored forms: none for a unit variant, and the fields of its
    /// struct for a newtype variant.
    ///
    /// # Safety
    ///
    /// On R's main thread, with valid R objects.
    unsafe fn from_variant(variant: &str, fields: &[SEXP]) -> Result<Self>;

    /// The name of the value's variant.
    fn variant(&self) -> &'static CStr;

    /// The field `name` of the value's variant as an R value; `owner` keeps
    /// the state alive, for a view of its memory.
    fn get_field(&self, owner: SEXP, name: &str) -> Result<SEXP>;

    /// A value of the same variant with the field `name` replaced by
    /// `value`, which R8 has validated; the unchanged fields share their
    /// buffers.
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

/// A Rust-native type of either kind: its name, its codec table, and its
/// value from an R-native value of the type. `#[derive(R8)]` with
/// `#[r8(native)]` implements it; `#[r8::methods]`, [`Value`](crate::Value),
/// and `r8::exports!` take it.
pub trait Codec: rtemis_r8::derive::R8 + Send + Sync + Sized + 'static {
    /// The qualified type name, NUL-terminated.
    const TYPE_NAME: &'static CStr;

    /// The codec table: [`codec`] for a struct, [`enum_codec`] for an enum.
    const TABLE: Shared<abi::TypeVTable>;

    /// The value from an R-native value `x` of the type, whose fields are
    /// `fields`, as `serialize(version = 2)` reads one back.
    ///
    /// # Safety
    ///
    /// On R's main thread, with valid R objects.
    unsafe fn from_r_native(x: SEXP, fields: &[SEXP]) -> Result<Self>;
}

/// A table R8 reads through a pointer: static data without interior
/// mutability, so it is shared across threads.
#[repr(transparent)]
pub struct Shared<T>(pub T);

unsafe impl<T> Sync for Shared<T> {}

/// The codec table of the struct `T`.
pub const fn codec<T: Native>() -> Shared<abi::TypeVTable> {
    Shared(abi::TypeVTable {
        size: std::mem::size_of::<abi::TypeVTable>() as u32,
        type_name: T::TYPE_NAME.as_ptr(),
        from_fields: Some(from_fields::<T>),
        decode: Some(struct_fns::decode::<T>),
        encode: Some(struct_fns::encode::<T>),
        write_state: Some(struct_fns::write_state::<T>),
        read_state: Some(struct_fns::read_state::<T>),
        get_field: Some(struct_fns::get_field::<T>),
        with_field: Some(struct_fns::with_field::<T>),
        drop: Some(struct_fns::drop_state::<T>),
        bytes: Some(struct_fns::bytes::<T>),
        variant: None,
        from_variant: None,
    })
}

/// The codec table of the enum `T`.
pub const fn enum_codec<T: NativeEnum>() -> Shared<abi::TypeVTable> {
    Shared(abi::TypeVTable {
        size: std::mem::size_of::<abi::TypeVTable>() as u32,
        type_name: T::TYPE_NAME.as_ptr(),
        from_fields: None,
        decode: Some(enum_fns::decode::<T>),
        encode: Some(enum_fns::encode::<T>),
        write_state: Some(enum_fns::write_state::<T>),
        read_state: Some(enum_fns::read_state::<T>),
        get_field: Some(enum_fns::get_field::<T>),
        with_field: Some(enum_fns::with_field::<T>),
        drop: Some(enum_fns::drop_state::<T>),
        bytes: Some(enum_fns::bytes::<T>),
        variant: Some(variant::<T>),
        from_variant: Some(from_variant::<T>),
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

/// The codec functions both kinds share, generic over the kind's trait.
macro_rules! shared_fns {
    ($module:ident, $tr:ident) => {
        mod $module {
            use std::ffi::{CStr, c_char, c_void};
            use std::panic::{AssertUnwindSafe, catch_unwind};

            use rtemis_r8::abi::{self, Status};

            use crate::api::Error;
            use crate::state::{StateReader, StateWriter};

            pub(super) unsafe extern "C" fn decode<T: super::$tr>(
                json: *const u8,
                len: usize,
                out: *mut *mut c_void,
                err: *mut abi::Error,
            ) -> Status {
                super::guard(err, || {
                    let bytes = unsafe { std::slice::from_raw_parts(json, len) };
                    let text = std::str::from_utf8(bytes)
                        .map_err(|e| Error::new("decode_failed", e.to_string()))?;
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

            pub(super) unsafe extern "C" fn encode<T: super::$tr>(
                s: *const c_void,
                w: *mut abi::Writer,
                err: *mut abi::Error,
            ) -> Status {
                super::guard(err, || {
                    let doc = super::state::<T>(s).to_document().map_err(|errors| {
                        let m: Vec<String> = errors
                            .iter()
                            .map(|e| format!("{} at {}", e.message, e.path))
                            .collect();
                        Error::new("encode_failed", m.join("; "))
                    })?;
                    StateWriter::new(w).write(doc.as_bytes())
                })
            }

            pub(super) unsafe extern "C" fn write_state<T: super::$tr>(
                s: *const c_void,
                w: *mut abi::Writer,
                err: *mut abi::Error,
            ) -> Status {
                super::guard(err, || {
                    super::state::<T>(s).write_state(&mut StateWriter::new(w))
                })
            }

            pub(super) unsafe extern "C" fn read_state<T: super::$tr>(
                bytes: *const u8,
                len: usize,
                out: *mut *mut c_void,
                err: *mut abi::Error,
            ) -> Status {
                super::guard(err, || {
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

            pub(super) unsafe extern "C" fn get_field<T: super::$tr>(
                s: *const c_void,
                owner: abi::Sexp,
                field: *const c_char,
                value: *mut abi::Sexp,
                err: *mut abi::Error,
            ) -> Status {
                super::guard(err, || {
                    let name = unsafe { CStr::from_ptr(field) }
                        .to_str()
                        .map_err(|e| Error::new("bad_field", e.to_string()))?;
                    let v = super::state::<T>(s).get_field(owner, name)?;
                    unsafe { *value = v };
                    Ok(())
                })
            }

            pub(super) unsafe extern "C" fn with_field<T: super::$tr>(
                s: *const c_void,
                field: *const c_char,
                value: abi::Sexp,
                out: *mut *mut c_void,
                err: *mut abi::Error,
            ) -> Status {
                super::guard(err, || {
                    let name = unsafe { CStr::from_ptr(field) }
                        .to_str()
                        .map_err(|e| Error::new("bad_field", e.to_string()))?;
                    let v = unsafe { super::state::<T>(s).with_field(name, value) }?;
                    unsafe { *out = Box::into_raw(Box::new(v)).cast() };
                    Ok(())
                })
            }

            pub(super) unsafe extern "C" fn drop_state<T: super::$tr>(s: *mut c_void) {
                crate::keep::drain();
                // A panic in a field's drop stops here; `drop` returns no status.
                let _ = catch_unwind(AssertUnwindSafe(|| {
                    drop(unsafe { Box::from_raw(s.cast::<T>()) })
                }));
            }

            pub(super) unsafe extern "C" fn bytes<T: super::$tr>(s: *const c_void) -> usize {
                catch_unwind(AssertUnwindSafe(|| {
                    std::mem::size_of::<T>() + super::state::<T>(s).owned_bytes()
                }))
                .unwrap_or(0)
            }
        }
    };
}

shared_fns!(struct_fns, Native);
shared_fns!(enum_fns, NativeEnum);

unsafe extern "C" fn variant<T: NativeEnum>(s: *const c_void) -> *const c_char {
    catch_unwind(AssertUnwindSafe(|| state::<T>(s).variant().as_ptr())).unwrap_or(std::ptr::null())
}

unsafe extern "C" fn from_variant<T: NativeEnum>(
    variant: *const c_char,
    fields: *const abi::Sexp,
    n: usize,
    out: *mut *mut c_void,
    err: *mut abi::Error,
) -> Status {
    guard(err, || {
        let name = unsafe { CStr::from_ptr(variant) }
            .to_str()
            .map_err(|e| Error::new("bad_variant", e.to_string()))?;
        let f: &[SEXP] = if n == 0 {
            &[]
        } else {
            unsafe { std::slice::from_raw_parts(fields, n) }
        };
        let v = unsafe { T::from_variant(name, f) }?;
        unsafe { *out = Box::into_raw(Box::new(v)).cast() };
        Ok(())
    })
}
