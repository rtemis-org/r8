//! `RustStoredStruct` and `RustStoredEnum`: a Rust-stored struct or enum, whose values R8
//! holds as the codec's state; `codec::<T>()` and `enum_codec::<T>()`, the
//! codec tables R8 calls; `RustStored`, which both kinds implement; and the
//! `extern "C"` functions behind them.

use crate::reader::{Node, Reader};
use std::ffi::{CStr, c_char, c_void};
use std::panic::{AssertUnwindSafe, catch_unwind};

use rtemis_r8::abi::{self, ERROR, OK, Status};

use crate::api::{Error, Result};
use crate::keep;
use crate::state::{StateReader, StateWriter};
use crate::sys::SEXP;

/// A struct whose values R8 holds in Rust. `#[derive(R8)]` with
/// `#[r8(storage = "rust")]` implements it from the struct's fields, each a
/// [`Field`](crate::Field).
pub trait RustStoredStruct: rtemis_r8::derive::R8 + Send + Sync + Sized + 'static {
    /// The qualified type name, NUL-terminated.
    const TYPE_NAME: &'static CStr;

    /// The number of the struct's fields, those it inherits included.
    const N_FIELDS: usize;

    /// The value from its fields, R values in declaration order that R8 has
    /// validated and converted to their stored forms.
    ///
    /// # Safety
    ///
    /// On R's main thread, with valid R objects.
    unsafe fn from_fields(fields: &[SEXP]) -> Result<Self>;

    /// The value from the root node of a value R8 has read from a document
    /// and validated; each field is read by name.
    fn from_reader<'a>(r: &Reader<'a>, root: Node<'a>) -> Result<Self>;

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

    /// The memory Rust owns for the value beyond its inline size.
    fn owned_bytes(&self) -> usize;

    /// The state of the ancestor `type_name` that the value embeds; NULL when
    /// it embeds none of that name.
    fn upcast(&self, _type_name: &CStr) -> *const c_void {
        std::ptr::null()
    }
}

/// An enum whose values, unit variants included, R8 holds in Rust.
/// `#[derive(R8)]` with `#[r8(storage = "rust")]` implements it from the enum's
/// variants: a struct variant's fields are each a [`Field`](crate::Field),
/// and a newtype variant's struct implements [`RustStoredStruct`].
pub trait RustStoredEnum: rtemis_r8::derive::R8 + Send + Sync + Sized + 'static {
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

    /// The value from the root node of a value R8 has read from a document
    /// and validated: the variant from the variant's name, or from the tag
    /// member of an object, and its fields by name.
    fn from_reader<'a>(r: &Reader<'a>, root: Node<'a>) -> Result<Self>;

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

    /// The memory Rust owns for the value beyond its inline size.
    fn owned_bytes(&self) -> usize;
}

/// A Rust-stored type of either kind: its name, its codec table, and its
/// value from the fields of a value whose state is not lent. `#[derive(R8)]`
/// with `#[r8(storage = "rust")]` implements it. A type that `r8::exports!`
/// lists implements it, and a method of `#[r8::methods]` returns a value of
/// one, and takes it as `&T`.
#[diagnostic::on_unimplemented(
    message = "`{Self}` is not a Rust-stored type",
    note = "a Rust-stored type is declared with `#[derive(R8)]` and `#[r8(storage = \"rust\")]`"
)]
pub trait RustStored: rtemis_r8::derive::R8 + Send + Sync + Sized + 'static {
    /// The qualified type name, NUL-terminated.
    const TYPE_NAME: &'static CStr;

    /// The codec table: [`codec`] for a struct, [`enum_codec`] for an enum.
    const TABLE: Shared<abi::Codec>;

    /// The value from the fields of `x`, a value of the type whose state
    /// R8 does not lend to this module: an R-stored one, as
    /// `serialize(version = 2)` reads one back, or one of another package's
    /// codec. `x` may be a value of a struct that extends the type, whose
    /// first fields are the type's.
    ///
    /// # Safety
    ///
    /// On R's main thread, with valid R objects.
    unsafe fn from_value(x: SEXP, fields: &[SEXP]) -> Result<Self>;
}

/// Identifies this shared object to R8 (`R8Codec.module`): each package
/// links its own copy of the crate, and so has its own address. R8 lends a
/// state to the shared object whose code built it, since the layout of a Rust
/// type is fixed within one compilation only.
static MODULE: u8 = 0;

/// The address that identifies this shared object to R8.
pub const fn module() -> *const c_void {
    (&raw const MODULE).cast()
}

/// A table R8 reads through a pointer: static data without interior
/// mutability, so it is shared across threads.
#[repr(transparent)]
pub struct Shared<T>(pub T);

unsafe impl<T> Sync for Shared<T> {}

/// The codec table of the struct `T`.
pub const fn codec<T: RustStoredStruct>() -> Shared<abi::Codec> {
    Shared(abi::Codec {
        size: std::mem::size_of::<abi::Codec>() as u32,
        type_name: T::TYPE_NAME.as_ptr(),
        module: module(),
        from_fields: Some(from_fields::<T>),
        encode: Some(struct_fns::encode::<T>),
        write_state: Some(struct_fns::write_state::<T>),
        read_state: Some(struct_fns::read_state::<T>),
        get_field: Some(struct_fns::get_field::<T>),
        with_field: Some(struct_fns::with_field::<T>),
        drop: Some(struct_fns::drop_state::<T>),
        owned_bytes: Some(struct_fns::owned_bytes::<T>),
        variant: None,
        from_variant: None,
        from_reader: Some(struct_from_reader::<T>),
        declaration_hash: std::ptr::null(),
        upcast: Some(upcast::<T>),
    })
}

/// The codec table of the enum `T`.
pub const fn enum_codec<T: RustStoredEnum>() -> Shared<abi::Codec> {
    Shared(abi::Codec {
        size: std::mem::size_of::<abi::Codec>() as u32,
        type_name: T::TYPE_NAME.as_ptr(),
        module: module(),
        from_fields: None,
        encode: Some(enum_fns::encode::<T>),
        write_state: Some(enum_fns::write_state::<T>),
        read_state: Some(enum_fns::read_state::<T>),
        get_field: Some(enum_fns::get_field::<T>),
        with_field: Some(enum_fns::with_field::<T>),
        drop: Some(enum_fns::drop_state::<T>),
        owned_bytes: Some(enum_fns::owned_bytes::<T>),
        variant: Some(variant::<T>),
        from_variant: Some(from_variant::<T>),
        from_reader: Some(enum_from_reader::<T>),
        declaration_hash: std::ptr::null(),
        upcast: None,
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

unsafe extern "C" fn from_fields<T: RustStoredStruct>(
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

unsafe extern "C" fn struct_from_reader<T: RustStoredStruct>(
    r: *const abi::Reader,
    root: abi::Node,
    out: *mut *mut c_void,
    err: *mut abi::Error,
) -> Status {
    guard(err, || {
        let reader = unsafe { Reader::new(r) }?;
        let v = T::from_reader(&reader, reader.root(root))?;
        unsafe { *out = Box::into_raw(Box::new(v)).cast() };
        Ok(())
    })
}

unsafe extern "C" fn enum_from_reader<T: RustStoredEnum>(
    r: *const abi::Reader,
    root: abi::Node,
    out: *mut *mut c_void,
    err: *mut abi::Error,
) -> Status {
    guard(err, || {
        let reader = unsafe { Reader::new(r) }?;
        let v = T::from_reader(&reader, reader.root(root))?;
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

            pub(super) unsafe extern "C" fn owned_bytes<T: super::$tr>(s: *const c_void) -> usize {
                catch_unwind(AssertUnwindSafe(|| {
                    std::mem::size_of::<T>() + super::state::<T>(s).owned_bytes()
                }))
                .unwrap_or(0)
            }
        }
    };
}

shared_fns!(struct_fns, RustStoredStruct);
shared_fns!(enum_fns, RustStoredEnum);

unsafe extern "C" fn upcast<T: RustStoredStruct>(
    s: *const c_void,
    type_name: *const c_char,
) -> *const c_void {
    catch_unwind(AssertUnwindSafe(|| {
        state::<T>(s).upcast(unsafe { CStr::from_ptr(type_name) })
    }))
    .unwrap_or(std::ptr::null())
}

unsafe extern "C" fn variant<T: RustStoredEnum>(s: *const c_void) -> *const c_char {
    catch_unwind(AssertUnwindSafe(|| state::<T>(s).variant().as_ptr())).unwrap_or(std::ptr::null())
}

unsafe extern "C" fn from_variant<T: RustStoredEnum>(
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
