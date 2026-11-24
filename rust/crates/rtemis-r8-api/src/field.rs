//! `Field`: a field type of a Rust-stored struct, with its conversions from
//! and to R, its bytes in the codec's state, and the memory Rust owns for
//! it; and `Encode` for [`Buffer`], its declaration and JSON form. A
//! Rust-stored type is a field type, whose R value is a value of the type.

use rtemis_r8::derive::{Bytes, Encode};
use rtemis_r8::ir::FieldType;
use serde_json::Value;

use crate::api::{self, Error, Result};
use crate::buffer::{Buffer, Element, Logical};
use crate::call::{Receiver, rust_value};
use crate::reader::{Node, Reader};
use crate::rust_stored::RustStored;
use crate::state::{StateReader, StateWriter};
use crate::sys::{self, SEXP};

/// A field type of a Rust-stored struct. R8 validates a field and converts
/// it to its stored form before `from_r` reads it, so `from_r` checks only
/// what Rust needs.
#[diagnostic::on_unimplemented(
    message = "`{Self}` is not a field type of a Rust-stored type",
    note = "a field is a `bool`, an `i32`, an `f64`, a `String`, a `Buffer`, a Rust-stored type that is `Clone`, a `SubtypeValue`, an `ImplementorValue`, or an `Option` of one"
)]
pub trait Field: Clone + Send + Sync + 'static {
    /// The field from its R value in its stored form.
    ///
    /// # Safety
    ///
    /// On R's main thread, with a valid `x`.
    unsafe fn from_r(x: SEXP) -> Result<Self>;
    /// The field from the node of a value R8 has read from a document and
    /// validated.
    fn from_node<'a>(r: &Reader<'a>, n: Node<'a>) -> Result<Self>;
    /// The field as an R value; `owner` keeps the struct's state alive, for
    /// a view of its memory.
    fn to_r(&self, owner: SEXP) -> Result<SEXP>;
    /// Writes the field's bytes into the codec's state.
    fn write(&self, out: &mut StateWriter) -> Result<()>;
    /// Reads the field's bytes from the codec's state.
    fn read(input: &mut StateReader) -> Result<Self>;
    /// The memory Rust owns for the field beyond its inline size.
    fn owned_bytes(&self) -> usize {
        0
    }
}

/// An R object allocated through `R8Api.protect`.
fn alloc<F: Fn() -> SEXP>(f: &F) -> Result<SEXP> {
    api::protect(f).map_err(api::Jumped::into_error)
}

fn scalar(x: SEXP, t: std::ffi::c_int, what: &str) -> Result<()> {
    if unsafe { sys::TYPEOF(x) } != t || unsafe { sys::Rf_xlength(x) } != 1 {
        return Err(Error::new(
            "type_mismatch",
            format!("expected {what} of length 1"),
        ));
    }
    Ok(())
}

impl Field for f64 {
    unsafe fn from_r(x: SEXP) -> Result<Self> {
        scalar(x, sys::REALSXP, "a double")?;
        Ok(unsafe { sys::REAL_ELT(x, 0) })
    }
    fn from_node<'a>(r: &Reader<'a>, n: Node<'a>) -> Result<Self> {
        r.number(n)
    }
    fn to_r(&self, _owner: SEXP) -> Result<SEXP> {
        let v = *self;
        alloc(&|| unsafe { sys::Rf_ScalarReal(v) })
    }
    fn write(&self, out: &mut StateWriter) -> Result<()> {
        out.write(&self.to_le_bytes())
    }
    fn read(input: &mut StateReader) -> Result<Self> {
        Ok(f64::from_le_bytes(input.take_array()?))
    }
}

impl Field for i32 {
    unsafe fn from_r(x: SEXP) -> Result<Self> {
        scalar(x, sys::INTSXP, "an integer")?;
        Ok(unsafe { sys::INTEGER_ELT(x, 0) })
    }
    fn from_node<'a>(r: &Reader<'a>, n: Node<'a>) -> Result<Self> {
        if r.is_none(n) {
            return Ok(i32::MIN);
        }
        let x = r.number(n)?;
        if x.fract() == 0.0 && (-2147483647.0..=2147483647.0).contains(&x) {
            Ok(x as i32)
        } else {
            Err(Error::new(
                "type_mismatch",
                format!("{x} is not an integer"),
            ))
        }
    }
    fn to_r(&self, _owner: SEXP) -> Result<SEXP> {
        let v = *self;
        alloc(&|| unsafe { sys::Rf_ScalarInteger(v) })
    }
    fn write(&self, out: &mut StateWriter) -> Result<()> {
        out.write(&self.to_le_bytes())
    }
    fn read(input: &mut StateReader) -> Result<Self> {
        Ok(i32::from_le_bytes(input.take_array()?))
    }
}

impl Field for bool {
    unsafe fn from_r(x: SEXP) -> Result<Self> {
        scalar(x, sys::LGLSXP, "a logical")?;
        Logical(unsafe { sys::LOGICAL_ELT(x, 0) })
            .get()
            .ok_or_else(|| Error::new("na_value", "the logical is NA"))
    }
    fn from_node<'a>(r: &Reader<'a>, n: Node<'a>) -> Result<Self> {
        match r.logical(n)? {
            0 => Ok(false),
            1 => Ok(true),
            _ => Err(Error::new("na_value", "the logical is NA")),
        }
    }
    fn to_r(&self, _owner: SEXP) -> Result<SEXP> {
        let v = i32::from(*self);
        alloc(&|| unsafe { sys::Rf_ScalarLogical(v) })
    }
    fn write(&self, out: &mut StateWriter) -> Result<()> {
        out.write(&[u8::from(*self)])
    }
    fn read(input: &mut StateReader) -> Result<Self> {
        Ok(input.take_array::<1>()?[0] != 0)
    }
}

impl Field for String {
    unsafe fn from_r(x: SEXP) -> Result<Self> {
        scalar(x, sys::STRSXP, "a string")?;
        let c = unsafe { sys::STRING_ELT(x, 0) };
        if c == unsafe { sys::R_NaString } {
            return Err(Error::new("na_value", "the string is NA"));
        }
        let p = unsafe { sys::Rf_translateCharUTF8(c) };
        Ok(unsafe { std::ffi::CStr::from_ptr(p) }
            .to_string_lossy()
            .into_owned())
    }
    fn from_node<'a>(r: &Reader<'a>, n: Node<'a>) -> Result<Self> {
        r.string(n).map(str::to_string)
    }
    fn to_r(&self, _owner: SEXP) -> Result<SEXP> {
        let s: &str = self;
        let n = i32::try_from(s.len())
            .map_err(|_| Error::new("too_long", "the string is longer than R allows"))?;
        alloc(&|| unsafe {
            let v = sys::Rf_protect(sys::Rf_allocVector(sys::STRSXP, 1));
            sys::SET_STRING_ELT(
                v,
                0,
                sys::Rf_mkCharLenCE(s.as_ptr().cast(), n, sys::CE_UTF8),
            );
            sys::Rf_unprotect(1);
            v
        })
    }
    fn write(&self, out: &mut StateWriter) -> Result<()> {
        out.write(&(self.len() as u64).to_le_bytes())?;
        out.write(self.as_bytes())
    }
    fn read(input: &mut StateReader) -> Result<Self> {
        let n = u64::from_le_bytes(input.take_array()?) as usize;
        String::from_utf8(input.take(n)?.to_vec())
            .map_err(|e| Error::new("bad_state", e.to_string()))
    }
    fn owned_bytes(&self) -> usize {
        self.capacity()
    }
}

impl<T: Field> Field for Option<T> {
    unsafe fn from_r(x: SEXP) -> Result<Self> {
        if x == unsafe { sys::R_NilValue } {
            return Ok(None);
        }
        unsafe { T::from_r(x) }.map(Some)
    }
    fn from_node<'a>(r: &Reader<'a>, n: Node<'a>) -> Result<Self> {
        if r.is_none(n) {
            return Ok(None);
        }
        T::from_node(r, n).map(Some)
    }
    fn to_r(&self, owner: SEXP) -> Result<SEXP> {
        match self {
            Some(v) => v.to_r(owner),
            None => Ok(unsafe { sys::R_NilValue }),
        }
    }
    fn write(&self, out: &mut StateWriter) -> Result<()> {
        match self {
            Some(v) => {
                out.write(&[1])?;
                v.write(out)
            }
            None => out.write(&[0]),
        }
    }
    fn read(input: &mut StateReader) -> Result<Self> {
        match input.take_array::<1>()?[0] {
            0 => Ok(None),
            _ => T::read(input).map(Some),
        }
    }
    fn owned_bytes(&self) -> usize {
        self.as_ref().map_or(0, Field::owned_bytes)
    }
}

impl<T: Element> Field for Buffer<T> {
    unsafe fn from_r(x: SEXP) -> Result<Self> {
        unsafe { Buffer::from_r(x) }
    }
    fn from_node<'a>(r: &Reader<'a>, n: Node<'a>) -> Result<Self> {
        T::from_node(r, n).map(Buffer::from)
    }
    fn to_r(&self, owner: SEXP) -> Result<SEXP> {
        Buffer::to_r(self, owner)
    }
    fn write(&self, out: &mut StateWriter) -> Result<()> {
        out.write(&(self.len() as u64).to_le_bytes())?;
        out.write(self.as_bytes())
    }
    fn read(input: &mut StateReader) -> Result<Self> {
        let n = u64::from_le_bytes(input.take_array()?) as usize;
        let size = std::mem::size_of::<T>();
        let bytes = input.take(
            n.checked_mul(size)
                .ok_or_else(|| Error::new("bad_state", "the vector's length overflows"))?,
        )?;
        Buffer::from_bytes(bytes, n)
    }
    fn owned_bytes(&self) -> usize {
        Buffer::owned_bytes(self)
    }
}

/// A value of a Rust-stored type `T` exactly, which the holding value owns; its
/// R value is another value of `T`.
impl<T: RustStored + Clone> Field for T {
    unsafe fn from_r(x: SEXP) -> Result<Self> {
        match unsafe { Receiver::<T>::from_named_arg(x, "value") }? {
            Receiver::Borrowed(t) => Ok(t.clone()),
            Receiver::Built(t) => Ok(t),
        }
    }
    fn from_node<'a>(r: &Reader<'a>, n: Node<'a>) -> Result<Self> {
        <T as RustStored>::from_node(r, n)
    }
    fn to_r(&self, _owner: SEXP) -> Result<SEXP> {
        rust_value(self.clone()).map_err(api::Jumped::into_error)
    }
    fn write(&self, out: &mut StateWriter) -> Result<()> {
        self.write_bytes(out)
    }
    fn read(input: &mut StateReader) -> Result<Self> {
        T::read_bytes(input)
    }
    fn owned_bytes(&self) -> usize {
        self.heap_bytes()
    }
}

/// The declaration and JSON form of a [`Buffer`]'s elements, which `Encode`
/// for the buffer uses for documents.
pub trait ElementType: Element {
    /// The buffer's field type.
    fn buffer_type() -> FieldType;
    /// The buffer's JSON form.
    fn encode_all(items: &[Self]) -> std::result::Result<Value, String>;
    /// The buffer from its JSON form.
    fn decode_all(v: &Value) -> std::result::Result<Vec<Self>, String>;
}

macro_rules! vec_element {
    ($t:ty) => {
        impl ElementType for $t {
            fn buffer_type() -> FieldType {
                <Vec<$t> as Encode>::field_type()
            }
            fn encode_all(items: &[Self]) -> std::result::Result<Value, String> {
                items.to_vec().encode()
            }
            fn decode_all(v: &Value) -> std::result::Result<Vec<Self>, String> {
                <Vec<$t> as Encode>::decode(v)
            }
        }
    };
}
vec_element!(f64);
vec_element!(i32);

impl ElementType for Logical {
    fn buffer_type() -> FieldType {
        <Vec<bool> as Encode>::field_type()
    }
    fn encode_all(items: &[Self]) -> std::result::Result<Value, String> {
        items
            .iter()
            .map(|l| {
                l.get()
                    .map(Value::Bool)
                    .ok_or_else(|| "NA has no form in a vec(type_logical())".to_string())
            })
            .collect::<std::result::Result<Vec<_>, _>>()
            .map(Value::Array)
    }
    fn decode_all(v: &Value) -> std::result::Result<Vec<Self>, String> {
        Ok(<Vec<bool> as Encode>::decode(v)?
            .into_iter()
            .map(Logical::from)
            .collect())
    }
}

impl ElementType for u8 {
    fn buffer_type() -> FieldType {
        <Bytes as Encode>::field_type()
    }
    fn encode_all(items: &[Self]) -> std::result::Result<Value, String> {
        Bytes(items.to_vec()).encode()
    }
    fn decode_all(v: &Value) -> std::result::Result<Vec<Self>, String> {
        Ok(<Bytes as Encode>::decode(v)?.0)
    }
}

impl<T: ElementType> Encode for Buffer<T> {
    fn field_type() -> FieldType {
        T::buffer_type()
    }
    fn encode(&self) -> std::result::Result<Value, String> {
        T::encode_all(self)
    }
    fn decode(v: &Value) -> std::result::Result<Self, String> {
        T::decode_all(v).map(Buffer::from)
    }
}
