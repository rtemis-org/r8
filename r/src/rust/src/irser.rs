//! A serde `Serializer` to R objects, the mirror of `irde`: the IR types'
//! own derives write a declaration as the R lists the declaration functions
//! build. A map or a struct is a named list, a sequence an unnamed list, a
//! scalar a length-one vector, `null` and an absent value `NULL`. An integer
//! within R's integer range is an integer, and any other integer is a
//! `json_ir()` of its text, as `r8_json_parse()` writes it, which `irde` reads
//! back exactly: a double does not hold every integer beyond 2^53. A number
//! with a fraction is a double.
//!
//! The writer allocates through R's C API. Each open list holds one slot of
//! R's protection stack, released in the order the lists were opened, and
//! each value is stored into its protected parent as soon as it is written,
//! so that no unprotected value outlives the next allocation.
//! spec: R8/class-system#declaring-types

use std::fmt;

use extendr_api::prelude::*;
use extendr_ffi::{
    INTEGER, LOGICAL, R_NilValue, RAW, REAL, Rf_allocVector, Rf_mkCharLenCE, Rf_protect,
    Rf_setAttrib, Rf_unprotect, Rf_xlengthgets, SET_STRING_ELT, SET_VECTOR_ELT, SEXP, SEXPTYPE,
    cetype_t,
};
use serde::ser::{self, Serialize};

use crate::rview::{sym_class, sym_names};

// R's C API: a protection slot whose value can be replaced, for a list that
// grows. `SEXP` is a pointer to R's opaque `SEXPREC`, which C code does not
// read through, as in extendr_ffi's own declarations of R's API.
#[allow(improper_ctypes)]
unsafe extern "C" {
    fn R_ProtectWithIndex(s: SEXP, index: *mut std::os::raw::c_int);
    fn R_Reprotect(s: SEXP, index: std::os::raw::c_int);
}

/// An error of writing a declaration as R objects.
#[derive(Debug)]
pub(crate) struct Error(String);

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.0)
    }
}

impl std::error::Error for Error {}

impl ser::Error for Error {
    fn custom<T: fmt::Display>(m: T) -> Self {
        Error(m.to_string())
    }
}

type Out = std::result::Result<SEXP, Error>;

/// Writes an IR value as R objects.
pub(crate) fn to_r<T: Serialize + ?Sized>(x: &T) -> std::result::Result<Robj, Error> {
    let sexp = x.serialize(Ser)?;
    // `from_sexp` protects the value before any further R allocation.
    Ok(unsafe { Robj::from_sexp(sexp) })
}

struct Ser;

/// A CHARSXP of UTF-8 text.
fn charsxp(s: &str) -> SEXP {
    unsafe { Rf_mkCharLenCE(s.as_ptr().cast(), s.len() as i32, cetype_t::CE_UTF8) }
}

fn string(s: &str) -> SEXP {
    unsafe {
        let out = Rf_protect(Rf_allocVector(SEXPTYPE::STRSXP, 1));
        SET_STRING_ELT(out, 0, charsxp(s));
        Rf_unprotect(1);
        out
    }
}

fn int(i: i32) -> SEXP {
    unsafe {
        let out = Rf_allocVector(SEXPTYPE::INTSXP, 1);
        *INTEGER(out) = i;
        out
    }
}

fn real(d: f64) -> SEXP {
    unsafe {
        let out = Rf_allocVector(SEXPTYPE::REALSXP, 1);
        *REAL(out) = d;
        out
    }
}

fn logical(b: bool) -> SEXP {
    unsafe {
        let out = Rf_allocVector(SEXPTYPE::LGLSXP, 1);
        *LOGICAL(out) = i32::from(b);
        out
    }
}

/// A `json_ir()` object of JSON text.
fn json_ir(text: &str) -> SEXP {
    unsafe {
        let out = Rf_protect(string(text));
        let class = Rf_protect(string("r8_json_ir"));
        Rf_setAttrib(out, sym_class(), class);
        Rf_unprotect(2);
        out
    }
}

fn integer(n: i64) -> SEXP {
    match i32::try_from(n) {
        Ok(i) if i != i32::MIN => int(i),
        _ => json_ir(&n.to_string()),
    }
}

/// A list being written: a VECSXP in its protection slot `slot`, with its
/// names when it is a map or a struct, filled up to `len`.
struct ListW {
    list: SEXP,
    slot: std::os::raw::c_int,
    named: bool,
    len: usize,
    key: Option<String>,
    open: bool,
}

impl ListW {
    fn new(capacity: usize, named: bool) -> Self {
        unsafe {
            let list = Rf_allocVector(SEXPTYPE::VECSXP, capacity as isize);
            let mut slot = 0;
            R_ProtectWithIndex(list, &mut slot);
            if named {
                let names = Rf_protect(Rf_allocVector(SEXPTYPE::STRSXP, capacity as isize));
                Rf_setAttrib(list, sym_names(), names);
                Rf_unprotect(1);
            }
            ListW {
                list,
                slot,
                named,
                len: 0,
                key: None,
                open: true,
            }
        }
    }

    /// Stores `value` at the end, doubling the capacity of a list whose
    /// length was not known. `Rf_xlengthgets()` keeps the names.
    fn push(&mut self, name: Option<&str>, value: SEXP) {
        unsafe {
            if self.len as isize >= extendr_ffi::Rf_xlength(self.list) {
                Rf_protect(value);
                let cap = (2 * self.len).max(4) as isize;
                self.list = Rf_xlengthgets(self.list, cap);
                R_Reprotect(self.list, self.slot);
                Rf_unprotect(1);
            }
            SET_VECTOR_ELT(self.list, self.len as isize, value);
            if let (true, Some(n)) = (self.named, name) {
                let names = extendr_ffi::Rf_getAttrib(self.list, sym_names());
                SET_STRING_ELT(names, self.len as isize, charsxp(n));
            }
            self.len += 1;
        }
    }

    fn finish(mut self) -> Out {
        unsafe {
            let mut out = self.list;
            if (self.len as isize) < extendr_ffi::Rf_xlength(out) {
                out = Rf_xlengthgets(out, self.len as isize);
            }
            self.open = false;
            Rf_unprotect(1);
            Ok(out)
        }
    }
}

/// A named list written member by member, each member stored as it is
/// written, for a list whose members are R objects already and IR values.
pub(crate) struct Builder(ListW);

impl Builder {
    /// A named list of at most `capacity` members.
    pub(crate) fn named(capacity: usize) -> Self {
        Builder(ListW::new(capacity, true))
    }

    /// Adds an R object, which the caller keeps protected until this call.
    pub(crate) fn object(&mut self, name: &str, x: SEXP) {
        self.0.push(Some(name), x);
    }

    /// Adds an IR value, written as R objects.
    pub(crate) fn value<T: Serialize + ?Sized>(
        &mut self,
        name: &str,
        x: &T,
    ) -> std::result::Result<(), Error> {
        self.0.field(name, x)
    }

    /// The list.
    pub(crate) fn list(self) -> Robj {
        unsafe { Robj::from_sexp(self.0.finish().unwrap_or(R_NilValue)) }
    }

    /// The list, of class `class`.
    pub(crate) fn finish(self, class: &str) -> Robj {
        unsafe {
            let out = Rf_protect(self.0.finish().unwrap_or(R_NilValue));
            let c = Rf_protect(string(class));
            Rf_setAttrib(out, sym_class(), c);
            let r = Robj::from_sexp(out);
            Rf_unprotect(2);
            r
        }
    }
}

impl Drop for ListW {
    // A write that fails releases the list's protection slot; lists are
    // dropped in the reverse order of their opening.
    fn drop(&mut self) {
        if self.open {
            unsafe { Rf_unprotect(1) };
        }
    }
}

impl ser::Serializer for Ser {
    type Ok = SEXP;
    type Error = Error;
    type SerializeSeq = ListW;
    type SerializeTuple = ListW;
    type SerializeTupleStruct = ListW;
    type SerializeTupleVariant = ListW;
    type SerializeMap = ListW;
    type SerializeStruct = ListW;
    type SerializeStructVariant = ListW;

    fn serialize_bool(self, v: bool) -> Out {
        Ok(logical(v))
    }
    fn serialize_i8(self, v: i8) -> Out {
        Ok(integer(v.into()))
    }
    fn serialize_i16(self, v: i16) -> Out {
        Ok(integer(v.into()))
    }
    fn serialize_i32(self, v: i32) -> Out {
        Ok(integer(v.into()))
    }
    fn serialize_i64(self, v: i64) -> Out {
        Ok(integer(v))
    }
    fn serialize_u8(self, v: u8) -> Out {
        Ok(integer(v.into()))
    }
    fn serialize_u16(self, v: u16) -> Out {
        Ok(integer(v.into()))
    }
    fn serialize_u32(self, v: u32) -> Out {
        Ok(integer(v.into()))
    }
    fn serialize_u64(self, v: u64) -> Out {
        Ok(i64::try_from(v).map_or_else(|_| json_ir(&v.to_string()), integer))
    }
    fn serialize_f32(self, v: f32) -> Out {
        Ok(real(f64::from(v)))
    }
    fn serialize_f64(self, v: f64) -> Out {
        Ok(real(v))
    }
    fn serialize_char(self, v: char) -> Out {
        Ok(string(&v.to_string()))
    }
    fn serialize_str(self, v: &str) -> Out {
        Ok(string(v))
    }
    fn serialize_bytes(self, v: &[u8]) -> Out {
        unsafe {
            let out = Rf_allocVector(SEXPTYPE::RAWSXP, v.len() as isize);
            std::ptr::copy_nonoverlapping(v.as_ptr(), RAW(out), v.len());
            Ok(out)
        }
    }
    fn serialize_none(self) -> Out {
        Ok(unsafe { R_NilValue })
    }
    fn serialize_some<T: Serialize + ?Sized>(self, v: &T) -> Out {
        v.serialize(self)
    }
    fn serialize_unit(self) -> Out {
        Ok(unsafe { R_NilValue })
    }
    fn serialize_unit_struct(self, _: &'static str) -> Out {
        Ok(unsafe { R_NilValue })
    }
    fn serialize_unit_variant(self, _: &'static str, _: u32, variant: &'static str) -> Out {
        Ok(string(variant))
    }
    fn serialize_newtype_struct<T: Serialize + ?Sized>(self, _: &'static str, v: &T) -> Out {
        v.serialize(self)
    }
    fn serialize_newtype_variant<T: Serialize + ?Sized>(
        self,
        _: &'static str,
        _: u32,
        variant: &'static str,
        v: &T,
    ) -> Out {
        let mut w = ListW::new(1, true);
        let value = v.serialize(Ser)?;
        w.push(Some(variant), value);
        w.finish()
    }
    fn serialize_seq(self, n: Option<usize>) -> std::result::Result<ListW, Error> {
        Ok(ListW::new(n.unwrap_or(0), false))
    }
    fn serialize_tuple(self, n: usize) -> std::result::Result<ListW, Error> {
        Ok(ListW::new(n, false))
    }
    fn serialize_tuple_struct(
        self,
        _: &'static str,
        n: usize,
    ) -> std::result::Result<ListW, Error> {
        Ok(ListW::new(n, false))
    }
    fn serialize_tuple_variant(
        self,
        _: &'static str,
        _: u32,
        _: &'static str,
        n: usize,
    ) -> std::result::Result<ListW, Error> {
        Ok(ListW::new(n, false))
    }
    fn serialize_map(self, n: Option<usize>) -> std::result::Result<ListW, Error> {
        Ok(ListW::new(n.unwrap_or(0), true))
    }
    fn serialize_struct(self, _: &'static str, n: usize) -> std::result::Result<ListW, Error> {
        Ok(ListW::new(n, true))
    }
    fn serialize_struct_variant(
        self,
        _: &'static str,
        _: u32,
        _: &'static str,
        n: usize,
    ) -> std::result::Result<ListW, Error> {
        Ok(ListW::new(n, true))
    }
}

impl ListW {
    fn element<T: Serialize + ?Sized>(&mut self, v: &T) -> std::result::Result<(), Error> {
        let value = v.serialize(Ser)?;
        self.push(None, value);
        Ok(())
    }

    fn field<T: Serialize + ?Sized>(&mut self, key: &str, v: &T) -> std::result::Result<(), Error> {
        let value = v.serialize(Ser)?;
        self.push(Some(key), value);
        Ok(())
    }
}

macro_rules! sequence {
    ($trait:path, $method:ident) => {
        impl $trait for ListW {
            type Ok = SEXP;
            type Error = Error;
            fn $method<T: Serialize + ?Sized>(&mut self, v: &T) -> std::result::Result<(), Error> {
                self.element(v)
            }
            fn end(self) -> Out {
                self.finish()
            }
        }
    };
}

sequence!(ser::SerializeSeq, serialize_element);
sequence!(ser::SerializeTuple, serialize_element);
sequence!(ser::SerializeTupleStruct, serialize_field);
sequence!(ser::SerializeTupleVariant, serialize_field);

/// The text of a map key, which the IR writes as a string.
struct KeyText;

impl ser::Serializer for KeyText {
    type Ok = String;
    type Error = Error;
    type SerializeSeq = ser::Impossible<String, Error>;
    type SerializeTuple = ser::Impossible<String, Error>;
    type SerializeTupleStruct = ser::Impossible<String, Error>;
    type SerializeTupleVariant = ser::Impossible<String, Error>;
    type SerializeMap = ser::Impossible<String, Error>;
    type SerializeStruct = ser::Impossible<String, Error>;
    type SerializeStructVariant = ser::Impossible<String, Error>;

    fn serialize_str(self, v: &str) -> std::result::Result<String, Error> {
        Ok(v.to_string())
    }
    fn serialize_unit_variant(
        self,
        _: &'static str,
        _: u32,
        variant: &'static str,
    ) -> std::result::Result<String, Error> {
        Ok(variant.to_string())
    }
    fn serialize_newtype_struct<T: Serialize + ?Sized>(
        self,
        _: &'static str,
        v: &T,
    ) -> std::result::Result<String, Error> {
        v.serialize(self)
    }
    fn serialize_bool(self, _: bool) -> std::result::Result<String, Error> {
        Err(key_error())
    }
    fn serialize_i8(self, _: i8) -> std::result::Result<String, Error> {
        Err(key_error())
    }
    fn serialize_i16(self, _: i16) -> std::result::Result<String, Error> {
        Err(key_error())
    }
    fn serialize_i32(self, _: i32) -> std::result::Result<String, Error> {
        Err(key_error())
    }
    fn serialize_i64(self, _: i64) -> std::result::Result<String, Error> {
        Err(key_error())
    }
    fn serialize_u8(self, _: u8) -> std::result::Result<String, Error> {
        Err(key_error())
    }
    fn serialize_u16(self, _: u16) -> std::result::Result<String, Error> {
        Err(key_error())
    }
    fn serialize_u32(self, _: u32) -> std::result::Result<String, Error> {
        Err(key_error())
    }
    fn serialize_u64(self, _: u64) -> std::result::Result<String, Error> {
        Err(key_error())
    }
    fn serialize_f32(self, _: f32) -> std::result::Result<String, Error> {
        Err(key_error())
    }
    fn serialize_f64(self, _: f64) -> std::result::Result<String, Error> {
        Err(key_error())
    }
    fn serialize_char(self, v: char) -> std::result::Result<String, Error> {
        Ok(v.to_string())
    }
    fn serialize_bytes(self, _: &[u8]) -> std::result::Result<String, Error> {
        Err(key_error())
    }
    fn serialize_none(self) -> std::result::Result<String, Error> {
        Err(key_error())
    }
    fn serialize_some<T: Serialize + ?Sized>(self, v: &T) -> std::result::Result<String, Error> {
        v.serialize(self)
    }
    fn serialize_unit(self) -> std::result::Result<String, Error> {
        Err(key_error())
    }
    fn serialize_unit_struct(self, _: &'static str) -> std::result::Result<String, Error> {
        Err(key_error())
    }
    fn serialize_newtype_variant<T: Serialize + ?Sized>(
        self,
        _: &'static str,
        _: u32,
        _: &'static str,
        _: &T,
    ) -> std::result::Result<String, Error> {
        Err(key_error())
    }
    fn serialize_seq(self, _: Option<usize>) -> std::result::Result<Self::SerializeSeq, Error> {
        Err(key_error())
    }
    fn serialize_tuple(self, _: usize) -> std::result::Result<Self::SerializeTuple, Error> {
        Err(key_error())
    }
    fn serialize_tuple_struct(
        self,
        _: &'static str,
        _: usize,
    ) -> std::result::Result<Self::SerializeTupleStruct, Error> {
        Err(key_error())
    }
    fn serialize_tuple_variant(
        self,
        _: &'static str,
        _: u32,
        _: &'static str,
        _: usize,
    ) -> std::result::Result<Self::SerializeTupleVariant, Error> {
        Err(key_error())
    }
    fn serialize_map(self, _: Option<usize>) -> std::result::Result<Self::SerializeMap, Error> {
        Err(key_error())
    }
    fn serialize_struct(
        self,
        _: &'static str,
        _: usize,
    ) -> std::result::Result<Self::SerializeStruct, Error> {
        Err(key_error())
    }
    fn serialize_struct_variant(
        self,
        _: &'static str,
        _: u32,
        _: &'static str,
        _: usize,
    ) -> std::result::Result<Self::SerializeStructVariant, Error> {
        Err(key_error())
    }
}

fn key_error() -> Error {
    Error("a map key is a string".to_string())
}

impl ser::SerializeMap for ListW {
    type Ok = SEXP;
    type Error = Error;
    fn serialize_key<T: Serialize + ?Sized>(&mut self, k: &T) -> std::result::Result<(), Error> {
        self.key = Some(k.serialize(KeyText)?);
        Ok(())
    }
    fn serialize_value<T: Serialize + ?Sized>(&mut self, v: &T) -> std::result::Result<(), Error> {
        let key = self
            .key
            .take()
            .ok_or_else(|| Error("a map value follows its key".to_string()))?;
        self.field(&key, v)
    }
    fn end(self) -> Out {
        self.finish()
    }
}

impl ser::SerializeStruct for ListW {
    type Ok = SEXP;
    type Error = Error;
    fn serialize_field<T: Serialize + ?Sized>(
        &mut self,
        key: &'static str,
        v: &T,
    ) -> std::result::Result<(), Error> {
        self.field(key, v)
    }
    fn end(self) -> Out {
        self.finish()
    }
}

impl ser::SerializeStructVariant for ListW {
    type Ok = SEXP;
    type Error = Error;
    fn serialize_field<T: Serialize + ?Sized>(
        &mut self,
        key: &'static str,
        v: &T,
    ) -> std::result::Result<(), Error> {
        self.field(key, v)
    }
    fn end(self) -> Out {
        self.finish()
    }
}
