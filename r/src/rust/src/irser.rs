//! A serde `Serializer` to R objects, the mirror of `irde`: the IR types'
//! own derives write a declaration as the R lists the declaration functions
//! build. A map or a struct is a named list, a sequence an unnamed list, a
//! scalar a length-one vector, `null` and an absent value `NULL`. An integer
//! within R's integer range is an integer, any other number a double.
//! spec: R8/class-system#declaring-types

use std::fmt;

use extendr_api::prelude::*;
use serde::ser::{self, Serialize};

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

/// Writes an IR value as R objects.
pub(crate) fn to_r<T: Serialize + ?Sized>(x: &T) -> std::result::Result<Robj, Error> {
    x.serialize(Ser)
}

struct Ser;

fn integer(n: i64) -> Robj {
    match i32::try_from(n) {
        Ok(i) if i != i32::MIN => Robj::from(i),
        _ => Robj::from(n as f64),
    }
}

/// The elements of an unnamed list being written.
struct SeqW(Vec<Robj>);

/// The names and elements of a named list being written.
struct MapW {
    names: Vec<String>,
    values: Vec<Robj>,
    key: Option<String>,
}

impl MapW {
    fn new(n: Option<usize>) -> Self {
        MapW {
            names: Vec::with_capacity(n.unwrap_or(0)),
            values: Vec::with_capacity(n.unwrap_or(0)),
            key: None,
        }
    }

    fn finish(self) -> std::result::Result<Robj, Error> {
        List::from_names_and_values(self.names, self.values)
            .map(Robj::from)
            .map_err(|e| Error(e.to_string()))
    }
}

impl ser::Serializer for Ser {
    type Ok = Robj;
    type Error = Error;
    type SerializeSeq = SeqW;
    type SerializeTuple = SeqW;
    type SerializeTupleStruct = SeqW;
    type SerializeTupleVariant = SeqW;
    type SerializeMap = MapW;
    type SerializeStruct = MapW;
    type SerializeStructVariant = MapW;

    fn serialize_bool(self, v: bool) -> std::result::Result<Robj, Error> {
        Ok(Robj::from(v))
    }
    fn serialize_i8(self, v: i8) -> std::result::Result<Robj, Error> {
        Ok(integer(v.into()))
    }
    fn serialize_i16(self, v: i16) -> std::result::Result<Robj, Error> {
        Ok(integer(v.into()))
    }
    fn serialize_i32(self, v: i32) -> std::result::Result<Robj, Error> {
        Ok(integer(v.into()))
    }
    fn serialize_i64(self, v: i64) -> std::result::Result<Robj, Error> {
        Ok(integer(v))
    }
    fn serialize_u8(self, v: u8) -> std::result::Result<Robj, Error> {
        Ok(integer(v.into()))
    }
    fn serialize_u16(self, v: u16) -> std::result::Result<Robj, Error> {
        Ok(integer(v.into()))
    }
    fn serialize_u32(self, v: u32) -> std::result::Result<Robj, Error> {
        Ok(integer(v.into()))
    }
    fn serialize_u64(self, v: u64) -> std::result::Result<Robj, Error> {
        Ok(i64::try_from(v).map_or_else(|_| Robj::from(v as f64), integer))
    }
    fn serialize_f32(self, v: f32) -> std::result::Result<Robj, Error> {
        Ok(Robj::from(f64::from(v)))
    }
    fn serialize_f64(self, v: f64) -> std::result::Result<Robj, Error> {
        Ok(Robj::from(v))
    }
    fn serialize_char(self, v: char) -> std::result::Result<Robj, Error> {
        Ok(Robj::from(v.to_string()))
    }
    fn serialize_str(self, v: &str) -> std::result::Result<Robj, Error> {
        Ok(Robj::from(v))
    }
    fn serialize_bytes(self, v: &[u8]) -> std::result::Result<Robj, Error> {
        Ok(Robj::from(v))
    }
    fn serialize_none(self) -> std::result::Result<Robj, Error> {
        Ok(Robj::from(()))
    }
    fn serialize_some<T: Serialize + ?Sized>(self, v: &T) -> std::result::Result<Robj, Error> {
        v.serialize(self)
    }
    fn serialize_unit(self) -> std::result::Result<Robj, Error> {
        Ok(Robj::from(()))
    }
    fn serialize_unit_struct(self, _: &'static str) -> std::result::Result<Robj, Error> {
        Ok(Robj::from(()))
    }
    fn serialize_unit_variant(
        self,
        _: &'static str,
        _: u32,
        variant: &'static str,
    ) -> std::result::Result<Robj, Error> {
        Ok(Robj::from(variant))
    }
    fn serialize_newtype_struct<T: Serialize + ?Sized>(
        self,
        _: &'static str,
        v: &T,
    ) -> std::result::Result<Robj, Error> {
        v.serialize(self)
    }
    fn serialize_newtype_variant<T: Serialize + ?Sized>(
        self,
        _: &'static str,
        _: u32,
        variant: &'static str,
        v: &T,
    ) -> std::result::Result<Robj, Error> {
        let value = v.serialize(Ser)?;
        List::from_names_and_values([variant], [value])
            .map(Robj::from)
            .map_err(|e| Error(e.to_string()))
    }
    fn serialize_seq(self, n: Option<usize>) -> std::result::Result<SeqW, Error> {
        Ok(SeqW(Vec::with_capacity(n.unwrap_or(0))))
    }
    fn serialize_tuple(self, n: usize) -> std::result::Result<SeqW, Error> {
        Ok(SeqW(Vec::with_capacity(n)))
    }
    fn serialize_tuple_struct(self, _: &'static str, n: usize) -> std::result::Result<SeqW, Error> {
        Ok(SeqW(Vec::with_capacity(n)))
    }
    fn serialize_tuple_variant(
        self,
        _: &'static str,
        _: u32,
        _: &'static str,
        n: usize,
    ) -> std::result::Result<SeqW, Error> {
        Ok(SeqW(Vec::with_capacity(n)))
    }
    fn serialize_map(self, n: Option<usize>) -> std::result::Result<MapW, Error> {
        Ok(MapW::new(n))
    }
    fn serialize_struct(self, _: &'static str, n: usize) -> std::result::Result<MapW, Error> {
        Ok(MapW::new(Some(n)))
    }
    fn serialize_struct_variant(
        self,
        _: &'static str,
        _: u32,
        _: &'static str,
        n: usize,
    ) -> std::result::Result<MapW, Error> {
        Ok(MapW::new(Some(n)))
    }
}

impl SeqW {
    fn push<T: Serialize + ?Sized>(&mut self, v: &T) -> std::result::Result<(), Error> {
        self.0.push(v.serialize(Ser)?);
        Ok(())
    }
    fn finish(self) -> std::result::Result<Robj, Error> {
        Ok(List::from_values(self.0).into())
    }
}

impl ser::SerializeSeq for SeqW {
    type Ok = Robj;
    type Error = Error;
    fn serialize_element<T: Serialize + ?Sized>(
        &mut self,
        v: &T,
    ) -> std::result::Result<(), Error> {
        self.push(v)
    }
    fn end(self) -> std::result::Result<Robj, Error> {
        self.finish()
    }
}

impl ser::SerializeTuple for SeqW {
    type Ok = Robj;
    type Error = Error;
    fn serialize_element<T: Serialize + ?Sized>(
        &mut self,
        v: &T,
    ) -> std::result::Result<(), Error> {
        self.push(v)
    }
    fn end(self) -> std::result::Result<Robj, Error> {
        self.finish()
    }
}

impl ser::SerializeTupleStruct for SeqW {
    type Ok = Robj;
    type Error = Error;
    fn serialize_field<T: Serialize + ?Sized>(&mut self, v: &T) -> std::result::Result<(), Error> {
        self.push(v)
    }
    fn end(self) -> std::result::Result<Robj, Error> {
        self.finish()
    }
}

impl ser::SerializeTupleVariant for SeqW {
    type Ok = Robj;
    type Error = Error;
    fn serialize_field<T: Serialize + ?Sized>(&mut self, v: &T) -> std::result::Result<(), Error> {
        self.push(v)
    }
    fn end(self) -> std::result::Result<Robj, Error> {
        self.finish()
    }
}

impl ser::SerializeMap for MapW {
    type Ok = Robj;
    type Error = Error;
    fn serialize_key<T: Serialize + ?Sized>(&mut self, k: &T) -> std::result::Result<(), Error> {
        let key = k.serialize(Ser)?;
        self.key = Some(
            key.as_str()
                .ok_or_else(|| Error("a map key is a string".to_string()))?
                .to_string(),
        );
        Ok(())
    }
    fn serialize_value<T: Serialize + ?Sized>(&mut self, v: &T) -> std::result::Result<(), Error> {
        let key = self
            .key
            .take()
            .ok_or_else(|| Error("a map value follows its key".to_string()))?;
        self.names.push(key);
        self.values.push(v.serialize(Ser)?);
        Ok(())
    }
    fn end(self) -> std::result::Result<Robj, Error> {
        self.finish()
    }
}

impl ser::SerializeStruct for MapW {
    type Ok = Robj;
    type Error = Error;
    fn serialize_field<T: Serialize + ?Sized>(
        &mut self,
        key: &'static str,
        v: &T,
    ) -> std::result::Result<(), Error> {
        self.names.push(key.to_string());
        self.values.push(v.serialize(Ser)?);
        Ok(())
    }
    fn end(self) -> std::result::Result<Robj, Error> {
        self.finish()
    }
}

impl ser::SerializeStructVariant for MapW {
    type Ok = Robj;
    type Error = Error;
    fn serialize_field<T: Serialize + ?Sized>(
        &mut self,
        key: &'static str,
        v: &T,
    ) -> std::result::Result<(), Error> {
        self.names.push(key.to_string());
        self.values.push(v.serialize(Ser)?);
        Ok(())
    }
    fn end(self) -> std::result::Result<Robj, Error> {
        self.finish()
    }
}
