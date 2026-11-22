//! A serde `Deserializer` over the R objects that the declaration functions
//! build, so the IR types' own derives read a declaration from R directly.
//!
//! A named list is a map; an unnamed list is a sequence; an atomic vector is
//! a sequence where the IR type expects one, and otherwise a scalar when its
//! length is one and a sequence when it is not; `NULL` is `null`. An object of
//! class `r8_json_ir`, from `json_ir(text)`, reads as its JSON text, read
//! straight from the text. Where the IR expects an integer, a whole double of
//! magnitude at most 2^53 reads as that integer, and a string of class
//! `r8_int64`, from `int64(text)`, as the integer of its decimal digits; an
//! `r8_int64` reads as a number anywhere. A declaration holds no `NA` and no
//! number that is not finite; an error names the path of the value.
//! spec: R8/class-system#declaring-types

use std::borrow::Cow;
use std::fmt;

use extendr_ffi::{INTEGER, LOGICAL, REAL, SEXP, SEXPTYPE, VECTOR_ELT};
use serde::de::{self, DeserializeSeed, IntoDeserializer, MapAccess, SeqAccess, Visitor};
use serde::forward_to_deserialize_any;

use crate::rview::{attr, inherits, sexptype, string_elt, sym_names, xlen};

/// A path into a declaration, built as a chain and rendered for an error
/// only.
#[derive(Clone, Copy)]
enum Path<'p> {
    Root,
    Key(&'p Path<'p>, &'p str),
    Idx(&'p Path<'p>, usize),
}

impl fmt::Display for Path<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Path::Root => Ok(()),
            Path::Key(p, k) => write!(f, "{p}/{k}"),
            Path::Idx(p, i) => write!(f, "{p}/{i}"),
        }
    }
}

/// An error of reading a declaration.
#[derive(Debug)]
pub(crate) struct Error(pub(crate) String);

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.0)
    }
}

impl std::error::Error for Error {}

impl de::Error for Error {
    fn custom<T: fmt::Display>(m: T) -> Self {
        Error(m.to_string())
    }
}

/// Reads an IR type from the R object `x`.
pub(crate) fn from_r<T: de::DeserializeOwned>(x: SEXP) -> Result<T, Error> {
    T::deserialize(De {
        x,
        elt: None,
        path: &Path::Root,
    })
}

/// An R object, or the element `elt` of an atomic vector.
#[derive(Clone, Copy)]
struct De<'p> {
    x: SEXP,
    elt: Option<usize>,
    path: &'p Path<'p>,
}

const NA_INTEGER: i32 = i32::MIN;
const NA_LOGICAL: i32 = i32::MIN;

impl De<'_> {
    fn fail(&self, message: &str) -> Error {
        Error(format!("{}: {message}", self.path))
    }

    /// The JSON text of a `json_ir()` object.
    fn json_ir(&self) -> Option<Result<Cow<'static, str>, Error>> {
        if self.elt.is_some() || !inherits(self.x, "r8_json_ir") {
            return None;
        }
        Some(
            (sexptype(self.x) == SEXPTYPE::STRSXP && xlen(self.x) == 1)
                .then(|| string_elt(self.x, 0))
                .flatten()
                .ok_or_else(|| self.fail("json_ir() holds one string")),
        )
    }

    /// The integer of an `int64()` object.
    fn int64(&self) -> Option<Result<Int64, Error>> {
        if self.elt.is_some() || !inherits(self.x, "r8_int64") {
            return None;
        }
        Some(
            (sexptype(self.x) == SEXPTYPE::STRSXP && xlen(self.x) == 1)
                .then(|| string_elt(self.x, 0))
                .flatten()
                .and_then(|t| int64_of(&t))
                .ok_or_else(|| self.fail("int64() holds the decimal digits of one integer")),
        )
    }

    /// A whole double of magnitude at most 2^53, where the IR expects an
    /// integer.
    fn whole(&self) -> Option<i64> {
        if sexptype(self.x) != SEXPTYPE::REALSXP || (self.elt.is_none() && xlen(self.x) != 1) {
            return None;
        }
        let d = unsafe { *REAL(self.x).add(self.elt.unwrap_or(0)) };
        (d.fract() == 0.0 && d.abs() <= 9007199254740992.0).then_some(d as i64)
    }

    /// An integer member: `int64()`, a whole double, or any other value.
    fn integer<'de, V: Visitor<'de>>(self, v: V) -> Result<V::Value, Error> {
        if let Some(n) = self.int64() {
            return match n? {
                Int64::Signed(i) => v.visit_i64(i),
                Int64::Unsigned(u) => v.visit_u64(u),
            };
        }
        match self.whole() {
            Some(i) => v.visit_i64(i),
            None => de::Deserializer::deserialize_any(self, v),
        }
    }

    /// The `i`th element of an atomic vector, visited as a scalar.
    fn scalar<'de, V: Visitor<'de>>(&self, i: usize, v: V) -> Result<V::Value, Error> {
        let x = self.x;
        unsafe {
            match sexptype(x) {
                SEXPTYPE::LGLSXP => match *LOGICAL(x).add(i) {
                    NA_LOGICAL => Err(self.fail("a declaration holds no NA")),
                    b => v.visit_bool(b != 0),
                },
                SEXPTYPE::INTSXP => match *INTEGER(x).add(i) {
                    NA_INTEGER => Err(self.fail("a declaration holds no NA")),
                    n => v.visit_i64(i64::from(n)),
                },
                SEXPTYPE::REALSXP => {
                    let d = *REAL(x).add(i);
                    if d.is_finite() {
                        v.visit_f64(d)
                    } else {
                        Err(self.fail("a declaration holds finite numbers only"))
                    }
                }
                SEXPTYPE::STRSXP => match string_elt(x, i) {
                    Some(s) => v.visit_str(&s),
                    None => Err(self.fail("a declaration holds no NA")),
                },
                other => Err(self.fail(&format!(
                    "a declaration cannot hold an R object of type {other:?}"
                ))),
            }
        }
    }

    /// The names of a list, when any is not empty.
    fn names(&self) -> Option<SEXP> {
        let names = attr(self.x, sym_names());
        (sexptype(names) == SEXPTYPE::STRSXP
            && (0..xlen(names)).any(|i| string_elt(names, i).is_some_and(|s| !s.is_empty())))
        .then_some(names)
    }

    fn seq<'de, V: Visitor<'de>>(&self, v: V) -> Result<V::Value, Error> {
        v.visit_seq(Seq {
            x: self.x,
            list: sexptype(self.x) == SEXPTYPE::VECSXP,
            i: 0,
            n: xlen(self.x),
            path: self.path,
        })
    }
}

/// Deserializes `json_ir()` text through `f`, from a reader over the text, so
/// that its strings are owned and it serves a visitor of any lifetime.
fn from_text<'de, V: Visitor<'de>>(
    path: &Path<'_>,
    text: &str,
    v: V,
    f: impl FnOnce(
        &mut serde_json::Deserializer<serde_json::de::IoRead<&[u8]>>,
        V,
    ) -> Result<V::Value, serde_json::Error>,
) -> Result<V::Value, Error> {
    let fail =
        |e: serde_json::Error| Error(format!("{path}: the text of json_ir() does not read: {e}"));
    let mut de = serde_json::Deserializer::from_reader(text.as_bytes());
    let out = f(&mut de, v).map_err(fail)?;
    de.end().map_err(fail)?;
    Ok(out)
}

/// An integer of `int64()`'s decimal digits.
enum Int64 {
    Signed(i64),
    Unsigned(u64),
}

fn int64_of(text: &str) -> Option<Int64> {
    text.parse::<i64>()
        .map(Int64::Signed)
        .ok()
        .or_else(|| text.parse::<u64>().map(Int64::Unsigned).ok())
}

impl<'de> de::Deserializer<'de> for De<'_> {
    type Error = Error;

    fn deserialize_any<V: Visitor<'de>>(self, v: V) -> Result<V::Value, Error> {
        if let Some(text) = self.json_ir() {
            return from_text(self.path, &text?, v, |de, v| {
                de::Deserializer::deserialize_any(de, v)
            });
        }
        if let Some(n) = self.int64() {
            return match n? {
                Int64::Signed(i) => v.visit_i64(i),
                Int64::Unsigned(u) => v.visit_u64(u),
            };
        }
        if let Some(i) = self.elt {
            return self.scalar(i, v);
        }
        match sexptype(self.x) {
            SEXPTYPE::NILSXP => v.visit_unit(),
            SEXPTYPE::VECSXP => match self.names() {
                Some(names) => v.visit_map(Map {
                    x: self.x,
                    names,
                    i: 0,
                    n: xlen(self.x),
                    path: self.path,
                }),
                None => self.seq(v),
            },
            _ if xlen(self.x) == 1 => self.scalar(0, v),
            _ => self.seq(v),
        }
    }

    fn deserialize_option<V: Visitor<'de>>(self, v: V) -> Result<V::Value, Error> {
        if let Some(text) = self.json_ir() {
            return from_text(self.path, &text?, v, |de, v| {
                de::Deserializer::deserialize_option(de, v)
            });
        }
        if self.elt.is_none() && sexptype(self.x) == SEXPTYPE::NILSXP {
            v.visit_none()
        } else {
            v.visit_some(self)
        }
    }

    fn deserialize_seq<V: Visitor<'de>>(self, v: V) -> Result<V::Value, Error> {
        if let Some(text) = self.json_ir() {
            return from_text(self.path, &text?, v, |de, v| {
                de::Deserializer::deserialize_seq(de, v)
            });
        }
        if self.elt.is_some() || self.names().is_some() {
            return self.deserialize_any(v);
        }
        match sexptype(self.x) {
            SEXPTYPE::VECSXP
            | SEXPTYPE::LGLSXP
            | SEXPTYPE::INTSXP
            | SEXPTYPE::REALSXP
            | SEXPTYPE::STRSXP => self.seq(v),
            _ => self.deserialize_any(v),
        }
    }

    fn deserialize_enum<V: Visitor<'de>>(
        self,
        name: &'static str,
        variants: &'static [&'static str],
        v: V,
    ) -> Result<V::Value, Error> {
        if let Some(text) = self.json_ir() {
            return from_text(self.path, &text?, v, |de, v| {
                de::Deserializer::deserialize_enum(de, name, variants, v)
            });
        }
        let i = self.elt.unwrap_or(0);
        if sexptype(self.x) == SEXPTYPE::STRSXP && (self.elt.is_some() || xlen(self.x) == 1) {
            match string_elt(self.x, i) {
                Some(s) => v.visit_enum(s.as_ref().into_deserializer()),
                None => Err(self.fail("a declaration holds no NA")),
            }
        } else {
            self.deserialize_any(v)
        }
    }

    fn deserialize_newtype_struct<V: Visitor<'de>>(
        self,
        _name: &'static str,
        v: V,
    ) -> Result<V::Value, Error> {
        v.visit_newtype_struct(self)
    }

    fn deserialize_i8<V: Visitor<'de>>(self, v: V) -> Result<V::Value, Error> {
        self.integer(v)
    }
    fn deserialize_i16<V: Visitor<'de>>(self, v: V) -> Result<V::Value, Error> {
        self.integer(v)
    }
    fn deserialize_i32<V: Visitor<'de>>(self, v: V) -> Result<V::Value, Error> {
        self.integer(v)
    }
    fn deserialize_i64<V: Visitor<'de>>(self, v: V) -> Result<V::Value, Error> {
        self.integer(v)
    }
    fn deserialize_u8<V: Visitor<'de>>(self, v: V) -> Result<V::Value, Error> {
        self.integer(v)
    }
    fn deserialize_u16<V: Visitor<'de>>(self, v: V) -> Result<V::Value, Error> {
        self.integer(v)
    }
    fn deserialize_u32<V: Visitor<'de>>(self, v: V) -> Result<V::Value, Error> {
        self.integer(v)
    }
    fn deserialize_u64<V: Visitor<'de>>(self, v: V) -> Result<V::Value, Error> {
        self.integer(v)
    }

    forward_to_deserialize_any! {
        bool i128 u128 f32 f64 char str string bytes
        byte_buf unit unit_struct tuple tuple_struct map struct identifier ignored_any
    }
}

struct Map<'p> {
    x: SEXP,
    names: SEXP,
    i: usize,
    n: usize,
    path: &'p Path<'p>,
}

impl<'de> MapAccess<'de> for Map<'_> {
    type Error = Error;

    fn next_key_seed<K: DeserializeSeed<'de>>(
        &mut self,
        seed: K,
    ) -> Result<Option<K::Value>, Error> {
        if self.i >= self.n {
            return Ok(None);
        }
        let key = string_elt(self.names, self.i).unwrap_or_default();
        seed.deserialize(key.as_ref().into_deserializer()).map(Some)
    }

    fn next_value_seed<S: DeserializeSeed<'de>>(&mut self, seed: S) -> Result<S::Value, Error> {
        let key = string_elt(self.names, self.i).unwrap_or_default();
        let path = Path::Key(self.path, &key);
        let x = unsafe { VECTOR_ELT(self.x, self.i as isize) };
        self.i += 1;
        seed.deserialize(De {
            x,
            elt: None,
            path: &path,
        })
    }

    fn size_hint(&self) -> Option<usize> {
        Some(self.n - self.i)
    }
}

struct Seq<'p> {
    x: SEXP,
    list: bool,
    i: usize,
    n: usize,
    path: &'p Path<'p>,
}

impl<'de> SeqAccess<'de> for Seq<'_> {
    type Error = Error;

    fn next_element_seed<S: DeserializeSeed<'de>>(
        &mut self,
        seed: S,
    ) -> Result<Option<S::Value>, Error> {
        if self.i >= self.n {
            return Ok(None);
        }
        let path = Path::Idx(self.path, self.i);
        let de = if self.list {
            De {
                x: unsafe { VECTOR_ELT(self.x, self.i as isize) },
                elt: None,
                path: &path,
            }
        } else {
            De {
                x: self.x,
                elt: Some(self.i),
                path: &path,
            }
        };
        self.i += 1;
        seed.deserialize(de).map(Some)
    }

    fn size_hint(&self) -> Option<usize> {
        Some(self.n - self.i)
    }
}
