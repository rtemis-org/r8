//! A number of the IR and of a document read: a JSON number as the
//! document writes it, a signed or unsigned integer or a double, 16 B.

use std::fmt;

use serde::de::{self, Deserializer, Visitor};
use serde::{Deserialize, Serialize, Serializer};

/// A JSON number.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Number {
    /// A negative integer, or any integer read as signed.
    I64(i64),
    /// A non-negative integer.
    U64(u64),
    /// A number written with a fraction or an exponent.
    F64(f64),
}

/// The largest magnitude of an integer that every double holds exactly.
pub const EXACT_DOUBLE: u64 = 1 << 53;

impl Number {
    /// The number as a double.
    pub fn as_f64(self) -> f64 {
        match self {
            Number::I64(i) => i as f64,
            Number::U64(u) => u as f64,
            Number::F64(d) => d,
        }
    }

    /// The number as a non-negative integer, when it is written as one.
    pub fn as_u64(self) -> Option<u64> {
        match self {
            Number::I64(i) => u64::try_from(i).ok(),
            Number::U64(u) => Some(u),
            Number::F64(_) => None,
        }
    }

    /// The number as a signed integer, when it is written as one.
    pub fn as_i64(self) -> Option<i64> {
        match self {
            Number::I64(i) => Some(i),
            Number::U64(u) => i64::try_from(u).ok(),
            Number::F64(_) => None,
        }
    }

    /// The number that reading `x`'s ECMAScript text gives: an integer for a
    /// whole number of magnitude at most 2^53, and a double otherwise.
    pub fn of_f64(x: f64) -> Number {
        if x.fract() == 0.0 && x.abs() <= EXACT_DOUBLE as f64 {
            if x >= 0.0 {
                Number::U64(x as u64)
            } else {
                Number::I64(x as i64)
            }
        } else {
            Number::F64(x)
        }
    }

    /// A double holds the number exactly.
    pub fn is_exact_f64(self) -> bool {
        match self {
            Number::I64(i) => i.unsigned_abs() <= EXACT_DOUBLE,
            Number::U64(u) => u <= EXACT_DOUBLE,
            Number::F64(_) => true,
        }
    }
}

impl From<Number> for serde_json::Value {
    fn from(n: Number) -> Self {
        match n {
            Number::I64(i) => serde_json::Value::from(i),
            Number::U64(u) => serde_json::Value::from(u),
            Number::F64(d) => serde_json::Number::from_f64(d)
                .map_or(serde_json::Value::Null, serde_json::Value::Number),
        }
    }
}

impl Serialize for Number {
    fn serialize<S: Serializer>(&self, s: S) -> Result<S::Ok, S::Error> {
        match *self {
            Number::I64(i) => s.serialize_i64(i),
            Number::U64(u) => s.serialize_u64(u),
            Number::F64(d) => s.serialize_f64(d),
        }
    }
}

struct NumberVisitor;

impl Visitor<'_> for NumberVisitor {
    type Value = Number;

    fn expecting(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("a number")
    }
    fn visit_i64<E: de::Error>(self, v: i64) -> Result<Number, E> {
        Ok(if v >= 0 {
            Number::U64(v as u64)
        } else {
            Number::I64(v)
        })
    }
    fn visit_u64<E: de::Error>(self, v: u64) -> Result<Number, E> {
        Ok(Number::U64(v))
    }
    fn visit_f64<E: de::Error>(self, v: f64) -> Result<Number, E> {
        Ok(Number::F64(v))
    }
}

impl<'de> Deserialize<'de> for Number {
    fn deserialize<D: Deserializer<'de>>(d: D) -> Result<Self, D::Error> {
        d.deserialize_any(NumberVisitor)
    }
}
