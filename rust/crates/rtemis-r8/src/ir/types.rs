//! Field types of the IR: the base types, the composite types, and the
//! containers that compose them.
//!
//! Each variant serializes as an object whose `kind` member names it, so
//! `option(vec(type_double(min = 0)))` is
//! `{"kind": "option", "inner": {"kind": "vec", "element": {"kind": "double", "min": 0}}}`.

use serde::{Deserialize, Serialize};

use super::decl::RuleDef;
use super::logic::Logic;
use crate::name::QualifiedName;

fn yes() -> bool {
    true
}

fn is_true(b: &bool) -> bool {
    *b
}

fn is_false(b: &bool) -> bool {
    !*b
}

/// A bound of a numeric range: the value, and whether the range excludes it.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Bound<T> {
    /// The bound's value.
    pub value: T,
    /// The range excludes `value` itself.
    pub exclusive: bool,
}

impl<T: PartialOrd + Copy> Bound<T> {
    /// An inclusive bound.
    pub fn inclusive(value: T) -> Self {
        Bound {
            value,
            exclusive: false,
        }
    }

    /// An exclusive bound.
    pub fn exclusive(value: T) -> Self {
        Bound {
            value,
            exclusive: true,
        }
    }

    /// Whether `x` lies on the admitted side of this lower bound.
    #[inline(always)]
    pub fn admits_above(&self, x: T) -> bool {
        if self.exclusive {
            x > self.value
        } else {
            x >= self.value
        }
    }

    /// Whether `x` lies on the admitted side of this upper bound.
    #[inline(always)]
    pub fn admits_below(&self, x: T) -> bool {
        if self.exclusive {
            x < self.value
        } else {
            x <= self.value
        }
    }
}

/// The bound of a side of a range, from the IR's inclusive and exclusive
/// members, which a declaration sets one of at most.
fn side<T: PartialOrd + Copy>(
    inclusive: Option<T>,
    exclusive: Option<T>,
    names: (&str, &str),
) -> Result<Option<Bound<T>>, String> {
    match (inclusive, exclusive) {
        (Some(_), Some(_)) => Err(format!(
            "a numeric type sets `{}` or `{}`, not both",
            names.0, names.1
        )),
        (Some(v), None) => Ok(Some(Bound::inclusive(v))),
        (None, Some(v)) => Ok(Some(Bound::exclusive(v))),
        (None, None) => Ok(None),
    }
}

/// The IR's inclusive and exclusive members of a side of a range.
fn members<T: Copy>(b: Option<Bound<T>>) -> (Option<T>, Option<T>) {
    match b {
        Some(Bound {
            value,
            exclusive: false,
        }) => (Some(value), None),
        Some(Bound {
            value,
            exclusive: true,
        }) => (None, Some(value)),
        None => (None, None),
    }
}

/// Constraints of `type_integer()`, whose domain is −2147483647 to
/// 2147483647.
#[derive(Debug, Clone, Default, PartialEq)]
pub struct IntegerType {
    /// The lower bound.
    pub lower: Option<Bound<i32>>,
    /// The upper bound.
    pub upper: Option<Bound<i32>>,
    /// The admitted values, when the domain is a closed set.
    pub values: Option<Box<[i32]>>,
}

impl IntegerType {
    /// The inclusive lower bound, the IR's `min`.
    pub fn min(&self) -> Option<i32> {
        self.lower.filter(|b| !b.exclusive).map(|b| b.value)
    }

    /// The inclusive upper bound, the IR's `max`.
    pub fn max(&self) -> Option<i32> {
        self.upper.filter(|b| !b.exclusive).map(|b| b.value)
    }

    /// The exclusive lower bound, the IR's `exclusive_min`.
    pub fn exclusive_min(&self) -> Option<i32> {
        self.lower.filter(|b| b.exclusive).map(|b| b.value)
    }

    /// The exclusive upper bound, the IR's `exclusive_max`.
    pub fn exclusive_max(&self) -> Option<i32> {
        self.upper.filter(|b| b.exclusive).map(|b| b.value)
    }

    /// Whether `x` lies within both bounds.
    #[inline(always)]
    pub fn in_range(&self, x: i32) -> bool {
        self.lower.is_none_or(|b| b.admits_above(x)) && self.upper.is_none_or(|b| b.admits_below(x))
    }
}

/// Constraints of `type_integer()`, whose domain is −2147483647 to
/// 2147483647.
#[derive(Serialize, Deserialize)]
#[cfg_attr(feature = "schema", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "schema", schemars(rename = "IntegerType"))]
#[serde(deny_unknown_fields)]
struct IntegerTypeJson {
    #[serde(default, skip_serializing_if = "Option::is_none")]
    /// The smallest value.
    min: Option<i32>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    /// The largest value.
    max: Option<i32>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    /// Values are above this.
    exclusive_min: Option<i32>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    /// Values are below this.
    exclusive_max: Option<i32>,
    /// The admitted values, when the domain is a closed set.
    #[serde(default, rename = "enum", skip_serializing_if = "Option::is_none")]
    values: Option<Box<[i32]>>,
}

impl TryFrom<IntegerTypeJson> for IntegerType {
    type Error = String;
    fn try_from(j: IntegerTypeJson) -> Result<Self, String> {
        Ok(IntegerType {
            lower: side(j.min, j.exclusive_min, ("min", "exclusive_min"))?,
            upper: side(j.max, j.exclusive_max, ("max", "exclusive_max"))?,
            values: j.values,
        })
    }
}

impl From<IntegerType> for IntegerTypeJson {
    fn from(t: IntegerType) -> Self {
        let (min, exclusive_min) = members(t.lower);
        let (max, exclusive_max) = members(t.upper);
        IntegerTypeJson {
            min,
            max,
            exclusive_min,
            exclusive_max,
            values: t.values,
        }
    }
}

impl Serialize for IntegerType {
    fn serialize<S: serde::Serializer>(&self, s: S) -> Result<S::Ok, S::Error> {
        IntegerTypeJson::from(self.clone()).serialize(s)
    }
}

impl<'de> Deserialize<'de> for IntegerType {
    fn deserialize<D: serde::Deserializer<'de>>(d: D) -> Result<Self, D::Error> {
        IntegerTypeJson::deserialize(d)?
            .try_into()
            .map_err(serde::de::Error::custom)
    }
}

#[cfg(feature = "schema")]
impl schemars::JsonSchema for IntegerType {
    fn schema_name() -> std::borrow::Cow<'static, str> {
        "IntegerType".into()
    }
    fn json_schema(g: &mut schemars::SchemaGenerator) -> schemars::Schema {
        IntegerTypeJson::json_schema(g)
    }
}

/// Constraints of `type_double()`. `finite = false` admits `Inf`, `-Inf`,
/// and `NaN`.
#[derive(Debug, Clone, PartialEq)]
pub struct DoubleType {
    /// The lower bound.
    pub lower: Option<Bound<f64>>,
    /// The upper bound.
    pub upper: Option<Bound<f64>>,
    /// Values are finite: no `Inf`, `-Inf`, or `NaN`.
    pub finite: bool,
    /// The admitted values.
    pub values: Option<Box<[f64]>>,
}

impl DoubleType {
    /// The inclusive lower bound, the IR's `min`.
    pub fn min(&self) -> Option<f64> {
        self.lower.filter(|b| !b.exclusive).map(|b| b.value)
    }

    /// The inclusive upper bound, the IR's `max`.
    pub fn max(&self) -> Option<f64> {
        self.upper.filter(|b| !b.exclusive).map(|b| b.value)
    }

    /// The exclusive lower bound, the IR's `exclusive_min`.
    pub fn exclusive_min(&self) -> Option<f64> {
        self.lower.filter(|b| b.exclusive).map(|b| b.value)
    }

    /// The exclusive upper bound, the IR's `exclusive_max`.
    pub fn exclusive_max(&self) -> Option<f64> {
        self.upper.filter(|b| b.exclusive).map(|b| b.value)
    }

    /// Whether `x` lies within both bounds.
    #[inline(always)]
    pub fn in_range(&self, x: f64) -> bool {
        self.lower.is_none_or(|b| b.admits_above(x)) && self.upper.is_none_or(|b| b.admits_below(x))
    }
}

impl Default for DoubleType {
    fn default() -> Self {
        DoubleType {
            lower: None,
            upper: None,
            finite: true,
            values: None,
        }
    }
}

/// Constraints of `type_double()`. `finite = false` admits `Inf`, `-Inf`,
/// and `NaN`.
#[derive(Serialize, Deserialize)]
#[cfg_attr(feature = "schema", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "schema", schemars(rename = "DoubleType"))]
#[serde(deny_unknown_fields)]
struct DoubleTypeJson {
    #[serde(default, skip_serializing_if = "Option::is_none")]
    /// The smallest value.
    min: Option<f64>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    /// The largest value.
    max: Option<f64>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    /// Values are above this.
    exclusive_min: Option<f64>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    /// Values are below this.
    exclusive_max: Option<f64>,
    #[serde(default = "yes", skip_serializing_if = "is_true")]
    /// Values are finite: no `Inf`, `-Inf`, or `NaN`.
    finite: bool,
    #[serde(default, rename = "enum", skip_serializing_if = "Option::is_none")]
    /// The admitted values.
    values: Option<Box<[f64]>>,
}

impl TryFrom<DoubleTypeJson> for DoubleType {
    type Error = String;
    fn try_from(j: DoubleTypeJson) -> Result<Self, String> {
        Ok(DoubleType {
            lower: side(j.min, j.exclusive_min, ("min", "exclusive_min"))?,
            upper: side(j.max, j.exclusive_max, ("max", "exclusive_max"))?,
            finite: j.finite,
            values: j.values,
        })
    }
}

impl From<DoubleType> for DoubleTypeJson {
    fn from(t: DoubleType) -> Self {
        let (min, exclusive_min) = members(t.lower);
        let (max, exclusive_max) = members(t.upper);
        DoubleTypeJson {
            min,
            max,
            exclusive_min,
            exclusive_max,
            finite: t.finite,
            values: t.values,
        }
    }
}

impl Serialize for DoubleType {
    fn serialize<S: serde::Serializer>(&self, s: S) -> Result<S::Ok, S::Error> {
        DoubleTypeJson::from(self.clone()).serialize(s)
    }
}

impl<'de> Deserialize<'de> for DoubleType {
    fn deserialize<D: serde::Deserializer<'de>>(d: D) -> Result<Self, D::Error> {
        DoubleTypeJson::deserialize(d)?
            .try_into()
            .map_err(serde::de::Error::custom)
    }
}

#[cfg(feature = "schema")]
impl schemars::JsonSchema for DoubleType {
    fn schema_name() -> std::borrow::Cow<'static, str> {
        "DoubleType".into()
    }
    fn json_schema(g: &mut schemars::SchemaGenerator) -> schemars::Schema {
        DoubleTypeJson::json_schema(g)
    }
}

/// Constraints of `type_character()`, and of the keys of `btree_map()` and
/// `index_map()`.
#[derive(Debug, Clone, Default, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "schema", derive(schemars::JsonSchema))]
#[serde(deny_unknown_fields)]
pub struct CharacterType {
    /// A regular expression in Rust `regex` syntax, compiled at declaration.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub pattern: Option<Box<str>>,
    #[serde(default, rename = "enum", skip_serializing_if = "Option::is_none")]
    /// The admitted values.
    pub values: Option<Box<[Box<str>]>>,
}

/// Who fills an absent value of a struct field of type `option()`; a field
/// without a policy keeps its absent value.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "schema", derive(schemars::JsonSchema))]
#[serde(tag = "policy", rename_all = "snake_case", deny_unknown_fields)]
pub enum Resolve {
    /// `r8_resolve()` fills the value from an expression over other fields.
    Computed {
        /// The expression over the other fields.
        expr: Logic,
    },
    /// `r8_resolve(x, context)` fills the value from its `context`.
    Context,
}

/// A field type.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "schema", derive(schemars::JsonSchema))]
#[serde(tag = "kind", rename_all = "snake_case", deny_unknown_fields)]
pub enum FieldType {
    /// `type_logical()`: a boolean, not `NA`.
    Logical,
    /// `type_integer()`.
    Integer(IntegerType),
    /// `type_double()`.
    Double(DoubleType),
    /// `type_character()`: a string without U+0000, not `NA`.
    Character(CharacterType),
    /// `type_raw()`: bytes, written as padded base64 (RFC 4648 section 4).
    Raw,
    /// `type_date()`: a calendar date from 0000-01-01 to 9999-12-31.
    Date,
    /// `type_posixct()`: a UTC instant in microseconds, from 0000-01-01 to
    /// 2112-09-17.
    Posixct,
    /// `type_factor()`: a vector of categorical values.
    Factor {
        #[serde(default, skip_serializing_if = "Option::is_none")]
        /// The declared levels, or `None` for levels from the data.
        levels: Option<Box<[Box<str>]>>,
        #[serde(default, skip_serializing_if = "is_false")]
        /// An ordered factor.
        ordered: bool,
    },
    /// `type_matrix(T, nrow, ncol)`: a dense matrix of a scalar type.
    Matrix {
        /// The element type.
        element: Box<FieldType>,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        /// The number of rows, when declared.
        nrow: Option<u32>,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        /// The number of columns, when declared.
        ncol: Option<u32>,
    },
    /// `type_array(T, dim)`: a dense array of a scalar type.
    Array {
        /// The element type.
        element: Box<FieldType>,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        /// The extent of each dimension, when declared.
        dim: Option<Box<[u32]>>,
    },
    /// `type_sparse_matrix(T)`: a matrix in compressed sparse column form,
    /// `dgCMatrix` in R and `{"shape", "indptr", "indices", "data"}` in JSON.
    SparseMatrix {
        /// The element type.
        element: Box<FieldType>,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        /// The number of rows, when declared.
        nrow: Option<u32>,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        /// The number of columns, when declared.
        ncol: Option<u32>,
    },
    /// `type_matrix(S)`: a matrix whose columns are the fields of the struct
    /// `of`, all of one scalar type.
    MatrixOf {
        /// The struct whose fields are the columns.
        of: QualifiedName,
    },
    /// `type_table(columns, open, rules)`: a data.frame of equal-length
    /// columns. An open table also accepts columns the declaration does not
    /// name, each of a column type, whose types its JSON records in
    /// `"$columns"`. Each rule runs once per row and reads the row's columns
    /// by name.
    Table {
        /// The declared columns.
        columns: Box<[super::decl::ColumnDef]>,
        #[serde(default, skip_serializing_if = "is_false")]
        /// Columns the declaration does not list are admitted.
        open: bool,
        #[serde(default, skip_serializing_if = "<[_]>::is_empty")]
        /// The rules every row satisfies.
        rules: Box<[RuleDef]>,
    },
    /// `type_table(S)`: a data.frame whose columns are the fields of the
    /// struct `of`.
    TableOf {
        /// The struct whose fields are the columns.
        of: QualifiedName,
    },
    /// `type_json()`: any JSON value.
    Json,
    /// `type_r(class)`: an R value of a class, runtime-only.
    R {
        /// The R class.
        class: Box<str>,
    },
    /// `vec(T, min_length, max_length, unique)`.
    Vec {
        /// The element type.
        element: Box<FieldType>,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        /// The fewest elements.
        min_length: Option<u64>,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        /// The most elements.
        max_length: Option<u64>,
        #[serde(default, skip_serializing_if = "is_false")]
        /// The elements are distinct.
        unique: bool,
    },
    /// `option(T)`: the value may be absent, `Option<T>`.
    Option {
        /// The type of a present value.
        inner: Box<FieldType>,
    },
    /// `btree_map(T, keys)`: `BTreeMap<String, T>`, string keys in code-point
    /// order, equal by key.
    BtreeMap {
        /// The value type.
        value: Box<FieldType>,
        #[serde(default, skip_serializing_if = "is_default_keys")]
        /// The constraints on keys.
        keys: CharacterType,
    },
    /// `index_map(T, keys)`: unique string keys in insertion order; a named
    /// list in R and an array of `{"name", "value"}` pairs in JSON. Order is
    /// part of the value.
    IndexMap {
        /// The value type.
        value: Box<FieldType>,
        #[serde(default, skip_serializing_if = "is_default_keys")]
        /// The constraints on keys.
        keys: CharacterType,
    },
    /// A named struct or enum, by its qualified name.
    Named {
        /// The type's qualified name.
        name: QualifiedName,
    },
    /// `dyn(Trait)`: any type that implements the trait.
    Dyn {
        /// The trait.
        r#trait: QualifiedName,
    },
    /// `subtype(Parent)`: the struct `of` or any struct that extends a
    /// subtype of it.
    Subtype {
        /// The struct.
        of: QualifiedName,
    },
}

fn is_default_keys(k: &CharacterType) -> bool {
    *k == CharacterType::default()
}

impl FieldType {
    /// `true` for the base types that hold one value: logical, integer,
    /// double, character, date, and date-time.
    pub fn is_scalar(&self) -> bool {
        matches!(
            self,
            FieldType::Logical
                | FieldType::Integer(_)
                | FieldType::Double(_)
                | FieldType::Character(_)
                | FieldType::Date
                | FieldType::Posixct
        )
    }
}
