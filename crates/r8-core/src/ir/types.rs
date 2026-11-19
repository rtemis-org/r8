//! Field types of the IR: the base types, the composite types, and the
//! containers that compose them.
//!
//! Each variant serializes as an object whose `kind` member names it, so
//! `option(vec(type_double(min = 0)))` is
//! `{"kind": "option", "inner": {"kind": "vec", "element": {"kind": "double", "min": 0}}}`.

use serde::{Deserialize, Serialize};

use super::decl::{FieldDef, RuleDef};
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

/// Constraints of `type_integer()`, whose domain is −2147483647 to
/// 2147483647.
#[derive(Debug, Clone, Default, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "schema", derive(schemars::JsonSchema))]
#[serde(deny_unknown_fields)]
pub struct IntegerType {
    #[serde(default, skip_serializing_if = "Option::is_none")]
    /// The smallest value.
    pub min: Option<i32>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    /// The largest value.
    pub max: Option<i32>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    /// Values are above this.
    pub exclusive_min: Option<i32>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    /// Values are below this.
    pub exclusive_max: Option<i32>,
    /// The admitted values, when the domain is a closed set.
    #[serde(default, rename = "enum", skip_serializing_if = "Option::is_none")]
    pub values: Option<Vec<i32>>,
}

/// Constraints of `type_double()`. `finite = false` admits `Inf`, `-Inf`,
/// and `NaN`.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "schema", derive(schemars::JsonSchema))]
#[serde(deny_unknown_fields)]
pub struct DoubleType {
    #[serde(default, skip_serializing_if = "Option::is_none")]
    /// The smallest value.
    pub min: Option<f64>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    /// The largest value.
    pub max: Option<f64>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    /// Values are above this.
    pub exclusive_min: Option<f64>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    /// Values are below this.
    pub exclusive_max: Option<f64>,
    #[serde(default = "yes", skip_serializing_if = "is_true")]
    /// Values are finite: no `Inf`, `-Inf`, or `NaN`.
    pub finite: bool,
    #[serde(default, rename = "enum", skip_serializing_if = "Option::is_none")]
    /// The admitted values.
    pub values: Option<Vec<f64>>,
}

impl Default for DoubleType {
    fn default() -> Self {
        DoubleType {
            min: None,
            max: None,
            exclusive_min: None,
            exclusive_max: None,
            finite: true,
            values: None,
        }
    }
}

/// Constraints of `type_character()`, and of the keys of `hashmap()`.
#[derive(Debug, Clone, Default, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "schema", derive(schemars::JsonSchema))]
#[serde(deny_unknown_fields)]
pub struct CharacterType {
    /// A regular expression in Rust `regex` syntax, compiled at declaration.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub pattern: Option<String>,
    #[serde(default, rename = "enum", skip_serializing_if = "Option::is_none")]
    /// The admitted values.
    pub values: Option<Vec<String>>,
}

/// What an absent `option()` value means.
#[derive(Debug, Clone, Default, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "schema", derive(schemars::JsonSchema))]
#[serde(tag = "policy", rename_all = "snake_case", deny_unknown_fields)]
pub enum Resolve {
    /// Absent is final.
    #[default]
    None,
    /// `r8_resolve()` fills the value from an expression over other fields.
    Computed {
        /// The expression over the other fields.
        expr: Logic,
    },
    /// `r8_resolve(x, context)` fills the value from data or the environment.
    Runtime,
}

impl Resolve {
    fn is_none(&self) -> bool {
        matches!(self, Resolve::None)
    }
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
        levels: Option<Vec<String>>,
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
        dim: Option<Vec<u32>>,
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
        columns: Vec<FieldDef>,
        #[serde(default, skip_serializing_if = "is_false")]
        /// Columns the declaration does not list are admitted.
        open: bool,
        #[serde(default, skip_serializing_if = "Vec::is_empty")]
        /// The rules every row satisfies.
        rules: Vec<RuleDef>,
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
        class: String,
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
    /// `option(T, resolve)`.
    Option {
        /// The type of a present value.
        inner: Box<FieldType>,
        #[serde(default, skip_serializing_if = "Resolve::is_none")]
        /// Who fills an absent value.
        resolve: Resolve,
    },
    /// `hashmap(T, keys)`: string keys, sorted at construction.
    Hashmap {
        /// The value type.
        value: Box<FieldType>,
        #[serde(default, skip_serializing_if = "is_default_keys")]
        /// The constraints on keys.
        keys: CharacterType,
    },
    /// `ordered_map(T, keys)`: unique string keys in insertion order; a named
    /// list in R and an array of `{"name", "value"}` pairs in JSON. Order is
    /// part of the value.
    OrderedMap {
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
