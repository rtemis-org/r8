//! The notation of a field type: one line of text that a host's printed
//! signatures and documentation show, such as `integer [0, Inf)` or
//! `optional vector of double`. The names of types, classes, and patterns are
//! in backticks, which a host writes as code or as a link.
//! spec: R8/class-system#notation-of-a-type

use super::decl::ColumnDef;
use super::types::{CharacterType, DoubleType, FieldType, IntegerType};
use crate::problem::{LISTED, number_text};

/// The notation of `t`.
///
/// Bounds are an interval, closed `[` or open `(` at each end; allowed values
/// are a set, of which the first [`LISTED`] are listed.
pub fn notation(t: &FieldType) -> String {
    match t {
        FieldType::Logical => "logical".into(),
        FieldType::Integer(i) => integer(i),
        FieldType::Double(d) => double(d),
        FieldType::Character(c) => format!("character{}", strings(c)),
        FieldType::Raw => "raw".into(),
        FieldType::Date => "date".into(),
        FieldType::Posixct => "date-time".into(),
        FieldType::Factor { levels, ordered } => format!(
            "{}factor{}",
            if *ordered { "ordered " } else { "" },
            levels.as_ref().map_or_else(String::new, |l| string_set(l))
        ),
        FieldType::Matrix {
            element,
            nrow,
            ncol,
        } => matrix("matrix", element, *nrow, *ncol),
        FieldType::SparseMatrix {
            element,
            nrow,
            ncol,
        } => matrix("sparse matrix", element, *nrow, *ncol),
        FieldType::Array { element, dim } => {
            let extents = dim.as_ref().map_or_else(String::new, |d| {
                let d: Vec<String> = d.iter().map(u32::to_string).collect();
                format!("{} ", d.join(" x "))
            });
            format!("{extents}array of {}", notation(element))
        }
        FieldType::MatrixOf { of } => format!("matrix with the columns of `{of}`"),
        FieldType::TableOf { of } => format!("table with the columns of `{of}`"),
        FieldType::Table { columns, open, .. } => table(columns, *open),
        FieldType::Json => "JSON".into(),
        FieldType::HostClass { class, .. } => format!("object of class `{class}`"),
        FieldType::Vec {
            element,
            min_length,
            max_length,
            unique,
        } => {
            let length = match (min_length, max_length) {
                (Some(a), Some(b)) if a == b => format!(", length {a}"),
                (Some(a), Some(b)) => format!(", length {a} to {b}"),
                (Some(a), None) => format!(", length {a} or more"),
                (None, Some(b)) => format!(", length {b} or less"),
                (None, None) => String::new(),
            };
            let distinct = if *unique { ", distinct" } else { "" };
            format!("vector of {}{length}{distinct}", notation(element))
        }
        FieldType::Option { inner } => format!("optional {}", notation(inner)),
        FieldType::BtreeMap { value, keys } => map("map", value, keys),
        FieldType::IndexMap { value, keys } => map("ordered map", value, keys),
        FieldType::Named { name } => format!("`{name}`"),
        FieldType::Dyn { r#trait } => format!("implementor of `{trait}`"),
        FieldType::Subtype { of } => format!("subtype of `{of}`"),
    }
}

/// An interval with the ends `lower` and `upper`, each its text and whether
/// it is excluded.
fn interval(
    lower: Option<(String, bool)>,
    upper: Option<(String, bool)>,
    infinite: bool,
) -> String {
    // An end without a bound admits every finite number, and the infinity
    // when the type admits it.
    let open = if infinite { ('[', ']') } else { ('(', ')') };
    let (a, a_open) = match lower {
        Some((t, excluded)) => (t, if excluded { '(' } else { '[' }),
        None => ("-Inf".into(), open.0),
    };
    let (b, b_open) = match upper {
        Some((t, excluded)) => (t, if excluded { ')' } else { ']' }),
        None => ("Inf".into(), open.1),
    };
    format!(" {a_open}{a}, {b}{b_open}")
}

/// The items of a set, the first [`LISTED`] of them.
fn set(items: impl ExactSizeIterator<Item = String>) -> String {
    let total = items.len();
    let mut shown: Vec<String> = items.take(LISTED).collect();
    if total > shown.len() {
        shown.push(format!("and {} more", total - shown.len()));
    }
    format!(" {{{}}}", shown.join(", "))
}

fn string_set(values: &[Box<str>]) -> String {
    set(values.iter().map(|v| format!("{v:?}")))
}

fn integer(t: &IntegerType) -> String {
    let mut out = String::from("integer");
    if t.lower.is_some() || t.upper.is_some() {
        out.push_str(&interval(
            t.lower.map(|b| (b.value.to_string(), b.exclusive)),
            t.upper.map(|b| (b.value.to_string(), b.exclusive)),
            false,
        ));
    }
    if let Some(v) = &t.values {
        out.push_str(&set(v.iter().map(i32::to_string)));
    }
    out
}

fn double(t: &DoubleType) -> String {
    let mut out = String::from("double");
    let bounded = t.lower.is_some() || t.upper.is_some();
    if bounded || t.infinite {
        out.push_str(&interval(
            t.lower.map(|b| (number_text(b.value), b.exclusive)),
            t.upper.map(|b| (number_text(b.value), b.exclusive)),
            t.infinite,
        ));
    }
    if let Some(v) = &t.values {
        out.push_str(&set(v.iter().map(|x| number_text(*x))));
    }
    if t.nan {
        out.push_str(" or NaN");
    }
    out
}

/// The constraints of a string: its values and its pattern.
fn strings(t: &CharacterType) -> String {
    let mut out = t
        .values
        .as_ref()
        .map_or_else(String::new, |v| string_set(v));
    if let Some(p) = &t.pattern {
        out.push_str(&format!(" matching `{p}`"));
    }
    out
}

fn matrix(what: &str, element: &FieldType, nrow: Option<u32>, ncol: Option<u32>) -> String {
    let of = notation(element);
    let count = |n: u32, one: &str| format!("{n} {one}{}", if n == 1 { "" } else { "s" });
    match (nrow, ncol) {
        (Some(r), Some(c)) => format!("{r} x {c} {what} of {of}"),
        (Some(r), None) => format!("{what} of {of} with {}", count(r, "row")),
        (None, Some(c)) => format!("{what} of {of} with {}", count(c, "column")),
        (None, None) => format!("{what} of {of}"),
    }
}

fn table(columns: &[ColumnDef], open: bool) -> String {
    let what = if open { "open table" } else { "table" };
    if columns.is_empty() {
        return what.into();
    }
    let items: Vec<String> = columns
        .iter()
        .map(|c| format!("{} ({})", c.field.name, notation(&c.field.ty)))
        .collect();
    let word = if items.len() == 1 {
        "column"
    } else {
        "columns"
    };
    format!("{what} with the {word} {}", items.join(", "))
}

fn map(what: &str, value: &FieldType, keys: &CharacterType) -> String {
    let k = strings(keys);
    let keys = if k.is_empty() {
        String::new()
    } else {
        format!(" with keys{k}")
    };
    format!("{what} of {}{keys}", notation(value))
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn notations() {
        let cases = [
            (json!({"kind": "logical"}), "logical"),
            (json!({"kind": "integer"}), "integer"),
            (json!({"kind": "integer", "min": 0}), "integer [0, Inf)"),
            (
                json!({"kind": "integer", "exclusive_min": 0, "max": 10}),
                "integer (0, 10]",
            ),
            (
                json!({"kind": "integer", "values": [1, 2, 3, 4, 5, 6, 7]}),
                "integer {1, 2, 3, 4, 5, and 2 more}",
            ),
            (json!({"kind": "double"}), "double"),
            (
                json!({"kind": "double", "exclusive_min": 0.0, "max": 1.0}),
                "double (0, 1]",
            ),
            (
                json!({"kind": "double", "infinite": true, "nan": true}),
                "double [-Inf, Inf] or NaN",
            ),
            (
                json!({"kind": "double", "min": 0.5, "infinite": true}),
                "double [0.5, Inf]",
            ),
            (
                json!({"kind": "double", "min": 0.0, "max": 1.0, "nan": true}),
                "double [0, 1] or NaN",
            ),
            (
                json!({"kind": "double", "values": [1.0, 2.0], "nan": true}),
                "double {1, 2} or NaN",
            ),
            (json!({"kind": "double", "nan": true}), "double or NaN"),
            (
                json!({"kind": "character", "values": ["a", "b"]}),
                "character {\"a\", \"b\"}",
            ),
            (
                json!({"kind": "character", "pattern": "^[a-z]+$"}),
                "character matching `^[a-z]+$`",
            ),
            (json!({"kind": "posixct"}), "date-time"),
            (
                json!({"kind": "factor", "levels": ["lo", "hi"], "ordered": true}),
                "ordered factor {\"lo\", \"hi\"}",
            ),
            (
                json!({"kind": "matrix", "element": {"kind": "double"}, "ncol": 1}),
                "matrix of double with 1 column",
            ),
            (
                json!({"kind": "matrix", "element": {"kind": "double"}, "nrow": 2, "ncol": 3}),
                "2 x 3 matrix of double",
            ),
            (
                json!({"kind": "array", "element": {"kind": "integer"}, "dim": [2, 3, 4]}),
                "2 x 3 x 4 array of integer",
            ),
            (
                json!({"kind": "sparse_matrix", "element": {"kind": "logical"}, "nrow": 2}),
                "sparse matrix of logical with 2 rows",
            ),
            (
                json!({"kind": "table_of", "of": "ns::P"}),
                "table with the columns of `ns::P`",
            ),
            (
                json!({"kind": "table", "open": true, "columns": [
                    {"name": "k", "type": {"kind": "integer"}},
                    {"name": "l", "type": {"kind": "option", "inner": {"kind": "character"}}},
                ]}),
                "open table with the columns k (integer), l (optional character)",
            ),
            (
                json!({"kind": "host_class", "host": "r", "class": "lm", "system": "s3"}),
                "object of class `lm`",
            ),
            (
                json!({"kind": "vec", "element": {"kind": "double", "min": 0.0}, "min_length": 1, "unique": true}),
                "vector of double [0, Inf), length 1 or more, distinct",
            ),
            (
                json!({"kind": "option", "inner": {"kind": "subtype", "of": "ns::R"}}),
                "optional subtype of `ns::R`",
            ),
            (
                json!({"kind": "index_map", "value": {"kind": "json"}, "keys": {"pattern": "^k"}}),
                "ordered map of JSON with keys matching `^k`",
            ),
            (
                json!({"kind": "dyn", "trait": "ns::Fit"}),
                "implementor of `ns::Fit`",
            ),
            (json!({"kind": "named", "name": "ns::R"}), "`ns::R`"),
        ];
        for (ir, want) in cases {
            let t: FieldType = serde_json::from_value(ir).unwrap();
            assert_eq!(notation(&t), want);
        }
    }
}
