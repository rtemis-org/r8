//! Validation of a value's JSON form against its type.
//!
//! [`validate`] checks a value's JSON form against a named type: every field
//! type, constraint, and rule, collecting every error with a stable code, a
//! JSON Pointer path, and, for a rule, its ID. A value's JSON form is the form
//! the codec writes: an absent field is `null` or an omitted member, and the
//! document envelope (`"$type"`, `"$types"`, `"$provenance"`) is accepted on
//! the outermost object.

use std::collections::{HashMap, HashSet};

use regex::Regex;
use serde_json::{Map, Value};

use crate::ir::{
    CharacterType, DoubleType, FieldDef, FieldType, IntegerType, RuleDef, TypeDef, VariantShape,
};
use crate::name::{Name, QualifiedName};
use crate::rules::{CompiledRule, Outcome, TypeLookup};

/// A problem in a value.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ValueError {
    /// A stable code, such as `missing_field`.
    pub code: &'static str,
    /// A JSON Pointer into the value's JSON form.
    pub path: String,
    /// The ID of the violated rule, for `rule_violation` and `rule_error`.
    pub rule: Option<Name>,
    pub message: String,
}

/// The range of `type_integer()`: `i32::MIN` is R's `NA_integer_`.
const INT_MIN: i64 = -2_147_483_647;
const INT_MAX: i64 = 2_147_483_647;

/// The range of `type_posixct()`: microsecond counts of magnitude below 2^52,
/// which a double holds exactly as seconds.
const POSIXCT_MIN: &str = "1827-04-16T00:06:12.629505Z";
const POSIXCT_MAX: &str = "2112-09-17T23:53:47.370495Z";

const ENVELOPE: [&str; 3] = ["$type", "$types", "$provenance"];

struct Validator<'a> {
    lookup: &'a dyn TypeLookup,
    errors: Vec<ValueError>,
    patterns: HashMap<String, Option<Regex>>,
    rules: HashMap<usize, Option<CompiledRule>>,
}

fn esc(s: &str) -> String {
    s.replace('~', "~0").replace('/', "~1")
}

fn kind(v: &Value) -> &'static str {
    match v {
        Value::Null => "null",
        Value::Bool(_) => "a logical",
        Value::Number(_) => "a number",
        Value::String(_) => "a string",
        Value::Array(_) => "an array",
        Value::Object(_) => "an object",
    }
}

fn is_leap(y: u32) -> bool {
    (y.is_multiple_of(4) && !y.is_multiple_of(100)) || y.is_multiple_of(400)
}

fn digits(s: &str) -> Option<u32> {
    (!s.is_empty() && s.bytes().all(|b| b.is_ascii_digit())).then(|| s.parse().ok())?
}

/// A full-date `YYYY-MM-DD` in the proleptic Gregorian calendar, year 0000 to
/// 9999.
fn valid_date(s: &str) -> bool {
    let b = s.as_bytes();
    if b.len() != 10 || b[4] != b'-' || b[7] != b'-' {
        return false;
    }
    let (Some(y), Some(m), Some(d)) = (digits(&s[0..4]), digits(&s[5..7]), digits(&s[8..10]))
    else {
        return false;
    };
    let days = match m {
        1 | 3 | 5 | 7 | 8 | 10 | 12 => 31,
        4 | 6 | 9 | 11 => 30,
        2 if is_leap(y) => 29,
        2 => 28,
        _ => return false,
    };
    (1..=days).contains(&d)
}

/// The canonical date-time form, `YYYY-MM-DDTHH:MM:SS.ffffffZ`, within the
/// domain of `type_posixct()`.
fn valid_posixct(s: &str) -> bool {
    let b = s.as_bytes();
    if b.len() != 27
        || b[10] != b'T'
        || b[13] != b':'
        || b[16] != b':'
        || b[19] != b'.'
        || b[26] != b'Z'
    {
        return false;
    }
    let ok = valid_date(&s[0..10])
        && digits(&s[11..13]).is_some_and(|h| h <= 23)
        && digits(&s[14..16]).is_some_and(|m| m <= 59)
        && digits(&s[17..19]).is_some_and(|x| x <= 59)
        && digits(&s[20..26]).is_some();
    ok && (POSIXCT_MIN..=POSIXCT_MAX).contains(&s)
}

/// Canonical base64 (RFC 4648 section 4): the standard alphabet, padding to
/// a multiple of four, and zero bits after the last character's data.
fn valid_base64(s: &str) -> bool {
    let b = s.as_bytes();
    if !b.len().is_multiple_of(4) {
        return false;
    }
    let pad = b.iter().rev().take_while(|&&c| c == b'=').count();
    if pad > 2 {
        return false;
    }
    let val = |c: u8| match c {
        b'A'..=b'Z' => Some(c - b'A'),
        b'a'..=b'z' => Some(c - b'a' + 26),
        b'0'..=b'9' => Some(c - b'0' + 52),
        b'+' => Some(62),
        b'/' => Some(63),
        _ => None,
    };
    let body = &b[..b.len() - pad];
    if body.iter().any(|&c| val(c).is_none()) {
        return false;
    }
    match (pad, body.last()) {
        (1, Some(&c)) => val(c).unwrap() & 0b11 == 0,
        (2, Some(&c)) => val(c).unwrap() & 0b1111 == 0,
        _ => true,
    }
}

impl Validator<'_> {
    fn error(&mut self, code: &'static str, path: &str, message: String) {
        self.errors.push(ValueError {
            code,
            path: path.to_string(),
            rule: None,
            message,
        });
    }

    fn mismatch(&mut self, path: &str, want: &str, v: &Value) {
        self.error(
            "type_mismatch",
            path,
            format!("expected {want}, found {}", kind(v)),
        );
    }

    fn regex(&mut self, pattern: &str) -> Option<&Regex> {
        self.patterns
            .entry(pattern.to_string())
            .or_insert_with(|| Regex::new(pattern).ok())
            .as_ref()
    }

    /// Checks a value of a field type. `null` is reported as a missing value;
    /// callers handle absence where the type admits it.
    fn value(&mut self, ft: &FieldType, v: &Value, path: &str) {
        match ft {
            FieldType::Option { inner, .. } => {
                if !v.is_null() {
                    self.value(inner, v, path);
                }
            }
            _ if v.is_null() => {
                self.error("missing_value", path, "a value is required here".to_string());
            }
            FieldType::Logical => {
                if !v.is_boolean() {
                    self.mismatch(path, "a logical", v);
                }
            }
            FieldType::Integer(c) => self.integer(c, v, path),
            FieldType::Double(c) => self.double(c, v, path),
            FieldType::Character(c) => self.character(c, v, path),
            FieldType::Raw => match v.as_str() {
                Some(s) if valid_base64(s) => {}
                Some(_) => self.error("invalid_base64", path, "bytes are written in canonical padded base64 (RFC 4648 section 4)".to_string()),
                None => self.mismatch(path, "a base64 string", v),
            },
            FieldType::Date => match v.as_str() {
                Some(s) if valid_date(s) => {}
                Some(s) => self.error("invalid_date", path, format!("`{s}` is not a date of the form YYYY-MM-DD")),
                None => self.mismatch(path, "a date string", v),
            },
            FieldType::Posixct => match v.as_str() {
                Some(s) if valid_posixct(s) => {}
                Some(s) => self.error(
                    "invalid_datetime",
                    path,
                    format!("`{s}` is not a date-time of the form YYYY-MM-DDTHH:MM:SS.ffffffZ from {POSIXCT_MIN} to {POSIXCT_MAX}"),
                ),
                None => self.mismatch(path, "a date-time string", v),
            },
            FieldType::Factor { levels, .. } => self.factor(levels.as_deref(), false, v, path),
            FieldType::Matrix { element, nrow, ncol } => {
                self.dense(element, v, path, Some([*nrow, *ncol]), None);
            }
            FieldType::Array { element, dim } => self.dense(element, v, path, None, dim.as_deref()),
            FieldType::SparseMatrix { element, nrow, ncol } => self.sparse(element, v, path, *nrow, *ncol),
            FieldType::MatrixOf { of } => match self.struct_fields(of) {
                Some(fields) => {
                    let element = fields.first().map(|f| f.ty.clone()).unwrap_or(FieldType::Logical);
                    let n = fields.len() as u32;
                    self.dense(&element, v, path, Some([None, Some(n)]), None);
                }
                None => self.unresolved(of, path),
            },
            FieldType::Table { columns, open, rules } => self.table(columns, *open, rules, v, path),
            FieldType::TableOf { of } => match self.struct_fields(of) {
                Some(fields) => self.table(&fields, false, &[], v, path),
                None => self.unresolved(of, path),
            },
            FieldType::Json => {}
            FieldType::R { .. } => self.error(
                "runtime_only",
                path,
                "a runtime-only R value has no JSON form; the member is null or omitted".to_string(),
            ),
            FieldType::Vec { element, min_length, max_length, unique } => {
                let Some(items) = v.as_array() else { return self.mismatch(path, "an array", v) };
                self.lengths(items.len(), *min_length, *max_length, path);
                for (i, item) in items.iter().enumerate() {
                    self.value(element, item, &format!("{path}/{i}"));
                }
                if *unique {
                    self.distinct(items.iter().enumerate().map(|(i, x)| (format!("{path}/{i}"), x)));
                }
            }
            FieldType::Hashmap { value, keys } => {
                let Some(m) = v.as_object() else { return self.mismatch(path, "an object", v) };
                for (k, x) in m {
                    let p = format!("{path}/{}", esc(k));
                    self.key(keys, k, &p);
                    self.value(value, x, &p);
                }
            }
            FieldType::OrderedMap { value, keys } => {
                let Some(pairs) = v.as_array() else {
                    return self.mismatch(path, "an array of {\"name\", \"value\"} pairs", v);
                };
                let mut seen = HashSet::new();
                for (i, pair) in pairs.iter().enumerate() {
                    let p = format!("{path}/{i}");
                    let Some(m) = pair.as_object().filter(|m| m.len() == 2 && m.contains_key("name") && m.contains_key("value")) else {
                        self.error("type_mismatch", &p, "an ordered map entry is an object with exactly `name` and `value`".to_string());
                        continue;
                    };
                    match m["name"].as_str() {
                        Some(k) => {
                            if !seen.insert(k.to_string()) {
                                self.error("duplicate_key", &format!("{p}/name"), format!("key `{k}` appears twice"));
                            }
                            self.key(keys, k, &format!("{p}/name"));
                        }
                        None => self.mismatch(&format!("{p}/name"), "a string key", &m["name"]),
                    }
                    self.value(value, &m["value"], &format!("{p}/value"));
                }
            }
            FieldType::Named { name } => self.named(name, v, path, false),
            FieldType::Dyn { r#trait } => self.dynamic(r#trait, v, path),
        }
    }

    fn unresolved(&mut self, name: &QualifiedName, path: &str) {
        self.error(
            "unresolved_type",
            path,
            format!("`{name}` is not registered"),
        );
    }

    fn struct_fields(&self, name: &QualifiedName) -> Option<Vec<FieldDef>> {
        match self.lookup.find(name)? {
            TypeDef::Struct(s) => Some(s.fields.clone()),
            TypeDef::Enum(_) => None,
        }
    }

    fn integer(&mut self, c: &IntegerType, v: &Value, path: &str) {
        let Some(x) = v.as_f64() else {
            return self.mismatch(path, "an integer", v);
        };
        if x.fract() != 0.0 {
            return self.error("not_integer", path, format!("{x} is not a whole number"));
        }
        let x = x as i64;
        if !(INT_MIN..=INT_MAX).contains(&x) {
            return self.error(
                "out_of_range",
                path,
                format!("{x} is outside the integer range, -2147483647 to 2147483647"),
            );
        }
        let x = x as i32;
        self.bounds(
            f64::from(x),
            c.min.map(f64::from),
            c.max.map(f64::from),
            c.exclusive_min.map(f64::from),
            c.exclusive_max.map(f64::from),
            path,
        );
        if let Some(values) = &c.values
            && !values.contains(&x)
        {
            self.error("not_in_enum", path, format!("{x} is not one of {values:?}"));
        }
    }

    fn double(&mut self, c: &DoubleType, v: &Value, path: &str) {
        let x = match v {
            Value::Number(n) => n.as_f64().unwrap_or(f64::NAN),
            Value::String(s) if matches!(s.as_str(), "Infinity" | "-Infinity" | "NaN") => {
                if c.finite {
                    return self.error(
                        "not_finite",
                        path,
                        format!(
                            "`{s}` is not finite; declare `type_double(finite = FALSE)` to admit it"
                        ),
                    );
                }
                match s.as_str() {
                    "Infinity" => f64::INFINITY,
                    "-Infinity" => f64::NEG_INFINITY,
                    _ => f64::NAN,
                }
            }
            _ => return self.mismatch(path, "a number", v),
        };
        self.bounds(x, c.min, c.max, c.exclusive_min, c.exclusive_max, path);
        if let Some(values) = &c.values
            && !values.contains(&x)
        {
            self.error("not_in_enum", path, format!("{x} is not one of {values:?}"));
        }
    }

    /// Bounds; `NaN` satisfies none.
    fn bounds(
        &mut self,
        x: f64,
        min: Option<f64>,
        max: Option<f64>,
        emin: Option<f64>,
        emax: Option<f64>,
        path: &str,
    ) {
        if let Some(m) = min
            && (x.is_nan() || x < m)
        {
            self.error("below_min", path, format!("{x} is below the minimum {m}"));
        }
        if let Some(m) = max
            && (x.is_nan() || x > m)
        {
            self.error("above_max", path, format!("{x} is above the maximum {m}"));
        }
        if let Some(m) = emin
            && (x.is_nan() || x <= m)
        {
            self.error("below_min", path, format!("{x} is not above {m}"));
        }
        if let Some(m) = emax
            && (x.is_nan() || x >= m)
        {
            self.error("above_max", path, format!("{x} is not below {m}"));
        }
    }

    fn character(&mut self, c: &CharacterType, v: &Value, path: &str) {
        let Some(s) = v.as_str() else {
            return self.mismatch(path, "a string", v);
        };
        if s.contains('\0') {
            return self.error(
                "nul_character",
                path,
                "a string cannot hold U+0000, which R cannot represent".to_string(),
            );
        }
        self.string_constraints(c, s, path);
    }

    fn string_constraints(&mut self, c: &CharacterType, s: &str, path: &str) {
        if let Some(p) = &c.pattern
            && let Some(re) = self.regex(p)
            && !re.is_match(s)
        {
            self.error(
                "pattern_mismatch",
                path,
                format!("`{s}` does not match `{p}`"),
            );
        }
        if let Some(values) = &c.values
            && !values.iter().any(|x| x == s)
        {
            self.error(
                "not_in_enum",
                path,
                format!("`{s}` is not one of {values:?}"),
            );
        }
    }

    fn key(&mut self, keys: &CharacterType, k: &str, path: &str) {
        if k.is_empty() {
            return self.error("invalid_key", path, "a map key is not empty".to_string());
        }
        self.string_constraints(keys, k, path);
    }

    fn lengths(&mut self, n: usize, min: Option<u64>, max: Option<u64>, path: &str) {
        if let Some(m) = min
            && (n as u64) < m
        {
            self.error(
                "length_below_min",
                path,
                format!("length {n} is below the minimum {m}"),
            );
        }
        if let Some(m) = max
            && (n as u64) > m
        {
            self.error(
                "length_above_max",
                path,
                format!("length {n} is above the maximum {m}"),
            );
        }
    }

    fn distinct<'v>(&mut self, items: impl Iterator<Item = (String, &'v Value)>) {
        let mut seen: Vec<&Value> = Vec::new();
        for (p, x) in items {
            if x.is_null() {
                continue;
            }
            if seen.iter().any(|s| crate::rules::strict_equal(s, x)) {
                self.error("not_unique", &p, format!("{x} appears more than once"));
            } else {
                seen.push(x);
            }
        }
    }

    /// A factor: a string array when the levels are declared, otherwise
    /// `{"levels", "codes"}` with 1-based codes. `cells_optional` admits a
    /// missing cell, as in an `option()` table column.
    fn factor(&mut self, levels: Option<&[String]>, cells_optional: bool, v: &Value, path: &str) {
        match (levels, v) {
            (Some(levels), Value::Array(items)) => {
                for (i, x) in items.iter().enumerate() {
                    let p = format!("{path}/{i}");
                    match x {
                        Value::Null if cells_optional => {}
                        Value::String(s) if levels.contains(s) => {}
                        Value::String(s) => self.error(
                            "not_a_level",
                            &p,
                            format!("`{s}` is not one of the levels {levels:?}"),
                        ),
                        other => self.mismatch(&p, "a level", other),
                    }
                }
            }
            (None, Value::Object(m)) => {
                let lv = m.get("levels").and_then(Value::as_array);
                let codes = m.get("codes").and_then(Value::as_array);
                let (Some(lv), Some(codes)) = (lv, codes) else {
                    return self.error("factor_form", path, "a factor whose levels come from the data is {\"levels\": [...], \"codes\": [...]}".to_string());
                };
                if m.len() != 2 {
                    self.error(
                        "factor_form",
                        path,
                        "a factor holds `levels` and `codes` only".to_string(),
                    );
                }
                if lv.iter().any(|l| !l.is_string()) {
                    self.error(
                        "factor_form",
                        &format!("{path}/levels"),
                        "levels are strings".to_string(),
                    );
                }
                self.distinct(
                    lv.iter()
                        .enumerate()
                        .map(|(i, x)| (format!("{path}/levels/{i}"), x)),
                );
                for (i, c) in codes.iter().enumerate() {
                    let p = format!("{path}/codes/{i}");
                    match c.as_u64() {
                        Some(k) if k >= 1 && (k as usize) <= lv.len() => {}
                        _ if c.is_null() && cells_optional => {}
                        _ => self.error(
                            "invalid_code",
                            &p,
                            format!("a code is an integer from 1 to {}", lv.len()),
                        ),
                    }
                }
            }
            (Some(_), _) => self.error(
                "factor_form",
                path,
                "a factor with declared levels is a string array".to_string(),
            ),
            (None, _) => self.error(
                "factor_form",
                path,
                "a factor whose levels come from the data is {\"levels\": [...], \"codes\": [...]}"
                    .to_string(),
            ),
        }
    }

    fn shape(&mut self, v: &Value, path: &str) -> Option<(Vec<u64>, Map<String, Value>)> {
        let Some(m) = v.as_object() else {
            self.mismatch(path, "an object with `shape`", v);
            return None;
        };
        let Some(shape) = m
            .get("shape")
            .and_then(Value::as_array)
            .and_then(|s| s.iter().map(Value::as_u64).collect::<Option<Vec<u64>>>())
        else {
            self.error(
                "matrix_form",
                path,
                "`shape` is an array of non-negative integers".to_string(),
            );
            return None;
        };
        Some((shape, m.clone()))
    }

    fn dense(
        &mut self,
        element: &FieldType,
        v: &Value,
        path: &str,
        matrix: Option<[Option<u32>; 2]>,
        dim: Option<&[u32]>,
    ) {
        let Some((shape, m)) = self.shape(v, path) else {
            return;
        };
        if m.keys()
            .any(|k| !matches!(k.as_str(), "shape" | "data" | "dimnames"))
        {
            self.error(
                "matrix_form",
                path,
                "a matrix holds `shape`, `data`, and optionally `dimnames`".to_string(),
            );
        }
        if let Some([nrow, ncol]) = matrix {
            if shape.len() != 2 {
                return self.error(
                    "matrix_form",
                    &format!("{path}/shape"),
                    "a matrix has two dimensions".to_string(),
                );
            }
            for (i, want) in [nrow, ncol].into_iter().enumerate() {
                if let Some(w) = want
                    && shape[i] != u64::from(w)
                {
                    self.error(
                        "dimension_mismatch",
                        &format!("{path}/shape/{i}"),
                        format!("dimension {} is {}, declared {w}", i + 1, shape[i]),
                    );
                }
            }
        }
        if let Some(dim) = dim
            && shape != dim.iter().map(|&d| u64::from(d)).collect::<Vec<_>>()
        {
            self.error(
                "dimension_mismatch",
                &format!("{path}/shape"),
                format!("shape {shape:?} differs from the declared {dim:?}"),
            );
        }
        let Some(data) = m.get("data").and_then(Value::as_array) else {
            return self.error("matrix_form", path, "`data` is an array".to_string());
        };
        let n: u64 = shape.iter().product();
        if data.len() as u64 != n {
            return self.error(
                "matrix_form",
                &format!("{path}/data"),
                format!(
                    "`data` holds {} values, the shape {shape:?} needs {n}",
                    data.len()
                ),
            );
        }
        for (i, x) in data.iter().enumerate() {
            self.value(element, x, &format!("{path}/data/{i}"));
        }
        if let Some(dn) = m.get("dimnames") {
            let ok = dn.as_array().is_some_and(|d| {
                d.len() == shape.len()
                    && d.iter().zip(&shape).all(|(names, &len)| {
                        names.is_null()
                            || names.as_array().is_some_and(|a| {
                                a.len() as u64 == len && a.iter().all(Value::is_string)
                            })
                    })
            });
            if !ok {
                self.error(
                    "matrix_form",
                    &format!("{path}/dimnames"),
                    "`dimnames` holds, per dimension, null or one string per index".to_string(),
                );
            }
        }
    }

    fn sparse(
        &mut self,
        element: &FieldType,
        v: &Value,
        path: &str,
        nrow: Option<u32>,
        ncol: Option<u32>,
    ) {
        let Some((shape, m)) = self.shape(v, path) else {
            return;
        };
        if shape.len() != 2 {
            return self.error(
                "matrix_form",
                &format!("{path}/shape"),
                "a sparse matrix has two dimensions".to_string(),
            );
        }
        for (i, want) in [nrow, ncol].into_iter().enumerate() {
            if let Some(w) = want
                && shape[i] != u64::from(w)
            {
                self.error(
                    "dimension_mismatch",
                    &format!("{path}/shape/{i}"),
                    format!("dimension {} is {}, declared {w}", i + 1, shape[i]),
                );
            }
        }
        let ints = |k: &str| {
            m.get(k)
                .and_then(Value::as_array)
                .and_then(|a| a.iter().map(Value::as_u64).collect::<Option<Vec<u64>>>())
        };
        let (Some(indptr), Some(indices), Some(data)) = (
            ints("indptr"),
            ints("indices"),
            m.get("data").and_then(Value::as_array),
        ) else {
            return self.error(
                "sparse_form",
                path,
                "a sparse matrix holds `shape`, `indptr`, `indices`, and `data`".to_string(),
            );
        };
        let nnz = data.len() as u64;
        let well_formed = indptr.len() as u64 == shape[1] + 1
            && indptr.first() == Some(&0)
            && indptr.last() == Some(&nnz)
            && indptr.windows(2).all(|w| w[0] <= w[1])
            && indices.len() as u64 == nnz
            && indptr.windows(2).all(|w| {
                let col = &indices[w[0] as usize..w[1] as usize];
                col.iter().all(|&r| r < shape[0]) && col.windows(2).all(|p| p[0] < p[1])
            });
        if !well_formed {
            return self.error(
                "sparse_form",
                path,
                "compressed sparse column form: `indptr` has ncol + 1 non-decreasing offsets from 0 to the number of values, and each column's row `indices` increase and lie below nrow".to_string(),
            );
        }
        for (i, x) in data.iter().enumerate() {
            self.value(element, x, &format!("{path}/data/{i}"));
        }
    }

    fn table(
        &mut self,
        columns: &[FieldDef],
        open: bool,
        rules: &[RuleDef],
        v: &Value,
        path: &str,
    ) {
        let Some(m) = v.as_object() else {
            return self.mismatch(path, "a table object", v);
        };
        let Some(n) = m.get("$length").and_then(Value::as_u64) else {
            return self.error(
                "table_form",
                path,
                "a table holds `$length`, its number of rows".to_string(),
            );
        };
        let declared: HashSet<&str> = columns.iter().map(|c| c.name.as_str()).collect();
        let extra_types: Option<&Map<String, Value>> = m.get("$columns").and_then(Value::as_object);
        if m.contains_key("$columns") && (!open || extra_types.is_none()) {
            self.error(
                "table_form",
                &format!("{path}/$columns"),
                "`$columns` belongs to an open table and is an object of column types".to_string(),
            );
        }
        let mut all_columns: Vec<FieldDef> = Vec::new();
        for c in columns {
            match m.get(c.name.as_str()) {
                Some(_) => all_columns.push(c.clone()),
                None if c.optional => {}
                None => self.error(
                    "missing_column",
                    &format!("{path}/{}", esc(c.name.as_str())),
                    format!("column `{}` is required", c.name),
                ),
            }
        }
        for (k, _) in m
            .iter()
            .filter(|(k, _)| !k.starts_with('$') && !declared.contains(k.as_str()))
        {
            let p = format!("{path}/{}", esc(k));
            if !open {
                self.error(
                    "unknown_column",
                    &p,
                    format!("`{k}` is not a column of this table"),
                );
                continue;
            }
            let ty = extra_types
                .and_then(|t| t.get(k))
                .map(|t| serde_json::from_value::<FieldType>(t.clone()));
            match (ty, Name::new(k.clone())) {
                (Some(Ok(ft)), Ok(name)) if crate::ir::is_column_type(&ft) => all_columns.push(FieldDef {
                    name,
                    ty: ft,
                    default: None,
                    description: String::new(),
                    sensitive: false,
                    unit: None,
                    external: false,
                    unique: false,
                    optional: false,
                    inherited_from: None,
                    extensions: Default::default(),
                }),
                (_, Err(e)) => self.error("unknown_column", &p, e.to_string()),
                _ => self.error("column_type", &format!("{path}/$columns/{}", esc(k)), format!("column `{k}` of an open table needs its type in `$columns`: a scalar, a factor, or option() of one")),
            }
        }
        if let Some(t) = extra_types {
            for k in t
                .keys()
                .filter(|k| !m.contains_key(k.as_str()) || declared.contains(k.as_str()))
            {
                self.error(
                    "column_type",
                    &format!("{path}/$columns/{}", esc(k)),
                    format!("`$columns` lists `{k}`, which is not an extra column of the table"),
                );
            }
        }
        for c in &all_columns {
            let p = format!("{path}/{}", esc(c.name.as_str()));
            let cells = &m[c.name.as_str()];
            let (inner, optional) = match &c.ty {
                FieldType::Option { inner, .. } => (inner.as_ref(), true),
                other => (other, false),
            };
            if let FieldType::Factor { levels, .. } = inner {
                let len = match cells {
                    Value::Array(a) => Some(a.len()),
                    Value::Object(o) => o.get("codes").and_then(Value::as_array).map(Vec::len),
                    _ => None,
                };
                if let Some(len) = len
                    && len as u64 != n
                {
                    self.error(
                        "column_length",
                        &p,
                        format!(
                            "column `{}` has {len} cells, the table has {n} rows",
                            c.name
                        ),
                    );
                }
                self.factor(levels.as_deref(), optional, cells, &p);
                continue;
            }
            let Some(items) = cells.as_array() else {
                self.mismatch(&p, "a column array", cells);
                continue;
            };
            if items.len() as u64 != n {
                self.error(
                    "column_length",
                    &p,
                    format!(
                        "column `{}` has {} cells, the table has {n} rows",
                        c.name,
                        items.len()
                    ),
                );
                continue;
            }
            for (i, x) in items.iter().enumerate() {
                if !(x.is_null() && optional) {
                    self.value(inner, x, &format!("{p}/{i}"));
                }
            }
            if c.unique {
                self.distinct(
                    items
                        .iter()
                        .enumerate()
                        .map(|(i, x)| (format!("{p}/{i}"), x)),
                );
            }
        }
        if !rules.is_empty() && self.errors_under(path) == 0 {
            for r in rules {
                if let Some(rule) = self.compiled(r, &all_columns) {
                    match rule.check_rows(v) {
                        Ok(paths) => {
                            for p in paths {
                                self.violation(&rule, &format!("{path}{p}"));
                            }
                        }
                        Err(e) => self.rule_error(&rule, path, e),
                    }
                }
            }
        }
    }

    fn errors_under(&self, path: &str) -> usize {
        self.errors
            .iter()
            .filter(|e| e.path == path || e.path.starts_with(&format!("{path}/")))
            .count()
    }

    fn compiled(&mut self, r: &RuleDef, scope: &[FieldDef]) -> Option<CompiledRule> {
        let key = r as *const RuleDef as usize;
        if !self.rules.contains_key(&key) {
            let c = CompiledRule::compile(r, scope, self.lookup).ok();
            if c.is_none() {
                self.error(
                    "rule_invalid",
                    "",
                    format!("rule `{}` does not compile; check the declaration", r.id),
                );
            }
            self.rules.insert(key, c);
        }
        self.rules[&key].clone()
    }

    fn violation(&mut self, rule: &CompiledRule, path: &str) {
        self.errors.push(ValueError {
            code: "rule_violation",
            path: path.to_string(),
            rule: Some(rule.id.clone()),
            message: rule.message.clone(),
        });
    }

    fn rule_error(&mut self, rule: &CompiledRule, path: &str, e: String) {
        self.errors.push(ValueError {
            code: "rule_error",
            path: path.to_string(),
            rule: Some(rule.id.clone()),
            message: e,
        });
    }

    /// Checks an object against fields, with `allowed` extra members, then
    /// the rules, when the fields themselves hold no error.
    fn object(
        &mut self,
        fields: &[FieldDef],
        rules: &[RuleDef],
        v: &Value,
        path: &str,
        allowed: &[&str],
    ) {
        let Some(m) = v.as_object() else {
            return self.mismatch(path, "an object", v);
        };
        for (k, _) in m.iter().filter(|(k, _)| {
            !allowed.contains(&k.as_str()) && !fields.iter().any(|f| f.name.as_str() == k.as_str())
        }) {
            self.error(
                "unknown_field",
                &format!("{path}/{}", esc(k)),
                format!("`{k}` is not a field"),
            );
        }
        let before = self.errors.len();
        for f in fields {
            let p = format!("{path}/{}", esc(f.name.as_str()));
            match m.get(f.name.as_str()) {
                None | Some(Value::Null) => {
                    let admits_absence =
                        matches!(f.ty, FieldType::Option { .. }) || f.default.is_some();
                    if !admits_absence {
                        self.error(
                            "missing_field",
                            &p,
                            format!("field `{}` is required", f.name),
                        );
                    }
                }
                Some(x) => self.value(&f.ty, x, &p),
            }
        }
        if self.errors.len() > before {
            return;
        }
        for r in rules {
            if let Some(rule) = self.compiled(r, fields) {
                match rule.check(v) {
                    Ok(Outcome::Holds) => {}
                    Ok(Outcome::Violated(paths)) => {
                        for p in paths {
                            self.violation(&rule, &format!("{path}{p}"));
                        }
                    }
                    Err(e) => self.rule_error(&rule, path, e),
                }
            }
        }
    }

    fn named(&mut self, name: &QualifiedName, v: &Value, path: &str, top: bool) {
        let envelope: &[&str] = if top { &ENVELOPE } else { &[] };
        match self.lookup.find(name) {
            None => self.unresolved(name, path),
            Some(TypeDef::Struct(s)) => {
                self.object(&s.fields.clone(), &s.rules.clone(), v, path, envelope)
            }
            Some(TypeDef::Enum(e)) => {
                let e = e.clone();
                let unit_only = e
                    .variants
                    .iter()
                    .all(|x| matches!(x.shape, VariantShape::Unit));
                if unit_only {
                    match v.as_str() {
                        Some(s) if e.variants.iter().any(|x| x.name.as_str() == s) => {}
                        Some(s) => self.error(
                            "unknown_variant",
                            path,
                            format!("`{s}` is not a variant of `{name}`"),
                        ),
                        None => self.mismatch(path, &format!("a variant name of `{name}`"), v),
                    }
                    return;
                }
                let tag = e.tag.as_str();
                let Some(variant) = v.get(tag).and_then(Value::as_str) else {
                    return if v.is_object() {
                        self.error(
                            "missing_tag",
                            path,
                            format!("a `{name}` value names its variant in `{tag}`"),
                        )
                    } else {
                        self.mismatch(path, &format!("a `{name}` object"), v)
                    };
                };
                let Some(def) = e.variants.iter().find(|x| x.name.as_str() == variant) else {
                    return self.error(
                        "unknown_variant",
                        &format!("{path}/{}", esc(tag)),
                        format!("`{variant}` is not a variant of `{name}`"),
                    );
                };
                let mut allowed: Vec<&str> = envelope.to_vec();
                allowed.push(tag);
                match &def.shape {
                    VariantShape::Unit => self.object(&[], &[], v, path, &allowed),
                    VariantShape::Struct { fields, rules } => {
                        self.object(fields, rules, v, path, &allowed)
                    }
                    VariantShape::Newtype { of } => match self.lookup.find(of) {
                        Some(TypeDef::Struct(s)) => {
                            let (f, r) = (s.fields.clone(), s.rules.clone());
                            self.object(&f, &r, v, path, &allowed)
                        }
                        _ => self.unresolved(of, path),
                    },
                }
            }
        }
    }

    fn dynamic(&mut self, tr: &QualifiedName, v: &Value, path: &str) {
        match self.lookup.implementor(tr, v) {
            Ok((ty, member)) => {
                // The type member names the type; the rest is the type's value.
                let mut inner = v.clone();
                if let Some(o) = inner.as_object_mut() {
                    o.remove(&member);
                }
                self.named(&ty, &inner, path, false);
            }
            Err(msg) => self.error("unresolved_type", path, msg),
        }
    }
}

/// Validates a value's JSON form against a field type, as a field's default
/// is checked; `null` is valid for `option()` only.
pub fn validate_field(value: &Value, ty: &FieldType, lookup: &dyn TypeLookup) -> Vec<ValueError> {
    let mut v = Validator {
        lookup,
        errors: Vec::new(),
        patterns: HashMap::new(),
        rules: HashMap::new(),
    };
    v.value(ty, value, "");
    v.errors
}

/// Validates a value's JSON form against the named type and returns every
/// error; an empty vector means the value is valid.
pub fn validate(value: &Value, ty: &QualifiedName, lookup: &dyn TypeLookup) -> Vec<ValueError> {
    let mut v = Validator {
        lookup,
        errors: Vec::new(),
        patterns: HashMap::new(),
        rules: HashMap::new(),
    };
    v.named(ty, value, "", true);
    v.errors
}
