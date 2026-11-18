//! The canonical JSON form of a value, and its content hash.
//!
//! [`canonical_text`] writes any JSON value in the RFC 8785 canonical form:
//! no whitespace, object members sorted by the UTF-16 code units of their
//! names, numbers in ECMAScript's `Number.prototype.toString` form, and
//! strings escaped as `JSON.stringify` escapes them.
//!
//! [`canonical`] first validates a value against its type and normalizes it:
//! the envelope is removed, an absent field with a default takes the default,
//! and every other absent field is written as `null`.
//!
//! [`content_hash`] is the SHA-256 of the canonical text of
//! `{"type": <qualified name>, "declaration": <declaration hash>, "value":
//! <normalized value>}`, written `sha256:<hex>`. In the hashed value, each
//! matrix, array, sparse matrix, and table is replaced by
//! `{"$digest": "sha256:<hex>"}`, the SHA-256 of its canonical binary form,
//! so the same data hashes the same inline and in an external Arrow file.
//!
//! # Canonical binary form, version 1
//!
//! Integers are little-endian. A string is its UTF-8 byte length as `u64`
//! followed by its bytes. Element types and their values:
//!
//! | Tag | Type | Value |
//! |---|---|---|
//! | 1 | logical | `u8`, 0 or 1 |
//! | 2 | integer | `i32` |
//! | 3 | double | `f64` bits; `-0` as `+0`, every `NaN` as `0x7FF8000000000000` |
//! | 4 | character | string |
//! | 5 | date | `i32` days since 1970-01-01 |
//! | 6 | date-time | `i64` microseconds since 1970-01-01T00:00:00Z |
//! | 7 | factor | the levels (`u64` count, then each string), then `i32` 1-based codes |
//!
//! A validity bitmap of `ceil(n / 8)` bytes, least significant bit first,
//! marks present values with 1; a missing value is written as zeros of its
//! width (a string of length 0, a code of 0).
//!
//! - Dense matrix or array: `"R8M1"`, the element tag (`u8`), the number of
//!   dimensions (`u32`), each dimension (`u64`), the bitmap, the values in
//!   column-major order, then per dimension a `u8` (0 without names, 1 with)
//!   followed by its names.
//! - Sparse matrix: `"R8S1"`, the element tag, `nrow` and `ncol` (`u64`),
//!   the number of values (`u64`), `indptr` and `indices` (`u64` each), and
//!   the values.
//! - Table: `"R8T1"`, the number of rows (`u64`), the number of columns
//!   (`u32`), then each column in the byte order of its UTF-8 name: the name,
//!   the element tag, the bitmap, and the values. A factor column writes its
//!   levels between the tag and the bitmap.

use serde_json::{Map, Value, json};
use sha2::{Digest, Sha256};

use crate::ir::{FieldDef, FieldType, TypeDef, VariantShape};
use crate::name::QualifiedName;
use crate::rules::TypeLookup;
use crate::validate::{ValueError, validate};

// ---------------------------------------------------------------- RFC 8785

/// A number in ECMAScript's `Number.prototype.toString` form.
fn number_text(x: f64) -> String {
    if x == 0.0 {
        return "0".to_string();
    }
    let mut buf = ryu_js::Buffer::new();
    buf.format(x).to_string()
}

fn string_text(s: &str, out: &mut String) {
    out.push('"');
    for c in s.chars() {
        match c {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\u{8}' => out.push_str("\\b"),
            '\u{c}' => out.push_str("\\f"),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            c if (c as u32) < 0x20 => out.push_str(&format!("\\u{:04x}", c as u32)),
            c => out.push(c),
        }
    }
    out.push('"');
}

fn write_canonical(v: &Value, out: &mut String) {
    match v {
        Value::Null => out.push_str("null"),
        Value::Bool(b) => out.push_str(if *b { "true" } else { "false" }),
        Value::Number(n) => out.push_str(&number_text(n.as_f64().unwrap_or(f64::NAN))),
        Value::String(s) => string_text(s, out),
        Value::Array(items) => {
            out.push('[');
            for (i, x) in items.iter().enumerate() {
                if i > 0 {
                    out.push(',');
                }
                write_canonical(x, out);
            }
            out.push(']');
        }
        Value::Object(m) => {
            let mut keys: Vec<&String> = m.keys().collect();
            keys.sort_by(|a, b| a.encode_utf16().cmp(b.encode_utf16()));
            out.push('{');
            for (i, k) in keys.into_iter().enumerate() {
                if i > 0 {
                    out.push(',');
                }
                string_text(k, out);
                out.push(':');
                write_canonical(&m[k], out);
            }
            out.push('}');
        }
    }
}

/// Writes a JSON value in the RFC 8785 canonical form.
pub fn canonical_text(v: &Value) -> String {
    let mut out = String::new();
    write_canonical(v, &mut out);
    out
}

fn sha256_hex(bytes: &[u8]) -> String {
    let d = Sha256::digest(bytes);
    d.iter().map(|b| format!("{b:02x}")).collect()
}

// ------------------------------------------------------------ normalization

struct Normalizer<'a> {
    lookup: &'a dyn TypeLookup,
    /// Replace matrices, arrays, sparse matrices, and tables with digests.
    digest: bool,
    /// The named types the value contains, with their versions.
    contained: std::cell::RefCell<std::collections::BTreeMap<String, u32>>,
}

impl<'a> Normalizer<'a> {
    fn new(lookup: &'a dyn TypeLookup, digest: bool) -> Self {
        Normalizer {
            lookup,
            digest,
            contained: Default::default(),
        }
    }
}

impl Normalizer<'_> {
    fn value(&self, ft: &FieldType, v: &Value) -> Value {
        match ft {
            _ if v.is_null() => Value::Null,
            FieldType::Option { inner, .. } => self.value(inner, v),
            FieldType::Matrix { element, .. } | FieldType::Array { element, .. } if self.digest => {
                digest(&dense_binary(element, v))
            }
            FieldType::MatrixOf { of } if self.digest => {
                let element = self.struct_fields(of).and_then(|f| f.first().map(|f| f.ty.clone())).unwrap_or(FieldType::Logical);
                digest(&dense_binary(&element, v))
            }
            FieldType::SparseMatrix { element, .. } if self.digest => digest(&sparse_binary(element, v)),
            FieldType::Table { columns, .. } if self.digest => digest(&table_binary(columns, v)),
            FieldType::TableOf { of } if self.digest => {
                digest(&table_binary(&self.struct_fields(of).unwrap_or_default(), v))
            }
            FieldType::Vec { element, .. } => {
                Value::Array(v.as_array().map(|a| a.iter().map(|x| self.value(element, x)).collect()).unwrap_or_default())
            }
            FieldType::Hashmap { value, .. } => Value::Object(
                v.as_object().map(|m| m.iter().map(|(k, x)| (k.clone(), self.value(value, x))).collect()).unwrap_or_default(),
            ),
            FieldType::OrderedMap { value, .. } => Value::Array(
                v.as_array()
                    .map(|a| a.iter().map(|p| json!({"name": p["name"], "value": self.value(value, &p["value"])})).collect())
                    .unwrap_or_default(),
            ),
            FieldType::Named { name } => self.named(name, v),
            FieldType::Dyn { r#trait } => match self.lookup.implementor(r#trait, v) {
                Ok((ty, member)) => {
                    let mut out = self.named(&ty, v);
                    if let (Value::Object(o), Some(tag)) = (&mut out, v.get(&member)) {
                        o.insert(member, tag.clone());
                    }
                    out
                }
                Err(_) => v.clone(),
            },
            _ => v.clone(),
        }
    }

    fn struct_fields(&self, name: &QualifiedName) -> Option<Vec<FieldDef>> {
        match self.lookup.find(name)? {
            TypeDef::Struct(s) => Some(s.fields.clone()),
            TypeDef::Enum(_) => None,
        }
    }

    /// The declared fields of an object, in declaration order; an absent
    /// field takes its default, or `null`.
    fn fields(&self, fields: &[FieldDef], v: &Value) -> Map<String, Value> {
        let mut out = Map::new();
        for f in fields {
            let x = match v.get(f.name.as_str()) {
                None | Some(Value::Null) => f
                    .default
                    .as_ref()
                    .map(|d| self.value(&f.ty, d))
                    .unwrap_or(Value::Null),
                Some(x) => self.value(&f.ty, x),
            };
            out.insert(f.name.to_string(), x);
        }
        out
    }

    fn named(&self, name: &QualifiedName, v: &Value) -> Value {
        if let Some(def) = self.lookup.find(name) {
            let version = match def {
                TypeDef::Struct(s) => s.version,
                TypeDef::Enum(e) => e.version,
            };
            self.contained
                .borrow_mut()
                .insert(name.to_string(), version);
        }
        match self.lookup.find(name) {
            Some(TypeDef::Struct(s)) => Value::Object(self.fields(&s.fields, v)),
            Some(TypeDef::Enum(e)) => {
                if e.variants
                    .iter()
                    .all(|x| matches!(x.shape, VariantShape::Unit))
                {
                    return v.clone();
                }
                let tag = e.tag.as_str();
                let variant = v.get(tag).and_then(Value::as_str).unwrap_or_default();
                let mut out = match e
                    .variants
                    .iter()
                    .find(|x| x.name.as_str() == variant)
                    .map(|x| &x.shape)
                {
                    Some(VariantShape::Struct { fields, .. }) => self.fields(fields, v),
                    // The wrapped struct is a type the value contains.
                    Some(VariantShape::Newtype { of }) => match self.lookup.find(of) {
                        Some(TypeDef::Struct(s)) => {
                            self.contained
                                .borrow_mut()
                                .insert(of.to_string(), s.version);
                            self.fields(&s.fields, v)
                        }
                        _ => Map::new(),
                    },
                    _ => Map::new(),
                };
                out.insert(tag.to_string(), Value::from(variant));
                Value::Object(out)
            }
            None => v.clone(),
        }
    }
}

fn digest(bytes: &[u8]) -> Value {
    json!({"$digest": format!("sha256:{}", sha256_hex(bytes))})
}

// ------------------------------------------------------------ binary forms

/// Days from 1970-01-01 to a proleptic Gregorian date (Hinnant's
/// `days_from_civil`).
fn days_from_civil(y: i64, m: i64, d: i64) -> i64 {
    let y = if m <= 2 { y - 1 } else { y };
    let era = y.div_euclid(400);
    let yoe = y - era * 400;
    let mp = (m + 9) % 12;
    let doy = (153 * mp + 2) / 5 + d - 1;
    let doe = yoe * 365 + yoe / 4 - yoe / 100 + doy;
    era * 146_097 + doe - 719_468
}

fn date_days(s: &str) -> i64 {
    let n = |r: std::ops::Range<usize>| s.get(r).and_then(|x| x.parse::<i64>().ok()).unwrap_or(0);
    days_from_civil(n(0..4), n(5..7), n(8..10))
}

fn datetime_micros(s: &str) -> i64 {
    let n = |r: std::ops::Range<usize>| s.get(r).and_then(|x| x.parse::<i64>().ok()).unwrap_or(0);
    let secs = date_days(s) * 86_400 + n(11..13) * 3600 + n(14..16) * 60 + n(17..19);
    secs * 1_000_000 + n(20..26)
}

fn element_tag(ft: &FieldType) -> (u8, &FieldType) {
    let inner = match ft {
        FieldType::Option { inner, .. } => inner.as_ref(),
        other => other,
    };
    let tag = match inner {
        FieldType::Logical => 1,
        FieldType::Integer(_) => 2,
        FieldType::Double(_) => 3,
        FieldType::Character(_) => 4,
        FieldType::Date => 5,
        FieldType::Posixct => 6,
        FieldType::Factor { .. } => 7,
        _ => 0,
    };
    (tag, inner)
}

fn put_str(out: &mut Vec<u8>, s: &str) {
    out.extend_from_slice(&(s.len() as u64).to_le_bytes());
    out.extend_from_slice(s.as_bytes());
}

fn put_value(out: &mut Vec<u8>, tag: u8, v: &Value) {
    let present = !v.is_null();
    match tag {
        1 => out.push(u8::from(present && v.as_bool() == Some(true))),
        2 => out.extend_from_slice(
            &(if present {
                v.as_f64().unwrap_or(0.0) as i32
            } else {
                0
            })
            .to_le_bytes(),
        ),
        3 => {
            let x = if present {
                crate::rules::number_value(v).unwrap_or(0.0)
            } else {
                0.0
            };
            let bits = if x.is_nan() {
                0x7FF8_0000_0000_0000
            } else if x == 0.0 {
                0
            } else {
                x.to_bits()
            };
            out.extend_from_slice(&bits.to_le_bytes());
        }
        4 => put_str(
            out,
            if present {
                v.as_str().unwrap_or_default()
            } else {
                ""
            },
        ),
        5 => out.extend_from_slice(
            &(if present {
                date_days(v.as_str().unwrap_or_default()) as i32
            } else {
                0
            })
            .to_le_bytes(),
        ),
        6 => out.extend_from_slice(
            &(if present {
                datetime_micros(v.as_str().unwrap_or_default())
            } else {
                0
            })
            .to_le_bytes(),
        ),
        _ => {}
    }
}

fn put_bitmap(out: &mut Vec<u8>, items: &[Value]) {
    let mut bytes = vec![0u8; items.len().div_ceil(8)];
    for (i, x) in items.iter().enumerate() {
        if !x.is_null() {
            bytes[i / 8] |= 1 << (i % 8);
        }
    }
    out.extend_from_slice(&bytes);
}

/// The canonical binary form of a dense matrix or array.
pub fn dense_binary(element: &FieldType, v: &Value) -> Vec<u8> {
    let (tag, _) = element_tag(element);
    let shape: Vec<u64> = v["shape"]
        .as_array()
        .map(|a| a.iter().filter_map(Value::as_u64).collect())
        .unwrap_or_default();
    let data = v["data"].as_array().cloned().unwrap_or_default();
    let mut out = b"R8M1".to_vec();
    out.push(tag);
    out.extend_from_slice(&(shape.len() as u32).to_le_bytes());
    for d in &shape {
        out.extend_from_slice(&d.to_le_bytes());
    }
    put_bitmap(&mut out, &data);
    for x in &data {
        put_value(&mut out, tag, x);
    }
    let dimnames = v.get("dimnames").and_then(Value::as_array);
    for i in 0..shape.len() {
        match dimnames.and_then(|d| d.get(i)).and_then(Value::as_array) {
            Some(names) => {
                out.push(1);
                for n in names {
                    put_str(&mut out, n.as_str().unwrap_or_default());
                }
            }
            None => out.push(0),
        }
    }
    out
}

/// The canonical binary form of a sparse matrix.
pub fn sparse_binary(element: &FieldType, v: &Value) -> Vec<u8> {
    let (tag, _) = element_tag(element);
    let ints = |k: &str| -> Vec<u64> {
        v[k].as_array()
            .map(|a| a.iter().filter_map(Value::as_u64).collect())
            .unwrap_or_default()
    };
    let shape = ints("shape");
    let data = v["data"].as_array().cloned().unwrap_or_default();
    let mut out = b"R8S1".to_vec();
    out.push(tag);
    for d in shape.iter().take(2) {
        out.extend_from_slice(&d.to_le_bytes());
    }
    out.extend_from_slice(&(data.len() as u64).to_le_bytes());
    for k in ["indptr", "indices"] {
        for x in ints(k) {
            out.extend_from_slice(&x.to_le_bytes());
        }
    }
    for x in &data {
        put_value(&mut out, tag, x);
    }
    out
}

/// The canonical binary form of a table, with `columns` the declared columns
/// and the extra columns' types read from `"$columns"`.
pub fn table_binary(columns: &[FieldDef], v: &Value) -> Vec<u8> {
    let m = v.as_object().cloned().unwrap_or_default();
    let n = m.get("$length").and_then(Value::as_u64).unwrap_or(0);
    let extra = m
        .get("$columns")
        .and_then(Value::as_object)
        .cloned()
        .unwrap_or_default();
    let mut names: Vec<&String> = m.keys().filter(|k| !k.starts_with('$')).collect();
    names.sort_by(|a, b| a.as_bytes().cmp(b.as_bytes()));
    let mut out = b"R8T1".to_vec();
    out.extend_from_slice(&n.to_le_bytes());
    out.extend_from_slice(&(names.len() as u32).to_le_bytes());
    for name in names {
        let ft = columns
            .iter()
            .find(|c| c.name.as_str() == name.as_str())
            .map(|c| c.ty.clone())
            .or_else(|| {
                extra
                    .get(name.as_str())
                    .and_then(|t| serde_json::from_value(t.clone()).ok())
            })
            .unwrap_or(FieldType::Json);
        let (tag, inner) = element_tag(&ft);
        put_str(&mut out, name);
        out.push(tag);
        let cells = &m[name.as_str()];
        if tag == 7 {
            let declared = match inner {
                FieldType::Factor { levels, .. } => levels.clone(),
                _ => None,
            };
            let (levels, codes): (Vec<String>, Vec<Value>) = match (cells, declared) {
                (Value::Object(o), _) => (
                    o.get("levels")
                        .and_then(Value::as_array)
                        .map(|a| {
                            a.iter()
                                .filter_map(|x| x.as_str().map(str::to_string))
                                .collect()
                        })
                        .unwrap_or_default(),
                    o.get("codes")
                        .and_then(Value::as_array)
                        .cloned()
                        .unwrap_or_default(),
                ),
                (Value::Array(a), Some(levels)) => {
                    let codes = a
                        .iter()
                        .map(|x| {
                            match x.as_str().and_then(|s| levels.iter().position(|l| l == s)) {
                                Some(i) => Value::from(i + 1),
                                None => Value::Null,
                            }
                        })
                        .collect();
                    (levels, codes)
                }
                _ => (Vec::new(), Vec::new()),
            };
            out.extend_from_slice(&(levels.len() as u64).to_le_bytes());
            for l in &levels {
                put_str(&mut out, l);
            }
            put_bitmap(&mut out, &codes);
            for c in &codes {
                out.extend_from_slice(&(c.as_i64().unwrap_or(0) as i32).to_le_bytes());
            }
            continue;
        }
        let items = cells.as_array().cloned().unwrap_or_default();
        put_bitmap(&mut out, &items);
        for x in &items {
            put_value(&mut out, tag, x);
        }
    }
    out
}

// -------------------------------------------------------------- public API

/// The normalized JSON form of a valid value: the envelope removed, absent
/// fields given their defaults or `null`.
pub fn normalize(
    value: &Value,
    ty: &QualifiedName,
    lookup: &dyn TypeLookup,
) -> Result<Value, Vec<ValueError>> {
    let errors = validate(value, ty, lookup);
    if !errors.is_empty() {
        return Err(errors);
    }
    Ok(Normalizer::new(lookup, false).named(ty, value))
}

/// The RFC 8785 canonical text of a valid value's normalized form.
pub fn canonical(
    value: &Value,
    ty: &QualifiedName,
    lookup: &dyn TypeLookup,
) -> Result<String, Vec<ValueError>> {
    normalize(value, ty, lookup).map(|v| canonical_text(&v))
}

/// Collects the named types `ty` reaches through its fields, variants, and
/// type arguments, `ty` included.
fn reachable(
    ty: &QualifiedName,
    lookup: &dyn TypeLookup,
    seen: &mut std::collections::BTreeMap<String, Value>,
) {
    let key = ty.to_string();
    if seen.contains_key(&key) {
        return;
    }
    let Some(def) = lookup.find(ty) else {
        seen.insert(key, Value::Null);
        return;
    };
    let mut def = def.clone();
    strip(&mut def);
    seen.insert(
        key,
        serde_json::to_value(&def).expect("a declaration serializes"),
    );
    let mut names = Vec::new();
    match lookup.find(ty).expect("found above") {
        TypeDef::Struct(s) => s.fields.iter().for_each(|f| named_in(&f.ty, &mut names)),
        TypeDef::Enum(e) => {
            for v in &e.variants {
                match &v.shape {
                    VariantShape::Unit => {}
                    VariantShape::Struct { fields, .. } => {
                        fields.iter().for_each(|f| named_in(&f.ty, &mut names))
                    }
                    VariantShape::Newtype { of } => names.push(of.clone()),
                }
            }
        }
    }
    for n in names {
        reachable(&n, lookup, seen);
    }
}

fn named_in(ft: &FieldType, out: &mut Vec<QualifiedName>) {
    match ft {
        FieldType::Named { name } => out.push(name.clone()),
        FieldType::MatrixOf { of } | FieldType::TableOf { of } => out.push(of.clone()),
        FieldType::Option { inner, .. } => named_in(inner, out),
        FieldType::Vec { element, .. }
        | FieldType::Matrix { element, .. }
        | FieldType::Array { element, .. }
        | FieldType::SparseMatrix { element, .. } => named_in(element, out),
        FieldType::Hashmap { value, .. } | FieldType::OrderedMap { value, .. } => {
            named_in(value, out)
        }
        FieldType::Table { columns, .. } => columns.iter().for_each(|c| named_in(&c.ty, out)),
        _ => {}
    }
}

/// Removes what the declaration hash excludes: descriptions and extensions.
fn strip(def: &mut TypeDef) {
    fn fields(fs: &mut [FieldDef]) {
        for f in fs {
            f.description.clear();
            f.extensions.clear();
            if let FieldType::Table { columns, .. } = &mut f.ty {
                fields(columns);
            }
            if let FieldType::Option { inner, .. } = &mut f.ty
                && let FieldType::Table { columns, .. } = inner.as_mut()
            {
                fields(columns);
            }
        }
    }
    match def {
        TypeDef::Struct(s) => {
            s.description.clear();
            s.extensions.clear();
            fields(&mut s.fields);
        }
        TypeDef::Enum(e) => {
            e.description.clear();
            e.extensions.clear();
            for v in &mut e.variants {
                v.description.clear();
                if let VariantShape::Struct { fields: fs, .. } = &mut v.shape {
                    fields(fs);
                }
            }
        }
    }
}

/// The declaration hash of a type: the SHA-256 of the canonical text of an
/// object mapping the qualified name of the type and of every named type it
/// reaches to its declaration, with descriptions emptied and extensions removed. A
/// reached type outside the lookup maps to `null`.
pub fn declaration_hash(ty: &QualifiedName, lookup: &dyn TypeLookup) -> String {
    let mut seen = std::collections::BTreeMap::new();
    reachable(ty, lookup, &mut seen);
    let obj = Value::Object(seen.into_iter().collect());
    format!("sha256:{}", sha256_hex(canonical_text(&obj).as_bytes()))
}

/// The content hash of a valid value, `sha256:<hex>`.
pub fn content_hash(
    value: &Value,
    ty: &QualifiedName,
    lookup: &dyn TypeLookup,
) -> Result<String, Vec<ValueError>> {
    let errors = validate(value, ty, lookup);
    if !errors.is_empty() {
        return Err(errors);
    }
    let hashed = Normalizer::new(lookup, true).named(ty, value);
    let obj = json!({"type": ty.to_string(), "declaration": declaration_hash(ty, lookup), "value": hashed});
    Ok(format!(
        "sha256:{}",
        sha256_hex(canonical_text(&obj).as_bytes())
    ))
}

// --------------------------------------------------------------- documents

/// Writes a valid value as a document: the envelope first, `"$type"` and
/// `"$types"` (the version of every named type the value contains), then the
/// fields in declaration order, every absent field as `null`.
pub fn to_document(
    value: &Value,
    ty: &QualifiedName,
    lookup: &dyn TypeLookup,
) -> Result<String, Vec<ValueError>> {
    let errors = validate(value, ty, lookup);
    if !errors.is_empty() {
        return Err(errors);
    }
    let n = Normalizer::new(lookup, false);
    let body = n.named(ty, value);
    let mut doc = Map::new();
    doc.insert("$type".into(), Value::from(ty.to_string()));
    doc.insert(
        "$types".into(),
        Value::Object(
            n.contained
                .into_inner()
                .into_iter()
                .map(|(k, v)| (k, Value::from(v)))
                .collect(),
        ),
    );
    match body {
        Value::Object(fields) => doc.extend(fields),
        // An enum value of unit variants is a string; the document holds it
        // under `"$value"`.
        other => {
            doc.insert("$value".into(), other);
        }
    }
    Ok(serde_json::to_string(&Value::Object(doc)).expect("a document serializes"))
}

fn document_error(code: &'static str, path: &str, message: String) -> ValueError {
    ValueError {
        code,
        path: path.to_string(),
        rule: None,
        message,
    }
}

/// Reads a document of type `ty`: checks the envelope, validates the value,
/// and returns its normalized form. `"$type"` names `ty`, and `"$types"`
/// holds the current version of every named type the value contains; a
/// lower version needs a migration, which M6 adds.
pub fn read_document(
    text: &str,
    ty: &QualifiedName,
    lookup: &dyn TypeLookup,
) -> Result<Value, Vec<ValueError>> {
    let doc: Value = serde_json::from_str(text).map_err(|e| {
        vec![document_error(
            "document_json",
            "",
            format!("not a JSON document: {e}"),
        )]
    })?;
    let Some(m) = doc.as_object() else {
        return Err(vec![document_error(
            "document_form",
            "",
            "a document is a JSON object".to_string(),
        )]);
    };
    let mut errors = Vec::new();
    match m.get("$type").and_then(Value::as_str) {
        Some(t) if t == ty.to_string() => {}
        Some(t) => errors.push(document_error(
            "document_type",
            "/$type",
            format!("the document holds a `{t}`, not a `{ty}`"),
        )),
        None => errors.push(document_error(
            "document_form",
            "/$type",
            "a document names its type in `$type`".to_string(),
        )),
    }
    let Some(versions) = m.get("$types").and_then(Value::as_object) else {
        errors.push(document_error(
            "document_form",
            "/$types",
            "a document records its types' versions in `$types`".to_string(),
        ));
        return Err(errors);
    };
    let value = match m.get("$value") {
        Some(v) => v.clone(),
        None => doc.clone(),
    };
    let n = Normalizer::new(lookup, false);
    let validation = validate(&value, ty, lookup);
    if !validation.is_empty() {
        errors.extend(validation);
        return Err(errors);
    }
    let normalized = n.named(ty, &value);
    for (name, current) in n.contained.into_inner() {
        let path = format!("/$types/{}", name.replace('~', "~0").replace('/', "~1"));
        match versions.get(&name).and_then(Value::as_u64) {
            None => errors.push(document_error("version_missing", &path, format!("`$types` does not record the version of `{name}`"))),
            Some(v) if v == u64::from(current) => {}
            Some(v) if v < u64::from(current) => errors.push(document_error(
                "version_older",
                &path,
                format!("`{name}` is version {v} in the document and {current} in its declaration; reading it needs a migration"),
            )),
            Some(v) => errors.push(document_error(
                "version_newer",
                &path,
                format!("`{name}` is version {v} in the document, newer than its declaration, {current}; update the package that declares it"),
            )),
        }
    }
    if errors.is_empty() {
        Ok(normalized)
    } else {
        Err(errors)
    }
}
