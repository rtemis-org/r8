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

use serde_json::Value;
use sha2::{Digest, Sha256};

use crate::access::{Access, Json};
use crate::emit::{self, Emitter, Writer};

pub use crate::emit::runtime_paths;

use crate::ir::{FieldDef, FieldType, TypeDef, VariantShape};
use crate::name::QualifiedName;
use crate::rules::TypeLookup;
use crate::validate::{ValueError, validate};

// ---------------------------------------------------------------- RFC 8785

/// Writes a JSON value in the RFC 8785 canonical form.
pub fn canonical_text(v: &Value) -> String {
    let mut w = Writer::new(String::new(), true);
    w.value(v);
    w.out
}

fn sha256_hex(bytes: &[u8]) -> String {
    let d = Sha256::digest(bytes);
    d.iter().map(|b| format!("{b:02x}")).collect()
}

fn hex(d: impl AsRef<[u8]>) -> String {
    d.as_ref().iter().map(|b| format!("{b:02x}")).collect()
}

fn days_from_civil(y: i64, m: i64, d: i64) -> i64 {
    let y = if m <= 2 { y - 1 } else { y };
    let era = y.div_euclid(400);
    let yoe = y - era * 400;
    let mp = (m + 9) % 12;
    let doy = (153 * mp + 2) / 5 + d - 1;
    let doe = yoe * 365 + yoe / 4 - yoe / 100 + doy;
    era * 146_097 + doe - 719_468
}

/// The proleptic Gregorian date of a day count from 1970-01-01, as
/// `(year, month, day)` (Hinnant's `civil_from_days`).
pub fn civil_from_days(z: i64) -> (i64, u32, u32) {
    let z = z + 719_468;
    let era = z.div_euclid(146_097);
    let doe = z - era * 146_097;
    let yoe = (doe - doe / 1460 + doe / 36_524 - doe / 146_096) / 365;
    let y = yoe + era * 400;
    let doy = doe - (365 * yoe + yoe / 4 - yoe / 100);
    let mp = (5 * doy + 2) / 153;
    let d = (doy - (153 * mp + 2) / 5 + 1) as u32;
    let m = if mp < 10 { mp + 3 } else { mp - 9 } as u32;
    (if m <= 2 { y + 1 } else { y }, m, d)
}

/// The canonical text of a date, `YYYY-MM-DD`, from its day count since
/// 1970-01-01.
pub fn date_text(days: i64) -> String {
    let (y, m, d) = civil_from_days(days);
    format!("{y:04}-{m:02}-{d:02}")
}

/// The canonical text of a date-time, `YYYY-MM-DDTHH:MM:SS.ffffffZ`, from its
/// microsecond count since 1970-01-01T00:00:00Z.
pub fn datetime_text(micros: i64) -> String {
    let secs = micros.div_euclid(1_000_000);
    let frac = micros.rem_euclid(1_000_000);
    let days = secs.div_euclid(86_400);
    let tod = secs.rem_euclid(86_400);
    format!(
        "{}T{:02}:{:02}:{:02}.{frac:06}Z",
        date_text(days),
        tod / 3600,
        (tod / 60) % 60,
        tod % 60
    )
}

/// The day count since 1970-01-01 of a canonical date text.
pub fn date_days(s: &str) -> i64 {
    let n = |r: std::ops::Range<usize>| s.get(r).and_then(|x| x.parse::<i64>().ok()).unwrap_or(0);
    days_from_civil(n(0..4), n(5..7), n(8..10))
}

/// The microsecond count since 1970-01-01T00:00:00Z of a canonical date-time
/// text.
pub fn datetime_micros(s: &str) -> i64 {
    let n = |r: std::ops::Range<usize>| s.get(r).and_then(|x| x.parse::<i64>().ok()).unwrap_or(0);
    let secs = date_days(s) * 86_400 + n(11..13) * 3600 + n(14..16) * 60 + n(17..19);
    secs * 1_000_000 + n(20..26)
}

/// The canonical binary form of a dense matrix or array, from its JSON form.
pub fn dense_binary(element: &FieldType, v: &Value) -> Vec<u8> {
    let mut out = Vec::new();
    emit::dense_binary_access(element, Json::new(v), &mut out);
    out
}

/// The canonical binary form of a sparse matrix, from its JSON form.
pub fn sparse_binary(element: &FieldType, v: &Value) -> Vec<u8> {
    let mut out = Vec::new();
    emit::sparse_binary_access(element, Json::new(v), &mut out);
    out
}

/// The canonical binary form of a table, from its JSON form, with `columns`
/// the declared columns and the extra columns' types read from
/// `"$columns"`.
pub fn table_binary(columns: &[FieldDef], v: &Value) -> Vec<u8> {
    let mut out = Vec::new();
    let open = v.get("$columns").is_some();
    emit::table_binary_access(columns, open, Json::new(v), &mut out);
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
    let mut e = Emitter::new(lookup, String::new(), false, false);
    e.named(ty, Json::new(value), None);
    Ok(serde_json::from_str(&e.w.out).expect("the emitter writes JSON"))
}

/// The RFC 8785 canonical text of a valid value's normalized form.
pub fn canonical(
    value: &Value,
    ty: &QualifiedName,
    lookup: &dyn TypeLookup,
) -> Result<String, Vec<ValueError>> {
    let errors = validate(value, ty, lookup);
    if !errors.is_empty() {
        return Err(errors);
    }
    Ok(canonical_access(Json::new(value), ty, lookup))
}

/// The RFC 8785 canonical text of the normalized form of a valid value in
/// any host form.
pub fn canonical_access<'a, A: Access<'a>>(
    value: A,
    ty: &QualifiedName,
    lookup: &dyn TypeLookup,
) -> String {
    let mut e = Emitter::new(lookup, String::new(), true, false);
    e.named(ty, value, None);
    e.w.out
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
            f.description = Box::default();
            f.extensions = None;
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
    content_hash_with_runtime(value, ty, lookup, &[])
}

/// The content hash of a valid value whose runtime-only fields at the JSON
/// Pointers `runtime` hold values in the host. Their values have no JSON form
/// and stay out of the hash; their presence enters it as `"runtime"`, the
/// sorted paths, a member the hashed object holds only when `runtime` is not
/// empty.
pub fn content_hash_with_runtime(
    value: &Value,
    ty: &QualifiedName,
    lookup: &dyn TypeLookup,
    runtime: &[String],
) -> Result<String, Vec<ValueError>> {
    let errors = validate(value, ty, lookup);
    if !errors.is_empty() {
        return Err(errors);
    }
    Ok(hash_with(Json::new(value), ty, lookup, runtime.to_vec()))
}

/// The content hash of a valid value in any host form, streamed into
/// SHA-256 without building its text: the hashed object's members in RFC
/// 8785 order are `declaration`, `runtime` (the JSON Pointers of the
/// runtime-only fields that hold a value, when there are any), `type`, and
/// `value`.
pub fn content_hash_access<'a, A: Access<'a>>(
    value: A,
    ty: &QualifiedName,
    lookup: &dyn TypeLookup,
) -> String {
    hash_with(value, ty, lookup, Vec::new())
}

fn hash_with<'a, A: Access<'a>>(
    value: A,
    ty: &QualifiedName,
    lookup: &dyn TypeLookup,
    runtime: Vec<String>,
) -> String {
    let mut paths = runtime;
    paths.extend(emit::runtime_paths(ty, value, lookup));
    paths.sort();
    paths.dedup();
    let mut w = Writer::new(Sha256::new(), true);
    w.begin_object();
    w.key("declaration");
    w.string(&declaration_hash(ty, lookup));
    if !paths.is_empty() {
        w.key("runtime");
        w.begin_array();
        for p in &paths {
            w.string(p);
        }
        w.end_array();
    }
    w.key("type");
    w.string(&ty.to_string());
    w.key("value");
    let mut e = Emitter::new(lookup, w.out, true, true);
    e.named(ty, value, None);
    let mut h = e.w.out;
    h.update(b"}");
    format!("sha256:{}", hex(h.finalize()))
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
    Ok(document_access(Json::new(value), ty, lookup))
}

/// Writes a valid value in any host form as a document, streamed.
pub fn document_access<'a, A: Access<'a>>(
    value: A,
    ty: &QualifiedName,
    lookup: &dyn TypeLookup,
) -> String {
    // The body first, which collects the contained types.
    let mut e = Emitter::new(lookup, String::new(), false, false);
    e.named(ty, value, None);
    let body = e.w.out;
    let mut w = Writer::new(String::new(), false);
    w.begin_object();
    w.key("$type");
    w.string(&ty.to_string());
    w.key("$types");
    w.begin_object();
    for (k, v) in &e.contained {
        w.key(k);
        w.number(f64::from(*v));
    }
    w.end_object();
    let mut out = w.out;
    if let Some(fields) = body.strip_prefix('{').and_then(|b| b.strip_suffix('}')) {
        if !fields.is_empty() {
            out.push(',');
            out.push_str(fields);
        }
    } else {
        // An enum value of unit variants is a string; the document holds it
        // under `"$value"`.
        out.push_str(",\"$value\":");
        out.push_str(&body);
    }
    out.push('}');
    out
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
    let mut doc: Value = serde_json::from_str(text).map_err(|e| {
        vec![document_error(
            "document_json",
            "",
            format!("not a JSON document: {e}"),
        )]
    })?;
    let Some(m) = doc.as_object_mut() else {
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
    let Some(versions) = m.get("$types").and_then(Value::as_object).cloned() else {
        errors.push(document_error(
            "document_form",
            "/$types",
            "a document records its types' versions in `$types`".to_string(),
        ));
        return Err(errors);
    };
    // The value moves out of the document: `$value`, or the document itself.
    let value = match m.remove("$value") {
        Some(v) => v,
        None => doc,
    };
    let validation = validate(&value, ty, lookup);
    if !validation.is_empty() {
        errors.extend(validation);
        return Err(errors);
    }
    let mut e = Emitter::new(lookup, String::new(), false, false);
    e.named(ty, Json::new(&value), None);
    // One tree at a time: the source is freed before the normalized text is
    // read.
    drop(value);
    let normalized: Value = serde_json::from_str(&e.w.out).expect("the emitter writes JSON");
    drop(std::mem::take(&mut e.w.out));
    for (name, current) in e.contained {
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
