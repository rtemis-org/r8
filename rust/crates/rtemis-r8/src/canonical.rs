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
use crate::emit::{self, Emitter, SameSink, Writer};

pub use crate::emit::host_only_paths;

use crate::ir::{FieldDef, FieldType, TypeDef, VariantKind};
use crate::name::QualifiedName;
use crate::rules::TypeLookup;
use crate::tree::{Tree, TreeRef};
use crate::validate::{ValueError, validate};

// ---------------------------------------------------------------- RFC 8785

/// Writes a JSON value in the RFC 8785 canonical form.
pub fn canonical_text(v: &Value) -> String {
    let mut w = Writer::new(String::new(), true);
    w.value(v);
    w.out
}

/// The SHA-256 of `bytes`.
pub fn sha256(bytes: &[u8]) -> [u8; 32] {
    Sha256::digest(bytes).into()
}

fn sha256_hex(bytes: &[u8]) -> String {
    let d = Sha256::digest(bytes);
    d.iter().map(|b| format!("{b:02x}")).collect()
}

/// The SHA-256 of the canonical text of `v`, `sha256:<hex>`.
pub(crate) fn text_hash(v: &Value) -> String {
    format!("sha256:{}", sha256_hex(canonical_text(v).as_bytes()))
}

fn hex(d: impl AsRef<[u8]>) -> String {
    d.as_ref().iter().map(|b| format!("{b:02x}")).collect()
}

/// The day count since 1970-01-01 of a proleptic Gregorian date (Hinnant's
/// `days_from_civil`).
pub fn days_from_civil(y: i64, m: i64, d: i64) -> i64 {
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
    strip: fn(&mut TypeDef),
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
                match &v.kind {
                    VariantKind::Unit => {}
                    VariantKind::Struct { fields, .. } => {
                        fields.iter().for_each(|f| named_in(&f.ty, &mut names))
                    }
                    VariantKind::Newtype { of } => names.push(of.clone()),
                }
            }
        }
        TypeDef::Table(_) => {}
    }
    for n in names {
        reachable(&n, lookup, strip, seen);
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
        FieldType::BtreeMap { value, .. } | FieldType::IndexMap { value, .. } => {
            named_in(value, out)
        }
        FieldType::Table { columns, .. } => columns.iter().for_each(|c| named_in(&c.ty, out)),
        _ => {}
    }
}

/// Removes what the declaration hash excludes: descriptions, extensions,
/// a struct's `extends`, and the `inherited_from` marks of fields and rules,
/// so that the hash covers the flattened declaration.
fn strip(def: &mut TypeDef) {
    fn fields(fs: &mut [FieldDef]) {
        for f in fs {
            f.description = Box::default();
            f.extensions = None;
            f.inherited_from = None;
            if let FieldType::Table { columns, .. } = &mut f.ty {
                columns
                    .iter_mut()
                    .for_each(|c| fields(std::slice::from_mut(&mut c.field)));
            }
            if let FieldType::Option { inner, .. } = &mut f.ty
                && let FieldType::Table { columns, .. } = inner.as_mut()
            {
                columns
                    .iter_mut()
                    .for_each(|c| fields(std::slice::from_mut(&mut c.field)));
            }
        }
    }
    match def {
        TypeDef::Struct(s) => {
            s.description.clear();
            s.extensions.clear();
            s.extends = None;
            fields(&mut s.fields);
            for r in &mut s.rules {
                r.inherited_from = None;
            }
        }
        TypeDef::Enum(e) => {
            e.description.clear();
            e.extensions.clear();
            for v in &mut e.variants {
                v.description.clear();
                if let VariantKind::Struct { fields: fs, .. } = &mut v.kind {
                    fields(fs);
                }
            }
        }
        TypeDef::Table(t) => {
            t.description.clear();
            t.extensions.clear();
            t.columns
                .iter_mut()
                .for_each(|c| fields(std::slice::from_mut(&mut c.field)));
        }
    }
}

/// Whether two declarations of a type are the same: equal without what the
/// declaration hash excludes. The types that they reach are not compared.
pub fn same_declaration(a: &TypeDef, b: &TypeDef) -> bool {
    let stripped = |t: &TypeDef| {
        let mut t = t.clone();
        strip(&mut t);
        canonical_text(&serde_json::to_value(&t).expect("a declaration serializes"))
    };
    a == b || stripped(a) == stripped(b)
}

/// The declaration hash of a type: the SHA-256 of the canonical text of an
/// object mapping the qualified name of the type and of every named type it
/// reaches to its declaration, with descriptions emptied, and extensions,
/// `extends`, and `inherited_from` removed. A reached type outside the
/// lookup maps to `null`.
pub fn declaration_hash(ty: &QualifiedName, lookup: &dyn TypeLookup) -> String {
    text_hash(&hashed_declaration(ty, lookup))
}

/// The text whose SHA-256 is the declaration hash of a type.
pub fn declaration_text(ty: &QualifiedName, lookup: &dyn TypeLookup) -> String {
    canonical_text(&hashed_declaration(ty, lookup))
}

fn hashed_declaration(ty: &QualifiedName, lookup: &dyn TypeLookup) -> Value {
    let mut seen = std::collections::BTreeMap::new();
    reachable(ty, lookup, strip, &mut seen);
    Value::Object(seen.into_iter().collect())
}

/// The hash of what a declaration says about a value: the declaration hash
/// without defaults, `sensitive`, and the IDs and messages of rules, which have
/// no part in a value that holds every field, as a document does.
pub fn value_declaration_hash(ty: &QualifiedName, lookup: &dyn TypeLookup) -> String {
    let mut seen = std::collections::BTreeMap::new();
    reachable(ty, lookup, strip_neutral, &mut seen);
    text_hash(&Value::Object(seen.into_iter().collect()))
}

/// Removes what [`value_declaration_hash`] excludes.
fn strip_neutral(def: &mut TypeDef) {
    fn rules(rs: &mut [crate::ir::RuleDef]) {
        for r in rs {
            r.id = None;
            r.message = None;
        }
    }
    fn ty(t: &mut FieldType) {
        match t {
            FieldType::Table {
                columns, rules: rs, ..
            } => {
                columns
                    .iter_mut()
                    .for_each(|c| fields(std::slice::from_mut(&mut c.field)));
                rules(rs);
            }
            FieldType::Option { inner, .. } => ty(inner),
            _ => {}
        }
    }
    fn fields(fs: &mut [FieldDef]) {
        for f in fs {
            f.default = None;
            f.sensitive = false;
            ty(&mut f.ty);
        }
    }
    strip(def);
    match def {
        TypeDef::Struct(s) => {
            fields(&mut s.fields);
            rules(&mut s.rules);
        }
        TypeDef::Enum(e) => {
            for v in &mut e.variants {
                if let VariantKind::Struct {
                    fields: fs,
                    rules: rs,
                } = &mut v.kind
                {
                    fields(fs);
                    rules(rs);
                }
            }
        }
        TypeDef::Table(t) => {
            t.columns
                .iter_mut()
                .for_each(|c| fields(std::slice::from_mut(&mut c.field)));
            rules(&mut t.rules);
        }
    }
}

/// The content hash of a valid value, `sha256:<hex>`.
pub fn content_hash(
    value: &Value,
    ty: &QualifiedName,
    lookup: &dyn TypeLookup,
) -> Result<String, Vec<ValueError>> {
    content_hash_with_host_only(value, ty, lookup, &[])
}

/// The content hash of a valid value whose host-only fields at the JSON
/// Pointers `host_only` hold values in the host. Their values have no JSON form
/// and stay out of the hash; their presence enters it as `"host_only"`, the
/// sorted paths, a member the hashed object holds only when `host_only` is not
/// empty.
pub fn content_hash_with_host_only(
    value: &Value,
    ty: &QualifiedName,
    lookup: &dyn TypeLookup,
    host_only: &[String],
) -> Result<String, Vec<ValueError>> {
    let errors = validate(value, ty, lookup);
    if !errors.is_empty() {
        return Err(errors);
    }
    Ok(hash_with(Json::new(value), ty, lookup, host_only.to_vec()))
}

/// The content hash of a valid value in any host form, streamed into
/// SHA-256 without building its text: the hashed object's members in RFC
/// 8785 order are `declaration`, `host_only` (the JSON Pointers of the
/// host-only fields that hold a value, when there are any), `type`, and
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
    host_only: Vec<String>,
) -> String {
    let mut paths = host_only;
    paths.extend(emit::host_only_paths(ty, value, lookup));
    paths.sort();
    paths.dedup();
    let mut w = Writer::new(Sha256::new(), true);
    w.begin_object();
    w.key("declaration");
    w.string(&declaration_hash(ty, lookup));
    if !paths.is_empty() {
        w.key("host_only");
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

/// Writes a valid value as a document: the envelope first, `"$type"`,
/// `"$types"` (the version of every named type the value contains), and
/// `"$declarations"` (the [`value_declaration_hash`] of each), then the fields in
/// declaration order, every absent field as `null`.
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
    let hash = |q: &QualifiedName| value_declaration_hash(q, lookup).into();
    document_with(value, ty, lookup, false, &hash)
}

/// The [`value_declaration_hash`] of a registered type, which a host keeps
/// between documents.
pub type HashOf<'a> = &'a dyn Fn(&QualifiedName) -> std::sync::Arc<str>;

/// [`document_access`]; with `declarations`, the document also holds, as
/// `"$ir"`, the IR documents of the types that the value contains and of those
/// that they reach.
pub fn document_with<'a, A: Access<'a>>(
    value: A,
    ty: &QualifiedName,
    lookup: &dyn TypeLookup,
    declarations: bool,
    hash: HashOf<'_>,
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
    let names = || {
        e.contained
            .keys()
            .filter_map(|k| QualifiedName::parse(k).ok())
    };
    w.key("$declarations");
    w.begin_object();
    for q in names() {
        w.key(&q.to_string());
        w.string(&hash(&q));
    }
    w.end_object();
    if declarations {
        w.key("$ir");
        let ir = ir_documents(names(), lookup);
        w.value(&serde_json::to_value(ir).expect("a declaration serializes"));
    }
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

/// The IR documents of `names` and of the types that they reach, one per
/// namespace.
fn ir_documents(
    names: impl Iterator<Item = QualifiedName>,
    lookup: &dyn TypeLookup,
) -> Vec<crate::ir::IrDocument> {
    let mut seen = std::collections::BTreeMap::new();
    for q in names {
        reachable(&q, lookup, strip, &mut seen);
    }
    let mut out: Vec<crate::ir::IrDocument> = Vec::new();
    for name in seen.keys() {
        let Ok(q) = QualifiedName::parse(name) else {
            continue;
        };
        let Some(def) = lookup.find(&q) else { continue };
        match out.iter_mut().find(|d| d.namespace == q.namespace) {
            Some(d) => d.types.push(def.clone()),
            None => out.push(crate::ir::IrDocument {
                ir_version: crate::ir::IR_VERSION,
                namespace: q.namespace.clone(),
                types: vec![def.clone()],
                traits: Vec::new(),
                impls: Vec::new(),
                functions: Vec::new(),
            }),
        }
    }
    out
}

/// How a document is read.
#[derive(Clone, Copy)]
pub struct ReadOptions<'a> {
    /// Reads a document written under other declarations than the registered
    /// ones as one written under these.
    pub any_declaration: bool,
    /// The hashes of the registered declarations.
    pub hash: HashOf<'a>,
}

/// The members of a document's envelope beside `$type`.
struct Envelope<'a> {
    /// The versions of `$types`, by type; `None` for one that is not a whole
    /// number.
    versions: Vec<(&'a str, Option<u64>)>,
    /// The hashes of `$declarations`, by type, when the document holds it.
    declarations: Option<Vec<(&'a str, Option<&'a str>)>>,
    /// The IR documents of `$ir`.
    ir: Option<Value>,
}

fn pointer_token(name: &str) -> String {
    name.replace('~', "~0").replace('/', "~1")
}

impl Envelope<'_> {
    /// The error of the version of `name`, whose registered version is
    /// `current`.
    fn version(&self, name: &str, current: u32) -> Option<ValueError> {
        let path = format!("/$types/{}", pointer_token(name));
        let listed = self.versions.iter().find(|(n, _)| *n == name);
        let current_u64 = u64::from(current);
        match listed.and_then(|(_, v)| *v) {
            None => Some(document_error(
                "version_missing",
                &path,
                format!("`$types` does not record the version of `{name}`"),
            )),
            Some(v) if v == current_u64 => None,
            Some(v) if v < current_u64 => Some(document_error(
                "version_older",
                &path,
                format!(
                    "`{name}` is version {v} in the document and {current} in its declaration; reading it needs a migration"
                ),
            )),
            Some(v) => Some(document_error(
                "version_newer",
                &path,
                format!(
                    "`{name}` is version {v} in the document, newer than its declaration, {current}; update the package that declares it"
                ),
            )),
        }
    }

    /// The errors of the declarations that the document was written under: of a
    /// type whose hash is another than the registered one's, its version, or what
    /// differs in `$ir`, or, without `$ir`, that the declaration differs. A
    /// document without `$declarations`, as one written by hand, claims none.
    fn check(&self, lookup: &dyn TypeLookup, options: ReadOptions<'_>) -> Vec<ValueError> {
        let mut errors = Vec::new();
        let Some(listed) = &self.declarations else {
            return errors;
        };
        let version = TypeDef::version;
        let mut other: Vec<(QualifiedName, &str)> = Vec::new();
        for (name, hash) in listed {
            let Some(hash) = hash else {
                errors.push(document_error(
                    "document_form",
                    &format!("/$declarations/{}", pointer_token(name)),
                    "`$declarations` holds a hash per type".to_string(),
                ));
                continue;
            };
            let Ok(q) = QualifiedName::parse(name) else {
                continue;
            };
            let Some(def) = lookup.find(&q) else {
                continue;
            };
            if *(options.hash)(&q) == **hash {
                continue;
            }
            match self.version(name, version(def)) {
                Some(e) if e.code != "version_missing" => errors.push(e),
                _ => other.push((q, hash)),
            }
        }
        if other.is_empty() || options.any_declaration {
            return errors;
        }
        let path = "/$declarations";
        let Some(ir) = &self.ir else {
            // A type that holds another differs when it does, so a version that
            // differs is the reason for the rest.
            if errors.is_empty() {
                let names: Vec<String> = other.iter().map(|(q, _)| format!("`{q}`")).collect();
                let of = match names.len() {
                    1 => "another declaration",
                    _ => "other declarations",
                };
                errors.push(document_error(
                    "declaration_changed",
                    path,
                    format!(
                        "the document was written under {of} of {} than the registered, and does not hold it",
                        names.join(", ")
                    ),
                ));
            }
            return errors;
        };
        let docs: Option<Vec<crate::ir::IrDocument>> = ir.as_array().and_then(|a| {
            a.iter()
                .map(|d| crate::ir::IrDocument::from_json(&d.to_string()).ok())
                .collect()
        });
        let made = docs.map(crate::rules::DocumentSet::from);
        let Some(made) = made.filter(|m| {
            other
                .iter()
                .all(|(q, hash)| value_declaration_hash(q, m) == **hash)
        }) else {
            errors.push(document_error(
                "document_form",
                "/$ir",
                "`$ir` does not hold the declarations that `$declarations` names".to_string(),
            ));
            return errors;
        };
        let mut seen = std::collections::BTreeSet::new();
        for (q, _) in &other {
            for c in crate::update::changes(q, &made, lookup) {
                let at = format!("{path}/{}", pointer_token(&c.type_name));
                if c.effect == crate::update::Effect::Meaning
                    && c.code != "version_changed"
                    && seen.insert((at.clone(), c.message.clone()))
                {
                    errors.push(document_error("declaration_changed", &at, c.message));
                }
            }
        }
        errors
    }

    /// The errors of the named types that the value contains and the envelope
    /// does not list, or lists at another version, but those of `errors`.
    fn contained(&self, name: &str, current: u32, errors: &mut Vec<ValueError>) {
        if let Some(e) = self.version(name, current)
            && !errors.iter().any(|x| x.code == e.code && x.path == e.path)
        {
            errors.push(e);
        }
        if let Some(listed) = &self.declarations
            && !listed.iter().any(|(n, _)| *n == name)
        {
            errors.push(document_error(
                "declaration_missing",
                &format!("/$declarations/{}", pointer_token(name)),
                format!("`$declarations` does not record the declaration of `{name}`"),
            ));
        }
    }
}

fn document_error(code: &'static str, path: &str, message: String) -> ValueError {
    ValueError {
        code,
        path: path.to_string(),
        rule: None,
        rule_index: None,
        message,
        diagnosis: None,
        ..Default::default()
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
    read_document_cached(text, ty, lookup, &mut crate::validate::RuleCache::new())
}

/// [`read_document`] with compiled rules kept in `cache`, which is valid for
/// `lookup` while it is unchanged.
pub fn read_document_cached(
    text: &str,
    ty: &QualifiedName,
    lookup: &dyn TypeLookup,
    cache: &mut crate::validate::RuleCache,
) -> Result<Value, Vec<ValueError>> {
    let hash = |q: &QualifiedName| value_declaration_hash(q, lookup).into();
    let options = ReadOptions {
        any_declaration: false,
        hash: &hash,
    };
    // The tree refuses a member that repeats, of which a `Value` keeps the last.
    let read: crate::tree::Exact = serde_json::from_str(text).map_err(|e| {
        vec![document_error(
            "document_json",
            "",
            format!("not a JSON document: {e}"),
        )]
    })?;
    let mut doc = read.0.to_value();
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
    let Some(Value::Object(versions)) = m.shift_remove("$types") else {
        errors.push(document_error(
            "document_form",
            "/$types",
            "a document records its types' versions in `$types`".to_string(),
        ));
        return Err(errors);
    };
    let declarations = m.shift_remove("$declarations");
    let envelope = Envelope {
        versions: versions
            .iter()
            .map(|(k, v)| (k.as_str(), v.as_u64()))
            .collect(),
        declarations: match &declarations {
            Some(Value::Object(d)) => {
                Some(d.iter().map(|(k, v)| (k.as_str(), v.as_str())).collect())
            }
            Some(_) => {
                errors.push(document_error(
                    "document_form",
                    "/$declarations",
                    "`$declarations` holds a hash per type".to_string(),
                ));
                None
            }
            None => None,
        },
        ir: m.shift_remove("$ir"),
    };
    errors.extend(envelope.check(lookup, options));
    // The value moves out of the document: `$value`, or the document itself.
    let value = match m.remove("$value") {
        Some(v) => v,
        None => doc,
    };
    let validation = crate::validate::validate_cached(&value, ty, lookup, cache);
    if !validation.is_empty() {
        errors.extend(validation);
        return Err(errors);
    }
    // The envelope's members leave a document that is its own value.
    let mut value = value;
    if let Value::Object(m) = &mut value {
        m.shift_remove("$type");
    }
    // A value already in normalized form, such as a document R8 wrote, is
    // used as read: the emitter's stream is compared with it, building
    // nothing. Any other value is normalized through the emitter's text, one
    // tree at a time: the source is freed before the normalized text is read.
    let mut same = Emitter::with_sink(lookup, SameSink::new(&value), false);
    same.named(ty, Json::new(&value), None);
    let (is_same, contained) = (same.w.same(), std::mem::take(&mut same.contained));
    let (normalized, contained) = if is_same {
        (value, contained)
    } else {
        let mut e = Emitter::new(lookup, String::new(), false, false);
        e.named(ty, Json::new(&value), None);
        drop(value);
        let normalized: Value = serde_json::from_str(&e.w.out).expect("the emitter writes JSON");
        (normalized, e.contained)
    };
    for (name, current) in contained {
        envelope.contained(&name, current, &mut errors);
    }
    if errors.is_empty() {
        Ok(normalized)
    } else {
        Err(errors)
    }
}

// ------------------------------------------------------------- tree reads

/// A field type through which a value can hold a named type.
fn holds_named(ft: &FieldType) -> bool {
    match ft {
        FieldType::Named { .. } | FieldType::Dyn { .. } | FieldType::Subtype { .. } => true,
        FieldType::Option { inner, .. } => holds_named(inner),
        FieldType::Vec { element, .. } => holds_named(element),
        FieldType::BtreeMap { value, .. } | FieldType::IndexMap { value, .. } => holds_named(value),
        _ => false,
    }
}

/// The named types a valid value of `ft` contains, with their versions, as
/// the emitter records them: a named type, a `dyn()` value's implementor, and
/// a newtype variant's struct. The walk enters only the fields through which
/// a named type can be held.
fn contained<'a, A: Access<'a>>(
    v: A,
    ft: &FieldType,
    lookup: &dyn TypeLookup,
    out: &mut std::collections::BTreeMap<String, u32>,
) {
    if v.is_absent() || !holds_named(ft) {
        return;
    }
    match ft {
        FieldType::Option { inner, .. } => contained(v, inner, lookup, out),
        FieldType::Vec { element, .. } => {
            for i in 0..v.vec(element).unwrap_or(0) {
                contained(v.item(i), element, lookup, out);
            }
        }
        FieldType::BtreeMap { value, .. } | FieldType::IndexMap { value, .. } => {
            v.each_entry(|_, e| {
                if let Ok((_, x)) = e {
                    contained(x, value, lookup, out);
                }
            });
        }
        FieldType::Named { name } => contained_named(v, name, lookup, out),
        FieldType::Dyn { .. } | FieldType::Subtype { .. } => {
            if let Some(Ok((ty, _))) = crate::access::concrete(v, ft, lookup) {
                contained_named(v, &ty, lookup, out);
            }
        }
        _ => {}
    }
}

fn contained_named<'a, A: Access<'a>>(
    v: A,
    name: &QualifiedName,
    lookup: &dyn TypeLookup,
    out: &mut std::collections::BTreeMap<String, u32>,
) {
    let Some(def) = lookup.find(name) else { return };
    match def {
        TypeDef::Struct(s) => {
            out.insert(name.to_string(), s.version);
            for f in &s.fields {
                contained(v.member(f.name.as_str()), &f.ty, lookup, out);
            }
        }
        TypeDef::Enum(e) => {
            out.insert(name.to_string(), e.version);
            let Ok(variant) = v.variant(name, e) else {
                return;
            };
            let fields: &[FieldDef] = match e
                .variants
                .iter()
                .find(|x| x.name.as_str() == variant)
                .map(|x| &x.kind)
            {
                Some(VariantKind::Struct { fields, .. }) => fields,
                Some(VariantKind::Newtype { of }) => match lookup.find(of) {
                    Some(TypeDef::Struct(s)) => {
                        out.insert(of.to_string(), s.version);
                        &s.fields
                    }
                    _ => &[],
                },
                _ => &[],
            };
            for f in fields {
                contained(v.member(f.name.as_str()), &f.ty, lookup, out);
            }
        }
        TypeDef::Table(t) => {
            out.insert(name.to_string(), t.version);
        }
    }
}

/// A field type through which a value can hold a field with a default.
fn holds_defaults(ft: &FieldType) -> bool {
    holds_named(ft)
}

/// Gives each absent or null field of `t`, a valid value of `ft`, that has a
/// default its default, at every depth: the normalization of a document's
/// value on read, which the emitter applies when it writes.
pub fn fill_defaults(t: &mut Tree, ft: &FieldType, lookup: &dyn TypeLookup) {
    if matches!(t, Tree::Null) || !holds_defaults(ft) {
        return;
    }
    match ft {
        FieldType::Option { inner, .. } => fill_defaults(t, inner, lookup),
        FieldType::Vec { element, .. } => {
            if let Tree::Array(items) = t {
                for x in items.iter_mut() {
                    fill_defaults(x, element, lookup);
                }
            }
        }
        FieldType::BtreeMap { value, .. } => {
            if let Tree::Object(m) = t {
                for (_, x) in m.iter_mut() {
                    fill_defaults(x, value, lookup);
                }
            }
        }
        FieldType::IndexMap { value, .. } => {
            if let Tree::Array(pairs) = t {
                for p in pairs.iter_mut() {
                    if let Some(m) = p.members_mut()
                        && let Some((_, x)) = m.iter_mut().find(|(k, _)| &**k == "value")
                    {
                        fill_defaults(x, value, lookup);
                    }
                }
            }
        }
        FieldType::Named { name } => fill_named(t, name, lookup),
        FieldType::Dyn { .. } | FieldType::Subtype { .. } => {
            let found =
                crate::access::concrete(crate::access::JsonOf::of(TreeRef::new(t)), ft, lookup);
            if let Some(Ok((ty, _))) = found {
                fill_named(t, &ty, lookup);
            }
        }
        _ => {}
    }
}

fn fill_named(t: &mut Tree, name: &QualifiedName, lookup: &dyn TypeLookup) {
    let Some(def) = lookup.find(name) else { return };
    let fields: &[FieldDef] = match def {
        TypeDef::Struct(s) => &s.fields,
        TypeDef::Enum(e) => {
            let variant = match &*t {
                Tree::Object(_) => t.get(e.tag.as_str()).and_then(|v| match v {
                    Tree::Str(s) => Some(s.to_string()),
                    _ => None,
                }),
                _ => None,
            };
            let Some(variant) = variant else { return };
            match e
                .variants
                .iter()
                .find(|x| x.name.as_str() == variant)
                .map(|x| &x.kind)
            {
                Some(VariantKind::Struct { fields, .. }) => fields,
                Some(VariantKind::Newtype { of }) => match lookup.find(of) {
                    Some(TypeDef::Struct(s)) => &s.fields,
                    _ => return,
                },
                _ => return,
            }
        }
        // A column has no default.
        TypeDef::Table(_) => return,
    };
    let Some(members) = t.members_mut() else {
        return;
    };
    let mut added: Vec<(Box<str>, Tree)> = Vec::new();
    for f in fields {
        let at = members.iter().rposition(|(k, _)| &**k == f.name.as_str());
        match at {
            Some(i) if !matches!(members[i].1, Tree::Null) => {
                fill_defaults(&mut members[i].1, &f.ty, lookup);
            }
            _ => {
                let Some(d) = f.default.as_deref() else {
                    continue;
                };
                let value = d.clone();
                match at {
                    Some(i) => members[i].1 = value,
                    None => added.push((f.name.as_str().into(), value)),
                }
            }
        }
    }
    if !added.is_empty() {
        let mut all = std::mem::take(members).into_vec();
        all.extend(added);
        *members = all.into_boxed_slice();
    }
}

/// Reads a document of the type `ty` into a compact [`Tree`]: the value of
/// `$value`, or the document without its envelope, validated, with each
/// absent field that has a default given its default ([`fill_defaults`]),
/// and its `$types` checked against the contained types' declarations. The
/// read holds the document's text and one tree; a run of numbers takes 8 B
/// an item.
pub fn read_tree_cached(
    text: &str,
    ty: &QualifiedName,
    lookup: &dyn TypeLookup,
    cache: &mut crate::validate::RuleCache,
) -> Result<Tree, Vec<ValueError>> {
    let hash = |q: &QualifiedName| value_declaration_hash(q, lookup).into();
    let options = ReadOptions {
        any_declaration: false,
        hash: &hash,
    };
    read_tree_with(text, ty, lookup, cache, options)
}

/// [`read_tree_cached`] with options.
pub fn read_tree_with(
    text: &str,
    ty: &QualifiedName,
    lookup: &dyn TypeLookup,
    cache: &mut crate::validate::RuleCache,
    options: ReadOptions<'_>,
) -> Result<Tree, Vec<ValueError>> {
    let doc: Tree = serde_json::from_str(text).map_err(|e| {
        vec![document_error(
            "document_json",
            "",
            format!("not a JSON document: {e}"),
        )]
    })?;
    let Tree::Object(members) = doc else {
        return Err(vec![document_error(
            "document_form",
            "",
            "a document is a JSON object".to_string(),
        )]);
    };
    let mut errors = Vec::new();
    let mut members = members.into_vec();
    let take = |members: &mut Vec<(Box<str>, Tree)>, key: &str| {
        members
            .iter()
            .rposition(|(k, _)| &**k == key)
            .map(|i| members.remove(i).1)
    };
    match take(&mut members, "$type") {
        Some(Tree::Str(t)) if *t == *ty.to_string() => {}
        Some(Tree::Str(t)) => errors.push(document_error(
            "document_type",
            "/$type",
            format!("the document holds a `{t}`, not a `{ty}`"),
        )),
        _ => errors.push(document_error(
            "document_form",
            "/$type",
            "a document names its type in `$type`".to_string(),
        )),
    }
    let Some(versions @ Tree::Object(_)) = take(&mut members, "$types") else {
        errors.push(document_error(
            "document_form",
            "/$types",
            "a document records its types' versions in `$types`".to_string(),
        ));
        return Err(errors);
    };
    let declarations = take(&mut members, "$declarations");
    let ir = take(&mut members, "$ir");
    let envelope = Envelope {
        versions: match &versions {
            Tree::Object(m) => m
                .iter()
                .map(|(k, v)| {
                    let v = match v {
                        Tree::Num(n) => n.as_u64(),
                        _ => None,
                    };
                    (&**k, v)
                })
                .collect(),
            _ => Vec::new(),
        },
        declarations: match &declarations {
            Some(Tree::Object(m)) => Some(
                m.iter()
                    .map(|(k, v)| {
                        let v = match v {
                            Tree::Str(s) => Some(&**s),
                            _ => None,
                        };
                        (&**k, v)
                    })
                    .collect(),
            ),
            Some(_) => {
                errors.push(document_error(
                    "document_form",
                    "/$declarations",
                    "`$declarations` holds a hash per type".to_string(),
                ));
                None
            }
            None => None,
        },
        ir: ir.map(|t| t.to_value()),
    };
    errors.extend(envelope.check(lookup, options));
    let mut value = match take(&mut members, "$value") {
        Some(v) => v,
        None => Tree::Object(members.into_boxed_slice()),
    };
    let validation = crate::validate::validate_access(
        crate::access::JsonOf::of(TreeRef::new(&value)),
        ty,
        lookup,
        cache,
    );
    if !validation.is_empty() {
        errors.extend(validation);
        return Err(errors);
    }
    let named = FieldType::Named { name: ty.clone() };
    fill_defaults(&mut value, &named, lookup);
    let mut found = std::collections::BTreeMap::new();
    contained(
        crate::access::JsonOf::of(TreeRef::new(&value)),
        &named,
        lookup,
        &mut found,
    );
    for (name, current) in found {
        envelope.contained(&name, current, &mut errors);
    }
    if errors.is_empty() {
        Ok(value)
    } else {
        Err(errors)
    }
}

/// The normalized form of a valid value of the named type `ty`, in any host
/// form, as a [`Tree`] with no runs: what the emitter writes, built without
/// text. A host turns a value of the type into a field's default with it.
pub fn normalized_tree<'a, A: Access<'a>>(
    value: A,
    ty: &QualifiedName,
    lookup: &dyn TypeLookup,
) -> Tree {
    let mut e = Emitter::with_sink(lookup, crate::tree::TreeSink::default(), false);
    e.named(ty, value, None);
    e.w.out.take().unwrap_or(Tree::Null)
}

/// The normalized form of a valid value of the field type `ty`, in any host
/// form, as a [`Tree`] with no runs: the JSON form of a default that a host
/// gives as a value of the type.
pub fn normalized_field_tree<'a, A: Access<'a>>(
    value: A,
    ty: &FieldType,
    lookup: &dyn TypeLookup,
) -> Tree {
    let mut e = Emitter::with_sink(lookup, crate::tree::TreeSink::default(), false);
    e.value(ty, value);
    e.w.out.take().unwrap_or(Tree::Null)
}
