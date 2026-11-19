//! The normalized JSON form of a value in any host form, written as a
//! stream.
//!
//! [`Emitter`] walks a valid value through [`Access`] by its declared type and
//! writes its normalized form: the envelope removed, struct fields in
//! declaration order, an absent field with a default given its default, and
//! every other absent field written as `null`. A [`Writer`] writes the
//! stream as RFC 8785 text (strings escaped as `JSON.stringify` escapes them,
//! numbers in ECMAScript's `Number.prototype.toString` form) into any
//! [`Out`]: a `String` for text, a SHA-256 state for a hash, so a hash
//! builds neither the text nor a tree. In sorted mode, object members are
//! written in the order of the UTF-16 code units of their names, the RFC
//! 8785 canonical form; otherwise in declaration order, the form of
//! documents.
//!
//! In digest mode, a matrix, an array, a sparse matrix, a table, and a
//! `vec()` of logical, integer, double, date, or date-time values are written
//! as `{"$digest": "sha256:<hex>"}`, the SHA-256 of their canonical binary
//! form (`crate::codec`), which is the form the content hash takes.

use std::borrow::Cow;
use std::collections::BTreeMap;

use serde_json::Value;
use sha2::{Digest, Sha256};

use crate::access::{Access, Json, base64_encode};
use crate::codec::{date_text, datetime_text};
use crate::ir::{FieldDef, FieldType, TypeDef, VariantShape};
use crate::name::QualifiedName;
use crate::rules::TypeLookup;

// ------------------------------------------------------------------ output

/// A destination of text or bytes.
pub trait Out {
    fn put(&mut self, bytes: &[u8]);
}

impl Out for String {
    fn put(&mut self, bytes: &[u8]) {
        // The writer emits UTF-8 only.
        self.push_str(std::str::from_utf8(bytes).expect("UTF-8"));
    }
}

impl Out for Vec<u8> {
    fn put(&mut self, bytes: &[u8]) {
        self.extend_from_slice(bytes);
    }
}

impl Out for Sha256 {
    fn put(&mut self, bytes: &[u8]) {
        self.update(bytes);
    }
}

/// A number in ECMAScript's `Number.prototype.toString` form.
pub fn number_text(x: f64) -> String {
    if x == 0.0 {
        return "0".to_string();
    }
    let mut buf = ryu_js::Buffer::new();
    buf.format(x).to_string()
}

/// A JSON string as `JSON.stringify` writes it.
pub fn write_string<O: Out>(s: &str, out: &mut O) {
    out.put(b"\"");
    let bytes = s.as_bytes();
    let mut start = 0;
    for (i, &b) in bytes.iter().enumerate() {
        let esc: Option<&[u8]> = match b {
            b'"' => Some(b"\\\""),
            b'\\' => Some(b"\\\\"),
            0x08 => Some(b"\\b"),
            0x0c => Some(b"\\f"),
            b'\n' => Some(b"\\n"),
            b'\r' => Some(b"\\r"),
            b'\t' => Some(b"\\t"),
            _ => None,
        };
        if let Some(e) = esc {
            out.put(&bytes[start..i]);
            out.put(e);
            start = i + 1;
        } else if b < 0x20 {
            out.put(&bytes[start..i]);
            out.put(format!("\\u{:04x}", b).as_bytes());
            start = i + 1;
        }
    }
    out.put(&bytes[start..]);
    out.put(b"\"");
}

/// Writes RFC 8785 text into an [`Out`], placing separators.
pub struct Writer<O: Out> {
    pub out: O,
    /// Object members in the order of their names' UTF-16 code units.
    pub sorted: bool,
    /// Per open array or object, whether the next item is its first.
    first: Vec<bool>,
    after_key: bool,
}

impl<O: Out> Writer<O> {
    pub fn new(out: O, sorted: bool) -> Self {
        Writer {
            out,
            sorted,
            first: Vec::new(),
            after_key: false,
        }
    }

    fn sep(&mut self) {
        if self.after_key {
            self.after_key = false;
            return;
        }
        if let Some(f) = self.first.last_mut() {
            if *f {
                *f = false;
            } else {
                self.out.put(b",");
            }
        }
    }

    pub fn null(&mut self) {
        self.sep();
        self.out.put(b"null");
    }

    pub fn bool(&mut self, b: bool) {
        self.sep();
        self.out.put(if b { b"true" } else { b"false" });
    }

    /// A number; a non-finite double is its string form.
    pub fn number(&mut self, x: f64) {
        if x.is_nan() {
            return self.string("NaN");
        }
        if x.is_infinite() {
            return self.string(if x > 0.0 { "Infinity" } else { "-Infinity" });
        }
        self.sep();
        self.out.put(number_text(x).as_bytes());
    }

    pub fn string(&mut self, s: &str) {
        self.sep();
        write_string(s, &mut self.out);
    }

    pub fn begin_array(&mut self) {
        self.sep();
        self.out.put(b"[");
        self.first.push(true);
    }

    pub fn end_array(&mut self) {
        self.first.pop();
        self.out.put(b"]");
    }

    pub fn begin_object(&mut self) {
        self.sep();
        self.out.put(b"{");
        self.first.push(true);
    }

    pub fn key(&mut self, k: &str) {
        self.sep();
        write_string(k, &mut self.out);
        self.out.put(b":");
        self.after_key = true;
    }

    pub fn end_object(&mut self) {
        self.first.pop();
        self.out.put(b"}");
    }

    /// Writes a JSON value.
    pub fn value(&mut self, v: &Value) {
        match v {
            Value::Null => self.null(),
            Value::Bool(b) => self.bool(*b),
            Value::Number(n) => {
                self.sep();
                self.out
                    .put(number_text(n.as_f64().unwrap_or(f64::NAN)).as_bytes());
            }
            Value::String(s) => self.string(s),
            Value::Array(items) => {
                self.begin_array();
                for x in items {
                    self.value(x);
                }
                self.end_array();
            }
            Value::Object(m) => {
                let mut keys: Vec<&String> = m.keys().collect();
                if self.sorted {
                    keys.sort_by(|a, b| a.encode_utf16().cmp(b.encode_utf16()));
                }
                self.begin_object();
                for k in keys {
                    self.key(k);
                    self.value(&m[k]);
                }
                self.end_object();
            }
        }
    }
}

/// Sorts names by their UTF-16 code units, the RFC 8785 member order.
fn utf16_order(a: &str, b: &str) -> std::cmp::Ordering {
    a.encode_utf16().cmp(b.encode_utf16())
}

// ----------------------------------------------------------- binary forms

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

/// Whether a `vec()` of `element` takes a digest in the content hash.
fn digestible_vec(element: &FieldType) -> bool {
    matches!(element_tag(element).0, 1 | 2 | 3 | 5 | 6)
}

fn put_str<O: Out>(out: &mut O, s: &str) {
    out.put(&(s.len() as u64).to_le_bytes());
    out.put(s.as_bytes());
}

fn double_bits(x: f64) -> u64 {
    if x.is_nan() {
        0x7FF8_0000_0000_0000
    } else if x == 0.0 {
        0
    } else {
        x.to_bits()
    }
}

/// Writes the validity bitmap and the values of items `0..n` of a sequence
/// of the scalar element `tag`.
fn put_sequence<'a, A: Access<'a>, O: Out>(out: &mut O, tag: u8, v: A, n: usize) {
    let mut bitmap = vec![0u8; n.div_ceil(8)];
    let mut mark = |i: usize, present: bool| {
        if present {
            bitmap[i / 8] |= 1 << (i % 8);
        }
    };
    match tag {
        1 => v.each_logical(n, |i, r| mark(i, matches!(r, Ok(Some(_))))),
        2 => v.each_integer(n, |i, r| mark(i, matches!(r, Ok(Some(_))))),
        3 => v.each_double(n, |i, r| mark(i, matches!(r, Ok(Some(_))))),
        4 => v.each_character(n, |i, r| mark(i, matches!(r, Ok(Some(_))))),
        5 => v.each_date(n, |i, r| mark(i, matches!(r, Ok(Some(_))))),
        6 => v.each_datetime(n, |i, r| mark(i, matches!(r, Ok(Some(_))))),
        _ => {}
    }
    out.put(&bitmap);
    // Values, buffered so a hash is updated in blocks.
    let mut buf: Vec<u8> = Vec::with_capacity(8192);
    let flush = |buf: &mut Vec<u8>, out: &mut O| {
        if buf.len() >= 8000 {
            out.put(buf);
            buf.clear();
        }
    };
    match tag {
        1 => v.each_logical(n, |_, r| {
            buf.push(u8::from(matches!(r, Ok(Some(true)))));
            flush(&mut buf, out);
        }),
        2 => v.each_integer(n, |_, r| {
            let x = r.ok().flatten().unwrap_or(0) as i32;
            buf.extend_from_slice(&x.to_le_bytes());
            flush(&mut buf, out);
        }),
        3 => v.each_double(n, |_, r| {
            let x = r.ok().flatten().unwrap_or(0.0);
            buf.extend_from_slice(&double_bits(x).to_le_bytes());
            flush(&mut buf, out);
        }),
        4 => v.each_character(n, |_, r| {
            let s = r.ok().flatten().unwrap_or(Cow::Borrowed(""));
            buf.extend_from_slice(&(s.len() as u64).to_le_bytes());
            buf.extend_from_slice(s.as_bytes());
            flush(&mut buf, out);
        }),
        5 => v.each_date(n, |_, r| {
            let x = r.ok().flatten().unwrap_or(0) as i32;
            buf.extend_from_slice(&x.to_le_bytes());
            flush(&mut buf, out);
        }),
        6 => v.each_datetime(n, |_, r| {
            let x = r.ok().flatten().unwrap_or(0);
            buf.extend_from_slice(&x.to_le_bytes());
            flush(&mut buf, out);
        }),
        _ => {}
    }
    out.put(&buf);
}

/// The canonical binary form of a dense matrix or array (`R8M1`).
pub fn dense_binary_access<'a, A: Access<'a>, O: Out>(element: &FieldType, v: A, out: &mut O) {
    let (tag, _) = element_tag(element);
    let Ok(d) = v.dense(element) else { return };
    out.put(b"R8M1");
    out.put(&[tag]);
    out.put(&(d.shape.len() as u32).to_le_bytes());
    for x in &d.shape {
        out.put(&x.to_le_bytes());
    }
    put_sequence(out, tag, d.data, d.len);
    for i in 0..d.shape.len() {
        match d
            .dimnames
            .as_ref()
            .and_then(|dn| dn.get(i))
            .and_then(|x| x.as_ref())
        {
            Some(names) => {
                out.put(&[1]);
                for n in names {
                    put_str(out, n);
                }
            }
            None => out.put(&[0]),
        }
    }
}

/// The canonical binary form of a `vec()` of a scalar type: `R8M1` with one
/// dimension and no names.
pub fn vec_binary_access<'a, A: Access<'a>, O: Out>(element: &FieldType, v: A, out: &mut O) {
    let (tag, _) = element_tag(element);
    let n = v.vec(element).unwrap_or(0);
    out.put(b"R8M1");
    out.put(&[tag]);
    out.put(&1u32.to_le_bytes());
    out.put(&(n as u64).to_le_bytes());
    put_sequence(out, tag, v, n);
    out.put(&[0]);
}

/// The canonical binary form of a sparse matrix (`R8S1`).
pub fn sparse_binary_access<'a, A: Access<'a>, O: Out>(element: &FieldType, v: A, out: &mut O) {
    let (tag, _) = element_tag(element);
    let Ok(s) = v.sparse(element) else { return };
    out.put(b"R8S1");
    out.put(&[tag]);
    out.put(&s.shape[0].to_le_bytes());
    out.put(&s.shape[1].to_le_bytes());
    out.put(&(s.len as u64).to_le_bytes());
    for x in s.indptr.iter().chain(&s.indices) {
        out.put(&x.to_le_bytes());
    }
    // The values of a sparse matrix carry no bitmap.
    let mut skip = Vec::new();
    put_sequence(&mut skip, tag, s.data, s.len);
    out.put(&skip[s.len.div_ceil(8)..]);
}

/// The canonical binary form of a table (`R8T1`).
pub fn table_binary_access<'a, A: Access<'a>, O: Out>(
    columns: &[FieldDef],
    open: bool,
    v: A,
    out: &mut O,
) {
    let Ok(t) = v.table(columns, open) else {
        return;
    };
    let mut cols: Vec<(Cow<'a, str>, A)> = t.columns;
    cols.sort_by(|a, b| a.0.as_bytes().cmp(b.0.as_bytes()));
    out.put(b"R8T1");
    out.put(&t.rows.to_le_bytes());
    out.put(&(cols.len() as u32).to_le_bytes());
    let n = t.rows as usize;
    for (name, cells) in cols {
        let ft = columns
            .iter()
            .find(|c| c.name.as_str() == name)
            .map(|c| c.ty.clone())
            .or_else(|| v.extra_column_type(&name).ok())
            .unwrap_or(FieldType::Json);
        let (tag, inner) = element_tag(&ft);
        put_str(out, &name);
        out.put(&[tag]);
        if let FieldType::Factor { levels, ordered } = inner {
            let Ok(f) = cells.factor(levels.as_deref(), *ordered) else {
                continue;
            };
            let names: Vec<Cow<'_, str>> = match levels {
                Some(d) => d.iter().map(|l| Cow::Borrowed(l.as_str())).collect(),
                None => f.levels.clone(),
            };
            out.put(&(names.len() as u64).to_le_bytes());
            for l in &names {
                put_str(out, l);
            }
            let code = |i: usize| -> Option<i32> {
                let c = f.codes.get(i)?;
                let level = f.levels.get((c as usize).checked_sub(1)?)?;
                names.iter().position(|l| l == level).map(|k| k as i32 + 1)
            };
            let k = f.codes.len();
            let mut bitmap = vec![0u8; k.div_ceil(8)];
            for (i, byte) in (0..k).map(|i| (i, code(i))) {
                if byte.is_some() {
                    bitmap[i / 8] |= 1 << (i % 8);
                }
            }
            out.put(&bitmap);
            let mut buf = Vec::with_capacity(k * 4);
            for i in 0..k {
                buf.extend_from_slice(&code(i).unwrap_or(0).to_le_bytes());
            }
            out.put(&buf);
            continue;
        }
        put_sequence(out, tag, cells, n);
    }
}

fn digest_hex(f: impl FnOnce(&mut Sha256)) -> String {
    let mut h = Sha256::new();
    f(&mut h);
    let d = h.finalize();
    d.iter().map(|b| format!("{b:02x}")).collect()
}

// ----------------------------------------------------------------- emitter

/// Writes a valid value's normalized form.
pub struct Emitter<'l, O: Out> {
    pub lookup: &'l dyn TypeLookup,
    pub w: Writer<O>,
    /// Write matrices, arrays, sparse matrices, tables, and digestible
    /// vectors as digests.
    pub digest: bool,
    /// The named types the value contains, with their versions.
    pub contained: BTreeMap<String, u32>,
}

fn unwrap_option(ft: &FieldType) -> &FieldType {
    match ft {
        FieldType::Option { inner, .. } => inner,
        other => other,
    }
}

fn esc(s: &str) -> String {
    s.replace('~', "~0").replace('/', "~1")
}

impl<'l, O: Out> Emitter<'l, O> {
    pub fn new(lookup: &'l dyn TypeLookup, out: O, sorted: bool, digest: bool) -> Self {
        Emitter {
            lookup,
            w: Writer::new(out, sorted),
            digest,
            contained: BTreeMap::new(),
        }
    }

    fn digest_object(&mut self, hex: String) {
        self.w.begin_object();
        self.w.key("$digest");
        self.w.string(&format!("sha256:{hex}"));
        self.w.end_object();
    }

    /// Writes a value of the field type `ft`.
    pub fn value<'a, A: Access<'a>>(&mut self, ft: &FieldType, v: A) {
        if v.is_absent() {
            return self.w.null();
        }
        let lookup = self.lookup;
        match ft {
            FieldType::Option { inner, .. } => self.value(inner, v),
            FieldType::Logical => self.w.bool(v.logical().unwrap_or(false)),
            FieldType::Integer(_) => self.w.number(v.integer().unwrap_or(0) as f64),
            FieldType::Double(_) => self.w.number(v.double().unwrap_or(f64::NAN)),
            FieldType::Character(_) => {
                let s = v.character().unwrap_or(Cow::Borrowed(""));
                self.w.string(&s);
            }
            FieldType::Raw => {
                let b = v.bytes().unwrap_or(Cow::Borrowed(&[]));
                self.w.string(&base64_encode(&b));
            }
            FieldType::Date => self.w.string(&date_text(v.date().unwrap_or(0))),
            FieldType::Posixct => self.w.string(&datetime_text(v.datetime().unwrap_or(0))),
            FieldType::Factor { levels, ordered } => self.factor(levels.as_deref(), *ordered, v),
            FieldType::Vec { element, .. } => {
                if self.digest && digestible_vec(element) {
                    let hex = digest_hex(|h| vec_binary_access(element, v, h));
                    return self.digest_object(hex);
                }
                let n = v.vec(element).unwrap_or(0);
                self.sequence(element, v, n);
            }
            FieldType::Hashmap { value, .. } => {
                let mut entries: Vec<(Cow<'a, str>, A)> = Vec::new();
                v.each_entry(|_, e| {
                    if let Ok(e) = e {
                        entries.push(e);
                    }
                });
                if self.w.sorted {
                    entries.sort_by(|a, b| utf16_order(&a.0, &b.0));
                }
                self.w.begin_object();
                for (k, x) in entries {
                    self.w.key(&k);
                    self.value(value, x);
                }
                self.w.end_object();
            }
            FieldType::OrderedMap { value, .. } => {
                let mut entries: Vec<(Cow<'a, str>, A)> = Vec::new();
                v.each_entry(|_, e| {
                    if let Ok(e) = e {
                        entries.push(e);
                    }
                });
                self.w.begin_array();
                for (k, x) in entries {
                    self.w.begin_object();
                    self.w.key("name");
                    self.w.string(&k);
                    self.w.key("value");
                    self.value(value, x);
                    self.w.end_object();
                }
                self.w.end_array();
            }
            FieldType::Json => {
                let x = v.to_value(&FieldType::Json, lookup);
                self.w.value(&x);
            }
            FieldType::R { .. } => self.w.null(),
            FieldType::Matrix { element, .. } | FieldType::Array { element, .. } => {
                self.dense(element, v)
            }
            FieldType::MatrixOf { of } => {
                let element = match lookup.find(of) {
                    Some(TypeDef::Struct(s)) => s.fields.first().map(|f| &f.ty),
                    _ => None,
                };
                self.dense(element.unwrap_or(&FieldType::Logical), v);
            }
            FieldType::SparseMatrix { element, .. } => self.sparse(element, v),
            FieldType::Table { columns, open, .. } => self.table(columns, *open, v),
            FieldType::TableOf { of } => match lookup.find(of) {
                Some(TypeDef::Struct(s)) => self.table(&s.fields, false, v),
                _ => self.w.null(),
            },
            FieldType::Named { name } => self.named(name, v, None),
            FieldType::Dyn { r#trait } => match v.implementor(r#trait, lookup) {
                Ok((ty, member)) => {
                    let tag = match &member {
                        Some(m) => {
                            let t = v.member(m).character().map(|c| c.into_owned());
                            (m.clone(), t.unwrap_or_else(|_| ty.to_string()))
                        }
                        None => ("$type".to_string(), ty.to_string()),
                    };
                    self.named(&ty, v, Some(tag));
                }
                Err(_) => self.w.null(),
            },
        }
    }

    /// Items `0..n` of a sequence of `element`.
    fn sequence<'a, A: Access<'a>>(&mut self, element: &FieldType, v: A, n: usize) {
        let inner = unwrap_option(element);
        if !matches!(
            inner,
            FieldType::Logical
                | FieldType::Integer(_)
                | FieldType::Double(_)
                | FieldType::Character(_)
                | FieldType::Date
                | FieldType::Posixct
        ) {
            self.w.begin_array();
            for i in 0..n {
                self.value(element, v.item(i));
            }
            self.w.end_array();
            return;
        }
        let w = &mut self.w;
        w.begin_array();
        match inner {
            FieldType::Logical => v.each_logical(n, |_, r| match r {
                Ok(Some(b)) => w.bool(b),
                _ => w.null(),
            }),
            FieldType::Integer(_) => v.each_integer(n, |_, r| match r {
                Ok(Some(x)) => w.number(x as f64),
                _ => w.null(),
            }),
            FieldType::Double(_) => v.each_double(n, |_, r| match r {
                Ok(Some(x)) => w.number(x),
                _ => w.null(),
            }),
            FieldType::Character(_) => v.each_character(n, |_, r| match r {
                Ok(Some(s)) => w.string(&s),
                _ => w.null(),
            }),
            FieldType::Date => v.each_date(n, |_, r| match r {
                Ok(Some(d)) => w.string(&date_text(d)),
                _ => w.null(),
            }),
            _ => v.each_datetime(n, |_, r| match r {
                Ok(Some(m)) => w.string(&datetime_text(m)),
                _ => w.null(),
            }),
        }
        w.end_array();
    }

    fn factor<'a, A: Access<'a>>(&mut self, declared: Option<&[String]>, ordered: bool, v: A) {
        let Ok(f) = v.factor(declared, ordered) else {
            return self.w.null();
        };
        let n = f.codes.len();
        let level = |i: usize| -> Option<&str> {
            let c = f.codes.get(i)?;
            f.levels
                .get((c as usize).checked_sub(1)?)
                .map(|l| l.as_ref())
        };
        if declared.is_some() {
            self.w.begin_array();
            for i in 0..n {
                match level(i) {
                    Some(l) => self.w.string(l),
                    None => self.w.null(),
                }
            }
            self.w.end_array();
            return;
        }
        let write_levels = |w: &mut Writer<O>| {
            w.key("levels");
            w.begin_array();
            for l in &f.levels {
                w.string(l);
            }
            w.end_array();
        };
        let write_codes = |w: &mut Writer<O>| {
            w.key("codes");
            w.begin_array();
            for i in 0..n {
                match f.codes.get(i) {
                    Some(c) => w.number(f64::from(c)),
                    None => w.null(),
                }
            }
            w.end_array();
        };
        self.w.begin_object();
        if self.w.sorted {
            write_codes(&mut self.w);
            write_levels(&mut self.w);
        } else {
            write_levels(&mut self.w);
            write_codes(&mut self.w);
        }
        self.w.end_object();
    }

    fn dense<'a, A: Access<'a>>(&mut self, element: &FieldType, v: A) {
        if self.digest {
            let hex = digest_hex(|h| dense_binary_access(element, v, h));
            return self.digest_object(hex);
        }
        let Ok(d) = v.dense(element) else {
            return self.w.null();
        };
        let write_shape = |w: &mut Writer<O>| {
            w.key("shape");
            w.begin_array();
            for x in &d.shape {
                w.number(*x as f64);
            }
            w.end_array();
        };
        let write_dimnames = |w: &mut Writer<O>| {
            if let Some(dn) = &d.dimnames {
                w.key("dimnames");
                w.begin_array();
                for names in dn {
                    match names {
                        Some(n) => {
                            w.begin_array();
                            for s in n {
                                w.string(s);
                            }
                            w.end_array();
                        }
                        None => w.null(),
                    }
                }
                w.end_array();
            }
        };
        self.w.begin_object();
        // Sorted: data, dimnames, shape; declared: shape, data, dimnames.
        if !self.w.sorted {
            write_shape(&mut self.w);
        }
        self.w.key("data");
        self.sequence(element, d.data, d.len);
        write_dimnames(&mut self.w);
        if self.w.sorted {
            write_shape(&mut self.w);
        }
        self.w.end_object();
    }

    fn sparse<'a, A: Access<'a>>(&mut self, element: &FieldType, v: A) {
        if self.digest {
            let hex = digest_hex(|h| sparse_binary_access(element, v, h));
            return self.digest_object(hex);
        }
        let Ok(s) = v.sparse(element) else {
            return self.w.null();
        };
        let ints = |w: &mut Writer<O>, k: &str, xs: &[u64]| {
            w.key(k);
            w.begin_array();
            for x in xs {
                w.number(*x as f64);
            }
            w.end_array();
        };
        self.w.begin_object();
        // Sorted: data, indices, indptr, shape.
        if self.w.sorted {
            self.w.key("data");
            self.sequence(element, s.data, s.len);
            ints(&mut self.w, "indices", &s.indices);
            ints(&mut self.w, "indptr", &s.indptr);
            ints(&mut self.w, "shape", &s.shape);
        } else {
            ints(&mut self.w, "shape", &s.shape);
            ints(&mut self.w, "indptr", &s.indptr);
            ints(&mut self.w, "indices", &s.indices);
            self.w.key("data");
            self.sequence(element, s.data, s.len);
        }
        self.w.end_object();
    }

    fn table<'a, A: Access<'a>>(&mut self, columns: &[FieldDef], open: bool, v: A) {
        if self.digest {
            let hex = digest_hex(|h| table_binary_access(columns, open, v, h));
            return self.digest_object(hex);
        }
        let Ok(t) = v.table(columns, open) else {
            return self.w.null();
        };
        let n = t.rows as usize;
        // Each column with its type; extra columns of an open table listed
        // under "$columns".
        let mut cols: Vec<(Cow<'a, str>, A, FieldType, bool)> = Vec::new();
        for (name, cells) in t.columns {
            match columns.iter().find(|c| c.name.as_str() == name) {
                Some(c) => cols.push((name, cells, c.ty.clone(), false)),
                None => {
                    if let Ok(ft) = v.extra_column_type(&name) {
                        cols.push((name, cells, ft, true));
                    }
                }
            }
        }
        // Members: "$length", the columns, and "$columns".
        let mut members: Vec<(String, Option<usize>)> = vec![("$length".to_string(), None)];
        for (i, c) in cols.iter().enumerate() {
            members.push((c.0.to_string(), Some(i)));
        }
        let extras: Vec<(String, Value)> = cols
            .iter()
            .filter(|c| c.3)
            .map(|c| {
                (
                    c.0.to_string(),
                    serde_json::to_value(&c.2).unwrap_or(Value::Null),
                )
            })
            .collect();
        if !extras.is_empty() {
            members.push(("$columns".to_string(), None));
        }
        if self.w.sorted {
            members.sort_by(|a, b| utf16_order(&a.0, &b.0));
        }
        self.w.begin_object();
        for (k, col) in members {
            self.w.key(&k);
            match col {
                Some(i) => {
                    let (_, cells, ft, _) = &cols[i];
                    match unwrap_option(ft) {
                        FieldType::Factor { levels, ordered } => {
                            self.factor(levels.as_deref(), *ordered, *cells)
                        }
                        _ => self.sequence(ft, *cells, n),
                    }
                }
                None if k == "$length" => self.w.number(n as f64),
                None => {
                    let obj = Value::Object(extras.iter().cloned().collect());
                    self.w.value(&obj);
                }
            }
        }
        self.w.end_object();
    }

    /// Writes a value of a named type; `extra` is a member added to an
    /// object, such as the type member of a `dyn()` value.
    pub fn named<'a, A: Access<'a>>(
        &mut self,
        name: &QualifiedName,
        v: A,
        extra: Option<(String, String)>,
    ) {
        let lookup = self.lookup;
        let Some(def) = lookup.find(name) else {
            return self.w.null();
        };
        let version = match def {
            TypeDef::Struct(s) => s.version,
            TypeDef::Enum(e) => e.version,
        };
        self.contained.insert(name.to_string(), version);
        match def {
            TypeDef::Struct(s) => self.object(&s.fields, v, extra.into_iter().collect()),
            TypeDef::Enum(e) => {
                let variant = v
                    .variant(name, e)
                    .map(|c| c.into_owned())
                    .unwrap_or_default();
                if e.variants
                    .iter()
                    .all(|x| matches!(x.shape, VariantShape::Unit))
                {
                    return self.w.string(&variant);
                }
                let fields: &[FieldDef] = match e
                    .variants
                    .iter()
                    .find(|x| x.name.as_str() == variant)
                    .map(|x| &x.shape)
                {
                    Some(VariantShape::Struct { fields, .. }) => fields,
                    Some(VariantShape::Newtype { of }) => match lookup.find(of) {
                        Some(TypeDef::Struct(s)) => {
                            self.contained.insert(of.to_string(), s.version);
                            &s.fields
                        }
                        _ => &[],
                    },
                    _ => &[],
                };
                let mut extras: Vec<(String, String)> = vec![(e.tag.to_string(), variant)];
                extras.extend(extra);
                self.object(fields, v, extras);
            }
        }
    }

    /// The fields of a record, then string members `extras`.
    fn object<'a, A: Access<'a>>(
        &mut self,
        fields: &[FieldDef],
        v: A,
        extras: Vec<(String, String)>,
    ) {
        let mut members: Vec<(&str, Option<usize>)> = fields
            .iter()
            .enumerate()
            .map(|(i, f)| (f.name.as_str(), Some(i)))
            .collect();
        for e in &extras {
            members.push((e.0.as_str(), None));
        }
        if self.w.sorted {
            members.sort_by(|a, b| utf16_order(a.0, b.0));
        }
        self.w.begin_object();
        for (k, field) in members {
            self.w.key(k);
            match field {
                Some(i) => {
                    let f = &fields[i];
                    let x = v.member_at(i, k);
                    match (&f.default, x.is_absent()) {
                        (Some(d), true) => self.value(&f.ty, Json::new(d)),
                        _ => self.value(&f.ty, x),
                    }
                }
                None => {
                    let s = extras.iter().find(|e| e.0 == k).map(|e| e.1.clone());
                    self.w.string(&s.unwrap_or_default());
                }
            }
        }
        self.w.end_object();
    }
}

/// The JSON Pointers of the runtime-only fields of a value that hold a
/// value, found by a walk of its records and collections that reads no
/// scalar data.
pub fn runtime_paths<'a, A: Access<'a>>(
    ty: &QualifiedName,
    value: A,
    lookup: &dyn TypeLookup,
) -> Vec<String> {
    let mut out = Vec::new();
    walk_named(ty, value, lookup, "", &mut out);
    out
}

fn walk_named<'a, A: Access<'a>>(
    name: &QualifiedName,
    v: A,
    lookup: &dyn TypeLookup,
    path: &str,
    out: &mut Vec<String>,
) {
    let fields: &[FieldDef] = match lookup.find(name) {
        Some(TypeDef::Struct(s)) => &s.fields,
        Some(TypeDef::Enum(e)) => {
            let variant = v
                .variant(name, e)
                .map(|c| c.into_owned())
                .unwrap_or_default();
            match e
                .variants
                .iter()
                .find(|x| x.name.as_str() == variant)
                .map(|x| &x.shape)
            {
                Some(VariantShape::Struct { fields, .. }) => fields,
                Some(VariantShape::Newtype { of }) => match lookup.find(of) {
                    Some(TypeDef::Struct(s)) => &s.fields,
                    _ => return,
                },
                _ => return,
            }
        }
        None => return,
    };
    for (i, f) in fields.iter().enumerate() {
        let x = v.member_at(i, f.name.as_str());
        if !x.is_absent() {
            walk(
                &f.ty,
                x,
                lookup,
                &format!("{path}/{}", esc(f.name.as_str())),
                out,
            );
        }
    }
}

fn walk<'a, A: Access<'a>>(
    ft: &FieldType,
    v: A,
    lookup: &dyn TypeLookup,
    path: &str,
    out: &mut Vec<String>,
) {
    match unwrap_option(ft) {
        FieldType::R { .. } => out.push(path.to_string()),
        FieldType::Named { name } => walk_named(name, v, lookup, path, out),
        FieldType::Dyn { r#trait } => {
            if let Ok((ty, _)) = v.implementor(r#trait, lookup) {
                walk_named(&ty, v, lookup, path, out);
            }
        }
        FieldType::Vec { element, .. }
            if matches!(
                unwrap_option(element),
                FieldType::Named { .. } | FieldType::Dyn { .. } | FieldType::Vec { .. }
            ) =>
        {
            let n = v.vec(element).unwrap_or(0);
            for i in 0..n {
                let x = v.item(i);
                if !x.is_absent() {
                    walk(element, x, lookup, &format!("{path}/{i}"), out);
                }
            }
        }
        FieldType::Hashmap { value, .. } | FieldType::OrderedMap { value, .. }
            if matches!(
                unwrap_option(value),
                FieldType::Named { .. } | FieldType::Dyn { .. } | FieldType::Vec { .. }
            ) =>
        {
            let ordered = matches!(unwrap_option(ft), FieldType::OrderedMap { .. });
            let mut entries = Vec::new();
            v.each_entry(|i, e| {
                if let Ok((k, x)) = e {
                    entries.push((i, k.into_owned(), x));
                }
            });
            for (i, k, x) in entries {
                let p = if ordered {
                    format!("{path}/{i}/value")
                } else {
                    format!("{path}/{}", esc(&k))
                };
                if !x.is_absent() {
                    walk(value, x, lookup, &p, out);
                }
            }
        }
        _ => {}
    }
}
