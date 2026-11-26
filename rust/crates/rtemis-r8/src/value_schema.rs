//! The JSON Schema (draft 2020-12) of a type's values, for tools outside R8.
//!
//! [`json_schema`] writes each named type the type reaches into `$defs`,
//! keyed by its qualified name, and the type itself as the root, which also
//! admits the document envelope (`"$type"`, `"$types"`, `"$declarations"`, and `"$ir"`). A
//! struct is an object without other members; an absent field is `null` or
//! an omitted member. An enum of unit variants is an `enum` of names; an enum
//! with a struct or newtype variant is an `anyOf` of objects whose tag member
//! is a `const`. A `dyn()` field is an object with a required type member and
//! `x-r8.dyn` naming the trait. R8's facts that JSON Schema cannot state
//! travel as annotations under `x-r8`: rules, units, and host-only fields.
//!
//! The schema accepts every valid value. It rejects the errors JSON Schema
//! expresses: a value of the wrong kind, a missing or unknown member, a
//! bound, a pattern, a length, or a level. Rules, distinct keys of an
//! ordered map, column lengths, and the canonical forms of dates and bytes
//! are left to R8's validation.

use std::collections::BTreeMap;

use serde_json::{Map, Value, json};

use crate::ir::{FieldDef, FieldType, RuleDef, TypeDef, VariantKind};
use crate::name::QualifiedName;
use crate::rules::TypeLookup;

const INT_MIN: i64 = -2_147_483_647;
const INT_MAX: i64 = 2_147_483_647;

struct Writer<'a> {
    lookup: &'a dyn TypeLookup,
    defs: BTreeMap<String, Value>,
    /// Write a `dyn()` field as the closed list of the registered
    /// implementors, and a `subtype` field as the struct and its
    /// registered descendants.
    closed: bool,
}

fn reference(q: &QualifiedName) -> Value {
    json!({"$ref": format!("#/$defs/{q}")})
}

fn nullable(s: Value) -> Value {
    json!({"anyOf": [s, {"type": "null"}]})
}

fn with_description(mut s: Value, d: &str) -> Value {
    if !d.is_empty()
        && let Value::Object(m) = &mut s
    {
        m.insert("description".into(), Value::from(d));
    }
    s
}

fn rules_annotation(rules: &[RuleDef]) -> Option<Value> {
    (!rules.is_empty()).then(|| serde_json::to_value(rules).expect("rules serialize"))
}

impl Writer<'_> {
    /// The schema of a field type, for a present value.
    fn ty(&mut self, ft: &FieldType) -> Value {
        match ft {
            FieldType::Logical => json!({"type": "boolean"}),
            FieldType::Integer(c) => {
                let mut s = json!({"type": "integer", "minimum": INT_MIN, "maximum": INT_MAX});
                let m = s.as_object_mut().unwrap();
                if let Some(x) = c.min() {
                    m.insert("minimum".into(), json!(x));
                }
                if let Some(x) = c.max() {
                    m.insert("maximum".into(), json!(x));
                }
                if let Some(x) = c.exclusive_min() {
                    m.insert("exclusiveMinimum".into(), json!(x));
                }
                if let Some(x) = c.exclusive_max() {
                    m.insert("exclusiveMaximum".into(), json!(x));
                }
                if let Some(v) = &c.values {
                    m.insert("enum".into(), json!(v));
                }
                s
            }
            FieldType::Double(c) => {
                let mut n = json!({"type": "number"});
                let m = n.as_object_mut().unwrap();
                for (k, v) in [
                    ("minimum", c.min()),
                    ("maximum", c.max()),
                    ("exclusiveMinimum", c.exclusive_min()),
                    ("exclusiveMaximum", c.exclusive_max()),
                ] {
                    if let Some(x) = v {
                        m.insert(k.into(), json!(x));
                    }
                }
                if let Some(v) = &c.values {
                    m.insert("enum".into(), json!(v));
                }
                // The strings of the values that JSON numbers do not hold.
                let texts: Vec<&str> = [
                    (f64::INFINITY, "Infinity"),
                    (f64::NEG_INFINITY, "-Infinity"),
                    (f64::NAN, "NaN"),
                ]
                .into_iter()
                .filter(|(x, _)| c.admits(*x))
                .map(|(_, t)| t)
                .collect();
                if texts.is_empty() {
                    n
                } else {
                    json!({"anyOf": [n, {"enum": texts}]})
                }
            }
            FieldType::Character(c) => {
                let mut s = json!({"type": "string"});
                let m = s.as_object_mut().unwrap();
                if let Some(p) = &c.pattern {
                    m.insert("pattern".into(), json!(p));
                }
                if let Some(v) = &c.values {
                    m.insert("enum".into(), json!(v));
                }
                s
            }
            FieldType::Raw => json!({
                "type": "string",
                "contentEncoding": "base64",
                "pattern": "^(?:[A-Za-z0-9+/]{4})*(?:[A-Za-z0-9+/]{2}==|[A-Za-z0-9+/]{3}=)?$"
            }),
            FieldType::Date => {
                json!({"type": "string", "format": "date", "pattern": "^[0-9]{4}-[0-9]{2}-[0-9]{2}$"})
            }
            FieldType::Posixct => json!({
                "type": "string",
                "format": "date-time",
                "pattern": "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}\\.[0-9]{6}Z$"
            }),
            FieldType::Factor { levels, .. } => self.factor(levels.as_deref(), false),
            FieldType::Matrix {
                element,
                nrow,
                ncol,
            } => {
                let shape = json!({
                    "type": "array",
                    "prefixItems": [dim(*nrow), dim(*ncol)],
                    "minItems": 2,
                    "maxItems": 2
                });
                self.dense(element, shape)
            }
            FieldType::Array { element, dim: d } => {
                let shape = match d {
                    Some(d) => json!({"const": d}),
                    None => json!({"type": "array", "items": {"type": "integer", "minimum": 0}}),
                };
                self.dense(element, shape)
            }
            FieldType::SparseMatrix {
                element,
                nrow,
                ncol,
            } => {
                let ints = json!({"type": "array", "items": {"type": "integer", "minimum": 0}});
                json!({
                    "type": "object",
                    "properties": {
                        "shape": {"type": "array", "prefixItems": [dim(*nrow), dim(*ncol)], "minItems": 2, "maxItems": 2},
                        "indptr": ints,
                        "indices": ints,
                        "data": {"type": "array", "items": self.ty(element)}
                    },
                    "required": ["shape", "indptr", "indices", "data"],
                    "additionalProperties": false,
                    "x-r8": {"sparse": "csc"}
                })
            }
            FieldType::MatrixOf { of } => {
                let fields = self.struct_fields(of);
                let element = fields
                    .first()
                    .map(|f| f.ty.clone())
                    .unwrap_or(FieldType::Logical);
                let shape = json!({"type": "array", "prefixItems": [dim(None), dim(Some(fields.len() as u32))], "minItems": 2, "maxItems": 2});
                self.dense(&element, shape)
            }
            FieldType::Table {
                columns,
                open,
                rules,
            } => self.table(columns, *open, rules),
            FieldType::TableOf { of } => {
                let fields = self.struct_fields(of);
                self.table(&fields, false, &[])
            }
            FieldType::Json => json!({}),
            FieldType::HostClass { class, .. } => {
                json!({"type": "null", "x-r8": {"host_only": class}})
            }
            FieldType::Vec {
                element,
                min_length,
                max_length,
                unique,
            } => {
                let mut s = json!({"type": "array", "items": self.element(element)});
                let m = s.as_object_mut().unwrap();
                if let Some(x) = min_length {
                    m.insert("minItems".into(), json!(x));
                }
                if let Some(x) = max_length {
                    m.insert("maxItems".into(), json!(x));
                }
                if *unique {
                    m.insert("uniqueItems".into(), json!(true));
                }
                s
            }
            FieldType::Option { inner, .. } => nullable(self.ty(inner)),
            FieldType::BtreeMap { value, keys } => json!({
                "type": "object",
                "propertyNames": key_schema(keys),
                "additionalProperties": self.element(value)
            }),
            FieldType::IndexMap { value, keys } => json!({
                "type": "array",
                "items": {
                    "type": "object",
                    "properties": {"name": key_schema(keys), "value": self.element(value)},
                    "required": ["name", "value"],
                    "additionalProperties": false
                },
                "x-r8": {"index_map": true}
            }),
            FieldType::Named { name } => {
                self.define(name);
                reference(name)
            }
            FieldType::Dyn { r#trait } if self.closed => {
                let member = self.lookup_tag(r#trait);
                let alternatives: Vec<Value> = crate::rules::implementors(self.lookup, r#trait)
                    .iter()
                    .map(|t| {
                        let written = self
                            .lookup
                            .tag_value(r#trait, t)
                            .unwrap_or_else(|| t.to_string());
                        self.alternative(t, &member, &written)
                    })
                    .collect();
                json!({
                    "anyOf": alternatives,
                    "x-r8": {"dyn": r#trait.to_string(), "closed": "registered"}
                })
            }
            FieldType::Dyn { r#trait } => json!({
                "type": "object",
                "required": [self.lookup_tag(r#trait)],
                "x-r8": {"dyn": r#trait.to_string()}
            }),
            FieldType::Subtype { of } if self.closed => {
                let mut types = vec![of.clone()];
                types.extend(self.lookup.descendants_of(of));
                let alternatives: Vec<Value> = types
                    .iter()
                    .map(|t| self.alternative(t, "$type", &t.to_string()))
                    .collect();
                json!({
                    "anyOf": alternatives,
                    "x-r8": {"subtype": of.to_string(), "closed": "registered"}
                })
            }
            FieldType::Subtype { of } => json!({
                "type": "object",
                "required": ["$type"],
                "x-r8": {"subtype": of.to_string()}
            }),
        }
    }

    /// An element of a collection: `null` only for `option()` elements.
    fn element(&mut self, ft: &FieldType) -> Value {
        self.ty(ft)
    }

    fn lookup_tag(&self, tr: &QualifiedName) -> String {
        self.lookup.type_member(tr)
    }

    fn struct_fields(&self, of: &QualifiedName) -> Vec<FieldDef> {
        match self.lookup.find(of) {
            Some(TypeDef::Struct(s)) => s.fields.clone(),
            _ => Vec::new(),
        }
    }

    fn factor(&mut self, levels: Option<&[Box<str>]>, cells_optional: bool) -> Value {
        let maybe_null = |s: Value| if cells_optional { nullable(s) } else { s };
        match levels {
            Some(l) => json!({"type": "array", "items": maybe_null(json!({"enum": l}))}),
            None => json!({
                "type": "object",
                "properties": {
                    "levels": {"type": "array", "items": {"type": "string"}, "uniqueItems": true},
                    "codes": {"type": "array", "items": maybe_null(json!({"type": "integer", "minimum": 1}))}
                },
                "required": ["levels", "codes"],
                "additionalProperties": false
            }),
        }
    }

    fn dense(&mut self, element: &FieldType, shape: Value) -> Value {
        json!({
            "type": "object",
            "properties": {
                "shape": shape,
                "data": {"type": "array", "items": self.element(element)},
                "dimnames": {"type": "array", "items": {"anyOf": [{"type": "null"}, {"type": "array", "items": {"type": "string"}}]}}
            },
            "required": ["shape", "data"],
            "additionalProperties": false
        })
    }

    fn column(&mut self, c: &FieldDef, unique: bool) -> Value {
        let (inner, optional) = match &c.ty {
            FieldType::Option { inner, .. } => (inner.as_ref(), true),
            other => (other, false),
        };
        let s = match inner {
            FieldType::Factor { levels, .. } => self.factor(levels.as_deref(), optional),
            other => {
                let cell = self.ty(other);
                let cell = if optional { nullable(cell) } else { cell };
                let mut s = json!({"type": "array", "items": cell});
                if unique {
                    s["x-r8"] = json!({"unique": true});
                }
                s
            }
        };
        with_description(s, &c.description)
    }

    fn table<C: crate::ir::Column>(
        &mut self,
        columns: &[C],
        open: bool,
        rules: &[RuleDef],
    ) -> Value {
        let mut props = Map::new();
        props.insert("$length".into(), json!({"type": "integer", "minimum": 0}));
        let mut required = vec![Value::from("$length")];
        for c in columns {
            props.insert(
                c.field().name.to_string(),
                self.column(c.field(), c.unique()),
            );
            if c.required() {
                required.push(Value::from(c.field().name.as_str()));
            }
        }
        if open {
            props.insert("$columns".into(), json!({"type": "object"}));
        }
        let mut s = json!({
            "type": "object",
            "properties": props,
            "required": required,
            "additionalProperties": if open { json!({"anyOf": [{"type": "array"}, {"type": "object"}]}) } else { json!(false) }
        });
        if let Some(r) = rules_annotation(rules) {
            s["x-r8"] = json!({"row_rules": r});
        }
        s
    }

    /// The schema of a field, absence included.
    fn field(&mut self, f: &FieldDef) -> Value {
        let s = self.ty(&f.ty);
        let s = if f.default.is_some() && !matches!(f.ty, FieldType::Option { .. }) {
            nullable(s)
        } else {
            s
        };
        let mut s = with_description(s, &f.description);
        if let Some(u) = &f.unit {
            s["x-r8"] = json!({"unit": u});
        }
        if let Some(d) = &f.default {
            s["default"] = d.to_value();
        }
        s
    }

    /// An object of fields, with `extra` members admitted beside them.
    fn object(&mut self, fields: &[FieldDef], rules: &[RuleDef], extra: &[(&str, Value)]) -> Value {
        let mut props = Map::new();
        let mut required = Vec::new();
        for f in fields {
            props.insert(f.name.to_string(), self.field(f));
            if !matches!(f.ty, FieldType::Option { .. }) && f.default.is_none() {
                required.push(Value::from(f.name.as_str()));
            }
        }
        for (k, v) in extra {
            props.insert((*k).to_string(), v.clone());
            if v.get("const").is_some() {
                required.push(Value::from(*k));
            }
        }
        let mut s = json!({"type": "object", "properties": props, "additionalProperties": false});
        if !required.is_empty() {
            s["required"] = Value::Array(required);
        }
        if let Some(r) = rules_annotation(rules) {
            s["x-r8"] = json!({"rules": r});
        }
        s
    }

    /// One alternative of a closed `dyn()` field: the value of the type `t`
    /// with its type member `member`, the constant `written`; a unit-only
    /// enum is an object of the type member and its own tag.
    fn alternative(&mut self, t: &QualifiedName, member: &str, written: &str) -> Value {
        let named = json!({"const": written});
        if let Some(TypeDef::Enum(e)) = self.lookup.find(t).cloned()
            && e.variants
                .iter()
                .all(|v| matches!(v.kind, VariantKind::Unit))
        {
            let names: Vec<&str> = e.variants.iter().map(|v| v.name.as_str()).collect();
            let mut s = self.object(&[], &[], &[(member, named)]);
            s["properties"][e.tag.as_str()] = json!({"enum": names});
            if let Some(Value::Array(r)) = s.get_mut("required") {
                r.push(Value::from(e.tag.as_str()));
            }
            return s;
        }
        self.named(t, &[(member, named)])
    }

    fn define(&mut self, q: &QualifiedName) {
        let key = q.to_string();
        if self.defs.contains_key(&key) {
            return;
        }
        self.defs.insert(key.clone(), Value::Null);
        let schema = self.named(q, &[]);
        self.defs.insert(key, schema);
    }

    /// The schema of a named type, with `extra` members admitted on a struct.
    fn named(&mut self, q: &QualifiedName, extra: &[(&str, Value)]) -> Value {
        match self.lookup.find(q).cloned() {
            None => json!({"x-r8": {"unknown_type": q.to_string()}}),
            Some(TypeDef::Struct(s)) => {
                with_description(self.object(&s.fields, &s.rules, extra), &s.description)
            }
            Some(TypeDef::Table(t)) => {
                let mut s = self.table(&t.columns, t.open, &t.rules);
                if let Some(p) = s["properties"].as_object_mut() {
                    if t.open {
                        p.insert(
                            "$order".into(),
                            json!({"type": "array", "items": {"type": "string"}}),
                        );
                        p.insert("$facts".into(), json!({"type": "object"}));
                    }
                    for (k, v) in extra {
                        p.insert((*k).into(), v.clone());
                    }
                }
                with_description(s, &t.description)
            }
            Some(TypeDef::Enum(e)) => {
                if e.variants
                    .iter()
                    .all(|v| matches!(v.kind, VariantKind::Unit))
                {
                    let names: Vec<&str> = e.variants.iter().map(|v| v.name.as_str()).collect();
                    return with_description(json!({"enum": names}), &e.description);
                }
                let tag = e.tag.as_str();
                let alternatives: Vec<Value> = e
                    .variants
                    .iter()
                    .map(|v| {
                        let mut ex: Vec<(&str, Value)> = extra.to_vec();
                        ex.push((tag, json!({"const": v.name.as_str()})));
                        let s = match &v.kind {
                            VariantKind::Unit => self.object(&[], &[], &ex),
                            VariantKind::Struct { fields, rules } => {
                                self.object(fields, rules, &ex)
                            }
                            VariantKind::Newtype { of } => match self.lookup.find(of).cloned() {
                                Some(TypeDef::Struct(s)) => self.object(&s.fields, &s.rules, &ex),
                                _ => json!({"x-r8": {"unknown_type": of.to_string()}}),
                            },
                        };
                        with_description(s, &v.description)
                    })
                    .collect();
                with_description(
                    json!({"anyOf": alternatives, "x-r8": {"tag": tag}}),
                    &e.description,
                )
            }
        }
    }
}

fn dim(n: Option<u32>) -> Value {
    match n {
        Some(n) => json!({"const": n}),
        None => json!({"type": "integer", "minimum": 0}),
    }
}

fn key_schema(keys: &crate::ir::CharacterType) -> Value {
    let mut s = json!({"type": "string", "minLength": 1});
    if let Some(p) = &keys.pattern {
        s["pattern"] = json!(p);
    }
    if let Some(v) = &keys.values {
        s["enum"] = json!(v);
    }
    s
}

/// The JSON Schema 2020-12 of the values of a named type.
pub fn json_schema(ty: &QualifiedName, lookup: &dyn TypeLookup) -> Value {
    json_schema_with(ty, lookup, false)
}

/// [`json_schema`]; with `closed`, a `dyn(Trait)` field is the closed
/// list (`anyOf`) of the trait's registered implementors, each with its type
/// member as a constant, and a `subtype` field the struct and its
/// registered descendants, each with `"$type"` as a constant.
pub fn json_schema_with(ty: &QualifiedName, lookup: &dyn TypeLookup, closed: bool) -> Value {
    let mut w = Writer {
        lookup,
        defs: BTreeMap::new(),
        closed,
    };
    let envelope = [
        ("$type", json!({"type": "string"})),
        (
            "$types",
            json!({"type": "object", "additionalProperties": {"type": "integer", "minimum": 1}}),
        ),
        (
            "$declarations",
            json!({"type": "object", "additionalProperties": {"type": "string", "pattern": "^sha256:[0-9a-f]{64}$"}}),
        ),
        ("$ir", json!({"type": "array", "items": {"type": "object"}})),
    ];
    let root = match lookup.find(ty) {
        Some(TypeDef::Struct(_) | TypeDef::Table(_)) => w.named(ty, &envelope),
        _ => {
            w.define(ty);
            reference(ty)
        }
    };
    // `define` also writes the root type's own entry when another type
    // reaches it.
    let mut out = match root {
        Value::Object(m) => m,
        other => {
            let mut m = Map::new();
            m.insert("allOf".into(), json!([other]));
            m
        }
    };
    out.insert(
        "$schema".into(),
        json!("https://json-schema.org/draft/2020-12/schema"),
    );
    out.insert("title".into(), json!(ty.to_string()));
    if !w.defs.is_empty() {
        out.insert("$defs".into(), Value::Object(w.defs.into_iter().collect()));
    }
    Value::Object(out)
}
