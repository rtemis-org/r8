//! Declaration checks over one IR document.
//!
//! [`check_document`] reports every problem a document holds on its own:
//! names, duplicates, versions, and the shape of each field type. Checks that
//! need other documents (an impl's trait, the fields of `type_table(S)`, the
//! tag members of the traits a type implements) run when the registry holds
//! every declaration, and the rule dialect is checked in M1.

use std::collections::HashSet;

use super::decl::{FieldDef, MethodDef, RuleDef, StructDef, TraitDef, VariantShape};
use super::document::{IrDocument, TypeDef};
use super::types::{FieldType, Resolve};
use crate::name::{Name, QualifiedName, is_host_keyword, is_syntactic_r_name};
use crate::rules::{CompiledRule, check_computed};
use crate::validate::validate_field;

/// A problem in a declaration, located by a JSON Pointer into the document.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DeclError {
    /// A stable code, such as `name_host_keyword`.
    pub code: &'static str,
    /// A JSON Pointer to the offending member.
    pub path: String,
    pub message: String,
}

struct Checker<'a> {
    doc: &'a IrDocument,
    errors: Vec<DeclError>,
}

impl Checker<'_> {
    fn report(&mut self, code: &'static str, path: &str, message: String) {
        self.errors.push(DeclError {
            code,
            path: path.to_string(),
            message,
        });
    }

    fn class_name(&mut self, name: &Name, what: &str, path: &str) {
        if is_host_keyword(name.as_str()) {
            self.report(
                "name_host_keyword",
                path,
                format!(
                    "{what} name `{name}` is a reserved word of Rust, Python, or ECMAScript, where it becomes a class name; choose another name"
                ),
            );
        }
    }

    fn unique<'a>(
        &mut self,
        names: impl Iterator<Item = (&'a str, String)>,
        what: &str,
        code: &'static str,
    ) {
        let mut seen = HashSet::new();
        for (name, path) in names {
            if !seen.insert(name) {
                self.report(code, &path, format!("duplicate {what} `{name}`"));
            }
        }
    }

    fn version(&mut self, version: u32, path: &str) {
        if version == 0 {
            self.report(
                "version_zero",
                path,
                "a type's version starts at 1".to_string(),
            );
        }
    }

    /// Checks fields; `columns` is `true` for the columns of a table, the one
    /// position that admits `unique` and `optional`.
    fn fields(&mut self, fields: &[FieldDef], path: &str, columns: bool) {
        self.unique(
            fields
                .iter()
                .enumerate()
                .map(|(i, f)| (f.name.as_str(), format!("{path}/{i}/name"))),
            "field",
            "duplicate_field",
        );
        for (i, f) in fields.iter().enumerate() {
            let fpath = format!("{path}/{i}");
            self.field_type(&f.ty, &format!("{fpath}/type"), true);
            if !columns {
                for (flag, set) in [("unique", f.unique), ("optional", f.optional)] {
                    if set {
                        self.report(
                            "column_only",
                            &format!("{fpath}/{flag}"),
                            format!(
                                "`{flag}` applies to a table column; field `{}` is not one",
                                f.name
                            ),
                        );
                    }
                }
            }
            self.extensions(f.extensions.keys(), &format!("{fpath}/extensions"));
            if let Some(d) = &f.default {
                // A default whose type lies outside this document is checked
                // by the registry.
                for e in validate_field(d, &f.ty, self.doc)
                    .into_iter()
                    .filter(|e| e.code != "unresolved_type")
                {
                    self.report(
                        "default_invalid",
                        &format!("{fpath}/default{}", e.path),
                        format!("the default of `{}` is invalid: {}", f.name, e.message),
                    );
                }
            }
            if let FieldType::Option {
                resolve: Resolve::Computed { expr },
                ..
            } = &f.ty
                && let Err(errors) = check_computed(expr, fields, self.doc, f)
            {
                for e in errors {
                    self.report(
                        e.code,
                        &format!("{fpath}/type/resolve/expr{}", e.path),
                        e.message,
                    );
                }
            }
            if let Some(unit) = &f.unit
                && (unit.trim().is_empty() || unit.trim() != unit)
            {
                self.report(
                    "unit_blank",
                    &format!("{fpath}/unit"),
                    format!(
                        "unit of field `{}` is empty or has surrounding spaces",
                        f.name
                    ),
                );
            }
        }
    }

    fn extensions<'a>(&mut self, keys: impl Iterator<Item = &'a String>, path: &str) {
        for key in keys {
            let namespaced = key.split_once(':').is_some_and(|(ns, name)| {
                crate::name::Name::new(ns).is_ok()
                    && !name.is_empty()
                    && !name.contains(char::is_whitespace)
            });
            if !namespaced {
                self.report(
                    "extension_namespace",
                    &format!("{path}/{}", key.replace('~', "~0").replace('/', "~1")),
                    format!("extension `{key}` is not namespaced; write it as `namespace:name`, such as `rtemis:tunable`"),
                );
            }
        }
    }

    /// Checks rules in the scope of `scope`: distinct IDs, a message, and an
    /// invariant and condition that compile against the scope.
    fn rules(&mut self, rules: &[RuleDef], scope: &[FieldDef], path: &str) {
        self.unique(
            rules
                .iter()
                .enumerate()
                .map(|(i, r)| (r.id.as_str(), format!("{path}/{i}/id"))),
            "rule ID",
            "duplicate_rule",
        );
        for (i, r) in rules.iter().enumerate() {
            if r.message.trim().is_empty() {
                self.report(
                    "rule_message_empty",
                    &format!("{path}/{i}/message"),
                    format!("rule `{}` has no message", r.id),
                );
            }
            if let Err(errors) = CompiledRule::compile(r, scope, self.doc) {
                for e in errors {
                    self.report(e.code, &format!("{path}/{i}{}", e.path), e.message);
                }
            }
        }
    }

    /// Checks a field type. `at_field` is `true` where the type is a field's
    /// own type, the one position that admits `option(type_r())`.
    fn field_type(&mut self, ty: &FieldType, path: &str, at_field: bool) {
        match ty {
            FieldType::Integer(c) => {
                if let (Some(lo), Some(hi)) = (c.min, c.max)
                    && lo > hi
                {
                    self.report(
                        "bounds_inverted",
                        path,
                        format!("min {lo} exceeds max {hi}"),
                    );
                }
            }
            FieldType::Double(c) => {
                for (bound, v) in [
                    ("min", c.min),
                    ("max", c.max),
                    ("exclusive_min", c.exclusive_min),
                    ("exclusive_max", c.exclusive_max),
                ] {
                    if let Some(v) = v
                        && !v.is_finite()
                    {
                        self.report(
                            "bound_not_finite",
                            &format!("{path}/{bound}"),
                            format!("{bound} must be a finite number"),
                        );
                    }
                }
                if let (Some(lo), Some(hi)) = (c.min, c.max)
                    && lo > hi
                {
                    self.report(
                        "bounds_inverted",
                        path,
                        format!("min {lo} exceeds max {hi}"),
                    );
                }
            }
            FieldType::Character(c) => self.pattern(c.pattern.as_deref(), path),
            FieldType::Factor { levels, .. } => {
                if let Some(levels) = levels {
                    if levels.is_empty() {
                        self.report(
                            "factor_no_levels",
                            path,
                            "declared levels are empty".to_string(),
                        );
                    }
                    self.unique(
                        levels
                            .iter()
                            .enumerate()
                            .map(|(i, l)| (l.as_str(), format!("{path}/levels/{i}"))),
                        "factor level",
                        "duplicate_level",
                    );
                }
            }
            FieldType::Matrix { element, .. } | FieldType::Array { element, .. } => {
                let ok = element.is_scalar()
                    || matches!(element.as_ref(), FieldType::Option { inner, .. } if inner.is_scalar());
                if !ok {
                    self.report(
                        "matrix_element",
                        &format!("{path}/element"),
                        "a matrix or array element is a scalar base type, or `option()` of one"
                            .to_string(),
                    );
                }
            }
            FieldType::Table { columns, rules, .. } => {
                self.fields(columns, &format!("{path}/columns"), true);
                self.rules(rules, columns, &format!("{path}/rules"));
                for (i, c) in columns.iter().enumerate() {
                    if !is_column_type(&c.ty) {
                        self.report(
                            "table_column",
                            &format!("{path}/columns/{i}/type"),
                            format!(
                                "column `{}` of a table holds a scalar, a factor, or `option()` of a scalar",
                                c.name
                            ),
                        );
                    }
                }
            }
            // Reached only outside `option()` at a field's own type: the
            // `Option` arm returns before descending into `type_r()` there.
            FieldType::R { class } => {
                self.report(
                    "r_outside_option",
                    path,
                    format!("`type_r(\"{class}\")` is runtime-only; declare the field `option(type_r(\"{class}\"))`"),
                );
            }
            FieldType::Vec {
                element,
                min_length,
                max_length,
                ..
            } => {
                if let (Some(lo), Some(hi)) = (min_length, max_length)
                    && lo > hi
                {
                    self.report(
                        "bounds_inverted",
                        path,
                        format!("min_length {lo} exceeds max_length {hi}"),
                    );
                }
                self.field_type(element, &format!("{path}/element"), false);
            }
            FieldType::Option { inner, resolve } => {
                match inner.as_ref() {
                    FieldType::Option { .. } => self.report(
                        "option_option",
                        path,
                        "`option(option(T))` has two meanings of absent; declare `option(T)`"
                            .to_string(),
                    ),
                    FieldType::Json => self.report(
                        "option_json",
                        path,
                        "`option(type_json())` cannot tell an absent value from JSON `null`"
                            .to_string(),
                    ),
                    FieldType::R { .. } if at_field => {
                        if !matches!(resolve, Resolve::None | Resolve::Runtime) {
                            self.report(
                                "r_resolve",
                                &format!("{path}/resolve"),
                                "an R value is filled at run time; use `resolve = \"runtime\"` or none".to_string(),
                            );
                        }
                        return;
                    }
                    _ => {}
                }
                self.field_type(inner, &format!("{path}/inner"), false);
            }
            FieldType::SparseMatrix { element, .. } => {
                if !matches!(element.as_ref(), FieldType::Double(_) | FieldType::Logical) {
                    self.report(
                        "sparse_element",
                        &format!("{path}/element"),
                        "a sparse matrix holds doubles or logicals, as `dgCMatrix` and `lgCMatrix` do".to_string(),
                    );
                }
            }
            FieldType::OrderedMap { value, keys } => {
                self.pattern(keys.pattern.as_deref(), &format!("{path}/keys"));
                self.field_type(value, &format!("{path}/value"), false);
            }
            FieldType::Hashmap { value, keys } => {
                self.pattern(keys.pattern.as_deref(), &format!("{path}/keys"));
                self.field_type(value, &format!("{path}/value"), false);
            }
            FieldType::Logical
            | FieldType::Raw
            | FieldType::Date
            | FieldType::Posixct
            | FieldType::MatrixOf { .. }
            | FieldType::TableOf { .. }
            | FieldType::Json
            | FieldType::Named { .. }
            | FieldType::Dyn { .. } => {}
        }
    }

    fn pattern(&mut self, pattern: Option<&str>, path: &str) {
        if let Some(p) = pattern
            && let Err(e) = regex::Regex::new(p)
        {
            self.report(
                "pattern_invalid",
                &format!("{path}/pattern"),
                format!("pattern does not compile: {e}"),
            );
        }
    }

    fn method(&mut self, m: &MethodDef, path: &str) {
        let mut seen = HashSet::new();
        for (i, p) in m.params.iter().enumerate() {
            let ppath = format!("{path}/params/{i}");
            if p != "..." && !is_syntactic_r_name(p) {
                self.report(
                    "param_name",
                    &ppath,
                    format!("`{p}` is not a syntactic R argument name"),
                );
            }
            if !seen.insert(p.as_str()) {
                self.report(
                    "duplicate_param",
                    &ppath,
                    format!("duplicate argument `{p}`"),
                );
            }
        }
        if m.name.is_replacement() && m.params.last().map(String::as_str) != Some("value") {
            self.report(
                "replacement_value",
                &format!("{path}/params"),
                format!("replacement method `{}` takes `value` as its last argument, as R passes it by that name", m.name),
            );
        }
    }

    fn struct_def(&mut self, s: &StructDef, path: &str) {
        self.class_name(&s.name, "type", &format!("{path}/name"));
        self.version(s.version, &format!("{path}/version"));
        self.fields(&s.fields, &format!("{path}/fields"), false);
        self.extensions(s.extensions.keys(), &format!("{path}/extensions"));
        self.rules(&s.rules, &s.fields, &format!("{path}/rules"));
    }

    /// Checks a newtype variant: the enum wraps each struct at most once, and
    /// a struct declared in the same document is a struct with no field named
    /// as the enum's tag member. A struct in another document is checked when
    /// the registry holds both.
    fn newtype<'a>(
        &mut self,
        doc: &IrDocument,
        of: &'a QualifiedName,
        tag: &Name,
        path: &str,
        wrapped: &mut HashSet<&'a QualifiedName>,
    ) {
        if !wrapped.insert(of) {
            self.report(
                "duplicate_newtype",
                path,
                format!("`{of}` is wrapped by two variants; a value would not name its variant"),
            );
        }
        if of.namespace != doc.namespace {
            return;
        }
        match doc.types.iter().find(|t| t.name() == &of.name) {
            Some(TypeDef::Struct(s)) => {
                if s.fields.iter().any(|f| &f.name == tag) {
                    self.report(
                        "field_is_tag",
                        path,
                        format!("`{of}` has a field named `{tag}`, the enum's tag member, which the flattened variant would write twice"),
                    );
                }
            }
            Some(TypeDef::Enum(_)) => self.report(
                "newtype_not_struct",
                path,
                format!("`{of}` is an enum; a newtype variant wraps a struct"),
            ),
            None => self.report(
                "newtype_unknown",
                path,
                format!("`{of}` is not declared in namespace `{}`", doc.namespace),
            ),
        }
    }

    fn trait_def(&mut self, t: &TraitDef, path: &str) {
        self.class_name(&t.name, "trait", &format!("{path}/name"));
        self.unique(
            t.methods
                .iter()
                .enumerate()
                .map(|(i, m)| (m.name.as_str(), format!("{path}/methods/{i}/name"))),
            "method",
            "duplicate_method",
        );
        for (i, m) in t.methods.iter().enumerate() {
            self.method(m, &format!("{path}/methods/{i}"));
        }
        self.fields(
            &t.required_fields,
            &format!("{path}/required_fields"),
            false,
        );
        if let Some(tag) = &t.tag
            && let Some(i) = t.required_fields.iter().position(|f| &f.name == tag)
        {
            self.report(
                "field_is_tag",
                &format!("{path}/required_fields/{i}/name"),
                format!("required field `{tag}` has the name of the trait's tag member"),
            );
        }
    }
}

/// `true` for the types a table column holds: a scalar base type, a factor,
/// or `option()` of one.
pub fn is_column_type(ty: &FieldType) -> bool {
    let column = |t: &FieldType| t.is_scalar() || matches!(t, FieldType::Factor { .. });
    column(ty) || matches!(ty, FieldType::Option { inner, .. } if column(inner))
}

/// Checks one IR document and returns every problem found, in document
/// order; an empty vector means the document passes.
pub fn check_document(doc: &IrDocument) -> Vec<DeclError> {
    let mut c = Checker {
        doc,
        errors: Vec::new(),
    };
    c.unique(
        doc.types
            .iter()
            .enumerate()
            .map(|(i, t)| (t.name().as_str(), format!("/types/{i}/name")))
            .chain(
                doc.traits
                    .iter()
                    .enumerate()
                    .map(|(i, t)| (t.name.as_str(), format!("/traits/{i}/name"))),
            ),
        "type or trait",
        "duplicate_type",
    );
    for (i, t) in doc.types.iter().enumerate() {
        let path = format!("/types/{i}");
        match t {
            TypeDef::Struct(s) => c.struct_def(s, &path),
            TypeDef::Enum(e) => {
                c.class_name(&e.name, "type", &format!("{path}/name"));
                c.version(e.version, &format!("{path}/version"));
                c.unique(
                    e.variants
                        .iter()
                        .enumerate()
                        .map(|(j, v)| (v.name.as_str(), format!("{path}/variants/{j}/name"))),
                    "variant",
                    "duplicate_variant",
                );
                if e.variants.is_empty() {
                    c.report(
                        "enum_empty",
                        &format!("{path}/variants"),
                        format!("enum `{}` has no variants", e.name),
                    );
                }
                let mut wrapped = HashSet::new();
                for (j, v) in e.variants.iter().enumerate() {
                    let vpath = format!("{path}/variants/{j}");
                    c.class_name(&v.name, "variant", &format!("{vpath}/name"));
                    if let VariantShape::Newtype { of } = &v.shape {
                        c.newtype(doc, of, &e.tag, &format!("{vpath}/of"), &mut wrapped);
                    }
                    if let VariantShape::Struct { fields, rules } = &v.shape {
                        c.fields(fields, &format!("{vpath}/fields"), false);
                        c.rules(rules, fields, &format!("{vpath}/rules"));
                        if let Some(k) = fields.iter().position(|f| f.name == e.tag) {
                            c.report(
                                "field_is_tag",
                                &format!("{vpath}/fields/{k}/name"),
                                format!(
                                    "field `{}` of variant `{}` has the name of the enum's tag member; rename the field or set `tag =`",
                                    e.tag, v.name
                                ),
                            );
                        }
                    }
                }
            }
        }
    }
    for (i, t) in doc.traits.iter().enumerate() {
        c.trait_def(t, &format!("/traits/{i}"));
    }
    c.errors
}
