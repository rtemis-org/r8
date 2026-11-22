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

/// A problem in a declaration, located by a JSON Pointer into the document.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DeclError {
    /// A stable code, such as `name_host_keyword`.
    pub code: &'static str,
    /// A JSON Pointer to the offending member.
    pub path: String,
    /// The message.
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
    /// Reports a cycle among computed fields, which no order can resolve.
    fn computed_cycles(&mut self, fields: &[FieldDef], path: &str) {
        let computed: Vec<(usize, Vec<String>)> = fields
            .iter()
            .enumerate()
            .filter_map(|(i, f)| match &f.ty {
                FieldType::Option {
                    resolve: Resolve::Computed { expr },
                    ..
                } => Some((i, crate::rules::reads(expr))),
                _ => None,
            })
            .collect();
        let names: Vec<&str> = computed
            .iter()
            .map(|(i, _)| fields[*i].name.as_str())
            .collect();
        // Depth-first search for a computed field that reaches itself.
        fn reaches(
            start: &str,
            at: &str,
            deps: &[(usize, Vec<String>)],
            names: &[&str],
            seen: &mut Vec<String>,
        ) -> bool {
            let Some(k) = names.iter().position(|n| *n == at) else {
                return false;
            };
            for d in &deps[k].1 {
                if d == start {
                    return true;
                }
                if !seen.contains(d) {
                    seen.push(d.clone());
                    if reaches(start, d, deps, names, seen) {
                        return true;
                    }
                }
            }
            false
        }
        for (k, (i, _)) in computed.iter().enumerate() {
            if reaches(names[k], names[k], &computed, &names, &mut Vec::new()) {
                self.report(
                    "computed_cycle",
                    &format!("{path}/{i}/type/resolve/expr"),
                    format!(
                        "computed field `{}` depends on itself through other computed fields",
                        names[k]
                    ),
                );
            }
        }
    }

    fn fields(&mut self, fields: &[FieldDef], path: &str, columns: bool) {
        self.computed_cycles(fields, path);
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
            self.extensions(
                f.extensions.iter().flat_map(|m| m.keys()),
                &format!("{fpath}/extensions"),
            );
            if let Some(d) = &f.default {
                // A default whose type lies outside this document is checked
                // by the registry.
                for e in crate::validate::validate_field_access(
                    crate::access::JsonOf::of(crate::tree::TreeRef::new(d)),
                    &f.ty,
                    self.doc,
                    &mut crate::validate::RuleCache::new(),
                )
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
                && (unit.trim().is_empty() || unit.trim() != &**unit)
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
                if let (Some(lo), Some(hi)) = (c.min(), c.max())
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
                    ("min", c.min()),
                    ("max", c.max()),
                    ("exclusive_min", c.exclusive_min()),
                    ("exclusive_max", c.exclusive_max()),
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
                if let (Some(lo), Some(hi)) = (c.min(), c.max())
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
                            .map(|(i, l)| (&**l, format!("{path}/levels/{i}"))),
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
            && crate::validate::compiled_pattern(p).is_none()
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
        if t.generic {
            self.generic_def(t, path);
        }
    }

    /// Checks the trait of a generic: exactly one method, named as the trait,
    /// and no tag, required fields, or supertraits.
    fn generic_def(&mut self, t: &TraitDef, path: &str) {
        if t.methods.len() != 1 || t.methods[0].name.as_str() != t.name.as_str() {
            self.report(
                "generic_methods",
                &format!("{path}/methods"),
                format!(
                    "the trait of generic `{}` has exactly one method, named `{}`",
                    t.name, t.name
                ),
            );
        }
        for (member, present) in [
            ("tag", t.tag.is_some()),
            ("required_fields", !t.required_fields.is_empty()),
            ("supertraits", !t.supertraits.is_empty()),
        ] {
            if present {
                self.report(
                    "generic_member",
                    &format!("{path}/{member}"),
                    format!("the trait of generic `{}` has no `{member}`", t.name),
                );
            }
        }
    }

    /// Reports every method whose name a method of another trait in the
    /// document holds: a package namespace has one generic per name.
    fn method_names(&mut self, traits: &[TraitDef]) {
        for i in 0..traits.len() {
            self.trait_method_names(i);
        }
    }
}

/// `true` for the types a table column holds: a scalar base type, a factor,
/// or `option()` of one.
pub fn is_column_type(ty: &FieldType) -> bool {
    let column = |t: &FieldType| t.is_scalar() || matches!(t, FieldType::Factor { .. });
    column(ty) || matches!(ty, FieldType::Option { inner, .. } if column(inner))
}

impl Checker<'_> {
    /// The uniqueness of the document's names: a type or trait that shares
    /// its name with an earlier type or trait, or a type and a trait of one
    /// name, each reported at its own path, so the check of either
    /// declaration reports the conflict.
    fn names(&mut self) {
        let doc = self.doc;
        for i in 0..doc.types.len() {
            self.type_name(i);
        }
        for i in 0..doc.traits.len() {
            self.trait_name(i);
        }
    }

    /// The name of the type at `index`: unique among the types before it,
    /// and among the traits.
    fn type_name(&mut self, index: usize) {
        let doc = self.doc;
        let name = doc.types[index].name().as_str();
        if doc.types[..index].iter().any(|t| t.name().as_str() == name)
            || doc.traits.iter().any(|t| t.name.as_str() == name)
        {
            self.report(
                "duplicate_type",
                &format!("/types/{index}/name"),
                format!("duplicate type or trait `{name}`"),
            );
        }
    }

    /// The name of the trait at `index`: unique among the traits before it,
    /// and among the types.
    fn trait_name(&mut self, index: usize) {
        let doc = self.doc;
        let name = doc.traits[index].name.as_str();
        if doc.traits[..index].iter().any(|t| t.name.as_str() == name)
            || doc.types.iter().any(|t| t.name().as_str() == name)
        {
            self.report(
                "duplicate_type",
                &format!("/traits/{index}/name"),
                format!("duplicate type or trait `{name}`"),
            );
        }
    }

    /// The methods of the trait at `index` that another trait of the
    /// document also declares.
    fn trait_method_names(&mut self, index: usize) {
        let traits = &self.doc.traits;
        let t = &traits[index];
        for (j, m) in t.methods.iter().enumerate() {
            if let Some(other) = traits
                .iter()
                .enumerate()
                .find(|(k, o)| *k != index && o.methods.iter().any(|om| om.name == m.name))
                .map(|(_, o)| o)
            {
                let what = |x: &TraitDef| {
                    if x.generic {
                        format!("generic `{}`", x.name)
                    } else {
                        format!("trait `{}`", x.name)
                    }
                };
                self.report(
                    "duplicate_generic",
                    &format!("/traits/{index}/methods/{j}/name"),
                    format!(
                        "`{}` of {} is also a method of {}; a namespace has one generic per name",
                        m.name,
                        what(t),
                        what(other)
                    ),
                );
            }
        }
    }

    /// The checks of one type at `path`: its own declaration, with the
    /// document as the lookup of the types it names.
    fn type_def(&mut self, t: &TypeDef, path: &str) {
        let c = self;
        let doc = c.doc;
        match t {
            TypeDef::Struct(s) => c.struct_def(s, path),
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
}

/// The problems of the type at `index` of `doc`: those that
/// [`check_document`] reports at `/types/{index}` and below, from the
/// type's own checks and the uniqueness of its name, in time proportional
/// to the type and to the number of types and traits in the document. A
/// registry checks a type's declaration with it as the declaration enters a
/// namespace.
pub fn check_type(doc: &IrDocument, index: usize) -> Vec<DeclError> {
    let Some(t) = doc.types.get(index) else {
        return Vec::new();
    };
    let mut c = Checker {
        doc,
        errors: Vec::new(),
    };
    c.type_name(index);
    let path = format!("/types/{index}");
    c.type_def(t, &path);
    let prefix = format!("{path}/");
    c.errors
        .retain(|e| e.path == path || e.path.starts_with(&prefix));
    c.errors
}

/// Checks one IR document and returns every problem found, in document
/// order; an empty vector means the document passes.
pub fn check_document(doc: &IrDocument) -> Vec<DeclError> {
    let mut c = Checker {
        doc,
        errors: Vec::new(),
    };
    c.names();
    for (i, t) in doc.types.iter().enumerate() {
        c.type_def(t, &format!("/types/{i}"));
    }
    for (i, t) in doc.traits.iter().enumerate() {
        c.trait_def(t, &format!("/traits/{i}"));
    }
    c.method_names(&doc.traits);
    c.errors
}

/// The problems of the trait at `index` of `doc`: those that
/// [`check_document`] reports at `/traits/{index}` and below, from the
/// trait's own checks, the uniqueness of its name, and the uniqueness of
/// its methods' names among the document's traits, in time proportional to
/// the trait and to the number of types and trait methods in the document.
pub fn check_trait(doc: &IrDocument, index: usize) -> Vec<DeclError> {
    let Some(t) = doc.traits.get(index) else {
        return Vec::new();
    };
    let mut c = Checker {
        doc,
        errors: Vec::new(),
    };
    let path = format!("/traits/{index}");
    c.trait_name(index);
    c.trait_def(t, &path);
    c.trait_method_names(index);
    let prefix = format!("{path}/");
    c.errors
        .retain(|e| e.path == path || e.path.starts_with(&prefix));
    c.errors
}
