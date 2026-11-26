//! Declaration checks over one IR document.
//!
//! [`check_document`] reports every problem a document holds on its own:
//! names, duplicates, versions, and the shape of each field type. Checks that
//! need other documents (an impl's trait, the fields of `type_table(S)`, the
//! tag members of the traits a type implements) run when the registry holds
//! every declaration, and the rule dialect is checked in M1.

use std::collections::HashSet;

use crate::validate::MAX_NESTING;

use super::decl::{
    ColumnDef, FieldDef, MethodDef, ParamDef, ReturnType, RuleDef, StructDef, TableDef, TraitDef,
    VariantKind,
};
use super::document::{IrDocument, TypeDef};
use super::types::{FieldType, Resolve};
use crate::name::{Name, QualifiedName, is_host_keyword, is_syntactic_r_name};
use crate::rules::{CompiledRule, RowRule, check_computed};

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

impl std::fmt::Display for DeclError {
    /// The problem as one line of a host-neutral report: the message, the
    /// code, and the path.
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.path.is_empty() {
            write!(f, "{} [{}]", self.message, self.code)
        } else {
            write!(f, "{} [{} at {}]", self.message, self.code, self.path)
        }
    }
}

/// The report of a declaration's problems, one line each, as a host
/// without its own rendering writes it: "Declaration of `ns::T` failed:".
pub fn report(qname: &str, errors: &[DeclError]) -> String {
    let mut out = format!("Declaration of `{qname}` failed:");
    for e in errors {
        out.push_str("\n- ");
        out.push_str(&e.to_string());
    }
    out
}

/// Where a list of fields stands, which decides the members a field takes.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum FieldsAt {
    /// The fields of a struct.
    Struct,
    /// The fields of an enum's struct variant.
    Variant,
    /// The columns of a table.
    Column,
}

struct Checker<'a> {
    doc: &'a IrDocument,
    errors: Vec<DeclError>,
    /// The document's types by name, for `check_defaults()`; `None` where
    /// the caller runs it with its own lookup, or not at all.
    defaults: Option<Indexed<'a>>,
}

/// A document's types indexed by name: the lookup of `check_defaults()` over
/// a whole document, so that each find takes constant time.
struct Indexed<'a> {
    doc: &'a IrDocument,
    types: std::collections::HashMap<&'a str, usize>,
}

impl<'a> Indexed<'a> {
    fn new(doc: &'a IrDocument) -> Indexed<'a> {
        Indexed {
            doc,
            types: doc
                .types
                .iter()
                .enumerate()
                .map(|(i, t)| (t.name().as_str(), i))
                .collect(),
        }
    }
}

impl crate::rules::TypeLookup for Indexed<'_> {
    fn find(&self, name: &QualifiedName) -> Option<&TypeDef> {
        if name.namespace != self.doc.namespace {
            return None;
        }
        self.types
            .get(name.name.as_str())
            .map(|&i| &self.doc.types[i])
    }

    fn implementor(
        &self,
        tr: &QualifiedName,
        value: &serde_json::Value,
    ) -> Result<(QualifiedName, String), String> {
        self.doc.implementor(tr, value)
    }

    fn type_member(&self, tr: &QualifiedName) -> String {
        self.doc.type_member(tr)
    }

    fn impls_of(&self, tr: &QualifiedName) -> Vec<&super::decl::ImplDef> {
        self.doc.impls_of(tr)
    }

    fn trait_of(&self, tr: &QualifiedName) -> Option<&TraitDef> {
        self.doc.trait_of(tr)
    }
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

    /// Reports a cycle among computed fields, which no order can resolve.
    fn computed_cycles(&mut self, fields: &[FieldDef], path: &str) {
        let computed: Vec<(usize, Vec<String>)> = fields
            .iter()
            .enumerate()
            .filter_map(|(i, f)| match &f.resolve {
                Some(Resolve::Computed { expr }) => Some((i, crate::rules::reads(expr))),
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
                    &format!("{path}/{i}/resolve/expr"),
                    format!(
                        "computed field `{}` depends on itself through other computed fields",
                        names[k]
                    ),
                );
            }
        }
    }

    /// Checks fields at `at`: the fields of a struct or a struct variant admit
    /// a fill policy.
    fn fields(&mut self, fields: &[FieldDef], path: &str, at: FieldsAt) {
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
            if !type_fits(&f.ty, MAX_NESTING) {
                self.too_deep(&format!("{fpath}/type"));
                continue;
            }
            self.field_type(&f.ty, &format!("{fpath}/type"), true);
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
                .filter(|e| e.code != "unknown_type")
                {
                    self.report(
                        "default_invalid",
                        &format!("{fpath}/default{}", e.path),
                        format!("the default of `{}` is invalid: {}", f.name, e.message),
                    );
                }
            }
            if let Some(r) = &f.resolve {
                if !matches!(at, FieldsAt::Struct | FieldsAt::Variant) {
                    self.report(
                        "resolve_position",
                        &format!("{fpath}/resolve"),
                        format!(
                            "field `{}` has a fill policy, which the fields of a struct or a struct variant take",
                            f.name
                        ),
                    );
                } else if !matches!(f.ty, FieldType::Option { .. }) {
                    self.report(
                        "resolve_option",
                        &format!("{fpath}/resolve"),
                        format!(
                            "field `{}` has a fill policy, which applies to an absent value: declare it option()",
                            f.name
                        ),
                    );
                } else if let Resolve::Computed { expr } = r {
                    if matches!(&f.ty, FieldType::Option { inner } if matches!(**inner, FieldType::HostClass { .. }))
                    {
                        self.report(
                            "host_resolve",
                            &format!("{fpath}/resolve"),
                            format!(
                                "field `{}` holds an R value, which is filled from a context; use context = TRUE or none",
                                f.name
                            ),
                        );
                    } else if let Err(errors) = check_computed(expr, fields, self.doc, f) {
                        for e in errors {
                            self.report(
                                e.code,
                                &format!("{fpath}/resolve/expr{}", e.path),
                                e.message,
                            );
                        }
                    }
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

    /// Checks rules in the scope of `scope`: distinct IDs, distinct rules
    /// without IDs, a message that is not blank, and an invariant and
    /// condition that compile against the scope.
    /// Reports a field type or an expression that nests too deep.
    fn too_deep(&mut self, path: &str) {
        self.report(
            "too_deep",
            path,
            format!("a field type and an expression nest at most {MAX_NESTING} levels deep"),
        );
    }

    fn rules(&mut self, rules: &[RuleDef], scope: &[FieldDef], rows: bool, path: &str) {
        for (i, r) in rules.iter().enumerate() {
            for (l, name) in [(Some(&r.invariant), "invariant"), (r.when.as_ref(), "when")] {
                if l.is_some_and(|l| !logic_fits(l, MAX_NESTING)) {
                    self.too_deep(&format!("{path}/{i}/{name}"));
                    return;
                }
            }
        }
        self.unique(
            rules
                .iter()
                .enumerate()
                .filter_map(|(i, r)| Some((r.id.as_ref()?.as_str(), format!("{path}/{i}/id")))),
            "rule ID",
            "duplicate_rule",
        );
        // The same rule declared twice without an ID.
        for (i, r) in rules.iter().enumerate().filter(|(_, r)| r.id.is_none()) {
            if let Some(j) = rules[..i]
                .iter()
                .position(|p| p.id.is_none() && p.invariant == r.invariant && p.when == r.when)
            {
                self.report(
                    "duplicate_rule",
                    &format!("{path}/{i}"),
                    format!("rule {i} repeats rule {j}"),
                );
            }
        }
        for (i, r) in rules.iter().enumerate() {
            if r.message.as_deref().is_some_and(|m| m.trim().is_empty()) {
                self.report(
                    "rule_message_empty",
                    &format!("{path}/{i}/message"),
                    format!("rule {i} has a blank message"),
                );
            }
            let errors = if rows {
                RowRule::compile(r, &scope, self.doc).err()
            } else {
                CompiledRule::compile(r, scope, self.doc).err()
            };
            if let Some(errors) = errors {
                for e in errors {
                    self.report(e.code, &format!("{path}/{i}{}", e.path), e.message);
                }
            }
        }
    }

    /// Checks a field type. `at_field` is `true` where the type is a field's
    /// own type, the one position that admits `option(host_class())`.
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
                if c.infinite && (c.values.is_some() || (c.lower.is_some() && c.upper.is_some())) {
                    self.report(
                        "infinite_unused",
                        path,
                        format!(
                            "`infinite` admits no infinity: the {}",
                            if c.values.is_some() {
                                "values are listed, and finite"
                            } else {
                                "range is bounded on both sides"
                            }
                        ),
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
            FieldType::Table { columns, rules, .. } => self.columns(columns, rules, path),
            // Reached only outside `option()` at a field's own type: the
            // `Option` arm returns before descending into a host class there.
            FieldType::HostClass { class, .. } => {
                self.report(
                    "host_outside_option",
                    path,
                    format!("`host_class(\"{class}\")` is host-only, absent from documents; declare the field `option(host_class(\"{class}\"))`"),
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
            FieldType::Option { inner } => {
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
                    FieldType::HostClass { .. } if at_field => return,
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
            FieldType::IndexMap { value, keys } => {
                self.pattern(keys.pattern.as_deref(), &format!("{path}/keys"));
                self.field_type(value, &format!("{path}/value"), false);
            }
            FieldType::BtreeMap { value, keys } => {
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
            | FieldType::Subtype { .. } => {}
            FieldType::Dyn { r#trait } => {
                // A trait of another document is checked by the registry.
                if let Some(t) = crate::rules::TypeLookup::trait_of(self.doc, r#trait)
                    && t.dispatch_arity() > 1
                {
                    self.report(
                        "dyn_dispatch",
                        path,
                        format!(
                            "`{trait}` dispatches on {} parameters; `dyn()` takes a trait that dispatches on one",
                            t.dispatch_arity()
                        ),
                    );
                }
            }
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
            let name = p.name();
            if name != "..." && !is_syntactic_r_name(name) {
                self.report(
                    "param_name",
                    &ppath,
                    format!("`{name}` is not a syntactic R name"),
                );
            }
            if !seen.insert(name) {
                self.report(
                    "duplicate_param",
                    &ppath,
                    format!("duplicate parameter `{name}`"),
                );
            }
            if let ParamDef::Full(q) = p {
                self.param(q, &ppath);
            }
        }
        let marked: Vec<usize> = (0..m.params.len())
            .filter(|&i| m.params[i].is_dispatch())
            .collect();
        if marked == [0] {
            self.report(
                "dispatch_implicit",
                &format!("{path}/params/0/dispatch"),
                format!(
                    "method `{}` dispatches on its first parameter alone, which is written without a dispatch mark",
                    m.name
                ),
            );
        }
        if m.dispatched().is_empty() {
            self.report(
                "dispatch_none",
                &format!("{path}/params"),
                format!(
                    "method `{}` dispatches on at least one parameter, its first unless others are marked",
                    m.name
                ),
            );
        }
        for i in m.dispatched() {
            if m.params[i].ty().is_some_and(|t| !is_nominal(t)) {
                self.report(
                    "dispatch_param_type",
                    &format!("{path}/params/{i}/type"),
                    format!(
                        "dispatched parameter `{}` has a type that names its values' types: an R8 type, `subtype()`, `dyn()`, or `host_class()`",
                        m.params[i].name()
                    ),
                );
            }
        }
        if m.name.is_replacement() && m.params.last().map(ParamDef::name) != Some("value") {
            self.report(
                "replacement_value",
                &format!("{path}/params"),
                format!("replacement method `{}` takes `value` as its last parameter, as R passes it by that name", m.name),
            );
        }
        if m.name.is_replacement() && m.dispatched().len() > 1 {
            self.report(
                "dispatch_replacement",
                &format!("{path}/params"),
                format!(
                    "replacement method `{}` dispatches on its first parameter only",
                    m.name
                ),
            );
        }
        if m.name.is_replacement() && m.return_type.is_some() {
            self.report(
                "return_type_replacement",
                &format!("{path}/return_type"),
                format!(
                    "replacement method `{}` returns a value of its first argument's type and declares no return type",
                    m.name
                ),
            );
        }
        if let Some(ReturnType::Type(t)) = &m.return_type {
            self.value_type(t, &format!("{path}/return_type"));
        }
        if !m.rules.is_empty() {
            let scope = m.rule_scope();
            self.rules(&m.rules, &scope, false, &format!("{path}/rules"));
        }
    }

    /// The type of a parameter or a return value, which a call holds in the
    /// host: a field type, of which a host class is also admitted outside
    /// `option()`.
    fn value_type(&mut self, t: &FieldType, path: &str) {
        match t {
            FieldType::HostClass { .. } => {}
            FieldType::Option { inner } if matches!(**inner, FieldType::HostClass { .. }) => {}
            _ => self.field_type(t, path, true),
        }
    }

    /// The object form of a parameter: a dispatch mark on a named argument,
    /// a type checked as a field type, and a default that satisfies it.
    fn param(&mut self, q: &super::decl::Param, path: &str) {
        if !q.dispatch
            && q.ty.is_none()
            && q.default.is_none()
            && q.host_default.is_none()
            && q.compute.is_none()
            && q.description.is_empty()
        {
            self.report(
                "param_form",
                path,
                format!(
                    "parameter `{}` has its name only, which is written as a string",
                    q.name
                ),
            );
        }
        if q.dispatch && q.name == "..." {
            self.report(
                "dispatch_dots",
                &format!("{path}/dispatch"),
                "a method dispatches on named parameters, not on `...`".to_string(),
            );
        }
        if q.name == "..." && (q.ty.is_some() || q.default.is_some() || q.host_default.is_some()) {
            self.report(
                "param_dots",
                path,
                "`...` takes no type and no default".to_string(),
            );
        }
        if let Some(t) = &q.ty {
            self.value_type(t, &format!("{path}/type"));
        }
        if q.compute.is_some() {
            self.report(
                "param_compute",
                &format!("{path}/compute"),
                format!(
                    "parameter `{}` has a computed default, which R8 does not evaluate yet",
                    q.name
                ),
            );
        }
        if q.default.is_some() && q.host_default.is_some() {
            self.report(
                "default_host_both",
                path,
                format!(
                    "parameter `{}` has a default in its JSON form or in a host's language, not both",
                    q.name
                ),
            );
        }
        match (&q.ty, &q.default) {
            (None, Some(_)) => self.report(
                "param_default",
                &format!("{path}/default"),
                format!(
                    "the default of parameter `{}` is given with its type",
                    q.name
                ),
            ),
            (Some(t), Some(d)) => {
                for e in crate::validate::validate_field_access(
                    crate::access::JsonOf::of(crate::tree::TreeRef::new(d)),
                    t,
                    self.doc,
                    &mut crate::validate::RuleCache::new(),
                )
                .into_iter()
                .filter(|e| e.code != "unknown_type")
                {
                    self.report(
                        "default_invalid",
                        &format!("{path}/default{}", e.path),
                        format!("the default of `{}` is invalid: {}", q.name, e.message),
                    );
                }
            }
            _ => {}
        }
    }

    fn table_def(&mut self, t: &TableDef, path: &str) {
        self.class_name(&t.name, "type", &format!("{path}/name"));
        self.version(t.version, &format!("{path}/version"));
        self.extensions(t.extensions.keys(), &format!("{path}/extensions"));
        self.columns(&t.columns, &t.rules, path);
        for (i, c) in t.columns.iter().enumerate() {
            if c.default.is_some() {
                self.report(
                    "column_default",
                    &format!("{path}/columns/{i}/default"),
                    format!("column `{}` of a table has no default", c.name),
                );
            }
        }
    }

    /// The columns and the row rules of a table declared at `path`.
    fn columns(&mut self, columns: &[ColumnDef], rules: &[RuleDef], path: &str) {
        let fields: Vec<FieldDef> = columns.iter().map(|c| c.field.clone()).collect();
        self.fields(&fields, &format!("{path}/columns"), FieldsAt::Column);
        self.rules(rules, &fields, true, &format!("{path}/rules"));
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

    fn struct_def(&mut self, s: &StructDef, path: &str) {
        let before = self.errors.len();
        self.class_name(&s.name, "type", &format!("{path}/name"));
        self.version(s.version, &format!("{path}/version"));
        self.fields(&s.fields, &format!("{path}/fields"), FieldsAt::Struct);
        self.extensions(s.extensions.keys(), &format!("{path}/extensions"));
        self.rules(&s.rules, &s.fields, false, &format!("{path}/rules"));
        self.errors.extend(super::lower::check_order(s, path));
        // A parent in another document is checked by the registry.
        if self.errors.len() == before
            && let Some(parent) = &s.extends
            && parent.namespace == self.doc.namespace
        {
            let q = QualifiedName {
                namespace: self.doc.namespace.clone(),
                name: s.name.clone(),
            };
            let found = super::lower::check_extends(s, &q, path, self.doc, false);
            self.errors.extend(found);
        }
        if self.errors.len() == before
            && let Some(lookup) = &self.defaults
        {
            let q = QualifiedName {
                namespace: self.doc.namespace.clone(),
                name: s.name.clone(),
            };
            let found = super::lower::check_defaults(s, &q, path, lookup);
            self.errors.extend(found);
        }
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
            Some(TypeDef::Table(_)) => self.report(
                "newtype_not_struct",
                path,
                format!("`{of}` is a table; a newtype variant wraps a struct"),
            ),
            None => self.report(
                "newtype_unknown",
                path,
                format!("`{of}` is not declared in namespace `{}`", doc.namespace),
            ),
        }
    }

    /// A function: its name, which no type, trait, or generic of the namespace
    /// holds; its parameters, of which none dispatches; its return type, which
    /// is a field type; its rules, over its typed parameters; and, for
    /// `typed: strict`, a type for every parameter.
    fn function_def(&mut self, index: usize) {
        let doc = self.doc;
        let f = &doc.functions[index];
        let path = format!("/functions/{index}");
        let name = f.name.as_str();
        let generic = doc
            .traits
            .iter()
            .find(|t| t.methods.iter().any(|m| m.name.host_identifier() == name));
        if doc.functions[..index].iter().any(|o| o.name == f.name)
            || doc.types.iter().any(|t| t.name().as_str() == name)
            || doc.traits.iter().any(|t| t.name.as_str() == name)
            || generic.is_some()
        {
            self.report(
                "duplicate_function",
                &format!("{path}/name"),
                format!(
                    "`{name}` names another function, a type, a trait, or a generic of namespace `{}`",
                    doc.namespace
                ),
            );
        }
        let mut seen = HashSet::new();
        for (i, p) in f.params.iter().enumerate() {
            let ppath = format!("{path}/params/{i}");
            let pname = p.name();
            if pname != "..." && !is_syntactic_r_name(pname) {
                self.report(
                    "param_name",
                    &ppath,
                    format!("`{pname}` is not a syntactic R name"),
                );
            }
            if !seen.insert(pname) {
                self.report(
                    "duplicate_param",
                    &ppath,
                    format!("duplicate parameter `{pname}`"),
                );
            }
            if p.is_dispatch() {
                self.report(
                    "function_dispatch",
                    &format!("{ppath}/dispatch"),
                    format!("function `{name}` dispatches on no parameter; a generic does"),
                );
            }
            if let ParamDef::Full(q) = p {
                self.param(q, &ppath);
            }
            if f.typed == super::decl::Typed::Strict && (pname == "..." || p.ty().is_none()) {
                self.report(
                    "untyped_param",
                    &ppath,
                    if pname == "..." {
                        format!("function `{name}` is strictly typed, and takes no `...`")
                    } else {
                        format!("parameter `{pname}` of the strictly typed function `{name}` has no type")
                    },
                );
            }
        }
        match &f.return_type {
            Some(ReturnType::SelfType(_)) => self.report(
                "return_type_self",
                &format!("{path}/return_type"),
                format!("function `{name}` dispatches on no argument, whose type `Self` would be"),
            ),
            Some(ReturnType::Type(t)) => self.value_type(t, &format!("{path}/return_type")),
            None => {}
        }
        if let Some(on) = &f.on {
            self.report(
                "function_on",
                &format!("{path}/on"),
                format!(
                    "function `{name}` on the type `{on}`: functions on a type are not declared yet"
                ),
            );
        }
        if !f.rules.is_empty() {
            let scope = f.rule_scope();
            self.rules(&f.rules, &scope, false, &format!("{path}/rules"));
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
            let id = m.name.host_identifier();
            if let Some(other) = t.methods[..i]
                .iter()
                .find(|o| o.name != m.name && o.name.host_identifier() == id)
            {
                self.report(
                    "duplicate_method",
                    &format!("{path}/methods/{i}/name"),
                    format!(
                        "methods `{}` and `{}` share the identifier `{id}` of the other bindings",
                        other.name, m.name
                    ),
                );
            }
        }
        let arity = t.dispatch_arity();
        for (i, m) in t.methods.iter().enumerate() {
            let n = m.dispatched().len();
            if n != 0 && n != arity {
                self.report(
                    "dispatch_arity",
                    &format!("{path}/methods/{i}/params"),
                    format!(
                        "method `{}` dispatches on {n} parameters and `{}` on {arity}; the methods of a trait dispatch on the same number of parameters",
                        m.name, t.methods[0].name
                    ),
                );
            }
        }
        if arity > 1 && t.tag.is_some() {
            self.report(
                "dispatch_tag",
                &format!("{path}/tag"),
                format!(
                    "trait `{}` dispatches on {arity} parameters and has no tag member; a `dyn()` value implements a trait of one",
                    t.name
                ),
            );
        }
        for (i, m) in t.methods.iter().enumerate() {
            self.method(m, &format!("{path}/methods/{i}"));
        }
        self.unique(
            t.required_fields
                .iter()
                .enumerate()
                .map(|(i, f)| (f.name.as_str(), format!("{path}/required_fields/{i}/name"))),
            "field",
            "duplicate_field",
        );
        for (i, f) in t.required_fields.iter().enumerate() {
            self.field_type(&f.ty, &format!("{path}/required_fields/{i}/type"), true);
        }
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
    /// or, for a replacement generic `f<-`, named `f<-` under the trait
    /// `with_f`, the method's host identifier; and no tag, required fields,
    /// or supertraits.
    fn generic_def(&mut self, t: &TraitDef, path: &str) {
        let fits = t.methods.len() == 1 && {
            let m = &t.methods[0].name;
            if m.is_replacement() {
                m.host_identifier() == t.name.as_str()
            } else {
                m.as_str() == t.name.as_str()
            }
        };
        if !fits {
            self.report(
                "generic_methods",
                &format!("{path}/methods"),
                format!(
                    "the trait of generic `{}` has exactly one method, named `{}`, or `f<-` when the trait is `with_f`",
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

/// The one variant name R8 reserves: `Enum@from` names the enum's
/// conversion, as `Type@from` does for a struct.
pub const RESERVED_VARIANT: &str = "from";

/// `true` for a type that names the types of its values, which a
/// dispatched parameter declares: an R8 type, `subtype()`, `dyn()`, or
/// `host_class()`.
pub fn is_nominal(t: &FieldType) -> bool {
    matches!(
        t,
        FieldType::Named { .. }
            | FieldType::Subtype { .. }
            | FieldType::Dyn { .. }
            | FieldType::HostClass { .. }
    )
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
            || doc.functions.iter().any(|f| f.name.as_str() == name)
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
            || doc.functions.iter().any(|f| f.name.as_str() == name)
        {
            // The trait of a replacement generic is named by its method's
            // host identifier, which the message names.
            let setter = doc
                .traits
                .iter()
                .filter(|t| t.name.as_str() == name && t.generic)
                .filter_map(|t| t.methods.first())
                .find(|m| m.name.is_replacement());
            let note = match setter {
                Some(m) => format!("; `{name}` is the trait of generic `{}`", m.name),
                None => String::new(),
            };
            self.report(
                "duplicate_type",
                &format!("/traits/{index}/name"),
                format!("duplicate type or trait `{name}`{note}"),
            );
        }
    }

    /// The methods of the trait at `index` that another trait of the
    /// document also declares.
    fn trait_method_names(&mut self, index: usize) {
        let traits = &self.doc.traits;
        let t = &traits[index];
        for (j, m) in t.methods.iter().enumerate() {
            let id = m.name.host_identifier();
            if let Some((other, om)) = traits
                .iter()
                .enumerate()
                .filter(|(k, _)| *k != index)
                .find_map(|(_, o)| {
                    o.methods
                        .iter()
                        .find(|om| om.name.host_identifier() == id)
                        .map(|om| (o, om))
                })
            {
                // A generic is named by its method, which differs from its
                // trait's name for a replacement generic.
                let what = |x: &TraitDef| {
                    if x.generic {
                        let shown = x
                            .methods
                            .first()
                            .map_or(x.name.as_str(), |m| m.name.as_str());
                        format!("generic `{shown}`")
                    } else {
                        format!("trait `{}`", x.name)
                    }
                };
                let shared = if om.name == m.name {
                    String::new()
                } else {
                    format!(
                        " (as `{}`, by the identifier `{id}` of the other bindings)",
                        om.name
                    )
                };
                self.report(
                    "duplicate_generic",
                    &format!("/traits/{index}/methods/{j}/name"),
                    format!(
                        "`{}` of {} is also a method of {}{shared}; a namespace has one generic per name",
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
            TypeDef::Table(t) => c.table_def(t, path),
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
                    if v.name.as_str() == RESERVED_VARIANT {
                        c.report(
                            "name_reserved",
                            &format!("{vpath}/name"),
                            format!(
                                "variant name `{RESERVED_VARIANT}` is reserved: `{}@{RESERVED_VARIANT}` converts to the enum through its `From` impls",
                                e.name
                            ),
                        );
                    } else {
                        c.class_name(&v.name, "variant", &format!("{vpath}/name"));
                    }
                    if let VariantKind::Newtype { of } = &v.kind {
                        c.newtype(doc, of, &e.tag, &format!("{vpath}/of"), &mut wrapped);
                    }
                    if let VariantKind::Struct { fields, rules } = &v.kind {
                        c.fields(fields, &format!("{vpath}/fields"), FieldsAt::Variant);
                        c.rules(rules, fields, false, &format!("{vpath}/rules"));
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

/// Whether a field type nests at most `levels` deep.
fn type_fits(ft: &FieldType, levels: u32) -> bool {
    let Some(inner) = levels.checked_sub(1) else {
        return false;
    };
    match ft {
        FieldType::Option { inner: t } => type_fits(t, inner),
        FieldType::Vec { element, .. }
        | FieldType::Matrix { element, .. }
        | FieldType::Array { element, .. }
        | FieldType::SparseMatrix { element, .. } => type_fits(element, inner),
        FieldType::BtreeMap { value, .. } | FieldType::IndexMap { value, .. } => {
            type_fits(value, inner)
        }
        FieldType::Table { columns, .. } => columns.iter().all(|c| type_fits(&c.field.ty, inner)),
        _ => true,
    }
}

/// Whether an expression nests at most `levels` deep.
fn logic_fits(l: &crate::ir::Logic, levels: u32) -> bool {
    use crate::ir::Logic;
    let Some(inner) = levels.checked_sub(1) else {
        return false;
    };
    match l {
        Logic::Array(a) | Logic::Call { args: a, .. } => a.iter().all(|x| logic_fits(x, inner)),
        _ => true,
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
        defaults: None,
    };
    c.type_name(index);
    let path = format!("/types/{index}");
    c.type_def(t, &path);
    let q = QualifiedName {
        namespace: doc.namespace.clone(),
        name: t.name().clone(),
    };
    c.errors
        .extend(super::lower::check_default_cycles(&q, &path, doc));
    let prefix = format!("{path}/");
    c.errors
        .retain(|e| e.path == path || e.path.starts_with(&prefix));
    c.errors
}

/// The problems of the function at `index` of `doc`: those that
/// [`check_document`] reports at `/functions/{index}` and below.
pub fn check_function(doc: &IrDocument, index: usize) -> Vec<DeclError> {
    if index >= doc.functions.len() {
        return Vec::new();
    }
    let mut c = Checker {
        doc,
        errors: Vec::new(),
        defaults: None,
    };
    c.function_def(index);
    c.errors
}

/// Checks one IR document and returns every problem found, in document
/// order; an empty vector means the document passes. `check_defaults()` runs
/// for each struct, with the document as its lookup.
pub fn check_document(doc: &IrDocument) -> Vec<DeclError> {
    check_whole(doc, true)
}

/// The checks of [`check_document`] without `check_defaults()`, for a
/// document whose declarations were registered from records, which were
/// checked whole when they were written.
pub fn check_document_records(doc: &IrDocument) -> Vec<DeclError> {
    check_whole(doc, false)
}

fn check_whole(doc: &IrDocument, defaults: bool) -> Vec<DeclError> {
    let mut c = Checker {
        doc,
        errors: Vec::new(),
        defaults: defaults.then(|| Indexed::new(doc)),
    };
    c.names();
    for (i, t) in doc.types.iter().enumerate() {
        c.type_def(t, &format!("/types/{i}"));
    }
    if defaults {
        for (i, t) in doc.types.iter().enumerate() {
            let q = QualifiedName {
                namespace: doc.namespace.clone(),
                name: t.name().clone(),
            };
            let path = format!("/types/{i}");
            c.errors
                .extend(super::lower::check_default_cycles(&q, &path, doc));
        }
    }
    for (i, t) in doc.traits.iter().enumerate() {
        c.trait_def(t, &format!("/traits/{i}"));
    }
    c.method_names(&doc.traits);
    for i in 0..doc.functions.len() {
        c.function_def(i);
    }
    // The tag values of the traits the document declares.
    for t in &doc.traits {
        let q = QualifiedName {
            namespace: doc.namespace.clone(),
            name: t.name.clone(),
        };
        let path_of = |imp: &super::decl::ImplDef| {
            doc.impls
                .iter()
                .position(|i| i == imp)
                .map_or_else(String::new, |i| format!("/impls/{i}"))
        };
        c.errors.extend(super::tags::check_tags(doc, &q, &path_of));
        let impls: Vec<&super::decl::ImplDef> =
            doc.impls.iter().filter(|i| i.r#trait == q).collect();
        let arity = t.dispatch_arity();
        for a in super::dispatch::ambiguities(doc, &impls, arity, &|_, _| true) {
            c.errors.push(ambiguity_error(&impls, &a, &path_of));
        }
    }
    for (i, imp) in doc.impls.iter().enumerate() {
        c.errors.extend(super::dispatch::check_impl(
            doc,
            &doc.namespace,
            imp,
            &format!("/impls/{i}"),
        ));
    }
    c.errors
}

/// The problem of an ambiguous pair of impls, at the path of the first.
pub fn ambiguity_error(
    impls: &[&super::decl::ImplDef],
    a: &super::dispatch::Ambiguity,
    path_of: &dyn Fn(&super::decl::ImplDef) -> String,
) -> DeclError {
    use super::dispatch::tuple_text;
    let (x, y) = (impls[a.first], impls[a.second]);
    let arity = a.meet.len();
    DeclError {
        code: "dispatch_ambiguous",
        path: path_of(x),
        message: format!(
            "the impls of `{}` for ({}) and for ({}) are each more specific in one position, so a call on ({}) is ambiguous; implement `{}` for ({})",
            x.r#trait,
            tuple_text(super::dispatch::positions(x, arity)),
            tuple_text(super::dispatch::positions(y, arity)),
            tuple_text(&a.meet),
            x.r#trait,
            tuple_text(&a.meet)
        ),
    }
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
        defaults: None,
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
