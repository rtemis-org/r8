//! `extends`: the lowering of a child struct onto its parent, and the check
//! that a lowered child agrees with the parent a lookup holds.
//!
//! A child's declaration lists the fields and rules it adds. [`extend`]
//! flattens the parent's fields and rules in front of them, each marked
//! with `inherited_from`, the ancestor that declared it, and applies the
//! child's changes to inherited fields: a default, the removal of an
//! `option()` field's default, `sensitive`, and a description.
//! [`check_extends`] checks a lowered child against the parent:
//! the inherited members equal the parent's, up to those changes.
//! spec: R8/class-system#inheritance

use super::check::DeclError;
use super::decl::{FieldDef, RuleDef, StructDef, VariantKind};
use super::document::TypeDef;
use super::types::FieldType;
use crate::name::{Name, QualifiedName};
use crate::rules::TypeLookup;
use crate::tree::Tree;

/// A child's change to a field it inherits.
#[derive(Debug, Clone, PartialEq)]
pub struct InheritedChange {
    /// The inherited field.
    pub name: Name,
    /// The child's default: `Some(Some(d))` replaces the inherited one, and
    /// `Some(None)` removes it from an `option()` field.
    pub default: Option<Option<Box<Tree>>>,
    /// The child marks the field sensitive.
    pub sensitive: bool,
    /// The child's description, which replaces the inherited one.
    pub description: Option<Box<str>>,
}

/// Lowers `child` onto its parent `parent`, named `parent_name`: the
/// parent's fields and rules, marked with their declaring ancestor, precede
/// the child's own, `changes` apply to the inherited fields, and `extends`
/// names the parent. `child` holds the fields and rules it adds; each error
/// is located in the child's declaration before the lowering, and `child`
/// is unchanged when there is one.
pub fn extend(
    child: &mut StructDef,
    parent_name: &QualifiedName,
    parent: &StructDef,
    changes: &[InheritedChange],
) -> Result<(), Vec<DeclError>> {
    let mut errors = Vec::new();
    let mut report = |code, path: String, message: String| {
        errors.push(DeclError {
            code,
            path,
            message,
        })
    };
    for (i, f) in child.fields.iter().enumerate() {
        if parent.fields.iter().any(|p| p.name == f.name) {
            report(
                "duplicate_field",
                format!("/fields/{i}/name"),
                format!(
                    "field `{}` is inherited from `{parent_name}`; change its default with `defaults =`",
                    f.name
                ),
            );
        }
        if f.inherited_from.is_some() {
            report(
                "extends_order",
                format!("/fields/{i}/inherited_from"),
                format!("field `{}` of a child declaration is its own", f.name),
            );
        }
    }
    for (i, r) in child.rules.iter().enumerate() {
        if let Some(id) = &r.id
            && parent.rules.iter().any(|p| p.id.as_ref() == Some(id))
        {
            report(
                "duplicate_rule",
                format!("/rules/{i}/id"),
                format!(
                    "rule `{id}` is inherited from `{parent_name}`; a child adds rules and replaces none"
                ),
            );
        }
        if r.inherited_from.is_some() {
            report(
                "extends_order",
                format!("/rules/{i}/inherited_from"),
                format!("rule {i} of a child declaration is its own"),
            );
        }
    }
    for (i, c) in changes.iter().enumerate() {
        match parent.fields.iter().find(|p| p.name == c.name) {
            None => {
                let names: Vec<String> = parent
                    .fields
                    .iter()
                    .map(|p| format!("`{}`", p.name))
                    .collect();
                report(
                    "inherited_unknown",
                    format!("/defaults/{i}"),
                    format!(
                        "`{}` is not a field of `{parent_name}`, whose fields are {}",
                        c.name,
                        if names.is_empty() {
                            "none".to_string()
                        } else {
                            names.join(", ")
                        }
                    ),
                );
            }
            Some(p) if matches!(c.default, Some(None)) && !is_option(p) => report(
                "default_removed",
                format!("/defaults/{i}"),
                format!(
                    "`{}` is not an `option()` field; a child removes the default of an `option()` field only",
                    c.name
                ),
            ),
            Some(_) => {}
        }
        if changes[..i].iter().any(|o| o.name == c.name) {
            report(
                "duplicate_field",
                format!("/defaults/{i}"),
                format!("`{}` is named twice", c.name),
            );
        }
    }
    if !errors.is_empty() {
        return Err(errors);
    }
    let mut fields: Vec<FieldDef> = parent
        .fields
        .iter()
        .map(|p| inherited_field(p, parent_name))
        .collect();
    for c in changes {
        let f = fields
            .iter_mut()
            .find(|f| f.name == c.name)
            .expect("checked above");
        if let Some(d) = &c.default {
            f.default = d.clone();
        }
        f.sensitive |= c.sensitive;
        if let Some(d) = &c.description {
            f.description = d.clone();
        }
    }
    fields.append(&mut child.fields);
    let mut rules: Vec<RuleDef> = parent
        .rules
        .iter()
        .map(|r| inherited_rule(r, parent_name))
        .collect();
    rules.append(&mut child.rules);
    child.fields = fields;
    child.rules = rules;
    child.extends = Some(parent_name.clone());
    Ok(())
}

/// The parent's field `p` as a child holds it: marked with the ancestor
/// that declared it.
fn inherited_field(p: &FieldDef, parent_name: &QualifiedName) -> FieldDef {
    let mut f = p.clone();
    f.inherited_from
        .get_or_insert_with(|| Box::new(parent_name.clone()));
    f
}

/// The parent's rule `p` as a child holds it.
fn inherited_rule(p: &RuleDef, parent_name: &QualifiedName) -> RuleDef {
    let mut r = p.clone();
    r.inherited_from
        .get_or_insert_with(|| Box::new(parent_name.clone()));
    r
}

/// Whether the child's inherited field `c` agrees with the parent's field
/// `p`: equal up to the default, a `sensitive` that only tightens, and the
/// description.
fn field_agrees(c: &FieldDef, p: &FieldDef, parent_name: &QualifiedName) -> bool {
    let mut expected = inherited_field(p, parent_name);
    expected.default = c.default.clone();
    expected.description = c.description.clone();
    expected.sensitive = c.sensitive;
    c == &expected
        && (c.sensitive || !p.sensitive)
        && (c.default.is_some() || p.default.is_none() || is_option(p))
}

fn is_option(f: &FieldDef) -> bool {
    matches!(f.ty, super::types::FieldType::Option { .. })
}

/// The order of inherited members within one declaration, which needs no
/// lookup: the fields and rules marked `inherited_from` precede the others,
/// and a struct without `extends` has none.
pub(super) fn check_order(s: &StructDef, path: &str) -> Vec<DeclError> {
    let mut out = Vec::new();
    let fields = s.fields.iter().map(|f| f.inherited_from.is_some());
    let rules = s.rules.iter().map(|r| r.inherited_from.is_some());
    order_of(s, "fields", fields, path, &mut out);
    order_of(s, "rules", rules, path, &mut out);
    out
}

/// Reports each member marked inherited, of the `marks` of the members
/// `what` of `s`, that follows one of the struct's own, or any marked member
/// of a struct without `extends`.
fn order_of(
    s: &StructDef,
    what: &str,
    marks: impl Iterator<Item = bool>,
    path: &str,
    out: &mut Vec<DeclError>,
) {
    let mut own_seen = s.extends.is_none();
    for (i, marked) in marks.enumerate() {
        if !marked {
            own_seen = true;
            continue;
        }
        if own_seen {
            out.push(DeclError {
                code: "extends_order",
                path: format!("{path}/{what}/{i}/inherited_from"),
                message: if s.extends.is_none() {
                    format!("`{}` extends no struct; its {what} are its own", s.name)
                } else {
                    format!("the inherited {what} of `{}` precede its own", s.name)
                },
            });
        }
    }
}

/// The problems of the struct `s`, named `name` and declared at `path`,
/// that need its parent: the parent is a struct the lookup holds, the chain
/// of parents does not return to `s`, and the inherited fields and rules
/// agree with the parent's. With `stale`, as at a package's load, a
/// disagreement is `extends_stale`, a child built against another version of
/// its parent; otherwise it is `extends_field_changed` or
/// `extends_rule_changed`.
pub fn check_extends(
    s: &StructDef,
    name: &QualifiedName,
    path: &str,
    lookup: &dyn TypeLookup,
    stale: bool,
) -> Vec<DeclError> {
    let Some(parent_name) = &s.extends else {
        return Vec::new();
    };
    let mut out = Vec::new();
    let mut report = |code, at: String, message: String| {
        out.push(DeclError {
            code,
            path: at,
            message,
        })
    };
    let at = format!("{path}/extends");
    let parent = match lookup.find(parent_name) {
        Some(TypeDef::Struct(p)) => p,
        Some(TypeDef::Enum(_)) => {
            report(
                "extends_not_struct",
                at,
                format!("`{parent_name}` is an enum; `extends` names a struct"),
            );
            return out;
        }
        Some(TypeDef::Table(_)) => {
            report(
                "extends_not_struct",
                at,
                format!("`{parent_name}` is a table; `extends` names a struct"),
            );
            return out;
        }
        None => {
            report(
                "extends_unknown",
                at,
                format!(
                    "`{parent_name}` is not a registered type; declare it, or load the package that declares it, before `{name}`"
                ),
            );
            return out;
        }
    };
    if lookup.is_subtype(parent_name, name) {
        report(
            "extends_cycle",
            at,
            format!("`{name}` extends `{parent_name}`, which extends `{name}`"),
        );
        return out;
    }
    let stale_message = || {
        format!(
            "`{name}` was built against another declaration of `{parent_name}`; reinstall the package that declares `{name}`"
        )
    };
    let inherited = s
        .fields
        .iter()
        .take_while(|f| f.inherited_from.is_some())
        .count();
    let fields_agree = inherited == parent.fields.len()
        && s.fields[..inherited]
            .iter()
            .zip(&parent.fields)
            .all(|(c, p)| field_agrees(c, p, parent_name));
    if !fields_agree {
        if stale {
            report("extends_stale", format!("{path}/fields"), stale_message());
        } else {
            let at = s.fields[..inherited]
                .iter()
                .zip(&parent.fields)
                .position(|(c, p)| !field_agrees(c, p, parent_name))
                .unwrap_or(inherited.min(parent.fields.len()));
            report(
                "extends_field_changed",
                format!("{path}/fields/{at}"),
                format!(
                    "the inherited fields of `{name}` differ from the fields of `{parent_name}`; a child changes an inherited field's default, description, or `sensitive` only"
                ),
            );
        }
    }
    let inherited = s
        .rules
        .iter()
        .take_while(|r| r.inherited_from.is_some())
        .count();
    let rules_agree = inherited == parent.rules.len()
        && s.rules[..inherited]
            .iter()
            .zip(&parent.rules)
            .all(|(c, p)| c == &inherited_rule(p, parent_name));
    if !rules_agree {
        if stale {
            report("extends_stale", format!("{path}/rules"), stale_message());
        } else {
            report(
                "extends_rule_changed",
                format!("{path}/rules"),
                format!(
                    "the inherited rules of `{name}` differ from the rules of `{parent_name}`; a child adds rules and changes none"
                ),
            );
        }
    }
    out
}

/// A field whose default applies: its type, its index among the fields of the
/// struct or the variant, and the variant of an enum.
type Applied = (QualifiedName, Option<Name>, usize);

/// The fields of the struct or of the variant `variant` of an enum.
fn fields_of<'a>(
    q: &QualifiedName,
    variant: Option<&str>,
    lookup: &'a dyn TypeLookup,
) -> &'a [FieldDef] {
    match lookup.find(q) {
        Some(TypeDef::Struct(s)) => &s.fields,
        Some(TypeDef::Enum(e)) => {
            let kind = e
                .variants
                .iter()
                .find(|v| Some(v.name.as_str()) == variant)
                .map(|v| &v.kind);
            match kind {
                Some(VariantKind::Struct { fields, .. }) => fields,
                Some(VariantKind::Newtype { of }) => match lookup.find(of) {
                    Some(TypeDef::Struct(s)) => &s.fields,
                    _ => &[],
                },
                _ => &[],
            }
        }
        // A column has no default.
        Some(TypeDef::Table(_)) | None => &[],
    }
}

/// The fields that `t`, a value of `ft`, leaves to their defaults.
fn applied(t: &Tree, ft: &FieldType, lookup: &dyn TypeLookup, out: &mut Vec<Applied>) {
    if matches!(t, Tree::Null) {
        return;
    }
    let named = |q: &QualifiedName, out: &mut Vec<Applied>| {
        let variant = match lookup.find(q) {
            Some(TypeDef::Enum(e)) => match t.get(e.tag.as_str()) {
                Some(Tree::Str(s)) => Name::new(&**s).ok(),
                _ => return,
            },
            _ => None,
        };
        let fields = fields_of(q, variant.as_ref().map(Name::as_str), lookup);
        for (i, f) in fields.iter().enumerate() {
            match t.get(f.name.as_str()) {
                Some(x) if !matches!(x, Tree::Null) => applied(x, &f.ty, lookup, out),
                _ if f.default.is_some() => out.push((q.clone(), variant.clone(), i)),
                _ => {}
            }
        }
    };
    match ft {
        FieldType::Option { inner, .. } => applied(t, inner, lookup, out),
        FieldType::Vec { element, .. } => {
            if let Tree::Array(items) = t {
                items.iter().for_each(|x| applied(x, element, lookup, out));
            }
        }
        FieldType::BtreeMap { value, .. } => {
            if let Tree::Object(m) = t {
                m.iter().for_each(|(_, x)| applied(x, value, lookup, out));
            }
        }
        FieldType::IndexMap { value, .. } => {
            if let Tree::Array(pairs) = t {
                let of = pairs.iter().filter_map(|p| p.get("value"));
                of.for_each(|x| applied(x, value, lookup, out));
            }
        }
        FieldType::Named { name } => named(name, out),
        FieldType::Dyn { .. } | FieldType::Subtype { .. } => {
            let v = crate::access::JsonOf::of(crate::tree::TreeRef::new(t));
            if let Some(Ok((q, _))) = crate::access::concrete(v, ft, lookup) {
                named(&q, out);
            }
        }
        _ => {}
    }
}

/// The problems of the defaults of the type `name`, declared at `path`, that hold
/// themselves: a default that holds a value whose absent field has a default that
/// leads back to it has no end.
pub fn check_default_cycles(
    name: &QualifiedName,
    path: &str,
    lookup: &dyn TypeLookup,
) -> Vec<DeclError> {
    let starts: Vec<(Option<Name>, usize, String)> = match lookup.find(name) {
        Some(TypeDef::Struct(s)) => (0..s.fields.len())
            .map(|i| (None, i, format!("{path}/fields/{i}/default")))
            .collect(),
        Some(TypeDef::Enum(e)) => e
            .variants
            .iter()
            .enumerate()
            .flat_map(|(k, v)| {
                let n = match &v.kind {
                    VariantKind::Struct { fields, .. } => fields.len(),
                    _ => 0,
                };
                (0..n).map(move |i| (v.name.clone(), k, i))
            })
            .map(|(v, k, i)| {
                let at = format!("{path}/variants/{k}/fields/{i}/default");
                (Some(v), i, at)
            })
            .collect(),
        Some(TypeDef::Table(_)) | None => Vec::new(),
    };
    let mut out = Vec::new();
    for (variant, i, at) in starts {
        let start: Applied = (name.clone(), variant, i);
        let mut seen: Vec<Applied> = Vec::new();
        let mut next = vec![start.clone()];
        while let Some((q, v, k)) = next.pop() {
            let Some(f) = fields_of(&q, v.as_ref().map(Name::as_str), lookup).get(k) else {
                continue;
            };
            let Some(d) = f.default.as_deref() else {
                continue;
            };
            let mut found = Vec::new();
            applied(d, &f.ty, lookup, &mut found);
            if found.contains(&start) {
                out.push(DeclError {
                    code: "default_recursive",
                    path: at.clone(),
                    message: format!(
                        "the default of `{}` holds a value of `{name}` that leaves `{}` to its default, without end; give it in the default, or declare the field without one",
                        start_name(&start, lookup),
                        start_name(&start, lookup)
                    ),
                });
                break;
            }
            for a in found {
                if !seen.contains(&a) {
                    seen.push(a.clone());
                    next.push(a);
                }
            }
        }
    }
    out
}

/// The name of the field of `a`.
fn start_name(a: &Applied, lookup: &dyn TypeLookup) -> String {
    fields_of(&a.0, a.1.as_ref().map(Name::as_str), lookup)
        .get(a.2)
        .map(|f| f.name.to_string())
        .unwrap_or_default()
}

/// The problems of the struct `s`, named `name` and declared at `path`,
/// whose fields all have defaults or are `option()`: the value of its
/// defaults, filled by `canonical::fill_defaults()`, satisfies its rules, so that
/// `T()` constructs a value. A struct without rules or with a required field
/// has none. A rule whose fields hold no value in the defaults, such as a
/// computed field before resolution, holds.
pub fn check_defaults(
    s: &StructDef,
    name: &QualifiedName,
    path: &str,
    lookup: &dyn TypeLookup,
) -> Vec<DeclError> {
    if s.rules.is_empty() || s.fields.iter().any(FieldDef::is_required) {
        return Vec::new();
    }
    let mut defaults = Tree::from_value(&serde_json::json!({}));
    let ty = super::types::FieldType::Named { name: name.clone() };
    crate::canonical::fill_defaults(&mut defaults, &ty, lookup);
    crate::validate::validate(&defaults.to_value(), name, lookup)
        .into_iter()
        // The struct's own rules report at its root.
        .filter(|e| e.code == "rule_violation" && e.path.is_empty())
        .map(|e| DeclError {
            code: "default_violates_rule",
            path: e
                .rule_index
                .map_or_else(|| format!("{path}/rules"), |i| format!("{path}/rules/{i}")),
            message: format!(
                "the value of the defaults of `{name}`, which `{}()` constructs, violates a rule: {}",
                s.name, e.message
            ),
        })
        .collect()
}
