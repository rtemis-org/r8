//! What differs between the declaration that a value was made under and the
//! registered one, and whether the value's fields mean the same under both, so
//! that they can be given to the registered type, which validates them.
//!
//! A value keeps its meaning when every field that it holds is declared under
//! the same name, as the same kind of type, in the same unit, and with the same
//! factor levels in the same order, and every field that the registered declaration adds is
//! optional or has a default. A version that differs states that the meaning
//! differs. Factor levels that are added or removed, bounds, rules, defaults, and the order of fields have no part in
//! the meaning: the registered type validates the value.
//! spec: R8/class-system#values-of-another-declaration

use std::collections::HashSet;

use serde_json::Value;

use crate::canonical::{canonical_text, text_hash};
use crate::ir::notation::notation;
use crate::ir::{
    Column, ColumnDef, EnumDef, FieldDef, FieldType, RuleDef, StructDef, TableDef, TypeDef,
    VariantKind,
};
use crate::name::QualifiedName;
use crate::rules::{DocumentSet, TypeLookup};

/// A difference between two declarations of a type.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Change {
    /// The stable code, such as `unit_changed`.
    pub code: &'static str,
    /// The type that differs: the one compared, or one that it holds.
    pub type_name: String,
    /// What differs, such as "`dose` was in `mg`, and is in `mg/kg`".
    pub message: String,
    /// What it means for a value made under the earlier declaration.
    pub effect: Effect,
}

/// What a difference means for a value made under the earlier declaration.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Effect {
    /// The value is one of the registered declaration as it is: a default and
    /// whether a field is shown have no part in a value.
    None,
    /// The fields mean the same, and the registered declaration may hold them in
    /// another order, add fields, or not admit their values; a variant that was
    /// removed is not admitted.
    Form,
    /// The fields may mean something else, or are not declared.
    Meaning,
}

/// The declarations that a text of [`crate::canonical::declaration_text`]
/// holds, whose hash is `hash`; `None` for another text, such as one that a
/// later version of the IR wrote.
pub fn read_declarations(text: &str, hash: &str) -> Option<DocumentSet> {
    let Value::Object(types) = serde_json::from_str(text).ok()? else {
        return None;
    };
    let read = Value::Object(types);
    if text_hash(&read) != hash {
        return None;
    }
    let Value::Object(types) = read else {
        return None;
    };
    let mut out = DocumentSet::default();
    for (name, def) in types {
        if def.is_null() {
            continue;
        }
        let q = QualifiedName::parse(&name).ok()?;
        let def: TypeDef = serde_json::from_value(def).ok()?;
        if def.name() != &q.name {
            return None;
        }
        out.put_type(&q.namespace, def);
    }
    Some(out)
}

/// What differs between the declaration of `ty` in `made` and the registered
/// one, the named types that its fields hold included.
pub fn changes(
    ty: &QualifiedName,
    made: &dyn TypeLookup,
    registered: &dyn TypeLookup,
) -> Vec<Change> {
    let mut c = Comparison {
        made,
        registered,
        seen: HashSet::new(),
        out: Vec::new(),
    };
    c.named(ty);
    c.out
}

struct Comparison<'a> {
    made: &'a dyn TypeLookup,
    registered: &'a dyn TypeLookup,
    seen: HashSet<QualifiedName>,
    out: Vec<Change>,
}

/// The canonical text of a rule without its ID and message.
fn requirement(r: &RuleDef) -> String {
    let when = r.when.as_ref().map(|w| w.to_value());
    canonical_text(&serde_json::json!({"invariant": r.invariant.to_value(), "when": when}))
}

/// What differs between two types of a field: a kind of type or a constraint.
enum TypeChange {
    /// The values have another form or meaning.
    Kind,
    Levels,
    Unit,
    /// A column that the registered table does not declare.
    Column,
    /// Bounds, patterns, sizes, and optionality, which validation checks.
    Constraint,
}

/// What differs between two factors: the meaning, when the levels that both
/// declare are in another order, or one is ordered and the other is not. A level
/// that is added or removed is checked as a bound is.
fn factors(was: Option<&[Box<str>]>, is: Option<&[Box<str>]>, ordered: bool) -> Option<TypeChange> {
    if !ordered {
        return Some(TypeChange::Levels);
    }
    if was == is {
        return None;
    }
    let (Some(w), Some(i)) = (was, is) else {
        return Some(TypeChange::Constraint);
    };
    let before: Vec<_> = w.iter().filter(|l| i.contains(l)).collect();
    let after: Vec<_> = i.iter().filter(|l| w.contains(l)).collect();
    Some(if before == after {
        TypeChange::Constraint
    } else {
        TypeChange::Levels
    })
}

impl Comparison<'_> {
    fn report(&mut self, code: &'static str, of: &str, effect: Effect, message: String) {
        self.out.push(Change {
            code,
            type_name: of.to_string(),
            message,
            effect,
        });
    }

    /// Compares the named type `q`, once.
    fn named(&mut self, q: &QualifiedName) {
        if !self.seen.insert(q.clone()) {
            return;
        }
        let (made, registered) = (self.made, self.registered);
        let Some(was) = made.find(q) else {
            return;
        };
        let of = q.to_string();
        let Some(is) = registered.find(q) else {
            self.report(
                "type_removed",
                &of,
                Effect::Meaning,
                format!("`{q}` is not registered"),
            );
            return;
        };
        match (was, is) {
            (TypeDef::Struct(w), TypeDef::Struct(i)) => self.structs(&of, w, i),
            (TypeDef::Enum(w), TypeDef::Enum(i)) => self.enums(&of, w, i),
            (TypeDef::Struct(_), TypeDef::Enum(_)) => self.report(
                "type_changed",
                &of,
                Effect::Meaning,
                format!("`{q}` was a struct, and is an enum"),
            ),
            (TypeDef::Enum(_), TypeDef::Struct(_)) => self.report(
                "type_changed",
                &of,
                Effect::Meaning,
                format!("`{q}` was an enum, and is a struct"),
            ),
            (TypeDef::Table(w), TypeDef::Table(i)) => self.tables(&of, w, i),
            (TypeDef::Table(_), _) => self.report(
                "type_changed",
                &of,
                Effect::Meaning,
                format!("`{q}` was a table"),
            ),
            (_, TypeDef::Table(_)) => self.report(
                "type_changed",
                &of,
                Effect::Meaning,
                format!("`{q}` is a table"),
            ),
        }
    }

    fn version(&mut self, of: &str, was: u32, is: u32) {
        if was != is {
            self.report(
                "version_changed",
                of,
                Effect::Meaning,
                format!("the version of `{of}` was {was}, and is {is}"),
            );
        }
    }

    fn tables(&mut self, of: &str, was: &TableDef, is: &TableDef) {
        self.version(of, was.version, is.version);
        let cells =
            |t: &TableDef| -> Vec<FieldDef> { t.columns.iter().map(|c| c.field.clone()).collect() };
        self.fields(of, "", &cells(was), &cells(is));
        self.rules(of, "", &was.rules, &is.rules);
        for w in &was.columns {
            if let Some(i) = is.columns.iter().find(|i| i.name == w.name)
                && (w.unique != i.unique || w.required != i.required)
            {
                self.report(
                    "constraint_changed",
                    of,
                    Effect::Form,
                    format!("what column `{}` requires changed", w.name),
                );
            }
        }
        if was.open && !is.open {
            self.report(
                "constraint_changed",
                of,
                Effect::Form,
                format!("`{of}` admitted other columns, and does not"),
            );
        }
    }

    fn structs(&mut self, of: &str, was: &StructDef, is: &StructDef) {
        self.version(of, was.version, is.version);
        self.fields(of, "", &was.fields, &is.fields);
        self.rules(of, "", &was.rules, &is.rules);
    }

    fn enums(&mut self, of: &str, was: &EnumDef, is: &EnumDef) {
        self.version(of, was.version, is.version);
        for w in &was.variants {
            let Some(i) = is.variants.iter().find(|i| i.name == w.name) else {
                self.report(
                    "variant_removed",
                    of,
                    Effect::Form,
                    format!("the variant `{}` was removed", w.name),
                );
                continue;
            };
            let at = format!("{}.", w.name);
            match (&w.kind, &i.kind) {
                (VariantKind::Unit, VariantKind::Unit) => {}
                (
                    VariantKind::Struct { fields, rules },
                    VariantKind::Struct {
                        fields: is_fields,
                        rules: is_rules,
                    },
                ) => {
                    self.fields(of, &at, fields, is_fields);
                    self.rules(of, &at, rules, is_rules);
                }
                (VariantKind::Unit, VariantKind::Struct { fields, .. }) => {
                    self.fields(of, &at, &[], fields);
                }
                (VariantKind::Newtype { of: w }, VariantKind::Newtype { of: i }) if w == i => {
                    self.named(w);
                }
                _ => self.report(
                    "type_changed",
                    of,
                    Effect::Meaning,
                    format!("the variant `{}` was of another kind", w.name),
                ),
            }
        }
        for i in &is.variants {
            if !was.variants.iter().any(|w| w.name == i.name) {
                self.report(
                    "variant_added",
                    of,
                    Effect::Form,
                    format!("the variant `{}` was added", i.name),
                );
            }
        }
        if was.tag != is.tag {
            self.report(
                "tag_changed",
                of,
                Effect::Form,
                format!("the tag was `{}`, and is `{}`", was.tag, is.tag),
            );
        }
    }

    fn fields(&mut self, of: &str, at: &str, was: &[FieldDef], is: &[FieldDef]) {
        for w in was {
            match is.iter().find(|i| i.name == w.name) {
                None => self.report(
                    "field_removed",
                    of,
                    Effect::Meaning,
                    format!("`{at}{}` was removed", w.name),
                ),
                Some(i) => self.field(of, at, w, i),
            }
        }
        for i in is {
            if was.iter().any(|w| w.name == i.name) {
                continue;
            }
            let filled = i.default.is_some() || matches!(i.ty, FieldType::Option { .. });
            let how = match (&i.default, filled) {
                (Some(_), _) => "with a default",
                (None, true) => "as an optional field",
                (None, false) => "without a default",
            };
            let name = &i.name;
            self.report(
                "field_added",
                of,
                if filled {
                    Effect::Form
                } else {
                    Effect::Meaning
                },
                format!("`{at}{name}` was added {how}"),
            );
        }
        let common = |of: &[FieldDef], other: &[FieldDef]| -> Vec<String> {
            let held = of.iter().filter(|f| other.iter().any(|o| o.name == f.name));
            held.map(|f| f.name.to_string()).collect()
        };
        let (before, after) = (common(was, is), common(is, was));
        if before != after {
            let scope = at.strip_suffix('.').unwrap_or(of);
            self.report(
                "order_changed",
                of,
                Effect::Form,
                format!(
                    "the fields of `{scope}` were in the order {}, and are in the order {}",
                    before.join(", "),
                    after.join(", ")
                ),
            );
        }
    }

    fn field(&mut self, of: &str, at: &str, was: &FieldDef, is: &FieldDef) {
        let name = &was.name;
        if let Some(t) = self.types(&was.ty, &is.ty) {
            let (code, effect) = match t {
                TypeChange::Kind => ("type_changed", Effect::Meaning),
                TypeChange::Levels => ("levels_changed", Effect::Meaning),
                TypeChange::Unit => ("unit_changed", Effect::Meaning),
                TypeChange::Column => ("field_removed", Effect::Meaning),
                TypeChange::Constraint => ("constraint_changed", Effect::Form),
            };
            let (before, after) = (notation(&was.ty), notation(&is.ty));
            let message = if before == after {
                format!("the type of `{at}{name}` is declared otherwise")
            } else {
                format!("`{at}{name}` was {before}, and is {after}")
            };
            self.report(code, of, effect, message);
        }
        if was.unit != is.unit {
            let text = |u: &Option<Box<str>>| match u {
                Some(u) => format!("in `{u}`"),
                None => "without a unit".to_string(),
            };
            let message = format!(
                "`{at}{name}` was {}, and is {}",
                text(&was.unit),
                text(&is.unit)
            );
            self.report("unit_changed", of, Effect::Meaning, message);
        }
        if was.default != is.default {
            let message = format!("the default of `{at}{name}` differs");
            self.report("default_changed", of, Effect::None, message);
        }
        if was.resolve != is.resolve {
            let message = format!("how `{at}{name}` is filled differs");
            self.report("fill_changed", of, Effect::Form, message);
        }
        if was.sensitive != is.sensitive {
            let message = format!("whether `{at}{name}` is shown differs");
            self.report("field_changed", of, Effect::None, message);
        }
    }

    fn rules(&mut self, of: &str, at: &str, was: &[RuleDef], is: &[RuleDef]) {
        let of_rules = |rules: &[RuleDef]| -> Vec<String> {
            let mut out: Vec<String> = rules.iter().map(requirement).collect();
            out.sort();
            out
        };
        if of_rules(was) != of_rules(is) {
            let scope = match at.strip_suffix('.') {
                Some(v) => format!("the variant `{v}`"),
                None => format!("`{of}`"),
            };
            self.report(
                "rules_changed",
                of,
                Effect::Form,
                format!("the rules of {scope} differ"),
            );
        }
    }

    fn types(&mut self, was: &FieldType, is: &FieldType) -> Option<TypeChange> {
        use FieldType as T;
        match (was, is) {
            (T::Option { inner: w }, T::Option { inner: i }) => self.types(w, i),
            (T::Option { inner: w }, i) => Some(self.types(w, i).unwrap_or(TypeChange::Constraint)),
            (w, T::Option { inner: i }) => Some(self.types(w, i).unwrap_or(TypeChange::Constraint)),
            (T::Integer(w), T::Integer(i)) => (w != i).then_some(TypeChange::Constraint),
            (T::Double(w), T::Double(i)) => (w != i).then_some(TypeChange::Constraint),
            (T::Character(w), T::Character(i)) => (w != i).then_some(TypeChange::Constraint),
            (
                T::Factor {
                    levels: w,
                    ordered: wo,
                },
                T::Factor {
                    levels: i,
                    ordered: io,
                },
            ) => factors(w.as_deref(), i.as_deref(), wo == io),
            (T::Vec { element: w, .. }, T::Vec { element: i, .. })
            | (T::Matrix { element: w, .. }, T::Matrix { element: i, .. })
            | (T::Array { element: w, .. }, T::Array { element: i, .. })
            | (T::SparseMatrix { element: w, .. }, T::SparseMatrix { element: i, .. })
            | (T::BtreeMap { value: w, .. }, T::BtreeMap { value: i, .. })
            | (T::IndexMap { value: w, .. }, T::IndexMap { value: i, .. }) => {
                let inner = self.types(w, i);
                inner.or_else(|| (was != is).then_some(TypeChange::Constraint))
            }
            (T::Table { columns: w, .. }, T::Table { columns: i, .. }) => {
                let inner = self.columns(w, i);
                inner.or_else(|| (was != is).then_some(TypeChange::Constraint))
            }
            (T::MatrixOf { of: w }, T::MatrixOf { of: i })
            | (T::TableOf { of: w }, T::TableOf { of: i })
            | (T::Named { name: w }, T::Named { name: i })
                if w == i =>
            {
                self.named(w);
                None
            }
            (w, i) if w == i => None,
            _ => Some(TypeChange::Kind),
        }
    }

    /// The change of the columns of a table that matters most.
    fn columns(&mut self, was: &[ColumnDef], is: &[ColumnDef]) -> Option<TypeChange> {
        let mut out = None;
        for w in was {
            let Some(i) = is.iter().find(|i| i.field().name == w.field().name) else {
                return Some(TypeChange::Column);
            };
            if w.field().unit != i.field().unit {
                return Some(TypeChange::Unit);
            }
            match self.types(&w.field().ty, &i.field().ty) {
                None => {}
                Some(TypeChange::Constraint) => out = Some(TypeChange::Constraint),
                Some(t) => return Some(t),
            }
        }
        out
    }
}
