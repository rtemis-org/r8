//! Whether a registered declaration refines a compiled one, so that code
//! compiled against the compiled declaration may read the fields of a value of
//! the registered type by name.
//!
//! The registered declaration of a type refines the compiled one when every
//! value that it admits, seen through the fields of the compiled declaration,
//! is a value that the compiled declaration admits. The order of the fields,
//! added fields, defaults, descriptions, extensions, `sensitive`, and fill
//! policies have no part in it. A variant that the registered enum adds passes:
//! code refuses a value of it when it reads the value.
//! spec: R8/class-system#reads-across-packages

use std::collections::HashSet;

use crate::canonical::canonical_text;
use crate::ir::notation::notation;
use crate::ir::{
    CharacterType, Column, ColumnDef, DoubleType, EnumDef, FieldDef, FieldType, IntegerType,
    RuleDef, StructDef, TypeDef, VariantKind,
};
use crate::name::QualifiedName;
use crate::rules::TypeLookup;

/// A difference for which the registered declaration of a type does not
/// refine the compiled one.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Difference {
    /// The stable code, such as `field_removed`.
    pub code: &'static str,
    /// The type that differs: the one compared, or one that it holds.
    pub type_name: String,
    /// What differs, such as "`x` is compiled as double \[0, Inf), and declared
    /// as double".
    pub message: String,
}

/// The differences for which the declaration of `ty` in `registered` does not
/// refine the one in `compiled`, the named types that its compiled fields hold
/// included; none when it refines it. A type that `compiled` does not declare
/// has none.
pub fn differences(
    ty: &QualifiedName,
    compiled: &dyn TypeLookup,
    registered: &dyn TypeLookup,
) -> Vec<Difference> {
    let mut c = Comparison {
        compiled,
        registered,
        seen: HashSet::new(),
        out: Vec::new(),
    };
    c.named(ty);
    c.out
}

struct Comparison<'a> {
    compiled: &'a dyn TypeLookup,
    registered: &'a dyn TypeLookup,
    seen: HashSet<QualifiedName>,
    out: Vec<Difference>,
}

/// The canonical text of what a rule requires, without its ID and message.
fn requirement(r: &RuleDef) -> String {
    let when = r.when.as_ref().map(|w| w.to_value());
    canonical_text(&serde_json::json!({"invariant": r.invariant.to_value(), "when": when}))
}

/// A rule as a message names it.
fn rule_name(r: &RuleDef) -> String {
    match &r.id {
        Some(id) => format!("rule `{id}`"),
        None => format!("rule {}", canonical_text(&r.invariant.to_value())),
    }
}

/// Whether a lower bound is equal to, or above, the compiled one.
fn lower_within<T: PartialOrd + Copy>(
    have: Option<crate::ir::Bound<T>>,
    want: Option<crate::ir::Bound<T>>,
) -> bool {
    match (have, want) {
        (_, None) => true,
        (None, Some(_)) => false,
        (Some(h), Some(w)) => {
            h.value > w.value || (h.value == w.value && (h.exclusive || !w.exclusive))
        }
    }
}

/// Whether an upper bound is equal to, or below, the compiled one.
fn upper_within<T: PartialOrd + Copy>(
    have: Option<crate::ir::Bound<T>>,
    want: Option<crate::ir::Bound<T>>,
) -> bool {
    match (have, want) {
        (_, None) => true,
        (None, Some(_)) => false,
        (Some(h), Some(w)) => {
            h.value < w.value || (h.value == w.value && (h.exclusive || !w.exclusive))
        }
    }
}

fn integer_within(have: &IntegerType, want: &IntegerType) -> bool {
    if let Some(v) = &have.values {
        // A closed set is within when each value that the type admits is.
        return v
            .iter()
            .filter(|x| have.in_range(**x))
            .all(|x| want.admits(i64::from(*x)));
    }
    want.values.is_none()
        && lower_within(have.lower, want.lower)
        && upper_within(have.upper, want.upper)
}

fn double_within(have: &DoubleType, want: &DoubleType) -> bool {
    if let Some(v) = &have.values {
        return v
            .iter()
            .filter(|x| have.admits(**x))
            .all(|x| want.admits(*x));
    }
    want.values.is_none()
        && (have.finite || !want.finite)
        && lower_within(have.lower, want.lower)
        && upper_within(have.upper, want.upper)
}

fn character_within(have: &CharacterType, want: &CharacterType) -> bool {
    let values = match (&have.values, &want.values) {
        (_, None) => true,
        (None, Some(_)) => false,
        (Some(h), Some(w)) => h.iter().all(|x| w.contains(x)),
    };
    // Whether one regular expression admits a subset of another is not decided.
    values && (want.pattern.is_none() || have.pattern == want.pattern)
}

/// Whether a size is equal to the compiled one, when the compiled type
/// declares it.
fn size_within<T: PartialEq>(have: &Option<T>, want: &Option<T>) -> bool {
    want.is_none() || have == want
}

impl Comparison<'_> {
    fn report(&mut self, code: &'static str, of: &str, message: String) {
        self.out.push(Difference {
            code,
            type_name: of.to_string(),
            message,
        });
    }

    /// Compares the named type `q`, once.
    fn named(&mut self, q: &QualifiedName) {
        if !self.seen.insert(q.clone()) {
            return;
        }
        let compiled = self.compiled;
        let Some(want) = compiled.find(q) else {
            return;
        };
        let of = q.to_string();
        let registered = self.registered;
        let Some(have) = registered.find(q) else {
            self.report("type_removed", &of, format!("`{q}` is not registered"));
            return;
        };
        match (have, want) {
            (TypeDef::Struct(h), TypeDef::Struct(w)) => self.structs(&of, h, w),
            (TypeDef::Enum(h), TypeDef::Enum(w)) => self.enums(&of, h, w),
            (TypeDef::Struct(_), TypeDef::Enum(_)) => self.report(
                "type_changed",
                &of,
                format!("`{q}` is compiled as an enum, and declared as a struct"),
            ),
            (TypeDef::Enum(_), TypeDef::Struct(_)) => self.report(
                "type_changed",
                &of,
                format!("`{q}` is compiled as a struct, and declared as an enum"),
            ),
        }
    }

    fn version(&mut self, of: &str, have: u32, want: u32) {
        if have != want {
            self.report(
                "version_changed",
                of,
                format!("the version of `{of}` is compiled as {want}, and declared as {have}"),
            );
        }
    }

    fn structs(&mut self, of: &str, have: &StructDef, want: &StructDef) {
        self.version(of, have.version, want.version);
        self.fields(of, "", &have.fields, &want.fields);
        self.rules(of, "", &have.rules, &want.rules);
    }

    fn enums(&mut self, of: &str, have: &EnumDef, want: &EnumDef) {
        self.version(of, have.version, want.version);
        for w in &want.variants {
            // A variant that is removed has no values.
            let Some(h) = have.variants.iter().find(|h| h.name == w.name) else {
                continue;
            };
            let at = format!("{}.", w.name);
            match (&h.kind, &w.kind) {
                // Code that knows a unit variant reads no field of it.
                (VariantKind::Unit | VariantKind::Struct { .. }, VariantKind::Unit) => {}
                (
                    VariantKind::Struct { fields, rules },
                    VariantKind::Struct {
                        fields: wanted,
                        rules: required,
                    },
                ) => {
                    self.fields(of, &at, fields, wanted);
                    self.rules(of, &at, rules, required);
                }
                (VariantKind::Newtype { of: h }, VariantKind::Newtype { of: w }) if h == w => {
                    self.named(w);
                }
                _ => self.report(
                    "type_changed",
                    of,
                    format!("the variant `{}` is declared as another kind", w.name),
                ),
            }
        }
    }

    fn fields(&mut self, of: &str, at: &str, have: &[FieldDef], want: &[FieldDef]) {
        for w in want {
            match have.iter().find(|h| h.name == w.name) {
                None => self.report(
                    "field_removed",
                    of,
                    format!("`{at}{}` is compiled, and not declared", w.name),
                ),
                Some(h) => self.field(of, at, h, w),
            }
        }
    }

    fn field(&mut self, of: &str, at: &str, have: &FieldDef, want: &FieldDef) {
        if let Some(code) = self.within(&have.ty, &want.ty) {
            self.report(
                code,
                of,
                format!(
                    "`{at}{}` is compiled as {}, and declared as {}",
                    want.name,
                    notation(&want.ty),
                    notation(&have.ty)
                ),
            );
        }
        if have.unit != want.unit {
            let text = |u: &Option<Box<str>>| match u {
                Some(u) => format!("in `{u}`"),
                None => "without a unit".to_string(),
            };
            self.report(
                "unit_changed",
                of,
                format!(
                    "`{at}{}` is compiled {}, and declared {}",
                    want.name,
                    text(&want.unit),
                    text(&have.unit)
                ),
            );
        }
    }

    fn rules(&mut self, of: &str, at: &str, have: &[RuleDef], want: &[RuleDef]) {
        if want.is_empty() {
            return;
        }
        let have: HashSet<String> = have.iter().map(requirement).collect();
        for w in want {
            if !have.contains(&requirement(w)) {
                let scope = match at.strip_suffix('.') {
                    Some(v) => format!(" of the variant `{v}`"),
                    None => String::new(),
                };
                self.report(
                    "rule_removed",
                    of,
                    format!("the {}{scope} is compiled, and not declared", rule_name(w)),
                );
            }
        }
    }

    /// The code of the difference when a value of the registered type `have`
    /// may not be one of the compiled type `want`.
    fn within(&mut self, have: &FieldType, want: &FieldType) -> Option<&'static str> {
        use FieldType as T;
        let ok = |within: bool| (!within).then_some("bound_loosened");
        match (have, want) {
            (T::Option { inner: h }, T::Option { inner: w }) => self.within(h, w),
            (h, T::Option { inner: w }) => self.within(h, w),
            (T::Option { inner: h }, w) => Some(match self.within(h, w) {
                Some("type_changed") => "type_changed",
                _ => "required_to_optional",
            }),
            (T::Integer(h), T::Integer(w)) => ok(integer_within(h, w)),
            (T::Double(h), T::Double(w)) => ok(double_within(h, w)),
            (T::Character(h), T::Character(w)) => {
                if w.pattern.is_some() && h.pattern != w.pattern {
                    return Some("pattern_changed");
                }
                ok(character_within(h, w))
            }
            (
                T::Vec {
                    element: h,
                    min_length: h_min,
                    max_length: h_max,
                    unique: h_unique,
                },
                T::Vec {
                    element: w,
                    min_length: w_min,
                    max_length: w_max,
                    unique: w_unique,
                },
            ) => self.within(h, w).or_else(|| {
                ok(h_min.unwrap_or(0) >= w_min.unwrap_or(0)
                    && h_max.unwrap_or(u64::MAX) <= w_max.unwrap_or(u64::MAX)
                    && (*h_unique || !*w_unique))
            }),
            (T::BtreeMap { value: h, keys: hk }, T::BtreeMap { value: w, keys: wk })
            | (T::IndexMap { value: h, keys: hk }, T::IndexMap { value: w, keys: wk }) => {
                self.within(h, w).or_else(|| ok(character_within(hk, wk)))
            }
            (
                T::Matrix {
                    element: h,
                    nrow: hr,
                    ncol: hc,
                },
                T::Matrix {
                    element: w,
                    nrow: wr,
                    ncol: wc,
                },
            )
            | (
                T::SparseMatrix {
                    element: h,
                    nrow: hr,
                    ncol: hc,
                },
                T::SparseMatrix {
                    element: w,
                    nrow: wr,
                    ncol: wc,
                },
            ) => self
                .within(h, w)
                .or_else(|| ok(size_within(hr, wr) && size_within(hc, wc))),
            (
                T::Array {
                    element: h,
                    dim: hd,
                },
                T::Array {
                    element: w,
                    dim: wd,
                },
            ) => self.within(h, w).or_else(|| ok(size_within(hd, wd))),
            (
                T::Table {
                    columns: h,
                    open: h_open,
                    rules: h_rules,
                },
                T::Table {
                    columns: w,
                    open: w_open,
                    rules: w_rules,
                },
            ) => {
                if *h_open && !*w_open {
                    return Some("bound_loosened");
                }
                self.columns(h, w).or_else(|| {
                    let have: HashSet<String> = h_rules.iter().map(requirement).collect();
                    ok(w_rules.iter().all(|r| have.contains(&requirement(r))))
                })
            }
            (T::MatrixOf { of: h }, T::MatrixOf { of: w })
            | (T::TableOf { of: h }, T::TableOf { of: w })
            | (T::Named { name: h }, T::Named { name: w })
                if h == w =>
            {
                self.named(w);
                None
            }
            (T::Subtype { of: h } | T::Named { name: h }, T::Subtype { of: w })
                if self.registered.is_subtype(h, w) =>
            {
                self.named(w);
                None
            }
            (h, w) if h == w => None,
            _ => Some("type_changed"),
        }
    }

    /// `within()` of the columns of a table: each compiled column is declared,
    /// required where the compiled one is, and unique where it is.
    fn columns(&mut self, have: &[ColumnDef], want: &[ColumnDef]) -> Option<&'static str> {
        for w in want {
            let code = match have.iter().find(|h| h.field().name == w.field().name) {
                None => Some("field_removed"),
                Some(h) => self.column(h, w),
            };
            if code.is_some() {
                return code;
            }
        }
        None
    }

    fn column(&mut self, have: &ColumnDef, want: &ColumnDef) -> Option<&'static str> {
        if have.field().unit != want.field().unit {
            return Some("unit_changed");
        }
        if (want.required() && !have.required()) || (want.unique() && !have.unique()) {
            return Some("bound_loosened");
        }
        self.within(&have.field().ty, &want.field().ty)
    }
}
