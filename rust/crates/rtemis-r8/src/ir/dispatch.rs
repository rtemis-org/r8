//! Multiple dispatch: the positions of an impl, the orphan rule over them,
//! coherence across namespaces, ambiguity, and the selection of a call's
//! impl.
//!
//! A trait dispatches on the parameters its methods mark, the first when
//! none is marked ([`TraitDef::dispatch_arity`]). An impl gives one type per
//! dispatched position: its `target`, then the first `arity - 1` of its
//! `type_args`; [`TypeRef::Any`] matches any value. A position's types are
//! ordered by `extends`, with any above every type, so an impl is more
//! specific than another when it is at least as specific in every position
//! and more specific in one. With single inheritance, a call is ambiguous
//! exactly when two applicable impls are each more specific in some position
//! and no impl is declared at their meet, which [`ambiguities`] finds from
//! the impls alone.
//! spec: R8/class-system#multiple-dispatch

use super::check::DeclError;
use super::decl::{ImplDef, RequiredField, ReturnType, TraitDef, TypeRef};
use super::types::FieldType;
use crate::name::{Name, QualifiedName};
use crate::rules::{DocumentSet, TypeLookup};

/// The dispatched positions of an impl of a trait of `arity` positions: its
/// target, then its first `arity - 1` type arguments.
pub fn positions(imp: &ImplDef, arity: usize) -> Vec<&TypeRef> {
    std::iter::once(&imp.target)
        .chain(imp.type_args.iter())
        .take(arity)
        .collect()
}

/// `true` when two impls are of one trait for one tuple of types.
pub fn same_tuple(a: &ImplDef, b: &ImplDef) -> bool {
    a.r#trait == b.r#trait && a.target == b.target && a.type_args == b.type_args
}

/// The problems of an impl declared in the namespace `ns`, at `path`: a number
/// of types that differs from the trait's dispatched positions and type
/// parameters (`impl_arity`), any in every dispatched position, which the
/// trait's default implementation serves (`impl_all_any`), any in a type
/// parameter (`impl_type_param`), and an impl outside the trait's namespace
/// that names no type of its own namespace in a position with only types before
/// it (`impl_orphan`, Rust's orphan rule over the tuple). The checks that read
/// the trait run when `lookup` holds it.
pub fn check_impl(lookup: &dyn TypeLookup, ns: &Name, imp: &ImplDef, path: &str) -> Vec<DeclError> {
    let mut out = check_impl_arity(lookup, imp, path);
    out.extend(check_required(lookup, imp, path));
    out.extend(check_signatures(lookup, imp, path));
    if imp.r#trait.namespace != *ns && !names_own_type(ns, imp) {
        out.push(DeclError {
            code: "impl_orphan",
            path: path.to_string(),
            message: format!(
                "an impl of `{}` outside its namespace names a type of the declaring namespace `{ns}` in one of its positions, after types only (Rust's orphan rule); this impl is for ({})",
                imp.r#trait,
                tuple_text(std::iter::once(&imp.target).chain(&imp.type_args))
            ),
        });
    }
    out
}

/// The required fields of the trait `tr` and of its supertraits, each once,
/// when `lookup` holds them.
pub fn required_fields(lookup: &dyn TypeLookup, tr: &QualifiedName) -> Vec<RequiredField> {
    fn collect(
        lookup: &dyn TypeLookup,
        tr: &QualifiedName,
        seen: &mut Vec<QualifiedName>,
        out: &mut Vec<RequiredField>,
    ) {
        if seen.contains(tr) {
            return;
        }
        seen.push(tr.clone());
        let Some(t) = lookup.trait_of(tr) else { return };
        for f in &t.required_fields {
            if !out.iter().any(|g| g.name == f.name) {
                out.push(f.clone());
            }
        }
        for s in &t.supertraits {
            collect(lookup, s, seen, out);
        }
    }
    let mut out = Vec::new();
    collect(lookup, tr, &mut Vec::new(), &mut out);
    out
}

/// The problems of an impl against the required fields of its trait and its
/// supertraits: an implementor that is not a struct
/// (`required_field_target`), a struct without a required field
/// (`required_field_missing`), and a field whose type does not fit the
/// required type (`required_field_type`, [`super::fit::fits`]).
pub fn check_required(lookup: &dyn TypeLookup, imp: &ImplDef, path: &str) -> Vec<DeclError> {
    let required = required_fields(lookup, &imp.r#trait);
    if required.is_empty() {
        return Vec::new();
    }
    let fields = match &imp.target {
        TypeRef::Named { name } => match lookup.find(name) {
            Some(crate::ir::TypeDef::Struct(s)) => Some((name, &s.fields)),
            Some(_) => None,
            // A target outside the lookup is checked where it is declared.
            None => return Vec::new(),
        },
        _ => None,
    };
    let Some((name, fields)) = fields else {
        return vec![DeclError {
            code: "required_field_target",
            path: format!("{path}/target"),
            message: format!(
                "`{}` requires fields, which a struct holds; its impl for {} names no struct",
                imp.r#trait, imp.target
            ),
        }];
    };
    let mut out = Vec::new();
    for r in &required {
        match fields.iter().find(|f| f.name == r.name) {
            None => out.push(DeclError {
                code: "required_field_missing",
                path: format!("{path}/target"),
                message: format!(
                    "`{}` requires the field `{}`, which `{name}` does not have",
                    imp.r#trait, r.name
                ),
            }),
            Some(f) if !super::fit::fits(lookup, &f.ty, &r.ty) => out.push(DeclError {
                code: "required_field_type",
                path: format!("{path}/target"),
                message: format!(
                    "`{}` requires the field `{}` of a type that fits the required one; the field of `{name}` is of another type",
                    imp.r#trait, r.name
                ),
            }),
            Some(_) => {}
        }
    }
    out
}

/// The problems of an impl against its trait's method signatures, when
/// `lookup` holds the trait: a position that a dispatched parameter's type
/// does not admit (`dispatch_param_type`), and a method's return type that
/// does not fit its trait method's (`return_type_unfit`,
/// [`super::fit::fits`]), including one on a method whose trait method
/// declares none, returns `Self`, or replaces.
pub fn check_signatures(lookup: &dyn TypeLookup, imp: &ImplDef, path: &str) -> Vec<DeclError> {
    let Some(tr) = lookup.trait_of(&imp.r#trait) else {
        return Vec::new();
    };
    let mut out = Vec::new();
    let arity = tr.dispatch_arity();
    let at = positions(imp, arity);
    for m in &tr.methods {
        for (k, i) in m.dispatched().into_iter().enumerate() {
            let (Some(t), Some(p)) = (m.params[i].ty(), at.get(k)) else {
                continue;
            };
            if !admits(lookup, t, p) {
                out.push(DeclError {
                    code: "dispatch_param_type",
                    path: if k == 0 {
                        format!("{path}/target")
                    } else {
                        format!("{path}/type_args/{}", k - 1)
                    },
                    message: format!(
                        "the parameter `{}` of `{}@{}` admits no value of {p}",
                        m.params[i].name(),
                        imp.r#trait,
                        m.name
                    ),
                });
            }
        }
    }
    for (j, im) in imp.methods.iter().enumerate() {
        let Some(have) = im.return_type() else {
            continue;
        };
        let Some(m) = tr.methods.iter().find(|m| &m.name == im.name()) else {
            continue;
        };
        let unfit = match &m.return_type {
            _ if m.name.is_replacement() => {
                Some("replaces its value, whose type is `Self`".to_string())
            }
            None => Some("declares no return type to narrow".to_string()),
            Some(ReturnType::SelfType(_)) => Some("returns `Self`".to_string()),
            Some(ReturnType::Type(want)) => (!super::fit::fits(lookup, have, want))
                .then(|| "declares a return type that this one does not fit".to_string()),
        };
        if let Some(why) = unfit {
            out.push(DeclError {
                code: "return_type_unfit",
                path: format!("{path}/methods/{j}/return_type"),
                message: format!(
                    "the method `{}` of the impl of `{}` for {} declares a return type; `{}@{}` {why}",
                    m.name, imp.r#trait, imp.target, imp.r#trait, m.name
                ),
            });
        }
    }
    out
}

/// `true` when a dispatched parameter of type `t` admits the values of the
/// position `p`: any; under an R8 type, that type; under `subtype(S)`, `S`
/// or a struct that extends it; under `dyn(Trait)`, an implementor of the
/// trait; under `host_class()`, a host class.
fn admits(lookup: &dyn TypeLookup, t: &FieldType, p: &TypeRef) -> bool {
    match (t, p) {
        (_, TypeRef::Any) => true,
        (FieldType::Named { name }, TypeRef::Named { name: n }) => name == n,
        (FieldType::Subtype { of }, TypeRef::Named { name }) => lookup.is_subtype(name, of),
        (FieldType::Dyn { r#trait }, TypeRef::Named { name }) => {
            lookup.implements(r#trait, name).is_some()
        }
        (FieldType::HostClass { .. }, TypeRef::HostClass { .. }) => true,
        _ => false,
    }
}

/// The problems of an impl's positions against its trait, when `lookup`
/// holds the trait: `impl_arity`, `impl_all_any`, and `impl_type_param` of
/// [`check_impl`].
pub fn check_impl_arity(lookup: &dyn TypeLookup, imp: &ImplDef, path: &str) -> Vec<DeclError> {
    match lookup.trait_of(&imp.r#trait) {
        Some(tr) => check_arity(tr, imp, path),
        None => Vec::new(),
    }
}

fn check_arity(tr: &TraitDef, imp: &ImplDef, path: &str) -> Vec<DeclError> {
    let mut out = Vec::new();
    let arity = tr.dispatch_arity();
    let expected = arity - 1 + usize::from(tr.extra_type_params);
    if imp.type_args.len() != expected {
        out.push(DeclError {
            code: "impl_arity",
            path: format!("{path}/type_args"),
            message: format!(
                "an impl of `{}` gives {} types after its target; the trait dispatches on {arity} parameters and has {} type parameters",
                imp.r#trait,
                imp.type_args.len(),
                tr.extra_type_params
            ),
        });
        return out;
    }
    if positions(imp, arity)
        .iter()
        .all(|p| matches!(p, TypeRef::Any))
    {
        out.push(DeclError {
            code: "impl_all_any",
            path: path.to_string(),
            message: format!(
                "an impl of `{}` names a type in at least one position; the trait's default implementation serves any value in every position",
                imp.r#trait
            ),
        });
    }
    for (k, p) in imp.type_args.iter().enumerate().skip(arity - 1) {
        if matches!(p, TypeRef::Any) {
            out.push(DeclError {
                code: "impl_type_param",
                path: format!("{path}/type_args/{k}"),
                message: format!(
                    "a type parameter of `{}` is a type or a class, not any",
                    imp.r#trait
                ),
            });
        }
    }
    out
}

/// `true` when a position of `imp` names a type of the namespace `ns`, or a
/// host class whose package is `ns`, and every position before it names a
/// type or a class.
fn names_own_type(ns: &Name, imp: &ImplDef) -> bool {
    for p in std::iter::once(&imp.target).chain(&imp.type_args) {
        match p {
            TypeRef::Named { name } if name.namespace == *ns => return true,
            TypeRef::HostClass { package, .. } if package.as_str() == ns.as_str() => {
                return true;
            }
            TypeRef::Named { .. } | TypeRef::HostClass { .. } => {}
            TypeRef::Any => return false,
        }
    }
    false
}

/// A tuple of positions as text, `ns::A, ns::B, any`.
pub fn tuple_text<'a>(ps: impl IntoIterator<Item = &'a TypeRef>) -> String {
    ps.into_iter()
        .map(ToString::to_string)
        .collect::<Vec<_>>()
        .join(", ")
}

/// The impls of the trait `tr` in every document of `docs`, each with the
/// namespace that declares it.
pub fn impls_by_namespace<'a>(
    docs: &'a DocumentSet,
    tr: &QualifiedName,
) -> Vec<(&'a Name, &'a ImplDef)> {
    docs.documents()
        .iter()
        .flat_map(|d| {
            d.impls
                .iter()
                .filter(move |i| &i.r#trait == tr)
                .map(move |i| (&d.namespace, i))
        })
        .collect()
}

/// The impl of the same trait and tuple as `imp` that another namespace than
/// `ns` declares (`duplicate_impl`): a trait has one impl per tuple across
/// the registry.
pub fn duplicate_impls(docs: &DocumentSet, ns: &Name, imp: &ImplDef, path: &str) -> Vec<DeclError> {
    docs.documents()
        .iter()
        .filter(|d| d.namespace != *ns && d.impls.iter().any(|i| same_tuple(i, imp)))
        .map(|d| DeclError {
            code: "duplicate_impl",
            path: path.to_string(),
            message: format!(
                "the impl of `{}` for ({}) is also declared in the namespace `{}`; a trait has one impl per tuple of types",
                imp.r#trait,
                tuple_text(std::iter::once(&imp.target).chain(&imp.type_args)),
                d.namespace
            ),
        })
        .collect()
}

/// The order of two positions' types.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Order {
    Equal,
    /// The first is more specific.
    Below,
    /// The second is more specific.
    Above,
    /// Neither descends from the other.
    Apart,
}

fn order(lookup: &dyn TypeLookup, a: &TypeRef, b: &TypeRef) -> Order {
    use TypeRef::{Any, HostClass, Named};
    match (a, b) {
        (Any, Any) => Order::Equal,
        (Any, _) => Order::Above,
        (_, Any) => Order::Below,
        (Named { name: x }, Named { name: y }) => {
            if x == y {
                Order::Equal
            } else if lookup.is_subtype(x, y) {
                Order::Below
            } else if lookup.is_subtype(y, x) {
                Order::Above
            } else {
                Order::Apart
            }
        }
        (
            HostClass {
                class: x,
                system: sx,
                ..
            },
            HostClass {
                class: y,
                system: sy,
                ..
            },
        ) if x == y && sx == sy => Order::Equal,
        _ => Order::Apart,
    }
}

/// Two impls of a trait that are each more specific in some position, with
/// no impl declared at their meet, the tuple whose impl disambiguates them.
#[derive(Debug, Clone, PartialEq)]
pub struct Ambiguity {
    /// The position of the first impl in the list given.
    pub first: usize,
    /// The position of the second impl.
    pub second: usize,
    /// The positionwise more specific tuple.
    pub meet: Vec<TypeRef>,
}

/// The ambiguous pairs among `impls`, the impls of one trait of `arity`
/// dispatched positions, for the pairs that `pair` admits by their indices.
/// Complete and exact for R8 types under single inheritance: a call is
/// ambiguous if and only if a pair reported here applies to it.
pub fn ambiguities(
    lookup: &dyn TypeLookup,
    impls: &[&ImplDef],
    arity: usize,
    pair: &dyn Fn(usize, usize) -> bool,
) -> Vec<Ambiguity> {
    let tuples: Vec<Vec<&TypeRef>> = impls.iter().map(|i| positions(i, arity)).collect();
    let mut out = Vec::new();
    for i in 0..impls.len() {
        for j in (i + 1)..impls.len() {
            if !pair(i, j) || tuples[i].len() != arity || tuples[j].len() != arity {
                continue;
            }
            let mut below = false;
            let mut above = false;
            let mut meet = Vec::with_capacity(arity);
            let mut apart = false;
            for (a, b) in tuples[i].iter().zip(&tuples[j]) {
                match order(lookup, a, b) {
                    Order::Equal => meet.push((*a).clone()),
                    Order::Below => {
                        below = true;
                        meet.push((*a).clone());
                    }
                    Order::Above => {
                        above = true;
                        meet.push((*b).clone());
                    }
                    Order::Apart => apart = true,
                }
            }
            if apart || !(below && above) {
                continue;
            }
            let declared = tuples
                .iter()
                .any(|t| t.iter().zip(&meet).all(|(x, y)| *x == y));
            if !declared {
                out.push(Ambiguity {
                    first: i,
                    second: j,
                    meet,
                });
            }
        }
    }
    out
}

/// The type of a call's argument in one dispatched position.
#[derive(Debug, Clone, PartialEq)]
pub enum Arg {
    /// A value of an R8 type.
    Type(QualifiedName),
    /// An object of a host class, its class vector in the host's order.
    Classes(Vec<String>),
    /// A value that no position names but any, such as `NULL`.
    Other,
}

/// The impl that dispatch selects for a call.
#[derive(Debug, Clone, PartialEq)]
pub enum Selection {
    /// The impl at this index of the list given, which provides every method
    /// of the trait.
    Impl(usize),
    /// No impl applies; the trait's default implementation serves the call.
    Default,
    /// Two applicable impls, each more specific in some position.
    Ambiguous(usize, usize),
    /// An argument's type is stale.
    Stale(QualifiedName),
}

/// The chain of an argument in one position: the R8 type and its
/// ancestors, or a host object's class vector. A type position matches an
/// R8 chain only and a host-class position a host chain only, so an object
/// whose class vector names an R8 type is not a value of that type.
enum Chain {
    Type(Vec<QualifiedName>),
    Host(Vec<String>),
    None,
}

/// The rank of an impl's position for an argument: its index in the
/// argument's chain, or the top for any; `None` when the position does not
/// apply.
fn rank(p: &TypeRef, chain: &Chain) -> Option<usize> {
    match (p, chain) {
        (TypeRef::Any, _) => Some(usize::MAX),
        (TypeRef::Named { name }, Chain::Type(c)) => c.iter().position(|x| x == name),
        (TypeRef::HostClass { class, .. }, Chain::Host(c)) => c.iter().position(|x| x == class),
        _ => None,
    }
}

/// Selects the impl for a call of a trait of `arity` dispatched positions, whose
/// arguments are `args`, among `impls`: the most specific applicable impl.
/// With `above`, the impl at that index is the running one, and the result
/// is the most specific applicable impl that is less specific than it, for
/// `r8_next_impl()`.
pub fn select(
    lookup: &dyn TypeLookup,
    impls: &[&ImplDef],
    arity: usize,
    args: &[Arg],
    above: Option<usize>,
) -> Selection {
    let mut chains: Vec<Chain> = Vec::with_capacity(args.len());
    for a in args {
        chains.push(match a {
            Arg::Type(t) => {
                if lookup.is_stale(t) {
                    return Selection::Stale(t.clone());
                }
                Chain::Type(
                    std::iter::once(t.clone())
                        .chain(lookup.ancestors(t))
                        .collect(),
                )
            }
            Arg::Classes(c) => Chain::Host(c.clone()),
            Arg::Other => Chain::None,
        });
    }
    let ranks: Vec<Option<Vec<usize>>> = impls
        .iter()
        .map(|imp| {
            let ps = positions(imp, arity);
            if ps.len() != arity || chains.len() != arity {
                return None;
            }
            ps.iter()
                .zip(&chains)
                .map(|(p, c)| rank(p, c))
                .collect::<Option<Vec<usize>>>()
        })
        .collect();
    let at_most = |a: &[usize], b: &[usize]| a.iter().zip(b).all(|(x, y)| x <= y);
    let floor = above.and_then(|k| ranks[k].clone());
    let candidates: Vec<usize> = (0..impls.len())
        .filter(|&k| match (&ranks[k], &floor) {
            (None, _) => false,
            (Some(_), None) => true,
            (Some(r), Some(f)) => Some(k) != above && at_most(f, r) && r != f,
        })
        .collect();
    let minimal: Vec<usize> = candidates
        .iter()
        .copied()
        .filter(|&k| {
            let rk = ranks[k].as_ref().expect("a candidate applies");
            !candidates.iter().any(|&o| {
                let ro = ranks[o].as_ref().expect("a candidate applies");
                o != k && at_most(ro, rk) && ro != rk
            })
        })
        .collect();
    match minimal.as_slice() {
        [] => Selection::Default,
        [k] => Selection::Impl(*k),
        [a, b, ..] => Selection::Ambiguous(*a, *b),
    }
}
