//! Multiple dispatch in the registry: coherence across namespaces, the
//! orphan rule at declaration, ambiguity at the namespace check, and the
//! resolution of the next impl.
//! spec: R8/class-system#multiple-dispatch

use rtemis_r8::ir::dispatch::{Arg, Resolution, impls_by_namespace, positions, resolve};
use rtemis_r8::ir::{ImplDef, TraitDef, TypeDef};
use rtemis_r8::name::{Name, QualifiedName};
use rtemis_r8::registry::Registry;
use rtemis_r8::rules::TypeLookup;
use serde_json::json;

fn name(s: &str) -> Name {
    Name::new(s).unwrap()
}

fn q(s: &str) -> QualifiedName {
    QualifiedName::parse(s).unwrap()
}

fn struct_of(name: &str, own: &str, parent: Option<(&str, &[&str])>) -> TypeDef {
    let mut fields = Vec::new();
    if let Some((p, inherited)) = parent {
        for f in inherited {
            fields.push(json!({"name": f, "type": {"kind": "double"}, "inherited_from": p}));
        }
    }
    fields.push(json!({"name": own, "type": {"kind": "double"}}));
    let mut s = json!({"kind": "struct", "name": name, "version": 1, "fields": fields});
    if let Some((p, _)) = parent {
        s["extends"] = json!(p);
    }
    serde_json::from_value(s).unwrap()
}

fn combine() -> TraitDef {
    serde_json::from_value(json!({"name": "Combine", "methods": [{"name": "combine",
        "params": [{"name": "x", "dispatch": true}, {"name": "y", "dispatch": true}]}]}))
    .unwrap()
}

fn imp(target: &str, other: &str) -> ImplDef {
    let pos = |s: &str| {
        if s == "any" {
            json!({"kind": "any"})
        } else {
            json!({"kind": "type", "name": s})
        }
    };
    serde_json::from_value(json!({"trait": "core::Combine", "target": pos(target),
        "type_args": [pos(other)], "methods": []}))
    .unwrap()
}

/// `core` declares A <- A1, B <- B1, and the trait `Combine`; `pa` and `pb`
/// each declare a child of `core::A` and `core::B`.
fn registry() -> Registry {
    let r = Registry::new();
    let core = name("core");
    for t in [
        struct_of("A", "a", None),
        struct_of("A1", "a1", Some(("core::A", &["a"]))),
        struct_of("B", "b", None),
        struct_of("B1", "b1", Some(("core::B", &["b"]))),
    ] {
        assert!(r.declare_type(&core, t).unwrap().problems.is_empty());
    }
    assert!(
        r.declare_trait(&core, combine())
            .unwrap()
            .problems
            .is_empty()
    );
    r.declare_type(
        &name("pa"),
        struct_of("A3", "a3", Some(("core::A", &["a"]))),
    )
    .unwrap();
    r.declare_type(
        &name("pb"),
        struct_of("B3", "b3", Some(("core::B", &["b"]))),
    )
    .unwrap();
    r
}

fn codes(out: &rtemis_r8::registry::Outcome) -> Vec<&'static str> {
    out.problems.iter().map(|e| e.code).collect()
}

#[test]
fn an_impl_of_one_tuple_in_two_namespaces_is_refused() {
    let r = registry();
    let first = r
        .declare_impl(&name("pa"), imp("pa::A3", "core::B"))
        .unwrap();
    assert!(codes(&first).is_empty(), "{:?}", first.problems);
    // The trait's namespace declares the same tuple.
    let err = r.declare_impl(&name("core"), imp("pa::A3", "core::B"));
    let out = err.unwrap();
    assert_eq!(codes(&out), ["duplicate_impl"]);
    assert!(out.problems[0].message.contains("`pa`"));
    // The same namespace replaces its own impl.
    let again = r
        .declare_impl(&name("pa"), imp("pa::A3", "core::B"))
        .unwrap();
    assert!(codes(&again).is_empty());
}

#[test]
fn the_orphan_rule_applies_to_every_position() {
    let r = registry();
    // `pb`'s own type after a type: allowed.
    let ok = r
        .declare_impl(&name("pb"), imp("core::A", "pb::B3"))
        .unwrap();
    assert!(codes(&ok).is_empty(), "{:?}", ok.problems);
    // `pb`'s own type after any: refused.
    let out = r.declare_impl(&name("pb"), imp("any", "pb::B3")).unwrap();
    assert_eq!(codes(&out), ["impl_orphan"]);
    // No type of `pa`: refused.
    let out = r
        .declare_impl(&name("pa"), imp("core::A1", "core::B1"))
        .unwrap();
    assert_eq!(codes(&out), ["impl_orphan"]);
    // A refused impl leaves the registry as it was.
    let docs = r.snapshot();
    assert_eq!(impls_by_namespace(&docs, &q("core::Combine")).len(), 1);
}

#[test]
fn the_namespace_check_reports_ambiguity_with_imports_only() {
    let r = registry();
    r.declare_impl(&name("core"), imp("core::A", "core::B1"))
        .unwrap();
    r.declare_impl(&name("pa"), imp("pa::A3", "core::B"))
        .unwrap();
    r.declare_impl(&name("pb"), imp("core::A", "pb::B3"))
        .unwrap();
    let ambiguous = |ns: &str, imports: &[&str]| -> Vec<String> {
        let imports: Vec<String> = imports.iter().map(ToString::to_string).collect();
        let mut out: Vec<String> = r
            .check_namespaces_with(&[ns.to_string()], &imports)
            .into_iter()
            .filter(|(_, e)| e.code == "dispatch_ambiguous")
            .map(|(n, _)| n)
            .collect();
        out.sort();
        out
    };
    // `pa` imports `core`: (pa::A3, core::B) against (core::A, core::B1).
    assert_eq!(ambiguous("pa", &["core"]), ["pa"]);
    // Without its imports, `pa` alone holds no pair.
    assert!(ambiguous("pa", &[]).is_empty());
    // `pa` and `pb` are unrelated: their pair waits for the call.
    assert!(ambiguous("pb", &[]).is_empty());
    // The meet, declared by `pa`, resolves the pair with `core`.
    r.declare_impl(&name("pa"), imp("pa::A3", "core::B1"))
        .unwrap();
    assert!(ambiguous("pa", &["core"]).is_empty());
}

#[test]
fn the_next_impl_is_the_unique_less_specific_one() {
    let r = registry();
    for (t, o) in [
        ("core::A", "core::B"),
        ("core::A1", "core::B"),
        ("core::A", "core::B1"),
        ("core::A1", "core::B1"),
    ] {
        let out = r.declare_impl(&name("core"), imp(t, o)).unwrap();
        assert!(codes(&out).is_empty(), "{:?}", out.problems);
    }
    let docs = r.snapshot();
    let tr = q("core::Combine");
    let found = impls_by_namespace(&docs, &tr);
    let impls: Vec<&ImplDef> = found.iter().map(|(_, i)| *i).collect();
    let arity = docs.trait_of(&tr).unwrap().dispatch_arity();
    let args = [Arg::Type(q("core::A1")), Arg::Type(q("core::B1"))];
    let tuple = |k: usize| {
        positions(impls[k], arity)
            .iter()
            .map(ToString::to_string)
            .collect::<Vec<_>>()
    };
    let Resolution::Impl(top) = resolve(&*docs, &impls, arity, &args, None) else {
        panic!("the meet resolves the call");
    };
    assert_eq!(tuple(top), ["core::A1", "core::B1"]);
    // From the meet, both (A1, B) and (A, B1) are next.
    let Resolution::Ambiguous(a, b) = resolve(&*docs, &impls, arity, &args, Some(top)) else {
        panic!("two impls are next");
    };
    let mut next = [tuple(a), tuple(b)];
    next.sort();
    assert_eq!(
        next,
        [
            vec!["core::A".to_string(), "core::B1".to_string()],
            vec!["core::A1".to_string(), "core::B".to_string()]
        ]
    );
    // From (A1, B), the next is (A, B), then the default.
    let a1b = (0..impls.len())
        .find(|&k| tuple(k) == ["core::A1", "core::B"])
        .unwrap();
    let Resolution::Impl(k) = resolve(&*docs, &impls, arity, &args, Some(a1b)) else {
        panic!("one impl is next");
    };
    assert_eq!(tuple(k), ["core::A", "core::B"]);
    assert_eq!(
        resolve(&*docs, &impls, arity, &args, Some(k)),
        Resolution::Default
    );
}
