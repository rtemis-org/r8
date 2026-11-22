//! `extends`: the lowering of a child onto its parent, the checks of a
//! lowered child, the declaration hash of a flattened declaration, and
//! `dyn(Parent)` values.
//! spec: R8/class-system#inheritance

use rtemis_r8::codec::declaration_hash;
use rtemis_r8::ir::{InheritedChange, IrDocument, StructDef, TypeDef, check_document, extend};
use rtemis_r8::name::{Name, QualifiedName};
use rtemis_r8::registry::Registry;
use rtemis_r8::rules::TypeLookup;
use rtemis_r8::tree::Tree;
use rtemis_r8::validate::validate;
use serde_json::json;

fn q(s: &str) -> QualifiedName {
    QualifiedName::parse(s).unwrap()
}

fn name(s: &str) -> Name {
    Name::new(s).unwrap()
}

fn st(v: serde_json::Value) -> StructDef {
    let doc =
        IrDocument::from_json(&json!({"$ir": 1, "namespace": "ns", "types": [v]}).to_string())
            .unwrap();
    match doc.types.into_iter().next().unwrap() {
        TypeDef::Struct(s) => s,
        TypeDef::Enum(_) => panic!("a struct"),
    }
}

fn double(n: &str) -> serde_json::Value {
    json!({"name": n, "type": {"kind": "double"}})
}

fn positive(n: &str, id: &str) -> serde_json::Value {
    json!({"id": id, "invariant": {">": [{"var": n}, 0]}})
}

/// `A { a }`, `B: A { b }`, `C: B { c }`, each with one rule on its own
/// field; `B` gives `a` a default and marks it sensitive.
fn chain() -> (StructDef, StructDef, StructDef) {
    let a = st(json!({"kind": "struct", "name": "A", "version": 1,
        "fields": [double("a")], "rules": [positive("a", "A_POS")]}));
    let mut b = st(json!({"kind": "struct", "name": "B", "version": 1,
        "fields": [double("b")], "rules": [positive("b", "B_POS")]}));
    let change = InheritedChange {
        name: name("a"),
        default: Some(Some(Box::new(Tree::from_value(&json!(1.5))))),
        sensitive: true,
        description: Some("The a of B.".into()),
    };
    extend(&mut b, &q("ns::A"), &a, &[change]).unwrap();
    let mut c = st(json!({"kind": "struct", "name": "C", "version": 1,
        "fields": [double("c")], "rules": [positive("c", "C_POS")]}));
    extend(&mut c, &q("ns::B"), &b, &[]).unwrap();
    (a, b, c)
}

fn registry_of(types: Vec<StructDef>) -> Registry {
    let r = Registry::new();
    for t in types {
        let out = r.declare_type(&name("ns"), TypeDef::Struct(t)).unwrap();
        assert!(out.problems.is_empty(), "{:?}", out.problems);
    }
    r
}

#[test]
fn a_child_holds_the_parents_fields_and_rules_first() {
    let (_, b, c) = chain();
    let names: Vec<_> = c.fields.iter().map(|f| f.name.as_str()).collect();
    assert_eq!(names, ["a", "b", "c"]);
    let from: Vec<_> = c
        .fields
        .iter()
        .map(|f| f.inherited_from.as_ref().map(|q| q.to_string()))
        .collect();
    assert_eq!(
        from,
        [Some("ns::A".to_string()), Some("ns::B".to_string()), None]
    );
    let ids: Vec<_> = c
        .rules
        .iter()
        .map(|r| r.id.as_ref().unwrap().as_str())
        .collect();
    assert_eq!(ids, ["A_POS", "B_POS", "C_POS"]);
    assert_eq!(c.rules[0].inherited_from.as_deref(), Some(&q("ns::A")));
    assert_eq!(c.extends, Some(q("ns::B")));
    // B's change to `a` reaches C.
    assert!(b.fields[0].sensitive && c.fields[0].sensitive);
    assert!(c.fields[0].default.is_some());
    assert_eq!(&*c.fields[0].description, "The a of B.");
}

#[test]
fn a_registered_chain_descends_and_lists_its_ancestors() {
    let (a, b, c) = chain();
    let r = registry_of(vec![a, b, c]);
    let docs = r.snapshot();
    assert!(docs.descends(&q("ns::C"), &q("ns::A")));
    assert!(docs.descends(&q("ns::C"), &q("ns::C")));
    assert!(!docs.descends(&q("ns::A"), &q("ns::C")));
    assert_eq!(docs.ancestors(&q("ns::C")), [q("ns::B"), q("ns::A")]);
    // A child's record carries its ancestors, which a worker registers.
    let entries = rtemis_r8::record::decode(&r.type_record(&q("ns::C")).unwrap()).unwrap();
    assert_eq!(entries.len(), 3);
    // The parent's rules apply to the child.
    let errors = validate(&json!({"a": -1, "b": 1, "c": 1}), &q("ns::C"), &*docs);
    assert_eq!(errors.len(), 1, "{errors:?}");
    assert_eq!(errors[0].rule.as_ref().map(Name::as_str), Some("A_POS"));
}

#[test]
fn the_declaration_hash_covers_the_flattened_declaration() {
    // C extends A directly, or through B0, which adds nothing.
    let a = st(json!({"kind": "struct", "name": "A", "version": 1,
        "fields": [double("a")], "rules": [positive("a", "A_POS")]}));
    let own = || {
        st(json!({"kind": "struct", "name": "C", "version": 1,
            "fields": [double("c")]}))
    };
    let mut direct = own();
    extend(&mut direct, &q("ns::A"), &a, &[]).unwrap();
    let mut b0 = st(json!({"kind": "struct", "name": "B0", "version": 1, "fields": []}));
    extend(&mut b0, &q("ns::A"), &a, &[]).unwrap();
    let mut through = own();
    extend(&mut through, &q("ns::B0"), &b0, &[]).unwrap();
    let r1 = registry_of(vec![a.clone(), direct]);
    let r2 = registry_of(vec![a.clone(), b0, through]);
    // A struct that declares the same fields and rules itself.
    let flat = st(json!({"kind": "struct", "name": "C", "version": 1,
        "fields": [double("a"), double("c")], "rules": [positive("a", "A_POS")]}));
    let r3 = registry_of(vec![a, flat]);
    let h = |r: &Registry| declaration_hash(&q("ns::C"), &*r.snapshot());
    assert_eq!(h(&r1), h(&r2));
    assert_eq!(h(&r1), h(&r3));
}

#[test]
fn the_lowering_refuses_what_a_child_cannot_change() {
    let a = st(json!({"kind": "struct", "name": "A", "version": 1,
        "fields": [double("a")], "rules": [positive("a", "A_POS")]}));
    let mut c = st(json!({"kind": "struct", "name": "C", "version": 1,
        "fields": [double("a")], "rules": [positive("a", "A_POS")]}));
    let unknown = InheritedChange {
        name: name("z"),
        default: None,
        sensitive: false,
        description: None,
    };
    let before = c.clone();
    let errors = extend(&mut c, &q("ns::A"), &a, &[unknown]).unwrap_err();
    let codes: Vec<_> = errors.iter().map(|e| e.code).collect();
    assert_eq!(
        codes,
        ["duplicate_field", "duplicate_rule", "inherited_unknown"]
    );
    assert_eq!(c, before);
}

fn codes_of(r: &Registry, t: StructDef) -> Vec<&'static str> {
    let out = r.declare_type(&name("ns"), TypeDef::Struct(t)).unwrap();
    assert!(!out.changed);
    out.problems.iter().map(|p| p.code).collect()
}

#[test]
fn the_registry_checks_a_child_against_its_parent() {
    let (a, b, c) = chain();
    let r = registry_of(vec![a.clone()]);
    // B's parent is registered; C's is not.
    assert_eq!(codes_of(&r, c.clone()), ["extends_unknown"]);
    // An inherited field with another unit.
    let mut changed = b.clone();
    changed.fields[0].unit = Some("mg".into());
    assert_eq!(codes_of(&r, changed), ["extends_field_changed"]);
    // A dropped inherited rule.
    let mut dropped = b.clone();
    dropped.rules.remove(0);
    assert_eq!(codes_of(&r, dropped), ["extends_rule_changed"]);
    // `sensitive` only tightens.
    let mut widened = b.clone();
    let r2 = registry_of(vec![{
        let mut s = a.clone();
        s.fields[0].sensitive = true;
        s
    }]);
    widened.fields[0].sensitive = false;
    assert_eq!(codes_of(&r2, widened), ["extends_field_changed"]);
    // An enum parent.
    let r3 = Registry::new();
    let e = IrDocument::from_json(
        &json!({"$ir": 1, "namespace": "ns", "types": [
            {"kind": "enum", "name": "A", "version": 1, "variants": [{"name": "X", "shape": "unit"}]}
        ]})
        .to_string(),
    )
    .unwrap();
    r3.declare_type(&name("ns"), e.types[0].clone()).unwrap();
    assert_eq!(codes_of(&r3, b.clone()), ["extends_not_struct"]);
    // A cycle: A declared again as a child of B.
    let r4 = registry_of(vec![a.clone(), b.clone()]);
    let mut looped = a.clone();
    looped.extends = Some(q("ns::B"));
    assert_eq!(codes_of(&r4, looped), ["extends_cycle"]);
}

#[test]
fn inherited_members_precede_the_childs_own() {
    let (_, b, _) = chain();
    let mut swapped = b.clone();
    swapped.fields.reverse();
    let doc = IrDocument {
        types: vec![TypeDef::Struct(swapped)],
        ..IrDocument::from_json(r#"{"$ir": 1, "namespace": "ns"}"#).unwrap()
    };
    let codes: Vec<_> = check_document(&doc).iter().map(|e| e.code).collect();
    assert_eq!(codes, ["extends_order"]);
    let mut orphan = b;
    orphan.extends = None;
    let doc = IrDocument {
        types: vec![TypeDef::Struct(orphan)],
        ..IrDocument::from_json(r#"{"$ir": 1, "namespace": "ns"}"#).unwrap()
    };
    let codes: Vec<_> = check_document(&doc).iter().map(|e| e.code).collect();
    assert_eq!(codes, ["extends_order", "extends_order"]);
}

#[test]
fn the_defaults_of_a_child_satisfy_its_rules() {
    let a = st(json!({"kind": "struct", "name": "A", "version": 1,
        "fields": [{"name": "a", "type": {"kind": "double"}, "default": 1}]}));
    let mut c = st(
        json!({"kind": "struct", "name": "C", "version": 1, "fields": [],
        "rules": [{"id": "BIG", "invariant": {">": [{"var": "a"}, 5]}}]}),
    );
    extend(&mut c, &q("ns::A"), &a, &[]).unwrap();
    let r = registry_of(vec![a]);
    assert_eq!(codes_of(&r, c), ["default_violates_rule"]);
}

#[test]
fn a_parent_declared_again_reports_its_stale_children() {
    let (a, b, _) = chain();
    let r = Registry::new();
    r.declare_type(
        &name("base"),
        TypeDef::Struct({
            let mut s = a.clone();
            s.fields.truncate(1);
            s
        }),
    )
    .unwrap();
    let mut child = st(json!({"kind": "struct", "name": "B", "version": 1,
        "fields": [double("b")]}));
    extend(&mut child, &q("base::A"), &a, &[]).unwrap();
    let out = r.declare_type(&name("ns"), TypeDef::Struct(child)).unwrap();
    assert!(out.problems.is_empty(), "{:?}", out.problems);
    assert!(r.check_namespaces(&["base".to_string()]).is_empty());
    // `base::A` gains a field.
    let mut grown = a;
    grown.fields.push(rtemis_r8::ir::FieldDef {
        name: name("z"),
        ..b.fields[1].clone()
    });
    grown.fields[1].inherited_from = None;
    let out = r
        .declare_type(&name("base"), TypeDef::Struct(grown))
        .unwrap();
    assert_eq!(out.stale, [q("ns::B")]);
    assert_eq!(r.stale_types(), [q("ns::B")]);
    // The parent's package loads; the stale child is not its problem.
    assert!(r.check_namespaces(&["base".to_string()]).is_empty());
    // The child's package, loaded against the new parent, is refused.
    let codes: Vec<_> = r
        .check_namespaces(&["ns".to_string()])
        .into_iter()
        .map(|(ns, e)| (ns, e.code))
        .collect();
    assert_eq!(codes, [("ns".to_string(), "extends_stale")]);
    // A stale type validates no value.
    let docs = r.snapshot();
    let errors = validate(&json!({"a": 1, "b": 2}), &q("ns::B"), &*docs);
    let codes: Vec<_> = errors.iter().map(|e| e.code).collect();
    assert_eq!(codes, ["type_stale"]);
}

/// `A`, `B: A`, `C: B` with `A` declared again with a new field `z`.
fn grown_a(a: &StructDef) -> StructDef {
    let mut grown = a.clone();
    grown.fields.push(rtemis_r8::ir::FieldDef {
        name: name("z"),
        ..a.fields[0].clone()
    });
    grown
}

#[test]
fn a_stale_parent_makes_its_descendants_stale_until_they_are_declared_again() {
    let (a, b, c) = chain();
    let r = registry_of(vec![a.clone(), b, c]);
    let grown = grown_a(&a);
    let out = r
        .declare_type(&name("ns"), TypeDef::Struct(grown.clone()))
        .unwrap();
    assert_eq!(out.stale, [q("ns::B"), q("ns::C")]);
    // B declared again against the new A clears B; C's copy of B is stale.
    let mut b2 = st(json!({"kind": "struct", "name": "B", "version": 1,
        "fields": [double("b")], "rules": [positive("b", "B_POS")]}));
    extend(&mut b2, &q("ns::A"), &grown, &[]).unwrap();
    let out = r
        .declare_type(&name("ns"), TypeDef::Struct(b2.clone()))
        .unwrap();
    assert!(out.problems.is_empty(), "{:?}", out.problems);
    assert_eq!(out.stale, [q("ns::C")]);
    assert_eq!(r.stale_types(), [q("ns::C")]);
    let mut c2 = st(json!({"kind": "struct", "name": "C", "version": 1,
        "fields": [double("c")], "rules": [positive("c", "C_POS")]}));
    extend(&mut c2, &q("ns::B"), &b2, &[]).unwrap();
    let out = r.declare_type(&name("ns"), TypeDef::Struct(c2)).unwrap();
    assert!(out.stale.is_empty());
    assert!(r.stale_types().is_empty());
}

#[test]
fn a_parent_declared_back_clears_its_children_and_a_restore_restores_the_marks() {
    let (a, b, _) = chain();
    let r = registry_of(vec![a.clone(), b]);
    let token = r.save();
    r.declare_type(&name("ns"), TypeDef::Struct(grown_a(&a)))
        .unwrap();
    assert_eq!(r.stale_types(), [q("ns::B")]);
    r.restore(token).unwrap();
    assert!(r.stale_types().is_empty());
    r.declare_type(&name("ns"), TypeDef::Struct(grown_a(&a)))
        .unwrap();
    let out = r.declare_type(&name("ns"), TypeDef::Struct(a)).unwrap();
    assert!(out.stale.is_empty());
    assert!(r.stale_types().is_empty());
}

#[test]
fn every_struct_with_rules_and_defaults_checks_its_defaults() {
    let range = st(json!({"kind": "struct", "name": "Range", "version": 1,
        "fields": [
            {"name": "lo", "type": {"kind": "double"}, "default": 5},
            {"name": "hi", "type": {"kind": "double"}, "default": 1}
        ],
        "rules": [{"id": "ORDER", "invariant": {"<": [{"var": "lo"}, {"var": "hi"}]}}]}));
    let r = Registry::new();
    assert_eq!(codes_of(&r, range), ["default_violates_rule"]);
    // A rule over a computed field, absent before resolution, holds.
    let computed = st(json!({"kind": "struct", "name": "Dose", "version": 1,
        "fields": [
            {"name": "mg", "type": {"kind": "double"}, "default": 250},
            {"name": "g", "type": {"kind": "option", "inner": {"kind": "double"},
                "resolve": {"policy": "computed", "expr": {"/": [{"var": "mg"}, 1000]}}}}
        ],
        "rules": [{"id": "SMALL", "invariant": {"<": [{"var": "g"}, 0.1]}}]}));
    let out = r
        .declare_type(&name("ns"), TypeDef::Struct(computed))
        .unwrap();
    assert!(out.problems.is_empty(), "{:?}", out.problems);
}

#[test]
fn a_descendant_field_admits_the_struct_and_its_descendants() {
    let (a, b, c) = chain();
    let holder = st(json!({"kind": "struct", "name": "H", "version": 1,
        "fields": [{"name": "x", "type": {"kind": "descendant", "of": "ns::A"}}]}));
    let other = st(json!({"kind": "struct", "name": "O", "version": 1,
        "fields": [double("a")]}));
    let r = registry_of(vec![a, b, c, holder, other]);
    let docs = r.snapshot();
    let ok = json!({"x": {"$type": "ns::C", "a": 1, "b": 2, "c": 3}});
    assert!(validate(&ok, &q("ns::H"), &*docs).is_empty());
    let base = json!({"x": {"$type": "ns::A", "a": 1}});
    assert!(validate(&base, &q("ns::H"), &*docs).is_empty());
    let codes = |v: serde_json::Value| -> Vec<&'static str> {
        validate(&v, &q("ns::H"), &*docs)
            .iter()
            .map(|e| e.code)
            .collect()
    };
    assert_eq!(
        codes(json!({"x": {"$type": "ns::O", "a": 1}})),
        ["not_descendant"]
    );
    assert_eq!(
        codes(json!({"x": {"$type": "ns::Z", "a": 1}})),
        ["unresolved_type"]
    );
    assert_eq!(codes(json!({"x": {"a": 1}})), ["unresolved_type"]);
    // The child's rules apply through the field.
    assert_eq!(
        codes(json!({"x": {"$type": "ns::C", "a": 1, "b": 2, "c": -3}})),
        ["rule_violation"]
    );
}

#[test]
fn a_child_removes_the_default_of_an_option_field_only() {
    let a = st(
        json!({"kind": "struct", "name": "A", "version": 1, "fields": [
            {"name": "o", "type": {"kind": "option", "inner": {"kind": "double"}}, "default": 1},
            {"name": "d", "type": {"kind": "double"}, "default": 1}
        ]}),
    );
    let change = |n: &str| InheritedChange {
        name: name(n),
        default: Some(None),
        sensitive: false,
        description: None,
    };
    let own = || st(json!({"kind": "struct", "name": "C", "version": 1, "fields": []}));
    let mut c = own();
    extend(&mut c, &q("ns::A"), &a, &[change("o")]).unwrap();
    assert!(c.fields[0].default.is_none());
    let r = registry_of(vec![a.clone(), c]);
    assert!(r.stale_types().is_empty());
    let mut d = own();
    let errors = extend(&mut d, &q("ns::A"), &a, &[change("d"), change("d")]).unwrap_err();
    let codes: Vec<_> = errors.iter().map(|e| e.code).collect();
    assert_eq!(
        codes,
        ["default_removed", "default_removed", "duplicate_field"]
    );
    // A lowered child that drops a required field's default disagrees.
    let mut e = own();
    extend(&mut e, &q("ns::A"), &a, &[]).unwrap();
    e.fields[1].default = None;
    let r = registry_of(vec![a]);
    assert_eq!(codes_of(&r, e), ["extends_field_changed"]);
}
