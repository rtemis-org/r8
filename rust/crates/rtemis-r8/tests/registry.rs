//! The registry: declarations put, checked, and undone; namespace claims;
//! saved states that restore the documents and the owners, in order.

use rtemis_r8::ir::{IrDocument, TypeDef};
use rtemis_r8::name::{Name, QualifiedName};
use rtemis_r8::registry::{Registry, RegistryError};
use rtemis_r8::rules::TypeLookup;

fn ty(json: &str) -> TypeDef {
    let doc = IrDocument::from_json(&format!(
        r#"{{"$ir": 1, "namespace": "ns", "types": [{json}]}}"#
    ))
    .unwrap();
    doc.types.into_iter().next().unwrap()
}

fn ns() -> Name {
    Name::new("ns").unwrap()
}

fn q(s: &str) -> QualifiedName {
    QualifiedName::parse(s).unwrap()
}

const GOOD: &str = r#"{"kind": "struct", "name": "S", "version": 1, "fields": [{"name": "x", "type": {"kind": "double"}}]}"#;
const BAD: &str = r#"{"kind": "struct", "name": "T", "version": 1, "fields": [{"name": "x", "type": {"kind": "double"}}, {"name": "x", "type": {"kind": "double"}}]}"#;

#[test]
fn a_declaration_registers_or_leaves_the_registry_unchanged() {
    let r = Registry::new();
    let g0 = r.generation();
    let out = r.declare_type(&ns(), ty(GOOD)).unwrap();
    assert!(out.changed && out.problems.is_empty());
    assert!(r.generation() > g0);
    // The same declaration again changes nothing.
    let g1 = r.generation();
    assert!(!r.declare_type(&ns(), ty(GOOD)).unwrap().changed);
    assert_eq!(r.generation(), g1);
    // A declaration with problems is undone; its paths are relative to it.
    let out = r.declare_type(&ns(), ty(BAD)).unwrap();
    assert!(!out.changed);
    assert!(!out.problems.is_empty());
    assert!(
        out.problems.iter().all(|p| !p.path.starts_with("/types/")),
        "{:?}",
        out.problems
    );
    let docs = r.snapshot();
    assert!(docs.find(&q("ns::S")).is_some());
    assert!(docs.find(&q("ns::T")).is_none());
}

#[test]
fn a_snapshot_does_not_see_later_changes() {
    let r = Registry::new();
    let before = r.snapshot();
    r.declare_type(&ns(), ty(GOOD)).unwrap();
    assert!(before.find(&q("ns::S")).is_none());
    assert!(r.snapshot().find(&q("ns::S")).is_some());
}

#[test]
fn a_namespace_belongs_to_the_first_package_that_claims_it() {
    let r = Registry::new();
    assert_eq!(r.claim("ns", ""), None);
    assert_eq!(r.owner("ns"), None);
    assert_eq!(r.claim("ns", "pkgA"), None);
    assert_eq!(r.claim("ns", "pkgB"), Some("pkgA".to_string()));
    assert_eq!(r.claim("ns", ""), Some("pkgA".to_string()));
    assert_eq!(r.owner("ns").as_deref(), Some("pkgA"));
    assert_eq!(r.namespaces_of("pkgA"), vec!["ns".to_string()]);
}

#[test]
fn a_restore_brings_back_the_documents_and_the_owners() {
    let r = Registry::new();
    let token = r.save();
    assert_eq!(r.claim("ns", "pkgA"), None);
    r.declare_type(&ns(), ty(GOOD)).unwrap();
    r.restore(token).unwrap();
    assert_eq!(r.owner("ns"), None);
    assert!(r.snapshot().find(&q("ns::S")).is_none());
    // Another package may claim the namespace the failed load had claimed.
    assert_eq!(r.claim("ns", "pkgB"), None);
}

#[test]
fn saved_states_restore_and_release_in_reverse_order() {
    let r = Registry::new();
    let outer = r.save();
    let inner = r.save();
    assert_eq!(r.release(outer), Err(RegistryError::Unbalanced));
    let outer = rtemis_r8::registry::SaveToken(1);
    r.release(inner).unwrap();
    r.release(outer).unwrap();
    assert_eq!(
        r.release(rtemis_r8::registry::SaveToken(1)),
        Err(RegistryError::Unbalanced)
    );
}

#[test]
fn a_record_holds_the_type_and_the_types_it_reaches() {
    let r = Registry::new();
    r.declare_type(&ns(), ty(GOOD)).unwrap();
    r.declare_type(
        &ns(),
        ty(r#"{"kind": "struct", "name": "U", "version": 1, "fields": [{"name": "s", "type": {"kind": "named", "name": "ns::S"}}]}"#),
    )
    .unwrap();
    let names = r.snapshot().reached(&q("ns::U"));
    assert_eq!(names, vec![q("ns::S"), q("ns::U")]);
    let bytes = r.type_record(&q("ns::U")).unwrap();
    assert_eq!(rtemis_r8::record::decode(&bytes).unwrap().len(), 2);
    assert!(r.type_record(&q("ns::Missing")).is_none());
}
