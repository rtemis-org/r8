//! The registry's set of documents: its name index through every change, and
//! the lookup of `dyn()` implementors across namespaces.

use rtemis_r8::ir::{IrDocument, TypeDef};
use rtemis_r8::name::{Name, QualifiedName};
use rtemis_r8::rules::{DocumentSet, TypeLookup};
use serde_json::json;

fn ty(name: &str, version: u32) -> TypeDef {
    serde_json::from_value(json!({
        "kind": "struct", "name": name, "version": version,
        "fields": [{"name": "x", "type": {"kind": "double"}}]
    }))
    .unwrap()
}

fn q(s: &str) -> QualifiedName {
    QualifiedName::parse(s).unwrap()
}

fn version(set: &DocumentSet, name: &str) -> Option<u32> {
    match set.find(&q(name))? {
        TypeDef::Struct(s) => Some(s.version),
        TypeDef::Enum(e) => Some(e.version),
    }
}

#[test]
fn the_index_follows_insertion_replacement_and_rollback() {
    let mut set = DocumentSet::default();
    let a = Name::new("a").unwrap();
    for i in 0..50 {
        set.put_type(&a, ty(&format!("T{i}"), 1));
    }
    assert_eq!(version(&set, "a::T0"), Some(1));
    assert_eq!(version(&set, "a::T49"), Some(1));
    assert_eq!(version(&set, "a::T50"), None);
    assert_eq!(version(&set, "b::T0"), None);

    // Replacement keeps the position and the index.
    let placed = set.put_type(&a, ty("T7", 2));
    assert_eq!(placed.index, 7);
    assert_eq!(version(&set, "a::T7"), Some(2));
    set.undo_put(placed);
    assert_eq!(version(&set, "a::T7"), Some(1));

    // Rolling back a new type removes it from the index.
    let placed = set.put_type(&a, ty("New", 1));
    assert_eq!(version(&set, "a::New"), Some(1));
    set.undo_put(placed);
    assert_eq!(version(&set, "a::New"), None);
    assert_eq!(set.documents()[0].types.len(), 50);

    // Rolling back the first type of a new namespace removes its document,
    // and the index still finds the types of the documents after it.
    let b = Name::new("b").unwrap();
    let c = Name::new("c").unwrap();
    let placed = set.put_type(&b, ty("B", 1));
    set.put_type(&c, ty("C", 1));
    assert!(placed.new_document);
    let d = placed.document;
    set.undo_put(placed);
    assert!(set.documents().iter().all(|doc| doc.namespace != b));
    assert_eq!(set.documents().len(), 2);
    assert_eq!(version(&set, "c::C"), Some(1));
    assert_eq!(set.document(d).namespace, c);
}

#[test]
fn a_set_built_from_documents_is_indexed() {
    let doc = |ns: &str, names: &[&str]| -> IrDocument {
        IrDocument {
            ir_version: rtemis_r8::ir::IR_VERSION,
            namespace: Name::new(ns).unwrap(),
            types: names.iter().map(|n| ty(n, 3)).collect(),
            traits: Vec::new(),
            impls: Vec::new(),
        }
    };
    let set: DocumentSet = vec![doc("a", &["X", "Y"]), doc("b", &["X"])]
        .into_iter()
        .collect();
    assert_eq!(version(&set, "a::Y"), Some(3));
    assert_eq!(version(&set, "b::X"), Some(3));
    assert_eq!(version(&set, "b::Y"), None);
    let clone = set.clone();
    assert_eq!(version(&clone, "a::X"), Some(3));
}

#[test]
fn implementors_are_found_through_the_trait_tag_in_every_namespace() {
    let home = IrDocument::from_json(
        &json!({
            "$ir": 1, "namespace": "k",
            "types": [{"kind": "struct", "name": "Linear", "version": 1, "fields": []}],
            "traits": [{"name": "Kernel", "methods": [], "tag": "kind"}],
            "impls": [{"trait": "k::Kernel", "target": {"kind": "named", "name": "k::Linear"},
                       "tag_value": "linear", "methods": []}]
        })
        .to_string(),
    )
    .unwrap();
    let other = IrDocument::from_json(
        &json!({
            "$ir": 1, "namespace": "ext",
            "types": [{"kind": "struct", "name": "Rbf", "version": 1, "fields": []}],
            "impls": [{"trait": "k::Kernel", "target": {"kind": "named", "name": "ext::Rbf"},
                       "methods": []}]
        })
        .to_string(),
    )
    .unwrap();
    let set = DocumentSet::from(vec![home, other]);
    let tr = q("k::Kernel");
    let found = |v: serde_json::Value| set.implementor(&tr, &v);
    assert_eq!(
        found(json!({"kind": "linear"})).unwrap(),
        (q("k::Linear"), "kind".to_string())
    );
    // An implementor in another namespace is named with its namespace.
    assert_eq!(
        found(json!({"kind": "ext::Rbf"})).unwrap(),
        (q("ext::Rbf"), "kind".to_string())
    );
    assert!(found(json!({"kind": "Rbf"})).is_err());
    assert!(found(json!({"kind": "ext::Linear"})).is_err());
    assert!(found(json!({"type": "linear"})).is_err());
    // A trait whose namespace the set does not hold reads `$type`.
    assert_eq!(
        set.implementor(&q("none::T"), &json!({"$type": "ext::Rbf"}))
            .unwrap(),
        (q("ext::Rbf"), "$type".to_string())
    );
}
