//! `validate::constrains()`: the types whose values a host validates when
//! code it does not check, such as Rust code, makes them.
//!
//! spec: R8/class-system#rust-impls-and-rust-stored-types-rtemis-r8-api

use rtemis_r8::ir::IrDocument;
use rtemis_r8::name::QualifiedName;
use rtemis_r8::rules::DocumentSet;
use rtemis_r8::validate::constrains;
use serde_json::json;

#[test]
fn a_type_constrains_its_values_by_rules_bounds_and_open_fields() {
    let doc = json!({
        "$ir": 1, "namespace": "ns",
        "types": [
            {"kind": "struct", "name": "Plain", "description": "", "version": 1,
             "fields": [{"name": "x", "type": {"kind": "double"}, "description": ""},
                        {"name": "s", "type": {"kind": "character"}, "description": ""}]},
            {"kind": "struct", "name": "Ruled", "description": "", "version": 1,
             "fields": [{"name": "lo", "type": {"kind": "double"}, "description": ""},
                        {"name": "hi", "type": {"kind": "double"}, "description": ""}],
             "rules": [{"invariant": {"<=": [{"var": "lo"}, {"var": "hi"}]}}]},
            {"kind": "struct", "name": "Bounded", "description": "", "version": 1,
             "fields": [{"name": "n", "type": {"kind": "vec", "element": {"kind": "integer", "min": 0}}, "description": ""}]},
            {"kind": "struct", "name": "Holds", "description": "", "version": 1,
             "fields": [{"name": "r", "type": {"kind": "option", "inner": {"kind": "named", "name": "ns::Ruled"}}, "description": ""}]},
            {"kind": "struct", "name": "Open", "description": "", "version": 1,
             "fields": [{"name": "p", "type": {"kind": "subtype", "of": "ns::Plain"}, "description": ""}]}
        ],
        "traits": [], "impls": []
    });
    let docs = DocumentSet::from(vec![IrDocument::from_json(&doc.to_string()).unwrap()]);
    let q = |s: &str| QualifiedName::parse(s).unwrap();
    assert!(!constrains(&q("ns::Plain"), &docs));
    assert!(constrains(&q("ns::Ruled"), &docs));
    assert!(constrains(&q("ns::Bounded"), &docs));
    assert!(constrains(&q("ns::Holds"), &docs));
    assert!(constrains(&q("ns::Open"), &docs));
}
