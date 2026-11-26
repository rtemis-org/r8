//! The JSON Schema of a function's arguments, over the declarations of the
//! suite `conformance/call/read.json`.
//!
//! spec: R8/class-system#typed-functions

use std::path::Path;

use rtemis_r8::call::arguments_schema;
use rtemis_r8::ir::IrDocument;
use rtemis_r8::name::QualifiedName;
use rtemis_r8::rules::DocumentSet;
use serde_json::{Value, json};

fn suite() -> DocumentSet {
    let path = Path::new(env!("CARGO_MANIFEST_DIR")).join("../../../conformance/call/read.json");
    let suite: Value = serde_json::from_str(&std::fs::read_to_string(path).unwrap()).unwrap();
    DocumentSet::from(vec![
        IrDocument::from_json(&suite["ir"].to_string()).unwrap(),
    ])
}

fn q(s: &str) -> QualifiedName {
    QualifiedName::parse(s).unwrap()
}

#[test]
fn the_schema_of_arguments_states_each_parameter() {
    let docs = suite();
    let s = arguments_schema(&q("ns::clip"), &docs, false).unwrap();
    assert_eq!(s["title"], "ns::clip");
    assert_eq!(s["description"], "Clips values to a range.");
    assert_eq!(s["additionalProperties"], false);
    // Required: no default; `w`, of type option() without one, admits null.
    assert_eq!(s["required"], json!(["x", "hi", "w"]));
    let p = &s["properties"];
    assert_eq!(p["x"]["description"], "The values.");
    assert_eq!(p["lo"]["default"], 0);
    assert!(p["n"].get("default").is_none());
    assert_eq!(
        p["n"]["x-r8"]["host_default"],
        json!({"host": "r", "text": "length(x)"})
    );
    assert_eq!(
        p["w"],
        json!({"anyOf": [{"$ref": "#/$defs/ns::Range"}, {"type": "null"}]})
    );
    assert_eq!(p["z"]["default"], Value::Null);
    assert!(p["z"].as_object().unwrap().contains_key("default"));
    assert_eq!(s["x-r8"]["function"], "ns::clip");
    assert_eq!(s["x-r8"]["rules"].as_array().unwrap().len(), 2);
    assert!(s["$defs"].get("ns::Range").is_some());
}

#[test]
fn a_function_without_a_document_form_has_no_schema() {
    let docs = suite();
    assert_eq!(
        arguments_schema(&q("ns::loose"), &docs, false).unwrap_err(),
        vec!["x".to_string()]
    );
    assert!(
        arguments_schema(&q("ns::none"), &docs, false)
            .unwrap_err()
            .is_empty()
    );
}
