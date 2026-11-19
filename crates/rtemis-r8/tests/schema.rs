//! The JSON Schema of the IR document: the published copy matches the IR
//! types, and the schema accepts exactly the documents the reader accepts.
#![cfg(feature = "schema")]

use rtemis_r8::ir::{IrDocument, schema};
use serde_json::{Value, json};

const PUBLISHED: &str = "schemas/ir-v1.schema.json";
const SVM: &str = include_str!("fixtures/svm.ir.json");

/// Fails when the published schema differs from the generated one. With
/// `R8_UPDATE_SCHEMA=1`, writes the generated schema to the published file.
#[test]
fn published_schema_matches_the_ir_types() {
    let generated = serde_json::to_string_pretty(&schema()).unwrap() + "\n";
    let path = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join(PUBLISHED);
    if std::env::var_os("R8_UPDATE_SCHEMA").is_some() {
        std::fs::write(&path, &generated).unwrap();
    }
    let published = std::fs::read_to_string(&path).unwrap_or_default();
    assert!(
        published == generated,
        "{PUBLISHED} differs from the IR types; run `R8_UPDATE_SCHEMA=1 cargo test --features schema` and review the diff"
    );
}

fn field(name: &str, ty: Value) -> Value {
    json!({"name": name, "type": ty})
}

fn with_fields(fields: Value) -> Value {
    json!({
        "$ir": 1, "namespace": "ns",
        "types": [{"kind": "struct", "name": "T", "version": 1, "fields": fields}]
    })
}

/// Each case is a document and whether the reader accepts it.
fn cases() -> Vec<(&'static str, Value)> {
    vec![
        ("svm fixture", serde_json::from_str(SVM).unwrap()),
        ("empty namespace", json!({"$ir": 1, "namespace": "ns"})),
        ("format version 2", json!({"$ir": 2, "namespace": "ns"})),
        ("missing namespace", json!({"$ir": 1})),
        (
            "unknown top-level member",
            json!({"$ir": 1, "namespace": "ns", "extra": 1}),
        ),
        ("reserved namespace", json!({"$ir": 1, "namespace": "NULL"})),
        (
            "field named if",
            with_fields(json!([field("if", json!({"kind": "logical"}))])),
        ),
        (
            "field named x.y",
            with_fields(json!([field("x.y", json!({"kind": "logical"}))])),
        ),
        (
            "field named lambda",
            with_fields(json!([field("lambda", json!({"kind": "double"}))])),
        ),
        (
            "unknown field member",
            with_fields(json!([{"name": "a", "type": {"kind": "logical"}, "colour": 1}])),
        ),
        (
            "unknown kind",
            with_fields(json!([field("a", json!({"kind": "quaternion"}))])),
        ),
        (
            "unknown member in a type",
            with_fields(json!([field("a", json!({"kind": "integer", "colour": 1}))])),
        ),
        (
            "integer min as a string",
            with_fields(json!([field("a", json!({"kind": "integer", "min": "1"}))])),
        ),
        (
            "unknown resolve policy",
            with_fields(json!([field(
                "a",
                json!({"kind": "option", "inner": {"kind": "logical"}, "resolve": {"policy": "later"}})
            )])),
        ),
        (
            "computed resolve",
            with_fields(json!([field(
                "a",
                json!({"kind": "option", "inner": {"kind": "double"}, "resolve": {"policy": "computed", "expr": {"*": [{"var": "b"}, 2]}}})
            )])),
        ),
        (
            "named type, qualified",
            with_fields(json!([field(
                "k",
                json!({"kind": "named", "name": "ns::Kernel"})
            )])),
        ),
        (
            "named type, unqualified",
            with_fields(json!([field(
                "k",
                json!({"kind": "named", "name": "Kernel"})
            )])),
        ),
        (
            "named type, reserved part",
            with_fields(json!([field(
                "k",
                json!({"kind": "named", "name": "ns::NULL"})
            )])),
        ),
        (
            "dyn of a trait",
            with_fields(json!([field(
                "a",
                json!({"kind": "dyn", "trait": "ns::Algorithm"})
            )])),
        ),
        (
            "unit variant",
            json!({"$ir": 1, "namespace": "ns", "types": [{"kind": "enum", "name": "E", "version": 1, "variants": [{"name": "A", "shape": "unit"}]}]}),
        ),
        (
            "variant shape missing",
            json!({"$ir": 1, "namespace": "ns", "types": [{"kind": "enum", "name": "E", "version": 1, "variants": [{"name": "A"}]}]}),
        ),
        (
            "replacement method name",
            json!({"$ir": 1, "namespace": "ns", "traits": [{"name": "L", "methods": [{"name": "labels<-", "params": ["x", "value"]}]}]}),
        ),
        (
            "method name with a dot",
            json!({"$ir": 1, "namespace": "ns", "traits": [{"name": "L", "methods": [{"name": "as.data.frame", "params": ["x", "..."]}]}]}),
        ),
        (
            "method name starting with a digit",
            json!({"$ir": 1, "namespace": "ns", "traits": [{"name": "L", "methods": [{"name": "1fit", "params": []}]}]}),
        ),
        (
            "method name reserved",
            json!({"$ir": 1, "namespace": "ns", "traits": [{"name": "L", "methods": [{"name": "if", "params": []}]}]}),
        ),
        (
            "host-class impl",
            json!({"$ir": 1, "namespace": "ns", "impls": [{"trait": "ns::MatrixLike", "target": {"kind": "host_class", "host": "r", "package": "Matrix", "class": "dgCMatrix", "system": "s4"}, "methods": []}]}),
        ),
        (
            "migration rename",
            json!({"$ir": 1, "namespace": "ns", "types": [{"kind": "struct", "name": "T", "version": 2, "fields": [], "migrations": [{"from_version": 1, "steps": [{"op": "rename", "from": "a", "to": "b"}]}]}]}),
        ),
        (
            "open table with row rules",
            with_fields(json!([field(
                "t",
                json!({"kind": "table", "columns": [{"name": "a", "type": {"kind": "integer"}, "unique": true}], "open": true, "rules": [{"id": "R", "invariant": {">=": [{"var": "a"}, 0]}, "message": "m"}]})
            )])),
        ),
        (
            "optional column",
            with_fields(json!([field(
                "t",
                json!({"kind": "table", "columns": [{"name": "auc", "type": {"kind": "double"}, "optional": true}]})
            )])),
        ),
        (
            "field extensions",
            with_fields(
                json!([{"name": "a", "type": {"kind": "double"}, "extensions": {"rtemis:tunable": true}}]),
            ),
        ),
        (
            "ordered map",
            with_fields(json!([field(
                "m",
                json!({"kind": "ordered_map", "value": {"kind": "double"}})
            )])),
        ),
        (
            "sparse matrix",
            with_fields(json!([field(
                "m",
                json!({"kind": "sparse_matrix", "element": {"kind": "double"}})
            )])),
        ),
        (
            "newtype variant",
            json!({"$ir": 1, "namespace": "ns", "types": [{"kind": "enum", "name": "E", "version": 1, "variants": [{"name": "A", "shape": "newtype", "of": "ns::A"}]}]}),
        ),
        (
            "newtype variant without of",
            json!({"$ir": 1, "namespace": "ns", "types": [{"kind": "enum", "name": "E", "version": 1, "variants": [{"name": "A", "shape": "newtype"}]}]}),
        ),
        (
            "table open as a string",
            with_fields(json!([field(
                "t",
                json!({"kind": "table", "columns": [], "open": "yes"})
            )])),
        ),
        (
            "migration unknown op",
            json!({"$ir": 1, "namespace": "ns", "types": [{"kind": "struct", "name": "T", "version": 2, "fields": [], "migrations": [{"from_version": 1, "steps": [{"op": "shuffle"}]}]}]}),
        ),
    ]
}

/// The schema and the reader agree on every case: a schema validator outside
/// R8 accepts a document exactly when `rtemis-r8` reads it.
#[test]
fn schema_and_reader_agree() {
    let validator = jsonschema::validator_for(&schema()).expect("the schema compiles");
    let mut disagreements = Vec::new();
    let (mut accepted, mut rejected) = (0, 0);
    for (label, doc) in cases() {
        let read = IrDocument::from_json(&doc.to_string()).is_ok();
        let valid = validator.is_valid(&doc);
        if read {
            accepted += 1;
        } else {
            rejected += 1;
        }
        if read != valid {
            disagreements.push(format!("{label}: reader {read}, schema {valid}"));
        }
    }
    assert!(disagreements.is_empty(), "{disagreements:#?}");
    // The cases exercise both outcomes.
    assert!(
        accepted >= 10 && rejected >= 10,
        "{accepted} accepted, {rejected} rejected"
    );
}
