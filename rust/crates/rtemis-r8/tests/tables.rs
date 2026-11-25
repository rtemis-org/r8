//! Named tables: the declaration, validation, documents, equality, and hashes of
//! a type whose values hold columns.

use rtemis_r8::access::Json;
use rtemis_r8::canonical;
use rtemis_r8::ir::{IrDocument, check_document};
use rtemis_r8::name::QualifiedName;
use rtemis_r8::validate::validate;
use serde_json::{Value, json};

fn document() -> IrDocument {
    let doc: IrDocument = serde_json::from_value(json!({
        "$ir": 1,
        "namespace": "ns",
        "types": [
            {
                "kind": "table", "name": "Vitals", "description": "", "version": 1, "open": true,
                "columns": [
                    {"name": "age", "type": {"kind": "double", "min": 0.0}, "unit": "a"},
                    {"name": "sbp", "type": {"kind": "option", "inner": {"kind": "double"}}},
                    {"name": "id", "type": {"kind": "integer"}, "unique": true, "required": false}
                ],
                "rules": [{"id": "ADULT", "invariant": {"<": [{"var": "age"}, 130]}}]
            },
            {
                "kind": "table", "name": "Closed", "description": "", "version": 1,
                "columns": [{"name": "x", "type": {"kind": "integer"}}]
            },
            {
                "kind": "table", "name": "Raven", "description": "", "version": 1, "open": true,
                "columns": []
            },
            {
                "kind": "struct", "name": "Fit", "description": "", "version": 1,
                "fields": [{"name": "cases", "type": {"kind": "named", "name": "ns::Raven"}}]
            }
        ]
    }))
    .unwrap();
    assert_eq!(check_document(&doc), vec![]);
    doc
}

fn q(s: &str) -> QualifiedName {
    QualifiedName::parse(s).unwrap()
}

fn codes(v: &Value, ty: &str) -> Vec<(&'static str, String)> {
    let mut out: Vec<_> = validate(v, &q(ty), &document())
        .into_iter()
        .map(|e| (e.code, e.path))
        .collect();
    out.sort();
    out
}

fn raven() -> Value {
    json!({
        "$length": 2,
        "Sepal.Length": [5.1, "NaN"],
        "Age (years)": [40.0, null],
        "if": ["a", null],
        "$columns": {
            "Sepal.Length": {"kind": "option", "inner": {"kind": "double", "infinite": true, "nan": true}},
            "Age (years)": {"kind": "option", "inner": {"kind": "double"}},
            "if": {"kind": "option", "inner": {"kind": "character"}}
        },
        "$order": ["if", "Sepal.Length", "Age (years)"],
        "$facts": {"Age (years)": {"unit": "a", "description": "Age"}}
    })
}

#[test]
fn a_table_reads_its_columns_and_others_of_any_name() {
    assert_eq!(codes(&raven(), "ns::Raven"), vec![]);
    let v = json!({"$length": 2, "age": [1.0, 2.0], "sbp": [null, 3.0]});
    assert_eq!(codes(&v, "ns::Vitals"), vec![]);
    assert_eq!(codes(&json!({"$length": 0}), "ns::Raven"), vec![]);
}

#[test]
fn a_table_is_checked_by_its_declaration() {
    let v = json!({"$length": 2, "age": [1.0, -2.0], "id": [1, 1]});
    assert_eq!(
        codes(&v, "ns::Vitals"),
        vec![
            ("below_min", "/age/1".to_string()),
            ("missing_column", "/sbp".to_string()),
            ("not_unique", "/id/1".to_string()),
        ]
    );
    let v = json!({"$length": 2, "age": [1.0], "sbp": [1.0, 2.0]});
    assert_eq!(
        codes(&v, "ns::Vitals"),
        vec![("column_length", "/age".to_string())]
    );
    let v = json!({"$length": 2, "age": [1.0, 130.0], "sbp": [1.0, 2.0]});
    assert_eq!(
        codes(&v, "ns::Vitals"),
        vec![("rule_violation", "/age/1".to_string())]
    );
    let v = json!({"$length": 1, "x": [1], "y": [2]});
    assert_eq!(
        codes(&v, "ns::Closed"),
        vec![("unknown_column", "/y".to_string())]
    );
}

#[test]
fn extra_columns_state_their_order_and_facts() {
    let mut v = raven();
    v["$order"] = json!(["if", "Sepal.Length"]);
    assert_eq!(
        codes(&v, "ns::Raven"),
        vec![("column_order", "/$order".to_string())]
    );
    v.as_object_mut().unwrap().remove("$order");
    assert_eq!(
        codes(&v, "ns::Raven"),
        vec![("column_order", "/$order".to_string())]
    );
    let mut v = raven();
    v["$facts"] = json!({"if": {"unit": " a"}});
    assert_eq!(
        codes(&v, "ns::Raven"),
        vec![("unit_blank", "/if".to_string())]
    );
    v["$facts"] = json!({"if": {"units": "a"}});
    assert_eq!(
        codes(&v, "ns::Raven"),
        vec![("column_facts", "/$facts/if".to_string())]
    );
    v["$facts"] = json!({"absent": {"unit": "a"}});
    assert_eq!(
        codes(&v, "ns::Raven"),
        vec![("column_facts", "/$facts/absent".to_string())]
    );
    let mut v = raven();
    v["$columns"].as_object_mut().unwrap().remove("if");
    assert_eq!(
        codes(&v, "ns::Raven"),
        vec![("column_type", "/$columns/if".to_string())]
    );
}

#[test]
fn a_document_reads_back_with_its_order_and_facts() {
    let doc = document();
    let ty = q("ns::Raven");
    let v = raven();
    let text = canonical::to_document(&v, &ty, &doc).unwrap();
    assert!(
        text.contains(r#""$order":["if","Sepal.Length","Age (years)"]"#),
        "{text}"
    );
    let back = canonical::read_document(&text, &ty, &doc).unwrap();
    assert_eq!(canonical::to_document(&back, &ty, &doc).unwrap(), text);
    // Within a struct.
    let fit = json!({"cases": v});
    let ty = q("ns::Fit");
    let text = canonical::to_document(&fit, &ty, &doc).unwrap();
    let back = canonical::read_document(&text, &ty, &doc).unwrap();
    assert_eq!(canonical::to_document(&back, &ty, &doc).unwrap(), text);
}

#[test]
fn equality_and_the_hash_agree_on_order_and_units() {
    let doc = document();
    let ty = q("ns::Raven");
    let hash = |v: &Value| canonical::content_hash(v, &ty, &doc).unwrap();
    let equal = |a: &Value, b: &Value| {
        rtemis_r8::equal::equal_access(&ty, Json::new(a), Json::new(b), &doc)
    };
    let v = raven();
    let mut order = raven();
    order["$order"] = json!(["Sepal.Length", "if", "Age (years)"]);
    let mut unit = raven();
    unit["$facts"]["Age (years)"]["unit"] = json!("mo");
    let mut cell = raven();
    cell["if"][0] = json!("b");
    let mut described = raven();
    described["$facts"]["Age (years)"]["description"] = json!("Age at admission");
    for other in [&order, &unit, &cell] {
        assert_ne!(hash(&v), hash(other));
        assert!(!equal(&v, other));
    }
    assert_eq!(hash(&v), hash(&described));
    assert!(equal(&v, &described));
    // The members of a document have no order of their own.
    let mut moved = serde_json::Map::new();
    for (k, x) in raven().as_object().unwrap().iter().rev() {
        moved.insert(k.clone(), x.clone());
    }
    let moved = Value::Object(moved);
    assert_eq!(hash(&v), hash(&moved));
    assert!(equal(&v, &moved));
}

#[test]
fn a_declaration_of_a_table_is_checked() {
    let check = |t: Value| -> Vec<&'static str> {
        let doc: IrDocument =
            serde_json::from_value(json!({"$ir": 1, "namespace": "ns", "types": [t]})).unwrap();
        check_document(&doc).into_iter().map(|e| e.code).collect()
    };
    let table =
        |columns: Value| json!({"kind": "table", "name": "T", "version": 1, "columns": columns});
    assert_eq!(
        check(table(
            json!([{"name": "x", "type": {"kind": "vec", "element": {"kind": "double"}}}])
        )),
        vec!["table_column"]
    );
    assert_eq!(
        check(table(
            json!([{"name": "x", "type": {"kind": "double"}, "default": 1.0}])
        )),
        vec!["column_default"]
    );
    assert_eq!(
        check(table(json!([
            {"name": "x", "type": {"kind": "double"}},
            {"name": "x", "type": {"kind": "double"}}
        ]))),
        vec!["duplicate_field"]
    );
}
