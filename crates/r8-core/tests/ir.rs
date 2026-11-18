//! IR documents: reading and writing through the Rust API. The declaration
//! checks are tested by the conformance corpus (`tests/conformance.rs`).

use r8_core::ir::{IrDocument, ReadError, check_document};
use serde_json::{Value, json};

const SVM: &str = include_str!("fixtures/svm.ir.json");

#[test]
fn the_plan_example_passes_and_round_trips() {
    let d = IrDocument::from_json(SVM).expect("fixture parses");
    assert_eq!(check_document(&d), vec![]);
    let written = d.to_json();
    assert_eq!(IrDocument::from_json(&written).unwrap(), d);
    // The fixture is written in the canonical form: the same members, values,
    // and order as the writer produces.
    let fixture: Value = serde_json::from_str(SVM).unwrap();
    let rewritten: Value = serde_json::from_str(&written).unwrap();
    assert_eq!(fixture, rewritten);
}

#[test]
fn another_format_version_is_rejected() {
    let mut v: Value = serde_json::from_str(SVM).unwrap();
    v["$ir"] = json!(2);
    assert!(matches!(
        IrDocument::from_json(&v.to_string()),
        Err(ReadError::Version { found: 2 })
    ));
}

/// A non-finite bound has no JSON form, so this check is reached through the
/// Rust API only.
#[test]
fn a_non_finite_bound_is_reported() {
    let mut d = IrDocument::from_json(SVM).unwrap();
    let r8_core::ir::TypeDef::Struct(svm) = &mut d.types[1] else {
        panic!("the fixture's second type is Svm");
    };
    let r8_core::ir::FieldType::Double(cost) = &mut svm.fields[0].ty else {
        panic!("Svm's first field is a double");
    };
    cost.max = Some(f64::INFINITY);
    let codes: Vec<_> = check_document(&d)
        .into_iter()
        .map(|e| (e.code, e.path))
        .collect();
    assert_eq!(
        codes,
        vec![("bound_not_finite", "/types/1/fields/0/type/max".to_string())]
    );
}
