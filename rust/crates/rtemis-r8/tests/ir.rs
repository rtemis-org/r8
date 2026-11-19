//! IR documents: reading and writing through the Rust API. The declaration
//! checks are tested by the conformance corpus (`tests/conformance.rs`).

use rtemis_r8::ir::{IrDocument, ReadError, check_document};
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
    let rtemis_r8::ir::TypeDef::Struct(svm) = &mut d.types[1] else {
        panic!("the fixture's second type is Svm");
    };
    let rtemis_r8::ir::FieldType::Double(cost) = &mut svm.fields[0].ty else {
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

/// The R package's test fixtures, one IR document per type, hold the
/// declarations of this crate's fixture, so the R and Rust tests check one
/// IR.
#[test]
fn the_r_fixtures_match_this_fixture() {
    let d = IrDocument::from_json(SVM).unwrap();
    for (file, index) in [("kernel.ir.json", 0), ("svm.ir.json", 1)] {
        let path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("../../../r/tests/testthat/fixtures")
            .join(file);
        let r = IrDocument::from_json(&std::fs::read_to_string(&path).unwrap()).unwrap();
        assert_eq!(r.namespace, d.namespace, "{file}");
        assert_eq!(r.types, vec![d.types[index].clone()], "{file}");
    }
}
