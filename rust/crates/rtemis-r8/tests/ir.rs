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
    cost.upper = Some(rtemis_r8::ir::Bound::inclusive(f64::INFINITY));
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

/// Each side of a numeric range holds one bound: inclusive (`min`, `max`) or
/// exclusive (`exclusive_min`, `exclusive_max`), each written back as read.
#[test]
fn a_numeric_type_holds_one_bound_per_side() {
    use rtemis_r8::ir::{Bound, DoubleType, FieldType, IntegerType};
    let read = |v: serde_json::Value| serde_json::from_value::<FieldType>(v);
    for v in [
        json!({"kind": "double", "min": 0.0, "max": 1.0}),
        json!({"kind": "double", "exclusive_min": 0.0, "exclusive_max": 1.0}),
        json!({"kind": "double", "max": 1.0, "exclusive_min": 0.0}),
        json!({"kind": "integer", "min": 1, "exclusive_max": 10}),
        json!({"kind": "double", "finite": false}),
    ] {
        assert_eq!(serde_json::to_value(read(v.clone()).unwrap()).unwrap(), v);
    }
    for v in [
        json!({"kind": "double", "min": 0.0, "exclusive_min": 0.0}),
        json!({"kind": "integer", "max": 3, "exclusive_max": 4}),
    ] {
        let e = read(v).unwrap_err().to_string();
        assert!(e.contains("not both"), "{e}");
    }
    let d = DoubleType {
        lower: Some(Bound::exclusive(0.0)),
        upper: Some(Bound::inclusive(1.0)),
        ..DoubleType::default()
    };
    assert!(!d.in_range(0.0) && d.in_range(1e-300) && d.in_range(1.0) && !d.in_range(1.0001));
    assert_eq!(
        (d.min(), d.exclusive_min(), d.max()),
        (None, Some(0.0), Some(1.0))
    );
    let i = IntegerType {
        lower: Some(Bound::inclusive(1)),
        upper: Some(Bound::exclusive(10)),
        values: None,
    };
    assert!(!i.in_range(0) && i.in_range(1) && i.in_range(9) && !i.in_range(10));
}

/// A parameter's `null` default, of a parameter of type `option()`, is read,
/// written, and recorded apart from a parameter without a default.
#[test]
fn a_null_parameter_default_is_not_an_absent_one() {
    use rtemis_r8::ir::{ParamDef, TraitDef};
    use rtemis_r8::record::{self, Declaration, DeclarationRef};
    let text = json!({
        "name": "Fit",
        "description": "",
        "methods": [{
            "name": "fit",
            "description": "",
            "params": [
                "x",
                {"name": "n", "type": {"kind": "option", "inner": {"kind": "integer"}}, "default": null},
                {"name": "m", "type": {"kind": "option", "inner": {"kind": "integer"}}},
            ],
        }],
    });
    let tr: TraitDef = serde_json::from_value(text.clone()).unwrap();
    let given: Vec<bool> = tr.methods[0]
        .params
        .iter()
        .map(|p| matches!(p, ParamDef::Full(q) if q.default.is_some()))
        .collect();
    assert_eq!(given, [false, true, false]);
    assert_eq!(serde_json::to_value(&tr).unwrap(), text);

    let ns = rtemis_r8::name::Name::new("ns").unwrap();
    let read = record::decode(&record::encode([(&ns, DeclarationRef::Trait(&tr))])).unwrap();
    assert!(matches!(&read[0].declaration, Declaration::Trait(t) if t == &tr));
}
