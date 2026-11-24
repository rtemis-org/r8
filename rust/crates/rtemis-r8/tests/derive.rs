//! `#[derive(R8)]`: the plan's Kernel and Svm declared in Rust produce the
//! same IR as the fixture, which the R declarations produce, and encode and
//! decode through the same validation.
#![cfg(feature = "macros")]

use std::collections::BTreeMap;

use rtemis_r8::R8;
use rtemis_r8::canonical::{normalize, to_document as write_doc};
use rtemis_r8::derive::{Bytes, Encode, R8 as _};
use rtemis_r8::ir::{IrDocument, TypeDef};
use rtemis_r8::rules::TypeLookup;
use serde_json::json;

/// Kernel function.
#[derive(R8, Debug, Clone, PartialEq)]
#[r8(namespace = "rtemis", version = 1)]
enum Kernel {
    Linear,
    Rbf {
        /// Kernel width.
        #[r8(exclusive_min = 0)]
        gamma: f64,
    },
}

/// Support vector machine settings.
#[derive(R8, Debug, Clone, PartialEq)]
#[r8(namespace = "rtemis", version = 1)]
#[r8(rule(
    id = "RBF_GAMMA",
    invariant = r#"{"<=": [{"var": "kernel.gamma"}, 10]}"#,
    message = "gamma is at most 10."
))]
#[r8(rule(
    id = "RBF_COST",
    invariant = r#"{"<=": [{"var": "cost"}, 100]}"#,
    when = r#"{"is_variant": [{"var": "kernel"}, "Rbf"]}"#,
    message = "cost is at most 100 with the Rbf kernel."
))]
struct Svm {
    /// Cost of a margin violation.
    #[r8(exclusive_min = 0, default = "1.0")]
    cost: f64,
    /// Kernel function.
    #[r8(default = r#"{"type": "Linear"}"#)]
    kernel: Kernel,
    /// Case weights.
    #[r8(min = 0)]
    weights: Option<Vec<f64>>,
    /// Variables per split.
    #[r8(min = 1, context)]
    mtry: Option<i32>,
}

fn fixture() -> IrDocument {
    IrDocument::from_json(include_str!("fixtures/svm.ir.json")).unwrap()
}

#[test]
fn the_derived_declarations_equal_the_fixture() {
    let doc = fixture();
    assert_eq!(
        &Kernel::type_def(),
        doc.find(&Kernel::qualified_name()).unwrap()
    );
    assert_eq!(&Svm::type_def(), doc.find(&Svm::qualified_name()).unwrap());
    // Svm reaches Kernel, so its declarations hold both.
    let set = Svm::declarations();
    assert_eq!(set.documents().len(), 1);
    assert_eq!(set.documents()[0].types.len(), 2);
}

#[test]
fn values_encode_to_the_json_form_and_round_trip() {
    let svm = Svm {
        cost: 2.0,
        kernel: Kernel::Rbf { gamma: 0.5 },
        weights: Some(vec![1.0, 2.5]),
        mtry: None,
    };
    let v = svm.encode().unwrap();
    assert_eq!(
        v,
        json!({"cost": 2.0, "kernel": {"gamma": 0.5, "type": "Rbf"}, "weights": [1.0, 2.5], "mtry": null})
    );
    let text = svm.to_document().unwrap();
    // The document a derived value writes equals the one `canonical::to_document` writes
    // from the same JSON against the fixture's declarations.
    assert_eq!(
        text,
        write_doc(&v, &Svm::qualified_name(), &fixture()).unwrap()
    );
    assert_eq!(Svm::from_document(&text).unwrap(), svm);
    assert_eq!(
        Svm::decode(&normalize(&v, &Svm::qualified_name(), &fixture()).unwrap()).unwrap(),
        svm
    );
}

#[test]
fn rules_and_constraints_apply_to_derived_values() {
    let gamma = Svm {
        cost: 1.0,
        kernel: Kernel::Rbf { gamma: 20.0 },
        weights: None,
        mtry: None,
    };
    let errors = gamma.to_document().unwrap_err();
    assert_eq!(errors.len(), 1);
    assert_eq!(
        (
            errors[0].code,
            errors[0].rule.as_ref().map(|r| r.to_string())
        ),
        ("rule_violation", Some("RBF_GAMMA".to_string()))
    );
    let cost = Svm {
        cost: -1.0,
        kernel: Kernel::Linear,
        weights: None,
        mtry: Some(0),
    };
    let codes: Vec<_> = cost
        .to_document()
        .unwrap_err()
        .into_iter()
        .map(|e| (e.code, e.path))
        .collect();
    assert_eq!(
        codes,
        vec![
            ("below_min", "/cost".to_string()),
            ("below_min", "/mtry".to_string())
        ]
    );
}

#[test]
fn values_outside_the_r8_domain_do_not_encode() {
    let na = Svm {
        cost: 1.0,
        kernel: Kernel::Linear,
        weights: None,
        mtry: Some(i32::MIN),
    };
    assert_eq!(na.to_document().unwrap_err()[0].code, "encode_failed");
}

/// A unit-only enum and the remaining encodings.
#[derive(R8, Debug, Clone, PartialEq)]
#[r8(namespace = "demo", version = 1)]
enum Figure {
    Circle,
    Square,
}

/// A struct holding every encoding.
#[derive(R8, Debug, Clone, PartialEq)]
#[r8(namespace = "demo", version = 2)]
struct Every {
    flag: bool,
    small: u8,
    big: u64,
    #[r8(pattern = "^[a-z]+$")]
    word: String,
    #[r8(min_length = 1, unique)]
    tags: Vec<String>,
    counts: BTreeMap<String, i32>,
    shape: Figure,
    blob: Bytes,
    meta: serde_json::Value,
    inner: Option<Box<Every>>,
}

impl Encode for Box<Every> {
    fn field_type() -> rtemis_r8::ir::FieldType {
        Every::field_type()
    }
    fn encode(&self) -> Result<serde_json::Value, String> {
        self.as_ref().encode()
    }
    fn decode(v: &serde_json::Value) -> Result<Self, String> {
        Every::decode(v).map(Box::new)
    }
    fn register(set: &mut rtemis_r8::rules::DocumentSet) {
        Every::register(set);
    }
}

#[test]
fn every_encoding_round_trips_including_a_recursive_type() {
    let leaf = Every {
        flag: false,
        small: 7,
        big: 9_007_199_254_740_991,
        word: "leaf".into(),
        tags: vec!["a".into()],
        counts: BTreeMap::from([("x".to_string(), 1)]),
        shape: Figure::Square,
        blob: Bytes(vec![0, 1, 2, 250]),
        meta: json!({"any": [1, "two"]}),
        inner: None,
    };
    let root = Every {
        inner: Some(Box::new(leaf.clone())),
        shape: Figure::Circle,
        ..leaf.clone()
    };
    let text = root.to_document().unwrap();
    assert!(
        text.starts_with(r#"{"$type":"demo::Every","$types":{"demo::Every":2,"demo::Figure":1}"#),
        "{text}"
    );
    assert_eq!(Every::from_document(&text).unwrap(), root);
    let TypeDef::Enum(shape) = Figure::type_def() else {
        panic!()
    };
    assert_eq!(shape.variants.len(), 2);
    let bad = Every {
        word: "Leaf".into(),
        tags: vec![],
        ..leaf
    };
    let codes: Vec<_> = bad
        .to_document()
        .unwrap_err()
        .into_iter()
        .map(|e| (e.code, e.path))
        .collect();
    assert_eq!(
        codes,
        vec![
            ("pattern_mismatch", "/word".to_string()),
            ("length_below_min", "/tags".to_string())
        ]
    );
}

/// Ranges of a feature.
#[derive(R8, Debug, Clone, PartialEq)]
#[r8(namespace = "demo", version = 1)]
struct Ranges {
    #[r8(min = 0)]
    start: i32,
}

/// Features: plain names, or ranges.
#[derive(R8, Debug, Clone, PartialEq)]
#[r8(namespace = "demo", version = 1, tag = "kind")]
enum Rows {
    Named { names: Vec<String> },
    Ranged(Ranges),
}

#[test]
fn a_newtype_variant_is_flattened_with_the_tag() {
    let r = Rows::Ranged(Ranges { start: 5 });
    assert_eq!(r.encode().unwrap(), json!({"start": 5, "kind": "Ranged"}));
    let text = r.to_document().unwrap();
    assert_eq!(
        text,
        r#"{"$type":"demo::Rows","$types":{"demo::Ranges":1,"demo::Rows":1},"$declarations":{"demo::Ranges":"sha256:5470bf935b42395179cbb7b6109f38213f8d6ecc62ef8c99547dcde5e5de7431","demo::Rows":"sha256:53dd3a0d902ef3d1044a91ab460aa46de4c47e8d13dbf07bf05297953f7a3fd6"},"start":5,"kind":"Ranged"}"#
    );
    assert_eq!(Rows::from_document(&text).unwrap(), r);
    let bad = Rows::Ranged(Ranges { start: -1 });
    assert_eq!(bad.to_document().unwrap_err()[0].code, "below_min");
    assert!(rtemis_r8::ir::check_document(&Rows::declarations().documents()[0]).is_empty());
}
