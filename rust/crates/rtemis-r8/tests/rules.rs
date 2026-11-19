//! Evaluation of compiled rules over JSON values.

use rtemis_r8::ir::{FieldType, IrDocument, RuleDef, TypeDef};
use rtemis_r8::rules::{CompiledRule, Outcome};
use serde_json::{Value, json};

const SVM: &str = include_str!("fixtures/svm.ir.json");
const BIOC: &str = include_str!("../../../../conformance/declare/exercise/bioconductor.json");

fn svm_doc() -> IrDocument {
    IrDocument::from_json(SVM).unwrap()
}

/// Compiles a rule given as JSON against struct `index` of `doc`.
fn compile(doc: &IrDocument, index: usize, rule: Value) -> CompiledRule {
    let TypeDef::Struct(s) = &doc.types[index] else {
        panic!("not a struct")
    };
    let rule: RuleDef = serde_json::from_value(rule).unwrap();
    CompiledRule::compile(&rule, &s.fields, doc).expect("rule compiles")
}

fn fixture_rule(doc: &IrDocument, id: &str) -> CompiledRule {
    let TypeDef::Struct(s) = &doc.types[1] else {
        panic!()
    };
    let r = s.rules.iter().find(|r| r.id.as_str() == id).unwrap();
    CompiledRule::compile(r, &s.fields, doc).unwrap()
}

fn violated(paths: &[&str]) -> Outcome {
    Outcome::Violated(paths.iter().map(|p| p.to_string()).collect())
}

#[test]
fn a_variant_field_is_absent_for_other_variants() {
    let doc = svm_doc();
    let gamma = fixture_rule(&doc, "RBF_GAMMA");
    let linear = json!({"cost": 1, "kernel": "Linear"});
    assert_eq!(gamma.check(&linear).unwrap(), Outcome::Holds);
    let small = json!({"cost": 1, "kernel": {"type": "Rbf", "gamma": 0.5}});
    assert_eq!(gamma.check(&small).unwrap(), Outcome::Holds);
    let large = json!({"cost": 1, "kernel": {"type": "Rbf", "gamma": 20}});
    assert_eq!(gamma.check(&large).unwrap(), violated(&[""]));
}

#[test]
fn when_selects_the_values_a_rule_applies_to() {
    let doc = svm_doc();
    let cost = fixture_rule(&doc, "RBF_COST");
    let rbf = json!({"cost": 500, "kernel": {"type": "Rbf", "gamma": 0.5}});
    assert_eq!(cost.check(&rbf).unwrap(), violated(&[""]));
    // Kernel has a struct variant, so a Linear value is an object too.
    let linear = json!({"cost": 500, "kernel": {"type": "Linear"}});
    assert_eq!(cost.check(&linear).unwrap(), Outcome::Holds);
}

#[test]
fn an_absent_operand_makes_the_rule_hold() {
    let doc = svm_doc();
    let r = compile(
        &doc,
        1,
        json!({"id": "MTRY_MAX", "invariant": {"<=": [{"var": "mtry"}, 10]}, "message": "m"}),
    );
    assert_eq!(
        r.check(&json!({"cost": 1, "mtry": null})).unwrap(),
        Outcome::Holds
    );
    assert_eq!(r.check(&json!({"cost": 1})).unwrap(), Outcome::Holds);
    assert_eq!(
        r.check(&json!({"cost": 1, "mtry": 50})).unwrap(),
        violated(&[""])
    );
}

#[test]
fn is_null_is_a_presence_test() {
    let doc = svm_doc();
    // b is required when a is set: here, weights requires mtry.
    let r = compile(
        &doc,
        1,
        json!({"id": "W", "invariant": {"or": [{"is_null": [{"var": "weights"}]}, {"!": [{"is_null": [{"var": "mtry"}]}]}]}, "message": "m"}),
    );
    assert_eq!(
        r.check(&json!({"weights": null, "mtry": null})).unwrap(),
        Outcome::Holds
    );
    assert_eq!(
        r.check(&json!({"weights": [1, 2], "mtry": 3})).unwrap(),
        Outcome::Holds
    );
    assert_eq!(
        r.check(&json!({"weights": [1, 2], "mtry": null})).unwrap(),
        violated(&[""])
    );
}

#[test]
fn conditions_take_logicals_only() {
    // A `type_json()` field compiles as any value; the evaluator still
    // refuses a number where a logical belongs.
    let doc = IrDocument::from_json(&json!({"$ir": 1, "namespace": "ns", "types": [
        {"kind": "struct", "name": "T", "version": 1, "fields": [{"name": "meta", "type": {"kind": "json"}}]}
    ]}).to_string()).unwrap();
    let r = compile(
        &doc,
        0,
        json!({"id": "T", "invariant": {"and": [{"var": "meta"}, true]}, "message": "m"}),
    );
    assert_eq!(r.check(&json!({"meta": true})).unwrap(), Outcome::Holds);
    let err = r.check(&json!({"meta": 1})).unwrap_err();
    assert!(err.contains("logical"), "{err}");
}

fn bioc_doc(case: &str) -> IrDocument {
    let suite: Value = serde_json::from_str(BIOC).unwrap();
    let input = suite["cases"]
        .as_array()
        .unwrap()
        .iter()
        .find(|c| c["id"] == case)
        .unwrap()["input"]
        .clone();
    serde_json::from_value(input).unwrap()
}

fn rule_of(doc: &IrDocument, ty: &str, id: &str) -> CompiledRule {
    let s = doc
        .types
        .iter()
        .find_map(|t| match t {
            TypeDef::Struct(s) if s.name.as_str() == ty => Some(s),
            _ => None,
        })
        .unwrap();
    let r = s.rules.iter().find(|r| r.id.as_str() == id).unwrap();
    CompiledRule::compile(r, &s.fields, doc).unwrap()
}

#[test]
fn all_reports_each_failing_element_of_an_ordered_map() {
    let doc = bioc_doc("summarized-experiment");
    let rows = rule_of(&doc, "SummarizedExperiment", "ASSAY_ROWS");
    let matrix = |r: u64| json!({"type": "Double", "values": {"shape": [r, 2], "data": vec![0.0; (r * 2) as usize]}});
    let se = json!({
        "assays": [{"name": "counts", "value": matrix(3)}, {"name": "logcounts", "value": matrix(4)}, {"name": "tpm", "value": matrix(5)}],
        "row_data": {"$length": 3},
        "col_data": {"$length": 2},
        "metadata": {}
    });
    assert_eq!(
        rows.check(&se).unwrap(),
        violated(&["/assays/1/value", "/assays/2/value"])
    );
}

#[test]
fn row_rules_report_the_cell_and_decode_factors() {
    let doc = bioc_doc("granges");
    let TypeDef::Struct(gr) = &doc.types[0] else {
        panic!()
    };
    let FieldType::Table { columns, rules, .. } = &gr.fields[0].ty else {
        panic!()
    };
    let end_fits = CompiledRule::compile(&rules[0], columns, &doc).unwrap();
    let ranges = json!({
        "$length": 3,
        "seqnames": {"levels": ["chr1", "chr2"], "codes": [1, 1, 2]},
        "start": [1, 2147483640, 5],
        "width": [10, 100, 0],
        "strand": ["+", "-", "*"]
    });
    assert_eq!(
        end_fits.check_rows(&ranges).unwrap(),
        vec!["/start/1".to_string()]
    );
}

#[test]
fn levels_reads_data_levels_and_compares_arrays_in_order() {
    let doc = bioc_doc("granges");
    let r = rule_of(&doc, "GRanges", "SEQNAMES_LEVELS");
    let gr = |seqnames: Value| {
        json!({
            "ranges": {"$length": 1, "seqnames": {"levels": ["chr1", "chr2"], "codes": [1]}, "start": [1], "width": [1], "strand": ["*"]},
            "mcols": {"$length": 1},
            "seqinfo": {"seqnames": seqnames, "seqlengths": [null, null], "is_circular": [null, null], "genome": [null, null]},
            "metadata": {}
        })
    };
    assert_eq!(
        r.check(&gr(json!(["chr1", "chr2"]))).unwrap(),
        Outcome::Holds
    );
    assert_eq!(
        r.check(&gr(json!(["chr2", "chr1"]))).unwrap(),
        violated(&[""])
    );
}

#[test]
fn division_by_zero_is_an_error_not_a_pass() {
    let doc = svm_doc();
    let r = compile(
        &doc,
        1,
        json!({"id": "D", "invariant": {"<": [{"/": [{"var": "cost"}, 0]}, 1]}, "message": "m"}),
    );
    assert!(r.check(&json!({"cost": 1})).is_err());
}

#[test]
fn round_halves_to_even_as_r_does() {
    let doc = svm_doc();
    for (x, want) in [(0.5, 0), (1.5, 2), (2.5, 2), (-0.5, 0)] {
        let r = compile(
            &doc,
            1,
            json!({"id": "R", "invariant": {"===": [{"round": [{"var": "cost"}]}, want]}, "message": "m"}),
        );
        assert_eq!(
            r.check(&json!({"cost": x})).unwrap(),
            Outcome::Holds,
            "round({x})"
        );
    }
}
