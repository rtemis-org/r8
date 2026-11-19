//! M1's exit test: the JSON Schema that `json_schema()` exports for a type
//! accepts every valid value of the corpus and the normalized form the codec
//! writes, and rejects every invalid value whose error JSON Schema expresses.

use std::path::{Path, PathBuf};

use rtemis_r8::codec::{normalize, to_document};
use rtemis_r8::ir::{FieldType, IrDocument, TypeDef};
use rtemis_r8::name::QualifiedName;
use rtemis_r8::rules::{DocumentSet, TypeLookup};
use rtemis_r8::value_schema::json_schema;
use serde_json::Value;

/// Error codes whose every instance a JSON Schema rejects. The others (rules,
/// distinct keys and cells, column lengths, factor codes beyond the levels,
/// the calendar and canonical forms of dates and bytes, sparse indices, and
/// types outside the lookup) are R8's validation alone, as is every error
/// inside a `dyn()` field, whose implementors an open schema does not list.
const STRUCTURAL: &[&str] = &[
    "type_mismatch",
    "missing_field",
    "unknown_field",
    "missing_value",
    "below_min",
    "above_max",
    "not_integer",
    "out_of_range",
    "not_in_enum",
    "pattern_mismatch",
    "not_a_level",
    "factor_form",
    "length_below_min",
    "length_above_max",
    "missing_column",
    "unknown_column",
    "unknown_variant",
    "missing_tag",
    "not_finite",
    "runtime_only",
    "dimension_mismatch",
];

fn suites(dir: &Path) -> Vec<PathBuf> {
    let mut out = Vec::new();
    for entry in std::fs::read_dir(dir).unwrap() {
        let path = entry.unwrap().path();
        if path.is_dir() {
            out.extend(suites(&path));
        } else if path.extension().is_some_and(|e| e == "json")
            && path.file_name().is_some_and(|n| n != "corpus.schema.json")
        {
            out.push(path);
        }
    }
    out
}

/// `true` when an expected error lies inside a `dyn()` field of the root
/// struct.
fn under_dyn(docs: &DocumentSet, ty: &QualifiedName, expect: &Value) -> bool {
    let Some(TypeDef::Struct(s)) = docs.find(ty) else {
        return false;
    };
    let dyn_fields: Vec<&str> = s
        .fields
        .iter()
        .filter(|f| matches!(&f.ty, FieldType::Dyn { .. }) || matches!(&f.ty, FieldType::Option { inner, .. } if matches!(**inner, FieldType::Dyn { .. })))
        .map(|f| f.name.as_str())
        .collect();
    expect["errors"].as_array().unwrap().iter().any(|e| {
        let first = e["path"]
            .as_str()
            .unwrap()
            .split('/')
            .nth(1)
            .unwrap_or_default();
        dyn_fields.contains(&first)
    })
}

#[test]
fn exported_schemas_agree_with_validation() {
    let dir = Path::new(env!("CARGO_MANIFEST_DIR")).join("../../conformance");
    let (mut accepted, mut rejected, mut failures) = (0, 0, Vec::new());
    for path in suites(&dir) {
        let suite: Value = serde_json::from_str(&std::fs::read_to_string(&path).unwrap()).unwrap();
        let Some(ir) = suite.get("ir") else { continue };
        let docs = DocumentSet(match ir {
            Value::Array(a) => a
                .iter()
                .map(|d| IrDocument::from_json(&d.to_string()).unwrap())
                .collect(),
            one => vec![IrDocument::from_json(&one.to_string()).unwrap()],
        });
        let name = path.strip_prefix(&dir).unwrap().display().to_string();
        for case in suite["cases"].as_array().unwrap() {
            let ty = QualifiedName::parse(case["type"].as_str().unwrap()).unwrap();
            let schema = json_schema(&ty, &docs);
            let validator = jsonschema::validator_for(&schema)
                .unwrap_or_else(|e| panic!("{name}: the schema of {ty} does not compile: {e}"));
            let id = case["id"].as_str().unwrap();
            let input = &case["input"];
            let codes: Vec<&str> = case["expect"]["errors"]
                .as_array()
                .map(|a| a.iter().map(|e| e["code"].as_str().unwrap()).collect())
                .unwrap_or_default();
            if codes.is_empty() {
                let document: Value =
                    serde_json::from_str(&to_document(input, &ty, &docs).unwrap()).unwrap();
                for (what, doc) in [
                    ("input", input.clone()),
                    ("normalized form", normalize(input, &ty, &docs).unwrap()),
                    ("document", document),
                ] {
                    if validator.is_valid(&doc) {
                        accepted += 1;
                    } else {
                        let why: Vec<String> = validator
                            .iter_errors(&doc)
                            .map(|e| format!("{e} at {}", e.instance_path()))
                            .collect();
                        failures.push(format!(
                            "{name} {id}: the schema rejects the valid {what}: {why:?}"
                        ));
                    }
                }
            } else if codes.iter().all(|c| STRUCTURAL.contains(c))
                && !under_dyn(&docs, &ty, &case["expect"])
            {
                if validator.is_valid(input) {
                    failures.push(format!(
                        "{name} {id}: the schema accepts a value with {codes:?}"
                    ));
                } else {
                    rejected += 1;
                }
            }
        }
    }
    println!("schemas: {accepted} valid documents accepted, {rejected} invalid values rejected");
    assert!(failures.is_empty(), "{failures:#?}");
    assert!(
        accepted > 50 && rejected > 20,
        "{accepted} accepted, {rejected} rejected"
    );
}

/// Prints the schema of the plan's Svm type (`--ignored --nocapture`).
#[test]
#[ignore]
fn print_svm_schema() {
    let doc = IrDocument::from_json(include_str!("fixtures/svm.ir.json")).unwrap();
    let ty = QualifiedName::parse("rtemis::Svm").unwrap();
    println!(
        "{}",
        serde_json::to_string_pretty(&json_schema(&ty, &doc)).unwrap()
    );
}
