//! Runs the conformance corpus in `conformance/` at the repository root.
//!
//! Each suite is validated against `conformance/corpus.schema.json` before its
//! cases run. Operations `r8-core` does not implement yet are skipped and
//! counted.

use std::path::{Path, PathBuf};

use r8_core::ir::{IrDocument, check_document};
use serde_json::Value;

fn corpus_dir() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("../../conformance")
}

fn suites(dir: &Path) -> Vec<PathBuf> {
    let mut out = Vec::new();
    for entry in std::fs::read_dir(dir).expect("conformance directory") {
        let path = entry.unwrap().path();
        if path.is_dir() {
            out.extend(suites(&path));
        } else if path.extension().is_some_and(|e| e == "json")
            && path.file_name().is_some_and(|n| n != "corpus.schema.json")
        {
            out.push(path);
        }
    }
    out.sort();
    out
}

/// `(path, code, rule)` triples, sorted, for comparison as multisets.
fn sorted_errors(
    errors: impl Iterator<Item = (String, String, Option<String>)>,
) -> Vec<(String, String, Option<String>)> {
    let mut v: Vec<_> = errors.collect();
    v.sort();
    v
}

fn expected_errors(expect: &Value) -> Vec<(String, String, Option<String>)> {
    sorted_errors(expect["errors"].as_array().unwrap().iter().map(|e| {
        (
            e["path"].as_str().unwrap().to_string(),
            e["code"].as_str().unwrap().to_string(),
            e.get("rule").and_then(Value::as_str).map(str::to_string),
        )
    }))
}

/// Runs one `declare` case and returns a failure description, if any.
fn run_declare(input: &Value, expect: &Value) -> Option<String> {
    let parsed = IrDocument::from_json(&input.to_string());
    let wants_parse_error = expect.get("parse_error").is_some();
    match (parsed, wants_parse_error) {
        (Err(_), true) => None,
        (Err(e), false) => Some(format!("did not read: {e}")),
        (Ok(_), true) => Some("read, but a parse error was expected".to_string()),
        (Ok(doc), false) => {
            let got = sorted_errors(
                check_document(&doc)
                    .into_iter()
                    .map(|e| (e.path, e.code.to_string(), None)),
            );
            let want = expected_errors(expect);
            (got != want).then(|| format!("errors {got:?}, expected {want:?}"))
        }
    }
}

#[test]
fn conformance_corpus() {
    let dir = corpus_dir();
    let schema: Value =
        serde_json::from_str(&std::fs::read_to_string(dir.join("corpus.schema.json")).unwrap())
            .unwrap();
    let validator = jsonschema::validator_for(&schema).expect("the corpus schema compiles");

    let (mut passed, mut skipped, mut failures) = (0usize, 0usize, Vec::new());
    for path in suites(&dir) {
        let name = path.strip_prefix(&dir).unwrap().display().to_string();
        let suite: Value = serde_json::from_str(&std::fs::read_to_string(&path).unwrap())
            .unwrap_or_else(|e| panic!("{name}: not JSON: {e}"));
        let problems: Vec<String> = validator
            .iter_errors(&suite)
            .map(|e| format!("{e} at {}", e.instance_path()))
            .collect();
        assert!(
            problems.is_empty(),
            "{name} does not match the corpus schema: {problems:#?}"
        );

        let mut ids = std::collections::HashSet::new();
        for case in suite["cases"].as_array().unwrap() {
            let id = case["id"].as_str().unwrap();
            assert!(ids.insert(id), "{name}: duplicate case id `{id}`");
            let outcome = match case["op"].as_str().unwrap() {
                "declare" => run_declare(&case["input"], &case["expect"]),
                _ => {
                    skipped += 1;
                    continue;
                }
            };
            match outcome {
                None => passed += 1,
                Some(why) => failures.push(format!("{name} {id}: {why}")),
            }
        }
    }
    println!("conformance: {passed} passed, {skipped} skipped");
    assert!(failures.is_empty(), "{failures:#?}");
    assert!(passed > 0, "no conformance case ran");
}
