//! Runs the conformance corpus in `conformance/` at the repository root.
//!
//! Each suite is validated against `conformance/corpus.schema.json` before its
//! cases run. Operations `rtemis-r8` does not implement yet are skipped and
//! counted.

use std::path::{Path, PathBuf};

use rtemis_r8::codec::{canonical, canonical_text, content_hash};
use rtemis_r8::ir::{IrDocument, check_document, check_type};
use rtemis_r8::name::QualifiedName;
use rtemis_r8::rules::DocumentSet;
use rtemis_r8::validate::validate;
use serde_json::Value;

fn corpus_dir() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("../../../conformance")
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
            let all = check_document(&doc);
            // The check of one type reports what the check of the document
            // reports at that type's path.
            for i in 0..doc.types.len() {
                let path = format!("/types/{i}");
                let prefix = format!("{path}/");
                let mut whole: Vec<(String, String)> = all
                    .iter()
                    .filter(|e| e.path == path || e.path.starts_with(&prefix))
                    .map(|e| (e.path.clone(), e.code.to_string()))
                    .collect();
                let mut one: Vec<(String, String)> = check_type(&doc, i)
                    .into_iter()
                    .map(|e| (e.path, e.code.to_string()))
                    .collect();
                whole.sort();
                one.sort();
                if one != whole {
                    return Some(format!(
                        "check_type at {path}: {one:?}, check_document: {whole:?}"
                    ));
                }
            }
            let got = sorted_errors(all.into_iter().map(|e| (e.path, e.code.to_string(), None)));
            let want = expected_errors(expect);
            (got != want).then(|| format!("errors {got:?}, expected {want:?}"))
        }
    }
}

/// Runs one `decode` case against the suite's IR document.
fn run_decode(
    ir: Option<&DocumentSet>,
    ty: &Value,
    input: &Value,
    expect: &Value,
) -> Option<String> {
    let Some(doc) = ir else {
        return Some("a decode case needs the suite's `ir`".to_string());
    };
    let ty = QualifiedName::parse(ty.as_str().unwrap()).unwrap();
    let got = sorted_errors(
        validate(input, &ty, doc)
            .into_iter()
            .map(|e| (e.path, e.code.to_string(), e.rule.map(|r| r.to_string()))),
    );
    let want = if expect.get("valid").is_some() {
        Vec::new()
    } else {
        expected_errors(expect)
    };
    (got != want).then(|| format!("errors {got:?}, expected {want:?}"))
}

/// Runs one `canonical` or `hash` case.
fn run_codec(
    op: &str,
    ir: Option<&DocumentSet>,
    ty: &Value,
    input: &Value,
    expect: &Value,
) -> Option<String> {
    let Some(docs) = ir else {
        return Some(format!("a {op} case needs the suite's `ir`"));
    };
    let ty = QualifiedName::parse(ty.as_str().unwrap()).unwrap();
    let (got, want) = if op == "canonical" {
        (
            canonical(input, &ty, docs),
            expect["json"].as_str().unwrap(),
        )
    } else {
        (
            content_hash(input, &ty, docs),
            expect["hash"].as_str().unwrap(),
        )
    };
    match got {
        Ok(text) if text == want => None,
        Ok(text) => Some(format!("{op} {text}, expected {want}")),
        Err(errors) => Some(format!("the input is invalid: {errors:?}")),
    }
}

/// The canonical form of a suite's `ir`: each document as `to_json()`
/// writes it.
fn canonical_ir(ir: &Value) -> Value {
    let canon = |d: &Value| -> Value {
        serde_json::from_str(&IrDocument::from_json(&d.to_string()).unwrap().to_json()).unwrap()
    };
    match ir {
        Value::Array(a) => Value::Array(a.iter().map(canon).collect()),
        one => canon(one),
    }
}

/// Rewrites the `ir` of every suite in canonical form.
fn rewrite_canonical_ir(dir: &Path) {
    for path in suites(dir) {
        let mut suite: Value =
            serde_json::from_str(&std::fs::read_to_string(&path).unwrap()).unwrap();
        let Some(ir) = suite.get_mut("ir") else {
            continue;
        };
        *ir = canonical_ir(ir);
        std::fs::write(&path, serde_json::to_string_pretty(&suite).unwrap() + "\n").unwrap();
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
    let mut not_canonical = Vec::new();
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

        // The declaration hash reads the IR's canonical form, so the corpus
        // holds IR in that form, compared as RFC 8785 text.
        if let Some(v) = suite.get("ir")
            && canonical_text(&canonical_ir(v)) != canonical_text(v)
        {
            not_canonical.push(name.clone());
        }
        // `ir` is one IR document or an array of them, one per namespace.
        let ir = suite.get("ir").map(|v| {
            let docs: Vec<&Value> = match v {
                Value::Array(a) => a.iter().collect(),
                one => vec![one],
            };
            DocumentSet(
                docs.into_iter()
                    .map(|d| {
                        let doc = IrDocument::from_json(&d.to_string())
                            .unwrap_or_else(|e| panic!("{name}: `ir` does not read: {e}"));
                        let problems = check_document(&doc);
                        assert!(
                            problems.is_empty(),
                            "{name}: `ir` fails the declaration checks: {problems:#?}"
                        );
                        doc
                    })
                    .collect(),
            )
        });
        let mut ids = std::collections::HashSet::new();
        for case in suite["cases"].as_array().unwrap() {
            let id = case["id"].as_str().unwrap();
            assert!(ids.insert(id), "{name}: duplicate case id `{id}`");
            let outcome = match case["op"].as_str().unwrap() {
                "declare" => run_declare(&case["input"], &case["expect"]),
                "decode" => run_decode(ir.as_ref(), &case["type"], &case["input"], &case["expect"]),
                op @ ("canonical" | "hash") => run_codec(
                    op,
                    ir.as_ref(),
                    &case["type"],
                    &case["input"],
                    &case["expect"],
                ),
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
    if std::env::var_os("R8_UPDATE_CORPUS").is_some() {
        rewrite_canonical_ir(&dir);
    } else {
        assert!(
            not_canonical.is_empty(),
            "`ir` is not in canonical form in {not_canonical:?}; R8_UPDATE_CORPUS=1 rewrites it"
        );
    }
    println!("conformance: {passed} passed, {skipped} skipped");
    assert!(failures.is_empty(), "{failures:#?}");
    assert!(passed > 0, "no conformance case ran");
}
