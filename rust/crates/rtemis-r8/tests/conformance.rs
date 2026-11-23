//! Runs the conformance corpus in `conformance/` at the repository root.
//!
//! Each suite is validated against `conformance/corpus.schema.json` before its
//! cases run. Operations `rtemis-r8` does not implement yet are skipped and
//! counted.

use std::path::{Path, PathBuf};

use rtemis_r8::codec::{canonical, canonical_text, content_hash};
use rtemis_r8::ir::{IrDocument, check_document, check_trait, check_type};
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

/// `(path, code, rule)` triples, sorted, for comparison as multisets; the
/// rule is its ID, or `#<index>` for a rule without one.
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
            e.get("rule")
                .and_then(Value::as_str)
                .map(str::to_string)
                .or_else(|| {
                    e.get("rule_index")
                        .and_then(Value::as_u64)
                        .map(|i| format!("#{i}"))
                }),
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
            // The check of one type or trait, with `check_defaults()`, which
            // a registry runs after `check_type()`, reports what the check of
            // the document reports at its path.
            let declarations = (0..doc.types.len())
                .map(|i| {
                    let mut own = check_type(&doc, i);
                    if own.is_empty()
                        && let rtemis_r8::ir::TypeDef::Struct(s) = &doc.types[i]
                    {
                        let q = rtemis_r8::name::QualifiedName {
                            namespace: doc.namespace.clone(),
                            name: s.name.clone(),
                        };
                        own = rtemis_r8::ir::check_defaults(s, &q, &format!("/types/{i}"), &doc);
                    }
                    (format!("/types/{i}"), own)
                })
                .chain(
                    (0..doc.traits.len()).map(|i| (format!("/traits/{i}"), check_trait(&doc, i))),
                );
            for (path, own) in declarations {
                let prefix = format!("{path}/");
                let mut whole: Vec<(String, String)> = all
                    .iter()
                    .filter(|e| e.path == path || e.path.starts_with(&prefix))
                    .map(|e| (e.path.clone(), e.code.to_string()))
                    .collect();
                let mut one: Vec<(String, String)> = own
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

/// Runs one `dispatch` case: resolves a call of the trait `type`, whose
/// arguments' types are `input`, among the impls of the suite's IR.
fn run_dispatch(
    ir: Option<&DocumentSet>,
    trait_name: &Value,
    input: &Value,
    expect: &Value,
) -> Option<String> {
    use rtemis_r8::ir::dispatch::{Arg, Resolution, impls_by_namespace, positions, resolve};
    use rtemis_r8::rules::TypeLookup;
    let Some(docs) = ir else {
        return Some("a `dispatch` case needs the suite's `ir`".into());
    };
    let tr = QualifiedName::parse(trait_name.as_str()?).ok()?;
    let Some(t) = docs.trait_of(&tr) else {
        return Some(format!("`{tr}` is not declared in the suite's `ir`"));
    };
    let arity = t.dispatch_arity();
    let args: Vec<Arg> = input
        .as_array()?
        .iter()
        .map(|a| match a {
            Value::String(s) => Arg::Type(QualifiedName::parse(s).unwrap()),
            Value::Object(o) => Arg::Classes(
                o["classes"]
                    .as_array()
                    .unwrap()
                    .iter()
                    .map(|c| c.as_str().unwrap().to_string())
                    .collect(),
            ),
            _ => Arg::Other,
        })
        .collect();
    let found = impls_by_namespace(docs, &tr);
    let impls: Vec<&rtemis_r8::ir::ImplDef> = found.iter().map(|(_, i)| *i).collect();
    let tuple = |k: usize| -> Value {
        positions(impls[k], arity)
            .iter()
            .map(|p| Value::String(p.to_string()))
            .collect()
    };
    let got = match resolve(docs, &impls, arity, &args, None) {
        Resolution::Impl(k) => serde_json::json!({ "impl": tuple(k) }),
        Resolution::Default => serde_json::json!({ "default": true }),
        Resolution::Ambiguous(a, b) => {
            let mut pair = vec![tuple(a), tuple(b)];
            pair.sort_by_key(ToString::to_string);
            serde_json::json!({ "ambiguous": pair })
        }
        Resolution::Stale(q) => serde_json::json!({ "stale": q.to_string() }),
    };
    let mut want = expect.clone();
    if let Some(Value::Array(pair)) = want.get_mut("ambiguous") {
        pair.sort_by_key(ToString::to_string);
    }
    (got != want).then(|| format!("resolved {got}, expected {want}"))
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
    let got = sorted_errors(validate(input, &ty, doc).into_iter().map(|e| {
        let rule = match (&e.rule, e.rule_index) {
            (Some(r), _) => Some(r.to_string()),
            (None, Some(i)) => Some(format!("#{i}")),
            (None, None) => None,
        };
        (e.path, e.code.to_string(), rule)
    }));
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
            DocumentSet::from(
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
                    .collect::<Vec<_>>(),
            )
        });
        let mut ids = std::collections::HashSet::new();
        for case in suite["cases"].as_array().unwrap() {
            let id = case["id"].as_str().unwrap();
            assert!(ids.insert(id), "{name}: duplicate case id `{id}`");
            let outcome = match case["op"].as_str().unwrap() {
                "declare" => run_declare(&case["input"], &case["expect"]),
                "decode" => run_decode(ir.as_ref(), &case["type"], &case["input"], &case["expect"]),
                "dispatch" => {
                    run_dispatch(ir.as_ref(), &case["type"], &case["input"], &case["expect"])
                }
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

/// The suite's IR as a lookup.
fn suite_ir(suite: &Value) -> Option<DocumentSet> {
    let v = suite.get("ir")?;
    let docs: Vec<&Value> = match v {
        Value::Array(a) => a.iter().collect(),
        one => vec![one],
    };
    Some(DocumentSet::from(
        docs.into_iter()
            .map(|d| IrDocument::from_json(&d.to_string()).unwrap())
            .collect::<Vec<_>>(),
    ))
}

/// The value of a parsed document: `$value`, or the document without its
/// envelope.
fn document_value(mut doc: Value) -> Value {
    let m = doc.as_object_mut().unwrap();
    if let Some(v) = m.remove("$value") {
        return v;
    }
    m.shift_remove("$type");
    m.shift_remove("$types");
    doc
}

/// A document read in normalized form, as R8 writes it, is used as read; a
/// document in any other accepted form is normalized through the emitter's
/// text. Both give the normalized form R8 writes, for every valid decode
/// case of the corpus.
#[test]
fn documents_read_to_the_normalized_form() {
    use rtemis_r8::access::Json;
    use rtemis_r8::codec::{document_access, read_document};
    let mut checked = 0;
    for path in suites(&corpus_dir()) {
        let suite: Value = serde_json::from_str(&std::fs::read_to_string(&path).unwrap()).unwrap();
        let Some(set) = suite_ir(&suite) else {
            continue;
        };
        for case in suite["cases"].as_array().unwrap() {
            if case["op"] != "decode" || case["expect"].get("valid").is_none() {
                continue;
            }
            let id = case["id"].as_str().unwrap();
            let ty = QualifiedName::parse(case["type"].as_str().unwrap()).unwrap();
            let input = &case["input"];
            let written = document_access(Json::new(input), &ty, &set);
            let parsed: Value = serde_json::from_str(&written).unwrap();
            let types = parsed["$types"].clone();
            let want = document_value(parsed);
            let as_written = read_document(&written, &ty, &set)
                .unwrap_or_else(|e| panic!("{id}: the written document does not read: {e:?}"));
            assert_eq!(as_written, want, "{id}: the document as R8 writes it");
            let raw = match input {
                Value::Object(m) => {
                    let mut m = m.clone();
                    m.insert("$type".into(), Value::from(ty.to_string()));
                    m.insert("$types".into(), types);
                    Value::Object(m)
                }
                other => {
                    serde_json::json!({"$type": ty.to_string(), "$types": types, "$value": other})
                }
            };
            let as_input = read_document(&raw.to_string(), &ty, &set)
                .unwrap_or_else(|e| panic!("{id}: the input as a document does not read: {e:?}"));
            assert_eq!(as_input, want, "{id}: the input as a document");
            checked += 1;
        }
    }
    assert!(checked >= 31, "{checked} cases");
}

/// A document read into a compact tree (`read_tree_cached`) holds the value
/// the reference read (`read_document`) gives, by R8's equality of
/// normalized forms, for every decode case of the corpus: the same value for
/// a valid case, and the same error codes for an invalid one.
#[test]
fn tree_reads_match_the_reference_read() {
    use rtemis_r8::access::Json;
    use rtemis_r8::codec::{document_access, read_document, read_tree_cached};
    use rtemis_r8::equal::equal_access;
    use rtemis_r8::validate::RuleCache;
    let mut checked = 0;
    for path in suites(&corpus_dir()) {
        let suite: Value = serde_json::from_str(&std::fs::read_to_string(&path).unwrap()).unwrap();
        let Some(set) = suite_ir(&suite) else {
            continue;
        };
        for case in suite["cases"].as_array().unwrap() {
            if case["op"] != "decode" {
                continue;
            }
            let id = case["id"].as_str().unwrap();
            let ty = QualifiedName::parse(case["type"].as_str().unwrap()).unwrap();
            let input = &case["input"];
            let types =
                serde_json::from_str::<Value>(&document_access(Json::new(input), &ty, &set))
                    .ok()
                    .and_then(|d| d.get("$types").cloned())
                    .unwrap_or_else(|| serde_json::json!({}));
            let raw = match input {
                Value::Object(m) => {
                    let mut m = m.clone();
                    m.insert("$type".into(), Value::from(ty.to_string()));
                    m.insert("$types".into(), types);
                    Value::Object(m)
                }
                other => {
                    serde_json::json!({"$type": ty.to_string(), "$types": types, "$value": other})
                }
            };
            let text = raw.to_string();
            let reference = read_document(&text, &ty, &set);
            let tree = read_tree_cached(&text, &ty, &set, &mut RuleCache::new());
            match (reference, tree) {
                (Ok(v), Ok(t)) => {
                    let tv = t.to_value();
                    assert!(
                        equal_access(&ty, Json::new(&v), Json::new(&tv), &set),
                        "{id}: the tree read differs from the reference read\n{tv}\n{v}"
                    );
                }
                (Err(a), Err(b)) => {
                    let codes = |e: &[rtemis_r8::validate::ValueError]| {
                        let mut c: Vec<&str> = e.iter().map(|x| x.code).collect();
                        c.sort();
                        c
                    };
                    assert_eq!(codes(&a), codes(&b), "{id}: the error codes differ");
                }
                (a, b) => panic!("{id}: the reference read gives {a:?}, the tree read {b:?}"),
            }
            checked += 1;
        }
    }
    assert!(checked >= 31, "{checked} cases");
}
