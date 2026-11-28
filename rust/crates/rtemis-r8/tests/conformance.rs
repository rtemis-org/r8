//! Runs the conformance corpus in `conformance/` at the repository root.
//!
//! Each suite is validated against `conformance/corpus.schema.json` before its
//! cases run. Operations `rtemis-r8` does not implement yet are skipped and
//! counted.

use std::path::{Path, PathBuf};

use rtemis_r8::canonical::{canonical, canonical_text, content_hash};
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

/// Runs one `dispatch` case: selects the impl for a call of the trait `type`, whose
/// arguments' types are `input`, among the impls of the suite's IR.
fn run_dispatch(
    ir: Option<&DocumentSet>,
    trait_name: &Value,
    input: &Value,
    expect: &Value,
) -> Option<String> {
    use rtemis_r8::ir::dispatch::{Arg, Selection, impls_by_namespace, positions, select};
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
    let got = match select(docs, &impls, arity, &args, None) {
        Selection::Impl(k) => serde_json::json!({ "impl": tuple(k) }),
        Selection::Default => serde_json::json!({ "default": true }),
        Selection::Ambiguous(a, b) => {
            let mut pair = vec![tuple(a), tuple(b)];
            pair.sort_by_key(ToString::to_string);
            serde_json::json!({ "ambiguous": pair })
        }
        Selection::Stale(q) => serde_json::json!({ "stale": q.to_string() }),
    };
    let mut want = expect.clone();
    if let Some(Value::Array(pair)) = want.get_mut("ambiguous") {
        pair.sort_by_key(ToString::to_string);
    }
    (got != want).then(|| format!("selected {got}, expected {want}"))
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
fn run_canonical(
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
                "update" => run_update(ir.as_ref(), &case["type"], &case["input"], &case["expect"]),
                "filter" => run_filter(ir.as_ref(), case),
                op @ ("calc" | "summarize" | "map") => run_calc(op, ir.as_ref(), case),
                "check" => run_check(ir.as_ref(), case),
                "call" => run_call(ir.as_ref(), case),
                "message" => run_message(case),
                "plan" => run_plan(ir.as_ref(), case),
                "refine" => run_refine(ir.as_ref(), &case["type"], &case["input"], &case["expect"]),
                op @ ("canonical" | "hash") => run_canonical(
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

/// Runs one `call` case: the errors of the document of a call of the
/// function `type`, and the rules left unchecked. The arguments of a valid
/// call satisfy the function's JSON Schema of arguments, and those of a call
/// whose problems are all structural do not.
fn run_message(case: &Value) -> Option<String> {
    use rtemis_r8::message::{Body, Path, Place, Size, line, map_ended, map_started};
    let input = &case["input"];
    let n = |v: &Value| v.as_u64();
    let place = Place {
        path: match input["place"]["path"].as_str()? {
            "rust" => Path::Rust,
            _ => Path::Host,
        },
        bodies: input["place"]["bodies"]
            .as_array()?
            .iter()
            .map(|b| Body {
                function: b["function"].as_str().unwrap_or_default().to_string(),
                in_rust: b["in_rust"].as_bool().unwrap_or_default(),
            })
            .collect(),
    };
    let host = input["host"].as_str()?;
    let text = match input["phase"].as_str()? {
        "start" => {
            let size = Size {
                columns: n(&input["size"]["columns"]),
                groups: n(&input["size"]["groups"]),
                items: n(&input["size"]["items"]),
                calls: n(&input["size"]["calls"]),
            };
            map_started(input["label"].as_str()?, &size, &place, host)
        }
        _ => map_ended(
            input["wall"].as_f64()?,
            input["cpu"].as_f64()?,
            n(&input["threads"])?,
            &place,
            host,
        ),
    };
    let got = serde_json::json!({
        "text": text,
        "line": line(input["timestamp"].as_str()?, &text, input["caller"].as_str()),
        "engine": place.engine().name(),
    });
    (got != case["expect"]).then(|| format!("expected {}, got {got}", case["expect"]))
}

fn run_plan(ir: Option<&DocumentSet>, case: &Value) -> Option<String> {
    use rtemis_r8::ir::{Logic, TypeDef};
    use rtemis_r8::plan::{ListCall, TableCall, headline, list_plan, mermaid, table_plan};
    use rtemis_r8::rules::TypeLookup;
    use rtemis_r8::rules::calc::{NotComputed, mapped, planned};
    let Some(docs) = ir else {
        return Some("a `plan` case needs the suite's `ir`".to_string());
    };
    let options = &case["options"];
    let in_rust: Vec<QualifiedName> = options["in_rust"]
        .as_array()
        .map(|a| {
            a.iter()
                .filter_map(|q| QualifiedName::parse(q.as_str()?).ok())
                .collect()
        })
        .unwrap_or_default();
    let function = options["function"].as_str().unwrap_or("r8map");
    let plan = if let Some(typed) = options["typed"].as_str() {
        let typed = QualifiedName::parse(typed).ok()?;
        let items = case["input"].as_array()?.len() as u64;
        list_plan(
            &ListCall {
                function,
                typed: &typed,
                items,
                in_rust: in_rust.contains(&typed),
                host: "r",
            },
            docs,
        )
    } else {
        let ty = QualifiedName::parse(case["type"].as_str()?).ok()?;
        let Some(TypeDef::Table(def)) = docs.find(&ty) else {
            return Some(format!("`{ty}` is not a table of the suite's `ir`"));
        };
        let by: Vec<&str> = options["by"]
            .as_array()
            .map(|a| a.iter().filter_map(Value::as_str).collect())
            .unwrap_or_default();
        let (names, exprs): (Vec<String>, Vec<Logic>) = if options.get("f").is_some() {
            let columns: Vec<&str> = options["columns"]
                .as_array()?
                .iter()
                .filter_map(Value::as_str)
                .collect();
            let e = mapped(&Logic::from_value(&options["f"]), &columns).ok()?;
            (columns.iter().map(|c| c.to_string()).collect(), e)
        } else {
            options["columns"]
                .as_object()?
                .iter()
                .map(|(k, v)| (k.clone(), Logic::from_value(v)))
                .unzip()
        };
        let refs: Vec<&Logic> = exprs.iter().collect();
        let columns: Vec<&str> = names.iter().map(String::as_str).collect();
        let summary = options["summary"].as_bool().unwrap_or(true);
        let threads = options["threads"].as_u64().unwrap_or(1) as usize;
        let table = rtemis_r8::access::Json::new(&case["input"]);
        let planned = match planned(table, def, &by, &refs, docs, summary, threads) {
            Ok(p) => p,
            Err(NotComputed::Key(m)) => {
                let got = serde_json::json!({"key_error": m});
                return (got != case["expect"])
                    .then(|| format!("{got}, expected {}", case["expect"]));
            }
            Err(NotComputed::Rule { errors, .. }) => {
                let got = sorted_errors(errors.into_iter().map(|e| (e.path, e.code.into(), None)));
                let want = expected_errors(&case["expect"]);
                return (got != want).then(|| format!("errors {got:?}, expected {want:?}"));
            }
            Err(other) => return Some(format!("{other:?}")),
        };
        let call = TableCall {
            function,
            label: options["label"].as_str().unwrap_or(""),
            table: &ty,
            by: &by,
            columns: &columns,
            exprs: &refs,
            summary,
            threads,
            host: "r",
        };
        let plan = table_plan(&call, &planned, docs, &|f| in_rust.contains(f));
        // A run with a trace builds the plan that the dry run gives, by the same
        // compiled step; its statuses and times aside, the two are equal.
        let is_rust = |f: &QualifiedName| in_rust.contains(f);
        let mut trace = rtemis_r8::rules::calc::Trace {
            call,
            in_rust: &is_rust,
            before: 0.0,
            plan: None,
        };
        let _ = rtemis_r8::rules::calc::computed_into(
            rtemis_r8::access::Json::new(&case["input"]),
            def,
            &by,
            &refs,
            docs,
            &rtemis_r8::rules::calc::NoFunctions,
            summary,
            threads,
            &mut rtemis_r8::rules::calc::OwnColumns,
            Some(&mut trace),
        );
        let mut run = trace.plan?;
        for n in &mut run.nodes {
            n.status = rtemis_r8::plan::Status::Pending;
            n.t_start = None;
            n.t_end = None;
            n.meta.busy = None;
            n.meta.group = None;
            n.meta.row = None;
            n.meta.message = None;
        }
        if run != plan {
            return Some(format!(
                "the run's plan differs from the dry run's: {}",
                serde_json::to_string(&run).unwrap_or_default()
            ));
        }
        plan
    };
    let got = serde_json::json!({
        "plan": serde_json::to_value(&plan).ok()?,
        "headline": headline(&plan, "R"),
        "mermaid": mermaid(&plan),
    });
    (got != case["expect"]).then(|| format!("expected {}, got {got}", case["expect"]))
}

fn run_call(ir: Option<&DocumentSet>, case: &Value) -> Option<String> {
    use rtemis_r8::call::{arguments_schema, read_call};
    let Some(docs) = ir else {
        return Some("a `call` case needs the suite's `ir`".into());
    };
    let f = QualifiedName::parse(case["type"].as_str()?).ok()?;
    let text = case["input"].to_string();
    let mut cache = rtemis_r8::validate::RuleCache::new();
    let rule_of = |e: &rtemis_r8::validate::ValueError| match (&e.rule, e.rule_index) {
        (Some(r), _) => Some(r.to_string()),
        (None, Some(i)) => Some(format!("#{i}")),
        (None, None) => None,
    };
    let (got, unchecked) = match read_call(&text, std::slice::from_ref(&f), docs, &mut cache) {
        Ok(call) => (
            Vec::new(),
            sorted_errors(
                call.unchecked
                    .iter()
                    .map(|e| (e.path.clone(), e.code.to_string(), rule_of(e))),
            ),
        ),
        Err(errors) => (
            sorted_errors(
                errors
                    .iter()
                    .map(|e| (e.path.clone(), e.code.to_string(), rule_of(e))),
            ),
            Vec::new(),
        ),
    };
    let want = expected_errors(&case["expect"]);
    let want_unchecked = match case["expect"].get("unchecked") {
        Some(u) => expected_errors(&serde_json::json!({ "errors": u })),
        None => Vec::new(),
    };
    if got != want {
        return Some(format!("errors {got:?}, expected {want:?}"));
    }
    if unchecked != want_unchecked {
        return Some(format!(
            "unchecked {unchecked:?}, expected {want_unchecked:?}"
        ));
    }
    if let (Ok(schema), Some(args)) = (
        arguments_schema(&f, docs, false),
        case["input"].get("arguments"),
    ) {
        let structural = |code: &str| {
            !matches!(
                code,
                "rule_violation"
                    | "rule_error"
                    | "version_missing"
                    | "version_older"
                    | "version_newer"
                    | "unknown_function"
                    | "call_form"
            )
        };
        let accepted = jsonschema::draft202012::is_valid(&schema, args);
        if got.is_empty() && !accepted {
            return Some("the schema of the arguments rejects a valid call".into());
        }
        if !got.is_empty() && got.iter().all(|(_, c, _)| structural(c)) && accepted {
            return Some("the schema of the arguments accepts an invalid call".into());
        }
    }
    None
}

/// Runs one `check` case.
fn run_check(ir: Option<&DocumentSet>, case: &Value) -> Option<String> {
    use rtemis_r8::ir::{RuleDef, TypeDef};
    use rtemis_r8::report::{Kind, Of, check_rules, check_table};
    use rtemis_r8::rules::TypeLookup;
    let Some(docs) = ir else {
        return Some("a `check` case needs the suite's `ir`".into());
    };
    let ty = QualifiedName::parse(case["type"].as_str()?).ok()?;
    let Some(TypeDef::Table(def)) = docs.find(&ty) else {
        return Some(format!("`{ty}` is not a table of the suite's `ir`"));
    };
    let table = rtemis_r8::access::Json::new(&case["input"]);
    let done = match case.get("options").and_then(|o| o.get("rules")) {
        Some(rules) => {
            let rules: Vec<RuleDef> = serde_json::from_value(rules.clone()).unwrap();
            check_rules(table, &rules, docs, 10)
        }
        None => check_table(table, &def.columns, def.open, &def.rules, docs, 10),
    };
    let (rows, lines) = match done {
        Ok(found) => found,
        Err(f) => return Some(f.message),
    };
    let lines: Vec<Value> = lines
        .into_iter()
        .map(|l| {
            let mut out = serde_json::json!({
                "checked": l.checked, "failed": l.failed, "missing": l.missing, "errors": l.errors,
            });
            match &l.of {
                Of::Column(c) => out["column"] = c.as_str().into(),
                Of::Rule(i) => out["rule"] = (*i).into(),
            }
            let problems: Vec<Value> = l
                .problems
                .iter()
                .map(|p| {
                    let code = match &p.kind {
                        Kind::Value(e) => e.code,
                        Kind::Violation(_) => "rule_violation",
                        Kind::Error(..) => "rule_error",
                        Kind::Unread(e) => e[0].code,
                    };
                    match p.row {
                        Some(r) => serde_json::json!({ "row": r, "code": code }),
                        None => serde_json::json!({ "code": code }),
                    }
                })
                .collect();
            if !problems.is_empty() {
                out["problems"] = problems.into();
            }
            out
        })
        .collect();
    let got = serde_json::json!({ "rows": rows, "lines": lines });
    (got != case["expect"]).then(|| format!("{got}, expected {}", case["expect"]))
}

/// Runs one `filter` case.
fn run_filter(ir: Option<&DocumentSet>, case: &Value) -> Option<String> {
    use rtemis_r8::ir::{Logic, TypeDef};
    use rtemis_r8::rules::{NotChosen, TypeLookup, choose_rows};
    let Some(docs) = ir else {
        return Some("a `filter` case needs the suite's `ir`".into());
    };
    let ty = QualifiedName::parse(case["type"].as_str()?).ok()?;
    let Some(TypeDef::Table(def)) = docs.find(&ty) else {
        return Some(format!("`{ty}` is not a table of the suite's `ir`"));
    };
    let problems = validate(&case["input"], &ty, docs);
    if !problems.is_empty() {
        return Some(format!("the input is invalid: {problems:?}"));
    }
    let rule = Logic::from_value(&case["options"]["rule"]);
    let table = rtemis_r8::access::Json::new(&case["input"]);
    let got = match choose_rows(table, def, &rule, docs) {
        Ok(rows) => serde_json::json!({ "rows": rows }),
        Err(NotChosen::Row(e)) => {
            serde_json::json!({ "row_error": { "row": e.row, "count": e.count } })
        }
        Err(NotChosen::Rule(errors)) => {
            let got = sorted_errors(
                errors
                    .into_iter()
                    .map(|e| (e.path.replacen("/invariant", "", 1), e.code.into(), None)),
            );
            if case["expect"].get("errors").is_none() {
                return Some(format!("errors {got:?}, expected {}", case["expect"]));
            }
            let want = expected_errors(&case["expect"]);
            return (got != want).then(|| format!("errors {got:?}, expected {want:?}"));
        }
        Err(NotChosen::Table(m)) => return Some(m),
    };
    (got != case["expect"]).then(|| format!("{got}, expected {}", case["expect"]))
}

/// Runs one `calc`, `summarize`, or `map` case: the columns that the
/// expressions of `options.columns` compute over the rows, or over the groups
/// by `options.by`; a `map` sets `col()` of `options.f` to each column of
/// `options.columns`.
fn run_calc(op: &str, ir: Option<&DocumentSet>, case: &Value) -> Option<String> {
    use rtemis_r8::canonical::{date_text, datetime_text};
    use rtemis_r8::ir::{Logic, TypeDef};
    use rtemis_r8::rules::TypeLookup;
    use rtemis_r8::rules::calc::{Data, Kind, NotComputed, computed, mapped};
    let Some(docs) = ir else {
        return Some(format!("a `{op}` case needs the suite's `ir`"));
    };
    let ty = QualifiedName::parse(case["type"].as_str()?).ok()?;
    let Some(TypeDef::Table(def)) = docs.find(&ty) else {
        return Some(format!("`{ty}` is not a table of the suite's `ir`"));
    };
    let problems = validate(&case["input"], &ty, docs);
    if !problems.is_empty() {
        return Some(format!("the input is invalid: {problems:?}"));
    }
    let options = &case["options"];
    let by: Vec<&str> = options["by"]
        .as_array()
        .map(|a| a.iter().filter_map(Value::as_str).collect())
        .unwrap_or_default();
    let (names, exprs): (Vec<String>, Vec<Logic>) = if op == "map" {
        let columns: Vec<&str> = options["columns"]
            .as_array()?
            .iter()
            .filter_map(Value::as_str)
            .collect();
        match mapped(&Logic::from_value(&options["f"]), &columns) {
            Ok(e) => (columns.iter().map(|c| c.to_string()).collect(), e),
            Err(e) => {
                let got = serde_json::json!({"errors": [{"code": e.code, "path": e.path}]});
                return (got != case["expect"])
                    .then(|| format!("{got}, expected {}", case["expect"]));
            }
        }
    } else {
        options["columns"]
            .as_object()?
            .iter()
            .map(|(k, v)| (k.clone(), Logic::from_value(v)))
            .unzip()
    };
    let refs: Vec<&Logic> = exprs.iter().collect();
    let table = rtemis_r8::access::Json::new(&case["input"]);
    let got = match computed(
        table,
        def,
        &by,
        &refs,
        docs,
        &rtemis_r8::rules::calc::NoFunctions,
        op != "calc",
        1,
    ) {
        Ok(out) => {
            let mut columns = serde_json::Map::new();
            for (name, c) in names.iter().zip(&out.columns) {
                let kind = match c.kind {
                    Kind::Logical => "logical",
                    Kind::Integer => "integer",
                    Kind::Double => "double",
                    Kind::Character => "character",
                    Kind::Date => "date",
                    Kind::DateTime => "posixct",
                };
                // `computed()` holds every column.
                let cells: Vec<Value> = match c.data.as_ref()? {
                    Data::Logical(v) => v.iter().map(|x| serde_json::json!(x)).collect(),
                    Data::Integer(v) => v.iter().map(|x| serde_json::json!(x)).collect(),
                    Data::Double(v) => v
                        .iter()
                        .map(|&x| {
                            if x.is_nan() {
                                Value::Null
                            } else {
                                serde_json::json!(x)
                            }
                        })
                        .collect(),
                    Data::Character(v) => v.iter().map(|x| serde_json::json!(x)).collect(),
                    Data::Date(v) => v
                        .iter()
                        .map(|x| serde_json::json!(x.map(date_text)))
                        .collect(),
                    Data::DateTime(v) => v
                        .iter()
                        .map(|x| serde_json::json!(x.map(datetime_text)))
                        .collect(),
                };
                let mut entry = serde_json::json!({"kind": kind, "cells": cells});
                if let Some(u) = &c.unit {
                    entry["unit"] = Value::String(u.clone());
                }
                columns.insert(name.clone(), entry);
            }
            let mut got = serde_json::json!({"columns": columns});
            if let Some(g) = &out.groups {
                got["first"] = serde_json::json!(g.first);
            }
            got
        }
        Err(NotComputed::Row { at, error }) => serde_json::json!({
            "row_error": {"column": names[at], "row": error.row, "count": error.count}
        }),
        Err(NotComputed::Rule { at, errors }) => {
            let got = sorted_errors(errors.into_iter().map(|e| (e.path, e.code.into(), None)));
            if case["expect"].get("errors").is_none() {
                return Some(format!(
                    "errors of `{}` {got:?}, expected {}",
                    names[at], case["expect"]
                ));
            }
            let want = expected_errors(&case["expect"]);
            return (got != want).then(|| format!("errors {got:?}, expected {want:?}"));
        }
        Err(NotComputed::Key(m)) => serde_json::json!({"key_error": m}),
        Err(NotComputed::Call { function, .. }) => serde_json::json!({"call_error": function}),
        Err(NotComputed::Table(m)) => return Some(m),
    };
    if let Some(Value::String(_)) = case["expect"].get("key_error") {
        return got
            .get("key_error")
            .is_none()
            .then(|| format!("{got}, expected a key error"));
    }
    (got != case["expect"]).then(|| format!("{got}, expected {}", case["expect"]))
}

fn run_refine(
    ir: Option<&DocumentSet>,
    ty: &Value,
    input: &Value,
    expect: &Value,
) -> Option<String> {
    let Some(compiled) = ir else {
        return Some("a `refine` case needs the suite's `ir`".into());
    };
    let ty = QualifiedName::parse(ty.as_str()?).ok()?;
    let registered = match IrDocument::from_json(&input.to_string()) {
        Ok(d) => DocumentSet::from(vec![d]),
        Err(e) => return Some(format!("the input does not read: {e}")),
    };
    let mut got: Vec<Value> = rtemis_r8::refine::differences(&ty, compiled, &registered)
        .iter()
        .map(|d| serde_json::json!({"code": d.code, "type": d.type_name}))
        .collect();
    let mut want = match expect.get("differences") {
        Some(Value::Array(a)) => a.clone(),
        _ => Vec::new(),
    };
    got.sort_by_key(ToString::to_string);
    want.sort_by_key(ToString::to_string);
    (got != want).then(|| format!("expected {want:?}, got {got:?}"))
}

fn run_update(
    ir: Option<&DocumentSet>,
    ty: &Value,
    input: &Value,
    expect: &Value,
) -> Option<String> {
    use rtemis_r8::update::{Effect, changes, read_declarations};
    let Some(made) = ir else {
        return Some("an `update` case needs the suite's `ir`".into());
    };
    let ty = QualifiedName::parse(ty.as_str()?).ok()?;
    let text = rtemis_r8::canonical::declaration_text(&ty, made);
    let hash = rtemis_r8::canonical::declaration_hash(&ty, made);
    let Some(read) = read_declarations(&text, &hash) else {
        return Some("the hashed text of the declarations does not read".into());
    };
    if read_declarations(&text, "sha256:other").is_some() {
        return Some("a text with another hash reads".into());
    }
    let registered = match IrDocument::from_json(&input.to_string()) {
        Ok(d) => DocumentSet::from(vec![d]),
        Err(e) => return Some(format!("the input does not read: {e}")),
    };
    let mut got: Vec<Value> = changes(&ty, &read, &registered)
        .iter()
        .map(|c| {
            let effect = match c.effect {
                Effect::None => "none",
                Effect::Form => "form",
                Effect::Meaning => "meaning",
            };
            serde_json::json!({"code": c.code, "type": c.type_name, "effect": effect})
        })
        .collect();
    let mut want = match expect.get("changes") {
        Some(Value::Array(a)) => a.clone(),
        _ => Vec::new(),
    };
    got.sort_by_key(ToString::to_string);
    want.sort_by_key(ToString::to_string);
    (got != want).then(|| format!("expected {want:?}, got {got:?}"))
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
    m.shift_remove("$declarations");
    doc
}

/// A document read in normalized form, as R8 writes it, is used as read; a
/// document in any other accepted form is normalized through the emitter's
/// text. Both give the normalized form R8 writes, for every valid decode
/// case of the corpus.
#[test]
fn documents_read_to_the_normalized_form() {
    use rtemis_r8::access::Json;
    use rtemis_r8::canonical::{document_access, read_document};
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
    use rtemis_r8::canonical::{document_access, read_document, read_tree_cached};
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
