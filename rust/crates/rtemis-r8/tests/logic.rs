//! Rule expressions as a typed syntax tree: every expression of the
//! conformance suites and the fixtures writes back as it was read, operations
//! read as calls of the dialect's operators, and input that is not an
//! operation is kept for the declaration check.

use std::path::{Path, PathBuf};

use rtemis_r8::ir::{IrDocument, Logic, Operator};
use serde_json::{Value, json};

fn json_files(dir: &Path) -> Vec<PathBuf> {
    let mut out = Vec::new();
    for entry in std::fs::read_dir(dir).unwrap() {
        let path = entry.unwrap().path();
        if path.is_dir() {
            out.extend(json_files(&path));
        } else if path.extension().is_some_and(|e| e == "json") {
            out.push(path);
        }
    }
    out
}

/// Every value under a member named `invariant`, `when`, or `expr`.
fn expressions(v: &Value, out: &mut Vec<Value>) {
    match v {
        Value::Object(m) => {
            for (k, x) in m {
                if matches!(k.as_str(), "invariant" | "when" | "expr") {
                    out.push(x.clone());
                }
                expressions(x, out);
            }
        }
        Value::Array(a) => a.iter().for_each(|x| expressions(x, out)),
        _ => {}
    }
}

#[test]
fn every_expression_of_the_suites_writes_back_as_read() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"));
    let mut files = json_files(&root.join("../../../conformance"));
    files.extend(json_files(&root.join("tests/fixtures")));
    let mut n = 0;
    for path in files {
        let v: Value = serde_json::from_str(&std::fs::read_to_string(&path).unwrap()).unwrap();
        let mut found = Vec::new();
        expressions(&v, &mut found);
        for e in found {
            let text = serde_json::to_string(&e).unwrap();
            let logic: Logic = serde_json::from_str(&text).unwrap();
            assert_eq!(
                serde_json::to_string(&logic).unwrap(),
                text,
                "{}",
                path.display()
            );
            assert_eq!(Logic::from_value(&e).to_value(), e);
            n += 1;
        }
    }
    assert!(n >= 60, "found {n} expressions");
}

#[test]
fn a_document_writes_back_as_read() {
    let path = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/svm.ir.json");
    let v: Value = serde_json::from_str(&std::fs::read_to_string(path).unwrap()).unwrap();
    let text = serde_json::to_string(&v).unwrap();
    let doc = IrDocument::from_json(&text).unwrap();
    assert_eq!(doc.to_json(), text);
}

#[test]
fn operations_read_as_calls_of_the_dialect() {
    let read = |v: Value| -> Logic { serde_json::from_value(v).unwrap() };
    assert_eq!(read(json!({"var": "a.b"})), Logic::Var("a.b".into()));
    let Logic::Call { op, single, args } = read(json!({"<": [{"var": "a"}, 1]})) else {
        panic!("an operation")
    };
    assert_eq!((op, single, args.len()), (Operator::Lt, false, 2));
    let Logic::Call { op, single, .. } = read(json!({"!": {"var": "a"}})) else {
        panic!("an operation")
    };
    assert_eq!((op, single), (Operator::Not, true));
    // `var` with an array operand is a call, written back as declared.
    assert!(matches!(
        read(json!({"var": ["a"]})),
        Logic::Call {
            op: Operator::Var,
            single: false,
            ..
        }
    ));
    for text in [
        r#"{"!":{"var":"a"}}"#,
        r#"{"var":["a"]}"#,
        r#"{"+":[1,1.0,-2,0.5]}"#,
        r#"{"in":["a",["a","b"]]}"#,
        r#"[1,"x",null,true]"#,
    ] {
        let l: Logic = serde_json::from_str(text).unwrap();
        assert_eq!(serde_json::to_string(&l).unwrap(), text);
    }
    // Every operator reads back to its name.
    for name in [
        "===",
        "!==",
        "==",
        "non_decreasing",
        "is_variant",
        "levels",
        "dim",
    ] {
        assert_eq!(Operator::parse(name).map(Operator::as_str), Some(name));
    }
    assert_eq!(Operator::parse("nope"), None);
}

#[test]
fn input_that_is_not_an_operation_is_kept() {
    for text in [r#"{"nope":[1]}"#, r#"{"a":1,"b":{"var":"x"}}"#, r#"{}"#] {
        let l: Logic = serde_json::from_str(text).unwrap();
        assert!(matches!(l, Logic::Other(_)), "{text}");
        assert_eq!(serde_json::to_string(&l).unwrap(), text);
    }
}

#[test]
fn an_expression_is_three_words() {
    assert_eq!(size_of::<Logic>(), 24);
    assert_eq!(size_of::<Option<Logic>>(), 24);
}
