//! Calls of typed functions in the expressions of table functions: the
//! arguments that the host receives for each group, and the values it gives.

use std::cell::RefCell;

use rtemis_r8::access::Json;
use rtemis_r8::ir::{IrDocument, Logic, TypeDef};
use rtemis_r8::name::QualifiedName;
use rtemis_r8::rules::calc::{
    Argument, Call, CallFailed, CallValue, Data, Functions, NotComputed, computed,
};
use rtemis_r8::rules::{DocumentSet, TypeLookup};
use serde_json::{Value, json};

fn docs() -> DocumentSet {
    let ir = json!({
        "$ir": 1,
        "namespace": "ns",
        "types": [{
            "kind": "table",
            "name": "T",
            "description": "",
            "version": 1,
            "columns": [
                {"name": "g", "type": {"kind": "character"}, "description": ""},
                {"name": "x", "type": {"kind": "option", "inner": {"kind": "double"}}, "description": "", "unit": "kg"},
                {"name": "k", "type": {"kind": "integer"}, "description": ""}
            ]
        }],
        "functions": [
            {
                "name": "wmean",
                "description": "",
                "params": [
                    {"name": "x", "type": {"kind": "vec", "element": {"kind": "option", "inner": {"kind": "double"}}}},
                    {"name": "w", "type": {"kind": "vec", "element": {"kind": "double"}}},
                    {"name": "shift", "type": {"kind": "double"}, "default": 0}
                ],
                "return_type": {"kind": "option", "inner": {"kind": "double"}}
            },
            {
                "name": "label",
                "description": "",
                "params": [
                    {"name": "x", "type": {"kind": "vec", "element": {"kind": "character"}}}
                ],
                "return_type": {"kind": "character"}
            }
        ]
    });
    DocumentSet::from(vec![IrDocument::from_json(&ir.to_string()).unwrap()])
}

fn input() -> Value {
    json!({
        "$length": 5,
        "g": ["b", "a", "b", "a", "b"],
        "x": [1.0, 2.0, null, 4.0, 5.0],
        "k": [1, 2, 3, 4, 5]
    })
}

/// A host whose functions read the input's columns, and that records each
/// call's arguments.
struct Host {
    input: Value,
    seen: RefCell<Vec<String>>,
}

impl Host {
    fn numbers(&self, a: &Argument<'_>) -> Vec<Option<f64>> {
        match a {
            Argument::Column { name, rows } => rows
                .iter()
                .map(|&r| self.input[*name][r as usize].as_f64())
                .collect(),
            Argument::Cells {
                data: Data::Double(v),
                rows,
                ..
            } => rows
                .iter()
                .map(|&r| Some(v[r as usize]).filter(|x| !x.is_nan()))
                .collect(),
            Argument::Cells {
                data: Data::Integer(v),
                rows,
                ..
            } => rows.iter().map(|&r| v[r as usize].map(f64::from)).collect(),
            other => panic!("not cells: {other:?}"),
        }
    }
}

impl Functions for Host {
    fn call(
        &self,
        function: &QualifiedName,
        calls: &[Call<'_>],
    ) -> Result<Vec<CallValue>, CallFailed> {
        calls
            .iter()
            .enumerate()
            .map(|(at, args)| {
                self.one(function, args)
                    .map_err(|message| CallFailed { at, message })
            })
            .collect()
    }
}

impl Host {
    fn one(
        &self,
        function: &QualifiedName,
        args: &[(&str, Argument<'_>)],
    ) -> Result<CallValue, String> {
        let names: Vec<&str> = args.iter().map(|(n, _)| *n).collect();
        self.seen
            .borrow_mut()
            .push(format!("{function}({})", names.join(", ")));
        match function.to_string().as_str() {
            "ns::wmean" => {
                let x = self.numbers(&args[0].1);
                let w = self.numbers(&args[1].1);
                let shift = match args[2].1 {
                    Argument::Constant(Logic::Number(n)) => n.as_f64(),
                    Argument::Default(d) => d.to_value().as_f64().unwrap(),
                    ref other => panic!("not a constant: {other:?}"),
                };
                if x.iter().any(Option::is_none) {
                    return Ok(CallValue::Missing);
                }
                let total: f64 = w.iter().map(|w| w.unwrap()).sum();
                if total == 0.0 {
                    return Err("the weights sum to 0".to_string());
                }
                let s: f64 = x.iter().zip(&w).map(|(x, w)| x.unwrap() * w.unwrap()).sum();
                Ok(CallValue::Number(s / total + shift))
            }
            "ns::label" => match args[0].1 {
                Argument::Column { name, rows } => Ok(CallValue::Text(
                    rows.iter()
                        .map(|&r| self.input[name][r as usize].as_str().unwrap())
                        .collect::<Vec<_>>()
                        .join(""),
                )),
                ref other => panic!("not a column: {other:?}"),
            },
            other => Err(format!("no function {other}")),
        }
    }
}

fn run(
    exprs: &[Value],
    by: &[&str],
    summary: bool,
) -> (
    Result<rtemis_r8::rules::calc::Outcome, NotComputed>,
    Vec<String>,
) {
    let docs = docs();
    let q = QualifiedName::parse("ns::T").unwrap();
    let Some(TypeDef::Table(def)) = docs.find(&q).cloned() else {
        panic!("a table")
    };
    let input = input();
    let host = Host {
        input: input.clone(),
        seen: RefCell::new(Vec::new()),
    };
    let logic: Vec<Logic> = exprs.iter().map(Logic::from_value).collect();
    let refs: Vec<&Logic> = logic.iter().collect();
    let out = computed(Json::new(&input), &def, by, &refs, &docs, &host, summary, 1);
    (out, host.seen.into_inner())
}

fn doubles(out: &rtemis_r8::rules::calc::Outcome, i: usize) -> Vec<Option<f64>> {
    match &out.columns[i].data {
        Some(Data::Double(v)) => v.iter().map(|&x| Some(x).filter(|x| !x.is_nan())).collect(),
        other => panic!("not doubles: {other:?}"),
    }
}

#[test]
fn a_summary_calls_the_function_once_for_each_group() {
    let (out, seen) = run(
        &[
            json!({"call": ["ns::wmean", "x", {"var": "x"}, "w", {"var": "k"}]}),
            json!({"call": ["ns::label", "x", {"var": "g"}]}),
        ],
        &["g"],
        true,
    );
    let out = out.unwrap();
    // Group "a" is rows 1 and 3, and "b" rows 0, 2, and 4, of which x is
    // missing in row 2.
    assert_eq!(
        doubles(&out, 0),
        vec![Some((2.0 * 2.0 + 4.0 * 4.0) / 6.0), None]
    );
    assert_eq!(
        out.columns[1].data,
        Some(Data::Character(vec![
            Some("aa".to_string()),
            Some("bbb".to_string())
        ]))
    );
    assert_eq!(out.columns[0].unit, None);
    assert_eq!(
        seen,
        vec![
            "ns::wmean(x, w, shift)",
            "ns::wmean(x, w, shift)",
            "ns::label(x)",
            "ns::label(x)"
        ]
    );
}

#[test]
fn a_call_is_a_term_of_an_expression() {
    // Over all rows, each row's cell less the weighted mean, which an
    // expression argument and a constant give.
    let (out, _) = run(
        &[json!({"-": [
            {"var": "k"},
            {"call": ["ns::wmean", "x", {"var": "k"}, "w", {"*": [{"var": "k"}, 2]}, "shift", 1]}
        ]})],
        &[],
        false,
    );
    let out = out.unwrap();
    let m = (1.0 + 4.0 + 9.0 + 16.0 + 25.0) / 15.0 + 1.0;
    assert_eq!(
        doubles(&out, 0),
        (1..=5).map(|k| Some(f64::from(k) - m)).collect::<Vec<_>>()
    );
}

#[test]
fn a_column_of_every_row_takes_the_value_of_its_group() {
    let (out, _) = run(
        &[json!({"call": ["ns::wmean", "x", {"var": "k"}, "w", {"var": "k"}]})],
        &["g"],
        false,
    );
    let out = out.unwrap();
    let a = (4.0 + 16.0) / 6.0;
    let b = (1.0 + 9.0 + 25.0) / 9.0;
    assert_eq!(
        doubles(&out, 0),
        vec![Some(b), Some(a), Some(b), Some(a), Some(b)]
    );
}

#[test]
fn an_error_of_a_call_names_the_function_and_the_group() {
    let (out, seen) = run(
        &[
            json!({"call": ["ns::wmean", "x", {"var": "x"}, "w", {"-": [{"var": "k"}, {"var": "k"}]}]}),
        ],
        &["g"],
        true,
    );
    match out {
        Err(NotComputed::Call {
            at,
            function,
            group,
            first_row,
            message,
        }) => {
            assert_eq!((at, function.as_str(), group), (0, "ns::wmean", 0));
            assert_eq!(first_row, Some(1));
            assert_eq!(message, "the weights sum to 0");
        }
        other => panic!("not a call error: {other:?}"),
    }
    // The call stops at the first group.
    assert_eq!(seen.len(), 1);
}

#[test]
fn rules_call_no_function() {
    let docs = docs();
    let q = QualifiedName::parse("ns::T").unwrap();
    let Some(TypeDef::Table(def)) = docs.find(&q).cloned() else {
        panic!("a table")
    };
    let input = input();
    let condition = Logic::from_value(
        &json!({">": [{"call": ["ns::wmean", "x", {"var": "x"}, "w", {"var": "k"}]}, 0]}),
    );
    let err = rtemis_r8::rules::choose_rows(Json::new(&input), &def, &condition, &docs)
        .expect_err("a rule refuses a call");
    assert!(format!("{err:?}").contains("rule_call"), "{err:?}");
}
