//! Columns of doubles written into a host's buffers (`computed_into()`) equal
//! the columns that the core holds (`computed()`), with the same errors, over
//! several blocks of rows.

use rtemis_r8::access::Json;
use rtemis_r8::ir::{IrDocument, Logic, TypeDef};
use rtemis_r8::name::QualifiedName;
use rtemis_r8::plan::{Plan, Status, TableCall};
use rtemis_r8::rules::calc::{
    Data, NoFunctions, NotComputed, Outcome, Output, OwnColumns, Trace, computed, computed_into,
};
use rtemis_r8::rules::{DocumentSet, TypeLookup};
use serde_json::{Value, json};

/// The rows: more than two blocks of 4,096.
const ROWS: usize = 10_000;

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
                {"name": "w", "type": {"kind": "double"}, "description": ""},
                {"name": "h", "type": {"kind": "option", "inner": {"kind": "double"}}, "description": ""},
                {"name": "d", "type": {"kind": "double"}, "description": ""}
            ]
        }],
        "functions": [{
            "name": "spread",
            "description": "",
            "params": [{"name": "x", "type": {"kind": "vec", "element": {"kind": "double"}}}],
            "return_type": {"kind": "double"}
        }]
    });
    DocumentSet::from(vec![IrDocument::from_json(&ir.to_string()).unwrap()])
}

/// `h` is missing in every 7th row; `d` is 0 in row 9,000 alone.
fn input() -> Value {
    let g: Vec<Value> = (0..ROWS).map(|i| json!(["a", "b", "c"][i % 3])).collect();
    let w: Vec<Value> = (0..ROWS).map(|i| json!(40.0 + (i % 97) as f64)).collect();
    let h: Vec<Value> = (0..ROWS)
        .map(|i| {
            if i % 7 == 3 {
                Value::Null
            } else {
                json!(1.4 + (i % 13) as f64 / 20.0)
            }
        })
        .collect();
    let d: Vec<Value> = (0..ROWS)
        .map(|i| json!(if i == 9_000 { 0.0 } else { 1.0 + i as f64 }))
        .collect();
    json!({"$length": ROWS, "g": g, "w": w, "h": h, "d": d})
}

/// A host's buffers, one for each column.
struct Buffers(Vec<Option<Vec<f64>>>);

impl Output for Buffers {
    fn doubles(&mut self, at: usize, n: usize) -> Option<&mut [f64]> {
        self.0[at] = Some(vec![f64::NAN; n]);
        self.0[at].as_deref_mut()
    }
}

fn run(exprs: &[Value], by: &[&str], summary: bool, into: bool) -> Result<Vec<Vec<u64>>, String> {
    let docs = docs();
    let q = QualifiedName::parse("ns::T").unwrap();
    let Some(TypeDef::Table(def)) = docs.find(&q).cloned() else {
        panic!("a table")
    };
    let input = input();
    let logic: Vec<Logic> = exprs.iter().map(Logic::from_value).collect();
    let refs: Vec<&Logic> = logic.iter().collect();
    let table = Json::new(&input);
    let mut buffers = Buffers(vec![None; exprs.len()]);
    let out: Result<Outcome, NotComputed> = if into {
        computed_into(
            table,
            &def,
            by,
            &refs,
            &docs,
            &NoFunctions,
            summary,
            1,
            &mut buffers,
            None,
        )
    } else {
        computed(table, &def, by, &refs, &docs, &NoFunctions, summary, 1)
    };
    let out = out.map_err(|e| format!("{e:?}"))?;
    // Each column's cells as bits, so that a missing cell compares exactly.
    Ok(out
        .columns
        .iter()
        .enumerate()
        .map(|(at, c)| match (&c.data, &buffers.0[at]) {
            (Some(Data::Double(v)), _) | (None, Some(v)) => v.iter().map(|x| x.to_bits()).collect(),
            (other, _) => panic!("not doubles: {other:?}"),
        })
        .collect())
}

fn same(exprs: &[Value], by: &[&str], summary: bool) {
    let own = run(exprs, by, summary, false);
    let into = run(exprs, by, summary, true);
    assert_eq!(own, into, "{exprs:?}");
}

#[test]
fn columns_written_in_place_equal_the_columns_the_core_holds() {
    let w = json!({"var": "w"});
    let h = json!({"var": "h"});
    let exprs = [
        json!({"/": [w, {"^": [h, 2]}]}),
        json!({"-": [w, h]}),
        json!({"*": [w, 2]}),
        json!({"/": [2, h]}),
        json!({"+": [w, w]}),
        w.clone(),
        h.clone(),
        json!({"+": [1, 2]}),
        json!({"/": [w, {"mean": [w]}]}),
    ];
    same(&exprs, &[], false);
    same(&exprs, &["g"], false);
    let summaries = [
        json!({"/": [{"sum": [w]}, {"n": []}]}),
        json!({"-": [{"max": [w]}, {"min": [w]}]}),
        json!({"mean": [{"na_rm": h}]}),
    ];
    same(&summaries, &["g"], true);
    same(&summaries, &[], true);
}

#[test]
fn a_zero_divisor_is_the_same_error_in_place() {
    let exprs = [json!({"/": [{"var": "w"}, {"var": "d"}]})];
    let own = run(&exprs, &[], false, false);
    let into = run(&exprs, &[], false, true);
    assert!(own.is_err(), "{own:?}");
    assert_eq!(own, into);
    assert!(into.unwrap_err().contains("row: 9000"));
}

/// The plan that a traced run leaves, of `exprs` by `by`.
fn traced(exprs: &[Value], by: &[&str], summary: bool) -> Plan {
    let docs = docs();
    let q = QualifiedName::parse("ns::T").unwrap();
    let Some(TypeDef::Table(def)) = docs.find(&q).cloned() else {
        panic!("a table")
    };
    let input = input();
    let logic: Vec<Logic> = exprs.iter().map(Logic::from_value).collect();
    let refs: Vec<&Logic> = logic.iter().collect();
    let names: Vec<String> = (0..exprs.len()).map(|i| format!("c{i}")).collect();
    let columns: Vec<&str> = names.iter().map(String::as_str).collect();
    let never = |_: &QualifiedName| false;
    let mut trace = Trace {
        call: TableCall {
            function: "r8set",
            label: "",
            table: &q,
            by,
            columns: &columns,
            exprs: &refs,
            summary,
            threads: 1,
            host: "r",
        },
        in_rust: &never,
        before: 0.5,
        plan: None,
    };
    let _ = computed_into(
        Json::new(&input),
        &def,
        by,
        &refs,
        &docs,
        &NoFunctions,
        summary,
        1,
        &mut OwnColumns,
        Some(&mut trace),
    );
    trace.plan.expect("a plan")
}

fn statuses(plan: &Plan) -> Vec<(String, Status)> {
    plan.nodes
        .iter()
        .map(|n| (n.kind.clone(), n.status))
        .collect()
}

#[test]
fn a_traced_run_stamps_the_stages_it_runs() {
    let plan = traced(
        &[json!({"/": [{"var": "w"}, {"mean": [{"var": "w"}]}]})],
        &["g"],
        false,
    );
    let s = statuses(&plan);
    assert_eq!(
        s,
        vec![
            ("r8set".to_string(), Status::Running),
            ("r8.check".to_string(), Status::Pending),
            ("r8.compile".to_string(), Status::Completed),
            ("r8.group".to_string(), Status::Completed),
            ("r8.aggregate".to_string(), Status::Completed),
            ("r8.evaluate".to_string(), Status::Completed),
            ("r8.assemble".to_string(), Status::Pending),
        ]
    );
    for n in &plan.nodes[2..6] {
        let (a, b) = (n.t_start.unwrap(), n.t_end.unwrap());
        assert!(a >= 0.5 && b >= a, "{n:?}");
        assert!(n.meta.busy.unwrap() >= 0.0);
    }
}

#[test]
fn a_failed_stage_holds_its_row_and_the_later_stages_stay_pending() {
    let plan = traced(&[json!({"/": [{"var": "w"}, {"var": "d"}]})], &[], false);
    let evaluate = &plan.nodes[3];
    assert_eq!(evaluate.kind, "r8.evaluate");
    assert_eq!(evaluate.status, Status::Failed);
    assert_eq!(evaluate.meta.row, Some(9_001));
    assert!(evaluate.meta.message.is_some());
    assert!(evaluate.t_end.unwrap() >= evaluate.t_start.unwrap());
    assert_eq!(plan.nodes[0].status, Status::Failed);
    assert_eq!(plan.nodes[4].status, Status::Pending);
}

#[test]
fn a_failed_call_holds_its_group_and_first_row() {
    let plan = traced(
        &[json!({"call": ["ns::spread", "x", {"var": "w"}]})],
        &["g"],
        true,
    );
    let call = plan.nodes.iter().find(|n| n.kind == "r8.call").unwrap();
    assert_eq!(call.status, Status::Failed);
    assert_eq!((call.meta.group, call.meta.row), (Some(1), Some(1)));
    // A failed stage holds the time that it ran.
    assert!(call.t_end.unwrap() >= call.t_start.unwrap());
    assert!(call.meta.busy.unwrap() >= 0.0);
    assert_eq!(plan.nodes[0].status, Status::Failed);
}

#[test]
fn a_key_that_is_no_column_fails_the_group_stage() {
    let plan = traced(&[json!({"var": "w"})], &["nope"], false);
    assert_eq!(
        statuses(&plan),
        vec![
            ("r8set".to_string(), Status::Failed),
            ("r8.check".to_string(), Status::Completed),
            ("r8.group".to_string(), Status::Failed),
        ]
    );
    let plan = traced(&[json!({"var": "nope"})], &[], false);
    assert_eq!(plan.nodes[2].kind, "r8.compile");
    assert_eq!(plan.nodes[2].status, Status::Failed);
}
