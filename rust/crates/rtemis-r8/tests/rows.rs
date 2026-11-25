//! Rules over the rows of a table: the evaluation by column gives what the
//! evaluation of each row as a value gives, over random expressions.

use rtemis_r8::access::Json;
use rtemis_r8::ir::{FieldDef, Logic, RuleDef};
use rtemis_r8::rules::{CompiledRule, DocumentSet, Outcome, RowRule};
use serde_json::{Value, json};

const ROWS: usize = 1500;

struct Random(u64);

impl Random {
    fn next(&mut self, n: usize) -> usize {
        self.0 ^= self.0 << 13;
        self.0 ^= self.0 >> 7;
        self.0 ^= self.0 << 17;
        (self.0 >> 11) as usize % n
    }
    fn of<'a, T>(&mut self, xs: &'a [T]) -> &'a T {
        &xs[self.next(xs.len())]
    }
}

fn fields() -> Vec<FieldDef> {
    let option = |t: Value| json!({"kind": "option", "inner": t});
    let number = option(json!({"kind": "double", "infinite": true, "nan": true}));
    [
        ("a", number.clone()),
        ("b", number),
        ("i", option(json!({"kind": "integer"}))),
        ("p", option(json!({"kind": "logical"}))),
        ("s", option(json!({"kind": "character"}))),
        ("t", option(json!({"kind": "character"}))),
        ("d", option(json!({"kind": "date"}))),
        ("e", option(json!({"kind": "date"}))),
    ]
    .into_iter()
    .map(|(name, ty)| serde_json::from_value(json!({"name": name, "type": ty})).unwrap())
    .collect()
}

/// The cells of each column.
fn table(r: &mut Random) -> Value {
    let numbers = [
        json!(null),
        json!("NaN"),
        json!("Infinity"),
        json!("-Infinity"),
        json!(0.0),
        json!(-1.5),
        json!(2.0),
        json!(3.25),
        json!(-0.0),
    ];
    let dates = [
        json!(null),
        json!("2020-01-01"),
        json!("2021-06-01"),
        json!("1969-12-31"),
    ];
    let texts = [json!(null), json!("a"), json!("b"), json!("ab"), json!("é")];
    let whole = [json!(null), json!(-2), json!(0), json!(1), json!(7)];
    let flags = [json!(null), json!(true), json!(false)];
    let mut column = |xs: &[Value]| -> Value { (0..ROWS).map(|_| r.of(xs).clone()).collect() };
    json!({
        "$length": ROWS,
        "a": column(&numbers), "b": column(&numbers), "i": column(&whole),
        "p": column(&flags), "s": column(&texts), "t": column(&texts),
        "d": column(&dates), "e": column(&dates),
    })
}

fn number(r: &mut Random, depth: usize) -> Value {
    if depth == 0 || r.next(4) == 0 {
        return match r.next(5) {
            0 => json!(*r.of(&[0.0, 1.0, -2.0, 2.5])),
            _ => json!({"var": *r.of(&["a", "b", "i"])}),
        };
    }
    let d = depth - 1;
    match r.next(12) {
        0 => json!({"+": [number(r, d), number(r, d)]}),
        1 => json!({"-": [number(r, d), number(r, d)]}),
        2 => json!({"*": [number(r, d), number(r, d)]}),
        3 => json!({"/": [number(r, d), number(r, d)]}),
        4 => json!({"%": [number(r, d), number(r, d)]}),
        5 => json!({"pmin": [number(r, d), number(r, d), number(r, d)]}),
        6 => json!({"pmax": [number(r, d), number(r, d)]}),
        7 => json!({*r.of(&["abs", "floor", "ceiling", "round", "-"]): [number(r, d)]}),
        8 => json!({*r.of(&["sqrt", "log", "exp"]): [number(r, d)]}),
        9 => json!({"nchar": [text(r, d)]}),
        10 => json!({"if": [logical(r, d), number(r, d), number(r, d)]}),
        _ => json!({"var": "a"}),
    }
}

fn text(r: &mut Random, depth: usize) -> Value {
    if depth == 0 {
        return match r.next(4) {
            0 => json!(*r.of(&["a", "ab", "é"])),
            _ => json!({"var": *r.of(&["s", "t"])}),
        };
    }
    let d = depth - 1;
    match r.next(4) {
        0 => json!({"cat": [text(r, d), json!({"var": "i"})]}),
        1 => json!({"cat": [text(r, d), number(r, d), text(r, d)]}),
        2 => json!({"if": [logical(r, d), text(r, d), text(r, d)]}),
        _ => text(r, 0),
    }
}

fn date(r: &mut Random, depth: usize) -> Value {
    match r.next(if depth == 0 { 2 } else { 3 }) {
        2 => json!({"if": [logical(r, depth - 1), json!({"var": "d"}), json!({"var": "e"})]}),
        k => json!({"var": (["d", "e"][k])}),
    }
}

fn logical(r: &mut Random, depth: usize) -> Value {
    if depth == 0 {
        return json!({"var": "p"});
    }
    let d = depth - 1;
    let compare = *r.of(&["<", "<=", ">", ">=", "===", "!=="]);
    let equal = *r.of(&["===", "!=="]);
    match r.next(14) {
        0 | 1 => json!({"and": [logical(r, d), logical(r, d)]}),
        2 | 3 => json!({"or": [logical(r, d), logical(r, d), logical(r, d)]}),
        4 => json!({"!": [logical(r, d)]}),
        5 | 6 => {
            // Strict equality takes numbers that arithmetic has not rounded.
            let compare = if matches!(compare, "===" | "!==") {
                "<"
            } else {
                compare
            };
            json!({compare: [number(r, d), number(r, d)]})
        }
        7 => json!({equal: [{"var": "a"}, {"var": *r.of(&["b", "i"])}]}),
        8 => json!({equal: [text(r, d), text(r, d)]}),
        9 => json!({compare: [date(r, d), date(r, d)]}),
        10 => json!({compare: [date(r, d), *r.of(&["2020-01-01", "2020-07-01"])]}),
        11 => match r.next(4) {
            0 => json!({"in": [{"var": "i"}, [0, 7, 9]]}),
            1 => json!({"in": [text(r, d), ["a", "é"]]}),
            2 => json!({"in": [{"var": "d"}, ["2020-01-01"]]}),
            _ => json!({"in": [text(r, d), text(r, d)]}),
        },
        12 => json!({"is_na": [match r.next(4) {
            0 => number(r, d),
            1 => text(r, d),
            2 => date(r, d),
            _ => logical(r, d),
        }]}),
        _ => json!({equal: [logical(r, d), logical(r, d)]}),
    }
}

fn rule(invariant: &Value, when: Option<&Value>) -> RuleDef {
    let mut r = json!({"invariant": invariant});
    if let Some(w) = when {
        r["when"] = w.clone();
    }
    serde_json::from_value(r).unwrap()
}

/// `Ok(Some(b))` for a logical result, `Ok(None)` for a missing one, `Err` for
/// an error, of a condition over the row as a value.
fn of_row(
    holds: &CompiledRule,
    negated: &CompiledRule,
    row: &Value,
    fields: &[FieldDef],
    docs: &DocumentSet,
) -> Result<Option<bool>, ()> {
    let of = |r: &CompiledRule| {
        r.check_access(Json::new(row), fields, docs)
            .map(|o| o == Outcome::Holds)
            .map_err(|_| ())
    };
    Ok(match (of(holds)?, of(negated)?) {
        (false, true) => Some(false),
        (true, false) => Some(true),
        (true, true) => None,
        (false, false) => panic!("a condition and its negation are both false"),
    })
}

#[test]
fn a_rule_over_rows_is_the_rule_over_each_row() {
    let mut r = Random(0x2545_f491_4f6c_dd1d);
    let fields = fields();
    let docs = DocumentSet::from(Vec::new());
    let table = table(&mut r);
    let rows: Vec<Value> = (0..ROWS)
        .map(|i| {
            let cells = fields.iter().map(|f| {
                let k = f.name.as_str();
                (k.to_string(), table[k][i].clone())
            });
            Value::Object(cells.collect())
        })
        .collect();
    let (mut chosen, mut failing, mut errors) = (0, 0, 0);
    for _ in 0..120 {
        let condition = logical(&mut r, 4);
        let columns = fields.as_slice();
        let of = |c: &Value| {
            CompiledRule::compile(&rule(c, None), &fields, &docs)
                .unwrap_or_else(|e| panic!("{condition}: {e:?}"))
        };
        let (holds, negated) = (of(&condition), of(&json!({"!": [condition]})));
        let want: Vec<_> = rows
            .iter()
            .map(|row| of_row(&holds, &negated, row, &fields, &docs))
            .collect();

        let filter = RowRule::filter(&Logic::from_value(&condition), &columns, &docs).unwrap();
        match filter.choose(Json::new(&table), ROWS).unwrap() {
            Ok(got) => {
                let expected: Vec<u32> = (0..ROWS as u32)
                    .filter(|&i| want[i as usize] == Ok(Some(true)))
                    .collect();
                assert_eq!(got, expected, "{condition}");
                assert!(want.iter().all(Result::is_ok), "{condition}");
                chosen += got.len();
            }
            Err(e) => {
                assert_eq!(
                    want.iter().position(Result::is_err),
                    Some(e.row),
                    "{condition}"
                );
                assert_eq!(
                    want.iter().filter(|w| w.is_err()).count(),
                    e.count,
                    "{condition}"
                );
                errors += 1;
            }
        }

        // The rule as one that the rows satisfy, with a condition on it.
        let when = logical(&mut r, 2);
        let checked = RowRule::compile(&rule(&condition, Some(&when)), &columns, &docs).unwrap();
        let by_row = CompiledRule::compile(&rule(&condition, Some(&when)), &fields, &docs).unwrap();
        let expected: Vec<(usize, bool)> = rows
            .iter()
            .enumerate()
            .filter_map(
                |(i, row)| match by_row.check_access(Json::new(row), &fields, &docs) {
                    Ok(Outcome::Holds) => None,
                    Ok(Outcome::Violated(_)) => Some((i, false)),
                    Err(_) => Some((i, true)),
                },
            )
            .take(rtemis_r8::validate::ERROR_LIMIT + 1)
            .collect();
        let got: Vec<(usize, bool)> = checked
            .failing(Json::new(&table), ROWS)
            .unwrap()
            .into_iter()
            .map(|(i, e)| (i, e.is_some()))
            .collect();
        assert_eq!(got, expected, "{condition} when {when}");
        failing += got.len();
    }
    // The expressions reach every outcome.
    assert!(
        chosen > 5_000 && failing > 1_000 && errors > 5,
        "{chosen} {failing} {errors}"
    );
}

#[test]
fn an_aggregate_reads_every_row_once() {
    let docs = DocumentSet::from(Vec::new());
    let fields = fields();
    let columns = fields.as_slice();
    let x: Vec<f64> = (0..ROWS).map(|i| (i as f64).sin() * 1e8).collect();
    let table = json!({"$length": ROWS, "a": x});
    let count = |c: Value| {
        RowRule::filter(&Logic::from_value(&c), &columns, &docs)
            .unwrap()
            .choose(Json::new(&table), ROWS)
            .unwrap()
            .unwrap()
            .len()
    };
    let mean = {
        // The reference sums in the order of the magnitudes, which rounds least.
        let mut y = x.clone();
        y.sort_by(|a, b| a.abs().total_cmp(&b.abs()));
        y.iter().sum::<f64>() / ROWS as f64
    };
    let above = x.iter().filter(|&&v| v > mean).count();
    assert_eq!(
        count(json!({">": [{"var": "a"}, {"mean": [{"var": "a"}]}]})),
        above
    );
    let max = x.iter().copied().fold(f64::NEG_INFINITY, f64::max);
    let top = x.iter().filter(|&&v| v == max).count();
    assert_eq!(
        count(json!({"===": [{"var": "a"}, {"max": [{"var": "a"}]}]})),
        top
    );
    // An empty table has no row to read.
    let empty = json!({"$length": 0, "a": []});
    let none = RowRule::filter(
        &Logic::from_value(&json!({">": [{"var": "a"}, {"min": [{"var": "a"}]}]})),
        &columns,
        &docs,
    )
    .unwrap();
    assert_eq!(none.choose(Json::new(&empty), 0).unwrap(), Ok(vec![]));
}
