//! Evaluation of compiled expressions over JSON values.
//!
//! A `var` that reaches an absent value (a missing member or `null`) makes
//! the expression [`Absent`]; the absence propagates through every operator
//! except `is_null()`, and a rule with an absent operand holds. Conditional
//! positions take logicals only.

use serde_json::Value;

use super::compile::{Collection, Expr, Op, Root, Step, VariantTag};

/// An expression read an absent value.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Absent;

/// The result of evaluating an expression: a value, or absence. An `Err` is
/// a failure that compilation did not exclude, such as a division by zero.
pub type Evaluated = Result<Result<Value, Absent>, String>;

pub(crate) struct Env<'a> {
    pub data: &'a Value,
    pub element: Option<&'a Value>,
}

fn present(v: Value) -> Evaluated {
    Ok(Ok(v))
}

macro_rules! value {
    ($e:expr) => {
        match $e? {
            Ok(v) => v,
            Err(Absent) => return Ok(Err(Absent)),
        }
    };
}

/// A number: a JSON number, or a non-finite double in its string form.
pub(crate) fn as_number(v: &Value) -> Option<f64> {
    match v {
        Value::Number(n) => n.as_f64(),
        Value::String(s) => match s.as_str() {
            "Infinity" => Some(f64::INFINITY),
            "-Infinity" => Some(f64::NEG_INFINITY),
            "NaN" => Some(f64::NAN),
            _ => None,
        },
        _ => None,
    }
}

fn number(x: f64) -> Value {
    if x.is_finite() && x.fract() == 0.0 && x.abs() < 9.007_199_254_740_992e15 {
        Value::from(x as i64)
    } else if x.is_nan() {
        Value::from("NaN")
    } else if x.is_infinite() {
        Value::from(if x > 0.0 { "Infinity" } else { "-Infinity" })
    } else {
        serde_json::Number::from_f64(x).map_or(Value::Null, Value::Number)
    }
}

fn want_number(v: &Value, op: &str) -> Result<f64, String> {
    as_number(v).ok_or_else(|| format!("`{op}` takes numbers, found {v}"))
}

fn want_bool(v: &Value, what: &str) -> Result<bool, String> {
    v.as_bool()
        .ok_or_else(|| format!("{what} is a logical condition, found {v}"))
}

/// Strict equality: numbers by value, everything else structurally.
pub(crate) fn strict_eq(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Array(x), Value::Array(y)) => {
            x.len() == y.len() && x.iter().zip(y).all(|(p, q)| strict_eq(p, q))
        }
        _ => match (as_number(a), as_number(b)) {
            (Some(x), Some(y)) if a.is_number() || b.is_number() => x == y,
            _ => a == b,
        },
    }
}

/// The values of a factor: a string array, or the strings that
/// `{"levels", "codes"}` encodes.
fn factor_values(v: &Value) -> Option<Vec<Value>> {
    match v {
        Value::Array(items) => Some(items.clone()),
        Value::Object(m) => {
            let levels = m.get("levels")?.as_array()?;
            let codes = m.get("codes")?.as_array()?;
            codes
                .iter()
                .map(|c| {
                    let i = c.as_u64()? as usize;
                    levels.get(i.checked_sub(1)?).cloned()
                })
                .collect()
        }
        _ => None,
    }
}

/// The items of an array-like value: an array, or a factor's values.
fn items(v: &Value) -> Option<Vec<Value>> {
    factor_values(v)
}

fn shape(v: &Value) -> Option<Vec<u64>> {
    v.get("shape")?
        .as_array()?
        .iter()
        .map(Value::as_u64)
        .collect()
}

/// The number of rows of a table (`"$length"`) or a matrix (`shape[0]`).
fn nrow(v: &Value) -> Option<u64> {
    v.get("$length")
        .and_then(Value::as_u64)
        .or_else(|| shape(v)?.first().copied())
}

fn ncol(v: &Value) -> Option<u64> {
    if v.get("$length").is_some() {
        let m = v.as_object()?;
        return Some(m.keys().filter(|k| !k.starts_with('$')).count() as u64);
    }
    shape(v)?.get(1).copied()
}

impl Env<'_> {
    pub fn eval(&self, e: &Expr) -> Evaluated {
        match e {
            Expr::Lit(v) => present(v.clone()),
            Expr::Var { root, steps } => {
                let start = match root {
                    Root::Data => self.data,
                    Root::Element => match self.element {
                        Some(v) => v,
                        None => return Err("`.x` outside `all()` or `any()`".to_string()),
                    },
                };
                let mut cur = start;
                for s in steps {
                    let next = match (s, cur) {
                        (Step::Member(k), Value::Object(m)) => m.get(k),
                        (Step::OrderedKey(k), Value::Array(pairs)) => pairs
                            .iter()
                            .find(|p| p.get("name").and_then(Value::as_str) == Some(k))
                            .and_then(|p| p.get("value")),
                        _ => None,
                    };
                    match next {
                        Some(v) => cur = v,
                        None => return Ok(Err(Absent)),
                    }
                }
                if cur.is_null() {
                    Ok(Err(Absent))
                } else {
                    present(cur.clone())
                }
            }
            Expr::If(parts) => {
                let mut i = 0;
                while i + 1 < parts.len() {
                    let c = value!(self.eval(&parts[i]));
                    if want_bool(&c, "an `if` condition")? {
                        return self.eval(&parts[i + 1]);
                    }
                    i += 2;
                }
                self.eval(&parts[i])
            }
            Expr::And(parts) => {
                for p in parts {
                    if !want_bool(&value!(self.eval(p)), "an operand of `and`")? {
                        return present(Value::Bool(false));
                    }
                }
                present(Value::Bool(true))
            }
            Expr::Or(parts) => {
                for p in parts {
                    if want_bool(&value!(self.eval(p)), "an operand of `or`")? {
                        return present(Value::Bool(true));
                    }
                }
                present(Value::Bool(false))
            }
            Expr::IsNull(inner) => present(Value::Bool(self.eval(inner)?.is_err())),
            Expr::IsVariant {
                value,
                variant,
                tag,
            } => {
                let v = value!(self.eval(value));
                let name = match (tag, &v) {
                    (VariantTag::UnitOnly | VariantTag::Unknown, Value::String(s)) => {
                        Some(s.as_str())
                    }
                    (VariantTag::Member(m), Value::Object(o)) => o.get(m).and_then(Value::as_str),
                    (VariantTag::Unknown, Value::Object(_)) => {
                        return Err(
                            "the variant of an enum outside the lookup cannot be read".to_string()
                        );
                    }
                    _ => None,
                };
                match name {
                    Some(n) => present(Value::Bool(n == variant)),
                    None => Err(format!("{v} is not a value of the enum")),
                }
            }
            Expr::Levels { value, declared } => {
                let v = value!(self.eval(value));
                let levels = match (&v, declared) {
                    (Value::Object(m), _) => m.get("levels").cloned(),
                    (_, Some(d)) => Some(Value::from(d.clone())),
                    _ => None,
                };
                levels
                    .map(present)
                    .unwrap_or_else(|| Err(format!("{v} carries no levels")))
            }
            Expr::Quantifier {
                all,
                collection,
                kind,
                condition,
            } => {
                let c = value!(self.eval(collection));
                let elements = elements(&c, *kind)?;
                for (_, el) in &elements {
                    let env = Env {
                        data: self.data,
                        element: Some(el),
                    };
                    let holds = match env.eval(condition)? {
                        Ok(v) => want_bool(&v, "a quantifier's condition")?,
                        // An absent operand: the element holds.
                        Err(Absent) => *all,
                    };
                    if holds != *all {
                        return present(Value::Bool(!*all));
                    }
                }
                present(Value::Bool(*all))
            }
            Expr::Op(op, args) => {
                let mut vals = Vec::with_capacity(args.len());
                for a in args {
                    vals.push(value!(self.eval(a)));
                }
                apply(*op, &vals).map(Ok)
            }
        }
    }
}

/// The elements of a collection, each with its JSON Pointer suffix.
pub(crate) fn elements(c: &Value, kind: Collection) -> Result<Vec<(String, Value)>, String> {
    match (kind, c) {
        (Collection::OrderedMap, Value::Array(pairs)) => Ok(pairs
            .iter()
            .enumerate()
            .map(|(i, p)| {
                (
                    format!("/{i}/value"),
                    p.get("value").cloned().unwrap_or(Value::Null),
                )
            })
            .collect()),
        (Collection::Array | Collection::Dynamic, Value::Array(items)) => Ok(items
            .iter()
            .enumerate()
            .map(|(i, v)| (format!("/{i}"), v.clone()))
            .collect()),
        (Collection::Hashmap | Collection::Dynamic, Value::Object(m)) => Ok(m
            .iter()
            .map(|(k, v)| {
                (
                    format!("/{}", k.replace('~', "~0").replace('/', "~1")),
                    v.clone(),
                )
            })
            .collect()),
        _ => Err(format!("{c} is not a collection")),
    }
}

fn apply(op: Op, a: &[Value]) -> Result<Value, String> {
    let bool_ = |b: bool| Ok(Value::Bool(b));
    match op {
        Op::Eq => bool_(strict_eq(&a[0], &a[1])),
        Op::Ne => bool_(!strict_eq(&a[0], &a[1])),
        Op::Lt | Op::Le | Op::Gt | Op::Ge => {
            let ord = match (as_number(&a[0]), as_number(&a[1])) {
                (Some(x), Some(y)) => x.partial_cmp(&y),
                // Dates and date-times compare in their canonical text form,
                // which sorts in time order.
                _ => match (&a[0], &a[1]) {
                    (Value::String(x), Value::String(y)) => Some(x.cmp(y)),
                    _ => return Err(format!("cannot compare {} and {}", a[0], a[1])),
                },
            };
            let Some(o) = ord else { return bool_(false) };
            use std::cmp::Ordering::*;
            bool_(match op {
                Op::Lt => o == Less,
                Op::Le => o != Greater,
                Op::Gt => o == Greater,
                _ => o != Less,
            })
        }
        Op::Not => bool_(!want_bool(&a[0], "the operand of `!`")?),
        Op::Add | Op::Mul | Op::Min | Op::Max => {
            let mut xs = Vec::with_capacity(a.len());
            for v in a {
                xs.push(want_number(v, "arithmetic")?);
            }
            let r = match op {
                Op::Add => xs.iter().sum(),
                Op::Mul => xs.iter().product(),
                Op::Min => xs.iter().copied().fold(f64::INFINITY, f64::min),
                _ => xs.iter().copied().fold(f64::NEG_INFINITY, f64::max),
            };
            Ok(number(r))
        }
        Op::Sub => {
            let x = want_number(&a[0], "-")?;
            Ok(number(match a.get(1) {
                Some(y) => x - want_number(y, "-")?,
                None => -x,
            }))
        }
        Op::Div | Op::Mod => {
            let (x, y) = (want_number(&a[0], "/")?, want_number(&a[1], "/")?);
            if y == 0.0 {
                return Err("division by zero".to_string());
            }
            Ok(number(if op == Op::Div { x / y } else { x % y }))
        }
        Op::Floor => Ok(number(want_number(&a[0], "floor")?.floor())),
        Op::Ceiling => Ok(number(want_number(&a[0], "ceiling")?.ceil())),
        // R's round(): halves to even (IEC 60559).
        Op::Round => Ok(number(want_number(&a[0], "round")?.round_ties_even())),
        Op::In => match &a[1] {
            Value::String(s) => match &a[0] {
                Value::String(n) => bool_(s.contains(n.as_str())),
                other => Err(format!("`in` over a string takes a string, found {other}")),
            },
            other => {
                let hay =
                    items(other).ok_or_else(|| format!("`in` searches an array, found {other}"))?;
                bool_(hay.iter().any(|i| strict_eq(i, &a[0])))
            }
        },
        Op::Cat => {
            let mut out = String::new();
            for v in a {
                match v {
                    Value::String(s) => out.push_str(s),
                    Value::Number(n) => out.push_str(&n.to_string()),
                    other => return Err(format!("`cat` joins strings and numbers, found {other}")),
                }
            }
            Ok(Value::String(out))
        }
        Op::Length => match &a[0] {
            Value::String(s) => Ok(Value::from(s.chars().count())),
            other => items(other)
                .map(|i| Value::from(i.len()))
                .ok_or_else(|| format!("`length` takes an array, found {other}")),
        },
        Op::Subset => {
            let (x, y) = (items(&a[0]), items(&a[1]));
            match (x, y) {
                (Some(x), Some(y)) => bool_(x.iter().all(|i| y.iter().any(|j| strict_eq(i, j)))),
                _ => Err("`subset` takes arrays".to_string()),
            }
        }
        Op::NonIncreasing | Op::NonDecreasing => {
            let xs = items(&a[0]).ok_or("`non_increasing` takes an array")?;
            let mut ok = true;
            for w in xs.windows(2) {
                let o = match (as_number(&w[0]), as_number(&w[1])) {
                    (Some(p), Some(q)) => p.partial_cmp(&q),
                    _ => match (&w[0], &w[1]) {
                        (Value::String(p), Value::String(q)) => Some(p.cmp(q)),
                        _ => {
                            return Err(
                                "an ordered array holds numbers, dates, or date-times".to_string()
                            );
                        }
                    },
                };
                ok &= match o {
                    Some(std::cmp::Ordering::Less) => op == Op::NonDecreasing,
                    Some(std::cmp::Ordering::Greater) => op == Op::NonIncreasing,
                    Some(std::cmp::Ordering::Equal) => true,
                    None => false,
                };
            }
            bool_(ok)
        }
        Op::Nrow => nrow(&a[0])
            .map(Value::from)
            .ok_or_else(|| format!("`nrow` takes a table or a matrix, found {}", a[0])),
        Op::Ncol => ncol(&a[0])
            .map(Value::from)
            .ok_or_else(|| format!("`ncol` takes a table or a matrix, found {}", a[0])),
        Op::Dim => shape(&a[0])
            .map(Value::from)
            .ok_or_else(|| format!("`dim` takes a matrix, found {}", a[0])),
    }
}

/// The rows of a table value, each as an object of the row's cells; a factor
/// column given as `{"levels", "codes"}` contributes its decoded strings.
pub(crate) fn rows(table: &Value) -> Result<Vec<Value>, String> {
    let m = table.as_object().ok_or("a table is a JSON object")?;
    let n = m
        .get("$length")
        .and_then(Value::as_u64)
        .ok_or("a table has `$length`")? as usize;
    let mut columns = Vec::new();
    for (k, v) in m.iter().filter(|(k, _)| !k.starts_with('$')) {
        let cells = factor_values(v).ok_or_else(|| format!("column `{k}` is not an array"))?;
        if cells.len() != n {
            return Err(format!(
                "column `{k}` has {} cells, the table has {n} rows",
                cells.len()
            ));
        }
        columns.push((k.clone(), cells));
    }
    Ok((0..n)
        .map(|i| {
            Value::Object(
                columns
                    .iter()
                    .map(|(k, c)| (k.clone(), c[i].clone()))
                    .collect(),
            )
        })
        .collect())
}
