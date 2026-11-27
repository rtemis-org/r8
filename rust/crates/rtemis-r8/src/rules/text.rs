//! The text of an expression of the rule dialect, in the syntax that rules
//! are written in: `lo <= hi`, `length(x) == length(y)`, `kernel$gamma <= 10`.
//!
//! [`text`] writes operators infix with R's precedence and parentheses where
//! the precedence needs them, functions by the names that rules call them,
//! fields with `$`, and constants as literals; an expression without such a
//! form, such as an operator that the dialect does not define, is its JSON
//! text.

use crate::ir::{Logic, Number, Operator};

/// The text of `e`.
pub fn text(e: &Logic) -> String {
    write(e)
}

/// The binding strength of an infix or prefix operator, after R's
/// `?Syntax`: a larger value binds more tightly.
fn precedence(op: Operator, unary: bool) -> Option<u8> {
    Some(match op {
        Operator::Pow => 9,
        Operator::Sub if unary => 8,
        Operator::In => 7,
        Operator::Mod => 7,
        Operator::Mul | Operator::Div => 6,
        Operator::Add | Operator::Sub => 5,
        Operator::StrictEq
        | Operator::StrictNe
        | Operator::Lt
        | Operator::Le
        | Operator::Gt
        | Operator::Ge => 4,
        Operator::Not => 3,
        Operator::And => 2,
        Operator::Or => 1,
        _ => return None,
    })
}

/// The infix spelling of a binary operator.
fn infix(op: Operator) -> Option<&'static str> {
    Some(match op {
        Operator::StrictEq => "==",
        Operator::StrictNe => "!=",
        Operator::Lt => "<",
        Operator::Le => "<=",
        Operator::Gt => ">",
        Operator::Ge => ">=",
        Operator::Add => "+",
        Operator::Sub => "-",
        Operator::Mul => "*",
        Operator::Div => "/",
        Operator::Mod => "%%",
        Operator::And => "&&",
        Operator::Or => "||",
        Operator::In => "%in%",
        Operator::Pow => "^",
        _ => return None,
    })
}

/// The name that a rule calls a function of the dialect by.
fn function(op: Operator) -> Option<&'static str> {
    Some(match op {
        Operator::Cat => "paste0",
        Operator::IsNull => "is.null",
        Operator::IsNa => "is.na",
        Operator::IsVariant => "is_variant",
        Operator::Length => "length",
        Operator::Nrow => "nrow",
        Operator::Ncol => "ncol",
        Operator::Dim => "dim",
        Operator::Levels => "levels",
        Operator::Floor => "floor",
        Operator::Ceiling => "ceiling",
        Operator::Round => "round",
        Operator::Min => "min",
        Operator::Max => "max",
        Operator::Abs => "abs",
        Operator::Sqrt => "sqrt",
        Operator::Log => "log",
        Operator::Exp => "exp",
        Operator::Sum => "sum",
        Operator::Mean => "mean",
        Operator::Pmin => "pmin",
        Operator::Pmax => "pmax",
        Operator::Nchar => "nchar",
        Operator::All => "all",
        Operator::Any => "any",
        Operator::Subset => "subset",
        Operator::NonIncreasing => "non_increasing",
        Operator::NonDecreasing => "non_decreasing",
        Operator::N => "n",
        Operator::Count => "count",
        Operator::Median => "median",
        Operator::Col => "col",
        _ => return None,
    })
}

/// Whether `e` is `na_rm` of one operand.
fn is_na_rm(e: &Logic) -> bool {
    matches!(e, Logic::Call { op: Operator::NaRm, args, .. } if args.len() == 1)
}

/// A field path as `a$b`, `.x` the element of an enclosing `all()` or
/// `any()`.
fn path(p: &str) -> String {
    let (head, rest) = match p.strip_prefix(".x") {
        Some(r) => (".x", r.strip_prefix('.').unwrap_or(r)),
        None => ("", p),
    };
    let mut parts: Vec<&str> = Vec::new();
    if !head.is_empty() {
        parts.push(head);
    }
    parts.extend(rest.split('.').filter(|s| !s.is_empty()));
    parts.join("$")
}

fn number(n: &Number) -> String {
    match n {
        Number::I64(i) => i.to_string(),
        Number::U64(u) => u.to_string(),
        Number::F64(x) => serde_json::Number::from_f64(*x)
            .map(|n| crate::canonical::canonical_text(&serde_json::Value::Number(n)))
            .unwrap_or_else(|| x.to_string()),
    }
}

fn string(s: &str) -> String {
    serde_json::to_string(s).unwrap_or_default()
}

/// The JSON text of an expression that has no form in the syntax of rules.
fn json(e: &Logic) -> String {
    serde_json::to_string(e).unwrap_or_default()
}

/// The text of `e` as the operand of an operator of precedence `outer`, in
/// parentheses when it binds less tightly, or as tightly on the side that
/// the operator does not group from (`right`).
fn operand(e: &Logic, outer: u8, right: bool) -> String {
    let inner = match e {
        Logic::Call { op, args, .. } => precedence(*op, args.len() == 1),
        _ => None,
    };
    let t = write(e);
    match inner {
        Some(p) if p < outer || (right && p == outer) => format!("({t})"),
        _ => t,
    }
}

fn write(e: &Logic) -> String {
    match e {
        Logic::Null => "NA".to_string(),
        Logic::Bool(true) => "TRUE".to_string(),
        Logic::Bool(false) => "FALSE".to_string(),
        Logic::Number(n) => number(n),
        Logic::String(s) => string(s),
        Logic::Array(items) => {
            let parts: Vec<String> = items.iter().map(write).collect();
            format!("c({})", parts.join(", "))
        }
        Logic::Var(p) => path(p),
        Logic::Other(_) => json(e),
        Logic::Call { op, args, .. } => {
            let args = &**args;
            match (op, args.len()) {
                (Operator::Var, 1) => match &args[0] {
                    Logic::String(p) => path(p),
                    _ => json(e),
                },
                (Operator::Path, _) => {
                    let parts: Option<Vec<&str>> = args.iter().map(Logic::as_str).collect();
                    parts.map_or_else(|| json(e), |p| p.join("$"))
                }
                (Operator::Not, 1) => format!("!{}", operand(&args[0], 3, false)),
                // `^` groups from the right: `a^b^c` is `a^(b^c)`.
                (Operator::Pow, 2) => {
                    // R reads `-2^2` as `-(2^2)`.
                    let base = match &args[0] {
                        Logic::Number(n) if number(n).starts_with('-') => {
                            format!("({})", number(n))
                        }
                        x => operand(x, 10, false),
                    };
                    format!("{base}^{}", operand(&args[1], 9, false))
                }
                // An aggregate of the cells that are not missing.
                (op, 1) if function(*op).is_some() && is_na_rm(&args[0]) => {
                    let Logic::Call { args: inner, .. } = &args[0] else {
                        unreachable!()
                    };
                    match &**inner {
                        [x] => format!(
                            "{}({}, na.rm = TRUE)",
                            function(*op).unwrap_or_default(),
                            write(x)
                        ),
                        _ => json(e),
                    }
                }
                (Operator::Sub, 1) => format!("-{}", operand(&args[0], 8, false)),
                // A typed function: its qualified name, then its arguments by
                // name.
                (Operator::Call, n) if n % 2 == 1 => {
                    let Some(name) = args[0].as_str() else {
                        return json(e);
                    };
                    let mut parts = Vec::with_capacity(n / 2);
                    for pair in args[1..].chunks(2) {
                        let Some(p) = pair[0].as_str() else {
                            return json(e);
                        };
                        parts.push(format!("{p} = {}", write(&pair[1])));
                    }
                    format!("{name}({})", parts.join(", "))
                }
                (Operator::If, 2 | 3) => {
                    let mut t = format!("if ({}) {}", write(&args[0]), write(&args[1]));
                    if let Some(no) = args.get(2) {
                        t.push_str(&format!(" else {}", write(no)));
                    }
                    t
                }
                (op, 2) if infix(*op).is_some() => {
                    let p = precedence(*op, false).unwrap_or(0);
                    format!(
                        "{} {} {}",
                        operand(&args[0], p, false),
                        infix(*op).unwrap_or_default(),
                        operand(&args[1], p, true)
                    )
                }
                (op, _) => match function(*op) {
                    Some(name) => {
                        let parts: Vec<String> = args.iter().map(write).collect();
                        format!("{name}({})", parts.join(", "))
                    }
                    None => json(e),
                },
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::text;
    use crate::ir::Logic;

    fn t(j: serde_json::Value) -> String {
        let e: Logic = serde_json::from_value(j).unwrap();
        text(&e)
    }

    #[test]
    fn writes_the_syntax_of_rules() {
        use serde_json::json;
        assert_eq!(t(json!({"<=": [{"var": "lo"}, {"var": "hi"}]})), "lo <= hi");
        assert_eq!(
            t(json!({"===": [{"length": {"var": "a"}}, {"length": {"var": "b"}}]})),
            "length(a) == length(b)"
        );
        assert_eq!(
            t(json!({"*": [{"+": [{"var": "a"}, 1]}, 2]})),
            "(a + 1) * 2"
        );
        assert_eq!(
            t(json!({"-": [{"var": "a"}, {"-": [{"var": "b"}, 1]}]})),
            "a - (b - 1)"
        );
        assert_eq!(t(json!({"!": {"is_null": {"var": "x"}}})), "!is.null(x)");
        assert_eq!(
            t(json!({"in": [{"var": "k"}, ["a", "b"]]})),
            "k %in% c(\"a\", \"b\")"
        );
        assert_eq!(
            t(json!({"<=": [{"var": "kernel.gamma"}, 10]})),
            "kernel$gamma <= 10"
        );
        assert_eq!(
            t(json!({"all": [{"var": "v"}, {">": [{"var": ".x"}, 0.5]}]})),
            "all(v, .x > 0.5)"
        );
        assert_eq!(
            t(json!({"or": [{"and": [{"var": "a"}, {"var": "b"}]}, {"var": "c"}]})),
            "a && b || c"
        );
        assert_eq!(
            t(json!({"and": [{"or": [{"var": "a"}, {"var": "b"}]}, {"var": "c"}]})),
            "(a || b) && c"
        );
        assert_eq!(t(json!({"if": [{"var": "a"}, 1, 2]})), "if (a) 1 else 2");
        assert_eq!(
            t(json!({"/": [{"var": "w"}, {"^": [{"var": "h"}, 2]}]})),
            "w / h^2"
        );
        assert_eq!(t(json!({"^": [{"^": [{"var": "a"}, 2]}, 3]})), "(a^2)^3");
        assert_eq!(t(json!({"^": [{"-": {"var": "a"}}, 2]})), "(-a)^2");
        assert_eq!(t(json!({"^": [-2, 2]})), "(-2)^2");
        assert_eq!(
            t(json!({"mean": {"na_rm": {"var": "sbp"}}})),
            "mean(sbp, na.rm = TRUE)"
        );
        assert_eq!(t(json!({"n": []})), "n()");
        assert_eq!(
            t(json!({"count": {">": [{"var": "x"}, 0]}})),
            "count(x > 0)"
        );
        assert_eq!(
            t(json!({"if": [{"===": [{"var": "d"}, 0]}, null, {"var": "d"}]})),
            "if (d == 0) NA else d"
        );
        assert_eq!(
            t(json!({"call": ["pkg::trimmed_mean", "x", {"col": []}, "trim", 0.2]})),
            "pkg::trimmed_mean(x = col(), trim = 0.2)"
        );
    }
}
