//! A check of a table that reports what it finds: how many rows satisfy each
//! column's type and each rule, and the rows that do not.
//!
//! A report reads every row and refuses nothing. [`check_rules`] holds a table
//! against rules, and [`check_table`] against the columns and the rules of a
//! type; a rule is read where the columns it names read, whatever their cells.

use std::collections::HashMap;

use crate::access::{Access, Form};
use crate::ir::{Column, ColumnDef, RuleDef};
use crate::rules::{Diagnosis, Held, RowRule, RuleError, TypeLookup};
use crate::validate::{RuleCache, ValueError, check_column};

/// What a line of a report is of.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Of {
    /// A column of the type, or one that the type does not admit.
    Column(String),
    /// The rule at a position.
    Rule(usize),
}

/// What a check found of a column or a rule.
#[derive(Debug, Clone)]
pub struct Line {
    /// The column or the rule.
    pub of: Of,
    /// The rows read: 0 where the column or the rule was not read.
    pub checked: u64,
    /// The rows that do not satisfy it.
    pub failed: u64,
    /// The rows where a rule has no answer.
    pub missing: u64,
    /// The rows where a rule is an error, such as a division by zero.
    pub errors: u64,
    /// The first problems.
    pub problems: Vec<Problem>,
}

impl Line {
    fn unread(of: Of) -> Line {
        Line {
            of,
            checked: 0,
            failed: 0,
            missing: 0,
            errors: 0,
            problems: Vec::new(),
        }
    }
}

/// A problem of a row, or of a column or a rule as a whole.
#[derive(Debug, Clone)]
pub struct Problem {
    /// The row, from 0.
    pub row: Option<u64>,
    /// The columns read.
    pub columns: Vec<String>,
    /// What was found.
    pub kind: Kind,
}

/// What a problem is.
#[derive(Debug, Clone)]
pub enum Kind {
    /// A value that a column's type does not admit.
    Value(Box<ValueError>),
    /// A rule that is false, with the values that it reads.
    Violation(Diagnosis),
    /// A rule that is an error, with the values that it reads.
    Error(&'static str, Diagnosis),
    /// A rule that does not fit the columns.
    Unread(Vec<RuleError>),
}

/// The rows of `table` held against `rules`, with at most `limit` problems for
/// each: the number of rows, and a line for each rule.
pub fn check_rules<'a, A: Access<'a>>(
    table: A,
    rules: &[RuleDef],
    lookup: &dyn TypeLookup,
    limit: usize,
) -> Result<(u64, Vec<Line>), Form> {
    let t = table.table::<ColumnDef>(&[], true)?;
    let held = Held::<A, ColumnDef> {
        declared: &[],
        table,
        columns: &t.columns,
    };
    let lines = rules
        .iter()
        .enumerate()
        .map(|(i, r)| {
            let rule = RowRule::report(r, &held, lookup);
            rule_line(i, rule, table, t.rows, limit)
        })
        .collect();
    Ok((t.rows, lines))
}

/// The columns of `table` held against the declared `columns`, and its rows
/// against `rules`, with at most `limit` problems for each: the number of rows, and
/// a line for each declared column, for each column that the type does not admit,
/// and for each rule.
pub fn check_table<'a, A: Access<'a>, C: Column>(
    table: A,
    columns: &[C],
    open: bool,
    rules: &[RuleDef],
    lookup: &dyn TypeLookup,
    limit: usize,
) -> Result<(u64, Vec<Line>), Form> {
    let t = table.table(columns, open)?;
    let mut cache = RuleCache::new();
    let mut lines = Vec::new();
    // Whether each column reads.
    let mut reads: HashMap<&str, bool> = HashMap::new();
    let fact = |of: &str, code: &'static str, message: String| {
        let error = ValueError {
            code,
            message,
            ..Default::default()
        };
        Problem {
            row: None,
            columns: vec![of.to_string()],
            kind: Kind::Value(Box::new(error)),
        }
    };
    for c in columns {
        let f = c.field();
        let name = f.name.as_str();
        let mut line = Line::unread(Of::Column(name.to_string()));
        let mut of = t.columns.iter().filter(|(k, _)| k == name);
        match (of.next(), of.next()) {
            (None, _) if !c.required() => {}
            (None, _) => {
                reads.insert(name, false);
                let p = fact(name, "missing_column", "is required".to_string());
                line.problems.push(p);
            }
            (Some(_), Some(_)) => {
                reads.insert(name, false);
                line.problems.push(fact(
                    name,
                    "duplicate_column",
                    "names two columns".to_string(),
                ));
            }
            (Some((_, cells)), None) => {
                let found = check_column(
                    name,
                    &f.ty,
                    c.unique(),
                    *cells,
                    t.rows,
                    lookup,
                    &mut cache,
                    limit,
                );
                reads.insert(name, found.read);
                if found.read {
                    line.checked = t.rows;
                    line.failed = found.rows;
                }
                line.problems = found
                    .errors
                    .into_iter()
                    .map(|e| Problem {
                        row: match e.segments.last() {
                            Some(crate::problem::Segment::Element(i)) => Some(*i),
                            _ => None,
                        },
                        columns: vec![name.to_string()],
                        kind: Kind::Value(Box::new(e)),
                    })
                    .collect();
            }
        }
        lines.push(line);
    }
    for (k, cells) in &t.columns {
        if columns.iter().any(|c| c.field().name.as_str() == &**k) {
            continue;
        }
        let problem = if !open {
            Some(fact(
                k,
                "unknown_column",
                "is not a column of the type".into(),
            ))
        } else {
            table
                .extra_column_type(k, *cells)
                .err()
                .map(|f| fact(k, f.code, f.message))
        };
        if let Some(p) = problem {
            let mut line = Line::unread(Of::Column(k.to_string()));
            line.problems.push(p);
            lines.push(line);
        }
    }
    for (i, r) in rules.iter().enumerate() {
        // A rule reads the columns that read; one that is absent is missing.
        let rule = RowRule::report(r, &columns, lookup).and_then(|rule| {
            let lost = rule
                .columns()
                .find(|c| reads.get(c) == Some(&false))
                .map(str::to_string);
            match lost {
                Some(c) => {
                    let (code, message) = if t.columns.iter().any(|(k, _)| **k == *c) {
                        ("column_type", format!("the cells of `{c}` do not read"))
                    } else {
                        ("rule_field", format!("`{c}` is not a column of the table"))
                    };
                    Err(vec![RuleError {
                        code,
                        path: String::new(),
                        message,
                    }])
                }
                None => Ok(rule),
            }
        });
        lines.push(rule_line(i, rule, table, t.rows, limit));
    }
    Ok((t.rows, lines))
}

fn rule_line<'a, A: Access<'a>>(
    index: usize,
    rule: Result<RowRule, Vec<RuleError>>,
    table: A,
    rows: u64,
    limit: usize,
) -> Line {
    let mut line = Line::unread(Of::Rule(index));
    let unread = |errors: Vec<RuleError>| Problem {
        row: None,
        columns: Vec::new(),
        kind: Kind::Unread(errors),
    };
    let rule = match rule {
        Ok(rule) => rule,
        Err(errors) => {
            line.problems.push(unread(errors));
            return line;
        }
    };
    match rule.tally(table, rows as usize, limit) {
        Ok(t) => {
            line.checked = t.checked;
            line.failed = t.failed;
            line.missing = t.missing;
            line.errors = t.errors;
            line.problems = t
                .found
                .into_iter()
                .map(|(row, error, d)| Problem {
                    row: row.map(|r| r as u64),
                    columns: rule.columns().map(str::to_string).collect(),
                    kind: match error {
                        Some(e) => Kind::Error(e, d),
                        None => Kind::Violation(d),
                    },
                })
                .collect();
        }
        Err(message) => line.problems.push(unread(vec![RuleError {
            code: "rule_error",
            path: String::new(),
            message,
        }])),
    }
    line
}
