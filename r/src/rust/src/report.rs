//! The report of a check: a table held against rules or a type, as R vectors.
//!
//! spec: R8/raven#checks

use extendr_api::prelude::*;
use extendr_api::{Error, Result, SEXP};
use rtemis_r8::ir::{FieldType, TypeDef};
use rtemis_r8::problem::{Problem as Requirement, Syntax};
use rtemis_r8::report::{Kind, Line, Of, check_rules, check_table};
use rtemis_r8::rules::{Diagnosis, Shown, TypeLookup};
use rtemis_r8::validate::ValueError;

use crate::construct::with_cache;
use crate::declare::registry;
use crate::fieldtype::in_r;
use crate::rview::{RView, is_null};
use crate::values::{RSyntax, r_path, shown_text, shown_value};

fn fail(m: impl Into<String>) -> Error {
    Error::Other(m.into())
}

/// A report as R vectors: `rows`, the number of rows; of each line, `column`, `rule`,
/// `checked`, `failed`, `missing`, and `errors`; and of each problem, `line`, `row`,
/// `columns`, `found`, `code`, and `message`.
fn written(rows: u64, lines: &[Line]) -> Result<Robj> {
    let (mut column, mut rule) = (Vec::new(), Vec::new());
    let mut counts: [Vec<f64>; 4] = Default::default();
    let (mut line, mut row) = (Vec::new(), Vec::new());
    let (mut columns, mut found, mut code, mut message) =
        (Vec::new(), Vec::new(), Vec::new(), Vec::new());
    for (k, l) in lines.iter().enumerate() {
        let (c, r) = match &l.of {
            Of::Column(c) => (Rstr::from(c.as_str()), Rint::na()),
            Of::Rule(i) => (Rstr::na(), Rint::from(*i as i32 + 1)),
        };
        column.push(c);
        rule.push(r);
        for (to, n) in counts
            .iter_mut()
            .zip([l.checked, l.failed, l.missing, l.errors])
        {
            to.push(n as f64);
        }
        for p in &l.problems {
            line.push(k as i32 + 1);
            row.push(p.row.map_or(Rfloat::na(), |r| Rfloat::from(r as f64 + 1.0)));
            columns.push(p.columns.join(", "));
            let (f, c, m) = match &p.kind {
                Kind::Value(e) => (value_found(e, &p.columns), e.code, value_message(e)),
                Kind::Violation(d) => (read(d), "rule_violation", String::new()),
                Kind::Error(m, d) => (read(d), "rule_error", m.to_string()),
                Kind::Unread(errors) => {
                    let texts: Vec<String> = errors.iter().map(|e| in_r(&e.message)).collect();
                    let code = errors.first().map_or("rule_error", |e| e.code);
                    (String::new(), code, texts.join("; "))
                }
            };
            found.push(f);
            code.push(c);
            message.push(m);
        }
    }
    let [checked, failed, missing, errors] = counts;
    let lines: Robj = list!(
        column = column.into_iter().collect::<Strings>(),
        rule = rule.into_iter().collect::<Integers>(),
        checked = checked,
        failed = failed,
        missing = missing,
        errors = errors
    )
    .into();
    let problems: Robj = list!(
        line = line,
        row = row.into_iter().collect::<Doubles>(),
        columns = columns,
        found = found,
        code = code,
        message = message
    )
    .into();
    Ok(list!(rows = rows as f64, lines = lines, problems = problems).into())
}

/// The values that a rule reads: `dbp = 130, sbp = 118`.
fn read(d: &Diagnosis) -> String {
    let parts: Vec<String> = d
        .fields
        .iter()
        .filter_map(|(p, s)| shown_text(s, &r_path(p)))
        .collect();
    parts.join(", ")
}

/// The value found in a column: `age = 152`.
fn value_found(e: &ValueError, columns: &[String]) -> String {
    match (e.found.as_ref().and_then(shown_value), columns) {
        (Some(v), [c]) => format!("{c} = {v}"),
        _ => String::new(),
    }
}

/// What a value fails: `must be at most 130`.
fn value_message(e: &ValueError) -> String {
    let requirement = e
        .problem
        .as_ref()
        .and_then(|p: &Requirement| p.requirement(&RSyntax));
    match (requirement, &e.found) {
        // What is found of a column as a whole.
        (Some(r), Some(Shown::Kind(k))) => format!("must {r}; found {k}"),
        (Some(r), Some(f @ (Shown::Length(_) | Shown::Dim(_)))) => {
            format!("must {r}; found {}", RSyntax.shown(f))
        }
        (Some(r), _) => format!("must {r}"),
        (None, _) => e.message.clone(),
    }
}

/// The rows of a table held against rules, or against a table type.
///
/// @param columns List: The columns by name.
/// @param value A value of a table, or `NULL`.
/// @param rules List of `rule()`, or `NULL`.
/// @param type An R8 table type, `type_table()`, or `NULL`.
/// @param limit Integer: The problems kept of each line.
/// @return List, as `written()` gives it, or the message of what is refused.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_report(columns: Robj, value: Robj, rules: Robj, r#type: Robj, limit: i32) -> Result<Robj> {
    let xs: SEXP = unsafe { columns.get() };
    let limit = usize::try_from(limit).unwrap_or(0);
    crate::interrupt::interruptible(xs, || {
        let docs = registry().snapshot();
        let lookup: &dyn TypeLookup = &*docs;
        // A value's units are those of its columns.
        let view = if is_null(unsafe { value.get() }) {
            RView::Draft(xs, "")
        } else {
            RView::Whole(unsafe { value.get() })
        };
        let done = if is_null(unsafe { r#type.get() }) {
            let given = match crate::reader::rules(unsafe { rules.get() }, "/rules") {
                Ok(r) => r,
                Err(p) => return Ok(Robj::from(p.message)),
            };
            check_rules(view, &given, lookup, limit)
        } else {
            let ty = crate::fieldtype::field_type(unsafe { r#type.get() })
                .map_err(|e| fail(e.to_string()))?;
            match &ty {
                FieldType::Table {
                    columns,
                    open,
                    rules,
                } => check_table(view, columns, *open, rules, lookup, limit),
                FieldType::Named { name } => match lookup.find(name) {
                    Some(TypeDef::Table(t)) => {
                        check_table(view, &t.columns, t.open, &t.rules, lookup, limit)
                    }
                    _ => return Ok(Robj::from(format!("`{name}` is not a table type"))),
                },
                _ => return Ok(Robj::from("the type is not one of tables")),
            }
        };
        match done {
            Ok((rows, lines)) => written(rows, &lines),
            Err(f) => Ok(Robj::from(f.message)),
        }
    })
}

/// The problems of an object that is not a table, held against a type.
///
/// @param x An object.
/// @param type A field type, or an R8 type.
/// @return List, as `written()` gives it.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_report_object(x: Robj, r#type: Robj) -> Result<Robj> {
    let xs = unsafe { x.get() };
    let ty =
        crate::fieldtype::field_type(unsafe { r#type.get() }).map_err(|e| fail(e.to_string()))?;
    crate::interrupt::interruptible(xs, || {
        let docs = registry().snapshot();
        let lookup: &dyn TypeLookup = &*docs;
        let errors = with_cache(|cache| {
            rtemis_r8::validate::validate_field_access(RView::Whole(xs), &ty, lookup, cache)
        });
        let (mut path, mut row, mut found) = (Vec::new(), Vec::new(), Vec::new());
        let (mut code, mut message) = (Vec::new(), Vec::new());
        for e in &errors {
            path.push(r_path(&e.segments));
            row.push(match e.segments.as_slice() {
                [rtemis_r8::problem::Segment::Element(i)] => Rfloat::from(*i as f64 + 1.0),
                _ => Rfloat::na(),
            });
            found.push(e.found.as_ref().and_then(shown_value).unwrap_or_default());
            code.push(e.code);
            message.push(match &e.diagnosis {
                Some(d) if e.message.is_empty() => read(d),
                _ => value_message(e),
            });
        }
        Ok(list!(
            path = path,
            row = row.into_iter().collect::<Doubles>(),
            found = found,
            code = code,
            message = message
        )
        .into())
    })
}

extendr_module! {
    mod report;
    fn r8_report;
    fn r8_report_object;
}
