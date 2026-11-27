//! Rules over the rows of a table, evaluated by column.
//!
//! [`RowRule`] holds a rule whose names are the columns of a table. It is
//! evaluated over blocks of rows: an operator reads the cells of each operand
//! of a block and writes those of its result, so that the cost of the
//! expression is per block. Aggregates are evaluated first, over all rows.
//!
//! A cell is a value, missing, or an error, such as a division by zero. `NaN`
//! is missing. Every operator gives an error for an operand that is one, and
//! otherwise a missing cell for an operand that is missing, except `is_na()`,
//! and an `and` or an `or` that another operand decides. A check fails where
//! its rule is false, and a filter keeps the rows where it is true.

use std::borrow::Cow;
use std::cell::{Cell as Flag, RefCell};
use std::collections::{HashMap, HashSet};
use std::sync::Arc;

use serde_json::Value;

use super::calc::{self, Argument, CallValue, Data, Functions, Kind};
use super::compile::{
    ArgValue, CallArg, Cell, Expr, Op, RowScope, Rows, RuleError, compile_rows, reads_cell_outside,
};
use super::diagnose::{Diagnosis, comparison, shown_scalar};
use super::eval::{LOGARITHM, POWER, ROOT, Scalar, ZERO, extreme, median, power, unwrap_option};
use super::exact::{ExactSum, PairSum};
use super::parallel::{self, Cells, Part};
use super::{Shown, TypeLookup};
use crate::access::{Access, Factor, Json, Read, is_na_real};
use crate::emit::number_text;
use crate::ir::{FieldType, Logic, RuleDef, TableDef};
use crate::name::Name;
use crate::name::QualifiedName;
use crate::problem::Segment;

/// The rows of a block.
const BLOCK: usize = 1024;

/// The states of a cell without a value: missing, and the errors.
pub(super) const MISSING: u8 = 1;
pub(super) const STATES: [&str; 6] = ["", "", ZERO, ROOT, LOGARITHM, POWER];
const ZERO_STATE: u8 = 2;
const ROOT_STATE: u8 = 3;
const LOG_STATE: u8 = 4;
const POWER_STATE: u8 = 5;

/// The logical of a cell in state `s`: 0 is false and 1 true, so that the
/// result of an `and` without a false operand, and of an `or` without a true
/// one, is the greatest.
pub(super) const fn stated(s: u8) -> u8 {
    s + 1
}
pub(super) const NA: u8 = stated(MISSING);

/// A missing number, a `NaN`.
pub(super) const NA_NUMBER: f64 = f64::from_bits(crate::abi::NA_DOUBLE_BITS);

/// A row whose rule is an error.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RowError {
    /// The row, from 0.
    pub row: usize,
    /// The number of rows with an error.
    pub count: usize,
    /// What the error is, such as a division by zero.
    pub message: &'static str,
}

/// A rule compiled against the columns of a table.
pub struct RowRule {
    /// The rule's ID, if it has one.
    pub id: Option<Name>,
    /// The rule's message, if it has one.
    pub message: Option<String>,
    rows: Rows,
}

impl std::fmt::Debug for RowRule {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("RowRule")
            .field("id", &self.id)
            .field("invariant", &self.rows.invariant)
            .finish_non_exhaustive()
    }
}

impl RowRule {
    /// Compiles a rule that the rows of a table satisfy. Error paths are
    /// relative to the rule (`/invariant/...`, `/when/...`).
    pub fn compile(
        rule: &RuleDef,
        columns: &dyn RowScope,
        lookup: &dyn TypeLookup,
    ) -> Result<RowRule, Vec<RuleError>> {
        Ok(RowRule {
            id: rule.id.clone(),
            message: rule.message.clone(),
            rows: compile_rows(&rule.invariant, rule.when.as_ref(), columns, lookup, false)?,
        })
    }

    /// Compiles a condition that chooses rows of a table, which may read whole
    /// columns with `mean`, `sum`, `min`, and `max`.
    pub fn filter(
        condition: &Logic,
        columns: &dyn RowScope,
        lookup: &dyn TypeLookup,
    ) -> Result<RowRule, Vec<RuleError>> {
        Ok(RowRule {
            id: None,
            message: None,
            rows: compile_rows(condition, None, columns, lookup, true)?,
        })
    }

    /// The path of a row's problem, relative to the table: the cell of the
    /// first column that the rule reads, or the row alone.
    pub fn path(&self, row: usize) -> Vec<Segment> {
        self.rows
            .cells
            .first()
            .map(|c| Segment::Column(c.name.to_string()))
            .into_iter()
            .chain([Segment::Element(row as u64)])
            .collect()
    }

    /// The rows of `0..rows` that fail the rule, with the error of a row that
    /// has one, in order; one past the limit of reported problems marks the
    /// list as truncated.
    pub fn failing<'a, A: Access<'a>>(
        &self,
        table: A,
        rows: usize,
    ) -> Result<Vec<(usize, Option<&'static str>)>, String> {
        let mut out = Vec::new();
        if rows == 0 {
            return Ok(out);
        }
        let eval = Eval::new(&self.rows, table, rows, None, 1)?;
        for lo in (0..rows).step_by(BLOCK) {
            let hi = (lo + BLOCK).min(rows);
            for (i, &v) in eval.holds(&self.rows, lo, hi)?.iter().enumerate() {
                match v {
                    1 | NA => {}
                    0 => out.push((lo + i, None)),
                    e => out.push((lo + i, Some(STATES[e as usize - 1]))),
                }
            }
            if out.len() > crate::validate::ERROR_LIMIT {
                break;
            }
        }
        out.truncate(crate::validate::ERROR_LIMIT + 1);
        Ok(out)
    }

    /// The rows of `0..rows` where the rule is true, in order.
    pub fn choose<'a, A: Access<'a>>(
        &self,
        table: A,
        rows: usize,
    ) -> Result<Result<Vec<u32>, RowError>, String> {
        let mut out: Vec<u32> = Vec::new();
        if rows == 0 {
            return Ok(Ok(out));
        }
        if u32::try_from(rows).is_err() {
            return Err(format!(
                "rows are chosen in a table of at most 4,294,967,295; this one has {rows}"
            ));
        }
        let mut error: Option<RowError> = None;
        let eval = Eval::new(&self.rows, table, rows, None, 1)?;
        for lo in (0..rows).step_by(BLOCK) {
            let hi = (lo + BLOCK).min(rows);
            let found = eval.holds(&self.rows, lo, hi)?;
            let from = out.len();
            // Each row is written, and kept where the rule is true.
            out.resize(from + found.len(), 0);
            let mut at = from;
            let mut highest = 0;
            for (i, &v) in found.iter().enumerate() {
                out[at] = (lo + i) as u32;
                at += usize::from(v == 1);
                highest = highest.max(v);
            }
            out.truncate(at);
            if highest > NA {
                for (i, &v) in found.iter().enumerate().filter(|(_, v)| **v > NA) {
                    match &mut error {
                        Some(e) => e.count += 1,
                        None => {
                            error = Some(RowError {
                                row: lo + i,
                                count: 1,
                                message: STATES[v as usize - 1],
                            })
                        }
                    }
                }
            }
        }
        Ok(match error {
            Some(e) => Err(e),
            None => Ok(out),
        })
    }

    /// Compiles a rule over the rows of a table for a report, which may read
    /// whole columns.
    pub fn report(
        rule: &RuleDef,
        columns: &dyn RowScope,
        lookup: &dyn TypeLookup,
    ) -> Result<RowRule, Vec<RuleError>> {
        Ok(RowRule {
            id: rule.id.clone(),
            message: rule.message.clone(),
            rows: compile_rows(&rule.invariant, rule.when.as_ref(), columns, lookup, true)?,
        })
    }

    /// The columns that the rule reads, in the order it reads them.
    pub fn columns(&self) -> impl Iterator<Item = &str> {
        self.rows.cells.iter().map(|c| &*c.name)
    }

    /// Counts the rows of `0..rows` by what the rule is in them, and gives the
    /// first `limit` rows that fail, with the values it reads there. A rule
    /// that reads whole columns only is read once, for the table.
    pub fn tally<'a, A: Access<'a>>(
        &self,
        table: A,
        rows: usize,
        limit: usize,
    ) -> Result<Tally, String> {
        let mut out = Tally::default();
        if rows == 0 {
            return Ok(out);
        }
        let eval = Eval::new(&self.rows, table, rows, None, 1)?;
        let by_row =
            reads_row(&self.rows.invariant) || self.rows.when.as_ref().is_some_and(reads_row);
        let last = if by_row { rows } else { 1 };
        out.checked = last as u64;
        for lo in (0..last).step_by(BLOCK) {
            let hi = (lo + BLOCK).min(last);
            for (i, &v) in eval.results(&self.rows, lo, hi)?.iter().enumerate() {
                let error = match v {
                    1 => continue,
                    NA => {
                        out.missing += 1;
                        continue;
                    }
                    0 => {
                        out.failed += 1;
                        None
                    }
                    e => {
                        out.errors += 1;
                        Some(STATES[e as usize - 1])
                    }
                };
                if out.found.len() < limit {
                    let mut d = self.diagnosis(&eval, lo + i);
                    if error.is_some() {
                        d.operands.clear();
                    }
                    out.found.push((by_row.then_some(lo + i), error, d));
                }
            }
        }
        Ok(out)
    }

    /// The paths of the rows of a table's JSON form that fail the rule; the
    /// error of a row is an `Err`.
    pub fn check_rows(&self, table: &Value) -> Result<Vec<Vec<Segment>>, String> {
        let rows = table
            .get("$length")
            .and_then(Value::as_u64)
            .ok_or("a table has `$length`")? as usize;
        self.failing(Json::new(table), rows)?
            .into_iter()
            .map(|(row, error)| match error {
                Some(e) => Err(e.to_string()),
                None => Ok(self.path(row)),
            })
            .collect()
    }

    /// The values that the rule reads in the row `row`, and the computed
    /// operands of the comparison that it tests.
    pub fn diagnose<'a, A: Access<'a>>(&self, table: A, rows: usize, row: usize) -> Diagnosis {
        match Eval::new(&self.rows, table, rows, None, 1) {
            Ok(eval) => self.diagnosis(&eval, row),
            Err(_) => Diagnosis::default(),
        }
    }

    fn diagnosis<'a, A: Access<'a>>(&self, eval: &Eval<'a, A>, row: usize) -> Diagnosis {
        let mut out = Diagnosis::default();
        let shown = |e: &Expr| match eval.block(e, row, row + 1) {
            Ok(b) => match b.scalar() {
                Some(s) => shown_scalar(s),
                None => Shown::Missing,
            },
            Err(_) => Shown::Other,
        };
        let mut read = vec![false; self.rows.cells.len()];
        read_cells(&self.rows.invariant, &mut read);
        if let Some(w) = &self.rows.when {
            read_cells(w, &mut read);
        }
        for (k, c) in self.rows.cells.iter().enumerate().filter(|(k, _)| read[*k]) {
            let s = if c.sensitive {
                Shown::Sensitive
            } else {
                shown(&Expr::Cell(k))
            };
            out.fields
                .push((vec![Segment::Field(c.name.to_string())], s));
        }
        if let Expr::Op(op, args) = &self.rows.invariant
            && let Some(name) = comparison(*op)
        {
            for (i, x) in args.iter().enumerate() {
                if !matches!(x, Expr::Cell(_) | Expr::Lit(_)) {
                    out.operands.push((format!("/{name}/{i}"), shown(x)));
                }
            }
        }
        out
    }
}

/// What a rule is in the rows of a table.
#[derive(Debug, Clone, Default)]
pub struct Tally {
    /// The rows read: one for a rule that reads whole columns only.
    pub checked: u64,
    /// The rows where the rule is false.
    pub failed: u64,
    /// The rows where it is missing.
    pub missing: u64,
    /// The rows where it is an error.
    pub errors: u64,
    /// The first rows where it is false or an error: the row, unless the rule
    /// is of the table, the error, and the values that the rule reads.
    pub found: Vec<(Option<usize>, Option<&'static str>, Diagnosis)>,
}

/// Marks the cells of the row that an expression reads, without those of columns
/// that it reads as a whole only.
fn read_cells(e: &Expr, out: &mut [bool]) {
    match e {
        Expr::Cell(k) => out[*k] = true,
        Expr::If(xs) | Expr::And(xs) | Expr::Or(xs) | Expr::Op(_, xs) => {
            xs.iter().for_each(|x| read_cells(x, out));
        }
        Expr::IsNa(x) | Expr::IsNull(x) => read_cells(x, out),
        _ => {}
    }
}

/// Whether an expression reads a cell of the row, and not only whole columns.
fn reads_row(e: &Expr) -> bool {
    match e {
        Expr::Cell(_) => true,
        Expr::If(xs) | Expr::And(xs) | Expr::Or(xs) | Expr::Op(_, xs) => xs.iter().any(reads_row),
        Expr::IsNa(x) | Expr::IsNull(x) => reads_row(x),
        _ => false,
    }
}

/// Why a condition chooses no rows of a table.
#[derive(Debug, Clone, PartialEq)]
pub enum NotChosen {
    /// The condition does not fit the columns; paths are relative to it.
    Rule(Vec<RuleError>),
    /// The condition is an error in a row.
    Row(RowError),
    /// The table does not read.
    Table(String),
}

/// The columns that a table has: those that a type declares with its types, and
/// the others with the types that their vectors have.
pub(crate) struct Held<'t, 'a, A, C> {
    pub declared: &'t [C],
    pub table: A,
    pub columns: &'t [(Cow<'a, str>, A)],
}

impl<'a, A: Access<'a>, C: crate::ir::Column> RowScope for Held<'_, 'a, A, C> {
    fn column(&self, name: &str) -> Option<Result<(Cell, Option<String>), String>> {
        let mut of = self.columns.iter().filter(|(k, _)| k == name);
        let (_, cells) = of.next()?;
        if of.next().is_some() {
            return Some(Err(format!("two columns are named `{name}`")));
        }
        if let Some(found) = self.declared.column(name) {
            return Some(found);
        }
        Some(match self.table.extra_column_type(name, *cells) {
            Ok(ty) => {
                let cell = Cell {
                    name: name.into(),
                    ty,
                    sensitive: false,
                };
                let facts = self.table.column_facts(name).unwrap_or_default();
                Ok((cell, facts.unit))
            }
            Err(f) => Err(f.message),
        })
    }

    fn nearest(&self, name: &str) -> Option<String> {
        crate::name::nearest(name, self.columns.iter().map(|(k, _)| &**k)).map(str::to_string)
    }
}

/// The rows of a valid value of the table `def` where `condition` is true, from
/// 0 and in order. A name in the condition is a column that the value has.
pub fn choose_rows<'a, A: Access<'a>>(
    table: A,
    def: &TableDef,
    condition: &Logic,
    lookup: &dyn TypeLookup,
) -> Result<Vec<u32>, NotChosen> {
    let t = table
        .table(&def.columns, def.open)
        .map_err(|f| NotChosen::Table(f.message))?;
    let rows = t.rows as usize;
    let held = Held {
        declared: &def.columns,
        table,
        columns: &t.columns,
    };
    RowRule::filter(condition, &held, lookup)
        .map_err(NotChosen::Rule)?
        .choose(table, rows)
        .map_err(NotChosen::Table)?
        .map_err(NotChosen::Row)
}

/// The states of the cells of a block; `None` where each has a value.
pub(super) type States = Option<Vec<u8>>;

/// What the instants of a block are.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(super) enum Instant {
    Date,
    DateTime,
}

impl Instant {
    fn scalar(self, t: i64) -> Scalar {
        match self {
            Instant::Date => Scalar::Date(t),
            Instant::DateTime => Scalar::DateTime(t),
        }
    }
}

/// The numbers of a block; `NaN` is missing.
pub(super) enum Numbers<'a> {
    Host(&'a [f64]),
    Own(Vec<f64>),
}

impl Numbers<'_> {
    pub(super) fn all(&self) -> &[f64] {
        match self {
            Numbers::Host(x) => x,
            Numbers::Own(x) => x,
        }
    }
}

/// The cells of one expression in a block of rows.
pub(super) enum Block<'e, 'a> {
    /// Logicals, by `stated()`.
    Bool(Vec<u8>),
    /// The number of a cell whose state is missing is `NaN`.
    Number(Numbers<'a>, States),
    /// Days of dates, or microseconds of date-times.
    Time(Vec<i64>, States, Instant),
    Text(Vec<Cow<'a, str>>, States),
    /// The labels of a factor: 1-based codes, and those outside the levels
    /// missing.
    Label(Cow<'e, [i32]>, &'e [Cow<'a, str>]),
    /// One value for every row.
    One(Scalar),
    /// One state for every row.
    Nothing(u8),
}

impl<'e, 'a> Block<'e, 'a> {
    /// The value of a block of one row, or of every row.
    fn scalar(&self) -> Option<Scalar> {
        let set = |s: &States| s.as_ref().is_none_or(|s| s[0] == 0);
        match self {
            Block::Bool(x) => (x[0] < NA).then_some(Scalar::Bool(x[0] == 1)),
            Block::Number(x, s) => {
                let x = x.all()[0];
                (set(s) && !is_na_real(x)).then_some(Scalar::Num(x))
            }
            Block::Time(x, s, of) => set(s).then(|| of.scalar(x[0])),
            Block::Text(x, s) => set(s).then(|| Scalar::Str(x[0].to_string())),
            Block::Label(codes, levels) => {
                label(levels, codes[0]).map(|l| Scalar::Str(l.to_string()))
            }
            Block::One(s) => Some(s.clone()),
            Block::Nothing(_) => None,
        }
    }

    /// The logicals of a condition.
    pub(super) fn logicals(self, n: usize) -> Result<Vec<u8>, String> {
        match self {
            Block::Bool(x) => Ok(x),
            Block::One(Scalar::Bool(b)) => Ok(vec![u8::from(b); n]),
            Block::Nothing(s) => Ok(vec![stated(s); n]),
            _ => Err("a condition is a logical".to_string()),
        }
    }

    /// The numbers, or the one of every row, and the states.
    fn numbers(&self, n: usize) -> Result<(Operand<'_>, States), String> {
        match self {
            Block::Number(x, s) => Ok((Operand::Many(x.all()), s.clone())),
            Block::One(Scalar::Num(x)) => Ok((Operand::One(*x), None)),
            Block::Nothing(s) => Ok((Operand::One(f64::NAN), (*s > MISSING).then(|| vec![*s; n]))),
            _ => Err("the operand is a number".to_string()),
        }
    }

    /// The text of cell `i`, `Err` its state.
    pub(super) fn text(&self, i: usize) -> Result<&str, u8> {
        match self {
            Block::Text(x, s) => match s.as_ref().map_or(0, |s| s[i]) {
                0 => Ok(&x[i]),
                s => Err(s),
            },
            Block::Label(codes, levels) => label(levels, codes[i]).ok_or(MISSING),
            Block::One(Scalar::Str(t)) => Ok(t),
            Block::Nothing(s) => Err(*s),
            _ => Err(MISSING),
        }
    }

    /// The instant of cell `i`, `Err` its state.
    pub(super) fn time(&self, i: usize) -> Result<i64, u8> {
        match self {
            Block::Time(x, s, _) => match s.as_ref().map_or(0, |s| s[i]) {
                0 => Ok(x[i]),
                s => Err(s),
            },
            Block::One(Scalar::Date(x) | Scalar::DateTime(x)) => Ok(*x),
            Block::Nothing(s) => Err(*s),
            _ => Err(MISSING),
        }
    }

    fn is_text(&self) -> bool {
        matches!(
            self,
            Block::Text(..) | Block::Label(..) | Block::One(Scalar::Str(_))
        )
    }

    /// What the instants of the block are, if it holds some.
    fn instant(&self) -> Option<Instant> {
        match self {
            Block::Time(_, _, of) => Some(*of),
            Block::One(Scalar::Date(_)) => Some(Instant::Date),
            Block::One(Scalar::DateTime(_)) => Some(Instant::DateTime),
            _ => None,
        }
    }

    /// The instant of every row.
    fn one_time(&self) -> Option<i64> {
        match self {
            Block::One(Scalar::Date(x) | Scalar::DateTime(x)) => Some(*x),
            _ => None,
        }
    }

    fn is_time(&self) -> bool {
        self.instant().is_some()
    }

    fn is_number(&self) -> bool {
        matches!(self, Block::Number(..) | Block::One(Scalar::Num(_)))
    }
}

/// The level of a code, `None` for one outside the levels.
fn label<'s>(levels: &'s [Cow<'_, str>], code: i32) -> Option<&'s str> {
    let at = usize::try_from(code).ok()?.checked_sub(1)?;
    levels.get(at).map(|l| &**l)
}

/// The numbers of an operand.
#[derive(Clone, Copy)]
enum Operand<'s> {
    Many(&'s [f64]),
    One(f64),
}

impl Operand<'_> {
    /// `f` of each pair of cells.
    #[inline]
    fn with<T>(self, other: Operand<'_>, n: usize, f: impl Fn(f64, f64) -> T) -> Vec<T> {
        match (self, other) {
            (Operand::Many(a), Operand::Many(b)) => {
                a.iter().zip(b).map(|(&a, &b)| f(a, b)).collect()
            }
            (Operand::Many(a), Operand::One(b)) => a.iter().map(|&a| f(a, b)).collect(),
            (Operand::One(a), Operand::Many(b)) => b.iter().map(|&b| f(a, b)).collect(),
            (Operand::One(a), Operand::One(b)) => (0..n).map(|_| f(a, b)).collect(),
        }
    }

    fn each<T>(self, n: usize, f: impl Fn(f64) -> T) -> Vec<T> {
        self.with(Operand::One(0.0), n, |a, _| f(a))
    }
}

/// The states of a result: the greatest of the operands', since an error is
/// above a missing cell.
fn merge(a: States, b: States) -> States {
    match (a, b) {
        (None, s) | (s, None) => s,
        (Some(mut a), Some(b)) => {
            a.iter_mut().zip(&b).for_each(|(a, &b)| *a = (*a).max(b));
            Some(a)
        }
    }
}

/// Marks the cells of `states` where `is` as `state`, unless they are an
/// error.
fn mark(states: States, is: &[bool], state: u8) -> States {
    if !is.iter().any(|&x| x) {
        return states;
    }
    let mut s = states.unwrap_or_else(|| vec![0; is.len()]);
    for (s, &is) in s.iter_mut().zip(is) {
        if is && *s <= MISSING {
            *s = state;
        }
    }
    Some(s)
}

/// A logical for each cell with a value, and the state of another.
fn overlay(mut x: Vec<u8>, states: States) -> Vec<u8> {
    if let Some(s) = states {
        for (x, &s) in x.iter_mut().zip(&s) {
            if s != 0 {
                *x = stated(s);
            }
        }
    }
    x
}

/// A comparison of numbers, missing where one is `NaN`.
#[inline]
fn compared(a: Operand<'_>, b: Operand<'_>, n: usize, f: impl Fn(f64, f64) -> bool) -> Vec<u8> {
    a.with(b, n, |x, y| {
        let missing = u8::from(x.is_nan() | y.is_nan());
        (u8::from(f(x, y)) & (1 - missing)) | (missing * NA)
    })
}

/// The literals of an `in`, by kind.
enum Set<'e> {
    Numbers(Vec<u64>),
    Times(HashSet<i64>),
    Texts(HashSet<&'e str>),
    Bools([bool; 2]),
    Empty,
}

/// A number as a key: equal numbers have one, and 0 and -0.
fn key(x: f64) -> u64 {
    (x + 0.0).to_bits()
}

impl<'e> Set<'e> {
    fn of(xs: &'e [Scalar]) -> Set<'e> {
        match xs.first() {
            Some(Scalar::Num(_)) => {
                let mut keys: Vec<u64> = xs
                    .iter()
                    .filter_map(|x| match x {
                        Scalar::Num(x) => Some(key(*x)),
                        _ => None,
                    })
                    .collect();
                keys.sort_unstable();
                Set::Numbers(keys)
            }
            Some(Scalar::Date(_) | Scalar::DateTime(_)) => Set::Times(
                xs.iter()
                    .filter_map(|x| match x {
                        Scalar::Date(x) | Scalar::DateTime(x) => Some(*x),
                        _ => None,
                    })
                    .collect(),
            ),
            Some(Scalar::Str(_)) => Set::Texts(
                xs.iter()
                    .filter_map(|x| match x {
                        Scalar::Str(x) => Some(x.as_str()),
                        _ => None,
                    })
                    .collect(),
            ),
            Some(Scalar::Bool(_)) => Set::Bools([
                xs.contains(&Scalar::Bool(false)),
                xs.contains(&Scalar::Bool(true)),
            ]),
            _ => Set::Empty,
        }
    }

    fn number(&self, x: f64) -> bool {
        match self {
            Set::Numbers(keys) if keys.len() <= 8 => keys.contains(&key(x)),
            Set::Numbers(keys) => keys.binary_search(&key(x)).is_ok(),
            _ => false,
        }
    }
}

/// A column as the host holds it.
enum Column<'a, A> {
    Double(A, Option<&'a [f64]>),
    Integer(A, Option<&'a [i32]>),
    Logical(A),
    Character(A),
    Date(A),
    Datetime(A),
    Factor(Factor<'a, A>),
    Absent,
}

/// The rows of a table in groups, which the order of their keys numbers from
/// 0.
#[derive(Debug, Clone)]
pub struct Groups {
    /// The group of each row.
    pub ids: Vec<u32>,
    /// The number of groups.
    pub count: usize,
    /// The first row of each group.
    pub first: Vec<u32>,
    /// The number of rows of each group.
    sizes: Vec<u32>,
    /// The rows of each group in order, made when a median asks for them:
    /// those of group `k` are `rows[starts[k]..starts[k + 1]]`.
    order: std::sync::OnceLock<(Vec<u32>, Vec<u32>)>,
}

impl Groups {
    /// The groups of `ids`, which number `count` groups from 0, each with a
    /// row.
    pub fn new(ids: Vec<u32>, count: usize) -> Groups {
        let mut sizes = vec![0u32; count];
        let mut first = vec![u32::MAX; count];
        for (r, &k) in ids.iter().enumerate() {
            let k = k as usize;
            if sizes[k] == 0 {
                first[k] = r as u32;
            }
            sizes[k] += 1;
        }
        Groups {
            ids,
            count,
            first,
            sizes,
            order: std::sync::OnceLock::new(),
        }
    }

    /// The groups of `ids`, with the number of rows and the first row of
    /// each.
    pub(super) fn from_parts(ids: Vec<u32>, sizes: Vec<u32>, first: Vec<u32>) -> Groups {
        Groups {
            ids,
            count: sizes.len(),
            first,
            sizes,
            order: std::sync::OnceLock::new(),
        }
    }

    /// The rows of group `k`, in order.
    pub fn rows_of(&self, k: usize) -> &[u32] {
        let (starts, rows) = self.order.get_or_init(|| {
            let mut starts = vec![0u32; self.count + 1];
            for k in 0..self.count {
                starts[k + 1] = starts[k] + self.sizes[k];
            }
            let mut next = starts[..self.count].to_vec();
            let mut rows = vec![0u32; self.ids.len()];
            for (r, &k) in self.ids.iter().enumerate() {
                rows[next[k as usize] as usize] = r as u32;
                next[k as usize] += 1;
            }
            (starts, rows)
        });
        &rows[starts[k] as usize..starts[k + 1] as usize]
    }

    /// The number of rows of each group.
    pub fn sizes(&self) -> impl Iterator<Item = usize> + '_ {
        self.sizes.iter().map(|&n| n as usize)
    }
}

/// The values of an aggregate or a call for each group, or for the table.
pub(super) enum Values {
    Numbers(Vec<f64>),
    Times(Vec<i64>, Instant),
    Logicals(Vec<bool>),
    Texts(Vec<String>),
}

/// Why the aggregates and calls of an expression have no values.
pub(super) enum Failed {
    /// The table does not read.
    Table(String),
    /// A cell of an argument of a call is an error.
    Row(RowError),
    /// A call is an error.
    Call {
        /// The function's qualified name.
        function: String,
        /// The group, from 0.
        group: usize,
        /// The call's message.
        message: String,
    },
}

/// An aggregate or a call of an expression, at its place.
enum Found<'e> {
    Aggregate(Op, &'e [Expr], bool),
    Call(&'e QualifiedName, &'e [CallArg], Kind),
}

/// An aggregate's value for each group, and its state, 0 where it has a value.
pub(super) struct Aggregated {
    pub(super) values: Values,
    pub(super) states: Vec<u8>,
}

/// The columns of a table, and the results of the aggregates over them.
pub(super) struct Eval<'a, A> {
    columns: Vec<Column<'a, A>>,
    rows: usize,
    /// The groups of the rows, if they are in groups.
    groups: Option<Arc<Groups>>,
    /// Each aggregate's value for each group, or for the table.
    aggregates: Vec<Aggregated>,
    /// The rows of a block are groups, whose cells are the aggregates'.
    over_groups: Flag<bool>,
    /// The threads that the aggregates of a column may run on.
    threads: usize,
    /// By the address of a comparison with literals, what it is for each level
    /// of the factor compared.
    levels: RefCell<HashMap<usize, Vec<u8>>>,
    /// By the address of a comparison with literals, what it is for strings
    /// that it met, at a place that their key gives.
    met: RefCell<HashMap<usize, Vec<(usize, u8)>>>,
}

/// The values of an aggregate as its rows are read.
struct Acc {
    op: Op,
    skip: bool,
    least: bool,
    /// The cells with a value, of each group.
    count: Vec<u64>,
    sums: Vec<PairSum>,
    extremes: Vec<f64>,
    states: Vec<u8>,
    /// The value of each row, for a gathered aggregate: `NaN` where it has
    /// none.
    by_row: Vec<f64>,
}

impl Acc {
    /// A missing cell of group `k`, which `skip` leaves out, or an error.
    fn note(&mut self, k: usize, s: u8) {
        if s > MISSING || !self.skip {
            self.states[k] = self.states[k].max(s);
        }
    }

    /// A value of group `k`, of the row `r` for a gathered aggregate.
    fn push(&mut self, k: usize, x: f64, r: Option<usize>) {
        self.count[k] += 1;
        match self.op {
            Op::Sum | Op::Mean => self.sums[k].add(x),
            Op::Count => {}
            Op::Min | Op::Max => self.extremes[k] = extreme(self.least, self.extremes[k], x),
            _ => {
                if let Some(r) = r {
                    self.by_row[r] = x;
                }
            }
        }
    }
}

/// The strings that a comparison with literals keeps the result of.
const MET: usize = 4096;

impl<'a, A: Access<'a>> Eval<'a, A> {
    pub(super) fn new(
        of: &Rows,
        table: A,
        rows: usize,
        groups: Option<Arc<Groups>>,
        threads: usize,
    ) -> Result<Self, String> {
        Self::calling(of, table, rows, groups, threads, &calc::NoFunctions).map_err(|f| match f {
            Failed::Table(m) => m,
            Failed::Row(e) => e.message.to_string(),
            Failed::Call { message, .. } => message,
        })
    }

    /// [`Eval::new`], whose expressions call typed functions through
    /// `functions`.
    pub(super) fn calling(
        of: &Rows,
        table: A,
        rows: usize,
        groups: Option<Arc<Groups>>,
        threads: usize,
        functions: &dyn Functions,
    ) -> Result<Self, Failed> {
        let columns = of
            .cells
            .iter()
            .map(|c| Self::column(c, table.member(&c.name)))
            .collect();
        let mut eval = Eval {
            columns,
            rows,
            groups,
            aggregates: Vec::with_capacity(of.aggregates),
            over_groups: Flag::new(false),
            threads,
            levels: RefCell::default(),
            met: RefCell::default(),
        };
        let mut found: Vec<(usize, Found<'_>)> = Vec::with_capacity(of.aggregates);
        aggregates(&of.invariant, &mut found);
        if let Some(w) = &of.when {
            aggregates(w, &mut found);
        }
        // One within the operand of another comes before it.
        found.sort_by_key(|(at, _)| *at);
        for (_, f) in found {
            let v = match f {
                Found::Aggregate(op, operands, skip) => {
                    eval.aggregate(op, operands, skip).map_err(Failed::Table)?
                }
                Found::Call(function, args, kind) => {
                    eval.call(function, args, kind, &of.cells, functions)?
                }
            };
            eval.aggregates.push(v);
        }
        Ok(eval)
    }

    /// The value of a call of `function` over all rows, or for each group:
    /// the cells of each argument of `vec(T)` in the rows of the group, and
    /// the constants and defaults of the others.
    fn call(
        &self,
        function: &QualifiedName,
        args: &[CallArg],
        kind: Kind,
        cells: &[Cell],
        functions: &dyn Functions,
    ) -> Result<Aggregated, Failed> {
        // The cells of each argument that is not one column, of every row.
        let mut data: Vec<Option<(Data, Option<String>)>> = Vec::with_capacity(args.len());
        for a in args {
            data.push(match &a.value {
                ArgValue::Cells {
                    expr: Expr::Cell(_),
                    ..
                } => None,
                ArgValue::Cells { expr, kind } => {
                    let mut out = calc::empty(*kind, self.rows);
                    let mut error = None;
                    for lo in (0..self.rows).step_by(BLOCK) {
                        let hi = (lo + BLOCK).min(self.rows);
                        let b = self.block(expr, lo, hi).map_err(Failed::Table)?;
                        calc::append(&mut out, &b, lo, hi - lo, &mut error);
                    }
                    if let Some(e) = error {
                        return Err(Failed::Row(e));
                    }
                    Some((out, first_zone(expr, cells)))
                }
                _ => None,
            });
        }
        let all: Vec<u32> = if self.groups.is_none() {
            (0..self.rows as u32).collect()
        } else {
            Vec::new()
        };
        let g = self.group_count();
        let rows_of = |k: usize| -> &[u32] {
            match &self.groups {
                Some(x) => x.rows_of(k),
                None => &all,
            }
        };
        let mut calls: Vec<calc::Call<'_>> = Vec::with_capacity(g);
        for k in 0..g {
            if k % 256 == 255 {
                crate::progress::tick_block();
            }
            let rows = rows_of(k);
            let given: calc::Call<'_> = args
                .iter()
                .zip(&data)
                .map(|(a, d)| {
                    let arg = match (&a.value, d) {
                        (ArgValue::Cells { .. }, Some((data, zone))) => Argument::Cells {
                            data,
                            rows,
                            zone: zone.as_deref(),
                        },
                        (
                            ArgValue::Cells {
                                expr: Expr::Cell(c),
                                ..
                            },
                            None,
                        ) => Argument::Column {
                            name: &cells[*c].name,
                            rows,
                        },
                        (ArgValue::Constant(c), _) => Argument::Constant(c),
                        (ArgValue::Default(d), _) => Argument::Default(d),
                        (ArgValue::Cells { .. }, None) => unreachable!("gathered above"),
                    };
                    (&*a.name, arg)
                })
                .collect();
            calls.push(given);
        }
        let got = functions.call(function, &calls).map_err(|f| Failed::Call {
            function: function.to_string(),
            group: f.at,
            message: f.message,
        })?;
        if got.len() != g {
            return Err(Failed::Call {
                function: function.to_string(),
                group: 0,
                message: format!("the host gives {} values for {g} calls", got.len()),
            });
        }
        let mut values = CallValues::new(kind, g);
        for (k, v) in got.into_iter().enumerate() {
            values.set(k, v).map_err(|message| Failed::Call {
                function: function.to_string(),
                group: k,
                message,
            })?;
        }
        Ok(values.into_aggregated())
    }

    /// The number of groups: 1 for rows without groups.
    pub(super) fn group_count(&self) -> usize {
        self.groups.as_ref().map_or(1, |g| g.count)
    }

    /// Evaluates blocks whose rows are groups, or rows of the table.
    pub(super) fn set_over_groups(&self, on: bool) {
        self.over_groups.set(on);
    }

    /// The text of the row `r` of the column `k` of strings, `None` where it
    /// is missing.
    pub(super) fn text_at(&self, k: usize, r: usize) -> Option<String> {
        let Column::Character(a) = &self.columns[k] else {
            return None;
        };
        let mut out = None;
        a.each_character_in(r, r + 1, |_, t| {
            if let Ok(Some(t)) = t {
                out = Some(t.into_owned());
            }
        });
        out
    }

    /// The integers of the column `k`, where the host holds them as one
    /// array, `i32::MIN` missing.
    pub(super) fn integers(&self, k: usize) -> Option<&'a [i32]> {
        match &self.columns[k] {
            Column::Integer(_, Some(x)) if x.len() >= self.rows => Some(x),
            _ => None,
        }
    }

    /// The host's keys of the strings of the column `k` from the row `lo`, of
    /// which equal ones have equal texts, where the host has them.
    pub(super) fn text_keys(&self, k: usize, lo: usize, out: &mut [usize]) -> bool {
        match &self.columns[k] {
            Column::Character(a) => a.text_keys(lo, out),
            _ => false,
        }
    }

    fn column(c: &Cell, a: A) -> Column<'a, A> {
        if a.is_absent() {
            return Column::Absent;
        }
        match unwrap_option(&c.ty) {
            FieldType::Double(_) => Column::Double(a, a.f64_slice()),
            FieldType::Integer(_) => Column::Integer(a, a.i32_slice()),
            FieldType::Logical => Column::Logical(a),
            FieldType::Character(_) => Column::Character(a),
            FieldType::Date => Column::Date(a),
            FieldType::Posixct => Column::Datetime(a),
            FieldType::Factor { levels, ordered } => match a.factor(levels.as_deref(), *ordered) {
                Ok(f) => Column::Factor(f),
                Err(_) => Column::Absent,
            },
            _ => Column::Absent,
        }
    }

    /// What the rule is in each row of `lo..hi`: true where its `when` is
    /// false or missing.
    fn holds(&self, of: &Rows, lo: usize, hi: usize) -> Result<Vec<u8>, String> {
        let mut out = self.results(of, lo, hi)?;
        if of.when.is_some() {
            out.iter_mut().filter(|x| **x == NA).for_each(|x| *x = 1);
        }
        Ok(out)
    }

    /// What the rule is in each row of `lo..hi`: true where its `when` is
    /// false, and missing where that is.
    fn results(&self, of: &Rows, lo: usize, hi: usize) -> Result<Vec<u8>, String> {
        crate::progress::tick_block();
        let mut out = self.block(&of.invariant, lo, hi)?.logicals(hi - lo)?;
        if let Some(w) = &of.when {
            let when = self.block(w, lo, hi)?.logicals(hi - lo)?;
            for (x, w) in out.iter_mut().zip(when) {
                match w {
                    1 => {}
                    0 => *x = 1,
                    e => *x = e,
                }
            }
        }
        Ok(out)
    }

    /// The cells of column `k` in the rows `lo..hi`.
    pub(super) fn cells(&self, k: usize, lo: usize, hi: usize) -> Block<'_, 'a> {
        let n = hi - lo;
        match &self.columns[k] {
            Column::Absent => Block::Nothing(MISSING),
            Column::Double(_, Some(x)) if x.len() >= hi => {
                Block::Number(Numbers::Host(&x[lo..hi]), None)
            }
            Column::Double(a, _) => {
                let mut out = vec![NA_NUMBER; n];
                if a.f64_region(lo, &mut out) != Some(n) {
                    out.fill(NA_NUMBER);
                    a.each_double_in(lo, hi, |i, r| {
                        out[i - lo] = r.ok().flatten().unwrap_or(NA_NUMBER)
                    });
                }
                Block::Number(Numbers::Own(out), None)
            }
            Column::Integer(_, Some(x)) if x.len() >= hi => Block::Number(
                Numbers::Own(
                    x[lo..hi]
                        .iter()
                        .map(|&v| {
                            if v == i32::MIN {
                                NA_NUMBER
                            } else {
                                f64::from(v)
                            }
                        })
                        .collect(),
                ),
                None,
            ),
            Column::Integer(a, _) => {
                let mut out = vec![NA_NUMBER; n];
                a.each_integer_in(lo, hi, |i, r| {
                    out[i - lo] = r.ok().flatten().map_or(NA_NUMBER, |v| v as f64)
                });
                Block::Number(Numbers::Own(out), None)
            }
            Column::Logical(a) => {
                let mut out = vec![NA; n];
                a.each_logical_in(lo, hi, |i, r| {
                    out[i - lo] = r.ok().flatten().map_or(NA, u8::from)
                });
                Block::Bool(out)
            }
            Column::Character(a) => {
                let mut out: Vec<Cow<'a, str>> = Vec::with_capacity(n);
                let mut states: States = None;
                a.each_character_in(lo, hi, |i, r| match r {
                    Ok(Some(t)) => out.push(t),
                    _ => {
                        out.push(Cow::Borrowed(""));
                        states.get_or_insert_with(|| vec![0; n])[i - lo] = MISSING;
                    }
                });
                Block::Text(out, states)
            }
            Column::Date(a) | Column::Datetime(a) => {
                let is_date = matches!(self.columns[k], Column::Date(_));
                // Days, or seconds, where the host holds them as one array.
                if let Some(x) = a.f64_slice().and_then(|x| x.get(lo..hi)) {
                    let scale = if is_date { 1.0 } else { 1e6 };
                    let out = x
                        .iter()
                        .map(|&v| {
                            if v.is_nan() {
                                i64::MIN
                            } else {
                                (v * scale).round() as i64
                            }
                        })
                        .collect::<Vec<i64>>();
                    return instants(out, is_date);
                }
                let mut out = vec![0; n];
                let put = |i: usize, r: Read<Option<i64>>| {
                    out[i - lo] = r.ok().flatten().unwrap_or(i64::MIN);
                };
                if is_date {
                    a.each_date_in(lo, hi, put);
                } else {
                    a.each_datetime_in(lo, hi, put);
                }
                instants(out, is_date)
            }
            Column::Factor(f) => Block::Label(
                match f.codes.slice() {
                    Some(x) => Cow::Borrowed(&x[lo..hi]),
                    None => {
                        let mut out = vec![0; n];
                        f.codes.read(lo, &mut out);
                        Cow::Owned(out)
                    }
                },
                &f.levels,
            ),
        }
    }

    /// The value of an aggregate for each group, or for the table, and its
    /// state. An operand that reads no cell outside an aggregate has one
    /// value for each group, which counts once.
    fn aggregate(&self, op: Op, operands: &[Expr], skip: bool) -> Result<Aggregated, String> {
        let g = self.group_count();
        let groups = self.groups.clone();
        let group = |r: usize| groups.as_ref().map_or(0, |x| x.ids[r] as usize);
        if op == Op::N {
            let counts = match &groups {
                Some(x) => x.sizes().map(|n| n as f64).collect(),
                None => vec![self.rows as f64],
            };
            return Ok(Aggregated {
                values: Values::Numbers(counts),
                states: vec![0; g],
            });
        }
        // A column that the host holds as one array: a kernel over its rows.
        if matches!(op, Op::Sum | Op::Mean | Op::Min | Op::Max)
            && let [Expr::Cell(k)] = operands
            && let Some(x) = self.array(*k)
        {
            let ids = groups.as_ref().map(|g| &g.ids[..]);
            let part = parallel::column_states(op, x, ids, g, self.rows, skip, self.threads);
            return Ok(self.finish_column(op, part, x));
        }
        let gather = op == Op::Median;
        let summed = matches!(op, Op::Sum | Op::Mean);
        let least = op == Op::Min;
        let mut acc = Acc {
            op,
            skip,
            least,
            count: vec![0; g],
            sums: if summed {
                vec![PairSum::default(); g]
            } else {
                Vec::new()
            },
            extremes: vec![
                if least {
                    f64::INFINITY
                } else {
                    f64::NEG_INFINITY
                };
                g
            ],
            states: vec![0; g],
            by_row: if gather {
                vec![f64::NAN; self.rows]
            } else {
                Vec::new()
            },
        };
        let mut times: Option<(Vec<i64>, Instant)> = None;
        for e in operands {
            let once = !reads_cell_outside(e);
            let (n_rows, at_group): (usize, bool) =
                if once { (g, true) } else { (self.rows, false) };
            self.over_groups.set(at_group);
            for lo in (0..n_rows).step_by(BLOCK) {
                crate::progress::tick_block();
                let hi = (lo + BLOCK).min(n_rows);
                let n = hi - lo;
                let b = self.block(e, lo, hi);
                let b = match b {
                    Ok(b) => b,
                    Err(m) => {
                        self.over_groups.set(false);
                        return Err(m);
                    }
                };
                let key = |i: usize| if at_group { lo + i } else { group(lo + i) };
                if op == Op::Count {
                    let x = b.logicals(n)?;
                    for (i, &v) in x.iter().enumerate() {
                        match v {
                            1 => acc.push(key(i), 0.0, None),
                            0 => {}
                            v => acc.note(key(i), v - 1),
                        }
                    }
                    continue;
                }
                if let Some(of) = b.instant() {
                    let (ts, _) = times.get_or_insert_with(|| {
                        (vec![if least { i64::MAX } else { i64::MIN }; g], of)
                    });
                    for i in 0..n {
                        match b.time(i) {
                            Ok(t) => {
                                let k = key(i);
                                acc.count[k] += 1;
                                ts[k] = if least { ts[k].min(t) } else { ts[k].max(t) };
                            }
                            Err(s) => acc.note(key(i), s),
                        }
                    }
                    continue;
                }
                let (x, st) = b.numbers(n)?;
                for i in 0..n {
                    let k = key(i);
                    let s = st.as_ref().map_or(0, |s| s[i]);
                    let v = match x {
                        Operand::Many(xs) => xs[i],
                        Operand::One(v) => v,
                    };
                    if s != 0 {
                        acc.note(k, s);
                    } else if v.is_nan() {
                        acc.note(k, MISSING);
                    } else {
                        acc.push(k, v, (!at_group).then_some(lo + i));
                    }
                }
            }
            self.over_groups.set(false);
        }
        let Acc {
            count,
            sums,
            extremes,
            mut states,
            by_row,
            ..
        } = acc;
        let missing_where_none = |values: &mut Vec<f64>, states: &mut Vec<u8>| {
            for k in 0..g {
                if count[k] == 0 && states[k] == 0 {
                    states[k] = MISSING;
                    values[k] = NA_NUMBER;
                }
            }
        };
        if let Some((mut ts, of)) = times {
            for k in 0..g {
                if count[k] == 0 && states[k] == 0 {
                    states[k] = MISSING;
                }
                if states[k] != 0 {
                    ts[k] = i64::MIN;
                }
            }
            return Ok(Aggregated {
                values: Values::Times(ts, of),
                states,
            });
        }
        let mut values: Vec<f64> = match op {
            Op::Count => count.iter().map(|&c| c as f64).collect(),
            Op::Min | Op::Max => {
                let mut v = extremes;
                missing_where_none(&mut v, &mut states);
                v
            }
            Op::Sum | Op::Mean => {
                let mut v = vec![0.0; g];
                // The groups whose bound does not decide the rounding, each
                // with its exact sum.
                let mut exact: Vec<u32> = vec![u32::MAX; g];
                let mut again: Vec<ExactSum> = Vec::new();
                for k in 0..g {
                    match sums[k].value() {
                        Some(x) => v[k] = x,
                        None if states[k] == 0 => {
                            exact[k] = again.len() as u32;
                            again.push(ExactSum::default());
                        }
                        None => {}
                    }
                }
                if !again.is_empty() {
                    self.each_number(&operands[0], |k, x| {
                        if exact[k] != u32::MAX {
                            again[exact[k] as usize].add(x);
                        }
                    })?;
                    for k in 0..g {
                        if exact[k] != u32::MAX {
                            v[k] = again[exact[k] as usize].value();
                        }
                    }
                }
                if op == Op::Mean {
                    for k in 0..g {
                        v[k] /= count[k] as f64;
                    }
                    missing_where_none(&mut v, &mut states);
                }
                v
            }
            _ => {
                // The median: the rows of each group, gathered in order.
                let rows_of = |k: usize| -> &[u32] {
                    match &groups {
                        Some(x) => x.rows_of(k),
                        None => &[],
                    }
                };
                let whole = groups.is_none();
                let all: Vec<u32> = if whole {
                    (0..self.rows as u32).collect()
                } else {
                    Vec::new()
                };
                let mut v = vec![0.0; g];
                let mut cells: Vec<f64> = Vec::new();
                for k in 0..g {
                    if states[k] != 0 {
                        continue;
                    }
                    let rows = if whole { &all[..] } else { rows_of(k) };
                    cells.clear();
                    cells.extend(
                        rows.iter()
                            .map(|&r| by_row[r as usize])
                            .filter(|x| !x.is_nan()),
                    );
                    v[k] = if cells.is_empty() {
                        NA_NUMBER
                    } else {
                        median(std::mem::take(&mut cells))
                    };
                }
                missing_where_none(&mut v, &mut states);
                v
            }
        };
        for k in 0..g {
            if states[k] != 0 {
                values[k] = NA_NUMBER;
            }
        }
        Ok(Aggregated {
            values: Values::Numbers(values),
            states,
        })
    }

    /// The cells of `e` in the rows `lo..hi`.
    pub(super) fn block<'e>(
        &'e self,
        e: &'e Expr,
        lo: usize,
        hi: usize,
    ) -> Result<Block<'e, 'a>, String> {
        let n = hi - lo;
        match e {
            Expr::Lit(Scalar::Null) => Ok(Block::Nothing(MISSING)),
            Expr::Lit(s) => Ok(Block::One(s.clone())),
            Expr::Cell(k) => Ok(self.cells(*k, lo, hi)),
            Expr::Aggregate { at, .. } | Expr::Call { at, .. } => {
                Ok(self.aggregated(&self.aggregates[*at], lo, hi))
            }
            Expr::And(parts) | Expr::Or(parts) => {
                let decides = u8::from(matches!(e, Expr::Or(_)));
                let mut out = vec![1 - decides; n];
                for p in parts {
                    let x = self.block(p, lo, hi)?.logicals(n)?;
                    for (o, x) in out.iter_mut().zip(x) {
                        *o = if *o == decides || x == decides {
                            decides
                        } else {
                            (*o).max(x)
                        };
                    }
                }
                Ok(Block::Bool(out))
            }
            Expr::IsNa(inner) => Ok(Block::Bool(match self.block(inner, lo, hi)? {
                Block::Bool(x) => x
                    .into_iter()
                    .map(|v| if v > NA { v } else { u8::from(v == NA) })
                    .collect(),
                Block::Label(codes, levels) => codes
                    .iter()
                    .map(|&c| u8::from(label(levels, c).is_none()))
                    .collect(),
                b => {
                    let mut out = match &b {
                        Block::Number(x, _) => x
                            .all()
                            .iter()
                            .map(|x| u8::from(x.is_nan()))
                            .collect::<Vec<u8>>(),
                        Block::One(Scalar::Num(x)) => vec![u8::from(x.is_nan()); n],
                        Block::Nothing(MISSING) => vec![1; n],
                        _ => vec![0; n],
                    };
                    let states = match b {
                        Block::Number(_, s) | Block::Time(_, s, _) | Block::Text(_, s) => s,
                        Block::Nothing(s) if s > MISSING => Some(vec![s; n]),
                        _ => None,
                    };
                    for (o, s) in out.iter_mut().zip(states.into_iter().flatten()) {
                        match s {
                            0 => {}
                            MISSING => *o = 1,
                            s => *o = stated(s),
                        }
                    }
                    out
                }
            })),
            Expr::If(parts) => self.choice(parts, lo, hi),
            Expr::Op(op, args) => self.operation(e, *op, args, lo, hi),
            _ => Err("the expression reads a whole field".to_string()),
        }
    }

    /// The numbers of the column `k`, where the host holds them as one array of
    /// the table's rows.
    fn array(&self, k: usize) -> Option<Cells<'a>> {
        match &self.columns[k] {
            Column::Double(_, Some(x)) if x.len() >= self.rows => Some(Cells::Doubles(x)),
            Column::Integer(_, Some(x)) if x.len() >= self.rows => Some(Cells::Integers(x)),
            _ => None,
        }
    }

    /// The values of an aggregate of a column from its states: a sum that the
    /// bound of its pair does not decide is summed exactly, in a pass of its
    /// own.
    fn finish_column(&self, op: Op, part: Part, x: Cells<'a>) -> Aggregated {
        let g = part.count.len();
        let mut states: Vec<u8> = part
            .missing
            .iter()
            .map(|&m| u8::from(m) * MISSING)
            .collect();
        let mut values = vec![NA_NUMBER; g];
        match op {
            Op::Sum | Op::Mean => {
                let mut exact: Vec<u32> = vec![u32::MAX; g];
                let mut again: Vec<ExactSum> = Vec::new();
                for k in (0..g).filter(|&k| states[k] == 0) {
                    match part.sums[k].value() {
                        Some(v) => values[k] = v,
                        None => {
                            exact[k] = again.len() as u32;
                            again.push(ExactSum::default());
                        }
                    }
                }
                if !again.is_empty() {
                    let ids = self.groups.as_ref().map(|g| &g.ids[..]);
                    for r in 0..self.rows {
                        let k = ids.map_or(0, |ids| ids[r] as usize);
                        let v = x.at(r);
                        if exact[k] != u32::MAX && !v.is_nan() {
                            again[exact[k] as usize].add(v);
                        }
                    }
                    for k in 0..g {
                        if exact[k] != u32::MAX {
                            values[k] = again[exact[k] as usize].value();
                        }
                    }
                }
                if op == Op::Mean {
                    for k in 0..g {
                        if part.count[k] == 0 && states[k] == 0 {
                            states[k] = MISSING;
                        }
                        values[k] /= part.count[k] as f64;
                    }
                }
            }
            _ => {
                for k in 0..g {
                    if part.count[k] == 0 && states[k] == 0 {
                        states[k] = MISSING;
                    }
                    values[k] = part.extremes[k];
                }
            }
        }
        for k in 0..g {
            if states[k] != 0 || values[k].is_nan() {
                values[k] = NA_NUMBER;
                if states[k] == 0 {
                    states[k] = MISSING;
                }
            }
        }
        Aggregated {
            values: Values::Numbers(values),
            states,
        }
    }

    /// Calls `f` with the group and the number of each row of `e` that has
    /// one.
    fn each_number(&self, e: &Expr, mut f: impl FnMut(usize, f64)) -> Result<(), String> {
        for lo in (0..self.rows).step_by(BLOCK) {
            crate::progress::tick_block();
            let hi = (lo + BLOCK).min(self.rows);
            let b = self.block(e, lo, hi)?;
            let (x, st) = b.numbers(hi - lo)?;
            for i in 0..hi - lo {
                let v = match x {
                    Operand::Many(xs) => xs[i],
                    Operand::One(v) => v,
                };
                if st.as_ref().is_none_or(|s| s[i] == 0) && !v.is_nan() {
                    f(
                        self.groups.as_ref().map_or(0, |g| g.ids[lo + i] as usize),
                        v,
                    );
                }
            }
        }
        Ok(())
    }

    /// The cells of an aggregate in the rows `lo..hi`: its one value for rows
    /// without groups, the value of each row's group, or, over groups, the
    /// value of each group.
    fn aggregated<'e>(&'e self, a: &'e Aggregated, lo: usize, hi: usize) -> Block<'e, 'a> {
        let states = |at: &mut dyn Iterator<Item = usize>| -> States {
            let s: Vec<u8> = at.map(|k| a.states[k]).collect();
            s.iter().any(|&x| x != 0).then_some(s)
        };
        let over = self.over_groups.get();
        match (&self.groups, over) {
            (None, false) => match (&a.values, a.states[0]) {
                (_, s) if s != 0 => Block::Nothing(s),
                (Values::Numbers(v), _) => Block::One(Scalar::Num(v[0])),
                (Values::Times(t, of), _) => Block::One(of.scalar(t[0])),
                (Values::Logicals(v), _) => Block::One(Scalar::Bool(v[0])),
                (Values::Texts(v), _) => Block::One(Scalar::Str(v[0].clone())),
            },
            (_, true) => values_of(a, lo..hi, states(&mut (lo..hi))),
            (Some(g), false) => {
                let ids = &g.ids[lo..hi];
                let st = states(&mut ids.iter().map(|&k| k as usize));
                values_of(a, ids.iter().map(|&k| k as usize), st)
            }
        }
    }

    /// `if`: in each row the value of the first condition that is true.
    fn choice<'e>(
        &'e self,
        parts: &'e [Expr],
        lo: usize,
        hi: usize,
    ) -> Result<Block<'e, 'a>, String> {
        let n = hi - lo;
        // The branch of each row, or the state of the condition that has none.
        let last = parts.len() / 2;
        let mut branch = vec![last; n];
        let mut state = vec![0u8; n];
        let mut open = vec![true; n];
        let mut values = Vec::with_capacity(last + 1);
        let mut i = 0;
        while i + 1 < parts.len() {
            let c = self.block(&parts[i], lo, hi)?.logicals(n)?;
            for k in 0..n {
                if open[k] && c[k] != 0 {
                    open[k] = false;
                    if c[k] == 1 {
                        branch[k] = i / 2;
                    } else {
                        state[k] = c[k] - 1;
                    }
                }
            }
            values.push(self.block(&parts[i + 1], lo, hi)?);
            i += 2;
        }
        values.push(self.block(&parts[i], lo, hi)?);
        let bools = |b: &Block| matches!(b, Block::Bool(_) | Block::One(Scalar::Bool(_)));
        let any = |f: &dyn Fn(&Block) -> bool| values.iter().any(f);
        let of = |k: usize| &values[branch[k]];
        if any(&|b| b.is_number()) {
            let mut out = vec![f64::NAN; n];
            let parts = values
                .iter()
                .map(|b| b.numbers(n))
                .collect::<Result<Vec<_>, _>>()?;
            for k in (0..n).filter(|&k| state[k] == 0).collect::<Vec<_>>() {
                let (x, s) = &parts[branch[k]];
                out[k] = match x {
                    Operand::Many(x) => x[k],
                    Operand::One(x) => *x,
                };
                state[k] = s.as_ref().map_or(0, |s| s[k]);
            }
            return Ok(Block::Number(Numbers::Own(out), Some(state)));
        }
        if any(&|b| b.is_text()) {
            let mut out = vec![Cow::Borrowed(""); n];
            for k in (0..n).filter(|&k| state[k] == 0).collect::<Vec<_>>() {
                match of(k).text(k) {
                    Ok(t) => out[k] = Cow::Owned(t.to_string()),
                    Err(s) => state[k] = s,
                }
            }
            return Ok(Block::Text(out, Some(state)));
        }
        if let Some(kind) = values.iter().find_map(Block::instant) {
            let mut out = vec![0; n];
            for k in (0..n).filter(|&k| state[k] == 0).collect::<Vec<_>>() {
                match of(k).time(k) {
                    Ok(t) => out[k] = t,
                    Err(s) => state[k] = s,
                }
            }
            return Ok(Block::Time(out, Some(state), kind));
        }
        if values
            .iter()
            .all(|b| bools(b) || matches!(b, Block::Nothing(_)))
        {
            let values = values
                .into_iter()
                .map(|b| b.logicals(n))
                .collect::<Result<Vec<_>, _>>()?;
            return Ok(Block::Bool(
                (0..n)
                    .map(|k| match state[k] {
                        0 => values[branch[k]][k],
                        s => stated(s),
                    })
                    .collect(),
            ));
        }
        Err("the values of an `if` are of one kind".to_string())
    }

    fn operation<'e>(
        &'e self,
        e: &'e Expr,
        op: Op,
        args: &'e [Expr],
        lo: usize,
        hi: usize,
    ) -> Result<Block<'e, 'a>, String> {
        let n = hi - lo;
        let one = |k: usize| self.block(&args[k], lo, hi);
        match op {
            Op::Not => Ok(Block::Bool(
                one(0)?
                    .logicals(n)?
                    .into_iter()
                    .map(|v| v ^ u8::from(v < NA))
                    .collect(),
            )),
            Op::Eq | Op::Ne | Op::Lt | Op::Le | Op::Gt | Op::Ge => {
                if let ([Expr::Cell(k), Expr::Lit(Scalar::Str(t))], Op::Eq | Op::Ne)
                | ([Expr::Lit(Scalar::Str(t)), Expr::Cell(k)], Op::Eq | Op::Ne) = (args, op)
                    && let Some(x) = self.by_key(e, *k, lo, hi, |x| (x == t) == (op == Op::Eq))
                {
                    return Ok(Block::Bool(x));
                }
                self.compare(e, op, &one(0)?, &one(1)?, n)
            }
            Op::In => {
                if let [Expr::Cell(k), Expr::Lit(Scalar::Array(xs))] = args
                    && let Some(x) = self.by_key(e, *k, lo, hi, |x| {
                        xs.iter().any(|y| matches!(y, Scalar::Str(y) if y == x))
                    })
                {
                    return Ok(Block::Bool(x));
                }
                let needle = one(0)?;
                match &args[1] {
                    Expr::Lit(Scalar::Array(xs)) => self.member(e, &needle, xs, n),
                    hay => {
                        let hay = self.block(hay, lo, hi)?;
                        Ok(Block::Bool(
                            (0..n)
                                .map(|i| match (needle.text(i), hay.text(i)) {
                                    (Ok(a), Ok(b)) => u8::from(b.contains(a)),
                                    (a, b) => {
                                        stated(a.err().unwrap_or(0).max(b.err().unwrap_or(0)))
                                    }
                                })
                                .collect(),
                        ))
                    }
                }
            }
            Op::Nchar => {
                let x = one(0)?;
                let mut out = vec![f64::NAN; n];
                let mut states: States = None;
                for (i, o) in out.iter_mut().enumerate() {
                    match x.text(i) {
                        Ok(t) => *o = t.chars().count() as f64,
                        Err(s) => states.get_or_insert_with(|| vec![0; n])[i] = s,
                    }
                }
                Ok(Block::Number(Numbers::Own(out), states))
            }
            Op::Cat => {
                let parts = (0..args.len()).map(one).collect::<Result<Vec<_>, _>>()?;
                let mut out = vec![String::new(); n];
                let mut states: States = None;
                for b in &parts {
                    for (i, o) in out.iter_mut().enumerate() {
                        if let Err(s) = append(o, b, i) {
                            let s0 = &mut states.get_or_insert_with(|| vec![0; n])[i];
                            *s0 = (*s0).max(s);
                        }
                    }
                }
                Ok(Block::Text(
                    out.into_iter().map(Cow::Owned).collect(),
                    states,
                ))
            }
            Op::Pow => {
                let (a, b) = (one(0)?, one(1)?);
                let ((x, s), (y, t)) = (a.numbers(n)?, b.numbers(n)?);
                let mut states = merge(s, t);
                // A square is one product, without an error.
                if let Operand::One(2.0) = y {
                    return Ok(Block::Number(Numbers::Own(x.each(n, |v| v * v)), states));
                }
                let out = x.with(y, n, |x, y| power(x, y).unwrap_or(f64::NAN));
                let failed = x.with(y, n, |x, y| match power(x, y) {
                    Err(e) if e == ZERO => ZERO_STATE,
                    Err(_) => POWER_STATE,
                    Ok(_) => 0,
                });
                if failed.iter().any(|&f| f != 0) {
                    let s = states.get_or_insert_with(|| vec![0; n]);
                    for (s, f) in s.iter_mut().zip(&failed) {
                        if *f != 0 && *s <= MISSING {
                            *s = *f;
                        }
                    }
                }
                Ok(Block::Number(Numbers::Own(out), states))
            }
            Op::Add | Op::Sub | Op::Mul | Op::Div | Op::Mod | Op::Min | Op::Max
                if args.len() > 1 =>
            {
                let first = one(0)?;
                let (x, mut states) = first.numbers(n)?;
                let mut acc = match x {
                    Operand::Many(x) => Cow::Borrowed(x),
                    Operand::One(x) => Cow::Owned(vec![x; n]),
                };
                for k in 1..args.len() {
                    let b = one(k)?;
                    let (y, s) = b.numbers(n)?;
                    states = merge(states, s);
                    let a = Operand::Many(&acc);
                    let next = match op {
                        Op::Add => a.with(y, n, |a, b| a + b),
                        Op::Sub => a.with(y, n, |a, b| a - b),
                        Op::Mul => a.with(y, n, |a, b| a * b),
                        Op::Min => a.with(y, n, |a, b| extreme(true, a, b)),
                        Op::Max => a.with(y, n, |a, b| extreme(false, a, b)),
                        _ => {
                            // A zero divisor is rare: its cells are marked once
                            // one is found.
                            let any = match y {
                                Operand::Many(ys) => ys.contains(&0.0),
                                Operand::One(b) => b == 0.0,
                            };
                            if any {
                                let zero = a.with(y, n, |a, b| b == 0.0 && !a.is_nan());
                                states = mark(states, &zero, ZERO_STATE);
                            }
                            if op == Op::Div {
                                a.with(y, n, |a, b| a / b)
                            } else {
                                a.with(y, n, |a, b| a % b)
                            }
                        }
                    };
                    acc = Cow::Owned(next);
                }
                Ok(Block::Number(Numbers::Own(acc.into_owned()), states))
            }
            Op::Sub
            | Op::Add
            | Op::Mul
            | Op::Floor
            | Op::Ceiling
            | Op::Round
            | Op::Abs
            | Op::Sqrt
            | Op::Log
            | Op::Exp => {
                let b = one(0)?;
                let (x, mut states) = b.numbers(n)?;
                let out = match op {
                    Op::Sub => x.each(n, |x| -x),
                    Op::Floor => x.each(n, f64::floor),
                    Op::Ceiling => x.each(n, f64::ceil),
                    Op::Round => x.each(n, f64::round_ties_even),
                    Op::Abs => x.each(n, f64::abs),
                    Op::Sqrt => {
                        states = mark(states, &x.each(n, |x| x < 0.0), ROOT_STATE);
                        x.each(n, f64::sqrt)
                    }
                    Op::Log => {
                        states = mark(states, &x.each(n, |x| x <= 0.0), LOG_STATE);
                        x.each(n, f64::ln)
                    }
                    Op::Exp => x.each(n, f64::exp),
                    _ => x.each(n, |x| x),
                };
                Ok(Block::Number(Numbers::Own(out), states))
            }
            _ => Err("the operator reads a whole field".to_string()),
        }
    }

    /// What each level of a factor is under `f`, once for the evaluation.
    fn by_level(
        &self,
        e: &Expr,
        levels: &[Cow<'a, str>],
        codes: &[i32],
        f: impl Fn(&str) -> bool,
    ) -> Vec<u8> {
        let mut held = self.levels.borrow_mut();
        let of = held
            .entry(std::ptr::from_ref(e) as usize)
            .or_insert_with(|| levels.iter().map(|l| u8::from(f(l))).collect());
        codes
            .iter()
            .map(|&c| {
                usize::try_from(c)
                    .ok()
                    .and_then(|c| of.get(c.wrapping_sub(1)).copied())
                    .unwrap_or(NA)
            })
            .collect()
    }

    /// What the strings of the column `k` are under `f`, for a host that has
    /// keys of them: each text is read once, while its place holds it.
    fn by_key(
        &self,
        e: &Expr,
        k: usize,
        lo: usize,
        hi: usize,
        f: impl Fn(&str) -> bool,
    ) -> Option<Vec<u8>> {
        let Column::Character(a) = &self.columns[k] else {
            return None;
        };
        let mut keys = vec![0usize; hi - lo];
        if !a.text_keys(lo, &mut keys) {
            return None;
        }
        let mut held = self.met.borrow_mut();
        let met = held
            .entry(std::ptr::from_ref(e) as usize)
            .or_insert_with(|| vec![(0, NA); MET]);
        Some(
            keys.iter()
                .enumerate()
                .map(|(i, &key)| {
                    // A key is an address, whose low bits are zero.
                    let place = (key >> 4).wrapping_mul(0x9e37_79b9) >> 8;
                    let at = &mut met[place % MET];
                    if at.0 != key {
                        let mut found = NA;
                        a.each_character_in(lo + i, lo + i + 1, |_, r| {
                            if let Ok(Some(t)) = r {
                                found = u8::from(f(&t));
                            }
                        });
                        *at = (key, found);
                    }
                    at.1
                })
                .collect(),
        )
    }

    fn compare<'e>(
        &'e self,
        e: &Expr,
        op: Op,
        a: &Block<'e, 'a>,
        b: &Block<'e, 'a>,
        n: usize,
    ) -> Result<Block<'e, 'a>, String> {
        use std::cmp::Ordering;
        let ordered = |o: Ordering| match op {
            Op::Eq => o == Ordering::Equal,
            Op::Ne => o != Ordering::Equal,
            Op::Lt => o == Ordering::Less,
            Op::Le => o != Ordering::Greater,
            Op::Gt => o == Ordering::Greater,
            _ => o != Ordering::Less,
        };
        let cell = |x: Result<bool, u8>| match x {
            Ok(b) => u8::from(b),
            Err(s) => stated(s),
        };
        let both = |x: Result<bool, u8>, y: Option<u8>| match (x, y) {
            (Err(s), Some(t)) => Err(s.max(t)),
            (x, _) => x,
        };
        if a.is_number() || b.is_number() {
            let ((x, s), (y, t)) = (a.numbers(n)?, b.numbers(n)?);
            let out = match op {
                Op::Eq => compared(x, y, n, |x, y| x == y),
                Op::Ne => compared(x, y, n, |x, y| x != y),
                Op::Lt => compared(x, y, n, |x, y| x < y),
                Op::Le => compared(x, y, n, |x, y| x <= y),
                Op::Gt => compared(x, y, n, |x, y| x > y),
                _ => compared(x, y, n, |x, y| x >= y),
            };
            return Ok(Block::Bool(overlay(out, merge(s, t))));
        }
        if let (Block::Time(x, s, _), Some(y)) = (a, b.one_time()) {
            let out = x.iter().map(|x| u8::from(ordered(x.cmp(&y)))).collect();
            return Ok(Block::Bool(overlay(out, s.clone())));
        }
        if a.is_time() || b.is_time() {
            return Ok(Block::Bool(
                (0..n)
                    .map(|i| {
                        let (x, y) = (a.time(i), b.time(i));
                        let v = x.and_then(|x| y.map(|y| ordered(x.cmp(&y))));
                        cell(both(v, y.err()))
                    })
                    .collect(),
            ));
        }
        if a.is_text() || b.is_text() {
            match (a, b) {
                (Block::Label(codes, levels), Block::One(Scalar::Str(t)))
                | (Block::One(Scalar::Str(t)), Block::Label(codes, levels)) => {
                    return Ok(Block::Bool(
                        self.by_level(e, levels, codes, |l| (l == t) == (op == Op::Eq)),
                    ));
                }
                (Block::Text(x, s), Block::One(Scalar::Str(t)))
                | (Block::One(Scalar::Str(t)), Block::Text(x, s)) => {
                    let out = x
                        .iter()
                        .map(|x| u8::from((**x == **t) == (op == Op::Eq)))
                        .collect();
                    return Ok(Block::Bool(overlay(out, s.clone())));
                }
                _ => {}
            }
            return Ok(Block::Bool(
                (0..n)
                    .map(|i| {
                        let (x, y) = (a.text(i), b.text(i));
                        let v = x.and_then(|x| y.map(|y| ordered(x.cmp(y))));
                        cell(both(v, y.err()))
                    })
                    .collect(),
            ));
        }
        let logicals = |b: &Block<'e, 'a>| match b {
            Block::Bool(x) => Ok(x.clone()),
            Block::One(s) => Block::One(s.clone()).logicals(n),
            Block::Nothing(s) => Block::Nothing(*s).logicals(n),
            _ => Err("the operands are of one kind".to_string()),
        };
        let (x, y) = (logicals(a)?, logicals(b)?);
        Ok(Block::Bool(
            x.into_iter()
                .zip(y)
                .map(|(x, y)| {
                    if x >= NA || y >= NA {
                        // A logical is below every state.
                        x.max(y)
                    } else {
                        u8::from((x == y) == (op == Op::Eq))
                    }
                })
                .collect(),
        ))
    }

    /// Whether each cell is one of the literals `xs`.
    fn member<'e>(
        &'e self,
        e: &'e Expr,
        needle: &Block<'e, 'a>,
        xs: &'e [Scalar],
        n: usize,
    ) -> Result<Block<'e, 'a>, String> {
        let set = Set::of(xs);
        if let Block::Label(codes, levels) = needle {
            return Ok(Block::Bool(self.by_level(
                e,
                levels,
                codes,
                |l| matches!(&set, Set::Texts(t) if t.contains(l)),
            )));
        }
        if needle.is_number() || matches!(needle, Block::Nothing(_)) {
            let (x, s) = needle.numbers(n)?;
            let out = x.each(n, |x| {
                if x.is_nan() {
                    NA
                } else {
                    u8::from(set.number(x))
                }
            });
            return Ok(Block::Bool(overlay(out, s)));
        }
        if needle.is_time() {
            return Ok(Block::Bool(
                (0..n)
                    .map(|i| match (needle.time(i), &set) {
                        (Ok(t), Set::Times(ts)) => u8::from(ts.contains(&t)),
                        (Ok(_), _) => 0,
                        (Err(s), _) => stated(s),
                    })
                    .collect(),
            ));
        }
        if needle.is_text() {
            return Ok(Block::Bool(
                (0..n)
                    .map(|i| match (needle.text(i), &set) {
                        (Ok(t), Set::Texts(ts)) => u8::from(ts.contains(t)),
                        (Ok(_), _) => 0,
                        (Err(s), _) => stated(s),
                    })
                    .collect(),
            ));
        }
        match needle {
            Block::Bool(x) => Ok(Block::Bool(
                x.iter()
                    .map(|&v| match (v, &set) {
                        (0 | 1, Set::Bools(b)) => u8::from(b[v as usize]),
                        (0 | 1, _) => 0,
                        (v, _) => v,
                    })
                    .collect(),
            )),
            _ => Err("`in` takes a value".to_string()),
        }
    }
}

/// A block of instants, where the least `i64` is missing.
fn instants<'e, 'a>(x: Vec<i64>, is_date: bool) -> Block<'e, 'a> {
    let states = x.contains(&i64::MIN).then(|| {
        x.iter()
            .map(|&t| u8::from(t == i64::MIN) * MISSING)
            .collect()
    });
    let of = if is_date {
        Instant::Date
    } else {
        Instant::DateTime
    };
    Block::Time(x, states, of)
}

/// The block of the values of `a` at the places `at`, with their states.
fn values_of<'e, 'a>(
    a: &'e Aggregated,
    at: impl Iterator<Item = usize>,
    st: States,
) -> Block<'e, 'a> {
    match &a.values {
        Values::Numbers(v) => Block::Number(Numbers::Own(at.map(|k| v[k]).collect()), st),
        Values::Times(t, of) => Block::Time(at.map(|k| t[k]).collect(), st, *of),
        Values::Logicals(v) => Block::Bool(
            at.map(|k| match a.states[k] {
                0 => u8::from(v[k]),
                s => stated(s),
            })
            .collect(),
        ),
        Values::Texts(v) => Block::Text(at.map(|k| Cow::Owned(v[k].clone())).collect(), st),
    }
}

/// The values of a call for each group, as they come.
struct CallValues {
    kind: Kind,
    values: Values,
    states: Vec<u8>,
}

impl CallValues {
    fn new(kind: Kind, n: usize) -> CallValues {
        let values = match kind {
            Kind::Logical => Values::Logicals(vec![false; n]),
            Kind::Integer | Kind::Double => Values::Numbers(vec![NA_NUMBER; n]),
            Kind::Character => Values::Texts(vec![String::new(); n]),
            Kind::Date => Values::Times(vec![i64::MIN; n], Instant::Date),
            Kind::DateTime => Values::Times(vec![i64::MIN; n], Instant::DateTime),
        };
        CallValues {
            kind,
            values,
            states: vec![MISSING; n],
        }
    }

    /// The value of group `k`; the error of a value of another kind.
    fn set(&mut self, k: usize, v: CallValue) -> Result<(), String> {
        match (&mut self.values, v) {
            (_, CallValue::Missing) => return Ok(()),
            (Values::Numbers(x), CallValue::Number(n)) if !n.is_nan() => x[k] = n,
            (Values::Numbers(_), CallValue::Number(_)) => return Ok(()),
            (Values::Logicals(x), CallValue::Logical(b)) => x[k] = b,
            (Values::Texts(x), CallValue::Text(t)) => x[k] = t,
            (Values::Times(x, Instant::Date), CallValue::Date(d)) => x[k] = d,
            (Values::Times(x, Instant::DateTime), CallValue::DateTime(t)) => x[k] = t,
            (_, v) => {
                return Err(format!(
                    "the function gives {v:?}, and its declared return type is of {:?}",
                    self.kind
                ));
            }
        }
        self.states[k] = 0;
        Ok(())
    }

    fn into_aggregated(self) -> Aggregated {
        Aggregated {
            values: self.values,
            states: self.states,
        }
    }
}

/// The first date-time column that `e` reads.
fn first_zone(e: &Expr, cells: &[Cell]) -> Option<String> {
    let mut found = None;
    visit_cells(e, &mut |k| {
        if found.is_none() && matches!(unwrap_option(&cells[k].ty), FieldType::Posixct) {
            found = Some(cells[k].name.to_string());
        }
    });
    found
}

/// Calls `f` with each cell that `e` reads outside calls.
fn visit_cells(e: &Expr, f: &mut dyn FnMut(usize)) {
    match e {
        Expr::Cell(k) => f(*k),
        Expr::Aggregate { operands: xs, .. }
        | Expr::If(xs)
        | Expr::And(xs)
        | Expr::Or(xs)
        | Expr::Op(_, xs) => xs.iter().for_each(|x| visit_cells(x, f)),
        Expr::IsNa(x) | Expr::IsNull(x) => visit_cells(x, f),
        _ => {}
    }
}

/// The aggregates and calls of `e`, with their places.
fn aggregates<'e>(e: &'e Expr, out: &mut Vec<(usize, Found<'e>)>) {
    match e {
        Expr::Aggregate {
            op,
            operands,
            at,
            skip,
        } => {
            operands.iter().for_each(|x| aggregates(x, out));
            out.push((*at, Found::Aggregate(*op, operands, *skip)));
        }
        Expr::Call {
            function,
            args,
            kind,
            at,
        } => {
            for a in args {
                if let ArgValue::Cells { expr, .. } = &a.value {
                    aggregates(expr, out);
                }
            }
            out.push((*at, Found::Call(function, args, *kind)));
        }
        Expr::If(xs) | Expr::And(xs) | Expr::Or(xs) | Expr::Op(_, xs) => {
            xs.iter().for_each(|x| aggregates(x, out));
        }
        Expr::IsNa(x) | Expr::IsNull(x) => aggregates(x, out),
        _ => {}
    }
}

/// Appends cell `i` of an operand of `cat`, as text.
fn append(to: &mut String, b: &Block<'_, '_>, i: usize) -> Result<(), u8> {
    let x = match b {
        Block::Number(x, s) => match s.as_ref().map_or(0, |s| s[i]) {
            0 => x.all()[i],
            s => return Err(s),
        },
        Block::One(Scalar::Num(x)) => *x,
        _ => {
            to.push_str(b.text(i)?);
            return Ok(());
        }
    };
    if x.is_nan() {
        return Err(MISSING);
    }
    to.push_str(&number_text(x));
    Ok(())
}
