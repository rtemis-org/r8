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
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};

use serde_json::Value;

use super::compile::{Cell, Expr, Op, RowScope, Rows, RuleError, compile_rows};
use super::diagnose::{Diagnosis, comparison, shown_scalar};
use super::eval::{LOGARITHM, ROOT, Reduce, Scalar, ZERO, extreme, unwrap_option};
use super::{Shown, TypeLookup};
use crate::access::{Access, Factor, Json, Read, is_na_real};
use crate::emit::number_text;
use crate::ir::{FieldType, Logic, RuleDef, TableDef};
use crate::name::Name;
use crate::problem::Segment;

/// The rows of a block.
const BLOCK: usize = 1024;

/// The states of a cell without a value: missing, and the errors.
const MISSING: u8 = 1;
const STATES: [&str; 5] = ["", "", ZERO, ROOT, LOGARITHM];
const ZERO_STATE: u8 = 2;
const ROOT_STATE: u8 = 3;
const LOG_STATE: u8 = 4;

/// The logical of a cell in state `s`: 0 is false and 1 true, so that the
/// result of an `and` without a false operand, and of an `or` without a true
/// one, is the greatest.
const fn stated(s: u8) -> u8 {
    s + 1
}
const NA: u8 = stated(MISSING);

/// A missing number, a `NaN`.
const NA_NUMBER: f64 = f64::from_bits(crate::abi::NA_DOUBLE_BITS);

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
        let eval = Eval::new(&self.rows, table, rows)?;
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
        let mut error: Option<RowError> = None;
        let eval = Eval::new(&self.rows, table, rows)?;
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
        let mut out = Diagnosis::default();
        let Ok(eval) = Eval::new(&self.rows, table, rows) else {
            return out;
        };
        let shown = |e: &Expr| match eval.block(e, row, row + 1) {
            Ok(b) => match b.scalar() {
                Some(s) => shown_scalar(s),
                None => Shown::Absent,
            },
            Err(_) => Shown::Other,
        };
        for (k, c) in self.rows.cells.iter().enumerate() {
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

/// The columns of a table's value.
struct Held<'t, 'a, A> {
    def: &'t TableDef,
    table: A,
    columns: Vec<(Cow<'a, str>, A)>,
}

impl<'a, A: Access<'a>> RowScope for Held<'_, 'a, A> {
    fn column(&self, name: &str) -> Option<(Cell, Option<String>)> {
        let (_, cells) = self.columns.iter().find(|(k, _)| k == name)?;
        if let Some(found) = (&*self.def.columns).column(name) {
            return Some(found);
        }
        let cell = Cell {
            name: name.into(),
            ty: self.table.extra_column_type(name, *cells).ok()?,
            sensitive: false,
        };
        let facts = self.table.column_facts(name).unwrap_or_default();
        Some((cell, facts.unit))
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
        def,
        table,
        columns: t.columns,
    };
    RowRule::filter(condition, &held, lookup)
        .map_err(NotChosen::Rule)?
        .choose(table, rows)
        .map_err(NotChosen::Table)?
        .map_err(NotChosen::Row)
}

/// The states of the cells of a block; `None` where each has a value.
type States = Option<Vec<u8>>;

/// What the instants of a block are.
#[derive(Clone, Copy, PartialEq)]
enum Instant {
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
enum Numbers<'a> {
    Host(&'a [f64]),
    Own(Vec<f64>),
}

impl Numbers<'_> {
    fn all(&self) -> &[f64] {
        match self {
            Numbers::Host(x) => x,
            Numbers::Own(x) => x,
        }
    }
}

/// The cells of one expression in a block of rows.
enum Block<'e, 'a> {
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
    fn logicals(self, n: usize) -> Result<Vec<u8>, String> {
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
    fn text(&self, i: usize) -> Result<&str, u8> {
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
    fn time(&self, i: usize) -> Result<i64, u8> {
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

/// The columns of a table, and the results of the aggregates over them.
struct Eval<'a, A> {
    columns: Vec<Column<'a, A>>,
    rows: usize,
    /// Each aggregate's value, or its state.
    aggregates: Vec<Result<Scalar, u8>>,
    /// By the address of a comparison with literals, what it is for each level
    /// of the factor compared.
    levels: RefCell<HashMap<usize, Vec<u8>>>,
    /// By the address of a comparison with literals, what it is for strings
    /// that it met, at a place that their key gives.
    met: RefCell<HashMap<usize, Vec<(usize, u8)>>>,
}

/// The strings that a comparison with literals keeps the result of.
const MET: usize = 4096;

impl<'a, A: Access<'a>> Eval<'a, A> {
    fn new(of: &Rows, table: A, rows: usize) -> Result<Self, String> {
        let columns = of
            .cells
            .iter()
            .map(|c| Self::column(c, table.member(&c.name)))
            .collect();
        let mut eval = Eval {
            columns,
            rows,
            aggregates: Vec::with_capacity(of.aggregates),
            levels: RefCell::default(),
            met: RefCell::default(),
        };
        let mut found = Vec::with_capacity(of.aggregates);
        aggregates(&of.invariant, &mut found);
        if let Some(w) = &of.when {
            aggregates(w, &mut found);
        }
        // One within the operand of another comes before it.
        found.sort_by_key(|(at, _, _)| *at);
        for (_, op, operands) in found {
            let v = eval.aggregate(op, operands)?;
            eval.aggregates.push(v);
        }
        Ok(eval)
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
        crate::progress::tick_block();
        let mut out = self.block(&of.invariant, lo, hi)?.logicals(hi - lo)?;
        if let Some(w) = &of.when {
            let when = self.block(w, lo, hi)?.logicals(hi - lo)?;
            for (x, w) in out.iter_mut().zip(when) {
                match w {
                    1 => {}
                    0 | NA => *x = 1,
                    e => *x = e,
                }
            }
        }
        Ok(out)
    }

    /// The cells of column `k` in the rows `lo..hi`.
    fn cells(&self, k: usize, lo: usize, hi: usize) -> Block<'_, 'a> {
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

    /// The value of an aggregate, or its state.
    fn aggregate(&self, op: Op, operands: &[Expr]) -> Result<Result<Scalar, u8>, String> {
        let mut state = 0u8;
        let mut reduce = Reduce::new(op);
        let mut time: Option<(i64, Instant)> = None;
        let least = op == Op::Min;
        for e in operands {
            for lo in (0..self.rows).step_by(BLOCK) {
                crate::progress::tick_block();
                let hi = (lo + BLOCK).min(self.rows);
                let n = hi - lo;
                let b = self.block(e, lo, hi)?;
                if let Some(of) = b.instant() {
                    for i in 0..n {
                        match b.time(i) {
                            Ok(t) => {
                                time = Some(match time {
                                    None => (t, of),
                                    Some((s, of)) => (if least { s.min(t) } else { s.max(t) }, of),
                                })
                            }
                            Err(s) => state = state.max(s),
                        }
                    }
                    continue;
                }
                // One value for every row is one value.
                let once = matches!(b, Block::One(_));
                let (x, states) = b.numbers(n)?;
                if let Some(s) = states {
                    state = s.iter().copied().fold(state, u8::max);
                }
                match x {
                    Operand::Many(xs) => {
                        if xs.iter().any(|x| x.is_nan()) {
                            state = state.max(MISSING);
                        } else {
                            xs.iter().for_each(|&x| reduce.push(x));
                        }
                    }
                    Operand::One(x) if x.is_nan() => state = state.max(MISSING),
                    Operand::One(x) => reduce.push(x),
                }
                if once {
                    break;
                }
            }
        }
        if state != 0 {
            return Ok(Err(state));
        }
        Ok(Ok(match time {
            Some((t, of)) => of.scalar(t),
            None => reduce.finish()?,
        }))
    }

    /// The cells of `e` in the rows `lo..hi`.
    fn block<'e>(&'e self, e: &'e Expr, lo: usize, hi: usize) -> Result<Block<'e, 'a>, String> {
        let n = hi - lo;
        match e {
            Expr::Lit(s) => Ok(Block::One(s.clone())),
            Expr::Cell(k) => Ok(self.cells(*k, lo, hi)),
            Expr::Aggregate { at, .. } => Ok(match &self.aggregates[*at] {
                Ok(v) => Block::One(v.clone()),
                Err(s) => Block::Nothing(*s),
            }),
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
                            let zero = a.with(y, n, |a, b| b == 0.0 && !a.is_nan());
                            states = mark(states, &zero, ZERO_STATE);
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

/// The aggregates of `e`, with their places.
fn aggregates<'e>(e: &'e Expr, out: &mut Vec<(usize, Op, &'e [Expr])>) {
    match e {
        Expr::Aggregate { op, operands, at } => {
            operands.iter().for_each(|x| aggregates(x, out));
            out.push((*at, *op, operands));
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
