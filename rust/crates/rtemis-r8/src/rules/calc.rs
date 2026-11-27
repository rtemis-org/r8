//! Columns computed by an expression of the rule dialect, and summaries of
//! the groups of a table's rows.
//!
//! [`computed`] evaluates expressions over the rows of a table: a column of
//! every row, in which an aggregate is over all rows or over the row's group,
//! or, for a summary, a column of one value for each group. Groups are the
//! distinct values of key columns, numbered in the order of [`r8_sort()`]'s
//! keys: ascending, missing last, strings by code point, factors by level.
//! `col()` in an expression is the column that [`mapped`] sets it to.
//!
//! [`r8_sort()`]: crate::rules

use std::collections::HashMap;
use std::hash::{BuildHasherDefault, Hasher};
use std::sync::Arc;

use super::compile::{Calc, Cell, Expr, Op, RowScope, Rows, RuleError, compile_calc};
use super::eval::Scalar;
use super::rows::{Block, Eval, Groups, Held, MISSING, NA_NUMBER, RowError, STATES};
use super::{Ty, TypeLookup};
use crate::access::Access;
use crate::ir::{FieldType, Logic, Operator, TableDef, build};

/// The rows of a block.
const BLOCK: usize = 1024;

/// The kind of a computed column.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Kind {
    /// A logical.
    Logical,
    /// An integer: `n()`, `count()`, and a choice among integers.
    Integer,
    /// A double.
    Double,
    /// A string.
    Character,
    /// A date.
    Date,
    /// A date-time.
    DateTime,
}

/// The cells of a computed column; `None`, or `NA_NUMBER` for a double, is
/// missing.
#[derive(Debug, Clone, PartialEq)]
pub enum Data {
    /// Logicals.
    Logical(Vec<Option<bool>>),
    /// Integers.
    Integer(Vec<Option<i32>>),
    /// Doubles, `NA_NUMBER` where missing.
    Double(Vec<f64>),
    /// Strings.
    Character(Vec<Option<String>>),
    /// Days since 1970-01-01.
    Date(Vec<Option<i64>>),
    /// Microseconds since 1970-01-01T00:00:00Z.
    DateTime(Vec<Option<i64>>),
}

/// A column that an expression computes.
#[derive(Debug, Clone, PartialEq)]
pub struct Computed {
    /// The column's kind.
    pub kind: Kind,
    /// The unit that the compiler infers.
    pub unit: Option<String>,
    /// The cells.
    pub data: Data,
    /// The date-time column whose time zone the column takes, if it reads
    /// one.
    pub zone_of: Option<String>,
}

/// Why expressions compute no columns.
#[derive(Debug, Clone, PartialEq)]
pub enum NotComputed {
    /// Expression `at` does not fit the columns; paths are into it.
    Rule {
        /// The expression's place.
        at: usize,
        /// Its errors.
        errors: Vec<RuleError>,
    },
    /// Expression `at` is an error in a row, or, for a summary, a group.
    Row {
        /// The expression's place.
        at: usize,
        /// The first row, or group, with an error.
        error: RowError,
    },
    /// A key column does not group rows.
    Key(String),
    /// The table does not read.
    Table(String),
}

/// What [`computed`] gives: the groups of the rows, by the keys, and the
/// columns.
#[derive(Debug, Clone)]
pub struct Outcome {
    /// The groups, when keys are given.
    pub groups: Option<Groups>,
    /// The rows of the table.
    pub rows: usize,
    /// The computed columns, in the order of the expressions.
    pub columns: Vec<Computed>,
}

/// A hash of integers by one multiplication and shift, for the keys of groups.
#[derive(Default)]
struct Mix(u64);

impl Hasher for Mix {
    fn finish(&self) -> u64 {
        self.0
    }
    fn write(&mut self, bytes: &[u8]) {
        for &b in bytes {
            self.write_u64(u64::from(b));
        }
    }
    fn write_u64(&mut self, x: u64) {
        let h = (self.0 ^ x).wrapping_mul(0x9e37_79b9_7f4a_7c15);
        self.0 = h ^ (h >> 29);
    }
    fn write_u32(&mut self, x: u32) {
        self.write_u64(u64::from(x));
    }
    fn write_usize(&mut self, x: usize) {
        self.write_u64(x as u64);
    }
}

type MixMap<K, V> = HashMap<K, V, BuildHasherDefault<Mix>>;

/// The key of a missing cell, which orders last.
const MISSING_KEY: u64 = u64::MAX;

/// A key of a double that orders as the numbers: `-0` and `0` are one key.
fn double_key(x: f64) -> u64 {
    if x.is_nan() {
        return MISSING_KEY;
    }
    let bits = (x + 0.0).to_bits();
    if bits >> 63 == 1 {
        !bits
    } else {
        bits | 1 << 63
    }
}

/// A key of an integer that orders as the integers.
fn integer_key(x: i64) -> u64 {
    (x as u64) ^ (1 << 63)
}

/// The keys of the rows of the column `k`, which order as `r8_sort()` orders
/// the column.
fn column_keys<'a, A: Access<'a>>(eval: &Eval<'a, A>, k: usize, rows: usize) -> Vec<u64> {
    if let Some(x) = eval.integers(k) {
        return x[..rows]
            .iter()
            .map(|&v| {
                if v == i32::MIN {
                    MISSING_KEY
                } else {
                    integer_key(i64::from(v))
                }
            })
            .collect();
    }
    if let Some(keys) = text_ranks(eval, k, rows) {
        return keys;
    }
    let mut out = Vec::with_capacity(rows);
    // Texts, by the text.
    let mut ranks_of_keys: MixMap<usize, u32> = MixMap::default();
    let mut texts: Vec<String> = Vec::new();
    let mut ranks_of_texts: HashMap<String, u32> = HashMap::new();
    let mut is_text = false;
    // Numbers, which become keys once their kind is known.
    let mut numbers: Vec<f64> = Vec::new();
    for lo in (0..rows).step_by(BLOCK) {
        crate::progress::tick_block();
        let hi = (lo + BLOCK).min(rows);
        let state = |s: &Option<Vec<u8>>, i: usize| s.as_ref().map_or(0, |s| s[i]);
        match eval.cells(k, lo, hi) {
            Block::Number(x, s) => numbers.extend(
                x.all().iter().enumerate().map(
                    |(i, &v)| {
                        if state(&s, i) != 0 { f64::NAN } else { v }
                    },
                ),
            ),
            Block::Bool(x) => out.extend(x.iter().map(|&v| match v {
                0 | 1 => u64::from(v),
                _ => MISSING_KEY,
            })),
            Block::Time(x, s, _) => out.extend(x.iter().enumerate().map(|(i, &t)| {
                if state(&s, i) != 0 || t == i64::MIN {
                    MISSING_KEY
                } else {
                    integer_key(t)
                }
            })),
            Block::Label(codes, levels) => out.extend(codes.iter().map(|&c| {
                if c >= 1 && (c as usize) <= levels.len() {
                    c as u64
                } else {
                    MISSING_KEY
                }
            })),
            Block::Text(x, s) => {
                is_text = true;
                let mut host = vec![0usize; hi - lo];
                let by_host = eval.text_keys(k, lo, &mut host);
                for (i, t) in x.iter().enumerate() {
                    if state(&s, i) != 0 {
                        out.push(MISSING_KEY);
                        continue;
                    }
                    let rank = if by_host {
                        *ranks_of_keys.entry(host[i]).or_insert_with(|| {
                            *ranks_of_texts.entry(t.to_string()).or_insert_with(|| {
                                texts.push(t.to_string());
                                (texts.len() - 1) as u32
                            })
                        })
                    } else {
                        *ranks_of_texts.entry(t.to_string()).or_insert_with(|| {
                            texts.push(t.to_string());
                            (texts.len() - 1) as u32
                        })
                    };
                    out.push(u64::from(rank));
                }
            }
            Block::One(_) | Block::Nothing(_) => {
                out.extend(std::iter::repeat_n(MISSING_KEY, hi - lo))
            }
        }
    }
    if !numbers.is_empty() {
        // Whole numbers as integers, whose keys are dense; others as doubles.
        let whole = numbers
            .iter()
            .all(|v| v.is_nan() || (v.fract() == 0.0 && v.abs() < 9.0e15));
        out = numbers
            .iter()
            .map(|&v| match v {
                v if v.is_nan() => MISSING_KEY,
                v if whole => integer_key(v as i64),
                v => double_key(v),
            })
            .collect();
    }
    if is_text {
        // The ranks of the texts in the order of their code points.
        let mut order: Vec<u32> = (0..texts.len() as u32).collect();
        order.sort_unstable_by(|&a, &b| texts[a as usize].cmp(&texts[b as usize]));
        let mut rank = vec![0u64; texts.len()];
        for (r, &t) in order.iter().enumerate() {
            rank[t as usize] = r as u64;
        }
        for k in out.iter_mut().filter(|k| **k != MISSING_KEY) {
            *k = rank[*k as usize];
        }
    }
    out
}

/// The places of a direct index of groups: above them, a hash.
const DIRECT: usize = 1 << 22;

/// The keys of the strings of the column `k`, the ranks of their texts by
/// code point, where the host has keys of its strings: each text is read
/// once.
fn text_ranks<'a, A: Access<'a>>(eval: &Eval<'a, A>, k: usize, rows: usize) -> Option<Vec<u64>> {
    if rows == 0 || !eval.text_keys(k, 0, &mut [0usize]) {
        return None;
    }
    let mut out = Vec::with_capacity(rows);
    let mut of_key: MixMap<usize, u32> = MixMap::default();
    let mut texts: Vec<String> = Vec::new();
    let mut host = vec![0usize; BLOCK];
    for lo in (0..rows).step_by(BLOCK) {
        crate::progress::tick_block();
        let hi = (lo + BLOCK).min(rows);
        let host = &mut host[..hi - lo];
        if !eval.text_keys(k, lo, host) {
            return None;
        }
        for (i, &h) in host.iter().enumerate() {
            let at = match of_key.get(&h) {
                Some(&at) => at,
                None => {
                    let at = match eval.text_at(k, lo + i) {
                        Some(t) => {
                            texts.push(t);
                            (texts.len() - 1) as u32
                        }
                        None => u32::MAX,
                    };
                    of_key.insert(h, at);
                    at
                }
            };
            out.push(if at == u32::MAX {
                MISSING_KEY
            } else {
                u64::from(at)
            });
        }
    }
    // One text may have two keys, in two encodings: equal texts take one
    // rank.
    let mut order: Vec<u32> = (0..texts.len() as u32).collect();
    order.sort_unstable_by(|&a, &b| texts[a as usize].cmp(&texts[b as usize]));
    let mut rank = vec![0u64; texts.len()];
    let mut r = 0u64;
    for (i, &t) in order.iter().enumerate() {
        if i > 0 && texts[t as usize] != texts[order[i - 1] as usize] {
            r += 1;
        }
        rank[t as usize] = r;
    }
    for x in out.iter_mut().filter(|x| **x != MISSING_KEY) {
        *x = rank[*x as usize];
    }
    Some(out)
}

/// The groups of the rows by the keys of their columns, numbered in the
/// order of the keys. Each key refines the groups of the keys before it: by a
/// direct index where the groups so far times the range of the key are few,
/// and by a hash otherwise.
fn group_ids(keys: &[Vec<u64>], rows: usize) -> Groups {
    let mut ids = vec![0u32; rows];
    let mut count = usize::from(rows > 0);
    // Groups numbered by direct places are in the order of their keys.
    let mut sorted = true;
    for key in keys {
        let (lo, hi) = key
            .iter()
            .filter(|&&k| k != MISSING_KEY)
            .fold((u64::MAX, 0u64), |(lo, hi), &k| (lo.min(k), hi.max(k)));
        // The places of the key's values, and one for a missing cell.
        let width = if lo > hi { 1 } else { (hi - lo) as usize + 2 };
        let place = |k: u64| {
            if k == MISSING_KEY {
                width - 1
            } else {
                (k - lo) as usize
            }
        };
        if sorted
            && count
                .checked_mul(width)
                .is_some_and(|w| w <= DIRECT.max(2 * rows))
        {
            // Each place in use, by the group so far and the key's value,
            // takes its rank among those in use, which orders as the keys.
            let mut at = vec![u32::MAX; count * width];
            for (r, &k) in key.iter().enumerate() {
                at[ids[r] as usize * width + place(k)] = 0;
            }
            let mut next = 0u32;
            for a in at.iter_mut().filter(|a| **a == 0) {
                *a = next;
                next += 1;
            }
            for (r, &k) in key.iter().enumerate() {
                ids[r] = at[ids[r] as usize * width + place(k)];
            }
            count = next as usize;
        } else {
            sorted = false;
            let mut at: MixMap<(u32, u64), u32> = MixMap::default();
            for (r, &k) in key.iter().enumerate() {
                let next = at.len() as u32;
                ids[r] = *at.entry((ids[r], k)).or_insert(next);
            }
            count = at.len();
        }
    }
    if sorted {
        return Groups::new(ids, count);
    }
    // The groups in the order of their keys.
    let mut first = vec![u32::MAX; count];
    for (r, &g) in ids.iter().enumerate() {
        if first[g as usize] == u32::MAX {
            first[g as usize] = r as u32;
        }
    }
    let mut order: Vec<u32> = (0..count as u32).collect();
    order.sort_unstable_by(|&a, &b| {
        let (ra, rb) = (first[a as usize] as usize, first[b as usize] as usize);
        keys.iter()
            .map(|k| k[ra].cmp(&k[rb]))
            .find(|o| o.is_ne())
            .unwrap_or(std::cmp::Ordering::Equal)
    });
    let mut place = vec![0u32; count];
    for (p, &g) in order.iter().enumerate() {
        place[g as usize] = p as u32;
    }
    for g in ids.iter_mut() {
        *g = place[*g as usize];
    }
    Groups::new(ids, count)
}

/// Whether an expression gives an integer whatever its values: a cell of an
/// integer column, `n()`, `count()`, and a choice among integers.
fn integer(e: &Expr, cells: &[Cell]) -> bool {
    match e {
        Expr::Cell(k) => matches!(
            super::eval::unwrap_option(&cells[*k].ty),
            FieldType::Integer(_)
        ),
        Expr::Aggregate {
            op: Op::N | Op::Count,
            ..
        } => true,
        Expr::Aggregate {
            op: Op::Min | Op::Max,
            operands,
            ..
        }
        | Expr::Op(Op::Min | Op::Max, operands) => operands.iter().all(|x| integer(x, cells)),
        Expr::If(parts) => parts.iter().enumerate().all(|(i, p)| {
            (i % 2 == 0 && i + 1 < parts.len())
                || matches!(p, Expr::Lit(Scalar::Null))
                || integer(p, cells)
        }),
        _ => false,
    }
}

/// The name of the first date-time column that an expression reads.
fn zone_of(cells: &[Cell]) -> Option<String> {
    cells
        .iter()
        .find(|c| matches!(super::eval::unwrap_option(&c.ty), FieldType::Posixct))
        .map(|c| c.name.to_string())
}

/// The kind of a computed column of type `ty`.
fn kind_of(ty: &Ty, int: bool) -> Kind {
    match ty {
        Ty::Bool | Ty::Any => Kind::Logical,
        Ty::Number { .. } if int => Kind::Integer,
        Ty::Number { .. } => Kind::Double,
        Ty::Date => Kind::Date,
        Ty::Posixct => Kind::DateTime,
        _ => Kind::Character,
    }
}

/// Empty cells of a kind.
fn empty(kind: Kind, n: usize) -> Data {
    match kind {
        Kind::Logical => Data::Logical(Vec::with_capacity(n)),
        Kind::Integer => Data::Integer(Vec::with_capacity(n)),
        Kind::Double => Data::Double(Vec::with_capacity(n)),
        Kind::Character => Data::Character(Vec::with_capacity(n)),
        Kind::Date => Data::Date(Vec::with_capacity(n)),
        Kind::DateTime => Data::DateTime(Vec::with_capacity(n)),
    }
}

/// Appends the cells of a block of `n` rows from `lo` to `out`; the first
/// error, at its row, and the number of rows with one, go to `error`.
fn append(out: &mut Data, b: &Block<'_, '_>, lo: usize, n: usize, error: &mut Option<RowError>) {
    // Numbers of a block without states: `NaN` is missing.
    if let (Data::Double(v), Block::Number(x, None)) = (&mut *out, b) {
        v.extend(
            x.all()[..n]
                .iter()
                .map(|&x| if x.is_nan() { NA_NUMBER } else { x }),
        );
        return;
    }
    let mut fail = |i: usize, s: u8| match error {
        Some(e) => e.count += 1,
        None => {
            *error = Some(RowError {
                row: lo + i,
                count: 1,
                message: STATES[s as usize],
            })
        }
    };
    // The state of cell `i`: 0 with a value, `MISSING`, or an error.
    let state_of = |i: usize| -> u8 {
        match b {
            Block::Bool(x) => match x[i] {
                0 | 1 => 0,
                v => v - 1,
            },
            Block::Number(x, s) => match s.as_ref().map_or(0, |s| s[i]) {
                0 if x.all()[i].is_nan() => MISSING,
                s => s,
            },
            Block::Time(x, s, _) => match s.as_ref().map_or(0, |s| s[i]) {
                0 if x[i] == i64::MIN => MISSING,
                s => s,
            },
            Block::Text(_, s) => s.as_ref().map_or(0, |s| s[i]),
            Block::Label(..) => u8::from(b.text(i).is_err()) * MISSING,
            Block::One(Scalar::Null) => MISSING,
            Block::One(Scalar::Num(x)) if x.is_nan() => MISSING,
            Block::One(_) => 0,
            Block::Nothing(s) => *s,
        }
    };
    for i in 0..n {
        let s = state_of(i);
        if s > MISSING {
            fail(i, s);
        }
        let present = s == 0;
        match out {
            Data::Logical(v) => v.push(present.then(|| match b {
                Block::Bool(x) => x[i] == 1,
                Block::One(Scalar::Bool(t)) => *t,
                _ => false,
            })),
            Data::Integer(v) => v.push(present.then(|| number(b, i) as i32)),
            Data::Double(v) => v.push(if present { number(b, i) } else { NA_NUMBER }),
            Data::Character(v) => v.push(if present {
                b.text(i).ok().map(str::to_string).or_else(|| match b {
                    Block::One(Scalar::Num(x)) => Some(crate::emit::number_text(*x)),
                    _ => None,
                })
            } else {
                None
            }),
            Data::Date(v) | Data::DateTime(v) => {
                v.push(if present { b.time(i).ok() } else { None })
            }
        }
    }
}

/// The number of cell `i` of a block of numbers.
fn number(b: &Block<'_, '_>, i: usize) -> f64 {
    match b {
        Block::Number(x, _) => x.all()[i],
        Block::One(Scalar::Num(x)) => *x,
        _ => NA_NUMBER,
    }
}

/// `f` with `col()` set to the column `column`.
fn set_column(f: &Logic, column: &str) -> Logic {
    match f {
        Logic::Call {
            op: Operator::Col,
            args,
            ..
        } if args.is_empty() => build::path(&[column]),
        Logic::Call { op, single, args } => Logic::Call {
            op: *op,
            single: *single,
            args: args.iter().map(|a| set_column(a, column)).collect(),
        },
        other => other.clone(),
    }
}

/// Whether `f` holds `col()`.
fn has_column(f: &Logic) -> bool {
    match f {
        Logic::Call {
            op: Operator::Col,
            args,
            ..
        } => args.is_empty(),
        Logic::Call { args, .. } => args.iter().any(has_column),
        _ => false,
    }
}

/// The expressions of a map of `f` over `columns`: `f` with `col()` set to
/// each column, in order. `f` holds `col()`.
pub fn mapped(f: &Logic, columns: &[&str]) -> Result<Vec<Logic>, RuleError> {
    if !has_column(f) {
        return Err(RuleError {
            code: "map_column",
            path: String::new(),
            message: "the expression of a map reads the column it maps as `col()`, as in `calc(mean(col()))`".to_string(),
        });
    }
    Ok(columns.iter().map(|c| set_column(f, c)).collect())
}

/// The columns that `exprs` compute over a valid value of the table `def`: of
/// every row, or, where `summary`, of each group of the rows by the columns
/// `by`, or of the table without them. In a column of every row, an
/// aggregate is over the row's group.
pub fn computed<'a, A: Access<'a>>(
    table: A,
    def: &TableDef,
    by: &[&str],
    exprs: &[&Logic],
    lookup: &dyn TypeLookup,
    summary: bool,
) -> Result<Outcome, NotComputed> {
    let t = table
        .table(&def.columns, def.open)
        .map_err(|f| NotComputed::Table(f.message))?;
    let rows = t.rows as usize;
    if u32::try_from(rows).is_err() {
        return Err(NotComputed::Table(format!(
            "rows are grouped in a table of at most 4,294,967,295; this one has {rows}"
        )));
    }
    let held = Held {
        declared: &def.columns,
        table,
        columns: &t.columns,
    };
    let scope: &dyn RowScope = &held;
    // The keys.
    let mut cells = Vec::with_capacity(by.len());
    for (i, name) in by.iter().enumerate() {
        if by[..i].contains(name) {
            return Err(NotComputed::Key(format!("`{name}` is a key once")));
        }
        match scope.column(name) {
            Some(Ok((c, _))) => cells.push(c),
            Some(Err(m)) => return Err(NotComputed::Key(m)),
            None => {
                let near = scope
                    .nearest(name)
                    .map(|n| format!("; the nearest column is `{n}`"))
                    .unwrap_or_default();
                return Err(NotComputed::Key(format!(
                    "`{name}` is not a column of the table{near}"
                )));
            }
        }
    }
    let groups = if by.is_empty() {
        None
    } else {
        let keys_of = Rows {
            invariant: Expr::Lit(Scalar::Null),
            when: None,
            cells,
            aggregates: 0,
        };
        let eval = Eval::new(&keys_of, table, rows, None).map_err(NotComputed::Table)?;
        let keys: Vec<Vec<u64>> = (0..by.len()).map(|k| column_keys(&eval, k, rows)).collect();
        Some(Arc::new(group_ids(&keys, rows)))
    };
    let mut columns = Vec::with_capacity(exprs.len());
    for (at, e) in exprs.iter().enumerate() {
        let calc: Calc = compile_calc(e, scope, lookup, summary)
            .map_err(|errors| NotComputed::Rule { at, errors })?;
        let unit = match &calc.ty {
            Ty::Number { unit, .. } => unit.clone(),
            _ => None,
        };
        let kind = kind_of(&calc.ty, integer(&calc.rows.invariant, &calc.rows.cells));
        let eval =
            Eval::new(&calc.rows, table, rows, groups.clone()).map_err(NotComputed::Table)?;
        let n = if summary { eval.group_count() } else { rows };
        // A summary of a table of no rows, without keys, is one row.
        let n = if summary && groups.as_ref().is_some_and(|g| g.count == 0) {
            0
        } else {
            n
        };
        eval.set_over_groups(summary);
        let mut data = empty(kind, n);
        let mut error: Option<RowError> = None;
        for lo in (0..n).step_by(BLOCK) {
            crate::progress::tick_block();
            let hi = (lo + BLOCK).min(n);
            let b = eval
                .block(&calc.rows.invariant, lo, hi)
                .map_err(NotComputed::Table)?;
            append(&mut data, &b, lo, hi - lo, &mut error);
        }
        eval.set_over_groups(false);
        if let Some(error) = error {
            return Err(NotComputed::Row { at, error });
        }
        columns.push(Computed {
            kind,
            unit,
            data,
            zone_of: zone_of(&calc.rows.cells),
        });
    }
    Ok(Outcome {
        groups: groups.map(|g| Arc::try_unwrap(g).unwrap_or_else(|g| (*g).clone())),
        rows,
        columns,
    })
}
