//! Columns computed by an expression of the rule dialect, and summaries of
//! the groups of a table's rows.
//!
//! [`computed`] evaluates expressions over the rows of a table: a column of
//! every row, in which an aggregate is over all rows or over the row's group,
//! or, for a summary, a column of one value for each group. Groups are the
//! distinct values of key columns, numbered in the order of [`r8sort()`]'s
//! keys: ascending, missing last, strings by code point, factors by level.
//! `col()` in an expression is the column that [`mapped`] sets it to.
//!
//! An expression calls a typed function through the host's [`Functions`]:
//! once over all rows, or once for each group, with the cells of the group
//! for each parameter of `vec(T)` and a constant for each scalar parameter.
//!
//! [`r8sort()`]: crate::rules

use std::collections::HashMap;
use std::hash::{BuildHasherDefault, Hasher};
use std::sync::Arc;
use std::time::Instant;

use super::compile::{ArgValue, Calc, Cell, Expr, Op, RowScope, Rows, RuleError, compile_calc};
use super::eval::Scalar;
use super::rows::{
    Block, Eval, Failed, Groups, Held, MISSING, NA_NUMBER, Placed, RowError, STATES,
};
use super::{Ty, TypeLookup};
use crate::access::Access;
use crate::ir::{FieldType, Logic, Operator, TableDef, build};
use crate::name::QualifiedName;
use crate::tree::Tree;

/// The rows of a block: 4,096 doubles, 32 KiB, an operand that stays in the
/// cache.
const BLOCK: usize = 4096;

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
    /// The cells, or `None` where they are in the host's buffer
    /// ([`Output::doubles`]).
    pub data: Option<Data>,
    /// The date-time column whose time zone the column takes, if it reads
    /// one.
    pub zone_of: Option<String>,
}

/// The argument of one parameter in a call of a typed function, for one
/// group of rows or for all rows.
#[derive(Debug, Clone, Copy)]
pub enum Argument<'c> {
    /// The cells of the table's column `name` in the rows `rows`, in order.
    Column {
        /// The column.
        name: &'c str,
        /// The rows, from 0.
        rows: &'c [u32],
    },
    /// The cells that an expression computes, in the rows `rows`; a date-time
    /// takes the time zone of the column `zone`.
    Cells {
        /// The cells of every row.
        data: &'c Data,
        /// The rows, from 0.
        rows: &'c [u32],
        /// The date-time column whose time zone the cells take.
        zone: Option<&'c str>,
    },
    /// A constant, as the expression writes it.
    Constant(&'c Logic),
    /// The parameter's default, which the expression omits.
    Default(&'c Tree),
}

/// The value of a call of a typed function: one value of its return type.
#[derive(Debug, Clone, PartialEq)]
pub enum CallValue {
    /// A missing value, of a return type of `option()`.
    Missing,
    /// A logical.
    Logical(bool),
    /// An integer or a double.
    Number(f64),
    /// A string.
    Text(String),
    /// Days since 1970-01-01.
    Date(i64),
    /// Microseconds since 1970-01-01T00:00:00Z.
    DateTime(i64),
}

/// The arguments of one call of a typed function, each by its parameter's
/// name, in the order of the parameters.
pub type Call<'c> = Vec<(&'c str, Argument<'c>)>;

/// A call of a typed function that is an error.
#[derive(Debug, Clone, PartialEq)]
pub struct CallFailed {
    /// The call, by its place among the calls.
    pub at: usize,
    /// The call's message.
    pub message: String,
}

/// The host's calls of typed functions.
pub trait Functions {
    /// The values of `function` for each of `calls`, in order, one for each
    /// group; the first call that is an error stops the others.
    fn call(
        &self,
        function: &QualifiedName,
        calls: &[Call<'_>],
    ) -> Result<Vec<CallValue>, CallFailed>;

    /// Called before the calls of the expression at the place `at` of the
    /// expressions computed, so that a host can name the column of a call.
    fn expression(&self, _at: usize) {}
}

/// A host that calls no typed function.
pub struct NoFunctions;

impl Functions for NoFunctions {
    fn call(&self, function: &QualifiedName, _: &[Call<'_>]) -> Result<Vec<CallValue>, CallFailed> {
        Err(CallFailed {
            at: 0,
            message: format!("`{function}()` is called where the host calls no typed function"),
        })
    }
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
    /// A call of a typed function in expression `at` is an error.
    Call {
        /// The expression's place.
        at: usize,
        /// The function's qualified name.
        function: String,
        /// The group of the call, from 0, or 0 for all rows.
        group: usize,
        /// The first row of the group, when there are groups.
        first_row: Option<usize>,
        /// The call's message.
        message: String,
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

/// The keys of the rows of the column `k`, which order as `r8sort()` orders
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

/// The groups of one key column of integers, `i32::MIN` missing, numbered in
/// the order of the integers, the missing last; `None` where their range is
/// too wide for a direct index.
fn integer_groups(x: &[i32]) -> Option<Groups> {
    let rows = x.len();
    let (lo, hi) = x
        .iter()
        .filter(|&&v| v != i32::MIN)
        .fold((i32::MAX, i32::MIN), |(lo, hi), &v| (lo.min(v), hi.max(v)));
    let width = if lo > hi {
        1
    } else {
        (i64::from(hi) - i64::from(lo)) as usize + 2
    };
    if width > DIRECT.max(2 * rows) {
        return None;
    }
    let place = |v: i32| {
        if v == i32::MIN {
            width - 1
        } else {
            (i64::from(v) - i64::from(lo)) as usize
        }
    };
    let mut at = vec![0u32; width];
    for &v in x {
        at[place(v)] += 1;
    }
    // Each place in use takes its rank, which orders as the integers.
    let mut sizes = Vec::new();
    for a in at.iter_mut() {
        if *a > 0 {
            sizes.push(*a);
            *a = (sizes.len() - 1) as u32;
        } else {
            *a = u32::MAX;
        }
    }
    let count = sizes.len();
    let mut ids = vec![0u32; rows];
    let mut first = vec![0u32; count];
    // Backward, so that each group's first row is written last.
    for r in (0..rows).rev() {
        let g = at[place(x[r])];
        ids[r] = g;
        first[g as usize] = r as u32;
    }
    Some(Groups::from_parts(ids, sizes, first))
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
pub(super) fn integer(e: &Expr, cells: &[Cell]) -> bool {
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
        Expr::Call { kind, .. } => *kind == Kind::Integer,
        Expr::If(parts) => parts.iter().enumerate().all(|(i, p)| {
            (i % 2 == 0 && i + 1 < parts.len())
                || matches!(p, Expr::Lit(Scalar::Null))
                || integer(p, cells)
        }),
        _ => false,
    }
}

/// The name of the first date-time column that an expression reads.
pub(super) fn zone_of(cells: &[Cell]) -> Option<String> {
    cells
        .iter()
        .find(|c| matches!(super::eval::unwrap_option(&c.ty), FieldType::Posixct))
        .map(|c| c.name.to_string())
}

/// The kind of a computed column of type `ty`.
pub(super) fn kind_of(ty: &Ty, int: bool) -> Kind {
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
pub(super) fn empty(kind: Kind, n: usize) -> Data {
    match kind {
        Kind::Logical => Data::Logical(Vec::with_capacity(n)),
        Kind::Integer => Data::Integer(Vec::with_capacity(n)),
        Kind::Double => Data::Double(Vec::with_capacity(n)),
        Kind::Character => Data::Character(Vec::with_capacity(n)),
        Kind::Date => Data::Date(Vec::with_capacity(n)),
        Kind::DateTime => Data::DateTime(Vec::with_capacity(n)),
    }
}

/// The state of cell `i` of a block: 0 with a value, `MISSING`, or an error.
fn cell_state(b: &Block<'_, '_>, i: usize) -> u8 {
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
}

/// Records the error of state `s` at row `row`: the first error, at its row,
/// and the number of rows with one.
fn note_error(error: &mut Option<RowError>, row: usize, s: u8) {
    match error {
        Some(e) => e.count += 1,
        None => {
            *error = Some(RowError {
                row,
                count: 1,
                message: STATES[s as usize],
            })
        }
    }
}

/// Writes the cells of a block of `buf.len()` rows from `lo` into `buf`,
/// `NA_NUMBER` where a cell is missing; the first error, at its row, and the
/// number of rows with one, go to `error`.
pub(super) fn write_doubles(
    buf: &mut [f64],
    b: &Block<'_, '_>,
    lo: usize,
    error: &mut Option<RowError>,
) {
    // Numbers of a block without states: `NaN` is missing.
    if let Block::Number(x, None) = b {
        let n = buf.len();
        for (o, &v) in buf.iter_mut().zip(&x.all()[..n]) {
            *o = if v.is_nan() { NA_NUMBER } else { v };
        }
        return;
    }
    for (i, o) in buf.iter_mut().enumerate() {
        let s = cell_state(b, i);
        if s > MISSING {
            note_error(error, lo + i, s);
        }
        *o = if s == 0 { number(b, i) } else { NA_NUMBER };
    }
}

/// Sets the cells of `buf`, rows from `lo`, to `NA_NUMBER` where their state
/// is not 0; the first error, at its row, and the number of rows with one, go
/// to `error`.
fn finish_doubles(buf: &mut [f64], states: Option<&[u8]>, lo: usize, error: &mut Option<RowError>) {
    let Some(states) = states else {
        return;
    };
    for (i, (o, &s)) in buf.iter_mut().zip(states).enumerate() {
        if s > MISSING {
            note_error(error, lo + i, s);
        }
        if s != 0 {
            *o = NA_NUMBER;
        }
    }
}

/// Appends the cells of a block of `n` rows from `lo` to `out`; the first
/// error, at its row, and the number of rows with one, go to `error`.
pub(super) fn append(
    out: &mut Data,
    b: &Block<'_, '_>,
    lo: usize,
    n: usize,
    error: &mut Option<RowError>,
) {
    if let Data::Double(v) = out {
        let at = v.len();
        v.resize(at + n, 0.0);
        write_doubles(&mut v[at..], b, lo, error);
        return;
    }
    for i in 0..n {
        let s = cell_state(b, i);
        if s > MISSING {
            note_error(error, lo + i, s);
        }
        let present = s == 0;
        match out {
            Data::Logical(v) => v.push(present.then(|| match b {
                Block::Bool(x) => x[i] == 1,
                Block::One(Scalar::Bool(t)) => *t,
                _ => false,
            })),
            Data::Integer(v) => v.push(present.then(|| number(b, i) as i32)),
            Data::Double(_) => unreachable!("written above"),
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

/// Reads the table, checks the keys, and numbers the groups of the rows, then
/// gives the columns that expressions read, the rows, and the groups to
/// `then`.
fn prepared<'a, A: Access<'a>, R>(
    table: A,
    def: &TableDef,
    by: &[&str],
    then: impl FnOnce(&dyn RowScope, usize, Option<Arc<Groups>>, Instant) -> Result<R, NotComputed>,
) -> Result<R, NotComputed> {
    let begun = Instant::now();
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
        let eval = Eval::new(&keys_of, table, rows, None, 1).map_err(NotComputed::Table)?;
        let one = (by.len() == 1)
            .then(|| eval.integers(0))
            .flatten()
            .and_then(|x| integer_groups(&x[..rows]));
        Some(Arc::new(match one {
            Some(g) => g,
            None => {
                let keys: Vec<Vec<u64>> =
                    (0..by.len()).map(|k| column_keys(&eval, k, rows)).collect();
                group_ids(&keys, rows)
            }
        }))
    };
    then(scope, rows, groups, begun)
}

/// Every expression of a call compiled, before any is evaluated, and what
/// each computes.
fn compiled(
    exprs: &[&Logic],
    scope: &dyn RowScope,
    lookup: &dyn TypeLookup,
    summary: bool,
) -> Result<Vec<(Calc, ExprWork)>, NotComputed> {
    let mut out = Vec::with_capacity(exprs.len());
    for (at, e) in exprs.iter().enumerate() {
        let calc: Calc = compile_calc(e, scope, lookup, summary)
            .map_err(|errors| NotComputed::Rule { at, errors })?;
        let mut w = ExprWork::default();
        work(&calc.rows.invariant, &mut w);
        w.rows = !summary;
        out.push((calc, w));
    }
    Ok(out)
}

/// A host's buffers for computed columns of doubles, which the core fills in
/// place of columns of its own, so that a column is written once, where the
/// host keeps it.
pub trait Output {
    /// A buffer of `n` doubles for the column of the expression at `at`, or
    /// `None` for a column that the core holds.
    fn doubles(&mut self, at: usize, n: usize) -> Option<&mut [f64]>;
}

/// A host that takes every column as the core holds it.
pub struct OwnColumns;

impl Output for OwnColumns {
    fn doubles(&mut self, _: usize, _: usize) -> Option<&mut [f64]> {
        None
    }
}

/// The columns that `exprs` compute over a valid value of the table `def`: of
/// every row, or, where `summary`, of each group of the rows by the columns
/// `by`, or of the table without them. In a column of every row, an
/// aggregate is over the row's group.
#[allow(clippy::too_many_arguments)]
pub fn computed<'a, A: Access<'a>>(
    table: A,
    def: &TableDef,
    by: &[&str],
    exprs: &[&Logic],
    lookup: &dyn TypeLookup,
    functions: &dyn Functions,
    summary: bool,
    threads: usize,
) -> Result<Outcome, NotComputed> {
    computed_into(
        table,
        def,
        by,
        exprs,
        lookup,
        functions,
        summary,
        threads,
        &mut OwnColumns,
        None,
    )
}

/// The plan of a run, filled as the run goes, for a collector: each stage's
/// status, the first time it started and the last time it ended, in seconds
/// from the call's start, and the seconds it was busy; a failed stage holds
/// its group, its first row, and its message, and the stages after it stay
/// `pending`.
pub struct Trace<'a> {
    /// The call, as its plan names it.
    pub call: crate::plan::TableCall<'a>,
    /// Whether a package registered a typed function's body in compiled code.
    pub in_rust: &'a dyn Fn(&QualifiedName) -> bool,
    /// The seconds from the call's start to the core's: the host's checks,
    /// which the host stamps on `check`.
    pub before: f64,
    /// The plan, once [`computed_into`] has returned.
    pub plan: Option<crate::plan::Plan>,
}

/// The stamps of a run's stages into its plan.
struct Timer {
    plan: crate::plan::Plan,
    index: crate::plan::StageIndex,
    entered: Instant,
    before: f64,
}

impl Timer {
    /// The seconds from the call's start to `t`.
    fn at(&self, t: Instant) -> f64 {
        self.before + t.saturating_duration_since(self.entered).as_secs_f64()
    }

    /// Records the work of stage `i` from `a` to `b` in its first start, its
    /// last end, and its busy seconds.
    fn time(&mut self, i: usize, a: Instant, b: Instant) {
        let (s, e) = (self.at(a), self.at(b));
        let n = &mut self.plan.nodes[i];
        n.t_start = Some(n.t_start.map_or(s, |t| t.min(s)));
        n.t_end = Some(n.t_end.map_or(e, |t| t.max(e)));
        n.meta.busy = Some(n.meta.busy.unwrap_or(0.0) + (e - s));
    }

    /// Records the work of stage `i` from `a` to `b`, and marks it completed.
    fn stamp(&mut self, i: usize, a: Instant, b: Instant) {
        self.time(i, a, b);
        self.plan.nodes[i].status = crate::plan::Status::Completed;
    }

    /// Marks stage `i` failed, or the call where `i` is `None`, with its
    /// message and, for a call of a typed function, its group and first row.
    fn fail(
        &mut self,
        i: Option<usize>,
        message: String,
        group: Option<usize>,
        row: Option<usize>,
    ) {
        self.plan.nodes[0].status = crate::plan::Status::Failed;
        let n = &mut self.plan.nodes[i.unwrap_or(0)];
        n.status = crate::plan::Status::Failed;
        n.meta.message = Some(message);
        n.meta.group = group.map(|g| g as u64 + 1);
        n.meta.row = row.map(|r| r as u64 + 1);
    }

    /// The first `call` stage of expression `at` that has not run, of the
    /// function `function` where it is given.
    fn pending_call(&self, at: usize, function: Option<&str>) -> Option<usize> {
        self.index.calls.get(at)?.iter().copied().find(|&i| {
            let n = &self.plan.nodes[i];
            n.status == crate::plan::Status::Pending
                && function.is_none_or(|f| n.meta.function.as_deref() == Some(f))
        })
    }
}

/// The columns of [`computed`], each column of doubles written into the
/// buffer that `out` gives for it, where it gives one; that column's `data` is
/// `None`. With `trace`, the run builds its plan by
/// [`table_plan`](crate::plan::table_plan) from its own compiled expressions,
/// before it evaluates any, stamps the times of its stages, and leaves the
/// plan in `trace`, whether the run completes or fails.
#[allow(clippy::too_many_arguments)]
pub fn computed_into<'a, A: Access<'a>>(
    table: A,
    def: &TableDef,
    by: &[&str],
    exprs: &[&Logic],
    lookup: &dyn TypeLookup,
    functions: &dyn Functions,
    summary: bool,
    threads: usize,
    out: &mut dyn Output,
    trace: Option<&mut Trace<'_>>,
) -> Result<Outcome, NotComputed> {
    let entered = Instant::now();
    let mut timer: Option<Timer> = None;
    let result = prepared(table, def, by, |scope, rows, groups, begun| {
        let grouped = Instant::now();
        let compiled_exprs = compiled(exprs, scope, lookup, summary)?;
        let done_compiling = Instant::now();
        if let Some(t) = trace.as_deref() {
            let planned = Planned {
                rows,
                groups: groups.as_ref().map(|g| g.count),
                exprs: compiled_exprs.iter().map(|(_, w)| w.clone()).collect(),
                kernel_threads: super::parallel::threads_for(rows, threads.max(1)),
            };
            let (mut plan, index) =
                crate::plan::table_plan_indexed(&t.call, &planned, lookup, t.in_rust);
            plan.nodes[0].status = crate::plan::Status::Running;
            let mut tm = Timer {
                plan,
                index,
                entered,
                before: t.before,
            };
            if let Some(g) = tm.index.group {
                tm.stamp(g, begun, grouped);
            }
            tm.stamp(tm.index.compile, grouped, done_compiling);
            timer = Some(tm);
        }
        let mut columns = Vec::with_capacity(exprs.len());
        for (at, (calc, work)) in compiled_exprs.iter().enumerate() {
            let unit = match &calc.ty {
                Ty::Number { unit, .. } => unit.clone(),
                _ => None,
            };
            let kind = kind_of(&calc.ty, integer(&calc.rows.invariant, &calc.rows.cells));
            functions.expression(at);
            let stage_of = |slot: usize, tm: &Timer| -> Option<usize> {
                if let Some(k) = work.call_slots.iter().position(|&s| s == slot) {
                    return tm.index.calls.get(at).and_then(|c| c.get(k)).copied();
                }
                work.aggregate_slots
                    .contains(&slot)
                    .then_some(tm.index.aggregate)
                    .flatten()
            };
            let evaluated = {
                // A slot that fails keeps its stage's status for `fail()`.
                let mut done = |slot: usize, started: Instant, ok: bool| {
                    if let Some(tm) = timer.as_mut()
                        && let Some(i) = stage_of(slot, tm)
                    {
                        if ok {
                            tm.stamp(i, started, Instant::now());
                        } else {
                            tm.time(i, started, Instant::now());
                        }
                    }
                };
                Eval::calling_timed(
                    &calc.rows,
                    table,
                    rows,
                    groups.clone(),
                    threads,
                    functions,
                    &mut done,
                )
            };
            let eval = evaluated.map_err(|f| match f {
                Failed::Table(m) => NotComputed::Table(m),
                Failed::Row(error) => {
                    if let Some(tm) = timer.as_mut() {
                        let i = tm.pending_call(at, None);
                        tm.fail(i, error.message.to_string(), None, Some(error.row));
                    }
                    NotComputed::Row { at, error }
                }
                Failed::Call {
                    function,
                    group,
                    message,
                } => {
                    let first_row = groups.as_ref().map(|g| g.first[group] as usize);
                    if let Some(tm) = timer.as_mut() {
                        let i = tm.pending_call(at, Some(&function));
                        tm.fail(i, message.clone(), Some(group), first_row);
                    }
                    NotComputed::Call {
                        at,
                        function,
                        group,
                        first_row,
                        message,
                    }
                }
            })?;
            let n = if summary { eval.group_count() } else { rows };
            // A summary of a table of no rows, without keys, is one row.
            let n = if summary && groups.as_ref().is_some_and(|g| g.count == 0) {
                0
            } else {
                n
            };
            eval.set_over_groups(summary);
            let mut error: Option<RowError> = None;
            let buffer = if kind == Kind::Double {
                out.doubles(at, n)
            } else {
                None
            };
            let started = Instant::now();
            let data = match buffer {
                Some(buf) => {
                    for lo in (0..n).step_by(BLOCK) {
                        crate::progress::tick_block();
                        let hi = (lo + BLOCK).min(n);
                        let cells = &mut buf[lo..hi];
                        match eval
                            .numbers_into(&calc.rows.invariant, lo, hi, cells)
                            .map_err(NotComputed::Table)?
                        {
                            Placed::Written(states) => {
                                finish_doubles(cells, states.as_deref(), lo, &mut error)
                            }
                            Placed::Block(b) => write_doubles(cells, &b, lo, &mut error),
                        }
                    }
                    None
                }
                None => {
                    let mut data = empty(kind, n);
                    for lo in (0..n).step_by(BLOCK) {
                        crate::progress::tick_block();
                        let hi = (lo + BLOCK).min(n);
                        let b = eval
                            .block(&calc.rows.invariant, lo, hi)
                            .map_err(NotComputed::Table)?;
                        append(&mut data, &b, lo, hi - lo, &mut error);
                    }
                    Some(data)
                }
            };
            eval.set_over_groups(false);
            if let Some(tm) = timer.as_mut()
                && work.rows
                && let Some(i) = tm.index.evaluate
            {
                let ended = Instant::now();
                if let Some(e) = &error {
                    tm.time(i, started, ended);
                    tm.fail(Some(i), e.message.to_string(), None, Some(e.row));
                } else {
                    tm.stamp(i, started, ended);
                }
            }
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
    });
    if let Some(t) = trace {
        t.plan = Some(match timer {
            Some(mut tm) => {
                if let Err(e) = &result
                    && tm.plan.nodes[0].status != crate::plan::Status::Failed
                {
                    tm.fail(None, not_computed_text(e), None, None);
                }
                tm.plan
            }
            None => {
                let (stage, message) = match &result {
                    Err(NotComputed::Key(m)) => (Some("group"), m.clone()),
                    Err(NotComputed::Rule { errors, .. }) => (
                        Some("compile"),
                        errors
                            .first()
                            .map(|e| e.message.clone())
                            .unwrap_or_default(),
                    ),
                    Err(e) => (None, not_computed_text(e)),
                    Ok(_) => (None, String::new()),
                };
                crate::plan::failed_plan(&t.call, stage, &message)
            }
        });
    }
    result
}

/// The message of a run that computes no columns, for its plan.
fn not_computed_text(e: &NotComputed) -> String {
    match e {
        NotComputed::Rule { errors, .. } => errors
            .first()
            .map(|e| e.message.clone())
            .unwrap_or_default(),
        NotComputed::Row { error, .. } => error.message.to_string(),
        NotComputed::Call { message, .. } => message.clone(),
        NotComputed::Key(m) | NotComputed::Table(m) => m.clone(),
    }
}

/// What one expression of a call computes, before any row is evaluated.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct ExprWork {
    /// The typed functions that it calls, in the order of their calls.
    pub calls: Vec<QualifiedName>,
    /// For each call, whether its arguments read an aggregate.
    pub call_reads_aggregate: Vec<bool>,
    /// For each call, its place among the expression's aggregates and calls.
    pub call_slots: Vec<usize>,
    /// The places of its aggregates among its aggregates and calls.
    pub aggregate_slots: Vec<usize>,
    /// Its aggregates of the dialect.
    pub aggregates: usize,
    /// Its aggregates that run as a kernel over one column, which may take
    /// several threads: `sum()`, `mean()`, `min()`, or `max()` of a column.
    pub kernels: usize,
    /// It is evaluated for each row: an expression of a column of every row,
    /// whose aggregates and calls are over the row's group.
    pub rows: bool,
}

/// The expressions of a call compiled against a table and the groups of its
/// rows, before any row is evaluated.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Planned {
    /// The rows of the table.
    pub rows: usize,
    /// The number of groups, when keys are given.
    pub groups: Option<usize>,
    /// What each expression computes, in order.
    pub exprs: Vec<ExprWork>,
    /// The threads that an aggregate's kernel over the rows takes.
    pub kernel_threads: usize,
}

/// The work of `exprs` over a valid value of the table `def`, as
/// [`computed`] does it, without evaluating a row or calling a typed
/// function: the table is read, the keys are checked, the groups are
/// numbered, and each expression is compiled, with the errors of
/// [`computed`].
pub fn planned<'a, A: Access<'a>>(
    table: A,
    def: &TableDef,
    by: &[&str],
    exprs: &[&Logic],
    lookup: &dyn TypeLookup,
    summary: bool,
    threads: usize,
) -> Result<Planned, NotComputed> {
    prepared(table, def, by, |scope, rows, groups, _| {
        let exprs = compiled(exprs, scope, lookup, summary)?;
        Ok(Planned {
            rows,
            groups: groups.map(|g| g.count),
            exprs: exprs.into_iter().map(|(_, w)| w).collect(),
            kernel_threads: super::parallel::threads_for(rows, threads.max(1)),
        })
    })
}

/// Adds the calls and the aggregates of `e` to `w`.
fn work(e: &Expr, w: &mut ExprWork) {
    match e {
        Expr::Call {
            function,
            args,
            at: slot,
            ..
        } => {
            let at = w.calls.len();
            w.calls.push(function.clone());
            w.call_reads_aggregate.push(false);
            w.call_slots.push(*slot);
            let before = w.aggregates;
            for a in args {
                if let ArgValue::Cells { expr, .. } = &a.value {
                    work(expr, w);
                }
            }
            w.call_reads_aggregate[at] = w.aggregates > before;
        }
        Expr::Aggregate {
            op,
            operands,
            at: slot,
            ..
        } => {
            w.aggregates += 1;
            w.aggregate_slots.push(*slot);
            if matches!(op, Op::Sum | Op::Mean | Op::Min | Op::Max)
                && matches!(operands.as_slice(), [Expr::Cell(_)])
            {
                w.kernels += 1;
            }
            operands.iter().for_each(|o| work(o, w));
        }
        Expr::If(xs) | Expr::And(xs) | Expr::Or(xs) | Expr::Op(_, xs) => {
            xs.iter().for_each(|x| work(x, w));
        }
        Expr::IsNa(x) | Expr::IsNull(x) => work(x, w),
        _ => {}
    }
}
