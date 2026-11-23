//! The value-access trait: a read-only view of a value in R8's data model.
//!
//! [`Access`] is implemented once per host form of a value: [`Json`] reads a
//! value's JSON form, and the R package reads R objects in place. The
//! validator ([`crate::validate`]) and the rule evaluator are generic over the
//! trait, so every host checks the same constraints and rules, and errors
//! carry the JSON Pointer of the JSON form whatever the host form.
//!
//! An implementation reads its own form and reports a problem with that form
//! as a [`Form`] error, such as a date string that is not canonical or an R
//! value with extra attributes. The validator checks the declared constraints
//! on what the implementation reads: bounds, lengths, patterns, levels,
//! uniqueness, and rules.
//!
//! A handle is `Copy`. A handle that names an absent member is valid and
//! reports [`Access::is_absent`].
//!
//! The trait is public so that host bindings can implement it; its shape is
//! unstable until the Rust-stored values of M3.

use std::borrow::Cow;

use serde_json::{Map, Value};

use crate::canonical::{date_days, datetime_micros};
use crate::ir::{EnumDef, FieldType, VariantKind};
use crate::name::QualifiedName;
use crate::rules::TypeLookup;

/// A problem with a value's host form.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Form {
    /// A stable code, such as `invalid_date` or `r_attributes`.
    pub code: &'static str,
    /// A JSON Pointer suffix below the value read, empty for the value itself.
    pub suffix: String,
    /// The message.
    pub message: String,
    /// What the value fails and the value found, when the reader states the
    /// problem by its parts; `None` reports `message` as text. Boxed, so a
    /// read's result stays small.
    pub detail: Option<Box<(crate::problem::Problem, Option<crate::rules::Shown>)>>,
}

impl Form {
    /// A problem with the value itself, with the code `code`.
    pub fn new(code: &'static str, message: impl Into<String>) -> Self {
        Form {
            code,
            suffix: String::new(),
            message: message.into(),
            detail: None,
        }
    }

    /// A problem with the value itself, stated by its parts.
    pub fn problem(
        code: &'static str,
        problem: crate::problem::Problem,
        found: Option<crate::rules::Shown>,
    ) -> Self {
        let message = problem.sentence(&[], found.as_ref(), None, &crate::problem::Neutral);
        Form {
            code,
            suffix: String::new(),
            message,
            detail: Some(Box::new((problem, found))),
        }
    }

    /// The same problem, at `suffix` below the value read.
    pub fn at(mut self, suffix: impl Into<String>) -> Self {
        self.suffix = suffix.into();
        self
    }

    /// `type_mismatch`: the value is not of the expected kind.
    pub fn mismatch(want: &str, found: &str) -> Self {
        Form::problem(
            "type_mismatch",
            crate::problem::Problem::Kind(want.to_string()),
            Some(crate::rules::Shown::Kind(found.to_string())),
        )
    }
}

/// A read from a host form.
pub type Read<T> = Result<T, Form>;

/// Why a value of a `dyn()` or `subtype()` type does not name its type.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Unnamed {
    /// The value names no type, or one that is not known: the message.
    Unknown(String),
    /// The host's object is not a value of an R8 type: what it is.
    NotValue(String),
}

/// The names of a dense matrix's or array's dimensions: per dimension, a
/// handle over its names, or `None` for a dimension without names.
pub type Dimnames<A> = Vec<Option<Names<A>>>;

/// A sequence handle over the names of one dimension, and their number,
/// read in place; a missing name reads as `NA`.
pub struct Names<A> {
    /// A sequence handle over strings.
    pub seq: A,
    /// The number of names.
    pub len: usize,
}

impl<'a, A: Access<'a>> Names<A> {
    /// Calls `f` with consecutive blocks of at most 4096 names, in order.
    pub fn each_block(&self, mut f: impl FnMut(&[Cow<'a, str>])) {
        let mut buf: Vec<Cow<'a, str>> = Vec::with_capacity(self.len.min(BLOCK));
        let mut start = 0;
        while start < self.len {
            let end = (start + BLOCK).min(self.len);
            buf.clear();
            self.seq.each_character_in(start, end, |_, r| {
                buf.push(match r {
                    Ok(Some(s)) => s,
                    _ => Cow::Borrowed("NA"),
                })
            });
            f(&buf);
            start = end;
        }
    }

    /// Whether two sequences hold the same names.
    pub fn same<B: Access<'a>>(&self, other: &Names<B>) -> bool {
        if self.len != other.len {
            return false;
        }
        let mut start = 0;
        let mut same = true;
        self.each_block(|a| {
            if same {
                let end = start + a.len();
                let mut k = 0;
                other.seq.each_character_in(start, end, |_, r| {
                    let b = match &r {
                        Ok(Some(s)) => s.as_ref(),
                        _ => "NA",
                    };
                    same &= a[k] == b;
                    k += 1;
                });
                start = end;
            }
        });
        same
    }
}

/// Whether two matrices' dimension names are the same: equal in number, and
/// per dimension both absent or the same names.
pub fn same_dimnames<'a, A: Access<'a>>(x: &Option<Dimnames<A>>, y: &Option<Dimnames<A>>) -> bool {
    match (x, y) {
        (None, None) => true,
        (Some(x), Some(y)) => {
            x.len() == y.len()
                && x.iter().zip(y).all(|(a, b)| match (a, b) {
                    (None, None) => true,
                    (Some(a), Some(b)) => a.same(b),
                    _ => false,
                })
        }
        _ => false,
    }
}

/// A dense matrix or array: its shape and a sequence handle over its values
/// in column-major order.
pub struct Dense<A> {
    /// The extent of each dimension.
    pub shape: Vec<u64>,
    /// A sequence handle over the values.
    pub data: A,
    /// The names of the dimensions, when the matrix has them.
    pub dimnames: Option<Dimnames<A>>,
    /// The number of values `data` holds.
    pub len: usize,
    /// Problems with the host form that leave the data readable.
    pub problems: Vec<Form>,
}

/// A sparse matrix in compressed sparse column form, whose structure the
/// implementation has checked: its shape and a sequence handle over its
/// stored values.
pub struct Sparse<A> {
    /// The number of rows and columns.
    pub shape: [u64; 2],
    /// A sequence handle over the stored values.
    pub data: A,
    /// The number of stored values.
    pub len: usize,
    /// The `ncol + 1` offsets of the columns' values.
    pub indptr: Indices<A>,
    /// The row index of each value.
    pub indices: Indices<A>,
}

/// A sequence handle over the offsets or the row indices of a sparse
/// matrix, and their number. The validator checks the compressed sparse
/// column structure with [`csc_well_formed`]; an item that is not a
/// non-negative whole number reads as [`INVALID_INDEX`].
pub struct Indices<A> {
    /// A sequence handle over whole numbers.
    pub seq: A,
    /// The number of items.
    pub len: usize,
}

/// The message of the `sparse_form` problem that [`csc_well_formed`]
/// detects.
pub const CSC_FORM: &str = "compressed sparse column form: `indptr` has ncol + 1 non-decreasing offsets from 0 to the number of values, and each column's row `indices` increase and lie below nrow";

/// The value read for an item of [`Indices`] that is not a non-negative
/// whole number.
pub const INVALID_INDEX: u64 = u64::MAX;

impl<'a, A: Access<'a>> Indices<A> {
    /// The number of items.
    pub fn len(&self) -> usize {
        self.len
    }

    /// Whether the sequence has no items.
    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    /// Calls `f` with each item of `start..end`, in order.
    pub fn each_in(&self, start: usize, end: usize, mut f: impl FnMut(usize, u64)) {
        if let Some(s) = self.seq.i32_slice() {
            for (k, &x) in s[start..end].iter().enumerate() {
                f(start + k, u64::try_from(x).unwrap_or(INVALID_INDEX));
            }
            return;
        }
        self.seq.each_integer_in(start, end, |i, r| {
            f(
                i,
                match r {
                    Ok(Some(x)) => u64::try_from(x).unwrap_or(INVALID_INDEX),
                    _ => INVALID_INDEX,
                },
            )
        });
    }

    /// Reads items `start..start + buf.len()` into `buf`, and returns how
    /// many it read.
    pub fn read(&self, start: usize, buf: &mut [u64]) -> usize {
        let end = (start + buf.len()).min(self.len);
        self.each_in(start, end, |i, x| buf[i - start] = x);
        end.saturating_sub(start)
    }

    /// Calls `f` with consecutive blocks of at most 4096 items, in order.
    pub fn each_block(&self, mut f: impl FnMut(&[u64])) {
        let mut buf = vec![0u64; self.len.min(BLOCK)];
        let mut start = 0;
        while start < self.len {
            let got = self.read(start, &mut buf);
            if got == 0 {
                break;
            }
            f(&buf[..got]);
            start += got;
        }
    }

    /// Whether two sequences hold the same items.
    pub fn same<B: Access<'a>>(&self, other: &Indices<B>) -> bool {
        if self.len != other.len {
            return false;
        }
        let mut b = vec![0u64; self.len.min(BLOCK)];
        let mut start = 0;
        let mut same = true;
        self.each_block(|a| {
            if same {
                other.read(start, &mut b[..a.len()]);
                same = a == &b[..a.len()];
                start += a.len();
            }
        });
        same
    }
}

/// The number of items a reader of [`Indices`] holds at once.
const BLOCK: usize = 4096;

/// A table: its number of rows and its columns, each a sequence handle over
/// the column's cells, in the host's order.
pub struct Table<'a, A> {
    /// The number of rows.
    pub rows: u64,
    /// Each column's name and a sequence handle over its cells.
    pub columns: Vec<(Cow<'a, str>, A)>,
    /// Problems with the host form of the table as a whole.
    pub problems: Vec<Form>,
}

/// The 1-based codes of a factor: `i32::MIN` for a missing cell, and, in
/// codes read from another form, 0 for a cell whose problem is among the
/// factor's `problems`. The validator checks that every other code lies in
/// `1..=levels.len()`; readers of an unvalidated value check each lookup.
pub enum Codes<A> {
    /// A sequence handle over the host's integer codes, and their number,
    /// read in place by `each_in()`.
    Host(A, usize),
    /// Codes read from another form.
    Owned(Vec<i32>),
}

/// The marker of a missing cell among a factor's codes.
pub const MISSING_CODE: i32 = i32::MIN;

impl<'a, A: Access<'a>> Codes<A> {
    /// The number of cells.
    pub fn len(&self) -> usize {
        match self {
            Codes::Host(_, n) => *n,
            Codes::Owned(v) => v.len(),
        }
    }

    /// Whether the factor has no cells.
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// The codes as one array, where the host holds them as one.
    pub fn slice<'s>(&'s self) -> Option<&'s [i32]>
    where
        'a: 's,
    {
        match self {
            Codes::Host(a, _) => a.i32_slice(),
            Codes::Owned(v) => Some(v),
        }
    }

    /// Calls `f` with the code of each cell of `start..end`, in order.
    pub fn each_in(&self, start: usize, end: usize, mut f: impl FnMut(usize, i32)) {
        if let Some(s) = self.slice() {
            for (k, &c) in s[start..end].iter().enumerate() {
                f(start + k, c);
            }
            return;
        }
        if let Codes::Host(a, _) = self {
            a.each_integer_in(start, end, |i, r| {
                f(
                    i,
                    match r {
                        Ok(Some(v)) => i32::try_from(v).unwrap_or(0),
                        Ok(None) => MISSING_CODE,
                        Err(_) => 0,
                    },
                )
            });
        }
    }

    /// Reads the codes of cells `start..start + buf.len()` into `buf`, and
    /// returns how many it read.
    pub fn read(&self, start: usize, buf: &mut [i32]) -> usize {
        let end = (start + buf.len()).min(self.len());
        self.each_in(start, end, |i, c| buf[i - start] = c);
        end.saturating_sub(start)
    }
}

/// A factor: its levels and the code of each cell.
pub struct Factor<'a, A> {
    /// The levels, in order.
    pub levels: Vec<Cow<'a, str>>,
    /// The code of each cell.
    pub codes: Codes<A>,
    /// Problems with the host form that leave the other cells readable.
    pub problems: Vec<Form>,
}

impl<A> Factor<'_, A> {
    /// The level of code `c`, `None` for a missing cell or a code outside
    /// the levels.
    pub fn level(&self, c: i32) -> Option<&str> {
        usize::try_from(c)
            .ok()
            .and_then(|c| c.checked_sub(1))
            .and_then(|k| self.levels.get(k))
            .map(|l| l.as_ref())
    }
}

/// A read-only view of a value in one host form.
pub trait Access<'a>: Copy + 'a {
    /// The value is absent: an omitted member or `null` in JSON; `NULL`, or
    /// `NA` as an element of a vector, in R.
    fn is_absent(self) -> bool;

    /// A logical.
    fn logical(self) -> Read<bool>;
    /// A whole number; the validator checks the integer range.
    fn integer(self) -> Read<i64>;
    /// A double, non-finite values included; the validator checks `finite`.
    fn double(self) -> Read<f64>;
    /// A string.
    fn character(self) -> Read<Cow<'a, str>>;
    /// Days since 1970-01-01.
    fn date(self) -> Read<i64>;
    /// Microseconds since 1970-01-01T00:00:00Z.
    fn datetime(self) -> Read<i64>;
    /// A `type_raw()` value in its host form.
    fn raw(self) -> Read<()>;
    /// The bytes of a `type_raw()` value.
    fn bytes(self) -> Read<Cow<'a, [u8]>>;
    /// A value of `type_json()`.
    fn json(self) -> Read<()>;
    /// A present value of a host-only field, an object of the class `class`
    /// of the host `host`.
    fn host_class(self, host: &str, class: &str) -> Read<()>;

    /// A `vec()` of `element`: the number of items.
    fn vec(self, element: &FieldType) -> Read<usize>;
    /// Item `i` of a sequence: a `vec()`, the data of a matrix, or a column.
    fn item(self, i: usize) -> Self;

    /// The doubles of a sequence held as one array, where the host form has
    /// one; a missing item is R's `NA_real_`, the `NaN` whose low word is
    /// 1954. The validator scans the array and reads items one by one only
    /// to report a problem.
    fn f64_slice(self) -> Option<&'a [f64]> {
        None
    }

    /// The whole numbers of a sequence held as one array of `i32`, where the
    /// host form has one; a missing item is `i32::MIN`.
    fn i32_slice(self) -> Option<&'a [i32]> {
        None
    }

    /// Copies doubles of a sequence from item `start` into `buf`, where the
    /// host form reads a sequence by region (an R vector that ALTREP holds),
    /// and returns how many it copied; `None` where it does not.
    fn f64_region(self, _start: usize, _buf: &mut [f64]) -> Option<usize> {
        None
    }

    /// Copies whole numbers of a sequence from item `start` into `buf`, as
    /// [`Access::f64_region`] copies doubles; a missing item is `i32::MIN`.
    fn i32_region(self, _start: usize, _buf: &mut [i32]) -> Option<usize> {
        None
    }

    /// `Some(true)` when the host records the sequence as sorted in
    /// increasing order and without missing items, `Some(false)` sorted in
    /// decreasing order and without missing items, and `None` otherwise. Its
    /// smallest and largest items are then its first and last.
    fn sorted_without_na(self) -> Option<bool> {
        None
    }

    /// Items `0..n` of a sequence of doubles, `Ok(None)` for a missing item.
    fn each_double(self, n: usize, f: impl FnMut(usize, Read<Option<f64>>)) {
        self.each_double_in(0, n, f)
    }

    /// Items `start..end` of a sequence of `each_double`'s kind.
    fn each_double_in(self, start: usize, end: usize, mut f: impl FnMut(usize, Read<Option<f64>>)) {
        for i in start..end {
            let x = self.item(i);
            f(
                i,
                if x.is_absent() {
                    Ok(None)
                } else {
                    x.double().map(Some)
                },
            );
        }
    }

    /// Items `0..n` of a sequence of whole numbers.
    fn each_integer(self, n: usize, f: impl FnMut(usize, Read<Option<i64>>)) {
        self.each_integer_in(0, n, f)
    }

    /// Items `start..end` of a sequence of `each_integer`'s kind.
    fn each_integer_in(
        self,
        start: usize,
        end: usize,
        mut f: impl FnMut(usize, Read<Option<i64>>),
    ) {
        for i in start..end {
            let x = self.item(i);
            f(
                i,
                if x.is_absent() {
                    Ok(None)
                } else {
                    x.integer().map(Some)
                },
            );
        }
    }

    /// Items `0..n` of a sequence of logicals.
    fn each_logical(self, n: usize, f: impl FnMut(usize, Read<Option<bool>>)) {
        self.each_logical_in(0, n, f)
    }

    /// Items `start..end` of a sequence of `each_logical`'s kind.
    fn each_logical_in(
        self,
        start: usize,
        end: usize,
        mut f: impl FnMut(usize, Read<Option<bool>>),
    ) {
        for i in start..end {
            let x = self.item(i);
            f(
                i,
                if x.is_absent() {
                    Ok(None)
                } else {
                    x.logical().map(Some)
                },
            );
        }
    }

    /// Items `0..n` of a sequence of strings.
    fn each_character(self, n: usize, f: impl FnMut(usize, Read<Option<Cow<'a, str>>>)) {
        self.each_character_in(0, n, f)
    }

    /// Items `start..end` of a sequence of `each_character`'s kind.
    fn each_character_in(
        self,
        start: usize,
        end: usize,
        mut f: impl FnMut(usize, Read<Option<Cow<'a, str>>>),
    ) {
        for i in start..end {
            let x = self.item(i);
            f(
                i,
                if x.is_absent() {
                    Ok(None)
                } else {
                    x.character().map(Some)
                },
            );
        }
    }

    /// Items `0..n` of a sequence of dates.
    fn each_date(self, n: usize, f: impl FnMut(usize, Read<Option<i64>>)) {
        self.each_date_in(0, n, f)
    }

    /// Items `start..end` of a sequence of `each_date`'s kind.
    fn each_date_in(self, start: usize, end: usize, mut f: impl FnMut(usize, Read<Option<i64>>)) {
        for i in start..end {
            let x = self.item(i);
            f(
                i,
                if x.is_absent() {
                    Ok(None)
                } else {
                    x.date().map(Some)
                },
            );
        }
    }

    /// Items `0..n` of a sequence of date-times.
    fn each_datetime(self, n: usize, f: impl FnMut(usize, Read<Option<i64>>)) {
        self.each_datetime_in(0, n, f)
    }

    /// Items `start..end` of a sequence of `each_datetime`'s kind.
    fn each_datetime_in(
        self,
        start: usize,
        end: usize,
        mut f: impl FnMut(usize, Read<Option<i64>>),
    ) {
        for i in start..end {
            let x = self.item(i);
            f(
                i,
                if x.is_absent() {
                    Ok(None)
                } else {
                    x.datetime().map(Some)
                },
            );
        }
    }

    /// A `btree_map()` (`ordered = false`) or an `index_map()` of `value`:
    /// the number of entries.
    fn map(self, value: &FieldType, ordered: bool) -> Read<usize>;
    /// Each entry of a map, in the host's order: its key and its value. A
    /// [`Form`] error's suffix is relative to the entry.
    fn each_entry(self, f: impl FnMut(usize, Read<(Cow<'a, str>, Self)>));

    /// A factor; `declared` holds the levels its type declares, and
    /// `ordered` whether it is an ordered factor.
    fn factor(self, declared: Option<&[Box<str>]>, ordered: bool) -> Read<Factor<'a, Self>>;
    /// A matrix or an array of `element`.
    fn dense(self, element: &FieldType) -> Read<Dense<Self>>;
    /// A sparse matrix of `element`.
    fn sparse(self, element: &FieldType) -> Read<Sparse<Self>>;
    /// A table whose type declares `columns`; `open` admits other columns.
    fn table<C: crate::ir::Column>(self, columns: &[C], open: bool) -> Read<Table<'a, Self>>;
    /// The type of a column of an open table that the declaration does not
    /// list, from the host form.
    fn extra_column_type(self, name: &str) -> Read<FieldType>;
    /// Row `i` of a table, whose members are the row's cells.
    fn row(self, i: usize) -> Self;

    /// A value of the struct `name`.
    fn record(self, name: &QualifiedName) -> Read<()>;
    /// The member `name` of a struct value, a struct variant, or a row.
    fn member(self, name: &str) -> Self;
    /// The member `name` of a record, which the declaration lists at
    /// `index`; a host form that keeps members in declaration order reads
    /// that position first.
    fn member_at(self, _index: usize, name: &str) -> Self {
        self.member(name)
    }
    /// The names of the members present, for the check of unknown members.
    fn members(self) -> Vec<Cow<'a, str>>;
    /// The variant of a value of the enum `name`.
    fn variant(self, name: &QualifiedName, def: &EnumDef) -> Read<Cow<'a, str>>;
    /// The concrete type of a `dyn(trait)` value, and the member of the
    /// value that names it, if the host form has one.
    fn implementor(
        self,
        r#trait: &QualifiedName,
        lookup: &dyn TypeLookup,
    ) -> Result<(QualifiedName, Option<String>), Unnamed>;
    /// The type a value of a `subtype(Parent)` field names, and the member of
    /// the value that names it, if the host form has one.
    fn named_type(self) -> Result<(QualifiedName, Option<String>), Unnamed>;

    /// Whether two handles name the same object, which is then equal to
    /// itself without a read.
    fn same(self, _other: Self) -> bool {
        false
    }

    /// Whether two values of a host-only field are identical in the host.
    fn identical(self, other: Self) -> bool {
        self.to_value(&FieldType::Json, &NoTypes) == other.to_value(&FieldType::Json, &NoTypes)
    }

    /// The value's JSON form, for the rule evaluator and for export.
    fn to_value(self, ft: &FieldType, lookup: &dyn TypeLookup) -> Value;
}

/// The concrete type of a value of the field type `ft`, and the member of
/// the value that names it: a trait's implementor for `dyn(Trait)`, the named
/// struct for `subtype(Parent)`, and `None` for any other field type.
pub fn concrete<'a, A: Access<'a>>(
    v: A,
    ft: &FieldType,
    lookup: &dyn TypeLookup,
) -> Option<Result<(QualifiedName, Option<String>), Unnamed>> {
    match ft {
        FieldType::Dyn { r#trait } => Some(v.implementor(r#trait, lookup)),
        FieldType::Subtype { .. } => Some(v.named_type()),
        _ => None,
    }
}

/// A lookup that holds no type.
struct NoTypes;

impl TypeLookup for NoTypes {
    fn find(&self, _name: &QualifiedName) -> Option<&crate::ir::TypeDef> {
        None
    }
}

/// R's `NA_real_`: a `NaN` whose low word is 1954.
#[inline(always)]
pub fn is_na_real(x: f64) -> bool {
    x.is_nan() && (x.to_bits() as u32) == 1954
}

/// The kind of a JSON value, for messages.
pub fn json_kind(v: &Value) -> &'static str {
    match v {
        Value::Null => "null",
        Value::Bool(_) => "a logical",
        Value::Number(_) => "a number",
        Value::String(_) => "a string",
        Value::Array(_) => "an array",
        Value::Object(_) => "an object",
    }
}

fn is_leap(y: u32) -> bool {
    (y.is_multiple_of(4) && !y.is_multiple_of(100)) || y.is_multiple_of(400)
}

fn digits(s: &str) -> Option<u32> {
    (!s.is_empty() && s.bytes().all(|b| b.is_ascii_digit())).then(|| s.parse().ok())?
}

/// The range of `type_posixct()`: microsecond counts of magnitude below 2^52,
/// which a double holds exactly as seconds.
pub const POSIXCT_MIN: &str = "1827-04-16T00:06:12.629505Z";
/// The latest value of `type_posixct()`.
pub const POSIXCT_MAX: &str = "2112-09-17T23:53:47.370495Z";
/// The largest magnitude of a `type_posixct()` value in microseconds.
pub const POSIXCT_MICROS: i64 = (1 << 52) - 1;
/// The days of 0000-01-01 and 9999-12-31, the range of `type_date()`.
pub const DATE_MIN_DAYS: i64 = -719_528;
/// The day of 9999-12-31.
pub const DATE_MAX_DAYS: i64 = 2_932_896;

/// A full-date `YYYY-MM-DD` in the proleptic Gregorian calendar, year 0000 to
/// 9999.
pub fn valid_date(s: &str) -> bool {
    let b = s.as_bytes();
    if b.len() != 10 || b[4] != b'-' || b[7] != b'-' {
        return false;
    }
    let (Some(y), Some(m), Some(d)) = (digits(&s[0..4]), digits(&s[5..7]), digits(&s[8..10]))
    else {
        return false;
    };
    let days = match m {
        1 | 3 | 5 | 7 | 8 | 10 | 12 => 31,
        4 | 6 | 9 | 11 => 30,
        2 if is_leap(y) => 29,
        2 => 28,
        _ => return false,
    };
    (1..=days).contains(&d)
}

/// The canonical date-time form, `YYYY-MM-DDTHH:MM:SS.ffffffZ`, within the
/// domain of `type_posixct()`.
pub fn valid_posixct(s: &str) -> bool {
    let b = s.as_bytes();
    if b.len() != 27
        || b[10] != b'T'
        || b[13] != b':'
        || b[16] != b':'
        || b[19] != b'.'
        || b[26] != b'Z'
    {
        return false;
    }
    let ok = valid_date(&s[0..10])
        && digits(&s[11..13]).is_some_and(|h| h <= 23)
        && digits(&s[14..16]).is_some_and(|m| m <= 59)
        && digits(&s[17..19]).is_some_and(|x| x <= 59)
        && digits(&s[20..26]).is_some();
    ok && (POSIXCT_MIN..=POSIXCT_MAX).contains(&s)
}

/// Canonical base64 (RFC 4648 section 4): the standard alphabet, padding to
/// a multiple of four, and zero bits after the last character's data.
pub fn valid_base64(s: &str) -> bool {
    let b = s.as_bytes();
    if !b.len().is_multiple_of(4) {
        return false;
    }
    let pad = b.iter().rev().take_while(|&&c| c == b'=').count();
    if pad > 2 {
        return false;
    }
    let val = |c: u8| match c {
        b'A'..=b'Z' => Some(c - b'A'),
        b'a'..=b'z' => Some(c - b'a' + 26),
        b'0'..=b'9' => Some(c - b'0' + 52),
        b'+' => Some(62),
        b'/' => Some(63),
        _ => None,
    };
    let body = &b[..b.len() - pad];
    if body.iter().any(|&c| val(c).is_none()) {
        return false;
    }
    match (pad, body.last()) {
        (1, Some(&c)) => val(c).unwrap() & 0b11 == 0,
        (2, Some(&c)) => val(c).unwrap() & 0b1111 == 0,
        _ => true,
    }
}

/// Whether compressed sparse column offsets and row indices describe a
/// matrix of `shape` with `nnz` stored values: `ncol + 1` non-decreasing
/// offsets from 0 to `nnz`, and within each column, row indices that
/// increase and lie below `nrow`. Reads both sequences once, in blocks.
pub fn csc_well_formed<'a, A: Access<'a>>(
    shape: [u64; 2],
    indptr: &Indices<A>,
    indices: &Indices<A>,
    nnz: u64,
) -> bool {
    if indptr.len() as u64 != shape[1].saturating_add(1) || indices.len() as u64 != nnz {
        return false;
    }
    let mut rows = vec![0u64; indices.len().min(BLOCK)];
    // `rows` holds the items `held..held + got` of `indices`.
    let (mut held, mut got) = (0usize, 0usize);
    let mut pos = 0usize;
    let mut prev: Option<u64> = None;
    let mut ok = true;
    indptr.each_block(|block| {
        for &p in block {
            if !ok {
                return;
            }
            match prev {
                None if p != 0 => ok = false,
                Some(q) if p < q || p > nnz => ok = false,
                _ => {
                    let mut last: Option<u64> = None;
                    while (pos as u64) < p {
                        if pos >= held + got {
                            held = pos;
                            got = indices.read(pos, &mut rows);
                            if got == 0 {
                                ok = false;
                                return;
                            }
                        }
                        let r = rows[pos - held];
                        if r >= shape[0] || last.is_some_and(|l| r <= l) {
                            ok = false;
                            return;
                        }
                        last = Some(r);
                        pos += 1;
                    }
                }
            }
            prev = Some(p);
        }
    });
    ok && prev == Some(nnz)
}

/// The bytes of canonical padded base64 text, which [`valid_base64`] has
/// checked.
pub fn base64_decode(s: &str) -> Vec<u8> {
    let val = |c: u8| -> u32 {
        match c {
            b'A'..=b'Z' => u32::from(c - b'A'),
            b'a'..=b'z' => u32::from(c - b'a' + 26),
            b'0'..=b'9' => u32::from(c - b'0' + 52),
            b'+' => 62,
            b'/' => 63,
            _ => 0,
        }
    };
    let b = s.as_bytes();
    let mut out = Vec::with_capacity(b.len() / 4 * 3);
    for chunk in b.chunks(4) {
        let pad = chunk.iter().filter(|&&c| c == b'=').count();
        let n = chunk.iter().fold(0u32, |acc, &c| (acc << 6) | val(c));
        let bytes = [(n >> 16) as u8, (n >> 8) as u8, n as u8];
        out.extend_from_slice(&bytes[..3 - pad]);
    }
    out
}

/// Canonical padded base64 text of `bytes` (RFC 4648 section 4).
pub fn base64_encode(bytes: &[u8]) -> String {
    const A: &[u8; 64] = b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
    let mut out = String::with_capacity(bytes.len().div_ceil(3) * 4);
    for chunk in bytes.chunks(3) {
        let n = chunk
            .iter()
            .enumerate()
            .fold(0u32, |acc, (i, &c)| acc | (u32::from(c) << (16 - 8 * i)));
        for i in 0..4 {
            if i <= chunk.len() {
                out.push(A[((n >> (18 - 6 * i)) & 63) as usize] as char);
            } else {
                out.push('=');
            }
        }
    }
    out
}

fn esc(s: &str) -> String {
    s.replace('~', "~0").replace('/', "~1")
}

// --------------------------------------------------------------------- JSON

/// A JSON node that [`JsonOf`] reads: a `serde_json::Value`, or a node of
/// the compact tree of a document read (`crate::tree`). One implementation
/// of the value-access trait serves both.
pub trait Node<'a>: Copy + 'a {
    /// `null`.
    fn is_null(self) -> bool;
    /// A boolean.
    fn as_bool(self) -> Option<bool>;
    /// A JSON number, as a double.
    fn number(self) -> Option<f64>;
    /// A JSON number that is a non-negative integer.
    fn as_u64(self) -> Option<u64>;
    /// A string.
    fn as_str(self) -> Option<&'a str>;
    /// An array's length; `None` for any other node.
    fn array_len(self) -> Option<usize>;
    /// An array's item `i`.
    fn item(self, i: usize) -> Option<Self>;
    /// An object's member `key`.
    fn get(self, key: &str) -> Option<Self>;
    /// An object's members, in order; `None` for any other node.
    fn entries(self) -> Option<Vec<(&'a str, Self)>>;
    /// An object's number of members; `None` for any other node.
    fn object_len(self) -> Option<usize>;
    /// The node's kind, for a message: "null", "a number", and so on.
    fn kind(self) -> &'static str;
    /// The doubles of an array that the node holds as one run.
    fn f64_slice(self) -> Option<&'a [f64]> {
        None
    }
    /// The two handles name the same node.
    fn same(self, other: Self) -> bool;
    /// Runs `f` on the node as a `serde_json::Value`, for the operations
    /// whose interfaces take one: a `dyn()` value's implementor and an open
    /// table's column type.
    fn with_value<R>(self, f: impl FnOnce(&Value) -> R) -> R;
    /// The node as an owned `serde_json::Value`.
    fn to_value(self) -> Value;

    /// A JSON number.
    fn is_number(self) -> bool {
        self.number().is_some()
    }
    /// A string.
    fn is_string(self) -> bool {
        self.as_str().is_some()
    }
    /// An array.
    fn is_array(self) -> bool {
        self.array_len().is_some()
    }
}

impl<'a> Node<'a> for &'a Value {
    fn is_null(self) -> bool {
        Value::is_null(self)
    }
    fn as_bool(self) -> Option<bool> {
        Value::as_bool(self)
    }
    fn number(self) -> Option<f64> {
        match self {
            Value::Number(n) => n.as_f64(),
            _ => None,
        }
    }
    fn as_u64(self) -> Option<u64> {
        Value::as_u64(self)
    }
    fn as_str(self) -> Option<&'a str> {
        Value::as_str(self)
    }
    fn array_len(self) -> Option<usize> {
        Value::as_array(self).map(Vec::len)
    }
    fn item(self, i: usize) -> Option<Self> {
        Value::as_array(self).and_then(|a| a.get(i))
    }
    fn get(self, key: &str) -> Option<Self> {
        Value::as_object(self).and_then(|m| m.get(key))
    }
    fn entries(self) -> Option<Vec<(&'a str, Self)>> {
        Value::as_object(self).map(|m| m.iter().map(|(k, v)| (k.as_str(), v)).collect())
    }
    fn object_len(self) -> Option<usize> {
        Value::as_object(self).map(Map::len)
    }
    fn kind(self) -> &'static str {
        json_kind(self)
    }
    fn same(self, other: Self) -> bool {
        std::ptr::eq(self, other)
    }
    fn with_value<R>(self, f: impl FnOnce(&Value) -> R) -> R {
        f(self)
    }
    fn to_value(self) -> Value {
        self.clone()
    }
}

/// A handle into a value's JSON form, over the nodes `N`.
#[derive(Debug, Clone, Copy)]
pub enum JsonOf<N> {
    /// A JSON value.
    V(N),
    /// An omitted member.
    Missing,
    /// Row `i` of a table object.
    Row(N, usize),
    /// Cell `i` of a table column: an array, or a factor's
    /// `{"levels", "codes"}`.
    Cell(N, usize),
}

/// A handle into a `serde_json::Value`.
pub type Json<'a> = JsonOf<&'a Value>;

impl<'a> Json<'a> {
    /// A handle to a JSON value.
    pub fn new(v: &'a Value) -> Self {
        JsonOf::V(v)
    }
}

impl<'a, N: Node<'a>> JsonOf<N> {
    /// A handle to a node.
    pub fn of(n: N) -> Self {
        JsonOf::V(n)
    }

    /// The node the handle names, if it names one.
    fn val(self) -> Option<N> {
        match self {
            JsonOf::V(v) => Some(v),
            JsonOf::Cell(c, i) if c.is_array() => c.item(i),
            _ => None,
        }
    }

    fn kind(self) -> &'static str {
        match self {
            JsonOf::Row(..) => "a table row",
            JsonOf::Cell(c, _) if c.object_len().is_some() => "a factor code",
            _ => self.val().map_or("null", Node::kind),
        }
    }

    fn mismatch(self, want: &str) -> Form {
        Form::mismatch(want, self.kind())
    }

    fn string(self, want: &str) -> Read<&'a str> {
        self.val()
            .and_then(Node::as_str)
            .ok_or_else(|| self.mismatch(want))
    }
}

impl<'a, N: Node<'a> + std::fmt::Debug> Access<'a> for JsonOf<N> {
    fn is_absent(self) -> bool {
        match self {
            JsonOf::Missing => true,
            JsonOf::Row(..) => false,
            JsonOf::Cell(c, i) if c.object_len().is_some() => c
                .get("codes")
                .and_then(|codes| codes.item(i))
                .is_none_or(Node::is_null),
            _ => self.val().is_none_or(Node::is_null),
        }
    }

    fn logical(self) -> Read<bool> {
        self.val()
            .and_then(Node::as_bool)
            .ok_or_else(|| self.mismatch("a logical"))
    }

    fn integer(self) -> Read<i64> {
        let x = self
            .val()
            .and_then(Node::number)
            .ok_or_else(|| self.mismatch("an integer"))?;
        if x.fract() != 0.0 {
            return Err(Form::new(
                "not_integer",
                format!("{x} is not a whole number"),
            ));
        }
        Ok(x as i64)
    }

    fn double(self) -> Read<f64> {
        let v = self.val();
        if let Some(x) = v.and_then(Node::number) {
            return Ok(x);
        }
        match v.and_then(Node::as_str) {
            Some("Infinity") => Ok(f64::INFINITY),
            Some("-Infinity") => Ok(f64::NEG_INFINITY),
            Some("NaN") => Ok(f64::NAN),
            _ => Err(self.mismatch("a number")),
        }
    }

    fn character(self) -> Read<Cow<'a, str>> {
        self.string("a string").map(Cow::Borrowed)
    }

    fn date(self) -> Read<i64> {
        let s = self.string("a date string")?;
        if valid_date(s) {
            Ok(date_days(s))
        } else {
            Err(Form::new(
                "invalid_date",
                format!("`{s}` is not a date of the form YYYY-MM-DD"),
            ))
        }
    }

    fn datetime(self) -> Read<i64> {
        let s = self.string("a date-time string")?;
        if valid_posixct(s) {
            Ok(datetime_micros(s))
        } else {
            Err(Form::new(
                "invalid_posixct",
                format!(
                    "`{s}` is not a date-time of the form YYYY-MM-DDTHH:MM:SS.ffffffZ from {POSIXCT_MIN} to {POSIXCT_MAX}"
                ),
            ))
        }
    }

    fn raw(self) -> Read<()> {
        let s = self.string("a base64 string")?;
        if valid_base64(s) {
            Ok(())
        } else {
            Err(Form::new(
                "invalid_base64",
                "bytes are written in canonical padded base64 (RFC 4648 section 4)",
            ))
        }
    }

    fn bytes(self) -> Read<Cow<'a, [u8]>> {
        self.raw()?;
        let s = self.string("a base64 string")?;
        Ok(Cow::Owned(base64_decode(s)))
    }

    fn json(self) -> Read<()> {
        Ok(())
    }

    fn host_class(self, _host: &str, _class: &str) -> Read<()> {
        Err(Form::new(
            "host_only",
            "a host-only R value has no JSON form; the member is null or omitted",
        ))
    }

    fn vec(self, _element: &FieldType) -> Read<usize> {
        self.val()
            .and_then(Node::array_len)
            .ok_or_else(|| self.mismatch("an array"))
    }

    fn item(self, i: usize) -> Self {
        match self.val().and_then(|v| v.item(i)) {
            Some(x) => JsonOf::V(x),
            None => JsonOf::Missing,
        }
    }

    fn f64_slice(self) -> Option<&'a [f64]> {
        self.val().and_then(Node::f64_slice)
    }

    fn map(self, _value: &FieldType, ordered: bool) -> Read<usize> {
        let v = self.val();
        match (
            ordered,
            v.and_then(Node::object_len),
            v.and_then(Node::array_len),
        ) {
            (false, Some(n), _) => Ok(n),
            (true, _, Some(n)) => Ok(n),
            (false, ..) => Err(self.mismatch("an object")),
            (true, ..) => Err(self.mismatch("an array of {\"name\", \"value\"} pairs")),
        }
    }

    fn each_entry(self, mut f: impl FnMut(usize, Read<(Cow<'a, str>, Self)>)) {
        let Some(v) = self.val() else { return };
        if let Some(members) = v.entries() {
            for (i, (k, x)) in members.into_iter().enumerate() {
                f(i, Ok((Cow::Borrowed(k), JsonOf::V(x))));
            }
            return;
        }
        let Some(n) = v.array_len() else { return };
        for i in 0..n {
            let pair = v.item(i).expect("an item below the array's length");
            let entry = match (pair.object_len(), pair.get("name"), pair.get("value")) {
                (Some(2), Some(name), Some(value)) => match name.as_str() {
                    Some(k) => Ok((Cow::Borrowed(k), JsonOf::V(value))),
                    None => Err(Form::mismatch("a string key", name.kind()).at("/name")),
                },
                _ => Err(Form::new(
                    "type_mismatch",
                    "an ordered map entry is an object with exactly `name` and `value`",
                )),
            };
            f(i, entry);
        }
    }

    fn factor(self, declared: Option<&[Box<str>]>, _ordered: bool) -> Read<Factor<'a, Self>> {
        let form = || {
            Form::new(
                "factor_form",
                "a factor whose levels come from the data is {\"levels\": [...], \"codes\": [...]}",
            )
        };
        let v = self.val();
        match (
            declared,
            v.and_then(Node::array_len),
            v.and_then(Node::object_len),
        ) {
            (Some(_), Some(n), _) => {
                let v = v.expect("an array");
                let mut levels: Vec<Cow<'a, str>> = Vec::new();
                let mut index: std::collections::HashMap<&'a str, i32> =
                    std::collections::HashMap::new();
                let mut codes = Vec::with_capacity(n);
                let mut problems = Vec::new();
                for i in 0..n {
                    let x = v.item(i).expect("an item below the array's length");
                    if x.is_null() {
                        codes.push(MISSING_CODE);
                    } else if let Some(s) = x.as_str() {
                        let code = *index.entry(s).or_insert_with(|| {
                            levels.push(Cow::Borrowed(s));
                            levels.len() as i32
                        });
                        codes.push(code);
                    } else {
                        problems.push(Form::mismatch("a level", x.kind()).at(format!("/{i}")));
                        codes.push(0);
                    }
                }
                Ok(Factor {
                    levels,
                    codes: Codes::Owned(codes),
                    problems,
                })
            }
            (None, _, Some(len)) => {
                let m = v.expect("an object");
                let (Some(lv), Some(codes)) = (
                    m.get("levels").filter(|l| l.is_array()),
                    m.get("codes").filter(|c| c.is_array()),
                ) else {
                    return Err(form());
                };
                let mut problems = Vec::new();
                if len != 2 {
                    problems.push(Form::new(
                        "factor_form",
                        "a factor holds `levels` and `codes` only",
                    ));
                }
                let n_levels = lv.array_len().unwrap_or(0);
                let level_nodes: Vec<N> = (0..n_levels).filter_map(|i| lv.item(i)).collect();
                if level_nodes.iter().any(|l| !l.is_string()) {
                    problems.push(Form::new("factor_form", "levels are strings").at("/levels"));
                }
                let levels = level_nodes
                    .iter()
                    .map(|l| match l.as_str() {
                        Some(s) => Cow::Borrowed(s),
                        None => Cow::Owned(l.to_value().to_string()),
                    })
                    .collect();
                let n_codes = codes.array_len().unwrap_or(0);
                let mut out = Vec::with_capacity(n_codes);
                for i in 0..n_codes {
                    let c = codes.item(i).expect("an item below the array's length");
                    match c.as_u64() {
                        Some(k) if k >= 1 && (k as usize) <= n_levels => out.push(k as i32),
                        _ if c.is_null() => out.push(MISSING_CODE),
                        _ => {
                            problems.push(
                                Form::new(
                                    "invalid_code",
                                    format!("a code is an integer from 1 to {n_levels}"),
                                )
                                .at(format!("/codes/{i}")),
                            );
                            out.push(0);
                        }
                    }
                }
                Ok(Factor {
                    levels,
                    codes: Codes::Owned(out),
                    problems,
                })
            }
            (Some(_), ..) => Err(Form::new(
                "factor_form",
                "a factor with declared levels is a string array",
            )),
            (None, ..) => Err(form()),
        }
    }

    fn dense(self, _element: &FieldType) -> Read<Dense<Self>> {
        let m = self
            .val()
            .filter(|v| v.object_len().is_some())
            .ok_or_else(|| self.mismatch("an object with `shape`"))?;
        let shape = m
            .get("shape")
            .and_then(|s| {
                (0..s.array_len()?)
                    .map(|i| s.item(i).and_then(Node::as_u64))
                    .collect::<Option<Vec<u64>>>()
            })
            .ok_or_else(|| {
                Form::new(
                    "matrix_form",
                    "`shape` is an array of non-negative integers",
                )
            })?;
        let mut problems = Vec::new();
        if m.entries()
            .unwrap_or_default()
            .iter()
            .any(|(k, _)| !matches!(*k, "shape" | "data" | "dimnames"))
        {
            problems.push(Form::new(
                "matrix_form",
                "a matrix holds `shape`, `data`, and optionally `dimnames`",
            ));
        }
        let data = m
            .get("data")
            .filter(|d| d.is_array())
            .ok_or_else(|| Form::new("matrix_form", "`data` is an array"))?;
        let dn = m.get("dimnames");
        if let Some(dn) = dn {
            let ok = dn.array_len().is_some_and(|n| {
                n == shape.len()
                    && shape.iter().enumerate().all(|(i, &len)| {
                        let names = dn.item(i).expect("an item below the array's length");
                        names.is_null()
                            || names.array_len().is_some_and(|a| {
                                a as u64 == len
                                    && (0..a).all(|j| names.item(j).is_some_and(Node::is_string))
                            })
                    })
            });
            if !ok {
                problems.push(
                    Form::new(
                        "matrix_form",
                        "`dimnames` holds, per dimension, null or one string per index",
                    )
                    .at("/dimnames"),
                );
            }
        }
        let dimnames = dn.filter(|d| d.is_array()).map(|d| {
            (0..d.array_len().unwrap_or(0))
                .map(|i| {
                    let names = d.item(i).expect("an item below the array's length");
                    names.array_len().map(|len| Names {
                        seq: JsonOf::V(names),
                        len,
                    })
                })
                .collect()
        });
        Ok(Dense {
            shape,
            len: data.array_len().unwrap_or(0),
            data: JsonOf::V(data),
            dimnames,
            problems,
        })
    }

    fn sparse(self, _element: &FieldType) -> Read<Sparse<Self>> {
        let m = self
            .val()
            .filter(|v| v.object_len().is_some())
            .ok_or_else(|| self.mismatch("an object with `shape`"))?;
        let ints = |k: &str| {
            m.get(k).and_then(|a| {
                (0..a.array_len()?)
                    .map(|i| a.item(i).and_then(Node::as_u64))
                    .collect::<Option<Vec<u64>>>()
            })
        };
        let shape = ints("shape").ok_or_else(|| {
            Form::new(
                "matrix_form",
                "`shape` is an array of non-negative integers",
            )
        })?;
        if shape.len() != 2 {
            return Err(
                Form::new("matrix_form", "a sparse matrix has two dimensions").at("/shape"),
            );
        }
        let shape = [shape[0], shape[1]];
        let array = |k: &str| m.get(k).filter(|d| d.is_array());
        let (Some(indptr), Some(indices), Some(data)) =
            (array("indptr"), array("indices"), array("data"))
        else {
            return Err(Form::new(
                "sparse_form",
                "a sparse matrix holds `shape`, `indptr`, `indices`, and `data`",
            ));
        };
        let len = |v: N| v.array_len().unwrap_or(0);
        Ok(Sparse {
            shape,
            data: JsonOf::V(data),
            len: len(data),
            indptr: Indices {
                seq: JsonOf::V(indptr),
                len: len(indptr),
            },
            indices: Indices {
                seq: JsonOf::V(indices),
                len: len(indices),
            },
        })
    }

    fn table<C: crate::ir::Column>(self, columns: &[C], open: bool) -> Read<Table<'a, Self>> {
        let m = self
            .val()
            .filter(|v| v.object_len().is_some())
            .ok_or_else(|| self.mismatch("a table object"))?;
        let rows = m.get("$length").and_then(Node::as_u64).ok_or_else(|| {
            Form::new("table_form", "a table holds `$length`, its number of rows")
        })?;
        let mut problems = Vec::new();
        if let Some(t) = m.get("$columns") {
            match t.entries().filter(|_| open) {
                None => problems.push(
                    Form::new(
                        "table_form",
                        "`$columns` belongs to an open table and is an object of column types",
                    )
                    .at("/$columns"),
                ),
                Some(listed) => {
                    let declared = |k: &str| columns.iter().any(|c| c.field().name.as_str() == k);
                    for (k, _) in listed
                        .into_iter()
                        .filter(|(k, _)| m.get(k).is_none() || declared(k))
                    {
                        problems.push(
                            Form::new(
                                "column_type",
                                format!("`$columns` lists `{k}`, which is not an extra column of the table"),
                            )
                            .at(format!("/$columns/{}", esc(k))),
                        );
                    }
                }
            }
        }
        let columns = m
            .entries()
            .unwrap_or_default()
            .into_iter()
            .filter(|(k, _)| !k.starts_with('$'))
            .map(|(k, v)| (Cow::Borrowed(k), JsonOf::V(v)))
            .collect();
        Ok(Table {
            rows,
            columns,
            problems,
        })
    }

    fn extra_column_type(self, name: &str) -> Read<FieldType> {
        let listed = self
            .val()
            .and_then(|v| v.get("$columns"))
            .and_then(|t| t.get(name));
        let fail = || {
            Form::new(
                "column_type",
                format!("column `{name}` of an open table needs its type in `$columns`: a scalar, a factor, or option() of one"),
            )
            .at(format!("/$columns/{}", esc(name)))
        };
        match listed.map(|t| t.with_value(|v| <FieldType as serde::Deserialize>::deserialize(v))) {
            Some(Ok(ft)) => Ok(ft),
            _ => Err(fail()),
        }
    }

    fn row(self, i: usize) -> Self {
        match self.val() {
            Some(v) if v.object_len().is_some() => JsonOf::Row(v, i),
            _ => JsonOf::Missing,
        }
    }

    fn record(self, _name: &QualifiedName) -> Read<()> {
        match self.val() {
            Some(v) if v.object_len().is_some() => Ok(()),
            _ => Err(self.mismatch("an object")),
        }
    }

    fn member(self, name: &str) -> Self {
        match self {
            JsonOf::Row(m, i) => m.get(name).map_or(JsonOf::Missing, |c| JsonOf::Cell(c, i)),
            _ => match self.val().and_then(|v| v.get(name)) {
                Some(x) => JsonOf::V(x),
                None => JsonOf::Missing,
            },
        }
    }

    fn members(self) -> Vec<Cow<'a, str>> {
        self.val()
            .and_then(Node::entries)
            .map(|m| m.into_iter().map(|(k, _)| Cow::Borrowed(k)).collect())
            .unwrap_or_default()
    }

    fn variant(self, name: &QualifiedName, def: &EnumDef) -> Read<Cow<'a, str>> {
        let unit_only = def
            .variants
            .iter()
            .all(|x| matches!(x.kind, VariantKind::Unit));
        let tag = def.tag.as_str();
        // Inside `dyn()`, a unit-only enum's value is an object holding the
        // type member and its tag.
        let object = self.val().is_some_and(|v| v.object_len().is_some());
        if unit_only && !object {
            return self
                .string(&format!("a variant name of `{name}`"))
                .map(Cow::Borrowed);
        }
        match self.val() {
            Some(v) if v.object_len().is_some() => match v.get(tag).and_then(Node::as_str) {
                Some(s) => Ok(Cow::Borrowed(s)),
                None => Err(Form::new(
                    "missing_tag",
                    format!("a `{name}` value names its variant in `{tag}`"),
                )),
            },
            _ => Err(self.mismatch(&format!("a `{name}` object"))),
        }
    }

    fn implementor(
        self,
        r#trait: &QualifiedName,
        lookup: &dyn TypeLookup,
    ) -> Result<(QualifiedName, Option<String>), Unnamed> {
        let found = match self.val() {
            Some(v) => v.with_value(|v| lookup.implementor(r#trait, v)),
            None => lookup.implementor(r#trait, &Value::Null),
        };
        found
            .map(|(ty, member)| (ty, Some(member)))
            .map_err(Unnamed::Unknown)
    }

    fn named_type(self) -> Result<(QualifiedName, Option<String>), Unnamed> {
        let found = match self.val() {
            Some(v) => v.with_value(crate::rules::dollar_type),
            None => crate::rules::dollar_type(&Value::Null),
        };
        found
            .map(|(ty, member)| (ty, Some(member)))
            .map_err(Unnamed::Unknown)
    }

    fn same(self, other: Self) -> bool {
        match (self, other) {
            (JsonOf::V(a), JsonOf::V(b)) => a.same(b),
            _ => false,
        }
    }

    fn to_value(self, _ft: &FieldType, _lookup: &dyn TypeLookup) -> Value {
        match self {
            JsonOf::Missing => Value::Null,
            JsonOf::Row(m, i) => Value::Object(
                m.entries()
                    .unwrap_or_default()
                    .into_iter()
                    .filter(|(k, _)| !k.starts_with('$'))
                    .map(|(k, c)| (k.to_string(), JsonOf::Cell(c, i).to_value(_ft, _lookup)))
                    .collect(),
            ),
            JsonOf::Cell(c, i) if c.object_len().is_some() => {
                let level = c.get("levels").and_then(|levels| {
                    let code = c.get("codes")?.item(i)?.as_u64()?;
                    (code >= 1).then(|| levels.item(code as usize - 1))?
                });
                level.map_or(Value::Null, Node::to_value)
            }
            _ => self.val().map_or(Value::Null, Node::to_value),
        }
    }
}
