//! The cells of a delimited text as the kinds of their columns.

use std::collections::HashMap;

use rtemis_r8::abi::NA_DOUBLE_BITS;
use rtemis_r8::progress;

use crate::csv::{Cell, Csv, Malformed};
use crate::stored::{Stored, Zone};
use crate::text;

/// A missing integer, date, logical, or factor code.
pub const MISSING_INTEGER: i32 = i32::MIN;

/// A missing date-time.
pub const MISSING_DATETIME: i64 = i64::MIN;

/// A missing double, which is apart from `NaN`.
pub fn missing_double() -> f64 {
    f64::from_bits(NA_DOUBLE_BITS)
}

/// The most texts that do not read that a column lists, each once.
const UNREAD_TEXTS: usize = 1000;

/// The first rows that a text that does not read lists.
const UNREAD_ROWS: usize = 10;

/// The most rows of another width that are listed.
const WIDTHS: usize = 100;

/// The greatest whole number below which a double holds every one, 2^53.
const WHOLE: i64 = 9_007_199_254_740_992;

/// The domain of R8's integers, -2147483647 to 2147483647.
fn in_domain(v: i64) -> bool {
    v.unsigned_abs() <= i32::MAX as u64
}

/// How the cells of a file are read.
#[derive(Debug, Clone)]
pub struct ReadOptions {
    /// The most cells that do not read that each column lists, with their rows.
    pub limit: usize,
    /// The zone of the date-times without an offset.
    pub zone: Zone,
}

impl Default for ReadOptions {
    fn default() -> Self {
        ReadOptions {
            limit: 1000,
            zone: Zone::utc(),
        }
    }
}

/// What a column is read as.
#[derive(Debug, Clone, PartialEq)]
pub enum Kind {
    /// The kind that all its cells have, or text.
    Infer,
    /// A logical.
    Logical,
    /// An integer.
    Integer,
    /// A double.
    Double,
    /// A text.
    Text,
    /// A date.
    Date,
    /// A date-time.
    DateTime,
    /// One of the levels.
    Factor(Vec<String>),
}

/// The value that a text is read as.
#[derive(Debug, Clone, PartialEq)]
pub enum Scalar {
    /// A logical.
    Logical(bool),
    /// An integer.
    Integer(i32),
    /// A double.
    Double(f64),
    /// A text, or the label of a factor.
    Text(String),
    /// The days since 1970-01-01.
    Date(i32),
    /// The microseconds since 1970-01-01T00:00:00Z.
    DateTime(i64),
}

/// How a column is read.
#[derive(Debug, Clone)]
pub struct ColumnSpec {
    /// What its cells are.
    pub kind: Kind,
    /// The texts of a missing cell of any column. In a column of text or of a
    /// factor, a quoted empty cell and a quoted `NA` are texts.
    pub missing: Vec<String>,
    /// The texts of a missing cell of this column, where it is quoted too.
    pub missing_here: Vec<String>,
    /// The values that texts are read as.
    pub recode: Vec<(String, Scalar)>,
}

impl ColumnSpec {
    /// A column of `kind`, whose empty cells are missing.
    pub fn new(kind: Kind) -> ColumnSpec {
        ColumnSpec {
            kind,
            missing: vec![String::new()],
            missing_here: Vec::new(),
            recode: Vec::new(),
        }
    }
}

/// A value of [`ColumnSpec::recode`] that does not apply.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Refused {
    /// The kind of the column is inferred.
    Inferred,
    /// The text has two values.
    Twice,
    /// The text is one of a missing cell.
    Missing,
    /// The text reads as the kind of the column.
    Reads,
    /// The value is not of the kind of the column.
    Kind,
}

impl ColumnSpec {
    /// The first place of `recode` whose text or value does not apply, and why.
    pub fn refused(&self) -> Option<(usize, Refused)> {
        let levels: HashMap<&str, i32> = match &self.kind {
            Kind::Factor(l) => l.iter().zip(1..).map(|(l, k)| (l.as_str(), k)).collect(),
            _ => HashMap::new(),
        };
        self.recode.iter().enumerate().find_map(|(k, (t, v))| {
            let mut cells = Column::new(&self.kind);
            let why = if self.kind == Kind::Infer {
                Refused::Inferred
            } else if self.recode[..k].iter().any(|(u, _)| u == t) {
                Refused::Twice
            } else if self.missing.contains(t) || self.missing_here.contains(t) {
                Refused::Missing
            } else if self.kind != Kind::Text && cells.push(t, true, &levels, &Zone::utc()) {
                Refused::Reads
            } else if !cells.push_scalar(v, &levels) {
                Refused::Kind
            } else {
                return None;
            };
            Some((k, why))
        })
    }
}

/// The texts of a column.
#[derive(Debug, Clone, Default, PartialEq)]
pub struct Texts {
    bytes: String,
    /// The end of each text in `bytes`, with the highest bit set where it is missing.
    ends: Vec<u64>,
}

impl Texts {
    const MISSING: u64 = 1 << 63;

    fn push(&mut self, t: &str) {
        self.bytes.push_str(t);
        self.ends.push(self.bytes.len() as u64);
    }

    fn push_missing(&mut self) {
        self.ends.push(self.bytes.len() as u64 | Texts::MISSING);
    }

    /// The number of cells.
    pub fn len(&self) -> usize {
        self.ends.len()
    }

    /// Whether there is no cell.
    pub fn is_empty(&self) -> bool {
        self.ends.is_empty()
    }

    /// The text of a cell, `None` for a missing one.
    pub fn get(&self, i: usize) -> Option<&str> {
        let end = self.ends[i];
        if end & Texts::MISSING != 0 {
            return None;
        }
        let start = match i {
            0 => 0,
            _ => self.ends[i - 1] & !Texts::MISSING,
        };
        Some(&self.bytes[start as usize..end as usize])
    }
}

/// The cells of a column.
#[derive(Debug, Clone, PartialEq)]
pub enum Column {
    /// 0 and 1, or [`MISSING_INTEGER`].
    Logical(Vec<i32>),
    /// Integers, or [`MISSING_INTEGER`].
    Integer(Vec<i32>),
    /// Doubles, or [`missing_double`].
    Double(Vec<f64>),
    /// Texts.
    Text(Texts),
    /// The days since 1970-01-01, or [`MISSING_INTEGER`].
    Date(Vec<i32>),
    /// The microseconds since 1970-01-01T00:00:00Z, or [`MISSING_DATETIME`].
    DateTime(Vec<i64>),
    /// The places of the levels, from 1, or [`MISSING_INTEGER`].
    Factor(Vec<i32>),
}

impl Column {
    fn new(kind: &Kind) -> Column {
        match kind {
            Kind::Infer | Kind::Logical => Column::Logical(Vec::new()),
            Kind::Integer => Column::Integer(Vec::new()),
            Kind::Double => Column::Double(Vec::new()),
            Kind::Text => Column::Text(Texts::default()),
            Kind::Date => Column::Date(Vec::new()),
            Kind::DateTime => Column::DateTime(Vec::new()),
            Kind::Factor(_) => Column::Factor(Vec::new()),
        }
    }

    /// The number of cells.
    pub fn len(&self) -> usize {
        match self {
            Column::Logical(x) | Column::Integer(x) | Column::Date(x) | Column::Factor(x) => {
                x.len()
            }
            Column::Double(x) => x.len(),
            Column::Text(x) => x.len(),
            Column::DateTime(x) => x.len(),
        }
    }

    /// Whether there is no cell.
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    fn push_missing(&mut self) {
        match self {
            Column::Logical(x) | Column::Integer(x) | Column::Date(x) | Column::Factor(x) => {
                x.push(MISSING_INTEGER);
            }
            Column::Double(x) => x.push(missing_double()),
            Column::Text(x) => x.push_missing(),
            Column::DateTime(x) => x.push(MISSING_DATETIME),
        }
    }

    /// Reads the text of a cell; `false` where it does not read. A date-time without
    /// an offset is the clock reading of `zone`.
    fn push(&mut self, t: &str, declared: bool, levels: &HashMap<&str, i32>, zone: &Zone) -> bool {
        match self {
            Column::Text(x) => x.push(t),
            Column::Logical(x) => match text::logical(text::trimmed(t)) {
                Some(v) => x.push(i32::from(v)),
                None => return false,
            },
            Column::Integer(x) => match text::integer(text::trimmed(t), declared) {
                Some(v) => x.push(v),
                None => return false,
            },
            Column::Double(x) => match text::double(text::trimmed(t), declared) {
                Some(v) => x.push(v),
                None => return false,
            },
            Column::Date(x) => match text::date(text::trimmed(t)) {
                Some(v) => x.push(v),
                None => return false,
            },
            Column::DateTime(x) => {
                let t = text::trimmed(t);
                match text::datetime(t).or_else(|| text::clock(t).and_then(|c| zone.instant(c))) {
                    Some(v) => x.push(v),
                    None => return false,
                }
            }
            Column::Factor(x) => match levels.get(t) {
                Some(&v) => x.push(v),
                None => return false,
            },
        }
        true
    }

    /// Adds a value that a text is read as; one of another kind does not read.
    fn push_scalar(&mut self, v: &Scalar, levels: &HashMap<&str, i32>) -> bool {
        match (self, v) {
            (Column::Logical(x), Scalar::Logical(v)) => x.push(i32::from(*v)),
            (Column::Integer(x), Scalar::Integer(v)) | (Column::Date(x), Scalar::Date(v)) => {
                x.push(*v);
            }
            (Column::Double(x), Scalar::Double(v)) => x.push(*v),
            (Column::Double(x), Scalar::Integer(v)) => x.push(f64::from(*v)),
            (Column::Text(x), Scalar::Text(v)) => x.push(v),
            (Column::DateTime(x), Scalar::DateTime(v)) => x.push(*v),
            (Column::Factor(x), Scalar::Text(v)) => match levels.get(v.as_str()) {
                Some(&k) => x.push(k),
                None => return false,
            },
            _ => return false,
        }
        true
    }

    /// Adds a typed value where it is one of the column's kind exactly: a whole
    /// double as an integer, an integer within 2^53 as a double, a date-time at
    /// midnight as a date, any value as its text; `false` otherwise.
    fn push_stored(&mut self, s: &Stored<'_>, levels: &HashMap<&str, i32>, zone: &Zone) -> bool {
        const DAY: i64 = 86_400_000_000;
        match (self, s) {
            (Column::Logical(x), Stored::Logical(b)) => x.push(i32::from(*b)),
            (Column::Integer(x), Stored::Integer(v)) if in_domain(*v) => x.push(*v as i32),
            (Column::Integer(x), Stored::Double(v))
                if v.fract() == 0.0 && v.abs() <= f64::from(i32::MAX) =>
            {
                x.push(*v as i32);
            }
            (Column::Double(x), Stored::Double(v)) => x.push(*v),
            (Column::Double(x), Stored::Integer(v)) if v.unsigned_abs() <= WHOLE as u64 => {
                x.push(*v as f64);
            }
            (Column::Date(x), Stored::Date(d)) => x.push(*d),
            (Column::Date(x), Stored::DateTime(m) | Stored::Clock(m)) if m % DAY == 0 => {
                match i32::try_from(m / DAY) {
                    Ok(d) if d != MISSING_INTEGER => x.push(d),
                    _ => return false,
                }
            }
            (Column::DateTime(x), Stored::DateTime(m)) if *m != MISSING_DATETIME => x.push(*m),
            (Column::DateTime(x), Stored::Clock(m)) => match zone.instant(*m) {
                Some(v) if v != MISSING_DATETIME => x.push(v),
                _ => return false,
            },
            (Column::Text(x), s) => match s.text() {
                Some(t) => x.push(&t),
                None => return false,
            },
            (Column::Factor(x), s) => {
                match s.text().and_then(|t| levels.get(t.as_ref()).copied()) {
                    Some(k) => x.push(k),
                    None => return false,
                }
            }
            _ => return false,
        }
        true
    }

    /// The columns that an inferred one tries in order, after this one.
    fn wider(&self) -> Option<Column> {
        Some(match self {
            Column::Logical(_) => Column::Integer(Vec::new()),
            Column::Integer(_) => Column::Double(Vec::new()),
            Column::Double(_) => Column::Date(Vec::new()),
            Column::Date(_) => Column::DateTime(Vec::new()),
            Column::DateTime(_) => Column::Text(Texts::default()),
            Column::Text(_) | Column::Factor(_) => return None,
        })
    }
}

/// A text that does not read as its column, with its cells.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Unread {
    /// The place of the column, from 0.
    pub column: usize,
    /// The text; `None` for those past the most that a column lists.
    pub text: Option<String>,
    /// The number of its cells.
    pub cells: u64,
    /// Its first rows, from 0.
    pub rows: Vec<u64>,
}

/// A cell that does not read as its column.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnreadCell {
    /// The place of the column, from 0.
    pub column: usize,
    /// The row, from 0.
    pub row: u64,
    /// The text.
    pub text: String,
}

/// A text that was read as a given value.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Recoded {
    /// The place of the column, from 0.
    pub column: usize,
    /// The place in [`ColumnSpec::recode`], from 0.
    pub text: usize,
    /// The number of its cells.
    pub cells: u64,
}

/// A line that has another number of cells than the header.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Width {
    /// The line, from 1.
    pub line: u64,
    /// Its cells.
    pub cells: usize,
}

/// What was read.
#[derive(Debug, Clone, Default)]
pub struct Read {
    /// The number of rows, without those of another width.
    pub rows: u64,
    /// The columns, in the order of the header.
    pub columns: Vec<Column>,
    /// The texts that do not read, by column, and by their first rows.
    pub unread: Vec<Unread>,
    /// The first cells that do not read of each column, by column and row.
    pub unread_cells: Vec<UnreadCell>,
    /// The texts that were read as given values, those without a cell included.
    pub recoded: Vec<Recoded>,
    /// The lines of another width, the first of them.
    pub widths: Vec<Width>,
    /// The number of lines of another width.
    pub other_widths: u64,
}

/// A column while it is read.
struct Reading<'s> {
    spec: &'s ColumnSpec,
    zone: &'s Zone,
    cells: Column,
    levels: HashMap<&'s str, i32>,
    /// The length of the longest text of a missing cell.
    longest_missing: usize,
    /// The missing codes that may be the text of a typed value.
    typed_codes: Vec<&'s str>,
    /// Whether the kind of the column is that of its stored cells.
    typed: bool,
    /// Whether a cell was read that is not missing.
    seen: bool,
    /// Whether a missing cell was quoted, which may be a text.
    quoted: bool,
    /// Whether the column is read again as text.
    again: bool,
    /// Whether the column is read in this pass.
    active: bool,
    unread: HashMap<String, (u64, Vec<u64>)>,
    unread_cells: Vec<(u64, String)>,
    more: (u64, Vec<u64>),
    recoded: Vec<u64>,
}

/// What the stored cells of a column are, as far as they are read.
#[derive(Clone, Copy, PartialEq)]
enum Seen {
    Logical,
    /// Whole numbers, and whether all are integers and all within 2^53.
    Integer(bool, bool),
    Double,
    Date,
    DateTime,
    /// Clock readings, and whether the zone shows each once.
    Clock(bool),
    Text,
}

impl<'s> Reading<'s> {
    fn new(spec: &'s ColumnSpec, zone: &'s Zone) -> Reading<'s> {
        let levels = match &spec.kind {
            Kind::Factor(l) => l.iter().zip(1..).map(|(l, k)| (l.as_str(), k)).collect(),
            _ => HashMap::new(),
        };
        let codes = || spec.missing.iter().chain(&spec.missing_here);
        Reading {
            spec,
            zone,
            cells: Column::new(&spec.kind),
            levels,
            longest_missing: codes().map(String::len).max().unwrap_or(0),
            // No typed value has the text "" or "NA", or one that begins with
            // another character than these.
            typed_codes: codes()
                .filter(|c| {
                    c.as_str() != "NA"
                        && c.starts_with(|k: char| k.is_ascii_digit() || "-TFNI".contains(k))
                })
                .map(String::as_str)
                .collect(),
            typed: false,
            seen: false,
            quoted: false,
            again: false,
            active: true,
            unread: HashMap::new(),
            unread_cells: Vec::new(),
            more: (0, Vec::new()),
            recoded: vec![0; spec.recode.len()],
        }
    }

    fn missing(&self, t: &str, quoted: bool) -> bool {
        if t.len() > self.longest_missing {
            return false;
        }
        let text = matches!(self.cells, Column::Text(_) | Column::Factor(_));
        let says_text = quoted && text && (t.is_empty() || t == "NA");
        (!says_text && self.spec.missing.iter().any(|m| m == t))
            || self.spec.missing_here.iter().any(|m| m == t)
    }

    /// Whether a stored cell is missing: a null, or the text of a missing code.
    fn stored_missing(&self, s: &Stored<'_>) -> bool {
        match s {
            Stored::Missing => true,
            Stored::Text { text, quoted } => self.missing(text, *quoted),
            _ => {
                !self.typed_codes.is_empty()
                    && s.text()
                        .is_some_and(|t| self.typed_codes.contains(&t.as_ref()))
            }
        }
    }

    fn cell(&mut self, c: &Cell<'_>, row: u64, limit: usize) {
        self.text_cell(&c.text(), c.quoted, row, limit);
    }

    fn text_cell(&mut self, t: &str, quoted: bool, row: u64, limit: usize) {
        if self.missing(t, quoted) {
            self.quoted |= quoted;
            return self.cells.push_missing();
        }
        if let Some(k) = self.spec.recode.iter().position(|(from, _)| *from == *t)
            && self.cells.push_scalar(&self.spec.recode[k].1, &self.levels)
        {
            self.recoded[k] += 1;
            return;
        }
        if self.typed || self.spec.kind != Kind::Infer || matches!(self.cells, Column::Text(_)) {
            if !self.cells.push(t, true, &self.levels, self.zone) {
                self.cells.push_missing();
                self.unreadable(t, row, limit);
            }
            return;
        }
        self.inferred(t);
    }

    /// Reads a stored cell. A typed value that is not of the column's kind exactly is
    /// read by its text, as `recode` names it, or does not read.
    fn stored(&mut self, s: &Stored<'_>, row: u64, limit: usize) {
        match s {
            Stored::Missing => return self.cells.push_missing(),
            Stored::Text { text, quoted } => return self.text_cell(text, *quoted, row, limit),
            _ => {}
        }
        if self.stored_missing(s) {
            return self.cells.push_missing();
        }
        if self.cells.push_stored(s, &self.levels, self.zone) {
            return;
        }
        let t = s.text().unwrap_or_default();
        if let Some(k) = self.spec.recode.iter().position(|(from, _)| *from == *t)
            && self.cells.push_scalar(&self.spec.recode[k].1, &self.levels)
        {
            self.recoded[k] += 1;
            return;
        }
        self.cells.push_missing();
        self.unreadable(&t, row, limit);
    }

    /// Gives an inferred column the kind of its stored cells: logical, integer,
    /// double, date, or date-time where every cell that is not missing has it, an
    /// integer within 2^53 a double, and text otherwise. A column without such a
    /// cell has the kind `empty`.
    fn infer<'c>(&mut self, cells: impl Iterator<Item = Stored<'c>>, empty: &Kind) {
        if self.spec.kind != Kind::Infer {
            return;
        }
        self.typed = true;
        let mut seen: Option<Seen> = None;
        for s in cells {
            if self.stored_missing(&s) {
                continue;
            }
            let now = match &s {
                Stored::Missing => continue,
                Stored::Text { .. } => Seen::Text,
                Stored::Logical(_) => Seen::Logical,
                Stored::Integer(v) => {
                    Seen::Integer(in_domain(*v), v.unsigned_abs() <= WHOLE as u64)
                }
                Stored::Double(_) => Seen::Double,
                Stored::Date(_) => Seen::Date,
                Stored::DateTime(_) => Seen::DateTime,
                Stored::Clock(m) => Seen::Clock(self.zone.instant(*m).is_some()),
            };
            seen = Some(match (seen, now) {
                (None, now) => now,
                (Some(Seen::Integer(a, b)), Seen::Integer(c, d)) => Seen::Integer(a && c, b && d),
                (Some(Seen::Clock(a)), Seen::Clock(b)) => Seen::Clock(a && b),
                (Some(was), now) if was == now => now,
                _ => Seen::Text,
            });
            if seen == Some(Seen::Text) {
                break;
            }
        }
        let kind = match seen {
            None => empty.clone(),
            Some(Seen::Logical) => Kind::Logical,
            Some(Seen::Integer(true, _)) => Kind::Integer,
            Some(Seen::Integer(false, true) | Seen::Double) => Kind::Double,
            Some(Seen::Date) => Kind::Date,
            Some(Seen::DateTime | Seen::Clock(true)) => Kind::DateTime,
            Some(Seen::Integer(false, false) | Seen::Clock(false) | Seen::Text) => Kind::Text,
        };
        self.cells = Column::new(&kind);
    }

    /// Reads a cell of a column whose kind is inferred: the first kind of those in
    /// order that the cells read so far have, too.
    fn inferred(&mut self, t: &str) {
        while !self.cells.push(t, false, &self.levels, self.zone) {
            let Some(mut wider) = self.cells.wider() else {
                return;
            };
            if self.seen {
                // The doubles are the integers that were read.
                if let (Column::Integer(x), Column::Double(d)) = (&self.cells, &mut wider)
                    && text::double(text::trimmed(t), false).is_some()
                {
                    d.extend(x.iter().map(|&v| match v {
                        MISSING_INTEGER => missing_double(),
                        v => f64::from(v),
                    }));
                    self.cells = wider;
                    continue;
                }
                // The texts of the cells that were read are read again.
                self.again = true;
                self.active = false;
                return;
            }
            if self.quoted && matches!(wider, Column::Text(_)) {
                self.again = true;
                self.active = false;
                return;
            }
            for _ in 0..self.cells.len() {
                wider.push_missing();
            }
            self.cells = wider;
        }
        self.seen = true;
    }

    fn unreadable(&mut self, t: &str, row: u64, limit: usize) {
        if self.unread_cells.len() < limit {
            self.unread_cells.push((row, t.to_string()));
        }
        let full = self.unread.len() == UNREAD_TEXTS;
        let of = match self.unread.get_mut(t) {
            Some(of) => of,
            None if full => &mut self.more,
            None => self.unread.entry(t.to_string()).or_default(),
        };
        of.0 += 1;
        if of.1.len() < UNREAD_ROWS {
            of.1.push(row);
        }
    }
}

/// Adds the columns that were read to `out`, with their texts that do not read and
/// those that were recoded.
fn finish(columns: Vec<Reading<'_>>, out: &mut Read) {
    for (k, c) in columns.into_iter().enumerate() {
        let mut of: Vec<Unread> = c
            .unread
            .into_iter()
            .map(|(t, (cells, rows))| Unread {
                column: k,
                text: Some(t),
                cells,
                rows,
            })
            .collect();
        of.sort_by(|a, b| (&a.rows, &a.text).cmp(&(&b.rows, &b.text)));
        out.unread.extend(of);
        if c.more.0 > 0 {
            out.unread.push(Unread {
                column: k,
                text: None,
                cells: c.more.0,
                rows: c.more.1,
            });
        }
        out.unread_cells
            .extend(c.unread_cells.into_iter().map(|(row, text)| UnreadCell {
                column: k,
                row,
                text,
            }));
        out.recoded.extend(
            c.recoded
                .into_iter()
                .enumerate()
                .map(|(text, cells)| Recoded {
                    column: k,
                    text,
                    cells,
                }),
        );
        out.columns.push(c.cells);
    }
}

/// Reads columns of stored cells as the columns of `specs`, with at most
/// `options.limit` cells that do not read of each column. `cells(k)` gives the cells
/// of column `k`, the same each time it is called: twice for a column whose kind is
/// inferred, which has the kind of its stored cells, and `empty` without a cell.
/// Every column has `rows` cells.
pub fn read_stored<'c, I>(
    specs: &[ColumnSpec],
    rows: u64,
    mut cells: impl FnMut(usize) -> I,
    empty: impl Fn(usize) -> Kind,
    options: &ReadOptions,
) -> Read
where
    I: Iterator<Item = Stored<'c>>,
{
    let mut columns: Vec<Reading<'_>> = specs
        .iter()
        .map(|s| Reading::new(s, &options.zone))
        .collect();
    for (k, c) in columns.iter_mut().enumerate() {
        c.infer(cells(k), &empty(k));
        for (row, s) in cells(k).enumerate() {
            progress::tick();
            c.stored(&s, row as u64, options.limit);
        }
    }
    let mut out = Read {
        rows,
        ..Read::default()
    };
    finish(columns, &mut out);
    out
}

impl Csv<'_> {
    /// The cells, as the columns of `specs`, one for each name of the header, with
    /// at most `options.limit` cells that do not read of each column.
    ///
    /// # Panics
    ///
    /// Where `specs` has another length than the header.
    pub fn read(&self, specs: &[ColumnSpec], options: &ReadOptions) -> Result<Read, Malformed> {
        assert_eq!(specs.len(), self.header().len(), "one column for each name");
        let mut columns: Vec<Reading<'_>> = specs
            .iter()
            .map(|s| Reading::new(s, &options.zone))
            .collect();
        let mut out = Read::default();
        self.pass(&mut columns, options.limit, &mut out)?;
        if columns.iter().any(|c| c.again) {
            for c in &mut columns {
                c.active = c.again;
                if c.again {
                    c.cells = Column::Text(Texts::default());
                }
            }
            let mut again = Read::default();
            self.pass(&mut columns, options.limit, &mut again)?;
        }
        finish(columns, &mut out);
        Ok(out)
    }

    fn pass(
        &self,
        columns: &mut [Reading<'_>],
        limit: usize,
        out: &mut Read,
    ) -> Result<(), Malformed> {
        let mut rows = self.rows();
        let mut cells = Vec::with_capacity(columns.len());
        loop {
            let line = rows.line;
            if !rows.next(&mut cells)? {
                return Ok(());
            }
            progress::tick();
            if cells.len() != columns.len() {
                if cells.len() == 1 && cells[0].is_empty() {
                    continue;
                }
                out.other_widths += 1;
                if out.widths.len() < WIDTHS {
                    out.widths.push(Width {
                        line,
                        cells: cells.len(),
                    });
                }
                continue;
            }
            for (c, cell) in columns.iter_mut().zip(&cells) {
                if c.active {
                    c.cell(cell, out.rows, limit);
                }
            }
            out.rows += 1;
        }
    }
}
