//! Delimited text, split as RFC 4180 states it.
//!
//! A line ends with a line feed, a carriage return, or both. A cell that begins with
//! a quote ends with one, and two quotes within it are one; a quote within a cell
//! that does not begin with one is a character of it. A line without a character is
//! no row of a table of two or more columns.

use std::borrow::Cow;
use std::fmt;

/// The form of a delimited text.
#[derive(Debug, Clone, Copy)]
pub struct Options {
    /// The character between two cells, an ASCII character other than a quote and
    /// the ends of a line.
    pub delimiter: u8,
}

impl Default for Options {
    fn default() -> Self {
        Options { delimiter: b',' }
    }
}

/// A cell.
#[derive(Debug, Clone, Copy)]
pub struct Cell<'a> {
    /// The characters, without the quotes around them.
    raw: &'a str,
    /// Whether the cell begins and ends with a quote.
    pub quoted: bool,
    /// Whether it holds two quotes for one.
    doubled: bool,
}

impl<'a> Cell<'a> {
    /// Whether the cell holds no character and no quote.
    pub fn is_empty(&self) -> bool {
        !self.quoted && self.raw.is_empty()
    }

    /// The text of the cell.
    pub fn text(&self) -> Cow<'a, str> {
        if self.doubled {
            Cow::Owned(self.raw.replace("\"\"", "\""))
        } else {
            Cow::Borrowed(self.raw)
        }
    }
}

/// A text that does not split into cells.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Malformed {
    /// The line, from 1.
    pub line: u64,
    /// What it is.
    pub kind: MalformedKind,
}

/// What a text that does not split is.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MalformedKind {
    /// A cell that begins with a quote and does not end with one.
    Unclosed,
    /// Characters after the quote that ends a cell.
    AfterQuote,
    /// No line.
    Empty,
    /// A delimiter that does not separate cells.
    Delimiter,
}

impl fmt::Display for Malformed {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.kind {
            MalformedKind::Unclosed => {
                write!(
                    f,
                    "the quote that begins a cell in line {} is not closed",
                    self.line
                )
            }
            MalformedKind::AfterQuote => write!(
                f,
                "a cell in line {} has characters after its closing quote",
                self.line
            ),
            MalformedKind::Empty => f.write_str("the file has no header"),
            MalformedKind::Delimiter => f.write_str(
                "a delimiter is an ASCII character other than a quote and the end of a line",
            ),
        }
    }
}

impl std::error::Error for Malformed {}

/// A delimited text with a header.
#[derive(Debug, Clone)]
pub struct Csv<'a> {
    header: Vec<String>,
    body: Rows<'a>,
}

impl<'a> Csv<'a> {
    /// The text, whose first line that is not empty is the header.
    pub fn new(text: &'a str, options: Options) -> Result<Csv<'a>, Malformed> {
        let d = options.delimiter;
        if !d.is_ascii() || matches!(d, b'"' | b'\n' | b'\r' | 0) {
            return Err(Malformed {
                line: 1,
                kind: MalformedKind::Delimiter,
            });
        }
        let mut body = Rows {
            text,
            at: 0,
            line: 1,
            delimiter: d,
        };
        let mut cells = Vec::new();
        loop {
            if !body.next(&mut cells)? {
                return Err(Malformed {
                    line: 1,
                    kind: MalformedKind::Empty,
                });
            }
            if cells.len() != 1 || !cells[0].is_empty() {
                break;
            }
        }
        Ok(Csv {
            header: cells.iter().map(|c| c.text().into_owned()).collect(),
            body,
        })
    }

    /// The names of the columns.
    pub fn header(&self) -> &[String] {
        &self.header
    }

    /// The rows after the header.
    pub(crate) fn rows(&self) -> Rows<'a> {
        self.body.clone()
    }
}

/// The rows of a delimited text.
#[derive(Debug, Clone)]
pub(crate) struct Rows<'a> {
    text: &'a str,
    at: usize,
    /// The line of the next row, from 1.
    pub line: u64,
    delimiter: u8,
}

impl<'a> Rows<'a> {
    /// Reads the cells of the next line; `false` at the end.
    pub fn next(&mut self, cells: &mut Vec<Cell<'a>>) -> Result<bool, Malformed> {
        cells.clear();
        let b = self.text.as_bytes();
        if self.at >= b.len() {
            return Ok(false);
        }
        let first = self.line;
        let malformed = |kind, line| Err(Malformed { line, kind });
        loop {
            let mut i = self.at;
            if i < b.len() && b[i] == b'"' {
                let line = self.line;
                let start = i + 1;
                let mut doubled = false;
                let mut k = start;
                loop {
                    let Some(q) = memchr::memchr(b'"', &b[k..]) else {
                        return malformed(MalformedKind::Unclosed, line);
                    };
                    k += q;
                    if b.get(k + 1) == Some(&b'"') {
                        doubled = true;
                        k += 2;
                        continue;
                    }
                    break;
                }
                let raw = &self.text[start..k];
                self.line += lines(raw.as_bytes());
                cells.push(Cell {
                    raw,
                    quoted: true,
                    doubled,
                });
                i = k + 1;
                match b.get(i) {
                    None | Some(b'\n' | b'\r') => {}
                    Some(c) if *c == self.delimiter => {}
                    Some(_) => return malformed(MalformedKind::AfterQuote, self.line),
                }
            } else {
                let start = i;
                while i < b.len() && b[i] != self.delimiter && b[i] != b'\n' && b[i] != b'\r' {
                    i += 1;
                }
                cells.push(Cell {
                    raw: &self.text[start..i],
                    quoted: false,
                    doubled: false,
                });
            }
            match b.get(i) {
                Some(c) if *c == self.delimiter => self.at = i + 1,
                Some(b'\r') => {
                    self.at = i + 1 + usize::from(b.get(i + 1) == Some(&b'\n'));
                    break;
                }
                Some(_) => {
                    self.at = i + 1;
                    break;
                }
                None => {
                    self.at = i;
                    break;
                }
            }
        }
        debug_assert!(self.line >= first);
        self.line += 1;
        Ok(true)
    }
}

/// The line ends within a cell.
fn lines(b: &[u8]) -> u64 {
    let mut n = 0;
    for (k, &c) in b.iter().enumerate() {
        n += u64::from(c == b'\n' || (c == b'\r' && b.get(k + 1) != Some(&b'\n')));
    }
    n
}
