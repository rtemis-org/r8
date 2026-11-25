//! A table as delimited text.

use std::io::{self, Write};

use rtemis_r8::progress;

/// The cells of a column that is written.
pub trait Cells {
    /// Whether the cells are texts, whose quotes tell an empty one from a missing one.
    fn is_text(&self) -> bool;

    /// Adds the text of a cell to `out`; `false` for a missing one.
    fn text(&self, row: usize, out: &mut String) -> bool;
}

/// Writes a header and the rows of columns. A missing cell is empty. A text is quoted
/// where it holds the delimiter, a quote, or the end of a line, and where it is empty
/// or `NA`, so that it reads back as a text.
pub fn write(
    to: &mut dyn Write,
    names: &[&str],
    columns: &[&dyn Cells],
    rows: usize,
    delimiter: u8,
) -> io::Result<()> {
    let mut line = String::new();
    let mut cell = String::new();
    for (k, name) in names.iter().enumerate() {
        if k > 0 {
            line.push(delimiter as char);
        }
        put(&mut line, name, delimiter, true);
    }
    line.push('\n');
    for i in 0..rows {
        progress::tick();
        for (k, c) in columns.iter().enumerate() {
            if k > 0 {
                line.push(delimiter as char);
            }
            cell.clear();
            if c.text(i, &mut cell) {
                put(&mut line, &cell, delimiter, c.is_text());
            }
        }
        line.push('\n');
        if line.len() > 1 << 16 {
            to.write_all(line.as_bytes())?;
            line.clear();
        }
    }
    to.write_all(line.as_bytes())?;
    to.flush()
}

fn put(line: &mut String, t: &str, delimiter: u8, text: bool) {
    let special = |b: &u8| *b == delimiter || matches!(b, b'"' | b'\n' | b'\r');
    if (text && (t.is_empty() || t == "NA")) || t.as_bytes().iter().any(special) {
        line.push('"');
        for (k, part) in t.split('"').enumerate() {
            if k > 0 {
                line.push_str("\"\"");
            }
            line.push_str(part);
        }
        line.push('"');
    } else {
        line.push_str(t);
    }
}
