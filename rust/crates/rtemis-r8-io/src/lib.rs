//! Readers of table files for the R8 type system.
//!
//! [`decode`] gives the text of a file, [`Csv`] splits it into cells, and
//! [`Csv::read`] reads the cells as the kinds of their columns: declared kinds, or
//! the kind that all cells of a column have. A text that does not read is collected
//! once, with its count and its first rows, and changes no other cell. [`write()`]
//! writes a table, which reads back as it was.
//!
//! spec: R8/io

mod csv;
mod decode;
mod read;
pub mod text;
mod write;

pub use csv::{Cell, Csv, Malformed, MalformedKind, Options};
pub use decode::{Encoding, NotText, decode};
pub use read::{
    Column, ColumnSpec, Kind, MISSING_DATETIME, MISSING_INTEGER, Read, Recoded, Refused, Scalar,
    Texts, Unread, UnreadCell, Width, missing_double,
};
pub use write::{Cells, write};
