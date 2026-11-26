//! Readers and writers of table files for the R8 type system.
//!
//! [`decode`] gives the text of a file, [`Csv`] splits it into cells, and
//! [`Csv::read`] reads the cells as the kinds of their columns: declared kinds, or
//! the kind that all cells of a column have. A typed format, Parquet under the
//! feature `parquet`, gives [`Stored`] cells, which the same column reader reads: a
//! typed value that is not of its column's kind is read by its canonical text,
//! [`Stored::text`]. A text that does not read is collected once, with its count and
//! its first rows, and changes no other cell. [`write()`] writes a table as delimited
//! text, which reads back as it was.
//!
//! spec: R8/io

mod csv;
mod decode;
#[cfg(feature = "parquet")]
pub mod parquet;
mod read;
mod stored;
pub mod text;
mod write;

pub use csv::{Cell, Csv, Malformed, MalformedKind, Options};
pub use decode::{Encoding, NotText, decode};
pub use read::{
    Column, ColumnSpec, Kind, MISSING_DATETIME, MISSING_INTEGER, Read, ReadOptions, Recoded,
    Refused, Scalar, Texts, Unread, UnreadCell, Width, missing_double, read_stored,
};
pub use stored::{Stored, Zone, number_text};
pub use write::{Cells, write};

/// Whether a double is R's missing value, a NaN whose low word is 1954, which is apart
/// from the other NaNs.
pub fn is_missing_double(v: f64) -> bool {
    v.is_nan() && (v.to_bits() as u32) == 1954
}
