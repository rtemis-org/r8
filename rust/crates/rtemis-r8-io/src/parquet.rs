//! Parquet files: the stored cells of their columns, and a table written as one.
//!
//! A column of a file is read as [`Stored`] cells: BOOLEAN as logicals; INT32,
//! INT64, and the integer logical types as whole numbers; FLOAT and DOUBLE as doubles;
//! STRING, ENUM, JSON, and UUID as quoted texts; DATE as dates; TIMESTAMP as
//! date-times, those not adjusted to UTC as clock readings; DECIMAL of scale 0 as
//! whole numbers and of another scale as doubles. A column of another type, and a
//! nested or repeated column, are refused.
//!
//! spec: R8/io#parquet

use std::fmt;
use std::fs::File;
use std::io::Write;
use std::sync::Arc;

use parquet::basic::{
    Compression, ConvertedType, LogicalType, Repetition, TimeUnit, TimestampType,
    Type as PhysicalType,
};
use parquet::column::reader::{ColumnReader, ColumnReaderImpl};
use parquet::column::writer::ColumnWriter;
use parquet::data_type::{ByteArray, DataType, FixedLenByteArray};
use parquet::file::properties::WriterProperties;
use parquet::file::reader::{FileReader, SerializedFileReader};
use parquet::file::writer::SerializedFileWriter;
use parquet::schema::types::{ColumnDescriptor, Type};
use rtemis_r8::progress;

use crate::read::{MISSING_DATETIME, MISSING_INTEGER, read_stored};
use crate::{ColumnSpec, Kind, Read, ReadOptions, Stored, is_missing_double};

/// A file that does not read, or a column of it.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParquetError {
    /// The file is not Parquet, or is damaged: the message of the reader.
    File(String),
    /// A column whose type has no R8 kind.
    Column {
        /// The name of the column.
        column: String,
        /// Its type in the file.
        stored: String,
    },
}

impl fmt::Display for ParquetError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParquetError::File(m) => f.write_str(m),
            ParquetError::Column { column, stored } => {
                write!(
                    f,
                    "the column `{column}` is of the type {stored}, which R8 does not read"
                )
            }
        }
    }
}

impl std::error::Error for ParquetError {}

impl From<parquet::errors::ParquetError> for ParquetError {
    fn from(e: parquet::errors::ParquetError) -> Self {
        ParquetError::File(e.to_string())
    }
}

/// How the values of a column are read.
#[derive(Debug, Clone, Copy, PartialEq)]
enum Of {
    Logical,
    /// An INT32; `signed` is false for an unsigned one.
    Int32 {
        signed: bool,
    },
    /// An INT64; `signed` is false for an unsigned one.
    Int64 {
        signed: bool,
    },
    Float,
    Double,
    /// A text of UTF-8.
    Text,
    Uuid,
    Date,
    /// A timestamp, in units of `per_second`, adjusted to UTC or a clock reading.
    Timestamp {
        per_second: i64,
        utc: bool,
    },
    /// A decimal of the stored physical type, with its scale.
    Decimal {
        scale: i32,
    },
}

/// The values of a column, without its nulls, and whether each cell has one.
enum Values {
    Bool(Vec<bool>),
    Int32(Vec<i32>),
    Int64(Vec<i64>),
    Float(Vec<f32>),
    Double(Vec<f64>),
    Bytes(Vec<ByteArray>),
    Fixed(Vec<FixedLenByteArray>),
}

/// The cells of a column as they are stored.
struct Stock {
    of: Of,
    values: Values,
    /// The place in `values` of each cell, or `None` for a null.
    at: Vec<Option<u32>>,
    /// Whether the timestamps hold a fraction of a microsecond.
    sub_micros: bool,
}

/// A Parquet file.
pub struct Parquet {
    reader: SerializedFileReader<File>,
    header: Vec<String>,
    of: Vec<Result<Of, String>>,
}

impl Parquet {
    /// Opens a file and reads its schema; a nested or repeated column is refused.
    pub fn open(path: &str) -> Result<Parquet, ParquetError> {
        let file = File::open(path).map_err(|e| ParquetError::File(e.to_string()))?;
        let reader = SerializedFileReader::new(file)?;
        let schema = reader.metadata().file_metadata().schema_descr_ptr();
        let mut header = Vec::new();
        for field in schema.root_schema().get_fields() {
            let repeated = field.get_basic_info().has_repetition()
                && field.get_basic_info().repetition() == Repetition::REPEATED;
            if field.is_group() || repeated {
                return Err(ParquetError::Column {
                    column: field.name().to_string(),
                    stored: (if field.is_group() {
                        "a nested group"
                    } else {
                        "a repeated field"
                    })
                    .to_string(),
                });
            }
            header.push(field.name().to_string());
        }
        let of = schema.columns().iter().map(|c| of(c)).collect();
        Ok(Parquet { reader, header, of })
    }

    /// The names of the columns.
    pub fn header(&self) -> &[String] {
        &self.header
    }

    /// The number of rows.
    pub fn rows(&self) -> u64 {
        self.reader.metadata().file_metadata().num_rows().max(0) as u64
    }

    /// The cells, as the columns of `specs`, one for each name of the header. A
    /// column whose kind is inferred and that has no value has the kind of its type.
    ///
    /// # Panics
    ///
    /// Where `specs` has another length than the header.
    pub fn read(&self, specs: &[ColumnSpec], options: &ReadOptions) -> Result<Read, ParquetError> {
        assert_eq!(specs.len(), self.header.len(), "one column for each name");
        let mut stocks = Vec::with_capacity(self.header.len());
        for (k, of) in self.of.iter().enumerate() {
            let of = of.clone().map_err(|stored| ParquetError::Column {
                column: self.header[k].clone(),
                stored,
            })?;
            stocks.push(self.stock(k, of)?);
        }
        let rows = self.rows();
        Ok(read_stored(
            specs,
            rows,
            |k| {
                let s = &stocks[k];
                s.at.iter().map(move |at| s.cell(*at))
            },
            |k| stocks[k].kind(),
            options,
        ))
    }

    /// The cells of column `k` of every row group.
    fn stock(&self, k: usize, of: Of) -> Result<Stock, ParquetError> {
        let groups = self.reader.metadata().num_row_groups();
        let mut defs: Vec<i16> = Vec::new();
        let mut values = match self
            .reader
            .metadata()
            .file_metadata()
            .schema_descr()
            .column(k)
            .physical_type()
        {
            PhysicalType::BOOLEAN => Values::Bool(Vec::new()),
            PhysicalType::INT32 => Values::Int32(Vec::new()),
            PhysicalType::INT64 => Values::Int64(Vec::new()),
            PhysicalType::FLOAT => Values::Float(Vec::new()),
            PhysicalType::DOUBLE => Values::Double(Vec::new()),
            PhysicalType::BYTE_ARRAY => Values::Bytes(Vec::new()),
            PhysicalType::FIXED_LEN_BYTE_ARRAY => Values::Fixed(Vec::new()),
            PhysicalType::INT96 => unreachable!("INT96 is refused"),
        };
        for g in 0..groups {
            let group = self.reader.get_row_group(g)?;
            let n = group.metadata().num_rows().max(0) as usize;
            let column = group.get_column_reader(k)?;
            match (column, &mut values) {
                (ColumnReader::BoolColumnReader(r), Values::Bool(v)) => all(r, n, &mut defs, v)?,
                (ColumnReader::Int32ColumnReader(r), Values::Int32(v)) => all(r, n, &mut defs, v)?,
                (ColumnReader::Int64ColumnReader(r), Values::Int64(v)) => all(r, n, &mut defs, v)?,
                (ColumnReader::FloatColumnReader(r), Values::Float(v)) => all(r, n, &mut defs, v)?,
                (ColumnReader::DoubleColumnReader(r), Values::Double(v)) => {
                    all(r, n, &mut defs, v)?
                }
                (ColumnReader::ByteArrayColumnReader(r), Values::Bytes(v)) => {
                    all(r, n, &mut defs, v)?
                }
                (ColumnReader::FixedLenByteArrayColumnReader(r), Values::Fixed(v)) => {
                    all(r, n, &mut defs, v)?;
                }
                _ => unreachable!("the reader of a column is of its physical type"),
            }
        }
        let max = self
            .reader
            .metadata()
            .file_metadata()
            .schema_descr()
            .column(k)
            .max_def_level();
        let mut next = 0u32;
        let at: Vec<Option<u32>> = if max == 0 {
            let n = values.len() as u32;
            (0..n).map(Some).collect()
        } else {
            defs.iter()
                .map(|&d| {
                    (d == max).then(|| {
                        next += 1;
                        next - 1
                    })
                })
                .collect()
        };
        if let Values::Bytes(v) = &values
            && matches!(of, Of::Text)
            && let Some(k_bad) = v
                .iter()
                .position(|b| std::str::from_utf8(b.data()).is_err())
        {
            return Err(ParquetError::File(format!(
                "the column `{}` holds a text that is not UTF-8, its value {}",
                self.header[k],
                k_bad + 1
            )));
        }
        let sub_micros = match (of, &values) {
            (Of::Timestamp { per_second, .. }, Values::Int64(v)) if per_second > 1_000_000 => {
                v.iter().any(|x| x % (per_second / 1_000_000) != 0)
            }
            _ => false,
        };
        Ok(Stock {
            of,
            values,
            at,
            sub_micros,
        })
    }
}

/// Reads every value and definition level of a column chunk of `n` rows.
fn all<T: DataType>(
    mut r: ColumnReaderImpl<T>,
    n: usize,
    defs: &mut Vec<i16>,
    values: &mut Vec<T::T>,
) -> Result<(), ParquetError> {
    let mut left = n;
    while left > 0 {
        let (records, _, _) = r.read_records(left, Some(defs), None, values)?;
        if records == 0 {
            break;
        }
        left -= records;
    }
    if left > 0 {
        return Err(ParquetError::File(format!(
            "a column chunk holds {} of its {n} rows",
            n - left
        )));
    }
    Ok(())
}

impl Values {
    fn len(&self) -> usize {
        match self {
            Values::Bool(v) => v.len(),
            Values::Int32(v) => v.len(),
            Values::Int64(v) => v.len(),
            Values::Float(v) => v.len(),
            Values::Double(v) => v.len(),
            Values::Bytes(v) => v.len(),
            Values::Fixed(v) => v.len(),
        }
    }
}

/// How a column of a file is read, or the name of its type where R8 does not read it.
fn of(c: &ColumnDescriptor) -> Result<Of, String> {
    let p = c.physical_type();
    let refused = || {
        Err(match c.logical_type_ref() {
            Some(l) => format!("{p} ({l:?})"),
            None if c.converted_type() != ConvertedType::NONE => {
                format!("{p} ({})", c.converted_type())
            }
            None => p.to_string(),
        })
    };
    let decimal = || Of::Decimal {
        scale: c.type_scale(),
    };
    if let Some(l) = c.logical_type_ref() {
        return match (l, p) {
            (
                LogicalType::String | LogicalType::Enum | LogicalType::Json,
                PhysicalType::BYTE_ARRAY,
            ) => Ok(Of::Text),
            (LogicalType::Uuid, PhysicalType::FIXED_LEN_BYTE_ARRAY) => Ok(Of::Uuid),
            (LogicalType::Date, PhysicalType::INT32) => Ok(Of::Date),
            (LogicalType::Integer(i), PhysicalType::INT32) => Ok(Of::Int32 {
                signed: i.is_signed,
            }),
            (LogicalType::Integer(i), PhysicalType::INT64) => Ok(Of::Int64 {
                signed: i.is_signed,
            }),
            (
                LogicalType::Timestamp(TimestampType {
                    is_adjusted_to_u_t_c,
                    unit,
                }),
                PhysicalType::INT64,
            ) => Ok(Of::Timestamp {
                per_second: match unit {
                    TimeUnit::MILLIS => 1_000,
                    TimeUnit::MICROS => 1_000_000,
                    TimeUnit::NANOS => 1_000_000_000,
                },
                utc: *is_adjusted_to_u_t_c,
            }),
            (
                LogicalType::Decimal(_),
                PhysicalType::INT32
                | PhysicalType::INT64
                | PhysicalType::BYTE_ARRAY
                | PhysicalType::FIXED_LEN_BYTE_ARRAY,
            ) => Ok(decimal()),
            _ => refused(),
        };
    }
    match (c.converted_type(), p) {
        (ConvertedType::NONE, PhysicalType::BOOLEAN) => Ok(Of::Logical),
        (
            ConvertedType::NONE
            | ConvertedType::INT_8
            | ConvertedType::INT_16
            | ConvertedType::INT_32,
            PhysicalType::INT32,
        ) => Ok(Of::Int32 { signed: true }),
        (
            ConvertedType::UINT_8 | ConvertedType::UINT_16 | ConvertedType::UINT_32,
            PhysicalType::INT32,
        ) => Ok(Of::Int32 { signed: false }),
        (ConvertedType::NONE | ConvertedType::INT_64, PhysicalType::INT64) => {
            Ok(Of::Int64 { signed: true })
        }
        (ConvertedType::UINT_64, PhysicalType::INT64) => Ok(Of::Int64 { signed: false }),
        (ConvertedType::NONE, PhysicalType::FLOAT) => Ok(Of::Float),
        (ConvertedType::NONE, PhysicalType::DOUBLE) => Ok(Of::Double),
        (
            ConvertedType::UTF8 | ConvertedType::ENUM | ConvertedType::JSON,
            PhysicalType::BYTE_ARRAY,
        ) => Ok(Of::Text),
        (ConvertedType::DATE, PhysicalType::INT32) => Ok(Of::Date),
        (ConvertedType::TIMESTAMP_MILLIS, PhysicalType::INT64) => Ok(Of::Timestamp {
            per_second: 1_000,
            utc: true,
        }),
        (ConvertedType::TIMESTAMP_MICROS, PhysicalType::INT64) => Ok(Of::Timestamp {
            per_second: 1_000_000,
            utc: true,
        }),
        (
            ConvertedType::DECIMAL,
            PhysicalType::INT32
            | PhysicalType::INT64
            | PhysicalType::BYTE_ARRAY
            | PhysicalType::FIXED_LEN_BYTE_ARRAY,
        ) => Ok(decimal()),
        _ => refused(),
    }
}

/// A whole number of a decimal's big-endian two's complement bytes.
fn unscaled(b: &[u8]) -> Option<i128> {
    if b.is_empty() || b.len() > 16 {
        return None;
    }
    let fill = if b[0] & 0x80 != 0 { 0xff } else { 0 };
    let mut x = [fill; 16];
    x[16 - b.len()..].copy_from_slice(b);
    Some(i128::from_be_bytes(x))
}

fn bytes_decimal(b: &[u8], scale: i32) -> Stored<'static> {
    match unscaled(b) {
        Some(u) => decimal(u, scale),
        None => Stored::Double(f64::NAN),
    }
}

/// The stored cell of a decimal of `scale` digits after the point.
fn decimal(v: i128, scale: i32) -> Stored<'static> {
    if scale == 0 {
        return match i64::try_from(v) {
            Ok(v) => Stored::Integer(v),
            Err(_) => Stored::Text {
                text: v.to_string().into(),
                quoted: true,
            },
        };
    }
    // The nearest double to the decimal, as its text reads.
    let t = format!("{v}e{}", -scale);
    Stored::Double(t.parse().unwrap_or(f64::NAN))
}

impl Stock {
    /// The kind of a column whose kind is inferred and that has no value.
    fn kind(&self) -> Kind {
        match self.of {
            Of::Logical => Kind::Logical,
            Of::Int32 { .. } | Of::Int64 { .. } => Kind::Integer,
            Of::Decimal { scale: 0 } => Kind::Integer,
            Of::Float | Of::Double | Of::Decimal { .. } => Kind::Double,
            Of::Text | Of::Uuid => Kind::Text,
            Of::Date => Kind::Date,
            Of::Timestamp { .. } if self.sub_micros => Kind::Text,
            Of::Timestamp { .. } => Kind::DateTime,
        }
    }

    fn cell(&self, at: Option<u32>) -> Stored<'_> {
        let Some(i) = at else {
            return Stored::Missing;
        };
        let i = i as usize;
        match (&self.values, self.of) {
            (Values::Bool(v), _) => Stored::Logical(v[i]),
            (Values::Int32(v), Of::Int32 { signed: true }) => Stored::Integer(i64::from(v[i])),
            (Values::Int32(v), Of::Int32 { signed: false }) => {
                Stored::Integer(i64::from(v[i] as u32))
            }
            (Values::Int32(v), Of::Date) => Stored::Date(v[i]),
            (Values::Int32(v), Of::Decimal { scale }) => decimal(i128::from(v[i]), scale),
            (Values::Int64(v), Of::Int64 { signed: false }) if v[i] < 0 => Stored::Text {
                text: (v[i] as u64).to_string().into(),
                quoted: true,
            },
            (Values::Int64(v), Of::Int64 { .. }) => Stored::Integer(v[i]),
            (Values::Int64(v), Of::Decimal { scale }) => decimal(i128::from(v[i]), scale),
            (Values::Int64(v), Of::Timestamp { per_second, utc }) => {
                timestamp(v[i], per_second, utc, self.sub_micros)
            }
            (Values::Float(v), _) => Stored::Double(f64::from(v[i])),
            (Values::Double(v), _) => Stored::Double(v[i]),
            (Values::Bytes(v), Of::Text) => Stored::Text {
                // Checked as UTF-8 when the column was read.
                text: std::str::from_utf8(v[i].data()).unwrap_or_default().into(),
                quoted: true,
            },
            (Values::Bytes(v), Of::Decimal { scale }) => bytes_decimal(v[i].data(), scale),
            (Values::Fixed(v), Of::Decimal { scale }) => bytes_decimal(v[i].data(), scale),
            (Values::Fixed(v), Of::Uuid) => Stored::Text {
                text: uuid(v[i].data()).into(),
                quoted: true,
            },
            _ => Stored::Missing,
        }
    }
}

/// A timestamp of `per_second` units: a date-time, a clock reading, or, where the
/// column holds a fraction of a microsecond, the text of its nanoseconds.
fn timestamp(v: i64, per_second: i64, utc: bool, sub_micros: bool) -> Stored<'static> {
    if sub_micros {
        let secs = v.div_euclid(1_000_000_000);
        let nanos = v.rem_euclid(1_000_000_000);
        let mut t = rtemis_r8::canonical::datetime_text(secs * 1_000_000);
        // `.000000Z` becomes the nine digits of the nanoseconds.
        t.truncate(t.len() - 8);
        t.push_str(&format!("{nanos:09}"));
        if utc {
            t.push('Z');
        }
        return Stored::Text {
            text: t.into(),
            quoted: true,
        };
    }
    let micros = match per_second {
        1_000 => v.checked_mul(1_000),
        1_000_000 => Some(v),
        _ => Some(v / 1_000),
    };
    match micros {
        Some(m) if utc => Stored::DateTime(m),
        Some(m) => Stored::Clock(m),
        None => Stored::Text {
            text: v.to_string().into(),
            quoted: true,
        },
    }
}

fn uuid(b: &[u8]) -> String {
    let mut t = String::with_capacity(36);
    for (k, x) in b.iter().enumerate() {
        if matches!(k, 4 | 6 | 8 | 10) {
            t.push('-');
        }
        t.push_str(&format!("{x:02x}"));
    }
    t
}

/// The texts of a column of text that is written.
pub trait TextColumn {
    /// The text at `i`, `None` for a missing one.
    fn text(&self, i: usize) -> Option<&str>;
}

/// The cells of a column that is written, with R's missing values.
pub enum Column<'a> {
    /// 0, 1, or [`MISSING_INTEGER`].
    Logical(&'a [i32]),
    /// Integers, or [`MISSING_INTEGER`].
    Integer(&'a [i32]),
    /// Doubles; R's missing value is a null, and another NaN a value.
    Double(&'a [f64]),
    /// Texts.
    Text(&'a dyn TextColumn),
    /// The places of the levels, from 1, or [`MISSING_INTEGER`]; written as their labels.
    Factor {
        /// The places.
        codes: &'a [i32],
        /// The labels.
        levels: &'a [String],
    },
    /// The days since 1970-01-01, or [`MISSING_INTEGER`].
    Date(&'a [i32]),
    /// The microseconds since 1970-01-01T00:00:00Z, or [`MISSING_DATETIME`].
    DateTime(&'a [i64]),
}

/// The rows of a row group.
const GROUP: usize = 1 << 20;

/// Writes the columns, each of `rows` cells, as a Parquet file: logical as BOOLEAN,
/// integer as INT32, double as DOUBLE, text as STRING, a factor as the STRING of its
/// labels, date as DATE, and date-time as TIMESTAMP of microseconds adjusted to UTC. Every column is OPTIONAL,
/// a missing cell is a null, and the pages are compressed with snappy.
pub fn write_parquet<W: Write + Send>(
    to: W,
    names: &[&str],
    columns: &[Column<'_>],
    rows: usize,
) -> Result<(), ParquetError> {
    let fields = names
        .iter()
        .zip(columns)
        .map(|(name, c)| field(name, c).map(Arc::new))
        .collect::<Result<Vec<_>, _>>()?;
    let schema = Type::group_type_builder("schema")
        .with_fields(fields)
        .build()?;
    let props = WriterProperties::builder()
        .set_compression(Compression::SNAPPY)
        .set_max_row_group_row_count(Some(GROUP))
        .set_created_by(format!("rtemis-r8-io {}", env!("CARGO_PKG_VERSION")))
        .build();
    let mut writer = SerializedFileWriter::new(to, Arc::new(schema), Arc::new(props))?;
    let mut start = 0;
    loop {
        let end = (start + GROUP).min(rows);
        let mut group = writer.next_row_group()?;
        let mut k = 0;
        while let Some(mut w) = group.next_column()? {
            put(w.untyped(), &columns[k], start, end)?;
            w.close()?;
            k += 1;
        }
        group.close()?;
        start = end;
        if start >= rows {
            break;
        }
    }
    writer.close()?;
    Ok(())
}

fn field(name: &str, c: &Column<'_>) -> Result<Type, ParquetError> {
    let (p, l) = match c {
        Column::Logical(_) => (PhysicalType::BOOLEAN, None),
        Column::Integer(_) => (PhysicalType::INT32, None),
        Column::Double(_) => (PhysicalType::DOUBLE, None),
        Column::Text(_) | Column::Factor { .. } => {
            (PhysicalType::BYTE_ARRAY, Some(LogicalType::String))
        }
        Column::Date(_) => (PhysicalType::INT32, Some(LogicalType::Date)),
        Column::DateTime(_) => (
            PhysicalType::INT64,
            Some(LogicalType::Timestamp(TimestampType {
                is_adjusted_to_u_t_c: true,
                unit: TimeUnit::MICROS,
            })),
        ),
    };
    Ok(Type::primitive_type_builder(name, p)
        .with_repetition(Repetition::OPTIONAL)
        .with_logical_type(l)
        .build()?)
}

/// Writes the cells `start..end` of a column.
fn put(
    w: &mut ColumnWriter<'_>,
    c: &Column<'_>,
    start: usize,
    end: usize,
) -> Result<(), ParquetError> {
    let mut defs = Vec::with_capacity(end - start);
    fn kept<T: Copy, U>(
        x: &[T],
        defs: &mut Vec<i16>,
        missing: impl Fn(T) -> bool,
        value: impl Fn(T) -> U,
    ) -> Vec<U> {
        let mut out = Vec::with_capacity(x.len());
        for &v in x {
            progress::tick();
            if missing(v) {
                defs.push(0);
            } else {
                defs.push(1);
                out.push(value(v));
            }
        }
        out
    }
    match (w, c) {
        (ColumnWriter::BoolColumnWriter(w), Column::Logical(x)) => {
            let v = kept(
                &x[start..end],
                &mut defs,
                |v| v == MISSING_INTEGER,
                |v| v != 0,
            );
            w.write_batch(&v, Some(&defs), None)?;
        }
        (ColumnWriter::Int32ColumnWriter(w), Column::Integer(x) | Column::Date(x)) => {
            let v = kept(&x[start..end], &mut defs, |v| v == MISSING_INTEGER, |v| v);
            w.write_batch(&v, Some(&defs), None)?;
        }
        (ColumnWriter::DoubleColumnWriter(w), Column::Double(x)) => {
            let v = kept(&x[start..end], &mut defs, is_missing_double, |v| v);
            w.write_batch(&v, Some(&defs), None)?;
        }
        (ColumnWriter::Int64ColumnWriter(w), Column::DateTime(x)) => {
            let v = kept(&x[start..end], &mut defs, |v| v == MISSING_DATETIME, |v| v);
            w.write_batch(&v, Some(&defs), None)?;
        }
        (ColumnWriter::ByteArrayColumnWriter(w), Column::Text(x)) => {
            let mut v = Vec::new();
            for i in start..end {
                progress::tick();
                match x.text(i) {
                    Some(t) => {
                        defs.push(1);
                        v.push(ByteArray::from(t.as_bytes().to_vec()));
                    }
                    None => defs.push(0),
                }
            }
            w.write_batch(&v, Some(&defs), None)?;
        }
        (ColumnWriter::ByteArrayColumnWriter(w), Column::Factor { codes, levels }) => {
            let labels: Vec<ByteArray> = levels
                .iter()
                .map(|l| ByteArray::from(l.as_bytes().to_vec()))
                .collect();
            let v = kept(
                &codes[start..end],
                &mut defs,
                |k| k == MISSING_INTEGER || k < 1 || k as usize > labels.len(),
                |k| labels[k as usize - 1].clone(),
            );
            w.write_batch(&v, Some(&defs), None)?;
        }
        _ => unreachable!("the writer of a column is of its schema"),
    }
    Ok(())
}
