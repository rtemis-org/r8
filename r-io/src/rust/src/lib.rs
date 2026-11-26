//! The R bindings of `rtemis-r8-io`: R's vectors of the columns that are read, and
//! the cells of R's vectors that are written.

use std::panic::{AssertUnwindSafe, catch_unwind, resume_unwind};
use std::sync::atomic::{AtomicPtr, Ordering};

use extendr_api::prelude::*;
use extendr_api::{Error, Result};
use extendr_ffi::{
    INTEGER, INTEGER_ELT, LOGICAL, LOGICAL_ELT, R_NaString, R_xlen_t, REAL, REAL_ELT,
    Rf_allocVector, Rf_mkCharLenCE, Rf_protect, Rf_unprotect, SET_STRING_ELT, SEXP, SEXPTYPE,
    STRING_ELT, TYPEOF, cetype_t,
};
use rtemis_r8::canonical::{date_text, datetime_text};
use rtemis_r8_io::parquet::{Column as Out, Parquet, ParquetError, TextColumn, write_parquet};
use rtemis_r8_io::{
    Cells, Column, ColumnSpec, Csv, Encoding, Kind, MISSING_DATETIME, MISSING_INTEGER, Options,
    Read, ReadOptions, Refused, Scalar, Texts, Zone, decode, is_missing_double, number_text,
};

const NA_INTEGER: i32 = i32::MIN;

static INTERRUPTED: AtomicPtr<()> = AtomicPtr::new(std::ptr::null_mut());

/// Receives `interrupted()` of `entrypoint.c`.
///
/// # Safety
///
/// The argument is that function.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn r8_io_set_interrupted(check: *mut ()) {
    INTERRUPTED.store(check, Ordering::Release);
}

/// The payload of the unwinding of a call that the user or a time limit ends.
struct Ended;

fn hook() {
    let f = INTERRUPTED.load(Ordering::Acquire);
    if f.is_null() {
        return;
    }
    let f: unsafe extern "C" fn() -> i32 = unsafe { std::mem::transmute(f) };
    if unsafe { f() } != 0 {
        resume_unwind(Box::new(Ended));
    }
}

/// The outcome of `f`, which the user's interrupt and R's time limits end.
fn interruptible<T>(f: impl FnOnce() -> Result<T>) -> Result<T> {
    let before = rtemis_r8::progress::set_hook(Some(hook));
    let out = catch_unwind(AssertUnwindSafe(f));
    rtemis_r8::progress::set_hook(before);
    match out {
        Ok(v) => v,
        Err(p) if p.is::<Ended>() => Err(Error::Other(
            "R8.io stopped: interrupted, or at a time limit".to_string(),
        )),
        Err(p) => resume_unwind(p),
    }
}

fn own(x: SEXP) -> Robj {
    unsafe { Robj::from_sexp(x) }
}

/// A `CHARSXP` of a text.
fn charsxp(t: &str) -> SEXP {
    unsafe { Rf_mkCharLenCE(t.as_ptr().cast(), t.len() as i32, cetype_t::CE_UTF8) }
}

/// A character vector of `n` texts.
fn strings<'t>(n: usize, text: impl Fn(usize) -> Option<&'t str>) -> Robj {
    unsafe {
        let out = Rf_protect(Rf_allocVector(SEXPTYPE::STRSXP, n as R_xlen_t));
        for i in 0..n {
            if i % 1024 == 0 {
                rtemis_r8::progress::tick_block();
            }
            let s = match text(i) {
                Some(t) => charsxp(t),
                None => R_NaString,
            };
            SET_STRING_ELT(out, i as R_xlen_t, s);
        }
        let r = own(out);
        Rf_unprotect(1);
        r
    }
}

fn integers(x: &[i32]) -> Robj {
    unsafe {
        let out = Rf_protect(Rf_allocVector(SEXPTYPE::INTSXP, x.len() as R_xlen_t));
        if !x.is_empty() {
            std::slice::from_raw_parts_mut(INTEGER(out), x.len()).copy_from_slice(x);
        }
        let r = own(out);
        Rf_unprotect(1);
        r
    }
}

fn logicals(x: &[i32]) -> Robj {
    unsafe {
        let out = Rf_protect(Rf_allocVector(SEXPTYPE::LGLSXP, x.len() as R_xlen_t));
        if !x.is_empty() {
            std::slice::from_raw_parts_mut(LOGICAL(out), x.len()).copy_from_slice(x);
        }
        let r = own(out);
        Rf_unprotect(1);
        r
    }
}

fn doubles<T: Copy>(x: &[T], value: impl Fn(T) -> f64) -> Robj {
    unsafe {
        let out = Rf_protect(Rf_allocVector(SEXPTYPE::REALSXP, x.len() as R_xlen_t));
        for (k, &v) in x.iter().enumerate() {
            *REAL(out).add(k) = value(v);
        }
        let r = own(out);
        Rf_unprotect(1);
        r
    }
}

/// R's vector of a column.
fn vector(c: &Column, kind: &Kind) -> Result<Robj> {
    let na = rtemis_r8_io::missing_double();
    Ok(match c {
        Column::Logical(x) => logicals(x),
        Column::Integer(x) => integers(x),
        Column::Double(x) => doubles(x, |v| v),
        Column::Text(x) => texts(x),
        Column::Date(x) => {
            let mut r = doubles(x, |v| {
                if v == MISSING_INTEGER {
                    na
                } else {
                    f64::from(v)
                }
            });
            r.set_class(["Date"])?;
            r
        }
        Column::DateTime(x) => {
            let mut r = doubles(x, |v| {
                if v == MISSING_DATETIME {
                    na
                } else {
                    v as f64 / 1e6
                }
            });
            r.set_class(["POSIXct", "POSIXt"])?;
            r.set_attrib("tzone", "UTC")?;
            r
        }
        Column::Factor(x) => {
            let mut r = integers(x);
            if let Kind::Factor(levels) = kind {
                r.set_attrib("levels", strings(levels.len(), |i| Some(&levels[i])))?;
            }
            r.set_class(["factor"])?;
            r
        }
    })
}

fn texts(x: &Texts) -> Robj {
    strings(x.len(), |i| x.get(i))
}

/// The strings of a character vector without `NA`, or `NULL`.
fn given(x: &Robj) -> Vec<String> {
    // An empty vector's data pointer is no slice.
    if x.len() == 0 {
        return Vec::new();
    }
    x.as_str_iter()
        .map(|it| it.filter(|s| !s.is_na()).map(|s| s.to_string()).collect())
        .unwrap_or_default()
}

/// The elements of a list by their names.
fn named(x: &Robj) -> Vec<(String, Robj)> {
    let Some(l) = x.as_list() else {
        return Vec::new();
    };
    l.iter().map(|(k, v)| (k.to_string(), v)).collect()
}

/// The value at `i` of a vector, as a column's kind takes it.
fn scalar(x: &Robj, i: usize, kind: &Kind) -> Option<Scalar> {
    if x.len() <= i {
        return None;
    }
    let number = || -> Option<f64> {
        match (x.as_real_slice(), x.as_integer_slice()) {
            (Some(s), _) => Some(s[i]).filter(|v| !v.is_nan()),
            (_, Some(s)) => Some(s[i]).filter(|v| *v != NA_INTEGER).map(f64::from),
            _ => None,
        }
    };
    if x.inherits("factor") {
        return None;
    }
    match kind {
        Kind::Logical => x
            .as_logical_slice()
            .filter(|s| !s[i].is_na())
            .map(|s| Scalar::Logical(s[i].is_true())),
        Kind::Text | Kind::Factor(_) => x
            .as_str_vector()
            .and_then(|s| s.get(i).map(|t| Scalar::Text(t.to_string()))),
        Kind::Date if x.inherits("Date") => number().map(|v| Scalar::Date(v as i32)),
        Kind::DateTime if x.inherits("POSIXct") => {
            number().map(|v| Scalar::DateTime((v * 1e6).round() as i64))
        }
        _ if x.inherits("Date") || x.inherits("POSIXct") => None,
        Kind::Integer => number()
            .filter(|v| v.fract() == 0.0 && v.abs() <= 2_147_483_647.0)
            .map(|v| Scalar::Integer(v as i32)),
        Kind::Double => number().map(Scalar::Double),
        _ => None,
    }
}

fn kind(k: &str, levels: &Robj) -> Kind {
    match k {
        "logical" => Kind::Logical,
        "integer" => Kind::Integer,
        "double" => Kind::Double,
        "date" => Kind::Date,
        "posixct" => Kind::DateTime,
        "factor" => {
            let l = given(levels);
            if l.is_empty() {
                Kind::Text
            } else {
                Kind::Factor(l)
            }
        }
        "infer" => Kind::Infer,
        _ => Kind::Text,
    }
}

/// What is refused: its kind and the parts that R's message names.
fn refused(kind: &str, message: String) -> Robj {
    list!(refused = kind, message = message).into()
}

/// The names of a header that are missing or repeat.
fn header_refused(header: &[String]) -> Option<Robj> {
    for (k, h) in header.iter().enumerate() {
        if h.is_empty() {
            return Some(refused(
                "header",
                format!("column {} has no name in the header", k + 1),
            ));
        }
        if header[..k].contains(h) {
            return Some(refused(
                "header",
                format!("two columns are named `{h}` in the header"),
            ));
        }
    }
    None
}

/// How the columns of a header are read, or what is refused.
#[allow(clippy::too_many_arguments)]
fn column_specs(
    header: &[String],
    names: &Robj,
    kinds: &Robj,
    levels: &Robj,
    other: &str,
    na_strings: &Robj,
    na_strings_in: &Robj,
    recode_in: &Robj,
) -> std::result::Result<Vec<ColumnSpec>, Robj> {
    let (names, kinds) = (given(names), given(kinds));
    let levels: Vec<Robj> = levels
        .as_list()
        .map(|l| l.values().collect())
        .unwrap_or_default();
    let missing = given(na_strings);
    let missing_in = named(na_strings_in);
    let recode_in = named(recode_in);
    for (arg, of) in [("na_strings_in", &missing_in), ("recode_in", &recode_in)] {
        if let Some((k, _)) = of.iter().find(|(k, _)| !header.contains(k)) {
            return Err(refused(
                "argument",
                format!("`{arg}` names `{k}`, which is not a column of the file"),
            ));
        }
    }
    let mut specs = Vec::with_capacity(header.len());
    for h in header {
        let k = match names.iter().position(|n| n == h) {
            Some(i) => kind(&kinds[i], &levels[i]),
            None => kind(other, &().into()),
        };
        let mut recode = Vec::new();
        if let Some((_, v)) = recode_in.iter().find(|(n, _)| n == h) {
            for (i, t) in v.names().into_iter().flatten().enumerate() {
                match scalar(v, i, &k) {
                    Some(s) => recode.push((t.to_string(), s)),
                    None if k == Kind::Infer => {
                        recode.push((t.to_string(), Scalar::Logical(true)));
                    }
                    None => {
                        return Err(refused(
                            "argument",
                            format!(
                                "`recode_in` gives a value of another type for \"{t}\" of `{h}`"
                            ),
                        ));
                    }
                }
            }
        }
        let spec = ColumnSpec {
            kind: k,
            missing: missing.clone(),
            missing_here: missing_in
                .iter()
                .filter(|(n, _)| n == h)
                .flat_map(|(_, v)| given(v))
                .collect(),
            recode,
        };
        if let Some((i, why)) = spec.refused() {
            let t = &spec.recode[i].0;
            return Err(refused(
                "argument",
                match why {
                    Refused::Inferred => format!(
                        "`recode_in` names `{h}`, whose type is not declared; a value is of a declared column"
                    ),
                    Refused::Twice => format!("`recode_in` gives \"{t}\" of `{h}` two values"),
                    Refused::Missing => format!(
                        "`recode_in` gives a value for \"{t}\" of `{h}`, which `na_strings` or `na_strings_in` makes missing"
                    ),
                    Refused::Reads => format!(
                        "`recode_in` gives a value for \"{t}\" of `{h}`, which reads as it is"
                    ),
                    Refused::Kind => {
                        format!("`recode_in` gives a value of another type for \"{t}\" of `{h}`")
                    }
                },
            ));
        }
        specs.push(spec);
    }
    Ok(specs)
}

/// The options of a read, or the refusal of the time zone.
fn read_options(limit: i32, time_zone: &str) -> std::result::Result<ReadOptions, Robj> {
    let Some(zone) = Zone::named(time_zone) else {
        return Err(refused(
            "argument",
            format!(
                "`time_zone` is the IANA name of a time zone, such as \"America/New_York\"; \"{time_zone}\" is not one"
            ),
        ));
    };
    Ok(ReadOptions {
        limit: usize::try_from(limit).unwrap_or(0),
        zone,
    })
}

/// What was read, or what is refused, for R.
fn outcome(header: &[String], specs: &[ColumnSpec], read: Read) -> Result<Robj> {
    if read.rows > i32::MAX as u64 {
        return Ok(refused(
            "rows",
            format!(
                "a table has at most 2,147,483,647 rows; the file has {}",
                read.rows
            ),
        ));
    }
    written(header, specs, read)
}

macro_rules! or_refused {
    ($e:expr) => {
        match $e {
            Ok(v) => v,
            Err(r) => return Ok(r),
        }
    };
}

/// Reads a delimited file.
///
/// @param bytes Raw: The bytes of the file, or `NULL` for those of `path`.
/// @param path Character: The file.
/// @param delimiter Integer: The byte between two cells.
/// @param encoding Character: `"UTF-8"`, `"latin1"`, or `"windows-1252"`.
/// @param names Character: The names of the declared columns.
/// @param kinds Character: Their kinds.
/// @param levels List: Their levels.
/// @param other Character: The kind of the other columns: `"infer"` or `"character"`.
/// @param na_strings Character: The texts of missing cells.
/// @param na_strings_in List: Those of columns, by name.
/// @param recode_in List: The values of texts of columns, by name, named vectors.
/// @param limit Integer: The cells that do not read that each column lists.
/// @param time_zone Character: The zone of the date-times without an offset.
/// @return List: what was read, or `refused` and `message`.
/// @keywords internal
/// @noRd
#[extendr]
#[allow(clippy::too_many_arguments)]
fn read_delimited(
    bytes: Robj,
    path: &str,
    delimiter: i32,
    encoding: &str,
    names: Robj,
    kinds: Robj,
    levels: Robj,
    other: &str,
    na_strings: Robj,
    na_strings_in: Robj,
    recode_in: Robj,
    limit: i32,
    time_zone: &str,
) -> Result<Robj> {
    interruptible(|| {
        let options = or_refused!(read_options(limit, time_zone));
        let held;
        let bytes: &[u8] = match bytes.as_raw_slice() {
            Some(_) if bytes.len() == 0 => &[],
            Some(b) => b,
            None => {
                held = std::fs::read(path).map_err(|e| Error::Other(e.to_string()))?;
                &held
            }
        };
        let encoding = match encoding {
            "latin1" => Encoding::Latin1,
            "windows-1252" => Encoding::Windows1252,
            _ => Encoding::Utf8,
        };
        let text = match decode(bytes, encoding) {
            Ok(t) => t,
            Err(e) => return Ok(refused("encoding", e.to_string())),
        };
        let delimiter = u8::try_from(delimiter).unwrap_or(0);
        let csv = match Csv::new(&text, Options { delimiter }) {
            Ok(c) => c,
            Err(e) => return Ok(refused("malformed", e.to_string())),
        };
        let header = csv.header();
        if let Some(r) = header_refused(header) {
            return Ok(r);
        }
        let specs = or_refused!(column_specs(
            header,
            &names,
            &kinds,
            &levels,
            other,
            &na_strings,
            &na_strings_in,
            &recode_in
        ));
        let read = match csv.read(&specs, &options) {
            Ok(r) => r,
            Err(e) => return Ok(refused("malformed", e.to_string())),
        };
        outcome(header, &specs, read)
    })
}

/// Reads a Parquet file.
///
/// @param path Character: The file.
/// @param names,kinds,levels,other,na_strings,na_strings_in,recode_in,limit,time_zone
///   As `read_delimited()` takes them.
/// @return List: what was read, or `refused` and `message`.
/// @keywords internal
/// @noRd
#[extendr]
#[allow(clippy::too_many_arguments)]
fn read_parquet(
    path: &str,
    names: Robj,
    kinds: Robj,
    levels: Robj,
    other: &str,
    na_strings: Robj,
    na_strings_in: Robj,
    recode_in: Robj,
    limit: i32,
    time_zone: &str,
) -> Result<Robj> {
    interruptible(|| {
        let options = or_refused!(read_options(limit, time_zone));
        let file = match Parquet::open(path) {
            Ok(f) => f,
            Err(e) => return Ok(parquet_refused(e)),
        };
        let header = file.header();
        if let Some(r) = header_refused(header) {
            return Ok(r);
        }
        let specs = or_refused!(column_specs(
            header,
            &names,
            &kinds,
            &levels,
            other,
            &na_strings,
            &na_strings_in,
            &recode_in
        ));
        let read = match file.read(&specs, &options) {
            Ok(r) => r,
            Err(e) => return Ok(parquet_refused(e)),
        };
        outcome(header, &specs, read)
    })
}

fn parquet_refused(e: ParquetError) -> Robj {
    match e {
        ParquetError::File(m) => refused("malformed", m),
        e @ ParquetError::Column { .. } => refused("column", e.to_string()),
    }
}

/// What was read, as R's vectors.
fn written(header: &[String], specs: &[ColumnSpec], mut read: Read) -> Result<Robj> {
    let row = |r: u64| r as i32 + 1;
    let mut columns: Vec<Robj> = Vec::with_capacity(header.len());
    for (c, s) in read.columns.drain(..).zip(specs) {
        columns.push(vector(&c, &s.kind)?);
    }
    let mut columns: Robj = List::from_values(columns).into();
    columns.set_attrib("names", strings(header.len(), |i| Some(&header[i])))?;
    let unread = &read.unread;
    let unread: Robj = list!(
        column = strings(unread.len(), |i| Some(&header[unread[i].column])),
        text = strings(unread.len(), |i| unread[i].text.as_deref()),
        cells = unread.iter().map(|u| u.cells as f64).collect::<Vec<_>>(),
        rows = List::from_values(
            unread
                .iter()
                .map(|u| u.rows.iter().map(|&r| row(r)).collect::<Vec<_>>())
        )
    )
    .into();
    let cells = &read.unread_cells;
    let cells: Robj = list!(
        column = strings(cells.len(), |i| Some(&header[cells[i].column])),
        row = cells.iter().map(|c| row(c.row)).collect::<Vec<_>>(),
        text = strings(cells.len(), |i| Some(&cells[i].text))
    )
    .into();
    let recoded = &read.recoded;
    let recoded: Robj = list!(
        column = strings(recoded.len(), |i| Some(&header[recoded[i].column])),
        text = strings(recoded.len(), |i| Some(
            &specs[recoded[i].column].recode[recoded[i].text].0
        )),
        value = recoded
            .iter()
            .map(|r| value_text(&specs[r.column].recode[r.text].1))
            .collect::<Vec<_>>(),
        cells = recoded.iter().map(|r| r.cells as f64).collect::<Vec<_>>()
    )
    .into();
    let lines: Robj = list!(
        line = read
            .widths
            .iter()
            .map(|w| w.line as f64)
            .collect::<Vec<_>>(),
        cells = read
            .widths
            .iter()
            .map(|w| w.cells as i32)
            .collect::<Vec<_>>(),
        all = read.other_widths as f64
    )
    .into();
    Ok(list!(
        rows = read.rows as i32,
        columns = columns,
        unread = unread,
        cells = cells,
        recoded = recoded,
        lines = lines
    )
    .into())
}

/// A value as R writes it.
fn value_text(v: &Scalar) -> String {
    match v {
        Scalar::Logical(b) => (if *b { "TRUE" } else { "FALSE" }).to_string(),
        Scalar::Integer(i) => i.to_string(),
        Scalar::Double(d) => number_text(*d),
        Scalar::Text(t) => format!("{t:?}"),
        Scalar::Date(d) => date_text(i64::from(*d)),
        Scalar::DateTime(m) => datetime_text(*m),
    }
}

/// How the elements of R's vector are written.
#[derive(Clone, Copy, PartialEq)]
enum Form {
    Logical,
    Integer,
    Double,
    Text,
    Date,
    DateTime,
    Factor,
}

/// The cells of R's vector.
struct Vector {
    x: SEXP,
    form: Form,
    levels: Vec<String>,
}

impl Vector {
    fn of(x: &Robj) -> Result<Vector> {
        let s = unsafe { x.get() };
        let form = if x.inherits("factor") {
            Form::Factor
        } else if x.inherits("Date") {
            Form::Date
        } else if x.inherits("POSIXct") {
            Form::DateTime
        } else {
            match unsafe { TYPEOF(s) } {
                SEXPTYPE::LGLSXP => Form::Logical,
                SEXPTYPE::INTSXP => Form::Integer,
                SEXPTYPE::REALSXP => Form::Double,
                SEXPTYPE::STRSXP => Form::Text,
                _ => return Err(Error::Other("a column has no text".to_string())),
            }
        };
        let levels = match form {
            Form::Factor => x
                .get_attrib("levels")
                .map(|l| given(&l))
                .unwrap_or_default(),
            _ => Vec::new(),
        };
        Ok(Vector { x: s, form, levels })
    }

    /// The number at `i`, `None` for a missing one.
    fn number(&self, i: usize) -> Option<f64> {
        let i = i as R_xlen_t;
        unsafe {
            match TYPEOF(self.x) {
                SEXPTYPE::INTSXP => Some(INTEGER_ELT(self.x, i))
                    .filter(|v| *v != NA_INTEGER)
                    .map(f64::from),
                _ => Some(REAL_ELT(self.x, i)).filter(|v| !v.is_nan()),
            }
        }
    }
}

impl Cells for Vector {
    fn is_text(&self) -> bool {
        matches!(self.form, Form::Text | Form::Factor)
    }

    fn text(&self, row: usize, out: &mut String) -> bool {
        let i = row as R_xlen_t;
        unsafe {
            match self.form {
                Form::Logical => match LOGICAL_ELT(self.x, i) {
                    NA_INTEGER => return false,
                    v => out.push_str(if v != 0 { "TRUE" } else { "FALSE" }),
                },
                Form::Integer => match INTEGER_ELT(self.x, i) {
                    NA_INTEGER => return false,
                    v => out.push_str(&v.to_string()),
                },
                Form::Double => {
                    let v = REAL_ELT(self.x, i);
                    if is_missing_double(v) {
                        return false;
                    }
                    out.push_str(&number_text(v));
                }
                Form::Text => {
                    let c = STRING_ELT(self.x, i);
                    if c == R_NaString {
                        return false;
                    }
                    let t = std::ffi::CStr::from_ptr(extendr_ffi::R_CHAR(c));
                    out.push_str(&t.to_string_lossy());
                }
                Form::Factor => match INTEGER_ELT(self.x, i) {
                    NA_INTEGER => return false,
                    k => match self.levels.get((k - 1) as usize) {
                        Some(l) => out.push_str(l),
                        None => return false,
                    },
                },
                Form::Date => match self.number(row) {
                    None => return false,
                    Some(d) => out.push_str(&date_text(d as i64)),
                },
                Form::DateTime => match self.number(row) {
                    None => return false,
                    Some(s) => out.push_str(&datetime_text((s * 1e6).round() as i64)),
                },
            }
        }
        true
    }
}

/// Writes columns as a delimited file.
///
/// @param columns List: The columns by name.
/// @param path Character: The file.
/// @param delimiter Integer: The byte between two cells.
/// @return `NULL`, or the message of a file that is not written.
/// @keywords internal
/// @noRd
#[extendr]
fn write_delimited(columns: List, path: &str, delimiter: i32) -> Result<Robj> {
    interruptible(|| {
        let names: Vec<String> = columns.iter().map(|(k, _)| k.to_string()).collect();
        let names: Vec<&str> = names.iter().map(String::as_str).collect();
        let held: Vec<Robj> = columns.values().collect();
        let vectors = held.iter().map(Vector::of).collect::<Result<Vec<_>>>()?;
        let cells: Vec<&dyn Cells> = vectors.iter().map(|v| v as &dyn Cells).collect();
        let rows = held.first().map_or(0, |c| c.len());
        let delimiter = u8::try_from(delimiter).unwrap_or(b',');
        let done = std::fs::File::create(path)
            .and_then(|mut f| rtemis_r8_io::write(&mut f, &names, &cells, rows, delimiter));
        Ok(match done {
            Ok(()) => ().into(),
            Err(e) => e.to_string().into(),
        })
    })
}

/// The texts of a character vector of UTF-8.
struct Strings(SEXP);

impl TextColumn for Strings {
    fn text(&self, i: usize) -> Option<&str> {
        unsafe {
            let c = STRING_ELT(self.0, i as R_xlen_t);
            if c == R_NaString {
                return None;
            }
            // The vector is protected by its caller, and its texts are UTF-8.
            std::ffi::CStr::from_ptr(extendr_ffi::R_CHAR(c))
                .to_str()
                .ok()
        }
    }
}

/// The cells of R's vector, as Parquet takes them.
enum Held {
    Slice(Vector),
    Days(Vec<i32>),
    Micros(Vec<i64>),
}

/// Writes columns as a Parquet file.
///
/// @param columns List: The columns by name, of UTF-8.
/// @param path Character: The file.
/// @return `NULL`, or the message of a file that is not written.
/// @keywords internal
/// @noRd
#[extendr]
fn write_parquet_file(columns: List, path: &str) -> Result<Robj> {
    interruptible(|| {
        let names: Vec<String> = columns.iter().map(|(k, _)| k.to_string()).collect();
        let names: Vec<&str> = names.iter().map(String::as_str).collect();
        let held: Vec<Robj> = columns.values().collect();
        let vectors = held.iter().map(Vector::of).collect::<Result<Vec<_>>>()?;
        let rows = held.first().map_or(0, |c| c.len());
        let cells: Vec<Held> = vectors
            .into_iter()
            .map(|v| match v.form {
                Form::Date => Held::Days(
                    (0..rows)
                        .map(|i| v.number(i).map_or(MISSING_INTEGER, |d| d.floor() as i32))
                        .collect(),
                ),
                Form::DateTime => Held::Micros(
                    (0..rows)
                        .map(|i| {
                            v.number(i)
                                .map_or(MISSING_DATETIME, |s| (s * 1e6).round() as i64)
                        })
                        .collect(),
                ),
                _ => Held::Slice(v),
            })
            .collect();
        let strings: Vec<Strings> = cells
            .iter()
            .map(|c| match c {
                Held::Slice(v) => Strings(v.x),
                _ => Strings(std::ptr::null_mut()),
            })
            .collect();
        let out: Vec<Out<'_>> = cells
            .iter()
            .zip(&strings)
            .map(|(c, text)| match c {
                Held::Days(d) => Out::Date(d),
                Held::Micros(m) => Out::DateTime(m),
                Held::Slice(v) => unsafe {
                    let n = rows;
                    let ints = |x: SEXP| -> &[i32] {
                        if n == 0 {
                            &[]
                        } else {
                            std::slice::from_raw_parts(INTEGER(x), n)
                        }
                    };
                    match v.form {
                        Form::Logical => Out::Logical(if n == 0 {
                            &[]
                        } else {
                            std::slice::from_raw_parts(LOGICAL(v.x), n)
                        }),
                        Form::Integer => Out::Integer(ints(v.x)),
                        Form::Double => Out::Double(if n == 0 {
                            &[]
                        } else {
                            std::slice::from_raw_parts(REAL(v.x), n)
                        }),
                        Form::Text => Out::Text(text),
                        Form::Factor => Out::Factor {
                            codes: ints(v.x),
                            levels: &v.levels,
                        },
                        Form::Date | Form::DateTime => {
                            unreachable!("held as days and microseconds")
                        }
                    }
                },
            })
            .collect();
        let done = std::fs::File::create(path)
            .map_err(|e| e.to_string())
            .and_then(|f| {
                write_parquet(std::io::BufWriter::new(f), &names, &out, rows)
                    .map_err(|e| e.to_string())
            });
        Ok(match done {
            Ok(()) => ().into(),
            Err(e) => e.into(),
        })
    })
}

extendr_module! {
    mod R8_io;
    fn read_delimited;
    fn read_parquet;
    fn write_delimited;
    fn write_parquet_file;
}
