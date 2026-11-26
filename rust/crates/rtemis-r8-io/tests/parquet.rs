//! Parquet files: those of other writers, and those that the crate writes.

#![cfg(feature = "parquet")]

use rtemis_r8_io::parquet::{Column as Out, Parquet, ParquetError, TextColumn, write_parquet};
use rtemis_r8_io::{
    Column, ColumnSpec, Kind, MISSING_DATETIME, MISSING_INTEGER, Read, ReadOptions, Scalar, Zone,
    missing_double,
};

fn fixture(name: &str) -> String {
    format!("{}/tests/fixtures/{name}", env!("CARGO_MANIFEST_DIR"))
}

fn read_as(path: &str, kind: impl Fn(&str) -> Kind, options: &ReadOptions) -> (Vec<String>, Read) {
    let p = Parquet::open(path).unwrap();
    let specs: Vec<ColumnSpec> = p
        .header()
        .iter()
        .map(|h| ColumnSpec {
            missing: vec![String::new(), "NA".to_string()],
            ..ColumnSpec::new(kind(h))
        })
        .collect();
    (p.header().to_vec(), p.read(&specs, options).unwrap())
}

fn inferred(path: &str) -> Vec<(String, Column)> {
    let (h, r) = read_as(path, |_| Kind::Infer, &ReadOptions::default());
    assert!(r.unread.is_empty());
    h.into_iter().zip(r.columns).collect()
}

fn column<'a>(of: &'a [(String, Column)], name: &str) -> &'a Column {
    &of.iter().find(|(h, _)| h == name).unwrap().1
}

fn texts(c: &Column) -> Vec<Option<&str>> {
    let Column::Text(x) = c else {
        panic!("a column of text: {c:?}")
    };
    (0..x.len()).map(|i| x.get(i)).collect()
}

fn doubles(c: &Column) -> Vec<Option<f64>> {
    let Column::Double(x) = c else {
        panic!("a column of doubles: {c:?}")
    };
    x.iter()
        .map(|v| (v.to_bits() != missing_double().to_bits()).then_some(*v))
        .collect()
}

const NA: i32 = MISSING_INTEGER;
const TS: i64 = 1_580_464_800_000_000;

#[test]
fn the_files_of_arrow_read_as_their_types() {
    for codec in ["snappy", "zstd", "gzip"] {
        let c = inferred(&fixture(&format!("arrow_{codec}.parquet")));
        assert_eq!(*column(&c, "i"), Column::Integer(vec![1, NA, 3]));
        let d = doubles(column(&c, "d"));
        assert!(d[0] == Some(1.5) && d[1].unwrap().is_nan() && d[2].is_none());
        // A string is a quoted text: "" is a text.
        assert_eq!(texts(column(&c, "s")), [Some("a"), None, Some("")]);
        assert_eq!(*column(&c, "b"), Column::Logical(vec![1, NA, 0]));
        assert_eq!(*column(&c, "dt"), Column::Date(vec![18292, NA, -1]));
        assert_eq!(
            *column(&c, "ts"),
            Column::DateTime(vec![TS, MISSING_DATETIME, TS + 500_000])
        );
        // A dictionary is an encoding: the factor of R is a text.
        assert_eq!(texts(column(&c, "f")), [Some("lo"), Some("hi"), None]);
        assert_eq!(*column(&c, "i64"), Column::Integer(vec![1, 2, NA]));
        // 2^40 is beyond R8's integers and within 2^53.
        assert_eq!(
            doubles(column(&c, "big")),
            [Some(1_099_511_627_776.0), Some(1.0), Some(2.0)]
        );
        // A timestamp without a zone is the clock reading of UTC.
        assert_eq!(
            *column(&c, "naive"),
            Column::DateTime(vec![TS, MISSING_DATETIME, 1_609_459_200_000_000])
        );
        assert_eq!(
            *column(&c, "ns"),
            Column::DateTime(vec![TS, MISSING_DATETIME, 1_609_459_200_000_000])
        );
        assert_eq!(doubles(column(&c, "dec")), [Some(1.25), None, Some(3.0)]);
    }
}

#[test]
fn the_files_of_nanoparquet_and_duckdb_read() {
    let c = inferred(&fixture("nano.parquet"));
    assert_eq!(*column(&c, "i"), Column::Integer(vec![1, NA, 3]));
    assert_eq!(texts(column(&c, "f")), [Some("lo"), Some("hi"), None]);
    assert_eq!(
        *column(&c, "ts"),
        Column::DateTime(vec![TS, MISSING_DATETIME, TS + 500_000])
    );
    let c = inferred(&fixture("duck_flat.parquet"));
    assert_eq!(*column(&c, "a"), Column::Integer(vec![1, NA]));
    // DECIMAL(10, 2) is a double, DECIMAL(5, 0) a whole number.
    assert_eq!(doubles(column(&c, "dec")), [Some(2.5), Some(-0.01)]);
    assert_eq!(*column(&c, "d0"), Column::Integer(vec![7, NA]));
    assert_eq!(doubles(column(&c, "big")), [Some(123_456_789_012.34), None]);
    assert_eq!(
        *column(&c, "ts"),
        Column::DateTime(vec![1_577_872_800_000_000, MISSING_DATETIME])
    );
}

#[test]
fn a_clock_reading_is_an_instant_of_the_zone() {
    let options = ReadOptions {
        zone: Zone::named("America/New_York").unwrap(),
        ..ReadOptions::default()
    };
    let (h, r) = read_as(&fixture("duck_flat.parquet"), |_| Kind::Infer, &options);
    let ts = h.iter().position(|n| n == "ts").unwrap();
    // 10:00 in New York on 2020-01-01 is 15:00 UTC.
    assert_eq!(
        r.columns[ts],
        Column::DateTime(vec![
            1_577_872_800_000_000 + 5 * 3_600_000_000,
            MISSING_DATETIME
        ])
    );
}

#[test]
fn a_type_without_an_r8_kind_is_refused_with_its_column() {
    let refused = |name: &str| match Parquet::open(&fixture(name)) {
        Err(e) => e,
        Ok(p) => p
            .read(
                &vec![ColumnSpec::new(Kind::Infer); p.header().len()],
                &ReadOptions::default(),
            )
            .unwrap_err(),
    };
    for (file, column) in [
        ("duck_nested.parquet", "lst"),
        ("duck_blob.parquet", "bin"),
        ("duck_time.parquet", "tm"),
    ] {
        match refused(file) {
            ParquetError::Column { column: c, .. } => assert_eq!(c, column, "{file}"),
            e => panic!("{file}: {e}"),
        }
    }
    assert!(matches!(
        Parquet::open(&fixture("README.md")),
        Err(ParquetError::File(_))
    ));
}

#[test]
fn a_declared_kind_reads_the_values_that_are_of_it_exactly() {
    let kind = |h: &str| match h {
        "i64" | "big" | "d" | "dec" => Kind::Integer,
        "i" => Kind::Double,
        "s" | "dt" => Kind::Text,
        "f" => Kind::Factor(vec!["lo".to_string(), "hi".to_string()]),
        "ts" => Kind::Date,
        _ => Kind::Infer,
    };
    let (h, r) = read_as(
        &fixture("arrow_snappy.parquet"),
        kind,
        &ReadOptions::default(),
    );
    let at = |n: &str| h.iter().position(|x| x == n).unwrap();
    assert_eq!(r.columns[at("i64")], Column::Integer(vec![1, 2, NA]));
    assert_eq!(r.columns[at("big")], Column::Integer(vec![NA, 1, 2]));
    assert_eq!(doubles(&r.columns[at("i")]), [Some(1.0), None, Some(3.0)]);
    // Any value reads as its text.
    assert_eq!(
        texts(&r.columns[at("dt")]),
        [Some("2020-01-31"), None, Some("1969-12-31")]
    );
    assert_eq!(r.columns[at("f")], Column::Factor(vec![1, 2, NA]));
    let unread: Vec<_> = r
        .unread
        .iter()
        .map(|u| (h[u.column].as_str(), u.text.as_deref().unwrap(), u.cells))
        .collect();
    assert_eq!(
        unread,
        [
            ("d", "1.5", 1),
            ("d", "NaN", 1),
            ("ts", "2020-01-31T10:00:00.000000Z", 1),
            ("ts", "2020-01-31T10:00:00.500000Z", 1),
            ("big", "1099511627776", 1),
            ("dec", "1.25", 1),
        ]
    );
}

#[test]
fn a_typed_value_is_recoded_and_made_missing_by_its_text() {
    let p = Parquet::open(&fixture("arrow_snappy.parquet")).unwrap();
    let specs: Vec<ColumnSpec> = p
        .header()
        .iter()
        .map(|h| match h.as_str() {
            "d" => ColumnSpec {
                recode: vec![("1.5".to_string(), Scalar::Integer(2))],
                missing_here: vec!["NaN".to_string()],
                ..ColumnSpec::new(Kind::Integer)
            },
            "i" => ColumnSpec {
                missing_here: vec!["3".to_string()],
                ..ColumnSpec::new(Kind::Infer)
            },
            _ => ColumnSpec::new(Kind::Infer),
        })
        .collect();
    let r = p.read(&specs, &ReadOptions::default()).unwrap();
    let at = |n: &str| p.header().iter().position(|x| x == n).unwrap();
    assert!(r.unread.is_empty());
    assert_eq!(r.columns[at("d")], Column::Integer(vec![2, NA, NA]));
    assert_eq!(r.columns[at("i")], Column::Integer(vec![1, NA, NA]));
    assert_eq!(r.recoded[0].cells, 1);
}

struct Strings(Vec<Option<String>>);

impl TextColumn for Strings {
    fn text(&self, i: usize) -> Option<&str> {
        self.0[i].as_deref()
    }
}

#[test]
fn a_table_that_is_written_reads_back() {
    let n = 2_500;
    let logical: Vec<i32> = (0..n).map(|i| [0, 1, NA][i % 3]).collect();
    let integer: Vec<i32> = (0..n)
        .map(|i| if i % 7 == 0 { NA } else { i as i32 - 1000 })
        .collect();
    let na = missing_double();
    let double: Vec<f64> = (0..n)
        .map(|i| [0.1, f64::NAN, na, f64::INFINITY, -2.5e-300][i % 5])
        .collect();
    let text = Strings(
        (0..n)
            .map(|i| {
                ["", "NA", "a,\"b\"\n", "é"]
                    .get(i % 5)
                    .map(|t| t.to_string())
            })
            .collect(),
    );
    let levels = vec!["lo".to_string(), "hi".to_string()];
    let codes: Vec<i32> = (0..n).map(|i| [1, 2, NA][i % 3]).collect();
    let date: Vec<i32> = (0..n)
        .map(|i| if i % 4 == 0 { NA } else { i as i32 - 1000 })
        .collect();
    let datetime: Vec<i64> = (0..n)
        .map(|i| {
            if i % 4 == 1 {
                MISSING_DATETIME
            } else {
                i as i64 * 1_000_001 - 7
            }
        })
        .collect();
    let path = std::env::temp_dir().join(format!("r8-io-{}.parquet", std::process::id()));
    let file = std::fs::File::create(&path).unwrap();
    write_parquet(
        file,
        &["l", "i", "d", "t", "f", "date", "dt"],
        &[
            Out::Logical(&logical),
            Out::Integer(&integer),
            Out::Double(&double),
            Out::Text(&text),
            Out::Factor {
                codes: &codes,
                levels: &levels,
            },
            Out::Date(&date),
            Out::DateTime(&datetime),
        ],
        n,
    )
    .unwrap();
    let path = path.to_str().unwrap().to_string();
    let p = Parquet::open(&path).unwrap();
    // No text is missing by a code: what was written reads back.
    let specs = vec![
        ColumnSpec {
            missing: Vec::new(),
            ..ColumnSpec::new(Kind::Infer)
        };
        7
    ];
    let r = p.read(&specs, &ReadOptions::default()).unwrap();
    std::fs::remove_file(&path).unwrap();
    assert_eq!(r.rows, n as u64);
    assert_eq!(r.columns[0], Column::Logical(logical));
    assert_eq!(r.columns[1], Column::Integer(integer));
    let Column::Double(d) = &r.columns[2] else {
        panic!()
    };
    assert!(
        d.iter()
            .zip(&double)
            .all(|(a, b)| a.to_bits() == b.to_bits())
    );
    assert_eq!(
        texts(&r.columns[3]),
        text.0.iter().map(|t| t.as_deref()).collect::<Vec<_>>()
    );
    let labels: Vec<Option<&str>> = codes
        .iter()
        .map(|&k| (k != NA).then(|| levels[k as usize - 1].as_str()))
        .collect();
    assert_eq!(texts(&r.columns[4]), labels);
    assert_eq!(r.columns[5], Column::Date(date));
    assert_eq!(r.columns[6], Column::DateTime(datetime));
}
