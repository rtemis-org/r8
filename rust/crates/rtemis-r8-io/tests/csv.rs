//! The splitting of delimited text, and the kinds of its columns.

use rtemis_r8_io::{
    Column, ColumnSpec, Csv, Encoding, Kind, MISSING_INTEGER, MalformedKind, Options, Read,
    ReadOptions, Refused, Scalar, Zone, decode, text,
};

fn limit(limit: usize) -> ReadOptions {
    ReadOptions {
        limit,
        ..ReadOptions::default()
    }
}

fn read(t: &str, kinds: &[Kind]) -> Read {
    let csv = Csv::new(t, Options::default()).unwrap();
    let specs: Vec<ColumnSpec> = kinds
        .iter()
        .map(|k| ColumnSpec {
            missing: vec![String::new(), "NA".to_string()],
            ..ColumnSpec::new(k.clone())
        })
        .collect();
    csv.read(&specs, &limit(5)).unwrap()
}

fn inferred(t: &str) -> Vec<Column> {
    let n = Csv::new(t, Options::default()).unwrap().header().len();
    read(t, &vec![Kind::Infer; n]).columns
}

fn texts(c: &Column) -> Vec<Option<&str>> {
    let Column::Text(x) = c else {
        panic!("a column of text: {c:?}")
    };
    (0..x.len()).map(|i| x.get(i)).collect()
}

#[test]
fn cells_split_as_the_standard_states() {
    let t = "a,b\n\"x, \"\"y\"\"\nz\",2\r\nu\"v,\rw,\n";
    let r = read(t, &[Kind::Text, Kind::Text]);
    assert_eq!(
        texts(&r.columns[0]),
        [Some("x, \"y\"\nz"), Some("u\"v"), Some("w")]
    );
    assert_eq!(texts(&r.columns[1]), [Some("2"), None, None]);
    // The last line has no end, or the last cell no character.
    for t in ["a,b\n1,2", "a,b\n1,2\n", "\n\na,b\n\n1,2\n\n"] {
        assert_eq!(inferred(t)[1], Column::Integer(vec![2]), "{t:?}");
    }
    assert_eq!(texts(&inferred("a,b\nx,")[0]), [Some("x")]);
    // An empty line is a missing cell of one column.
    assert_eq!(
        inferred("a\n1\n\n2\n")[0],
        Column::Integer(vec![1, MISSING_INTEGER, 2])
    );
}

#[test]
fn a_text_that_does_not_split_gives_its_line() {
    let of = |t: &str| {
        let e = Csv::new(t, Options::default())
            .and_then(|c| {
                c.read(
                    &vec![ColumnSpec::new(Kind::Infer); c.header().len()],
                    &limit(5),
                )
            })
            .unwrap_err();
        (e.line, e.kind)
    };
    assert_eq!(of("a,b\n\"x\"y,2\n"), (2, MalformedKind::AfterQuote));
    assert_eq!(of("a,b\n1,2\n\"x,2\n3,4\n"), (3, MalformedKind::Unclosed));
    assert_eq!(of("a\n\"x\ny\"\n\"z"), (4, MalformedKind::Unclosed));
    assert_eq!(of("\n\n"), (1, MalformedKind::Empty));
    let e = Csv::new("a", Options { delimiter: b'"' }).unwrap_err();
    assert_eq!(e.kind, MalformedKind::Delimiter);
    // A line of another width is counted, and the others are read.
    let r = read("a,b\n1,2\n3\n4,5,6\n7,8\n", &[Kind::Infer, Kind::Infer]);
    assert_eq!((r.rows, r.other_widths), (2, 2));
    assert_eq!(
        r.widths
            .iter()
            .map(|w| (w.line, w.cells))
            .collect::<Vec<_>>(),
        [(3, 1), (4, 3)]
    );
    assert_eq!(r.columns[0], Column::Integer(vec![1, 7]));
}

#[test]
fn a_column_has_the_kind_that_all_its_cells_have() {
    let one = |cells: &[&str]| inferred(&format!("a,b\n{},\n", cells.join(",\n"))).remove(0);
    assert_eq!(
        one(&["1", "NA", "-3"]),
        Column::Integer(vec![1, MISSING_INTEGER, -3])
    );
    assert_eq!(one(&["TRUE", "false"]), Column::Logical(vec![1, 0]));
    assert!(matches!(one(&["1", "2.5"]), Column::Double(x) if x == [1.0, 2.5]));
    assert!(matches!(one(&["1", "3000000000"]), Column::Double(x) if x == [1.0, 3e9]));
    // 2^53 is a double, and the whole numbers beyond it are text.
    assert!(matches!(one(&["9007199254740992", "-9007199254740992"]),
        Column::Double(x) if x == [9_007_199_254_740_992.0, -9_007_199_254_740_992.0]));
    assert!(matches!(one(&["", "1", "", ".5"]),
        Column::Double(x) if x[0].is_nan() && x[1..2] == [1.0] && x[2].is_nan() && x[3] == 0.5));
    assert!(matches!(one(&["1e3", "Inf", "NaN"]), Column::Double(x) if x[1].is_infinite()));
    assert_eq!(
        one(&["2020-01-31", "1970-01-02"]),
        Column::Date(vec![18292, 1])
    );
    assert_eq!(
        one(&["1970-01-01T00:00:01Z", "1970-01-01 01:00:00+01:00"]),
        Column::DateTime(vec![1_000_000, 0])
    );
    // A date-time without an offset is the clock reading of the zone, UTC.
    assert_eq!(
        one(&["2024-01-31T10:00:00", "2024-01-31 10:00:00.5"]),
        Column::DateTime(vec![1_706_695_200_000_000, 1_706_695_200_500_000])
    );
    assert_eq!(one(&[" 1", "2 "]), Column::Integer(vec![1, 2]));
    assert_eq!(one(&["\"1\"", "2"]), Column::Integer(vec![1, 2]));
    // A column of missing cells has the first kind.
    assert_eq!(one(&["NA", ""]), Column::Logical(vec![MISSING_INTEGER; 2]));
    // Every other mixture is the text of the file.
    for cells in [
        vec!["00123", "00456"],
        vec!["1", "00456"],
        vec!["1.50", "x"],
        vec!["1", "2", "missing"],
        vec!["TRUE", "1"],
        vec!["1", "TRUE"],
        vec!["2024-01-31", "2024-02-30"],
        vec!["2024-01-31", "2024-01-31T00:00:00Z"],
        vec!["T", "F"],
        vec!["12345678901234567890"],
        vec!["9007199254740993"],
        vec!["1", "-9007199254740993"],
        vec!["1", "12345678901234567890"],
        vec!["1", "+2"],
        vec!["-0", "1"],
        vec!["1,5".trim_end_matches(",5"), "1e999"],
        vec!["0x10"],
        vec!["1e"],
        vec!["."],
        vec!["-"],
        vec!["inf"],
        vec!["nan"],
        vec!["1_000"],
    ] {
        let c = one(&cells);
        let expected: Vec<Option<&str>> = cells.iter().map(|c| Some(*c)).collect();
        // `-0` is the double.
        if cells[0] == "-0" {
            assert!(matches!(c, Column::Double(x) if x[0] == 0.0 && x[0].is_sign_negative()));
            continue;
        }
        assert_eq!(texts(&c), expected);
    }
}

#[test]
fn a_declared_column_reads_every_text_of_one_meaning() {
    let t = "a,b,c,d\n+5,007,2.,TRUE\n40.0,1e2,.5,true\n-2147483647,-0,+1E-3,False\n";
    let r = read(
        t,
        &[Kind::Integer, Kind::Double, Kind::Double, Kind::Logical],
    );
    assert_eq!(r.columns[0], Column::Integer(vec![5, 40, -2_147_483_647]));
    assert_eq!(r.columns[1], Column::Double(vec![7.0, 100.0, -0.0]));
    assert_eq!(r.columns[2], Column::Double(vec![2.0, 0.5, 1e-3]));
    assert_eq!(r.columns[3], Column::Logical(vec![1, 1, 0]));
    assert!(r.unread.is_empty());
    for t in [
        "1.5",
        "1e2",
        "2147483648",
        "-2147483648",
        "x",
        "0x1",
        "1,0",
        "1 2",
    ] {
        assert_eq!(text::integer(t, true), None, "{t}");
    }
    for t in ["1e999", "", "e5", "1.2.3", "--1", "1e+", "∞", "１"] {
        assert_eq!(text::double(t, true), None, "{t}");
    }
    for t in [
        "2020-1-31",
        "31/01/2020",
        "2020-13-01",
        "2020-01-31 ",
        "２020-01-3",
    ] {
        assert_eq!(text::date(t), None, "{t}");
    }
    for t in [
        "2020-01-31T10:00:00",
        "2020-01-31T24:00:00Z",
        "2020-01-31T10:00Z",
        "2020-01-31T10:00:00.Z",
        "2020-01-31T10:00:00.1234567Z",
        "2020-01-31T10:00:00+0100",
        "2020-01-31T10:00:00Zx",
    ] {
        assert_eq!(text::datetime(t), None, "{t}");
    }
    assert_eq!(
        text::datetime("1969-12-31T23:59:59.5-00:30"),
        Some(1_799_500_000)
    );
}

#[test]
fn a_text_that_does_not_read_is_collected_once() {
    let mut t = String::from("id,lab,g\n");
    for k in 0..50 {
        let lab = match k % 5 {
            0 => "<0.1",
            1 if k < 10 => "hemol",
            _ => "1.5",
        };
        t.push_str(&format!("{k},{lab},{}\n", if k == 7 { "IV" } else { "II" }));
    }
    let kinds = [
        Kind::Integer,
        Kind::Double,
        Kind::Factor(vec!["I".into(), "II".into()]),
    ];
    let r = read(&t, &kinds);
    let of: Vec<_> = r
        .unread
        .iter()
        .map(|u| (u.column, u.text.as_deref().unwrap(), u.cells, u.rows[0]))
        .collect();
    assert_eq!(
        of,
        [(1, "<0.1", 10, 0), (1, "hemol", 2, 1), (2, "IV", 1, 7)]
    );
    assert_eq!(r.rows, 50);
    let cells: Vec<_> = r
        .unread_cells
        .iter()
        .map(|c| (c.column, c.row, &*c.text))
        .collect();
    assert_eq!(
        cells,
        [
            (1, 0, "<0.1"),
            (1, 1, "hemol"),
            (1, 5, "<0.1"),
            (1, 6, "hemol"),
            (1, 10, "<0.1"),
            (2, 7, "IV")
        ]
    );
    // The texts of a free text are many, and are listed to a limit.
    let many: String = (0..1500).map(|k| format!("x{k}\n")).collect();
    let r = read(&format!("a\n{many}"), &[Kind::Double]);
    assert_eq!(r.unread.len(), 1001);
    assert_eq!((r.unread[1000].cells, &r.unread[1000].text), (500, &None));
}

#[test]
fn a_text_reads_as_a_given_value() {
    let t = "lab,g,d\n<0.1,hi,never\n2,lo,2020-01-31\n<0.1,I,never\nhemol,I,\n";
    let csv = Csv::new(t, Options::default()).unwrap();
    let specs = vec![
        ColumnSpec {
            recode: vec![
                ("<0.1".into(), Scalar::Double(0.1)),
                ("<0.2".into(), Scalar::Integer(0)),
            ],
            missing_here: vec!["hemol".into()],
            ..ColumnSpec::new(Kind::Double)
        },
        ColumnSpec {
            recode: vec![
                ("hi".into(), Scalar::Text("II".into())),
                ("lo".into(), Scalar::Text("I".into())),
            ],
            ..ColumnSpec::new(Kind::Factor(vec!["I".into(), "II".into()]))
        },
        ColumnSpec {
            recode: vec![("never".into(), Scalar::Date(0))],
            ..ColumnSpec::new(Kind::Date)
        },
    ];
    assert!(specs.iter().all(|s| s.refused().is_none()));
    let r = csv.read(&specs, &limit(5)).unwrap();
    assert!(
        matches!(&r.columns[0], Column::Double(x) if x[..3] == [0.1, 2.0, 0.1] && x[3].is_nan())
    );
    assert_eq!(r.columns[1], Column::Factor(vec![2, 1, 1, 1]));
    assert_eq!(
        r.columns[2],
        Column::Date(vec![0, 18292, 0, MISSING_INTEGER])
    );
    assert!(r.unread.is_empty());
    assert_eq!(
        r.recoded
            .iter()
            .map(|r| (r.column, r.text, r.cells))
            .collect::<Vec<_>>(),
        [(0, 0, 2), (0, 1, 0), (1, 0, 1), (1, 1, 1), (2, 0, 2)]
    );
    let refused = |kind: Kind, recode: &[(&str, Scalar)]| {
        ColumnSpec {
            recode: recode
                .iter()
                .map(|(t, v)| (t.to_string(), v.clone()))
                .collect(),
            missing_here: vec!["unk".into()],
            ..ColumnSpec::new(kind)
        }
        .refused()
    };
    let d = Scalar::Double(0.1);
    assert_eq!(
        refused(Kind::Double, &[("5", d.clone())]),
        Some((0, Refused::Reads))
    );
    assert_eq!(
        refused(Kind::Double, &[("unk", d.clone())]),
        Some((0, Refused::Missing))
    );
    assert_eq!(
        refused(Kind::Double, &[("", d.clone())]),
        Some((0, Refused::Missing))
    );
    assert_eq!(
        refused(Kind::Integer, &[("x", d.clone())]),
        Some((0, Refused::Kind))
    );
    assert_eq!(
        refused(Kind::Infer, &[("x", d.clone())]),
        Some((0, Refused::Inferred))
    );
    assert_eq!(
        refused(Kind::Double, &[("x", d.clone()), ("x", d.clone())]),
        Some((1, Refused::Twice))
    );
    assert_eq!(
        refused(
            Kind::Factor(vec!["I".into()]),
            &[("x", Scalar::Text("II".into()))]
        ),
        Some((0, Refused::Kind))
    );
}

#[test]
fn a_missing_cell_and_a_text_are_apart() {
    let t = "s,x\n,\n\"\",\"\"\nNA,NA\n\"NA\",\"NA\"\na,1\n";
    let r = read(t, &[Kind::Infer, Kind::Infer]);
    assert_eq!(
        texts(&r.columns[0]),
        [None, Some(""), None, Some("NA"), Some("a")]
    );
    assert_eq!(
        r.columns[1],
        Column::Integer(vec![
            MISSING_INTEGER,
            MISSING_INTEGER,
            MISSING_INTEGER,
            MISSING_INTEGER,
            1
        ])
    );
    // A column of missing cells and quoted `""` and `"NA"` alone is text; a column
    // whose quoted cells are missing codes of it alone is not.
    let r = read(
        "s,n\n,1\n\"\",2\nNA,3\n\"NA\",4\n",
        &[Kind::Infer, Kind::Infer],
    );
    assert_eq!(texts(&r.columns[0]), [None, Some(""), None, Some("NA")]);
    let csv = Csv::new("s\n\"\"\n\"NA\"\n", Options::default()).unwrap();
    let spec = ColumnSpec {
        missing: vec![String::new(), "NA".to_string()],
        missing_here: vec![String::new(), "NA".to_string()],
        ..ColumnSpec::new(Kind::Infer)
    };
    let r = csv.read(&[spec], &limit(5)).unwrap();
    assert_eq!(r.columns[0], Column::Logical(vec![MISSING_INTEGER; 2]));
}

#[test]
fn a_file_is_text_in_its_encoding() {
    assert_eq!(decode(b"\xEF\xBB\xBFa,b", Encoding::Utf8).unwrap(), "a,b");
    assert_eq!(decode(b"\xFF\xFEa\0,\0", Encoding::Utf8).unwrap(), "a,");
    assert_eq!(decode(b"\xFE\xFF\0a\0,", Encoding::Utf8).unwrap(), "a,");
    assert_eq!(decode(b"caf\xE9", Encoding::Latin1).unwrap(), "café");
    assert_eq!(
        decode(b"\x80 \x93q\x94", Encoding::Windows1252).unwrap(),
        "€ “q”"
    );
    let e = decode(b"a\nb\ncaf\xE9,1\n", Encoding::Utf8).unwrap_err();
    assert_eq!((e.line, e.byte, e.nul), (3, 4, false));
    let e = decode(b"a\nb\0\n", Encoding::Utf8).unwrap_err();
    assert_eq!((e.line, e.byte, e.nul), (2, 2, true));
    assert_eq!(decode(b"\x81", Encoding::Windows1252).unwrap_err().byte, 1);
}

/// A generator of numbers, as a seed gives them.
struct Numbers(u64);

impl Numbers {
    fn below(&mut self, n: usize) -> usize {
        self.0 = self
            .0
            .wrapping_mul(0x5851_F42D_4C95_7F2D)
            .wrapping_add(0x1405_7B7E_F767_814F);
        ((self.0 >> 33) as usize) % n
    }

    fn text(&mut self) -> String {
        const PARTS: [&str; 14] = [
            "a", "\"", ",", "\n", "\r", "\r\n", " ", "é", "0", "-1.5", "NA", "\"\"", "TRUE", "😀",
        ];
        (0..self.below(5))
            .map(|_| PARTS[self.below(PARTS.len())])
            .collect()
    }
}

#[test]
fn what_is_written_is_read() {
    for seed in 0..500 {
        let mut g = Numbers(seed);
        let (n, m) = (1 + g.below(4), 1 + g.below(6));
        let cells: Vec<Vec<String>> = (0..m).map(|_| (0..n).map(|_| g.text()).collect()).collect();
        let end = ["\n", "\r\n", "\r"][g.below(3)];
        let quoted = |t: &str| format!("\"{}\"", t.replace('"', "\"\""));
        let mut t = (0..n)
            .map(|k| format!("c{k}"))
            .collect::<Vec<_>>()
            .join(",")
            + end;
        for row in &cells {
            t += &row
                .iter()
                .map(|c| {
                    let plain = !c.is_empty() && !c.contains(['"', ',', '\n', '\r']) && c != "NA";
                    if plain { c.clone() } else { quoted(c) }
                })
                .collect::<Vec<_>>()
                .join(",");
            t += end;
        }
        let r = read(&t, &vec![Kind::Text; n]);
        assert_eq!(
            (r.rows as usize, r.other_widths),
            (m, 0),
            "seed {seed}: {t:?}"
        );
        for k in 0..n {
            let expected: Vec<Option<&str>> = cells.iter().map(|row| Some(&*row[k])).collect();
            assert_eq!(texts(&r.columns[k]), expected, "seed {seed}: {t:?}");
        }
        // Inference changes no text: a column of text has the texts of the file, and
        // a column of another kind has a cell for each row.
        for (k, c) in read(&t, &vec![Kind::Infer; n]).columns.iter().enumerate() {
            assert_eq!(c.len(), m);
            if matches!(c, Column::Text(_)) {
                let expected: Vec<Option<&str>> = cells.iter().map(|row| Some(&*row[k])).collect();
                assert_eq!(texts(c), expected, "seed {seed}: {t:?}");
            }
        }
    }
}

#[test]
fn any_text_is_read_or_refused() {
    for seed in 0..3000 {
        let mut g = Numbers(seed);
        let t: String = (0..g.below(12)).map(|_| g.text()).collect();
        if let Ok(csv) = Csv::new(&t, Options::default()) {
            let n = csv.header().len();
            for kind in [Kind::Infer, Kind::Text, Kind::Double, Kind::DateTime] {
                if let Ok(r) = csv.read(&vec![ColumnSpec::new(kind); n], &limit(5)) {
                    assert!(r.columns.iter().all(|c| c.len() as u64 == r.rows));
                }
            }
        }
    }
}

struct Strings(Vec<Option<String>>);

impl rtemis_r8_io::Cells for Strings {
    fn is_text(&self) -> bool {
        true
    }
    fn text(&self, row: usize, out: &mut String) -> bool {
        match &self.0[row] {
            Some(t) => {
                out.push_str(t);
                true
            }
            None => false,
        }
    }
}

#[test]
fn a_table_that_is_written_reads_back() {
    for seed in 0..500 {
        let mut g = Numbers(seed);
        let (n, m) = (1 + g.below(3), 1 + g.below(6));
        let columns: Vec<Strings> = (0..n)
            .map(|_| Strings((0..m).map(|_| (g.below(5) > 0).then(|| g.text())).collect()))
            .collect();
        let names: Vec<String> = (0..n).map(|k| format!("{}{k}", g.text())).collect();
        let names: Vec<&str> = names.iter().map(String::as_str).collect();
        let cells: Vec<&dyn rtemis_r8_io::Cells> = columns
            .iter()
            .map(|c| c as &dyn rtemis_r8_io::Cells)
            .collect();
        let mut out = Vec::new();
        let delimiter = b",\t;"[g.below(3)];
        rtemis_r8_io::write(&mut out, &names, &cells, m, delimiter).unwrap();
        let t = String::from_utf8(out).unwrap();
        let csv = Csv::new(&t, Options { delimiter }).unwrap();
        assert_eq!(csv.header(), names, "seed {seed}: {t:?}");
        let specs = vec![
            ColumnSpec {
                missing: vec![String::new(), "NA".to_string()],
                ..ColumnSpec::new(Kind::Text)
            };
            n
        ];
        let r = csv.read(&specs, &limit(0)).unwrap();
        for (c, written) in r.columns.iter().zip(&columns) {
            let expected: Vec<Option<&str>> = written.0.iter().map(Option::as_deref).collect();
            assert_eq!(texts(c), expected, "seed {seed}: {t:?}");
        }
    }
}

#[test]
fn a_quoted_text_is_a_level() {
    let r = read(
        "g\n\"\"\n\"NA\"\n\nNA\nx\n",
        &[Kind::Factor(vec!["".into(), "NA".into(), "x".into()])],
    );
    assert_eq!(
        r.columns[0],
        Column::Factor(vec![1, 2, MISSING_INTEGER, MISSING_INTEGER, 3])
    );
}

#[test]
fn a_clock_reading_is_an_instant_of_its_zone() {
    let t =
        "at\n2024-06-01 08:15:00\n2024-03-10 02:30:00\n2024-11-03 01:30:00\n2024-06-01T08:15:00Z\n";
    let csv = Csv::new(t, Options::default()).unwrap();
    let zone = Zone::named("America/New_York").unwrap();
    let options = ReadOptions {
        limit: 5,
        zone: zone.clone(),
    };
    let declared = csv
        .read(&[ColumnSpec::new(Kind::DateTime)], &options)
        .unwrap();
    // 08:15 in New York in June is 12:15 UTC; an offset keeps its instant. The clock
    // skips 02:30 on 2024-03-10 and shows 01:30 twice on 2024-11-03.
    assert_eq!(
        declared.columns[0],
        Column::DateTime(vec![
            1_717_244_100_000_000,
            rtemis_r8_io::MISSING_DATETIME,
            rtemis_r8_io::MISSING_DATETIME,
            1_717_229_700_000_000
        ])
    );
    let unread: Vec<_> = declared
        .unread
        .iter()
        .map(|u| (u.text.as_deref(), u.rows.clone()))
        .collect();
    assert_eq!(
        unread,
        [
            (Some("2024-03-10 02:30:00"), vec![1]),
            (Some("2024-11-03 01:30:00"), vec![2])
        ]
    );
    // Inferred, the column of such texts is text.
    let inferred = csv.read(&[ColumnSpec::new(Kind::Infer)], &options).unwrap();
    assert!(matches!(inferred.columns[0], Column::Text(_)));
    assert!(Zone::named("Mars/Olympus").is_none());
    assert_eq!(Zone::named("UTC").unwrap().instant(5), Some(5));
}
