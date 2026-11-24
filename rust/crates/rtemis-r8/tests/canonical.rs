//! The canonical JSON form, checked against JavaScript, whose
//! `JSON.stringify` and `Number.prototype.toString` RFC 8785 adopts.
//! `tests/fixtures/ecmascript-jcs.json` is written by
//! `tests/fixtures/gen-ecmascript-jcs.mjs`.

use rtemis_r8::canonical::canonical_text;
use serde_json::Value;

const FIXTURE: &str = include_str!("fixtures/ecmascript-jcs.json");

#[test]
fn numbers_match_ecmascript() {
    let fixture: Value = serde_json::from_str(FIXTURE).unwrap();
    let mut mismatches = Vec::new();
    let numbers = fixture["numbers"].as_array().unwrap();
    for pair in numbers {
        let bits = u64::from_str_radix(pair[0].as_str().unwrap(), 16).unwrap();
        let x = f64::from_bits(bits);
        let want = pair[1].as_str().unwrap();
        let got = canonical_text(&Value::from(x));
        if got != want {
            mismatches.push(format!("{x:e} ({bits:016x}): got {got}, JavaScript {want}"));
        }
    }
    assert!(numbers.len() > 2000);
    assert!(mismatches.is_empty(), "{mismatches:#?}");
}

#[test]
fn documents_match_ecmascript() {
    let fixture: Value = serde_json::from_str(FIXTURE).unwrap();
    for pair in fixture["documents"].as_array().unwrap() {
        let doc: Value = serde_json::from_str(pair[0].as_str().unwrap()).unwrap();
        assert_eq!(canonical_text(&doc), pair[1].as_str().unwrap());
    }
}

mod documents {
    use rtemis_r8::canonical::{
        ReadOptions, document_with, normalize, read_document, read_tree_with, to_document,
        value_declaration_hash,
    };
    use rtemis_r8::ir::{IrDocument, TypeDef};
    use rtemis_r8::name::QualifiedName;
    use serde_json::{Value, json};

    fn svm() -> (IrDocument, QualifiedName) {
        (
            IrDocument::from_json(include_str!("fixtures/svm.ir.json")).unwrap(),
            QualifiedName::parse("rtemis::Svm").unwrap(),
        )
    }

    fn codes(
        r: Result<Value, Vec<rtemis_r8::validate::ValueError>>,
    ) -> Vec<(&'static str, String)> {
        r.unwrap_err()
            .into_iter()
            .map(|e| (e.code, e.path))
            .collect()
    }

    #[test]
    fn a_document_starts_with_the_envelope_and_keeps_declaration_order() {
        let (doc, ty) = svm();
        let text = to_document(
            &json!({"mtry": 3, "kernel": {"gamma": 0.5, "type": "Rbf"}}),
            &ty,
            &doc,
        )
        .unwrap();
        assert_eq!(
            text,
            r#"{"$type":"rtemis::Svm","$types":{"rtemis::Kernel":1,"rtemis::Svm":1},"$declarations":{"rtemis::Kernel":"sha256:31cff0cd1a6c6cec2a7bb77f44dbc646a33a2ce0758da968fa188620b86f8fd0","rtemis::Svm":"sha256:9757f0755f90569265241c37d7fa5ab2bedb7765aa8a8fa8de32f7ed0a299d59"},"cost":1,"kernel":{"gamma":0.5,"type":"Rbf"},"weights":null,"mtry":3}"#
        );
    }

    #[test]
    fn reading_returns_the_normalized_value_and_writing_again_reproduces_the_text() {
        let (doc, ty) = svm();
        let value = json!({"cost": 2, "kernel": {"type": "Linear"}});
        let text = to_document(&value, &ty, &doc).unwrap();
        let read = read_document(&text, &ty, &doc).unwrap();
        assert_eq!(read, normalize(&value, &ty, &doc).unwrap());
        assert_eq!(to_document(&read, &ty, &doc).unwrap(), text);
    }

    fn set_svm_version(doc: &mut IrDocument, v: u32) {
        let TypeDef::Struct(s) = &mut doc.types[1] else {
            panic!("the fixture's second type is Svm")
        };
        s.version = v;
    }

    #[test]
    fn versions_are_checked() {
        let (mut doc, ty) = svm();
        let text = to_document(&json!({}), &ty, &doc).unwrap();
        set_svm_version(&mut doc, 2);
        assert_eq!(
            codes(read_document(&text, &ty, &doc)),
            vec![("version_older", "/$types/rtemis::Svm".to_string())]
        );
        set_svm_version(&mut doc, 1);
        let newer = text.replace(r#""rtemis::Kernel":1"#, r#""rtemis::Kernel":5"#);
        assert_eq!(
            codes(read_document(&newer, &ty, &doc)),
            vec![("version_newer", "/$types/rtemis::Kernel".to_string())]
        );
        let missing = text.replace(r#""rtemis::Kernel":1,"#, "");
        assert_eq!(
            codes(read_document(&missing, &ty, &doc)),
            vec![("version_missing", "/$types/rtemis::Kernel".to_string())]
        );
    }

    /// The document of an `Svm` with the declarations, and without them.
    fn written(doc: &IrDocument) -> (String, String) {
        let ty = QualifiedName::parse("rtemis::Svm").unwrap();
        let value = normalize(&json!({}), &ty, doc).unwrap();
        let hash = |q: &QualifiedName| value_declaration_hash(q, doc).into();
        let v = rtemis_r8::access::Json::new(&value);
        (
            document_with(v, &ty, doc, true, &hash),
            document_with(v, &ty, doc, false, &hash),
        )
    }

    /// The first field of `Svm`.
    fn first(doc: &mut IrDocument) -> &mut rtemis_r8::ir::FieldDef {
        let TypeDef::Struct(s) = &mut doc.types[1] else {
            panic!("the fixture's second type is Svm")
        };
        &mut s.fields[0]
    }

    fn read_any(text: &str, ty: &QualifiedName, doc: &IrDocument) -> bool {
        let hash = |q: &QualifiedName| value_declaration_hash(q, doc).into();
        let options = ReadOptions {
            any_declaration: true,
            hash: &hash,
        };
        read_tree_with(
            text,
            ty,
            doc,
            &mut rtemis_r8::validate::RuleCache::new(),
            options,
        )
        .is_ok()
    }

    #[test]
    fn a_document_of_another_declaration_is_refused_unless_it_shows_the_same_meaning() {
        let (doc, ty) = svm();
        let (full, short) = written(&doc);
        for text in [&full, &short] {
            assert!(read_document(text, &ty, &doc).is_ok());
        }
        // What has no part in a value.
        let mut same = doc.clone();
        first(&mut same).description = "Other.".into();
        first(&mut same).sensitive = true;
        first(&mut same).default = None;
        assert!(read_document(&short, &ty, &same).is_ok());
        // A bound: its meaning is the same, which the declarations show.
        let mut bound = doc.clone();
        first(&mut bound).ty = serde_json::from_value(json!({"kind": "double"})).unwrap();
        assert!(read_document(&full, &ty, &bound).is_ok());
        assert_eq!(
            codes(read_document(&short, &ty, &bound)),
            vec![("declaration_changed", "/$declarations".to_string())]
        );
        assert!(read_any(&short, &ty, &bound));
        // A unit.
        let mut unit = doc.clone();
        first(&mut unit).unit = Some("kg".into());
        assert_eq!(
            codes(read_document(&full, &ty, &unit)),
            vec![(
                "declaration_changed",
                "/$declarations/rtemis::Svm".to_string()
            )]
        );
        assert_eq!(
            codes(read_document(&short, &ty, &unit))[0].0,
            "declaration_changed"
        );
        assert!(read_any(&short, &ty, &unit));
        // Written by hand, without a claim.
        let mut hand: Value = serde_json::from_str(&short).unwrap();
        hand.as_object_mut().unwrap().shift_remove("$declarations");
        assert!(read_document(&hand.to_string(), &ty, &unit).is_ok());
    }

    #[test]
    fn the_members_of_the_declarations_are_checked() {
        let (doc, ty) = svm();
        let (full, short) = written(&doc);
        let edited = |text: &str, f: &dyn Fn(&mut Value)| {
            let mut v: Value = serde_json::from_str(text).unwrap();
            f(&mut v);
            codes(read_document(&v.to_string(), &ty, &doc))
        };
        assert_eq!(
            edited(&short, &|v| v["$declarations"] = json!([1])),
            vec![("document_form", "/$declarations".to_string())]
        );
        assert_eq!(
            edited(&short, &|v| v["$declarations"]["rtemis::Svm"] = json!(1)),
            vec![("document_form", "/$declarations/rtemis::Svm".to_string())]
        );
        assert_eq!(
            edited(&short, &|v| {
                v["$declarations"]
                    .as_object_mut()
                    .unwrap()
                    .shift_remove("rtemis::Kernel");
            }),
            vec![(
                "declaration_missing",
                "/$declarations/rtemis::Kernel".to_string()
            )]
        );
        // Declarations that are not those of the hash.
        for ir in [json!([]), json!({}), json!([{"$ir": 9}])] {
            assert_eq!(
                edited(&full, &|v| {
                    v["$declarations"]["rtemis::Svm"] = json!("sha256:other");
                    v["$ir"] = ir.clone();
                }),
                vec![("document_form", "/$ir".to_string())]
            );
        }
    }

    #[test]
    fn the_envelope_is_required_and_names_the_type() {
        let (doc, ty) = svm();
        let other = QualifiedName::parse("rtemis::Kernel").unwrap();
        let text = to_document(&json!({}), &ty, &doc).unwrap();
        assert_eq!(
            codes(read_document(&text, &other, &doc))[0],
            ("document_type", "/$type".to_string())
        );
        assert_eq!(
            codes(read_document(r#"{"cost": 1}"#, &ty, &doc)),
            vec![
                ("document_form", "/$type".to_string()),
                ("document_form", "/$types".to_string())
            ]
        );
        assert_eq!(codes(read_document("{", &ty, &doc))[0].0, "document_json");
    }
}

#[test]
fn dates_and_date_times_convert_both_ways() {
    use rtemis_r8::canonical::{date_days, date_text, datetime_micros, datetime_text};
    for (days, text) in [
        (0, "1970-01-01"),
        (-1, "1969-12-31"),
        (19782, "2024-02-29"),
        (-719528, "0000-01-01"),
        (2932896, "9999-12-31"),
    ] {
        assert_eq!(date_text(days), text);
        assert_eq!(date_days(text), days);
    }
    for (us, text) in [
        (0, "1970-01-01T00:00:00.000000Z"),
        (-1, "1969-12-31T23:59:59.999999Z"),
        (4_503_599_627_370_495, "2112-09-17T23:53:47.370495Z"),
        (-4_503_599_627_370_495, "1827-04-16T00:06:12.629505Z"),
    ] {
        assert_eq!(datetime_text(us), text);
        assert_eq!(datetime_micros(text), us);
    }
}
