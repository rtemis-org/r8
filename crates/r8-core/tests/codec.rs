//! The canonical JSON form, checked against JavaScript, whose
//! `JSON.stringify` and `Number.prototype.toString` RFC 8785 adopts.
//! `tests/fixtures/ecmascript-jcs.json` is written by
//! `tests/fixtures/gen-ecmascript-jcs.mjs`.

use r8_core::codec::canonical_text;
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
    use r8_core::codec::{normalize, read_document, to_document};
    use r8_core::ir::{IrDocument, TypeDef};
    use r8_core::name::QualifiedName;
    use serde_json::{Value, json};

    fn svm() -> (IrDocument, QualifiedName) {
        (
            IrDocument::from_json(include_str!("fixtures/svm.ir.json")).unwrap(),
            QualifiedName::parse("rtemis::Svm").unwrap(),
        )
    }

    fn codes(r: Result<Value, Vec<r8_core::validate::ValueError>>) -> Vec<(&'static str, String)> {
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
            r#"{"$type":"rtemis::Svm","$types":{"rtemis::Kernel":1,"rtemis::Svm":1},"cost":1,"kernel":{"gamma":0.5,"type":"Rbf"},"weights":null,"mtry":3}"#
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
    use r8_core::codec::{date_days, date_text, datetime_micros, datetime_text};
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
