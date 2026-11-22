//! The declaration record: every declaration of the conformance suites and
//! the fixtures round-trips; a committed golden record decodes to its
//! declarations and the current encoder reproduces its bytes; samples cover
//! every field type, migration step, trait member, impl member, and JSON value
//! form; damaged and hostile input returns an error without a panic.

use std::path::{Path, PathBuf};

use rtemis_r8::codec::declaration_hash;
use rtemis_r8::ir::{
    Bound, CharacterType, ClassSystem, DoubleType, FieldDef, FieldType, ImplDef, ImplTarget,
    IntegerType, IrDocument, Logic, MethodDef, Migration, MigrationStep, Resolve, RuleDef,
    SplitTarget, StructDef, TraitDef, TypeDef,
};
use rtemis_r8::name::{MethodName, Name, QualifiedName};
use rtemis_r8::record::{self, Declaration, DeclarationRef, Entry, RecordError};
use rtemis_r8::rules::DocumentSet;
use serde_json::{Value, json};

fn name(s: &str) -> Name {
    Name::new(s).unwrap()
}

fn q(s: &str) -> QualifiedName {
    QualifiedName::parse(s).unwrap()
}

fn json_files(dir: &Path) -> Vec<PathBuf> {
    let mut out = Vec::new();
    for entry in std::fs::read_dir(dir).unwrap() {
        let path = entry.unwrap().path();
        if path.is_dir() {
            out.extend(json_files(&path));
        } else if path.extension().is_some_and(|e| e == "json") {
            out.push(path);
        }
    }
    out.sort();
    out
}

/// Every IR document of the conformance suites and the fixtures that reads.
fn documents() -> Vec<IrDocument> {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"));
    let mut files = json_files(&root.join("../../../conformance"));
    files.extend(json_files(&root.join("tests/fixtures")));
    let mut out = Vec::new();
    for path in files {
        let v: Value = serde_json::from_str(&std::fs::read_to_string(&path).unwrap()).unwrap();
        let mut found = Vec::new();
        collect_documents(&v, &mut found);
        out.extend(
            found
                .into_iter()
                .filter_map(|d| IrDocument::from_json(&d.to_string()).ok()),
        );
    }
    out
}

fn collect_documents(v: &Value, out: &mut Vec<Value>) {
    match v {
        Value::Object(m) => {
            if m.contains_key("$ir") {
                out.push(v.clone());
                return;
            }
            m.values().for_each(|x| collect_documents(x, out));
        }
        Value::Array(a) => a.iter().for_each(|x| collect_documents(x, out)),
        _ => {}
    }
}

fn entries_of(doc: &IrDocument) -> Vec<(&Name, DeclarationRef<'_>)> {
    let ns = &doc.namespace;
    doc.types
        .iter()
        .map(|t| (ns, DeclarationRef::Type(t)))
        .chain(doc.traits.iter().map(|t| (ns, DeclarationRef::Trait(t))))
        .chain(doc.impls.iter().map(|i| (ns, DeclarationRef::Impl(i))))
        .collect()
}

/// The document of the namespace `ns` holding `entries`.
fn document_of(ns: &Name, entries: &[Entry]) -> IrDocument {
    let mut doc = IrDocument {
        ir_version: rtemis_r8::ir::IR_VERSION,
        namespace: ns.clone(),
        types: Vec::new(),
        traits: Vec::new(),
        impls: Vec::new(),
    };
    for e in entries {
        match &e.declaration {
            Declaration::Type(t) => doc.types.push(t.clone()),
            Declaration::Trait(t) => doc.traits.push(t.clone()),
            Declaration::Impl(i) => doc.impls.push(i.clone()),
        }
    }
    doc
}

#[test]
fn every_declaration_of_the_suites_round_trips() {
    let docs = documents();
    assert!(docs.len() >= 5, "found {} documents", docs.len());
    let mut n = 0;
    for doc in &docs {
        let bytes = record::encode(entries_of(doc));
        let entries = record::decode(&bytes).unwrap();
        assert!(entries.iter().all(|e| e.namespace == doc.namespace));
        let back = document_of(&doc.namespace, &entries);
        assert_eq!(&back, doc);
        assert_eq!(back.to_json(), doc.to_json());
        let (a, b) = (
            DocumentSet::from(vec![doc.clone()]),
            DocumentSet::from(vec![back]),
        );
        for t in &doc.types {
            let tq = QualifiedName::new(doc.namespace.clone(), t.name().clone());
            assert_eq!(declaration_hash(&tq, &a), declaration_hash(&tq, &b));
        }
        // One declaration has one encoding.
        assert_eq!(record::encode(entries_of(doc)), bytes);
        n += entries.len();
    }
    assert!(n >= 20, "{n} declarations");
}

/// A field of type `ty` with every optional member set.
fn full_field(n: &str, ty: FieldType) -> FieldDef {
    let mut ext = std::collections::BTreeMap::new();
    ext.insert("a:x".to_string(), json!({"b": [1, 2.5, null]}));
    ext.insert("b:y".to_string(), json!(true));
    FieldDef {
        name: name(n),
        ty,
        default: Some(Box::new(rtemis_r8::tree::Tree::Num(
            rtemis_r8::ir::Number::U64(0),
        ))),
        description: "A field.".into(),
        sensitive: true,
        unit: Some("mg/L".into()),
        external: true,
        unique: true,
        optional: true,
        inherited_from: Some(Box::new(q("ns::Parent"))),
        extensions: Some(Box::new(ext)),
    }
}

fn bare_field(n: &str, ty: FieldType) -> FieldDef {
    FieldDef {
        name: name(n),
        ty,
        default: None,
        description: Box::default(),
        sensitive: false,
        unit: None,
        external: false,
        unique: false,
        optional: false,
        inherited_from: None,
        extensions: None,
    }
}

/// One sample of every field type. The `match` below lists every variant, so
/// a new variant fails to compile until it has a sample here.
fn sample_types() -> Vec<FieldType> {
    let logic: Logic = serde_json::from_value(json!({"+": [{"var": "a"}, 1]})).unwrap();
    let rule = RuleDef {
        id: name("R"),
        invariant: serde_json::from_value(json!({"<": [{"var": "start"}, 10]})).unwrap(),
        when: Some(serde_json::from_value(json!({"!": {"is_null": {"var": "start"}}})).unwrap()),
        message: "start is small.".into(),
    };
    let out = vec![
        FieldType::Logical,
        FieldType::Integer(IntegerType {
            lower: Some(Bound::inclusive(-3)),
            upper: Some(Bound::exclusive(i32::MAX)),
            values: Some(Box::new([1, -2, 3])),
        }),
        FieldType::Integer(IntegerType::default()),
        FieldType::Double(DoubleType {
            lower: Some(Bound::exclusive(0.0)),
            upper: Some(Bound::inclusive(1e300)),
            finite: false,
            values: Some(Box::new([0.5, -0.0])),
        }),
        FieldType::Double(DoubleType::default()),
        FieldType::Character(CharacterType {
            pattern: Some("^[a-z]+$".into()),
            values: Some(Box::new(["a".into(), "é".into()])),
        }),
        FieldType::Raw,
        FieldType::Date,
        FieldType::Posixct,
        FieldType::Factor {
            levels: Some(Box::new(["lo".into(), "hi".into()])),
            ordered: true,
        },
        FieldType::Factor {
            levels: None,
            ordered: false,
        },
        FieldType::Matrix {
            element: Box::new(FieldType::Double(DoubleType::default())),
            nrow: Some(2),
            ncol: None,
        },
        FieldType::Array {
            element: Box::new(FieldType::Integer(IntegerType::default())),
            dim: Some(Box::new([2, 3, 4])),
        },
        FieldType::SparseMatrix {
            element: Box::new(FieldType::Logical),
            nrow: None,
            ncol: Some(7),
        },
        FieldType::MatrixOf { of: q("ns::P") },
        FieldType::Table {
            columns: Box::new([
                bare_field("start", FieldType::Integer(IntegerType::default())),
                full_field("label", FieldType::Character(CharacterType::default())),
            ]),
            open: true,
            rules: Box::new([rule]),
        },
        FieldType::TableOf { of: q("ns::P") },
        FieldType::Json,
        FieldType::R { class: "lm".into() },
        FieldType::Vec {
            element: Box::new(FieldType::Double(DoubleType::default())),
            min_length: Some(1),
            max_length: Some(u64::MAX),
            unique: true,
        },
        FieldType::Option {
            inner: Box::new(FieldType::Integer(IntegerType::default())),
            resolve: Resolve::Computed { expr: logic },
        },
        FieldType::Option {
            inner: Box::new(FieldType::Logical),
            resolve: Resolve::Runtime,
        },
        FieldType::Hashmap {
            value: Box::new(FieldType::Json),
            keys: CharacterType {
                pattern: Some("^k".into()),
                values: None,
            },
        },
        FieldType::OrderedMap {
            value: Box::new(FieldType::Date),
            keys: CharacterType::default(),
        },
        FieldType::Named { name: q("ns::P") },
        FieldType::Dyn {
            r#trait: q("ns::Kernel"),
        },
    ];
    for t in &out {
        match t {
            FieldType::Logical
            | FieldType::Integer(_)
            | FieldType::Double(_)
            | FieldType::Character(_)
            | FieldType::Raw
            | FieldType::Date
            | FieldType::Posixct
            | FieldType::Factor { .. }
            | FieldType::Matrix { .. }
            | FieldType::Array { .. }
            | FieldType::SparseMatrix { .. }
            | FieldType::MatrixOf { .. }
            | FieldType::Table { .. }
            | FieldType::TableOf { .. }
            | FieldType::Json
            | FieldType::R { .. }
            | FieldType::Vec { .. }
            | FieldType::Option { .. }
            | FieldType::Hashmap { .. }
            | FieldType::OrderedMap { .. }
            | FieldType::Named { .. }
            | FieldType::Dyn { .. } => {}
        }
    }
    out
}

/// One sample of every migration step, listed by an exhaustive `match`.
fn sample_steps() -> Vec<MigrationStep> {
    let e: Logic = serde_json::from_value(json!({"*": [{"var": "x"}, 2]})).unwrap();
    let mut values = std::collections::BTreeMap::new();
    values.insert(name("k"), json!(3));
    values.insert(name("z"), json!("s"));
    let out = vec![
        MigrationStep::Rename {
            from: name("a"),
            to: name("b"),
        },
        MigrationStep::Add {
            field: name("c"),
            value: json!([1, "x"]),
        },
        MigrationStep::Remove { field: name("d") },
        MigrationStep::Split {
            field: name("e"),
            into: vec![SplitTarget {
                field: name("f"),
                expr: e.clone(),
            }],
        },
        MigrationStep::Merge {
            fields: vec![name("g"), name("h")],
            into: name("i"),
            expr: e.clone(),
        },
        MigrationStep::ChangeType {
            field: name("j"),
            expr: e,
        },
        MigrationStep::RenameVariant {
            from: name("A"),
            to: name("B"),
        },
        MigrationStep::AddVariantField {
            variant: name("C"),
            field: name("k"),
            value: json!(null),
        },
        MigrationStep::UnitToStruct {
            variant: name("D"),
            values,
        },
        MigrationStep::Nest {
            fields: vec![name("l")],
            into: name("m"),
        },
        MigrationStep::ConvertUnit {
            field: name("n"),
            from: "mg".into(),
            to: "g".into(),
            factor: 0.001,
        },
    ];
    for s in &out {
        match s {
            MigrationStep::Rename { .. }
            | MigrationStep::Add { .. }
            | MigrationStep::Remove { .. }
            | MigrationStep::Split { .. }
            | MigrationStep::Merge { .. }
            | MigrationStep::ChangeType { .. }
            | MigrationStep::RenameVariant { .. }
            | MigrationStep::AddVariantField { .. }
            | MigrationStep::UnitToStruct { .. }
            | MigrationStep::Nest { .. }
            | MigrationStep::ConvertUnit { .. } => {}
        }
    }
    out
}

/// A struct holding every sample field type, every migration step, and the
/// JSON value edge cases as defaults.
fn sample_struct() -> TypeDef {
    let edge = [
        json!(1.0),
        json!(-0.0),
        json!(1e300),
        json!(u64::MAX),
        json!(i64::MIN),
        json!("a\u{0}b"),
        json!([]),
        json!({}),
        serde_json::from_str::<Value>(r#"{"z": 1, "a": [true, false, null]}"#).unwrap(),
    ];
    let mut fields: Vec<FieldDef> = sample_types()
        .into_iter()
        .enumerate()
        .map(|(i, t)| {
            if i % 2 == 0 {
                full_field(&format!("f{i}"), t)
            } else {
                bare_field(&format!("f{i}"), t)
            }
        })
        .collect();
    for (i, v) in edge.into_iter().enumerate() {
        let mut f = bare_field(&format!("j{i}"), FieldType::Json);
        f.default = Some(Box::new(rtemis_r8::tree::Tree::from_value(&v)));
        fields.push(f);
    }
    let mut ext = std::collections::BTreeMap::new();
    ext.insert("x:y".to_string(), json!({"k": 1}));
    TypeDef::Struct(StructDef {
        name: name("Sample"),
        description: "Every member.".into(),
        version: 3,
        fields,
        extends: Some(q("ns::Base")),
        rules: vec![RuleDef {
            id: name("ODD"),
            invariant: serde_json::from_value(json!({"nope": [1], "two": 2})).unwrap(),
            when: None,
            message: "kept as written".into(),
        }],
        migrations: vec![
            Migration {
                from_version: 1,
                steps: sample_steps(),
            },
            Migration {
                from_version: 2,
                steps: vec![],
            },
        ],
        aliases: vec![q("old::Sample")],
        provenance: true,
        s4: true,
        extensions: ext,
    })
}

fn sample_trait() -> TraitDef {
    TraitDef {
        name: name("Kernel"),
        description: "A kernel.".into(),
        methods: vec![
            MethodDef {
                name: MethodName::new("apply").unwrap(),
                description: "Applies it.".into(),
                params: vec!["x".into(), "...".into()],
                stochastic: true,
                has_default: true,
            },
            MethodDef {
                name: MethodName::new("[").unwrap(),
                description: String::new(),
                params: vec![],
                stochastic: false,
                has_default: false,
            },
        ],
        required_fields: vec![full_field(
            "gamma",
            FieldType::Double(DoubleType::default()),
        )],
        supertraits: vec![q("R8::Display")],
        tag: Some(name("kind")),
        generic: true,
    }
}

fn sample_impls() -> Vec<ImplDef> {
    vec![
        ImplDef {
            r#trait: q("ns::Kernel"),
            type_args: vec![],
            target: ImplTarget::Type {
                name: q("ns::Sample"),
            },
            tag_value: Some("sample".into()),
            methods: vec![MethodName::new("apply").unwrap()],
            codec: false,
        },
        ImplDef {
            r#trait: q("R8::From"),
            type_args: vec!["data.frame".into()],
            target: ImplTarget::HostClass {
                host: "r".into(),
                package: "Matrix".into(),
                class: "dgCMatrix".into(),
                system: ClassSystem::S4,
            },
            tag_value: None,
            methods: vec![],
            codec: true,
        },
    ]
}

fn sample_record() -> (Name, Vec<Entry>) {
    let ns = name("ns");
    let mut entries = vec![
        Entry {
            namespace: ns.clone(),
            declaration: Declaration::Type(sample_struct()),
        },
        Entry {
            namespace: ns.clone(),
            declaration: Declaration::Trait(sample_trait()),
        },
    ];
    entries.extend(sample_impls().into_iter().map(|i| Entry {
        namespace: ns.clone(),
        declaration: Declaration::Impl(i),
    }));
    (ns, entries)
}

fn encode_entries(entries: &[Entry]) -> Vec<u8> {
    record::encode(entries.iter().map(|e| {
        (
            &e.namespace,
            match &e.declaration {
                Declaration::Type(t) => DeclarationRef::Type(t),
                Declaration::Trait(t) => DeclarationRef::Trait(t),
                Declaration::Impl(i) => DeclarationRef::Impl(i),
            },
        )
    }))
}

#[test]
fn samples_of_every_member_round_trip() {
    let (_, entries) = sample_record();
    let bytes = encode_entries(&entries);
    let back = record::decode(&bytes).unwrap();
    assert_eq!(back, entries);
    // JSON values come back to identical JSON text.
    let (Declaration::Type(a), Declaration::Type(b)) =
        (&entries[0].declaration, &back[0].declaration)
    else {
        panic!("a type first")
    };
    assert_eq!(
        serde_json::to_string(a).unwrap(),
        serde_json::to_string(b).unwrap()
    );
}

/// The golden record of format 1: `tests/fixtures/sample.r8d` holds the
/// samples as format 1 wrote them, and `sample.r8d.json` their IR. With
/// `R8_UPDATE_RECORD=1`, the test writes both files.
#[test]
fn the_golden_record_of_format_1_reads_and_writes_back() {
    let dir = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures");
    let (_, entries) = sample_record();
    let bytes = encode_entries(&entries);
    let doc = document_of(&name("ns"), &entries);
    if std::env::var_os("R8_UPDATE_RECORD").is_some() {
        std::fs::write(dir.join("sample.r8d"), &bytes).unwrap();
        std::fs::write(dir.join("sample.r8d.json"), doc.to_json()).unwrap();
    }
    let golden = std::fs::read(dir.join("sample.r8d")).unwrap();
    let expected =
        IrDocument::from_json(&std::fs::read_to_string(dir.join("sample.r8d.json")).unwrap())
            .unwrap();
    let decoded = record::decode(&golden).unwrap();
    assert_eq!(document_of(&name("ns"), &decoded), expected);
    // The header names the writer; the payload from the checksum on is the
    // format's bytes, which the current encoder reproduces.
    let (format, _) = record::header(&golden).unwrap();
    assert_eq!(format, 1);
    let payload = |b: &[u8]| {
        let (_, writer) = record::header(b).unwrap();
        b[4 + 1 + 1 + writer.len()..].to_vec()
    };
    assert_eq!(
        payload(&bytes),
        payload(&golden),
        "set R8_UPDATE_RECORD=1 only for a new format version"
    );
}

/// The offset of the checksum: the magic bytes, the format, and the writer.
fn crc_offset(bytes: &[u8]) -> usize {
    let (_, writer) = record::header(bytes).unwrap();
    4 + 1 + 1 + writer.len()
}

/// `bytes` with the checksum recomputed over its payload.
fn reseal(mut bytes: Vec<u8>) -> Vec<u8> {
    let at = crc_offset(&bytes);
    let crc = record::crc32c(&bytes[at + 4..]).to_le_bytes();
    bytes[at..at + 4].copy_from_slice(&crc);
    bytes
}

#[test]
fn damaged_and_hostile_records_return_errors() {
    let (_, entries) = sample_record();
    let bytes = encode_entries(&entries);
    // Every strict prefix is an error.
    for n in 0..bytes.len() {
        assert!(record::decode(&bytes[..n]).is_err(), "prefix of {n} bytes");
    }
    // A changed payload byte fails the checksum.
    let at = crc_offset(&bytes);
    let mut flipped = bytes.clone();
    flipped[at + 10] ^= 0x40;
    assert_eq!(record::decode(&flipped), Err(RecordError::Checksum));
    // With the checksum recomputed, every single-byte change of the payload
    // reaches the decoder, which returns without a panic.
    for i in at + 4..bytes.len() {
        for x in [0x01u8, 0x80, 0xff] {
            let mut b = bytes.clone();
            b[i] ^= x;
            let _ = record::decode(&reseal(b));
        }
    }
    // The header.
    let mut magic = bytes.clone();
    magic[0] = b'X';
    assert_eq!(record::decode(&magic), Err(RecordError::Magic));
    let mut newer = bytes.clone();
    newer[4] = 2;
    assert!(matches!(
        record::decode(&newer),
        Err(RecordError::Newer { format: 2, .. })
    ));
    let mut trailing = bytes.clone();
    trailing.push(0);
    assert_eq!(
        record::decode(&reseal(trailing)),
        Err(RecordError::Trailing)
    );
    // A count beyond the bytes that remain is refused before allocation.
    let mut huge = bytes[..at + 4].to_vec();
    huge.extend_from_slice(&[0xff, 0xff, 0xff, 0xff, 0x0f]);
    assert_eq!(record::decode(&reseal(huge)), Err(RecordError::Truncated));
    // An overlong varint is refused.
    let mut overlong = bytes[..at + 4].to_vec();
    overlong.extend_from_slice(&[0x81, 0x00]);
    assert_eq!(record::decode(&reseal(overlong)), Err(RecordError::Varint));
}

#[test]
fn deep_nesting_is_refused() {
    let mut ty = FieldType::Logical;
    for _ in 0..200 {
        ty = FieldType::Vec {
            element: Box::new(ty),
            min_length: None,
            max_length: None,
            unique: false,
        };
    }
    let t = TypeDef::Struct(StructDef {
        name: name("Deep"),
        description: String::new(),
        version: 1,
        fields: vec![bare_field("x", ty)],
        extends: None,
        rules: vec![],
        migrations: vec![],
        aliases: vec![],
        provenance: false,
        s4: false,
        extensions: Default::default(),
    });
    let ns = name("ns");
    let bytes = record::encode([(&ns, DeclarationRef::Type(&t))]);
    assert_eq!(record::decode(&bytes), Err(RecordError::Depth));
}
