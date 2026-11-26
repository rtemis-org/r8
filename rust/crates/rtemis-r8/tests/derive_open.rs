//! Open fields of derived types: `SubtypeOf` and `ImplementorOf` declare
//! `subtype()` and `dyn()`, hold values of types the crate does not know, and
//! write and read documents with the declarations that hold those types.
//! spec: R8/class-system#types-declared-in-rust
#![cfg(feature = "macros")]

use std::collections::BTreeMap;

use rtemis_r8::R8;
use rtemis_r8::derive::{Encode, ImplementorOf, R8 as _, Subtype, SubtypeOf, TraitObject};
use rtemis_r8::ir::{FieldType, IrDocument, StructDef, TypeDef};
use rtemis_r8::name::{Name, QualifiedName};
use rtemis_r8::rules::DocumentSet;
use serde_json::{Value, json};

#[derive(R8, Debug, Clone, PartialEq)]
#[r8(namespace = "demo", version = 1, extensible)]
#[r8(rule(
    id = "ordered",
    invariant = r#"{"<=": [{"var": "lo"}, {"var": "hi"}]}"#
))]
pub struct Range {
    pub lo: f64,
    pub hi: f64,
}

#[derive(R8, Debug, Clone, PartialEq)]
#[r8(namespace = "demo", version = 1, extensible)]
pub struct Window {
    #[r8(extends)]
    pub base: Range,
    pub label: String,
}

#[derive(R8, Debug, Clone, PartialEq)]
#[r8(namespace = "demo", version = 1)]
pub struct Axis {
    pub range: SubtypeOf<Range>,
    pub inset: Option<SubtypeOf<Window>>,
    pub others: Vec<SubtypeOf<Range>>,
    pub named: BTreeMap<String, SubtypeOf<Range>>,
}

/// The type of a package that the crate does not know.
fn pane() -> StructDef {
    let TypeDef::Struct(parent) = Window::type_def() else {
        panic!("a struct")
    };
    let mut child: StructDef = serde_json::from_value(json!({
        "name": "Pane",
        "version": 3,
        "fields": [{"name": "number", "type": {"kind": "integer"}}],
        "rules": [{"id": "wide", "invariant": {">=": [{"-": [{"var": "hi"}, {"var": "lo"}]}, 1]}}],
    }))
    .unwrap();
    rtemis_r8::ir::extend(&mut child, &Window::qualified_name(), &parent, &[]).unwrap();
    child
}

/// The declarations of `T`, of `Window`, and of `other::Pane`.
fn types<T: rtemis_r8::derive::R8>() -> DocumentSet {
    let mut set = T::declarations();
    Window::register(&mut set);
    set.put_type(&Name::new("other").unwrap(), TypeDef::Struct(pane()));
    set
}

fn axis_text() -> String {
    json!({
        "$type": "demo::Axis",
        "$types": {"demo::Axis": 1, "demo::Range": 1, "demo::Window": 1, "other::Pane": 3},
        "range": {"$type": "other::Pane", "lo": 0, "hi": 4, "label": "p", "number": 2},
        "inset": {"$type": "demo::Window", "lo": 1, "hi": 2, "label": "w"},
        "others": [
            {"$type": "demo::Range", "lo": 5, "hi": 6},
            {"$type": "other::Pane", "lo": 1, "hi": 3, "label": "q", "number": 1},
        ],
        "named": {"a": {"$type": "demo::Range", "lo": 7, "hi": 8}},
    })
    .to_string()
}

fn q(s: &str) -> QualifiedName {
    QualifiedName::parse(s).unwrap()
}

fn codes(e: &[rtemis_r8::validate::ValueError]) -> Vec<&str> {
    e.iter().map(|e| e.code).collect()
}

#[test]
fn a_holder_declares_its_field_type() {
    let TypeDef::Struct(s) = Axis::type_def() else {
        panic!("a struct")
    };
    assert_eq!(
        s.fields[0].ty,
        FieldType::Subtype {
            of: q("demo::Range")
        }
    );
    assert_eq!(
        s.fields[1].ty,
        FieldType::Option {
            inner: Box::new(FieldType::Subtype {
                of: q("demo::Window")
            })
        }
    );
    let reached: Vec<String> = Axis::declarations().documents()[0]
        .types
        .iter()
        .map(|t| t.name().to_string())
        .collect();
    assert_eq!(reached, ["Axis", "Range", "Window"]);
}

#[test]
fn a_document_of_unknown_types_round_trips_unchanged() {
    let types = types::<Axis>();
    let text = axis_text();
    let x = Axis::from_document_in(&text, &types).unwrap();
    assert_eq!(x.range.type_name().to_string(), "other::Pane");
    let written = x.to_document_in(&types).unwrap();
    // The text of the test names no declarations.
    let of = |s: &str| {
        let mut v = serde_json::from_str::<Value>(s).unwrap();
        v.as_object_mut().unwrap().remove("$declarations");
        v
    };
    assert_eq!(of(&written), of(&text));
    assert_eq!(Axis::from_document_in(&written, &types).unwrap(), x);
    // The declarations that `Axis` reaches hold no `other::Pane`.
    assert_eq!(
        codes(&Axis::from_document(&text).unwrap_err()),
        ["unknown_type", "unknown_type"]
    );
    assert_eq!(
        codes(&x.to_document().unwrap_err()),
        ["unknown_type", "unknown_type"]
    );
}

#[test]
fn a_holder_has_the_interface_of_its_struct() {
    fn width(x: &impl RangeLike) -> f64 {
        x.hi() - x.lo()
    }
    fn end(x: &dyn WindowLike) -> f64 {
        *x.hi()
    }
    let types = types::<Axis>();
    let x = Axis::from_document_in(&axis_text(), &types).unwrap();
    assert_eq!(width(&x.range), 4.0);
    let inset = x.inset.as_ref().unwrap();
    assert_eq!((end(inset), inset.label().as_str()), (2.0, "w"));
    let base: &Range = inset.upcast();
    assert_eq!(base, &inset.to_range());
    // Another value of an ancestor keeps the value's type and other fields.
    let moved = x.range.with_range(Range { lo: 2.0, hi: 9.0 });
    assert_eq!(moved.type_name().to_string(), "other::Pane");
    assert_eq!(
        moved.encode().unwrap(),
        json!({"$type": "other::Pane", "lo": 2.0, "hi": 9.0, "label": "p", "number": 2})
    );
    // The rule of the unknown type is checked where the document is written.
    let narrow = Axis {
        range: x.range.with_range(Range { lo: 2.0, hi: 2.5 }),
        ..x.clone()
    };
    let errors = narrow.to_document_in(&types).unwrap_err();
    assert_eq!(codes(&errors), ["rule_violation"]);
    assert_eq!(errors[0].path, "/range");
}

#[test]
fn a_holder_gives_a_value_of_a_known_type() {
    let window = Window {
        base: Range { lo: 1.0, hi: 2.0 },
        label: "w".into(),
    };
    let held: SubtypeOf<Range> = SubtypeOf::new(&window).unwrap();
    assert_eq!(held.downcast::<Window>().unwrap(), Some(window.clone()));
    assert_eq!(held.downcast::<Range>().unwrap(), None);
    let own: SubtypeOf<Range> = SubtypeOf::new(&window.base).unwrap();
    assert_eq!(own.downcast::<Range>().unwrap(), Some(window.base));
    let shifted = held.with_range(Range { lo: 3.0, hi: 4.0 });
    assert_eq!(shifted.downcast::<Window>().unwrap().unwrap().lo(), &3.0);
    let nan = Range {
        lo: f64::NAN,
        hi: 1.0,
    };
    assert!(SubtypeOf::<Range>::new(&nan).is_err());
}

#[test]
fn the_declarations_declare_the_type_as_it_does() {
    let x = Axis::from_document_in(&axis_text(), &types::<Axis>()).unwrap();
    let mut changed = types::<Axis>();
    let TypeDef::Struct(mut range) = Range::type_def() else {
        panic!("a struct")
    };
    range.rules.clear();
    changed.put_type(&Name::new("demo").unwrap(), TypeDef::Struct(range));
    assert_eq!(
        codes(&x.to_document_in(&changed).unwrap_err()),
        ["declaration_changed"]
    );
    let errors = Axis::from_document_in(&axis_text(), &DocumentSet::default()).unwrap_err();
    assert_eq!(codes(&errors), ["unknown_type"; 3]);
}

/// The Rust side of `demo::Shape`, which R declares.
pub trait Shape {}
impl TraitObject for dyn Shape {
    const NAME: &'static str = "demo::Shape";
}

/// The Rust side of `demo::Model`, whose tag member is `algorithm`.
pub trait Model {}
impl TraitObject for dyn Model {
    const NAME: &'static str = "demo::Model";
}

#[derive(R8, Debug, Clone, PartialEq)]
#[r8(namespace = "demo", version = 1)]
pub enum Color {
    Red,
    Blue,
}

#[derive(R8, Debug, Clone, PartialEq)]
#[r8(namespace = "demo", version = 1)]
pub struct Drawing {
    pub shape: ImplementorOf<dyn Shape>,
    pub models: Vec<ImplementorOf<dyn Model>>,
}

/// The declarations of `Drawing`, with the traits and impls of `demo`.
fn drawing_types() -> DocumentSet {
    let mut set = types::<Drawing>();
    Color::register(&mut set);
    let more: IrDocument = serde_json::from_value(json!({
        "$ir": 1,
        "namespace": "demo",
        "traits": [
            {"name": "Shape", "methods": [{"name": "area", "params": [{"name": "x"}]}]},
            {"name": "Model", "tag": "algorithm",
             "methods": [{"name": "fit", "params": [{"name": "x"}]}]},
        ],
        "impls": [
            {"trait": "demo::Shape", "target": {"kind": "named", "name": "demo::Range"}, "methods": ["area"]},
            {"trait": "demo::Shape", "target": {"kind": "named", "name": "demo::Color"}, "methods": ["area"]},
            {"trait": "demo::Model", "target": {"kind": "named", "name": "demo::Window"},
             "tag_value": "WIN", "methods": ["fit"]},
        ],
    }))
    .unwrap();
    let d = set.document_position(&more.namespace);
    set.traits_mut(d).extend(more.traits);
    set.impls_mut(d).extend(more.impls);
    set
}

#[test]
fn an_implementor_round_trips_by_its_type_and_by_its_tag() {
    let types = drawing_types();
    let text = json!({
        "$type": "demo::Drawing",
        "$types": {"demo::Drawing": 1, "demo::Range": 1, "demo::Window": 1, "other::Pane": 3},
        "shape": {"$type": "demo::Range", "lo": 0, "hi": 1},
        "models": [
            {"algorithm": "WIN", "lo": 0, "hi": 1, "label": "a"},
            {"algorithm": "other::Pane", "lo": 0, "hi": 1, "label": "b", "number": 1},
        ],
    })
    .to_string();
    let x = Drawing::from_document_in(&text, &types).unwrap();
    let names: Vec<String> = x.models.iter().map(|m| m.type_name().to_string()).collect();
    assert_eq!(names, ["demo::Window", "other::Pane"]);
    let window = x.models[0].downcast::<Window>().unwrap().unwrap();
    assert_eq!(window.label, "a");
    assert_eq!(x.models[1].downcast::<Window>().unwrap(), None);
    // The text of the test names no declarations.
    let of = |s: &str| {
        let mut v = serde_json::from_str::<Value>(s).unwrap();
        v.as_object_mut().unwrap().remove("$declarations");
        v
    };
    assert_eq!(of(&x.to_document_in(&types).unwrap()), of(&text));
    let made = Drawing {
        models: vec![ImplementorOf::new(&window).unwrap()],
        ..x.clone()
    };
    assert_eq!(
        of(&made.to_document_in(&types).unwrap())["models"],
        json!([{"algorithm": "WIN", "lo": 0, "hi": 1, "label": "a"}])
    );
    // The declarations of `Drawing` hold no trait.
    assert!(!x.to_document().unwrap_err().is_empty());
}

#[test]
fn a_unit_only_enum_is_an_implementor() {
    let types = drawing_types();
    let x = Drawing {
        shape: ImplementorOf::new(&Color::Blue).unwrap(),
        models: vec![],
    };
    let text = x.to_document_in(&types).unwrap();
    let v: Value = serde_json::from_str(&text).unwrap();
    assert_eq!(v["shape"], json!({"$type": "demo::Color", "type": "Blue"}));
    let read = Drawing::from_document_in(&text, &types).unwrap();
    assert_eq!(read, x);
    assert_eq!(read.shape.downcast::<Color>().unwrap(), Some(Color::Blue));
}

#[test]
fn a_type_that_implements_no_trait_is_refused_in_the_document() {
    let types = drawing_types();
    let x = Drawing {
        shape: ImplementorOf::new(&Drawing {
            shape: ImplementorOf::new(&Color::Red).unwrap(),
            models: vec![],
        })
        .unwrap(),
        models: vec![],
    };
    assert_eq!(
        codes(&x.to_document_in(&types).unwrap_err()),
        ["not_implementor"]
    );
}

/// `other::Pane` as another crate compiled it: without the rule `wide` that
/// the declarations of the test give it.
#[derive(R8, Debug, Clone, PartialEq)]
#[r8(namespace = "other", version = 3)]
pub struct Pane {
    #[r8(extends)]
    pub base: Window,
    pub number: i32,
}

#[test]
fn a_held_value_read_under_another_declaration_does_not_downcast() {
    let types = types::<Axis>();
    let x = Axis::from_document_in(&axis_text(), &types).unwrap();
    // `demo::Window` is declared as this crate compiled it.
    assert!(
        x.inset
            .as_ref()
            .unwrap()
            .downcast::<Window>()
            .unwrap()
            .is_some()
    );
    let e = x.range.downcast::<Pane>().unwrap_err();
    assert!(e.contains("[declaration_changed]"), "{e}");
}

#[test]
fn declarations_checked_once_write_and_read_documents() {
    let types = types::<Axis>();
    let x = Axis::from_document_in(&axis_text(), &types).unwrap();
    let checked = Axis::checked_in(&types).unwrap();
    let text = checked.to_document(&x).unwrap();
    assert_eq!(text, x.to_document_in(&types).unwrap());
    assert_eq!(checked.from_document(&text).unwrap(), x);
    // Declarations that do not hold the type's own are refused once.
    assert!(Axis::checked_in(&DocumentSet::default()).is_err());
}
