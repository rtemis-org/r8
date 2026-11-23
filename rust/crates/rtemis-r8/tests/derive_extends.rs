//! `#[r8(extends)]` and `#[r8(extensible)]`: a derived child declares what
//! the lowering of the same child gives, encodes flat, validates with its
//! ancestors' rules, and implements the interface traits of its ancestors.
//! spec: R8/class-system#types-declared-in-rust
#![cfg(feature = "macros")]

use rtemis_r8::R8;
use rtemis_r8::derive::{Encode, R8 as _, Subtype};
use rtemis_r8::ir::{FieldDef, InheritedChange, StructDef, TypeDef, check_document, extend};
use rtemis_r8::name::{Name, QualifiedName};
use rtemis_r8::registry::Registry;
use rtemis_r8::rules::TypeLookup;
use rtemis_r8::tree::Tree;
use serde_json::json;

/// A numeric range.
#[derive(R8, Debug, Clone, PartialEq)]
#[r8(namespace = "demo", version = 1, extensible)]
#[r8(rule(
    id = "ordered",
    invariant = r#"{"<=": [{"var": "lo"}, {"var": "hi"}]}"#
))]
pub struct Range {
    /// The lower end.
    pub lo: f64,
    /// The upper end.
    #[r8(default = "10")]
    pub hi: f64,
    pub unit: Option<String>,
}

/// A labeled range.
#[derive(R8, Debug, Clone, PartialEq)]
#[r8(namespace = "demo", version = 2, extensible)]
#[r8(rule(id = "labeled", invariant = r#"{"!==": [{"var": "label"}, ""]}"#))]
pub struct Window {
    #[r8(
        extends,
        inherited(
            lo(default = "0"),
            hi(sensitive, description = "The upper end of the window."),
            unit(default = r#""s""#),
        )
    )]
    pub base: Range,
    /// The label.
    pub label: String,
}

/// A numbered window.
#[derive(R8, Debug, Clone, PartialEq)]
#[r8(namespace = "demo", version = 1)]
pub struct Pane {
    #[r8(extends, inherited(unit(default = "null")))]
    pub window: Window,
    #[r8(min = 1, default = "1")]
    pub number: i32,
}

fn pane() -> Pane {
    Pane {
        window: Window {
            base: Range {
                lo: 1.0,
                hi: 4.0,
                unit: None,
            },
            label: "w".into(),
        },
        number: 3,
    }
}

fn struct_of<T: rtemis_r8::derive::R8>() -> StructDef {
    match T::type_def() {
        TypeDef::Struct(s) => s,
        TypeDef::Enum(_) => panic!("a struct"),
    }
}

/// The members a child adds, as a declaration of its own.
fn own(s: &StructDef) -> StructDef {
    let mut out = s.clone();
    out.fields.retain(|f| f.inherited_from.is_none());
    out.rules.retain(|r| r.inherited_from.is_none());
    out.extends = None;
    out
}

fn change(name: &str, default: Option<Option<serde_json::Value>>) -> InheritedChange {
    InheritedChange {
        name: Name::new(name).unwrap(),
        default: default.map(|d| d.map(|v| Box::new(Tree::from_value(&v)))),
        sensitive: false,
        description: None,
    }
}

#[test]
fn a_derived_child_is_the_lowering_of_its_own_members() {
    let (range, window, pane) = (
        struct_of::<Range>(),
        struct_of::<Window>(),
        struct_of::<Pane>(),
    );
    let mut lowered = own(&window);
    let hi = InheritedChange {
        sensitive: true,
        description: Some("The upper end of the window.".into()),
        ..change("hi", None)
    };
    extend(
        &mut lowered,
        &Range::qualified_name(),
        &range,
        &[
            change("lo", Some(Some(json!(0)))),
            hi,
            change("unit", Some(Some(json!("s")))),
        ],
    )
    .unwrap();
    assert_eq!(window, lowered);
    let mut lowered = own(&pane);
    extend(
        &mut lowered,
        &Window::qualified_name(),
        &window,
        &[change("unit", Some(None))],
    )
    .unwrap();
    assert_eq!(pane, lowered);
    let names: Vec<_> = pane.fields.iter().map(|f| f.name.as_str()).collect();
    assert_eq!(names, ["lo", "hi", "unit", "label", "number"]);
    let from: Vec<_> = pane
        .fields
        .iter()
        .map(|f| f.inherited_from.as_ref().map(|q| q.to_string()))
        .collect();
    let of = |s: &str| Some(s.to_string());
    assert_eq!(
        from,
        [
            of("demo::Range"),
            of("demo::Range"),
            of("demo::Range"),
            of("demo::Window"),
            None
        ]
    );
    let field =
        |n: &str| -> &FieldDef { pane.fields.iter().find(|f| f.name.as_str() == n).unwrap() };
    assert_eq!(
        field("lo").default.as_deref(),
        Some(&Tree::from_value(&json!(0)))
    );
    assert_eq!(field("unit").default, None);
    assert!(field("hi").sensitive);
    assert_eq!(&*field("hi").description, "The upper end of the window.");
    assert_eq!(pane.rules.len(), 2);
}

#[test]
fn a_childs_declarations_hold_its_ancestors_and_pass_the_checks() {
    let set = Pane::declarations();
    assert_eq!(set.documents().len(), 1);
    assert_eq!(set.documents()[0].types.len(), 3);
    assert!(check_document(&set.documents()[0]).is_empty());
    assert!(set.is_subtype(&Pane::qualified_name(), &Range::qualified_name()));
    // The registry accepts them in the order of the chain.
    let r = Registry::new();
    for t in [Range::type_def(), Window::type_def(), Pane::type_def()] {
        let out = r.declare_type(&Name::new("demo").unwrap(), t).unwrap();
        assert!(out.problems.is_empty(), "{:?}", out.problems);
    }
}

#[test]
fn a_child_encodes_as_one_object_and_reads_with_its_own_defaults() {
    let p = pane();
    assert_eq!(
        p.encode().unwrap(),
        json!({"lo": 1.0, "hi": 4.0, "unit": null, "label": "w", "number": 3})
    );
    let text = p.to_document().unwrap();
    assert!(
        text.starts_with(r#"{"$type":"demo::Pane","$types":{"demo::Pane":1},"lo":1,"hi":4,"#),
        "{text}"
    );
    assert_eq!(Pane::from_document(&text).unwrap(), p);
    // `lo` and `unit` take the defaults of Window, `hi` that of Range.
    let w = Window::from_document(
        r#"{"$type": "demo::Window", "$types": {"demo::Window": 2}, "label": "w"}"#,
    )
    .unwrap();
    assert_eq!(
        w.base,
        Range {
            lo: 0.0,
            hi: 10.0,
            unit: Some("s".into())
        }
    );
    // Pane removed the default of `unit`.
    let p = Pane::from_document(
        r#"{"$type": "demo::Pane", "$types": {"demo::Pane": 1}, "label": "w"}"#,
    )
    .unwrap();
    assert_eq!((p.window.base.unit, p.number), (None, 1));
}

#[test]
fn the_rules_of_ancestors_apply_to_a_child() {
    let mut p = pane();
    p.window.base.lo = 5.0;
    p.window.label = String::new();
    let rules: Vec<_> = p
        .to_document()
        .unwrap_err()
        .into_iter()
        .map(|e| e.rule.unwrap().to_string())
        .collect();
    assert_eq!(rules, ["ordered", "labeled"]);
}

#[test]
fn a_struct_is_a_subtype_of_itself_and_of_its_ancestors() {
    let p = pane();
    let range: &Range = p.upcast();
    let window: &Window = p.upcast();
    assert_eq!((range, window), (&p.window.base, &p.window));
    assert_eq!(Subtype::<Range>::upcast(range), range);
    // The ancestors' values are the child's own, not copies.
    assert!(std::ptr::eq(range, &p.window.base));
}

fn width<T: RangeLike>(x: &T) -> f64 {
    x.hi() - x.lo()
}

fn text(x: &dyn WindowLike) -> String {
    format!("{}: {} to {}", x.label(), x.lo(), x.hi())
}

#[test]
fn the_interface_trait_of_a_struct_serves_every_struct_that_extends_it() {
    let p = pane();
    assert_eq!(
        (width(&p), width(&p.window), width(&p.window.base)),
        (3.0, 3.0, 3.0)
    );
    assert_eq!(text(&p), "w: 1 to 4");
    assert_eq!(p.to_range(), p.window.base);
    assert_eq!(p.to_window(), p.window);
    let all: Vec<Box<dyn RangeLike>> = vec![
        Box::new(p.clone()),
        Box::new(p.window.clone()),
        Box::new(p.window.base.clone()),
    ];
    assert_eq!(all.iter().map(|x| *x.hi()).sum::<f64>(), 12.0);
}

#[derive(R8, Debug, Clone, PartialEq)]
#[r8(namespace = "demo", version = 1)]
struct Twice {
    #[r8(extends)]
    base: Range,
    lo: f64,
}

#[derive(R8, Debug, Clone, PartialEq)]
#[r8(namespace = "demo", version = 1)]
struct Unknown {
    #[r8(extends, inherited(width(default = "1"), lo(default = "null")))]
    base: Range,
}

#[test]
fn a_declaration_that_cannot_extend_its_parent_panics_at_registration() {
    let message = |f: fn() -> TypeDef| {
        let p = std::panic::catch_unwind(f).unwrap_err();
        p.downcast_ref::<String>().unwrap().clone()
    };
    let twice = message(Twice::type_def);
    assert!(
        twice.starts_with("`Twice` does not extend `demo::Range`:")
            && twice.contains("field `lo` is inherited")
            && twice.contains("[duplicate_field]"),
        "{twice}"
    );
    let unknown = message(Unknown::type_def);
    assert!(
        unknown.contains("`width` is not a field of `demo::Range`")
            && unknown.contains("[inherited_unknown]")
            && unknown.contains("[default_removed]"),
        "{unknown}"
    );
    let _ = (
        Twice {
            base: pane().window.base,
            lo: 1.0,
        },
        Unknown {
            base: pane().window.base,
        },
    );
}

/// A struct of the parent's module, named by its path.
mod inner {
    use rtemis_r8::R8;

    #[derive(R8, Debug, Clone, PartialEq)]
    #[r8(namespace = "demo", version = 1, extensible)]
    pub struct Tile {
        #[r8(extends)]
        pub base: super::Range,
        /// The row.
        pub row: i32,
    }
}

/// A child of a struct whose interface trait is in another module.
#[derive(R8, Debug, Clone, PartialEq)]
#[r8(namespace = "demo", version = 1, extensible)]
struct Corner {
    #[r8(extends)]
    base: inner::Tile,
    column: i32,
}

#[test]
fn the_interface_trait_is_found_by_the_parents_path() {
    use inner::TileLike;
    let c = Corner {
        base: inner::Tile {
            base: pane().window.base,
            row: 2,
        },
        column: 3,
    };
    assert_eq!((*c.lo(), *c.row(), *c.column()), (1.0, 2, 3));
    assert_eq!(c.to_tile().base, c.to_range());
    assert_eq!(c.to_corner(), c);
}

/// Fields whose names the derive's generated code could bind.
#[derive(R8, Debug, Clone, PartialEq)]
#[r8(namespace = "demo", version = 1, extensible)]
struct Names {
    m: i32,
    v: i32,
    e: i32,
    set: i32,
    r#type: i32,
}

#[derive(R8, Debug, Clone, PartialEq)]
#[r8(namespace = "demo", version = 1)]
enum NamedVariants {
    Fields { m: i32, v: i32, e: i32, inner: i32 },
}

#[test]
fn a_field_takes_any_name() {
    let x = Names {
        m: 1,
        v: 2,
        e: 3,
        set: 4,
        r#type: 5,
    };
    assert_eq!(
        x.encode().unwrap(),
        json!({"m": 1, "v": 2, "e": 3, "set": 4, "type": 5})
    );
    assert_eq!(Names::decode(&x.encode().unwrap()).unwrap(), x);
    assert_eq!(*NamesLike::r#type(&x), 5);
    let y = NamedVariants::Fields {
        m: 1,
        v: 2,
        e: 3,
        inner: 4,
    };
    assert_eq!(NamedVariants::decode(&y.encode().unwrap()).unwrap(), y);
}

#[test]
fn a_newtype_variant_holds_a_child() {
    #[derive(R8, Debug, Clone, PartialEq)]
    #[r8(namespace = "demo", version = 1)]
    enum Figure {
        Flat(Window),
    }
    let f = Figure::Flat(pane().window);
    assert_eq!(
        f.encode().unwrap(),
        json!({"lo": 1.0, "hi": 4.0, "unit": null, "label": "w", "type": "Flat"})
    );
    assert_eq!(Figure::decode(&f.encode().unwrap()).unwrap(), f);
    let set = Figure::declarations();
    assert!(
        set.find(&QualifiedName::parse("demo::Range").unwrap())
            .is_some()
    );
}
