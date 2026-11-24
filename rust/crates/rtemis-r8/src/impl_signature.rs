//! The signatures of a compiled impl's methods, against the declaration of
//! their trait.
//!
//! Compiled code states a signature as the methods of a [`TraitDef`], whose
//! types are those of its parameters and results: a parameter without a type
//! is one that the code receives as a value of the host. [`check`] compares
//! it with the registered trait, and [`own_return_type`] gives the result type
//! of a method that returns a narrower type than its trait declares.
//! [`trait_hash`] identifies a trait's declaration.
//! spec: R8/class-system#signatures-of-impls-in-rust

use crate::canonical;
use crate::ir::notation::notation;
use crate::ir::{
    CharacterType, DoubleType, FieldType, IntegerType, MethodDef, ParamDef, RequiredField,
    ReturnType, TraitDef, TypeRef,
};
use crate::rules::TypeLookup;

/// A difference of a method's signature from its trait's declaration, or of
/// a required field's accessor.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Mismatch {
    /// The name of the method in the trait, or of the required field.
    pub member: String,
    /// The difference.
    pub message: String,
}

/// `t` without its constraints: bounds, allowed values, patterns, and
/// lengths, through `option()` and `vec()`.
pub fn bare(t: &FieldType) -> FieldType {
    match t {
        FieldType::Integer(_) => FieldType::Integer(IntegerType::default()),
        FieldType::Double(_) => FieldType::Double(DoubleType::default()),
        FieldType::Character(_) => FieldType::Character(CharacterType::default()),
        FieldType::Option { inner } => FieldType::Option {
            inner: Box::new(bare(inner)),
        },
        FieldType::Vec { element, .. } => FieldType::Vec {
            element: Box::new(bare(element)),
            min_length: None,
            max_length: None,
            unique: false,
        },
        other => other.clone(),
    }
}

/// Whether compiled code has a parameter type of its own for the bare type
/// `t`, and so states it: a scalar of a number, a logical, or a string, a
/// vector of numbers or logicals, bytes, and `option()` of one. It may
/// receive a value of any other type as a value of the host.
fn has_own_type(t: &FieldType) -> bool {
    match t {
        FieldType::Logical
        | FieldType::Integer(_)
        | FieldType::Double(_)
        | FieldType::Character(_)
        | FieldType::Raw => true,
        FieldType::Option { inner } => has_own_type(inner),
        FieldType::Vec { element, .. } => matches!(
            **element,
            FieldType::Logical | FieldType::Integer(_) | FieldType::Double(_)
        ),
        _ => false,
    }
}

/// A list of parameters as a message writes it.
fn names(params: &[ParamDef]) -> String {
    let n: Vec<&str> = params.iter().map(ParamDef::name).collect();
    format!("({})", n.join(", "))
}

/// The notation of a parameter's type, or that it has none.
fn type_text(t: Option<&FieldType>) -> String {
    t.map_or_else(|| "without a type".to_string(), notation)
}

fn check_params(have: &MethodDef, want: &MethodDef, out: &mut Vec<String>) {
    // The receiver of compiled code has a name only when the code states it.
    let named = have.params.len() == want.params.len()
        && have
            .params
            .iter()
            .zip(&want.params)
            .enumerate()
            .all(|(i, (h, w))| h.name() == w.name() || (i == 0 && h.name().is_empty()));
    if !named {
        let mut stated = have.params.clone();
        if let (Some(h), Some(w)) = (stated.first_mut(), want.params.first())
            && h.name().is_empty()
        {
            *h = ParamDef::Name(w.name().to_string());
        }
        out.push(format!(
            "it takes the parameters {}, and the declaration {}",
            names(&stated),
            names(&want.params)
        ));
        return;
    }
    let (hd, wd) = (have.dispatched(), want.dispatched());
    for (i, (h, w)) in have.params.iter().zip(&want.params).enumerate() {
        let name = w.name();
        match (hd.contains(&i), wd.contains(&i)) {
            (true, true) => continue,
            (true, false) => {
                out.push(format!(
                    "it dispatches on `{name}`, and the declaration does not"
                ));
                continue;
            }
            (false, true) => {
                out.push(format!(
                    "the declaration dispatches on `{name}`, and it does not"
                ));
                continue;
            }
            (false, false) => {}
        }
        if name == "..." {
            continue;
        }
        let same = match (h.ty(), w.ty()) {
            (Some(h), Some(w)) => h == w || *h == bare(w),
            (Some(_), None) => false,
            (None, Some(w)) => !has_own_type(&bare(w)),
            (None, None) => true,
        };
        if !same {
            out.push(format!(
                "its parameter `{name}` is {}, and in the declaration {}",
                type_text(h.ty()),
                type_text(w.ty())
            ));
            continue;
        }
        let default = |p: &ParamDef| match p {
            ParamDef::Full(p) => p.default.as_ref().map(|t| t.to_value()),
            ParamDef::Name(_) => None,
        };
        if let Some(d) = default(h)
            && default(w).map(|v| canonical::canonical_text(&v))
                != Some(canonical::canonical_text(&d))
        {
            out.push(format!(
                "the default of its parameter `{name}` is {d}, and the declaration's is another"
            ));
        }
    }
}

/// The qualified name of the type an impl is for, when it is an R8 type.
fn target_name(target: &TypeRef) -> Option<FieldType> {
    match target {
        TypeRef::Named { name } => Some(FieldType::Named { name: name.clone() }),
        _ => None,
    }
}

fn check_return(
    have: &MethodDef,
    want: &MethodDef,
    target: &TypeRef,
    lookup: &dyn TypeLookup,
    out: &mut Vec<String>,
) {
    let Some(ReturnType::Type(h)) = &have.return_type else {
        return;
    };
    let is_self =
        want.name.is_replacement() || matches!(want.return_type, Some(ReturnType::SelfType(_)));
    if is_self {
        if target_name(target).is_some_and(|t| t != **h) {
            out.push(format!(
                "it returns {}, and the declaration the type of its first argument, {target}",
                notation(h)
            ));
        }
        return;
    }
    if let Some(ReturnType::Type(w)) = &want.return_type
        && !crate::ir::fits(lookup, h, &bare(w))
    {
        out.push(format!(
            "it returns {}, and the declaration {}",
            notation(h),
            notation(w)
        ));
    }
}

/// The differences of `have`, the signature of a method of an impl for
/// `target`, from `want`, its method of the trait.
///
/// The parameters have the names of the declaration, in order, and dispatch
/// on the same ones. A parameter's type is the declared one without its
/// constraints, or with them; a parameter without a declared type has none,
/// and neither may one of a declared type that compiled code receives as a
/// value of the host. A stated default is the declared one. The result fits
/// the declared return type, without its constraints; the result of a method
/// that returns `Self` and of a replacement method is of the target.
pub fn check_method(
    have: &MethodDef,
    want: &MethodDef,
    target: &TypeRef,
    lookup: &dyn TypeLookup,
) -> Vec<Mismatch> {
    let mut lines = Vec::new();
    check_params(have, want, &mut lines);
    check_return(have, want, target, lookup, &mut lines);
    lines
        .into_iter()
        .map(|message| Mismatch {
            member: want.name.to_string(),
            message,
        })
        .collect()
}

/// The differences of the tag member and the accessors of required fields of
/// `have`, the signatures of an impl, from its trait `want`: a stated tag
/// member is the trait's, and each accessor gives a field that the trait
/// requires, of its type, without its constraints.
pub fn check_members(have: &TraitDef, want: &TraitDef) -> Vec<Mismatch> {
    let tag = have
        .tag
        .as_ref()
        .filter(|t| want.tag.as_ref() != Some(t))
        .map(|t| Mismatch {
            member: t.to_string(),
            message: match &want.tag {
                Some(w) => format!("its tag member is `{t}`, and in the declaration `{w}`"),
                None => format!("its tag member is `{t}`, and the declaration has none"),
            },
        });
    tag.into_iter()
        .chain(check_fields(&have.required_fields, want))
        .collect()
}

fn check_fields(have: &[RequiredField], want: &TraitDef) -> Vec<Mismatch> {
    have.iter()
        .filter_map(|h| {
            let declared = want.required_fields.iter().find(|w| w.name == h.name);
            let message = match declared {
                None => "the declaration requires no field of this name".to_string(),
                Some(w) if h.ty == w.ty || h.ty == bare(&w.ty) => return None,
                Some(w) => format!(
                    "its accessor gives {}, and the declaration requires {}",
                    notation(&h.ty),
                    notation(&w.ty)
                ),
            };
            Some(Mismatch {
                member: h.name.to_string(),
                message,
            })
        })
        .collect()
}

/// The result type of a method that returns a narrower type than `want`, its
/// method of the trait, declares.
pub fn own_return_type(
    have: &MethodDef,
    want: &MethodDef,
    lookup: &dyn TypeLookup,
) -> Option<FieldType> {
    match (&have.return_type, &want.return_type) {
        (Some(ReturnType::Type(h)), Some(ReturnType::Type(w)))
            if h != w && !want.name.is_replacement() && crate::ir::fits(lookup, h, w) =>
        {
            Some((**h).clone())
        }
        _ => None,
    }
}

/// The hash of a trait's declaration: the SHA-256 of the canonical text of
/// its IR with the descriptions emptied, `sha256:<hex>`.
pub fn trait_hash(t: &TraitDef) -> String {
    let mut t = t.clone();
    t.description.clear();
    for f in &mut t.required_fields {
        f.description = Box::default();
    }
    for m in &mut t.methods {
        m.description.clear();
        for p in &mut m.params {
            if let ParamDef::Full(full) = p {
                full.description = Box::default();
                *p = full.clone().into();
            }
        }
    }
    canonical::text_hash(&serde_json::to_value(&t).expect("a declaration serializes"))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::IrDocument;
    use serde_json::json;

    fn doc() -> IrDocument {
        serde_json::from_value(json!({
            "$ir": 1,
            "namespace": "p",
            "types": [
                {"kind": "struct", "name": "Range", "version": 1, "fields": []},
                {"kind": "struct", "name": "Window", "version": 1, "fields": [], "extends": "p::Range"},
            ],
        }))
        .unwrap()
    }

    fn method(v: serde_json::Value) -> MethodDef {
        serde_json::from_value(v).unwrap()
    }

    fn range() -> TypeRef {
        TypeRef::Named {
            name: crate::name::QualifiedName::parse("p::Range").unwrap(),
        }
    }

    fn found(have: serde_json::Value, want: serde_json::Value) -> Vec<String> {
        check_method(&method(have), &method(want), &range(), &doc())
            .into_iter()
            .map(|m| m.message)
            .collect()
    }

    fn m(params: serde_json::Value) -> serde_json::Value {
        json!({"name": "f", "params": params})
    }

    #[test]
    fn a_type_equals_the_declared_one_without_its_constraints() {
        let by = |t: serde_json::Value| m(json!(["x", {"name": "by", "type": t}]));
        let double = json!({"kind": "double"});
        let bounded = json!({"kind": "double", "min": 0});
        assert!(found(by(double.clone()), by(bounded.clone())).is_empty());
        assert!(found(by(bounded.clone()), by(bounded.clone())).is_empty());
        assert_eq!(
            found(by(bounded), by(double.clone())),
            ["its parameter `by` is double [0, Inf), and in the declaration double"]
        );
        let option = json!({"kind": "option", "inner": {"kind": "double", "max": 1}});
        let bare_option = json!({"kind": "option", "inner": {"kind": "double"}});
        assert!(found(by(bare_option), by(option)).is_empty());
        let vec = json!({"kind": "vec", "element": {"kind": "integer", "min": 1}, "min_length": 1});
        let bare_vec = json!({"kind": "vec", "element": {"kind": "integer"}});
        assert!(found(by(bare_vec), by(vec)).is_empty());
        assert_eq!(
            found(by(json!({"kind": "integer"})), by(double)),
            ["its parameter `by` is integer, and in the declaration double"]
        );
    }

    #[test]
    fn a_parameter_without_a_type_in_either_is_one_in_both() {
        let typed = |t: serde_json::Value| m(json!(["x", {"name": "by", "type": t}]));
        let double = json!({"kind": "double"});
        assert_eq!(
            found(typed(double.clone()), m(json!(["x", "by"]))),
            ["its parameter `by` is double, and in the declaration without a type"]
        );
        assert_eq!(
            found(m(json!(["x", "by"])), typed(double)),
            ["its parameter `by` is without a type, and in the declaration double"]
        );
        // Compiled code receives a value of these types as the host's.
        for t in [
            json!({"kind": "date"}),
            json!({"kind": "matrix", "element": {"kind": "double"}}),
            json!({"kind": "vec", "element": {"kind": "character"}}),
            json!({"kind": "named", "name": "p::Range"}),
            json!({"kind": "subtype", "of": "p::Range"}),
            json!({"kind": "option", "inner": {"kind": "named", "name": "p::Range"}}),
        ] {
            assert!(found(m(json!(["x", "by"])), typed(t)).is_empty());
        }
    }

    #[test]
    fn the_parameters_have_the_declared_names_and_dispatch() {
        assert!(
            found(
                m(json!(["", "by", "..."])),
                m(json!(["object", "by", "..."]))
            )
            .is_empty()
        );
        assert_eq!(
            found(m(json!(["", "to"])), m(json!(["x", "by"]))),
            ["it takes the parameters (x, to), and the declaration (x, by)"]
        );
        assert_eq!(
            found(m(json!(["y", "by"])), m(json!(["x", "by"]))),
            ["it takes the parameters (y, by), and the declaration (x, by)"]
        );
        let pair = |y: bool| {
            m(
                json!([{"name": "x", "dispatch": true}, if y { json!({"name": "y", "dispatch": true}) } else { json!("y") }]),
            )
        };
        assert!(found(pair(true), pair(true)).is_empty());
        assert_eq!(
            found(pair(true), pair(false)),
            ["it dispatches on `y`, and the declaration does not"]
        );
        assert_eq!(
            found(pair(false), pair(true)),
            ["the declaration dispatches on `y`, and it does not"]
        );
    }

    #[test]
    fn a_stated_default_is_the_declared_one() {
        let by = |d: serde_json::Value| {
            m(json!(["x", {"name": "by", "type": {"kind": "double"}, "default": d}]))
        };
        let none = || m(json!(["x", {"name": "by", "type": {"kind": "double"}}]));
        assert!(found(none(), by(json!(1))).is_empty());
        assert!(found(by(json!(1.0)), by(json!(1))).is_empty());
        assert_eq!(found(by(json!(2)), by(json!(1))).len(), 1);
        assert_eq!(found(by(json!(1)), none()).len(), 1);
    }

    #[test]
    fn a_result_fits_the_declared_return_type() {
        let r = |t: serde_json::Value| json!({"name": "f", "params": ["x"], "return_type": t});
        let named = |n: &str| json!({"kind": "named", "name": n});
        let subtype = json!({"kind": "subtype", "of": "p::Range"});
        let none = || m(json!(["x"]));
        assert!(found(r(json!({"kind": "double"})), none()).is_empty());
        assert!(found(none(), r(json!({"kind": "double"}))).is_empty());
        assert!(
            found(
                r(json!({"kind": "double"})),
                r(json!({"kind": "double", "min": 0}))
            )
            .is_empty()
        );
        assert!(found(r(named("p::Window")), r(subtype.clone())).is_empty());
        assert_eq!(
            found(r(json!({"kind": "double"})), r(subtype.clone())),
            ["it returns double, and the declaration subtype of `p::Range`"]
        );
        assert!(found(r(named("p::Range")), r(json!("Self"))).is_empty());
        assert_eq!(
            found(r(named("p::Window")), r(json!("Self"))),
            [
                "it returns `p::Window`, and the declaration the type of its first argument, p::Range"
            ]
        );
        let replace = json!({"name": "f<-", "params": ["x", "value"]});
        let have =
            json!({"name": "with_f", "params": ["", "value"], "return_type": named("p::Window")});
        assert_eq!(found(have, replace).len(), 1);
        let own = |h: serde_json::Value, w: serde_json::Value| {
            own_return_type(&method(h), &method(w), &doc())
        };
        assert!(matches!(
            own(r(named("p::Window")), r(subtype.clone())),
            Some(FieldType::Named { .. })
        ));
        assert_eq!(own(r(subtype.clone()), r(subtype)), None);
        assert_eq!(
            own(
                r(json!({"kind": "double"})),
                r(json!({"kind": "double", "min": 0}))
            ),
            None
        );
    }

    #[test]
    fn an_accessor_gives_a_required_field() {
        let t: TraitDef = serde_json::from_value(json!({
            "name": "T",
            "methods": [{"name": "f", "params": ["x"]}],
            "required_fields": [{"name": "label", "type": {"kind": "character", "pattern": "a"}}],
        }))
        .unwrap();
        let field = |n: &str, k: &str| -> RequiredField {
            serde_json::from_value(json!({"name": n, "type": {"kind": k}})).unwrap()
        };
        assert!(check_fields(&[field("label", "character")], &t).is_empty());
        let wrong = check_fields(&[field("label", "double"), field("name", "character")], &t);
        assert_eq!(wrong.len(), 2);
        assert!(wrong[0].message.starts_with("its accessor gives double"));
        assert!(wrong[1].message.contains("requires no field"));
        let mut tagged = t.clone();
        tagged.tag = Some(crate::name::Name::new("kind").unwrap());
        assert!(check_members(&t, &tagged).is_empty());
        assert!(check_members(&tagged, &tagged).is_empty());
        assert_eq!(check_members(&tagged, &t).len(), 1);
    }

    #[test]
    fn the_hash_of_a_trait_excludes_descriptions() {
        let t = |d: &str, min: i32| -> TraitDef {
            serde_json::from_value(json!({
                "name": "T", "description": d,
                "methods": [{"name": "f", "description": d, "params": ["x",
                    {"name": "by", "type": {"kind": "double", "min": min}, "description": d}]}],
            }))
            .unwrap()
        };
        assert_eq!(trait_hash(&t("a", 0)), trait_hash(&t("b", 0)));
        assert_ne!(trait_hash(&t("a", 0)), trait_hash(&t("a", 1)));
        let described: TraitDef = serde_json::from_value(json!({
            "name": "T", "methods": [{"name": "f", "params": ["x", {"name": "y", "description": "d"}]}],
        }))
        .unwrap();
        let plain: TraitDef = serde_json::from_value(json!({
            "name": "T", "methods": [{"name": "f", "params": ["x", "y"]}],
        }))
        .unwrap();
        assert_eq!(trait_hash(&described), trait_hash(&plain));
    }
}
