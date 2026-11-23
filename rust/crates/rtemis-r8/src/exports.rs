//! The host-neutral checks of a package's compiled exports (`R8Exports`):
//! the method of its trait that an impl entry names, with the number of its
//! parameters, the impl declarations that the entries make, and the codec of
//! a Rust-stored type against its type's declaration.
//!
//! A host binding reads the table's pointers and owns the registry; it
//! passes the rows and the codec tables here.

use crate::abi::Codec;
use crate::ir::dispatch::tuple_text;
use crate::ir::{FieldDef, FieldType, ImplDef, ImplMethod, TypeDef, TypeRef, VariantKind};
use crate::name::{MethodName, QualifiedName};
use crate::rules::{DocumentSet, TypeLookup};

/// One impl entry of an exports table, its strings read.
#[derive(Debug, Clone, PartialEq)]
pub struct ImplRow {
    /// The qualified trait name.
    pub trait_name: String,
    /// The type the trait is implemented for, its first dispatched position,
    /// then the trait's other positions.
    pub positions: Vec<TypeRef>,
    /// The method's name, as the trait declares it or as its identifier in
    /// the hosts other than R ([`MethodName::host_identifier`]).
    pub method: String,
    /// The number of the method's parameters, `x` included and `...`
    /// counted as one.
    pub n_params: usize,
}

impl ImplRow {
    /// The impl's positions as a message writes them: a single type bare,
    /// several in parentheses.
    fn target(&self) -> String {
        match &self.positions[..] {
            [one] => format!("`{one}`"),
            many => format!("({})", tuple_text(many)),
        }
    }
}

/// Reads each row's method as the one its trait declares: the method of that
/// name, or the one whose identifier in the hosts other than R it is, such as
/// `labels<-` for `with_labels`. The trait is declared, and its method has as
/// many parameters as the row's function.
pub fn resolve_methods(
    rows: &mut [ImplRow],
    set: &DocumentSet,
    package: &str,
) -> Result<(), String> {
    for r in rows {
        let Some(declared) = QualifiedName::parse(&r.trait_name).ok().and_then(|q| {
            set.namespace(&q.namespace)?
                .traits
                .iter()
                .find(|t| t.name == q.name)
        }) else {
            return Err(format!(
                "`{package}` implements `{}`, which is not registered",
                r.trait_name
            ));
        };
        let method = declared
            .methods
            .iter()
            .find(|m| m.name.as_str() == r.method)
            .or_else(|| {
                declared
                    .methods
                    .iter()
                    .find(|m| m.name.host_identifier() == r.method)
            });
        match method {
            None => {
                return Err(format!(
                    "`{package}` implements the method `{}` of `{}`, which declares no such method",
                    r.method, r.trait_name
                ));
            }
            Some(m) if m.params.len() != r.n_params => {
                return Err(format!(
                    "the impl function `{}` of `{}` for {} takes {} parameters, and the trait declares {}",
                    r.method,
                    r.trait_name,
                    r.target(),
                    r.n_params,
                    m.params.len()
                ));
            }
            Some(m) => r.method = m.name.to_string(),
        }
    }
    Ok(())
}

/// The impl declarations of an exports table's rows: one per trait and
/// positions, in the order of their first row, with the methods in row order.
/// A row that repeats a trait, positions, and method is an error.
pub fn impls_from_rows(rows: &[ImplRow], package: &str) -> Result<Vec<ImplDef>, String> {
    let mut out: Vec<ImplDef> = Vec::new();
    let mut keys: Vec<(&str, &[TypeRef])> = Vec::new();
    for r in rows {
        let Some((target, type_args)) = r.positions.split_first() else {
            return Err(format!(
                "the impl function `{}` of `{}` of `{package}` names no type that it is for",
                r.method, r.trait_name
            ));
        };
        let method = MethodName::new(r.method.clone()).map_err(|e| {
            format!(
                "the impl function `{}` of `{}` for {} of `{package}`: {e}",
                r.method,
                r.trait_name,
                r.target()
            )
        })?;
        let at = match keys
            .iter()
            .position(|k| *k == (r.trait_name.as_str(), &r.positions[..]))
        {
            Some(i) => i,
            None => {
                let r#trait = QualifiedName::parse(&r.trait_name).map_err(|e| {
                    format!(
                        "the trait name `{}` of an impl entry of `{package}`: {e}",
                        r.trait_name
                    )
                })?;
                out.push(ImplDef {
                    r#trait,
                    type_args: type_args.to_vec(),
                    target: target.clone(),
                    tag_value: None,
                    methods: Vec::new(),
                });
                keys.push((&r.trait_name, &r.positions));
                out.len() - 1
            }
        };
        let methods = &mut out[at].methods;
        if methods.iter().any(|m| m.name() == &method) {
            return Err(format!(
                "the exports table of `{package}` has two impl functions `{}` of `{}` for {}",
                r.method,
                r.trait_name,
                r.target()
            ));
        }
        methods.push(ImplMethod::Name(method));
    }
    Ok(out)
}

/// Checks that each impl defines every method of its trait that has no default
/// implementation. An impl for a class of the host may take the methods that
/// the class has, which the host checks.
pub fn check_complete(impls: &[ImplDef], set: &DocumentSet, package: &str) -> Result<(), String> {
    for i in impls {
        if matches!(i.target, TypeRef::HostClass { .. }) {
            continue;
        }
        let Some(tr) = set
            .namespace(&i.r#trait.namespace)
            .and_then(|d| d.traits.iter().find(|t| t.name == i.r#trait.name))
        else {
            continue;
        };
        let missing = tr
            .methods
            .iter()
            .find(|m| !m.has_default_impl && !i.methods.iter().any(|d| d.name() == &m.name));
        if let Some(m) = missing {
            let row = ImplRow {
                trait_name: i.r#trait.to_string(),
                positions: std::iter::once(&i.target)
                    .chain(&i.type_args)
                    .cloned()
                    .collect(),
                method: m.name.to_string(),
                n_params: 0,
            };
            return Err(format!(
                "the impl of `{}` for {} of `{package}` defines `{}`, which has no default implementation",
                row.trait_name,
                row.target(),
                m.name
            ));
        }
    }
    Ok(())
}

/// Whether a codec table sets the members of an enum's codec.
fn has_enum_members(v: &Codec) -> bool {
    v.variant.is_some() || v.from_variant.is_some()
}

/// Checks the codec of the Rust-stored type `name` against the type's
/// declaration in `lookup`: the codec names its module, a struct's codec sets `from_fields` and neither
/// `variant` nor `from_variant`, an enum's sets both and not `from_fields`,
/// and no field of the type, in any variant, is host-only, since the
/// codec's state holds every field.
pub fn check_codec(
    name: &QualifiedName,
    v: &Codec,
    lookup: &dyn TypeLookup,
    package: &str,
) -> Result<(), String> {
    let Some(def) = lookup.find(name) else {
        return Err(format!(
            "the codec of `{name}` of `{package}` has no declaration of its type"
        ));
    };
    if v.module.is_null() {
        return Err(format!(
            "the codec of `{name}` of `{package}` sets no `module`, the address that identifies the code whose states it holds"
        ));
    }
    let shape_ok = match def {
        TypeDef::Struct(_) => v.from_fields.is_some() && !has_enum_members(v),
        TypeDef::Enum(_) => {
            v.variant.is_some() && v.from_variant.is_some() && v.from_fields.is_none()
        }
    };
    if !shape_ok {
        return Err(format!(
            "the codec of `{name}` of `{package}` does not match its type: a struct's codec sets `from_fields` and neither `variant` nor `from_variant`, and an enum's sets `variant` and `from_variant` and not `from_fields`"
        ));
    }
    let host_only = |fs: &[FieldDef]| {
        fs.iter()
            .find(|f| match &f.ty {
                FieldType::HostClass { .. } => true,
                FieldType::Option { inner, .. } => matches!(**inner, FieldType::HostClass { .. }),
                _ => false,
            })
            .map(|f| f.name.to_string())
    };
    let found = match def {
        TypeDef::Struct(st) => host_only(&st.fields),
        TypeDef::Enum(e) => e.variants.iter().find_map(|var| match &var.kind {
            VariantKind::Struct { fields, .. } => host_only(fields),
            VariantKind::Newtype { of } => match lookup.find(of) {
                Some(TypeDef::Struct(st)) => host_only(&st.fields),
                _ => None,
            },
            VariantKind::Unit => None,
        }),
    };
    match found {
        Some(field) => Err(format!(
            "the Rust-stored type `{name}` of `{package}` declares the host-only field `{field}`; its codec's state holds every field"
        )),
        None => Ok(()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn named(name: &str) -> TypeRef {
        TypeRef::Named {
            name: QualifiedName::parse(name).unwrap(),
        }
    }

    fn row(t: &str, ty: &str, m: &str) -> ImplRow {
        ImplRow {
            trait_name: t.into(),
            positions: vec![named(ty)],
            method: m.into(),
            n_params: 1,
        }
    }

    #[test]
    fn rows_group_by_trait_and_positions_in_order() {
        let pair = |m: &str, y: TypeRef| {
            let mut r = row("p::Overlaps", "p::Point", m);
            r.positions.push(y);
            r
        };
        let rows = [
            row("p::Norm", "p::Point", "magnitude"),
            row("R8::Display", "p::Point", "format"),
            row("p::Norm", "p::Point", "ratio"),
            pair("overlaps", named("p::Line")),
            pair("overlaps", TypeRef::Any),
            pair("distance", named("p::Line")),
        ];
        let impls = impls_from_rows(&rows, "p").unwrap();
        assert_eq!(impls.len(), 4);
        assert_eq!(impls[0].r#trait.to_string(), "p::Norm");
        let methods = |i: usize| -> Vec<&str> {
            impls[i].methods.iter().map(|m| m.name().as_str()).collect()
        };
        assert_eq!(methods(0), ["magnitude", "ratio"]);
        assert_eq!(impls[1].r#trait.to_string(), "R8::Display");
        assert_eq!(methods(2), ["overlaps", "distance"]);
        assert_eq!(impls[2].target, named("p::Point"));
        assert_eq!(impls[2].type_args, [named("p::Line")]);
        assert_eq!(impls[3].type_args, [TypeRef::Any]);
    }

    #[test]
    fn a_repeated_row_is_an_error() {
        let rows = [
            row("p::Norm", "p::Point", "magnitude"),
            row("p::Norm", "p::Point", "magnitude"),
        ];
        let e = impls_from_rows(&rows, "p").unwrap_err();
        assert!(e.contains("two impl functions `magnitude`"), "{e}");
        assert!(e.contains("for `p::Point`"), "{e}");
    }

    #[test]
    fn a_row_without_positions_is_an_error() {
        let mut r = row("p::Norm", "p::Point", "magnitude");
        r.positions.clear();
        let e = impls_from_rows(&[r], "p").unwrap_err();
        assert!(e.contains("names no type"), "{e}");
    }

    #[test]
    fn an_impl_without_a_method_is_an_error() {
        let set = declared();
        let with = |rows: &[ImplRow]| {
            let impls = impls_from_rows(rows, "q").unwrap();
            check_complete(&impls, &set, "q")
        };
        let rows = [
            row("p::Label", "p::Point", "labels<-"),
            row("p::Label", "p::Point", "as.text"),
        ];
        assert_eq!(with(&rows), Ok(()));
        let e = with(&rows[..1]).unwrap_err();
        assert!(
            e.contains("for `p::Point`") && e.contains("defines `as.text`"),
            "{e}"
        );
        // A class of the host may have the method.
        let mut host = row("p::Label", "p::Point", "labels<-");
        host.positions = vec![TypeRef::HostClass {
            host: "r".into(),
            package: String::new(),
            class: "data.frame".into(),
            system: crate::ir::ClassSystem::S3,
        }];
        assert_eq!(with(&[host]), Ok(()));
    }

    fn declared() -> DocumentSet {
        let doc: crate::ir::IrDocument = serde_json::from_value(serde_json::json!({
            "$ir": 1,
            "namespace": "p",
            "types": [],
            "traits": [{
                "name": "Label",
                "description": "",
                "methods": [
                    {"name": "labels<-", "description": "", "params": ["x", "value"]},
                    {"name": "as.text", "description": "", "params": ["x"]},
                ],
            }],
            "impls": [],
        }))
        .unwrap();
        DocumentSet::from(vec![doc])
    }

    #[test]
    fn a_method_is_found_by_its_name_or_its_identifier() {
        let with = |m: &str, n: usize| ImplRow {
            n_params: n,
            ..row("p::Label", "p::Point", m)
        };
        let mut rows = [
            with("with_labels", 2),
            with("labels<-", 2),
            with("as_text", 1),
        ];
        resolve_methods(&mut rows, &declared(), "q").unwrap();
        let found: Vec<&str> = rows.iter().map(|r| r.method.as_str()).collect();
        assert_eq!(found, ["labels<-", "labels<-", "as.text"]);
        let e = resolve_methods(&mut [with("labels", 2)], &declared(), "q").unwrap_err();
        assert!(e.contains("declares no such method"), "{e}");
        let e = resolve_methods(&mut [with("as_text", 2)], &declared(), "q").unwrap_err();
        assert!(
            e.contains("takes 2 parameters, and the trait declares 1"),
            "{e}"
        );
        let mut other = [row("p::Other", "p::Point", "m")];
        let e = resolve_methods(&mut other, &declared(), "q").unwrap_err();
        assert!(e.contains("`p::Other`, which is not registered"), "{e}");
    }

    #[test]
    fn an_invalid_name_is_an_error() {
        let e = impls_from_rows(&[row("Norm", "p::Point", "m")], "p").unwrap_err();
        assert!(e.contains("trait name `Norm`"), "{e}");
    }
}
