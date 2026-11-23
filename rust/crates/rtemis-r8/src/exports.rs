//! The host-neutral checks of a package's compiled exports (`R8Exports`):
//! the impl declarations that the impl entries make, the codec of a
//! Rust-stored type against its type's declaration, and the number of an
//! impl function's parameters against its trait's method.
//!
//! A host binding reads the table's pointers and owns the registry; it
//! passes the rows and the codec tables here.

use crate::abi::Codec;
use crate::ir::{FieldDef, FieldType, ImplDef, ImplMethod, TypeDef, TypeRef, VariantKind};
use crate::name::{MethodName, QualifiedName};
use crate::rules::{DocumentSet, TypeLookup};

/// One impl entry of an exports table, its strings read.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ImplRow {
    /// The qualified trait name.
    pub trait_name: String,
    /// The qualified name of the type the trait is implemented for.
    pub type_name: String,
    /// The method's name.
    pub method: String,
    /// The number of the method's parameters, `x` included and `...`
    /// counted as one.
    pub n_params: usize,
}

/// The impl declarations of an exports table's rows: one per trait and
/// type, in the order of their first row, with the methods in row order.
/// A row that repeats a trait, type, and method is an error.
pub fn impls_from_rows(rows: &[ImplRow], package: &str) -> Result<Vec<ImplDef>, String> {
    let mut out: Vec<ImplDef> = Vec::new();
    let mut keys: Vec<(&str, &str)> = Vec::new();
    for r in rows {
        let method = MethodName::new(r.method.clone()).map_err(|e| {
            format!(
                "the impl function `{}` of `{}` for `{}` of `{package}`: {e}",
                r.method, r.trait_name, r.type_name
            )
        })?;
        let at = match keys
            .iter()
            .position(|k| *k == (r.trait_name.as_str(), r.type_name.as_str()))
        {
            Some(i) => i,
            None => {
                let parse = |s: &str, what: &str| {
                    QualifiedName::parse(s).map_err(|e| {
                        format!("the {what} name `{s}` of an impl entry of `{package}`: {e}")
                    })
                };
                out.push(ImplDef {
                    r#trait: parse(&r.trait_name, "trait")?,
                    type_args: Vec::new(),
                    target: TypeRef::Named {
                        name: parse(&r.type_name, "type")?,
                    },
                    tag_value: None,
                    methods: Vec::new(),
                });
                keys.push((&r.trait_name, &r.type_name));
                out.len() - 1
            }
        };
        let methods = &mut out[at].methods;
        if methods.iter().any(|m| m.name() == &method) {
            return Err(format!(
                "the exports table of `{package}` has two impl functions `{}` of `{}` for `{}`",
                r.method, r.trait_name, r.type_name
            ));
        }
        methods.push(ImplMethod::Name(method));
    }
    Ok(out)
}

/// Checks that each row's number of parameters equals the number its trait
/// declares for the method, the trait's package having declared it.
pub fn check_params(rows: &[ImplRow], set: &DocumentSet, package: &str) -> Result<(), String> {
    for r in rows {
        let declared = QualifiedName::parse(&r.trait_name).ok().and_then(|q| {
            set.namespace(&q.namespace)?
                .traits
                .iter()
                .find(|t| t.name == q.name)?
                .methods
                .iter()
                .find(|m| m.name.as_str() == r.method)
                .map(|m| m.params.len())
        });
        match declared {
            Some(n) if n != r.n_params => {
                return Err(format!(
                    "the impl function `{}` of `{}` for `{}` takes {} parameters, and the trait declares {n}",
                    r.method, r.trait_name, r.type_name, r.n_params
                ));
            }
            Some(_) => {}
            None => {
                return Err(format!(
                    "the trait `{}` declares no method `{}`; its package declares it before `{package}` registers",
                    r.trait_name, r.method
                ));
            }
        }
    }
    Ok(())
}

/// Whether a codec table sets the members of an enum's codec.
fn has_enum_members(v: &Codec) -> bool {
    v.variant.is_some() || v.from_variant.is_some()
}

/// Checks the codec of the Rust-stored type `name` against the type's
/// declaration in `lookup`: a struct's codec sets `from_fields` and neither
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

    fn row(t: &str, ty: &str, m: &str) -> ImplRow {
        ImplRow {
            trait_name: t.into(),
            type_name: ty.into(),
            method: m.into(),
            n_params: 1,
        }
    }

    #[test]
    fn rows_group_by_trait_and_type_in_order() {
        let rows = [
            row("p::Norm", "p::Point", "magnitude"),
            row("R8::Display", "p::Point", "format"),
            row("p::Norm", "p::Point", "ratio"),
        ];
        let impls = impls_from_rows(&rows, "p").unwrap();
        assert_eq!(impls.len(), 2);
        assert_eq!(impls[0].r#trait.to_string(), "p::Norm");
        let methods: Vec<&str> = impls[0].methods.iter().map(|m| m.name().as_str()).collect();
        assert_eq!(methods, ["magnitude", "ratio"]);
        assert_eq!(impls[1].r#trait.to_string(), "R8::Display");
    }

    #[test]
    fn a_repeated_row_is_an_error() {
        let rows = [
            row("p::Norm", "p::Point", "magnitude"),
            row("p::Norm", "p::Point", "magnitude"),
        ];
        let e = impls_from_rows(&rows, "p").unwrap_err();
        assert!(e.contains("two impl functions `magnitude`"), "{e}");
    }

    #[test]
    fn an_invalid_name_is_an_error() {
        let e = impls_from_rows(&[row("Norm", "p::Point", "m")], "p").unwrap_err();
        assert!(e.contains("trait name `Norm`"), "{e}");
    }
}
