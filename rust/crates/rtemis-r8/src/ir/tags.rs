//! The tag values of a trait's implementors.
//!
//! A trait with a tag member names each implementor of a `dyn()` value by
//! its written tag value: the `tag_value` of the type's own impl, or else the
//! type's name, bare in the trait's namespace and prefixed with the type's
//! namespace elsewhere. A descendant that inherits an impl through `extends`
//! writes its own name. [`check_tags`] reports what makes a written tag
//! ambiguous or a value unwritable.
//! spec: R8/class-system#dyn

use super::check::DeclError;
use super::decl::{ImplDef, TypeRef, VariantKind};
use super::document::TypeDef;
use crate::name::{Name, QualifiedName};
use crate::rules::TypeLookup;

/// The problems of the tag values of the implementors of the trait `tr`:
/// a `tag_value` on an impl of a trait without a tag member
/// (`tag_without_member`), a `tag_value` outside the name grammar
/// (`tag_value_invalid`), two implementors that write one tag value
/// (`duplicate_tag`), and an implementor or descendant with a field named as
/// the tag member, or an implementing enum whose own tag is the tag member
/// (`field_is_tag`). Each problem's path is `path_of(impl)`, the impl that
/// provides the implementor.
pub fn check_tags(
    lookup: &dyn TypeLookup,
    tr: &QualifiedName,
    path_of: &dyn Fn(&ImplDef) -> String,
) -> Vec<DeclError> {
    let mut out = Vec::new();
    let tag = lookup.type_member(tr);
    let tagged = tag != "$type";
    let impls: Vec<&ImplDef> = lookup.impls_of(tr);
    let mut written: Vec<(String, QualifiedName)> = Vec::new();
    for imp in &impls {
        let TypeRef::Named { name: target } = &imp.target else {
            continue;
        };
        let path = path_of(imp);
        if let Some(v) = &imp.tag_value {
            if !tagged {
                out.push(DeclError {
                    code: "tag_without_member",
                    path: format!("{path}/tag_value"),
                    message: format!(
                        "the impl of `{tr}` for `{target}` gives a tag value, but `{tr}` has no tag member"
                    ),
                });
                continue;
            }
            if Name::new(v).is_err() {
                out.push(DeclError {
                    code: "tag_value_invalid",
                    path: format!("{path}/tag_value"),
                    message: format!(
                        "tag value `{v}` of `{target}` is not a name: a letter, then letters, digits, and underscores"
                    ),
                });
                continue;
            }
        }
        if !tagged {
            continue;
        }
        // The target and its descendants that inherit this impl.
        let mut implementors = vec![target.clone()];
        implementors.extend(
            lookup
                .descendants_of(target)
                .into_iter()
                .filter(|d| lookup.implements(tr, d).as_ref() == Some(target)),
        );
        for ty in implementors {
            if let Some(field) = tag_field(lookup, &ty, &tag) {
                out.push(DeclError {
                    code: "field_is_tag",
                    path: path.clone(),
                    message: format!(
                        "`{ty}` has {field} named `{tag}`, the tag member of `{tr}`, which a `dyn({tr})` value would write twice"
                    ),
                });
            }
            let Some(v) = lookup.tag_value(tr, &ty) else {
                continue;
            };
            match written.iter().find(|(w, _)| *w == v) {
                Some((_, other)) if *other != ty => out.push(DeclError {
                    code: "duplicate_tag",
                    path: path.clone(),
                    message: format!(
                        "`{ty}` and `{other}` write the tag value `{v}` for `{tr}`; give one of them another `tag_value`"
                    ),
                }),
                Some(_) => {}
                None => written.push((v, ty)),
            }
        }
    }
    out
}

/// What of the type `ty` is named as the tag member `tag`: a field, a field
/// of a variant, or an enum's own tag.
fn tag_field(lookup: &dyn TypeLookup, ty: &QualifiedName, tag: &str) -> Option<&'static str> {
    match lookup.find(ty)? {
        // The members of a table's document are its columns.
        TypeDef::Table(_) => Some("a possible column"),
        TypeDef::Struct(s) => s
            .fields
            .iter()
            .any(|f| f.name.as_str() == tag)
            .then_some("a field"),
        TypeDef::Enum(e) => {
            if e.tag.as_str() == tag {
                return Some("its own tag");
            }
            e.variants
                .iter()
                .any(|v| match &v.kind {
                    VariantKind::Struct { fields, .. } => {
                        fields.iter().any(|f| f.name.as_str() == tag)
                    }
                    _ => false,
                })
                .then_some("a variant field")
        }
    }
}
