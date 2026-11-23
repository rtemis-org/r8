//! `extends` in the derive: the field of `#[r8(extends)]`, which embeds the
//! parent of a struct, and the interface trait of `#[r8(extensible)]`.
//! spec: R8/class-system#types-declared-in-rust

use proc_macro2::{Span, TokenStream as Tokens};
use quote::{format_ident, quote, quote_spanned};
use syn::spanned::Spanned;
use syn::{Error, Ident, Result, Visibility};

use crate::{default_tree, description, field_name, valid_name};

/// The field of `#[r8(extends)]`: the embedded parent.
pub struct Parent<'a> {
    /// The field.
    pub ident: &'a Ident,
    /// Its type, the parent struct.
    pub ty: &'a syn::Type,
    /// The construction of each `rtemis_r8::ir::InheritedChange`.
    pub changes: Vec<Tokens>,
}

/// Whether a field carries `#[r8(extends)]`.
fn is_extends(field: &syn::Field) -> bool {
    let mut found = false;
    for a in field.attrs.iter().filter(|a| a.path().is_ident("r8")) {
        // The parser of the field's kind reports an attribute that does not
        // read.
        let _ = a.parse_nested_meta(|m| {
            found |= m.path.is_ident("extends");
            if m.input.peek(syn::Token![=]) {
                let _: syn::Expr = m.value()?.parse()?;
            } else if m.input.peek(syn::token::Paren) {
                let content;
                syn::parenthesized!(content in m.input);
                let _: Tokens = content.parse()?;
            }
            Ok(())
        });
    }
    found
}

/// The embedded parent of a struct and the struct's own fields. The marked
/// field is the struct's first, as the parent's fields precede the struct's
/// own in its declaration.
pub fn split<'a>(fields: &[&'a syn::Field]) -> Result<(Option<Parent<'a>>, Vec<&'a syn::Field>)> {
    for f in fields.iter().skip(1) {
        if is_extends(f) {
            return Err(Error::new_spanned(
                f,
                "`#[r8(extends)]` marks the first field, and one only: the parent's fields precede the struct's own",
            ));
        }
    }
    match fields.split_first() {
        Some((first, own)) if is_extends(first) => Ok((Some(parent(first)?), own.to_vec())),
        _ => Ok((None, fields.to_vec())),
    }
}

/// The fields of an enum's struct variant, none of which embeds a parent.
pub fn none(fields: &[&syn::Field]) -> Result<()> {
    match fields.iter().find(|f| is_extends(f)) {
        Some(f) => Err(Error::new_spanned(
            f,
            "`#[r8(extends)]` marks a field of a struct; a variant extends no struct",
        )),
        None => Ok(()),
    }
}

fn parent(field: &syn::Field) -> Result<Parent<'_>> {
    let mut changes = Vec::new();
    for a in field.attrs.iter().filter(|a| a.path().is_ident("r8")) {
        a.parse_nested_meta(|meta| {
            if meta.path.is_ident("extends") {
                return Ok(());
            }
            if meta.path.is_ident("inherited") {
                return meta.parse_nested_meta(|m| {
                    changes.push(change(&m)?);
                    Ok(())
                });
            }
            Err(meta.error(
                "the field of `#[r8(extends)]` takes `inherited(...)`; its own attributes are the parent's",
            ))
        })?;
    }
    Ok(Parent {
        ident: field.ident.as_ref().expect("named fields"),
        ty: &field.ty,
        changes,
    })
}

/// One change to an inherited field, `name(default = "<JSON>", sensitive,
/// description = "...")`, as the construction of an
/// `rtemis_r8::ir::InheritedChange`; the default `null` removes the
/// inherited one.
fn change(meta: &syn::meta::ParseNestedMeta) -> Result<Tokens> {
    let name = match meta.path.get_ident() {
        Some(i) => field_name(i),
        None => return Err(meta.error("the name of an inherited field")),
    };
    if !valid_name(&name) {
        return Err(meta.error("a field name matches [A-Za-z][A-Za-z0-9_]*"));
    }
    let mut default = quote! { ::std::option::Option::None };
    let mut sensitive = false;
    let mut text = quote! { ::std::option::Option::None };
    meta.parse_nested_meta(|m| {
        match m.path.get_ident().map(|i| i.to_string()).as_deref() {
            Some("default") => {
                let s: syn::LitStr = m.value()?.parse()?;
                default = match default_tree(&s)? {
                    Some(tree) => quote! {
                        ::std::option::Option::Some(::std::option::Option::Some(::std::boxed::Box::new(#tree)))
                    },
                    None => quote! { ::std::option::Option::Some(::std::option::Option::None) },
                };
            }
            Some("sensitive") => sensitive = true,
            Some("description") => {
                let s: syn::LitStr = m.value()?.parse()?;
                text = quote! { ::std::option::Option::Some(#s.into()) };
            }
            _ => {
                return Err(m.error(
                    "a change to an inherited field takes default, sensitive, and description",
                ));
            }
        }
        Ok(())
    })?;
    Ok(quote! {
        ::rtemis_r8::ir::InheritedChange {
            name: ::rtemis_r8::derive::name(#name),
            default: #default,
            sensitive: #sensitive,
            description: #text,
        }
    })
}

/// The path of the interface trait of the struct that `ty` names: its path
/// with `Like` after the last segment's name.
fn like_path(ty: &syn::Type) -> Result<syn::Path> {
    if let syn::Type::Path(p) = ty
        && p.qself.is_none()
        && let Some(last) = p.path.segments.last()
        && last.arguments.is_empty()
    {
        let mut path = p.path.clone();
        let name = format_ident!("{}Like", last.ident);
        path.segments.last_mut().expect("a segment").ident = name;
        return Ok(path);
    }
    Err(Error::new_spanned(
        ty,
        "the type of `#[r8(extends)]` is the name of a struct of `#[r8(extensible)]`",
    ))
}

/// The snake_case form of a type's name.
fn snake(name: &str) -> String {
    let mut out = String::new();
    let chars: Vec<char> = name.chars().collect();
    for (i, c) in chars.iter().enumerate() {
        // A word begins at an uppercase letter after a lowercase letter or a
        // digit, and at the last of a run of uppercase letters before a
        // lowercase one: `GLMNETModel` is `glmnet_model`.
        let starts = c.is_uppercase()
            && i > 0
            && chars[i - 1] != '_'
            && (!chars[i - 1].is_uppercase() || chars.get(i + 1).is_some_and(|n| n.is_lowercase()));
        if starts {
            out.push('_');
        }
        out.extend(c.to_lowercase());
    }
    out
}

/// `impl Subtype<P>` for each type `P` that the embedded parent is a subtype
/// of.
pub fn subtype_of_parent(ident: &Ident, parent: &Parent) -> Tokens {
    let (base, ty) = (parent.ident, parent.ty);
    quote! {
        impl<__R8P> ::rtemis_r8::derive::Subtype<__R8P> for #ident
        where
            #ty: ::rtemis_r8::derive::Subtype<__R8P>,
        {
            fn upcast(&self) -> &__R8P {
                <#ty as ::rtemis_r8::derive::Subtype<__R8P>>::upcast(&self.#base)
            }
        }
    }
}

/// The requirement that the parent is extensible, reported at its type.
pub fn parent_is_extensible(parent: &Parent) -> Tokens {
    let ty = parent.ty;
    quote_spanned! { ty.span() =>
        const _: fn() = || {
            fn extensible<__R8P: ::rtemis_r8::derive::Extensible>() {}
            extensible::<#ty>();
        };
    }
}

/// `Subtype<Self>`, `Extensible`, and the interface trait `<Type>Like` of a
/// struct of `#[r8(extensible)]`: an accessor per field that the struct
/// adds, `to_<type>()`, and the interface trait of its parent as a
/// supertrait.
pub fn interface(
    ident: &Ident,
    vis: &Visibility,
    parent: Option<&Parent>,
    own: &[&syn::Field],
) -> Result<Tokens> {
    let like = format_ident!("{}Like", ident);
    let to = format_ident!("to_{}", snake(&ident.to_string()));
    let supertrait = parent.map(|p| like_path(p.ty)).transpose()?;
    let (colon, bound) = match &supertrait {
        Some(s) => (quote! { : #s }, quote! { + #s }),
        None => (quote! {}, quote! {}),
    };
    let mut decls = Vec::new();
    let mut impls = Vec::new();
    for f in own {
        let name = f.ident.as_ref().expect("named fields");
        let ty = &f.ty;
        let doc = match description(&f.attrs) {
            d if d.is_empty() => format!("The field `{}`.", field_name(name)),
            d => d,
        };
        decls.push(quote! {
            #[doc = #doc]
            fn #name(&self) -> &#ty;
        });
        impls.push(quote! {
            fn #name(&self) -> &#ty {
                &<__R8S as ::rtemis_r8::derive::Subtype<#ident>>::upcast(self).#name
            }
        });
    }
    let trait_doc = format!(
        "The interface of [`{ident}`] and of every struct that extends it: an accessor per field and the `{ident}` value."
    );
    let to_doc = format!("The [`{ident}`] value that the value holds.");
    Ok(quote! {
        impl ::rtemis_r8::derive::Subtype<#ident> for #ident {
            fn upcast(&self) -> &#ident {
                self
            }
        }
        impl ::rtemis_r8::derive::Extensible for #ident {}
        #[doc = #trait_doc]
        #[allow(dead_code)]
        #vis trait #like #colon {
            #(#decls)*
            #[doc = #to_doc]
            fn #to(&self) -> #ident;
        }
        impl<__R8S: ::rtemis_r8::derive::Subtype<#ident> #bound> #like for __R8S {
            #(#impls)*
            fn #to(&self) -> #ident {
                ::std::clone::Clone::clone(<__R8S as ::rtemis_r8::derive::Subtype<#ident>>::upcast(self))
            }
        }
    })
}

/// An error for `#[r8(extensible)]` on an enum.
pub fn enum_is_closed() -> Error {
    Error::new(
        Span::call_site(),
        "`#[r8(extensible)]` applies to a struct; an enum is closed",
    )
}

#[cfg(test)]
mod tests {
    use super::snake;

    #[test]
    fn a_type_name_in_snake_case() {
        for (name, expected) in [
            ("Range", "range"),
            ("SummarizedExperiment", "summarized_experiment"),
            ("GLMNETModel", "glmnet_model"),
            ("GRangesList", "g_ranges_list"),
            ("Wide20", "wide20"),
            ("Wide20Row", "wide20_row"),
            ("Point_too", "point_too"),
        ] {
            assert_eq!(snake(name), expected);
        }
    }
}
