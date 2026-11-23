//! `#[r8::methods]`: a Rust trait as the Rust side of an R8 trait.
//! spec: R8/class-system#rust-impls-and-rust-stored-types-rtemis-r8-api

use proc_macro2::{Group, Ident, Span, TokenStream as Tokens, TokenTree};
use quote::{ToTokens, format_ident, quote, quote_spanned};
use syn::spanned::Spanned;
use syn::{Error, FnArg, Pat, Result, TraitItem, Type};

use crate::{c_str, field_name, valid_qualified};

/// The R8 trait that `#[r8::methods(trait = "ns::Trait")]` names.
pub fn trait_name(attr: Tokens) -> Result<syn::LitStr> {
    let mut name: Option<syn::LitStr> = None;
    let parser = syn::meta::parser(|meta| {
        if meta.path.is_ident("trait") {
            name = Some(meta.value()?.parse()?);
            Ok(())
        } else {
            Err(meta.error("expected trait = \"ns::Trait\""))
        }
    });
    syn::parse::Parser::parse2(parser, attr)?;
    let name = name.ok_or_else(|| {
        Error::new(
            Span::call_site(),
            "#[r8::methods(trait = \"ns::Trait\")] names the R8 trait",
        )
    })?;
    if !valid_qualified(&name.value()) {
        return Err(Error::new_spanned(
            &name,
            "a qualified trait name is namespace::Trait",
        ));
    }
    Ok(name)
}

/// The trait with the hidden function `__r8_methods()`, which gives the
/// [`ImplSet`] of the type that implements it. The functions that R8 calls
/// are generic over that type, so each row of `r8::exports!` instantiates
/// them for its type, the type of a blanket impl included.
///
/// [`ImplSet`]: https://docs.rs/rtemis-r8-api
pub fn expand(name: &syn::LitStr, item: syn::Item) -> Result<Tokens> {
    let mut item = match item {
        syn::Item::Trait(t) => t,
        syn::Item::Impl(i) => {
            return Err(Error::new_spanned(
                i.impl_token,
                "`#[r8::methods]` marks the Rust trait; an impl takes no attribute, and `r8::exports!` lists it",
            ));
        }
        other => {
            return Err(Error::new_spanned(
                other,
                "`#[r8::methods]` marks a Rust trait",
            ));
        }
    };
    if !item.generics.params.is_empty() {
        return Err(Error::new_spanned(
            &item.generics,
            "a trait of `#[r8::methods]` has no generic parameters",
        ));
    }
    let api = quote! { ::rtemis_r8_api };
    let trait_c = c_str(&name.value());
    let mut fns = Vec::new();
    let mut rows = Vec::new();
    let mut bounds = Vec::new();
    for it in &item.items {
        let TraitItem::Fn(f) = it else { continue };
        let Method {
            function,
            row,
            predicates,
        } = method(&item.ident, f)?;
        fns.push(function);
        rows.push(row);
        bounds.extend(predicates);
    }
    if rows.is_empty() {
        return Err(Error::new_spanned(
            &item.ident,
            "a trait of `#[r8::methods]` has one method or more",
        ));
    }
    item.items.push(syn::parse_quote! {
        #[doc(hidden)]
        fn __r8_methods() -> #api::ImplSet
        where
            Self: #api::RustStored,
            #(#bounds,)*
        {
            #(#fns)*
            let mut __r8_rows = ::std::vec::Vec::new();
            #(#rows)*
            #api::ImplSet {
                trait_name: #trait_c,
                type_name: <Self as #api::RustStored>::TYPE_NAME,
                methods: __r8_rows,
            }
        }
    });
    Ok(item.into_token_stream())
}

/// What one method adds to `__r8_methods()`.
struct Method {
    /// The function R8 calls, generic over the implementing type.
    function: Tokens,
    /// The statement that adds its row.
    row: Tokens,
    /// The predicates of the method's `where` clause.
    predicates: Vec<syn::WherePredicate>,
}

fn method(trait_ident: &Ident, f: &syn::TraitItemFn) -> Result<Method> {
    let sig = &f.sig;
    if sig.asyncness.is_some() || sig.unsafety.is_some() || sig.abi.is_some() {
        return Err(Error::new_spanned(
            sig,
            "an R8 method is a function without `async`, `unsafe`, and `extern`",
        ));
    }
    if let Some(p) = sig
        .generics
        .params
        .iter()
        .find(|p| !matches!(p, syn::GenericParam::Lifetime(_)))
    {
        return Err(Error::new_spanned(
            p,
            "an R8 method has no type parameters: R passes each parameter as one type",
        ));
    }
    let mut inputs = sig.inputs.iter();
    match inputs.next() {
        Some(FnArg::Receiver(r)) if r.reference.is_some() && r.mutability.is_none() => {}
        _ => {
            return Err(Error::new_spanned(
                sig,
                "an R8 method takes `&self` first, which R passes as its first argument",
            ));
        }
    }
    let api = quote! { ::rtemis_r8_api };
    let implementor = format_ident!("__R8T");
    let mut convert = Vec::new();
    let mut args = Vec::new();
    for (k, a) in inputs.enumerate() {
        let FnArg::Typed(p) = a else {
            unreachable!("one receiver")
        };
        let Pat::Ident(name) = &*p.pat else {
            return Err(Error::new_spanned(
                &p.pat,
                "an R8 method's parameter is a name",
            ));
        };
        let name = field_name(&name.ident);
        let arg = format_ident!("__r8_arg_{}", k);
        let index = k + 1;
        match &*p.ty {
            // `&T`: a value of a Rust-stored type, borrowed from its state,
            // or built from its fields.
            Type::Reference(r) => {
                let is_str = matches!(&*r.elem, Type::Path(p) if p.path.is_ident("str"));
                if r.mutability.is_some() || is_str {
                    return Err(Error::new_spanned(
                        &p.ty,
                        "a reference parameter is `&T` of a Rust-stored type `T`; a string is `String`",
                    ));
                }
                let of = of_implementor(r.elem.to_token_stream(), &implementor);
                convert.push(quote! {
                    let #arg = unsafe { #api::Receiver::<#of>::from_named_arg(__r8_args[#index], #name) }?;
                });
                args.push(quote! { &*#arg });
            }
            Type::ImplTrait(_) => {
                return Err(Error::new_spanned(
                    &p.ty,
                    "an R8 method's parameter has one type, which `impl Trait` does not name",
                ));
            }
            ty => {
                let ty = of_implementor(ty.to_token_stream(), &implementor);
                convert.push(quote! {
                    let #arg = unsafe { <#ty as #api::FromR>::from_arg(__r8_args[#index], #name) }?;
                });
                args.push(quote! { #arg });
            }
        }
    }
    let n_params = args.len() + 1;
    let ident = &sig.ident;
    let output = match &sig.output {
        syn::ReturnType::Default => ident.span(),
        syn::ReturnType::Type(_, t) => t.span(),
    };
    let result = quote_spanned! { output =>
        #api::IntoR::into_r(<#implementor as #trait_ident>::#ident(&*__r8_x #(, #args)*))
    };
    let predicates: Vec<syn::WherePredicate> = sig
        .generics
        .where_clause
        .iter()
        .flat_map(|w| w.predicates.iter().cloned())
        .collect();
    let bounds = predicates
        .iter()
        .map(|p| of_implementor(p.to_token_stream(), &implementor));
    let cfg: Vec<_> = f
        .attrs
        .iter()
        .filter(|a| a.path().is_ident("cfg"))
        .collect();
    let function = quote! {
        #(#cfg)*
        unsafe extern "C" fn #ident<#implementor: #trait_ident + #api::RustStored>(
            _inputs: *const #api::rtemis_r8::abi::CallInputs,
            args: *const #api::rtemis_r8::abi::Sexp,
            nargs: usize,
            result: *mut #api::rtemis_r8::abi::Sexp,
            err: *mut #api::rtemis_r8::abi::Error,
        ) -> #api::rtemis_r8::abi::Status
        where
            #(#bounds,)*
        {
            unsafe {
                #api::call::method(args, nargs, #n_params, result, err, |__r8_args| {
                    let __r8_x = #api::Receiver::<#implementor>::from_arg(__r8_args[0])?;
                    #(#convert)*
                    #result
                })
            }
        }
    };
    let name_c = c_str(&field_name(ident));
    let row = quote! {
        #(#cfg)*
        __r8_rows.push(#api::MethodRow {
            name: #name_c,
            call: #ident::<Self>,
            n_params: #n_params,
        });
    };
    Ok(Method {
        function,
        row,
        predicates,
    })
}

/// `tokens` with `Self` as the parameter that stands for the implementing
/// type: an item inside a function cannot name the `Self` around it.
fn of_implementor(tokens: Tokens, implementor: &Ident) -> Tokens {
    tokens
        .into_iter()
        .map(|t| match t {
            TokenTree::Ident(i) if i == "Self" => {
                TokenTree::Ident(Ident::new(&implementor.to_string(), i.span()))
            }
            TokenTree::Group(g) => {
                let mut of = Group::new(g.delimiter(), of_implementor(g.stream(), implementor));
                of.set_span(g.span());
                TokenTree::Group(of)
            }
            t => t,
        })
        .collect()
}
