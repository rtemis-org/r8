//! `#[r8::methods]`: a Rust trait as the Rust side of an R8 trait.
//! spec: R8/class-system#rust-impls-and-rust-stored-types-rtemis-r8-api

use proc_macro2::{Group, Ident, Span, TokenStream as Tokens, TokenTree};
use quote::{ToTokens, format_ident, quote, quote_spanned};
use syn::ext::IdentExt;
use syn::spanned::Spanned;
use syn::{Error, FnArg, Pat, Result, Token, TraitItem, Type};

use crate::{c_str, field_name, valid_qualified};

/// The arguments of `#[r8::methods]`.
pub struct Args {
    /// The R8 trait, `trait = "ns::Trait"`.
    name: syn::LitStr,
    /// The dispatched arguments, `dispatch(x, y)`.
    dispatch: Option<Vec<Ident>>,
}

pub fn args(attr: Tokens) -> Result<Args> {
    let mut name: Option<syn::LitStr> = None;
    let mut dispatch: Option<Vec<Ident>> = None;
    let parser = syn::meta::parser(|meta| {
        if meta.path.is_ident("trait") {
            name = Some(meta.value()?.parse()?);
            Ok(())
        } else if meta.path.is_ident("dispatch") {
            let content;
            syn::parenthesized!(content in meta.input);
            let names = content.parse_terminated(Ident::parse_any, Token![,])?;
            dispatch = Some(names.into_iter().collect());
            Ok(())
        } else {
            Err(meta.error("expected trait = \"ns::Trait\" and dispatch(x, y)"))
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
    Ok(Args { name, dispatch })
}

/// The dispatched positions of a trait after the first: each type parameter
/// with the name of the argument that it types.
fn positions(args: &Args, generics: &syn::Generics) -> Result<Vec<(syn::TypeParam, String)>> {
    let mut params = Vec::new();
    for p in &generics.params {
        match p {
            syn::GenericParam::Type(t) => params.push(t.clone()),
            other => {
                return Err(Error::new_spanned(
                    other,
                    "a trait of `#[r8::methods]` has type parameters only, one per dispatched position after the first",
                ));
            }
        }
    }
    let names = args.dispatch.as_deref().unwrap_or_default();
    if params.is_empty() && names.len() < 2 {
        return Ok(Vec::new());
    }
    if names.len() != params.len() + 1 {
        let at = match names.first() {
            Some(n) => n.span(),
            None => generics.span(),
        };
        return Err(Error::new(
            at,
            "`dispatch(...)` names the first argument, and one argument per type parameter of the trait",
        ));
    }
    for p in &mut params {
        // The default of a parameter is the trait's, not a function's.
        p.eq_token = None;
        p.default = None;
    }
    Ok(params
        .into_iter()
        .zip(names[1..].iter().map(field_name))
        .collect())
}

/// The trait with the hidden function `__r8_methods()`, which gives the
/// [`ImplSet`] of the type that implements it. The functions that R8 calls
/// are generic over that type, so each row of `r8::exports!` instantiates
/// them for its type, the type of a blanket impl included.
///
/// [`ImplSet`]: https://docs.rs/rtemis-r8-api
///
/// A type parameter of the trait is a dispatched position after the first,
/// `fn overlaps(&self, y: &Y)` of `trait Overlaps<Y>`, and the functions are
/// generic over it too.
pub fn expand(args: &Args, item: syn::Item) -> Result<Tokens> {
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
    let positions = positions(args, &item.generics)?;
    let api = quote! { ::rtemis_r8_api };
    let trait_c = c_str(&args.name.value());
    let of = Trait {
        ident: &item.ident,
        predicates: item
            .generics
            .where_clause
            .iter()
            .flat_map(|w| w.predicates.iter().cloned())
            .collect(),
        positions: &positions,
    };
    let mut fns = Vec::new();
    let mut rows = Vec::new();
    let mut bounds = Vec::new();
    for it in &item.items {
        let TraitItem::Fn(f) = it else { continue };
        let Method {
            function,
            row,
            predicates,
        } = method(&of, f)?;
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
    let params: Vec<Ident> = positions.iter().map(|(p, _)| p.ident.clone()).collect();
    item.items.push(syn::parse_quote! {
        #[doc(hidden)]
        fn __r8_methods() -> #api::ImplSet
        where
            Self: #api::RustStored,
            #(#params: #api::Position,)*
            #(#bounds,)*
        {
            #(#fns)*
            let mut __r8_rows = ::std::vec::Vec::new();
            #(#rows)*
            #api::ImplSet {
                trait_name: #trait_c,
                positions: ::std::vec![
                    <Self as #api::Position>::ENTRY
                    #(, <#params as #api::Position>::ENTRY)*
                ],
                methods: __r8_rows,
            }
        }
    });
    Ok(item.into_token_stream())
}

/// The trait that a method belongs to.
struct Trait<'a> {
    ident: &'a Ident,
    /// The predicates of the trait's `where` clause.
    predicates: Vec<syn::WherePredicate>,
    /// The dispatched positions after the first.
    positions: &'a [(syn::TypeParam, String)],
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

/// The position of a parameter of type `&Y`, where `Y` is a type parameter of
/// the trait.
fn position_of(elem: &Type, of: &Trait) -> Option<usize> {
    let Type::Path(p) = elem else { return None };
    let ident = p.path.get_ident()?;
    of.positions
        .iter()
        .position(|(t, _)| p.qself.is_none() && t.ident == *ident)
}

fn method(of: &Trait, f: &syn::TraitItemFn) -> Result<Method> {
    let trait_ident = of.ident;
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
    let mut dispatched = 0;
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
            // `&Y`: the argument of a dispatched position.
            Type::Reference(r) if position_of(&r.elem, of).is_some() => {
                let at = position_of(&r.elem, of).expect("a position");
                let (param, expected) = &of.positions[at];
                if at != dispatched || name != *expected || r.mutability.is_some() {
                    return Err(Error::new_spanned(
                        p,
                        format!(
                            "the arguments of `dispatch(...)` after the first are `&` of the trait's type parameters, in order: `{expected}: &{}`",
                            param.ident
                        ),
                    ));
                }
                dispatched += 1;
                let y = &param.ident;
                convert.push(quote! {
                    let #arg = unsafe { <#y as #api::Position>::from_dispatched(__r8_args[#index], #name) }?;
                });
                args.push(quote! { &*#arg });
            }
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
    if let Some((param, name)) = of.positions.get(dispatched) {
        return Err(Error::new_spanned(
            sig,
            format!(
                "the method takes `{name}: &{}`, which `dispatch(...)` names",
                param.ident
            ),
        ));
    }
    let n_params = args.len() + 1;
    let ident = &sig.ident;
    let params: Vec<&Ident> = of.positions.iter().map(|(p, _)| &p.ident).collect();
    let generics = of.positions.iter().map(|(p, _)| {
        let bounds = of_implementor(p.bounds.to_token_stream(), &implementor);
        let param = &p.ident;
        if p.bounds.is_empty() {
            quote! { #param: #api::Position }
        } else {
            quote! { #param: #api::Position + #bounds }
        }
    });
    let output = match &sig.output {
        syn::ReturnType::Default => ident.span(),
        syn::ReturnType::Type(_, t) => t.span(),
    };
    let result = quote_spanned! { output =>
        #api::IntoR::into_r(<#implementor as #trait_ident<#(#params),*>>::#ident(&*__r8_x #(, #args)*))
    };
    let predicates: Vec<syn::WherePredicate> = sig
        .generics
        .where_clause
        .iter()
        .flat_map(|w| w.predicates.iter().cloned())
        .collect();
    let bounds = predicates
        .iter()
        .chain(&of.predicates)
        .map(|p| of_implementor(p.to_token_stream(), &implementor));
    let cfg: Vec<_> = f
        .attrs
        .iter()
        .filter(|a| a.path().is_ident("cfg"))
        .collect();
    let function = quote! {
        #(#cfg)*
        unsafe extern "C" fn #ident<
            #(#generics,)*
            #implementor: #trait_ident<#(#params),*> + #api::RustStored
        >(
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
            call: #ident::<#(#params,)* Self>,
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
