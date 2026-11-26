//! `#[r8::methods]`: a Rust trait as the Rust side of an R8 trait.
//! spec: R8/class-system#rust-impls-and-rust-stored-types-rtemis-r8-api

use proc_macro2::{Group, Ident, Span, TokenStream as Tokens, TokenTree};
use quote::{ToTokens, format_ident, quote, quote_spanned};
use syn::ext::IdentExt;
use syn::spanned::Spanned;
use syn::{Error, FnArg, Pat, Result, Token, TraitItem, Type};

use crate::{
    c_str, constraint, default_tree, description, field_name, valid_name, valid_qualified,
};

/// The arguments of `#[r8::methods]`.
pub struct Args {
    /// The R8 trait, `trait = "ns::Trait"`.
    name: syn::LitStr,
    /// The dispatched arguments, `dispatch(x, y)`.
    dispatch: Option<Vec<Ident>>,
    /// `dyn`: the trait object names the R8 trait.
    object: Option<Span>,
    /// The tag member, `tag = "type"`.
    tag: Option<syn::LitStr>,
}

pub fn args(attr: Tokens) -> Result<Args> {
    let mut name: Option<syn::LitStr> = None;
    let mut dispatch: Option<Vec<Ident>> = None;
    let mut object: Option<Span> = None;
    let mut tag: Option<syn::LitStr> = None;
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
        } else if meta.path.is_ident("dyn") {
            object = Some(meta.path.span());
            Ok(())
        } else if meta.path.is_ident("tag") {
            let s: syn::LitStr = meta.value()?.parse()?;
            if !valid_name(&s.value()) {
                return Err(Error::new_spanned(
                    &s,
                    "a tag member matches [A-Za-z][A-Za-z0-9_]*",
                ));
            }
            tag = Some(s);
            Ok(())
        } else {
            Err(meta
                .error("expected trait = \"ns::Trait\", dispatch(x, y), dyn, and tag = \"type\""))
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
    Ok(Args {
        name,
        dispatch,
        object,
        tag,
    })
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
/// [`ImplSet`] of the type that implements it, and the constant of the trait's
/// name, its [`TraitDecl`]. The functions that R8 calls are generic over the
/// implementing type, so each row of `r8::exports!` instantiates them for its
/// type, the type of a blanket impl included.
///
/// [`ImplSet`]: https://docs.rs/rtemis-r8-api
/// [`TraitDecl`]: https://docs.rs/rtemis-r8-api
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
        receiver: args
            .dispatch
            .as_ref()
            .and_then(|d| d.first())
            .map(field_name),
    };
    let mut methods = Vec::new();
    let mut fields = Vec::new();
    for it in &item.items {
        let TraitItem::Fn(f) = it else { continue };
        let attrs = method_attrs(f)?;
        if attrs.field {
            fields.push(accessor(f)?);
        } else {
            methods.push(method(&of, f, &attrs)?);
        }
    }
    if methods.is_empty() {
        return Err(Error::new_spanned(
            &item.ident,
            "a trait of `#[r8::methods]` has one method or more",
        ));
    }
    let decl = declaration(args, &item, &methods, &fields);
    let params: Vec<Ident> = positions.iter().map(|(p, _)| p.ident.clone()).collect();
    let fns = methods.iter().map(|m| &m.function);
    let rows = methods.iter().map(|m| &m.row);
    let pins = methods.iter().map(|m| &m.pins);
    let bounds: Vec<_> = methods.iter().flat_map(|m| &m.predicates).collect();
    let tag = option(args.tag.as_ref());
    let provided: TraitItem = syn::parse_quote! {
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
                tag: #tag,
                required_fields: ::std::vec![#(#fields),*],
                register: |__r8_set| {
                    <Self as #api::Position>::register(__r8_set);
                    #(<#params as #api::Position>::register(__r8_set);)*
                    #(#pins)*
                },
            }
        }
    };
    let object = match args.object {
        Some(span) if !positions.is_empty() => {
            return Err(Error::new(
                span,
                "`dyn` marks a trait that dispatches on one argument: no single value implements one of several",
            ));
        }
        Some(span) => {
            let (ident, name) = (&item.ident, &args.name);
            quote_spanned! { span =>
                impl #api::rtemis_r8::derive::TraitObject for dyn #ident {
                    const NAME: &'static str = #name;
                }
            }
        }
        None => quote! {},
    };
    item.items.push(provided);
    strip(&mut item);
    Ok(quote! { #item #decl #object })
}

/// An optional value as tokens.
fn option<T: ToTokens>(x: Option<T>) -> Tokens {
    match x {
        Some(x) => quote! { ::std::option::Option::Some(#x) },
        None => quote! { ::std::option::Option::None },
    }
}

/// Removes the attributes of the macro from the trait: `r8` of its methods and
/// their parameters, and the parameters' doc comments, which Rust does not
/// admit.
fn strip(item: &mut syn::ItemTrait) {
    for it in &mut item.items {
        let TraitItem::Fn(f) = it else { continue };
        f.attrs.retain(|a| !a.path().is_ident("r8"));
        for a in &mut f.sig.inputs {
            if let FnArg::Typed(p) = a {
                p.attrs
                    .retain(|a| !a.path().is_ident("r8") && !a.path().is_ident("doc"));
            }
        }
    }
}

/// The constant that declares the trait, of the trait's name in the namespace
/// of values, which a path to the trait reaches too.
fn declaration(
    args: &Args,
    item: &syn::ItemTrait,
    methods: &[Method],
    fields: &[Tokens],
) -> Tokens {
    let api = quote! { ::rtemis_r8_api };
    let (ident, vis) = (&item.ident, &item.vis);
    let qualified = args.name.value();
    let base = qualified.split_once("::").map_or("", |(_, b)| b);
    let why = methods.iter().find_map(|m| m.undeclared.clone());
    let unsupported = option(why.as_ref());
    let generic = matches!(methods, [m] if m.identifier == base) && fields.is_empty();
    let desc = description(&item.attrs);
    let tag = option(args.tag.as_ref());
    let (body, register) = if why.is_some() {
        (
            quote! { ::std::unreachable!("the trait is not declared in Rust") },
            quote! {},
        )
    } else {
        let defs = methods.iter().map(|m| &m.def);
        let registers = methods.iter().map(|m| &m.register);
        (
            quote! {
                #api::signature::trait_def(
                    #base,
                    #desc,
                    #tag,
                    #generic,
                    ::std::vec![#(#defs),*],
                    ::std::vec![#(#fields),*],
                )
            },
            quote! { #(#registers)* },
        )
    };
    let cfg: Vec<_> = item
        .attrs
        .iter()
        .filter(|a| a.path().is_ident("cfg"))
        .collect();
    quote! {
        #(#cfg)*
        #[doc(hidden)]
        #[allow(non_upper_case_globals, dead_code)]
        #vis const #ident: #api::TraitDecl = {
            fn __r8_def() -> #api::rtemis_r8::ir::TraitDef {
                #body
            }
            #[allow(unused_variables)]
            fn __r8_register(__r8_set: &mut #api::rtemis_r8::rules::DocumentSet) {
                #register
            }
            #api::TraitDecl {
                name: #qualified,
                unsupported: #unsupported,
                def: __r8_def,
                register: __r8_register,
            }
        };
    }
}

/// The trait that a method belongs to.
struct Trait<'a> {
    ident: &'a Ident,
    /// The predicates of the trait's `where` clause.
    predicates: Vec<syn::WherePredicate>,
    /// The dispatched positions after the first.
    positions: &'a [(syn::TypeParam, String)],
    /// The name of the first argument, when `dispatch(...)` states it.
    receiver: Option<String>,
}

/// What one method adds to the trait.
struct Method {
    /// The function R8 calls, generic over the implementing type.
    function: Tokens,
    /// The statement that adds its row.
    row: Tokens,
    /// The predicates of the method's `where` clause.
    predicates: Vec<syn::WherePredicate>,
    /// Its identifier in the languages other than R.
    identifier: String,
    /// Its declaration.
    def: Tokens,
    /// The statements that add the declarations of the types of its
    /// parameters and its result, for an impl.
    pins: Tokens,
    /// The registration of the types that it names.
    register: Tokens,
    /// Why Rust does not declare it.
    undeclared: Option<String>,
}

/// The attributes of a method, `#[r8(name = "label<-")]` and `#[r8(field)]`.
struct MethodAttrs {
    name: Option<String>,
    field: bool,
}

fn method_attrs(f: &syn::TraitItemFn) -> Result<MethodAttrs> {
    let mut out = MethodAttrs {
        name: None,
        field: false,
    };
    for a in f.attrs.iter().filter(|a| a.path().is_ident("r8")) {
        a.parse_nested_meta(|meta| {
            if meta.path.is_ident("name") {
                let s: syn::LitStr = meta.value()?.parse()?;
                let name = s.value();
                let identifier = match name.strip_suffix("<-") {
                    Some(b) => format!("with_{}", b.replace('.', "_")),
                    None => name.replace('.', "_"),
                };
                if identifier != field_name(&f.sig.ident) {
                    return Err(Error::new_spanned(
                        &s,
                        format!(
                            "the method of `{name}` in R is `{identifier}` in Rust: `f<-` is `with_f`, and a period an underscore"
                        ),
                    ));
                }
                out.name = Some(name);
                Ok(())
            } else if meta.path.is_ident("field") {
                out.field = true;
                Ok(())
            } else {
                Err(meta.error("a method takes `name = \"f<-\"` or `field`"))
            }
        })?;
    }
    if out.field && out.name.is_some() {
        return Err(Error::new_spanned(
            &f.sig.ident,
            "an accessor of a field has the field's name",
        ));
    }
    Ok(out)
}

/// The required field that an accessor, `fn label(&self) -> &String`, gives.
fn accessor(f: &syn::TraitItemFn) -> Result<Tokens> {
    let api = quote! { ::rtemis_r8_api };
    let sig = &f.sig;
    let name = field_name(&sig.ident);
    let ty = match (&sig.output, sig.inputs.len(), sig.inputs.first()) {
        (syn::ReturnType::Type(_, t), 1, Some(FnArg::Receiver(r)))
            if r.reference.is_some() && r.mutability.is_none() =>
        {
            match &**t {
                Type::Reference(r) => &*r.elem,
                t => t,
            }
        }
        _ => {
            return Err(Error::new_spanned(
                sig,
                "an accessor of a field is `fn name(&self) -> &T`",
            ));
        }
    };
    if !valid_name(&name) || names_self(ty.to_token_stream()) {
        return Err(Error::new_spanned(
            sig,
            "an accessor of a field has the field's name, and gives its type, which is not `Self`",
        ));
    }
    let desc = description(&f.attrs);
    Ok(quote! { #api::signature::required::<#ty>(#name, #desc) })
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

/// `&T` of a type `Option<&T>`.
pub(crate) fn optional_reference(ty: &Type) -> Option<&syn::TypeReference> {
    let Type::Path(p) = ty else { return None };
    let last = p.path.segments.last().filter(|s| s.ident == "Option")?;
    let syn::PathArguments::AngleBracketed(a) = &last.arguments else {
        return None;
    };
    match (a.args.len(), a.args.first()) {
        (1, Some(syn::GenericArgument::Type(Type::Reference(r)))) => Some(r),
        _ => None,
    }
}

/// The type `X` of a parameter `&X` that a call borrows from its R value: a
/// slice, `&[T]`, or a string, `&str`.
pub(crate) fn borrowed(ty: &Type) -> Option<&Type> {
    let Type::Reference(r) = ty else { return None };
    if r.mutability.is_some() {
        return None;
    }
    match &*r.elem {
        Type::Slice(_) => Some(&r.elem),
        Type::Path(p) if p.qself.is_none() && p.path.is_ident("str") => Some(&r.elem),
        _ => None,
    }
}

/// The type `X` of a parameter `Option<&X>` of a borrowed `&X`.
pub(crate) fn optional_borrowed(ty: &Type) -> Option<&Type> {
    let r = optional_reference(ty).filter(|r| r.mutability.is_none())?;
    match &*r.elem {
        Type::Slice(_) => Some(&r.elem),
        Type::Path(p) if p.qself.is_none() && p.path.is_ident("str") => Some(&r.elem),
        _ => None,
    }
}

/// The conversion of a borrowed parameter of type `ty` from the argument
/// `__r8_args[index]` into the holder `hold`, the expression that lends it, and
/// its R8 type; `None` for a parameter of another type.
pub(crate) fn borrowed_param(
    ty: &Type,
    hold: &Ident,
    index: usize,
    name: &str,
) -> Option<(Tokens, Tokens, Tokens)> {
    let api = quote! { ::rtemis_r8_api };
    if let Some(x) = borrowed(ty) {
        return Some((
            quote! {
                let #hold = unsafe { <#x as #api::Borrowed>::hold(__r8_args[#index], #name) }?;
            },
            quote! { <#x as #api::Borrowed>::lend(&#hold) },
            quote! { <#x as #api::Borrowed>::param_type() },
        ));
    }
    let x = optional_borrowed(ty)?;
    Some((
        quote! {
            let #hold = unsafe { #api::borrowed::optional_hold::<#x>(__r8_args[#index], #name) }?;
        },
        quote! { #hold.as_ref().map(|h| <#x as #api::Borrowed>::lend(h)) },
        quote! { #api::borrowed::optional_type::<#x>() },
    ))
}

/// The R name of a parameter: `#[r8(name = "...")]`, or its identifier.
pub(crate) fn param_name(p: &syn::PatType, ident: &Ident) -> Result<String> {
    let mut name = None;
    for a in p.attrs.iter().filter(|a| a.path().is_ident("r8")) {
        a.parse_nested_meta(|meta| {
            if meta.path.is_ident("name") {
                let s: syn::LitStr = meta.value()?.parse()?;
                if s.value().is_empty() {
                    return Err(Error::new_spanned(&s, "a parameter's name is not empty"));
                }
                name = Some(s.value());
            } else if meta.input.peek(Token![=]) {
                let _: syn::Expr = meta.value()?.parse()?;
            }
            Ok(())
        })?;
    }
    Ok(name.unwrap_or_else(|| field_name(ident)))
}

/// Whether `tokens` name `Self`.
fn names_self(tokens: Tokens) -> bool {
    names(tokens, "Self")
}

fn names(tokens: Tokens, what: &str) -> bool {
    tokens.into_iter().any(|t| match t {
        TokenTree::Ident(i) => i == what,
        TokenTree::Group(g) => names(g.stream(), what),
        _ => false,
    })
}

/// Whether `ty` is `Self`, or `Result<Self>` with any error.
fn is_self(ty: &Type) -> bool {
    let Type::Path(p) = ty else { return false };
    if p.path.is_ident("Self") {
        return true;
    }
    let Some(last) = p.path.segments.last().filter(|s| s.ident == "Result") else {
        return false;
    };
    let syn::PathArguments::AngleBracketed(a) = &last.arguments else {
        return false;
    };
    matches!(a.args.first(), Some(syn::GenericArgument::Type(Type::Path(t))) if t.path.is_ident("Self"))
}

/// `tokens` with each lifetime but `'static` as `'_`, for a type that is named
/// outside the function whose lifetimes it has.
pub(crate) fn unbound(tokens: Tokens) -> Tokens {
    let mut quoted = false;
    tokens
        .into_iter()
        .map(|t| {
            let lifetime = std::mem::replace(
                &mut quoted,
                matches!(&t, TokenTree::Punct(p) if p.as_char() == '\''),
            );
            match t {
                TokenTree::Ident(i) if lifetime && i != "static" => {
                    TokenTree::Ident(Ident::new("_", i.span()))
                }
                TokenTree::Group(g) => {
                    let mut of = Group::new(g.delimiter(), unbound(g.stream()));
                    of.set_span(g.span());
                    TokenTree::Group(of)
                }
                t => t,
            }
        })
        .collect()
}

/// The construction of the `Facts` of a parameter: its constraints, its
/// default, and its description.
pub(crate) fn facts(p: &syn::PatType) -> Result<(Tokens, bool)> {
    let api = quote! { ::rtemis_r8_api };
    let mut constraints = Vec::new();
    let mut default = quote! { ::std::option::Option::None };
    let mut stated = false;
    for a in p.attrs.iter().filter(|a| a.path().is_ident("r8")) {
        a.parse_nested_meta(|meta| {
            let key = meta
                .path
                .get_ident()
                .map(|i| i.to_string())
                .unwrap_or_default();
            // The name is read by `param_name()`.
            if key == "name" {
                let _: syn::LitStr = meta.value()?.parse()?;
                return Ok(());
            }
            stated = true;
            if constraint(&meta, &key, &mut constraints)? {
                return Ok(());
            }
            if key != "default" {
                return Err(meta.error("a parameter takes a constraint, `default`, or `name`"));
            }
            let s: syn::LitStr = meta.value()?.parse()?;
            let tree =
                default_tree(&s)?.unwrap_or_else(|| crate::tree_tokens(&serde_json::Value::Null));
            default = quote! { ::std::option::Option::Some(#tree) };
            Ok(())
        })?;
    }
    let desc = description(&p.attrs);
    Ok((
        quote! {
            #api::signature::Facts {
                constraints: #api::rtemis_r8::derive::Constraints {
                    #(#constraints,)*
                    ..::std::default::Default::default()
                },
                default: #default,
                description: #desc,
            }
        },
        stated,
    ))
}

fn method(of: &Trait, f: &syn::TraitItemFn, attrs: &MethodAttrs) -> Result<Method> {
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
    let sign = quote! { #api::signature };
    let implementor = format_ident!("__R8T");
    let marked = !of.positions.is_empty();
    let mut convert = Vec::new();
    let mut args = Vec::new();
    let mut params = Vec::new();
    let mut register = Vec::new();
    let mut undeclared = None;
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
        let name = param_name(p, &name.ident)?;
        let arg = format_ident!("__r8_arg_{}", k);
        let index = k + 1;
        let (facts, stated) = facts(p)?;
        let mut named_type = p.ty.to_token_stream();
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
                if stated {
                    return Err(Error::new_spanned(
                        p,
                        "a dispatched parameter takes no constraint and no default",
                    ));
                }
                dispatched += 1;
                let y = &param.ident;
                convert.push(quote! {
                    let #arg = unsafe { <#y as #api::Position>::from_dispatched(__r8_args[#index], #name) }?;
                });
                args.push(quote! { &*#arg });
                let desc = description(&p.attrs);
                params.push(quote! { #sign::dispatched(#name, true, #desc) });
                named_type = quote! {};
            }
            // `&[T]`, `&str`, and an `Option` of one: borrowed from the R
            // value for the call.
            ty if borrowed(ty).is_some() || optional_borrowed(ty).is_some() => {
                let (hold, lend, param_type) =
                    borrowed_param(ty, &arg, index, &name).expect("a borrowed parameter");
                convert.push(hold);
                args.push(lend);
                params.push(quote! { #sign::param(#name, #param_type, #facts) });
            }
            // `&T` and `Option<&T>`: a value of a Rust-stored type, borrowed
            // from its state, or built from its fields.
            ty if matches!(ty, Type::Reference(_)) || optional_reference(ty).is_some() => {
                let (r, optional) = match ty {
                    Type::Reference(r) => (r, false),
                    ty => (optional_reference(ty).expect("a reference"), true),
                };
                if r.mutability.is_some() {
                    return Err(Error::new_spanned(
                        &p.ty,
                        "a reference parameter is `&T` of a Rust-stored type `T`, `&[T]`, or `&str`",
                    ));
                }
                let elem = &r.elem;
                let of_t = of_implementor(elem.to_token_stream(), &implementor);
                let field = quote! { <#elem as #api::rtemis_r8::derive::Encode>::field_type() };
                if optional {
                    convert.push(quote! {
                        let #arg = unsafe { #api::call::optional_arg::<#of_t>(__r8_args[#index], #name) }?;
                    });
                    args.push(quote! { #arg.as_deref() });
                    params.push(quote! {
                        #sign::param(
                            #name,
                            #api::ParamType::Typed(#api::rtemis_r8::ir::FieldType::Option {
                                inner: ::std::boxed::Box::new(#field),
                            }),
                            #facts,
                        )
                    });
                } else {
                    convert.push(quote! {
                        let #arg = unsafe { #api::Receiver::<#of_t>::from_named_arg(__r8_args[#index], #name) }?;
                    });
                    args.push(quote! { &*#arg });
                    params.push(quote! {
                        #sign::param(#name, #api::ParamType::Typed(#field), #facts)
                    });
                }
                register.push(quote! {
                    <#elem as #api::rtemis_r8::derive::Encode>::register(__r8_set);
                });
            }
            Type::ImplTrait(_) => {
                return Err(Error::new_spanned(
                    &p.ty,
                    "an R8 method's parameter has one type, which `impl Trait` does not name",
                ));
            }
            ty => {
                let of_t = of_implementor(ty.to_token_stream(), &implementor);
                convert.push(quote! {
                    let #arg = unsafe { <#of_t as #api::FromR>::from_arg(__r8_args[#index], #name) }?;
                });
                args.push(quote! { #arg });
                let ty = unbound(ty.to_token_stream());
                params.push(quote! {
                    #sign::param(#name, <#ty as #api::FromR>::param_type(), #facts)
                });
                register.push(quote! { <#ty as #api::FromR>::register(__r8_set); });
            }
        }
        if names_self(named_type.clone())
            || of
                .positions
                .iter()
                .any(|(t, _)| names(named_type.clone(), &t.ident.to_string()))
        {
            undeclared = Some(format!(
                "a trait declared in Rust has parameters of one type each; `{name}` has a type of the implementing type"
            ));
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
    let generic_params: Vec<&Ident> = of.positions.iter().map(|(p, _)| &p.ident).collect();
    let generics = of.positions.iter().map(|(p, _)| {
        let bounds = of_implementor(p.bounds.to_token_stream(), &implementor);
        let param = &p.ident;
        if p.bounds.is_empty() {
            quote! { #param: #api::Position }
        } else {
            quote! { #param: #api::Position + #bounds }
        }
    });
    let (output, returned) = match &sig.output {
        syn::ReturnType::Default => (ident.span(), None),
        syn::ReturnType::Type(_, t) => (t.span(), Some(&**t)),
    };
    let result = quote_spanned! { output =>
        #api::IntoR::into_r(<#implementor as #trait_ident<#(#generic_params),*>>::#ident(&*__r8_x #(, #args)*))
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
    let pins = {
        let of_result = returned.map(|t| {
            let t = unbound(t.to_token_stream());
            quote! { <#t as #api::IntoR>::register(__r8_set); }
        });
        quote! {
            #(#cfg)*
            {
                #(#register)*
                #of_result
            }
        }
    };
    let function = quote! {
        #(#cfg)*
        unsafe extern "C" fn #ident<
            #(#generics,)*
            #implementor: #trait_ident<#(#generic_params),*> + #api::RustStored
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
    let identifier = field_name(ident);
    let r_name = attrs.name.clone().unwrap_or_else(|| identifier.clone());
    let name_c = c_str(&r_name);
    let stated = of.receiver.clone().unwrap_or_default();
    let result_type = returned.map(|t| unbound(t.to_token_stream()));
    let signed = result_type.as_ref().map_or_else(
        || quote! { ::std::option::Option::None },
        |t| quote! { #sign::returns(<#t as #api::IntoR>::return_type()) },
    );
    let row = quote! {
        #(#cfg)*
        __r8_rows.push(#api::MethodRow {
            name: #name_c,
            call: #ident::<#(#generic_params,)* Self>,
            signature: #sign::method(
                #r_name,
                "",
                ::std::vec![#sign::dispatched(#stated, #marked, "") #(, #params)*],
                #signed,
            ),
        });
    };
    if !cfg.is_empty() {
        undeclared = Some(format!(
            "a trait declared in Rust has the same methods in every build; `{r_name}` has a `cfg` attribute"
        ));
    }
    let replacement = r_name.ends_with("<-");
    let returns_self = returned.is_some_and(is_self);
    let declared = match (&result_type, returned) {
        _ if replacement => {
            if !returns_self {
                undeclared = Some(format!(
                    "the replacement method `{r_name}` returns `Self` or a `Result` of it"
                ));
            }
            quote! { ::std::option::Option::None }
        }
        (Some(_), _) if returns_self => quote! { #sign::returns_self() },
        (Some(t), _) => {
            if names_self(t.clone())
                || of
                    .positions
                    .iter()
                    .any(|(p, _)| names(t.clone(), &p.ident.to_string()))
            {
                undeclared = Some(format!(
                    "a trait declared in Rust returns one type, or `Self`; `{r_name}` returns a type of the implementing type"
                ));
            }
            register.push(quote! { <#t as #api::IntoR>::register(__r8_set); });
            quote! { #sign::returns(<#t as #api::IntoR>::return_type()) }
        }
        (None, _) => quote! { ::std::option::Option::None },
    };
    let first = of.receiver.clone().unwrap_or_else(|| "x".to_string());
    let desc = description(&f.attrs);
    let def = quote! {
        #sign::method(
            #r_name,
            #desc,
            ::std::vec![#sign::dispatched(#first, #marked, "") #(, #params)*],
            #declared,
        )
    };
    Ok(Method {
        function,
        row,
        predicates,
        identifier,
        def,
        pins,
        register: quote! { #(#register)* },
        undeclared,
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
