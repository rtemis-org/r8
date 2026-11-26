//! `#[r8::function]`: a Rust function as the body of a typed function that R
//! declares with `new_function(name, function(...) rust())`.
//! spec: R8/class-system#typed-functions

use proc_macro2::{Ident, TokenStream as Tokens};
use quote::{ToTokens, format_ident, quote, quote_spanned};
use syn::spanned::Spanned;
use syn::{Error, FnArg, Pat, Result, Type};

use crate::methods::{borrowed_param, facts, optional_reference, param_name, unbound};
use crate::{c_str, description, field_name, valid_name};

/// The arguments of `#[r8::function]`.
pub struct Args {
    /// The R8 name, `name = "..."`, when it is not the identifier.
    name: Option<syn::LitStr>,
}

/// The keys that a later form of the attribute holds: a function declared in
/// Rust.
const RESERVED: &[&str] = &["declare", "rule", "typed", "description", "returns"];

pub fn args(attr: Tokens) -> Result<Args> {
    let mut name: Option<syn::LitStr> = None;
    let parser = syn::meta::parser(|meta| {
        let key = meta
            .path
            .get_ident()
            .map(|i| i.to_string())
            .unwrap_or_default();
        if key == "name" {
            let s: syn::LitStr = meta.value()?.parse()?;
            if !valid_name(&s.value()) {
                return Err(Error::new_spanned(
                    &s,
                    "an R8 name matches [A-Za-z][A-Za-z0-9_]*",
                ));
            }
            name = Some(s);
            return Ok(());
        }
        if RESERVED.contains(&key.as_str()) {
            return Err(meta.error(format!(
                "`{key}` is reserved for a function declared in Rust; a function with a body in Rust is declared in R, by `new_function(name, function(...) rust())`"
            )));
        }
        Err(meta.error("`#[r8::function]` takes `name = \"...\"`"))
    });
    syn::parse::Parser::parse2(parser, attr)?;
    Ok(Args { name })
}

pub fn expand(args: &Args, mut item: syn::ItemFn) -> Result<Tokens> {
    let sig = &item.sig;
    if sig.asyncness.is_some()
        || sig.unsafety.is_some()
        || sig.abi.is_some()
        || sig.constness.is_some()
        || sig.variadic.is_some()
    {
        return Err(Error::new_spanned(
            sig,
            "the body of an R8 function is a function without `async`, `const`, `unsafe`, and `extern`",
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
            "an R8 function has no type parameters: R passes each parameter as one type",
        ));
    }
    let api = quote! { ::rtemis_r8_api };
    let sign = quote! { #api::signature };
    let ident = sig.ident.clone();
    let r_name = args
        .name
        .as_ref()
        .map_or_else(|| field_name(&ident), syn::LitStr::value);
    if !valid_name(&r_name) {
        return Err(Error::new_spanned(
            &ident,
            format!(
                "`{r_name}` is not an R8 name, [A-Za-z][A-Za-z0-9_]*; `#[r8::function(name = \"...\")]` names the function"
            ),
        ));
    }
    let mut convert = Vec::new();
    let mut call_args = Vec::new();
    let mut params = Vec::new();
    let mut register = Vec::new();
    for (k, a) in sig.inputs.iter().enumerate() {
        let p = match a {
            FnArg::Typed(p) => p,
            FnArg::Receiver(r) => {
                return Err(Error::new_spanned(
                    r,
                    "an R8 function is a free function; a method of a type is an impl of a trait, `#[r8::methods]`",
                ));
            }
        };
        let Pat::Ident(pat) = &*p.pat else {
            return Err(Error::new_spanned(
                &p.pat,
                "an R8 function's parameter is a name",
            ));
        };
        let name = param_name(p, &pat.ident)?;
        let arg = format_ident!("__r8_arg_{}", k);
        let (facts, _) = facts(p)?;
        let ty = &*p.ty;
        if let Some((hold, lend, param_type)) = borrowed_param(ty, &arg, k, &name) {
            convert.push(hold);
            call_args.push(lend);
            params.push(quote! { #sign::param(#name, #param_type, #facts) });
            continue;
        }
        match ty {
            // `&T` and `Option<&T>`: a value of a Rust-stored type, borrowed
            // from its state, or built from its fields.
            ty if matches!(ty, Type::Reference(_)) || optional_reference(ty).is_some() => {
                let (r, optional) = match ty {
                    Type::Reference(r) => (r, false),
                    ty => (optional_reference(ty).expect("a reference"), true),
                };
                if r.mutability.is_some() {
                    return Err(Error::new_spanned(
                        ty,
                        "a reference parameter is `&T` of a Rust-stored type `T`, `&[T]`, or `&str`",
                    ));
                }
                let elem = &r.elem;
                let field = quote! { <#elem as #api::rtemis_r8::derive::Encode>::field_type() };
                if optional {
                    convert.push(quote! {
                        let #arg = unsafe { #api::call::optional_arg::<#elem>(__r8_args[#k], #name) }?;
                    });
                    call_args.push(quote! { #arg.as_deref() });
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
                        let #arg = unsafe { #api::Receiver::<#elem>::from_named_arg(__r8_args[#k], #name) }?;
                    });
                    call_args.push(quote! { &*#arg });
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
                    ty,
                    "an R8 function's parameter has one type, which `impl Trait` does not name",
                ));
            }
            ty => {
                convert.push(quote! {
                    let #arg = unsafe { <#ty as #api::FromR>::from_arg(__r8_args[#k], #name) }?;
                });
                call_args.push(quote! { #arg });
                let ty = unbound(ty.to_token_stream());
                params.push(quote! {
                    #sign::param(#name, <#ty as #api::FromR>::param_type(), #facts)
                });
                register.push(quote! { <#ty as #api::FromR>::register(__r8_set); });
            }
        }
    }
    let n_params = call_args.len();
    let (output, returned) = match &sig.output {
        syn::ReturnType::Default => (ident.span(), None),
        syn::ReturnType::Type(_, t) => (t.span(), Some(unbound(t.to_token_stream()))),
    };
    let returns = returned.as_ref().map_or_else(
        || quote! { ::std::option::Option::None },
        |t| quote! { #sign::returns(<#t as #api::IntoR>::return_type()) },
    );
    if let Some(t) = &returned {
        register.push(quote! { <#t as #api::IntoR>::register(__r8_set); });
    }
    let result = quote_spanned! { output =>
        #api::IntoR::into_r(super::#ident(#(#call_args),*))
    };
    let desc = description(&item.attrs);
    let name_c = c_str(&r_name);
    // The parameters' attributes are read here; Rust admits none on them.
    for a in &mut item.sig.inputs {
        if let FnArg::Typed(p) = a {
            p.attrs
                .retain(|a| !a.path().is_ident("r8") && !a.path().is_ident("doc"));
        }
    }
    let visibility = match &item.vis {
        syn::Visibility::Public(_) => quote! { pub },
        _ => quote! { pub(crate) },
    };
    let module: Ident = ident.clone();
    Ok(quote! {
        #item

        /// The R8 row of the function of the same name, which `r8::exports!`
        /// lists in `functions`.
        #[doc(hidden)]
        #[allow(non_snake_case)]
        #visibility mod #module {
            #[allow(unused_imports)]
            use super::*;

            unsafe extern "C" fn __r8_call(
                _inputs: *const #api::rtemis_r8::abi::CallInputs,
                args: *const #api::rtemis_r8::abi::Sexp,
                nargs: usize,
                result: *mut #api::rtemis_r8::abi::Sexp,
                err: *mut #api::rtemis_r8::abi::Error,
            ) -> #api::rtemis_r8::abi::Status {
                unsafe {
                    #api::call::function(args, nargs, #n_params, result, err, |__r8_args| {
                        #(#convert)*
                        #result
                    })
                }
            }

            fn __r8_register(__r8_set: &mut #api::rtemis_r8::rules::DocumentSet) {
                #(#register)*
            }

            /// The row of the function.
            pub fn __r8_function() -> #api::FunctionRow {
                #api::FunctionRow {
                    name: #name_c,
                    call: __r8_call,
                    declaration: #sign::function(#r_name, #desc, ::std::vec![#(#params),*], #returns),
                    register: __r8_register,
                }
            }
        }
    })
}
