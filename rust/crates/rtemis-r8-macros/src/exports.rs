//! `r8::exports!`: the exports table of a package's Rust code.
//! spec: R8/class-system#rust-impls-and-rust-stored-types-rtemis-r8-api

use proc_macro2::{Span, TokenStream as Tokens};
use quote::{format_ident, quote, quote_spanned};
use syn::parse::{Parse, ParseStream};
use syn::spanned::Spanned;
use syn::{Error, Result, Token};

use crate::{c_str, valid_name};

/// The arguments of `r8::exports!`.
pub struct Args {
    package: syn::LitStr,
    namespace: syn::LitStr,
    types: Vec<syn::Path>,
    traits: Vec<syn::Path>,
    impls: Vec<Row>,
}

/// A row of `impls`, `Type: Trait` or `Type: Trait<Y>`.
struct Row {
    ty: syn::Path,
    of: syn::Path,
}

impl Parse for Row {
    fn parse(input: ParseStream) -> Result<Self> {
        let ty = input.call(syn::Path::parse_mod_style)?;
        input.parse::<Token![:]>()?;
        Ok(Row {
            ty,
            of: input.parse()?,
        })
    }
}

fn bracketed<T: Parse>(input: ParseStream) -> Result<Vec<T>> {
    let content;
    syn::bracketed!(content in input);
    Ok(content
        .parse_terminated(T::parse, Token![,])?
        .into_iter()
        .collect())
}

impl Parse for Args {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut package = None;
        let mut namespace = None;
        let mut types = Vec::new();
        let mut traits = Vec::new();
        let mut impls = Vec::new();
        while !input.is_empty() {
            let key: syn::Ident = input.parse()?;
            input.parse::<Token![=]>()?;
            match key.to_string().as_str() {
                "package" => package = Some(input.parse()?),
                "namespace" => namespace = Some(input.parse()?),
                "types" => types = bracketed(input)?,
                "traits" => traits = bracketed(input)?,
                "impls" => impls = bracketed(input)?,
                _ => {
                    return Err(Error::new_spanned(
                        key,
                        "expected package, namespace, types, traits, or impls",
                    ));
                }
            }
            if input.is_empty() {
                break;
            }
            input.parse::<Token![,]>()?;
        }
        Ok(Args {
            package: package.ok_or_else(|| input.error("`package = \"...\"` is required"))?,
            namespace: namespace.ok_or_else(|| input.error("`namespace = \"...\"` is required"))?,
            types,
            traits,
            impls,
        })
    }
}

/// Whether `s` is the name of an R package: ASCII letters, digits, and
/// periods, from a letter to a letter or digit.
fn valid_package(s: &str) -> bool {
    s.len() > 1
        && s.starts_with(|c: char| c.is_ascii_alphabetic())
        && s.ends_with(|c: char| c.is_ascii_alphanumeric())
        && s.chars().all(|c| c.is_ascii_alphanumeric() || c == '.')
}

/// The name of the function that registers the exports of `package`, with
/// the underscore for a period that R uses in `R_init_<package>`.
fn init_name(package: &str) -> String {
    format!("r8_init_{}", package.replace('.', "_"))
}

pub fn expand(a: &Args) -> Result<Tokens> {
    let package = a.package.value();
    if !valid_package(&package) {
        return Err(Error::new_spanned(
            &a.package,
            "the name of an R package holds letters, digits, and periods, and begins with a letter",
        ));
    }
    if !valid_name(&a.namespace.value()) {
        return Err(Error::new_spanned(
            &a.namespace,
            "a namespace matches [A-Za-z][A-Za-z0-9_]*",
        ));
    }
    let api = quote! { ::rtemis_r8_api };
    let package_c = c_str(&package);
    let init = format_ident!("{}", init_name(&package), span = Span::call_site());
    let namespace = &a.namespace;
    let types = &a.types;
    let n_types = types.len();
    let sets = a.impls.iter().map(|Row { ty, of }| {
        quote_spanned! { of.span() => <#ty as #of>::__r8_methods }
    });
    let n_impls = a.impls.len();
    let traits = &a.traits;
    // The constant of a trait of `#[r8::methods]` has the trait's path.
    let checks = traits.iter().map(|t| {
        quote_spanned! { t.span() =>
            const _: () = {
                if let ::std::option::Option::Some(why) = #t.unsupported {
                    panic!("{}", why);
                }
                if !#t.is_of(#namespace) {
                    panic!("a trait declared in Rust is of the namespace of its package");
                }
            };
        }
    });
    Ok(quote! {
        #(#checks)*
        static __R8_CODECS: #api::Shared<[#api::rtemis_r8::abi::Codec; #n_types]> =
            #api::Shared([#(<#types as #api::RustStored>::TABLE.0),*]);
        static __R8_IMPLS: [fn() -> #api::ImplSet; #n_impls] = [#(#sets),*];
        static __R8_TABLE: ::std::sync::OnceLock<usize> = ::std::sync::OnceLock::new();

        unsafe extern "C" fn __r8_exports() -> *const #api::rtemis_r8::abi::Exports {
            #api::exports::table(
                &__R8_TABLE,
                #package_c,
                #namespace,
                &__R8_CODECS.0,
                // The listed types and every type that they and the listed
                // traits reach, such as the struct of a newtype variant.
                |_set| {
                    #(<#types as #api::rtemis_r8::derive::Encode>::register(_set);)*
                    #((#traits.register)(_set);)*
                },
                &[#(#traits.def),*],
                &__R8_IMPLS,
            )
        }

        /// Registers the package's R8 exports; the package's
        /// initialization routine calls it.
        ///
        /// # Safety
        ///
        /// Called once, from the package's initialization routine.
        #[allow(non_snake_case)]
        #[unsafe(no_mangle)]
        pub unsafe extern "C" fn #init(_dll: *mut ::core::ffi::c_void) {
            unsafe { #api::exports::register(#package_c, __r8_exports) }
        }
    })
}

#[cfg(test)]
mod tests {
    use super::{init_name, valid_package};

    #[test]
    fn the_entry_point_of_a_package() {
        assert_eq!(init_name("r8pkgR"), "r8_init_r8pkgR");
        assert_eq!(init_name("rtemis.core"), "r8_init_rtemis_core");
    }

    #[test]
    fn the_name_of_a_package() {
        for name in ["r8pkgR", "rtemis.core", "S7"] {
            assert!(valid_package(name), "{name}");
        }
        for name in ["", "a", "8r", "a.", ".a", "a_b", "a-b", "é1"] {
            assert!(!valid_package(name), "{name}");
        }
    }
}
