//! The procedural macros of the R8 type system, which `rtemis-r8` re-exports
//! with its `macros` feature.
//!
//! `#[derive(R8)]` declares a Rust struct or enum as an R8 type. The
//! generated code names the runtime crate `::rtemis_r8`, so a crate that
//! uses the macro depends on `rtemis-r8` under that name.
//!
//! The derive implements `rtemis_r8::derive::Encode` and `rtemis_r8::derive::R8`:
//! the type's declaration, in the same IR a declaration in R produces, and
//! its encoding to and from the JSON form that `rtemis-r8` validates. A field's
//! R8 type comes from its Rust type's `Encode` implementation; attributes add
//! what the Rust type does not state.
//!
//! On the type: `#[r8(namespace = "rtemis", version = 1)]` (both required),
//! `tag = "type"` on an enum, and `rule(id = "...", invariant = "<JSONLogic>",
//! when = "<JSONLogic>", message = "...")`, repeated per rule. On a field:
//! `min`, `max`, `exclusive_min`, `exclusive_max`, `pattern`, `finite`,
//! `min_length`, `max_length`, `unique`, `default = "<JSON>"`, `unit`,
//! `sensitive`, `external`, and `resolve = "runtime"`. Doc comments are
//! descriptions.

use proc_macro::TokenStream;
use proc_macro2::{Span, TokenStream as Tokens};
use quote::{format_ident, quote};
use syn::{
    Attribute, Data, DeriveInput, Error, Expr, Fields, Lit, Result, UnOp, parse_macro_input,
};

/// Derives `Encode` and `R8` for a struct with named fields or an enum.
#[proc_macro_derive(R8, attributes(r8))]
pub fn derive_r8(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    expand(&input)
        .unwrap_or_else(Error::into_compile_error)
        .into()
}

/// The doc comments of an item, joined into one description.
fn description(attrs: &[Attribute]) -> String {
    let mut lines = Vec::new();
    for a in attrs.iter().filter(|a| a.path().is_ident("doc")) {
        if let syn::Meta::NameValue(nv) = &a.meta
            && let Expr::Lit(l) = &nv.value
            && let Lit::Str(s) = &l.lit
        {
            lines.push(s.value().trim().to_string());
        }
    }
    lines.join(" ").trim().to_string()
}

fn valid_name(s: &str) -> bool {
    let mut c = s.chars();
    c.next().is_some_and(|f| f.is_ascii_alphabetic())
        && c.all(|x| x.is_ascii_alphanumeric() || x == '_')
}

/// A literal attribute value as JSON text: numbers (a leading `-` allowed),
/// strings, and booleans.
fn json_literal(e: &Expr) -> Result<String> {
    match e {
        Expr::Lit(l) => match &l.lit {
            Lit::Int(i) => Ok(i.base10_digits().to_string()),
            Lit::Float(f) => Ok(f.base10_digits().to_string()),
            Lit::Str(s) => Ok(format!("{:?}", s.value())),
            Lit::Bool(b) => Ok(b.value.to_string()),
            other => Err(Error::new_spanned(
                other,
                "expected a number, a string, or a logical",
            )),
        },
        Expr::Unary(u) if matches!(u.op, UnOp::Neg(_)) => {
            Ok(format!("-{}", json_literal(&u.expr)?))
        }
        other => Err(Error::new_spanned(other, "expected a literal")),
    }
}

#[derive(Default)]
struct TypeAttrs {
    namespace: Option<String>,
    version: Option<u32>,
    tag: Option<String>,
    rules: Vec<Tokens>,
}

/// One `rule(id = ..., invariant = ..., when = ..., message = ...)`, as the
/// JSON of a rule; the expressions are JSONLogic text, parsed when the
/// declaration is built.
fn parse_rule(meta: &syn::meta::ParseNestedMeta) -> Result<Tokens> {
    let (mut id, mut invariant, mut when, mut message) = (None, None, None, None);
    meta.parse_nested_meta(|m| {
        let s: syn::LitStr = m.value()?.parse()?;
        match m.path.get_ident().map(|i| i.to_string()).as_deref() {
            Some("id") => id = Some(s),
            Some("invariant") => invariant = Some(s),
            Some("when") => when = Some(s),
            Some("message") => message = Some(s),
            _ => return Err(m.error("a rule takes id, invariant, when, and message")),
        }
        Ok(())
    })?;
    let missing = |what: &str| meta.error(format!("a rule needs `{what}`"));
    let id = id.ok_or_else(|| missing("id"))?;
    let invariant = invariant.ok_or_else(|| missing("invariant"))?;
    let message = message.ok_or_else(|| missing("message"))?;
    let when = match when {
        Some(w) => {
            quote! { m.insert("when".to_string(), ::rtemis_r8::derive::serde_json::from_str(#w).expect("the rule's JSONLogic text is JSON")); }
        }
        None => quote! {},
    };
    Ok(quote! {{
        let mut m = ::rtemis_r8::derive::serde_json::Map::new();
        m.insert("id".to_string(), #id.into());
        m.insert("invariant".to_string(), ::rtemis_r8::derive::serde_json::from_str(#invariant).expect("the rule's JSONLogic text is JSON"));
        #when
        m.insert("message".to_string(), #message.into());
        ::rtemis_r8::derive::serde_json::Value::Object(m)
    }})
}

fn type_attrs(attrs: &[Attribute]) -> Result<TypeAttrs> {
    let mut out = TypeAttrs::default();
    for a in attrs.iter().filter(|a| a.path().is_ident("r8")) {
        a.parse_nested_meta(|meta| {
            match meta.path.get_ident().map(|i| i.to_string()).as_deref() {
                Some("namespace") => {
                    let s: syn::LitStr = meta.value()?.parse()?;
                    if !valid_name(&s.value()) {
                        return Err(Error::new_spanned(
                            &s,
                            "a namespace matches [A-Za-z][A-Za-z0-9_]*",
                        ));
                    }
                    out.namespace = Some(s.value());
                }
                Some("version") => {
                    let i: syn::LitInt = meta.value()?.parse()?;
                    out.version = Some(i.base10_parse()?);
                }
                Some("tag") => {
                    let s: syn::LitStr = meta.value()?.parse()?;
                    out.tag = Some(s.value());
                }
                Some("rule") => out.rules.push(parse_rule(&meta)?),
                _ => return Err(meta.error("expected namespace, version, tag, or rule")),
            }
            Ok(())
        })?;
    }
    Ok(out)
}

/// The JSON of one field's declaration.
fn field_def(field: &syn::Field) -> Result<Tokens> {
    let ident = field.ident.as_ref().expect("named fields");
    let name = ident.to_string().trim_start_matches("r#").to_string();
    if !valid_name(&name) {
        return Err(Error::new_spanned(
            ident,
            "a field name matches [A-Za-z][A-Za-z0-9_]*",
        ));
    }
    let ty = &field.ty;
    let mut constraints: Vec<(String, String)> = Vec::new();
    let mut extra: Vec<Tokens> = Vec::new();
    let mut resolve = quote! { None };
    for a in field.attrs.iter().filter(|a| a.path().is_ident("r8")) {
        a.parse_nested_meta(|meta| {
            let key = meta.path.get_ident().map(|i| i.to_string()).unwrap_or_default();
            match key.as_str() {
                "min" | "max" | "exclusive_min" | "exclusive_max" | "pattern" | "finite" | "min_length" | "max_length" => {
                    let e: Expr = meta.value()?.parse()?;
                    constraints.push((key, json_literal(&e)?));
                }
                "unique" => constraints.push((key, "true".to_string())),
                "sensitive" | "external" => extra.push(quote! { (#key, ::rtemis_r8::derive::serde_json::Value::Bool(true)) }),
                "unit" => {
                    let s: syn::LitStr = meta.value()?.parse()?;
                    extra.push(quote! { ("unit", #s.into()) });
                }
                "default" => {
                    let s: syn::LitStr = meta.value()?.parse()?;
                    extra.push(quote! { ("default", ::rtemis_r8::derive::serde_json::from_str(#s).expect("a default is JSON text")) });
                }
                "resolve" => {
                    let s: syn::LitStr = meta.value()?.parse()?;
                    if !matches!(s.value().as_str(), "none" | "runtime") {
                        return Err(Error::new_spanned(&s, "resolve is \"none\" or \"runtime\"; a computed field is declared in R"));
                    }
                    let policy = s.value();
                    resolve = quote! { Some(::rtemis_r8::derive::serde_json::json!({"policy": #policy})) };
                }
                _ => return Err(meta.error("unknown R8 field attribute")),
            }
            Ok(())
        })?;
    }
    let constraint_text = format!(
        "{{{}}}",
        constraints
            .iter()
            .map(|(k, v)| format!("{k:?}: {v}"))
            .collect::<Vec<_>>()
            .join(", ")
    );
    let desc = description(&field.attrs);
    let field_name = name.clone();
    Ok(quote! {{
        let ft = ::rtemis_r8::derive::constrain(
            <#ty as ::rtemis_r8::derive::Encode>::field_type(),
            ::rtemis_r8::derive::serde_json::from_str(#constraint_text).expect("constraints are JSON"),
            #resolve,
        )
        .unwrap_or_else(|e| panic!("field `{}`: {}", #field_name, e));
        let mut pairs = vec![("name", #name.into()), ("type", ::rtemis_r8::derive::field_json(&ft))];
        pairs.extend([#(#extra),*]);
        pairs.push(("description", #desc.into()));
        ::rtemis_r8::derive::object(pairs)
    }})
}

fn fields_of(fields: &Fields) -> Result<Vec<&syn::Field>> {
    match fields {
        Fields::Named(n) => Ok(n.named.iter().collect()),
        Fields::Unit => Ok(Vec::new()),
        Fields::Unnamed(u) => Err(Error::new_spanned(u, "an R8 struct has named fields")),
    }
}

/// Encoding, decoding, and registration of named fields, reading and
/// writing `m`, the object of the JSON form.
fn field_codec(fields: &[&syn::Field]) -> (Vec<Tokens>, Vec<Tokens>, Vec<Tokens>) {
    let mut encode = Vec::new();
    let mut decode = Vec::new();
    let mut register = Vec::new();
    for f in fields {
        let ident = f.ident.as_ref().unwrap();
        let ty = &f.ty;
        let name = ident.to_string().trim_start_matches("r#").to_string();
        encode.push(quote! {
            m.insert(#name.to_string(), ::rtemis_r8::derive::Encode::encode(#ident).map_err(|e| format!("{}: {}", #name, e))?);
        });
        decode.push(quote! {
            #ident: ::rtemis_r8::derive::Encode::decode(v.get(#name).unwrap_or(&::rtemis_r8::derive::serde_json::Value::Null))
                .map_err(|e| format!("{}: {}", #name, e))?
        });
        register.push(quote! { <#ty as ::rtemis_r8::derive::Encode>::register(set); });
    }
    (encode, decode, register)
}

fn expand(input: &DeriveInput) -> Result<Tokens> {
    let attrs = type_attrs(&input.attrs)?;
    let ident = &input.ident;
    let type_name = ident.to_string();
    let namespace = attrs
        .namespace
        .ok_or_else(|| Error::new(Span::call_site(), "#[r8(namespace = \"...\")] is required"))?;
    let version = attrs
        .version
        .ok_or_else(|| Error::new(Span::call_site(), "#[r8(version = N)] is required"))?;
    let qualified = format!("{namespace}::{type_name}");
    let desc = description(&input.attrs);
    let rules = &attrs.rules;
    if !input.generics.params.is_empty() {
        return Err(Error::new_spanned(
            &input.generics,
            "an R8 type has no generic parameters",
        ));
    }
    let (def, encode, decode, register) = match &input.data {
        Data::Struct(s) => {
            let fields = fields_of(&s.fields)?;
            let defs = fields
                .iter()
                .map(|f| field_def(f))
                .collect::<Result<Vec<_>>>()?;
            let (enc, dec, reg) = field_codec(&fields);
            let names: Vec<_> = fields.iter().map(|f| f.ident.as_ref().unwrap()).collect();
            (
                quote! {{
                    let mut m = ::rtemis_r8::derive::serde_json::Map::new();
                    m.insert("kind".to_string(), "struct".into());
                    m.insert("name".to_string(), #type_name.into());
                    m.insert("description".to_string(), #desc.into());
                    m.insert("version".to_string(), #version.into());
                    m.insert("fields".to_string(), ::rtemis_r8::derive::serde_json::Value::Array(vec![#(#defs),*]));
                    let rules: Vec<::rtemis_r8::derive::serde_json::Value> = vec![#(#rules),*];
                    if !rules.is_empty() {
                        m.insert("rules".to_string(), ::rtemis_r8::derive::serde_json::Value::Array(rules));
                    }
                    ::rtemis_r8::derive::serde_json::Value::Object(m)
                }},
                quote! {
                    let Self { #(#names),* } = self;
                    let mut m = ::rtemis_r8::derive::serde_json::Map::new();
                    #(#enc)*
                    Ok(::rtemis_r8::derive::serde_json::Value::Object(m))
                },
                quote! { Ok(Self { #(#dec),* }) },
                quote! { #(#reg)* },
            )
        }
        Data::Enum(e) => {
            let tag = attrs.tag.unwrap_or_else(|| "type".to_string());
            let unit_only = e.variants.iter().all(|v| matches!(v.fields, Fields::Unit));
            let mut variant_defs = Vec::new();
            let mut enc_arms = Vec::new();
            let mut dec_arms = Vec::new();
            let mut reg = Vec::new();
            for v in &e.variants {
                let vid = &v.ident;
                let vname = vid.to_string();
                let vdesc = description(&v.attrs);
                let vattrs = type_attrs(&v.attrs)?;
                let vrules = &vattrs.rules;
                match &v.fields {
                    Fields::Unit => {
                        variant_defs.push(quote! { ::rtemis_r8::derive::serde_json::json!({"name": #vname, "description": #vdesc, "shape": "unit"}) });
                        if unit_only {
                            enc_arms.push(quote! { Self::#vid => Ok(#vname.into()) });
                        } else {
                            enc_arms.push(quote! { Self::#vid => Ok(::rtemis_r8::derive::serde_json::json!({#tag: #vname})) });
                        }
                        dec_arms.push(quote! { Some(#vname) => Ok(Self::#vid) });
                    }
                    Fields::Named(_) => {
                        let fields = fields_of(&v.fields)?;
                        let defs = fields
                            .iter()
                            .map(|f| field_def(f))
                            .collect::<Result<Vec<_>>>()?;
                        let (enc, dec, r) = field_codec(&fields);
                        let names: Vec<_> =
                            fields.iter().map(|f| f.ident.as_ref().unwrap()).collect();
                        variant_defs.push(quote! {{
                            let mut m = ::rtemis_r8::derive::serde_json::Map::new();
                            m.insert("name".to_string(), #vname.into());
                            m.insert("description".to_string(), #vdesc.into());
                            m.insert("shape".to_string(), "struct".into());
                            m.insert("fields".to_string(), ::rtemis_r8::derive::serde_json::Value::Array(vec![#(#defs),*]));
                            let rules: Vec<::rtemis_r8::derive::serde_json::Value> = vec![#(#vrules),*];
                            if !rules.is_empty() {
                                m.insert("rules".to_string(), ::rtemis_r8::derive::serde_json::Value::Array(rules));
                            }
                            ::rtemis_r8::derive::serde_json::Value::Object(m)
                        }});
                        enc_arms.push(quote! {
                            Self::#vid { #(#names),* } => {
                                let mut m = ::rtemis_r8::derive::serde_json::Map::new();
                                #(#enc)*
                                m.insert(#tag.to_string(), #vname.into());
                                Ok(::rtemis_r8::derive::serde_json::Value::Object(m))
                            }
                        });
                        dec_arms.push(quote! { Some(#vname) => Ok(Self::#vid { #(#dec),* }) });
                        reg.extend(r);
                    }
                    Fields::Unnamed(u) if u.unnamed.len() == 1 => {
                        let inner = &u.unnamed[0].ty;
                        variant_defs.push(quote! {
                            ::rtemis_r8::derive::serde_json::json!({"name": #vname, "description": #vdesc, "shape": "newtype", "of": (<#inner as ::rtemis_r8::derive::R8>::NAME)})
                        });
                        let x = format_ident!("inner");
                        enc_arms.push(quote! {
                            Self::#vid(#x) => {
                                let mut out = ::rtemis_r8::derive::Encode::encode(#x)?;
                                match &mut out {
                                    ::rtemis_r8::derive::serde_json::Value::Object(m) => {
                                        m.insert(#tag.to_string(), #vname.into());
                                    }
                                    _ => return Err(format!("variant `{}` wraps a struct", #vname)),
                                }
                                Ok(out)
                            }
                        });
                        dec_arms.push(quote! { Some(#vname) => Ok(Self::#vid(::rtemis_r8::derive::Encode::decode(v)?)) });
                        reg.push(
                            quote! { <#inner as ::rtemis_r8::derive::Encode>::register(set); },
                        );
                    }
                    Fields::Unnamed(u) => {
                        return Err(Error::new_spanned(u, "a newtype variant wraps one struct"));
                    }
                }
            }
            let decode = if unit_only {
                quote! {
                    match v.as_str() {
                        #(#dec_arms,)*
                        other => Err(format!("{:?} is not a variant of {}", other, #qualified)),
                    }
                }
            } else {
                quote! {
                    match v.get(#tag).and_then(|t| t.as_str()) {
                        #(#dec_arms,)*
                        other => Err(format!("{:?} is not a variant of {}", other, #qualified)),
                    }
                }
            };
            (
                quote! {{
                    let variants: Vec<::rtemis_r8::derive::serde_json::Value> = vec![#(#variant_defs),*];
                    ::rtemis_r8::derive::serde_json::json!({
                        "kind": "enum",
                        "name": #type_name,
                        "description": #desc,
                        "tag": #tag,
                        "version": #version,
                        "variants": variants
                    })
                }},
                quote! { match self { #(#enc_arms),* } },
                decode,
                quote! { #(#reg)* },
            )
        }
        Data::Union(u) => {
            return Err(Error::new_spanned(
                u.union_token,
                "R8 types are structs and enums",
            ));
        }
    };
    Ok(quote! {
        impl ::rtemis_r8::derive::Encode for #ident {
            fn field_type() -> ::rtemis_r8::ir::FieldType {
                ::rtemis_r8::derive::named(#qualified)
            }
            fn encode(&self) -> ::std::result::Result<::rtemis_r8::derive::serde_json::Value, ::std::string::String> {
                #encode
            }
            fn decode(v: &::rtemis_r8::derive::serde_json::Value) -> ::std::result::Result<Self, ::std::string::String> {
                #decode
            }
            fn register(set: &mut ::rtemis_r8::rules::DocumentSet) {
                if ::rtemis_r8::derive::register_def(set, #qualified, <Self as ::rtemis_r8::derive::R8>::type_def) {
                    #register
                }
            }
        }
        impl ::rtemis_r8::derive::R8 for #ident {
            const NAME: &'static str = #qualified;
            fn type_def() -> ::rtemis_r8::ir::TypeDef {
                ::rtemis_r8::derive::type_def(#def)
            }
        }
    })
}
