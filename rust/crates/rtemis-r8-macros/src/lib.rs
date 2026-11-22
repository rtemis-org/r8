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
//!
//! The derive builds the declaration as `rtemis_r8::ir` values. A rule's
//! JSONLogic and a field's default are parsed when the crate compiles, so a
//! text that does not read is a compile error at its attribute.
//!
//! `#[r8(native)]` on a struct also implements `rtemis_r8_api::Native`, and
//! on an enum `rtemis_r8_api::NativeEnum`; on both, `rtemis_r8_api::Codec`, so
//! R8 holds the type's values in Rust: each field's type implements
//! `rtemis_r8_api::Field`, a `vec()` field of numbers, logicals, or bytes is
//! a `rtemis_r8_api::Buffer`, and a newtype variant's struct implements
//! `rtemis_r8_api::Native`. `#[r8::methods]` and `r8::exports!`
//! register the struct and its impls with R8.

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

/// The operators of the rule dialect, by JSONLogic name, and the variants of
/// `rtemis_r8::ir::Operator` they are.
const OPERATORS: &[(&str, &str)] = &[
    ("var", "Var"),
    ("===", "StrictEq"),
    ("!==", "StrictNe"),
    ("==", "LooseEq"),
    ("!=", "LooseNe"),
    ("<", "Lt"),
    ("<=", "Le"),
    (">", "Gt"),
    (">=", "Ge"),
    ("!", "Not"),
    ("+", "Add"),
    ("-", "Sub"),
    ("*", "Mul"),
    ("/", "Div"),
    ("%", "Mod"),
    ("min", "Min"),
    ("max", "Max"),
    ("in", "In"),
    ("cat", "Cat"),
    ("length", "Length"),
    ("subset", "Subset"),
    ("non_increasing", "NonIncreasing"),
    ("non_decreasing", "NonDecreasing"),
    ("floor", "Floor"),
    ("ceiling", "Ceiling"),
    ("round", "Round"),
    ("nrow", "Nrow"),
    ("ncol", "Ncol"),
    ("dim", "Dim"),
    ("is_null", "IsNull"),
    ("is_variant", "IsVariant"),
    ("levels", "Levels"),
    ("all", "All"),
    ("any", "Any"),
    ("if", "If"),
    ("and", "And"),
    ("or", "Or"),
];

/// The construction of a `serde_json::Number`.
fn number_tokens(n: &serde_json::Number) -> Tokens {
    if let Some(u) = n.as_u64() {
        quote! { ::rtemis_r8::derive::serde_json::Number::from(#u) }
    } else if let Some(i) = n.as_i64() {
        quote! { ::rtemis_r8::derive::serde_json::Number::from(#i) }
    } else {
        let f = n.as_f64().expect("a JSON number is finite");
        quote! { ::rtemis_r8::derive::serde_json::Number::from_f64(#f).expect("a finite number") }
    }
}

/// The construction of a `serde_json::Value`.
fn value_tokens(v: &serde_json::Value) -> Tokens {
    use serde_json::Value as V;
    let j = quote! { ::rtemis_r8::derive::serde_json };
    match v {
        V::Null => quote! { #j::Value::Null },
        V::Bool(b) => quote! { #j::Value::Bool(#b) },
        V::Number(n) => {
            let n = number_tokens(n);
            quote! { #j::Value::Number(#n) }
        }
        V::String(s) => quote! { #j::Value::String(#s.to_string()) },
        V::Array(a) => {
            let items = a.iter().map(value_tokens);
            quote! { #j::Value::Array(::std::vec![#(#items),*]) }
        }
        V::Object(m) => {
            let (k, x): (Vec<_>, Vec<_>) = m.iter().map(|(k, x)| (k, value_tokens(x))).unzip();
            quote! {{
                let mut m = #j::Map::new();
                #(m.insert(#k.to_string(), #x);)*
                #j::Value::Object(m)
            }}
        }
    }
}

/// The construction of an `rtemis_r8::ir::Logic` tree from a parsed
/// expression, as its deserializer reads it.
fn logic_tokens(v: &serde_json::Value) -> Tokens {
    use serde_json::Value as V;
    let ir = quote! { ::rtemis_r8::ir };
    let other = |v: &V| {
        let v = value_tokens(v);
        quote! { #ir::Logic::Other(::std::boxed::Box::new(#v)) }
    };
    match v {
        V::Null => quote! { #ir::Logic::Null },
        V::Bool(b) => quote! { #ir::Logic::Bool(#b) },
        V::Number(n) => {
            let n = number_tokens(n);
            quote! { #ir::Logic::Number(#n) }
        }
        V::String(s) => quote! { #ir::Logic::String(#s.into()) },
        V::Array(a) => {
            let items = a.iter().map(logic_tokens);
            quote! { #ir::Logic::Array(::std::boxed::Box::new([#(#items),*])) }
        }
        V::Object(m) if m.len() == 1 => {
            let (op, arg) = m.iter().next().expect("one member");
            let Some((_, variant)) = OPERATORS.iter().find(|(name, _)| name == op) else {
                return other(v);
            };
            let variant = format_ident!("{}", variant);
            match (op.as_str(), arg) {
                ("var", V::String(path)) => quote! { #ir::Logic::Var(#path.into()) },
                (_, V::Array(args)) => {
                    let args = args.iter().map(logic_tokens);
                    quote! { #ir::Logic::Call {
                        op: #ir::Operator::#variant,
                        single: false,
                        args: ::std::boxed::Box::new([#(#args),*]),
                    } }
                }
                (_, one) => {
                    let one = logic_tokens(one);
                    quote! { #ir::Logic::Call {
                        op: #ir::Operator::#variant,
                        single: true,
                        args: ::std::boxed::Box::new([#one]),
                    } }
                }
            }
        }
        V::Object(_) => other(v),
    }
}

/// A JSONLogic attribute, parsed when the crate compiles.
fn logic_attr(s: &syn::LitStr) -> Result<Tokens> {
    let v: serde_json::Value = serde_json::from_str(&s.value())
        .map_err(|e| Error::new_spanned(s, format!("the JSONLogic text does not read: {e}")))?;
    Ok(logic_tokens(&v))
}

#[derive(Default)]
struct TypeAttrs {
    namespace: Option<String>,
    version: Option<u32>,
    tag: Option<String>,
    rules: Vec<Tokens>,
    native: bool,
}

/// One `rule(id = ..., invariant = ..., when = ..., message = ...)`, as the
/// construction of an `rtemis_r8::ir::RuleDef`; the expressions are
/// JSONLogic text, parsed when the crate compiles.
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
    if !valid_name(&id.value()) {
        return Err(Error::new_spanned(
            &id,
            "a rule ID matches [A-Za-z][A-Za-z0-9_]*",
        ));
    }
    let invariant = logic_attr(&invariant.ok_or_else(|| missing("invariant"))?)?;
    let message = message.ok_or_else(|| missing("message"))?;
    let when = match when {
        Some(w) => {
            let w = logic_attr(&w)?;
            quote! { ::std::option::Option::Some(#w) }
        }
        None => quote! { ::std::option::Option::None },
    };
    Ok(quote! {
        ::rtemis_r8::ir::RuleDef {
            id: ::rtemis_r8::derive::name(#id),
            invariant: #invariant,
            when: #when,
            message: #message.to_string(),
        }
    })
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
                Some("native") => out.native = true,
                _ => return Err(meta.error("expected namespace, version, tag, rule, or native")),
            }
            Ok(())
        })?;
    }
    Ok(out)
}

/// A numeric attribute value, a literal number with an optional `-`.
fn number_attr(e: &Expr) -> Result<f64> {
    json_literal(e)?
        .parse::<f64>()
        .map_err(|_| Error::new_spanned(e, "expected a number"))
}

/// The construction of one field's `rtemis_r8::ir::FieldDef`.
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
    let mut constraints: Vec<Tokens> = Vec::new();
    let (mut sensitive, mut external) = (false, false);
    let mut unit = quote! { ::std::option::Option::None };
    let mut default = quote! { ::std::option::Option::None };
    let mut resolve = quote! { ::std::option::Option::None };
    for a in field.attrs.iter().filter(|a| a.path().is_ident("r8")) {
        a.parse_nested_meta(|meta| {
            let key = meta.path.get_ident().map(|i| i.to_string()).unwrap_or_default();
            match key.as_str() {
                "min" | "max" | "exclusive_min" | "exclusive_max" => {
                    let e: Expr = meta.value()?.parse()?;
                    let x = number_attr(&e)?;
                    let k = format_ident!("{}", key);
                    constraints.push(quote! { #k: ::std::option::Option::Some(#x) });
                }
                "min_length" | "max_length" => {
                    let n: syn::LitInt = meta.value()?.parse()?;
                    let x: u64 = n.base10_parse()?;
                    let k = format_ident!("{}", key);
                    constraints.push(quote! { #k: ::std::option::Option::Some(#x) });
                }
                "pattern" => {
                    let s: syn::LitStr = meta.value()?.parse()?;
                    constraints.push(quote! { pattern: ::std::option::Option::Some(#s) });
                }
                "finite" => {
                    let b: syn::LitBool = meta.value()?.parse()?;
                    let b = b.value;
                    constraints.push(quote! { finite: ::std::option::Option::Some(#b) });
                }
                "unique" => constraints.push(quote! { unique: true }),
                "sensitive" => sensitive = true,
                "external" => external = true,
                "unit" => {
                    let s: syn::LitStr = meta.value()?.parse()?;
                    unit = quote! { ::std::option::Option::Some(#s.into()) };
                }
                "default" => {
                    let s: syn::LitStr = meta.value()?.parse()?;
                    let v: serde_json::Value = serde_json::from_str(&s.value())
                        .map_err(|e| Error::new_spanned(&s, format!("a default is JSON text: {e}")))?;
                    let v = value_tokens(&v);
                    default = quote! { ::std::option::Option::Some(::std::boxed::Box::new(#v)) };
                }
                "resolve" => {
                    let s: syn::LitStr = meta.value()?.parse()?;
                    resolve = match s.value().as_str() {
                        "none" => quote! { ::std::option::Option::Some(::rtemis_r8::ir::Resolve::None) },
                        "runtime" => quote! { ::std::option::Option::Some(::rtemis_r8::ir::Resolve::Runtime) },
                        _ => return Err(Error::new_spanned(&s, "resolve is \"none\" or \"runtime\"; a computed field is declared in R")),
                    };
                }
                _ => return Err(meta.error("unknown R8 field attribute")),
            }
            Ok(())
        })?;
    }
    let desc = description(&field.attrs);
    let field_name = name.clone();
    Ok(quote! {
        ::rtemis_r8::ir::FieldDef {
            name: ::rtemis_r8::derive::name(#name),
            ty: ::rtemis_r8::derive::constrain(
                <#ty as ::rtemis_r8::derive::Encode>::field_type(),
                ::rtemis_r8::derive::Constraints {
                    #(#constraints,)*
                    ..::std::default::Default::default()
                },
                #resolve,
            )
            .unwrap_or_else(|e| panic!("field `{}`: {}", #field_name, e)),
            default: #default,
            description: #desc.into(),
            sensitive: #sensitive,
            unit: #unit,
            external: #external,
            unique: false,
            optional: false,
            inherited_from: ::std::option::Option::None,
            extensions: ::std::option::Option::None,
        }
    })
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
                quote! {
                    ::rtemis_r8::ir::TypeDef::Struct(::rtemis_r8::ir::StructDef {
                        name: ::rtemis_r8::derive::name(#type_name),
                        description: #desc.to_string(),
                        version: #version,
                        fields: ::std::vec![#(#defs),*],
                        extends: ::std::option::Option::None,
                        rules: ::std::vec![#(#rules),*],
                        migrations: ::std::vec::Vec::new(),
                        aliases: ::std::vec::Vec::new(),
                        provenance: false,
                        s4: false,
                        extensions: ::std::collections::BTreeMap::new(),
                    })
                },
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
            let tag = attrs.tag.clone().unwrap_or_else(|| "type".to_string());
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
                        variant_defs.push(quote! {
                            ::rtemis_r8::ir::VariantDef {
                                name: ::rtemis_r8::derive::name(#vname),
                                description: #vdesc.to_string(),
                                shape: ::rtemis_r8::ir::VariantShape::Unit,
                            }
                        });
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
                        variant_defs.push(quote! {
                            ::rtemis_r8::ir::VariantDef {
                                name: ::rtemis_r8::derive::name(#vname),
                                description: #vdesc.to_string(),
                                shape: ::rtemis_r8::ir::VariantShape::Struct {
                                    fields: ::std::vec![#(#defs),*],
                                    rules: ::std::vec![#(#vrules),*],
                                },
                            }
                        });
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
                            ::rtemis_r8::ir::VariantDef {
                                name: ::rtemis_r8::derive::name(#vname),
                                description: #vdesc.to_string(),
                                shape: ::rtemis_r8::ir::VariantShape::Newtype {
                                    of: ::rtemis_r8::name::QualifiedName::parse(
                                        <#inner as ::rtemis_r8::derive::R8>::NAME,
                                    )
                                    .expect("the derive checks the name"),
                                },
                            }
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
                quote! {
                    ::rtemis_r8::ir::TypeDef::Enum(::rtemis_r8::ir::EnumDef {
                        name: ::rtemis_r8::derive::name(#type_name),
                        description: #desc.to_string(),
                        tag: ::rtemis_r8::derive::name(#tag),
                        version: #version,
                        variants: ::std::vec![#(#variant_defs),*],
                        migrations: ::std::vec::Vec::new(),
                        aliases: ::std::vec::Vec::new(),
                        extensions: ::std::collections::BTreeMap::new(),
                    })
                },
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
    let native = if attrs.native {
        match &input.data {
            Data::Struct(st) => native_impl(ident, &qualified, &fields_of(&st.fields)?)?,
            Data::Enum(e) => {
                native_enum_impl(ident, &qualified, e, attrs.tag.as_deref().unwrap_or("type"))?
            }
            Data::Union(_) => {
                return Err(Error::new(
                    Span::call_site(),
                    "#[r8(native)] applies to a struct or an enum",
                ));
            }
        }
    } else {
        quote! {}
    };
    Ok(quote! {
        #native
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
                #def
            }
        }
    })
}

/// `impl rtemis_r8_api::NativeEnum` and `impl rtemis_r8_api::Codec` for an
/// enum of `#[r8(native)]`: unit variants, struct variants whose fields are
/// each a `Field`, and newtype variants whose struct implements `Native`.
/// The state's bytes are the variant's declaration index, a `u32`, then its
/// fields.
fn native_enum_impl(
    ident: &syn::Ident,
    qualified: &str,
    e: &syn::DataEnum,
    tag: &str,
) -> Result<Tokens> {
    let type_c = c_str(qualified);
    let unit_only = e.variants.iter().all(|v| matches!(v.fields, Fields::Unit));
    let tag_c = c_str(tag);
    let mut from_node = Vec::new();
    let api = quote! { ::rtemis_r8_api };
    let unknown_field = quote! {
        Err(#api::Error::new("unknown_field", format!("`{}` has no field `{}`", #qualified, name)))
    };
    let mut from = Vec::new();
    let mut variant = Vec::new();
    let mut get = Vec::new();
    let mut with = Vec::new();
    let mut write = Vec::new();
    let mut read = Vec::new();
    let mut bytes = Vec::new();
    for (k, v) in e.variants.iter().enumerate() {
        let vid = &v.ident;
        let vname = vid.to_string();
        let vname_c = c_str(&vname);
        let idx = k as u32;
        match &v.fields {
            Fields::Unit => {
                from.push(quote! {
                    #vname => {
                        if !fields.is_empty() {
                            return Err(#api::Error::new("arity", format!("`{}@{}` has no fields, and R8 passed {}", #qualified, #vname, fields.len())));
                        }
                        Ok(Self::#vid)
                    }
                });
                from_node.push(quote! { #vname => Ok(Self::#vid) });
                variant.push(quote! { Self::#vid => #vname_c });
                get.push(quote! { Self::#vid => #unknown_field });
                with.push(quote! { Self::#vid => #unknown_field });
                write.push(quote! { Self::#vid => out.write(&#idx.to_le_bytes()) });
                read.push(quote! { #idx => Ok(Self::#vid) });
                bytes.push(quote! { Self::#vid => 0 });
            }
            Fields::Named(_) => {
                let fields = fields_of(&v.fields)?;
                let n = fields.len();
                let idents: Vec<_> = fields.iter().map(|f| f.ident.as_ref().unwrap()).collect();
                let names: Vec<String> = idents
                    .iter()
                    .map(|i| i.to_string().trim_start_matches("r#").to_string())
                    .collect();
                let tys: Vec<_> = fields.iter().map(|f| &f.ty).collect();
                let conv: Vec<Tokens> = idents
                    .iter()
                    .zip(&tys)
                    .zip(&names)
                    .enumerate()
                    .map(|(j, ((i, t), name))| {
                        quote! {
                            #i: unsafe { <#t as #api::Field>::from_r(fields[#j]) }
                                .map_err(|e| #api::Error::new(e.code, format!("field `{}`: {}", #name, e.message)))?
                        }
                    })
                    .collect();
                from.push(quote! {
                    #vname => {
                        if fields.len() != #n {
                            return Err(#api::Error::new("arity", format!("`{}@{}` has {} fields, and R8 passed {}", #qualified, #vname, #n, fields.len())));
                        }
                        Ok(Self::#vid { #(#conv),* })
                    }
                });
                let node_conv: Vec<Tokens> = idents
                    .iter()
                    .zip(&tys)
                    .zip(&names)
                    .map(|((i, t), name)| {
                        let name_c = c_str(name);
                        quote! {
                            #i: <#t as #api::Field>::from_node(r, r.field(root, #name_c))
                                .map_err(|e| #api::Error::new(e.code, format!("field `{}`: {}", #name, e.message)))?
                        }
                    })
                    .collect();
                from_node.push(quote! { #vname => Ok(Self::#vid { #(#node_conv),* }) });
                variant.push(quote! { Self::#vid { .. } => #vname_c });
                get.push(quote! {
                    Self::#vid { #(#idents),* } => match name {
                        #(#names => #api::Field::to_r(#idents, owner),)*
                        _ => #unknown_field,
                    }
                });
                let with_arms: Vec<Tokens> = idents
                    .iter()
                    .zip(&tys)
                    .zip(&names)
                    .map(|((i, t), name)| {
                        let others: Vec<Tokens> = idents
                            .iter()
                            .filter(|o| *o != i)
                            .map(|o| quote! { #o: ::std::clone::Clone::clone(#o) })
                            .collect();
                        quote! {
                            #name => Ok(Self::#vid {
                                #i: unsafe { <#t as #api::Field>::from_r(value) }?,
                                #(#others),*
                            })
                        }
                    })
                    .collect();
                with.push(quote! {
                    Self::#vid { #(#idents),* } => match name {
                        #(#with_arms,)*
                        _ => #unknown_field,
                    }
                });
                write.push(quote! {
                    Self::#vid { #(#idents),* } => {
                        out.write(&#idx.to_le_bytes())?;
                        #(#api::Field::write(#idents, out)?;)*
                        Ok(())
                    }
                });
                read.push(quote! {
                    #idx => Ok(Self::#vid { #(#idents: <#tys as #api::Field>::read(input)?),* })
                });
                bytes.push(quote! {
                    Self::#vid { #(#idents),* } => 0 #(+ #api::Field::owned_bytes(#idents))*
                });
            }
            Fields::Unnamed(u) if u.unnamed.len() == 1 => {
                let inner = &u.unnamed[0].ty;
                from.push(quote! {
                    #vname => Ok(Self::#vid(unsafe { <#inner as #api::Native>::from_fields(fields) }?))
                });
                from_node.push(quote! {
                    #vname => Ok(Self::#vid(<#inner as #api::Native>::from_reader(r, root)?))
                });
                variant.push(quote! { Self::#vid(_) => #vname_c });
                get.push(quote! { Self::#vid(inner) => <#inner as #api::Native>::get_field(inner, owner, name) });
                with.push(quote! {
                    Self::#vid(inner) => Ok(Self::#vid(unsafe { <#inner as #api::Native>::with_field(inner, name, value) }?))
                });
                write.push(quote! {
                    Self::#vid(inner) => {
                        out.write(&#idx.to_le_bytes())?;
                        <#inner as #api::Native>::write_state(inner, out)
                    }
                });
                read.push(
                    quote! { #idx => Ok(Self::#vid(<#inner as #api::Native>::read_state(input)?)) },
                );
                bytes.push(
                    quote! { Self::#vid(inner) => <#inner as #api::Native>::owned_bytes(inner) },
                );
            }
            Fields::Unnamed(u) => {
                return Err(Error::new_spanned(
                    u,
                    "a variant holds named fields or one struct",
                ));
            }
        }
    }
    Ok(quote! {
        impl #api::Codec for #ident {
            const TYPE_NAME: &'static ::core::ffi::CStr = #type_c;
            const TABLE: #api::Shared<#api::rtemis_r8::abi::TypeVTable> = #api::enum_codec::<Self>();
            unsafe fn from_r_native(x: #api::sys::SEXP, fields: &[#api::sys::SEXP]) -> #api::Result<Self> {
                let variant = unsafe { #api::call::r_variant(x) }?;
                unsafe { <Self as #api::NativeEnum>::from_variant(&variant, fields) }
            }
        }
        #[allow(unused_variables)]
        impl #api::NativeEnum for #ident {
            const TYPE_NAME: &'static ::core::ffi::CStr = #type_c;
            unsafe fn from_variant(variant: &str, fields: &[#api::sys::SEXP]) -> #api::Result<Self> {
                match variant {
                    #(#from,)*
                    _ => Err(#api::Error::new("unknown_variant", format!("`{}` has no variant `{}`", #qualified, variant))),
                }
            }
            fn from_reader<'a>(r: &#api::Reader<'a>, root: #api::Node<'a>) -> #api::Result<Self> {
                let variant = if #unit_only { r.string(root)? } else { r.string(r.field(root, #tag_c))? };
                match variant {
                    #(#from_node,)*
                    _ => Err(#api::Error::new("unknown_variant", format!("`{}` has no variant `{}`", #qualified, variant))),
                }
            }
            fn variant(&self) -> &'static ::core::ffi::CStr {
                match self { #(#variant,)* }
            }
            fn get_field(&self, owner: #api::sys::SEXP, name: &str) -> #api::Result<#api::sys::SEXP> {
                match self { #(#get,)* }
            }
            unsafe fn with_field(&self, name: &str, value: #api::sys::SEXP) -> #api::Result<Self> {
                match self { #(#with,)* }
            }
            fn write_state(&self, out: &mut #api::StateWriter) -> #api::Result<()> {
                match self { #(#write,)* }
            }
            fn read_state(input: &mut #api::StateReader) -> #api::Result<Self> {
                match u32::from_le_bytes(input.take_array()?) {
                    #(#read,)*
                    i => Err(#api::Error::new("bad_state", format!("`{}` has no variant of index {}", #qualified, i))),
                }
            }
            fn owned_bytes(&self) -> usize {
                match self { #(#bytes,)* }
            }
        }
    })
}

/// A C string literal, `c"..."`, of an R8 name, which holds ASCII letters,
/// digits, underscores, and `::`.
fn c_str(s: &str) -> Tokens {
    format!("c{s:?}").parse().expect("a C string literal")
}

/// `impl rtemis_r8_api::Native` for a struct of `#[r8(native)]`.
fn native_impl(ident: &syn::Ident, qualified: &str, fields: &[&syn::Field]) -> Result<Tokens> {
    let type_c = c_str(qualified);
    let n = fields.len();
    let idents: Vec<_> = fields.iter().map(|f| f.ident.as_ref().unwrap()).collect();
    let names: Vec<String> = idents
        .iter()
        .map(|i| i.to_string().trim_start_matches("r#").to_string())
        .collect();
    let tys: Vec<_> = fields.iter().map(|f| &f.ty).collect();
    let api = quote! { ::rtemis_r8_api };
    let from: Vec<Tokens> = idents
        .iter()
        .zip(&tys)
        .zip(&names)
        .enumerate()
        .map(|(k, ((i, t), name))| {
            quote! {
                #i: unsafe { <#t as #api::Field>::from_r(fields[#k]) }
                    .map_err(|e| #api::Error::new(e.code, format!("field `{}`: {}", #name, e.message)))?
            }
        })
        .collect();
    let from_node: Vec<Tokens> = idents
        .iter()
        .zip(&tys)
        .zip(&names)
        .map(|((i, t), name)| {
            let name_c = c_str(name);
            quote! {
                #i: <#t as #api::Field>::from_node(r, r.field(root, #name_c))
                    .map_err(|e| #api::Error::new(e.code, format!("field `{}`: {}", #name, e.message)))?
            }
        })
        .collect();
    let with_arms: Vec<Tokens> = idents
        .iter()
        .zip(&tys)
        .zip(&names)
        .map(|((i, t), name)| {
            let others: Vec<Tokens> = idents
                .iter()
                .filter(|o| *o != i)
                .map(|o| quote! { #o: ::std::clone::Clone::clone(&self.#o) })
                .collect();
            quote! {
                #name => Ok(Self {
                    #i: unsafe { <#t as #api::Field>::from_r(value) }?,
                    #(#others),*
                })
            }
        })
        .collect();
    Ok(quote! {
        impl #api::Codec for #ident {
            const TYPE_NAME: &'static ::core::ffi::CStr = #type_c;
            const TABLE: #api::Shared<#api::rtemis_r8::abi::TypeVTable> = #api::codec::<Self>();
            unsafe fn from_r_native(_x: #api::sys::SEXP, fields: &[#api::sys::SEXP]) -> #api::Result<Self> {
                unsafe { <Self as #api::Native>::from_fields(fields) }
            }
        }
        impl #api::Native for #ident {
            const TYPE_NAME: &'static ::core::ffi::CStr = #type_c;
            unsafe fn from_fields(fields: &[#api::sys::SEXP]) -> #api::Result<Self> {
                if fields.len() != #n {
                    return Err(#api::Error::new(
                        "arity",
                        format!("`{}` has {} fields, and R8 passed {}", #qualified, #n, fields.len()),
                    ));
                }
                Ok(Self { #(#from),* })
            }
            fn from_reader<'a>(r: &#api::Reader<'a>, root: #api::Node<'a>) -> #api::Result<Self> {
                Ok(Self { #(#from_node),* })
            }
            fn get_field(&self, owner: #api::sys::SEXP, name: &str) -> #api::Result<#api::sys::SEXP> {
                match name {
                    #(#names => #api::Field::to_r(&self.#idents, owner),)*
                    _ => Err(#api::Error::new("unknown_field", format!("`{}` has no field `{}`", #qualified, name))),
                }
            }
            unsafe fn with_field(&self, name: &str, value: #api::sys::SEXP) -> #api::Result<Self> {
                match name {
                    #(#with_arms,)*
                    _ => Err(#api::Error::new("unknown_field", format!("`{}` has no field `{}`", #qualified, name))),
                }
            }
            fn write_state(&self, out: &mut #api::StateWriter) -> #api::Result<()> {
                #(#api::Field::write(&self.#idents, out)?;)*
                Ok(())
            }
            fn read_state(input: &mut #api::StateReader) -> #api::Result<Self> {
                Ok(Self { #(#idents: <#tys as #api::Field>::read(input)?),* })
            }
            fn owned_bytes(&self) -> usize {
                0 #(+ #api::Field::owned_bytes(&self.#idents))*
            }
        }
    })
}

/// `#[r8::methods(trait = "pkg::Trait")]` on an impl of a Rust trait for a
/// Rust-native struct: keeps the impl, and generates one function per
/// method that R8 calls (`R8MethodFn`), and the constant
/// `__R8_IMPL_<Type>_<Trait>` that `r8::exports!` lists. A method takes
/// `&self` and arguments that implement `rtemis_r8_api::FromR` or are `&T`
/// of a Rust-native type `T` (`rtemis_r8_api::Receiver`), and returns
/// a type that implements `rtemis_r8_api::IntoR`; the trait is declared in
/// R, whose method has `x` and the other parameters by the same names.
#[proc_macro_attribute]
pub fn methods(attr: TokenStream, item: TokenStream) -> TokenStream {
    let item = parse_macro_input!(item as syn::ItemImpl);
    let mut trait_name: Option<syn::LitStr> = None;
    let parser = syn::meta::parser(|meta| {
        if meta.path.is_ident("trait") {
            trait_name = Some(meta.value()?.parse()?);
            Ok(())
        } else {
            Err(meta.error("expected trait = \"pkg::Trait\""))
        }
    });
    parse_macro_input!(attr with parser);
    methods_impl(trait_name, &item)
        .unwrap_or_else(Error::into_compile_error)
        .into()
}

fn valid_qualified(s: &str) -> bool {
    matches!(s.split_once("::"), Some((a, b)) if valid_name(a) && valid_name(b))
}

fn methods_impl(trait_name: Option<syn::LitStr>, item: &syn::ItemImpl) -> Result<Tokens> {
    let trait_lit = trait_name.ok_or_else(|| {
        Error::new(
            Span::call_site(),
            "#[r8::methods(trait = \"pkg::Trait\")] names the R8 trait",
        )
    })?;
    if !valid_qualified(&trait_lit.value()) {
        return Err(Error::new_spanned(
            &trait_lit,
            "a qualified trait name is namespace::Trait",
        ));
    }
    let Some((_, trait_path, _)) = &item.trait_ else {
        return Err(Error::new_spanned(
            item,
            "#[r8::methods] applies to an impl of a Rust trait",
        ));
    };
    let self_ty = &item.self_ty;
    let type_ident = match &**self_ty {
        syn::Type::Path(p) => p.path.segments.last().map(|s| s.ident.clone()),
        _ => None,
    }
    .ok_or_else(|| {
        Error::new_spanned(
            self_ty,
            "#[r8::methods] applies to an impl for a named struct",
        )
    })?;
    let trait_ident = &trait_path.segments.last().expect("a trait path").ident;
    let set_ident = format_ident!("__R8_IMPL_{}_{}", type_ident, trait_ident);
    let api = quote! { ::rtemis_r8_api };
    let trait_c = c_str(&trait_lit.value());
    let mut fns = Vec::new();
    let mut rows = Vec::new();
    for it in &item.items {
        let syn::ImplItem::Fn(f) = it else { continue };
        let sig = &f.sig;
        let mname = sig.ident.to_string();
        let mut inputs = sig.inputs.iter();
        match inputs.next() {
            Some(syn::FnArg::Receiver(r)) if r.reference.is_some() && r.mutability.is_none() => {}
            _ => {
                return Err(Error::new_spanned(
                    sig,
                    "an R8 method takes `&self` first, which R passes as `x`",
                ));
            }
        }
        let mut convert = Vec::new();
        let mut args = Vec::new();
        for (k, a) in inputs.enumerate() {
            let syn::FnArg::Typed(pt) = a else {
                unreachable!("one receiver")
            };
            let syn::Pat::Ident(pi) = &*pt.pat else {
                return Err(Error::new_spanned(
                    &pt.pat,
                    "an R8 method's parameter is a name",
                ));
            };
            let pname = pi.ident.to_string();
            let pident = format_ident!("__r8_arg_{}", k);
            let ty = &pt.ty;
            let index = k + 1;
            match &**ty {
                // `&T`: a value of a Rust-native type, borrowed from its
                // state, or built from an R-native value of the type.
                syn::Type::Reference(r) => {
                    let is_str = matches!(&*r.elem, syn::Type::Path(p) if p.path.is_ident("str"));
                    if r.mutability.is_some() || is_str {
                        return Err(Error::new_spanned(
                            ty,
                            "a reference parameter is `&T` of a Rust-native type `T`; a string is `String`",
                        ));
                    }
                    let inner = &r.elem;
                    convert.push(quote! {
                        let #pident = unsafe { #api::Receiver::<#inner>::from_named_arg(__r8_args[#index], #pname) }?;
                    });
                    args.push(quote! { &*#pident });
                }
                _ => {
                    convert.push(quote! {
                        let #pident: #ty = unsafe { <#ty as #api::FromR>::from_arg(__r8_args[#index], #pname) }?;
                    });
                    args.push(quote! { #pident });
                }
            }
        }
        let n_params = args.len() + 1;
        let fn_ident = format_ident!("__r8_{}_{}_{}", type_ident, trait_ident, sig.ident);
        let method = &sig.ident;
        fns.push(quote! {
            #[allow(non_snake_case)]
            unsafe extern "C" fn #fn_ident(
                _ctx: *const #api::rtemis_r8::abi::CallContext,
                args: *const #api::rtemis_r8::abi::Sexp,
                nargs: usize,
                result: *mut #api::rtemis_r8::abi::Sexp,
                err: *mut #api::rtemis_r8::abi::Error,
            ) -> #api::rtemis_r8::abi::Status {
                unsafe {
                    #api::call::method(args, nargs, #n_params, result, err, |__r8_args| {
                        let __r8_x = #api::Receiver::<#self_ty>::from_arg(__r8_args[0])?;
                        #(#convert)*
                        #api::IntoR::into_r(<#self_ty as #trait_path>::#method(&*__r8_x #(, #args)*))
                    })
                }
            }
        });
        let name_c = c_str(&mname);
        rows.push(quote! {
            #api::MethodRow { name: #name_c, call: #fn_ident, n_params: #n_params }
        });
    }
    Ok(quote! {
        #item
        #(#fns)*
        #[allow(non_upper_case_globals)]
        #[doc(hidden)]
        pub const #set_ident: #api::ImplSet = #api::ImplSet {
            trait_name: #trait_c,
            type_name: <#self_ty as #api::Codec>::TYPE_NAME,
            methods: &[#(#rows),*],
        };
    })
}

/// The arguments of `r8::exports!`.
struct ExportsArgs {
    package: syn::LitStr,
    namespace: syn::LitStr,
    types: Vec<syn::Path>,
    impls: Vec<(syn::Path, syn::Path)>,
}

impl syn::parse::Parse for ExportsArgs {
    fn parse(input: syn::parse::ParseStream) -> Result<Self> {
        let mut package = None;
        let mut namespace = None;
        let mut types = Vec::new();
        let mut impls = Vec::new();
        while !input.is_empty() {
            let key: syn::Ident = input.parse()?;
            input.parse::<syn::Token![=]>()?;
            match key.to_string().as_str() {
                "package" => package = Some(input.parse()?),
                "namespace" => namespace = Some(input.parse()?),
                "types" => {
                    let content;
                    syn::bracketed!(content in input);
                    types = content
                        .parse_terminated(syn::Path::parse_mod_style, syn::Token![,])?
                        .into_iter()
                        .collect();
                }
                "impls" => {
                    let content;
                    syn::bracketed!(content in input);
                    while !content.is_empty() {
                        let ty: syn::Path = content.call(syn::Path::parse_mod_style)?;
                        content.parse::<syn::Token![:]>()?;
                        let tr: syn::Path = content.call(syn::Path::parse_mod_style)?;
                        impls.push((ty, tr));
                        if content.is_empty() {
                            break;
                        }
                        content.parse::<syn::Token![,]>()?;
                    }
                }
                _ => {
                    return Err(Error::new_spanned(
                        key,
                        "expected package, namespace, types, or impls",
                    ));
                }
            }
            if input.is_empty() {
                break;
            }
            input.parse::<syn::Token![,]>()?;
        }
        Ok(ExportsArgs {
            package: package.ok_or_else(|| input.error("`package = \"...\"` is required"))?,
            namespace: namespace.ok_or_else(|| input.error("`namespace = \"...\"` is required"))?,
            types,
            impls,
        })
    }
}

/// `r8::exports! { package = "pkg", namespace = "ns", types = [T, ...],
/// impls = [T: Trait, ...] }`: the package's exports table, built on first
/// use, and `r8_init(dll)`, which the package's initialization routine
/// calls to register it with `R_RegisterCCallable(pkg, "r8_exports", ...)`.
/// It is invoked once per package, in the module where each impl's
/// `#[r8::methods]` constant is in scope.
#[proc_macro]
pub fn exports(input: TokenStream) -> TokenStream {
    let args = parse_macro_input!(input as ExportsArgs);
    exports_impl(&args)
        .unwrap_or_else(Error::into_compile_error)
        .into()
}

fn exports_impl(a: &ExportsArgs) -> Result<Tokens> {
    if !valid_name(&a.namespace.value()) {
        return Err(Error::new_spanned(
            &a.namespace,
            "a namespace matches [A-Za-z][A-Za-z0-9_]*",
        ));
    }
    let api = quote! { ::rtemis_r8_api };
    let package_c = c_str(&a.package.value());
    let namespace = &a.namespace;
    let types = &a.types;
    let n_types = types.len();
    let sets: Vec<syn::Ident> = a
        .impls
        .iter()
        .map(|(ty, tr)| {
            let t = &ty.segments.last().expect("a type path").ident;
            let r = &tr.segments.last().expect("a trait path").ident;
            format_ident!("__R8_IMPL_{}_{}", t, r)
        })
        .collect();
    let n_impls = sets.len();
    Ok(quote! {
        static __R8_CODECS: #api::Shared<[#api::rtemis_r8::abi::TypeVTable; #n_types]> =
            #api::Shared([#(<#types as #api::Codec>::TABLE.0),*]);
        static __R8_IMPLS: [&#api::ImplSet; #n_impls] = [#(&#sets),*];
        static __R8_TABLE: ::std::sync::OnceLock<usize> = ::std::sync::OnceLock::new();

        unsafe extern "C" fn __r8_exports() -> *const #api::rtemis_r8::abi::Exports {
            #api::exports::table(
                &__R8_TABLE,
                #package_c,
                #namespace,
                &__R8_CODECS.0,
                // The listed types and every type of the namespace they
                // reach, such as the struct of a newtype variant.
                || {
                    let mut set = #api::rtemis_r8::rules::DocumentSet::default();
                    #(<#types as #api::rtemis_r8::derive::Encode>::register(&mut set);)*
                    set.into_documents()
                        .into_iter()
                        .find(|d| d.namespace.as_str() == #namespace)
                        .map(|d| d.types)
                        .unwrap_or_default()
                },
                &__R8_IMPLS,
            )
        }

        /// Registers the package's R8 exports; the package's
        /// initialization routine calls it.
        ///
        /// # Safety
        ///
        /// Called once, from the package's initialization routine.
        #[unsafe(no_mangle)]
        pub unsafe extern "C" fn r8_init(_dll: *mut ::core::ffi::c_void) {
            unsafe { #api::exports::register(#package_c, __r8_exports) }
        }
    })
}
