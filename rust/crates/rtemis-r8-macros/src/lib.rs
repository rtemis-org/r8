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
//! `tag = "type"` on an enum, `extensible` on a struct that others extend,
//! and `rule(id = "...", invariant = "<JSONLogic>",
//! when = "<JSONLogic>", message = "...")`, repeated per rule. On a field:
//! `min`, `max`, `exclusive_min`, `exclusive_max`, `pattern`, `finite`,
//! `min_length`, `max_length`, `unique`, `default = "<JSON>"`, `unit`,
//! `sensitive`, and `context`, for a field of type `Option<T>` that
//! `r8_resolve(x, context)` fills. Doc comments are
//! descriptions.
//!
//! A struct extends another by embedding it: its first field, marked
//! `#[r8(extends)]`, holds the parent, a struct of `#[r8(extensible)]`, and
//! the R8 type has the parent's fields and rules, then its own.
//! `inherited(name(default = "<JSON>", sensitive, description = "..."))`
//! beside the mark changes an inherited field; the default `null` removes
//! the inherited one. The derive implements `rtemis_r8::derive::Subtype` for
//! the struct, and for an extensible struct `P` the trait `PLike`: an
//! accessor per field, `to_p()`, and `with_p(value)`, implemented for `P`
//! and every struct that extends it. The types of a child's own fields are
//! `Clone`.
//!
//! The derive builds the declaration as `rtemis_r8::ir` values. A rule's
//! JSONLogic and a field's default are parsed when the crate compiles, so a
//! text that does not read is a compile error at its attribute.
//!
//! `#[r8(storage = "rust")]` on a struct also implements `rtemis_r8_api::RustStoredStruct`, and
//! on an enum `rtemis_r8_api::RustStoredEnum`; on both, `rtemis_r8_api::RustStored`, so
//! R8 holds the type's values in Rust: each field's type implements
//! `rtemis_r8_api::Field`, a `vec()` field of numbers, logicals, or bytes is
//! a `rtemis_r8_api::Buffer`, and a newtype variant's struct implements
//! `rtemis_r8_api::RustStoredStruct`. `#[r8::methods]` and `r8::exports!`
//! register the struct and its impls with R8.

mod exports;
mod extends;
mod methods;

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

/// The R8 name of a field's identifier.
fn field_name(ident: &syn::Ident) -> String {
    ident.to_string().trim_start_matches("r#").to_string()
}

/// The identifier that generated code binds a field to, which no parameter
/// and no other variable of the generated code has.
fn bound(ident: &syn::Ident) -> syn::Ident {
    format_ident!("__r8_field_{}", field_name(ident))
}

/// The construction of the tree of a default, JSON text; `None` for `null`.
fn default_tree(s: &syn::LitStr) -> Result<Option<Tokens>> {
    let v: serde_json::Value = serde_json::from_str(&s.value())
        .map_err(|e| Error::new_spanned(s, format!("a default is JSON text: {e}")))?;
    Ok((!v.is_null()).then(|| tree_tokens(&v)))
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
    let ir = quote! { ::rtemis_r8::ir };
    if let Some(u) = n.as_u64() {
        quote! { #ir::Number::U64(#u) }
    } else if let Some(i) = n.as_i64() {
        quote! { #ir::Number::I64(#i) }
    } else {
        let f = n.as_f64().expect("a JSON number is finite");
        quote! { #ir::Number::F64(#f) }
    }
}

/// The construction of an `rtemis_r8::tree::Tree` of a JSON value, its
/// numbers kept as written.
fn tree_tokens(v: &serde_json::Value) -> Tokens {
    use serde_json::Value as V;
    let t = quote! { ::rtemis_r8::tree::Tree };
    match v {
        V::Null => quote! { #t::Null },
        V::Bool(b) => quote! { #t::Bool(#b) },
        V::Number(n) => {
            let n = number_tokens(n);
            quote! { #t::Num(#n) }
        }
        V::String(s) => quote! { #t::Str(#s.into()) },
        V::Array(a) => {
            let items = a.iter().map(tree_tokens);
            quote! { #t::Array(::std::boxed::Box::new([#(#items),*])) }
        }
        V::Object(m) => {
            let (k, x): (Vec<_>, Vec<_>) = m.iter().map(|(k, x)| (k, tree_tokens(x))).unzip();
            quote! { #t::Object(::std::boxed::Box::new([#((#k.into(), #x)),*])) }
        }
    }
}

/// The construction of an `rtemis_r8::ir::Logic` tree from a parsed
/// expression, as its deserializer reads it.
fn logic_tokens(v: &serde_json::Value) -> Tokens {
    use serde_json::Value as V;
    let ir = quote! { ::rtemis_r8::ir };
    let other = |v: &V| {
        let v = tree_tokens(v);
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
    rust_stored: bool,
    extensible: bool,
}

/// One `rule(invariant = ..., when = ..., message = ..., id = ...)`, as the
/// construction of an `rtemis_r8::ir::RuleDef`; the expressions are
/// JSONLogic text, parsed when the crate compiles, and `invariant` alone is
/// required.
fn parse_rule(meta: &syn::meta::ParseNestedMeta) -> Result<Tokens> {
    let (mut id, mut invariant, mut when, mut message) = (None, None, None, None);
    meta.parse_nested_meta(|m| {
        let s: syn::LitStr = m.value()?.parse()?;
        match m.path.get_ident().map(|i| i.to_string()).as_deref() {
            Some("id") => id = Some(s),
            Some("invariant") => invariant = Some(s),
            Some("when") => when = Some(s),
            Some("message") => message = Some(s),
            _ => return Err(m.error("a rule takes invariant, when, message, and id")),
        }
        Ok(())
    })?;
    let invariant = logic_attr(&invariant.ok_or_else(|| meta.error("a rule needs `invariant`"))?)?;
    let id = match id {
        Some(id) => {
            if !valid_name(&id.value()) {
                return Err(Error::new_spanned(
                    &id,
                    "a rule ID matches [A-Za-z][A-Za-z0-9_]*",
                ));
            }
            quote! { ::std::option::Option::Some(::rtemis_r8::derive::name(#id)) }
        }
        None => quote! { ::std::option::Option::None },
    };
    let message = match message {
        Some(m) => {
            if m.value().trim().is_empty() {
                return Err(Error::new_spanned(&m, "a rule's message is not blank"));
            }
            quote! { ::std::option::Option::Some(#m.to_string()) }
        }
        None => quote! { ::std::option::Option::None },
    };
    let when = match when {
        Some(w) => {
            let w = logic_attr(&w)?;
            quote! { ::std::option::Option::Some(#w) }
        }
        None => quote! { ::std::option::Option::None },
    };
    Ok(quote! {
        ::rtemis_r8::ir::RuleDef {
            invariant: #invariant,
            when: #when,
            message: #message,
            id: #id,
            inherited_from: ::std::option::Option::None,
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
                Some("extensible") => out.extensible = true,
                Some("storage") => {
                    let s: syn::LitStr = meta.value()?.parse()?;
                    match s.value().as_str() {
                        "rust" => out.rust_stored = true,
                        "r" => {}
                        _ => {
                            return Err(Error::new_spanned(&s, "storage is \"r\" or \"rust\""));
                        }
                    }
                }
                _ => {
                    return Err(meta
                        .error("expected namespace, version, tag, extensible, rule, or storage"));
                }
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
    let name = field_name(ident);
    if !valid_name(&name) {
        return Err(Error::new_spanned(
            ident,
            "a field name matches [A-Za-z][A-Za-z0-9_]*",
        ));
    }
    let ty = &field.ty;
    let mut constraints: Vec<Tokens> = Vec::new();
    let mut sensitive = false;
    let mut unit = quote! { ::std::option::Option::None };
    let mut default = quote! { ::std::option::Option::None };
    let mut resolve = quote! { ::std::option::Option::<::rtemis_r8::ir::Resolve>::None };
    for a in field.attrs.iter().filter(|a| a.path().is_ident("r8")) {
        a.parse_nested_meta(|meta| {
            let key = meta
                .path
                .get_ident()
                .map(|i| i.to_string())
                .unwrap_or_default();
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
                "unit" => {
                    let s: syn::LitStr = meta.value()?.parse()?;
                    unit = quote! { ::std::option::Option::Some(#s.into()) };
                }
                "default" => {
                    let s: syn::LitStr = meta.value()?.parse()?;
                    let v =
                        default_tree(&s)?.unwrap_or_else(|| tree_tokens(&serde_json::Value::Null));
                    default = quote! { ::std::option::Option::Some(::std::boxed::Box::new(#v)) };
                }
                "context" => {
                    resolve =
                        quote! { ::std::option::Option::Some(::rtemis_r8::ir::Resolve::Context) };
                }
                _ => return Err(meta.error("unknown R8 field attribute")),
            }
            Ok(())
        })?;
    }
    let desc = description(&field.attrs);
    Ok(quote! {
        ::rtemis_r8::ir::FieldDef {
            name: ::rtemis_r8::derive::name(#name),
            ty: ::rtemis_r8::derive::constrain(
                <#ty as ::rtemis_r8::derive::Encode>::field_type(),
                ::rtemis_r8::derive::Constraints {
                    #(#constraints,)*
                    ..::std::default::Default::default()
                },
                #resolve.is_some(),
            )
            .unwrap_or_else(|e| panic!("field `{}`: {}", #name, e)),
            default: #default,
            description: #desc.into(),
            sensitive: #sensitive,
            unit: #unit,
            resolve: #resolve,
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

/// Encoding, decoding, and registration of named fields: each field bound
/// to its `bound()` identifier is written into `__r8_m`, the object of the
/// JSON form, and read from `v`.
fn field_json(fields: &[&syn::Field]) -> (Vec<Tokens>, Vec<Tokens>, Vec<Tokens>) {
    let mut encode = Vec::new();
    let mut decode = Vec::new();
    let mut register = Vec::new();
    for f in fields {
        let ident = f.ident.as_ref().unwrap();
        let ty = &f.ty;
        let name = field_name(ident);
        let var = bound(ident);
        encode.push(quote! {
            __r8_m.insert(#name.to_string(), ::rtemis_r8::derive::Encode::encode(#var).map_err(|e| format!("{}: {}", #name, e))?);
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
            let (parent, fields) = extends::split(&fields_of(&s.fields)?)?;
            let defs = fields
                .iter()
                .map(|f| field_def(f))
                .collect::<Result<Vec<_>>>()?;
            let (enc, dec, reg) = field_json(&fields);
            let idents: Vec<_> = fields.iter().map(|f| f.ident.as_ref().unwrap()).collect();
            let vars: Vec<_> = idents.iter().map(|i| bound(i)).collect();
            let def = quote! {
                ::rtemis_r8::ir::StructDef {
                    name: ::rtemis_r8::derive::name(#type_name),
                    description: #desc.to_string(),
                    version: #version,
                    fields: ::std::vec![#(#defs),*],
                    extends: ::std::option::Option::None,
                    rules: ::std::vec![#(#rules),*],
                    migrations: ::std::vec::Vec::new(),
                    aliases: ::std::vec::Vec::new(),
                    extensions: ::std::collections::BTreeMap::new(),
                }
            };
            match &parent {
                None => (
                    quote! { ::rtemis_r8::ir::TypeDef::Struct(#def) },
                    quote! {
                        let Self { #(#idents: #vars),* } = self;
                        let mut __r8_m = ::rtemis_r8::derive::serde_json::Map::new();
                        #(#enc)*
                        Ok(::rtemis_r8::derive::serde_json::Value::Object(__r8_m))
                    },
                    quote! { Ok(Self { #(#dec),* }) },
                    quote! { #(#reg)* },
                ),
                Some(p) => {
                    let (base, ty, changes) = (p.ident, p.ty, &p.changes);
                    (
                        quote! {
                            ::rtemis_r8::derive::extended::<#ty>(#def, &[#(#changes),*])
                        },
                        quote! {
                            let Self { #base: __r8_base, #(#idents: #vars),* } = self;
                            let mut __r8_m = ::rtemis_r8::derive::inherited_members(__r8_base)?;
                            #(#enc)*
                            Ok(::rtemis_r8::derive::serde_json::Value::Object(__r8_m))
                        },
                        quote! {
                            Ok(Self {
                                #base: <#ty as ::rtemis_r8::derive::Encode>::decode(v)?,
                                #(#dec),*
                            })
                        },
                        quote! {
                            <#ty as ::rtemis_r8::derive::Encode>::register(set);
                            #(#reg)*
                        },
                    )
                }
            }
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
                                kind: ::rtemis_r8::ir::VariantKind::Unit,
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
                        extends::none(&fields)?;
                        let defs = fields
                            .iter()
                            .map(|f| field_def(f))
                            .collect::<Result<Vec<_>>>()?;
                        let (enc, dec, r) = field_json(&fields);
                        let idents: Vec<_> =
                            fields.iter().map(|f| f.ident.as_ref().unwrap()).collect();
                        let vars: Vec<_> = idents.iter().map(|i| bound(i)).collect();
                        variant_defs.push(quote! {
                            ::rtemis_r8::ir::VariantDef {
                                name: ::rtemis_r8::derive::name(#vname),
                                description: #vdesc.to_string(),
                                kind: ::rtemis_r8::ir::VariantKind::Struct {
                                    fields: ::std::vec![#(#defs),*],
                                    rules: ::std::vec![#(#vrules),*],
                                },
                            }
                        });
                        enc_arms.push(quote! {
                            Self::#vid { #(#idents: #vars),* } => {
                                let mut __r8_m = ::rtemis_r8::derive::serde_json::Map::new();
                                #(#enc)*
                                __r8_m.insert(#tag.to_string(), #vname.into());
                                Ok(::rtemis_r8::derive::serde_json::Value::Object(__r8_m))
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
                                kind: ::rtemis_r8::ir::VariantKind::Newtype {
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
    let rust_stored = if attrs.rust_stored {
        match &input.data {
            Data::Struct(st) => {
                let (parent, fields) = extends::split(&fields_of(&st.fields)?)?;
                rust_stored_impl(ident, &qualified, parent.as_ref(), &fields)?
            }
            Data::Enum(e) => {
                rust_stored_enum_impl(ident, &qualified, e, attrs.tag.as_deref().unwrap_or("type"))?
            }
            Data::Union(_) => {
                return Err(Error::new(
                    Span::call_site(),
                    "#[r8(storage = \"rust\")] applies to a struct or an enum",
                ));
            }
        }
    } else {
        quote! {}
    };
    let inheritance = match &input.data {
        Data::Struct(st) => {
            let (parent, own) = extends::split(&fields_of(&st.fields)?)?;
            let of_parent = parent.as_ref().map(|p| {
                let required = extends::parent_is_extensible(p);
                let subtype = extends::subtype_of_parent(ident, p, &own);
                quote! { #required #subtype }
            });
            let interface = if attrs.extensible {
                extends::interface(ident, &input.vis, parent.as_ref(), &own)?
            } else {
                quote! {}
            };
            quote! { #of_parent #interface }
        }
        _ if attrs.extensible => return Err(extends::enum_is_closed()),
        _ => quote! {},
    };
    Ok(quote! {
        #rust_stored
        #inheritance
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

/// `impl rtemis_r8_api::RustStoredEnum` and `impl rtemis_r8_api::RustStored` for an
/// enum of `#[r8(storage = "rust")]`: unit variants, struct variants whose fields are
/// each a `Field`, and newtype variants whose struct implements `RustStoredStruct`.
/// The state's bytes are the variant's declaration index, a `u32`, then its
/// fields.
fn rust_stored_enum_impl(
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
                let vars: Vec<_> = idents.iter().map(|i| bound(i)).collect();
                let names: Vec<String> = idents.iter().map(|i| field_name(i)).collect();
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
                            #i: <#t as #api::Field>::from_node(r, r.member(root, #name_c))
                                .map_err(|e| #api::Error::new(e.code, format!("field `{}`: {}", #name, e.message)))?
                        }
                    })
                    .collect();
                from_node.push(quote! { #vname => Ok(Self::#vid { #(#node_conv),* }) });
                variant.push(quote! { Self::#vid { .. } => #vname_c });
                get.push(quote! {
                    Self::#vid { #(#idents: #vars),* } => match name {
                        #(#names => #api::Field::to_r(#vars, owner),)*
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
                            .zip(&vars)
                            .filter(|(o, _)| *o != i)
                            .map(|(o, var)| quote! { #o: ::std::clone::Clone::clone(#var) })
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
                    Self::#vid { #(#idents: #vars),* } => match name {
                        #(#with_arms,)*
                        _ => #unknown_field,
                    }
                });
                write.push(quote! {
                    Self::#vid { #(#idents: #vars),* } => {
                        out.write(&#idx.to_le_bytes())?;
                        #(#api::Field::write(#vars, out)?;)*
                        Ok(())
                    }
                });
                read.push(quote! {
                    #idx => Ok(Self::#vid { #(#idents: <#tys as #api::Field>::read(input)?),* })
                });
                bytes.push(quote! {
                    Self::#vid { #(#idents: #vars),* } => 0 #(+ #api::Field::owned_bytes(#vars))*
                });
            }
            Fields::Unnamed(u) if u.unnamed.len() == 1 => {
                let inner = &u.unnamed[0].ty;
                from.push(quote! {
                    #vname => Ok(Self::#vid(unsafe { <#inner as #api::RustStoredStruct>::from_fields(fields) }?))
                });
                from_node.push(quote! {
                    #vname => Ok(Self::#vid(<#inner as #api::RustStoredStruct>::from_reader(r, root)?))
                });
                variant.push(quote! { Self::#vid(_) => #vname_c });
                get.push(quote! { Self::#vid(inner) => <#inner as #api::RustStoredStruct>::get_field(inner, owner, name) });
                with.push(quote! {
                    Self::#vid(inner) => Ok(Self::#vid(unsafe { <#inner as #api::RustStoredStruct>::with_field(inner, name, value) }?))
                });
                write.push(quote! {
                    Self::#vid(inner) => {
                        out.write(&#idx.to_le_bytes())?;
                        <#inner as #api::RustStoredStruct>::write_state(inner, out)
                    }
                });
                read.push(
                    quote! { #idx => Ok(Self::#vid(<#inner as #api::RustStoredStruct>::read_state(input)?)) },
                );
                bytes.push(
                    quote! { Self::#vid(inner) => <#inner as #api::RustStoredStruct>::owned_bytes(inner) },
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
        impl #api::RustStored for #ident {
            const TYPE_NAME: &'static ::core::ffi::CStr = #type_c;
            const TABLE: #api::Shared<#api::rtemis_r8::abi::Codec> = #api::enum_codec::<Self>();
            unsafe fn from_value(x: #api::sys::SEXP, fields: &[#api::sys::SEXP]) -> #api::Result<Self> {
                let variant = unsafe { #api::call::r_variant(x) }?;
                unsafe { <Self as #api::RustStoredEnum>::from_variant(&variant, fields) }
            }
        }
        #[allow(unused_variables)]
        impl #api::RustStoredEnum for #ident {
            const TYPE_NAME: &'static ::core::ffi::CStr = #type_c;
            unsafe fn from_variant(variant: &str, fields: &[#api::sys::SEXP]) -> #api::Result<Self> {
                match variant {
                    #(#from,)*
                    _ => Err(#api::Error::new("unknown_variant", format!("`{}` has no variant `{}`", #qualified, variant))),
                }
            }
            fn from_reader<'a>(r: &#api::Reader<'a>, root: #api::Node<'a>) -> #api::Result<Self> {
                let variant = if #unit_only { r.string(root)? } else { r.string(r.member(root, #tag_c))? };
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

/// `impl rtemis_r8_api::RustStoredStruct` for a struct of
/// `#[r8(storage = "rust")]`. A struct with an embedded parent delegates the
/// inherited fields, which precede its own in the fields R8 passes and in
/// the state's bytes, to the parent's implementation.
fn rust_stored_impl(
    ident: &syn::Ident,
    qualified: &str,
    parent: Option<&extends::Parent>,
    fields: &[&syn::Field],
) -> Result<Tokens> {
    let type_c = c_str(qualified);
    let n = fields.len();
    let idents: Vec<_> = fields.iter().map(|f| f.ident.as_ref().unwrap()).collect();
    let names: Vec<String> = idents.iter().map(|i| field_name(i)).collect();
    let tys: Vec<_> = fields.iter().map(|f| &f.ty).collect();
    let api = quote! { ::rtemis_r8_api };
    let base_ident = parent.map(|p| p.ident);
    let base: Vec<_> = parent.map(|p| p.ident).into_iter().collect();
    let base_ty: Vec<_> = parent.map(|p| p.ty).into_iter().collect();
    let from: Vec<Tokens> = idents
        .iter()
        .zip(&tys)
        .zip(&names)
        .enumerate()
        .map(|(k, ((i, t), name))| {
            quote! {
                #i: unsafe { <#t as #api::Field>::from_r(__r8_own[#k]) }
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
                #i: <#t as #api::Field>::from_node(r, r.member(root, #name_c))
                    .map_err(|e| #api::Error::new(e.code, format!("field `{}`: {}", #name, e.message)))?
            }
        })
        .collect();
    let with_arms: Vec<Tokens> = idents
        .iter()
        .zip(&tys)
        .zip(&names)
        .map(|((i, t), name)| {
            let others: Vec<Tokens> = base_ident
                .iter()
                .copied()
                .chain(idents.iter().copied().filter(|o| o != i))
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
    let unknown = quote! {
        Err(#api::Error::new("unknown_field", format!("`{}` has no field `{}`", #qualified, name)))
    };
    let (get_rest, with_rest) = match parent {
        None => (unknown.clone(), unknown),
        Some(p) => {
            let (b, t) = (p.ident, p.ty);
            (
                quote! { <#t as #api::RustStoredStruct>::get_field(&self.#b, owner, name) },
                quote! {
                    Ok(Self {
                        #b: unsafe { <#t as #api::RustStoredStruct>::with_field(&self.#b, name, value) }?,
                        #(#idents: ::std::clone::Clone::clone(&self.#idents)),*
                    })
                },
            )
        }
    };
    let inherited = match parent {
        None => quote! { 0 },
        Some(p) => {
            let t = p.ty;
            quote! { <#t as #api::RustStoredStruct>::N_FIELDS }
        }
    };
    Ok(quote! {
        impl #api::RustStored for #ident {
            const TYPE_NAME: &'static ::core::ffi::CStr = #type_c;
            const TABLE: #api::Shared<#api::rtemis_r8::abi::Codec> = #api::codec::<Self>();
            unsafe fn from_value(_x: #api::sys::SEXP, fields: &[#api::sys::SEXP]) -> #api::Result<Self> {
                let n = <Self as #api::RustStoredStruct>::N_FIELDS.min(fields.len());
                unsafe { <Self as #api::RustStoredStruct>::from_fields(&fields[..n]) }
            }
        }
        impl #api::RustStoredStruct for #ident {
            const TYPE_NAME: &'static ::core::ffi::CStr = #type_c;
            const N_FIELDS: usize = #inherited + #n;
            unsafe fn from_fields(fields: &[#api::sys::SEXP]) -> #api::Result<Self> {
                let expected = <Self as #api::RustStoredStruct>::N_FIELDS;
                if fields.len() != expected {
                    return Err(#api::Error::new(
                        "arity",
                        format!("`{}` has {} fields, and R8 passed {}", #qualified, expected, fields.len()),
                    ));
                }
                let (__r8_inherited, __r8_own) = fields.split_at(expected - #n);
                Ok(Self {
                    #(#base: unsafe { <#base_ty as #api::RustStoredStruct>::from_fields(__r8_inherited) }?,)*
                    #(#from),*
                })
            }
            fn from_reader<'a>(r: &#api::Reader<'a>, root: #api::Node<'a>) -> #api::Result<Self> {
                Ok(Self {
                    #(#base: <#base_ty as #api::RustStoredStruct>::from_reader(r, root)?,)*
                    #(#from_node),*
                })
            }
            fn get_field(&self, owner: #api::sys::SEXP, name: &str) -> #api::Result<#api::sys::SEXP> {
                match name {
                    #(#names => #api::Field::to_r(&self.#idents, owner),)*
                    _ => #get_rest,
                }
            }
            unsafe fn with_field(&self, name: &str, value: #api::sys::SEXP) -> #api::Result<Self> {
                match name {
                    #(#with_arms,)*
                    _ => #with_rest,
                }
            }
            fn write_state(&self, out: &mut #api::StateWriter) -> #api::Result<()> {
                #(<#base_ty as #api::RustStoredStruct>::write_state(&self.#base, out)?;)*
                #(#api::Field::write(&self.#idents, out)?;)*
                Ok(())
            }
            fn read_state(input: &mut #api::StateReader) -> #api::Result<Self> {
                Ok(Self {
                    #(#base: <#base_ty as #api::RustStoredStruct>::read_state(input)?,)*
                    #(#idents: <#tys as #api::Field>::read(input)?),*
                })
            }
            fn owned_bytes(&self) -> usize {
                0
                #(+ <#base_ty as #api::RustStoredStruct>::owned_bytes(&self.#base))*
                #(+ #api::Field::owned_bytes(&self.#idents))*
            }
            #(
                fn upcast(&self, type_name: &::core::ffi::CStr) -> *const ::core::ffi::c_void {
                    if type_name == <#base_ty as #api::RustStoredStruct>::TYPE_NAME {
                        ::core::ptr::from_ref(&self.#base).cast()
                    } else {
                        <#base_ty as #api::RustStoredStruct>::upcast(&self.#base, type_name)
                    }
                }
            )*
        }
    })
}

/// `#[r8::methods(trait = "ns::Trait")]` on a Rust trait, the Rust side of an
/// R8 trait: keeps the trait, and adds the hidden function that gives
/// `r8::exports!` the functions R8 calls for an impl. An impl is an ordinary
/// Rust impl, a blanket impl included, and `r8::exports!` lists the types it
/// is registered for. A method takes `&self` and arguments that implement
/// `rtemis_r8_api::FromR` or are `&T` of a Rust-stored type `T`
/// (`rtemis_r8_api::Receiver`), and returns a type that implements
/// `rtemis_r8_api::IntoR`, a Rust-stored type among them; the trait is
/// declared in R, whose method has the same parameters.
///
/// A type parameter of the trait is a dispatched position after the first,
/// which `dispatch(x, y)` beside `trait` names, the first argument first:
///
/// ```text
/// #[r8::methods(trait = "ns::overlaps", dispatch(x, y))]
/// pub trait Overlaps<Y> {
///     fn overlaps(&self, y: &Y) -> bool;
/// }
/// ```
///
/// Its argument is `&Y`, and a row names the type, `IRanges:
/// Overlaps<GRanges>`, or any value, `IRanges: Overlaps<r8::Any>`. A
/// replacement method `f<-` is `with_f(&self, value)`.
#[proc_macro_attribute]
pub fn methods(attr: TokenStream, item: TokenStream) -> TokenStream {
    let item = parse_macro_input!(item as syn::Item);
    methods::args(attr.into())
        .and_then(|args| methods::expand(&args, item))
        .unwrap_or_else(Error::into_compile_error)
        .into()
}

fn valid_qualified(s: &str) -> bool {
    matches!(s.split_once("::"), Some((a, b)) if valid_name(a) && valid_name(b))
}

/// `r8::exports! { package = "pkg", namespace = "ns", types = [T, ...],
/// impls = [T: Trait, T: Trait<Y>, ...] }`: the package's exports table, built on first
/// use, and `r8_init_<package>(dll)`, with an underscore for a period of the
/// package's name, which the package's initialization routine calls to
/// register it with `R_RegisterCCallable(pkg, "r8_exports", ...)`. It is
/// invoked once per package. A crate that invokes it is a dependency of
/// another one, whose rows may name its traits.
#[proc_macro]
pub fn exports(input: TokenStream) -> TokenStream {
    let args = parse_macro_input!(input as exports::Args);
    exports::expand(&args)
        .unwrap_or_else(Error::into_compile_error)
        .into()
}
