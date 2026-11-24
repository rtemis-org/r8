//! The registry of declarations, and the conversion of declarations built in
//! R to the IR of `rtemis-r8`.
//!
//! The registry lives in this shared object only: one `DocumentSet`, one IR
//! document per namespace. Declaring a type checks it against the registry
//! and, when it passes, adds it or replaces the type of the same name, so
//! re-evaluating a declaration, as `devtools::load_all()` does, replaces it.

use std::sync::{Arc, OnceLock};

use extendr_api::prelude::*;
use extendr_api::{Error, Result};
use rtemis_r8::canonical::canonical_text;
use rtemis_r8::ir::{IR_VERSION, IrDocument, TypeDef};
use rtemis_r8::name::{Name, QualifiedName};
use rtemis_r8::registry::{Registry, SaveToken};
use rtemis_r8::rules::DocumentSet;
use serde_json::{Map, Value};

/// The session's registry (`rtemis_r8::registry`). Readers take a
/// [`snapshot`]; no lock is held outside the registry's own calls.
pub(crate) fn registry() -> &'static Registry {
    static REGISTRY: OnceLock<Registry> = OnceLock::new();
    REGISTRY.get_or_init(Registry::new)
}

/// The registry's documents as they stand.
pub(crate) fn snapshot() -> Result<Arc<DocumentSet>> {
    Ok(registry().snapshot())
}

/// Invalidates the caches that read the registry, after a change.
fn changed(c: bool) {
    if c {
        crate::construct::registry_changed();
    }
}

fn fail(message: impl Into<String>) -> Error {
    Error::Other(message.into())
}

/// Converts an R object built by the declaration constructors to JSON: a
/// named list is an object, an unnamed list an array, a length-one atomic
/// vector a scalar, a longer one an array, and `NULL` is `null`.
fn to_json(x: &Robj, path: &str) -> Result<Value> {
    let scalar_or_array = |items: Vec<Value>| -> Value {
        if items.len() == 1 {
            items.into_iter().next().unwrap()
        } else {
            Value::Array(items)
        }
    };
    // `json_ir()`: JSON text spliced into the IR as written.
    // `int64()`: an integer beyond 2^53, as its decimal digits.
    if x.inherits("r8_int64") {
        let text =
            crate::utf8::one(x).ok_or_else(|| fail(format!("{path}: int64() holds one string")))?;
        return text
            .parse::<i64>()
            .map(Value::from)
            .or_else(|_| text.parse::<u64>().map(Value::from))
            .map_err(|_| fail(format!("{path}: int64() holds the digits of one integer")));
    }
    if x.inherits("r8_json_ir") {
        let text = crate::utf8::one(x)
            .ok_or_else(|| fail(format!("{path}: json_ir() holds one string")))?;
        return serde_json::from_str(&text)
            .map_err(|e| fail(format!("{path}: the text of json_ir() is not JSON: {e}")));
    }
    match x.rtype() {
        Rtype::Null => Ok(Value::Null),
        Rtype::Logicals => {
            let s = x.as_logical_slice().unwrap_or_default();
            if s.iter().any(|b| b.is_na()) {
                return Err(fail(format!("{path}: a declaration holds no NA")));
            }
            Ok(scalar_or_array(
                s.iter().map(|b| Value::Bool(b.is_true())).collect(),
            ))
        }
        Rtype::Integers => {
            let s = x.as_integer_slice().unwrap_or_default();
            if s.contains(&i32::MIN) {
                return Err(fail(format!("{path}: a declaration holds no NA")));
            }
            Ok(scalar_or_array(s.iter().map(|&i| Value::from(i)).collect()))
        }
        Rtype::Doubles => {
            let s = x.as_real_slice().unwrap_or_default();
            let mut out = Vec::with_capacity(s.len());
            for &d in s {
                out.push(
                    serde_json::Number::from_f64(d)
                        .map(Value::Number)
                        .ok_or_else(|| {
                            fail(format!("{path}: a declaration holds finite numbers only"))
                        })?,
                );
            }
            Ok(scalar_or_array(out))
        }
        Rtype::Strings => {
            let mut out = Vec::new();
            for s in crate::utf8::all(x).unwrap_or_default() {
                let Some(s) = s else {
                    return Err(fail(format!("{path}: a declaration holds no NA")));
                };
                out.push(Value::from(s));
            }
            Ok(scalar_or_array(out))
        }
        Rtype::List => {
            let list = x.as_list().unwrap();
            let names = crate::utf8::names(x).filter(|n| n.iter().any(|s| !s.is_empty()));
            if let Some(names) = names {
                let mut m = Map::new();
                for (k, v) in names.iter().zip(list.values()) {
                    m.insert(k.to_string(), to_json(&v, &format!("{path}/{k}"))?);
                }
                Ok(Value::Object(m))
            } else {
                list.values()
                    .enumerate()
                    .map(|(i, v)| to_json(&v, &format!("{path}/{i}")))
                    .collect::<Result<Vec<_>>>()
                    .map(Value::Array)
            }
        }
        other => Err(fail(format!(
            "{path}: a declaration cannot hold an R object of type {other:?}"
        ))),
    }
}

/// JSON text of an R declaration object.
///
/// @param x List: A declaration built by the R8 constructors.
/// @return Character: The JSON text.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_ir_json(x: Robj) -> Result<String> {
    to_json(&x, "").map(|v| v.to_string())
}

/// JSON text as R objects: `null` as `NULL`, a boolean as a logical, an
/// integer of 32 bits as an integer, another integer as its JSON text of class
/// `r8_json_ir` (which `r8_ir_json()` writes back unchanged), any other number
/// as a double, a string as a character, an array as an unnamed list, and an
/// object as a named list. Each number reads back through `r8_ir_json()` as
/// written.
///
/// @param json Character: JSON text.
/// @return The R object.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_json_parse(json: crate::utf8::Utf8) -> Result<Robj> {
    let json: &str = &json;
    let v: Value = serde_json::from_str(json).map_err(|e| fail(format!("not JSON: {e}")))?;
    Ok(from_json(&v))
}

/// The R object of a JSON value, as [`r8_json_parse`] describes.
fn from_json(v: &Value) -> Robj {
    match v {
        Value::Null => Robj::from(()),
        Value::Bool(b) => Robj::from(*b),
        Value::Number(n) => {
            if n.is_f64() {
                Robj::from(n.as_f64().unwrap_or(f64::NAN))
            } else if let Some(i) = n.as_i64().and_then(|i| i32::try_from(i).ok())
                && i != i32::MIN
            {
                Robj::from(i)
            } else {
                let mut r = Robj::from(n.to_string());
                let _ = r.set_class(["r8_json_ir"]);
                r
            }
        }
        Value::String(s) => Robj::from(s.as_str()),
        Value::Array(items) => List::from_values(items.iter().map(from_json)).into(),
        Value::Object(m) => List::from_names_and_values(m.keys(), m.values().map(from_json))
            .map(Robj::from)
            .unwrap_or_else(|_| Robj::from(())),
    }
}

/// The RFC 8785 canonical text of JSON text.
///
/// @param json Character: JSON text.
/// @return Character: The canonical text.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_canonical_json(json: crate::utf8::Utf8) -> Result<String> {
    let json: &str = &json;
    let v: Value = serde_json::from_str(json).map_err(|e| fail(format!("not JSON: {e}")))?;
    Ok(canonical_text(&v))
}

/// Checks a type declaration in its namespace and registers it.
///
/// Returns a list with the code, path, and message of each problem; an empty
/// list means the declaration passed and is registered.
///
/// @param ir List: The type's declaration, as the declaration functions build
///   it, read by the IR types' derives (`irde`).
/// @param namespace Character: The namespace.
/// @return List: The problems, as character vectors `code`, `path`, `message`.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_declare_type(ir: Robj, namespace: crate::utf8::Utf8) -> Result<List> {
    let namespace: &str = &namespace;
    let def: TypeDef = crate::irde::from_r(unsafe { ir.get() })
        .map_err(|e| fail(format!("the declaration does not read: {e}")))?;
    declare_type(def, namespace).map(|(p, _)| p)
}

/// Checks the type declaration `def` in its namespace and registers it, as
/// `r8_declare_type()` does; the problems, and the qualified names of the
/// structs the declaration leaves stale.
pub(crate) fn declare_type(def: TypeDef, namespace: &str) -> Result<(List, Vec<String>)> {
    let ns = Name::new(namespace).map_err(|e| fail(e.to_string()))?;
    let out = registry()
        .declare_type(&ns, def)
        .map_err(|e| fail(e.to_string()))?;
    changed(out.changed);
    let stale = out.stale.iter().map(|q| q.to_string()).collect();
    Ok((problems_list(&out.problems), stale))
}

/// The JSON Schema (draft 2020-12) of the values of the registered type
/// `name` (`value_schema::json_schema_with()`).
///
/// @param name Character: The qualified type name.
/// @param closed Logical: Write each `dyn()` field as the closed list of the
///   registered implementors, and each `subtype()` field as the struct and
///   its registered descendants.
/// @return Character: The schema as indented JSON text.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_type_schema(name: crate::utf8::Utf8, closed: bool) -> Result<String> {
    let name: &str = &name;
    let q = QualifiedName::parse(name).map_err(|e| fail(e.to_string()))?;
    let docs = registry().snapshot();
    if rtemis_r8::rules::TypeLookup::find(&*docs, &q).is_none() {
        return Err(fail(format!("`{name}` is not registered")));
    }
    let schema = rtemis_r8::value_schema::json_schema_with(&q, &*docs, closed);
    serde_json::to_string_pretty(&schema).map_err(|e| fail(e.to_string()))
}

/// The qualified names of the stale structs: those whose inherited fields or
/// rules disagree with their parent's, or whose parent is stale.
///
/// @param namespaces Character: When not empty, only the stale structs
///   outside these namespaces that descend from a type in them.
/// @return Character: The names, sorted.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_stale_types(namespaces: crate::utf8::Utf8s) -> Vec<String> {
    let namespaces: Vec<String> = namespaces.to_vec();
    use rtemis_r8::rules::TypeLookup;
    let docs = registry().snapshot();
    let within = |q: &QualifiedName| namespaces.iter().any(|n| n == q.namespace.as_str());
    docs.stale_types()
        .into_iter()
        .filter(|q| namespaces.is_empty() || (!within(q) && docs.ancestors(q).iter().any(within)))
        .map(|q| q.to_string())
        .collect()
}

/// The IR document of one registered type: its namespace and its
/// declaration.
///
/// @param name Character: The qualified name, `namespace::Type`.
/// @return Character: The IR document as JSON text.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_type_document(name: crate::utf8::Utf8) -> Result<String> {
    let name: &str = &name;
    let q = QualifiedName::parse(name).map_err(|e| fail(e.to_string()))?;
    let reg = registry().snapshot();
    let def = rtemis_r8::rules::TypeLookup::find(&*reg, &q)
        .cloned()
        .ok_or_else(|| fail(format!("`{name}` is not registered")))?;
    let doc = IrDocument {
        ir_version: IR_VERSION,
        namespace: q.namespace,
        types: vec![def],
        traits: Vec::new(),
        impls: Vec::new(),
    };
    Ok(doc.to_json())
}

/// A declaration's problems as R lists, with paths relative to the
/// declaration.
pub(crate) fn problems_list(problems: &[rtemis_r8::ir::DeclError]) -> List {
    let code: Vec<String> = problems.iter().map(|e| e.code.to_string()).collect();
    let path: Vec<String> = problems.iter().map(|e| e.path.clone()).collect();
    let message: Vec<String> = problems.iter().map(|e| e.message.clone()).collect();
    list!(code = code, path = path, message = message)
}

/// Checks a trait declaration in its namespace and registers it.
///
/// @param ir List: The trait's declaration, as `new_trait()` builds it.
/// @param namespace Character: The namespace.
/// @return List: The problems, as character vectors `code`, `path`, `message`.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_declare_trait(ir: Robj, namespace: crate::utf8::Utf8) -> Result<List> {
    let namespace: &str = &namespace;
    let def: rtemis_r8::ir::TraitDef = crate::irde::from_r(unsafe { ir.get() })
        .map_err(|e| fail(format!("the declaration does not read: {e}")))?;
    declare_trait(def, namespace)
}

/// Checks the trait declaration `def` in its namespace and registers it, as
/// `r8_declare_trait()` does.
pub(crate) fn declare_trait(def: rtemis_r8::ir::TraitDef, namespace: &str) -> Result<List> {
    let ns = Name::new(namespace).map_err(|e| fail(e.to_string()))?;
    let out = registry()
        .declare_trait(&ns, def)
        .map_err(|e| fail(e.to_string()))?;
    changed(out.changed);
    Ok(problems_list(&out.problems))
}

/// Registers an impl in its namespace; an impl of the same trait for the same
/// type replaces it.
///
/// @param ir List: The impl's declaration, as `impl()` builds it.
/// @param namespace Character: The namespace of the declaring code.
/// @return NULL.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_declare_impl(ir: Robj, namespace: crate::utf8::Utf8) -> Result<()> {
    let namespace: &str = &namespace;
    let def: rtemis_r8::ir::ImplDef = crate::irde::from_r(unsafe { ir.get() })
        .map_err(|e| fail(format!("the impl does not read: {e}")))?;
    declare_impl(def, namespace)
}

/// Registers the impl `def` in its namespace, as `r8_declare_impl()` does;
/// an impl whose trait's tag values it makes ambiguous is an error.
pub(crate) fn declare_impl(def: rtemis_r8::ir::ImplDef, namespace: &str) -> Result<()> {
    let ns = Name::new(namespace).map_err(|e| fail(e.to_string()))?;
    let out = registry()
        .declare_impl(&ns, def)
        .map_err(|e| fail(e.to_string()))?;
    changed(out.changed);
    if !out.problems.is_empty() {
        let lines: Vec<String> = out.problems.iter().map(|p| p.to_string()).collect();
        return Err(fail(lines.join("\n")));
    }
    Ok(())
}

/// Claims a namespace for the package that declares into it.
///
/// The first package to declare into a namespace owns it; a declaration by
/// another package is refused. Code outside a package (`package = ""`) owns
/// no namespace, and a package may claim a namespace such code declared
/// into.
///
/// @param namespace Character: The namespace.
/// @param package Character: The declaring package, or `""` outside a package.
/// @return Character: `""` when the claim holds, otherwise the owning package.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_claim_namespace(namespace: crate::utf8::Utf8, package: crate::utf8::Utf8) -> Result<String> {
    let namespace: &str = &namespace;
    let package: &str = &package;
    claim_namespace(namespace, package)
}

/// Claims `namespace` for `package`, as `r8_claim_namespace()` does: the
/// owner that holds it already, or `""`.
pub(crate) fn claim_namespace(namespace: &str, package: &str) -> Result<String> {
    Ok(registry().claim(namespace, package).unwrap_or_default())
}

/// The namespaces a package owns.
///
/// @param package Character: The package.
/// @return Character: The namespaces, sorted.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_package_namespaces(package: crate::utf8::Utf8) -> Result<Vec<String>> {
    let package: &str = &package;
    Ok(registry().namespaces_of(package))
}

/// The problems of whole namespaces, each document checked once: what a
/// declaration's own check cannot see, such as a rule that reads a field of
/// a type declared again without that field.
///
/// The impls of these namespaces are also checked against the registry,
/// and two impls of one trait that make a call ambiguous are reported when
/// one belongs to these namespaces and the other to them or to `imports`.
///
/// @param namespaces Character: The namespaces.
/// @param imports Character: The namespaces of the packages the package
///   imports.
/// @return List: The problems, as character vectors `namespace`, `code`,
///   `path`, and `message`.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_check_namespaces(
    namespaces: Robj,
    #[extendr(default = "character()")] imports: Robj,
) -> Result<List> {
    let namespaces =
        crate::text::str_vec(&namespaces).ok_or_else(|| fail("`namespaces` is character"))?;
    let imports = crate::text::str_vec(&imports).unwrap_or_default();
    let (mut ns, mut code, mut path, mut message) = (vec![], vec![], vec![], vec![]);
    for (n, e) in registry().check_namespaces_with(&namespaces, &imports) {
        ns.push(n);
        code.push(e.code.to_string());
        path.push(e.path);
        message.push(e.message);
    }
    Ok(list!(
        namespace = ns,
        code = code,
        path = path,
        message = message
    ))
}

/// The impls of a trait and the dispatched-position count of the trait, from
/// the registry's snapshot.
fn trait_impls(
    docs: &DocumentSet,
    tr: &QualifiedName,
) -> Result<(usize, Vec<(Name, rtemis_r8::ir::ImplDef)>)> {
    use rtemis_r8::rules::TypeLookup;
    let arity = docs
        .trait_of(tr)
        .map(|t| t.dispatch_arity())
        .ok_or_else(|| fail(format!("`{tr}` is not a registered trait")))?;
    let impls = rtemis_r8::ir::dispatch::impls_by_namespace(docs, tr)
        .into_iter()
        .map(|(n, i)| (n.clone(), i.clone()))
        .collect();
    Ok((arity, impls))
}

/// A position as R8's R code keys it: a qualified type name, `any`, or a
/// host class's name in double quotes, `"data.frame"`, which no type name
/// and no `any` can equal.
fn position_string(p: &rtemis_r8::ir::TypeRef) -> String {
    match p {
        rtemis_r8::ir::TypeRef::HostClass { class, .. } => format!("\"{class}\""),
        other => other.to_string(),
    }
}

/// The positions of an impl as text (`position_string()`).
fn tuple_strings(imp: &rtemis_r8::ir::ImplDef, arity: usize) -> Vec<String> {
    rtemis_r8::ir::dispatch::positions(imp, arity)
        .into_iter()
        .map(position_string)
        .collect()
}

/// Selects the impl for a call of a trait among its registered impls, with the core's
/// rule of multiple dispatch.
///
/// @param trait_name Character: The trait's qualified name.
/// @param types Character: The R8 type of each dispatched argument, `NA`
///   for a value of no R8 type.
/// @param chains List: For each argument of no R8 type, its class vector
///   as `.class2()` gives it; `NULL` for a value of an R8 type.
/// @param above Optional Character: The positions of the running impl, for
///   the next impl.
/// @return Character: the providing impl's positions; `NULL` when no impl
///   applies and the trait's default implementation serves the call; otherwise
///   a list: `kind` (`"ambiguous"` or `"stale"`), `tuple` and `other` (the two
///   impls of an ambiguous pair), `meet` (their positionwise more specific
///   tuple), and `stale` (a stale type).
/// @keywords internal
/// @noRd
#[extendr]
fn r8_dispatch_select(
    trait_name: crate::utf8::Utf8,
    types: Robj,
    chains: List,
    above: Robj,
) -> Result<Robj> {
    let trait_name: &str = &trait_name;
    use rtemis_r8::ir::dispatch::{Arg, Selection, select};
    use rtemis_r8::rules::TypeLookup;
    let tr = QualifiedName::parse(trait_name).map_err(|e| fail(e.to_string()))?;
    let docs = snapshot()?;
    let arity = docs
        .trait_of(&tr)
        .map(|t| t.dispatch_arity())
        .ok_or_else(|| fail(format!("`{tr}` is not a registered trait")))?;
    let impls: Vec<&rtemis_r8::ir::ImplDef> =
        rtemis_r8::ir::dispatch::impls_by_namespace(&docs, &tr)
            .into_iter()
            .map(|(_, i)| i)
            .collect();
    let strings = crate::utf8::all(&types).ok_or_else(|| fail("`types` is character"))?;
    let args: Vec<Arg> = strings
        .into_iter()
        .zip(chains.values())
        .map(|(s, chain)| match s {
            None => crate::utf8::all(&chain).map_or(Arg::Other, |c| {
                Arg::Classes(c.into_iter().flatten().collect())
            }),
            Some(s) => QualifiedName::parse(&s).map_or(Arg::Other, Arg::Type),
        })
        .collect();
    let above = match crate::utf8::all(&above) {
        Some(v) => Some(
            impls
                .iter()
                .position(|i| {
                    tuple_strings(i, arity)
                        .iter()
                        .map(Some)
                        .eq(v.iter().map(Option::as_ref))
                })
                .ok_or_else(|| fail("the running impl is not registered"))?,
        ),
        None => None,
    };
    let none: Vec<String> = Vec::new();
    let (kind, tuple, other, meet, stale) = match select(&*docs, &impls, arity, &args, above) {
        Selection::Impl(k) => return Ok(Robj::from(tuple_strings(impls[k], arity))),
        Selection::Default => return Ok(Robj::from(())),
        Selection::Ambiguous(a, b) => {
            let (ta, tb) = (
                tuple_strings(impls[a], arity),
                tuple_strings(impls[b], arity),
            );
            let meet = ta
                .iter()
                .zip(&tb)
                .zip(rtemis_r8::ir::dispatch::positions(impls[a], arity))
                .zip(rtemis_r8::ir::dispatch::positions(impls[b], arity))
                .map(|(((x, y), px), py)| {
                    let below =
                        |p: &rtemis_r8::ir::TypeRef, q: &rtemis_r8::ir::TypeRef| match (p, q) {
                            (rtemis_r8::ir::TypeRef::Any, _) => false,
                            (_, rtemis_r8::ir::TypeRef::Any) => true,
                            (
                                rtemis_r8::ir::TypeRef::Named { name: n },
                                rtemis_r8::ir::TypeRef::Named { name: m },
                            ) => rtemis_r8::rules::TypeLookup::is_subtype(&*docs, n, m),
                            _ => false,
                        };
                    if below(py, px) { y.clone() } else { x.clone() }
                })
                .collect();
            ("ambiguous", ta, tb, meet, None)
        }
        Selection::Stale(q) => (
            "stale",
            none.clone(),
            none.clone(),
            none,
            Some(q.to_string()),
        ),
    };
    Ok(list!(
        kind = kind,
        tuple = tuple,
        other = other,
        meet = meet,
        stale = stale
    )
    .into())
}

/// The ambiguous pairs among a trait's registered impls.
///
/// @param trait_name Character: The trait's qualified name.
/// @return data.frame: `first`, `second`, and `meet`, each a tuple as
///   `"(ns::A, ns::B)"`, and `namespaces`, the namespaces that declare the
///   pair.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_dispatch_ambiguities(trait_name: crate::utf8::Utf8) -> Result<Robj> {
    let trait_name: &str = &trait_name;
    let tr = QualifiedName::parse(trait_name).map_err(|e| fail(e.to_string()))?;
    let docs = snapshot()?;
    let (arity, found) = trait_impls(&docs, &tr)?;
    let impls: Vec<&rtemis_r8::ir::ImplDef> = found.iter().map(|(_, i)| i).collect();
    let text = |ps: Vec<String>| format!("({})", ps.join(", "));
    let (mut first, mut second, mut meet, mut namespaces) = (vec![], vec![], vec![], vec![]);
    for a in rtemis_r8::ir::dispatch::ambiguities(&*docs, &impls, arity, &|_, _| true) {
        first.push(text(tuple_strings(impls[a.first], arity)));
        second.push(text(tuple_strings(impls[a.second], arity)));
        meet.push(text(a.meet.iter().map(position_string).collect()));
        let (na, nb) = (&found[a.first].0, &found[a.second].0);
        namespaces.push(if na == nb {
            na.to_string()
        } else {
            format!("{na}, {nb}")
        });
    }
    let n = first.len() as i32;
    let df = list!(
        first = first,
        second = second,
        meet = meet,
        namespaces = namespaces
    );
    let mut df: Robj = df.into();
    df.set_attrib("class", "data.frame")?;
    df.set_attrib("row.names", (1..=n).collect::<Vec<i32>>())?;
    Ok(df)
}

/// The package that owns a namespace, without claiming it.
///
/// @param namespace Character: The namespace.
/// @return Character: The owning package, or `""` when no package owns it.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_namespace_owner(namespace: crate::utf8::Utf8) -> Result<String> {
    let namespace: &str = &namespace;
    Ok(registry().owner(namespace).unwrap_or_default())
}

/// The declaration of a registered type as R lists, in the form the
/// declaration functions build (`irser`).
///
/// @param name Character: The qualified name, `namespace::Type`.
/// @return List: The declaration.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_type_ir(name: crate::utf8::Utf8) -> Result<Robj> {
    let name: &str = &name;
    let q = QualifiedName::parse(name).map_err(|e| fail(e.to_string()))?;
    let reg = registry().snapshot();
    let def = rtemis_r8::rules::TypeLookup::find(&*reg, &q)
        .ok_or_else(|| fail(format!("`{name}` is not registered")))?;
    crate::irser::to_r(def).map_err(|e| fail(e.to_string()))
}

/// The absent fields of a struct value that resolution fills, each named by
/// its field and holding its policy, `"context"` or `"computed"`.
///
/// @param x An R8 value.
/// @return Character: The policies, named by field.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_pending_fields(x: Robj) -> Result<Robj> {
    use rtemis_r8::ir::Resolve;
    let xs = unsafe { x.get() };
    let type_name = crate::value::type_name(xs).ok_or_else(|| fail("not an R8 value"))?;
    let q = QualifiedName::parse(&type_name).map_err(|e| fail(e.to_string()))?;
    let reg = registry().snapshot();
    let (mut names, mut policies) = (Vec::new(), Vec::new());
    if let Some(TypeDef::Struct(s)) = rtemis_r8::rules::TypeLookup::find(&*reg, &q) {
        for (i, f) in s.fields.iter().enumerate() {
            let policy = match &f.resolve {
                Some(Resolve::Computed { .. }) => "computed",
                Some(Resolve::Context) => "context",
                None => continue,
            };
            let absent = i >= crate::rview::xlen(xs)
                || crate::rview::is_null(unsafe { extendr_ffi::VECTOR_ELT(xs, i as isize) });
            if absent {
                names.push(f.name.as_str().to_string());
                policies.push(policy);
            }
        }
    }
    let mut out = Robj::from(policies);
    if !names.is_empty() {
        out.set_names(names)?;
    }
    Ok(out)
}

/// The declaration record of the registered type `q` and of every named
/// type it reaches, the reached types first (`rtemis_r8::record`).
pub(crate) fn type_record(q: &QualifiedName) -> Result<Vec<u8>> {
    registry()
        .type_record(q)
        .ok_or_else(|| fail(format!("`{q}` is not registered")))
}

/// Whether a type is registered.
///
/// @param name Character: The qualified name.
/// @return Logical.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_is_registered(name: crate::utf8::Utf8) -> bool {
    let name: &str = &name;
    QualifiedName::parse(name)
        .is_ok_and(|q| rtemis_r8::rules::TypeLookup::find(&*registry().snapshot(), &q).is_some())
}

/// Declares the entries of declaration records, in order, through the
/// declaration checks: one call for a package's records at load, or for the
/// record a type object carries. The records of a package declare into the
/// namespaces it owns and into namespaces no package owns; an entry of a
/// namespace another package owns is skipped.
///
/// @param records List: Raw vectors, each a record.
/// @param source Character: The package the records come from, or `""`.
/// @param check Logical: Check each declaration; `FALSE` at package load,
///   where `check_namespaces()` checks the package's namespaces whole after
///   every declaration is registered.
/// @return List: `name`, `code`, `path`, and `message` of each problem.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_declare_records(records: List, source: crate::utf8::Utf8, check: bool) -> Result<List> {
    let source: &str = &source;
    use rtemis_r8::record;
    let from = if source.is_empty() {
        "a type object".to_string()
    } else {
        format!("package '{source}'")
    };
    let mut all = Vec::new();
    for r in records.values() {
        let bytes = r.as_raw_slice().ok_or_else(|| {
            fail(format!(
                "a declaration record of {from} is not a raw vector"
            ))
        })?;
        let entries = record::decode(bytes).map_err(|e| {
            let reinstall = if source.is_empty() {
                String::new()
            } else {
                format!("; reinstall '{source}'")
            };
            fail(format!(
                "the declarations of {from} do not read: {e}{reinstall}"
            ))
        })?;
        all.extend(entries);
    }
    // A package's records carry the declarations their types reach, among
    // them types of other packages' namespaces: the owner's own records
    // declare those, so an earlier copy never replaces the owner's.
    if !source.is_empty() {
        let reg = registry();
        all.retain(|e| match reg.owner(e.namespace.as_str()) {
            Some(owner) => owner == source,
            None => true,
        });
    }
    let (c, problems) = registry()
        .declare_records(all, check)
        .map_err(|e| fail(e.to_string()))?;
    changed(c);
    let (mut name, mut code, mut path, mut message) = (vec![], vec![], vec![], vec![]);
    for p in problems {
        name.push(p.name);
        code.push(p.problem.code.to_string());
        path.push(p.problem.path);
        message.push(p.problem.message);
    }
    Ok(list!(
        name = name,
        code = code,
        path = path,
        message = message
    ))
}

/// The registered declaration of the trait `name`, `namespace::Trait`.
fn find_trait<'a>(reg: &'a DocumentSet, name: &str) -> Result<&'a rtemis_r8::ir::TraitDef> {
    let q = QualifiedName::parse(name).map_err(|e| fail(e.to_string()))?;
    reg.namespace(&q.namespace)
        .and_then(|d| d.traits.iter().find(|t| t.name == q.name))
        .ok_or_else(|| fail(format!("`{name}` is not registered")))
}

/// The declaration record of a registered trait (`rtemis_r8::record`).
///
/// @param name Character: The qualified name, `namespace::Trait`.
/// @return Raw: The record.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_trait_record(name: crate::utf8::Utf8) -> Result<Robj> {
    let name: &str = &name;
    use rtemis_r8::record::{self, DeclarationRef};
    let reg = registry().snapshot();
    let t = find_trait(&reg, name)?;
    let ns = QualifiedName::parse(name)
        .map_err(|e| fail(e.to_string()))?
        .namespace;
    Ok(Robj::from(
        record::encode([(&ns, DeclarationRef::Trait(t))]).as_slice(),
    ))
}

/// The declaration record of an impl, read from the R lists `impl()` builds.
///
/// @param ir List: The impl's declaration.
/// @param namespace Character: The namespace of the declaring code.
/// @return Raw: The record.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_impl_record(ir: Robj, namespace: crate::utf8::Utf8) -> Result<Robj> {
    let namespace: &str = &namespace;
    use rtemis_r8::record::{self, DeclarationRef};
    let def: rtemis_r8::ir::ImplDef = crate::irde::from_r(unsafe { ir.get() })
        .map_err(|e| fail(format!("the impl does not read: {e}")))?;
    let ns = Name::new(namespace).map_err(|e| fail(e.to_string()))?;
    Ok(Robj::from(
        record::encode([(&ns, DeclarationRef::Impl(&def))]).as_slice(),
    ))
}

/// The declaration of a registered trait as R lists (`irser`).
///
/// @param name Character: The qualified name, `namespace::Trait`.
/// @return List: The declaration.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_trait_ir(name: crate::utf8::Utf8) -> Result<Robj> {
    let name: &str = &name;
    let reg = registry().snapshot();
    let t = find_trait(&reg, name)?;
    crate::irser::to_r(t).map_err(|e| fail(e.to_string()))
}

/// Saves the registry's documents and namespace owners, which
/// `r8_registry_restore()` brings back and `r8_registry_release()`
/// discards; saves nest.
///
/// @return Double: The saved state's token.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_registry_save() -> f64 {
    registry().save().0 as f64
}

/// Restores the registry to the saved state `token`, the most recent.
///
/// @param token Double: The token of `r8_registry_save()`.
/// @return `NULL`.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_registry_restore(token: f64) -> Result<()> {
    let before = registry().generation();
    registry()
        .restore(SaveToken(token as u64))
        .map_err(|e| fail(e.to_string()))?;
    changed(registry().generation() != before);
    Ok(())
}

/// Discards the saved state `token`, the most recent.
///
/// @param token Double: The token of `r8_registry_save()`.
/// @return `NULL`.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_registry_release(token: f64) -> Result<()> {
    registry()
        .release(SaveToken(token as u64))
        .map_err(|e| fail(e.to_string()))
}

/// The problem of a variant's parts, or `""`: a newtype variant takes
/// neither fields nor rules, and rules need fields to read
/// (`rtemis_r8::ir::build::variant_parts()`).
///
/// @param has_of,has_fields,has_rules Logical: What the variant holds.
/// @return Character: The message, or `""`.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_variant_problem(has_of: bool, has_fields: bool, has_rules: bool) -> String {
    match rtemis_r8::ir::build::variant_parts(has_of, has_fields, has_rules) {
        Ok(()) => String::new(),
        Err(e) => e.to_string(),
    }
}

/// Why `x` is not an R8 name, or `""` for a name: one string that
/// `rtemis_r8::name::Name::new()` accepts, the grammar every host shares.
///
/// @param x Object.
/// @return Character: The reason, or `""`.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_name_problem(x: Robj) -> String {
    match crate::utf8::one(&x) {
        Some(s) => match Name::new(s) {
            Ok(_) => String::new(),
            Err(e) => e.to_string(),
        },
        _ => "a name is one string".to_string(),
    }
}

extendr_module! {
    mod declare;
    fn r8_registry_save;
    fn r8_registry_restore;
    fn r8_registry_release;
    fn r8_trait_record;
    fn r8_impl_record;
    fn r8_trait_ir;
    fn r8_is_registered;
    fn r8_declare_records;
    fn r8_type_ir;
    fn r8_pending_fields;
    fn r8_claim_namespace;
    fn r8_name_problem;
    fn r8_variant_problem;
    fn r8_namespace_owner;
    fn r8_package_namespaces;
    fn r8_check_namespaces;
    fn r8_dispatch_select;
    fn r8_dispatch_ambiguities;
    fn r8_declare_trait;
    fn r8_declare_impl;
    fn r8_ir_json;
    fn r8_canonical_json;
    fn r8_json_parse;
    fn r8_declare_type;
    fn r8_stale_types;
    fn r8_type_schema;
    fn r8_type_document;
}
