//! The registry of declarations, and the conversion of declarations built in
//! R to the IR of `r8-core`.
//!
//! The registry lives in this shared object only: one `DocumentSet`, one IR
//! document per namespace. Declaring a type checks it against the registry
//! and, when it passes, adds it or replaces the type of the same name, so
//! re-evaluating a declaration, as `devtools::load_all()` does, replaces it.

use std::sync::{Mutex, OnceLock};

use extendr_api::prelude::*;
use extendr_api::{Error, Result};
use r8_core::codec::canonical_text;
use r8_core::ir::{IR_VERSION, IrDocument, TypeDef, check_document};
use r8_core::name::{Name, QualifiedName};
use r8_core::rules::DocumentSet;
use serde_json::{Map, Value};

pub(crate) fn registry() -> &'static Mutex<DocumentSet> {
    static REGISTRY: OnceLock<Mutex<DocumentSet>> = OnceLock::new();
    REGISTRY.get_or_init(|| Mutex::new(DocumentSet::default()))
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
            for s in x.as_str_iter().into_iter().flatten() {
                if s.is_na() {
                    return Err(fail(format!("{path}: a declaration holds no NA")));
                }
                out.push(Value::from(s));
            }
            Ok(scalar_or_array(out))
        }
        Rtype::List => {
            let list = x.as_list().unwrap();
            let named = x
                .names()
                .map(|n| n.into_iter().any(|s| !s.is_empty()))
                .unwrap_or(false);
            if named {
                let mut m = Map::new();
                for (k, v) in list.iter() {
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

/// The RFC 8785 canonical text of JSON text.
///
/// @param json Character: JSON text.
/// @return Character: The canonical text.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_canonical_json(json: &str) -> Result<String> {
    let v: Value = serde_json::from_str(json).map_err(|e| fail(format!("not JSON: {e}")))?;
    Ok(canonical_text(&v))
}

/// Checks a type declaration in its namespace and registers it.
///
/// Returns a list with the code, path, and message of each problem; an empty
/// list means the declaration passed and is registered.
///
/// @param type_json Character: The type's IR declaration as JSON text.
/// @param namespace Character: The namespace.
/// @return List: The problems, as character vectors `code`, `path`, `message`.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_declare_type(type_json: &str, namespace: &str) -> Result<List> {
    let def: TypeDef = serde_json::from_str(type_json)
        .map_err(|e| fail(format!("the declaration does not read: {e}")))?;
    let ns = Name::new(namespace).map_err(|e| fail(e.to_string()))?;
    let mut reg = registry()
        .lock()
        .map_err(|_| fail("the registry is unavailable"))?;
    let mut doc = reg
        .0
        .iter()
        .find(|d| d.namespace == ns)
        .cloned()
        .unwrap_or_else(|| IrDocument {
            ir_version: IR_VERSION,
            namespace: ns.clone(),
            types: Vec::new(),
            traits: Vec::new(),
            impls: Vec::new(),
        });
    let name = def.name().clone();
    let index = match doc.types.iter().position(|t| t.name() == &name) {
        Some(i) => {
            doc.types[i] = def;
            i
        }
        None => {
            doc.types.push(def);
            doc.types.len() - 1
        }
    };
    // Report the problems of this declaration only.
    let prefix = format!("/types/{index}");
    let problems: Vec<_> = check_document(&doc)
        .into_iter()
        .filter(|e| e.path == prefix || e.path.starts_with(&format!("{prefix}/")))
        .collect();
    if problems.is_empty() {
        match reg.0.iter().position(|d| d.namespace == ns) {
            Some(i) => reg.0[i] = doc,
            None => reg.0.push(doc),
        }
    }
    let code: Vec<String> = problems.iter().map(|e| e.code.to_string()).collect();
    let path: Vec<String> = problems
        .iter()
        .map(|e| e.path[prefix.len()..].to_string())
        .collect();
    let message: Vec<String> = problems.iter().map(|e| e.message.clone()).collect();
    Ok(list!(code = code, path = path, message = message))
}

/// The IR document of one registered type: its namespace and its
/// declaration.
///
/// @param name Character: The qualified name, `namespace::Type`.
/// @return Character: The IR document as JSON text.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_type_document(name: &str) -> Result<String> {
    let q = QualifiedName::parse(name).map_err(|e| fail(e.to_string()))?;
    let reg = registry()
        .lock()
        .map_err(|_| fail("the registry is unavailable"))?;
    let def = r8_core::rules::TypeLookup::find(&*reg, &q)
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

/// The namespace document of the registry, or an empty one.
fn namespace_doc(reg: &DocumentSet, ns: &Name) -> IrDocument {
    reg.0
        .iter()
        .find(|d| &d.namespace == ns)
        .cloned()
        .unwrap_or_else(|| IrDocument {
            ir_version: IR_VERSION,
            namespace: ns.clone(),
            types: Vec::new(),
            traits: Vec::new(),
            impls: Vec::new(),
        })
}

fn store(reg: &mut DocumentSet, doc: IrDocument) {
    match reg.0.iter().position(|d| d.namespace == doc.namespace) {
        Some(i) => reg.0[i] = doc,
        None => reg.0.push(doc),
    }
}

fn problems_list(problems: &[r8_core::ir::DeclError], prefix: &str) -> List {
    let code: Vec<String> = problems.iter().map(|e| e.code.to_string()).collect();
    let path: Vec<String> = problems
        .iter()
        .map(|e| e.path[prefix.len()..].to_string())
        .collect();
    let message: Vec<String> = problems.iter().map(|e| e.message.clone()).collect();
    list!(code = code, path = path, message = message)
}

/// Checks a trait declaration in its namespace and registers it.
///
/// @param trait_json Character: The trait's IR declaration as JSON text.
/// @param namespace Character: The namespace.
/// @return List: The problems, as character vectors `code`, `path`, `message`.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_declare_trait(trait_json: &str, namespace: &str) -> Result<List> {
    let def: r8_core::ir::TraitDef = serde_json::from_str(trait_json)
        .map_err(|e| fail(format!("the declaration does not read: {e}")))?;
    let ns = Name::new(namespace).map_err(|e| fail(e.to_string()))?;
    let mut reg = registry()
        .lock()
        .map_err(|_| fail("the registry is unavailable"))?;
    let mut doc = namespace_doc(&reg, &ns);
    let index = match doc.traits.iter().position(|t| t.name == def.name) {
        Some(i) => {
            doc.traits[i] = def;
            i
        }
        None => {
            doc.traits.push(def);
            doc.traits.len() - 1
        }
    };
    let prefix = format!("/traits/{index}");
    let problems: Vec<_> = check_document(&doc)
        .into_iter()
        .filter(|e| e.path == prefix || e.path.starts_with(&format!("{prefix}/")))
        .collect();
    if problems.is_empty() {
        store(&mut reg, doc);
    }
    Ok(problems_list(&problems, &prefix))
}

/// Registers an impl in its namespace; an impl of the same trait for the same
/// type replaces it.
///
/// @param impl_json Character: The impl's IR declaration as JSON text.
/// @param namespace Character: The namespace of the declaring code.
/// @return NULL.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_declare_impl(impl_json: &str, namespace: &str) -> Result<()> {
    let def: r8_core::ir::ImplDef = serde_json::from_str(impl_json)
        .map_err(|e| fail(format!("the impl does not read: {e}")))?;
    let ns = Name::new(namespace).map_err(|e| fail(e.to_string()))?;
    let mut reg = registry()
        .lock()
        .map_err(|_| fail("the registry is unavailable"))?;
    let mut doc = namespace_doc(&reg, &ns);
    doc.impls
        .retain(|i| !(i.r#trait == def.r#trait && i.target == def.target));
    doc.impls.push(def);
    store(&mut reg, doc);
    Ok(())
}

fn owners() -> &'static Mutex<std::collections::HashMap<String, String>> {
    static OWNERS: OnceLock<Mutex<std::collections::HashMap<String, String>>> = OnceLock::new();
    OWNERS.get_or_init(|| Mutex::new(std::collections::HashMap::new()))
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
fn r8_claim_namespace(namespace: &str, package: &str) -> Result<String> {
    let mut map = owners()
        .lock()
        .map_err(|_| fail("the registry is unavailable"))?;
    match map.get(namespace) {
        Some(owner) if !owner.is_empty() && owner != package => Ok(owner.clone()),
        Some(owner) if owner.is_empty() && package.is_empty() => Ok(String::new()),
        _ => {
            if !package.is_empty() || !map.contains_key(namespace) {
                map.insert(namespace.to_string(), package.to_string());
            }
            Ok(String::new())
        }
    }
}

extendr_module! {
    mod declare;
    fn r8_claim_namespace;
    fn r8_declare_trait;
    fn r8_declare_impl;
    fn r8_ir_json;
    fn r8_canonical_json;
    fn r8_declare_type;
    fn r8_type_document;
}
