//! What each package's compiled code was compiled against: the declarations of
//! the types whose values it reads by their fields, by the module that names
//! the code (`R8Codec.module`).
//!
//! The record of a package's exports holds its own types and those of other
//! namespaces that its declarations and its signatures reach; `R8Api.pin` adds
//! a type that a method's body alone names. [`check`] admits a read by fields
//! when the registered declaration refines the compiled one
//! (`rtemis_r8::refine`), which is computed once per type and change of the
//! registry.
//! spec: R8/class-system#reads-across-packages

use std::collections::HashMap;
use std::ffi::c_void;
use std::sync::atomic::Ordering;
use std::sync::{Arc, Mutex, MutexGuard, OnceLock};

use rtemis_r8::ir::TypeDef;
use rtemis_r8::name::{Name, QualifiedName};
use rtemis_r8::record::{self, Declaration};
use rtemis_r8::refine::{Difference, differences};
use rtemis_r8::rules::{DocumentSet, TypeLookup};

use crate::construct::GENERATION;
use crate::declare::registry;

/// The compiled code of a package.
struct Module {
    package: String,
    compiled: DocumentSet,
    /// Of each type that was compared, the generation of the registry, and the
    /// report of its differences, if it has any.
    verdicts: HashMap<String, (u64, Option<Arc<str>>)>,
}

/// The modules by address.
fn modules() -> MutexGuard<'static, HashMap<usize, Module>> {
    static MODULES: OnceLock<Mutex<HashMap<usize, Module>>> = OnceLock::new();
    MODULES
        .get_or_init(|| Mutex::new(HashMap::new()))
        .lock()
        .unwrap_or_else(|e| e.into_inner())
}

/// Keeps the types of the exports of `package`, whose compiled code is
/// `module`, in place of those of its earlier code.
pub(crate) fn put(package: &str, module: *const c_void, types: Vec<(Name, TypeDef)>) {
    let mut compiled = DocumentSet::default();
    for (ns, t) in types {
        compiled.put_type(&ns, t);
    }
    let mut held = modules();
    held.retain(|_, m| m.package != package);
    held.insert(
        module as usize,
        Module {
            package: package.to_string(),
            compiled,
            verdicts: HashMap::new(),
        },
    );
}

/// `R8Api.pin`: adds the types of a declaration record that `module` does not
/// hold.
pub(crate) fn add(module: *const c_void, bytes: &[u8]) -> Result<(), (&'static str, String)> {
    let entries = record::decode(bytes)
        .map_err(|e| ("bad_record", format!("the record does not read: {e}")))?;
    let mut held = modules();
    let Some(m) = held.get_mut(&(module as usize)) else {
        return Err((
            "unknown_module",
            "R8 does not hold the exports of the calling code; its package is unloaded, or was registered again".to_string(),
        ));
    };
    for e in entries {
        let Declaration::Type(t) = e.declaration else {
            return Err(("bad_record", "the record declares no type".to_string()));
        };
        let q = QualifiedName {
            namespace: e.namespace.clone(),
            name: t.name().clone(),
        };
        if m.compiled.find(&q).is_none() {
            m.compiled.put_type(&e.namespace, t);
        }
    }
    Ok(())
}

/// The package that declares into the namespace of `q`, as a message names it.
fn declaring_package(q: &QualifiedName) -> Option<String> {
    registry().owner(q.namespace.as_str())
}

/// The report of `found`, the differences of the type `q` for the package
/// `package`.
fn report(package: &str, q: &QualifiedName, found: &[Difference]) -> String {
    const SHOWN: usize = 4;
    let mut lines: Vec<&str> = found
        .iter()
        .take(SHOWN)
        .map(|d| d.message.as_str())
        .collect();
    let more = found.len().saturating_sub(SHOWN);
    let rest = format!("and {more} more");
    if more > 0 {
        lines.push(&rest);
    }
    let owner = match declaring_package(q) {
        Some(p) if p != package => format!("package '{p}'"),
        _ => format!("the package that declares `{q}`"),
    };
    format!(
        "package '{package}' was compiled against another declaration of `{q}` than the registered one: {}. Install versions of '{package}' and of {owner} that were built for each other",
        lines.join("; ")
    )
}

/// Whether `module` may read the fields of a value of the type `type_name`:
/// `not_pinned` when R8 does not hold the declaration that it was compiled
/// against, and `declaration_changed` when the registered one does not refine
/// it.
pub(crate) fn check(module: *const c_void, type_name: &str) -> Result<(), (&'static str, String)> {
    let mut held = modules();
    let not_pinned = || {
        (
            "not_pinned",
            format!("R8 does not hold the compiled declaration of `{type_name}`"),
        )
    };
    let Some(m) = held.get_mut(&(module as usize)) else {
        return Err(not_pinned());
    };
    let now = GENERATION.load(Ordering::Relaxed);
    let verdict = match m.verdicts.get(type_name) {
        Some((g, v)) if *g == now => v.clone(),
        _ => {
            let q = QualifiedName::parse(type_name).map_err(|e| ("unknown_type", e.to_string()))?;
            if m.compiled.find(&q).is_none() {
                return Err(not_pinned());
            }
            let found = differences(&q, &m.compiled, &*registry().snapshot());
            let v: Option<Arc<str>> =
                (!found.is_empty()).then(|| report(&m.package, &q, &found).into());
            m.verdicts.insert(type_name.to_string(), (now, v.clone()));
            v
        }
    };
    match verdict {
        None => Ok(()),
        Some(v) => Err(("declaration_changed", v.to_string())),
    }
}

/// Whether the registered declaration of the type `name` is the one that
/// `module` was compiled against; `true` for a type that `module` does not hold.
pub(crate) fn is_current(module: *const c_void, name: &str) -> bool {
    let Ok(q) = QualifiedName::parse(name) else {
        return true;
    };
    let held = modules();
    let Some(compiled) = held
        .get(&(module as usize))
        .and_then(|m| m.compiled.find(&q))
    else {
        return true;
    };
    registry()
        .snapshot()
        .find(&q)
        .is_some_and(|r| rtemis_r8::canonical::same_declaration(compiled, r))
}

/// The types of other namespaces than `ns` that `module` was compiled against,
/// that are registered, and whose registered declarations do not refine the
/// compiled ones, with the differences of each.
pub(crate) fn changed(module: *const c_void, ns: &Name) -> Vec<(String, Vec<Difference>)> {
    let held = modules();
    let Some(m) = held.get(&(module as usize)) else {
        return Vec::new();
    };
    let docs = registry().snapshot();
    let mut out = Vec::new();
    for d in m.compiled.documents().iter().filter(|d| d.namespace != *ns) {
        for t in &d.types {
            let q = QualifiedName {
                namespace: d.namespace.clone(),
                name: t.name().clone(),
            };
            if docs.find(&q).is_none() {
                continue;
            }
            // A type that the type holds is reported for itself.
            let mut found = differences(&q, &m.compiled, &*docs);
            let name = q.to_string();
            found.retain(|f| f.type_name == name);
            if !found.is_empty() {
                out.push((name, found));
            }
        }
    }
    out
}
