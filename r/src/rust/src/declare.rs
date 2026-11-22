//! The registry of declarations, and the conversion of declarations built in
//! R to the IR of `rtemis-r8`.
//!
//! The registry lives in this shared object only: one `DocumentSet`, one IR
//! document per namespace. Declaring a type checks it against the registry
//! and, when it passes, adds it or replaces the type of the same name, so
//! re-evaluating a declaration, as `devtools::load_all()` does, replaces it.

use std::sync::{Arc, Mutex, OnceLock};

use extendr_api::prelude::*;
use extendr_api::{Error, Result};
use rtemis_r8::codec::canonical_text;
use rtemis_r8::ir::{IR_VERSION, IrDocument, TypeDef, check_document, check_trait, check_type};
use rtemis_r8::name::{Name, QualifiedName};
use rtemis_r8::rules::DocumentSet;
use serde_json::{Map, Value};

/// The registry. Readers that call back into R take a [`snapshot`], which
/// holds the registry as it stands without its lock; a writer changes the
/// registry through `Arc::make_mut`, which copies it only while a snapshot is
/// alive.
pub(crate) fn registry() -> &'static Mutex<Arc<DocumentSet>> {
    static REGISTRY: OnceLock<Mutex<Arc<DocumentSet>>> = OnceLock::new();
    REGISTRY.get_or_init(|| Mutex::new(Arc::new(DocumentSet::default())))
}

/// The registry as it stands, without its lock.
pub(crate) fn snapshot() -> Result<Arc<DocumentSet>> {
    registry()
        .lock()
        .map(|g| Arc::clone(&g))
        .map_err(|_| fail("the registry is unavailable"))
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
        let text = x
            .as_str()
            .ok_or_else(|| fail(format!("{path}: int64() holds one string")))?;
        return text
            .parse::<i64>()
            .map(Value::from)
            .or_else(|_| text.parse::<u64>().map(Value::from))
            .map_err(|_| fail(format!("{path}: int64() holds the digits of one integer")));
    }
    if x.inherits("r8_json_ir") {
        let text = x
            .as_str()
            .ok_or_else(|| fail(format!("{path}: json_ir() holds one string")))?;
        return serde_json::from_str(text)
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
fn r8_json_parse(json: &str) -> Result<Robj> {
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
fn r8_canonical_json(json: &str) -> Result<String> {
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
fn r8_declare_type(ir: Robj, namespace: &str) -> Result<List> {
    let def: TypeDef = crate::irde::from_r(unsafe { ir.get() })
        .map_err(|e| fail(format!("the declaration does not read: {e}")))?;
    declare_type(def, namespace)
}

/// Checks the type declaration `def` in its namespace and registers it, as
/// `r8_declare_type()` does.
pub(crate) fn declare_type(def: TypeDef, namespace: &str) -> Result<List> {
    let ns = Name::new(namespace).map_err(|e| fail(e.to_string()))?;
    let mut guard = registry()
        .lock()
        .map_err(|_| fail("the registry is unavailable"))?;
    let q = QualifiedName {
        namespace: ns.clone(),
        name: def.name().clone(),
    };
    if rtemis_r8::rules::TypeLookup::find(&**guard, &q) == Some(&def) {
        return Ok(problems_list(&[], ""));
    }
    let reg = Arc::make_mut(&mut guard);
    let placed = reg.put_type(&ns, def);
    let (d, index) = (placed.document, placed.index);
    let prefix = format!("/types/{index}");
    let problems = checked(|| check_type(reg.document(d), index));
    match &problems {
        Ok(p) if p.is_empty() => crate::construct::registry_changed(),
        _ => reg.undo_put(placed),
    }
    let problems = problems?;
    Ok(problems_list(&problems, &prefix))
}

/// Runs a declaration's check, a panic of which reports an error and leaves
/// the registry's lock usable.
fn checked(
    check: impl FnOnce() -> Vec<rtemis_r8::ir::DeclError>,
) -> Result<Vec<rtemis_r8::ir::DeclError>> {
    std::panic::catch_unwind(std::panic::AssertUnwindSafe(check))
        .map_err(|_| fail("the declaration's check failed; the registry is unchanged"))
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
    let def = rtemis_r8::rules::TypeLookup::find(&**reg, &q)
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

pub(crate) fn problems_list(problems: &[rtemis_r8::ir::DeclError], prefix: &str) -> List {
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
/// @param ir List: The trait's declaration, as `new_trait()` builds it.
/// @param namespace Character: The namespace.
/// @return List: The problems, as character vectors `code`, `path`, `message`.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_declare_trait(ir: Robj, namespace: &str) -> Result<List> {
    let def: rtemis_r8::ir::TraitDef = crate::irde::from_r(unsafe { ir.get() })
        .map_err(|e| fail(format!("the declaration does not read: {e}")))?;
    declare_trait(def, namespace)
}

/// Checks the trait declaration `def` in its namespace and registers it, as
/// `r8_declare_trait()` does.
pub(crate) fn declare_trait(def: rtemis_r8::ir::TraitDef, namespace: &str) -> Result<List> {
    let ns = Name::new(namespace).map_err(|e| fail(e.to_string()))?;
    let mut guard = registry()
        .lock()
        .map_err(|_| fail("the registry is unavailable"))?;
    if guard
        .namespace(&ns)
        .is_some_and(|doc| doc.traits.contains(&def))
    {
        return Ok(problems_list(&[], ""));
    }
    let reg = Arc::make_mut(&mut guard);
    let d = reg.document_position(&ns);
    let traits = reg.traits_mut(d);
    let existing = traits.iter().position(|t| t.name == def.name);
    let (index, previous) = match existing {
        Some(i) => (i, Some(std::mem::replace(&mut traits[i], def))),
        None => {
            traits.push(def);
            (traits.len() - 1, None)
        }
    };
    let prefix = format!("/traits/{index}");
    let problems = checked(|| check_trait(reg.document(d), index));
    match &problems {
        Ok(p) if p.is_empty() => crate::construct::registry_changed(),
        _ => {
            let traits = reg.traits_mut(d);
            match previous {
                Some(p) => traits[index] = p,
                None => {
                    traits.pop();
                }
            }
            reg.remove_document_if_empty(d);
        }
    }
    let problems = problems?;
    Ok(problems_list(&problems, &prefix))
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
fn r8_declare_impl(ir: Robj, namespace: &str) -> Result<()> {
    let def: rtemis_r8::ir::ImplDef = crate::irde::from_r(unsafe { ir.get() })
        .map_err(|e| fail(format!("the impl does not read: {e}")))?;
    declare_impl(def, namespace)
}

/// Registers the impl `def` in its namespace, as `r8_declare_impl()` does.
pub(crate) fn declare_impl(def: rtemis_r8::ir::ImplDef, namespace: &str) -> Result<()> {
    let ns = Name::new(namespace).map_err(|e| fail(e.to_string()))?;
    let mut guard = registry()
        .lock()
        .map_err(|_| fail("the registry is unavailable"))?;
    if guard
        .namespace(&ns)
        .is_some_and(|doc| doc.impls.contains(&def))
    {
        return Ok(());
    }
    let reg = Arc::make_mut(&mut guard);
    let d = reg.document_position(&ns);
    let impls = reg.impls_mut(d);
    impls.retain(|i| {
        !(i.r#trait == def.r#trait && i.type_args == def.type_args && i.target == def.target)
    });
    impls.push(def);
    crate::construct::registry_changed();
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
    claim_namespace(namespace, package)
}

/// Claims `namespace` for `package`, as `r8_claim_namespace()` does: the
/// owner that holds it already, or `""`.
pub(crate) fn claim_namespace(namespace: &str, package: &str) -> Result<String> {
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

/// The namespaces a package owns.
///
/// @param package Character: The package.
/// @return Character: The namespaces, sorted.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_package_namespaces(package: &str) -> Result<Vec<String>> {
    let map = owners()
        .lock()
        .map_err(|_| fail("the registry is unavailable"))?;
    let mut out: Vec<String> = map
        .iter()
        .filter(|(_, owner)| owner.as_str() == package)
        .map(|(ns, _)| ns.clone())
        .collect();
    out.sort();
    Ok(out)
}

/// The problems of whole namespaces, each document checked once: what a
/// declaration's own check cannot see, such as a rule that reads a field of
/// a type declared again without that field.
///
/// @param namespaces Character: The namespaces.
/// @return List: The problems, as character vectors `namespace`, `code`,
///   `path`, and `message`.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_check_namespaces(namespaces: Robj) -> Result<List> {
    let namespaces =
        crate::text::str_vec(&namespaces).ok_or_else(|| fail("`namespaces` is character"))?;
    let reg = registry()
        .lock()
        .map_err(|_| fail("the registry is unavailable"))?;
    let (mut ns, mut code, mut path, mut message) = (vec![], vec![], vec![], vec![]);
    for n in &namespaces {
        if let Some(doc) = reg.documents().iter().find(|d| d.namespace.as_str() == n) {
            for e in check_document(doc) {
                ns.push(n.clone());
                code.push(e.code.to_string());
                path.push(e.path);
                message.push(e.message);
            }
        }
    }
    Ok(list!(
        namespace = ns,
        code = code,
        path = path,
        message = message
    ))
}

/// The package that owns a namespace, without claiming it.
///
/// @param namespace Character: The namespace.
/// @return Character: The owning package, or `""` when no package owns it.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_namespace_owner(namespace: &str) -> Result<String> {
    let map = owners()
        .lock()
        .map_err(|_| fail("the registry is unavailable"))?;
    Ok(map.get(namespace).cloned().unwrap_or_default())
}

/// The declaration of a registered type as R lists, in the form the
/// declaration functions build (`irser`).
///
/// @param name Character: The qualified name, `namespace::Type`.
/// @return List: The declaration.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_type_ir(name: &str) -> Result<Robj> {
    let q = QualifiedName::parse(name).map_err(|e| fail(e.to_string()))?;
    let reg = registry()
        .lock()
        .map_err(|_| fail("the registry is unavailable"))?;
    let def = rtemis_r8::rules::TypeLookup::find(&**reg, &q)
        .ok_or_else(|| fail(format!("`{name}` is not registered")))?;
    crate::irser::to_r(def).map_err(|e| fail(e.to_string()))
}

/// The absent fields of a struct value that resolution fills, each named by
/// its field and holding its policy, `"runtime"` or `"computed"`.
///
/// @param x An R8 value.
/// @return Character: The policies, named by field.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_pending_fields(x: Robj) -> Result<Robj> {
    use rtemis_r8::ir::{FieldType, Resolve};
    let xs = unsafe { x.get() };
    let type_name = crate::rview::attr_string(xs, crate::rview::sym_r8_type())
        .ok_or_else(|| fail("not an R8 value"))?;
    let q = QualifiedName::parse(&type_name).map_err(|e| fail(e.to_string()))?;
    let reg = registry()
        .lock()
        .map_err(|_| fail("the registry is unavailable"))?;
    let (mut names, mut policies) = (Vec::new(), Vec::new());
    if let Some(TypeDef::Struct(s)) = rtemis_r8::rules::TypeLookup::find(&**reg, &q) {
        for (i, f) in s.fields.iter().enumerate() {
            let policy = match &f.ty {
                FieldType::Option {
                    resolve: Resolve::Computed { .. },
                    ..
                } => "computed",
                FieldType::Option {
                    resolve: Resolve::Runtime,
                    ..
                } => "runtime",
                _ => continue,
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

/// The qualified names of `q` and of every named type it reaches through
/// field types and newtype variants, each once, the reached types before the
/// types that reach them.
fn reached(reg: &DocumentSet, q: &QualifiedName, out: &mut Vec<QualifiedName>) {
    use rtemis_r8::ir::{FieldDef, FieldType, VariantShape};
    fn of_type(reg: &DocumentSet, t: &FieldType, out: &mut Vec<QualifiedName>) {
        match t {
            FieldType::Named { name }
            | FieldType::MatrixOf { of: name }
            | FieldType::TableOf { of: name } => reached(reg, name, out),
            FieldType::Matrix { element, .. }
            | FieldType::Array { element, .. }
            | FieldType::SparseMatrix { element, .. }
            | FieldType::Vec { element, .. } => of_type(reg, element, out),
            FieldType::Option { inner, .. } => of_type(reg, inner, out),
            FieldType::Hashmap { value, .. } | FieldType::OrderedMap { value, .. } => {
                of_type(reg, value, out)
            }
            FieldType::Table { columns, .. } => of_fields(reg, columns, out),
            _ => {}
        }
    }
    fn of_fields(reg: &DocumentSet, fields: &[FieldDef], out: &mut Vec<QualifiedName>) {
        for f in fields {
            of_type(reg, &f.ty, out);
        }
    }
    if out.contains(q) {
        return;
    }
    let Some(def) = rtemis_r8::rules::TypeLookup::find(reg, q) else {
        return;
    };
    // Marked before its fields are walked, so a recursive type terminates.
    out.push(q.clone());
    let mark = out.len() - 1;
    match def {
        TypeDef::Struct(s) => of_fields(reg, &s.fields, out),
        TypeDef::Enum(e) => {
            for v in &e.variants {
                match &v.shape {
                    VariantShape::Struct { fields, .. } => of_fields(reg, fields, out),
                    VariantShape::Newtype { of } => reached(reg, of, out),
                    VariantShape::Unit => {}
                }
            }
        }
    }
    // The type follows the types it reaches.
    let me = out.remove(mark);
    out.push(me);
}

/// The declaration record of the registered type `q` and of every named
/// type it reaches, the reached types first (`rtemis_r8::record`).
pub(crate) fn type_record(q: &QualifiedName) -> Result<Vec<u8>> {
    use rtemis_r8::record::{self, DeclarationRef};
    let reg = registry()
        .lock()
        .map_err(|_| fail("the registry is unavailable"))?;
    let mut names = Vec::new();
    reached(&reg, q, &mut names);
    if names.is_empty() {
        return Err(fail(format!("`{q}` is not registered")));
    }
    let bytes = record::encode(names.iter().filter_map(|n| {
        rtemis_r8::rules::TypeLookup::find(&**reg, n)
            .map(|d| (&n.namespace, DeclarationRef::Type(d)))
    }));
    Ok(bytes)
}

/// Whether a type is registered.
///
/// @param name Character: The qualified name.
/// @return Logical.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_is_registered(name: &str) -> bool {
    QualifiedName::parse(name).is_ok_and(|q| {
        registry()
            .lock()
            .is_ok_and(|reg| rtemis_r8::rules::TypeLookup::find(&**reg, &q).is_some())
    })
}

/// Declares the entries of declaration records, in order, through the
/// declaration checks: one call for a package's records at load, or for the
/// record a type object carries.
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
fn r8_declare_records(records: List, source: &str, check: bool) -> Result<List> {
    use rtemis_r8::record::{self, Declaration};
    let from = if source.is_empty() {
        "a type object".to_string()
    } else {
        format!("package '{source}'")
    };
    let (mut name, mut code, mut path, mut message) = (vec![], vec![], vec![], vec![]);
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
        for entry in entries {
            let ns = entry.namespace.to_string();
            if !check {
                register_unchecked(entry.declaration, &entry.namespace)?;
                continue;
            }
            let (label, problems) = match entry.declaration {
                Declaration::Type(t) => (format!("{ns}::{}", t.name()), declare_type(t, &ns)?),
                Declaration::Trait(t) => (format!("{ns}::{}", t.name), declare_trait(t, &ns)?),
                Declaration::Impl(i) => {
                    declare_impl(i, &ns)?;
                    continue;
                }
            };
            let codes: Vec<String> = problems
                .dollar("code")
                .ok()
                .and_then(|x| x.as_string_vector())
                .unwrap_or_default();
            let paths: Vec<String> = problems
                .dollar("path")
                .ok()
                .and_then(|x| x.as_string_vector())
                .unwrap_or_default();
            let messages: Vec<String> = problems
                .dollar("message")
                .ok()
                .and_then(|x| x.as_string_vector())
                .unwrap_or_default();
            for ((c, p), m) in codes.into_iter().zip(paths).zip(messages) {
                name.push(label.clone());
                code.push(c);
                path.push(p);
                message.push(m);
            }
        }
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
fn r8_trait_record(name: &str) -> Result<Robj> {
    use rtemis_r8::record::{self, DeclarationRef};
    let reg = registry()
        .lock()
        .map_err(|_| fail("the registry is unavailable"))?;
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
fn r8_impl_record(ir: Robj, namespace: &str) -> Result<Robj> {
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
fn r8_trait_ir(name: &str) -> Result<Robj> {
    let reg = registry()
        .lock()
        .map_err(|_| fail("the registry is unavailable"))?;
    let t = find_trait(&reg, name)?;
    crate::irser::to_r(t).map_err(|e| fail(e.to_string()))
}

/// Registers a declaration without its check, for a package's records at
/// load, which `check_namespaces()` checks whole afterward.
fn register_unchecked(d: rtemis_r8::record::Declaration, ns: &Name) -> Result<()> {
    use rtemis_r8::record::Declaration;
    let mut guard = registry()
        .lock()
        .map_err(|_| fail("the registry is unavailable"))?;
    match d {
        Declaration::Type(t) => {
            let q = QualifiedName {
                namespace: ns.clone(),
                name: t.name().clone(),
            };
            if rtemis_r8::rules::TypeLookup::find(&**guard, &q) != Some(&t) {
                Arc::make_mut(&mut guard).put_type(ns, t);
                crate::construct::registry_changed();
            }
        }
        Declaration::Trait(t) => {
            if !guard
                .namespace(ns)
                .is_some_and(|doc| doc.traits.contains(&t))
            {
                let reg = Arc::make_mut(&mut guard);
                let d = reg.document_position(ns);
                let traits = reg.traits_mut(d);
                match traits.iter().position(|x| x.name == t.name) {
                    Some(i) => traits[i] = t,
                    None => traits.push(t),
                }
                crate::construct::registry_changed();
            }
        }
        Declaration::Impl(i) => {
            drop(guard);
            declare_impl(i, ns.as_str())?;
        }
    }
    Ok(())
}

/// Saved states of the registry, the most recent last.
fn saved() -> &'static Mutex<Vec<Arc<DocumentSet>>> {
    static SAVED: OnceLock<Mutex<Vec<Arc<DocumentSet>>>> = OnceLock::new();
    SAVED.get_or_init(|| Mutex::new(Vec::new()))
}

/// Saves the registry's state, which `r8_registry_restore()` brings back
/// and `r8_registry_release()` discards; saves nest.
///
/// @return `NULL`.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_registry_save() -> Result<()> {
    let snap = snapshot()?;
    saved()
        .lock()
        .map_err(|_| fail("the registry is unavailable"))?
        .push(snap);
    Ok(())
}

/// Restores the registry to its most recently saved state.
///
/// @return `NULL`.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_registry_restore() -> Result<()> {
    let snap = saved()
        .lock()
        .map_err(|_| fail("the registry is unavailable"))?
        .pop()
        .ok_or_else(|| fail("no saved state of the registry"))?;
    *registry()
        .lock()
        .map_err(|_| fail("the registry is unavailable"))? = snap;
    crate::construct::registry_changed();
    Ok(())
}

/// Discards the most recently saved state of the registry.
///
/// @return `NULL`.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_registry_release() -> Result<()> {
    saved()
        .lock()
        .map_err(|_| fail("the registry is unavailable"))?
        .pop();
    Ok(())
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
    fn r8_namespace_owner;
    fn r8_package_namespaces;
    fn r8_check_namespaces;
    fn r8_declare_trait;
    fn r8_declare_impl;
    fn r8_ir_json;
    fn r8_canonical_json;
    fn r8_json_parse;
    fn r8_declare_type;
    fn r8_type_document;
}
