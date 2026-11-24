//! The exports of compiled code: an `R8Exports` table from a package, its
//! registration, and calls of its impl functions.
//!
//! A package whose declarations are compiled registers a function returning
//! its static `R8Exports` with `R_RegisterCCallable(pkg, "r8_exports", fn)`
//! (`R8.h`). `r8_register()` fetches the table through R8's C routine
//! `exports` (`src/view.c`), as an external pointer tagged with the symbol
//! `r8_exports`, compares the table's types with the package's R
//! declarations ([`r8_exports_compare`]), and passes it to
//! [`r8_register_exports`], which declares the impls of its impl entries and
//! keeps its function pointers. An impl's R method passes the pointer that
//! [`r8_impl_fn`] returns to R8's C routine `invoke`, which calls it.
//!
//! Everything a table points to is static while the package's shared object
//! is loaded, so the registry keeps the pointers themselves.

use std::collections::HashMap;
use std::ffi::{CStr, c_char};
use std::sync::{Mutex, OnceLock};

use extendr_api::prelude::*;
use extendr_api::{Error, Result};
use extendr_ffi::{R_ExternalPtrAddr, R_ExternalPtrTag, Rf_install};
use rtemis_r8::abi::{
    ABI_VERSION, Codec, Exports, ImplEntry, MethodFn, POSITION_ANY, POSITION_CLASS, POSITION_TYPE,
    Position,
};
use rtemis_r8::exports::ImplRow;
use rtemis_r8::ir::{IrDocument, TraitDef, TypeDef, TypeRef};
use rtemis_r8::name::{Name, QualifiedName};

fn fail(message: impl Into<String>) -> Error {
    Error::Other(message.into())
}

/// The size of `R8Exports`, which every exports table holds.
const EXPORTS_SIZE: u32 = std::mem::size_of::<Exports>() as u32;

/// A registered impl function.
#[derive(Clone, Copy)]
struct ImplFn {
    call: MethodFn,
}

/// A registered codec of a Rust-stored type.
#[derive(Clone, Copy)]
struct CodecPtr {
    table: *const Codec,
    /// The generation of the registry at which the type's declaration was the
    /// one that the codec was compiled against.
    current: Option<u64>,
}

// The tables are static data of loaded shared objects, read on R's main
// thread only.
unsafe impl Send for CodecPtr {}

/// An impl function's trait, the key of its positions ([`tuple_key`]), and its
/// method.
type ImplKey = (String, String, String);

/// The entries one package registered.
#[derive(Default)]
struct Owned {
    impls: Vec<ImplKey>,
    types: Vec<String>,
}

#[derive(Default)]
struct Compiled {
    /// Impl functions by trait, positions, and method.
    impls: HashMap<ImplKey, ImplFn>,
    /// Codecs of Rust-stored types by qualified name.
    types: HashMap<String, CodecPtr>,
    /// The entries each package registered, for their replacement.
    owned: HashMap<String, Owned>,
}

fn compiled() -> &'static Mutex<Compiled> {
    static COMPILED: OnceLock<Mutex<Compiled>> = OnceLock::new();
    COMPILED.get_or_init(|| Mutex::new(Compiled::default()))
}

/// The codec of the Rust-stored type `name`, when a package registered one. A
/// type that was declared otherwise after that has no codec to build a value
/// with, which the error says.
pub(crate) fn codec(name: &str) -> std::result::Result<Option<*const Codec>, String> {
    let mut c = compiled().lock().unwrap_or_else(|e| e.into_inner());
    let Some(v) = c.types.get_mut(name) else {
        return Ok(None);
    };
    let now = crate::construct::GENERATION.load(std::sync::atomic::Ordering::Relaxed);
    if v.current != Some(now) {
        if !crate::pins::is_current(unsafe { (*v.table).module }, name) {
            return Err(format!(
                "`{name}` was declared again after its package registered its compiled code; load the package again"
            ));
        }
        v.current = Some(now);
    }
    Ok(Some(v.table))
}

/// The package that registered the codec of the Rust-stored type `name`.
pub(crate) fn codec_package(name: &str) -> Option<String> {
    let c = compiled().lock().ok()?;
    c.owned
        .iter()
        .find(|(_, o)| o.types.iter().any(|t| t == name))
        .map(|(p, _)| p.clone())
}

/// The registered Rust-stored types and whether each codec table holds
/// every function.
///
/// @return List: `type_name` and `complete`.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_compiled_types() -> Result<List> {
    let c = compiled()
        .lock()
        .map_err(|_| fail("the registry of compiled exports is unavailable"))?;
    let mut names: Vec<&String> = c.types.keys().collect();
    names.sort();
    let complete: Vec<bool> = names
        .iter()
        .map(|n| {
            let v = unsafe { &*c.types[*n].table };
            v.from_fields.is_some()
                && v.encode.is_some()
                && v.write_state.is_some()
                && v.read_state.is_some()
                && v.get_field.is_some()
                && v.with_field.is_some()
                && v.drop.is_some()
                && v.owned_bytes.is_some()
        })
        .collect();
    let type_name: Vec<String> = names.into_iter().cloned().collect();
    Ok(list!(type_name = type_name, complete = complete))
}

/// The size of `R8Codec`, which every codec table holds.
const CODEC_SIZE: u32 = std::mem::size_of::<Codec>() as u32;

/// The codec tables of an exports table. The array's stride is the `size` of
/// its entries, so a package built against a later header, whose entries
/// carry appended fields, is read at the offsets it wrote.
fn type_tables(e: &Exports) -> Result<Vec<*const Codec>> {
    if e.types.is_null() || e.n_types == 0 {
        return Ok(Vec::new());
    }
    let base = e.types.cast::<u8>();
    let stride = unsafe { (*e.types).size };
    if stride < CODEC_SIZE {
        return Err(fail(format!(
            "the codec tables have size {stride}; ABI version 1 requires at least {CODEC_SIZE}"
        )));
    }
    let mut out = Vec::with_capacity(e.n_types);
    for i in 0..e.n_types {
        let v = unsafe { base.add(i * stride as usize) }.cast::<Codec>();
        let size = unsafe { (*v).size };
        if size != stride {
            return Err(fail(format!(
                "codec table {} has size {size}; the first has size {stride}",
                i + 1
            )));
        }
        out.push(v);
    }
    Ok(out)
}

/// The impl entries of an exports table, whose array has the stride
/// `impl_size`.
fn impl_entries(e: &Exports) -> Result<Vec<&ImplEntry>> {
    if e.impls.is_null() || e.n_impls == 0 {
        return Ok(Vec::new());
    }
    let size = std::mem::size_of::<ImplEntry>();
    if e.impl_size < size {
        return Err(fail(format!(
            "the impl entries have size {}; ABI version 1 requires at least {size}",
            e.impl_size
        )));
    }
    let base = e.impls.cast::<u8>();
    Ok((0..e.n_impls)
        .map(|i| unsafe { &*base.add(i * e.impl_size).cast::<ImplEntry>() })
        .collect())
}

/// A NUL-terminated UTF-8 string of a table.
fn text(p: *const c_char, what: &str) -> Result<String> {
    if p.is_null() {
        return Err(fail(format!("the exports table has no {what}")));
    }
    unsafe { CStr::from_ptr(p) }
        .to_str()
        .map(str::to_string)
        .map_err(|_| fail(format!("the exports table's {what} is not UTF-8")))
}

/// The positions of an impl entry of the package `package`.
fn positions(entry: &ImplEntry, package: &str) -> Result<Vec<TypeRef>> {
    if entry.positions.is_null() || entry.n_positions == 0 {
        return Ok(Vec::new());
    }
    let rows: &[Position] =
        unsafe { std::slice::from_raw_parts(entry.positions, entry.n_positions) };
    rows.iter()
        .map(|p| match p.kind {
            POSITION_TYPE => {
                let name = text(p.name, "type name")?;
                let name = QualifiedName::parse(&name).map_err(|e| {
                    fail(format!(
                        "the type name `{name}` of an impl entry of `{package}`: {e}"
                    ))
                })?;
                Ok(TypeRef::Named { name })
            }
            POSITION_CLASS => Err(fail(format!(
                "an impl entry of `{package}` is for the class `{}` of R; the methods of a class of R are written in R",
                text(p.name, "class name")?
            ))),
            POSITION_ANY => Ok(TypeRef::Any),
            other => Err(fail(format!(
                "a position of an impl entry of `{package}` is of the kind {other}, neither R8_POSITION_TYPE nor R8_POSITION_ANY"
            ))),
        })
        .collect()
}

/// The key of an impl's positions, as R's `tuple_key()` writes it: each
/// position's qualified type name, class name in double quotes, or `any`,
/// joined by commas.
fn tuple_key(positions: &[TypeRef]) -> String {
    positions
        .iter()
        .map(|p| match p {
            TypeRef::Named { name } => name.to_string(),
            TypeRef::HostClass { class, .. } => format!("\"{class}\""),
            TypeRef::Any => "any".to_string(),
        })
        .collect::<Vec<_>>()
        .join(",")
}

/// The `R8Exports` an external pointer holds, checked: the tag, the ABI
/// version, and the size.
fn exports_of<'a>(ptr: &Robj) -> Result<&'a Exports> {
    let x = unsafe { ptr.get() };
    if ptr.rtype() != Rtype::ExternalPtr {
        return Err(fail(
            "`r8_exports` returned another object than an external pointer",
        ));
    }
    let tag = unsafe { R_ExternalPtrTag(x) };
    let expected = unsafe { Rf_install(c"r8_exports".as_ptr()) };
    if tag != expected {
        return Err(fail(
            "the external pointer of `r8_exports` is not tagged `r8_exports`",
        ));
    }
    let p = unsafe { R_ExternalPtrAddr(x) } as *const Exports;
    if p.is_null() {
        return Err(fail("the external pointer of `r8_exports` is NULL"));
    }
    let e = unsafe { &*p };
    if e.abi_version != ABI_VERSION {
        return Err(fail(format!(
            "the package was built against R8 ABI version {}, and this R8 provides version {ABI_VERSION}; reinstall the package",
            e.abi_version
        )));
    }
    if e.size < EXPORTS_SIZE {
        return Err(fail(format!(
            "the exports table has {} bytes, fewer than the {EXPORTS_SIZE} of ABI version {ABI_VERSION}; reinstall the package against the installed R8",
            e.size
        )));
    }
    Ok(e)
}

/// The declarations of an exports table's declaration record.
#[derive(Default)]
struct Declarations {
    /// The types of the package's namespace.
    types: Vec<TypeDef>,
    /// The traits of the package's namespace.
    traits: Vec<TraitDef>,
    /// The types of other namespaces that the code was compiled against.
    foreign: Vec<(Name, TypeDef)>,
}

/// The declarations of the record of an exports table, whose package declares
/// into the namespace `ns`.
fn record_declarations(e: &Exports, ns: &str) -> Result<Declarations> {
    use rtemis_r8::record::{self, Declaration};
    if e.ir_record.is_null() {
        return Err(fail("the exports table has no declaration record"));
    }
    let bytes = unsafe { std::slice::from_raw_parts(e.ir_record, e.ir_record_len) };
    let entries = record::decode(bytes)
        .map_err(|e| fail(format!("the exports table's record does not read: {e}")))?;
    let mut out = Declarations::default();
    for entry in entries {
        let own = entry.namespace.as_str() == ns;
        match entry.declaration {
            Declaration::Type(t) if own => out.types.push(t),
            Declaration::Type(t) => out.foreign.push((entry.namespace, t)),
            Declaration::Trait(t) if own => out.traits.push(t),
            Declaration::Trait(t) => {
                return Err(fail(format!(
                    "the exports table's record declares the trait `{}::{}`; the package declares into `{ns}`",
                    entry.namespace, t.name
                )));
            }
            Declaration::Impl(i) => {
                return Err(fail(format!(
                    "the exports table's record declares an impl of `{}`; R8 builds impl declarations from the impl entries",
                    i.r#trait
                )));
            }
        }
    }
    Ok(out)
}

/// The signature of an impl entry's method, and the record of its impl, read
/// from the entry's `signature`. The entries of one impl have one record, which
/// `read` keeps by its address.
fn signature(
    entry: &ImplEntry,
    method: &str,
    read: &mut HashMap<usize, std::rc::Rc<TraitDef>>,
) -> Result<(rtemis_r8::ir::MethodDef, std::rc::Rc<TraitDef>)> {
    use rtemis_r8::record::{self, Declaration};
    if entry.signature.is_null() {
        return Err(fail(format!(
            "the impl entry of `{method}` states no signature"
        )));
    }
    let held = match read.get(&(entry.signature as usize)) {
        Some(t) => t.clone(),
        None => {
            let bytes = unsafe { std::slice::from_raw_parts(entry.signature, entry.signature_len) };
            let t = record::decode(bytes)
                .map_err(|e| fail(format!("the signature of an impl entry does not read: {e}")))?
                .into_iter()
                .find_map(|e| match e.declaration {
                    Declaration::Trait(t) => Some(std::rc::Rc::new(t)),
                    _ => None,
                })
                .ok_or_else(|| fail("the signature of an impl entry holds no trait"))?;
            read.insert(entry.signature as usize, t.clone());
            t
        }
    };
    let found = held
        .methods
        .iter()
        .find(|m| m.name.as_str() == method)
        .cloned()
        .ok_or_else(|| {
            fail(format!(
                "the signature of an impl entry holds no method `{method}`"
            ))
        })?;
    Ok((found, held))
}

/// The rows of an exports table's impl entries, with their functions.
struct Rows {
    rows: Vec<ImplRow>,
    calls: Vec<ImplFn>,
    /// Each trait with the record of an impl's signatures, once per record.
    records: Vec<(String, std::rc::Rc<TraitDef>)>,
}

/// The rows of `e`, each method read as the one its trait declares in `reg`,
/// and the impl declarations that they make.
fn read_rows(
    e: &Exports,
    package: &str,
    reg: &rtemis_r8::rules::DocumentSet,
) -> Result<(Rows, Vec<rtemis_r8::exports::SignatureProblem>)> {
    let entries = impl_entries(e)?;
    let mut out = Rows {
        rows: Vec::with_capacity(entries.len()),
        calls: Vec::with_capacity(entries.len()),
        records: Vec::new(),
    };
    let mut read = HashMap::new();
    for entry in entries {
        let method = text(entry.method, "method name")?;
        let (signed, held) = signature(entry, &method, &mut read)?;
        let trait_name = text(entry.trait_name, "trait name")?;
        if !out
            .records
            .iter()
            .any(|(_, r)| std::rc::Rc::ptr_eq(r, &held))
        {
            out.records.push((trait_name.clone(), held));
        }
        let row = ImplRow {
            trait_name,
            positions: positions(entry, package)?,
            method,
            signature: signed,
            return_type: None,
        };
        let call = entry.call.ok_or_else(|| {
            fail(format!(
                "the impl function `{}` of `{}` for `{}` is NULL",
                row.method,
                row.trait_name,
                tuple_key(&row.positions)
            ))
        })?;
        out.rows.push(row);
        out.calls.push(ImplFn { call });
    }
    rtemis_r8::exports::resolve_methods(&mut out.rows, reg, package).map_err(fail)?;
    // A result may fit a trait that another impl of the table implements.
    let mut with = reg.clone();
    for i in rtemis_r8::exports::impls_from_rows(&out.rows, package).map_err(fail)? {
        let d = with.document_position(&i.r#trait.namespace);
        with.impls_mut(d).push(i);
    }
    let problems = rtemis_r8::exports::check_signatures(&mut out.rows, &with);
    Ok((out, problems))
}

/// The differences of the signatures of an exports table's impls from the
/// declarations of their traits.
///
/// @param ptr The external pointer that `r8_exports` returned.
/// @param package Character: The registering package.
/// @return List: One element per method, accessor, or tag member that differs,
///   each a list of `of` (the trait), `member`, `target` (the impl's positions,
///   `""` for an accessor and a tag), `messages`, and, for a method,
///   `signature`, its IR as compiled code has it.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_exports_signatures(ptr: Robj, package: crate::utf8::Utf8) -> Result<List> {
    let package: &str = &package;
    let e = exports_of(&ptr)?;
    let reg = crate::declare::snapshot()?;
    let lookup: &dyn rtemis_r8::rules::TypeLookup = &*reg;
    let (rows, problems) = read_rows(e, package, &reg)?;
    let mut out: Vec<Robj> = Vec::new();
    for p in problems {
        let signature = crate::irser::to_r(&p.signature).map_err(|e| fail(e.to_string()))?;
        out.push(
            list!(
                of = p.r#trait,
                member = p.method,
                target = p.target,
                messages = p.messages,
                signature = signature
            )
            .into(),
        );
    }
    let mut seen = Vec::new();
    for (name, held) in &rows.records {
        let Some(want) = QualifiedName::parse(name)
            .ok()
            .and_then(|q| lookup.trait_of(&q))
        else {
            continue;
        };
        for m in rtemis_r8::impl_signature::check_members(held, want) {
            if !seen.contains(&(name, m.clone())) {
                out.push(
                    list!(
                        of = name.as_str(),
                        member = m.member.as_str(),
                        target = "",
                        messages = m.message.as_str()
                    )
                    .into(),
                );
                seen.push((name, m));
            }
        }
    }
    Ok(List::from_values(out))
}

/// The IR document of the declarations of an exports table's record,
/// as the R lists the declaration functions build.
///
/// @param ptr The external pointer that `r8_exports` returned.
/// @param ns Character: The package's namespace.
/// @return List: The IR document.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_exports_document(ptr: Robj, ns: crate::utf8::Utf8) -> Result<Robj> {
    let ns: &str = &ns;
    let e = exports_of(&ptr)?;
    let Declarations { types, traits, .. } = record_declarations(e, ns)?;
    let doc = IrDocument {
        ir_version: rtemis_r8::ir::IR_VERSION,
        namespace: Name::new(ns).map_err(|e| fail(e.to_string()))?,
        types,
        traits,
        impls: Vec::new(),
    };
    crate::irser::to_r(&doc).map_err(|e| fail(e.to_string()))
}

/// The parent that a declaration extends.
fn parent_of(t: &TypeDef) -> Option<&QualifiedName> {
    match t {
        TypeDef::Struct(s) => s.extends.as_ref(),
        TypeDef::Enum(_) => None,
    }
}

/// Compares the types and traits of an exports table with the registered
/// declarations. A trait of the record compares by its hash
/// (`impl_signature::trait_hash()`). A type of the record compares by declaration hash and by its parent,
/// which the hash excludes: the Rust hash reads
/// the registry with the record's types in place of its namespace's types
/// of the same names, and the R hash reads the registry alone. A codec whose
/// type has no declaration in the record is an error.
///
/// @param ptr The external pointer that `r8_exports` returned.
/// @param ns Character: The package's namespace.
/// @return List: `type_name`, the qualified names of the types and traits of
///   the record; `status`, `"same"`, `"differs"`, or `"missing"` (no registered
///   declaration); and `codec`, whether the table carries the type's codec.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_exports_compare(ptr: Robj, ns: crate::utf8::Utf8) -> Result<List> {
    let ns: &str = &ns;
    let e = exports_of(&ptr)?;
    let Declarations { types, traits, .. } = record_declarations(e, ns)?;
    let namespace = Name::new(ns).map_err(|e| fail(e.to_string()))?;
    let mut codecs = Vec::new();
    for v in type_tables(e)? {
        let name = text(unsafe { (*v).type_name }, "type name")?;
        if !types.iter().any(|t| format!("{ns}::{}", t.name()) == name) {
            return Err(fail(format!(
                "the codec of `{name}` has no declaration in the exports' record"
            )));
        }
        codecs.push(name);
    }
    let reg = crate::declare::snapshot()?;
    let mut overlay = rtemis_r8::rules::DocumentSet::clone(&reg);
    for t in &types {
        overlay.put_type(&namespace, t.clone());
    }
    let lookup_r: &dyn rtemis_r8::rules::TypeLookup = &*reg;
    let lookup_rust: &dyn rtemis_r8::rules::TypeLookup = &overlay;
    let hash_r = |q: &QualifiedName| {
        lookup_r
            .find(q)
            .map(|_| rtemis_r8::canonical::declaration_hash(q, lookup_r))
    };
    let mut names = Vec::new();
    let mut status = Vec::new();
    let mut codec = Vec::new();
    for t in &types {
        let name = format!("{ns}::{}", t.name());
        let q = QualifiedName::parse(&name).map_err(|e| fail(e.to_string()))?;
        let s = match &hash_r(&q) {
            None => "missing",
            Some(h)
                if *h == rtemis_r8::canonical::declaration_hash(&q, lookup_rust)
                    && lookup_r.find(&q).and_then(parent_of) == parent_of(t) =>
            {
                "same"
            }
            Some(_) => "differs",
        };
        codec.push(codecs.contains(&name));
        names.push(name);
        status.push(s);
    }
    for t in &traits {
        let q = QualifiedName {
            namespace: namespace.clone(),
            name: t.name.clone(),
        };
        let rust = rtemis_r8::impl_signature::trait_hash(t);
        let s = match lookup_r
            .trait_of(&q)
            .map(rtemis_r8::impl_signature::trait_hash)
        {
            None => "missing",
            Some(h) if h == rust => "same",
            Some(_) => "differs",
        };
        names.push(q.to_string());
        status.push(s);
        codec.push(false);
    }
    Ok(list!(type_name = names, status = status, codec = codec))
}

/// Registers an exports table: declares the impls its impl entries make and
/// keeps the codecs of its Rust-stored types and its impl functions,
/// replacing the package's earlier entries. The types are the package's R
/// declarations, registered before, which `r8_exports_compare()` checks
/// against the table.
///
/// @param ptr The external pointer that `r8_exports` returned.
/// @param package Character: The registering package.
/// @param ns Character: The package's namespace.
/// @return List: of each impl function, `trait_name`; `tuple`, the key of its
///   positions; and `method`. `changed` holds the types of other packages whose
///   registered declarations do not refine those that the code was compiled
///   against: `type_name`, and `message`, of each difference.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_register_exports(
    ptr: Robj,
    package: crate::utf8::Utf8,
    ns: crate::utf8::Utf8,
) -> Result<List> {
    let package: &str = &package;
    let ns: &str = &ns;
    let e = exports_of(&ptr)?;
    let declared = text(e.package, "package name")?;
    if declared != package {
        return Err(fail(format!(
            "the exports table names the package `{declared}` and is registered by `{package}`"
        )));
    }
    let namespace = Name::new(ns).map_err(|e| fail(e.to_string()))?;
    let held = record_declarations(e, ns)?;
    if e.module.is_null() {
        return Err(fail(format!(
            "the exports table of `{package}` names no module"
        )));
    }
    // Codecs, checked against the registered declarations of their types.
    let reg = crate::declare::snapshot()?;
    let lookup: &dyn rtemis_r8::rules::TypeLookup = &*reg;
    let mut types = Vec::new();
    for v in type_tables(e)? {
        let name = text(unsafe { (*v).type_name }, "type name")?;
        let q = QualifiedName::parse(&name).map_err(|e| fail(e.to_string()))?;
        if q.namespace.as_str() != ns {
            return Err(fail(format!(
                "the codec of `{name}` of `{package}` names a type of another namespace than `{ns}`"
            )));
        }
        if unsafe { (*v).module } != e.module {
            return Err(fail(format!(
                "the codec of `{name}` of `{package}` names another module than the exports table"
            )));
        }
        rtemis_r8::exports::check_codec(&q, unsafe { &*v }, lookup, package).map_err(fail)?;
        types.push((
            name,
            CodecPtr {
                table: v,
                current: None,
            },
        ));
    }
    // Impl functions, whose rows make the impl declarations.
    let (Rows { rows, calls, .. }, problems) = read_rows(e, package, &reg)?;
    if let Some(p) = problems.first() {
        return Err(fail(format!(
            "the method `{}` of `{}` for {}: {}",
            p.method,
            p.r#trait,
            p.target,
            p.messages.join("; ")
        )));
    }
    let mut impls = rtemis_r8::exports::impls_from_rows(&rows, package).map_err(fail)?;
    rtemis_r8::exports::check_complete(&impls, &reg, package).map_err(fail)?;
    // A narrower result type is checked against the impls declared before it.
    impls.sort_by_key(|i| i.methods.iter().any(|m| m.return_type().is_some()));
    for i in impls {
        crate::declare::declare_impl(i, ns)?;
    }
    let keys: Vec<ImplKey> = rows
        .iter()
        .map(|r| {
            (
                r.trait_name.clone(),
                tuple_key(&r.positions),
                r.method.clone(),
            )
        })
        .collect();
    let own = held.types.into_iter().map(|t| (namespace.clone(), t));
    crate::pins::put(package, e.module, own.chain(held.foreign).collect());
    let mut changed_type = Vec::new();
    let mut changed_message = Vec::new();
    for (name, found) in crate::pins::changed(e.module, &namespace) {
        for d in found {
            changed_type.push(name.clone());
            changed_message.push(d.message);
        }
    }
    let mut c = compiled()
        .lock()
        .map_err(|_| fail("the registry of compiled exports is unavailable"))?;
    if let Some(old) = c.owned.remove(package) {
        for k in old.impls {
            c.impls.remove(&k);
        }
        for t in old.types {
            c.types.remove(&t);
        }
    }
    let names: Vec<String> = types.iter().map(|(n, _)| n.clone()).collect();
    for (k, f) in keys.iter().zip(calls) {
        c.impls.insert(k.clone(), f);
    }
    for (n, v) in types {
        c.types.insert(n, v);
    }
    c.owned.insert(
        package.to_string(),
        Owned {
            impls: keys.clone(),
            types: names,
        },
    );
    let trait_names: Vec<String> = keys.iter().map(|k| k.0.clone()).collect();
    let tuples: Vec<String> = keys.iter().map(|k| k.1.clone()).collect();
    let methods: Vec<String> = keys.iter().map(|k| k.2.clone()).collect();
    Ok(list!(
        trait_name = trait_names,
        tuple = tuples,
        method = methods,
        changed = list!(type_name = changed_type, message = changed_message)
    ))
}

/// The traits that the impl entries of an exports table name.
///
/// @param ptr The external pointer that `r8_exports` returned.
/// @return Character: The qualified trait names, each once.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_exports_traits(ptr: Robj) -> Result<Vec<String>> {
    let e = exports_of(&ptr)?;
    let mut out: Vec<String> = Vec::new();
    for entry in impl_entries(e)? {
        let name = text(entry.trait_name, "trait name")?;
        if !out.contains(&name) {
            out.push(name);
        }
    }
    Ok(out)
}

/// The compiled impl function of `method` of the trait `trait_name` for the
/// positions `tuple`, as an external pointer to the function, which R passes
/// to R8's C routine `invoke` (`r8_native_symbols()`).
///
/// @param trait_name Character: The qualified trait name.
/// @param tuple Character: The key of the positions (`tuple_key()`).
/// @param method Character: The method.
/// @return An external pointer.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_impl_fn(
    trait_name: crate::utf8::Utf8,
    tuple: crate::utf8::Utf8,
    method: crate::utf8::Utf8,
) -> Result<Robj> {
    let trait_name: &str = &trait_name;
    let tuple: &str = &tuple;
    let method: &str = &method;
    let f = {
        let c = compiled()
            .lock()
            .map_err(|_| fail("the registry of compiled exports is unavailable"))?;
        c.impls
            .get(&(
                trait_name.to_string(),
                tuple.to_string(),
                method.to_string(),
            ))
            .copied()
    };
    let Some(f) = f else {
        return Err(fail(format!(
            "`{tuple}` has no compiled impl of `{trait_name}` (method `{method}`)"
        )));
    };
    Ok(unsafe {
        Robj::from_sexp(extendr_ffi::R_MakeExternalPtr(
            f.call as *mut std::ffi::c_void,
            extendr_ffi::R_NilValue,
            extendr_ffi::R_NilValue,
        ))
    })
}

/// Clears external pointers to compiled functions, whose package's library
/// unloads or registers again, so a closure that still holds one raises an
/// error in `invoke` rather than calling freed code.
///
/// @param ptrs List: External pointers.
/// @return `NULL`.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_clear_external_ptrs(ptrs: List) {
    for p in ptrs.values() {
        let x = unsafe { p.get() };
        if unsafe { extendr_ffi::TYPEOF(x) } == extendr_ffi::SEXPTYPE::EXTPTRSXP {
            unsafe { extendr_ffi::R_ClearExternalPtr(x) };
        }
    }
}

extendr_module! {
    mod exports;
    fn r8_clear_external_ptrs;
    fn r8_exports_document;
    fn r8_exports_compare;
    fn r8_exports_traits;
    fn r8_exports_signatures;
    fn r8_register_exports;
    fn r8_impl_fn;
    fn r8_compiled_types;
}
