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
use rtemis_r8::abi::{ABI_VERSION, Codec, Exports, ImplEntry, MethodFn};
use rtemis_r8::ir::{IrDocument, TypeDef};
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
struct CodecPtr(*const Codec);

// The tables are static data of loaded shared objects, read on R's main
// thread only.
unsafe impl Send for CodecPtr {}

/// An impl function's trait, type, and method.
type ImplKey = (String, String, String);

/// The entries one package registered.
#[derive(Default)]
struct Owned {
    impls: Vec<ImplKey>,
    types: Vec<String>,
}

#[derive(Default)]
struct Compiled {
    /// Impl functions by trait, type, and method.
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

/// The codec of the Rust-stored type `name`, when a package registered one.
pub(crate) fn codec(name: &str) -> Option<*const Codec> {
    compiled().lock().ok()?.types.get(name).map(|v| v.0)
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
            let v = unsafe { &*c.types[*n].0 };
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
            "the exports table has {} bytes, fewer than the {EXPORTS_SIZE} of ABI version {ABI_VERSION}",
            e.size
        )));
    }
    Ok(e)
}

/// The type declarations of an exports table's declaration record, each of
/// the namespace `ns`; none for a table without a record, a table written by
/// hand in C.
fn record_types(e: &Exports, ns: &str) -> Result<Vec<TypeDef>> {
    use rtemis_r8::record::{self, Declaration};
    if e.ir_record.is_null() {
        return Ok(Vec::new());
    }
    let bytes = unsafe { std::slice::from_raw_parts(e.ir_record, e.ir_record_len) };
    let entries = record::decode(bytes)
        .map_err(|e| fail(format!("the exports table's record does not read: {e}")))?;
    let mut types = Vec::with_capacity(entries.len());
    for entry in entries {
        if entry.namespace.as_str() != ns {
            return Err(fail(format!(
                "the exports table's record declares a type of `{}`; the package declares its types in `{ns}`",
                entry.namespace
            )));
        }
        match entry.declaration {
            Declaration::Type(t) => types.push(t),
            Declaration::Trait(t) => {
                return Err(fail(format!(
                    "the exports table's record declares the trait `{}`; a record holds type declarations only",
                    t.name
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
    Ok(types)
}

/// The IR document of the type declarations of an exports table's record,
/// as the R lists the declaration functions build.
///
/// @param ptr The external pointer that `r8_exports` returned.
/// @param ns Character: The package's namespace.
/// @return List: The IR document.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_exports_document(ptr: Robj, ns: &str) -> Result<Robj> {
    let e = exports_of(&ptr)?;
    let doc = IrDocument {
        ir_version: rtemis_r8::ir::IR_VERSION,
        namespace: Name::new(ns).map_err(|e| fail(e.to_string()))?,
        types: record_types(e, ns)?,
        traits: Vec::new(),
        impls: Vec::new(),
    };
    crate::irser::to_r(&doc).map_err(|e| fail(e.to_string()))
}

/// Compares the types of an exports table with the registered declarations.
/// A type of the record compares by declaration hash: the Rust hash reads
/// the registry with the record's types in place of its namespace's types
/// of the same names, and the R hash reads the registry alone. A codec whose
/// type has no declaration in the record compares the hash it pins
/// (`declaration_hash`) with the registry's.
///
/// @param ptr The external pointer that `r8_exports` returned.
/// @param ns Character: The package's namespace.
/// @return List: `type_name`, the qualified names; `status`, for a type of
///   the record `"same"`, `"differs"`, or `"missing"` (no registered
///   declaration), and for a codec's type outside the record `"same"`,
///   `"stale"` (the pinned hash differs), `"unpinned"` (no pinned hash), or
///   `"undeclared"`; `codec`, whether the table carries the type's codec; and
///   `hash`, the registered declaration's hash, `""` without one.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_exports_compare(ptr: Robj, ns: &str) -> Result<List> {
    let e = exports_of(&ptr)?;
    let types = record_types(e, ns)?;
    let namespace = Name::new(ns).map_err(|e| fail(e.to_string()))?;
    let mut codecs = Vec::new();
    for v in type_tables(e)? {
        let name = text(unsafe { (*v).type_name }, "type name")?;
        let pin = unsafe { (*v).declaration_hash };
        let pin = if pin.is_null() {
            None
        } else {
            Some(text(pin, "declaration hash")?)
        };
        codecs.push((name, pin));
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
    let mut hash = Vec::new();
    for t in &types {
        let name = format!("{ns}::{}", t.name());
        let q = QualifiedName::parse(&name).map_err(|e| fail(e.to_string()))?;
        let h = hash_r(&q);
        let s = match &h {
            None => "missing",
            Some(h) if *h == rtemis_r8::canonical::declaration_hash(&q, lookup_rust) => "same",
            Some(_) => "differs",
        };
        codec.push(codecs.iter().any(|(n, _)| *n == name));
        hash.push(h.unwrap_or_default());
        names.push(name);
        status.push(s);
    }
    for (name, pin) in &codecs {
        if names.contains(name) {
            continue;
        }
        let h = QualifiedName::parse(name).ok().and_then(|q| hash_r(&q));
        let s = match (&h, pin) {
            (None, _) => "undeclared",
            (Some(_), None) => "unpinned",
            (Some(h), Some(p)) if h == p => "same",
            (Some(_), Some(_)) => "stale",
        };
        codec.push(true);
        hash.push(h.unwrap_or_default());
        names.push(name.clone());
        status.push(s);
    }
    Ok(list!(
        type_name = names,
        status = status,
        codec = codec,
        hash = hash
    ))
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
/// @return List: `trait_name`, `type_name`, and `method` of each impl function.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_register_exports(ptr: Robj, package: &str, ns: &str) -> Result<List> {
    let e = exports_of(&ptr)?;
    let declared = text(e.package, "package name")?;
    if declared != package {
        return Err(fail(format!(
            "the exports table names the package `{declared}` and is registered by `{package}`"
        )));
    }
    let in_record: Vec<String> = record_types(e, ns)?
        .iter()
        .map(|t| format!("{ns}::{}", t.name()))
        .collect();
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
        if in_record.contains(&name) && !unsafe { (*v).declaration_hash }.is_null() {
            return Err(fail(format!(
                "the codec of `{name}` of `{package}` sets `declaration_hash`, and its type is declared in the exports' record, which pins it"
            )));
        }
        rtemis_r8::exports::check_codec(&q, unsafe { &*v }, lookup, package).map_err(fail)?;
        types.push((name, CodecPtr(v)));
    }
    // Impl functions, whose rows make the impl declarations.
    let entries: &[ImplEntry] = if e.impls.is_null() || e.n_impls == 0 {
        &[]
    } else {
        unsafe { std::slice::from_raw_parts(e.impls, e.n_impls) }
    };
    let mut rows = Vec::with_capacity(entries.len());
    let mut impls = Vec::with_capacity(entries.len());
    for entry in entries {
        let key = (
            text(entry.trait_name, "trait name")?,
            text(entry.type_name, "type name")?,
            text(entry.method, "method name")?,
        );
        let call = entry.call.ok_or_else(|| {
            fail(format!(
                "the impl function `{}` of `{}` for `{}` is NULL",
                key.2, key.0, key.1
            ))
        })?;
        rows.push(rtemis_r8::exports::ImplRow {
            trait_name: key.0.clone(),
            type_name: key.1.clone(),
            method: key.2.clone(),
            n_params: entry.n_params,
        });
        impls.push((key, ImplFn { call }));
    }
    for i in rtemis_r8::exports::impls_from_rows(&rows, package).map_err(fail)? {
        crate::declare::declare_impl(i, ns)?;
    }
    let reg = crate::declare::snapshot()?;
    rtemis_r8::exports::check_params(&rows, &reg, package).map_err(fail)?;
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
    let keys: Vec<ImplKey> = impls.iter().map(|(k, _)| k.clone()).collect();
    let names: Vec<String> = types.iter().map(|(n, _)| n.clone()).collect();
    for (k, f) in impls {
        c.impls.insert(k, f);
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
    let type_names: Vec<String> = keys.iter().map(|k| k.1.clone()).collect();
    let methods: Vec<String> = keys.iter().map(|k| k.2.clone()).collect();
    Ok(list!(
        trait_name = trait_names,
        type_name = type_names,
        method = methods
    ))
}

/// The compiled impl function of `method` of the trait `trait_name` for the
/// type `type_name`, as an external pointer to the function, which R passes
/// to R8's C routine `invoke` (`r8_native_symbols()`).
///
/// @param trait_name Character: The qualified trait name.
/// @param type_name Character: The qualified type name.
/// @param method Character: The method.
/// @return An external pointer.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_impl_fn(trait_name: &str, type_name: &str, method: &str) -> Result<Robj> {
    let f = {
        let c = compiled()
            .lock()
            .map_err(|_| fail("the registry of compiled exports is unavailable"))?;
        c.impls
            .get(&(
                trait_name.to_string(),
                type_name.to_string(),
                method.to_string(),
            ))
            .copied()
    };
    let Some(f) = f else {
        return Err(fail(format!(
            "`{type_name}` has no compiled impl of `{trait_name}` (method `{method}`)"
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
    fn r8_register_exports;
    fn r8_impl_fn;
    fn r8_compiled_types;
}
