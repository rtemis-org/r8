//! The exports of compiled code: an `R8Exports` table from a package, its
//! registration, and calls of its impl functions.
//!
//! A package whose declarations are compiled registers a function returning
//! its static `R8Exports` with `R_RegisterCCallable(pkg, "r8_exports", fn)`
//! (`R8.h`). `r8_register()` fetches the table through R8's C routine
//! `exports` (`src/view.c`), as an external pointer tagged with the symbol
//! `r8_exports`, and passes it to [`r8_exports_info`], which checks the
//! table, and to [`r8_register_exports`], which declares its IR document and
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
use rtemis_r8::abi::{ABI_VERSION, Exports, ImplEntry, MethodFn, TypeVTable};
use rtemis_r8::ir::{ImplTarget, IrDocument};
use rtemis_r8::name::QualifiedName;

fn fail(message: impl Into<String>) -> Error {
    Error::Other(message.into())
}

/// The size of `R8Exports` in ABI version 1, the fields every table holds.
const EXPORTS_SIZE: u32 = std::mem::size_of::<Exports>() as u32;

/// A registered impl function.
#[derive(Clone, Copy)]
struct ImplFn {
    call: MethodFn,
}

/// A registered codec of a Rust-native type.
#[derive(Clone, Copy)]
struct VTable(*const TypeVTable);

// The tables are static data of loaded shared objects, read on R's main
// thread only.
unsafe impl Send for VTable {}

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
    /// Codecs of Rust-native types by qualified name.
    types: HashMap<String, VTable>,
    /// The entries each package registered, for their replacement.
    owned: HashMap<String, Owned>,
}

fn compiled() -> &'static Mutex<Compiled> {
    static COMPILED: OnceLock<Mutex<Compiled>> = OnceLock::new();
    COMPILED.get_or_init(|| Mutex::new(Compiled::default()))
}

/// The codec of the Rust-native type `name`, when a package registered one.
pub(crate) fn codec(name: &str) -> Option<*const TypeVTable> {
    compiled().lock().ok()?.types.get(name).map(|v| v.0)
}

/// The package that registered the codec of the Rust-native type `name`.
pub(crate) fn codec_package(name: &str) -> Option<String> {
    let c = compiled().lock().ok()?;
    c.owned
        .iter()
        .find(|(_, o)| o.types.iter().any(|t| t == name))
        .map(|(p, _)| p.clone())
}

/// The registered Rust-native types and whether each codec table holds
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
                && v.decode.is_some()
                && v.encode.is_some()
                && v.write_state.is_some()
                && v.read_state.is_some()
                && v.get_field.is_some()
                && v.with_field.is_some()
                && v.drop.is_some()
                && v.bytes.is_some()
        })
        .collect();
    let type_name: Vec<String> = names.into_iter().cloned().collect();
    Ok(list!(type_name = type_name, complete = complete))
}

/// The smallest `R8TypeVTable` of ABI version 1: the members before the enum
/// members, as tables written for R8 0.3.0 hold them.
const TYPE_VTABLE_SIZE: u32 = std::mem::offset_of!(TypeVTable, variant) as u32;

/// The codec tables of an exports table. The array's stride is the `size` of
/// its entries, so a package built against a later header, whose entries
/// carry appended fields, is read at the offsets it wrote.
fn type_tables(e: &Exports) -> Result<Vec<*const TypeVTable>> {
    if e.types.is_null() || e.n_types == 0 {
        return Ok(Vec::new());
    }
    let base = e.types.cast::<u8>();
    let stride = unsafe { (*e.types).size };
    if stride < TYPE_VTABLE_SIZE {
        return Err(fail(format!(
            "the codec tables have size {stride}; ABI version 1 requires at least {TYPE_VTABLE_SIZE}"
        )));
    }
    let mut out = Vec::with_capacity(e.n_types);
    for i in 0..e.n_types {
        let v = unsafe { base.add(i * stride as usize) }.cast::<TypeVTable>();
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

fn ir_document(e: &Exports) -> Result<IrDocument> {
    if e.ir_json.is_null() {
        return Err(fail("the exports table has no IR document"));
    }
    let bytes = unsafe { std::slice::from_raw_parts(e.ir_json as *const u8, e.ir_json_len) };
    let text = std::str::from_utf8(bytes)
        .map_err(|_| fail("the exports table's IR document is not UTF-8"))?;
    IrDocument::from_json(text).map_err(|e| {
        fail(format!(
            "the exports table's IR document does not read: {e}"
        ))
    })
}

/// The package and namespace of an exports table, checked.
///
/// @param ptr The external pointer that `r8_exports` returned.
/// @return List: `package` and `namespace`.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_exports_info(ptr: Robj) -> Result<List> {
    let e = exports_of(&ptr)?;
    let package = text(e.package, "package name")?;
    let doc = ir_document(e)?;
    Ok(list!(
        package = package,
        namespace = doc.namespace.to_string()
    ))
}

/// The IR document of an exports table, as JSON text.
///
/// @param ptr The external pointer that `r8_exports` returned.
/// @return Character: The IR document.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_exports_document(ptr: Robj) -> Result<String> {
    let e = exports_of(&ptr)?;
    let doc = ir_document(e)?;
    serde_json::to_string(&doc).map_err(|e| fail(e.to_string()))
}

/// Compares each type of an exports table with the registered declaration of
/// the same qualified name, by declaration hash. The Rust hash reads the
/// registry with the exports' IR document in place of its namespace's types
/// of the same names; the R hash reads the registry alone.
///
/// @param ptr The external pointer that `r8_exports` returned.
/// @return List: `type_name`, the qualified names; `status`, `"same"`,
///   `"differs"`, or `"missing"` (no registered declaration); and `codec`,
///   whether the table carries the type's codec.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_exports_compare(ptr: Robj) -> Result<List> {
    let e = exports_of(&ptr)?;
    let doc = ir_document(e)?;
    let codecs = type_tables(e)?
        .into_iter()
        .map(|v| text(unsafe { (*v).type_name }, "type name"))
        .collect::<Result<Vec<String>>>()?;
    let reg = crate::declare::snapshot()?;
    let mut overlay = rtemis_r8::rules::DocumentSet::clone(&reg);
    for t in &doc.types {
        overlay.put_type(&doc.namespace, t.clone());
    }
    let lookup_r: &dyn rtemis_r8::rules::TypeLookup = &*reg;
    let lookup_rust: &dyn rtemis_r8::rules::TypeLookup = &overlay;
    let mut names = Vec::new();
    let mut status = Vec::new();
    let mut codec = Vec::new();
    for t in &doc.types {
        let name = format!("{}::{}", doc.namespace, t.name());
        let q = QualifiedName::parse(&name).map_err(|e| fail(e.to_string()))?;
        let s = if lookup_r.find(&q).is_none() {
            "missing"
        } else if rtemis_r8::codec::declaration_hash(&q, lookup_r)
            == rtemis_r8::codec::declaration_hash(&q, lookup_rust)
        {
            "same"
        } else {
            "differs"
        };
        codec.push(codecs.contains(&name));
        names.push(name);
        status.push(s);
    }
    Ok(list!(type_name = names, status = status, codec = codec))
}

/// Registers an exports table: declares its IR document's types, traits, and
/// impls, and keeps the codecs of its Rust-native types and its impl
/// functions, replacing the package's earlier entries.
///
/// @param ptr The external pointer that `r8_exports` returned.
/// @param package Character: The registering package.
/// @return List: `trait_name`, `type_name`, and `method` of each impl function.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_register_exports(ptr: Robj, package: &str) -> Result<List> {
    let e = exports_of(&ptr)?;
    let declared = text(e.package, "package name")?;
    if declared != package {
        return Err(fail(format!(
            "the exports table names the package `{declared}` and is registered by `{package}`"
        )));
    }
    let doc = ir_document(e)?;
    let ns = doc.namespace.to_string();
    let report = |what: &str, name: &str, problems: List| -> Result<()> {
        let messages: Vec<String> = problems
            .dollar("message")
            .ok()
            .and_then(|m| m.as_string_vector())
            .unwrap_or_default();
        if messages.is_empty() {
            Ok(())
        } else {
            Err(fail(format!(
                "the {what} `{name}` of `{package}` does not declare: {}",
                messages.join("; ")
            )))
        }
    };
    for t in &doc.types {
        report(
            "type",
            t.name().as_str(),
            crate::declare::declare_type(t.clone(), &ns)?,
        )?;
    }
    for t in &doc.traits {
        report(
            "trait",
            t.name.as_str(),
            crate::declare::declare_trait(t.clone(), &ns)?,
        )?;
    }
    for i in &doc.impls {
        crate::declare::declare_impl(i.clone(), &ns)?;
    }
    // Codecs and impl functions.
    let vtables = type_tables(e)?;
    let entries: &[ImplEntry] = if e.impls.is_null() || e.n_impls == 0 {
        &[]
    } else {
        unsafe { std::slice::from_raw_parts(e.impls, e.n_impls) }
    };
    let mut types = Vec::new();
    for v in vtables {
        let name = text(unsafe { (*v).type_name }, "type name")?;
        let Some(def) = doc
            .types
            .iter()
            .find(|t| format!("{ns}::{}", t.name()) == name)
        else {
            return Err(fail(format!(
                "the codec of `{name}` has no declaration in the IR document of `{package}`"
            )));
        };
        // An enum's codec builds states by variant and names a state's
        // variant; a struct's builds them from its fields.
        let shape_ok = match def {
            rtemis_r8::ir::TypeDef::Struct(_) => {
                (unsafe { (*v).from_fields.is_some() }) && !crate::native::has_enum_members(v)
            }
            rtemis_r8::ir::TypeDef::Enum(_) => {
                crate::native::is_enum_codec(v) && (unsafe { (*v).from_fields.is_none() })
            }
        };
        if !shape_ok {
            return Err(fail(format!(
                "the codec of `{name}` of `{package}` does not match its type: a struct's codec sets `from_fields` and neither `variant` nor `from_variant`, and an enum's sets `variant` and `from_variant` and not `from_fields`"
            )));
        }
        // A codec's state carries every field; an R object has no form in
        // it, so a Rust-native type has no runtime-only field in any variant.
        let runtime = |fs: &[rtemis_r8::ir::FieldDef]| {
            fs.iter()
                .find(|f| {
                    matches!(f.ty, rtemis_r8::ir::FieldType::R { .. })
                        || matches!(&f.ty, rtemis_r8::ir::FieldType::Option { inner, .. } if matches!(**inner, rtemis_r8::ir::FieldType::R { .. }))
                })
                .map(|f| f.name.to_string())
        };
        let found = match def {
            rtemis_r8::ir::TypeDef::Struct(st) => runtime(&st.fields),
            rtemis_r8::ir::TypeDef::Enum(e) => e.variants.iter().find_map(|var| match &var.shape {
                rtemis_r8::ir::VariantShape::Struct { fields, .. } => runtime(fields),
                rtemis_r8::ir::VariantShape::Newtype { of } => doc
                    .types
                    .iter()
                    .find(|t| format!("{ns}::{}", t.name()) == of.to_string())
                    .and_then(|t| match t {
                        rtemis_r8::ir::TypeDef::Struct(st) => runtime(&st.fields),
                        _ => None,
                    }),
                rtemis_r8::ir::VariantShape::Unit => None,
            }),
        };
        if let Some(field) = found {
            return Err(fail(format!(
                "the Rust-native type `{name}` of `{package}` declares the runtime-only field `{field}`; its codec's state holds every field"
            )));
        }
        types.push((name, VTable(v)));
    }
    let mut impls = Vec::new();
    for entry in entries {
        let key = (
            text(entry.trait_name, "trait name")?,
            text(entry.type_name, "type name")?,
            text(entry.method, "method name")?,
        );
        let call = entry.call.ok_or_else(|| {
            fail(format!(
                "the impl function of `{}` for `{}` is NULL",
                key.0, key.1
            ))
        })?;
        let declared = doc.impls.iter().any(|i| {
            i.r#trait.to_string() == key.0
                && matches!(&i.target, ImplTarget::Type { name } if name.to_string() == key.1)
                && i.methods.iter().any(|m| m.as_str() == key.2)
        });
        if !declared {
            return Err(fail(format!(
                "the impl function `{}` of `{}` for `{}` has no impl in the IR document of `{package}`",
                key.2, key.0, key.1
            )));
        }
        // The number of parameters agrees with the trait's declaration,
        // registered by the trait's package.
        let declared_params = crate::declare::registry().lock().ok().and_then(|docs| {
            let q = QualifiedName::parse(&key.0).ok()?;
            let t = docs
                .documents()
                .iter()
                .find(|d| d.namespace == q.namespace)?
                .traits
                .iter()
                .find(|t| t.name == q.name)?;
            t.methods
                .iter()
                .find(|m| m.name.as_str() == key.2)
                .map(|m| m.params.len())
        });
        match declared_params {
            Some(n) if n != entry.n_params => {
                return Err(fail(format!(
                    "the impl function `{}` of `{}` for `{}` takes {} parameters, and the trait declares {n}",
                    key.2, key.0, key.1, entry.n_params
                )));
            }
            Some(_) => {}
            None => {
                return Err(fail(format!(
                    "the trait `{}` declares no method `{}`; its package declares it before `{package}` registers",
                    key.0, key.2
                )));
            }
        }
        impls.push((key, ImplFn { call }));
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

extendr_module! {
    mod exports;
    fn r8_exports_info;
    fn r8_exports_document;
    fn r8_exports_compare;
    fn r8_register_exports;
    fn r8_impl_fn;
    fn r8_compiled_types;
}
