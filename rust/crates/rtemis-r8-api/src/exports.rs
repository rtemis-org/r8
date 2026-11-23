//! The exports table of a package (`R8Exports`): what `r8::exports!`
//! builds from the package's Rust-stored types and the impls that
//! `#[r8::methods]` generates.

use std::ffi::{CStr, c_void};
use std::sync::OnceLock;

use rtemis_r8::abi::{self, ABI_VERSION, MethodFn};
use rtemis_r8::ir::TypeDef;
use rtemis_r8::name::Name;

use crate::keep;
use crate::sys;

/// One method of an impl: its name, the generated function, and the number
/// of its parameters, `x` included and `...` counted as one.
pub struct MethodRow {
    /// The method's name, as the trait declares it.
    pub name: &'static CStr,
    /// The generated function.
    pub call: MethodFn,
    /// The number of parameters.
    pub n_params: usize,
}

/// The methods of one impl, which `#[r8::methods]` generates.
pub struct ImplSet {
    /// The qualified trait name.
    pub trait_name: &'static CStr,
    /// The qualified type name.
    pub type_name: &'static CStr,
    /// The methods.
    pub methods: &'static [MethodRow],
}

/// The package's exports table, built on first use: the declaration record
/// of `types` (the declarations of the Rust-stored types), the codec tables
/// `codecs`, and the impl entries, from which R8 builds the impl
/// declarations.
///
/// # Panics
///
/// When the namespace is not a valid R8 name, which the macros check at
/// compile time.
#[doc(hidden)]
pub fn table(
    cell: &'static OnceLock<usize>,
    package: &'static CStr,
    namespace: &str,
    codecs: &'static [abi::TypeVTable],
    types: impl FnOnce() -> Vec<TypeDef>,
    impls: &'static [&'static ImplSet],
) -> *const abi::Exports {
    *cell.get_or_init(|| {
        use rtemis_r8::record::{self, DeclarationRef};
        let ns = Name::new(namespace).expect("the macro checks the namespace");
        let types = types();
        let bytes = record::encode(types.iter().map(|t| (&ns, DeclarationRef::Type(t))));
        let ir: &'static [u8] = Box::leak(bytes.into_boxed_slice());
        let entries: Vec<abi::ImplEntry> = impls
            .iter()
            .flat_map(|s| {
                s.methods.iter().map(|m| abi::ImplEntry {
                    trait_name: s.trait_name.as_ptr(),
                    type_name: s.type_name.as_ptr(),
                    method: m.name.as_ptr(),
                    call: Some(m.call),
                    n_params: m.n_params,
                })
            })
            .collect();
        let entries: &'static [abi::ImplEntry] = Box::leak(entries.into_boxed_slice());
        let e = Box::leak(Box::new(abi::Exports {
            abi_version: ABI_VERSION,
            size: std::mem::size_of::<abi::Exports>() as u32,
            package: package.as_ptr(),
            ir_record: ir.as_ptr(),
            ir_record_len: ir.len(),
            types: codecs.as_ptr(),
            n_types: codecs.len(),
            impls: entries.as_ptr(),
            n_impls: entries.len(),
        }));
        std::ptr::from_ref(e) as usize
    }) as *const abi::Exports
}

/// Registers the package's exports function with
/// `R_RegisterCCallable(package, "r8_exports", f)` and records R's main
/// thread; `r8::exports!` emits `r8_init()`, which calls it.
///
/// # Safety
///
/// Called from the package's initialization routine.
#[doc(hidden)]
pub unsafe fn register(package: &'static CStr, f: unsafe extern "C" fn() -> *const abi::Exports) {
    keep::init();
    unsafe {
        sys::R_RegisterCCallable(package.as_ptr(), c"r8_exports".as_ptr(), f as *const c_void)
    };
}
