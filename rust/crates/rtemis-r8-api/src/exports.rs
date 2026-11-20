//! The exports table of a package (`R8Exports`): what `r8::exports!`
//! builds from the package's Rust-native types and the impls that
//! `#[r8::methods]` generates.

use std::ffi::{CStr, c_void};
use std::sync::OnceLock;

use rtemis_r8::abi::{self, ABI_VERSION, MethodFn};
use rtemis_r8::ir::{IR_VERSION, ImplDef, ImplTarget, IrDocument, TypeDef};
use rtemis_r8::name::{MethodName, Name, QualifiedName};

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

/// The package's exports table, built on first use: the IR document of
/// `types` (the declarations of the Rust-native types) and the impls, the
/// codec tables `codecs`, and the impl entries.
///
/// # Panics
///
/// When a name is not a valid R8 name, which the macros check at compile
/// time.
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
        let text = |c: &CStr| c.to_str().expect("names are UTF-8").to_string();
        let doc = IrDocument {
            ir_version: IR_VERSION,
            namespace: Name::new(namespace).expect("the macro checks the namespace"),
            types: types(),
            traits: Vec::new(),
            impls: impls
                .iter()
                .map(|s| ImplDef {
                    r#trait: QualifiedName::parse(&text(s.trait_name))
                        .expect("the macro checks the trait name"),
                    type_args: Vec::new(),
                    target: ImplTarget::Type {
                        name: QualifiedName::parse(&text(s.type_name))
                            .expect("the derive checks the type name"),
                    },
                    tag_value: None,
                    methods: s
                        .methods
                        .iter()
                        .map(|m| {
                            MethodName::new(text(m.name)).expect("the macro checks the method name")
                        })
                        .collect(),
                    codec: false,
                })
                .collect(),
        };
        let ir: &'static str = Box::leak(
            serde_json::to_string(&doc)
                .expect("an IR document serializes")
                .into_boxed_str(),
        );
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
            ir_json: ir.as_ptr().cast(),
            ir_json_len: ir.len(),
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
