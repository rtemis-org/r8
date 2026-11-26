//! The exports table of a package (`R8Exports`): what `r8::exports!`
//! builds from the package's Rust-stored types and the impls of its rows.

use std::ffi::{CStr, c_void};
use std::sync::OnceLock;

use rtemis_r8::abi::{self, ABI_VERSION, MethodFn};
use rtemis_r8::ir::{MethodDef, RequiredField, TraitDef, TypeDef};
use rtemis_r8::name::Name;
use rtemis_r8::rules::DocumentSet;

use crate::dispatch::PositionEntry;
use crate::keep;
use crate::rust_stored::module;
use crate::sys;

/// One method of an impl: its name, the generated function, and its signature.
pub struct MethodRow {
    /// The method's name, as the trait declares it.
    pub name: &'static CStr,
    /// The generated function.
    pub call: MethodFn,
    /// The method's signature.
    pub signature: MethodDef,
}

/// The methods of one impl, which the function that `#[r8::methods]` adds
/// to a trait gives for a type that implements it.
pub struct ImplSet {
    /// The qualified trait name.
    pub trait_name: &'static CStr,
    /// The type the trait is implemented for, then the trait's other
    /// dispatched positions; for `R8::From`, the type and the source.
    pub positions: Vec<PositionEntry>,
    /// The methods.
    pub methods: Vec<MethodRow>,
    /// The trait's tag member, when the Rust trait states one.
    pub tag: Option<&'static str>,
    /// The fields that the trait's accessors give.
    pub required_fields: Vec<RequiredField>,
    /// Adds the declarations of the types of the impl's positions, parameters,
    /// and results.
    pub register: fn(&mut DocumentSet),
}

/// The declaration record of the signatures of an impl's methods.
fn signature(set: &mut ImplSet, ns: &Name) -> &'static [u8] {
    use rtemis_r8::record::{self, DeclarationRef};
    let methods: Vec<MethodDef> = set.methods.iter().map(|m| m.signature.clone()).collect();
    let name = set.trait_name.to_str().unwrap_or_default();
    let base = name.split_once("::").map_or(name, |(_, b)| b);
    let t = crate::signature::trait_def(
        base,
        "",
        set.tag,
        false,
        methods,
        std::mem::take(&mut set.required_fields),
    );
    Box::leak(record::encode([(ns, DeclarationRef::Trait(&t))]).into_boxed_slice())
}

/// A position as an impl entry states it.
fn position(p: &PositionEntry) -> abi::Position {
    let (kind, name) = match p {
        PositionEntry::Type(name) => (abi::POSITION_TYPE, name.as_ptr()),
        PositionEntry::Any => (abi::POSITION_ANY, std::ptr::null()),
    };
    abi::Position {
        kind,
        name,
        package: std::ptr::null(),
        system: std::ptr::null(),
    }
}

/// The package's exports table, built on first use: the declaration record
/// of the types that `types` adds and of `traits`, the traits declared in Rust,
/// with the types of other namespaces that they and the impls reach; the codec
/// tables `codecs`; and the entries of the impls that `impls` give, from which
/// R8 builds the impl declarations.
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
    codecs: &'static [abi::Codec],
    types: impl FnOnce(&mut DocumentSet),
    traits: &[fn() -> TraitDef],
    impls: &'static [fn() -> ImplSet],
) -> *const abi::Exports {
    // A declaration that panics, such as a field type that its constraints do
    // not fit, unwinds to here, not across `extern "C"`; R8 refuses the NULL
    // table and names the message written to the console.
    let built = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        build(cell, package, namespace, codecs, types, traits, impls)
    }));
    match built {
        Ok(p) => p,
        Err(payload) => {
            let why = payload
                .downcast_ref::<&str>()
                .map(|s| s.to_string())
                .or_else(|| payload.downcast_ref::<String>().cloned())
                .unwrap_or_else(|| "a panic without a message".to_string());
            let line = std::ffi::CString::new(format!(
                "The R8 declarations of package '{}' do not build: {}\n",
                package.to_string_lossy(),
                why.replace('\0', " ")
            ))
            .unwrap_or_default();
            unsafe { sys::REprintf(c"%s".as_ptr(), line.as_ptr()) };
            std::ptr::null()
        }
    }
}

/// The table of [`table()`], built once.
fn build(
    cell: &'static OnceLock<usize>,
    package: &'static CStr,
    namespace: &str,
    codecs: &'static [abi::Codec],
    types: impl FnOnce(&mut DocumentSet),
    traits: &[fn() -> TraitDef],
    impls: &'static [fn() -> ImplSet],
) -> *const abi::Exports {
    *cell.get_or_init(|| {
        use rtemis_r8::record::{self, DeclarationRef};
        let ns = Name::new(namespace).expect("the macro checks the namespace");
        let mut set = DocumentSet::default();
        types(&mut set);
        // The namespace's own types are those that the listed types and traits
        // reach; a signature adds types of other namespaces only.
        let own: Vec<TypeDef> = set
            .namespace(&ns)
            .map(|d| d.types.clone())
            .unwrap_or_default();
        let traits: Vec<TraitDef> = traits.iter().map(|t| t()).collect();
        let sets: Vec<ImplSet> = impls.iter().map(|f| f()).collect();
        for s in &sets {
            (s.register)(&mut set);
        }
        let foreign = set.into_documents();
        let bytes = record::encode(
            own.iter()
                .map(DeclarationRef::Type)
                .chain(traits.iter().map(DeclarationRef::Trait))
                .map(|d| (&ns, d))
                .chain(foreign.iter().filter(|d| d.namespace != ns).flat_map(|d| {
                    d.types
                        .iter()
                        .map(|t| (&d.namespace, DeclarationRef::Type(t)))
                })),
        );
        let ir: &'static [u8] = Box::leak(bytes.into_boxed_slice());
        let entries: Vec<abi::ImplEntry> = sets
            .into_iter()
            .flat_map(|mut set| {
                let signature = signature(&mut set, &ns);
                let positions: &'static [abi::Position] =
                    Box::leak(set.positions.iter().map(position).collect());
                set.methods.into_iter().map(move |m| abi::ImplEntry {
                    trait_name: set.trait_name.as_ptr(),
                    positions: positions.as_ptr(),
                    n_positions: positions.len(),
                    method: m.name.as_ptr(),
                    call: Some(m.call),
                    signature: signature.as_ptr(),
                    signature_len: signature.len(),
                })
            })
            .collect();
        let entries: &'static [abi::ImplEntry] = Box::leak(entries.into_boxed_slice());
        let e = Box::leak(Box::new(abi::Exports {
            abi_version: ABI_VERSION,
            size: std::mem::size_of::<abi::Exports>() as u32,
            package: package.as_ptr(),
            module: module(),
            ir_record: ir.as_ptr(),
            ir_record_len: ir.len(),
            types: codecs.as_ptr(),
            n_types: codecs.len(),
            impls: entries.as_ptr(),
            n_impls: entries.len(),
            impl_size: std::mem::size_of::<abi::ImplEntry>(),
        }));
        std::ptr::from_ref(e) as usize
    }) as *const abi::Exports
}

/// Registers the package's exports function with
/// `R_RegisterCCallable(package, "r8_exports", f)` and records R's main
/// thread; `r8::exports!` emits `r8_init_<package>()`, which calls it.
///
/// # Safety
///
/// Called from the package's initialization routine.
#[doc(hidden)]
pub unsafe fn register(package: &'static CStr, f: unsafe extern "C" fn() -> *const abi::Exports) {
    keep::init();
    pin::pin(module());
    unsafe {
        sys::R_RegisterCCallable(package.as_ptr(), c"r8_exports".as_ptr(), f as *const c_void)
    };
}

/// The package's shared object, kept loaded for the session: a value of a
/// Rust-stored type holds the address of its codec and its state, which an
/// unload, such as `pkgload::unload()`, would leave pointing into unmapped
/// memory when the value's finalizer runs.
mod pin {
    use std::ffi::{c_char, c_int, c_void};

    /// Adds a reference to the shared object that holds `addr`, which R's
    /// unload does not remove.
    #[cfg(all(unix, not(target_family = "wasm")))]
    pub(super) fn pin(addr: *const c_void) {
        #[repr(C)]
        struct DlInfo {
            fname: *const c_char,
            fbase: *mut c_void,
            sname: *const c_char,
            saddr: *mut c_void,
        }
        unsafe extern "C" {
            fn dladdr(addr: *const c_void, info: *mut DlInfo) -> c_int;
            fn dlopen(file: *const c_char, mode: c_int) -> *mut c_void;
        }
        // `RTLD_NOW` is 2 on Linux, macOS, the BSDs, and Solaris.
        const RTLD_NOW: c_int = 2;
        let mut info = DlInfo {
            fname: std::ptr::null(),
            fbase: std::ptr::null_mut(),
            sname: std::ptr::null(),
            saddr: std::ptr::null_mut(),
        };
        unsafe {
            if dladdr(addr, &mut info) != 0 && !info.fname.is_null() {
                // The object is loaded, so this adds a reference and loads
                // nothing; the handle is not closed.
                let _ = dlopen(info.fname, RTLD_NOW);
            }
        }
    }

    /// Pins the module that holds `addr` (`GET_MODULE_HANDLE_EX_FLAG_PIN`).
    #[cfg(windows)]
    pub(super) fn pin(addr: *const c_void) {
        unsafe extern "system" {
            fn GetModuleHandleExW(flags: u32, name: *const u16, module: *mut *mut c_void) -> c_int;
        }
        const FROM_ADDRESS: u32 = 0x4;
        const PIN: u32 = 0x1;
        let mut module = std::ptr::null_mut();
        unsafe {
            let _ = GetModuleHandleExW(FROM_ADDRESS | PIN, addr.cast(), &mut module);
        }
    }

    /// webR loads a package's code once and does not unload it.
    #[cfg(target_family = "wasm")]
    pub(super) fn pin(_addr: *const c_void) {}
}
