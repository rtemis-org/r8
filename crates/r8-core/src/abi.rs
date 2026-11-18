//! The C interface between R packages and the R8 registry, ABI version 1.
//!
//! These `#[repr(C)]` types mirror `R8/inst/include/R8.h` field for field,
//! which states the interface and its rules: every call on R's main thread,
//! no R errors or panics across the boundary, unprotected `SEXP` results,
//! each side freeing only what it allocated, and fields only appended within
//! a major version. The crate `r8-abi-check` compiles the header and asserts
//! that [`layout()`] matches it.
//!
//! `r8-core` does not depend on R, so an R value appears here as [`Sexp`], an
//! untyped pointer of the same size as `SEXP`.

use std::ffi::{c_char, c_void};

/// The ABI major version, `R8_ABI_VERSION` in the header.
pub const ABI_VERSION: u32 = 1;

/// An R value, `SEXP` in the header.
pub type Sexp = *mut c_void;

/// Return status: [`OK`] or [`ERROR`].
pub type Status = i32;
pub const OK: Status = 0;
pub const ERROR: Status = 1;

/// `R8_CONTEXT_HAS_SEED`: [`CallContext::seed`] holds a seed.
pub const CONTEXT_HAS_SEED: u32 = 1;

/// `R8Error`: an error report with a stable code and a message, each
/// truncated to fit and NUL-terminated.
#[repr(C)]
pub struct Error {
    pub code: [c_char; 64],
    pub message: [c_char; 1024],
}

/// `R8Writer`: a sink for bytes supplied by the caller of `encode`.
#[repr(C)]
pub struct Writer {
    pub context: *mut c_void,
    pub write: Option<unsafe extern "C" fn(*mut c_void, *const u8, usize) -> Status>,
}

/// `R8CallContext`: the per-call inputs of an impl function.
#[repr(C)]
pub struct CallContext {
    pub size: u32,
    pub flags: u32,
    pub seed: u64,
}

/// `R8TypeVTable`: the codec of a Rust-native type.
#[repr(C)]
pub struct TypeVTable {
    pub type_name: *const c_char,
    pub decode:
        Option<unsafe extern "C" fn(*const u8, usize, *mut *mut c_void, *mut Error) -> Status>,
    pub encode: Option<unsafe extern "C" fn(*const c_void, *mut Writer, *mut Error) -> Status>,
    pub get_field:
        Option<unsafe extern "C" fn(*const c_void, *const c_char, *mut Sexp, *mut Error) -> Status>,
    pub with_field: Option<
        unsafe extern "C" fn(
            *const c_void,
            *const c_char,
            Sexp,
            *mut *mut c_void,
            *mut Error,
        ) -> Status,
    >,
    pub drop: Option<unsafe extern "C" fn(*mut c_void)>,
    pub size: Option<unsafe extern "C" fn(*const c_void) -> usize>,
}

/// `R8MethodFn`: an impl function.
pub type MethodFn =
    unsafe extern "C" fn(*const CallContext, *const Sexp, usize, *mut Sexp, *mut Error) -> Status;

/// `R8ImplEntry`: one method of one impl.
#[repr(C)]
pub struct ImplEntry {
    pub trait_name: *const c_char,
    pub type_name: *const c_char,
    pub method: *const c_char,
    pub call: Option<MethodFn>,
}

/// `R8Exports`: what a package registers.
#[repr(C)]
pub struct Exports {
    pub abi_version: u32,
    pub size: u32,
    pub package: *const c_char,
    pub ir_json: *const c_char,
    pub ir_json_len: usize,
    pub types: *const TypeVTable,
    pub n_types: usize,
    pub impls: *const ImplEntry,
    pub n_impls: usize,
}

/// `R8Api`: R8's services to compiled code of other packages.
#[repr(C)]
pub struct Api {
    pub abi_version: u32,
    pub size: u32,
    pub new_value:
        Option<unsafe extern "C" fn(*const c_char, *mut c_void, *mut Sexp, *mut Error) -> Status>,
    pub value_state:
        Option<unsafe extern "C" fn(Sexp, *const c_char, *mut *const c_void, *mut Error) -> Status>,
}

/// The size of every struct and the offset of every field, in the order of
/// `layout.c` in `r8-abi-check`, which computes the same list from the C
/// header.
pub fn layout() -> Vec<(&'static str, usize)> {
    use std::mem::{offset_of, size_of};
    vec![
        ("R8Error", size_of::<Error>()),
        ("R8Error.message", offset_of!(Error, message)),
        ("R8Writer", size_of::<Writer>()),
        ("R8Writer.write", offset_of!(Writer, write)),
        ("R8CallContext", size_of::<CallContext>()),
        ("R8CallContext.flags", offset_of!(CallContext, flags)),
        ("R8CallContext.seed", offset_of!(CallContext, seed)),
        ("R8TypeVTable", size_of::<TypeVTable>()),
        ("R8TypeVTable.decode", offset_of!(TypeVTable, decode)),
        ("R8TypeVTable.encode", offset_of!(TypeVTable, encode)),
        ("R8TypeVTable.get_field", offset_of!(TypeVTable, get_field)),
        (
            "R8TypeVTable.with_field",
            offset_of!(TypeVTable, with_field),
        ),
        ("R8TypeVTable.drop", offset_of!(TypeVTable, drop)),
        ("R8TypeVTable.size", offset_of!(TypeVTable, size)),
        ("R8ImplEntry", size_of::<ImplEntry>()),
        ("R8ImplEntry.type_name", offset_of!(ImplEntry, type_name)),
        ("R8ImplEntry.method", offset_of!(ImplEntry, method)),
        ("R8ImplEntry.call", offset_of!(ImplEntry, call)),
        ("R8Exports", size_of::<Exports>()),
        ("R8Exports.size", offset_of!(Exports, size)),
        ("R8Exports.package", offset_of!(Exports, package)),
        ("R8Exports.ir_json", offset_of!(Exports, ir_json)),
        ("R8Exports.ir_json_len", offset_of!(Exports, ir_json_len)),
        ("R8Exports.types", offset_of!(Exports, types)),
        ("R8Exports.n_types", offset_of!(Exports, n_types)),
        ("R8Exports.impls", offset_of!(Exports, impls)),
        ("R8Exports.n_impls", offset_of!(Exports, n_impls)),
        ("R8Api", size_of::<Api>()),
        ("R8Api.size", offset_of!(Api, size)),
        ("R8Api.new_value", offset_of!(Api, new_value)),
        ("R8Api.value_state", offset_of!(Api, value_state)),
    ]
}
