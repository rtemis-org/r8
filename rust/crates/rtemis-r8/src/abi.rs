//! The C interface between R packages and the R8 registry, ABI version 1.
//!
//! These `#[repr(C)]` types mirror `r/inst/include/R8.h` field for field,
//! which states the interface and its rules: every call on R's main thread,
//! no R errors or panics across the boundary, unprotected `SEXP` results,
//! each side freeing only what it allocated, and fields only appended within
//! a major version. The crate `rtemis-r8-abi-check` compiles the header and asserts
//! that [`layout()`] matches it.
//!
//! `rtemis-r8` does not depend on R, so an R value appears here as [`Sexp`], an
//! untyped pointer of the same size as `SEXP`.

use std::ffi::{c_char, c_void};

/// The ABI major version, `R8_ABI_VERSION` in the header.
pub const ABI_VERSION: u32 = 1;

/// An R value, `SEXP` in the header.
pub type Sexp = *mut c_void;

/// Return status: [`OK`] or [`ERROR`].
pub type Status = i32;
/// The call succeeded.
pub const OK: Status = 0;
/// The call failed; the error report holds the code and message.
pub const ERROR: Status = 1;
/// `R8_UNWIND`: R jumped (an error or an interrupt) inside `R8Api.protect`.
pub const UNWIND: Status = 2;

/// `R8_CONTEXT_HAS_SEED`: [`CallContext::seed`] holds a seed.
pub const CONTEXT_HAS_SEED: u32 = 1;

/// `R8Error`: an error report with a stable code and a message, each
/// truncated to fit and NUL-terminated.
#[repr(C)]
pub struct Error {
    /// A stable error code, NUL-terminated.
    pub code: [c_char; 64],
    /// The message, NUL-terminated.
    pub message: [c_char; 1024],
}

/// `R8Writer`: a sink for bytes supplied by the caller of `encode`.
#[repr(C)]
pub struct Writer {
    /// The caller's state, passed to `write`.
    pub context: *mut c_void,
    /// Appends bytes to the sink.
    pub write: Option<unsafe extern "C" fn(*mut c_void, *const u8, usize) -> Status>,
}

/// `R8Node`: a part of a value that R8 has read and validated, lent to a
/// codec for the length of one call and read only through [`Reader`].
pub type Node = *const c_void;

/// `R8_NODE_ABSENT`: a member or an item that the value does not hold.
pub const NODE_ABSENT: u32 = 0;
/// `R8_NODE_NULL`.
pub const NODE_NULL: u32 = 1;
/// `R8_NODE_LOGICAL`.
pub const NODE_LOGICAL: u32 = 2;
/// `R8_NODE_NUMBER`.
pub const NODE_NUMBER: u32 = 3;
/// `R8_NODE_STRING`.
pub const NODE_STRING: u32 = 4;
/// `R8_NODE_ARRAY`.
pub const NODE_ARRAY: u32 = 5;
/// `R8_NODE_OBJECT`.
pub const NODE_OBJECT: u32 = 6;

/// `R8_NA_INTEGER`: the integer and logical a null reads as, R's
/// `NA_integer_`.
pub const NA_INTEGER: i32 = i32::MIN;
/// `R8_NA_DOUBLE_BITS`: the bits of the double a null reads as, R's
/// `NA_real_`, a NaN whose low word is 1954.
pub const NA_DOUBLE_BITS: u64 = 0x7FF0_0000_0000_07A2;

/// `R8Reader`: the functions through which a codec reads a value R8 lends
/// it. The reader, its nodes, and the strings it lends live for the call
/// that receives them; the codec writes block reads into memory it owns.
#[repr(C)]
pub struct Reader {
    /// The size of this struct in bytes.
    pub size: u32,
    /// The reader's state, passed to every function.
    pub context: *mut c_void,
    /// The node's kind, an `R8_NODE_*` constant; a NULL node is absent.
    pub kind: Option<unsafe extern "C" fn(*mut c_void, Node) -> u32>,
    /// An object's member by name, or NULL when the object has none.
    pub field: Option<unsafe extern "C" fn(*mut c_void, Node, *const c_char) -> Node>,
    /// An array's item, or NULL past its end.
    pub item: Option<unsafe extern "C" fn(*mut c_void, Node, usize) -> Node>,
    /// An array's or an object's length; 0 for any other node.
    pub length: Option<unsafe extern "C" fn(*mut c_void, Node) -> usize>,
    /// A logical: 0 or 1, and [`NA_INTEGER`] for null.
    pub logical: Option<unsafe extern "C" fn(*mut c_void, Node, *mut i32, *mut Error) -> Status>,
    /// A number: a non-finite double from its string form, and the double
    /// of [`NA_DOUBLE_BITS`] for null.
    pub number: Option<unsafe extern "C" fn(*mut c_void, Node, *mut f64, *mut Error) -> Status>,
    /// A string, UTF-8, lent with its length in bytes.
    pub string: Option<
        unsafe extern "C" fn(
            *mut c_void,
            Node,
            *mut *const c_char,
            *mut usize,
            *mut Error,
        ) -> Status,
    >,
    /// Items `start..start + count` of an array of numbers, as `number`
    /// reads each, into `out`.
    pub doubles: Option<
        unsafe extern "C" fn(*mut c_void, Node, usize, usize, *mut f64, *mut Error) -> Status,
    >,
    /// Items `start..start + count` of an array of whole numbers into
    /// `out`, [`NA_INTEGER`] for null.
    pub integers: Option<
        unsafe extern "C" fn(*mut c_void, Node, usize, usize, *mut i32, *mut Error) -> Status,
    >,
    /// Items `start..start + count` of an array of logicals into `out`, as
    /// `logical` reads each.
    pub logicals: Option<
        unsafe extern "C" fn(*mut c_void, Node, usize, usize, *mut i32, *mut Error) -> Status,
    >,
}

/// `R8CallContext`: the per-call inputs of an impl function.
#[repr(C)]
pub struct CallContext {
    /// The size of this struct in bytes, for appended fields.
    pub size: u32,
    /// Flags such as [`CONTEXT_HAS_SEED`].
    pub flags: u32,
    /// The seed of a stochastic method, when the flags say so.
    pub seed: u64,
}

/// `R8TypeVTable`: the codec of a Rust-native type. A package's codecs have
/// one size, `size`, which is the stride of its `types` array.
#[repr(C)]
pub struct TypeVTable {
    /// The size of this struct in bytes.
    pub size: u32,
    /// The qualified type name, NUL-terminated.
    pub type_name: *const c_char,
    /// Builds a state from the type's fields, R values in declaration order
    /// that R8 has validated and converted to their stored forms.
    pub from_fields:
        Option<unsafe extern "C" fn(*const Sexp, usize, *mut *mut c_void, *mut Error) -> Status>,
    /// Writes the state's JSON document to a writer, for export.
    pub encode: Option<unsafe extern "C" fn(*const c_void, *mut Writer, *mut Error) -> Status>,
    /// Writes the state's bytes, in a form of the package's choosing, for
    /// persistence and parallel workers. R8 calls it twice per
    /// serialization, to count the bytes and to write them, so every call
    /// writes the same bytes.
    pub write_state: Option<unsafe extern "C" fn(*const c_void, *mut Writer, *mut Error) -> Status>,
    /// Builds a state from bytes that `write_state` wrote.
    pub read_state:
        Option<unsafe extern "C" fn(*const u8, usize, *mut *mut c_void, *mut Error) -> Status>,
    /// Reads one field as an R value. `owner` is the external pointer that
    /// holds the state; a field that views the state's memory passes it as
    /// the view's owner. R8 calls it once per field and keeps the result.
    pub get_field: Option<
        unsafe extern "C" fn(*const c_void, Sexp, *const c_char, *mut Sexp, *mut Error) -> Status,
    >,
    /// Builds a new state with one field replaced by a validated R value;
    /// the new state shares the buffers of the unchanged fields.
    pub with_field: Option<
        unsafe extern "C" fn(
            *const c_void,
            *const c_char,
            Sexp,
            *mut *mut c_void,
            *mut Error,
        ) -> Status,
    >,
    /// Frees a value.
    pub drop: Option<unsafe extern "C" fn(*mut c_void)>,
    /// The bytes a state holds, including memory it owns.
    pub bytes: Option<unsafe extern "C" fn(*const c_void) -> usize>,
    /// The name of the state's variant, NUL-terminated, alive as long as
    /// the codec: set by an enum's codec, `None` in a struct's.
    pub variant: Option<unsafe extern "C" fn(*const c_void) -> *const c_char>,
    /// Builds a state of the named variant from its fields, in the
    /// variant's declaration order: set by an enum's codec, `None` in a
    /// struct's.
    pub from_variant: Option<
        unsafe extern "C" fn(
            *const c_char,
            *const Sexp,
            usize,
            *mut *mut c_void,
            *mut Error,
        ) -> Status,
    >,
    /// Builds a state from the value R8 has read from a document and
    /// validated, through the reader, from the value's root node. R8 reads a
    /// document through it when `size` covers it and it is set, and through
    /// `from_fields` or `from_variant` otherwise.
    pub from_reader:
        Option<unsafe extern "C" fn(*const Reader, Node, *mut *mut c_void, *mut Error) -> Status>,
    /// The declaration hash of the type the codec was written against,
    /// `sha256:<hex>`, NUL-terminated: set by a codec whose type has no
    /// declaration in the exports' record, a codec written by hand in C,
    /// and NULL otherwise. Registration requires it to equal the hash of
    /// the type's R declaration, since `from_fields` and `from_variant`
    /// read fields by position.
    pub declaration_hash: *const c_char,
}

/// `R8MethodFn`: an impl function.
pub type MethodFn =
    unsafe extern "C" fn(*const CallContext, *const Sexp, usize, *mut Sexp, *mut Error) -> Status;

/// `R8ImplEntry`: one method of one impl.
#[repr(C)]
pub struct ImplEntry {
    /// The qualified trait name, NUL-terminated.
    pub trait_name: *const c_char,
    /// The qualified type name, NUL-terminated.
    pub type_name: *const c_char,
    /// The method name, NUL-terminated.
    pub method: *const c_char,
    /// The impl function.
    pub call: Option<MethodFn>,
    /// The number of the method's parameters, `x` included and `...`
    /// counted as one, which registration checks against the trait's
    /// declaration.
    pub n_params: usize,
}

/// `R8Exports`: what a package registers.
#[repr(C)]
pub struct Exports {
    /// The ABI major version the package was built against.
    pub abi_version: u32,
    /// The size of this struct in bytes, for appended fields.
    pub size: u32,
    /// The package name, NUL-terminated.
    pub package: *const c_char,
    /// The declarations of the package's types in Rust as a declaration
    /// record ([`crate::record`]), which holds type declarations only; NULL
    /// in a table written by hand in C, whose types are the package's R
    /// declarations.
    pub ir_record: *const u8,
    /// The length of `ir_record` in bytes.
    pub ir_record_len: usize,
    /// The codecs of the package's Rust-native types.
    pub types: *const TypeVTable,
    /// The number of entries of `types`.
    pub n_types: usize,
    /// The package's impl functions.
    pub impls: *const ImplEntry,
    /// The number of entries of `impls`.
    pub n_impls: usize,
}

/// `R8Api`: R8's services to compiled code of other packages.
#[repr(C)]
pub struct Api {
    /// The ABI major version of R8.
    pub abi_version: u32,
    /// The size of this struct in bytes, for appended fields.
    pub size: u32,
    /// Wraps a Rust-native value of a registered type as an R value.
    pub new_value:
        Option<unsafe extern "C" fn(*const c_char, *mut c_void, *mut Sexp, *mut Error) -> Status>,
    /// The Rust-native state of an R value of a registered type.
    pub value_state:
        Option<unsafe extern "C" fn(Sexp, *const c_char, *mut *const c_void, *mut Error) -> Status>,
    /// Views doubles that `owner` keeps alive as an R vector, without copying.
    pub view_doubles: Option<ViewFn<f64>>,
    /// Views integers that `owner` keeps alive as an R vector.
    pub view_integers: Option<ViewFn<i32>>,
    /// Views logicals in R's form (0, 1, `NA_LOGICAL`) as an R vector.
    pub view_logicals: Option<ViewFn<i32>>,
    /// Views bytes as an R raw vector.
    pub view_bytes: Option<ViewFn<u8>>,
    /// Calls `fn(data)`, which may call R functions that jump, and stores
    /// its result; `UNWIND` when R jumped, which R8 continues once the impl
    /// function that received it returns it.
    pub protect: Option<
        unsafe extern "C" fn(
            unsafe extern "C" fn(*mut c_void) -> Sexp,
            *mut c_void,
            *mut Sexp,
            *mut Error,
        ) -> Status,
    >,
}

/// `R8ExportsFn`: the function a package registers as `"r8_exports"`.
pub type ExportsFn = unsafe extern "C" fn() -> *const Exports;

/// A view function of [`Api`]: the owner, the elements, their number, the
/// view, and the error report.
pub type ViewFn<T> = unsafe extern "C" fn(Sexp, *const T, usize, *mut Sexp, *mut Error) -> Status;

/// The size of every struct and the offset of every field, in the order of
/// `layout.c` in `rtemis-r8-abi-check`, which computes the same list from the C
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
        ("R8TypeVTable.type_name", offset_of!(TypeVTable, type_name)),
        (
            "R8TypeVTable.from_fields",
            offset_of!(TypeVTable, from_fields),
        ),
        ("R8TypeVTable.encode", offset_of!(TypeVTable, encode)),
        (
            "R8TypeVTable.write_state",
            offset_of!(TypeVTable, write_state),
        ),
        (
            "R8TypeVTable.read_state",
            offset_of!(TypeVTable, read_state),
        ),
        ("R8TypeVTable.get_field", offset_of!(TypeVTable, get_field)),
        (
            "R8TypeVTable.with_field",
            offset_of!(TypeVTable, with_field),
        ),
        ("R8TypeVTable.drop", offset_of!(TypeVTable, drop)),
        ("R8TypeVTable.bytes", offset_of!(TypeVTable, bytes)),
        ("R8TypeVTable.variant", offset_of!(TypeVTable, variant)),
        (
            "R8TypeVTable.from_variant",
            offset_of!(TypeVTable, from_variant),
        ),
        (
            "R8TypeVTable.from_reader",
            offset_of!(TypeVTable, from_reader),
        ),
        (
            "R8TypeVTable.declaration_hash",
            offset_of!(TypeVTable, declaration_hash),
        ),
        ("R8Reader", size_of::<Reader>()),
        ("R8Reader.context", offset_of!(Reader, context)),
        ("R8Reader.kind", offset_of!(Reader, kind)),
        ("R8Reader.field", offset_of!(Reader, field)),
        ("R8Reader.item", offset_of!(Reader, item)),
        ("R8Reader.length", offset_of!(Reader, length)),
        ("R8Reader.logical", offset_of!(Reader, logical)),
        ("R8Reader.number", offset_of!(Reader, number)),
        ("R8Reader.string", offset_of!(Reader, string)),
        ("R8Reader.doubles", offset_of!(Reader, doubles)),
        ("R8Reader.integers", offset_of!(Reader, integers)),
        ("R8Reader.logicals", offset_of!(Reader, logicals)),
        ("R8ImplEntry", size_of::<ImplEntry>()),
        ("R8ImplEntry.type_name", offset_of!(ImplEntry, type_name)),
        ("R8ImplEntry.method", offset_of!(ImplEntry, method)),
        ("R8ImplEntry.call", offset_of!(ImplEntry, call)),
        ("R8ImplEntry.n_params", offset_of!(ImplEntry, n_params)),
        ("R8Exports", size_of::<Exports>()),
        ("R8Exports.size", offset_of!(Exports, size)),
        ("R8Exports.package", offset_of!(Exports, package)),
        ("R8Exports.ir_record", offset_of!(Exports, ir_record)),
        (
            "R8Exports.ir_record_len",
            offset_of!(Exports, ir_record_len),
        ),
        ("R8Exports.types", offset_of!(Exports, types)),
        ("R8Exports.n_types", offset_of!(Exports, n_types)),
        ("R8Exports.impls", offset_of!(Exports, impls)),
        ("R8Exports.n_impls", offset_of!(Exports, n_impls)),
        ("R8Api", size_of::<Api>()),
        ("R8Api.size", offset_of!(Api, size)),
        ("R8Api.new_value", offset_of!(Api, new_value)),
        ("R8Api.value_state", offset_of!(Api, value_state)),
        ("R8Api.view_doubles", offset_of!(Api, view_doubles)),
        ("R8Api.view_integers", offset_of!(Api, view_integers)),
        ("R8Api.view_logicals", offset_of!(Api, view_logicals)),
        ("R8Api.view_bytes", offset_of!(Api, view_bytes)),
        ("R8Api.protect", offset_of!(Api, protect)),
    ]
}
