//! The interface between R8 and the compiled code of R packages, ABI version 2,
//! which packages reach through the crate `rtemis-r8-api`.
//!
//! These `#[repr(C)]` types mirror `r/inst/include/R8.h` field for field,
//! which states the interface and its rules: every call on R's main thread,
//! no R errors or panics across the boundary, unprotected `SEXP` results,
//! each side freeing only what it allocated, and fields only appended within
//! a major version. The crate `rtemis-r8-abi-check` compiles the header and asserts
//! that [`struct_offsets()`] matches it.
//!
//! `rtemis-r8` does not depend on R, so an R value appears here as [`Sexp`], an
//! untyped pointer of the same size as `SEXP`.

use std::ffi::{c_char, c_void};

/// The ABI major version, `R8_ABI_VERSION` in the header.
pub const ABI_VERSION: u32 = 2;

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

/// `R8_CALL_HAS_SEED`: [`CallInputs::seed`] holds a seed.
pub const CALL_HAS_SEED: u32 = 1;

/// `R8Error`: an error report with a stable code and a message, each
/// truncated to fit and NUL-terminated.
#[repr(C)]
pub struct Error {
    /// A stable error code, NUL-terminated.
    pub code: [c_char; 64],
    /// The message, NUL-terminated.
    pub message: [c_char; 1024],
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

/// `R8_VALUE_TYPE`: a value of the named type, exactly.
pub const VALUE_TYPE: u32 = 0;
/// `R8_VALUE_SUBTYPE`: a value of the named struct or of a struct that
/// extends it.
pub const VALUE_SUBTYPE: u32 = 1;
/// `R8_VALUE_IMPLEMENTOR`: a value of an implementor of the named trait.
pub const VALUE_IMPLEMENTOR: u32 = 2;

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
    pub member: Option<unsafe extern "C" fn(*mut c_void, Node, *const c_char) -> Node>,
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
    /// The R value of a node, unprotected: of the type, of a subtype of the
    /// struct, or of an implementor of the trait that the name, NUL-terminated,
    /// names, by a `VALUE_*` constant.
    pub value: Option<
        unsafe extern "C" fn(
            *mut c_void,
            Node,
            u32,
            *const c_char,
            *mut Sexp,
            *mut Error,
        ) -> Status,
    >,
}

/// `R8CallInputs`: the per-call inputs of an impl function.
#[repr(C)]
pub struct CallInputs {
    /// The size of this struct in bytes, for appended fields.
    pub size: u32,
    /// Flags such as [`CALL_HAS_SEED`].
    pub flags: u32,
    /// The seed of a stochastic method, when the flags say so.
    pub seed: u64,
}

/// `R8Codec`: the codec of a Rust-stored type. A package's codecs have
/// one size, `size`, which is the stride of its `types` array.
#[repr(C)]
pub struct Codec {
    /// The size of this struct in bytes.
    pub size: u32,
    /// The qualified type name, NUL-terminated.
    pub type_name: *const c_char,
    /// The compiled code that builds and reads the codec's states: the
    /// address of an object of the package's shared object, the same in
    /// each of its codecs. A state crosses to compiled code that names the
    /// same module only, since its layout is that code's.
    pub module: *const c_void,
    /// Builds a state from the type's fields, R values in declaration order
    /// that R8 has validated and converted to their stored forms.
    pub from_fields:
        Option<unsafe extern "C" fn(*const Sexp, usize, *mut *mut c_void, *mut Error) -> Status>,
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
    /// The bytes a state owns: its inline size and the memory it owns.
    pub owned_bytes: Option<unsafe extern "C" fn(*const c_void) -> usize>,
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
    /// document through it.
    pub from_reader:
        Option<unsafe extern "C" fn(*const Reader, Node, *mut *mut c_void, *mut Error) -> Status>,
    /// The state of the ancestor named, NUL-terminated, that the state
    /// embeds, which lives as long as the state; NULL when it embeds none of
    /// that name. `None` in the codec of a type whose states embed no
    /// ancestor.
    pub upcast: Option<unsafe extern "C" fn(*const c_void, *const c_char) -> *const c_void>,
}

/// `R8MethodFn`: an impl function.
pub type MethodFn =
    unsafe extern "C" fn(*const CallInputs, *const Sexp, usize, *mut Sexp, *mut Error) -> Status;

/// `R8_POSITION_TYPE`: a position that names an R8 type.
pub const POSITION_TYPE: u32 = 0;
/// `R8_POSITION_CLASS`: a position that names a class of the host.
pub const POSITION_CLASS: u32 = 1;
/// `R8_POSITION_ANY`: a position of any value.
pub const POSITION_ANY: u32 = 2;

/// `R8Position`: a position of an impl.
#[repr(C)]
pub struct Position {
    /// [`POSITION_TYPE`], [`POSITION_CLASS`], or [`POSITION_ANY`].
    pub kind: u32,
    /// The qualified name of a type, or the name of a class, NUL-terminated;
    /// NULL for any.
    pub name: *const c_char,
    /// The package that defines a class, NUL-terminated; NULL or empty when
    /// the declaration names none.
    pub package: *const c_char,
    /// The class system of a class, NUL-terminated: `s3`, `s4`, or
    /// `implicit`.
    pub system: *const c_char,
}

/// `R8ImplEntry`: one method of one impl.
#[repr(C)]
pub struct ImplEntry {
    /// The qualified trait name, NUL-terminated.
    pub trait_name: *const c_char,
    /// The type the trait is implemented for, its first dispatched position,
    /// then the trait's other dispatched positions in order; for `R8::From`,
    /// the type and the source.
    pub positions: *const Position,
    /// The number of `positions`.
    pub n_positions: usize,
    /// The method name, NUL-terminated, as the trait declares it or as its
    /// identifier in the hosts other than R.
    pub method: *const c_char,
    /// The impl function.
    pub call: Option<MethodFn>,
    /// The signatures of the impl's methods as the compiled code has them, a
    /// declaration record ([`crate::record`]) of one trait that holds them, which
    /// registration checks against the trait's declaration. The entries of one
    /// impl have one.
    pub signature: *const u8,
    /// The length of `signature` in bytes.
    pub signature_len: usize,
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
    /// The compiled code of the package, as its codecs name it
    /// ([`Codec::module`]).
    pub module: *const c_void,
    /// The declarations of the package's types and traits in Rust as a
    /// declaration record ([`crate::record`]), with the types of other
    /// namespaces that the code was compiled against, which R8 compares with
    /// the registered ones ([`crate::refine`]) and does not register.
    pub ir_record: *const u8,
    /// The length of `ir_record` in bytes.
    pub ir_record_len: usize,
    /// The codecs of the package's Rust-stored types.
    pub types: *const Codec,
    /// The number of entries of `types`.
    pub n_types: usize,
    /// The package's impl functions.
    pub impls: *const ImplEntry,
    /// The number of entries of `impls`.
    pub n_impls: usize,
    /// The size of an entry of `impls` in bytes, the stride of the array.
    pub impl_size: usize,
}

/// `R8Api`: R8's services to compiled code of other packages.
#[repr(C)]
pub struct Api {
    /// The ABI major version of R8.
    pub abi_version: u32,
    /// The size of this struct in bytes, for appended fields.
    pub size: u32,
    /// Wraps a state of a registered Rust-stored type, which the caller's
    /// module built, as an R value. R8 owns the state once it returns
    /// [`OK`]; on an error the state stays the caller's.
    pub new_value: Option<NewValueFn>,
    /// Lends the state of a value of the named type to the caller's module.
    pub value_state: Option<StateFn>,
    /// Lends the state of a value of the named type, or, for a value of a
    /// struct that extends it, the state that the value's state embeds.
    pub upcast_state: Option<StateFn>,
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
    /// Builds a value of a registered struct or enum from its fields,
    /// whichever module its codec has.
    pub value_of_fields: Option<ValueOfFieldsFn>,
    /// Builds a value from another with some of its fields replaced, which R8
    /// validates with the rules that read them.
    pub with_fields: Option<WithFieldsFn>,
    /// Gives R8 the declarations, as a declaration record, that the caller's
    /// module was compiled against, of a type whose values it reads by their
    /// fields.
    pub pin: Option<PinFn>,
    /// The names of the type and of the variant of a value.
    pub value_type: Option<ValueTypeFn>,
}

/// `R8ExportsFn`: the function a package registers as `"r8_exports"`.
pub type ExportsFn = unsafe extern "C" fn() -> *const Exports;

/// `new_value` of [`Api`]: the type name, the caller's module, the state, the
/// value, and the error report.
pub type NewValueFn = unsafe extern "C" fn(
    *const c_char,
    *const c_void,
    *mut c_void,
    *mut Sexp,
    *mut Error,
) -> Status;

/// `value_state` and `upcast_state` of [`Api`]: the value, the type name, the
/// caller's module, the state, and the error report.
pub type StateFn = unsafe extern "C" fn(
    Sexp,
    *const c_char,
    *const c_void,
    *mut *const c_void,
    *mut Error,
) -> Status;

/// `value_of_fields` of [`Api`]: the type name, the variant of an enum's value
/// or NULL, the declaration hash that the caller was compiled against, the
/// list of the fields in declaration order and in their stored forms, the
/// value, and the error report.
pub type ValueOfFieldsFn = unsafe extern "C" fn(
    *const c_char,
    *const c_char,
    *const c_char,
    Sexp,
    *mut Sexp,
    *mut Error,
) -> Status;

/// `pin` of [`Api`]: the caller's module, the declaration record and its
/// length in bytes, and the error report.
pub type PinFn = unsafe extern "C" fn(*const c_void, *const u8, usize, *mut Error) -> Status;

/// `value_type` of [`Api`]: the value, its type's qualified name, and the
/// variant of an enum's value, NULL for a struct's; both are NULL for an object
/// that is not a value, and valid for the session.
pub type ValueTypeFn = unsafe extern "C" fn(Sexp, *mut *const c_char, *mut *const c_char) -> Status;

/// `with_fields` of [`Api`]: the value, the named list of the new fields, the
/// new value, and the error report.
pub type WithFieldsFn = unsafe extern "C" fn(Sexp, Sexp, *mut Sexp, *mut Error) -> Status;

/// A view function of [`Api`]: the owner, the elements, their number, the
/// view, and the error report.
pub type ViewFn<T> = unsafe extern "C" fn(Sexp, *const T, usize, *mut Sexp, *mut Error) -> Status;

/// The size of every struct and the offset of every field, in the order of
/// `struct_offsets.c` in `rtemis-r8-abi-check`, which computes the same list from the C
/// header.
pub fn struct_offsets() -> Vec<(&'static str, usize)> {
    use std::mem::{offset_of, size_of};
    vec![
        ("R8Error", size_of::<Error>()),
        ("R8Error.message", offset_of!(Error, message)),
        ("R8CallInputs", size_of::<CallInputs>()),
        ("R8CallInputs.flags", offset_of!(CallInputs, flags)),
        ("R8CallInputs.seed", offset_of!(CallInputs, seed)),
        ("R8Codec", size_of::<Codec>()),
        ("R8Codec.type_name", offset_of!(Codec, type_name)),
        ("R8Codec.module", offset_of!(Codec, module)),
        ("R8Codec.from_fields", offset_of!(Codec, from_fields)),
        ("R8Codec.get_field", offset_of!(Codec, get_field)),
        ("R8Codec.with_field", offset_of!(Codec, with_field)),
        ("R8Codec.drop", offset_of!(Codec, drop)),
        ("R8Codec.owned_bytes", offset_of!(Codec, owned_bytes)),
        ("R8Codec.variant", offset_of!(Codec, variant)),
        ("R8Codec.from_variant", offset_of!(Codec, from_variant)),
        ("R8Codec.from_reader", offset_of!(Codec, from_reader)),
        ("R8Codec.upcast", offset_of!(Codec, upcast)),
        ("R8Reader", size_of::<Reader>()),
        ("R8Reader.context", offset_of!(Reader, context)),
        ("R8Reader.kind", offset_of!(Reader, kind)),
        ("R8Reader.member", offset_of!(Reader, member)),
        ("R8Reader.item", offset_of!(Reader, item)),
        ("R8Reader.length", offset_of!(Reader, length)),
        ("R8Reader.logical", offset_of!(Reader, logical)),
        ("R8Reader.number", offset_of!(Reader, number)),
        ("R8Reader.string", offset_of!(Reader, string)),
        ("R8Reader.doubles", offset_of!(Reader, doubles)),
        ("R8Reader.integers", offset_of!(Reader, integers)),
        ("R8Reader.logicals", offset_of!(Reader, logicals)),
        ("R8Reader.value", offset_of!(Reader, value)),
        ("R8Position", size_of::<Position>()),
        ("R8Position.name", offset_of!(Position, name)),
        ("R8Position.package", offset_of!(Position, package)),
        ("R8Position.system", offset_of!(Position, system)),
        ("R8ImplEntry", size_of::<ImplEntry>()),
        ("R8ImplEntry.positions", offset_of!(ImplEntry, positions)),
        (
            "R8ImplEntry.n_positions",
            offset_of!(ImplEntry, n_positions),
        ),
        ("R8ImplEntry.method", offset_of!(ImplEntry, method)),
        ("R8ImplEntry.call", offset_of!(ImplEntry, call)),
        ("R8ImplEntry.signature", offset_of!(ImplEntry, signature)),
        (
            "R8ImplEntry.signature_len",
            offset_of!(ImplEntry, signature_len),
        ),
        ("R8Exports", size_of::<Exports>()),
        ("R8Exports.size", offset_of!(Exports, size)),
        ("R8Exports.package", offset_of!(Exports, package)),
        ("R8Exports.module", offset_of!(Exports, module)),
        ("R8Exports.ir_record", offset_of!(Exports, ir_record)),
        (
            "R8Exports.ir_record_len",
            offset_of!(Exports, ir_record_len),
        ),
        ("R8Exports.types", offset_of!(Exports, types)),
        ("R8Exports.n_types", offset_of!(Exports, n_types)),
        ("R8Exports.impls", offset_of!(Exports, impls)),
        ("R8Exports.n_impls", offset_of!(Exports, n_impls)),
        ("R8Exports.impl_size", offset_of!(Exports, impl_size)),
        ("R8Api", size_of::<Api>()),
        ("R8Api.size", offset_of!(Api, size)),
        ("R8Api.new_value", offset_of!(Api, new_value)),
        ("R8Api.value_state", offset_of!(Api, value_state)),
        ("R8Api.upcast_state", offset_of!(Api, upcast_state)),
        ("R8Api.view_doubles", offset_of!(Api, view_doubles)),
        ("R8Api.view_integers", offset_of!(Api, view_integers)),
        ("R8Api.view_logicals", offset_of!(Api, view_logicals)),
        ("R8Api.view_bytes", offset_of!(Api, view_bytes)),
        ("R8Api.protect", offset_of!(Api, protect)),
        ("R8Api.value_of_fields", offset_of!(Api, value_of_fields)),
        ("R8Api.with_fields", offset_of!(Api, with_fields)),
        ("R8Api.pin", offset_of!(Api, pin)),
        ("R8Api.value_type", offset_of!(Api, value_type)),
    ]
}
