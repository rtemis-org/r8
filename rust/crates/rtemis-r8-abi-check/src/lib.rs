//! Checks that the Rust ABI types of `rtemis_r8::abi` match the C header
//! `r/inst/include/R8.h`. The build script compiles `csrc/struct_offsets.c` against
//! the header; [`c_struct_offsets()`] reads the sizes and offsets the C compiler
//! computed.

unsafe extern "C" {
    fn r8_abi_struct_offsets(out: *mut usize, cap: usize) -> usize;
    fn r8_abi_version() -> u32;
    fn r8_abi_has_seed() -> u32;
    fn r8_abi_statuses() -> u32;
    fn r8_abi_positions() -> u32;
    fn r8_abi_values() -> u32;
    fn r8_abi_min_sizes(out: *mut usize);
    fn r8_abi_declared_in_r() -> u32;
}

/// The sizes and offsets of `R8.h`, in the order of `rtemis_r8::abi::struct_offsets()`.
pub fn c_struct_offsets() -> Vec<usize> {
    let mut out = vec![0usize; 256];
    // SAFETY: `out` holds `out.len()` writable entries, and the function
    // writes at most that many.
    let n = unsafe { r8_abi_struct_offsets(out.as_mut_ptr(), out.len()) };
    out.truncate(n);
    out
}

/// `R8_ABI_VERSION`, `R8_CALL_HAS_SEED`, the status codes `R8_OK`,
/// `R8_ERROR`, and `R8_UNWIND`, the kinds of a position,
/// `R8_POSITION_TYPE`, `R8_POSITION_CLASS`, and `R8_POSITION_ANY`, and the
/// relations of a value, `R8_VALUE_TYPE`, `R8_VALUE_SUBTYPE`, and
/// `R8_VALUE_IMPLEMENTOR` (one per hexadecimal digit), as the header defines
/// them.
pub fn c_constants() -> (u32, u32, u32, u32, u32) {
    // SAFETY: the functions take no arguments and return a constant.
    unsafe {
        (
            r8_abi_version(),
            r8_abi_has_seed(),
            r8_abi_statuses(),
            r8_abi_positions(),
            r8_abi_values(),
        )
    }
}

/// The minimum sizes of the header, `R8_CALL_INPUTS_MIN_SIZE`,
/// `R8_READER_MIN_SIZE`, `R8_CODEC_MIN_SIZE`, `R8_IMPL_ENTRY_MIN_SIZE`,
/// `R8_EXPORTS_MIN_SIZE`, `R8_API_MIN_SIZE`, `R8_EXPORTS_FUNCTIONS_SIZE`, and
/// `R8_FUNCTION_ENTRY_MIN_SIZE`, in that order.
pub fn c_min_sizes() -> [usize; 8] {
    let mut out = [0usize; 8];
    // SAFETY: `out` holds the eight entries that the function writes.
    unsafe { r8_abi_min_sizes(out.as_mut_ptr()) };
    out
}

/// `R8_DECLARED_IN_R` as the header defines it.
pub fn c_declared_in_r() -> u32 {
    // SAFETY: the function takes no arguments and returns a constant.
    unsafe { r8_abi_declared_in_r() }
}
