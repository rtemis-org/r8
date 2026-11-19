//! Checks that the Rust ABI types of `rtemis_r8::abi` match the C header
//! `r/inst/include/R8.h`. The build script compiles `csrc/layout.c` against
//! the header; [`c_layout()`] reads the sizes and offsets the C compiler
//! computed.

unsafe extern "C" {
    fn r8_abi_layout(out: *mut usize, cap: usize) -> usize;
    fn r8_abi_version() -> u32;
    fn r8_abi_has_seed() -> u32;
}

/// The sizes and offsets of `R8.h`, in the order of `rtemis_r8::abi::layout()`.
pub fn c_layout() -> Vec<usize> {
    let mut out = vec![0usize; 256];
    // SAFETY: `out` holds `out.len()` writable entries, and the function
    // writes at most that many.
    let n = unsafe { r8_abi_layout(out.as_mut_ptr(), out.len()) };
    out.truncate(n);
    out
}

/// `R8_ABI_VERSION` and `R8_CONTEXT_HAS_SEED` as the header defines them.
pub fn c_constants() -> (u32, u32) {
    // SAFETY: both functions take no arguments and return a constant.
    unsafe { (r8_abi_version(), r8_abi_has_seed()) }
}
