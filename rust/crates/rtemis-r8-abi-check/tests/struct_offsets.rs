//! The Rust ABI types and the C header agree on every size and offset.

use rtemis_r8::abi;
use rtemis_r8_abi_check::{c_constants, c_struct_offsets};

#[test]
fn rust_and_c_struct_offsets_match() {
    let rust = abi::struct_offsets();
    let c = c_struct_offsets();
    assert_eq!(
        rust.len(),
        c.len(),
        "the two lists of sizes and offsets have different lengths"
    );
    let mismatches: Vec<String> = rust
        .iter()
        .zip(&c)
        .filter(|((_, r), c)| r != *c)
        .map(|((name, r), c)| format!("{name}: Rust {r}, C {c}"))
        .collect();
    assert!(mismatches.is_empty(), "{mismatches:#?}");
}

#[test]
fn constants_match() {
    let statuses = (abi::OK | abi::ERROR << 4 | abi::UNWIND << 8) as u32;
    assert_eq!(
        c_constants(),
        (abi::ABI_VERSION, abi::CALL_HAS_SEED, statuses)
    );
}
