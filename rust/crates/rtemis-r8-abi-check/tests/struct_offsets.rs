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
    let positions = abi::POSITION_TYPE | abi::POSITION_CLASS << 4 | abi::POSITION_ANY << 8;
    let values = abi::VALUE_TYPE | abi::VALUE_SUBTYPE << 4 | abi::VALUE_IMPLEMENTOR << 8;
    assert_eq!(
        c_constants(),
        (
            abi::ABI_VERSION,
            abi::CALL_HAS_SEED,
            statuses,
            positions,
            values
        )
    );
}

#[test]
fn minimum_sizes_match() {
    assert_eq!(
        rtemis_r8_abi_check::c_min_sizes(),
        [
            abi::CALL_INPUTS_MIN_SIZE,
            abi::READER_MIN_SIZE,
            abi::CODEC_MIN_SIZE,
            abi::IMPL_ENTRY_MIN_SIZE,
            abi::EXPORTS_MIN_SIZE,
            abi::API_MIN_SIZE,
        ]
    );
}
