# justfile
# ::r8::
# 2026- EDG rtemis.org

# List available recipes
default:
    @just --list

# The Cargo workspace is in rust/, the R package in r/.

# rustfmt check, clippy with warnings as errors, and tests of the workspace crates
[working-directory: "rust"]
rust-check:
    cargo fmt --all --check
    cargo clippy --workspace --all-targets --all-features --locked -- -D warnings
    cargo test --workspace --all-features --locked

# Format the workspace crates
[working-directory: "rust"]
rust-format:
    cargo fmt --all

# Build and test the workspace crates with the minimum supported Rust version
[working-directory: "rust"]
msrv:
    cargo +1.88 test --workspace --all-features --locked
