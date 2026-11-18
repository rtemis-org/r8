# justfile
# ::r8::
# 2026- EDG rtemis.org

# List available recipes
default:
    @just --list

# rustfmt check, clippy with warnings as errors, and tests of the workspace crates
rust-check:
    cargo fmt --all --check
    cargo clippy --workspace --all-targets --all-features --locked -- -D warnings
    cargo test --workspace --all-features --locked

# Format the workspace crates
rust-format:
    cargo fmt --all

# Build and test the workspace crates with the minimum supported Rust version
msrv:
    cargo +1.88 test --workspace --all-features --locked
