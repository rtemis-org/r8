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

# Check that the R package, its binding crate, and the workspace crates share a major and minor version
check-versions:
    bash tools/check-versions.sh

# Publish rtemis-r8-macros and rtemis-r8 to crates.io from a committed tree; then run `just update-crates` in r/
publish-crates: check-versions rust-check msrv
    @git diff --quiet && git diff --cached --quiet || (echo "Commit the working tree before publishing." && exit 1)
    cd rust && cargo publish --workspace --dry-run
    cd rust && cargo publish --workspace
