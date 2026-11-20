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

# Branches: devel is the development line, where the binding builds against
# this checkout's crates; main is the release line, whose binding builds from
# the crates.io release of rtemis-r8 in r/src/rust/vendor.tar.xz, as
# r-universe and CRAN build it. A release:
#   just release-check     on devel, then commit
#   just publish-crates    publishes the workspace crates
#   just update-crates     in r/: locks and vendors them, then commit
#   just release           fast-forwards main to devel and tags it
# `just open-version X.Y.0` opens the next version on devel, with the first
# change of the binding that needs an unpublished item of rtemis-r8.

# Check that the R package, its binding crate, and the workspace crates share a major and minor version, and report whether the tree is a release tree
check-versions:
    bash tools/check-versions.sh

# Open version X.Y.Z on devel: every crate and R package version, the requirements between them, and a NEWS.md heading
open-version version:
    bash tools/open-version.sh {{ version }}

# Checks before publishing: versions, the workspace's checks and MSRV, a publish dry run, and R CMD check --as-cran against this checkout's crates
release-check: check-versions rust-check msrv
    cd rust && cargo publish --workspace --dry-run --allow-dirty
    cd r && R8_ORACLE= just check-cran

# Publish the workspace crates to crates.io from a committed tree; then run `just update-crates` in r/
publish-crates: release-check
    @git diff --quiet && git diff --cached --quiet || (echo "Commit the working tree before publishing." && exit 1)
    cd rust && cargo publish --workspace

# Fast-forward main to devel and tag the release, from a committed release tree on devel
release:
    @test "$(git rev-parse --abbrev-ref HEAD)" = devel || (echo "Release from devel." && exit 1)
    @git diff --quiet && git diff --cached --quiet || (echo "Commit the working tree before releasing." && exit 1)
    bash tools/check-versions.sh --release
    git tag "v$(sed -n 's/^Version: *//p' r/DESCRIPTION)"
    git push origin devel devel:main "v$(sed -n 's/^Version: *//p' r/DESCRIPTION)"
