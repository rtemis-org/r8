#!/usr/bin/env bash
# Checks that the R packages R8 and R8.io, their binding crates r8-r and r8-io-r,
# and the workspace crates rtemis-r8, rtemis-r8-macros, rtemis-r8-api, and
# rtemis-r8-io share one major and minor version, and that the binding and the crates require that version of
# their dependencies. Patch versions are independent. It reports whether the
# tree is a release tree: the rtemis-r8 of the binding's Cargo.lock and of
# r/src/rust/vendor.tar.xz is a crates.io release of that version. With
# --release, it also fails when the tree is not a release tree. Run from the
# repository root.
set -euo pipefail

release=0
if [[ "${1:-}" == "--release" ]]; then
  release=1
fi

major_minor() { echo "$1" | cut -d. -f1,2; }

workspace=$(sed -n 's/^version = "\(.*\)"$/\1/p' rust/Cargo.toml | head -n 1)
macros_req=$(sed -n 's/^rtemis-r8-macros = .*version = "\([^"]*\)".*/\1/p' rust/crates/rtemis-r8/Cargo.toml)
r_package=$(sed -n 's/^Version: *//p' r/DESCRIPTION)
binding=$(sed -n "s/^version = '\(.*\)'$/\1/p" r/src/rust/Cargo.toml | head -n 1)
# A requirement is written `rtemis-r8 = '0.6'`, or as a table with `version`
# where it names features.
binding_req=$(sed -n -e "s/^rtemis-r8 = '\(.*\)'$/\1/p" -e "s/^rtemis-r8 = {.*version = '\([^']*\)'.*/\1/p" r/src/rust/Cargo.toml)
api_req=$(sed -n 's/^rtemis-r8 = .*version = "\([^"]*\)".*/\1/p' rust/crates/rtemis-r8-api/Cargo.toml)
api_macros_req=$(sed -n 's/^rtemis-r8-macros = .*version = "\([^"]*\)".*/\1/p' rust/crates/rtemis-r8-api/Cargo.toml)
io_req=$(sed -n 's/^rtemis-r8 = .*version = "\([^"]*\)".*/\1/p' rust/crates/rtemis-r8-io/Cargo.toml)
io_package=$(sed -n 's/^Version: *//p' r-io/DESCRIPTION)
io_binding=$(sed -n "s/^version = '\(.*\)'$/\1/p" r-io/src/rust/Cargo.toml | head -n 1)
io_binding_req=$(sed -n -e "s/^rtemis-r8 = '\(.*\)'$/\1/p" -e "s/^rtemis-r8 = {.*version = '\([^']*\)'.*/\1/p" r-io/src/rust/Cargo.toml)
io_binding_io_req=$(sed -n "s/^rtemis-r8-io = .*version = '\([^']*\)'.*/\1/p" r-io/src/rust/Cargo.toml)
io_r8=$(sed -n 's/^ *R8 (>= \([^)]*\)).*/\1/p' r-io/DESCRIPTION)

expected=$(major_minor "$workspace")
status=0
check() {
  local name=$1 value=$2 found
  found=$(major_minor "$value")
  if [[ "$found" == "$expected" ]]; then
    printf '  ok        %-40s %s\n' "$name" "$value"
  else
    printf '  MISMATCH  %-40s %s (expected %s)\n' "$name" "$value" "$expected"
    status=1
  fi
}

echo "Major and minor version: $expected (rust/Cargo.toml, workspace)"
check "rtemis-r8-macros requirement of rtemis-r8" "$macros_req"
check "R package R8 (r/DESCRIPTION)" "$r_package"
check "binding crate r8-r" "$binding"
check "binding requirement of rtemis-r8" "$binding_req"
check "rtemis-r8 requirement of rtemis-r8-api" "$api_req"
check "rtemis-r8-macros requirement of rtemis-r8-api" "$api_macros_req"
check "rtemis-r8 requirement of rtemis-r8-io" "$io_req"
check "R package R8.io (r-io/DESCRIPTION)" "$io_package"
check "R8 requirement of R8.io" "$io_r8"
check "binding crate r8-io-r" "$io_binding"
check "r8-io-r requirement of rtemis-r8" "$io_binding_req"
check "r8-io-r requirement of rtemis-r8-io" "$io_binding_io_req"

# The rtemis-r8 that a build without R8_CRATES_PATH compiles: the binding's
# lock entry, which names crates.io as its source, and the vendored copy.
locked=$(awk '/^name = "rtemis-r8"$/{f=1; next} f && /^version = /{gsub(/"/, "", $3); v=$3} f && /^source = "registry/{print v; exit} /^\[\[package\]\]/{f=0}' r/src/rust/Cargo.lock)
vendored=$(tar -xJOf r/src/rust/vendor.tar.xz vendor/rtemis-r8/Cargo.toml 2>/dev/null | sed -n 's/^version = "\(.*\)"$/\1/p' | head -n 1)
echo "rtemis-r8 of a build from crates.io: ${locked:-none} (Cargo.lock), ${vendored:-none} (vendor.tar.xz)"
if [[ -n "$locked" && "$locked" == "$vendored" && "$(major_minor "$locked")" == "$expected" ]]; then
  echo "Release tree: yes"
else
  echo "Release tree: no (just update-crates in r/ locks and vendors the published release)"
  if [[ $release -eq 1 ]]; then
    status=1
  fi
fi
exit $status
