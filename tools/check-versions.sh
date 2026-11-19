#!/usr/bin/env bash
# Checks that the R package R8, its binding crate r8-r, and the workspace
# crates rtemis-r8 and rtemis-r8-macros share one major and minor version, and
# that the binding and rtemis-r8 require that version of their dependencies.
# Patch versions are independent. Run from the repository root.
set -euo pipefail

major_minor() { echo "$1" | cut -d. -f1,2; }

workspace=$(sed -n 's/^version = "\(.*\)"$/\1/p' rust/Cargo.toml | head -n 1)
macros_req=$(sed -n 's/^rtemis-r8-macros = .*version = "\([^"]*\)".*/\1/p' rust/crates/rtemis-r8/Cargo.toml)
r_package=$(sed -n 's/^Version: *//p' r/DESCRIPTION)
binding=$(sed -n "s/^version = '\(.*\)'$/\1/p" r/src/rust/Cargo.toml | head -n 1)
binding_req=$(sed -n "s/^rtemis-r8 = '\(.*\)'$/\1/p" r/src/rust/Cargo.toml)

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
check "rtemis-r8 requirement of rtemis-r8-macros" "$macros_req"
check "R package R8 (r/DESCRIPTION)" "$r_package"
check "binding crate r8-r" "$binding"
check "binding requirement of rtemis-r8" "$binding_req"
exit $status
