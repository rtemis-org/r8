#!/usr/bin/env bash
# Opens version X.Y.Z on devel: writes it to the workspace crates, the
# binding crates r8-r and r8-io-r, and the R packages R8 and R8.io, sets every
# requirement between them to X.Y, refreshes rust/Cargo.lock, and gives each
# NEWS.md the version's heading: the heading of the development version at the
# top takes the version's name, and a NEWS.md without one gains the heading. The
# binding's committed Cargo.lock and vendored crates keep the published
# rtemis-r8 until `just update-crates` in r/. Run from the repository root.
set -euo pipefail

version=${1:?usage: tools/open-version.sh X.Y.Z}
if [[ ! "$version" =~ ^[0-9]+\.[0-9]+\.[0-9]+$ ]]; then
  echo "The version is X.Y.Z: $version" >&2
  exit 1
fi
mm=$(echo "$version" | cut -d. -f1,2)

# sed -i takes an argument on macOS and none on Linux.
edit() { sed -i.bak "$1" "$2" && rm "$2.bak"; }

edit "1,/^version = /s/^version = \".*\"$/version = \"$version\"/" rust/Cargo.toml
edit "s/^\(rtemis-r8-macros = {.*version = \"\)[^\"]*\"/\1$mm\"/" rust/crates/rtemis-r8/Cargo.toml
edit "s/^\(rtemis-r8 = {.*version = \"\)[^\"]*\"/\1$mm\"/" rust/crates/rtemis-r8-api/Cargo.toml
edit "s/^\(rtemis-r8-macros = {.*version = \"\)[^\"]*\"/\1$mm\"/" rust/crates/rtemis-r8-api/Cargo.toml
edit "s/^\(rtemis-r8 = {.*version = \"\)[^\"]*\"/\1$mm\"/" rust/crates/rtemis-r8-io/Cargo.toml
for crate in r/src/rust/Cargo.toml r-io/src/rust/Cargo.toml; do
  edit "1,/^version = /s/^version = '.*'$/version = '$version'/" "$crate"
  edit "s/^rtemis-r8 = '.*'$/rtemis-r8 = '$mm'/" "$crate"
  edit "s/^\(rtemis-r8 = {.*version = '\)[^']*'/\1$mm'/" "$crate"
done
edit "s/^\(rtemis-r8-io = {.*version = '\)[^']*'/\1$mm'/" r-io/src/rust/Cargo.toml
edit "s/^Version: .*/Version: $version/" r/DESCRIPTION
edit "s/^Version: .*/Version: $version/" r-io/DESCRIPTION
edit "s/^\(    R8 (>= \)[^)]*)/\1$version)/" r-io/DESCRIPTION
# The heading of each NEWS.md: the development version at the top takes the
# version's name.
for pair in "r/NEWS.md R8" "r-io/NEWS.md R8.io"; do
  set -- $pair
  pkg=$(echo "$2" | sed 's/[.]/[.]/g')
  if grep -q "^# $pkg $version\$" "$1"; then
    continue
  fi
  if head -n 1 "$1" | grep -q "^# $pkg [0-9.]*[.]9000\$"; then
    edit "1s/^# .*/# $2 $version/" "$1"
  else
    printf '# %s %s\n\n' "$2" "$version" | cat - "$1" > "$1.new" && mv "$1.new" "$1"
  fi
done
(cd rust && cargo update --workspace --offline)
bash tools/check-versions.sh
