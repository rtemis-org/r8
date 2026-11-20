#!/usr/bin/env bash
# Opens version X.Y.Z on devel: writes it to the workspace crates, the
# binding crate r8-r, and the R package R8, sets every requirement between
# them to X.Y, refreshes rust/Cargo.lock, and adds a NEWS.md heading. The
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
edit "1,/^version = /s/^version = '.*'$/version = '$version'/" r/src/rust/Cargo.toml
edit "s/^rtemis-r8 = '.*'$/rtemis-r8 = '$mm'/" r/src/rust/Cargo.toml
edit "s/^Version: .*/Version: $version/" r/DESCRIPTION
if ! grep -q "^# R8 $version\$" r/NEWS.md; then
  printf '# R8 %s\n\n' "$version" | cat - r/NEWS.md > r/NEWS.md.new && mv r/NEWS.md.new r/NEWS.md
fi
(cd rust && cargo update --workspace --offline)
bash tools/check-versions.sh
