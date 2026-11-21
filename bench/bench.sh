#!/bin/sh
# bench.sh
# ::r8 bench::
# 2026- EDG rtemis.org

# Runs the benchmark on a committed tree: installs the release build of R8,
# r8benchR, and the packages of namespace.R into a temporary library, runs
# bench/run.R, and prints the tables of bench/report.R. `just bench` runs
# this from the repository root.
# spec: R8/class-system#benchmark

set -eu

if [ -n "$(git status --porcelain)" ]; then
  echo "The tree has uncommitted changes; the benchmark measures a commit." >&2
  exit 1
fi
sha=$(git rev-parse --short HEAD)

# r8benchR builds on r8pkgR's scaffold: their crate dependencies, paths
# aside, are the same.
deps() { sed -n '/^\[dependencies\]/,/^\[/p' "$1" | grep -v '^\[' | sed 's/path = "[^"]*", *//; s/path = "[^"]*"//'; }
if ! [ "$(deps bench/r8bench/r8benchR/src/rust/Cargo.toml)" = "$(deps r/tests/testthat/packages/r8pkgR/src/rust/Cargo.toml)" ]; then
  echo "r8benchR's crate dependencies differ from r8pkgR's." >&2
  exit 1
fi

work=$(mktemp -d)
lib="$work/lib"
mkdir -p "$lib"
export R8_CRATES_PATH="$PWD/rust/crates"
export R8_TARGET_DIR="${R8_TARGET_DIR:-$PWD/target/r}"
(cd "$work" && R CMD build --no-build-vignettes --no-manual "$OLDPWD/r" > /dev/null)
R CMD INSTALL --library="$lib" "$work"/R8_*.tar.gz > "$work/install-R8.log" 2>&1
R_LIBS="$lib" R CMD INSTALL --no-test-load --library="$lib" bench/r8bench/r8benchR > "$work/install-r8benchR.log" 2>&1
R_LIBS="$lib" Rscript --vanilla -e "source('bench/namespace.R'); for (s in NAMESPACE_SYSTEMS) write_namespace_package(s, '$work')"
for s in S3 S4 RC R6 S7 R8; do
  R_LIBS="$lib" R CMD INSTALL --no-test-load --library="$lib" "$work/bench${s}200" > "$work/install-$s.log" 2>&1
done
rm -f "bench/results/$sha.jsonl"
Rscript --vanilla bench/run.R "$lib" "$sha"
Rscript --vanilla bench/report.R "$sha"
rm -rf "$work"
