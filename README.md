# R8

R8 is a developer-facing type system for R whose runtime is written in Rust. Types are structs, enums, and traits, declared in R or in Rust with `#[derive(R8)]`; values are immutable and validated once per value, and one declaration drives validation, JSON encoding, and every export. The Rust runtime serves R and is designed to serve Python and web applications through their own bindings.

| Directory | Contents |
|---|---|
| `crates/r8-core` | The runtime: declaration IR, validation, rules, JSON codec, hashing, provenance, and dispatch |
| `crates/r8-derive` | `#[derive(R8)]` |
| `crates/r8-abi-check` | Test-only: the Rust ABI types of `r8-core` match the C header |
| `conformance` | Language-neutral test cases that every host runs, with their format and JSON Schema |
| `R8` | The R package, which binds `r8-core` through extendr; `R8/inst/include/R8.h` is the C interface for other packages |

## Development

The R package builds against the `r8-core` of this checkout: its `justfile` sets `R8_CRATES_PATH` to `crates/`, and `tools/config.R` passes Cargo a patch of the crates.io release with that path.

```sh
just rust-check            # workspace: rustfmt, clippy, tests
cd R8 && just document     # build, regenerate R/extendr-wrappers.R, run roxygen2
cd R8 && just test         # R tests
cd R8 && just check-cran   # R CMD check --as-cran
```

The minimum Rust version is 1.88, and the R package requires R 4.3.0 or later.

## License

BSD 3-Clause; see `LICENSE.md`.
