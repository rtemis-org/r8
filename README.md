# R8

R8 is a developer-facing type system for R whose runtime is written in Rust. Types are structs, enums, and traits, declared in R or in Rust with `#[derive(R8)]`; values are immutable and validated once per value, and one declaration drives validation, JSON encoding, and every export. The Rust runtime serves R and is designed to serve Python and web applications through their own bindings.

| Directory | Contents |
|---|---|
| `rust` | The Cargo workspace |
| `rust/crates/rtemis-r8` | The runtime: declaration IR, validation, rules, the value-access trait, the JSON codec, equality, and hashing |
| `rust/crates/rtemis-r8-macros` | The procedural macros, `#[derive(R8)]` |
| `rust/crates/rtemis-r8-abi-check` | Test-only: the Rust ABI types of `rtemis-r8` match the C header |
| `r` | The R package `R8`, which binds `rtemis-r8` through extendr; `r/inst/include/R8.h` is the C interface for other packages |
| `conformance` | Language-neutral test cases that every host runs, with their format and JSON Schema |

## Development

The R package builds against the `rtemis-r8` of this checkout: its `justfile` sets `R8_CRATES_PATH` to `rust/crates/`, and `tools/config.R` passes Cargo a patch of the crates.io release with that path.

```sh
just rust-check            # workspace: rustfmt, clippy, tests
cd r && just document      # build, regenerate R/extendr-wrappers.R, run roxygen2
cd r && just test          # R tests
cd r && just check-cran    # R CMD check --as-cran
```

The minimum Rust version is 1.88, and the R package requires R 4.3.0 or later.

## License

BSD 3-Clause; see `LICENSE.md`.
