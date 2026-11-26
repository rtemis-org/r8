# R8

R8 is a type system whose runtime is written in Rust, with an API for R. Types are structs, enums, and traits, declared in Rust with `#[derive(R8)]` or in a host language; values are immutable and validated once per value, and one declaration drives validation, JSON encoding, and every export. The Rust crates hold everything independent of the host language, and each host's API is a binding over them: R's in `r/`, and Python's and web applications' through their own bindings.

## Rust crates

The Cargo workspace is `rust/`.

| Crate | Contents |
|---|---|
| `rtemis-r8` | The runtime: the declaration IR, validation, rules, the value-access trait, documents, the canonical form, equality, hashing, the declaration record, and the C ABI types |
| `rtemis-r8-macros` | The procedural macros: `#[derive(R8)]`, `#[r8::methods]`, and `r8::exports!` |
| `rtemis-r8-api` | R8's C services for a package's Rust code: Rust-stored types, their codecs, and trait impls registered with R8 |
| `rtemis-r8-abi-check` | Test-only: the ABI types of `rtemis-r8` match the header `R8.h` |

## R API

`r/` is the R package `R8`, which binds `rtemis-r8` through extendr. Compiled methods and Rust-stored types of other packages are written in Rust, through `rtemis-r8-api`; `r/inst/include/R8.h` is the interface between that crate and R8. An R method calls C or C++ code with `.Call()`.

## Conformance

`conformance/` holds language-neutral test cases, with their format and JSON Schema, that every host's API runs.

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
