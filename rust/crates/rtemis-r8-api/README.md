# rtemis-r8-api

R8's C services for an R package's Rust code. With it, a package declares Rust-stored types, whose values R8 holds in Rust, and implements R8 traits in Rust.

- `#[derive(R8)]` with `#[r8(storage = "rust")]` (from [`rtemis-r8`](https://crates.io/crates/rtemis-r8) with its `macros` feature) implements `RustStoredStruct` for a struct: R8 builds the value from validated fields, reads each field on first access, replaces one field with `$<-`, and persists the state's own bytes for `saveRDS()` and parallel workers.
- `Buffer<T>` is the field type of a large vector of `f64`, `i32`, `Logical`, or `u8`: it borrows the R vector it was built from, or owns Rust memory, and R reads an owned buffer through a view, without a copy.
- `#[r8::methods(trait = "pkg::Trait")]` on an ordinary Rust trait impl generates the functions R8 calls for a trait declared in R; arguments convert with `FromR`, results with `IntoR`, and an `Err(r8::Error)` becomes an R error with its code. An impl for a struct applies to the structs that extend it, and receives the struct's value that theirs embed.
- `r8::exports!` builds the package's exports table and `r8_init()`, which the package's initialization routine calls.

The crate declares the entry points of R's C API it calls and links R through the package's build, as R packages do. Every R function that can jump runs inside `R8Api.protect`, so a jump skips no Rust destructor. `examples/signal.rs` shows a struct and two impls.

## Stability

The crate is at version 0.2, with the C interface it builds on (`R8.h`, `rtemis_r8::abi`); both are unstable until the release that adds Rust-stored values.

## License

BSD-3-Clause.
