# rtemis-r8-api

R8's C services for an R package's Rust code. With it, a package declares Rust-stored types, whose values R8 holds in Rust, and implements R8 traits in Rust.

- `#[derive(R8)]` with `#[r8(storage = "rust")]` (from [`rtemis-r8`](https://crates.io/crates/rtemis-r8) with its `macros` feature) implements `RustStoredStruct` for a struct: R8 builds the value from validated fields, reads each field on first access, replaces one field with `$<-`, and persists the state's own bytes for `saveRDS()` and parallel workers.
- `Buffer<T>` is the field type of a large vector of `f64`, `i32`, `Logical`, or `u8`: it borrows the R vector it was built from, or owns Rust memory, and R reads an owned buffer through a view, without a copy.
- `SubtypeValue<P>` and `ImplementorValue<dyn Trait>` are the field types of a value of any struct that extends another and of any implementor of a trait: each keeps the R value, of a type that the package's code does not know too. A Rust-stored type is the field type of its values.
- `#[r8::methods(trait = "ns::Trait")]` on a Rust trait makes it the Rust side of a trait declared in R, with the functions R8 calls for each impl; arguments convert with `FromR`, results with `IntoR`, and an `Err(r8::Error)` becomes an R error with its code. Impls are ordinary Rust. An impl for a struct applies to the structs that extend it, and receives the struct's value that theirs embed; an impl over the struct's interface trait keeps the type of the value. A type parameter of the trait is a dispatched position, `Any` a position of any value, and `From` a conversion.
- `r8::exports!` lists the types and the impls, builds the package's exports table, and defines `r8_init_<package>()`, which the package's initialization routine calls.

The crate declares the entry points of R's C API it calls and links R through the package's build, as R packages do. Every R function that can jump runs inside `R8Api.protect`, so a jump skips no Rust destructor. `examples/signal.rs` shows a struct and two impls.

## Types of other packages

A crate that depends on the crate of another package reads the values of that package's types in the declaration that it was compiled against. R8 finds the fields by name, and refuses a read when the registered declaration admits a value that the compiled one does not: a field removed or renamed, another type, unit, or version, a loosened bound, or a removed rule. A package does not change a type that others use without coordinating the change with them: it raises the type's version and its own, and the packages that use the type require the new version in `DESCRIPTION` (`Imports: pa (>= 2.0.0)`) and in `Cargo.toml`, vendor the crate again, and release with it.

## Stability

The crate is at version 0.2, with the C interface it builds on (`R8.h`, `rtemis_r8::abi`); both are unstable until the release that adds Rust-stored values.

## License

BSD-3-Clause.
