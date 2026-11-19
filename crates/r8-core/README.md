# r8-core

Runtime of the R8 type system. R8 declares structs, enums, traits, and impls once, in an intermediate representation (IR) that every host language reads, and validates, encodes, and hashes values against those declarations.

The crate holds the IR and its declaration checks; validation, with every error carrying a stable code and a JSON Pointer; the rule dialect, strict JSONLogic compiled against the declared types; the value-access trait, through which validation, rules, encoding, and hashing read a value in any host form; the RFC 8785 canonical form, documents, and SHA-256 content hashes; the JSON Schema of a type's values; and the Rust mirror of the C interface `R8.h`.

The R package `R8`, in the [same repository](https://github.com/rtemis-org/r8), binds it through extendr. The conformance corpus in `conformance/` pins the behavior every host shares.

## Features

- `derive`: re-exports `#[derive(R8)]` from [`r8-derive`](https://crates.io/crates/r8-derive).
- `schema`: generates the JSON Schema of the IR document with `schemars`.

## Stability

The crate is at version 0.1. The value-access trait (`r8_core::access::Access`) is unstable until the release that adds Rust-native values.

## License

BSD-3-Clause.
