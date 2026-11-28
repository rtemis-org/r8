//! Runtime of the R8 type system.
//!
//! R8 declares structs, enums, traits, and impls once, in an intermediate
//! representation (IR) that every host language reads. `rtemis-r8` holds:
//!
//! - [`ir`]: the IR of declarations, its JSON form, and the declaration
//!   checks;
//! - [`name`]: names and qualified names, with the reserved words of each
//!   host;
//! - [`validate`]: validation of a value against its type, every error with a
//!   stable code and a JSON Pointer;
//! - [`rules`]: the rule dialect, strict JSONLogic, compiled against the
//!   declared types, and its evaluator;
//! - [`access`]: the value-access trait, through which validation, rules,
//!   encoding, and hashing read a value in any host form; [`access::Json`]
//!   reads the JSON form, and a host binding implements the trait for its own
//!   values;
//! - [`canonical`]: the canonical form (RFC 8785), documents, and content hashes,
//!   with the canonical binary forms of matrices, tables, and vectors;
//! - [`refine`]: whether a registered declaration refines the one that code
//!   was compiled against;
//! - [`equal`]: equality of two values by their normalized forms;
//! - [`value_schema`]: the JSON Schema of a type's values;
//! - [`derive`](mod@derive): the traits behind `#[derive(R8)]`, which the
//!   `macros` feature re-exports;
//! - [`abi`]: the types of the interface between R8 and the compiled code of R
//!   packages, which the crate `rtemis-r8-api` implements.
//!
//! The R package `R8` binds the crate through extendr. The [`access`]
//! module's trait is unstable until the release that adds Rust-stored values.
//!
//! Features: `macros` re-exports the procedural macros of `rtemis-r8-macros`,
//! `#[derive(R8)]`; `schema`
//! generates the JSON Schema of the IR document with `schemars`.

#![warn(missing_docs)]

pub mod abi;
pub mod access;
pub mod call;
pub mod canonical;
pub mod derive;
mod emit;
pub mod equal;
pub mod exports;
pub mod impl_signature;
pub mod ir;
pub mod layout;
pub mod message;
pub mod name;
mod open;
pub mod plan;
pub mod problem;
pub mod progress;
pub mod reader;
pub mod record;
pub mod refine;
pub mod registry;
pub mod report;
pub mod rules;
pub mod rust_signature;
pub mod text;
pub mod tree;
pub mod update;
pub mod validate;
pub mod value_schema;

/// `#[derive(R8)]`, with the `macros` feature.
#[cfg(feature = "macros")]
pub use rtemis_r8_macros::R8;

/// Version of this crate.
pub const VERSION: &str = env!("CARGO_PKG_VERSION");

#[cfg(test)]
mod tests {
    #[test]
    fn version_is_the_package_version() {
        assert_eq!(super::VERSION, env!("CARGO_PKG_VERSION"));
        assert!(!super::VERSION.is_empty());
    }
}
