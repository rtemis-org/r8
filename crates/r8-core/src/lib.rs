//! Runtime of the R8 type system.
//!
//! `r8-core` holds the declaration IR of structs, enums, traits, and impls;
//! validation of values against a declaration; the rule dialect and its
//! evaluator; the JSON codec; content hashing; provenance; and the dispatch
//! table. The R package `R8` binds it through extendr, and other hosts bind it
//! through their own bindings.
//!
//! Design: `~/Code/spec/R8/class-system/plan.md` in the specification
//! repository of rtemis-org.

pub mod abi;
pub mod codec;
pub mod derive;
pub mod ir;
pub mod name;
pub mod rules;
pub mod validate;
pub mod value_schema;

/// `#[derive(R8)]`, with the `derive` feature.
#[cfg(feature = "derive")]
pub use r8_derive::R8;

/// Version of this crate, as recorded in provenance.
pub const VERSION: &str = env!("CARGO_PKG_VERSION");

#[cfg(test)]
mod tests {
    #[test]
    fn version_is_the_package_version() {
        assert_eq!(super::VERSION, env!("CARGO_PKG_VERSION"));
        assert!(!super::VERSION.is_empty());
    }
}
