//! `#[derive(R8)]`, which declares a Rust struct or enum as an R8 type.
//!
//! The derive emits the type's IR document and the registration calls that pass
//! it, with its codecs, to the registry of the R package `R8` through the C
//! interface of `r8-core`.
