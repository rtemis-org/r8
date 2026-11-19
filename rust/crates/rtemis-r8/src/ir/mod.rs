//! The declaration IR: the versioned encoding of structs, enums, traits, and
//! impls that every host builds its types from.
//!
//! A type declared in R crosses into `rtemis-r8` as an [`IrDocument`] in JSON;
//! a type declared in Rust with `#[derive(R8)]` produces the same document.
//! [`check_document`] reports the problems a document holds on its own.

mod check;
mod decl;
mod document;
mod logic;
mod types;

pub use check::{DeclError, check_document, is_column_type};
pub use decl::{
    ClassSystem, EnumDef, FieldDef, ImplDef, ImplTarget, MethodDef, Migration, MigrationStep,
    RuleDef, SplitTarget, StructDef, TraitDef, VariantDef, VariantShape,
};
pub use document::{IR_VERSION, IrDocument, ReadError, TypeDef};
pub use logic::Logic;
pub use types::{CharacterType, DoubleType, FieldType, IntegerType, Resolve};

/// The JSON Schema (draft 2020-12) of the IR document, generated from the IR
/// types. `schemas/ir-v1.schema.json` holds the published copy, and a test
/// keeps the two identical.
#[cfg(feature = "schema")]
pub fn schema() -> serde_json::Value {
    let mut schema = schemars::schema_for!(IrDocument);
    schema.insert(
        "title".to_string(),
        serde_json::json!(format!("R8 IR document, version {IR_VERSION}")),
    );
    schema.to_value()
}
