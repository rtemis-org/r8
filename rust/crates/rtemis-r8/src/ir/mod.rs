//! The declaration IR: the versioned encoding of structs, enums, traits, and
//! impls that every host builds its types from.
//!
//! A type declared in R crosses into `rtemis-r8` as an [`IrDocument`] in JSON;
//! a type declared in Rust with `#[derive(R8)]` produces the same document.
//! [`check_document`] reports the problems a document holds on its own.

pub mod build;
mod check;
mod decl;
pub mod dispatch;
mod document;
pub mod fit;
mod logic;
mod lower;
pub mod notation;
mod number;
mod tags;
mod types;

pub use check::ambiguity_error;
pub use check::{
    DeclError, check_document, check_document_records, check_trait, check_type, is_column_type,
    report,
};
pub use decl::{
    ClassSystem, Column, ColumnDef, EnumDef, FieldDef, HostDefault, ImplDef, ImplMethod, MethodDef,
    Migration, MigrationStep, Param, ParamDef, RequiredField, ReturnType, RuleDef, SelfMarker,
    SplitTarget, StructDef, TraitDef, TypeRef, TypedImplMethod, VariantDef, VariantKind,
};
pub use document::{IR_VERSION, IrDocument, ReadError, TypeDef};
pub use fit::fits;
pub use logic::{Logic, Operator};
pub use lower::{InheritedChange, check_default_cycles, check_defaults, check_extends, extend};
pub use number::{EXACT_DOUBLE, Number};
pub use tags::check_tags;
pub use types::{Bound, CharacterType, DoubleType, FieldType, IntegerType, Resolve};

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
