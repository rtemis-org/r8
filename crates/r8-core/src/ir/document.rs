//! The IR document: the versioned JSON encoding of a namespace's
//! declarations, which crosses package and host boundaries.

use serde::{Deserialize, Serialize};

use super::decl::{EnumDef, ImplDef, StructDef, TraitDef};
use crate::name::Name;

/// The version of the IR document format that this crate reads and writes.
pub const IR_VERSION: u32 = 1;

/// A struct or an enum.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "schema", derive(schemars::JsonSchema))]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum TypeDef {
    Struct(StructDef),
    Enum(EnumDef),
}

impl TypeDef {
    pub fn name(&self) -> &Name {
        match self {
            TypeDef::Struct(s) => &s.name,
            TypeDef::Enum(e) => &e.name,
        }
    }
}

/// The declarations of one namespace.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "schema", derive(schemars::JsonSchema))]
#[serde(deny_unknown_fields)]
pub struct IrDocument {
    /// The IR format version.
    #[serde(rename = "$ir")]
    #[cfg_attr(feature = "schema", schemars(extend("const" = 1)))]
    pub ir_version: u32,
    pub namespace: Name,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub types: Vec<TypeDef>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub traits: Vec<TraitDef>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub impls: Vec<ImplDef>,
}

/// Why a document could not be read.
#[derive(Debug)]
pub enum ReadError {
    /// The text is not a well-formed IR document.
    Json(serde_json::Error),
    /// The document's format version is not [`IR_VERSION`].
    Version { found: u32 },
}

impl std::fmt::Display for ReadError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ReadError::Json(e) => write!(f, "invalid IR document: {e}"),
            ReadError::Version { found } => write!(
                f,
                "IR document version {found} is not supported; this r8-core reads version {IR_VERSION}"
            ),
        }
    }
}

impl std::error::Error for ReadError {}

impl IrDocument {
    /// Reads an IR document from JSON text.
    pub fn from_json(text: &str) -> Result<Self, ReadError> {
        let doc: IrDocument = serde_json::from_str(text).map_err(ReadError::Json)?;
        if doc.ir_version != IR_VERSION {
            return Err(ReadError::Version {
                found: doc.ir_version,
            });
        }
        Ok(doc)
    }

    /// Writes the document as JSON text, with members in declaration order.
    pub fn to_json(&self) -> String {
        serde_json::to_string(self).expect("an IR document always serializes")
    }
}
