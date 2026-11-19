//! Expressions of the rule dialect, held in the IR.
//!
//! A rule's invariant and condition, a computed field's expression, and a
//! migration's expressions are JSONLogic objects in the strict dialect of
//! `rtemis-schema`, extended by R8's intrinsics. The IR stores each
//! expression as declared; M1 adds the dialect's checker and evaluator.

use serde::{Deserialize, Serialize};
use serde_json::Value;

/// A JSONLogic expression, stored as its JSON value.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "schema", derive(schemars::JsonSchema))]
#[serde(transparent)]
pub struct Logic(pub Value);
