//! Declarations of the IR: structs, enums, traits, impls, fields, rules, and
//! migrations.

use std::collections::BTreeMap;

use serde::{Deserialize, Serialize};
use serde_json::Value;

use super::logic::Logic;
use super::types::FieldType;
use crate::name::{MethodName, Name, QualifiedName};

fn is_false(b: &bool) -> bool {
    !*b
}

fn default_tag() -> Name {
    Name::new("type").expect("`type` is a valid name")
}

/// A field of a struct, a struct variant, a table, or a trait's required
/// fields.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "schema", derive(schemars::JsonSchema))]
#[serde(deny_unknown_fields)]
pub struct FieldDef {
    pub name: Name,
    #[serde(rename = "type")]
    pub ty: FieldType,
    /// A literal default in the field's JSON form, filled by the constructor
    /// and by `r8_from_json()`.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub default: Option<Value>,
    #[serde(default)]
    pub description: String,
    #[serde(default, skip_serializing_if = "is_false")]
    pub sensitive: bool,
    /// A UCUM code.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub unit: Option<String>,
    /// Written to an Arrow IPC file by `write_json()`.
    #[serde(default, skip_serializing_if = "is_false")]
    pub external: bool,
    /// A table column whose values are distinct.
    #[serde(default, skip_serializing_if = "is_false")]
    pub unique: bool,
    /// A table column that may be absent from the table, as distinct from a
    /// present column with missing cells, which is `option()`.
    #[serde(default, skip_serializing_if = "is_false")]
    pub optional: bool,
    /// The ancestor that declared the field, for a field that `extends`
    /// flattened into a child.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub inherited_from: Option<QualifiedName>,
    /// Namespaced extensions, such as `"rtemis:tunable"`, which R8 stores and
    /// exports and does not interpret.
    #[serde(default, skip_serializing_if = "BTreeMap::is_empty")]
    pub extensions: BTreeMap<String, Value>,
}

/// `rule(id, invariant, message, when = )`: what must hold.
///
/// A rule holds when `when` is `false` or when a field that `invariant` or
/// `when` reads is absent, and reports a violation when `invariant` is
/// `false`. The IR stores both expressions as declared.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "schema", derive(schemars::JsonSchema))]
#[serde(deny_unknown_fields)]
pub struct RuleDef {
    pub id: Name,
    pub invariant: Logic,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub when: Option<Logic>,
    pub message: String,
}

/// One step of a migration, from the fixed vocabulary that `r8-core` defines.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "schema", derive(schemars::JsonSchema))]
#[serde(tag = "op", rename_all = "snake_case", deny_unknown_fields)]
pub enum MigrationStep {
    /// Renames a field.
    Rename { from: Name, to: Name },
    /// Adds a field with a value.
    Add { field: Name, value: Value },
    /// Removes a field.
    Remove { field: Name },
    /// Splits one field into several, each computed from the old document.
    Split { field: Name, into: Vec<SplitTarget> },
    /// Merges several fields into one, computed from the old document.
    Merge {
        fields: Vec<Name>,
        into: Name,
        expr: Logic,
    },
    /// Changes a field's type, computing the new value from the old.
    ChangeType { field: Name, expr: Logic },
    /// Renames an enum variant.
    RenameVariant { from: Name, to: Name },
    /// Adds a field to one variant of an enum, with a value.
    AddVariantField {
        variant: Name,
        field: Name,
        value: Value,
    },
    /// Turns a unit variant into a struct variant with the given field values.
    UnitToStruct {
        variant: Name,
        values: BTreeMap<Name, Value>,
    },
    /// Moves fields into a new nested struct field.
    Nest { fields: Vec<Name>, into: Name },
    /// Converts a field's values between units by a factor.
    ConvertUnit {
        field: Name,
        from: String,
        to: String,
        factor: f64,
    },
}

/// A target of [`MigrationStep::Split`].
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "schema", derive(schemars::JsonSchema))]
#[serde(deny_unknown_fields)]
pub struct SplitTarget {
    pub field: Name,
    pub expr: Logic,
}

/// The steps that take a document from `from_version` to the next version.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "schema", derive(schemars::JsonSchema))]
#[serde(deny_unknown_fields)]
pub struct Migration {
    pub from_version: u32,
    pub steps: Vec<MigrationStep>,
}

/// `new_struct()`: a product type.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "schema", derive(schemars::JsonSchema))]
#[serde(deny_unknown_fields)]
pub struct StructDef {
    pub name: Name,
    #[serde(default)]
    pub description: String,
    pub version: u32,
    pub fields: Vec<FieldDef>,
    /// The parent, whose fields are flattened into `fields`.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub extends: Option<QualifiedName>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub rules: Vec<RuleDef>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub migrations: Vec<Migration>,
    /// Former qualified names, which readers accept.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub aliases: Vec<QualifiedName>,
    /// Values carry a provenance record.
    #[serde(default, skip_serializing_if = "is_false")]
    pub provenance: bool,
    /// Registered with `methods::setOldClass()`.
    #[serde(default, skip_serializing_if = "is_false")]
    pub s4: bool,
    /// Namespaced extensions that R8 stores and exports and does not
    /// interpret.
    #[serde(default, skip_serializing_if = "BTreeMap::is_empty")]
    pub extensions: BTreeMap<String, Value>,
}

/// The shape of an enum variant.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "schema", derive(schemars::JsonSchema))]
#[serde(tag = "shape", rename_all = "snake_case", deny_unknown_fields)]
pub enum VariantShape {
    /// A unit variant, written as its name.
    Unit,
    /// A struct variant, written as an object with the enum's tag member.
    Struct {
        fields: Vec<FieldDef>,
        #[serde(default, skip_serializing_if = "Vec::is_empty")]
        rules: Vec<RuleDef>,
    },
    /// A newtype variant, `variant(of = S)`: one value of the named struct
    /// `of`, written as that struct's object with the enum's tag member added.
    /// In R the value is the struct's value itself.
    Newtype { of: QualifiedName },
}

/// A variant of an enum.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "schema", derive(schemars::JsonSchema))]
pub struct VariantDef {
    pub name: Name,
    #[serde(default)]
    pub description: String,
    #[serde(flatten)]
    pub shape: VariantShape,
}

/// `new_enum()`: a sum type.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "schema", derive(schemars::JsonSchema))]
#[serde(deny_unknown_fields)]
pub struct EnumDef {
    pub name: Name,
    #[serde(default)]
    pub description: String,
    /// The member that holds the variant name of a struct variant.
    #[serde(default = "default_tag")]
    pub tag: Name,
    pub version: u32,
    pub variants: Vec<VariantDef>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub migrations: Vec<Migration>,
    /// Former qualified names, which readers accept.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub aliases: Vec<QualifiedName>,
    #[serde(default, skip_serializing_if = "BTreeMap::is_empty")]
    pub extensions: BTreeMap<String, Value>,
}

/// A trait method's signature.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "schema", derive(schemars::JsonSchema))]
#[serde(deny_unknown_fields)]
pub struct MethodDef {
    pub name: MethodName,
    #[serde(default)]
    pub description: String,
    /// The formal argument names, in order; `...` is allowed.
    pub params: Vec<String>,
    /// The method receives a seed and records it in provenance.
    #[serde(default, skip_serializing_if = "is_false")]
    pub stochastic: bool,
    /// The trait supplies a default implementation.
    #[serde(default, skip_serializing_if = "is_false")]
    pub has_default: bool,
}

/// `new_trait()`: behavior that types implement.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "schema", derive(schemars::JsonSchema))]
#[serde(deny_unknown_fields)]
pub struct TraitDef {
    pub name: Name,
    #[serde(default)]
    pub description: String,
    pub methods: Vec<MethodDef>,
    /// Fields every implementing type has.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub required_fields: Vec<FieldDef>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub supertraits: Vec<QualifiedName>,
    /// The member that names an implementor in JSON; without it, `"$type"`.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub tag: Option<Name>,
}

/// The class system of a host class.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[cfg_attr(feature = "schema", derive(schemars::JsonSchema))]
#[serde(rename_all = "snake_case")]
pub enum ClassSystem {
    S3,
    S4,
}

/// What an impl implements its trait for.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "schema", derive(schemars::JsonSchema))]
#[serde(tag = "kind", rename_all = "snake_case", deny_unknown_fields)]
pub enum ImplTarget {
    /// An R8 type.
    Type { name: QualifiedName },
    /// A class of another package in a host language, such as `dgCMatrix`.
    HostClass {
        host: String,
        package: String,
        class: String,
        system: ClassSystem,
    },
}

/// `impl()`: a trait implemented for a type.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "schema", derive(schemars::JsonSchema))]
#[serde(deny_unknown_fields)]
pub struct ImplDef {
    pub r#trait: QualifiedName,
    /// The trait's type arguments, for a standard trait with a type
    /// parameter, such as `From(source)`: `["data.frame"]`.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub type_args: Vec<String>,
    pub target: ImplTarget,
    /// The value of the trait's tag member; defaults to the type's name.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub tag_value: Option<String>,
    /// The methods this impl defines; the others take the trait's defaults.
    pub methods: Vec<MethodName>,
    /// The impl supplies a JSON codec for a host class.
    #[serde(default, skip_serializing_if = "is_false")]
    pub codec: bool,
}
