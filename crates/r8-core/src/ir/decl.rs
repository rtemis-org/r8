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
    /// The field's name.
    pub name: Name,
    #[serde(rename = "type")]
    /// The field's type.
    pub ty: FieldType,
    /// A literal default in the field's JSON form, filled by the constructor
    /// and by `r8_from_json()`.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub default: Option<Value>,
    #[serde(default)]
    /// The field's description.
    pub description: String,
    #[serde(default, skip_serializing_if = "is_false")]
    /// The value is withheld from logs, prints, and exports.
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
    /// The rule's ID, unique within its type.
    pub id: Name,
    /// The condition that holds for a valid value.
    pub invariant: Logic,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    /// The condition under which the rule applies.
    pub when: Option<Logic>,
    /// The message of a violation.
    pub message: String,
}

/// One step of a migration, from the fixed vocabulary that `r8-core` defines.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "schema", derive(schemars::JsonSchema))]
#[serde(tag = "op", rename_all = "snake_case", deny_unknown_fields)]
pub enum MigrationStep {
    /// Renames a field.
    Rename {
        /// The old name.
        from: Name,
        /// The new name.
        to: Name,
    },
    /// Adds a field with a value.
    Add {
        /// The new field.
        field: Name,
        /// Its value.
        value: Value,
    },
    /// Removes a field.
    Remove {
        /// The field.
        field: Name,
    },
    /// Splits one field into several, each computed from the old document.
    Split {
        /// The field split.
        field: Name,
        /// The new fields.
        into: Vec<SplitTarget>,
    },
    /// Merges several fields into one, computed from the old document.
    Merge {
        /// The fields merged.
        fields: Vec<Name>,
        /// The new field.
        into: Name,
        /// The new field's value, computed from the old document.
        expr: Logic,
    },
    /// Changes a field's type, computing the new value from the old.
    ChangeType {
        /// The field.
        field: Name,
        /// The new value, computed from the old.
        expr: Logic,
    },
    /// Renames an enum variant.
    RenameVariant {
        /// The old name.
        from: Name,
        /// The new name.
        to: Name,
    },
    /// Adds a field to one variant of an enum, with a value.
    AddVariantField {
        /// The variant.
        variant: Name,
        /// The new field.
        field: Name,
        /// The new field's value.
        value: Value,
    },
    /// Turns a unit variant into a struct variant with the given field values.
    UnitToStruct {
        /// The variant.
        variant: Name,
        /// The new fields' values, by name.
        values: BTreeMap<Name, Value>,
    },
    /// Moves fields into a new nested struct field.
    Nest {
        /// The fields moved.
        fields: Vec<Name>,
        /// The new struct field.
        into: Name,
    },
    /// Converts a field's values between units by a factor.
    ConvertUnit {
        /// The field.
        field: Name,
        /// The UCUM code of the old unit.
        from: String,
        /// The UCUM code of the new unit.
        to: String,
        /// The factor that converts a value to the new unit.
        factor: f64,
    },
}

/// A target of [`MigrationStep::Split`].
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "schema", derive(schemars::JsonSchema))]
#[serde(deny_unknown_fields)]
pub struct SplitTarget {
    /// The new field.
    pub field: Name,
    /// Its value, computed from the old document.
    pub expr: Logic,
}

/// The steps that take a document from `from_version` to the next version.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "schema", derive(schemars::JsonSchema))]
#[serde(deny_unknown_fields)]
pub struct Migration {
    /// The version the steps start from.
    pub from_version: u32,
    /// The steps, in order.
    pub steps: Vec<MigrationStep>,
}

/// `new_struct()`: a product type.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "schema", derive(schemars::JsonSchema))]
#[serde(deny_unknown_fields)]
pub struct StructDef {
    /// The struct's name.
    pub name: Name,
    #[serde(default)]
    /// The struct's description.
    pub description: String,
    /// The struct's version, which a migration increments.
    pub version: u32,
    /// The fields, in declaration order.
    pub fields: Vec<FieldDef>,
    /// The parent, whose fields are flattened into `fields`.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub extends: Option<QualifiedName>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    /// The rules every value satisfies.
    pub rules: Vec<RuleDef>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    /// The migrations from earlier versions.
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
        /// The variant's fields.
        fields: Vec<FieldDef>,
        #[serde(default, skip_serializing_if = "Vec::is_empty")]
        /// The rules every value of the variant satisfies.
        rules: Vec<RuleDef>,
    },
    /// A newtype variant, `variant(of = S)`: one value of the named struct
    /// `of`, written as that struct's object with the enum's tag member added.
    /// In R the value is the struct's value itself.
    Newtype {
        /// The wrapped struct.
        of: QualifiedName,
    },
}

/// A variant of an enum.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "schema", derive(schemars::JsonSchema))]
pub struct VariantDef {
    /// The variant's name.
    pub name: Name,
    #[serde(default)]
    /// The variant's description.
    pub description: String,
    #[serde(flatten)]
    /// The variant's shape.
    pub shape: VariantShape,
}

/// `new_enum()`: a sum type.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "schema", derive(schemars::JsonSchema))]
#[serde(deny_unknown_fields)]
pub struct EnumDef {
    /// The enum's name.
    pub name: Name,
    #[serde(default)]
    /// The enum's description.
    pub description: String,
    /// The member that holds the variant name of a struct variant.
    #[serde(default = "default_tag")]
    pub tag: Name,
    /// The enum's version, which a migration increments.
    pub version: u32,
    /// The variants, in declaration order.
    pub variants: Vec<VariantDef>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    /// The migrations from earlier versions.
    pub migrations: Vec<Migration>,
    /// Former qualified names, which readers accept.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub aliases: Vec<QualifiedName>,
    #[serde(default, skip_serializing_if = "BTreeMap::is_empty")]
    /// Namespaced extensions that R8 stores and exports and does not interpret.
    pub extensions: BTreeMap<String, Value>,
}

/// A trait method's signature.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "schema", derive(schemars::JsonSchema))]
#[serde(deny_unknown_fields)]
pub struct MethodDef {
    /// The method's name.
    pub name: MethodName,
    #[serde(default)]
    /// The method's description.
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
    /// The trait's name.
    pub name: Name,
    #[serde(default)]
    /// The trait's description.
    pub description: String,
    /// The methods.
    pub methods: Vec<MethodDef>,
    /// Fields every implementing type has.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub required_fields: Vec<FieldDef>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    /// The traits every implementor also implements.
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
    /// An S3 class.
    S3,
    /// An S4 class.
    S4,
}

/// What an impl implements its trait for.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "schema", derive(schemars::JsonSchema))]
#[serde(tag = "kind", rename_all = "snake_case", deny_unknown_fields)]
pub enum ImplTarget {
    /// An R8 type.
    Type {
        /// The type's qualified name.
        name: QualifiedName,
    },
    /// A class of another package in a host language, such as `dgCMatrix`.
    HostClass {
        /// The host language, such as `"r"`.
        host: String,
        /// The package that defines the class.
        package: String,
        /// The class name.
        class: String,
        /// The class system.
        system: ClassSystem,
    },
}

/// `impl()`: a trait implemented for a type.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "schema", derive(schemars::JsonSchema))]
#[serde(deny_unknown_fields)]
pub struct ImplDef {
    /// The implemented trait.
    pub r#trait: QualifiedName,
    /// The trait's type arguments, for a standard trait with a type
    /// parameter, such as `From(source)`: `["data.frame"]`.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub type_args: Vec<String>,
    /// What the trait is implemented for.
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
