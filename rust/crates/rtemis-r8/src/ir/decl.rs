//! Declarations of the IR: structs, enums, traits, impls, fields, rules, and
//! migrations.

use std::collections::BTreeMap;

use serde::{Deserialize, Serialize};
use serde_json::Value;

use super::logic::Logic;
use super::types::FieldType;
use crate::name::{MethodName, Name, QualifiedName};

fn is_zero(n: &u8) -> bool {
    *n == 0
}

fn is_false(b: &bool) -> bool {
    !*b
}

fn default_tag() -> Name {
    Name::new("type").expect("`type` is a valid name")
}

/// A field of a struct, a struct variant, or a trait's required fields, and
/// the field of a table column.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "schema", derive(schemars::JsonSchema))]
#[serde(deny_unknown_fields)]
pub struct FieldDef {
    /// The field's name.
    pub name: Name,
    #[serde(rename = "type")]
    /// The field's type.
    pub ty: FieldType,
    /// A literal default in the field's JSON form as a tree, its numbers
    /// kept as written, filled by the constructor and by `r8from_json()`.
    #[serde(
        default,
        skip_serializing_if = "Option::is_none",
        deserialize_with = "crate::tree::exact_option"
    )]
    #[cfg_attr(feature = "schema", schemars(with = "Option<Value>"))]
    pub default: Option<Box<crate::tree::Tree>>,
    #[serde(default)]
    /// The field's description.
    pub description: Box<str>,
    #[serde(default, skip_serializing_if = "is_false")]
    /// The value is withheld from logs, prints, and exports.
    pub sensitive: bool,
    /// A unit code, by convention UCUM, such as `"mg"`.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub unit: Option<Box<str>>,
    /// Who fills the absent value of a struct field of type `option()`.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub resolve: Option<super::types::Resolve>,
    /// The ancestor that declared the field, for a field that `extends`
    /// flattened into a child.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub inherited_from: Option<Box<QualifiedName>>,
    /// Namespaced extensions, such as `"rtemis:tunable"`, which R8 stores and
    /// exports and does not interpret.
    // `None` when there are none: 8 bytes inline on every field, against 24
    // for a map, and a box only on the rare field that has extensions.
    #[allow(clippy::box_collection)]
    #[serde(
        default,
        skip_serializing_if = "Option::is_none",
        deserialize_with = "extensions_or_none"
    )]
    #[cfg_attr(feature = "schema", schemars(with = "BTreeMap<String, Value>"))]
    pub extensions: Option<Box<BTreeMap<String, Value>>>,
}

/// A column of `type_table(columns)`: its field, and the facts a column alone
/// takes. Its JSON form is the field's members with `unique` and `required`;
/// a column takes no `resolve` and no `inherited_from`.
#[derive(Debug, Clone, PartialEq)]
pub struct ColumnDef {
    /// The column's name, type, default, description, and other facts.
    pub field: FieldDef,
    /// The column's values are distinct.
    pub unique: bool,
    /// The column is present in every table; `false` for a column that may
    /// be absent, as distinct from a present column with missing cells,
    /// which is `option()`.
    pub required: bool,
}

/// A table's column as the code that reads tables sees it: a declared
/// column of `type_table(columns)`, or a struct's field in
/// `type_table(of = S)`, which is required and not unique.
pub trait Column {
    /// The column's field.
    fn field(&self) -> &FieldDef;
    /// The column is present in every table.
    fn required(&self) -> bool {
        true
    }
    /// The column's values are distinct.
    fn unique(&self) -> bool {
        false
    }
}

impl Column for FieldDef {
    fn field(&self) -> &FieldDef {
        self
    }
}

impl Column for ColumnDef {
    fn field(&self) -> &FieldDef {
        &self.field
    }
    fn required(&self) -> bool {
        self.required
    }
    fn unique(&self) -> bool {
        self.unique
    }
}

impl std::ops::Deref for ColumnDef {
    type Target = FieldDef;
    fn deref(&self) -> &FieldDef {
        &self.field
    }
}

fn is_true(b: &bool) -> bool {
    *b
}

fn yes() -> bool {
    true
}

/// The JSON form of a [`ColumnDef`].
#[derive(Serialize, Deserialize)]
#[cfg_attr(feature = "schema", derive(schemars::JsonSchema))]
#[serde(deny_unknown_fields)]
struct ColumnJson {
    name: Name,
    #[serde(rename = "type")]
    ty: FieldType,
    #[serde(
        default,
        skip_serializing_if = "Option::is_none",
        deserialize_with = "crate::tree::exact_option"
    )]
    #[cfg_attr(feature = "schema", schemars(with = "Option<Value>"))]
    default: Option<Box<crate::tree::Tree>>,
    #[serde(default)]
    description: Box<str>,
    #[serde(default, skip_serializing_if = "is_false")]
    sensitive: bool,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    unit: Option<Box<str>>,
    #[serde(default, skip_serializing_if = "is_false")]
    unique: bool,
    #[serde(default = "yes", skip_serializing_if = "is_true")]
    required: bool,
    #[allow(clippy::box_collection)]
    #[serde(
        default,
        skip_serializing_if = "Option::is_none",
        deserialize_with = "extensions_or_none"
    )]
    #[cfg_attr(feature = "schema", schemars(with = "BTreeMap<String, Value>"))]
    extensions: Option<Box<BTreeMap<String, Value>>>,
}

impl Serialize for ColumnDef {
    fn serialize<S: serde::Serializer>(&self, s: S) -> Result<S::Ok, S::Error> {
        let f = &self.field;
        ColumnJson {
            name: f.name.clone(),
            ty: f.ty.clone(),
            default: f.default.clone(),
            description: f.description.clone(),
            sensitive: f.sensitive,
            unit: f.unit.clone(),
            unique: self.unique,
            required: self.required,
            extensions: f.extensions.clone(),
        }
        .serialize(s)
    }
}

impl<'de> Deserialize<'de> for ColumnDef {
    fn deserialize<D: serde::Deserializer<'de>>(d: D) -> Result<Self, D::Error> {
        let c = ColumnJson::deserialize(d)?;
        Ok(ColumnDef {
            field: FieldDef {
                name: c.name,
                ty: c.ty,
                default: c.default,
                description: c.description,
                sensitive: c.sensitive,
                unit: c.unit,
                resolve: None,
                inherited_from: None,
                extensions: c.extensions,
            },
            unique: c.unique,
            required: c.required,
        })
    }
}

#[cfg(feature = "schema")]
impl schemars::JsonSchema for ColumnDef {
    fn schema_name() -> std::borrow::Cow<'static, str> {
        "ColumnDef".into()
    }
    fn json_schema(g: &mut schemars::SchemaGenerator) -> schemars::Schema {
        ColumnJson::json_schema(g)
    }
}

/// A field that every implementor of a trait has: a struct field of this
/// name whose type fits `ty` ([`super::fit::fits`]).
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "schema", derive(schemars::JsonSchema))]
#[serde(deny_unknown_fields)]
pub struct RequiredField {
    /// The field's name.
    pub name: Name,
    /// The type the implementor's field fits.
    #[serde(rename = "type")]
    pub ty: FieldType,
    /// What the field holds.
    #[serde(default)]
    pub description: Box<str>,
}

/// A field's extensions, or `None` for an empty object.
#[allow(clippy::box_collection)]
fn extensions_or_none<'de, D: serde::Deserializer<'de>>(
    d: D,
) -> Result<Option<Box<BTreeMap<String, Value>>>, D::Error> {
    let m = BTreeMap::<String, Value>::deserialize(d)?;
    Ok((!m.is_empty()).then(|| Box::new(m)))
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
    /// The rule's ID, unique within its scope: the stable handle by which
    /// code refers to the rule.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub id: Option<Name>,
    /// The condition that holds for a valid value.
    pub invariant: Logic,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    /// The condition under which the rule applies.
    pub when: Option<Logic>,
    /// What holds, stated for the user; a violation of a rule without one
    /// shows its invariant, rendered by the host.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub message: Option<String>,
    /// The ancestor that declared the rule, for a rule that `extends`
    /// flattened into a child.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub inherited_from: Option<Box<QualifiedName>>,
}

/// One step of a migration, from the fixed vocabulary that `rtemis-r8` defines.
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
    /// Namespaced extensions that R8 stores and exports and does not
    /// interpret.
    #[serde(default, skip_serializing_if = "BTreeMap::is_empty")]
    pub extensions: BTreeMap<String, Value>,
}

/// `new_table_type()`: a table type, whose values hold their columns, each a
/// vector of the cells of one type, all of one length.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "schema", derive(schemars::JsonSchema))]
#[serde(deny_unknown_fields)]
pub struct TableDef {
    /// The table's name.
    pub name: Name,
    #[serde(default)]
    /// The table's description.
    pub description: String,
    /// The table's version, which a migration increments.
    pub version: u32,
    /// The declared columns, in declaration order.
    pub columns: Vec<ColumnDef>,
    /// Columns other than the declared ones are admitted, after them.
    #[serde(default, skip_serializing_if = "std::ops::Not::not")]
    pub open: bool,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    /// The rules every row satisfies, which read the row's cells by name.
    pub rules: Vec<RuleDef>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    /// The migrations from earlier versions.
    pub migrations: Vec<Migration>,
    /// Former qualified names, which readers accept.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub aliases: Vec<QualifiedName>,
    /// Namespaced extensions that R8 stores and exports and does not
    /// interpret.
    #[serde(default, skip_serializing_if = "BTreeMap::is_empty")]
    pub extensions: BTreeMap<String, Value>,
}

/// The kind of an enum variant.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "schema", derive(schemars::JsonSchema))]
#[serde(tag = "kind", rename_all = "snake_case", deny_unknown_fields)]
pub enum VariantKind {
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
    /// The variant's kind and its members.
    pub kind: VariantKind,
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

/// A trait method's parameter: its name, or an object that also gives
/// whether the method dispatches on it, its type, its default, or its
/// description.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "schema", derive(schemars::JsonSchema))]
#[serde(untagged)]
pub enum ParamDef {
    /// An untyped parameter that is not marked as dispatched.
    Name(String),
    /// A parameter with any of a dispatch mark, a type, a default, and a
    /// description; the check refuses this form with the name alone.
    Full(Param),
}

/// The object form of a [`ParamDef`].
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "schema", derive(schemars::JsonSchema))]
#[serde(deny_unknown_fields)]
pub struct Param {
    /// The parameter's name, a syntactic R name or `...`.
    pub name: String,
    /// The method dispatches on the type of the parameter's argument.
    #[serde(default, skip_serializing_if = "is_false")]
    pub dispatch: bool,
    /// The parameter's type, which every argument satisfies.
    #[serde(rename = "type", default, skip_serializing_if = "Option::is_none")]
    pub ty: Option<FieldType>,
    /// The parameter's default in its JSON form, for a typed parameter. A
    /// `null` is the default of a parameter of type `option()`; a parameter
    /// without the member is required.
    #[serde(
        default,
        skip_serializing_if = "Option::is_none",
        deserialize_with = "crate::tree::exact_present"
    )]
    #[cfg_attr(feature = "schema", schemars(with = "Option<Value>"))]
    pub default: Option<Box<crate::tree::Tree>>,
    /// The parameter's default as an expression of one host's language,
    /// evaluated at each call that omits the argument. A call from that
    /// host may omit the argument; a call from any other host supplies it.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub host_default: Option<HostDefault>,
    /// The parameter's default as an expression of the rule dialect over the
    /// typed parameters, which every host evaluates the same; reserved, and
    /// refused by the check.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub compute: Option<Logic>,
    /// The parameter's description.
    #[serde(default, skip_serializing_if = "str::is_empty")]
    pub description: Box<str>,
}

/// A parameter's default written in a host's language, such as R's
/// `length(x)`.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[cfg_attr(feature = "schema", derive(schemars::JsonSchema))]
#[serde(deny_unknown_fields)]
pub struct HostDefault {
    /// The host language, such as `"r"`.
    pub host: Box<str>,
    /// The expression's source text.
    pub text: Box<str>,
}

impl ParamDef {
    /// The parameter's name.
    pub fn name(&self) -> &str {
        match self {
            ParamDef::Name(n) => n,
            ParamDef::Full(p) => &p.name,
        }
    }

    /// `true` for a parameter marked as dispatched.
    pub fn is_dispatch(&self) -> bool {
        matches!(self, ParamDef::Full(p) if p.dispatch)
    }

    /// The parameter's type, when it has one.
    pub fn ty(&self) -> Option<&FieldType> {
        match self {
            ParamDef::Name(_) => None,
            ParamDef::Full(p) => p.ty.as_ref(),
        }
    }
}

impl From<Param> for ParamDef {
    /// The name alone for a parameter with nothing else, the form the check
    /// requires, and the object form otherwise.
    fn from(p: Param) -> Self {
        if !p.dispatch
            && p.ty.is_none()
            && p.default.is_none()
            && p.host_default.is_none()
            && p.compute.is_none()
            && p.description.is_empty()
        {
            ParamDef::Name(p.name)
        } else {
            ParamDef::Full(p)
        }
    }
}

impl From<&str> for ParamDef {
    fn from(name: &str) -> Self {
        ParamDef::Name(name.to_string())
    }
}

/// The value a method returns: a field type, or `Self`, the type of the
/// method's first dispatched argument.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "schema", derive(schemars::JsonSchema))]
#[serde(untagged)]
pub enum ReturnType {
    /// `"Self"`.
    SelfType(SelfMarker),
    /// A field type.
    Type(Box<FieldType>),
}

/// The marker of [`ReturnType::SelfType`], written `"Self"`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[cfg_attr(feature = "schema", derive(schemars::JsonSchema))]
pub enum SelfMarker {
    /// The type of the method's first dispatched argument.
    #[serde(rename = "Self")]
    SelfType,
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
    /// The parameters, in order; `...` is allowed. Without a dispatch mark
    /// on any parameter, the method dispatches on its first.
    pub params: Vec<ParamDef>,
    /// The type of the value the method returns, which every call
    /// satisfies.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub return_type: Option<ReturnType>,
    /// The method draws random numbers.
    #[serde(default, skip_serializing_if = "is_false")]
    pub stochastic: bool,
    /// The trait supplies a default implementation.
    #[serde(default, skip_serializing_if = "is_false")]
    pub has_default_impl: bool,
    /// The rules over the typed parameters, which every call satisfies after
    /// each argument satisfies its type.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub rules: Vec<RuleDef>,
}

impl MethodDef {
    /// The typed parameters as the fields that the rules read.
    pub fn rule_scope(&self) -> Vec<FieldDef> {
        params_scope(&self.params)
    }

    /// The positions of the parameters the method dispatches on: those
    /// marked, or else the first parameter.
    pub fn dispatched(&self) -> Vec<usize> {
        let marked: Vec<usize> = (0..self.params.len())
            .filter(|&i| self.params[i].is_dispatch())
            .collect();
        if !marked.is_empty() {
            return marked;
        }
        match self.params.first() {
            Some(p) if p.name() != "..." => vec![0],
            _ => Vec::new(),
        }
    }

    /// The parameters' names, in order.
    pub fn param_names(&self) -> impl Iterator<Item = &str> {
        self.params.iter().map(ParamDef::name)
    }
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
    /// Fields every implementing struct has, each of a type that the
    /// struct's field fits.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub required_fields: Vec<RequiredField>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    /// The traits every implementor also implements.
    pub supertraits: Vec<QualifiedName>,
    /// The member that names an implementor in JSON; without it, `"$type"`.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub tag: Option<Name>,
    /// The trait of a generic declared by `generic()`: one method, named as
    /// the trait.
    #[serde(default, skip_serializing_if = "is_false")]
    pub generic: bool,
    /// The number of type parameters the trait declares beside its
    /// dispatched positions, which an impl gives after the dispatched types
    /// in `type_args`: 1 for the standard trait `From(source)`.
    #[serde(default, skip_serializing_if = "is_zero")]
    pub extra_type_params: u8,
}

impl TraitDef {
    /// The number of positions the trait's methods dispatch on: that of its
    /// first method, which the check requires of every method.
    pub fn dispatch_arity(&self) -> usize {
        self.methods
            .first()
            .map_or(1, |m| m.dispatched().len().max(1))
    }
}

/// What a function requires of its parameters' types.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Serialize, Deserialize)]
#[cfg_attr(feature = "schema", derive(schemars::JsonSchema))]
#[serde(rename_all = "snake_case")]
pub enum Typed {
    /// Parameters without a type, and `...`, are admitted.
    #[default]
    Mixed,
    /// Every parameter has a type, and there is no `...`.
    Strict,
}

impl Typed {
    fn is_mixed(&self) -> bool {
        *self == Typed::Mixed
    }
}

/// `new_function()`: a function with typed parameters, called by its name.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "schema", derive(schemars::JsonSchema))]
#[serde(deny_unknown_fields)]
pub struct FunctionDef {
    /// The function's name.
    pub name: Name,
    #[serde(default)]
    /// The function's description.
    pub description: String,
    /// The parameters, in order; none dispatches.
    pub params: Vec<ParamDef>,
    /// The type of the value the function returns, which every call
    /// satisfies; a field type, never `Self`.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub return_type: Option<ReturnType>,
    /// The rules over the typed parameters, which every call satisfies after
    /// each argument satisfies its type.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub rules: Vec<RuleDef>,
    /// What the function requires of its parameters' types.
    #[serde(default, skip_serializing_if = "Typed::is_mixed")]
    pub typed: Typed,
    /// The function draws random numbers.
    #[serde(default, skip_serializing_if = "is_false")]
    pub stochastic: bool,
    /// The type that the function belongs to, for a function on a type;
    /// reserved, and refused by the check.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub on: Option<QualifiedName>,
}

impl FunctionDef {
    /// The typed parameters as the fields that the rules read.
    pub fn rule_scope(&self) -> Vec<FieldDef> {
        params_scope(&self.params)
    }
}

/// Typed parameters as fields, for the rules that read them.
fn params_scope(params: &[ParamDef]) -> Vec<FieldDef> {
    params
        .iter()
        .filter_map(|p| {
            let ty = p.ty()?.clone();
            let name = Name::new(p.name()).ok()?;
            Some(FieldDef {
                name,
                ty,
                default: None,
                description: Box::from(""),
                sensitive: false,
                unit: None,
                resolve: None,
                inherited_from: None,
                extensions: None,
            })
        })
        .collect()
}

/// The class system of a host class.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[cfg_attr(feature = "schema", derive(schemars::JsonSchema))]
#[serde(rename_all = "snake_case")]
pub enum ClassSystem {
    /// An S3 class: a name in an object's `class` attribute.
    S3,
    /// An S4 class: a class defined with `methods::setClass()`.
    S4,
    /// An implicit class: a name that `.class2()` derives from an object's
    /// type and `dim` attribute, such as `"matrix"` or `"NULL"`.
    Implicit,
    /// An S7 class: its name in an object's `class` attribute, qualified by
    /// its package, `pkg::Class`, as S7 writes it.
    S7,
}

/// A type an impl names: its target, or one of its type arguments.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "schema", derive(schemars::JsonSchema))]
#[serde(tag = "kind", rename_all = "snake_case", deny_unknown_fields)]
pub enum TypeRef {
    /// An R8 type, by its qualified name.
    Named {
        /// The type's qualified name.
        name: QualifiedName,
    },
    /// A class of another class system of the host language, such as R's
    /// `data.frame` or `dgCMatrix`.
    HostClass {
        /// The host language, such as `"r"`.
        host: String,
        /// The package that defines the class, when the declaration names
        /// one.
        #[serde(default, skip_serializing_if = "String::is_empty")]
        package: String,
        /// The class name.
        class: String,
        /// The class system.
        system: ClassSystem,
    },
    /// Any value: a dispatched position that an impl does not specialize.
    Any,
}

impl std::fmt::Display for TypeRef {
    /// The qualified name of an R8 type, the class name of a host class, or
    /// `any`.
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeRef::Named { name } => write!(f, "{name}"),
            TypeRef::HostClass { class, .. } => f.write_str(class),
            TypeRef::Any => f.write_str("any"),
        }
    }
}

/// `impl()`: a trait implemented for a type.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "schema", derive(schemars::JsonSchema))]
#[serde(deny_unknown_fields)]
pub struct ImplDef {
    /// The implemented trait.
    pub r#trait: QualifiedName,
    /// The types of the trait's positions after the first: its dispatched
    /// positions after the first, in order, then its type parameters, such
    /// as the source of `From(source)`. Rust writes the impl
    /// `impl Trait<B> for A`.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub type_args: Vec<TypeRef>,
    /// What the trait is implemented for: its first dispatched position.
    pub target: TypeRef,
    /// The value of the trait's tag member; defaults to the type's name.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub tag_value: Option<String>,
    /// The methods this impl defines; the others take the trait's default
    /// implementations.
    pub methods: Vec<ImplMethod>,
}

/// A method an impl defines: its name, or an object that also gives the
/// type of the value this impl's method returns, which fits the trait
/// method's return type.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "schema", derive(schemars::JsonSchema))]
#[serde(untagged)]
pub enum ImplMethod {
    /// A method that returns the trait method's return type.
    Name(MethodName),
    /// A method with its own return type.
    Typed(TypedImplMethod),
}

/// The object form of an [`ImplMethod`].
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "schema", derive(schemars::JsonSchema))]
#[serde(deny_unknown_fields)]
pub struct TypedImplMethod {
    /// The method's name.
    pub name: MethodName,
    /// The type of the value the method returns.
    pub return_type: FieldType,
}

impl ImplMethod {
    /// The method's name.
    pub fn name(&self) -> &MethodName {
        match self {
            ImplMethod::Name(n) => n,
            ImplMethod::Typed(t) => &t.name,
        }
    }

    /// The method's own return type, when it declares one.
    pub fn return_type(&self) -> Option<&FieldType> {
        match self {
            ImplMethod::Name(_) => None,
            ImplMethod::Typed(t) => Some(&t.return_type),
        }
    }
}

impl From<MethodName> for ImplMethod {
    fn from(name: MethodName) -> Self {
        ImplMethod::Name(name)
    }
}
