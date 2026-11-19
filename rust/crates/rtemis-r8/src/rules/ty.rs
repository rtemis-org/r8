//! Static types of rule expressions, derived from field types.

use crate::ir::{FieldDef, FieldType, TypeDef, VariantShape};
use crate::name::QualifiedName;

/// Finds the declaration of a named type. A type that the lookup does not
/// hold compiles as [`Ty::Any`], and its checks wait for the registry.
pub trait TypeLookup {
    /// The declaration of the named type, if the lookup holds it.
    fn find(&self, name: &QualifiedName) -> Option<&TypeDef>;

    /// The concrete type of a `dyn(trait)` value, and the member of the value
    /// that names it. The default reads `"$type"`, a qualified name.
    fn implementor(
        &self,
        _trait: &QualifiedName,
        value: &serde_json::Value,
    ) -> Result<(QualifiedName, String), String> {
        dollar_type(value)
    }

    /// The member that names a `dyn(trait)` value's type: the trait's tag
    /// member, or `"$type"`.
    fn type_member(&self, _trait: &QualifiedName) -> String {
        "$type".to_string()
    }
}

fn dollar_type(value: &serde_json::Value) -> Result<(QualifiedName, String), String> {
    let s = value
        .get("$type")
        .and_then(serde_json::Value::as_str)
        .ok_or("a `dyn()` value names its type in `$type`")?;
    QualifiedName::parse(s)
        .map(|q| (q, "$type".to_string()))
        .map_err(|e| e.to_string())
}

impl TypeLookup for crate::ir::IrDocument {
    fn find(&self, name: &QualifiedName) -> Option<&TypeDef> {
        if name.namespace != self.namespace {
            return None;
        }
        self.types.iter().find(|t| t.name() == &name.name)
    }

    /// Resolves through the trait's tag member when the document declares the
    /// trait with one: a bare tag value names an implementor in the trait's
    /// namespace, and `namespace::Value` one in another namespace. Each
    /// implementor's tag value is its impl's `tag_value`, or its type name.
    fn implementor(
        &self,
        tr: &QualifiedName,
        value: &serde_json::Value,
    ) -> Result<(QualifiedName, String), String> {
        let tag = (tr.namespace == self.namespace)
            .then(|| self.traits.iter().find(|t| t.name == tr.name))
            .flatten()
            .and_then(|t| t.tag.clone());
        let Some(tag) = tag else {
            return dollar_type(value);
        };
        let written = value
            .get(tag.as_str())
            .and_then(serde_json::Value::as_str)
            .ok_or_else(|| format!("a `dyn({tr})` value names its type in `{tag}`"))?;
        for imp in self.impls.iter().filter(|i| &i.r#trait == tr) {
            let crate::ir::ImplTarget::Type { name } = &imp.target else {
                continue;
            };
            let bare = imp
                .tag_value
                .clone()
                .unwrap_or_else(|| name.name.to_string());
            let expected = if name.namespace == tr.namespace {
                bare
            } else {
                format!("{}::{bare}", name.namespace)
            };
            if expected == written {
                return Ok((name.clone(), tag.to_string()));
            }
        }
        Err(format!(
            "no registered implementor of `{tr}` has the tag value `{written}`"
        ))
    }

    fn type_member(&self, tr: &QualifiedName) -> String {
        (tr.namespace == self.namespace)
            .then(|| self.traits.iter().find(|t| t.name == tr.name))
            .flatten()
            .and_then(|t| t.tag.as_ref().map(|n| n.to_string()))
            .unwrap_or_else(|| "$type".to_string())
    }
}

/// The static type of an expression.
#[derive(Debug, Clone, PartialEq)]
pub enum Ty {
    /// A logical.
    Bool,
    /// A number, with its declared unit, if any. `None` is exempt from the
    /// unit check.
    Number {
        /// The declared unit.
        unit: Option<String>,
    },
    /// A string.
    String,
    /// A date.
    Date,
    /// A date-time.
    Posixct,
    /// A factor; `levels` holds the declared levels.
    Factor {
        /// The declared levels.
        levels: Option<Vec<String>>,
    },
    /// An array of the element type (`vec()`, and `levels()` and `dim()`
    /// results).
    Array(Box<Ty>),
    /// A table's columns, in declaration order.
    Table {
        /// The declared columns.
        columns: Vec<FieldDef>,
        /// Other columns are admitted.
        open: bool,
    },
    /// A matrix, array, or sparse matrix of the element type.
    Matrix(Box<Ty>),
    /// A map; `ordered` for `ordered_map()`, whose JSON is an array of
    /// `{"name", "value"}` pairs.
    Map {
        /// The value type.
        value: Box<Ty>,
        /// An `ordered_map()`.
        ordered: bool,
    },
    /// A named struct or enum.
    Named(QualifiedName),
    /// Any JSON value (`type_json()`).
    Json,
    /// A runtime-only R value, which rules cannot read.
    Runtime,
    /// Unknown until run time: `dyn()`, `type_raw()`, and named types the
    /// lookup does not hold.
    Any,
}

impl Ty {
    /// The static type of a field type; `unit` is the field's declared unit.
    pub fn of(ft: &FieldType, unit: Option<&str>) -> Ty {
        match ft {
            FieldType::Logical => Ty::Bool,
            FieldType::Integer(_) | FieldType::Double(_) => Ty::Number {
                unit: unit.map(str::to_string),
            },
            FieldType::Character(_) => Ty::String,
            FieldType::Raw => Ty::Any,
            FieldType::Date => Ty::Date,
            FieldType::Posixct => Ty::Posixct,
            FieldType::Factor { levels, .. } => Ty::Factor {
                levels: levels.clone(),
            },
            FieldType::Matrix { element, .. }
            | FieldType::Array { element, .. }
            | FieldType::SparseMatrix { element, .. } => {
                Ty::Matrix(Box::new(Ty::of(element, unit)))
            }
            FieldType::MatrixOf { .. } => Ty::Matrix(Box::new(Ty::Any)),
            FieldType::Table { columns, open, .. } => Ty::Table {
                columns: columns.clone(),
                open: *open,
            },
            FieldType::TableOf { .. } => Ty::Table {
                columns: Vec::new(),
                open: true,
            },
            FieldType::Json => Ty::Json,
            FieldType::R { .. } => Ty::Runtime,
            FieldType::Vec { element, .. } => Ty::Array(Box::new(Ty::of(element, unit))),
            FieldType::Option { inner, .. } => Ty::of(inner, unit),
            FieldType::Hashmap { value, .. } => Ty::Map {
                value: Box::new(Ty::of(value, unit)),
                ordered: false,
            },
            FieldType::OrderedMap { value, .. } => Ty::Map {
                value: Box::new(Ty::of(value, unit)),
                ordered: true,
            },
            FieldType::Named { name } => Ty::Named(name.clone()),
            FieldType::Dyn { .. } => Ty::Any,
        }
    }

    /// A short name for messages.
    pub fn describe(&self) -> String {
        match self {
            Ty::Bool => "a logical".into(),
            Ty::Number { unit: Some(u) } => format!("a number in `{u}`"),
            Ty::Number { unit: None } => "a number".into(),
            Ty::String => "a string".into(),
            Ty::Date => "a date".into(),
            Ty::Posixct => "a date-time".into(),
            Ty::Factor { .. } => "a factor".into(),
            Ty::Array(e) => format!("an array of {}", e.describe()),
            Ty::Table { .. } => "a table".into(),
            Ty::Matrix(_) => "a matrix".into(),
            Ty::Map { .. } => "a map".into(),
            Ty::Named(q) => format!("a `{q}`"),
            Ty::Json => "a JSON value".into(),
            Ty::Runtime => "a runtime-only R value".into(),
            Ty::Any => "a value of unknown type".into(),
        }
    }
}

/// What a named type contributes to path resolution: its fields, and for an
/// enum, how a value names its variant.
pub enum Shape<'a> {
    Struct(&'a [FieldDef]),
    Enum {
        /// `None` for an enum of unit variants, whose values are strings.
        tag: Option<&'a str>,
        variants: Vec<(&'a str, VariantFields<'a>)>,
    },
}

/// The fields a variant holds.
pub enum VariantFields<'a> {
    None,
    Fields(&'a [FieldDef]),
    /// A newtype variant: the wrapped struct's fields, or `None` when the
    /// lookup does not hold it.
    Wrapped(Option<&'a [FieldDef]>),
}

/// The shape of a named type, when the lookup holds it.
pub fn shape<'a>(lookup: &'a dyn TypeLookup, name: &QualifiedName) -> Option<Shape<'a>> {
    match lookup.find(name)? {
        TypeDef::Struct(s) => Some(Shape::Struct(&s.fields)),
        TypeDef::Enum(e) => {
            let unit_only = e
                .variants
                .iter()
                .all(|v| matches!(v.shape, VariantShape::Unit));
            let variants = e
                .variants
                .iter()
                .map(|v| {
                    let fields = match &v.shape {
                        VariantShape::Unit => VariantFields::None,
                        VariantShape::Struct { fields, .. } => VariantFields::Fields(fields),
                        VariantShape::Newtype { of } => {
                            VariantFields::Wrapped(match lookup.find(of) {
                                Some(TypeDef::Struct(s)) => Some(&s.fields),
                                _ => None,
                            })
                        }
                    };
                    (v.name.as_str(), fields)
                })
                .collect();
            Some(Shape::Enum {
                tag: (!unit_only).then_some(e.tag.as_str()),
                variants,
            })
        }
    }
}

/// A set of IR documents, each a namespace, resolved together: the lookup a
/// registry provides once every package's declarations are loaded.
#[derive(Debug, Clone, Default)]
pub struct DocumentSet(pub Vec<crate::ir::IrDocument>);

impl DocumentSet {
    fn namespace(&self, ns: &crate::name::Name) -> Option<&crate::ir::IrDocument> {
        self.0.iter().find(|d| &d.namespace == ns)
    }
}

impl TypeLookup for DocumentSet {
    fn find(&self, name: &QualifiedName) -> Option<&TypeDef> {
        self.namespace(&name.namespace)?.find(name)
    }

    /// Resolves through the trait's declaring document, over the impls of
    /// every document.
    fn implementor(
        &self,
        tr: &QualifiedName,
        value: &serde_json::Value,
    ) -> Result<(QualifiedName, String), String> {
        let Some(home) = self.namespace(&tr.namespace) else {
            return dollar_type(value);
        };
        let mut merged = home.clone();
        for d in self.0.iter().filter(|d| d.namespace != tr.namespace) {
            merged.impls.extend(d.impls.iter().cloned());
        }
        let (ty, member) = merged.implementor(tr, value)?;
        Ok((ty, member))
    }

    fn type_member(&self, tr: &QualifiedName) -> String {
        match self.namespace(&tr.namespace) {
            Some(d) => d.type_member(tr),
            None => "$type".to_string(),
        }
    }
}
