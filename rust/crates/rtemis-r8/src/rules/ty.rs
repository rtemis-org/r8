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

    /// The impls of the trait `tr` that the lookup holds.
    fn impls_of(&self, _tr: &QualifiedName) -> Vec<&crate::ir::ImplDef> {
        Vec::new()
    }

    /// The structs that descend from `ty` through `extends`, as far as the
    /// lookup holds them, every parent before its children.
    fn descendants_of(&self, _ty: &QualifiedName) -> Vec<QualifiedName> {
        Vec::new()
    }

    /// The impl of the trait `tr` for the type `ty` itself.
    fn impl_for(&self, tr: &QualifiedName, ty: &QualifiedName) -> Option<&crate::ir::ImplDef> {
        self.impls_of(tr)
            .into_iter()
            .find(|i| matches!(&i.target, crate::ir::ImplTarget::Type { name } if name == ty))
    }

    /// The type whose impl of the trait `tr` applies to the type `ty`: `ty`,
    /// or its nearest ancestor through `extends` with an impl, as dispatch
    /// finds it; `None` when neither has one.
    fn implements(&self, tr: &QualifiedName, ty: &QualifiedName) -> Option<QualifiedName> {
        if self.impl_for(tr, ty).is_some() {
            return Some(ty.clone());
        }
        self.ancestors(ty)
            .into_iter()
            .find(|a| self.impl_for(tr, a).is_some())
    }

    /// The written tag value of the type `ty` for the trait `tr`, when the
    /// trait has a tag member: the `tag_value` of `ty`'s own impl, or else
    /// `ty`'s name, bare when `ty` is in the trait's namespace and prefixed
    /// with its namespace otherwise.
    fn tag_value(&self, tr: &QualifiedName, ty: &QualifiedName) -> Option<String> {
        if self.type_member(tr) == "$type" {
            return None;
        }
        let own = self.impl_for(tr, ty).and_then(|i| i.tag_value.clone());
        let value = own.unwrap_or_else(|| ty.name.to_string());
        Some(if ty.namespace == tr.namespace {
            value
        } else {
            format!("{}::{value}", ty.namespace)
        })
    }

    /// Whether the struct `ty` is stale: its inherited fields or rules
    /// disagree with its parent's, or its parent is stale.
    fn is_stale(&self, _ty: &QualifiedName) -> bool {
        false
    }

    /// Whether the type `ty` is the struct `base` or descends from it
    /// through `extends`, as far as the lookup holds the chain.
    fn descends(&self, ty: &QualifiedName, base: &QualifiedName) -> bool {
        let mut cur = ty;
        // A chain longer than the lookup's types is a cycle.
        for _ in 0..MAX_DEPTH {
            if cur == base {
                return true;
            }
            match self.find(cur) {
                Some(TypeDef::Struct(s)) => match &s.extends {
                    Some(p) => cur = p,
                    None => return false,
                },
                _ => return false,
            }
        }
        false
    }

    /// The ancestors of the type `ty` through `extends`, the parent first,
    /// as far as the lookup holds the chain; a chain that returns to a type
    /// already listed ends before it.
    fn ancestors(&self, ty: &QualifiedName) -> Vec<QualifiedName> {
        let mut out: Vec<QualifiedName> = Vec::new();
        let mut cur = ty;
        while let Some(TypeDef::Struct(s)) = self.find(cur) {
            let Some(p) = &s.extends else { break };
            if p == ty || out.contains(p) {
                break;
            }
            out.push(p.clone());
            cur = p;
        }
        out
    }
}

/// The longest `extends` chain that [`TypeLookup::descends`] follows.
const MAX_DEPTH: usize = 1024;

pub(crate) fn dollar_type(value: &serde_json::Value) -> Result<(QualifiedName, String), String> {
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
        implementor_of(self, tr, value)
    }

    fn impls_of(&self, tr: &QualifiedName) -> Vec<&crate::ir::ImplDef> {
        self.impls.iter().filter(|i| &i.r#trait == tr).collect()
    }

    /// The document's structs that descend from `ty`.
    fn descendants_of(&self, ty: &QualifiedName) -> Vec<QualifiedName> {
        let mut out: Vec<QualifiedName> = Vec::new();
        let mut frontier = vec![ty.clone()];
        while let Some(p) = frontier.pop() {
            for t in &self.types {
                if let TypeDef::Struct(s) = t
                    && s.extends.as_ref() == Some(&p)
                {
                    let q = QualifiedName {
                        namespace: self.namespace.clone(),
                        name: s.name.clone(),
                    };
                    if &q != ty && !out.contains(&q) {
                        out.push(q.clone());
                        frontier.push(q);
                    }
                }
            }
        }
        out
    }

    fn type_member(&self, tr: &QualifiedName) -> String {
        (tr.namespace == self.namespace)
            .then(|| self.traits.iter().find(|t| t.name == tr.name))
            .flatten()
            .and_then(|t| t.tag.as_ref().map(|n| n.to_string()))
            .unwrap_or_else(|| "$type".to_string())
    }
}

/// The implementor of the trait `tr` that a `dyn()` value names: through
/// the trait's tag member when the trait declares one, and otherwise through
/// `"$type"`. A written tag value `v` names, in the trait's namespace, or
/// `ns::v` in the namespace `ns`, the type whose impl of `tr` gives `v` as
/// its `tag_value`, or else the type named `v` there whose impl gives none
/// or which inherits an impl through `extends`.
fn implementor_of(
    lookup: &dyn TypeLookup,
    tr: &QualifiedName,
    value: &serde_json::Value,
) -> Result<(QualifiedName, String), String> {
    let tag = lookup.type_member(tr);
    if tag == "$type" {
        return dollar_type(value);
    }
    let written = value
        .get(tag.as_str())
        .and_then(serde_json::Value::as_str)
        .ok_or_else(|| format!("a `dyn({tr})` value names its type in `{tag}`"))?;
    resolve_tag(lookup, tr, written)
        .map(|q| (q, tag.clone()))
        .ok_or_else(|| {
            let known = tag_values(lookup, tr);
            let listed = if known.is_empty() {
                "none".to_string()
            } else {
                known.iter().map(|v| format!("`{v}`")).collect::<Vec<_>>().join(", ")
            };
            format!(
                "no registered implementor of `{tr}` has the tag value `{written}`; the registered tag values are {listed}"
            )
        })
}

/// The written tag values of the registered implementors of `tr`.
fn tag_values(lookup: &dyn TypeLookup, tr: &QualifiedName) -> Vec<String> {
    let mut out = Vec::new();
    for t in implementors(lookup, tr) {
        if let Some(v) = lookup.tag_value(tr, &t)
            && !out.contains(&v)
        {
            out.push(v);
        }
    }
    out
}

/// The implementors of `tr` that the lookup holds: the targets of its
/// impls, each followed by the descendants that inherit its impl.
pub fn implementors(lookup: &dyn TypeLookup, tr: &QualifiedName) -> Vec<QualifiedName> {
    let mut out: Vec<QualifiedName> = Vec::new();
    for imp in lookup.impls_of(tr) {
        let crate::ir::ImplTarget::Type { name } = &imp.target else {
            continue;
        };
        for t in std::iter::once(name.clone()).chain(
            lookup
                .descendants_of(name)
                .into_iter()
                .filter(|d| lookup.implements(tr, d).as_ref() == Some(name)),
        ) {
            if !out.contains(&t) {
                out.push(t);
            }
        }
    }
    out
}

/// The implementor of `tr` whose written tag value is `written`.
fn resolve_tag(
    lookup: &dyn TypeLookup,
    tr: &QualifiedName,
    written: &str,
) -> Option<QualifiedName> {
    let (ns, v) = match written.split_once("::") {
        Some((ns, v)) => (ns, v),
        None => (tr.namespace.as_str(), written),
    };
    // A prefix names another namespace than the trait's.
    if written.contains("::") && ns == tr.namespace.as_str() {
        return None;
    }
    for imp in lookup.impls_of(tr) {
        if let crate::ir::ImplTarget::Type { name } = &imp.target
            && name.namespace.as_str() == ns
            && imp.tag_value.as_deref() == Some(v)
        {
            return Some(name.clone());
        }
    }
    let candidate = QualifiedName::parse(&format!("{ns}::{v}")).ok()?;
    lookup.find(&candidate)?;
    lookup.implements(tr, &candidate)?;
    match lookup
        .impl_for(tr, &candidate)
        .and_then(|i| i.tag_value.as_deref())
    {
        Some(own) if own != v => None,
        _ => Some(candidate),
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
        /// The number is an integer: an integer field or literal, or a
        /// result of integers that rounding leaves exact. A double field or
        /// literal, and a quotient, are not.
        exact: bool,
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
        levels: Option<Box<[Box<str>]>>,
    },
    /// An array of the element type (`vec()`, and `levels()` and `dim()`
    /// results).
    Array(Box<Ty>),
    /// A table's columns, in declaration order.
    Table {
        /// The declared columns.
        columns: Box<[FieldDef]>,
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
            FieldType::Integer(_) => Ty::Number {
                unit: unit.map(str::to_string),
                exact: true,
            },
            FieldType::Double(_) => Ty::Number {
                unit: unit.map(str::to_string),
                exact: false,
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
                columns: Box::default(),
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
            FieldType::Dyn { .. } | FieldType::Descendant { .. } => Ty::Any,
        }
    }

    /// A short name for messages.
    pub fn describe(&self) -> String {
        match self {
            Ty::Bool => "a logical".into(),
            Ty::Number { unit: Some(u), .. } => format!("a number in `{u}`"),
            Ty::Number { unit: None, .. } => "a number".into(),
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
///
/// The set indexes its types by qualified name, so [`TypeLookup::find`] takes
/// constant time in the number of types. The index holds positions only and
/// compares against the stored declarations, so it copies no name. Every
/// change to a document's types goes through the set's methods, which keep
/// the index current.
#[derive(Clone, Default)]
pub struct DocumentSet {
    docs: Vec<crate::ir::IrDocument>,
    /// `(document, type)` positions, hashed by namespace and name.
    index: hashbrown::HashTable<(u32, u32)>,
    hasher: hashbrown::DefaultHashBuilder,
    /// The structs that name each struct as their parent, by the parent.
    children: std::collections::HashMap<QualifiedName, Vec<QualifiedName>>,
    /// The structs whose inherited fields or rules disagree with their
    /// parent's, or whose parent is stale.
    stale: std::collections::HashSet<QualifiedName>,
}

impl std::fmt::Debug for DocumentSet {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("DocumentSet").field(&self.docs).finish()
    }
}

impl From<Vec<crate::ir::IrDocument>> for DocumentSet {
    fn from(docs: Vec<crate::ir::IrDocument>) -> Self {
        let mut set = DocumentSet {
            docs,
            ..DocumentSet::default()
        };
        set.reindex();
        set
    }
}

impl FromIterator<crate::ir::IrDocument> for DocumentSet {
    fn from_iter<I: IntoIterator<Item = crate::ir::IrDocument>>(iter: I) -> Self {
        DocumentSet::from(iter.into_iter().collect::<Vec<_>>())
    }
}

/// Where [`DocumentSet::put_type`] placed a declaration, and the one it
/// Where `DocumentSet::put_trait()` put a trait, and the trait it replaced.
#[derive(Debug)]
pub struct PlacedTrait {
    /// The position of the namespace's document.
    pub document: usize,
    /// The position of the trait in the document.
    pub index: usize,
    /// The trait of the same name that the put replaced.
    pub previous: Option<crate::ir::TraitDef>,
}

/// replaced; [`DocumentSet::undo_put`] takes it back.
#[derive(Debug)]
pub struct Placed {
    /// The position of the namespace's document.
    pub document: usize,
    /// The position of the type in the document.
    pub index: usize,
    /// The declaration replaced, or `None` for a new type.
    pub previous: Option<TypeDef>,
    /// The document was created for this declaration.
    pub new_document: bool,
}

/// The hash of a qualified name, by its namespace and name.
fn hash_name(hasher: &hashbrown::DefaultHashBuilder, ns: &str, name: &str) -> u64 {
    use std::hash::{BuildHasher, Hash, Hasher};
    let mut h = hasher.build_hasher();
    ns.hash(&mut h);
    name.hash(&mut h);
    h.finish()
}

impl DocumentSet {
    fn hash(&self, ns: &str, name: &str) -> u64 {
        hash_name(&self.hasher, ns, name)
    }

    fn at(&self, (d, t): (u32, u32)) -> &TypeDef {
        &self.docs[d as usize].types[t as usize]
    }

    fn slot(&self, ns: &str, name: &str) -> Option<(u32, u32)> {
        let h = self.hash(ns, name);
        self.index
            .find(h, |&(d, t)| {
                self.docs[d as usize].namespace.as_str() == ns
                    && self.docs[d as usize].types[t as usize].name().as_str() == name
            })
            .copied()
    }

    fn insert_slot(&mut self, d: usize, t: usize) {
        let doc = &self.docs[d];
        let h = self.hash(doc.namespace.as_str(), doc.types[t].name().as_str());
        let (docs, hasher) = (&self.docs, &self.hasher);
        // Growing the table rehashes the slots from the declarations.
        let rehash = |&(d, t): &(u32, u32)| {
            let doc = &docs[d as usize];
            hash_name(
                hasher,
                doc.namespace.as_str(),
                doc.types[t as usize].name().as_str(),
            )
        };
        self.index.insert_unique(h, (d as u32, t as u32), rehash);
    }

    fn reindex(&mut self) {
        self.index.clear();
        self.children.clear();
        for d in 0..self.docs.len() {
            for t in 0..self.docs[d].types.len() {
                self.insert_slot(d, t);
            }
        }
        for doc in &self.docs {
            for t in &doc.types {
                if let TypeDef::Struct(s) = t
                    && let Some(parent) = &s.extends
                {
                    self.children
                        .entry(parent.clone())
                        .or_default()
                        .push(QualifiedName {
                            namespace: doc.namespace.clone(),
                            name: s.name.clone(),
                        });
                }
            }
        }
    }

    /// Adds or removes the entry of the struct `def` of the namespace `ns`
    /// in the index of children.
    fn link(&mut self, ns: &crate::name::Name, def: &TypeDef, add: bool) {
        let TypeDef::Struct(s) = def else { return };
        let Some(parent) = &s.extends else { return };
        let child = QualifiedName {
            namespace: ns.clone(),
            name: s.name.clone(),
        };
        let list = self.children.entry(parent.clone()).or_default();
        if add {
            if !list.contains(&child) {
                list.push(child);
            }
        } else {
            list.retain(|c| c != &child);
            if list.is_empty() {
                self.children.remove(parent);
            }
        }
    }

    /// The structs whose parent is `q`.
    pub fn children(&self, q: &QualifiedName) -> &[QualifiedName] {
        self.children.get(q).map_or(&[], Vec::as_slice)
    }

    /// The structs that descend from `q`, each once, every parent before its
    /// children.
    pub fn descendants(&self, q: &QualifiedName) -> Vec<QualifiedName> {
        let mut out: Vec<QualifiedName> = Vec::new();
        let mut i = 0;
        let mut next: Vec<QualifiedName> = self.children(q).to_vec();
        while i < next.len() {
            let c = next[i].clone();
            i += 1;
            if c == *q || out.contains(&c) {
                continue;
            }
            next.extend(self.children(&c).iter().cloned());
            out.push(c);
        }
        out
    }

    /// Marks the struct `q` stale, or clears its mark.
    pub fn set_stale(&mut self, q: &QualifiedName, stale: bool) {
        if stale {
            self.stale.insert(q.clone());
        } else {
            self.stale.remove(q);
        }
    }

    /// Whether any struct is stale.
    pub fn has_stale(&self) -> bool {
        !self.stale.is_empty()
    }

    /// The stale structs, sorted.
    pub fn stale_types(&self) -> Vec<QualifiedName> {
        let mut out: Vec<QualifiedName> = self.stale.iter().cloned().collect();
        out.sort_by_key(|q| q.to_string());
        out
    }

    /// The documents, one per namespace.
    pub fn documents(&self) -> &[crate::ir::IrDocument] {
        &self.docs
    }

    /// The documents, one per namespace, taken out of the set.
    pub fn into_documents(self) -> Vec<crate::ir::IrDocument> {
        self.docs
    }

    /// The document of the namespace `ns`.
    pub fn namespace(&self, ns: &crate::name::Name) -> Option<&crate::ir::IrDocument> {
        self.docs.iter().find(|d| &d.namespace == ns)
    }

    /// The position of the document of `ns`, and whether it was created
    /// empty for this call.
    fn document_of(&mut self, ns: &crate::name::Name) -> (usize, bool) {
        match self.docs.iter().position(|d| &d.namespace == ns) {
            Some(i) => (i, false),
            None => {
                self.docs.push(crate::ir::IrDocument {
                    ir_version: crate::ir::IR_VERSION,
                    namespace: ns.clone(),
                    types: Vec::new(),
                    traits: Vec::new(),
                    impls: Vec::new(),
                });
                (self.docs.len() - 1, true)
            }
        }
    }

    /// Adds the declaration `def` to the namespace `ns`, or replaces the type
    /// of the same name.
    pub fn put_type(&mut self, ns: &crate::name::Name, def: TypeDef) -> Placed {
        let (d, new_document) = self.document_of(ns);
        self.link(ns, &def, true);
        match self.slot(ns.as_str(), def.name().as_str()) {
            Some((_, t)) => {
                let previous = std::mem::replace(&mut self.docs[d].types[t as usize], def);
                // Unlink the replaced declaration's parent, unless it is the
                // new one's.
                let same_parent = matches!(
                    (&previous, &self.docs[d].types[t as usize]),
                    (TypeDef::Struct(a), TypeDef::Struct(b)) if a.extends == b.extends
                );
                if !same_parent {
                    self.link(ns, &previous, false);
                }
                Placed {
                    document: d,
                    index: t as usize,
                    previous: Some(previous),
                    new_document,
                }
            }
            None => {
                self.docs[d].types.push(def);
                let t = self.docs[d].types.len() - 1;
                self.insert_slot(d, t);
                Placed {
                    document: d,
                    index: t,
                    previous: None,
                    new_document,
                }
            }
        }
    }

    /// Takes back the most recent [`put_type`](Self::put_type): restores the
    /// declaration it replaced, or removes the type it added, and the document
    /// it created.
    pub fn undo_put(&mut self, placed: Placed) {
        let Placed {
            document: d,
            index: t,
            previous,
            new_document,
        } = placed;
        let ns = self.docs[d].namespace.clone();
        let current = self.docs[d].types[t].clone();
        self.link(&ns, &current, false);
        if let Some(p) = &previous {
            self.link(&ns, p, true);
        }
        match previous {
            Some(p) => self.docs[d].types[t] = p,
            None => {
                let h = {
                    let doc = &self.docs[d];
                    self.hash(doc.namespace.as_str(), doc.types[t].name().as_str())
                };
                if let Ok(e) = self
                    .index
                    .find_entry(h, |&slot| slot == (d as u32, t as u32))
                {
                    e.remove();
                }
                self.docs[d].types.remove(t);
                if t < self.docs[d].types.len() {
                    self.reindex();
                }
            }
        }
        if new_document && self.docs[d].types.is_empty() {
            self.remove_document_if_empty(d);
        }
    }

    /// The position of the document of `ns`, created empty when the set has
    /// none; for traits and impls, which the type index does not cover.
    pub fn document_position(&mut self, ns: &crate::name::Name) -> usize {
        self.document_of(ns).0
    }

    /// The document at `d`.
    pub fn document(&self, d: usize) -> &crate::ir::IrDocument {
        &self.docs[d]
    }

    /// The traits of the document at `d`, to add or replace a trait.
    pub fn traits_mut(&mut self, d: usize) -> &mut Vec<crate::ir::TraitDef> {
        &mut self.docs[d].traits
    }

    /// The impls of the document at `d`, to add or replace an impl.
    pub fn impls_mut(&mut self, d: usize) -> &mut Vec<crate::ir::ImplDef> {
        &mut self.docs[d].impls
    }

    /// Puts a trait into the namespace's document, replacing a trait of the
    /// same name; `undo_put_trait()` with the result restores the set.
    pub fn put_trait(&mut self, ns: &crate::name::Name, def: crate::ir::TraitDef) -> PlacedTrait {
        let d = self.document_position(ns);
        let traits = self.traits_mut(d);
        match traits.iter().position(|t| t.name == def.name) {
            Some(i) => PlacedTrait {
                document: d,
                index: i,
                previous: Some(std::mem::replace(&mut traits[i], def)),
            },
            None => {
                traits.push(def);
                PlacedTrait {
                    document: d,
                    index: traits.len() - 1,
                    previous: None,
                }
            }
        }
    }

    /// Undoes a `put_trait()`.
    pub fn undo_put_trait(&mut self, placed: PlacedTrait) {
        let PlacedTrait {
            document: d,
            index,
            previous,
        } = placed;
        let traits = self.traits_mut(d);
        match previous {
            Some(p) => traits[index] = p,
            None => {
                traits.pop();
            }
        }
        self.remove_document_if_empty(d);
    }

    /// The qualified names of `q` and of every named type it reaches
    /// through field types, newtype variants, and `extends`, each once, the
    /// reached types before the types that reach them.
    pub fn reached(&self, q: &crate::name::QualifiedName) -> Vec<crate::name::QualifiedName> {
        let mut out = Vec::new();
        reached_into(self, q, &mut out);
        out
    }

    /// Removes the document at `d` when it declares nothing.
    pub fn remove_document_if_empty(&mut self, d: usize) {
        let doc = &self.docs[d];
        if doc.types.is_empty() && doc.traits.is_empty() && doc.impls.is_empty() {
            self.docs.remove(d);
            self.reindex();
        }
    }
}

impl TypeLookup for DocumentSet {
    fn find(&self, name: &QualifiedName) -> Option<&TypeDef> {
        self.slot(name.namespace.as_str(), name.name.as_str())
            .map(|s| self.at(s))
    }

    fn is_stale(&self, ty: &QualifiedName) -> bool {
        !self.stale.is_empty() && self.stale.contains(ty)
    }

    /// Resolves through the trait's declaring document, over the impls of
    /// every document: the declaring document's first, then the others' in
    /// order.
    fn implementor(
        &self,
        tr: &QualifiedName,
        value: &serde_json::Value,
    ) -> Result<(QualifiedName, String), String> {
        implementor_of(self, tr, value)
    }

    fn descendants_of(&self, ty: &QualifiedName) -> Vec<QualifiedName> {
        self.descendants(ty)
    }

    /// The impls of `tr` in every document: the trait's own document's
    /// first, then the others' in order.
    fn impls_of(&self, tr: &QualifiedName) -> Vec<&crate::ir::ImplDef> {
        let home = self.docs.iter().filter(|d| d.namespace == tr.namespace);
        let others = self.docs.iter().filter(|d| d.namespace != tr.namespace);
        home.chain(others)
            .flat_map(|d| d.impls.iter())
            .filter(|i| &i.r#trait == tr)
            .collect()
    }

    fn type_member(&self, tr: &QualifiedName) -> String {
        match self.namespace(&tr.namespace) {
            Some(d) => d.type_member(tr),
            None => "$type".to_string(),
        }
    }
}

fn reached_into(
    reg: &DocumentSet,
    q: &crate::name::QualifiedName,
    out: &mut Vec<crate::name::QualifiedName>,
) {
    use crate::ir::{FieldDef, FieldType, VariantShape};
    fn of_type(reg: &DocumentSet, t: &FieldType, out: &mut Vec<crate::name::QualifiedName>) {
        match t {
            FieldType::Named { name }
            | FieldType::MatrixOf { of: name }
            | FieldType::TableOf { of: name }
            | FieldType::Descendant { of: name } => reached_into(reg, name, out),
            FieldType::Matrix { element, .. }
            | FieldType::Array { element, .. }
            | FieldType::SparseMatrix { element, .. }
            | FieldType::Vec { element, .. } => of_type(reg, element, out),
            FieldType::Option { inner, .. } => of_type(reg, inner, out),
            FieldType::Hashmap { value, .. } | FieldType::OrderedMap { value, .. } => {
                of_type(reg, value, out)
            }
            FieldType::Table { columns, .. } => of_fields(reg, columns, out),
            _ => {}
        }
    }
    fn of_fields(
        reg: &DocumentSet,
        fields: &[FieldDef],
        out: &mut Vec<crate::name::QualifiedName>,
    ) {
        for f in fields {
            of_type(reg, &f.ty, out);
        }
    }
    if out.contains(q) {
        return;
    }
    let Some(def) = TypeLookup::find(reg, q) else {
        return;
    };
    // Marked before its fields are walked, so a recursive type terminates.
    out.push(q.clone());
    let mark = out.len() - 1;
    match def {
        TypeDef::Struct(s) => {
            if let Some(p) = &s.extends {
                reached_into(reg, p, out);
            }
            of_fields(reg, &s.fields, out)
        }
        TypeDef::Enum(e) => {
            for v in &e.variants {
                match &v.shape {
                    VariantShape::Struct { fields, .. } => of_fields(reg, fields, out),
                    VariantShape::Newtype { of } => reached_into(reg, of, out),
                    VariantShape::Unit => {}
                }
            }
        }
    }
    // The type follows the types it reaches.
    let me = out.remove(mark);
    out.push(me);
}
