//! Open fields of derived types: [`SubtypeOf`], a field of any struct that
//! extends another, `subtype(P)`, and [`ImplementorOf`], a field of any
//! implementor of a trait, `dyn(Trait)`. A holder keeps the whole value in its
//! JSON form, of a type that the crate does not know too.
//!
//! A holder's JSON form names its type in `"$type"`. A document names an
//! implementor of a trait with a tag member by its tag value; [`name_types`]
//! and [`name_tags`] convert between the two where a document is read and
//! written.
//! spec: R8/class-system#types-declared-in-rust

use std::fmt;
use std::marker::PhantomData;

use serde_json::{Map, Value};

use crate::derive::{Encode, Extensible, R8, Subtype, inherited_members};
use crate::ir::{EnumDef, FieldDef, FieldType, TypeDef, VariantKind};
use crate::name::QualifiedName;
use crate::rules::{DocumentSet, TypeLookup};

/// The member of a holder's JSON form that names its type.
const TYPE: &str = "$type";

/// The type and the other members of a holder's JSON form.
fn split(v: &Value) -> Result<(QualifiedName, Map<String, Value>), String> {
    let mut members = v.as_object().ok_or("an object")?.clone();
    let name = match members.shift_remove(TYPE) {
        Some(Value::String(s)) => QualifiedName::parse(&s).map_err(|e| e.to_string())?,
        _ => return Err("a value that names its type in `$type`".to_string()),
    };
    Ok((name, members))
}

/// The JSON form of a holder of a value of the type `name`.
fn join(name: &QualifiedName, members: Map<String, Value>) -> Value {
    let mut out = Map::new();
    out.insert(TYPE.to_string(), Value::from(name.to_string()));
    out.extend(members);
    Value::Object(out)
}

/// A field of `P` or of any struct that extends it, `subtype(P)`.
///
/// The holder keeps a value of a type that the crate does not know, and
/// encodes it unchanged. The value's fields that `P` declares are the value
/// of `P` ([`Subtype::upcast`]), so the holder implements the interface
/// traits of `P` and its ancestors: an accessor per field, `to_p()`, and
/// `with_p(value)`.
///
/// A document's declarations are those that `T::declarations()` reaches,
/// `P` among them. [`R8::to_document_in`] and [`R8::from_document_in`] take
/// the declarations of the structs that extend `P`.
#[cfg_attr(
    feature = "macros",
    doc = r##"

```
use rtemis_r8::R8;
use rtemis_r8::derive::{Encode, R8 as _, SubtypeOf};

/// A numeric range.
#[derive(R8, Clone)]
#[r8(namespace = "demo", version = 1, extensible)]
pub struct Range {
    pub lo: f64,
    pub hi: f64,
}

/// A labeled range.
#[derive(R8, Clone)]
#[r8(namespace = "demo", version = 1)]
pub struct Window {
    #[r8(extends)]
    pub base: Range,
    pub label: String,
}

/// An axis over any range.
#[derive(R8, Clone)]
#[r8(namespace = "demo", version = 1)]
pub struct Axis {
    pub range: SubtypeOf<Range>,
}

let window = Window { base: Range { lo: 0.0, hi: 4.0 }, label: "w".into() };
let axis = Axis { range: SubtypeOf::new(&window)? };

// The fields of `Range`, and the value of a type that the crate knows.
assert_eq!(*axis.range.hi(), 4.0);
let held: Option<Window> = axis.range.downcast()?;
assert_eq!(held.unwrap().label, "w");

// The declarations of `Axis`, and that of `Window`, which extends `Range`.
let mut types = Axis::declarations();
Window::register(&mut types);
let text = axis.to_document_in(&types).unwrap();
let read = Axis::from_document_in(&text, &types).unwrap();
assert_eq!(read.range.type_name().to_string(), "demo::Window");
# Ok::<(), String>(())
```
"##
)]
#[derive(Debug, Clone, PartialEq)]
pub struct SubtypeOf<P: Extensible> {
    name: QualifiedName,
    base: P,
    /// The members of the value that `P` does not declare.
    rest: Map<String, Value>,
}

impl<P: Extensible> SubtypeOf<P> {
    /// The holder of `value`; an `Err` for a value outside the R8 domain
    /// ([`Encode::encode`]).
    pub fn new<T: R8 + Subtype<P>>(value: &T) -> Result<Self, String> {
        match value.encode()? {
            Value::Object(members) => {
                Self::of(T::qualified_name(), value.upcast().clone(), members)
            }
            _ => Err(format!("`{}` is not an object", T::NAME)),
        }
    }

    fn of(name: QualifiedName, base: P, mut members: Map<String, Value>) -> Result<Self, String> {
        let inherited = inherited_members(&base)?;
        members.retain(|k, _| !inherited.contains_key(k));
        Ok(SubtypeOf {
            name,
            base,
            rest: members,
        })
    }

    /// The members of the value's JSON form.
    fn members(&self) -> Result<Map<String, Value>, String> {
        let mut out = inherited_members(&self.base)?;
        out.extend(self.rest.clone());
        Ok(out)
    }

    /// The qualified name of the value's type.
    pub fn type_name(&self) -> &QualifiedName {
        &self.name
    }

    /// The value as a `T`, when its type is `T` exactly; an `Err` for a value
    /// that `T` does not decode.
    pub fn downcast<T: R8 + Subtype<P>>(&self) -> Result<Option<T>, String> {
        if self.name.to_string() != T::NAME {
            return Ok(None);
        }
        T::decode(&Value::Object(self.members()?)).map(Some)
    }
}

impl<P: Extensible + Subtype<Q>, Q> Subtype<Q> for SubtypeOf<P> {
    fn upcast(&self) -> &Q {
        self.base.upcast()
    }
    fn with_upcast(&self, value: Q) -> Self {
        SubtypeOf {
            name: self.name.clone(),
            base: self.base.with_upcast(value),
            rest: self.rest.clone(),
        }
    }
}

impl<P: Extensible> Encode for SubtypeOf<P> {
    fn field_type() -> FieldType {
        FieldType::Subtype {
            of: P::qualified_name(),
        }
    }
    fn encode(&self) -> Result<Value, String> {
        Ok(join(&self.name, self.members()?))
    }
    fn decode(v: &Value) -> Result<Self, String> {
        let (name, members) = split(v)?;
        Self::of(name, P::decode(v)?, members)
    }
    fn register(set: &mut DocumentSet) {
        P::register(set);
    }
}

/// A Rust trait that stands for an R8 trait in the type of an open field,
/// `dyn Shape`. `#[r8::methods(trait = "ns::Shape", dyn)]` of
/// `rtemis-r8-api` implements it; a crate without it writes the name of a
/// trait that another one declares:
///
/// ```
/// pub trait Shape {}
///
/// impl rtemis_r8::derive::TraitObject for dyn Shape {
///     const NAME: &'static str = "demo::Shape";
/// }
/// ```
#[diagnostic::on_unimplemented(
    message = "`{Self}` does not name an R8 trait",
    note = "`#[r8::methods(trait = \"ns::Trait\", dyn)]` on the trait, or `impl rtemis_r8::derive::TraitObject for dyn Trait`, names one"
)]
pub trait TraitObject {
    /// The trait's qualified name, `namespace::Trait`.
    const NAME: &'static str;
}

/// The qualified name of the R8 trait that `D` names.
///
/// # Panics
///
/// When the name is not a qualified name.
pub(crate) fn trait_name<D: TraitObject + ?Sized>() -> QualifiedName {
    QualifiedName::parse(D::NAME).unwrap_or_else(|e| panic!("the trait `{}`: {e}", D::NAME))
}

/// The field type `dyn(Trait)` of the trait that `D`, a trait object, names.
#[doc(hidden)]
pub fn implementor_type<D: TraitObject + ?Sized>() -> FieldType {
    FieldType::Dyn {
        r#trait: trait_name::<D>(),
    }
}

/// A field of any implementor of a trait, `dyn(Trait)`, written
/// `ImplementorOf<dyn Shape>` ([`TraitObject`]).
///
/// The holder keeps a value of a type that the crate does not know, and
/// encodes it unchanged. A derived type's declarations hold no trait and no
/// impl, so a document is written and read with declarations that hold them
/// ([`R8::to_document_in`], [`R8::from_document_in`]), which decide whether the
/// value's type implements the trait.
pub struct ImplementorOf<D: TraitObject + ?Sized> {
    name: QualifiedName,
    members: Map<String, Value>,
    of: PhantomData<fn() -> Box<D>>,
}

/// The declaration of `T`, when it is an enum whose variants are all units,
/// whose JSON form is a string.
fn unit_only<T: R8>() -> Option<EnumDef> {
    match T::type_def() {
        TypeDef::Enum(e)
            if e.variants
                .iter()
                .all(|v| matches!(v.kind, VariantKind::Unit)) =>
        {
            Some(e)
        }
        _ => None,
    }
}

impl<D: TraitObject + ?Sized> ImplementorOf<D> {
    /// The holder of `value`; an `Err` for a value outside the R8 domain
    /// ([`Encode::encode`]).
    pub fn new<T: R8>(value: &T) -> Result<Self, String> {
        let members = match (value.encode()?, unit_only::<T>()) {
            (Value::Object(members), _) => members,
            // A unit-only enum is an object of its tag here.
            (Value::String(variant), Some(e)) => [(e.tag.to_string(), Value::String(variant))]
                .into_iter()
                .collect(),
            _ => return Err(format!("`{}` is not an object", T::NAME)),
        };
        Ok(Self::of(T::qualified_name(), members))
    }

    fn of(name: QualifiedName, members: Map<String, Value>) -> Self {
        ImplementorOf {
            name,
            members,
            of: PhantomData,
        }
    }

    /// The qualified name of the value's type.
    pub fn type_name(&self) -> &QualifiedName {
        &self.name
    }

    /// The value as a `T`, when its type is `T`; an `Err` for a value that
    /// `T` does not decode.
    pub fn downcast<T: R8>(&self) -> Result<Option<T>, String> {
        if self.name.to_string() != T::NAME {
            return Ok(None);
        }
        let v = match unit_only::<T>() {
            Some(e) => self
                .members
                .get(e.tag.as_str())
                .cloned()
                .unwrap_or(Value::Null),
            None => Value::Object(self.members.clone()),
        };
        T::decode(&v).map(Some)
    }
}

impl<D: TraitObject + ?Sized> Clone for ImplementorOf<D> {
    fn clone(&self) -> Self {
        ImplementorOf {
            name: self.name.clone(),
            members: self.members.clone(),
            of: PhantomData,
        }
    }
}

impl<D: TraitObject + ?Sized> PartialEq for ImplementorOf<D> {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.members == other.members
    }
}

impl<D: TraitObject + ?Sized> fmt::Debug for ImplementorOf<D> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("ImplementorOf")
            .field("name", &self.name)
            .field("members", &self.members)
            .finish()
    }
}

impl<D: TraitObject + ?Sized> Encode for ImplementorOf<D> {
    fn field_type() -> FieldType {
        implementor_type::<D>()
    }
    fn encode(&self) -> Result<Value, String> {
        Ok(join(&self.name, self.members.clone()))
    }
    fn decode(v: &Value) -> Result<Self, String> {
        split(v).map(|(name, members)| Self::of(name, members))
    }
}

/// The member that names the type of an implementor, in the two forms.
#[derive(Clone, Copy)]
enum Form {
    /// `"$type"`, as a holder names it.
    Types,
    /// The trait's tag member, as a document names it.
    Tags,
}

/// Replaces the tag member of each value of a trait with one in `v`, a valid
/// value of the type `ty`, by `"$type"`.
pub(crate) fn name_types(v: &mut Value, ty: &QualifiedName, types: &dyn TypeLookup) {
    of_type(v, ty, types, Form::Types);
}

/// Replaces `"$type"` of each value of a trait with a tag member in `v`, a
/// value of the type `ty`, by that member.
pub(crate) fn name_tags(v: &mut Value, ty: &QualifiedName, types: &dyn TypeLookup) {
    of_type(v, ty, types, Form::Tags);
}

fn of_type(v: &mut Value, ty: &QualifiedName, types: &dyn TypeLookup, form: Form) {
    let fields: &[FieldDef] = match types.find(ty) {
        Some(TypeDef::Struct(s)) => &s.fields,
        Some(TypeDef::Enum(e)) => {
            let kind = v
                .get(e.tag.as_str())
                .and_then(Value::as_str)
                .and_then(|t| e.variants.iter().find(|x| x.name.as_str() == t))
                .map(|x| &x.kind);
            match kind {
                Some(VariantKind::Struct { fields, .. }) => fields,
                Some(VariantKind::Newtype { of }) => match types.find(of) {
                    Some(TypeDef::Struct(s)) => &s.fields,
                    _ => return,
                },
                _ => return,
            }
        }
        None => return,
    };
    for f in fields {
        if let Some(x) = v.get_mut(f.name.as_str()) {
            of_field(x, &f.ty, types, form);
        }
    }
}

fn of_field(v: &mut Value, ft: &FieldType, types: &dyn TypeLookup, form: Form) {
    let each = |items: &mut dyn Iterator<Item = &mut Value>, ft: &FieldType| {
        for x in items {
            of_field(x, ft, types, form);
        }
    };
    match ft {
        FieldType::Option { inner } => of_field(v, inner, types, form),
        FieldType::Vec { element, .. } if holds_types(element) => {
            if let Some(a) = v.as_array_mut() {
                each(&mut a.iter_mut(), element);
            }
        }
        FieldType::BtreeMap { value, .. } if holds_types(value) => {
            if let Some(m) = v.as_object_mut() {
                each(&mut m.values_mut(), value);
            }
        }
        FieldType::IndexMap { value, .. } if holds_types(value) => {
            if let Some(a) = v.as_array_mut() {
                each(&mut a.iter_mut().filter_map(|p| p.get_mut("value")), value);
            }
        }
        FieldType::Named { name } => of_type(v, name, types, form),
        FieldType::Subtype { .. } => {
            if let Some(ty) = type_of(v) {
                of_type(v, &ty, types, form);
            }
        }
        FieldType::Dyn { r#trait } => implementor(v, r#trait, types, form),
        _ => {}
    }
}

/// Whether values of `ft` may hold a value of a named type.
fn holds_types(ft: &FieldType) -> bool {
    match ft {
        FieldType::Option { inner } => holds_types(inner),
        FieldType::Vec { element, .. } => holds_types(element),
        FieldType::BtreeMap { value, .. } | FieldType::IndexMap { value, .. } => holds_types(value),
        FieldType::Named { .. } | FieldType::Subtype { .. } | FieldType::Dyn { .. } => true,
        _ => false,
    }
}

fn type_of(v: &Value) -> Option<QualifiedName> {
    QualifiedName::parse(v.get(TYPE)?.as_str()?).ok()
}

fn implementor(v: &mut Value, tr: &QualifiedName, types: &dyn TypeLookup, form: Form) {
    let (ty, from, to, written) = match form {
        Form::Types => match types.implementor(tr, v) {
            Ok((ty, member)) => {
                let written = ty.to_string();
                (ty, member, TYPE.to_string(), written)
            }
            Err(_) => return,
        },
        Form::Tags => match type_of(v) {
            Some(ty) => {
                let written = types.tag_value(tr, &ty).unwrap_or_else(|| ty.to_string());
                (ty, TYPE.to_string(), types.type_member(tr), written)
            }
            None => return,
        },
    };
    of_type(v, &ty, types, form);
    if from != to
        && let Some(m) = v.as_object_mut()
    {
        m.shift_remove(&from);
        m.insert(to, Value::from(written));
    }
}
