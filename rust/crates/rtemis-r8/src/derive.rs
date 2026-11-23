//! Support for `#[derive(R8)]`: Rust types as R8 types.
//!
//! [`Encode`] gives a Rust type its R8 field type and its JSON form. It is
//! implemented for `bool`; `i8`, `i16`, `i32`, `u8`, and `u16` (integer);
//! `u32`, `f32`, `f64`, `i64`, and `u64` (double); `String`; `Vec<T>`;
//! `Option<T>`; `HashMap<String, T>` and `BTreeMap<String, T>`;
//! `serde_json::Value` (`type_json()`); and [`Bytes`] (`type_raw()`).
//! `usize` has no implementation, so a field of that type does not compile.
//! The derive implements [`Encode`] and [`R8`] for a struct or an enum.
//!
//! A derived type validates and encodes through the same checks as a type
//! declared in R: [`R8::to_document`] validates the encoded value, rules
//! included, and writes a document; [`R8::from_document`] reads and validates
//! a document, then decodes it. A struct extends another by embedding it
//! ([`Extensible`], [`Subtype`]).

use std::collections::{BTreeMap, HashMap};

use serde_json::Value;

use crate::canonical::{read_document, to_document};
use crate::ir::{FieldType, InheritedChange, StructDef, TypeDef};
use crate::name::{Name, QualifiedName};
use crate::rules::DocumentSet;
use crate::validate::ValueError;

#[doc(hidden)]
pub use serde_json;

/// A Rust type with an R8 field type and a JSON form.
pub trait Encode: Sized {
    /// The R8 field type of the Rust type.
    fn field_type() -> FieldType;
    /// The JSON form of a value; an `Err` for a value outside the R8 domain,
    /// such as `i32::MIN`, which R reads as `NA`.
    fn encode(&self) -> Result<Value, String>;
    /// A value from its JSON form, which validation has accepted.
    fn decode(v: &Value) -> Result<Self, String>;
    /// Adds the declarations of the named types this type reaches.
    fn register(_set: &mut DocumentSet) {}
}

/// A struct or enum declared in Rust with `#[derive(R8)]`.
pub trait R8: Encode {
    /// The qualified name, `namespace::Type`.
    const NAME: &'static str;

    /// The type's declaration.
    fn type_def() -> TypeDef;

    /// The qualified name, parsed.
    fn qualified_name() -> QualifiedName {
        QualifiedName::parse(Self::NAME).expect("the derive checks the name")
    }

    /// The declarations of the type and every named type it reaches.
    fn declarations() -> DocumentSet {
        let mut set = DocumentSet::default();
        <Self as Encode>::register(&mut set);
        set
    }

    /// Validates the value and writes it as a document.
    fn to_document(&self) -> Result<String, Vec<ValueError>> {
        let v = self.encode().map_err(|e| vec![encode_error(e)])?;
        to_document(&v, &Self::qualified_name(), &Self::declarations())
    }

    /// Reads and validates a document, then decodes it.
    fn from_document(text: &str) -> Result<Self, Vec<ValueError>> {
        let v = read_document(text, &Self::qualified_name(), &Self::declarations())?;
        Self::decode(&v).map_err(|e| vec![encode_error(e)])
    }
}

/// A subtype of the struct `P`: `P` itself, or a struct that extends a
/// subtype of `P`.
///
/// The derive implements it for a struct of `#[r8(extensible)]`, as a
/// subtype of itself, and for a struct with a field of `#[r8(extends)]`, as
/// a subtype of each type that the field's type is a subtype of, whichever
/// crates declare them. The interface trait `PLike` of an extensible struct
/// `P` is implemented for every `Subtype<P>`.
#[diagnostic::on_unimplemented(
    message = "`{Self}` is not a subtype of `{P}`",
    note = "a struct extends another with a field of `#[r8(extends)]`, whose type is declared with `#[r8(extensible)]`"
)]
pub trait Subtype<P> {
    /// The value of `P` that the value holds: the value itself, or its
    /// embedded ancestor.
    fn upcast(&self) -> &P;
}

/// A struct that other structs extend, declared with `#[r8(extensible)]`.
/// Its `Clone` gives the parent value of a child's value.
///
/// A child embeds its parent as its first field, marked `#[r8(extends)]`.
/// The derive writes the interface trait of an extensible struct, named with
/// `Like` after the struct: an accessor per field, and the struct's value.
///
#[cfg_attr(
    feature = "macros",
    doc = r##"
```
use rtemis_r8::R8;
use rtemis_r8::derive::{R8 as _, Subtype};

/// A numeric range.
#[derive(R8, Clone)]
#[r8(namespace = "demo", version = 1, extensible)]
pub struct Range {
    pub lo: f64,
    pub hi: f64,
}

/// A labeled range, whose `lo` is 0 by default.
#[derive(R8, Clone)]
#[r8(namespace = "demo", version = 1)]
pub struct Window {
    #[r8(extends, inherited(lo(default = "0")))]
    pub base: Range,
    pub label: String,
}

// A function for `Range` and every struct that extends it.
fn width(x: &impl RangeLike) -> f64 {
    x.hi() - x.lo()
}

let w = Window::from_document(
    r#"{"$type": "demo::Window", "$types": {"demo::Window": 1}, "hi": 4, "label": "w"}"#,
)
.unwrap();
assert_eq!(width(&w), 4.0);
let range: &Range = w.upcast();
assert_eq!(range.hi, w.to_range().hi);
```
"##
)]
#[diagnostic::on_unimplemented(
    message = "`{Self}` is not extensible",
    label = "the parent of `#[r8(extends)]`",
    note = "a struct that others extend is declared with `#[r8(extensible)]`"
)]
pub trait Extensible: R8 + Clone + Subtype<Self> {}

/// Declarations that the derive refuses when the crate compiles.
///
/// A parent that is not extensible:
///
/// ```compile_fail,E0277
/// use rtemis_r8::R8;
/// #[derive(R8, Clone)]
/// #[r8(namespace = "demo", version = 1)]
/// struct Range { lo: f64 }
/// #[derive(R8)]
/// #[r8(namespace = "demo", version = 1)]
/// struct Window { #[r8(extends)] base: Range, label: String }
/// ```
///
/// An extensible struct without `Clone`:
///
/// ```compile_fail,E0277
/// use rtemis_r8::R8;
/// #[derive(R8)]
/// #[r8(namespace = "demo", version = 1, extensible)]
/// struct Range { lo: f64 }
/// ```
///
/// A parent that is not the first field:
///
/// ```compile_fail
/// use rtemis_r8::R8;
/// #[derive(R8, Clone)]
/// #[r8(namespace = "demo", version = 1, extensible)]
/// struct Range { lo: f64 }
/// #[derive(R8)]
/// #[r8(namespace = "demo", version = 1)]
/// struct Window { label: String, #[r8(extends)] base: Range }
/// ```
///
/// A constraint beside `extends`, and a change that changes nothing:
///
/// ```compile_fail
/// use rtemis_r8::R8;
/// #[derive(R8, Clone)]
/// #[r8(namespace = "demo", version = 1, extensible)]
/// struct Range { lo: f64 }
/// #[derive(R8)]
/// #[r8(namespace = "demo", version = 1)]
/// struct Window { #[r8(extends, min = 0)] base: Range }
/// ```
///
/// ```compile_fail
/// use rtemis_r8::R8;
/// #[derive(R8, Clone)]
/// #[r8(namespace = "demo", version = 1, extensible)]
/// struct Range { lo: f64 }
/// #[derive(R8)]
/// #[r8(namespace = "demo", version = 1)]
/// struct Window { #[r8(extends, inherited(lo()))] base: Range }
/// ```
///
/// A field named as a method of the values that extend the struct:
///
/// ```compile_fail
/// use rtemis_r8::R8;
/// #[derive(R8, Clone)]
/// #[r8(namespace = "demo", version = 1, extensible)]
/// struct Range { to_range: f64 }
/// ```
///
/// `extensible` on an enum, and `extends` in a variant:
///
/// ```compile_fail
/// use rtemis_r8::R8;
/// #[derive(R8, Clone)]
/// #[r8(namespace = "demo", version = 1, extensible)]
/// enum Kernel { Linear, Rbf { gamma: f64 } }
/// ```
///
/// ```compile_fail
/// use rtemis_r8::R8;
/// #[derive(R8, Clone)]
/// #[r8(namespace = "demo", version = 1, extensible)]
/// struct Range { lo: f64 }
/// #[derive(R8)]
/// #[r8(namespace = "demo", version = 1)]
/// enum Figure { Flat { #[r8(extends)] base: Range, n: i32 } }
/// ```
#[cfg(all(doctest, feature = "macros"))]
struct RefusedByTheDerive;

/// The declaration of `child`, which holds the fields and rules it adds,
/// lowered onto its parent `P` with the changes to inherited fields.
///
/// # Panics
///
/// When the declaration cannot extend `P`, such as a field of a name that
/// `P` holds; the message lists each problem.
#[doc(hidden)]
pub fn extended<P: Extensible>(mut child: StructDef, changes: &[InheritedChange]) -> TypeDef {
    let parent = P::qualified_name();
    let TypeDef::Struct(def) = P::type_def() else {
        unreachable!("the derive implements `Extensible` for structs only");
    };
    if let Err(errors) = crate::ir::extend(&mut child, &parent, &def, changes) {
        let lines: Vec<String> = errors
            .iter()
            .map(|e| format!("{} [{}]", e.message, e.code))
            .collect();
        panic!(
            "`{}` does not extend `{parent}`: {}",
            child.name,
            lines.join("; ")
        );
    }
    TypeDef::Struct(child)
}

/// The members of the JSON form of an embedded parent, which the child's
/// object continues.
#[doc(hidden)]
pub fn inherited_members<P: Extensible>(
    base: &P,
) -> Result<serde_json::Map<String, Value>, String> {
    match base.encode()? {
        Value::Object(m) => Ok(m),
        _ => Err(format!("`{}` is not an object", P::NAME)),
    }
}

fn encode_error(message: String) -> ValueError {
    ValueError {
        code: "encode_failed",
        path: String::new(),
        rule: None,
        rule_index: None,
        message,
        diagnosis: None,
        ..Default::default()
    }
}

/// Bytes, `type_raw()`, written as canonical padded base64.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Bytes(pub Vec<u8>);

const B64: &[u8; 64] = b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

impl Encode for Bytes {
    fn field_type() -> FieldType {
        FieldType::Raw
    }
    fn encode(&self) -> Result<Value, String> {
        let mut out = String::new();
        for chunk in self.0.chunks(3) {
            let b = [
                chunk[0],
                *chunk.get(1).unwrap_or(&0),
                *chunk.get(2).unwrap_or(&0),
            ];
            let n = (u32::from(b[0]) << 16) | (u32::from(b[1]) << 8) | u32::from(b[2]);
            for i in 0..4 {
                if i <= chunk.len() {
                    out.push(B64[((n >> (18 - 6 * i)) & 63) as usize] as char);
                } else {
                    out.push('=');
                }
            }
        }
        Ok(Value::from(out))
    }
    fn decode(v: &Value) -> Result<Self, String> {
        let s = v.as_str().ok_or("bytes are a base64 string")?;
        let val = |c: u8| B64.iter().position(|&x| x == c).map(|p| p as u32);
        let mut out = Vec::new();
        for chunk in s.as_bytes().chunks(4) {
            let mut n = 0u32;
            let mut len: usize = 0;
            for (i, &c) in chunk.iter().enumerate() {
                if c != b'=' {
                    n |= val(c).ok_or("invalid base64")? << (18 - 6 * i);
                    len += 1;
                }
            }
            for i in 0..len.saturating_sub(1) {
                out.push((n >> (16 - 8 * i)) as u8);
            }
        }
        Ok(Bytes(out))
    }
}

macro_rules! integer {
    ($($t:ty),*) => {$(
        impl Encode for $t {
            fn field_type() -> FieldType {
                FieldType::Integer(Default::default())
            }
            fn encode(&self) -> Result<Value, String> {
                let x = i64::from(*self);
                if x == i64::from(i32::MIN) {
                    return Err("-2147483648 is R's NA_integer_ and has no R8 form".to_string());
                }
                Ok(Value::from(x))
            }
            fn decode(v: &Value) -> Result<Self, String> {
                let x = v.as_f64().ok_or("an integer")?;
                <$t>::try_from(x as i64).map_err(|e| e.to_string())
            }
        }
    )*};
}
integer!(i8, i16, i32, u8, u16);

/// The largest integer a double holds exactly, 2^53 - 1.
const EXACT: i64 = 9_007_199_254_740_991;

macro_rules! double {
    ($($t:ty => $to:expr, $from:expr);*) => {$(
        impl Encode for $t {
            fn field_type() -> FieldType {
                FieldType::Double(Default::default())
            }
            fn encode(&self) -> Result<Value, String> {
                let f: fn(&$t) -> Result<f64, String> = $to;
                let x = f(self)?;
                serde_json::Number::from_f64(x)
                    .map(Value::Number)
                    .ok_or_else(|| format!("{x} is not finite"))
            }
            fn decode(v: &Value) -> Result<Self, String> {
                let x = v.as_f64().ok_or("a number")?;
                let f: fn(f64) -> Result<$t, String> = $from;
                f(x)
            }
        }
    )*};
}
double!(
    f64 => |x| Ok(if *x == 0.0 { 0.0 } else { *x }), Ok;
    f32 => |x| Ok(if *x == 0.0 { 0.0 } else { f64::from(*x) }), |x| Ok(x as f32);
    u32 => |x| Ok(f64::from(*x)), |x| u32::try_from(x as i64).map_err(|e| e.to_string());
    i64 => |x| if x.abs() <= EXACT { Ok(*x as f64) } else { Err(format!("{x} is beyond 2^53 - 1, the range a double holds exactly")) },
           |x| Ok(x as i64);
    u64 => |x| if *x <= EXACT as u64 { Ok(*x as f64) } else { Err(format!("{x} is beyond 2^53 - 1, the range a double holds exactly")) },
           |x| Ok(x as u64)
);

impl Encode for bool {
    fn field_type() -> FieldType {
        FieldType::Logical
    }
    fn encode(&self) -> Result<Value, String> {
        Ok(Value::Bool(*self))
    }
    fn decode(v: &Value) -> Result<Self, String> {
        v.as_bool().ok_or_else(|| "a logical".to_string())
    }
}

impl Encode for String {
    fn field_type() -> FieldType {
        FieldType::Character(Default::default())
    }
    fn encode(&self) -> Result<Value, String> {
        if self.contains('\0') {
            return Err("a string holds U+0000, which R cannot represent".to_string());
        }
        Ok(Value::from(self.as_str()))
    }
    fn decode(v: &Value) -> Result<Self, String> {
        v.as_str()
            .map(str::to_string)
            .ok_or_else(|| "a string".to_string())
    }
}

impl Encode for Value {
    fn field_type() -> FieldType {
        FieldType::Json
    }
    fn encode(&self) -> Result<Value, String> {
        Ok(self.clone())
    }
    fn decode(v: &Value) -> Result<Self, String> {
        Ok(v.clone())
    }
}

impl<T: Encode> Encode for Vec<T> {
    fn field_type() -> FieldType {
        FieldType::Vec {
            element: Box::new(T::field_type()),
            min_length: None,
            max_length: None,
            unique: false,
        }
    }
    fn encode(&self) -> Result<Value, String> {
        self.iter()
            .map(Encode::encode)
            .collect::<Result<Vec<_>, _>>()
            .map(Value::Array)
    }
    fn decode(v: &Value) -> Result<Self, String> {
        v.as_array()
            .ok_or("an array")?
            .iter()
            .map(T::decode)
            .collect()
    }
    fn register(set: &mut DocumentSet) {
        T::register(set);
    }
}

impl<T: Encode> Encode for Option<T> {
    fn field_type() -> FieldType {
        FieldType::Option {
            inner: Box::new(T::field_type()),
        }
    }
    fn encode(&self) -> Result<Value, String> {
        match self {
            Some(x) => x.encode(),
            None => Ok(Value::Null),
        }
    }
    fn decode(v: &Value) -> Result<Self, String> {
        if v.is_null() {
            Ok(None)
        } else {
            T::decode(v).map(Some)
        }
    }
    fn register(set: &mut DocumentSet) {
        T::register(set);
    }
}

macro_rules! map {
    ($($m:ident),*) => {$(
        impl<T: Encode> Encode for $m<String, T> {
            fn field_type() -> FieldType {
                FieldType::BtreeMap { value: Box::new(T::field_type()), keys: Default::default() }
            }
            fn encode(&self) -> Result<Value, String> {
                let mut m = serde_json::Map::new();
                let mut keys: Vec<&String> = self.keys().collect();
                keys.sort();
                for k in keys {
                    m.insert(k.clone(), self[k].encode()?);
                }
                Ok(Value::Object(m))
            }
            fn decode(v: &Value) -> Result<Self, String> {
                v.as_object()
                    .ok_or("an object")?
                    .iter()
                    .map(|(k, x)| T::decode(x).map(|t| (k.clone(), t)))
                    .collect()
            }
            fn register(set: &mut DocumentSet) {
                T::register(set);
            }
        }
    )*};
}
map!(HashMap, BTreeMap);

/// The constraints of a derived field's attributes.
#[doc(hidden)]
#[derive(Debug, Default, Clone, Copy)]
pub struct Constraints {
    pub min: Option<f64>,
    pub max: Option<f64>,
    pub exclusive_min: Option<f64>,
    pub exclusive_max: Option<f64>,
    pub pattern: Option<&'static str>,
    pub finite: Option<bool>,
    pub min_length: Option<u64>,
    pub max_length: Option<u64>,
    pub unique: bool,
}

/// An integer bound of a constraint: a whole number in the integer range.
fn int_bound(x: Option<f64>, what: &str) -> Result<Option<i32>, String> {
    x.map(|v| {
        if v.fract() == 0.0 && (-2_147_483_647.0..=2_147_483_647.0).contains(&v) {
            Ok(v as i32)
        } else {
            Err(format!(
                "`{what}` of an integer field is a whole number in the integer range"
            ))
        }
    })
    .transpose()
}

/// The bound of a side from an inclusive and an exclusive value.
fn bound<T: PartialOrd + Copy>(
    inclusive: Option<T>,
    exclusive: Option<T>,
    names: (&str, &str),
) -> Result<Option<crate::ir::Bound<T>>, String> {
    use crate::ir::Bound;
    match (inclusive, exclusive) {
        (Some(_), Some(_)) => Err(format!(
            "`{}` and `{}` bound the same side",
            names.0, names.1
        )),
        (Some(v), None) => Ok(Some(Bound::inclusive(v))),
        (None, Some(v)) => Ok(Some(Bound::exclusive(v))),
        (None, None) => Ok(None),
    }
}

/// Applies a derived field's constraints to its field type: `min_length`,
/// `max_length`, and `unique` on the outermost `vec()`, every other
/// constraint on the innermost scalar, through `option()` and `vec()`, and
/// checks that a field with a fill policy (`filled`) is `option()`; a
/// constraint that the scalar's type does not take is an error.
#[doc(hidden)]
pub fn constrain(mut ft: FieldType, c: Constraints, filled: bool) -> Result<FieldType, String> {
    fn outer_vec(ft: &mut FieldType) -> Option<&mut FieldType> {
        match ft {
            FieldType::Vec { .. } => Some(ft),
            FieldType::Option { inner, .. } => outer_vec(inner),
            _ => None,
        }
    }
    fn scalar(ft: &mut FieldType) -> &mut FieldType {
        match ft {
            FieldType::Option { inner, .. } => scalar(inner),
            FieldType::Vec { element, .. } => scalar(element),
            other => other,
        }
    }
    if c.min_length.is_some() || c.max_length.is_some() || c.unique {
        match outer_vec(&mut ft) {
            Some(FieldType::Vec {
                min_length,
                max_length,
                unique,
                ..
            }) => {
                *min_length = c.min_length.or(*min_length);
                *max_length = c.max_length.or(*max_length);
                *unique |= c.unique;
            }
            _ => return Err("min_length, max_length, and unique apply to a Vec field".to_string()),
        }
    }
    let numeric = c.min.is_some()
        || c.max.is_some()
        || c.exclusive_min.is_some()
        || c.exclusive_max.is_some();
    match scalar(&mut ft) {
        FieldType::Double(d) => {
            if c.pattern.is_some() {
                return Err("`pattern` applies to a String field".to_string());
            }
            if c.min.is_some() || c.exclusive_min.is_some() {
                d.lower = bound(c.min, c.exclusive_min, ("min", "exclusive_min"))?;
            }
            if c.max.is_some() || c.exclusive_max.is_some() {
                d.upper = bound(c.max, c.exclusive_max, ("max", "exclusive_max"))?;
            }
            if let Some(f) = c.finite {
                d.finite = f;
            }
        }
        FieldType::Integer(i) => {
            if c.pattern.is_some() || c.finite.is_some() {
                return Err("`pattern` and `finite` do not apply to an integer field".to_string());
            }
            if c.min.is_some() || c.exclusive_min.is_some() {
                i.lower = bound(
                    int_bound(c.min, "min")?,
                    int_bound(c.exclusive_min, "exclusive_min")?,
                    ("min", "exclusive_min"),
                )?;
            }
            if c.max.is_some() || c.exclusive_max.is_some() {
                i.upper = bound(
                    int_bound(c.max, "max")?,
                    int_bound(c.exclusive_max, "exclusive_max")?,
                    ("max", "exclusive_max"),
                )?;
            }
        }
        FieldType::Character(ch) => {
            if numeric || c.finite.is_some() {
                return Err("numeric constraints do not apply to a String field".to_string());
            }
            if let Some(p) = c.pattern {
                ch.pattern = Some(p.into());
            }
        }
        _ => {
            if numeric || c.finite.is_some() || c.pattern.is_some() {
                return Err("value constraints apply to a number or a String field".to_string());
            }
        }
    }
    if filled && !matches!(ft, FieldType::Option { .. }) {
        return Err("context applies to an Option field".to_string());
    }
    Ok(ft)
}

/// Adds a declaration to the document of its namespace, once. Returns
/// `true` when the declaration is new, so a recursive type registers its
/// fields once.
#[doc(hidden)]
pub fn register_def(set: &mut DocumentSet, name: &str, def: impl FnOnce() -> TypeDef) -> bool {
    let q = QualifiedName::parse(name).expect("the derive checks the name");
    if crate::rules::TypeLookup::find(&*set, &q).is_some() {
        return false;
    }
    set.put_type(&q.namespace, def());
    true
}

/// The named field type of a derived type.
#[doc(hidden)]
pub fn named(name: &str) -> FieldType {
    FieldType::Named {
        name: QualifiedName::parse(name).expect("the derive checks the name"),
    }
}

/// A name, checked.
#[doc(hidden)]
pub fn name(s: &str) -> Name {
    Name::new(s).expect("the derive checks names")
}
