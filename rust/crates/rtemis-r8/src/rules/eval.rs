//! Evaluation of compiled expressions over values in any host form.
//!
//! The evaluator reads a value through [`Access`]. A `var` evaluates to a
//! reference into the value with its declared type, read as a [`Scalar`]
//! only where an operator needs its value: a number, a logical, a string, a
//! date, a date-time, or an array of them. `is.null()`, `length()`, `nrow()`,
//! `ncol()`, `dim()`, `levels()`, `all()`, and `any()` read the reference
//! directly, `in`, `non_increasing()`, and `non_decreasing()` scan a vector
//! in place, and `==` compares two struct or enum values with
//! [`crate::equal`]. A rule's literals, JSONLogic in the IR, become scalars
//! when the rule is evaluated.
//!
//! A `var` that reaches an absent value (a missing member or `null`) makes
//! the expression [`Absent`], and so does a comparison with `NaN`. The absence
//! propagates through every operator except `is_null()`, `is_na()`, and an
//! `and` or an `or` that another operand decides, and a rule with an absent
//! result holds. An operand that decides an `and` or an `or` decides it over
//! an error of another operand too. Conditional positions take logicals only.

use std::cmp::Ordering;

use serde_json::Value;

use super::compile::{Collection, Expr, Op, Root, Step, VariantTag};
use super::exact::ExactSum;
use super::ty::TypeLookup;
use crate::access::{Access, valid_date, valid_posixct};
use crate::canonical::{date_days, date_text, datetime_micros, datetime_text};
use crate::emit::number_text;
use crate::ir::{FieldDef, FieldType, TypeDef, VariantKind};

/// An expression read an absent value.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Absent;

/// A value of the rule dialect.
#[derive(Debug, Clone, PartialEq)]
pub enum Scalar {
    /// `null`, from a literal.
    Null,
    /// A logical.
    Bool(bool),
    /// A number; integers and doubles alike.
    Num(f64),
    /// A string.
    Str(String),
    /// Days since 1970-01-01.
    Date(i64),
    /// Microseconds since 1970-01-01T00:00:00Z.
    DateTime(i64),
    /// An array, from a vector, a factor, `levels()`, or `dim()`.
    Array(Vec<Scalar>),
}

impl Scalar {
    /// A value of the JSON host form without a declared type as a scalar.
    /// A string stays a string; comparison with a number reads a
    /// non-finite number's string form (`"NaN"`, `"Infinity"`,
    /// `"-Infinity"`) as that number.
    pub fn from_value(v: &Value) -> Scalar {
        match v {
            Value::Null => Scalar::Null,
            Value::Bool(b) => Scalar::Bool(*b),
            Value::Number(n) => Scalar::Num(n.as_f64().unwrap_or(f64::NAN)),
            Value::String(s) => Scalar::Str(s.clone()),
            Value::Array(a) => Scalar::Array(a.iter().map(Scalar::from_value).collect()),
            Value::Object(_) => Scalar::Null,
        }
    }

    /// The scalar's JSON form, for the JSON host form's callers.
    pub fn to_value(&self) -> Value {
        match self {
            Scalar::Null => Value::Null,
            Scalar::Bool(b) => Value::Bool(*b),
            Scalar::Num(x) => number_value(*x),
            Scalar::Str(s) => Value::from(s.as_str()),
            Scalar::Date(d) => Value::from(date_text(*d)),
            Scalar::DateTime(m) => Value::from(datetime_text(*m)),
            Scalar::Array(a) => Value::Array(a.iter().map(Scalar::to_value).collect()),
        }
    }

    /// The value as a rule's message shows it.
    pub fn shown(&self) -> String {
        match self {
            Scalar::Null => "null".to_string(),
            Scalar::Bool(b) => b.to_string(),
            Scalar::Num(x) => number_text(*x),
            Scalar::Str(s) => format!("\"{s}\""),
            Scalar::Date(d) => date_text(*d),
            Scalar::DateTime(m) => datetime_text(*m),
            Scalar::Array(a) => format!(
                "[{}]",
                a.iter().map(Scalar::shown).collect::<Vec<_>>().join(", ")
            ),
        }
    }

    /// A number, or the string form of a non-finite number.
    fn number(&self) -> Option<f64> {
        match self {
            Scalar::Num(x) => Some(*x),
            Scalar::Str(s) => as_number(&Value::from(s.as_str())),
            _ => None,
        }
    }

    fn is_num(&self) -> bool {
        matches!(self, Scalar::Num(_))
    }

    /// The days of a date, or of a date literal.
    fn days(&self) -> Option<i64> {
        match self {
            Scalar::Date(d) => Some(*d),
            Scalar::Str(s) if valid_date(s) => Some(date_days(s)),
            _ => None,
        }
    }

    /// The microseconds of a date-time, or of a date-time literal.
    fn micros(&self) -> Option<i64> {
        match self {
            Scalar::DateTime(m) => Some(*m),
            Scalar::Str(s) if valid_posixct(s) => Some(datetime_micros(s)),
            _ => None,
        }
    }
}

fn number_value(x: f64) -> Value {
    if x.is_finite() && x.fract() == 0.0 && x.abs() < 9.007_199_254_740_992e15 {
        Value::from(x as i64)
    } else if x.is_nan() {
        Value::from("NaN")
    } else if x.is_infinite() {
        Value::from(if x > 0.0 { "Infinity" } else { "-Infinity" })
    } else {
        serde_json::Number::from_f64(x).map_or(Value::Null, Value::Number)
    }
}

/// An evaluated expression: a scalar, or a reference into the host form
/// with its declared type, if known.
pub(crate) enum Val<'l, A> {
    V(Scalar),
    R(A, Option<&'l FieldType>),
}

/// The result of evaluating an expression: a value, or absence. An `Err` is
/// a failure that compilation did not exclude, such as a division by zero.
pub(crate) type Evaluated<'l, A> = Result<Result<Val<'l, A>, Absent>, String>;

/// An element's place in its collection, borrowed from the host form, so a
/// passing element allocates nothing.
pub enum Suffix<'s> {
    /// The element at an index of a vector.
    Index(usize),
    /// The value at a key of a `btree_map()`.
    Key(std::borrow::Cow<'s, str>),
    /// The value of the item at an index of an `index_map()`.
    Item(usize),
}

impl Suffix<'_> {
    /// The element's path below its collection.
    pub fn segments(&self) -> Vec<crate::problem::Segment> {
        use crate::problem::Segment;
        match self {
            Suffix::Index(i) => vec![Segment::Element(*i as u64)],
            Suffix::Key(k) => vec![Segment::Key(k.to_string())],
            Suffix::Item(i) => vec![
                Segment::Element(*i as u64),
                Segment::Form("/value".to_string()),
            ],
        }
    }
}

/// The type of a value whose declared type is not known.
pub(crate) static UNTYPED: FieldType = FieldType::Json;

/// A number: a JSON number, or a non-finite double in its string form.
pub(crate) fn as_number(v: &Value) -> Option<f64> {
    match v {
        Value::Number(n) => n.as_f64(),
        Value::String(s) => match s.as_str() {
            "Infinity" => Some(f64::INFINITY),
            "-Infinity" => Some(f64::NEG_INFINITY),
            "NaN" => Some(f64::NAN),
            _ => None,
        },
        _ => None,
    }
}

/// Strict equality of JSON values: numbers by value, everything else
/// structurally.
pub(crate) fn strict_eq(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Array(x), Value::Array(y)) => {
            x.len() == y.len() && x.iter().zip(y).all(|(p, q)| strict_eq(p, q))
        }
        _ => match (as_number(a), as_number(b)) {
            (Some(x), Some(y)) if a.is_number() || b.is_number() => x == y,
            _ => a == b,
        },
    }
}

/// Strict equality of scalars: numbers by value, with `NaN` equal to
/// itself; a date or date-time equal to the literal of the same instant;
/// arrays element by element.
fn scalar_eq(a: &Scalar, b: &Scalar) -> bool {
    match (a, b) {
        (Scalar::Num(_), _) | (_, Scalar::Num(_)) => match (a.number(), b.number()) {
            (Some(x), Some(y)) => x == y || (x.is_nan() && y.is_nan()),
            _ => false,
        },
        (Scalar::Array(x), Scalar::Array(y)) => {
            x.len() == y.len() && x.iter().zip(y).all(|(p, q)| scalar_eq(p, q))
        }
        (Scalar::Date(_), _) | (_, Scalar::Date(_)) => match (a.days(), b.days()) {
            (Some(x), Some(y)) => x == y,
            _ => false,
        },
        (Scalar::DateTime(_), _) | (_, Scalar::DateTime(_)) => match (a.micros(), b.micros()) {
            (Some(x), Some(y)) => x == y,
            _ => false,
        },
        _ => a == b,
    }
}

/// Whether a scalar is the number `NaN`.
fn is_nan(v: &Scalar) -> bool {
    matches!(v, Scalar::Num(x) if x.is_nan())
}

/// The order of two scalars: numbers, dates, date-times, and strings.
fn scalar_cmp(a: &Scalar, b: &Scalar) -> Result<Option<Ordering>, String> {
    if (a.is_num() || b.is_num())
        && let (Some(x), Some(y)) = (a.number(), b.number())
    {
        return Ok(x.partial_cmp(&y));
    }
    if (matches!(a, Scalar::Date(_)) || matches!(b, Scalar::Date(_)))
        && let (Some(x), Some(y)) = (a.days(), b.days())
    {
        return Ok(Some(x.cmp(&y)));
    }
    if (matches!(a, Scalar::DateTime(_)) || matches!(b, Scalar::DateTime(_)))
        && let (Some(x), Some(y)) = (a.micros(), b.micros())
    {
        return Ok(Some(x.cmp(&y)));
    }
    match (a, b) {
        (Scalar::Str(x), Scalar::Str(y)) => Ok(Some(x.cmp(y))),
        _ => Err(format!("cannot compare {} and {}", a.shown(), b.shown())),
    }
}

fn want_number(v: &Scalar, op: &str) -> Result<f64, String> {
    v.number()
        .ok_or_else(|| format!("`{op}` takes numbers, found {}", v.shown()))
}

fn want_bool(v: &Scalar, what: &str) -> Result<bool, String> {
    match v {
        Scalar::Bool(b) => Ok(*b),
        other => Err(format!(
            "{what} is a logical condition, found {}",
            other.shown()
        )),
    }
}

pub(crate) fn unwrap_option(ft: &FieldType) -> &FieldType {
    match ft {
        FieldType::Option { inner, .. } => inner,
        other => other,
    }
}

/// A value of the host form read as a scalar by its declared type; a value
/// of an unknown type, or of a type without a scalar reading, through its
/// JSON form.
pub(crate) fn read_scalar<'a, A: Access<'a>>(
    a: A,
    ty: Option<&FieldType>,
    lookup: &dyn TypeLookup,
) -> Scalar {
    let Some(ft) = ty.map(unwrap_option) else {
        return untyped(&a.to_value(&UNTYPED, lookup));
    };
    match ft {
        FieldType::Logical => a.logical().map_or(Scalar::Null, Scalar::Bool),
        FieldType::Integer(_) => a.integer().map_or(Scalar::Null, |x| Scalar::Num(x as f64)),
        FieldType::Double(_) => a.double().map_or(Scalar::Null, Scalar::Num),
        FieldType::Character(_) => a
            .character()
            .map_or(Scalar::Null, |s| Scalar::Str(s.into_owned())),
        FieldType::Date => a.date().map_or(Scalar::Null, Scalar::Date),
        FieldType::Posixct => a.datetime().map_or(Scalar::Null, Scalar::DateTime),
        FieldType::Vec { element, .. } => {
            let n = a.vec(element).unwrap_or(0);
            Scalar::Array(read_items(a, element, n, lookup))
        }
        FieldType::Factor { levels, ordered } => match a.factor(levels.as_deref(), *ordered) {
            Ok(f) => {
                let mut items = Vec::with_capacity(f.codes.len());
                f.codes.each_in(0, f.codes.len(), |_, c| {
                    items.push(
                        f.level(c)
                            .map_or(Scalar::Null, |l| Scalar::Str(l.to_string())),
                    );
                });
                Scalar::Array(items)
            }
            Err(_) => Scalar::from_value(&a.to_value(ft, lookup)),
        },
        _ => Scalar::from_value(&a.to_value(ft, lookup)),
    }
}

/// Whether `read_scalar` reads a value of `ft` without its JSON form: a
/// scalar, a factor, or a vector of these.
pub(crate) fn readable(ft: &FieldType) -> bool {
    match unwrap_option(ft) {
        FieldType::Logical
        | FieldType::Integer(_)
        | FieldType::Double(_)
        | FieldType::Character(_)
        | FieldType::Date
        | FieldType::Posixct
        | FieldType::Factor { .. } => true,
        FieldType::Vec { element, .. } => readable(element),
        _ => false,
    }
}

/// Whether `==` compares two values of `ft` with `equal_field_access`:
/// every type but a scalar, whose comparison follows `scalar_eq`, and a
/// factor, which compares by labels.
fn compared_in_place(ft: &FieldType) -> bool {
    !matches!(
        ft,
        FieldType::Logical
            | FieldType::Integer(_)
            | FieldType::Double(_)
            | FieldType::Character(_)
            | FieldType::Date
            | FieldType::Posixct
            | FieldType::Factor { .. }
    )
}

/// Items `0..n` of a sequence of `element` as scalars.
fn read_items<'a, A: Access<'a>>(
    a: A,
    element: &FieldType,
    n: usize,
    lookup: &dyn TypeLookup,
) -> Vec<Scalar> {
    let mut out = Vec::with_capacity(n);
    each_item(a, element, n, lookup, |x| {
        out.push(x);
        true
    });
    out
}

/// Calls `f` with items `0..n` of a sequence of `element` as scalars, in
/// order, until `f` returns `false`. Reads blocks of 4096 items in place.
fn each_item<'a, A: Access<'a>>(
    a: A,
    element: &FieldType,
    n: usize,
    lookup: &dyn TypeLookup,
    mut f: impl FnMut(Scalar) -> bool,
) {
    const BLOCK: usize = 4096;
    let inner = unwrap_option(element);
    let mut go = true;
    let mut start = 0;
    while go && start < n {
        let end = (start + BLOCK).min(n);
        let mut put = |x: Scalar| {
            if go {
                go = f(x);
            }
        };
        let or_null = |o: Option<Scalar>| o.unwrap_or(Scalar::Null);
        match inner {
            FieldType::Double(_) => a.each_double_in(start, end, |_, r| {
                put(or_null(r.ok().flatten().map(Scalar::Num)))
            }),
            FieldType::Integer(_) => a.each_integer_in(start, end, |_, r| {
                put(or_null(r.ok().flatten().map(|x| Scalar::Num(x as f64))))
            }),
            FieldType::Logical => a.each_logical_in(start, end, |_, r| {
                put(or_null(r.ok().flatten().map(Scalar::Bool)))
            }),
            FieldType::Character(_) => a.each_character_in(start, end, |_, r| {
                put(or_null(
                    r.ok().flatten().map(|s| Scalar::Str(s.into_owned())),
                ))
            }),
            FieldType::Date => a.each_date_in(start, end, |_, r| {
                put(or_null(r.ok().flatten().map(Scalar::Date)))
            }),
            FieldType::Posixct => a.each_datetime_in(start, end, |_, r| {
                put(or_null(r.ok().flatten().map(Scalar::DateTime)))
            }),
            _ => {
                for i in start..end {
                    let x = a.item(i);
                    put(if x.is_absent() {
                        Scalar::Null
                    } else {
                        read_scalar(x, Some(element), lookup)
                    });
                }
            }
        }
        start = end;
    }
}

/// A value of the JSON host form without a declared type: a factor's
/// `{"levels", "codes"}` reads as its level strings.
fn untyped(v: &Value) -> Scalar {
    if let Value::Object(m) = v
        && let (Some(Value::Array(levels)), Some(Value::Array(codes))) =
            (m.get("levels"), m.get("codes"))
    {
        return Scalar::Array(
            codes
                .iter()
                .map(|c| {
                    c.as_u64()
                        .and_then(|k| levels.get((k as usize).checked_sub(1)?))
                        .map_or(Scalar::Null, Scalar::from_value)
                })
                .collect(),
        );
    }
    Scalar::from_value(v)
}

/// `nrow()`, `ncol()`, and `dim()` of a value of the JSON host form without
/// a declared type: a table's `"$length"` and columns, a matrix's `shape`.
fn untyped_measure(op: Op, v: &Value) -> Option<Scalar> {
    let shape = || -> Option<Vec<u64>> {
        v.get("shape")?
            .as_array()?
            .iter()
            .map(Value::as_u64)
            .collect()
    };
    let rows = v.get("$length").and_then(Value::as_u64);
    match op {
        Op::Nrow => rows
            .or_else(|| shape()?.first().copied())
            .map(|x| Scalar::Num(x as f64)),
        Op::Ncol => match (rows, v.as_object()) {
            (Some(_), Some(m)) => Some(Scalar::Num(
                m.keys().filter(|k| !k.starts_with('$')).count() as f64,
            )),
            _ => shape()?.get(1).map(|&x| Scalar::Num(x as f64)),
        },
        Op::Dim => {
            shape().map(|s| Scalar::Array(s.into_iter().map(|x| Scalar::Num(x as f64)).collect()))
        }
        _ => None,
    }
}

fn present<'l, A>(v: Scalar) -> Evaluated<'l, A> {
    Ok(Ok(Val::V(v)))
}

macro_rules! value {
    ($self:ident, $e:expr) => {
        match $e? {
            Ok(v) => $self.scalar(v),
            Err(Absent) => return Ok(Err(Absent)),
        }
    };
}

macro_rules! val {
    ($e:expr) => {
        match $e? {
            Ok(v) => v,
            Err(Absent) => return Ok(Err(Absent)),
        }
    };
}

/// An evaluation environment: the value a rule belongs to and the fields in
/// its scope, and the element of an enclosing quantifier.
pub(crate) struct Env<'l, A> {
    pub data: A,
    pub scope: &'l [FieldDef],
    pub lookup: &'l dyn TypeLookup,
    pub element: Option<(A, Option<&'l FieldType>)>,
}

impl<'a, 'l, A: Access<'a>> Env<'l, A> {
    /// The value of an evaluated expression, read by its declared type.
    pub fn scalar(&self, v: Val<'l, A>) -> Scalar {
        match v {
            Val::V(v) => v,
            Val::R(a, ty) => read_scalar(a, ty, self.lookup),
        }
    }

    /// The fields of a value of type `ty`; `None` means the rule's scope.
    fn fields_of(&self, ty: Option<&'l FieldType>, cur: A) -> Option<&'l [FieldDef]> {
        let lookup = self.lookup;
        match ty.map(unwrap_option) {
            None => Some(self.scope),
            Some(FieldType::Named { name }) => match lookup.find(name)? {
                TypeDef::Struct(s) => Some(&s.fields),
                TypeDef::Enum(e) => {
                    let variant = cur.variant(name, e).ok()?;
                    match &e.variants.iter().find(|v| v.name.as_str() == variant)?.kind {
                        VariantKind::Struct { fields, .. } => Some(fields),
                        VariantKind::Newtype { of } => match lookup.find(of)? {
                            TypeDef::Struct(s) => Some(&s.fields),
                            TypeDef::Enum(_) | TypeDef::Table(_) => None,
                        },
                        VariantKind::Unit => None,
                    }
                }
                TypeDef::Table(_) => None,
            },
            _ => None,
        }
    }

    fn var(&self, root: Root, steps: &[Step]) -> Evaluated<'l, A> {
        let (mut cur, mut ty) = match root {
            Root::Data => (self.data, None),
            Root::Element => match self.element {
                Some(e) => e,
                None => return Err("`.x` outside `all()` or `any()`".to_string()),
            },
        };
        let mut at_root = root == Root::Data;
        for s in steps {
            if cur.is_absent() {
                return Ok(Err(Absent));
            }
            match s {
                Step::Field(k) | Step::Key(k) | Step::Column(k) => {
                    let next_ty = match ty.map(unwrap_option) {
                        Some(FieldType::BtreeMap { value, .. }) => Some(value.as_ref()),
                        _ if at_root || ty.is_some() => self
                            .fields_of(if at_root { None } else { ty }, cur)
                            .and_then(|f| f.iter().find(|f| f.name.as_str() == k.as_str()))
                            .map(|f| &f.ty),
                        _ => None,
                    };
                    cur = match ty.map(unwrap_option) {
                        Some(FieldType::BtreeMap { .. }) => {
                            let mut found = None;
                            cur.each_entry(|_, e| {
                                if let Ok((key, x)) = e
                                    && found.is_none()
                                    && key == k.as_str()
                                {
                                    found = Some(x);
                                }
                            });
                            match found {
                                Some(x) => x,
                                None => return Ok(Err(Absent)),
                            }
                        }
                        _ => cur.member(k),
                    };
                    ty = next_ty;
                }
                Step::OrderedKey(k) => {
                    let mut found = None;
                    cur.each_entry(|_, e| {
                        if let Ok((key, x)) = e
                            && found.is_none()
                            && key == k.as_str()
                        {
                            found = Some(x);
                        }
                    });
                    ty = match ty.map(unwrap_option) {
                        Some(FieldType::IndexMap { value, .. }) => Some(value.as_ref()),
                        _ => None,
                    };
                    match found {
                        Some(x) => cur = x,
                        None => return Ok(Err(Absent)),
                    }
                }
            }
            at_root = false;
        }
        if cur.is_absent() {
            Ok(Err(Absent))
        } else {
            Ok(Ok(Val::R(cur, ty)))
        }
    }

    pub fn eval(&self, e: &Expr) -> Evaluated<'l, A> {
        match e {
            Expr::Lit(v) => present(v.clone()),
            Expr::Var { root, steps } => self.var(*root, steps),
            Expr::If(parts) => {
                let mut i = 0;
                while i + 1 < parts.len() {
                    let c = value!(self, self.eval(&parts[i]));
                    if want_bool(&c, "an `if` condition")? {
                        return self.eval(&parts[i + 1]);
                    }
                    i += 2;
                }
                self.eval(&parts[i])
            }
            Expr::And(parts) => self.decided(parts, false),
            Expr::Or(parts) => self.decided(parts, true),
            Expr::Cell(_) | Expr::Aggregate { .. } => {
                Err("a cell is read by the rows of a table".to_string())
            }
            Expr::IsNa(inner) => match self.eval(inner)? {
                Ok(v) => present(Scalar::Bool(is_nan(&self.scalar(v)))),
                Err(Absent) => present(Scalar::Bool(true)),
            },
            Expr::IsNull(inner) => present(Scalar::Bool(self.eval(inner)?.is_err())),
            Expr::IsVariant {
                value,
                variant,
                tag,
            } => {
                let v = val!(self.eval(value));
                if let Val::R(a, Some(ft)) = &v
                    && let FieldType::Named { name } = unwrap_option(ft)
                    && let Some(TypeDef::Enum(def)) = self.lookup.find(name)
                {
                    return match a.variant(name, def) {
                        Ok(n) => present(Scalar::Bool(n == variant.as_str())),
                        Err(f) => Err(f.message),
                    };
                }
                // A value of the JSON host form without a declared type.
                let json = match v {
                    Val::R(a, ty) => a.to_value(ty.unwrap_or(&UNTYPED), self.lookup),
                    Val::V(s) => s.to_value(),
                };
                let name = match (tag, &json) {
                    (VariantTag::UnitOnly | VariantTag::Unknown, Value::String(s)) => {
                        Some(s.as_str())
                    }
                    (VariantTag::Member(m), Value::Object(o)) => o.get(m).and_then(Value::as_str),
                    (VariantTag::Unknown, Value::Object(_)) => {
                        return Err(
                            "the variant of an enum outside the lookup cannot be read".to_string()
                        );
                    }
                    _ => None,
                };
                match name {
                    Some(n) => present(Scalar::Bool(n == variant)),
                    None => Err(format!("{json} is not a value of the enum")),
                }
            }
            Expr::Levels { value, declared } => {
                let v = val!(self.eval(value));
                if let Val::R(a, Some(ft)) = &v
                    && let FieldType::Factor { levels, ordered } = unwrap_option(ft)
                {
                    return match a.factor(levels.as_deref(), *ordered) {
                        Ok(f) => match (levels, declared) {
                            (Some(d), _) | (None, Some(d)) => present(strings(d)),
                            _ => present(Scalar::Array(
                                f.levels
                                    .iter()
                                    .map(|l| Scalar::Str(l.to_string()))
                                    .collect(),
                            )),
                        },
                        Err(e) => Err(e.message),
                    };
                }
                // A factor of the JSON host form: `{"levels", "codes"}`.
                if let Val::R(a, ty) = &v
                    && let Value::Object(m) = a.to_value(ty.unwrap_or(&UNTYPED), self.lookup)
                    && let Some(l) = m.get("levels")
                {
                    return present(Scalar::from_value(l));
                }
                match declared {
                    Some(d) => present(strings(d)),
                    None => Err("the value carries no levels".to_string()),
                }
            }
            Expr::Quantifier {
                all,
                collection,
                kind,
                condition,
            } => {
                let c = val!(self.eval(collection));
                let mut result = None;
                self.each_element(c, *kind, &mut |_, el| {
                    let env = Env {
                        data: self.data,
                        scope: self.scope,
                        lookup: self.lookup,
                        element: Some(el),
                    };
                    let holds = match env.eval(condition)? {
                        Ok(v) => want_bool(&env.scalar(v), "a quantifier's condition")?,
                        // An absent operand: the element holds.
                        Err(Absent) => *all,
                    };
                    if holds != *all {
                        result = Some(!*all);
                        return Ok(false);
                    }
                    Ok(true)
                })?;
                present(Scalar::Bool(result.unwrap_or(*all)))
            }
            Expr::Op(op, args) => {
                if args.len() == 1 && matches!(op, Op::Length | Op::Nrow | Op::Ncol | Op::Dim) {
                    let v = val!(self.eval(&args[0]));
                    if let Val::R(a, Some(ft)) = &v
                        && let Some(r) = self.measure(*op, *a, ft)
                    {
                        return r.map(Ok);
                    }
                    if let Val::R(a, None) = &v
                        && *op != Op::Length
                        && let Some(r) = untyped_measure(*op, &a.to_value(&UNTYPED, self.lookup))
                    {
                        return present(r);
                    }
                    return apply(*op, &[self.scalar(v)]).map(|v| Ok(Val::V(v)));
                }
                if let [x, y] = args.as_slice() {
                    return self.binary(*op, x, y);
                }
                let mut vals = Vec::with_capacity(args.len());
                let mut absent = false;
                for a in args {
                    match self.eval(a)? {
                        Ok(v) => vals.push(v),
                        Err(Absent) => absent = true,
                    }
                }
                if absent {
                    return Ok(Err(Absent));
                }
                if let Some(r) = self.in_place(*op, &vals) {
                    return r.map(|v| v.map(Val::V));
                }
                let scalars: Vec<Scalar> = vals.into_iter().map(|v| self.scalar(v)).collect();
                // `NaN` is missing, to an operator whose result is no number.
                if matches!(
                    op,
                    Op::Eq | Op::Ne | Op::Lt | Op::Le | Op::Gt | Op::Ge | Op::In | Op::Cat
                ) && scalars.iter().any(is_nan)
                {
                    return Ok(Err(Absent));
                }
                apply(*op, &scalars).map(|v| Ok(Val::V(v)))
            }
        }
    }

    /// An operation of two operands, as `Expr::Op` evaluates one of any number,
    /// without a vector of them.
    fn binary(&self, op: Op, x: &Expr, y: &Expr) -> Evaluated<'l, A> {
        let (a, b) = (self.eval(x)?, self.eval(y)?);
        let (Ok(a), Ok(b)) = (a, b) else {
            return Ok(Err(Absent));
        };
        let vals = [a, b];
        if let Some(r) = self.in_place(op, &vals) {
            return r.map(|v| v.map(Val::V));
        }
        let [a, b] = vals;
        let scalars = [self.scalar(a), self.scalar(b)];
        // `NaN` is missing, to an operator whose result is no number.
        if matches!(
            op,
            Op::Eq | Op::Ne | Op::Lt | Op::Le | Op::Gt | Op::Ge | Op::In | Op::Cat
        ) && scalars.iter().any(is_nan)
        {
            return Ok(Err(Absent));
        }
        apply(op, &scalars).map(|v| Ok(Val::V(v)))
    }

    /// `and` and `or`: the first operand that is `deciding` decides; otherwise
    /// an error, then an absent operand, is the result.
    fn decided(&self, parts: &[Expr], deciding: bool) -> Evaluated<'l, A> {
        let what = if deciding {
            "an operand of `or`"
        } else {
            "an operand of `and`"
        };
        let mut error = None;
        let mut absent = false;
        for p in parts {
            let found = match self.eval(p) {
                Ok(Ok(v)) => want_bool(&self.scalar(v), what),
                Ok(Err(Absent)) => {
                    absent = true;
                    continue;
                }
                Err(e) => Err(e),
            };
            match found {
                Ok(b) if b == deciding => return present(Scalar::Bool(deciding)),
                Ok(_) => {}
                Err(e) => error = error.or(Some(e)),
            }
        }
        match (error, absent) {
            (Some(e), _) => Err(e),
            (None, true) => Ok(Err(Absent)),
            (None, false) => present(Scalar::Bool(!deciding)),
        }
    }

    /// The operators that read references in place: `==` and `!=` of two
    /// struct or enum values, `in` over a vector, and `non_increasing()` and
    /// `non_decreasing()` of a vector; `None` for the others.
    fn in_place(
        &self,
        op: Op,
        vals: &[Val<'l, A>],
    ) -> Option<Result<Result<Scalar, Absent>, String>> {
        match (op, vals) {
            (Op::Eq | Op::Ne, [Val::R(a, ta), Val::R(b, tb)]) => {
                let (ta, tb) = (ta.map(unwrap_option), tb.map(unwrap_option));
                let eq = match (ta, tb) {
                    // Two values of one declared type that is not a scalar
                    // or a factor: compared in place.
                    (Some(fa), Some(fb)) if fa == fb && compared_in_place(fa) => {
                        crate::equal::equal_field_access(fa, *a, *b, self.lookup)
                    }
                    // A value without a scalar reading, or of an unknown
                    // type: compared by canonical form.
                    _ if !ta.is_some_and(readable) || !tb.is_some_and(readable) => {
                        crate::equal::canonical_equal(
                            ta.unwrap_or(&UNTYPED),
                            *a,
                            tb.unwrap_or(&UNTYPED),
                            *b,
                            self.lookup,
                        )
                    }
                    _ => return None,
                };
                Some(Ok(Ok(Scalar::Bool(eq == (op == Op::Eq)))))
            }
            (Op::In, [needle, Val::R(a, Some(ft))]) => {
                let FieldType::Vec { element, .. } = unwrap_option(ft) else {
                    return None;
                };
                let needle = match needle {
                    Val::V(s) => s.clone(),
                    Val::R(x, t) => read_scalar(*x, *t, self.lookup),
                };
                if is_nan(&needle) {
                    return Some(Ok(Err(Absent)));
                }
                let n = a.vec(element).ok()?;
                let found = contains(*a, element, n, &needle, self.lookup);
                Some(Ok(Ok(Scalar::Bool(found))))
            }
            (Op::Subset, [Val::R(a, Some(ft)), set]) => {
                let FieldType::Vec { element, .. } = unwrap_option(ft) else {
                    return None;
                };
                let set = match set {
                    Val::V(s) => s.clone(),
                    Val::R(x, t) => read_scalar(*x, *t, self.lookup),
                };
                let Scalar::Array(set) = set else {
                    return None;
                };
                let n = a.vec(element).ok()?;
                let mut all = true;
                each_item(*a, element, n, self.lookup, |x| {
                    all = set.iter().any(|y| scalar_eq(&x, y));
                    all
                });
                Some(Ok(Ok(Scalar::Bool(all))))
            }
            (Op::Sum | Op::Mean | Op::Median | Op::Min | Op::Max, [Val::R(a, Some(ft))]) => {
                let FieldType::Vec { element, .. } = unwrap_option(ft) else {
                    return None;
                };
                let n = a.vec(element).ok()?;
                let mut r = Reduce::new(op);
                Some(match each_number(*a, element, n, r.name(), |x| r.push(x)) {
                    Ok(true) => r.finish().map(Ok),
                    Ok(false) => Ok(Err(Absent)),
                    Err(e) => Err(e),
                })
            }
            (Op::NonIncreasing | Op::NonDecreasing, [Val::R(a, Some(ft))]) => {
                let FieldType::Vec { element, .. } = unwrap_option(ft) else {
                    return None;
                };
                let n = a.vec(element).ok()?;
                Some(monotone(*a, element, n, op == Op::NonIncreasing, self.lookup).map(Ok))
            }
            _ => None,
        }
    }

    /// `length()`, `nrow()`, `ncol()`, and `dim()` of a reference, read
    /// without converting the value; `None` where the declared type gives no
    /// direct reading.
    fn measure(&self, op: Op, a: A, ft: &'l FieldType) -> Option<Result<Val<'l, A>, String>> {
        let ft = unwrap_option(ft);
        let lookup = self.lookup;
        let n = |x: u64| Some(Ok(Val::V(Scalar::Num(x as f64))));
        match (op, ft) {
            (Op::Length, FieldType::Vec { element, .. }) => {
                a.vec(element).ok().and_then(|k| n(k as u64))
            }
            (Op::Length, FieldType::Factor { levels, ordered }) => a
                .factor(levels.as_deref(), *ordered)
                .ok()
                .and_then(|f| n(f.codes.len() as u64)),
            (_, FieldType::Matrix { element, .. } | FieldType::Array { element, .. }) => {
                let d = a.dense(element).ok()?;
                match op {
                    Op::Nrow => n(*d.shape.first()?),
                    Op::Ncol => n(*d.shape.get(1)?),
                    Op::Dim => Some(Ok(Val::V(Scalar::Array(
                        d.shape.iter().map(|&x| Scalar::Num(x as f64)).collect(),
                    )))),
                    _ => None,
                }
            }
            (_, FieldType::SparseMatrix { element, .. }) => {
                let s = a.sparse(element).ok()?;
                match op {
                    Op::Nrow => n(s.shape[0]),
                    Op::Ncol => n(s.shape[1]),
                    Op::Dim => Some(Ok(Val::V(Scalar::Array(
                        s.shape.iter().map(|&x| Scalar::Num(x as f64)).collect(),
                    )))),
                    _ => None,
                }
            }
            (Op::Nrow | Op::Ncol, FieldType::Table { columns, open, .. }) => {
                let t = a.table(columns, *open).ok()?;
                n(if op == Op::Nrow {
                    t.rows
                } else {
                    t.columns.len() as u64
                })
            }
            (Op::Nrow | Op::Ncol, FieldType::TableOf { of }) => {
                let TypeDef::Struct(s) = lookup.find(of)? else {
                    return None;
                };
                let t = a.table(&s.fields, false).ok()?;
                n(if op == Op::Nrow {
                    t.rows
                } else {
                    t.columns.len() as u64
                })
            }
            _ => None,
        }
    }

    /// Calls `f` with each element of a collection and its JSON Pointer
    /// suffix, until `f` returns `false`.
    pub fn each_element(
        &self,
        c: Val<'l, A>,
        kind: Collection,
        f: &mut dyn FnMut(Suffix<'a>, (A, Option<&'l FieldType>)) -> Result<bool, String>,
    ) -> Result<(), String> {
        let (a, ty) = match c {
            Val::R(a, ty) => (a, ty.map(unwrap_option)),
            Val::V(v) => return Err(format!("{} is not a collection", v.shown())),
        };
        let as_array = match (kind, ty) {
            (Collection::Array, _) => true,
            (Collection::BtreeMap | Collection::IndexMap, _) => false,
            (Collection::Dynamic, Some(FieldType::Vec { .. })) => true,
            (Collection::Dynamic, Some(_)) => false,
            (Collection::Dynamic, None) => a.vec(&UNTYPED).is_ok(),
        };
        if as_array {
            let element = match ty {
                Some(FieldType::Vec { element, .. }) => Some(element.as_ref()),
                _ => None,
            };
            let n = a
                .vec(element.unwrap_or(&UNTYPED))
                .map_err(|_| "the value is not a collection".to_string())?;
            for i in 0..n {
                if !f(Suffix::Index(i), (a.item(i), element))? {
                    break;
                }
            }
            return Ok(());
        }
        let ordered =
            kind == Collection::IndexMap || matches!(ty, Some(FieldType::IndexMap { .. }));
        let value_ty = match ty {
            Some(FieldType::BtreeMap { value, .. } | FieldType::IndexMap { value, .. }) => {
                Some(value.as_ref())
            }
            _ => None,
        };
        if a.map(value_ty.unwrap_or(&UNTYPED), ordered).is_err() {
            return Err("the value is not a collection".to_string());
        }
        let mut entries = Vec::new();
        a.each_entry(|i, e| {
            if let Ok((k, x)) = e {
                let suffix = if ordered {
                    Suffix::Item(i)
                } else {
                    Suffix::Key(k)
                };
                entries.push((suffix, x));
            }
        });
        for (suffix, x) in entries {
            if !f(suffix, (x, value_ty))? {
                break;
            }
        }
        Ok(())
    }
}

fn strings(items: &[Box<str>]) -> Scalar {
    Scalar::Array(items.iter().map(|s| Scalar::Str(s.to_string())).collect())
}

/// Whether items `0..n` of a vector hold `needle`, scanning doubles and
/// integers in place.
fn contains<'a, A: Access<'a>>(
    a: A,
    element: &FieldType,
    n: usize,
    needle: &Scalar,
    lookup: &dyn TypeLookup,
) -> bool {
    match (unwrap_option(element), needle) {
        (FieldType::Double(_) | FieldType::Integer(_), Scalar::Num(x)) => {
            if let Some(s) = a.f64_slice() {
                return s[..n.min(s.len())]
                    .iter()
                    .any(|&v| !crate::access::is_na_real(v) && scalar_eq(&Scalar::Num(v), needle));
            }
            if let Some(s) = a.i32_slice() {
                return s[..n.min(s.len())]
                    .iter()
                    .any(|&v| v != i32::MIN && f64::from(v) == *x);
            }
            found_in(a, element, n, needle, lookup)
        }
        _ => found_in(a, element, n, needle, lookup),
    }
}

/// Whether items `0..n` of a vector hold `needle`, read in blocks until the
/// first match.
fn found_in<'a, A: Access<'a>>(
    a: A,
    element: &FieldType,
    n: usize,
    needle: &Scalar,
    lookup: &dyn TypeLookup,
) -> bool {
    let mut found = false;
    each_item(a, element, n, lookup, |x| {
        found = scalar_eq(&x, needle);
        !found
    });
    found
}

/// Whether items `0..n` of a vector are in order, scanning doubles in
/// place.
fn monotone<'a, A: Access<'a>>(
    a: A,
    element: &FieldType,
    n: usize,
    non_increasing: bool,
    lookup: &dyn TypeLookup,
) -> Result<Scalar, String> {
    let ok_pair = |o: Option<Ordering>| match o {
        Some(Ordering::Less) => !non_increasing,
        Some(Ordering::Greater) => non_increasing,
        Some(Ordering::Equal) => true,
        None => false,
    };
    let unordered = || "an ordered array holds numbers, dates, or date-times".to_string();
    let inner = unwrap_option(element);
    if matches!(inner, FieldType::Double(_))
        && let Some(s) = a.f64_slice()
    {
        let s = &s[..n.min(s.len())];
        let mut ok = true;
        for (i, &x) in s.iter().enumerate() {
            if crate::access::is_na_real(x) {
                return Err(unordered());
            }
            if i > 0 {
                ok &= ok_pair(s[i - 1].partial_cmp(&x));
            }
        }
        return Ok(Scalar::Bool(ok));
    }
    // Numbers, dates, and date-times streamed in place; `NA` is not
    // ordered.
    let mut prev: Option<f64> = None;
    let (mut ok, mut missing) = (true, false);
    let mut step = |r: Option<f64>| match r {
        None => missing = true,
        Some(x) => {
            if let Some(p) = prev {
                ok &= ok_pair(p.partial_cmp(&x));
            }
            prev = Some(x);
        }
    };
    match inner {
        FieldType::Double(_) => a.each_double(n, |_, r| step(r.ok().flatten())),
        FieldType::Integer(_) => a.each_integer(n, |_, r| step(r.ok().flatten().map(|x| x as f64))),
        FieldType::Date => a.each_date(n, |_, r| step(r.ok().flatten().map(|d| d as f64))),
        FieldType::Posixct => a.each_datetime(n, |_, r| step(r.ok().flatten().map(|m| m as f64))),
        _ => {
            let items = read_items(a, element, n, lookup);
            let mut ok = true;
            for w in items.windows(2) {
                ok &= ok_pair(scalar_cmp(&w[0], &w[1]).map_err(|_| unordered())?);
            }
            return Ok(Scalar::Bool(ok));
        }
    }
    if missing {
        Err(unordered())
    } else {
        Ok(Scalar::Bool(ok))
    }
}

/// Items `0..n` of a vector of numbers, passed to `f` in order and read in
/// place; whether none is missing.
fn each_number<'a, A: Access<'a>>(
    a: A,
    element: &FieldType,
    n: usize,
    op: &str,
    mut f: impl FnMut(f64),
) -> Result<bool, String> {
    let inner = unwrap_option(element);
    if matches!(inner, FieldType::Double(_))
        && let Some(s) = a.f64_slice()
    {
        for &x in &s[..n.min(s.len())] {
            if crate::access::is_na_real(x) {
                return Ok(false);
            }
            f(x);
        }
        return Ok(true);
    }
    let mut whole = true;
    match inner {
        FieldType::Double(_) => a.each_double(n, |_, r| match r.ok().flatten() {
            Some(x) => f(x),
            None => whole = false,
        }),
        FieldType::Integer(_) => a.each_integer(n, |_, r| match r.ok().flatten() {
            Some(x) => f(x as f64),
            None => whole = false,
        }),
        _ => return Err(format!("`{op}` takes an array of numbers")),
    }
    Ok(whole)
}

/// `sum()`, `mean()`, `min()`, or `max()` of numbers: the sum is exact,
/// rounded once, and the mean is that sum over the count; `NaN` is the least
/// and the greatest of numbers that hold it. All but the sum of no numbers are
/// errors.
pub(super) struct Reduce {
    op: Op,
    sum: ExactSum,
    n: u64,
    extreme: f64,
    /// The numbers of a median.
    values: Vec<f64>,
}

impl Reduce {
    pub fn new(op: Op) -> Reduce {
        Reduce {
            op,
            sum: ExactSum::default(),
            n: 0,
            extreme: if op == Op::Min {
                f64::INFINITY
            } else {
                f64::NEG_INFINITY
            },
            values: Vec::new(),
        }
    }
    pub fn name(&self) -> &'static str {
        match self.op {
            Op::Sum => "sum",
            Op::Mean => "mean",
            Op::Min => "min",
            Op::Median => "median",
            _ => "max",
        }
    }
    pub fn push(&mut self, x: f64) {
        self.n += 1;
        match self.op {
            Op::Sum | Op::Mean => self.sum.add(x),
            Op::Median => self.values.push(x),
            _ if x.is_nan() || self.extreme.is_nan() => self.extreme = f64::NAN,
            Op::Min => self.extreme = extreme(true, self.extreme, x),
            _ => self.extreme = extreme(false, self.extreme, x),
        }
    }
    pub fn finish(self) -> Result<Scalar, String> {
        if self.n == 0 && self.op != Op::Sum {
            return Err(format!("`{}` of an empty array has no value", self.name()));
        }
        Ok(Scalar::Num(match self.op {
            Op::Sum => self.sum.value(),
            Op::Mean => self.sum.value() / self.n as f64,
            Op::Median => median(self.values),
            _ => self.extreme,
        }))
    }
}

/// The middle number of `xs`, or the mean of the two middle ones: their sum,
/// rounded once, over 2. `xs` is not empty and holds no `NaN`.
pub(super) fn median(mut xs: Vec<f64>) -> f64 {
    let n = xs.len();
    let mid = n / 2;
    let (_, &mut hi, _) = xs.select_nth_unstable_by(mid, f64::total_cmp);
    if n % 2 == 1 {
        return hi;
    }
    let lo = xs[..mid].iter().copied().fold(f64::NEG_INFINITY, f64::max);
    (lo + hi) / 2.0
}

/// The least or the greatest of numbers, `NaN` where one is; `-0` is below
/// `0`, so that the result does not depend on the order of the numbers.
pub(super) fn extreme(least: bool, a: f64, b: f64) -> f64 {
    if a.is_nan() || b.is_nan() {
        f64::NAN
    } else if (a.total_cmp(&b) == Ordering::Less) == least {
        a
    } else {
        b
    }
}

/// A negative number to a power that is not a whole number.
pub(super) const POWER: &str = "a negative number to a power that is not a whole number";

/// `x^y`, or the error of a power without a real value: a negative number to
/// a power that is not a whole number, and 0 to a negative power, a division
/// by zero. `x^2` is `x * x`, rounded once; another power is the platform's
/// `pow()`.
pub(super) fn power(x: f64, y: f64) -> Result<f64, &'static str> {
    if x < 0.0 && y.is_finite() && y.fract() != 0.0 {
        return Err(POWER);
    }
    if x == 0.0 && y < 0.0 {
        return Err(ZERO);
    }
    // The square is one rounded product, as in R.
    if y == 2.0 {
        return Ok(x * x);
    }
    Ok(x.powf(y))
}

/// Division by zero.
pub(super) const ZERO: &str = "division by zero";
/// The root of a number below 0.
pub(super) const ROOT: &str = "`sqrt` of a negative number";
/// The logarithm of a number that is not above 0.
pub(super) const LOGARITHM: &str = "`log` of a number that is not positive";

fn apply(op: Op, a: &[Scalar]) -> Result<Scalar, String> {
    let bool_ = |b: bool| Ok(Scalar::Bool(b));
    match op {
        Op::Eq => bool_(scalar_eq(&a[0], &a[1])),
        Op::Ne => bool_(!scalar_eq(&a[0], &a[1])),
        Op::Lt | Op::Le | Op::Gt | Op::Ge => {
            let Some(o) = scalar_cmp(&a[0], &a[1])? else {
                return bool_(false);
            };
            bool_(match op {
                Op::Lt => o == Ordering::Less,
                Op::Le => o != Ordering::Greater,
                Op::Gt => o == Ordering::Greater,
                _ => o != Ordering::Less,
            })
        }
        Op::Not => bool_(!want_bool(&a[0], "the operand of `!`")?),
        Op::Sum | Op::Mean | Op::Median | Op::Min | Op::Max
            if a.len() == 1 && matches!(a[0], Scalar::Array(_)) =>
        {
            let Scalar::Array(xs) = &a[0] else {
                unreachable!()
            };
            let mut r = Reduce::new(op);
            for x in xs {
                r.push(want_number(x, "an array's element")?);
            }
            r.finish()
        }
        Op::Sum | Op::Mean | Op::Median => Err(format!(
            "`{}` takes an array of numbers, found {}",
            Reduce::new(op).name(),
            a[0].shown()
        )),
        Op::Sqrt => match want_number(&a[0], "sqrt")? {
            x if x < 0.0 => Err(ROOT.to_string()),
            x => Ok(Scalar::Num(x.sqrt())),
        },
        Op::Log => match want_number(&a[0], "log")? {
            x if x <= 0.0 => Err(LOGARITHM.to_string()),
            x => Ok(Scalar::Num(x.ln())),
        },
        Op::Exp => Ok(Scalar::Num(want_number(&a[0], "exp")?.exp())),
        Op::Pow => {
            let (x, y) = (want_number(&a[0], "^")?, want_number(&a[1], "^")?);
            power(x, y).map(Scalar::Num).map_err(str::to_string)
        }
        Op::N | Op::Count => Err("`n` and `count` read the rows of a table".to_string()),
        Op::Abs => Ok(Scalar::Num(want_number(&a[0], "abs")?.abs())),
        Op::Nchar => match &a[0] {
            Scalar::Str(s) => Ok(Scalar::Num(s.chars().count() as f64)),
            other => Err(format!("`nchar` takes a string, found {}", other.shown())),
        },
        Op::Add | Op::Mul | Op::Min | Op::Max => {
            let mut xs = Vec::with_capacity(a.len());
            for v in a {
                xs.push(want_number(v, "arithmetic")?);
            }
            Ok(Scalar::Num(match op {
                Op::Add => xs.iter().sum(),
                Op::Mul => xs.iter().product(),
                Op::Min => xs.iter().fold(f64::INFINITY, |m, &x| extreme(true, m, x)),
                _ => xs
                    .iter()
                    .fold(f64::NEG_INFINITY, |m, &x| extreme(false, m, x)),
            }))
        }
        Op::Sub => {
            let x = want_number(&a[0], "-")?;
            Ok(Scalar::Num(match a.get(1) {
                Some(y) => x - want_number(y, "-")?,
                None => -x,
            }))
        }
        Op::Div | Op::Mod => {
            let (x, y) = (want_number(&a[0], "/")?, want_number(&a[1], "/")?);
            if y == 0.0 && !x.is_nan() {
                return Err(ZERO.to_string());
            }
            Ok(Scalar::Num(if op == Op::Div { x / y } else { x % y }))
        }
        Op::Floor => Ok(Scalar::Num(want_number(&a[0], "floor")?.floor())),
        Op::Ceiling => Ok(Scalar::Num(want_number(&a[0], "ceiling")?.ceil())),
        // R's round(): halves to even (IEC 60559).
        Op::Round => Ok(Scalar::Num(want_number(&a[0], "round")?.round_ties_even())),
        Op::In => match (&a[0], &a[1]) {
            (Scalar::Str(n), Scalar::Str(s)) => bool_(s.contains(n.as_str())),
            (other, Scalar::Str(_)) => Err(format!(
                "`in` over a string takes a string, found {}",
                other.shown()
            )),
            (needle, Scalar::Array(hay)) => bool_(hay.iter().any(|i| scalar_eq(i, needle))),
            (_, other) => Err(format!("`in` searches an array, found {}", other.shown())),
        },
        Op::Cat => {
            let mut out = String::new();
            for v in a {
                match v {
                    Scalar::Str(s) => out.push_str(s),
                    Scalar::Num(x) => out.push_str(&number_text(*x)),
                    Scalar::Date(d) => out.push_str(&date_text(*d)),
                    Scalar::DateTime(m) => out.push_str(&datetime_text(*m)),
                    other => {
                        return Err(format!(
                            "`cat` joins strings and numbers, found {}",
                            other.shown()
                        ));
                    }
                }
            }
            Ok(Scalar::Str(out))
        }
        Op::Length => match &a[0] {
            Scalar::Array(i) => Ok(Scalar::Num(i.len() as f64)),
            Scalar::Str(_) => Err(
                "`length` counts the elements of an array, found a string; `nchar` counts a string's characters"
                    .to_string(),
            ),
            other => Err(format!("`length` counts the elements of an array, found {}", other.shown())),
        },
        Op::Subset => match (&a[0], &a[1]) {
            (Scalar::Array(x), Scalar::Array(y)) => {
                bool_(x.iter().all(|i| y.iter().any(|j| scalar_eq(i, j))))
            }
            _ => Err("`subset` takes arrays".to_string()),
        },
        Op::NonIncreasing | Op::NonDecreasing => {
            let Scalar::Array(xs) = &a[0] else {
                return Err("`non_increasing` takes an array".to_string());
            };
            let mut ok = true;
            for w in xs.windows(2) {
                let o = scalar_cmp(&w[0], &w[1]).map_err(|_| {
                    "an ordered array holds numbers, dates, or date-times".to_string()
                })?;
                ok &= match o {
                    Some(Ordering::Less) => op == Op::NonDecreasing,
                    Some(Ordering::Greater) => op == Op::NonIncreasing,
                    Some(Ordering::Equal) => true,
                    None => false,
                };
            }
            bool_(ok)
        }
        Op::Nrow | Op::Ncol | Op::Dim => Err(format!(
            "`{}` takes a table or a matrix",
            match op {
                Op::Nrow => "nrow",
                Op::Ncol => "ncol",
                _ => "dim",
            }
        )),
    }
}
