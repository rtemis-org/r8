//! Evaluation of compiled expressions over values in any host form.
//!
//! The evaluator reads a value through [`Access`]. A `var` evaluates to a
//! reference into the value with its declared type, converted to its JSON
//! form only where an operator needs the value; `is.null()`, `length()`,
//! `nrow()`, `ncol()`, `dim()`, `levels()`, `all()`, and `any()` read the
//! reference directly.
//!
//! A `var` that reaches an absent value (a missing member or `null`) makes
//! the expression [`Absent`]; the absence propagates through every operator
//! except `is_null()`, and a rule with an absent operand holds. Conditional
//! positions take logicals only.

use serde_json::Value;

use super::compile::{Collection, Expr, Op, Root, Step, VariantTag};
use super::ty::TypeLookup;
use crate::access::Access;
use crate::ir::{FieldDef, FieldType, TypeDef, VariantShape};

/// An expression read an absent value.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Absent;

/// An evaluated expression: a JSON value, or a reference into the host
/// form with its declared type, if known.
pub(crate) enum Val<'l, A> {
    V(Value),
    R(A, Option<&'l FieldType>),
}

/// The result of evaluating an expression: a value, or absence. An `Err` is
/// a failure that compilation did not exclude, such as a division by zero.
pub(crate) type Evaluated<'l, A> = Result<Result<Val<'l, A>, Absent>, String>;

/// The type of a value whose declared type is not known.
static UNTYPED: FieldType = FieldType::Json;

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

fn number(x: f64) -> Value {
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

fn want_number(v: &Value, op: &str) -> Result<f64, String> {
    as_number(v).ok_or_else(|| format!("`{op}` takes numbers, found {v}"))
}

fn want_bool(v: &Value, what: &str) -> Result<bool, String> {
    v.as_bool()
        .ok_or_else(|| format!("{what} is a logical condition, found {v}"))
}

/// Strict equality: numbers by value, everything else structurally.
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

/// The values of a factor: a string array, or the strings that
/// `{"levels", "codes"}` encodes.
fn factor_values(v: &Value) -> Option<Vec<Value>> {
    match v {
        Value::Array(items) => Some(items.clone()),
        Value::Object(m) => {
            let levels = m.get("levels")?.as_array()?;
            let codes = m.get("codes")?.as_array()?;
            codes
                .iter()
                .map(|c| {
                    let i = c.as_u64()? as usize;
                    levels.get(i.checked_sub(1)?).cloned()
                })
                .collect()
        }
        _ => None,
    }
}

/// The items of an array-like value: an array, or a factor's values.
fn items(v: &Value) -> Option<Vec<Value>> {
    factor_values(v)
}

fn shape(v: &Value) -> Option<Vec<u64>> {
    v.get("shape")?
        .as_array()?
        .iter()
        .map(Value::as_u64)
        .collect()
}

/// The number of rows of a table (`"$length"`) or a matrix (`shape[0]`).
fn nrow(v: &Value) -> Option<u64> {
    v.get("$length")
        .and_then(Value::as_u64)
        .or_else(|| shape(v)?.first().copied())
}

fn ncol(v: &Value) -> Option<u64> {
    if v.get("$length").is_some() {
        let m = v.as_object()?;
        return Some(m.keys().filter(|k| !k.starts_with('$')).count() as u64);
    }
    shape(v)?.get(1).copied()
}

fn present<'l, A>(v: Value) -> Evaluated<'l, A> {
    Ok(Ok(Val::V(v)))
}

macro_rules! value {
    ($self:ident, $e:expr) => {
        match $e? {
            Ok(v) => $self.json(v),
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

fn unwrap_option(ft: &FieldType) -> &FieldType {
    match ft {
        FieldType::Option { inner, .. } => inner,
        other => other,
    }
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
    fn json(&self, v: Val<'l, A>) -> Value {
        match v {
            Val::V(v) => v,
            Val::R(a, ty) => a.to_value(ty.unwrap_or(&UNTYPED), self.lookup),
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
                    match &e
                        .variants
                        .iter()
                        .find(|v| v.name.as_str() == variant)?
                        .shape
                    {
                        VariantShape::Struct { fields, .. } => Some(fields),
                        VariantShape::Newtype { of } => match lookup.find(of)? {
                            TypeDef::Struct(s) => Some(&s.fields),
                            TypeDef::Enum(_) => None,
                        },
                        VariantShape::Unit => None,
                    }
                }
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
                Step::Member(k) => {
                    let next_ty = match ty.map(unwrap_option) {
                        Some(FieldType::Hashmap { value, .. }) => Some(value.as_ref()),
                        _ if at_root || ty.is_some() => self
                            .fields_of(if at_root { None } else { ty }, cur)
                            .and_then(|f| f.iter().find(|f| f.name.as_str() == k.as_str()))
                            .map(|f| &f.ty),
                        _ => None,
                    };
                    cur = match ty.map(unwrap_option) {
                        Some(FieldType::Hashmap { .. }) => {
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
                        Some(FieldType::OrderedMap { value, .. }) => Some(value.as_ref()),
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
            Expr::And(parts) => {
                for p in parts {
                    if !want_bool(&value!(self, self.eval(p)), "an operand of `and`")? {
                        return present(Value::Bool(false));
                    }
                }
                present(Value::Bool(true))
            }
            Expr::Or(parts) => {
                for p in parts {
                    if want_bool(&value!(self, self.eval(p)), "an operand of `or`")? {
                        return present(Value::Bool(true));
                    }
                }
                present(Value::Bool(false))
            }
            Expr::IsNull(inner) => present(Value::Bool(self.eval(inner)?.is_err())),
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
                        Ok(n) => present(Value::Bool(n == variant.as_str())),
                        Err(f) => Err(f.message),
                    };
                }
                let v = self.json(v);
                let name = match (tag, &v) {
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
                    Some(n) => present(Value::Bool(n == variant)),
                    None => Err(format!("{v} is not a value of the enum")),
                }
            }
            Expr::Levels { value, declared } => {
                let v = val!(self.eval(value));
                if let Val::R(a, Some(ft)) = &v
                    && let FieldType::Factor { levels, ordered } = unwrap_option(ft)
                {
                    return match a.factor(levels.as_deref(), *ordered) {
                        Ok(f) => match (levels, declared) {
                            (Some(d), _) | (None, Some(d)) => present(Value::from(d.clone())),
                            _ => present(Value::from(
                                f.levels.iter().map(|l| l.to_string()).collect::<Vec<_>>(),
                            )),
                        },
                        Err(e) => Err(e.message),
                    };
                }
                let v = self.json(v);
                let levels = match (&v, declared) {
                    (Value::Object(m), _) => m.get("levels").cloned(),
                    (_, Some(d)) => Some(Value::from(d.clone())),
                    _ => None,
                };
                levels
                    .map(present)
                    .unwrap_or_else(|| Err(format!("{v} carries no levels")))
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
                        Ok(v) => want_bool(&env.json(v), "a quantifier's condition")?,
                        // An absent operand: the element holds.
                        Err(Absent) => *all,
                    };
                    if holds != *all {
                        result = Some(!*all);
                        return Ok(false);
                    }
                    Ok(true)
                })?;
                present(Value::Bool(result.unwrap_or(*all)))
            }
            Expr::Op(op, args) => {
                if args.len() == 1 && matches!(op, Op::Length | Op::Nrow | Op::Ncol | Op::Dim) {
                    let v = val!(self.eval(&args[0]));
                    if let Val::R(a, Some(ft)) = &v
                        && let Some(r) = self.measure(*op, *a, ft)
                    {
                        return r.map(Ok);
                    }
                    return apply(*op, &[self.json(v)]).map(|v| Ok(Val::V(v)));
                }
                let mut vals = Vec::with_capacity(args.len());
                for a in args {
                    vals.push(value!(self, self.eval(a)));
                }
                apply(*op, &vals).map(|v| Ok(Val::V(v)))
            }
        }
    }

    /// `length()`, `nrow()`, `ncol()`, and `dim()` of a reference, read
    /// without converting the value; `None` where the declared type gives no
    /// direct reading.
    fn measure(&self, op: Op, a: A, ft: &'l FieldType) -> Option<Result<Val<'l, A>, String>> {
        let ft = unwrap_option(ft);
        let lookup = self.lookup;
        let n = |x: u64| Some(Ok(Val::V(Value::from(x))));
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
                    Op::Dim => Some(Ok(Val::V(Value::from(d.shape)))),
                    _ => None,
                }
            }
            (_, FieldType::SparseMatrix { element, .. }) => {
                let s = a.sparse(element).ok()?;
                match op {
                    Op::Nrow => n(s.shape[0]),
                    Op::Ncol => n(s.shape[1]),
                    Op::Dim => Some(Ok(Val::V(Value::from(s.shape.to_vec())))),
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
        f: &mut dyn FnMut(String, (A, Option<&'l FieldType>)) -> Result<bool, String>,
    ) -> Result<(), String> {
        let (a, ty) = match c {
            Val::R(a, ty) => (a, ty.map(unwrap_option)),
            Val::V(v) => return Err(format!("{v} is not a collection")),
        };
        let as_array = match (kind, ty) {
            (Collection::Array, _) => true,
            (Collection::Hashmap | Collection::OrderedMap, _) => false,
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
                .map_err(|_| format!("{} is not a collection", self.json(Val::R(a, ty))))?;
            for i in 0..n {
                if !f(format!("/{i}"), (a.item(i), element))? {
                    break;
                }
            }
            return Ok(());
        }
        let ordered =
            kind == Collection::OrderedMap || matches!(ty, Some(FieldType::OrderedMap { .. }));
        let value_ty = match ty {
            Some(FieldType::Hashmap { value, .. } | FieldType::OrderedMap { value, .. }) => {
                Some(value.as_ref())
            }
            _ => None,
        };
        if a.map(value_ty.unwrap_or(&UNTYPED), ordered).is_err() {
            return Err(format!("{} is not a collection", self.json(Val::R(a, ty))));
        }
        let mut entries = Vec::new();
        a.each_entry(|i, e| {
            if let Ok((k, x)) = e {
                let suffix = if ordered {
                    format!("/{i}/value")
                } else {
                    format!("/{}", k.replace('~', "~0").replace('/', "~1"))
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
fn apply(op: Op, a: &[Value]) -> Result<Value, String> {
    let bool_ = |b: bool| Ok(Value::Bool(b));
    match op {
        Op::Eq => bool_(strict_eq(&a[0], &a[1])),
        Op::Ne => bool_(!strict_eq(&a[0], &a[1])),
        Op::Lt | Op::Le | Op::Gt | Op::Ge => {
            let ord = match (as_number(&a[0]), as_number(&a[1])) {
                (Some(x), Some(y)) => x.partial_cmp(&y),
                // Dates and date-times compare in their canonical text form,
                // which sorts in time order.
                _ => match (&a[0], &a[1]) {
                    (Value::String(x), Value::String(y)) => Some(x.cmp(y)),
                    _ => return Err(format!("cannot compare {} and {}", a[0], a[1])),
                },
            };
            let Some(o) = ord else { return bool_(false) };
            use std::cmp::Ordering::*;
            bool_(match op {
                Op::Lt => o == Less,
                Op::Le => o != Greater,
                Op::Gt => o == Greater,
                _ => o != Less,
            })
        }
        Op::Not => bool_(!want_bool(&a[0], "the operand of `!`")?),
        Op::Add | Op::Mul | Op::Min | Op::Max => {
            let mut xs = Vec::with_capacity(a.len());
            for v in a {
                xs.push(want_number(v, "arithmetic")?);
            }
            let r = match op {
                Op::Add => xs.iter().sum(),
                Op::Mul => xs.iter().product(),
                Op::Min => xs.iter().copied().fold(f64::INFINITY, f64::min),
                _ => xs.iter().copied().fold(f64::NEG_INFINITY, f64::max),
            };
            Ok(number(r))
        }
        Op::Sub => {
            let x = want_number(&a[0], "-")?;
            Ok(number(match a.get(1) {
                Some(y) => x - want_number(y, "-")?,
                None => -x,
            }))
        }
        Op::Div | Op::Mod => {
            let (x, y) = (want_number(&a[0], "/")?, want_number(&a[1], "/")?);
            if y == 0.0 {
                return Err("division by zero".to_string());
            }
            Ok(number(if op == Op::Div { x / y } else { x % y }))
        }
        Op::Floor => Ok(number(want_number(&a[0], "floor")?.floor())),
        Op::Ceiling => Ok(number(want_number(&a[0], "ceiling")?.ceil())),
        // R's round(): halves to even (IEC 60559).
        Op::Round => Ok(number(want_number(&a[0], "round")?.round_ties_even())),
        Op::In => match &a[1] {
            Value::String(s) => match &a[0] {
                Value::String(n) => bool_(s.contains(n.as_str())),
                other => Err(format!("`in` over a string takes a string, found {other}")),
            },
            other => {
                let hay =
                    items(other).ok_or_else(|| format!("`in` searches an array, found {other}"))?;
                bool_(hay.iter().any(|i| strict_eq(i, &a[0])))
            }
        },
        Op::Cat => {
            let mut out = String::new();
            for v in a {
                match v {
                    Value::String(s) => out.push_str(s),
                    Value::Number(n) => out.push_str(&n.to_string()),
                    other => return Err(format!("`cat` joins strings and numbers, found {other}")),
                }
            }
            Ok(Value::String(out))
        }
        Op::Length => match &a[0] {
            Value::String(s) => Ok(Value::from(s.chars().count())),
            other => items(other)
                .map(|i| Value::from(i.len()))
                .ok_or_else(|| format!("`length` takes an array, found {other}")),
        },
        Op::Subset => {
            let (x, y) = (items(&a[0]), items(&a[1]));
            match (x, y) {
                (Some(x), Some(y)) => bool_(x.iter().all(|i| y.iter().any(|j| strict_eq(i, j)))),
                _ => Err("`subset` takes arrays".to_string()),
            }
        }
        Op::NonIncreasing | Op::NonDecreasing => {
            let xs = items(&a[0]).ok_or("`non_increasing` takes an array")?;
            let mut ok = true;
            for w in xs.windows(2) {
                let o = match (as_number(&w[0]), as_number(&w[1])) {
                    (Some(p), Some(q)) => p.partial_cmp(&q),
                    _ => match (&w[0], &w[1]) {
                        (Value::String(p), Value::String(q)) => Some(p.cmp(q)),
                        _ => {
                            return Err(
                                "an ordered array holds numbers, dates, or date-times".to_string()
                            );
                        }
                    },
                };
                ok &= match o {
                    Some(std::cmp::Ordering::Less) => op == Op::NonDecreasing,
                    Some(std::cmp::Ordering::Greater) => op == Op::NonIncreasing,
                    Some(std::cmp::Ordering::Equal) => true,
                    None => false,
                };
            }
            bool_(ok)
        }
        Op::Nrow => nrow(&a[0])
            .map(Value::from)
            .ok_or_else(|| format!("`nrow` takes a table or a matrix, found {}", a[0])),
        Op::Ncol => ncol(&a[0])
            .map(Value::from)
            .ok_or_else(|| format!("`ncol` takes a table or a matrix, found {}", a[0])),
        Op::Dim => shape(&a[0])
            .map(Value::from)
            .ok_or_else(|| format!("`dim` takes a matrix, found {}", a[0])),
    }
}
