//! Equality of two values of one type, in one host form.
//!
//! [`equal_access`] decides whether two valid values of a type have the same
//! normalized form, as their canonical texts would, without writing either:
//! it walks both values by the declared type and stops at the first
//! difference. Numbers are equal when their canonical forms are (`-0` equals
//! `0`, a `NaN` equals a `NaN`, and a missing value equals only a missing
//! value); an absent field with a default equals its default; members of
//! maps and tables compare by name. Runtime-only fields, which have no JSON
//! form, compare with [`Access::identical`]. Two handles to the same object
//! are equal without a read ([`Access::same`]).

use std::borrow::Cow;

use crate::access::{Access, Json};
use crate::emit::Emitter;
use crate::ir::{FieldDef, FieldType, TypeDef, VariantShape};
use crate::name::QualifiedName;
use crate::rules::TypeLookup;

fn unwrap_option(ft: &FieldType) -> &FieldType {
    match ft {
        FieldType::Option { inner, .. } => inner,
        other => other,
    }
}

/// Doubles equal in the canonical form.
#[inline]
fn double_eq(a: f64, b: f64) -> bool {
    a == b || (a.is_nan() && b.is_nan())
}

/// Whether two valid values of the named type `ty` are equal.
pub fn equal_access<'a, A: Access<'a>>(
    ty: &QualifiedName,
    a: A,
    b: A,
    lookup: &dyn TypeLookup,
) -> bool {
    Eq { lookup }.named(ty, a, b)
}

struct Eq<'l> {
    lookup: &'l dyn TypeLookup,
}

impl Eq<'_> {
    /// The canonical text of a value of `ft`, for the comparisons that
    /// compare text.
    fn text<'a, B: Access<'a>>(&self, ft: &FieldType, v: B) -> String {
        let mut e = Emitter::new(self.lookup, String::new(), true, false);
        e.value(ft, v);
        e.w.out
    }

    fn value<'a, A: Access<'a>>(&self, ft: &FieldType, a: A, b: A) -> bool {
        if a.same(b) {
            return true;
        }
        match (a.is_absent(), b.is_absent()) {
            (true, true) => return true,
            (true, false) | (false, true) => return false,
            _ => {}
        }
        let lookup = self.lookup;
        match ft {
            FieldType::Option { inner, .. } => self.value(inner, a, b),
            FieldType::Logical => a.logical().ok() == b.logical().ok(),
            FieldType::Integer(_) => a.integer().ok() == b.integer().ok(),
            FieldType::Double(_) => match (a.double(), b.double()) {
                (Ok(x), Ok(y)) => double_eq(x, y),
                _ => false,
            },
            FieldType::Character(_) => a.character().ok() == b.character().ok(),
            FieldType::Date => a.date().ok() == b.date().ok(),
            FieldType::Posixct => a.datetime().ok() == b.datetime().ok(),
            FieldType::Raw => a.bytes().ok() == b.bytes().ok(),
            FieldType::R { .. } => a.identical(b),
            FieldType::Vec { element, .. } => match (a.vec(element), b.vec(element)) {
                (Ok(n), Ok(m)) if n == m => self.sequence(element, a, b, n),
                _ => false,
            },
            FieldType::Matrix { element, .. } | FieldType::Array { element, .. } => {
                self.dense(element, a, b)
            }
            FieldType::MatrixOf { of } => match lookup.find(of) {
                Some(TypeDef::Struct(s)) => match s.fields.first() {
                    Some(f) => self.dense(&f.ty, a, b),
                    None => true,
                },
                _ => false,
            },
            FieldType::Named { name } => self.named(name, a, b),
            FieldType::Dyn { r#trait } => {
                match (
                    a.implementor(r#trait, lookup),
                    b.implementor(r#trait, lookup),
                ) {
                    (Ok((x, _)), Ok((y, _))) if x == y => self.named(&x, a, b),
                    _ => false,
                }
            }
            // Factors, sparse matrices, tables, maps, and JSON values compare
            // as canonical text.
            _ => self.text(ft, a) == self.text(ft, b),
        }
    }

    /// Items `0..n` of two sequences of `element`.
    fn sequence<'a, A: Access<'a>>(&self, element: &FieldType, a: A, b: A, n: usize) -> bool {
        let inner = unwrap_option(element);
        match inner {
            FieldType::Double(_) => {
                if let (Some(x), Some(y)) = (a.f64_slice(), b.f64_slice()) {
                    return x[..n].iter().zip(&y[..n]).all(|(&p, &q)| {
                        let (na_p, na_q) =
                            (crate::access::is_na_real(p), crate::access::is_na_real(q));
                        if na_p || na_q {
                            na_p && na_q
                        } else {
                            double_eq(p, q)
                        }
                    });
                }
                collect_doubles(a, n)
                    .iter()
                    .zip(&collect_doubles(b, n))
                    .all(|(p, q)| match (p, q) {
                        (Some(p), Some(q)) => double_eq(*p, *q),
                        (None, None) => true,
                        _ => false,
                    })
            }
            FieldType::Integer(_) => {
                if let (Some(x), Some(y)) = (a.i32_slice(), b.i32_slice()) {
                    return x[..n] == y[..n];
                }
                let get = |v: A| {
                    let mut out = Vec::with_capacity(n);
                    v.each_integer(n, |_, r| out.push(r.ok().flatten()));
                    out
                };
                get(a) == get(b)
            }
            FieldType::Logical => {
                let get = |v: A| {
                    let mut out = Vec::with_capacity(n);
                    v.each_logical(n, |_, r| out.push(r.ok().flatten()));
                    out
                };
                get(a) == get(b)
            }
            FieldType::Character(_) => {
                let get = |v: A| {
                    let mut out: Vec<Option<Cow<'a, str>>> = Vec::with_capacity(n);
                    v.each_character(n, |_, r| out.push(r.ok().flatten()));
                    out
                };
                get(a) == get(b)
            }
            FieldType::Date => {
                let get = |v: A| {
                    let mut out = Vec::with_capacity(n);
                    v.each_date(n, |_, r| out.push(r.ok().flatten()));
                    out
                };
                get(a) == get(b)
            }
            FieldType::Posixct => {
                let get = |v: A| {
                    let mut out = Vec::with_capacity(n);
                    v.each_datetime(n, |_, r| out.push(r.ok().flatten()));
                    out
                };
                get(a) == get(b)
            }
            _ => (0..n).all(|i| self.value(element, a.item(i), b.item(i))),
        }
    }

    fn dense<'a, A: Access<'a>>(&self, element: &FieldType, a: A, b: A) -> bool {
        match (a.dense(element), b.dense(element)) {
            (Ok(x), Ok(y)) => {
                x.shape == y.shape
                    && x.dimnames == y.dimnames
                    && x.len == y.len
                    && self.sequence(element, x.data, y.data, x.len)
            }
            _ => false,
        }
    }

    fn named<'a, A: Access<'a>>(&self, name: &QualifiedName, a: A, b: A) -> bool {
        let lookup = self.lookup;
        match lookup.find(name) {
            Some(TypeDef::Struct(s)) => self.fields(&s.fields, a, b),
            Some(TypeDef::Enum(e)) => {
                let (Ok(va), Ok(vb)) = (a.variant(name, e), b.variant(name, e)) else {
                    return false;
                };
                if va != vb {
                    return false;
                }
                match e
                    .variants
                    .iter()
                    .find(|x| x.name.as_str() == va)
                    .map(|x| &x.shape)
                {
                    Some(VariantShape::Struct { fields, .. }) => self.fields(fields, a, b),
                    Some(VariantShape::Newtype { of }) => match lookup.find(of) {
                        Some(TypeDef::Struct(s)) => self.fields(&s.fields, a, b),
                        _ => false,
                    },
                    _ => true,
                }
            }
            None => false,
        }
    }

    fn fields<'a, A: Access<'a>>(&self, fields: &[FieldDef], a: A, b: A) -> bool {
        fields.iter().enumerate().all(|(i, f)| {
            let (x, y) = (
                a.member_at(i, f.name.as_str()),
                b.member_at(i, f.name.as_str()),
            );
            match (&f.default, x.is_absent(), y.is_absent()) {
                (Some(d), true, false) => self.text(&f.ty, Json::new(d)) == self.text(&f.ty, y),
                (Some(d), false, true) => self.text(&f.ty, x) == self.text(&f.ty, Json::new(d)),
                _ => self.value(&f.ty, x, y),
            }
        })
    }
}

fn collect_doubles<'a, A: Access<'a>>(v: A, n: usize) -> Vec<Option<f64>> {
    let mut out = Vec::with_capacity(n);
    v.each_double(n, |_, r| out.push(r.ok().flatten()));
    out
}
