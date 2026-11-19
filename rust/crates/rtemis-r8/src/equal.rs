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

use crate::access::{Access, Json, Read};
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

/// Whether two valid values of the field type `ft` are equal.
pub fn equal_field_access<'a, A: Access<'a>>(
    ft: &FieldType,
    a: A,
    b: A,
    lookup: &dyn TypeLookup,
) -> bool {
    Eq { lookup }.value(ft, a, b)
}

/// Whether a value of `ta` and a value of `tb` have the same canonical
/// form.
pub(crate) fn canonical_equal<'a, A: Access<'a>>(
    ta: &FieldType,
    a: A,
    tb: &FieldType,
    b: A,
    lookup: &dyn TypeLookup,
) -> bool {
    let e = Eq { lookup };
    e.digest(ta, a) == e.digest(tb, b)
}

struct Eq<'l> {
    lookup: &'l dyn TypeLookup,
}

impl Eq<'_> {
    /// The SHA-256 digest of the canonical form of a value of `ft`,
    /// streamed from the value, for the comparisons with no typed pairing:
    /// JSON values, a default against a present value, and values of two
    /// types.
    fn digest<'a, B: Access<'a>>(&self, ft: &FieldType, v: B) -> [u8; 32] {
        use sha2::{Digest, Sha256};
        let mut e = Emitter::new(self.lookup, Sha256::new(), true, true);
        e.value(ft, v);
        e.w.out.finalize().into()
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
            FieldType::Factor { levels, ordered } => self.factor(levels.as_deref(), *ordered, a, b),
            FieldType::SparseMatrix { element, .. } => self.sparse(element, a, b),
            FieldType::Table { columns, open, .. } => self.table(columns, *open, a, b),
            FieldType::TableOf { of } => match lookup.find(of) {
                Some(TypeDef::Struct(s)) => self.table(&s.fields, false, a, b),
                _ => false,
            },
            FieldType::Hashmap { value, .. } => self.map(value, false, a, b),
            FieldType::OrderedMap { value, .. } => self.map(value, true, a, b),
            // JSON values compare by the digest of their canonical form.
            _ => self.digest(ft, a) == self.digest(ft, b),
        }
    }

    /// Items `0..n` of two sequences of `element`: plain arrays compared as
    /// slices, other host forms read block by block, stopping at the first
    /// difference.
    fn sequence<'a, A: Access<'a>>(&self, element: &FieldType, a: A, b: A, n: usize) -> bool {
        let inner = unwrap_option(element);
        match inner {
            FieldType::Double(_) => {
                if let (Some(x), Some(y)) = (a.f64_slice(), b.f64_slice()) {
                    return x.len() >= n
                        && y.len() >= n
                        && x[..n].iter().zip(&y[..n]).all(|(&p, &q)| {
                            let (na_p, na_q) =
                                (crate::access::is_na_real(p), crate::access::is_na_real(q));
                            if na_p || na_q {
                                na_p && na_q
                            } else {
                                double_eq(p, q)
                            }
                        });
                }
                blocks(
                    n,
                    |s, e, out| a.each_double_in(s, e, |_, r| out.push(r)),
                    |s, e, out| b.each_double_in(s, e, |_, r| out.push(r)),
                    |p, q| match (p, q) {
                        (Some(p), Some(q)) => double_eq(*p, *q),
                        (None, None) => true,
                        _ => false,
                    },
                )
            }
            FieldType::Integer(_) => {
                if let (Some(x), Some(y)) = (a.i32_slice(), b.i32_slice()) {
                    return x.len() >= n && y.len() >= n && x[..n] == y[..n];
                }
                blocks(
                    n,
                    |s, e, out| a.each_integer_in(s, e, |_, r| out.push(r)),
                    |s, e, out| b.each_integer_in(s, e, |_, r| out.push(r)),
                    |p, q| p == q,
                )
            }
            FieldType::Logical => blocks(
                n,
                |s, e, out| a.each_logical_in(s, e, |_, r| out.push(r)),
                |s, e, out| b.each_logical_in(s, e, |_, r| out.push(r)),
                |p, q| p == q,
            ),
            FieldType::Character(_) => blocks(
                n,
                |s, e, out| a.each_character_in(s, e, |_, r| out.push(r)),
                |s, e, out| b.each_character_in(s, e, |_, r| out.push(r)),
                |p: &Option<Cow<'a, str>>, q| p == q,
            ),
            FieldType::Date => blocks(
                n,
                |s, e, out| a.each_date_in(s, e, |_, r| out.push(r)),
                |s, e, out| b.each_date_in(s, e, |_, r| out.push(r)),
                |p, q| p == q,
            ),
            FieldType::Posixct => blocks(
                n,
                |s, e, out| a.each_datetime_in(s, e, |_, r| out.push(r)),
                |s, e, out| b.each_datetime_in(s, e, |_, r| out.push(r)),
                |p, q| p == q,
            ),
            _ => (0..n).all(|i| self.value(element, a.item(i), b.item(i))),
        }
    }

    /// Two factors: by label per cell for declared levels, by levels and
    /// codes otherwise, as their canonical forms are written.
    fn factor<'a, A: Access<'a>>(
        &self,
        declared: Option<&[String]>,
        ordered: bool,
        a: A,
        b: A,
    ) -> bool {
        let (Ok(x), Ok(y)) = (a.factor(declared, ordered), b.factor(declared, ordered)) else {
            return false;
        };
        let n = x.codes.len();
        if y.codes.len() != n {
            return false;
        }
        if x.levels == y.levels {
            if let (crate::access::Codes::I32(p), crate::access::Codes::I32(q)) =
                (&x.codes, &y.codes)
            {
                return p == q;
            }
            return (0..n).all(|i| x.codes.get(i) == y.codes.get(i));
        }
        if declared.is_none() {
            return false;
        }
        let label = |f: &crate::access::Factor<'_>, i: usize| -> Option<String> {
            let c = f.codes.get(i)?;
            f.levels
                .get((c as usize).checked_sub(1)?)
                .map(|l| l.to_string())
        };
        (0..n).all(|i| label(&x, i) == label(&y, i))
    }

    /// Two sparse matrices: shape, structure, and stored values.
    fn sparse<'a, A: Access<'a>>(&self, element: &FieldType, a: A, b: A) -> bool {
        match (a.sparse(element), b.sparse(element)) {
            (Ok(x), Ok(y)) => {
                x.shape == y.shape
                    && x.len == y.len
                    && x.indptr == y.indptr
                    && x.indices == y.indices
                    && self.sequence(element, x.data, y.data, x.len)
            }
            _ => false,
        }
    }

    /// Two tables: rows, then each column by name, with equal types for the
    /// extra columns of an open table.
    fn table<'a, A: Access<'a>>(&self, columns: &[FieldDef], open: bool, a: A, b: A) -> bool {
        let (Ok(x), Ok(y)) = (a.table(columns, open), b.table(columns, open)) else {
            return false;
        };
        if x.rows != y.rows || x.columns.len() != y.columns.len() {
            return false;
        }
        let n = x.rows as usize;
        x.columns.iter().all(|(name, cx)| {
            let Some((_, cy)) = y.columns.iter().find(|(k, _)| k == name) else {
                return false;
            };
            let ty = match columns.iter().find(|c| c.name.as_str() == name.as_ref()) {
                Some(c) => c.ty.clone(),
                None => match (a.extra_column_type(name), b.extra_column_type(name)) {
                    (Ok(p), Ok(q)) if p == q => p,
                    _ => return false,
                },
            };
            match unwrap_option(&ty) {
                FieldType::Factor { .. } => self.value(&ty, *cx, *cy),
                _ => self.sequence(&ty, *cx, *cy, n),
            }
        })
    }

    /// Two maps: an ordered map pairs entries by position, a hashmap by
    /// name.
    fn map<'a, A: Access<'a>>(&self, value: &FieldType, ordered: bool, a: A, b: A) -> bool {
        let entries = |v: A| {
            let mut out: Vec<(Cow<'a, str>, A)> = Vec::new();
            let mut ok = true;
            v.each_entry(|_, e| match e {
                Ok(e) => out.push(e),
                Err(_) => ok = false,
            });
            ok.then_some(out)
        };
        let (Some(x), Some(y)) = (entries(a), entries(b)) else {
            return false;
        };
        if x.len() != y.len() {
            return false;
        }
        if ordered {
            return x
                .iter()
                .zip(&y)
                .all(|((k, p), (l, q))| k == l && self.value(value, *p, *q));
        }
        let index: std::collections::HashMap<&str, A> =
            y.iter().map(|(k, q)| (k.as_ref(), *q)).collect();
        index.len() == y.len()
            && x.iter().all(|(k, p)| {
                index
                    .get(k.as_ref())
                    .is_some_and(|q| self.value(value, *p, *q))
            })
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
                (Some(d), true, false) => self.digest(&f.ty, Json::new(d)) == self.digest(&f.ty, y),
                (Some(d), false, true) => self.digest(&f.ty, x) == self.digest(&f.ty, Json::new(d)),
                _ => self.value(&f.ty, x, y),
            }
        })
    }
}

/// The number of items `blocks` reads from each side at a time.
const BLOCK: usize = 4096;

/// Items `0..n` of two sequences, read block by block with `read_a` and
/// `read_b` and compared with `same`, stopping at the first difference. An
/// item that does not read, or a block read short, is a difference. Peak
/// memory is two blocks.
fn blocks<T>(
    n: usize,
    mut read_a: impl FnMut(usize, usize, &mut Vec<Read<Option<T>>>),
    mut read_b: impl FnMut(usize, usize, &mut Vec<Read<Option<T>>>),
    same: impl Fn(&Option<T>, &Option<T>) -> bool,
) -> bool {
    let (mut x, mut y) = (Vec::with_capacity(BLOCK), Vec::with_capacity(BLOCK));
    let mut start = 0;
    while start < n {
        let end = (start + BLOCK).min(n);
        x.clear();
        y.clear();
        read_a(start, end, &mut x);
        read_b(start, end, &mut y);
        if x.len() != end - start || y.len() != end - start {
            return false;
        }
        for (p, q) in x.iter().zip(&y) {
            match (p, q) {
                (Ok(p), Ok(q)) if same(p, q) => {}
                _ => return false,
            }
        }
        start = end;
    }
    true
}
