//! Validation of a value against its type.
//!
//! [`validate`] checks a value's JSON form against a named type: every field
//! type, constraint, and rule, collecting every error with a stable code, a
//! JSON Pointer path, and, for a rule, its ID. A value's JSON form is the form
//! the codec writes: an absent field is `null` or an omitted member, and the
//! document envelope (`"$type"`, `"$types"`, `"$provenance"`) is accepted on
//! the outermost object.
//!
//! [`validate_access`] checks a value in any host form through
//! [`Access`]; [`validate`] is its JSON instance. Paths are the JSON Pointers
//! of the JSON form whatever the host form, and are rendered only for an
//! error.

use std::collections::{HashMap, HashSet};
use std::sync::Arc;

use regex::Regex;
use serde_json::Value;

use crate::access::{
    Access, CSC_FORM, Codes, DATE_MAX_DAYS, DATE_MIN_DAYS, Form, Json, MISSING_CODE,
    POSIXCT_MICROS, csc_well_formed,
};
use crate::ir::{
    CharacterType, DoubleType, FieldDef, FieldType, IntegerType, RuleDef, TypeDef, VariantShape,
};
use crate::name::{Name, QualifiedName};
use crate::problem::{Lit, Problem, Segment, listed};
use crate::rules::{CompiledRule, Outcome, Shown, TypeLookup};

/// A problem in a value.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct ValueError {
    /// A stable code, such as `missing_field`.
    pub code: &'static str,
    /// A JSON Pointer into the value's JSON form.
    pub path: String,
    /// The ID of the violated rule, for `rule_violation` and `rule_error`,
    /// when the rule has one.
    pub rule: Option<Name>,
    /// The violated rule's 0-based position among its scope's rules, for
    /// `rule_violation` and `rule_error`.
    pub rule_index: Option<usize>,
    /// The message.
    pub message: String,
    /// The values a violated rule read, for the first violations of each
    /// rule ([`DIAGNOSED`]); `None` for other problems.
    pub diagnosis: Option<Box<crate::rules::Diagnosis>>,
    /// The path as typed segments, from which a host writes it in its own
    /// syntax; `path` is their JSON Pointer.
    pub segments: Vec<crate::problem::Segment>,
    /// What the value fails, for a host to write its message.
    pub problem: Option<crate::problem::Problem>,
    /// The value found: bounded in size, and withheld for a `sensitive` field.
    pub found: Option<crate::rules::Shown>,
    /// The nearest allowed name or value.
    pub suggestion: Option<String>,
}

/// The number of violations of one rule in one validation that carry a
/// diagnosis.
pub const DIAGNOSED: usize = 5;

/// A diagnosis with its rule's expressions, as declared.
pub fn diagnosis(r: &RuleDef, mut d: crate::rules::Diagnosis) -> Box<crate::rules::Diagnosis> {
    d.invariant = Some(r.invariant.clone());
    d.when = r.when.clone();
    Box::new(d)
}

/// The range of `type_integer()`: `i32::MIN` is R's `NA_integer_`.
const INT_MIN: i64 = -2_147_483_647;
const INT_MAX: i64 = 2_147_483_647;

const ENVELOPE: [&str; 3] = ["$type", "$types", "$provenance"];

/// Compiled rules and patterns, kept between validations.
///
/// Rules are keyed by the address of their declaration, so a cache is valid
/// for the lookup it was used with while that lookup is unchanged; a caller
/// that changes its lookup clears the cache.
#[derive(Debug, Default)]
pub struct RuleCache {
    patterns: HashMap<String, Option<Regex>>,
    rules: HashMap<usize, Option<Arc<CompiledRule>>>,
}

impl RuleCache {
    /// An empty cache.
    pub fn new() -> Self {
        Self::default()
    }

    /// Removes every compiled rule. Compiled patterns stay: a pattern's
    /// compiled form depends on its text alone.
    pub fn clear(&mut self) {
        self.rules.clear();
    }

    /// The compiled form of `rule` in the scope of `fields`, compiled on
    /// first use; `None` when the rule does not compile. The cache keys a
    /// rule by its address, which holds while `lookup` is unchanged.
    pub fn compiled(
        &mut self,
        rule: &RuleDef,
        fields: &[FieldDef],
        lookup: &dyn TypeLookup,
    ) -> Option<Arc<CompiledRule>> {
        self.rules
            .entry(rule as *const RuleDef as usize)
            .or_insert_with(|| {
                CompiledRule::compile(rule, fields, lookup)
                    .ok()
                    .map(Arc::new)
            })
            .clone()
    }
}

/// A JSON Pointer, built as a chain and rendered for an error only.
#[derive(Clone, Copy)]
enum P<'p> {
    Root(&'p str),
    /// A field.
    Key(&'p P<'p>, &'p str),
    /// A key of a `hashmap()`.
    MapKey(&'p P<'p>, &'p str),
    /// A table column.
    Col(&'p P<'p>, &'p str),
    /// An element of a vector or an `ordered_map()`.
    Idx(&'p P<'p>, usize),
    /// A JSON Pointer suffix, already escaped.
    Raw(&'p P<'p>, &'p str),
}

impl P<'_> {
    fn render(&self) -> String {
        match self {
            P::Root(s) => s.to_string(),
            P::Key(p, k) | P::MapKey(p, k) | P::Col(p, k) => format!("{}/{}", p.render(), esc(k)),
            P::Idx(p, i) => format!("{}/{i}", p.render()),
            P::Raw(p, s) => format!("{}{s}", p.render()),
        }
    }

    fn segments(&self) -> Vec<Segment> {
        let mut out = match self {
            P::Root(_) => return Vec::new(),
            P::Key(p, _) | P::MapKey(p, _) | P::Col(p, _) | P::Idx(p, _) | P::Raw(p, _) => {
                p.segments()
            }
        };
        out.push(match self {
            P::Root(_) => unreachable!(),
            P::Key(_, k) => Segment::Field(k.to_string()),
            P::MapKey(_, k) => Segment::Key(k.to_string()),
            P::Col(_, k) => Segment::Column(k.to_string()),
            P::Idx(_, i) => Segment::Element(*i as u64),
            P::Raw(_, s) => Segment::Form(s.to_string()),
        });
        out
    }
}

fn esc(s: &str) -> String {
    s.replace('~', "~0").replace('/', "~1")
}

fn unwrap_option(ft: &FieldType) -> (&FieldType, bool) {
    match ft {
        FieldType::Option { inner, .. } => (inner.as_ref(), true),
        other => (other, false),
    }
}

fn is_scalar(ft: &FieldType) -> bool {
    matches!(
        ft,
        FieldType::Logical
            | FieldType::Integer(_)
            | FieldType::Double(_)
            | FieldType::Character(_)
            | FieldType::Date
            | FieldType::Posixct
    )
}

/// A scalar's identity for uniqueness: numbers by value, `NaN` equal to
/// itself, as the dialect's strict equality decides.
#[derive(PartialEq, Eq, Hash)]
enum Key {
    B(bool),
    N(u64),
    S(String),
}

fn number_key(x: f64) -> Key {
    Key::N(if x.is_nan() {
        f64::NAN.to_bits()
    } else {
        (x + 0.0).to_bits()
    })
}

/// Whether a double satisfies its constraints; the reporting check runs
/// only when it does not.
#[inline(always)]
fn double_ok(c: &DoubleType, x: f64) -> bool {
    (!c.finite || x.is_finite())
        && c.in_range(x)
        && c.values.as_ref().is_none_or(|v| v.contains(&x))
}

/// Whether a whole number satisfies the integer range and its constraints.
#[inline(always)]
fn integer_ok(c: &IntegerType, x: i64) -> bool {
    (INT_MIN..=INT_MAX).contains(&x) && {
        let y = x as i32;
        c.in_range(y) && c.values.as_ref().is_none_or(|v| v.contains(&y))
    }
}

/// A chunk of a numeric sequence.
#[derive(Clone, Copy)]
enum Chunk<'c> {
    F64(&'c [f64]),
    I32(&'c [i32]),
}

/// Whether every item of `chunk` satisfies the scalar type `inner`, a
/// missing item satisfying it when `optional`.
#[inline(always)]
fn chunk_ok(inner: &FieldType, optional: bool, chunk: Chunk) -> bool {
    let na_ok = |missing: bool, ok: bool| if missing { optional } else { ok };
    match (inner, chunk) {
        (FieldType::Double(c), Chunk::F64(s)) => s
            .iter()
            .all(|&x| na_ok(crate::access::is_na_real(x), double_ok(c, x))),
        (FieldType::Double(c), Chunk::I32(s)) => s
            .iter()
            .all(|&x| na_ok(x == i32::MIN, double_ok(c, f64::from(x)))),
        (FieldType::Integer(c), Chunk::I32(s)) => s
            .iter()
            .all(|&x| na_ok(x == i32::MIN, integer_ok(c, i64::from(x)))),
        (FieldType::Integer(c), Chunk::F64(s)) => s.iter().all(|&x| {
            na_ok(
                crate::access::is_na_real(x),
                x.fract() == 0.0 && integer_ok(c, x as i64),
            )
        }),
        _ => false,
    }
}

/// Whether the first `n` items of the numeric sequence `v` satisfy the
/// scalar type `inner`, scanned as one array or by region in chunks; `false`
/// also when neither form is available, so the caller reads item by item.
fn scan_clean<'a, A: Access<'a>>(inner: &FieldType, optional: bool, v: A, n: usize) -> bool {
    if !matches!(inner, FieldType::Double(_) | FieldType::Integer(_)) {
        return false;
    }
    if let Some(s) = v.f64_slice() {
        return chunk_ok(inner, optional, Chunk::F64(&s[..n.min(s.len())]));
    }
    if let Some(s) = v.i32_slice() {
        return chunk_ok(inner, optional, Chunk::I32(&s[..n.min(s.len())]));
    }
    // Sorted without missing items: bounds and finiteness hold for every
    // item when they hold for the first and the last. A closed set of values,
    // and whole numbers held as doubles, need every item.
    let bounds_only = match inner {
        FieldType::Double(c) => c.values.is_none(),
        FieldType::Integer(c) => c.values.is_none(),
        _ => false,
    };
    if n > 0 && bounds_only && v.sorted_without_na().is_some() {
        let mut first = [0i32; 1];
        let mut last = [0i32; 1];
        if v.i32_region(0, &mut first) == Some(1) && v.i32_region(n - 1, &mut last) == Some(1) {
            return chunk_ok(inner, optional, Chunk::I32(&first))
                && chunk_ok(inner, optional, Chunk::I32(&last));
        }
        if matches!(inner, FieldType::Double(_)) {
            let mut first = [0f64; 1];
            let mut last = [0f64; 1];
            if v.f64_region(0, &mut first) == Some(1) && v.f64_region(n - 1, &mut last) == Some(1) {
                return chunk_ok(inner, optional, Chunk::F64(&first))
                    && chunk_ok(inner, optional, Chunk::F64(&last));
            }
        }
    }
    const CHUNK: usize = 4096;
    let mut i = 0;
    let mut ibuf = [0i32; CHUNK];
    if v.i32_region(0, &mut ibuf[..0]).is_some() {
        while i < n {
            let want = (n - i).min(CHUNK);
            match v.i32_region(i, &mut ibuf[..want]) {
                Some(0) | None => return false,
                Some(got) => {
                    if !chunk_ok(inner, optional, Chunk::I32(&ibuf[..got])) {
                        return false;
                    }
                    i += got;
                }
            }
        }
        return true;
    }
    let mut fbuf = [0f64; CHUNK];
    if v.f64_region(0, &mut fbuf[..0]).is_some() {
        while i < n {
            let want = (n - i).min(CHUNK);
            match v.f64_region(i, &mut fbuf[..want]) {
                Some(0) | None => return false,
                Some(got) => {
                    if !chunk_ok(inner, optional, Chunk::F64(&fbuf[..got])) {
                        return false;
                    }
                    i += got;
                }
            }
        }
        return true;
    }
    false
}

/// The most errors one validation lists. A value with more is reported by
/// its first `ERROR_LIMIT` errors and one `error_limit` entry, so that the
/// report of a large invalid value stays small.
pub const ERROR_LIMIT: usize = 100;

struct Validator<'l, 'c> {
    lookup: &'l dyn TypeLookup,
    errors: Vec<ValueError>,
    /// The number of errors found, including those past the limit.
    count: usize,
    cache: &'c mut RuleCache,
    /// The depth of `sensitive` fields around the value being checked.
    sensitive: u32,
}

impl<'l, 'c> Validator<'l, 'c> {
    fn new(lookup: &'l dyn TypeLookup, cache: &'c mut RuleCache) -> Self {
        Validator {
            lookup,
            errors: Vec::new(),
            count: 0,
            cache,
            sensitive: 0,
        }
    }

    fn push(&mut self, e: ValueError) {
        self.count += 1;
        if self.errors.len() < ERROR_LIMIT {
            self.errors.push(e);
        }
    }

    fn finish(mut self) -> Vec<ValueError> {
        if self.count > ERROR_LIMIT {
            let limit = ValueError {
                code: "error_limit",
                path: String::new(),
                rule: None,
                rule_index: None,
                message: format!(
                    "the value has more than {ERROR_LIMIT} errors; the first {ERROR_LIMIT} are listed"
                ),
                diagnosis: None,
                ..Default::default()
            };
            self.errors.push(limit);
        }
        self.errors
    }
}

impl<'l> Validator<'l, '_> {
    fn error(&mut self, code: &'static str, path: &P, message: String) {
        self.report(code, path, Problem::Text(message), None, None);
    }

    /// A problem at `path`: its parts, and its message in the host-neutral
    /// syntax. A value found within a `sensitive` field is withheld.
    fn report(
        &mut self,
        code: &'static str,
        path: &P,
        problem: Problem,
        found: Option<Shown>,
        suggestion: Option<String>,
    ) {
        let segments = path.segments();
        let found = if self.sensitive > 0 {
            found.map(|_| Shown::Sensitive)
        } else {
            found
        };
        let message = problem.sentence(
            &segments,
            found.as_ref(),
            suggestion.as_deref(),
            &crate::problem::Neutral,
        );
        self.push(ValueError {
            code,
            path: path.render(),
            message,
            segments,
            problem: Some(problem),
            found,
            suggestion,
            ..ValueError::default()
        });
    }

    fn form(&mut self, f: Form, path: &P) {
        let suffix = P::Raw(path, &f.suffix);
        let p = if f.suffix.is_empty() { *path } else { suffix };
        let (problem, found) = match f.detail {
            Some(d) => *d,
            None => (Problem::Text(f.message), None),
        };
        self.report(f.code, &p, problem, found, None);
    }

    fn missing(&mut self, path: &P) {
        self.report("missing_value", path, Problem::Present, None, None);
    }

    fn regex(&mut self, pattern: &str) -> Option<&Regex> {
        if !self.cache.patterns.contains_key(pattern) {
            self.cache
                .patterns
                .insert(pattern.to_string(), compiled_pattern(pattern));
        }
        self.cache.patterns[pattern].as_ref()
    }

    /// Checks a value of a field type. An absent value is reported as a
    /// missing value; callers handle absence where the type admits it.
    fn value<'a, A: Access<'a>>(&mut self, ft: &FieldType, v: A, path: &P) {
        match ft {
            FieldType::Option { inner, .. } => {
                if !v.is_absent() {
                    self.value(inner, v, path);
                }
            }
            _ if v.is_absent() => self.missing(path),
            FieldType::Logical => {
                if let Err(f) = v.logical() {
                    self.form(f, path);
                }
            }
            FieldType::Integer(c) => match v.integer() {
                Ok(x) if integer_ok(c, x) => {}
                Ok(x) => self.integer(c, x, path),
                Err(f) => self.form(f, path),
            },
            FieldType::Double(c) => match v.double() {
                Ok(x) if double_ok(c, x) => {}
                Ok(x) => self.double(c, x, path),
                Err(f) => self.form(f, path),
            },
            FieldType::Character(c) => match v.character() {
                Ok(s) => self.character(c, &s, path),
                Err(f) => self.form(f, path),
            },
            FieldType::Raw => {
                if let Err(f) = v.raw() {
                    self.form(f, path);
                }
            }
            FieldType::Date => match v.date() {
                Ok(d) => self.date(d, path),
                Err(f) => self.form(f, path),
            },
            FieldType::Posixct => match v.datetime() {
                Ok(m) => self.datetime(m, path),
                Err(f) => self.form(f, path),
            },
            FieldType::Factor { levels, ordered } => {
                self.factor(levels.as_deref(), *ordered, false, v, path);
            }
            FieldType::Matrix {
                element,
                nrow,
                ncol,
            } => self.dense(element, v, path, Some([*nrow, *ncol]), None),
            FieldType::Array { element, dim } => self.dense(element, v, path, None, dim.as_deref()),
            FieldType::SparseMatrix {
                element,
                nrow,
                ncol,
            } => self.sparse(element, v, path, *nrow, *ncol),
            FieldType::MatrixOf { of } => {
                let lookup = self.lookup;
                match lookup.find(of) {
                    Some(TypeDef::Struct(s)) => {
                        let element = s.fields.first().map_or(&FieldType::Logical, |f| &f.ty);
                        let n = s.fields.len() as u32;
                        self.dense(element, v, path, Some([None, Some(n)]), None);
                    }
                    _ => self.unresolved(of, path),
                }
            }
            FieldType::Table {
                columns,
                open,
                rules,
            } => self.table(columns, *open, rules, v, path),
            FieldType::TableOf { of } => {
                let lookup = self.lookup;
                match lookup.find(of) {
                    Some(TypeDef::Struct(s)) => self.table(&s.fields, false, &[], v, path),
                    _ => self.unresolved(of, path),
                }
            }
            FieldType::Json => {
                if let Err(f) = v.json() {
                    self.form(f, path);
                }
            }
            FieldType::R { .. } => {
                if let Err(f) = v.runtime() {
                    self.form(f, path);
                }
            }
            FieldType::Vec {
                element,
                min_length,
                max_length,
                unique,
            } => {
                let n = match v.vec(element) {
                    Ok(n) => n,
                    Err(f) => return self.form(f, path),
                };
                self.lengths(n, *min_length, *max_length, path);
                self.sequence(element, v, n, path, false, *unique);
            }
            FieldType::Hashmap { value, keys } => self.map(value, keys, false, v, path),
            FieldType::OrderedMap { value, keys } => self.map(value, keys, true, v, path),
            FieldType::Named { name } => self.named(name, v, path, &[]),
            FieldType::Dyn { r#trait } => self.dynamic(r#trait, v, path),
        }
    }

    /// Checks items `0..n` of a sequence of `element`. `cells_optional`
    /// admits a missing item, as in an `option()` table column; `unique`
    /// requires distinct present items.
    fn sequence<'a, A: Access<'a>>(
        &mut self,
        element: &FieldType,
        v: A,
        n: usize,
        path: &P,
        cells_optional: bool,
        unique: bool,
    ) {
        let (inner, optional) = unwrap_option(element);
        let optional = optional || cells_optional;
        if !is_scalar(inner) {
            for i in 0..n {
                let item = v.item(i);
                if !(cells_optional && item.is_absent()) {
                    self.value(element, item, &P::Idx(path, i));
                }
            }
            if unique {
                self.distinct_values(element, v, n, path);
            }
            return;
        }
        // A sequence held as one array, or read by region in chunks, is
        // scanned first; items are read one by one only when the scan finds a
        // problem.
        if !unique && scan_clean(inner, optional, v, n) {
            return;
        }
        let mut seen: HashSet<Key> = HashSet::new();
        let mut dup = |this: &mut Self, i: usize, k: Key, shown: &dyn Fn() -> String| {
            if !seen.insert(k) {
                this.error(
                    "not_unique",
                    &P::Idx(path, i),
                    format!("{} appears more than once", shown()),
                );
            }
        };
        match inner {
            FieldType::Double(c) => v.each_double(n, |i, r| match r {
                Ok(Some(x)) => {
                    if !double_ok(c, x) {
                        self.double(c, x, &P::Idx(path, i));
                    }
                    if unique {
                        dup(self, i, number_key(x), &|| x.to_string());
                    }
                }
                Ok(None) if optional => {}
                Ok(None) => self.missing(&P::Idx(path, i)),
                Err(f) => self.form(f, &P::Idx(path, i)),
            }),
            FieldType::Integer(c) => v.each_integer(n, |i, r| match r {
                Ok(Some(x)) => {
                    if !integer_ok(c, x) {
                        self.integer(c, x, &P::Idx(path, i));
                    }
                    if unique {
                        dup(self, i, number_key(x as f64), &|| x.to_string());
                    }
                }
                Ok(None) if optional => {}
                Ok(None) => self.missing(&P::Idx(path, i)),
                Err(f) => self.form(f, &P::Idx(path, i)),
            }),
            FieldType::Logical => v.each_logical(n, |i, r| match r {
                Ok(Some(b)) => {
                    if unique {
                        dup(self, i, Key::B(b), &|| b.to_string());
                    }
                }
                Ok(None) if optional => {}
                Ok(None) => self.missing(&P::Idx(path, i)),
                Err(f) => self.form(f, &P::Idx(path, i)),
            }),
            FieldType::Character(c) => v.each_character(n, |i, r| match r {
                Ok(Some(s)) => {
                    self.character(c, &s, &P::Idx(path, i));
                    if unique {
                        let shown = format!("\"{s}\"");
                        dup(self, i, Key::S(s.into_owned()), &|| shown.clone());
                    }
                }
                Ok(None) if optional => {}
                Ok(None) => self.missing(&P::Idx(path, i)),
                Err(f) => self.form(f, &P::Idx(path, i)),
            }),
            FieldType::Date => v.each_date(n, |i, r| match r {
                Ok(Some(d)) => {
                    if !(DATE_MIN_DAYS..=DATE_MAX_DAYS).contains(&d) {
                        self.date(d, &P::Idx(path, i));
                    }
                    if unique {
                        dup(self, i, Key::N(d as u64), &|| crate::codec::date_text(d));
                    }
                }
                Ok(None) if optional => {}
                Ok(None) => self.missing(&P::Idx(path, i)),
                Err(f) => self.form(f, &P::Idx(path, i)),
            }),
            FieldType::Posixct => v.each_datetime(n, |i, r| match r {
                Ok(Some(m)) => {
                    if !(-POSIXCT_MICROS..=POSIXCT_MICROS).contains(&m) {
                        self.datetime(m, &P::Idx(path, i));
                    }
                    if unique {
                        dup(self, i, Key::N(m as u64), &|| {
                            crate::codec::datetime_text(m)
                        });
                    }
                }
                Ok(None) if optional => {}
                Ok(None) => self.missing(&P::Idx(path, i)),
                Err(f) => self.form(f, &P::Idx(path, i)),
            }),
            _ => unreachable!("is_scalar"),
        }
    }

    /// Distinct items of a sequence of non-scalar values, by the SHA-256
    /// digest of each item's canonical form, streamed from the value: one
    /// pass, 32 bytes per item.
    fn distinct_values<'a, A: Access<'a>>(
        &mut self,
        element: &FieldType,
        v: A,
        n: usize,
        path: &P,
    ) {
        use sha2::{Digest, Sha256};
        let mut seen: HashMap<[u8; 32], usize> = HashMap::with_capacity(n);
        for i in 0..n {
            let item = v.item(i);
            if item.is_absent() {
                continue;
            }
            let mut e = crate::emit::Emitter::new(self.lookup, Sha256::new(), true, true);
            e.value(element, item);
            let key: [u8; 32] = e.w.out.finalize().into();
            if let Some(&first) = seen.get(&key) {
                let first = P::Idx(path, first).segments();
                self.report(
                    "not_unique",
                    &P::Idx(path, i),
                    Problem::Repeats { first },
                    None,
                    None,
                );
            } else {
                seen.insert(key, i);
            }
        }
    }

    fn map<'a, A: Access<'a>>(
        &mut self,
        value: &FieldType,
        keys: &CharacterType,
        ordered: bool,
        v: A,
        path: &P,
    ) {
        if let Err(f) = v.map(value, ordered) {
            return self.form(f, path);
        }
        let mut seen: HashMap<String, usize> = HashMap::new();
        v.each_entry(|i, entry| {
            let at = P::Idx(path, i);
            let (k, x) = match entry {
                Ok(e) => e,
                Err(f) => return self.form(f, &at),
            };
            if ordered {
                let name = P::Raw(&at, "/name");
                if let Some(&first) = seen.get(k.as_ref()) {
                    let first = P::Raw(&P::Idx(path, first), "/name").segments();
                    self.report(
                        "duplicate_key",
                        &name,
                        Problem::Repeats { first },
                        None,
                        None,
                    );
                } else {
                    seen.insert(k.to_string(), i);
                }
                self.key(keys, &k, &name);
                self.value(value, x, &P::Raw(&at, "/value"));
            } else {
                let p = P::MapKey(path, &k);
                self.key(keys, &k, &p);
                self.value(value, x, &p);
            }
        });
    }

    fn unresolved(&mut self, name: &QualifiedName, path: &P) {
        self.report(
            "unresolved_type",
            path,
            Problem::Unresolved(name.to_string()),
            None,
            None,
        );
    }

    fn integer(&mut self, c: &IntegerType, x: i64, path: &P) {
        if !(INT_MIN..=INT_MAX).contains(&x) {
            return self.report(
                "out_of_range",
                path,
                Problem::IntegerRange,
                Some(Shown::number(x as f64)),
                None,
            );
        }
        let x = x as i32;
        self.bounds(
            f64::from(x),
            c.min().map(f64::from),
            c.max().map(f64::from),
            c.exclusive_min().map(f64::from),
            c.exclusive_max().map(f64::from),
            path,
        );
        if let Some(values) = &c.values
            && !values.contains(&x)
        {
            let (values, total) = listed(values);
            let values = values.into_iter().map(|v| Lit::Num(f64::from(v))).collect();
            self.report(
                "not_in_enum",
                path,
                Problem::OneOf { values, total },
                Some(Shown::number(f64::from(x))),
                None,
            );
        }
    }

    fn double(&mut self, c: &DoubleType, x: f64, path: &P) {
        if c.finite && !x.is_finite() {
            return self.report(
                "not_finite",
                path,
                Problem::Finite,
                Some(Shown::number(x)),
                None,
            );
        }
        self.bounds(
            x,
            c.min(),
            c.max(),
            c.exclusive_min(),
            c.exclusive_max(),
            path,
        );
        if let Some(values) = &c.values
            && !values.contains(&x)
        {
            let (values, total) = listed(values);
            let values = values.into_iter().map(Lit::Num).collect();
            self.report(
                "not_in_enum",
                path,
                Problem::OneOf { values, total },
                Some(Shown::number(x)),
                None,
            );
        }
    }

    /// Bounds; `NaN` satisfies none.
    fn bounds(
        &mut self,
        x: f64,
        min: Option<f64>,
        max: Option<f64>,
        emin: Option<f64>,
        emax: Option<f64>,
        path: &P,
    ) {
        if let Some(m) = min
            && (x.is_nan() || x < m)
        {
            self.report(
                "below_min",
                path,
                Problem::AtLeast(Lit::Num(m)),
                Some(Shown::number(x)),
                None,
            );
        }
        if let Some(m) = max
            && (x.is_nan() || x > m)
        {
            self.report(
                "above_max",
                path,
                Problem::AtMost(Lit::Num(m)),
                Some(Shown::number(x)),
                None,
            );
        }
        if let Some(m) = emin
            && (x.is_nan() || x <= m)
        {
            self.report(
                "below_min",
                path,
                Problem::Above(Lit::Num(m)),
                Some(Shown::number(x)),
                None,
            );
        }
        if let Some(m) = emax
            && (x.is_nan() || x >= m)
        {
            self.report(
                "above_max",
                path,
                Problem::Below(Lit::Num(m)),
                Some(Shown::number(x)),
                None,
            );
        }
    }

    fn character(&mut self, c: &CharacterType, s: &str, path: &P) {
        if s.contains('\0') {
            return self.report("nul_character", path, Problem::NoNul, None, None);
        }
        self.string_constraints(c, s, path);
    }

    fn date(&mut self, d: i64, path: &P) {
        if !(DATE_MIN_DAYS..=DATE_MAX_DAYS).contains(&d) {
            self.report("invalid_date", path, Problem::DateRange, None, None);
        }
    }

    fn datetime(&mut self, m: i64, path: &P) {
        if !(-POSIXCT_MICROS..=POSIXCT_MICROS).contains(&m) {
            self.report("invalid_datetime", path, Problem::DateTimeRange, None, None);
        }
    }

    fn string_constraints(&mut self, c: &CharacterType, s: &str, path: &P) {
        if let Some(p) = &c.pattern
            && let Some(re) = self.regex(p)
            && !re.is_match(s)
        {
            let p = p.to_string();
            self.report(
                "pattern_mismatch",
                path,
                Problem::Pattern(p),
                Some(Shown::string(s)),
                None,
            );
        }
        if let Some(values) = &c.values
            && !values.iter().any(|x| &**x == s)
        {
            let suggestion =
                crate::name::nearest(s, values.iter().map(|v| &**v)).map(str::to_string);
            let (values, total) = listed(values);
            let values = values
                .into_iter()
                .map(|v| Lit::Str(v.to_string()))
                .collect();
            self.report(
                "not_in_enum",
                path,
                Problem::OneOf { values, total },
                Some(Shown::string(s)),
                suggestion,
            );
        }
    }

    fn key(&mut self, keys: &CharacterType, k: &str, path: &P) {
        if k.is_empty() {
            return self.report("invalid_key", path, Problem::NonEmptyKey, None, None);
        }
        self.string_constraints(keys, k, path);
    }

    fn lengths(&mut self, n: usize, min: Option<u64>, max: Option<u64>, path: &P) {
        if let Some(m) = min
            && (n as u64) < m
        {
            self.report(
                "length_below_min",
                path,
                Problem::LengthAtLeast(m),
                Some(Shown::Length(n as u64)),
                None,
            );
        }
        if let Some(m) = max
            && (n as u64) > m
        {
            self.report(
                "length_above_max",
                path,
                Problem::LengthAtMost(m),
                Some(Shown::Length(n as u64)),
                None,
            );
        }
    }

    /// A factor. With declared levels, a cell's path is its index; with
    /// levels from the data, its index under `codes`. `cells_optional` admits
    /// a missing cell, as in an `option()` table column.
    fn factor<'a, A: Access<'a>>(
        &mut self,
        declared: Option<&[Box<str>]>,
        ordered: bool,
        cells_optional: bool,
        v: A,
        path: &P,
    ) -> Option<usize> {
        let f = match v.factor(declared, ordered) {
            Ok(f) => f,
            Err(e) => {
                self.form(e, path);
                return None;
            }
        };
        let problems = f.problems;
        for e in problems {
            self.form(e, path);
        }
        let codes_path = P::Raw(path, "/codes");
        let cell_base = if declared.is_some() {
            path
        } else {
            &codes_path
        };
        if declared.is_none() {
            let lp = P::Raw(path, "/levels");
            let mut seen: HashMap<&str, usize> = HashMap::new();
            for (i, l) in f.levels.iter().enumerate() {
                if let Some(&first) = seen.get(&**l) {
                    let first = P::Idx(&lp, first).segments();
                    self.report(
                        "not_unique",
                        &P::Idx(&lp, i),
                        Problem::Repeats { first },
                        None,
                        None,
                    );
                } else {
                    seen.insert(l, i);
                }
            }
        }
        let known: Vec<bool> = match declared {
            Some(d) => {
                let d: HashSet<&str> = d.iter().map(|l| &**l).collect();
                f.levels.iter().map(|l| d.contains(l.as_ref())).collect()
            }
            None => vec![true; f.levels.len()],
        };
        let all_known = known.iter().all(|&k| k);
        let k = f.levels.len();
        // Codes read from another form mark a cell whose problem is among
        // `problems` with 0; a host's code 0 is out of range.
        let reported = matches!(f.codes, Codes::Owned(_));
        let n = f.codes.len();
        let mut buf = vec![0i32; n.min(4096)];
        let mut start = 0;
        while start < n {
            let got = f.codes.read(start, &mut buf);
            if got == 0 {
                break;
            }
            let chunk = &buf[..got];
            // One test per cell for the common case: every code in range
            // and its level known.
            let clean = chunk.iter().all(|&c| {
                (c as u32).wrapping_sub(1) < k as u32 && (all_known || known[c as usize - 1])
                    || (c == MISSING_CODE && cells_optional)
            });
            if !clean {
                for (j, &c) in chunk.iter().enumerate() {
                    let i = start + j;
                    match c {
                        MISSING_CODE if cells_optional => {}
                        MISSING_CODE if declared.is_some() => self.report(
                            "type_mismatch",
                            &P::Idx(cell_base, i),
                            Problem::Present,
                            None,
                            None,
                        ),
                        0 if reported => {}
                        c if (c as u32).wrapping_sub(1) < k as u32 => {
                            if !known[c as usize - 1] {
                                let found = &*f.levels[c as usize - 1];
                                let all = declared.unwrap_or_default();
                                let suggestion =
                                    crate::name::nearest(found, all.iter().map(|l| &**l))
                                        .map(str::to_string);
                                let (levels, total) = listed(all);
                                let levels = levels
                                    .into_iter()
                                    .map(|l| Lit::Str(l.to_string()))
                                    .collect();
                                self.report(
                                    "not_a_level",
                                    &P::Idx(cell_base, i),
                                    Problem::Level { levels, total },
                                    Some(Shown::string(found)),
                                    suggestion,
                                );
                            }
                        }
                        _ => self.report(
                            "invalid_code",
                            &P::Idx(cell_base, i),
                            Problem::Code(k as u64),
                            Some(Shown::number(f64::from(c))),
                            None,
                        ),
                    }
                }
            }
            start += got;
        }
        Some(n)
    }

    fn dense<'a, A: Access<'a>>(
        &mut self,
        element: &FieldType,
        v: A,
        path: &P,
        matrix: Option<[Option<u32>; 2]>,
        dim: Option<&[u32]>,
    ) {
        let d = match v.dense(element) {
            Ok(d) => d,
            Err(f) => return self.form(f, path),
        };
        let shape = &d.shape;
        for f in d.problems {
            self.form(f, path);
        }
        if let Some([nrow, ncol]) = matrix {
            if shape.len() != 2 {
                return self.error(
                    "matrix_form",
                    &P::Raw(path, "/shape"),
                    "a matrix has two dimensions".to_string(),
                );
            }
            for (i, want) in [nrow, ncol].into_iter().enumerate() {
                if let Some(w) = want
                    && shape[i] != u64::from(w)
                {
                    self.error(
                        "dimension_mismatch",
                        &P::Idx(&P::Raw(path, "/shape"), i),
                        format!("dimension {} is {}, declared {w}", i + 1, shape[i]),
                    );
                }
            }
        }
        if let Some(dim) = dim
            && *shape != dim.iter().map(|&x| u64::from(x)).collect::<Vec<_>>()
        {
            self.error(
                "dimension_mismatch",
                &P::Raw(path, "/shape"),
                format!("shape {shape:?} differs from the declared {dim:?}"),
            );
        }
        let n: u64 = shape.iter().product();
        let data = P::Raw(path, "/data");
        if d.len as u64 != n {
            return self.error(
                "matrix_form",
                &data,
                format!(
                    "`data` holds {} values, the shape {shape:?} needs {n}",
                    d.len
                ),
            );
        }
        self.sequence(element, d.data, d.len, &data, false, false);
    }

    fn sparse<'a, A: Access<'a>>(
        &mut self,
        element: &FieldType,
        v: A,
        path: &P,
        nrow: Option<u32>,
        ncol: Option<u32>,
    ) {
        let s = match v.sparse(element) {
            Ok(s) => s,
            Err(f) => return self.form(f, path),
        };
        if !csc_well_formed(s.shape, &s.indptr, &s.indices, s.len as u64) {
            return self.form(Form::new("sparse_form", CSC_FORM), path);
        }
        for (i, want) in [nrow, ncol].into_iter().enumerate() {
            if let Some(w) = want
                && s.shape[i] != u64::from(w)
            {
                self.error(
                    "dimension_mismatch",
                    &P::Idx(&P::Raw(path, "/shape"), i),
                    format!("dimension {} is {}, declared {w}", i + 1, s.shape[i]),
                );
            }
        }
        self.sequence(element, s.data, s.len, &P::Raw(path, "/data"), false, false);
    }

    fn table<'a, A: Access<'a>>(
        &mut self,
        columns: &[FieldDef],
        open: bool,
        rules: &[RuleDef],
        v: A,
        path: &P,
    ) {
        let t = match v.table(columns, open) {
            Ok(t) => t,
            Err(f) => return self.form(f, path),
        };
        let before = self.count;
        for f in t.problems {
            self.form(f, path);
        }
        let n = t.rows;
        let present = |name: &str| t.columns.iter().find(|(k, _)| k == name).map(|(_, c)| *c);
        let mut all_columns: Vec<(FieldDef, A)> = Vec::new();
        for c in columns {
            match present(c.name.as_str()) {
                Some(cells) => all_columns.push((c.clone(), cells)),
                None if c.optional => {}
                None => self.report(
                    "missing_column",
                    &P::Col(path, c.name.as_str()),
                    Problem::Required,
                    None,
                    None,
                ),
            }
        }
        for (k, cells) in &t.columns {
            if columns.iter().any(|c| c.name.as_str() == k.as_ref()) {
                continue;
            }
            if !open {
                let suggestion = crate::name::nearest(k, columns.iter().map(|c| c.name.as_str()))
                    .map(str::to_string);
                let names = if columns.len() <= crate::problem::LISTED_NAMES {
                    columns.iter().map(|c| c.name.to_string()).collect()
                } else {
                    Vec::new()
                };
                self.report(
                    "unknown_column",
                    &P::Col(path, k),
                    Problem::UnknownColumn { names },
                    None,
                    suggestion,
                );
                continue;
            }
            match (v.extra_column_type(k), Name::new(k.to_string())) {
                (_, Err(e)) => self.error("unknown_column", &P::Col(path, k), e.to_string()),
                (Ok(ft), Ok(name)) if crate::ir::is_column_type(&ft) => all_columns.push((
                    FieldDef {
                        name,
                        ty: ft,
                        default: None,
                        description: Box::default(),
                        sensitive: false,
                        unit: None,
                        external: false,
                        unique: false,
                        optional: false,
                        inherited_from: None,
                        extensions: Default::default(),
                    },
                    *cells,
                )),
                (Ok(_), Ok(_)) => self.error(
                    "column_type",
                    &P::Raw(&P::Raw(path, "/$columns"), &format!("/{}", esc(k))),
                    format!("column `{k}` of an open table needs its type in `$columns`: a scalar, a factor, or option() of one"),
                ),
                (Err(f), Ok(_)) => self.form(f, path),
            }
        }
        for (c, cells) in &all_columns {
            let p = P::Col(path, c.name.as_str());
            let (inner, optional) = unwrap_option(&c.ty);
            if let FieldType::Factor { levels, ordered } = inner {
                if let Some(len) = self.factor(levels.as_deref(), *ordered, optional, *cells, &p)
                    && len as u64 != n
                {
                    self.report(
                        "column_length",
                        &p,
                        Problem::Cells(n),
                        Some(Shown::Length(len as u64)),
                        None,
                    );
                }
                continue;
            }
            let len = match cells.vec(inner) {
                Ok(len) => len,
                Err(f) => {
                    self.form(f, &p);
                    continue;
                }
            };
            if len as u64 != n {
                self.report(
                    "column_length",
                    &p,
                    Problem::Cells(n),
                    Some(Shown::Length(len as u64)),
                    None,
                );
                continue;
            }
            self.sequence(inner, *cells, len, &p, optional, c.unique);
        }
        if !rules.is_empty() && self.count == before {
            let scope: Vec<FieldDef> = all_columns.into_iter().map(|(c, _)| c).collect();
            for (ri, r) in rules.iter().enumerate() {
                if let Some(rule) = self.compiled(r, ri, &scope) {
                    match rule.check_rows_access(v, n as usize, &scope, self.lookup) {
                        Ok(paths) => {
                            for (k, p) in paths.into_iter().enumerate() {
                                // A row rule's path ends with the row.
                                let row = match p.last() {
                                    Some(Segment::Element(i)) => Some(*i as usize),
                                    _ => None,
                                };
                                let d = match row {
                                    Some(i) if k < DIAGNOSED => Some(diagnosis(
                                        r,
                                        rule.diagnose_access(v.row(i), &scope, self.lookup, &[]),
                                    )),
                                    _ => None,
                                };
                                self.violation(&rule, ri, path, p, d);
                            }
                        }
                        Err(e) => self.rule_error(&rule, ri, &path.render(), e),
                    }
                }
            }
        }
    }

    fn compiled(
        &mut self,
        r: &RuleDef,
        index: usize,
        scope: &[FieldDef],
    ) -> Option<Arc<CompiledRule>> {
        let c = self.cache.compiled(r, scope, self.lookup);
        if c.is_none() {
            self.error(
                "rule_invalid",
                &P::Root(""),
                format!("rule {index} does not compile; check the declaration"),
            );
        }
        c
    }

    fn violation(
        &mut self,
        rule: &CompiledRule,
        index: usize,
        base: &P,
        rel: Vec<Segment>,
        diagnosis: Option<Box<crate::rules::Diagnosis>>,
    ) {
        let mut segments = base.segments();
        segments.extend(rel);
        self.push(ValueError {
            code: "rule_violation",
            path: crate::problem::pointer(&segments),
            segments,
            rule: rule.id.clone(),
            rule_index: Some(index),
            message: rule.message.clone().unwrap_or_default(),
            diagnosis,
            ..Default::default()
        });
    }

    fn rule_error(&mut self, rule: &CompiledRule, index: usize, path: &str, e: String) {
        self.push(ValueError {
            code: "rule_error",
            path: path.to_string(),
            rule: rule.id.clone(),
            rule_index: Some(index),
            message: e,
            diagnosis: None,
            ..Default::default()
        });
    }

    /// Checks a record against fields, with `allowed` extra members, then
    /// the rules, when the fields themselves hold no error.
    fn object<'a, A: Access<'a>>(
        &mut self,
        owner: &str,
        fields: &[FieldDef],
        rules: &[RuleDef],
        v: A,
        path: &P,
        allowed: &[&str],
    ) {
        let members = v.members();
        let in_order = members.len() <= fields.len()
            && members
                .iter()
                .zip(fields)
                .all(|(k, f)| k == f.name.as_str());
        if !in_order {
            for k in members {
                if !allowed.contains(&k.as_ref()) && !fields.iter().any(|f| f.name.as_str() == k) {
                    let suggestion =
                        crate::name::nearest(&k, fields.iter().map(|f| f.name.as_str()))
                            .map(str::to_string);
                    let names = if fields.len() <= crate::problem::LISTED_NAMES {
                        fields.iter().map(|f| f.name.to_string()).collect()
                    } else {
                        Vec::new()
                    };
                    self.report(
                        "unknown_field",
                        &P::Key(path, &k),
                        Problem::UnknownField {
                            of: owner.to_string(),
                            names,
                        },
                        None,
                        suggestion,
                    );
                }
            }
        }
        let before = self.count;
        for (index, f) in fields.iter().enumerate() {
            let x = v.member_at(index, f.name.as_str());
            if x.is_absent() {
                let admits_absence =
                    matches!(f.ty, FieldType::Option { .. }) || f.default.is_some();
                if !admits_absence {
                    self.report(
                        "missing_field",
                        &P::Key(path, f.name.as_str()),
                        Problem::Required,
                        None,
                        None,
                    );
                }
            } else {
                self.sensitive += u32::from(f.sensitive);
                self.value(&f.ty, x, &P::Key(path, f.name.as_str()));
                self.sensitive -= u32::from(f.sensitive);
            }
        }
        if self.count > before {
            return;
        }
        for (ri, r) in rules.iter().enumerate() {
            if let Some(rule) = self.compiled(r, ri, fields) {
                match rule.check_access(v, fields, self.lookup) {
                    Ok(Outcome::Holds) => {}
                    Ok(Outcome::Violated(paths)) => {
                        for (k, p) in paths.into_iter().enumerate() {
                            let d = (k < DIAGNOSED).then(|| {
                                diagnosis(r, rule.diagnose_access(v, fields, self.lookup, &p))
                            });
                            self.violation(&rule, ri, path, p, d);
                        }
                    }
                    Err(e) => self.rule_error(&rule, ri, &path.render(), e),
                }
            }
        }
    }

    fn named<'a, A: Access<'a>>(
        &mut self,
        name: &QualifiedName,
        v: A,
        path: &P,
        envelope: &[&str],
    ) {
        let lookup = self.lookup;
        match lookup.find(name) {
            None => self.unresolved(name, path),
            Some(TypeDef::Struct(s)) => match v.record(name) {
                Ok(()) => self.object(&name.to_string(), &s.fields, &s.rules, v, path, envelope),
                Err(f) => self.form(f, path),
            },
            Some(TypeDef::Enum(e)) => {
                let unit_only = e
                    .variants
                    .iter()
                    .all(|x| matches!(x.shape, VariantShape::Unit));
                let variant = match v.variant(name, e) {
                    Ok(s) => s,
                    Err(f) => return self.form(f, path),
                };
                let tag = e.tag.as_str();
                let Some(def) = e.variants.iter().find(|x| x.name.as_str() == variant) else {
                    // The subject is the enum value; the path reaches its tag.
                    let tag_suffix = format!("/{}", esc(tag));
                    let at = if unit_only {
                        *path
                    } else {
                        P::Raw(path, &tag_suffix)
                    };
                    let names: Vec<&str> = e.variants.iter().map(|x| x.name.as_str()).collect();
                    let suggestion =
                        crate::name::nearest(&variant, names.iter().copied()).map(str::to_string);
                    let (variants, total) = listed(&names);
                    let variants = variants
                        .into_iter()
                        .map(|v| Lit::Str(v.to_string()))
                        .collect();
                    return self.report(
                        "unknown_variant",
                        &at,
                        Problem::Variant {
                            of: name.to_string(),
                            variants,
                            total,
                        },
                        Some(Shown::string(&variant)),
                        suggestion,
                    );
                };
                if unit_only {
                    return;
                }
                let mut allowed: Vec<&str> = envelope.to_vec();
                allowed.push(tag);
                match &def.shape {
                    VariantShape::Unit => {
                        self.object(&format!("{name}@{variant}"), &[], &[], v, path, &allowed)
                    }
                    VariantShape::Struct { fields, rules } => self.object(
                        &format!("{name}@{variant}"),
                        fields,
                        rules,
                        v,
                        path,
                        &allowed,
                    ),
                    VariantShape::Newtype { of } => match lookup.find(of) {
                        Some(TypeDef::Struct(s)) => self.object(
                            &format!("{name}@{variant}"),
                            &s.fields,
                            &s.rules,
                            v,
                            path,
                            &allowed,
                        ),
                        _ => self.unresolved(of, path),
                    },
                }
            }
        }
    }

    fn dynamic<'a, A: Access<'a>>(&mut self, tr: &QualifiedName, v: A, path: &P) {
        match v.implementor(tr, self.lookup) {
            // The type member names the type; the rest is the type's value.
            Ok((ty, member)) => match &member {
                Some(m) => self.named(&ty, v, path, &[m.as_str()]),
                None => self.named(&ty, v, path, &[]),
            },
            Err(msg) => self.error("unresolved_type", path, msg),
        }
    }
}

/// Validates a value in any host form against a field type, as a field's
/// default is checked; an absent value is valid for `option()` only.
pub fn validate_field_access<'a, A: Access<'a>>(
    value: A,
    ty: &FieldType,
    lookup: &dyn TypeLookup,
    cache: &mut RuleCache,
) -> Vec<ValueError> {
    let mut v = Validator::new(lookup, cache);
    v.value(ty, value, &P::Root(""));
    v.finish()
}

/// Validates a value in any host form against the named type and returns
/// every error; an empty vector means the value is valid.
pub fn validate_access<'a, A: Access<'a>>(
    value: A,
    ty: &QualifiedName,
    lookup: &dyn TypeLookup,
    cache: &mut RuleCache,
) -> Vec<ValueError> {
    let mut v = Validator::new(lookup, cache);
    v.named(ty, value, &P::Root(""), &[]);
    v.finish()
}

/// Validates a value's JSON form against a field type, as a field's default
/// is checked; `null` is valid for `option()` only.
pub fn validate_field(value: &Value, ty: &FieldType, lookup: &dyn TypeLookup) -> Vec<ValueError> {
    validate_field_access(Json::new(value), ty, lookup, &mut RuleCache::new())
}

/// Validates a value's JSON form against the named type and returns every
/// error; an empty vector means the value is valid. The document envelope is
/// accepted on the outermost object.
pub fn validate(value: &Value, ty: &QualifiedName, lookup: &dyn TypeLookup) -> Vec<ValueError> {
    validate_cached(value, ty, lookup, &mut RuleCache::new())
}

/// [`validate`] with compiled rules kept in `cache`, which is valid for
/// `lookup` while it is unchanged.
pub fn validate_cached(
    value: &Value,
    ty: &QualifiedName,
    lookup: &dyn TypeLookup,
    cache: &mut RuleCache,
) -> Vec<ValueError> {
    let mut v = Validator::new(lookup, cache);
    v.named(ty, Json::new(value), &P::Root(""), &ENVELOPE);
    v.finish()
}

/// The compiled form of the regular expression `pattern`, or `None` when it
/// does not compile, kept for the process: a pattern's compiled form depends
/// on its text alone, so every declaration check and validation that names
/// it shares one compilation.
pub fn compiled_pattern(pattern: &str) -> Option<Regex> {
    static PATTERNS: std::sync::OnceLock<std::sync::Mutex<HashMap<Box<str>, Option<Regex>>>> =
        std::sync::OnceLock::new();
    let m = PATTERNS.get_or_init(Default::default);
    let mut map = m.lock().unwrap_or_else(|e| e.into_inner());
    if let Some(r) = map.get(pattern) {
        return r.clone();
    }
    let r = Regex::new(pattern).ok();
    map.insert(pattern.into(), r.clone());
    r
}
