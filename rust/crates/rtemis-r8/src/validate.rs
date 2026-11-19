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
    Access, Codes, DATE_MAX_DAYS, DATE_MIN_DAYS, Form, Json, POSIXCT_MAX, POSIXCT_MICROS,
    POSIXCT_MIN,
};
use crate::ir::{
    CharacterType, DoubleType, FieldDef, FieldType, IntegerType, RuleDef, TypeDef, VariantShape,
};
use crate::name::{Name, QualifiedName};
use crate::rules::{CompiledRule, Outcome, TypeLookup};

/// A problem in a value.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ValueError {
    /// A stable code, such as `missing_field`.
    pub code: &'static str,
    /// A JSON Pointer into the value's JSON form.
    pub path: String,
    /// The ID of the violated rule, for `rule_violation` and `rule_error`.
    pub rule: Option<Name>,
    /// The message.
    pub message: String,
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

    /// Removes every compiled rule and pattern.
    pub fn clear(&mut self) {
        self.patterns.clear();
        self.rules.clear();
    }
}

/// A JSON Pointer, built as a chain and rendered for an error only.
#[derive(Clone, Copy)]
enum P<'p> {
    Root(&'p str),
    Key(&'p P<'p>, &'p str),
    Idx(&'p P<'p>, usize),
    /// A JSON Pointer suffix, already escaped.
    Raw(&'p P<'p>, &'p str),
}

impl P<'_> {
    fn render(&self) -> String {
        match self {
            P::Root(s) => s.to_string(),
            P::Key(p, k) => format!("{}/{}", p.render(), esc(k)),
            P::Idx(p, i) => format!("{}/{i}", p.render()),
            P::Raw(p, s) => format!("{}{s}", p.render()),
        }
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
        && c.min.is_none_or(|m| x >= m)
        && c.max.is_none_or(|m| x <= m)
        && c.exclusive_min.is_none_or(|m| x > m)
        && c.exclusive_max.is_none_or(|m| x < m)
        && c.values.as_ref().is_none_or(|v| v.contains(&x))
}

/// Whether a whole number satisfies the integer range and its constraints.
#[inline(always)]
fn integer_ok(c: &IntegerType, x: i64) -> bool {
    (INT_MIN..=INT_MAX).contains(&x) && {
        let y = x as i32;
        c.min.is_none_or(|m| y >= m)
            && c.max.is_none_or(|m| y <= m)
            && c.exclusive_min.is_none_or(|m| y > m)
            && c.exclusive_max.is_none_or(|m| y < m)
            && c.values.as_ref().is_none_or(|v| v.contains(&y))
    }
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
}

impl<'l, 'c> Validator<'l, 'c> {
    fn new(lookup: &'l dyn TypeLookup, cache: &'c mut RuleCache) -> Self {
        Validator {
            lookup,
            errors: Vec::new(),
            count: 0,
            cache,
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
                message: format!(
                    "the value has more than {ERROR_LIMIT} errors; the first {ERROR_LIMIT} are listed"
                ),
            };
            self.errors.push(limit);
        }
        self.errors
    }
}

impl<'l> Validator<'l, '_> {
    fn error(&mut self, code: &'static str, path: &P, message: String) {
        self.push(ValueError {
            code,
            path: path.render(),
            rule: None,
            message,
        });
    }

    fn form(&mut self, f: Form, path: &P) {
        self.push(ValueError {
            code: f.code,
            path: format!("{}{}", path.render(), f.suffix),
            rule: None,
            message: f.message,
        });
    }

    fn missing(&mut self, path: &P) {
        self.error(
            "missing_value",
            path,
            "a value is required here".to_string(),
        );
    }

    fn regex(&mut self, pattern: &str) -> Option<&Regex> {
        self.cache
            .patterns
            .entry(pattern.to_string())
            .or_insert_with(|| Regex::new(pattern).ok())
            .as_ref()
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
        // A sequence held as one array is scanned first; items are read one
        // by one only when the scan finds a problem.
        if !unique {
            let na_ok = |missing: bool, ok: bool| if missing { optional } else { ok };
            let clean = match (inner, v.f64_slice(), v.i32_slice()) {
                (FieldType::Double(c), Some(s), _) => s[..n.min(s.len())]
                    .iter()
                    .all(|&x| na_ok(crate::access::is_na_real(x), double_ok(c, x))),
                (FieldType::Double(c), None, Some(s)) => s[..n.min(s.len())]
                    .iter()
                    .all(|&x| na_ok(x == i32::MIN, double_ok(c, f64::from(x)))),
                (FieldType::Integer(c), _, Some(s)) => s[..n.min(s.len())]
                    .iter()
                    .all(|&x| na_ok(x == i32::MIN, integer_ok(c, i64::from(x)))),
                (FieldType::Integer(c), Some(s), None) => s[..n.min(s.len())].iter().all(|&x| {
                    na_ok(
                        crate::access::is_na_real(x),
                        x.fract() == 0.0 && integer_ok(c, x as i64),
                    )
                }),
                _ => false,
            };
            if clean {
                return;
            }
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
                self.error(
                    "not_unique",
                    &P::Idx(path, i),
                    format!("the item at index {i} equals the item at index {first}"),
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
        let mut seen: HashSet<String> = HashSet::new();
        v.each_entry(|i, entry| {
            let at = P::Idx(path, i);
            let (k, x) = match entry {
                Ok(e) => e,
                Err(f) => return self.form(f, &at),
            };
            if ordered {
                let name = P::Raw(&at, "/name");
                if !seen.insert(k.to_string()) {
                    self.error("duplicate_key", &name, format!("key `{k}` appears twice"));
                }
                self.key(keys, &k, &name);
                self.value(value, x, &P::Raw(&at, "/value"));
            } else {
                let p = P::Key(path, &k);
                self.key(keys, &k, &p);
                self.value(value, x, &p);
            }
        });
    }

    fn unresolved(&mut self, name: &QualifiedName, path: &P) {
        self.error(
            "unresolved_type",
            path,
            format!("`{name}` is not registered"),
        );
    }

    fn integer(&mut self, c: &IntegerType, x: i64, path: &P) {
        if !(INT_MIN..=INT_MAX).contains(&x) {
            return self.error(
                "out_of_range",
                path,
                format!("{x} is outside the integer range, -2147483647 to 2147483647"),
            );
        }
        let x = x as i32;
        self.bounds(
            f64::from(x),
            c.min.map(f64::from),
            c.max.map(f64::from),
            c.exclusive_min.map(f64::from),
            c.exclusive_max.map(f64::from),
            path,
        );
        if let Some(values) = &c.values
            && !values.contains(&x)
        {
            self.error("not_in_enum", path, format!("{x} is not one of {values:?}"));
        }
    }

    fn double(&mut self, c: &DoubleType, x: f64, path: &P) {
        if c.finite && !x.is_finite() {
            let shown = if x.is_nan() {
                "NaN"
            } else if x > 0.0 {
                "Infinity"
            } else {
                "-Infinity"
            };
            return self.error(
                "not_finite",
                path,
                format!(
                    "`{shown}` is not finite; declare `type_double(finite = FALSE)` to admit it"
                ),
            );
        }
        self.bounds(x, c.min, c.max, c.exclusive_min, c.exclusive_max, path);
        if let Some(values) = &c.values
            && !values.contains(&x)
        {
            self.error("not_in_enum", path, format!("{x} is not one of {values:?}"));
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
            self.error("below_min", path, format!("{x} is below the minimum {m}"));
        }
        if let Some(m) = max
            && (x.is_nan() || x > m)
        {
            self.error("above_max", path, format!("{x} is above the maximum {m}"));
        }
        if let Some(m) = emin
            && (x.is_nan() || x <= m)
        {
            self.error("below_min", path, format!("{x} is not above {m}"));
        }
        if let Some(m) = emax
            && (x.is_nan() || x >= m)
        {
            self.error("above_max", path, format!("{x} is not below {m}"));
        }
    }

    fn character(&mut self, c: &CharacterType, s: &str, path: &P) {
        if s.contains('\0') {
            return self.error(
                "nul_character",
                path,
                "a string cannot hold U+0000, which R cannot represent".to_string(),
            );
        }
        self.string_constraints(c, s, path);
    }

    fn date(&mut self, d: i64, path: &P) {
        if !(DATE_MIN_DAYS..=DATE_MAX_DAYS).contains(&d) {
            self.error(
                "invalid_date",
                path,
                format!("{d} days since 1970-01-01 is outside the years 0000 to 9999"),
            );
        }
    }

    fn datetime(&mut self, m: i64, path: &P) {
        if !(-POSIXCT_MICROS..=POSIXCT_MICROS).contains(&m) {
            self.error(
                "invalid_datetime",
                path,
                format!("the date-time is outside {POSIXCT_MIN} to {POSIXCT_MAX}"),
            );
        }
    }

    fn string_constraints(&mut self, c: &CharacterType, s: &str, path: &P) {
        if let Some(p) = &c.pattern
            && let Some(re) = self.regex(p)
            && !re.is_match(s)
        {
            self.error(
                "pattern_mismatch",
                path,
                format!("`{s}` does not match `{p}`"),
            );
        }
        if let Some(values) = &c.values
            && !values.iter().any(|x| x == s)
        {
            self.error(
                "not_in_enum",
                path,
                format!("`{s}` is not one of {values:?}"),
            );
        }
    }

    fn key(&mut self, keys: &CharacterType, k: &str, path: &P) {
        if k.is_empty() {
            return self.error("invalid_key", path, "a map key is not empty".to_string());
        }
        self.string_constraints(keys, k, path);
    }

    fn lengths(&mut self, n: usize, min: Option<u64>, max: Option<u64>, path: &P) {
        if let Some(m) = min
            && (n as u64) < m
        {
            self.error(
                "length_below_min",
                path,
                format!("length {n} is below the minimum {m}"),
            );
        }
        if let Some(m) = max
            && (n as u64) > m
        {
            self.error(
                "length_above_max",
                path,
                format!("length {n} is above the maximum {m}"),
            );
        }
    }

    /// A factor. With declared levels, a cell's path is its index; with
    /// levels from the data, its index under `codes`. `cells_optional` admits
    /// a missing cell, as in an `option()` table column.
    fn factor<'a, A: Access<'a>>(
        &mut self,
        declared: Option<&[String]>,
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
            let mut seen: HashSet<&str> = HashSet::new();
            for (i, l) in f.levels.iter().enumerate() {
                if !seen.insert(l) {
                    self.error(
                        "not_unique",
                        &P::Idx(&lp, i),
                        format!("\"{l}\" appears more than once"),
                    );
                }
            }
        }
        let known: Vec<bool> = match declared {
            Some(d) => f.levels.iter().map(|l| d.iter().any(|x| x == l)).collect(),
            None => vec![true; f.levels.len()],
        };
        let n = f.codes.len();
        let fast = matches!(f.codes, Codes::I32(_)) && known.iter().all(|&k| k);
        for i in 0..n {
            match f.codes.get(i) {
                None if cells_optional => {}
                None if declared.is_some() => self.error(
                    "type_mismatch",
                    &P::Idx(cell_base, i),
                    "expected a level, found null".to_string(),
                ),
                None => self.error(
                    "invalid_code",
                    &P::Idx(cell_base, i),
                    format!("a code is an integer from 1 to {}", f.levels.len()),
                ),
                Some(_) if fast => {}
                Some(0) => {}
                Some(c) => match known.get((c as usize).wrapping_sub(1)) {
                    Some(true) => {}
                    Some(false) => self.error(
                        "not_a_level",
                        &P::Idx(cell_base, i),
                        format!(
                            "`{}` is not one of the levels {:?}",
                            f.levels[c as usize - 1],
                            declared.unwrap_or_default()
                        ),
                    ),
                    None => self.error(
                        "invalid_code",
                        &P::Idx(cell_base, i),
                        format!("a code is an integer from 1 to {}", f.levels.len()),
                    ),
                },
            }
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
                None => self.error(
                    "missing_column",
                    &P::Key(path, c.name.as_str()),
                    format!("column `{}` is required", c.name),
                ),
            }
        }
        for (k, cells) in &t.columns {
            if columns.iter().any(|c| c.name.as_str() == k.as_ref()) {
                continue;
            }
            if !open {
                self.error(
                    "unknown_column",
                    &P::Key(path, k),
                    format!("`{k}` is not a column of this table"),
                );
                continue;
            }
            match (v.extra_column_type(k), Name::new(k.to_string())) {
                (_, Err(e)) => self.error("unknown_column", &P::Key(path, k), e.to_string()),
                (Ok(ft), Ok(name)) if crate::ir::is_column_type(&ft) => all_columns.push((
                    FieldDef {
                        name,
                        ty: ft,
                        default: None,
                        description: String::new(),
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
                    &P::Key(&P::Raw(path, "/$columns"), k),
                    format!("column `{k}` of an open table needs its type in `$columns`: a scalar, a factor, or option() of one"),
                ),
                (Err(f), Ok(_)) => self.form(f, path),
            }
        }
        for (c, cells) in &all_columns {
            let p = P::Key(path, c.name.as_str());
            let (inner, optional) = unwrap_option(&c.ty);
            if let FieldType::Factor { levels, ordered } = inner {
                if let Some(len) = self.factor(levels.as_deref(), *ordered, optional, *cells, &p)
                    && len as u64 != n
                {
                    self.error(
                        "column_length",
                        &p,
                        format!(
                            "column `{}` has {len} cells, the table has {n} rows",
                            c.name
                        ),
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
                self.error(
                    "column_length",
                    &p,
                    format!(
                        "column `{}` has {len} cells, the table has {n} rows",
                        c.name
                    ),
                );
                continue;
            }
            self.sequence(inner, *cells, len, &p, optional, c.unique);
        }
        if !rules.is_empty() && self.count == before {
            let scope: Vec<FieldDef> = all_columns.into_iter().map(|(c, _)| c).collect();
            for r in rules {
                if let Some(rule) = self.compiled(r, &scope) {
                    match rule.check_rows_access(v, n as usize, &scope, self.lookup) {
                        Ok(paths) => {
                            for p in paths {
                                self.violation(&rule, &format!("{}{p}", path.render()));
                            }
                        }
                        Err(e) => self.rule_error(&rule, &path.render(), e),
                    }
                }
            }
        }
    }

    fn compiled(&mut self, r: &RuleDef, scope: &[FieldDef]) -> Option<Arc<CompiledRule>> {
        let key = r as *const RuleDef as usize;
        if !self.cache.rules.contains_key(&key) {
            let c = CompiledRule::compile(r, scope, self.lookup)
                .ok()
                .map(Arc::new);
            if c.is_none() {
                self.error(
                    "rule_invalid",
                    &P::Root(""),
                    format!("rule `{}` does not compile; check the declaration", r.id),
                );
            }
            self.cache.rules.insert(key, c);
        }
        self.cache.rules[&key].clone()
    }

    fn violation(&mut self, rule: &CompiledRule, path: &str) {
        self.push(ValueError {
            code: "rule_violation",
            path: path.to_string(),
            rule: Some(rule.id.clone()),
            message: rule.message.clone(),
        });
    }

    fn rule_error(&mut self, rule: &CompiledRule, path: &str, e: String) {
        self.push(ValueError {
            code: "rule_error",
            path: path.to_string(),
            rule: Some(rule.id.clone()),
            message: e,
        });
    }

    /// Checks a record against fields, with `allowed` extra members, then
    /// the rules, when the fields themselves hold no error.
    fn object<'a, A: Access<'a>>(
        &mut self,
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
                    self.error(
                        "unknown_field",
                        &P::Key(path, &k),
                        format!("`{k}` is not a field"),
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
                    self.error(
                        "missing_field",
                        &P::Key(path, f.name.as_str()),
                        format!("field `{}` is required", f.name),
                    );
                }
            } else {
                self.value(&f.ty, x, &P::Key(path, f.name.as_str()));
            }
        }
        if self.count > before {
            return;
        }
        for r in rules {
            if let Some(rule) = self.compiled(r, fields) {
                match rule.check_access(v, fields, self.lookup) {
                    Ok(Outcome::Holds) => {}
                    Ok(Outcome::Violated(paths)) => {
                        for p in paths {
                            self.violation(&rule, &format!("{}{p}", path.render()));
                        }
                    }
                    Err(e) => self.rule_error(&rule, &path.render(), e),
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
                Ok(()) => self.object(&s.fields, &s.rules, v, path, envelope),
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
                    let at = if unit_only { *path } else { P::Key(path, tag) };
                    return self.error(
                        "unknown_variant",
                        &at,
                        format!("`{variant}` is not a variant of `{name}`"),
                    );
                };
                if unit_only {
                    return;
                }
                let mut allowed: Vec<&str> = envelope.to_vec();
                allowed.push(tag);
                match &def.shape {
                    VariantShape::Unit => self.object(&[], &[], v, path, &allowed),
                    VariantShape::Struct { fields, rules } => {
                        self.object(fields, rules, v, path, &allowed)
                    }
                    VariantShape::Newtype { of } => match lookup.find(of) {
                        Some(TypeDef::Struct(s)) => {
                            self.object(&s.fields, &s.rules, v, path, &allowed)
                        }
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
    let mut cache = RuleCache::new();
    let mut v = Validator::new(lookup, &mut cache);
    v.named(ty, Json::new(value), &P::Root(""), &ENVELOPE);
    v.finish()
}
