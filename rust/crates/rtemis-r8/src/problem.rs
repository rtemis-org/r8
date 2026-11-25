//! The parts of a validation problem, from which each host writes its
//! message: the path as typed segments, the requirement or fact, the value
//! found, and the nearest name. The wording lives here, once; a host
//! supplies the subject and its literals in its own syntax, so every host
//! reads the same.
//! spec: R8/class-system#validation-messages

use crate::rules::Shown;

/// The number of allowed values a message lists before it counts the rest.
pub const LISTED: usize = 5;

/// The number of names an unknown-name message lists when none is near.
pub const LISTED_NAMES: usize = 8;

/// A number in its shortest decimal form that reads back exactly, as a
/// message writes a bound or an allowed value.
pub fn number_text(x: f64) -> String {
    crate::emit::number_text(x)
}

/// One step of a problem's path.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Segment {
    /// A field of a struct or an enum value.
    Field(String),
    /// A key of a `btree_map()`.
    Key(String),
    /// A column of a table.
    Column(String),
    /// An element of a vector, a table column, or an `index_map()`,
    /// 0-based.
    Element(u64),
    /// A part of a value's JSON form that the host writes another way, such
    /// as `/levels` or `/shape`, with its JSON Pointer suffix.
    Form(String),
}

/// The JSON Pointer of a path.
pub fn pointer(segments: &[Segment]) -> String {
    let esc = |s: &str| s.replace('~', "~0").replace('/', "~1");
    let mut out = String::new();
    for s in segments {
        match s {
            Segment::Field(k) | Segment::Key(k) | Segment::Column(k) => {
                out.push('/');
                out.push_str(&esc(k));
            }
            Segment::Element(i) => {
                out.push('/');
                out.push_str(&i.to_string());
            }
            Segment::Form(suffix) => out.push_str(suffix),
        }
    }
    out
}

/// A literal of a requirement: a bound, an allowed value, or a name.
#[derive(Debug, Clone, PartialEq)]
pub enum Lit {
    /// A number.
    Num(f64),
    /// A string.
    Str(String),
}

// The numbers of a declaration are finite, so equality is reflexive.
impl Eq for Lit {}

/// What a value fails: a requirement it must meet, or a fact about it.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Problem {
    /// At least the bound.
    AtLeast(Lit),
    /// Above the bound.
    Above(Lit),
    /// At most the bound.
    AtMost(Lit),
    /// Below the bound.
    Below(Lit),
    /// An integer in R's range.
    IntegerRange,
    /// A finite number.
    Finite,
    /// A number, which `NaN` is not.
    Number,
    /// One of the allowed values, of which `total` exist.
    OneOf {
        /// The first allowed values, at most [`LISTED`].
        values: Vec<Lit>,
        /// The number of allowed values.
        total: usize,
    },
    /// One of a factor's levels, of which `total` exist.
    Level {
        /// The first levels, at most [`LISTED`].
        levels: Vec<Lit>,
        /// The number of levels.
        total: usize,
    },
    /// A variant of the enum `of`.
    Variant {
        /// The enum's qualified name.
        of: String,
        /// The first variants, at most [`LISTED`].
        variants: Vec<Lit>,
        /// The number of variants.
        total: usize,
    },
    /// A match of the regular expression.
    Pattern(String),
    /// A length of at least the bound.
    LengthAtLeast(u64),
    /// A length of at most the bound.
    LengthAtMost(u64),
    /// A string without U+0000.
    NoNul,
    /// A date in the years 0000 to 9999.
    DateRange,
    /// A date-time in R8's range.
    DateTimeRange,
    /// The declared dimensions.
    Dims(Vec<u64>),
    /// One cell per row of a table of `rows` rows.
    Cells(u64),
    /// A code from 1 to the number of levels.
    Code(u64),
    /// A value, not a missing one.
    Present,
    /// A value of the kind, such as "a number".
    Kind(String),
    /// One value, as a scalar field holds.
    OneValue,
    /// A value that carries the attributes, which its type does not hold.
    Attributes(Vec<String>),
    /// A map key that is not empty.
    NonEmptyKey,
    /// A required field or column is absent.
    Required,
    /// A name that is not a field of `of`; `names` lists the fields when
    /// there are at most [`LISTED_NAMES`].
    UnknownField {
        /// The type's qualified name.
        of: String,
        /// The fields, when few.
        names: Vec<String>,
    },
    /// A name that is not a column of the table.
    UnknownColumn {
        /// The columns, when few.
        names: Vec<String>,
    },
    /// An element or a key that repeats an earlier one, at `first`.
    Repeats {
        /// The earlier element's path.
        first: Vec<Segment>,
    },
    /// A type the lookup does not hold.
    UnknownType(String),
    /// A problem of a value's form, as text.
    Text(String),
}

/// The parts of `values` that a message lists: the first [`LISTED`], and
/// their number.
pub fn listed<T: Clone>(values: &[T]) -> (Vec<T>, usize) {
    (values.iter().take(LISTED).cloned().collect(), values.len())
}

fn and_list(items: &[String], conj: &str) -> String {
    match items {
        [] => String::new(),
        [a] => a.clone(),
        [a, b] => format!("{a} {conj} {b}"),
        _ => format!(
            "{}, {conj} {}",
            items[..items.len() - 1].join(", "),
            items[items.len() - 1]
        ),
    }
}

/// The allowed values of a message: all of them, or the first and the
/// number of the rest.
fn one_of(values: &[Lit], total: usize, lit: &dyn Fn(&Lit) -> String) -> String {
    let items: Vec<String> = values.iter().map(lit).collect();
    if total > items.len() {
        format!("{}, and {} more", items.join(", "), total - items.len())
    } else {
        and_list(&items, "or")
    }
}

/// How a host writes the parts of a message.
pub trait Syntax {
    /// A path, as the subject of a sentence, quoted as code.
    fn path(&self, segments: &[Segment]) -> String;
    /// A literal.
    fn lit(&self, x: &Lit) -> String;
    /// A value found.
    fn shown(&self, x: &Shown) -> String;
    /// A field or column name, quoted as code.
    fn name(&self, n: &str) -> String;
}

impl Problem {
    /// The requirement a value must meet, as a verb phrase ("be at least
    /// 0"); `None` for a fact.
    pub fn requirement(&self, s: &dyn Syntax) -> Option<String> {
        let lit = |x: &Lit| s.lit(x);
        Some(match self {
            Problem::AtLeast(b) => format!("be at least {}", lit(b)),
            Problem::Above(b) => format!("be above {}", lit(b)),
            Problem::AtMost(b) => format!("be at most {}", lit(b)),
            Problem::Below(b) => format!("be below {}", lit(b)),
            Problem::IntegerRange => "be an integer from -2147483647 to 2147483647".to_string(),
            Problem::Finite => "be finite".to_string(),
            Problem::Number => "be a number".to_string(),
            Problem::OneOf { values, total } => {
                format!("be one of {}", one_of(values, *total, &lit))
            }
            Problem::Level { levels, total } => {
                format!("be one of the levels {}", one_of(levels, *total, &lit))
            }
            Problem::Variant {
                of,
                variants,
                total,
            } => format!("be a variant of `{of}`: {}", one_of(variants, *total, &lit)),
            Problem::Pattern(p) => format!("match {}", lit(&Lit::Str(p.clone()))),
            Problem::LengthAtLeast(n) => format!("have length at least {n}"),
            Problem::LengthAtMost(n) => format!("have length at most {n}"),
            Problem::NoNul => "be a string without U+0000".to_string(),
            Problem::DateRange => "be a date in the years 0000 to 9999".to_string(),
            Problem::DateTimeRange => "be a date-time from 0000-01-01 to 9999-12-31".to_string(),
            Problem::Dims(d) => format!(
                "have dimensions {}",
                d.iter().map(u64::to_string).collect::<Vec<_>>().join(" x ")
            ),
            Problem::Cells(n) => format!("have {n} cells, one per row"),
            Problem::Code(k) => format!("be a code from 1 to {k}"),
            Problem::Present => "be a value".to_string(),
            Problem::Kind(k) => format!("be {k}"),
            Problem::OneValue => "be one value".to_string(),
            Problem::NonEmptyKey => "be a key that is not empty".to_string(),
            Problem::Required
            | Problem::Attributes(_)
            | Problem::UnknownField { .. }
            | Problem::UnknownColumn { .. }
            | Problem::Repeats { .. }
            | Problem::UnknownType(_)
            | Problem::Text(_) => return None,
        })
    }

    /// The problem's sentence, ending with a period: `subject must
    /// requirement; found value.` for a requirement, and the fact
    /// otherwise; `suggestion` is the nearest name.
    pub fn sentence(
        &self,
        segments: &[Segment],
        found: Option<&Shown>,
        suggestion: Option<&str>,
        s: &dyn Syntax,
    ) -> String {
        let subject = if segments.is_empty() {
            "The value".to_string()
        } else {
            s.path(segments)
        };
        let near = |what: &str, quoted: String| format!("; the nearest {what} is {quoted}");
        let mut out = match self.requirement(s) {
            Some(r) => {
                let mut t = format!("{subject} must {r}");
                if let Some(f) = found {
                    t.push_str("; found ");
                    t.push_str(&s.shown(f));
                }
                if let Some(n) = suggestion {
                    t.push_str(&near("value", s.lit(&Lit::Str(n.to_string()))));
                }
                t
            }
            None => match self {
                Problem::Required => format!("{subject} is required"),
                Problem::UnknownField { of, names } => {
                    let mut t = format!("{subject} is not a field of `{of}`");
                    match suggestion {
                        Some(n) => t.push_str(&near("field", s.name(n))),
                        None if !names.is_empty() => {
                            let items: Vec<String> = names.iter().map(|n| s.name(n)).collect();
                            t.push_str(&format!(", whose fields are {}", and_list(&items, "and")));
                        }
                        None => {}
                    }
                    t
                }
                Problem::UnknownColumn { names } => {
                    let mut t = format!("{subject} is not a column of the table");
                    match suggestion {
                        Some(n) => t.push_str(&near("column", s.name(n))),
                        None if !names.is_empty() => {
                            let items: Vec<String> = names.iter().map(|n| s.name(n)).collect();
                            t.push_str(&format!(", whose columns are {}", and_list(&items, "and")));
                        }
                        None => {}
                    }
                    t
                }
                Problem::Repeats { first } => {
                    format!("{subject} repeats {}", s.path(first))
                }
                Problem::Attributes(names) => {
                    let items: Vec<String> =
                        names.iter().map(|n| s.lit(&Lit::Str(n.clone()))).collect();
                    format!(
                        "{subject} carries the attributes {}, which its type does not hold",
                        and_list(&items, "and")
                    )
                }
                Problem::UnknownType(t) => {
                    format!("{subject} has the type `{t}`, which is not registered")
                }
                Problem::Text(t) => format!("{subject}: {t}"),
                _ => unreachable!("every requirement has a phrase"),
            },
        };
        out.push('.');
        out
    }
}

/// The host-neutral syntax, for logs and for hosts without a syntax of their
/// own: a path is a JSON Pointer, and a literal or a value is JSON.
pub struct Neutral;

impl Syntax for Neutral {
    fn path(&self, segments: &[Segment]) -> String {
        format!("`{}`", pointer(segments))
    }
    fn lit(&self, x: &Lit) -> String {
        match x {
            Lit::Num(n) => crate::emit::number_text(*n),
            Lit::Str(t) => serde_json::Value::String(t.clone()).to_string(),
        }
    }
    fn shown(&self, x: &Shown) -> String {
        match x {
            Shown::Number(t) | Shown::Date(t) | Shown::DateTime(t) => t.clone(),
            Shown::Bool(b) => b.to_string(),
            Shown::String { text, cut } => {
                let q = serde_json::Value::String(text.clone()).to_string();
                if *cut {
                    format!("{}...\"", &q[..q.len() - 1])
                } else {
                    q
                }
            }
            Shown::Length(n) => format!("length {n}"),
            Shown::Dim(d) => d.iter().map(u64::to_string).collect::<Vec<_>>().join(" x "),
            Shown::Sensitive => "<sensitive>".to_string(),
            Shown::Absent => "null".to_string(),
            Shown::Missing => "a missing value".to_string(),
            Shown::Kind(k) => k.clone(),
            Shown::Other => "a value of another kind".to_string(),
        }
    }
    fn name(&self, n: &str) -> String {
        format!("`{n}`")
    }
}
