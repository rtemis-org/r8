//! Names of R8 declarations and the rule they follow.
//!
//! Every name of a field, variant, type, trait, namespace, or tag member, and
//! every tag value, is an identifier that each host language can hold:
//!
//! 1. It matches `[A-Za-z][A-Za-z0-9_]*`.
//! 2. It is not an R reserved word, and not `self`, `Self`, `super`, or
//!    `crate`, which Rust cannot escape.
//! 3. A type, trait, or variant name is in addition not a reserved word of
//!    Rust, Python, or ECMAScript, since each becomes a class name in a host.
//!
//! [`Name`] enforces rules 1 and 2; [`is_host_keyword`] supplies rule 3, which
//! the declaration checks apply where a name becomes a class name. A field
//! name that is a keyword of one host keeps its name, and each binding escapes
//! it (`r#type` in Rust, `lambda_` in Python).
//!
//! Method names follow R's syntax instead, since a trait method is an R
//! generic: see [`MethodName`].

use std::fmt;

use serde::{Deserialize, Deserializer, Serialize, Serializer};

/// R reserved words, as `?Reserved` lists them; each fails as `x$word` or as
/// a formal argument.
pub const R_RESERVED: &[&str] = &[
    "if",
    "else",
    "repeat",
    "while",
    "function",
    "for",
    "in",
    "next",
    "break",
    "TRUE",
    "FALSE",
    "NULL",
    "Inf",
    "NaN",
    "NA",
    "NA_integer_",
    "NA_real_",
    "NA_character_",
    "NA_complex_",
];

/// Rust path keywords that have no raw-identifier form.
pub const RUST_UNESCAPABLE: &[&str] = &["self", "Self", "super", "crate"];

/// Rust strict and reserved keywords of the 2024 edition.
pub const RUST_KEYWORDS: &[&str] = &[
    "as", "async", "await", "break", "const", "continue", "crate", "dyn", "else", "enum", "extern",
    "false", "fn", "for", "gen", "if", "impl", "in", "let", "loop", "match", "mod", "move", "mut",
    "pub", "ref", "return", "self", "Self", "static", "struct", "super", "trait", "true", "type",
    "unsafe", "use", "where", "while", "abstract", "become", "box", "do", "final", "macro",
    "override", "priv", "try", "typeof", "unsized", "virtual", "yield",
];

/// Python hard keywords (`keyword.kwlist`, Python 3.15).
pub const PYTHON_KEYWORDS: &[&str] = &[
    "False", "None", "True", "and", "as", "assert", "async", "await", "break", "class", "continue",
    "def", "del", "elif", "else", "except", "finally", "for", "from", "global", "if", "import",
    "in", "is", "lambda", "nonlocal", "not", "or", "pass", "raise", "return", "try", "while",
    "with", "yield",
];

/// ECMAScript reserved words, including those reserved in strict mode.
pub const ECMASCRIPT_RESERVED: &[&str] = &[
    "await",
    "break",
    "case",
    "catch",
    "class",
    "const",
    "continue",
    "debugger",
    "default",
    "delete",
    "do",
    "else",
    "enum",
    "export",
    "extends",
    "false",
    "finally",
    "for",
    "function",
    "if",
    "implements",
    "import",
    "in",
    "instanceof",
    "interface",
    "let",
    "new",
    "null",
    "package",
    "private",
    "protected",
    "public",
    "return",
    "static",
    "super",
    "switch",
    "this",
    "throw",
    "true",
    "try",
    "typeof",
    "var",
    "void",
    "while",
    "with",
    "yield",
];

/// Why a string is not a valid name.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NameError {
    /// Empty, or a character outside `[A-Za-z0-9_]`, or a first character
    /// that is not a letter.
    Pattern(String),
    /// An R reserved word or an unescapable Rust keyword.
    Reserved(String),
    /// A qualified name without exactly one `::` separator.
    Qualified(String),
    /// A method name that is not a syntactic R name.
    Method(String),
}

impl fmt::Display for NameError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            NameError::Pattern(s) => write!(
                f,
                "`{s}` is not a valid name: a name starts with a letter and holds only letters, digits, and underscores"
            ),
            NameError::Reserved(s) => write!(
                f,
                "`{s}` is a reserved word in R or Rust; choose another name"
            ),
            NameError::Qualified(s) => write!(
                f,
                "`{s}` is not a qualified name of the form `namespace::Name`"
            ),
            NameError::Method(s) => write!(
                f,
                "`{s}` is not a valid method name: use a syntactic R name, optionally ending in `<-`"
            ),
        }
    }
}

impl std::error::Error for NameError {}

fn matches_pattern(s: &str) -> bool {
    let mut chars = s.chars();
    match chars.next() {
        Some(c) if c.is_ascii_alphabetic() => {}
        _ => return false,
    }
    chars.all(|c| c.is_ascii_alphanumeric() || c == '_')
}

/// `true` when `s` is a reserved word of Rust, Python, or ECMAScript (rule 3).
pub fn is_host_keyword(s: &str) -> bool {
    RUST_KEYWORDS.contains(&s) || PYTHON_KEYWORDS.contains(&s) || ECMASCRIPT_RESERVED.contains(&s)
}

/// A name that satisfies rules 1 and 2.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Name(Box<str>);

impl Name {
    /// Checks `s` against rules 1 and 2.
    pub fn new(s: impl Into<String>) -> Result<Self, NameError> {
        let s = s.into();
        if !matches_pattern(&s) {
            return Err(NameError::Pattern(s));
        }
        if R_RESERVED.contains(&s.as_str()) || RUST_UNESCAPABLE.contains(&s.as_str()) {
            return Err(NameError::Reserved(s));
        }
        Ok(Name(s.into_boxed_str()))
    }

    /// The name as text.
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl fmt::Display for Name {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.0)
    }
}

impl Serialize for Name {
    fn serialize<S: Serializer>(&self, s: S) -> Result<S::Ok, S::Error> {
        s.serialize_str(&self.0)
    }
}

impl<'de> Deserialize<'de> for Name {
    fn deserialize<D: Deserializer<'de>>(d: D) -> Result<Self, D::Error> {
        let s = String::deserialize(d)?;
        Name::new(s).map_err(serde::de::Error::custom)
    }
}

/// A type's or trait's identity in every host, `namespace::Name`.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct QualifiedName {
    /// The namespace.
    pub namespace: Name,
    /// The name within the namespace.
    pub name: Name,
}

impl QualifiedName {
    /// The qualified name `namespace::name`.
    pub fn new(namespace: Name, name: Name) -> Self {
        QualifiedName { namespace, name }
    }

    /// Parses `namespace::Name`; both parts follow rules 1 and 2.
    pub fn parse(s: &str) -> Result<Self, NameError> {
        let mut parts = s.split("::");
        match (parts.next(), parts.next(), parts.next()) {
            (Some(ns), Some(name), None) => Ok(QualifiedName {
                namespace: Name::new(ns)?,
                name: Name::new(name)?,
            }),
            _ => Err(NameError::Qualified(s.to_string())),
        }
    }
}

impl fmt::Display for QualifiedName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}::{}", self.namespace, self.name)
    }
}

impl Serialize for QualifiedName {
    fn serialize<S: Serializer>(&self, s: S) -> Result<S::Ok, S::Error> {
        s.collect_str(self)
    }
}

impl<'de> Deserialize<'de> for QualifiedName {
    fn deserialize<D: Deserializer<'de>>(d: D) -> Result<Self, D::Error> {
        let s = String::deserialize(d)?;
        QualifiedName::parse(&s).map_err(serde::de::Error::custom)
    }
}

/// A trait method's name: a syntactic R name that is not reserved, optionally
/// ending in `<-` for a replacement method, or `[`.
///
/// Each binding maps periods to underscores (`as.data.frame` becomes
/// `as_data_frame`).
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MethodName(Box<str>);

impl MethodName {
    /// A method name: a name, `...`, or an R function name.
    pub fn new(s: impl Into<String>) -> Result<Self, NameError> {
        let s = s.into();
        if s == "[" {
            return Ok(MethodName(s.into_boxed_str()));
        }
        let base = s.strip_suffix("<-").unwrap_or(&s);
        if !is_syntactic_r_name(base) {
            return Err(NameError::Method(s));
        }
        Ok(MethodName(s.into_boxed_str()))
    }

    /// The name as text.
    pub fn as_str(&self) -> &str {
        &self.0
    }

    /// `true` for a replacement method, whose name ends in `<-`.
    pub fn is_replacement(&self) -> bool {
        self.0.ends_with("<-")
    }
}

impl fmt::Display for MethodName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.0)
    }
}

impl Serialize for MethodName {
    fn serialize<S: Serializer>(&self, s: S) -> Result<S::Ok, S::Error> {
        s.serialize_str(&self.0)
    }
}

impl<'de> Deserialize<'de> for MethodName {
    fn deserialize<D: Deserializer<'de>>(d: D) -> Result<Self, D::Error> {
        let s = String::deserialize(d)?;
        MethodName::new(s).map_err(serde::de::Error::custom)
    }
}

#[cfg(feature = "schema")]
mod schema {
    //! JSON Schemas of the name types: the pattern of rule 1 and the reserved
    //! words of rule 2, so a schema validator rejects what [`Name::new()`]
    //! rejects. Rule 3 depends on the declaration a name is used in and is
    //! left to the declaration checks.

    use std::borrow::Cow;

    use schemars::{JsonSchema, Schema, SchemaGenerator, json_schema};

    use super::{MethodName, Name, QualifiedName, R_RESERVED, RUST_UNESCAPABLE};

    fn reserved() -> Vec<&'static str> {
        R_RESERVED.iter().chain(RUST_UNESCAPABLE).copied().collect()
    }

    impl JsonSchema for Name {
        fn schema_name() -> Cow<'static, str> {
            "Name".into()
        }

        fn json_schema(_: &mut SchemaGenerator) -> Schema {
            json_schema!({
                "description": "A name: a letter followed by letters, digits, and underscores; not an R reserved word or `self`, `Self`, `super`, or `crate`.",
                "type": "string",
                "pattern": "^[A-Za-z][A-Za-z0-9_]*$",
                "not": {"enum": reserved()}
            })
        }
    }

    impl JsonSchema for QualifiedName {
        fn schema_name() -> Cow<'static, str> {
            "QualifiedName".into()
        }

        fn json_schema(_: &mut SchemaGenerator) -> Schema {
            let word = reserved().join("|");
            json_schema!({
                "description": "A qualified name, `namespace::Name`; both parts are names.",
                "type": "string",
                "pattern": "^[A-Za-z][A-Za-z0-9_]*::[A-Za-z][A-Za-z0-9_]*$",
                "not": {"pattern": format!("^({word})::|::({word})$")}
            })
        }
    }

    impl JsonSchema for MethodName {
        fn schema_name() -> Cow<'static, str> {
            "MethodName".into()
        }

        fn json_schema(_: &mut SchemaGenerator) -> Schema {
            let word = super::R_RESERVED.join("|");
            json_schema!({
                "description": "A method name: a syntactic R name that is not reserved, optionally ending in `<-`, or `[`.",
                "type": "string",
                "anyOf": [
                    {"const": "["},
                    {"pattern": "^([A-Za-z][._A-Za-z0-9]*|[.]([._A-Za-z][._A-Za-z0-9]*)?)(<-)?$"}
                ],
                "not": {"pattern": format!("^({word})(<-)?$")}
            })
        }
    }
}

/// `true` when `s` is a syntactic R name (`make.names()` leaves it unchanged)
/// and not a reserved word. Restricted to ASCII.
pub fn is_syntactic_r_name(s: &str) -> bool {
    if s.is_empty() || R_RESERVED.contains(&s) {
        return false;
    }
    let bytes = s.as_bytes();
    let valid_char = |c: u8| c.is_ascii_alphanumeric() || c == b'.' || c == b'_';
    if !bytes.iter().all(|&c| valid_char(c)) {
        return false;
    }
    match bytes[0] {
        c if c.is_ascii_alphabetic() => true,
        b'.' => !matches!(bytes.get(1), Some(c) if c.is_ascii_digit()),
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn names_follow_the_pattern() {
        for ok in [
            "cost",
            "Svm",
            "mtry",
            "x1",
            "kernel_width",
            "lambda",
            "type",
        ] {
            assert!(Name::new(ok).is_ok(), "{ok}");
        }
        for bad in ["", "1x", "_x", "x.y", "x-y", "ç", "x y"] {
            assert!(
                matches!(Name::new(bad), Err(NameError::Pattern(_))),
                "{bad}"
            );
        }
    }

    #[test]
    fn reserved_words_abort() {
        for bad in [
            "if",
            "in",
            "NULL",
            "TRUE",
            "NA_complex_",
            "self",
            "Self",
            "super",
            "crate",
        ] {
            assert!(
                matches!(Name::new(bad), Err(NameError::Reserved(_))),
                "{bad}"
            );
        }
    }

    #[test]
    fn host_keywords_are_detected_case_sensitively() {
        for kw in [
            "type", "match", "lambda", "class", "None", "True", "function", "gen",
        ] {
            assert!(is_host_keyword(kw), "{kw}");
        }
        for ok in ["Type", "none", "Rbf", "Linear", "cost"] {
            assert!(!is_host_keyword(ok), "{ok}");
        }
    }

    #[test]
    fn qualified_names_parse_and_print() {
        let q = QualifiedName::parse("rtemis::Svm").unwrap();
        assert_eq!(q.namespace.as_str(), "rtemis");
        assert_eq!(q.to_string(), "rtemis::Svm");
        for bad in ["Svm", "a::b::c", "rtemis.ml::Svm", "::Svm"] {
            assert!(QualifiedName::parse(bad).is_err(), "{bad}");
        }
    }

    #[test]
    fn method_names_follow_r_syntax() {
        for ok in [
            "fit",
            "labels<-",
            "as.data.frame",
            ".hidden",
            "[",
            "summary",
        ] {
            assert!(MethodName::new(ok).is_ok(), "{ok}");
        }
        for bad in ["", "1fit", ".1x", "if", "a-b", "<-", "$"] {
            assert!(MethodName::new(bad).is_err(), "{bad}");
        }
        assert!(MethodName::new("labels<-").unwrap().is_replacement());
    }
}

/// The candidate nearest to `name` by edit distance (Levenshtein, in
/// characters), when it is within one edit, or two for a name of six
/// characters or more; the first of equally near candidates.
pub fn nearest<'a>(name: &str, candidates: impl IntoIterator<Item = &'a str>) -> Option<&'a str> {
    fn distance(a: &str, b: &str) -> usize {
        let b: Vec<char> = b.chars().collect();
        let mut row: Vec<usize> = (0..=b.len()).collect();
        for (i, ca) in a.chars().enumerate() {
            let mut prev = row[0];
            row[0] = i + 1;
            for (j, &cb) in b.iter().enumerate() {
                let next = (prev + usize::from(ca != cb))
                    .min(row[j] + 1)
                    .min(row[j + 1] + 1);
                prev = row[j + 1];
                row[j + 1] = next;
            }
        }
        row[b.len()]
    }
    let limit = if name.chars().count() >= 6 { 2 } else { 1 };
    candidates
        .into_iter()
        .map(|c| (distance(name, c), c))
        .filter(|&(d, c)| d <= limit && c != name)
        .min_by_key(|&(d, _)| d)
        .map(|(_, c)| c)
}

#[cfg(test)]
mod nearest_tests {
    use super::nearest;

    #[test]
    fn nearest_within_the_limit() {
        assert_eq!(nearest("hj", ["lo", "hi", "values"]), Some("hi"));
        assert_eq!(nearest("valeus", ["lo", "hi", "values"]), Some("values"));
        assert_eq!(nearest("mean", ["min", "max"]), None);
        assert_eq!(nearest("sum", ["sum"]), None);
    }
}
