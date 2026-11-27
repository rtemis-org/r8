//! The Rust signature of a typed function whose body moves to Rust: the text
//! of a `#[r8::function]` item that binds to the function's declaration.
//!
//! Each parameter and the result take the Rust type of their declared type:
//! scalars as `bool`, `i32`, `f64`, and `&str` (`String` as a result), bytes
//! and vectors of logicals, integers, and doubles as slices borrowed for the
//! call (`&[u8]`, `&[bool]`, `&[i32]`, `&[f64]`; `Vec<T>` as a result), and
//! `option()` of one of these as `Option<T>`. A parameter without a type, and
//! one of any other type, is `r8::Sexp`, an R value; `...` is `r8::Dots`. A
//! declared constraint and a rule are written as a comment, since R8 checks
//! them before the body runs.
//! spec: R8/class-system#typed-functions

use crate::ir::notation::notation;
use crate::ir::{FieldType, FunctionDef, ReturnType};
use crate::rules::text::text;

/// The Rust keywords, which an identifier writes as a raw identifier.
const KEYWORDS: &[&str] = &[
    "as", "async", "await", "break", "const", "continue", "crate", "dyn", "else", "enum", "extern",
    "false", "fn", "for", "gen", "if", "impl", "in", "let", "loop", "match", "mod", "move", "mut",
    "pub", "ref", "return", "static", "struct", "super", "trait", "true", "type", "unsafe", "use",
    "where", "while", "abstract", "become", "box", "do", "final", "macro", "override", "priv",
    "try", "typeof", "unsized", "virtual", "yield",
];

/// Where a Rust type appears: a parameter borrows, a result owns.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Place {
    Param,
    Result,
}

/// The Rust type of the field type `t`, or `None` for a type that compiled
/// code receives as an R value.
fn rust_type(t: &FieldType, place: Place) -> Option<String> {
    let owned = place == Place::Result;
    Some(match t {
        FieldType::Logical => "bool".into(),
        FieldType::Integer(_) => "i32".into(),
        FieldType::Double(_) => "f64".into(),
        FieldType::Character(_) if owned => "String".into(),
        FieldType::Character(_) => "&str".into(),
        FieldType::Raw if owned => "Vec<u8>".into(),
        FieldType::Raw => "&[u8]".into(),
        FieldType::Vec { element, .. } => {
            let e = match **element {
                FieldType::Logical => "bool",
                FieldType::Integer(_) => "i32",
                FieldType::Double(_) => "f64",
                _ => return None,
            };
            if owned {
                format!("Vec<{e}>")
            } else {
                format!("&[{e}]")
            }
        }
        FieldType::Option { inner } if !matches!(**inner, FieldType::Option { .. }) => {
            format!("Option<{}>", rust_type(inner, place)?)
        }
        _ => return None,
    })
}

/// Whether `s` is a Rust identifier of ASCII letters, digits, and
/// underscores, beginning with a letter or an underscore.
fn is_identifier(s: &str) -> bool {
    let mut c = s.chars();
    c.next()
        .is_some_and(|f| f.is_ascii_alphabetic() || f == '_')
        && c.all(|x| x.is_ascii_alphanumeric() || x == '_')
}

/// The Rust identifier of the R name `name`, and whether it differs from the
/// name, which the item then states with `#[r8(name = "...")]`.
fn identifier(name: &str) -> (String, bool) {
    if is_identifier(name) {
        if KEYWORDS.contains(&name) {
            return (format!("r#{name}"), false);
        }
        return (name.to_string(), false);
    }
    let mut id: String = name
        .chars()
        .map(|c| if c.is_ascii_alphanumeric() { c } else { '_' })
        .collect();
    if !id.starts_with(|c: char| c.is_ascii_alphabetic() || c == '_') {
        id.insert(0, '_');
    }
    (id, true)
}

/// The text of the `#[r8::function]` signature of `f`, with the body
/// `todo!()`.
pub fn function_signature(f: &FunctionDef) -> String {
    let mut lines = Vec::new();
    for line in f.description.lines().filter(|l| !l.trim().is_empty()) {
        lines.push(format!("/// {}", line.trim()));
    }
    let mut checks = Vec::new();
    let mut host = Vec::new();
    let mut params = Vec::new();
    let mut taken: Vec<String> = Vec::new();
    for p in &f.params {
        let name = p.name();
        if name == "..." {
            host.push("`...`".to_string());
            params.push("    dots: r8::Dots,".to_string());
            continue;
        }
        let (mut id, mut renamed) = identifier(name);
        // Two R names of one identifier, such as `a.b` and `a_b`, take a suffix.
        if taken.contains(&id) {
            let mut k = 2;
            while taken.contains(&format!("{id}_{k}")) {
                k += 1;
            }
            id = format!("{id}_{k}");
            renamed = true;
        }
        taken.push(id.clone());
        let ty = p.ty().and_then(|t| rust_type(t, Place::Param));
        if let Some(t) = p.ty() {
            checks.push(format!("`{name}` is {}", notation(t)));
        }
        if ty.is_none() {
            host.push(format!("`{name}`"));
        }
        let attr = if renamed {
            format!("#[r8(name = \"{name}\")] ")
        } else {
            String::new()
        };
        params.push(format!(
            "    {attr}{id}: {},",
            ty.unwrap_or_else(|| "r8::Sexp".to_string())
        ));
    }
    for r in &f.rules {
        let rule = text(&r.invariant);
        checks.push(match &r.when {
            Some(w) => format!("{rule}, where {}", text(w)),
            None => rule,
        });
    }
    let result = match &f.return_type {
        Some(ReturnType::Type(t)) => {
            checks.push(format!("the result is {}", notation(t)));
            rust_type(t, Place::Result)
        }
        _ => None,
    };
    if result.is_none() {
        host.push("the result".to_string());
    }
    if !checks.is_empty() {
        if !lines.is_empty() {
            lines.push("///".to_string());
        }
        lines.push("/// R8 checks, before and after the body:".to_string());
        for c in &checks {
            lines.push(format!("/// - {c}"));
        }
    }
    if !host.is_empty() {
        if !lines.is_empty() {
            lines.push("///".to_string());
        }
        let mut items = list(&host);
        if let Some(first) = items.get_mut(0..1) {
            first.make_ascii_uppercase();
        }
        lines.push(format!(
            "/// {items} {}, so this body runs in R alone.",
            if host.len() == 1 {
                "is an R value"
            } else {
                "are R values"
            }
        ));
    }
    let (fn_id, renamed) = identifier(f.name.as_str());
    lines.push(if renamed {
        format!("#[r8::function(name = \"{}\")]", f.name.as_str())
    } else {
        "#[r8::function]".to_string()
    });
    let result = result.unwrap_or_else(|| "r8::Sexp".to_string());
    if params.is_empty() {
        lines.push(format!("fn {fn_id}() -> {result} {{"));
    } else {
        lines.push(format!("fn {fn_id}("));
        lines.extend(params);
        lines.push(format!(") -> {result} {{"));
    }
    lines.push("    todo!()".to_string());
    lines.push("}".to_string());
    let mut out = lines.join("\n");
    out.push('\n');
    out
}

/// A list of names with the serial comma.
fn list(items: &[String]) -> String {
    match items {
        [] => String::new(),
        [a] => a.clone(),
        [a, b] => format!("{a} and {b}"),
        [rest @ .., last] => format!("{}, and {last}", rest.join(", ")),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::Logic;
    use crate::ir::{DoubleType, Param, ParamDef, RuleDef};
    use crate::name::Name;

    fn typed(name: &str, ty: FieldType) -> ParamDef {
        Param {
            name: name.into(),
            dispatch: false,
            ty: Some(ty),
            default: None,
            host_default: None,
            compute: None,
            description: "".into(),
        }
        .into()
    }

    fn function(params: Vec<ParamDef>, return_type: Option<FieldType>) -> FunctionDef {
        FunctionDef {
            name: Name::new("level").unwrap(),
            description: "Holds a running level between `lo` and `hi`.".into(),
            params,
            return_type: return_type.map(|t| ReturnType::Type(Box::new(t))),
            rules: Vec::new(),
            typed: Default::default(),
            stochastic: false,
            on: None,
        }
    }

    fn doubles() -> FieldType {
        FieldType::Vec {
            element: Box::new(FieldType::Double(DoubleType::default())),
            min_length: None,
            max_length: None,
            unique: false,
        }
    }

    #[test]
    fn a_function_of_vectors_and_scalars() {
        let mut f = function(
            vec![
                typed("x", doubles()),
                typed("lo", FieldType::Double(DoubleType::default())),
                typed("hi", FieldType::Double(DoubleType::default())),
            ],
            Some(doubles()),
        );
        let lo_le_hi: Logic =
            serde_json::from_value(serde_json::json!({"<=": [{"var": "lo"}, {"var": "hi"}]}))
                .unwrap();
        f.rules.push(RuleDef {
            id: None,
            invariant: lo_le_hi,
            when: None,
            message: None,
            inherited_from: None,
        });
        let text = function_signature(&f);
        assert!(text.starts_with("/// Holds a running level between `lo` and `hi`.\n///\n"));
        assert!(text.contains(
            "/// R8 checks, before and after the body:\n/// - `x` is vector of double\n/// - `lo` is double\n/// - `hi` is double\n/// - lo <= hi\n/// - the result is vector of double\n"
        ), "{text}");
        assert!(text.contains(
            "#[r8::function]\nfn level(\n    x: &[f64],\n    lo: f64,\n    hi: f64,\n) -> Vec<f64> {\n    todo!()\n}\n"
        ), "{text}");
        assert!(!text.contains("R alone"));
    }

    #[test]
    fn a_name_that_is_no_identifier_and_values_of_r() {
        let f = function(
            vec![
                typed("na.rm", FieldType::Logical),
                ParamDef::Name("data".into()),
                typed(
                    "s",
                    FieldType::Option {
                        inner: Box::new(FieldType::Character(Default::default())),
                    },
                ),
                ParamDef::Name("...".into()),
            ],
            None,
        );
        let text = function_signature(&f);
        assert!(
            text.contains("    #[r8(name = \"na.rm\")] na_rm: bool,\n"),
            "{text}"
        );
        assert!(text.contains("    data: r8::Sexp,\n"));
        assert!(text.contains("    s: Option<&str>,\n"));
        assert!(text.contains("    dots: r8::Dots,\n"));
        assert!(
            text.contains(
                "/// `data`, `...`, and the result are R values, so this body runs in R alone."
            ),
            "{text}"
        );
        assert!(text.contains(") -> r8::Sexp {"));
    }

    #[test]
    fn two_names_of_one_identifier_take_a_suffix() {
        let f = function(
            vec![
                typed("a_b", FieldType::Logical),
                typed("a.b", FieldType::Logical),
            ],
            None,
        );
        let text = function_signature(&f);
        assert!(text.contains("    a_b: bool,\n"), "{text}");
        assert!(
            text.contains("    #[r8(name = \"a.b\")] a_b_2: bool,\n"),
            "{text}"
        );
    }

    #[test]
    fn a_keyword_is_a_raw_identifier() {
        assert_eq!(identifier("match"), ("r#match".to_string(), false));
        assert_eq!(identifier("na.rm"), ("na_rm".to_string(), true));
        assert_eq!(identifier("x"), ("x".to_string(), false));
    }
}
