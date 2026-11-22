//! Checked builders of field types and rule expressions, which every host's
//! declaration functions call: `type_double(min = 0)` in R and its Python
//! counterpart build the same [`FieldType`] through [`double`], and report the
//! same [`BuildError`].
//!
//! A host reads its arguments into the typed parameters here and reports an
//! argument it cannot read with the matching [`BuildError`], so that the text
//! of every message lives in this module. The checks of a whole declaration,
//! which need the registry, are [`super::check_type`]'s.
//! spec: R8/class-system#declaring-types

use std::fmt;

use super::{Bound, CharacterType, DoubleType, FieldDef, FieldType, IntegerType, Logic, Resolve};
use crate::name::QualifiedName;
use crate::validate::compiled_pattern;

/// An argument of a declaration function that does not build.
#[derive(Debug, Clone, PartialEq)]
pub enum BuildError {
    /// A number argument that is not one finite number.
    NotOneNumber {
        /// The argument.
        arg: &'static str,
    },
    /// An integer bound or value that is not a whole number in R8's integer
    /// domain.
    NotInteger {
        /// The argument.
        arg: &'static str,
    },
    /// A length or an extent that is not a whole number from 0 to
    /// 4294967295.
    NotCount {
        /// The argument.
        arg: &'static str,
    },
    /// Both an inclusive and an exclusive bound on one side.
    BothBounds {
        /// The inclusive bound's argument.
        inclusive: &'static str,
        /// The exclusive bound's argument.
        exclusive: &'static str,
    },
    /// A list of numbers that holds one that is not finite.
    NotFinite {
        /// The argument.
        arg: &'static str,
    },
    /// A string argument that is not one string.
    NotOneString {
        /// The argument.
        arg: &'static str,
    },
    /// A list of strings that holds a missing one.
    MissingString {
        /// The argument.
        arg: &'static str,
    },
    /// A pattern that does not compile.
    Pattern {
        /// The pattern.
        pattern: String,
    },
    /// The smallest length is above the largest.
    LengthOrder,
    /// Key constraints that are not a character type.
    Keys,
    /// A computed `option()` without its expression, or an expression
    /// without `resolve = "computed"`.
    Compute {
        /// The expression is given.
        given: bool,
    },
    /// A rule constant that is not one value or is missing.
    Constant,
    /// An array literal in a rule that holds an operand other than a
    /// constant.
    ArrayLiteral,
    /// The variant operand of `is_variant()` that is not a string.
    VariantName,
    /// A list of strings that is not a character vector.
    NotStrings {
        /// The argument.
        arg: &'static str,
    },
    /// A field type argument that is not a field type or an R8 type.
    NotFieldType,
    /// A rule ID that is not an R8 name.
    RuleId,
    /// A rule message that is not one non-empty string.
    RuleMessage,
    /// A rule operand that is not a constant, a field, or a call; `expr` is
    /// the operand as the host writes it.
    RuleOperand {
        /// The operand.
        expr: String,
    },
    /// A rule call whose function is not a name; `expr` is the call as the
    /// host writes it.
    RuleCallee {
        /// The call.
        expr: String,
    },
    /// A newtype variant (`of`) given fields.
    VariantOfFields,
    /// A newtype variant (`of`) given rules.
    VariantOfRules,
    /// A variant given rules without fields to read.
    VariantRulesWithoutFields,
    /// A newtype variant's `of` that is not a struct type.
    VariantOf,
    /// A rule indexes a field, as in `values[[1]]`.
    RuleIndex {
        /// The expression, as the host writes it.
        expr: String,
    },
    /// A rule calls a function outside the dialect; `known` lists the
    /// functions the dialect has, as the host writes them.
    UnknownFunction {
        /// The function.
        name: String,
        /// The function of the dialect nearest to `name`, as the host
        /// writes it, if one is near.
        nearest: Option<String>,
        /// The functions of the dialect.
        known: &'static str,
    },
    /// The default of a field of a named type that is not a value of an R8
    /// enum.
    DefaultNotValue,
    /// A variant argument of an enum that is not a variant declaration.
    NotVariant {
        /// The variant's name.
        name: String,
    },
    /// `int64()` in a rule that does not hold the decimal digits of one
    /// integer.
    Int64,
    /// `json_ir()` in a rule that does not hold one string of JSONLogic.
    JsonIr {
        /// Why the text does not read, or `None` when it is not one string.
        detail: Option<String>,
    },
}

impl fmt::Display for BuildError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BuildError::NotOneNumber { arg } => write!(f, "`{arg}` is one finite number."),
            BuildError::NotInteger { arg } => write!(
                f,
                "`{arg}` holds whole numbers from -2147483647 to 2147483647."
            ),
            BuildError::NotCount { arg } => {
                write!(f, "`{arg}` holds whole numbers from 0 to 4294967295.")
            }
            BuildError::BothBounds {
                inclusive,
                exclusive,
            } => write!(
                f,
                "`{inclusive}` and `{exclusive}` bound the same side; give one of them."
            ),
            BuildError::NotFinite { arg } => write!(f, "`{arg}` holds finite numbers."),
            BuildError::NotOneString { arg } => write!(f, "`{arg}` is one string."),
            BuildError::MissingString { arg } => write!(f, "`{arg}` holds no missing string."),
            BuildError::Pattern { pattern } => write!(
                f,
                "`pattern` is a regular expression in Rust `regex` syntax; \"{pattern}\" does not compile."
            ),
            BuildError::LengthOrder => f.write_str("`min_length` is at most `max_length`."),
            BuildError::Keys => f.write_str("`keys` is a type_character()."),
            BuildError::Compute { given: false } => {
                f.write_str("A computed field gives its value in `compute`.")
            }
            BuildError::Compute { given: true } => {
                f.write_str("`compute` belongs to resolve = \"computed\".")
            }
            BuildError::Constant => f.write_str(
                "A constant in a rule is one value, not NA; write an array as c(...).",
            ),
            BuildError::ArrayLiteral => f.write_str("c() in a rule holds literal values."),
            BuildError::NotStrings { arg } => write!(f, "`{arg}` is a character vector."),
            BuildError::NotFieldType => f.write_str(
                "A field type is a type_*(), vec(), option(), hashmap(), or ordered_map() call, or an R8 type.",
            ),
            BuildError::RuleId => f.write_str("A rule's `id` is an R8 name, such as \"COST_MAX\"."),
            BuildError::RuleMessage => f.write_str("A rule's `message` is one non-empty string."),
            BuildError::RuleOperand { expr } => write!(f, "A rule cannot hold {expr}."),
            BuildError::RuleCallee { expr } => {
                write!(f, "A rule calls a function by name: {expr}.")
            }
            BuildError::VariantOfFields => f.write_str(
                "A newtype variant, `of`, holds its struct's fields; it takes no fields of its own.",
            ),
            BuildError::VariantOfRules => f.write_str(
                "A newtype variant, `of`, takes its struct's rules; it takes no rules of its own.",
            ),
            BuildError::VariantRulesWithoutFields => {
                f.write_str("A variant's rules read its fields; a variant without fields takes no rules.")
            }
            BuildError::VariantOf => {
                f.write_str("A newtype variant's `of` is an R8 struct type.")
            }
            BuildError::RuleIndex { expr } => write!(
                f,
                "`{expr}` indexes a field; a rule reads whole fields: `all()` and `any()` test each element of a vector, and `$` reads a field of a field."
            ),
            BuildError::UnknownFunction {
                name,
                nearest,
                known,
            } => {
                write!(f, "`{name}()` is not a function of R8 rules")?;
                if let Some(n) = nearest {
                    write!(f, "; the nearest is `{n}()`")?;
                }
                write!(f, ". Rules use {known}.")
            }
            BuildError::DefaultNotValue => {
                f.write_str("The default of a field of an R8 type is a value of that type.")
            }
            BuildError::NotVariant { name } => write!(f, "Variant `{name}` is a variant() call."),
            BuildError::Int64 => {
                f.write_str("int64() in a rule holds the decimal digits of one integer.")
            }
            BuildError::JsonIr { detail: None } => {
                f.write_str("json_ir() in a rule holds one string of JSONLogic.")
            }
            BuildError::JsonIr { detail: Some(d) } => {
                write!(f, "The text of json_ir() in a rule does not read: {d}.")
            }
            BuildError::VariantName => {
                f.write_str("is_variant() names the variant as a string: is_variant(kernel, \"Rbf\").")
            }
        }
    }
}

impl std::error::Error for BuildError {}

type Built<T> = Result<T, BuildError>;

/// The bound of one side from its inclusive and exclusive arguments.
fn side<T: PartialOrd + Copy>(
    inclusive: Option<T>,
    exclusive: Option<T>,
    names: (&'static str, &'static str),
) -> Built<Option<Bound<T>>> {
    match (inclusive, exclusive) {
        (Some(_), Some(_)) => Err(BuildError::BothBounds {
            inclusive: names.0,
            exclusive: names.1,
        }),
        (Some(v), None) => Ok(Some(Bound::inclusive(v))),
        (None, Some(v)) => Ok(Some(Bound::exclusive(v))),
        (None, None) => Ok(None),
    }
}

fn finite(x: Option<f64>, arg: &'static str) -> Built<Option<f64>> {
    match x {
        Some(v) if !v.is_finite() => Err(BuildError::NotOneNumber { arg }),
        other => Ok(other),
    }
}

/// A whole number in R8's integer domain, −2147483647 to 2147483647; R's
/// `NA_integer_` is −2147483648.
fn whole_i32(x: f64, arg: &'static str) -> Built<i32> {
    if x.is_finite() && x.fract() == 0.0 && (-2147483647.0..=2147483647.0).contains(&x) {
        Ok(x as i32)
    } else {
        Err(BuildError::NotInteger { arg })
    }
}

/// A whole number from 0 to `u32::MAX`.
pub fn count(x: f64, arg: &'static str) -> Built<u32> {
    if x.is_finite() && x.fract() == 0.0 && (0.0..=u32::MAX as f64).contains(&x) {
        Ok(x as u32)
    } else {
        Err(BuildError::NotCount { arg })
    }
}

/// The numeric bounds of `type_integer()` and `type_double()`, as given.
#[derive(Debug, Clone, Copy, Default)]
pub struct Bounds {
    /// `min`.
    pub min: Option<f64>,
    /// `max`.
    pub max: Option<f64>,
    /// `exclusive_min`.
    pub exclusive_min: Option<f64>,
    /// `exclusive_max`.
    pub exclusive_max: Option<f64>,
}

impl Bounds {
    fn checked(self) -> Built<Bounds> {
        Ok(Bounds {
            min: finite(self.min, "min")?,
            max: finite(self.max, "max")?,
            exclusive_min: finite(self.exclusive_min, "exclusive_min")?,
            exclusive_max: finite(self.exclusive_max, "exclusive_max")?,
        })
    }
}

/// `type_logical()`.
pub fn logical() -> FieldType {
    FieldType::Logical
}

/// `type_integer(min, max, exclusive_min, exclusive_max, values)`.
pub fn integer(bounds: Bounds, values: Option<&[f64]>) -> Built<FieldType> {
    let b = bounds.checked()?;
    let w = |x: Option<f64>, arg| x.map(|v| whole_i32(v, arg)).transpose();
    let lower = side(
        w(b.min, "min")?,
        w(b.exclusive_min, "exclusive_min")?,
        ("min", "exclusive_min"),
    )?;
    let upper = side(
        w(b.max, "max")?,
        w(b.exclusive_max, "exclusive_max")?,
        ("max", "exclusive_max"),
    )?;
    let values = values
        .map(|v| {
            v.iter()
                .map(|&x| whole_i32(x, "values"))
                .collect::<Built<Box<[i32]>>>()
        })
        .transpose()?;
    Ok(FieldType::Integer(IntegerType {
        lower,
        upper,
        values,
    }))
}

/// `type_double(min, max, exclusive_min, exclusive_max, finite, values)`.
pub fn double(bounds: Bounds, finite_only: bool, values: Option<&[f64]>) -> Built<FieldType> {
    let b = bounds.checked()?;
    let lower = side(b.min, b.exclusive_min, ("min", "exclusive_min"))?;
    let upper = side(b.max, b.exclusive_max, ("max", "exclusive_max"))?;
    if values.is_some_and(|v| v.iter().any(|x| !x.is_finite())) {
        return Err(BuildError::NotFinite { arg: "values" });
    }
    Ok(FieldType::Double(DoubleType {
        lower,
        upper,
        finite: finite_only,
        values: values.map(Box::from),
    }))
}

/// The constraints of `type_character(pattern, values)`; a pattern compiles
/// here, through the process-wide cache that validation shares.
pub fn character_type(pattern: Option<&str>, values: Option<Vec<String>>) -> Built<CharacterType> {
    if let Some(p) = pattern
        && compiled_pattern(p).is_none()
    {
        return Err(BuildError::Pattern {
            pattern: p.to_string(),
        });
    }
    Ok(CharacterType {
        pattern: pattern.map(Box::from),
        values: values.map(|v| v.into_iter().map(String::into_boxed_str).collect()),
    })
}

/// `type_character(pattern, values)`.
pub fn character(pattern: Option<&str>, values: Option<Vec<String>>) -> Built<FieldType> {
    character_type(pattern, values).map(FieldType::Character)
}

/// `type_factor(levels, ordered)`.
pub fn factor(levels: Option<Vec<String>>, ordered: bool) -> FieldType {
    FieldType::Factor {
        levels: levels.map(|v| v.into_iter().map(String::into_boxed_str).collect()),
        ordered,
    }
}

/// `type_r(class)`.
pub fn r_class(class: &str) -> FieldType {
    FieldType::R {
        class: class.into(),
    }
}

/// `vec(element, min_length, max_length, unique)`.
pub fn vec(
    element: FieldType,
    min_length: Option<f64>,
    max_length: Option<f64>,
    unique: bool,
) -> Built<FieldType> {
    let (min_length, max_length) = lengths(min_length, max_length)?;
    Ok(FieldType::Vec {
        element: Box::new(element),
        min_length,
        max_length,
        unique,
    })
}

/// The `min_length` and `max_length` of `vec()`: whole numbers from 0, the
/// first at most the second.
pub fn lengths(
    min_length: Option<f64>,
    max_length: Option<f64>,
) -> Built<(Option<u64>, Option<u64>)> {
    let min_length = min_length.map(|x| count(x, "min_length")).transpose()?;
    let max_length = max_length.map(|x| count(x, "max_length")).transpose()?;
    if let (Some(lo), Some(hi)) = (min_length, max_length)
        && lo > hi
    {
        return Err(BuildError::LengthOrder);
    }
    Ok((min_length.map(u64::from), max_length.map(u64::from)))
}

/// `option(inner, resolve, compute)`: `policy` is `"none"`, `"runtime"`, or
/// `"computed"`, with `compute` its expression.
pub fn option(inner: FieldType, policy: &str, compute: Option<Logic>) -> Built<FieldType> {
    Ok(FieldType::Option {
        inner: Box::new(inner),
        resolve: resolve(policy, compute)?,
    })
}

/// Who fills an absent `option()` value: `policy` is `"none"`,
/// `"runtime"`, or `"computed"`, with `compute` its expression.
pub fn resolve(policy: &str, compute: Option<Logic>) -> Built<Resolve> {
    Ok(match (policy, compute) {
        ("computed", Some(expr)) => Resolve::Computed { expr },
        ("computed", None) => return Err(BuildError::Compute { given: false }),
        (_, Some(_)) => return Err(BuildError::Compute { given: true }),
        ("runtime", None) => Resolve::Runtime,
        _ => Resolve::None,
    })
}

/// The key constraints of `hashmap()` and `ordered_map()`, from a character
/// type or none.
pub fn keys(keys: Option<FieldType>) -> Built<CharacterType> {
    match keys {
        None => Ok(CharacterType::default()),
        Some(FieldType::Character(k)) => Ok(k),
        Some(_) => Err(BuildError::Keys),
    }
}

/// `hashmap(value, keys)`.
pub fn hashmap(value: FieldType, key_type: Option<FieldType>) -> Built<FieldType> {
    Ok(FieldType::Hashmap {
        value: Box::new(value),
        keys: keys(key_type)?,
    })
}

/// `ordered_map(value, keys)`.
pub fn ordered_map(value: FieldType, key_type: Option<FieldType>) -> Built<FieldType> {
    Ok(FieldType::OrderedMap {
        value: Box::new(value),
        keys: keys(key_type)?,
    })
}

/// `type_matrix(element, nrow, ncol)`.
pub fn matrix(element: FieldType, nrow: Option<f64>, ncol: Option<f64>) -> Built<FieldType> {
    Ok(FieldType::Matrix {
        element: Box::new(element),
        nrow: nrow.map(|x| count(x, "nrow")).transpose()?,
        ncol: ncol.map(|x| count(x, "ncol")).transpose()?,
    })
}

/// `type_array(element, dim)`.
pub fn array(element: FieldType, dim: Option<&[f64]>) -> Built<FieldType> {
    Ok(FieldType::Array {
        element: Box::new(element),
        dim: dim
            .map(|d| {
                d.iter()
                    .map(|&x| count(x, "dim"))
                    .collect::<Built<Box<[u32]>>>()
            })
            .transpose()?,
    })
}

/// `type_sparse_matrix(element, nrow, ncol)`.
pub fn sparse_matrix(element: FieldType, nrow: Option<f64>, ncol: Option<f64>) -> Built<FieldType> {
    Ok(FieldType::SparseMatrix {
        element: Box::new(element),
        nrow: nrow.map(|x| count(x, "nrow")).transpose()?,
        ncol: ncol.map(|x| count(x, "ncol")).transpose()?,
    })
}

/// `type_matrix(S)` and `type_table(S)` for a struct `S`, and a named type.
pub fn matrix_of(of: QualifiedName) -> FieldType {
    FieldType::MatrixOf { of }
}

/// `type_table(S)` for a struct `S`.
pub fn table_of(of: QualifiedName) -> FieldType {
    FieldType::TableOf { of }
}

/// A named struct or enum as a field type.
pub fn named(name: QualifiedName) -> FieldType {
    FieldType::Named { name }
}

impl FieldDef {
    /// A field without a default whose type is not `option()`: its
    /// constructor argument is required.
    pub fn is_required(&self) -> bool {
        self.default.is_none() && !matches!(self.ty, FieldType::Option { .. })
    }
}

/// `rule(id, invariant, message, when)`.
pub fn rule(
    invariant: Logic,
    when: Option<Logic>,
    message: Option<&str>,
    id: Option<&str>,
) -> Built<super::RuleDef> {
    let id = id
        .map(|i| crate::name::Name::new(i).map_err(|_| BuildError::RuleId))
        .transpose()?;
    if message.is_some_and(|m| m.trim().is_empty()) {
        return Err(BuildError::RuleMessage);
    }
    Ok(super::RuleDef {
        invariant,
        when,
        message: message.map(str::to_string),
        id,
        inherited_from: None,
    })
}

/// Checks the parts of a variant: a newtype variant (`of`) takes neither
/// fields nor rules, and rules need fields to read.
pub fn variant_parts(has_of: bool, has_fields: bool, has_rules: bool) -> Built<()> {
    match (has_of, has_fields, has_rules) {
        (true, true, _) => Err(BuildError::VariantOfFields),
        (true, false, true) => Err(BuildError::VariantOfRules),
        (false, false, true) => Err(BuildError::VariantRulesWithoutFields),
        _ => Ok(()),
    }
}

/// The shape of a variant: a newtype for `of`, a unit variant without
/// fields, and a struct variant otherwise; the parts are checked by
/// [`variant_parts`].
pub fn variant_shape(
    fields: Vec<super::FieldDef>,
    rules: Vec<super::RuleDef>,
    of: Option<crate::name::QualifiedName>,
) -> Built<super::VariantShape> {
    variant_parts(of.is_some(), !fields.is_empty(), !rules.is_empty())?;
    Ok(match of {
        Some(of) => super::VariantShape::Newtype { of },
        None if fields.is_empty() => super::VariantShape::Unit,
        None => super::VariantShape::Struct { fields, rules },
    })
}

/// A rule's `json_ir()` operand: JSONLogic as text.
pub fn json_logic(text: &str) -> Built<Logic> {
    serde_json::from_str(text).map_err(|e| BuildError::JsonIr {
        detail: Some(e.to_string()),
    })
}

/// A rule constant: a boolean, a number, or a string.
pub fn is_constant(x: &Logic) -> bool {
    matches!(x, Logic::Bool(_) | Logic::Number(_) | Logic::String(_))
}

/// An array literal of a rule, `c(...)`, whose items are constants.
pub fn array_literal(items: Vec<Logic>) -> Built<Logic> {
    if items.iter().all(is_constant) {
        Ok(Logic::Array(items.into_boxed_slice()))
    } else {
        Err(BuildError::ArrayLiteral)
    }
}

/// A field, or a path into one, `{"var": "a.b"}`.
pub fn var(path: impl Into<Box<str>>) -> Logic {
    Logic::Var(path.into())
}

/// `is_variant(x, "Name")`, whose variant operand is a string.
pub fn is_variant(x: Logic, variant: Logic) -> Built<Logic> {
    if !matches!(variant, Logic::String(_)) {
        return Err(BuildError::VariantName);
    }
    Ok(call(super::Operator::IsVariant, vec![x, variant]))
}

/// An operation of the dialect with its operands as an array.
pub fn call(op: super::Operator, args: Vec<Logic>) -> Logic {
    Logic::Call {
        op,
        single: false,
        args: args.into_boxed_slice(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn b(min: Option<f64>, exclusive_min: Option<f64>) -> Bounds {
        Bounds {
            min,
            exclusive_min,
            ..Bounds::default()
        }
    }

    #[test]
    fn bounds() {
        assert_eq!(
            double(b(Some(0.0), Some(0.0)), true, None)
                .unwrap_err()
                .to_string(),
            "`min` and `exclusive_min` bound the same side; give one of them."
        );
        assert_eq!(
            double(b(Some(f64::INFINITY), None), true, None).unwrap_err(),
            BuildError::NotOneNumber { arg: "min" }
        );
        let FieldType::Double(d) = double(b(None, Some(0.0)), true, None).unwrap() else {
            panic!()
        };
        assert_eq!(d.exclusive_min(), Some(0.0));
        assert!(d.finite);
    }

    #[test]
    fn integers_are_whole() {
        assert_eq!(
            integer(b(Some(1.5), None), None).unwrap_err(),
            BuildError::NotInteger { arg: "min" }
        );
        assert_eq!(
            integer(Bounds::default(), Some(&[1.0, -2147483648.0])).unwrap_err(),
            BuildError::NotInteger { arg: "values" }
        );
        let FieldType::Integer(i) = integer(b(Some(2.0), None), Some(&[2.0, 3.0])).unwrap() else {
            panic!()
        };
        assert_eq!((i.min(), i.values.as_deref()), (Some(2), Some(&[2, 3][..])));
    }

    #[test]
    fn patterns_compile_at_the_call() {
        assert!(character(Some("^(a|b)$"), None).is_ok());
        assert_eq!(
            character(Some("(a"), None).unwrap_err(),
            BuildError::Pattern {
                pattern: "(a".into()
            }
        );
    }

    #[test]
    fn containers() {
        assert_eq!(
            vec(logical(), Some(3.0), Some(2.0), false).unwrap_err(),
            BuildError::LengthOrder
        );
        assert_eq!(
            vec(logical(), Some(-1.0), None, false).unwrap_err(),
            BuildError::NotCount { arg: "min_length" }
        );
        assert_eq!(
            hashmap(logical(), Some(logical())).unwrap_err(),
            BuildError::Keys
        );
        assert_eq!(
            option(logical(), "computed", None).unwrap_err(),
            BuildError::Compute { given: false }
        );
        assert_eq!(
            option(logical(), "none", Some(var("x"))).unwrap_err(),
            BuildError::Compute { given: true }
        );
    }

    #[test]
    fn rule_dialect() {
        assert_eq!(
            array_literal(vec![Logic::Bool(true), var("x")]).unwrap_err(),
            BuildError::ArrayLiteral
        );
        assert_eq!(
            is_variant(var("k"), var("Rbf")).unwrap_err(),
            BuildError::VariantName
        );
        assert!(is_variant(var("k"), Logic::String("Rbf".into())).is_ok());
    }
}
