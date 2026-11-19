//! Compilation of a JSONLogic expression against the fields in scope.
//!
//! Compiling resolves every `var` path to a field and its static type,
//! checks each operator's operands, checks units, and checks variant names,
//! so a rule's mistakes are declaration errors. The result is a typed tree
//! that `eval` runs.

use serde_json::Value;

use super::eval::Scalar;
use super::ty::{Shape, Ty, TypeLookup, VariantFields, shape};
use crate::ir::{FieldDef, Logic};

/// A problem in a rule expression, located by a JSON Pointer into the
/// expression.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RuleError {
    /// A stable code, such as `unknown_field`.
    pub code: &'static str,
    /// A JSON Pointer into the expression.
    pub path: String,
    /// The message.
    pub message: String,
}

/// Where a `var` path starts.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Root {
    /// The value the rule belongs to: a struct, a variant, or a table row.
    Data,
    /// The element `.x` of an enclosing `all()` or `any()`.
    Element,
}

/// One step of a resolved path.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Step {
    /// An object member: a struct field, an enum value's field, a table
    /// column, or a `hashmap()` key.
    Member(String),
    /// A key of an `ordered_map()`, whose JSON is an array of `{"name",
    /// "value"}` pairs.
    OrderedKey(String),
}

/// How a quantifier reads the elements of its collection.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Collection {
    /// A JSON array (`vec()`).
    Array,
    /// An array of `{"name", "value"}` pairs (`ordered_map()`); the element
    /// is the value.
    OrderedMap,
    /// A JSON object (`hashmap()`); the element is each member's value.
    Hashmap,
    /// Unknown until run time: an array's items or an object's values.
    Dynamic,
}

/// How a value names its enum variant.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum VariantTag {
    /// An enum of unit variants: the value is the variant name.
    UnitOnly,
    /// An enum with a struct or newtype variant: the value is an object whose
    /// tag member holds the variant name.
    Member(String),
    /// The enum is not in the lookup.
    Unknown,
}

/// Operators that take evaluated operands.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Op {
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    Not,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Min,
    Max,
    In,
    Cat,
    Length,
    Subset,
    NonIncreasing,
    NonDecreasing,
    Floor,
    Ceiling,
    Round,
    Nrow,
    Ncol,
    Dim,
}

/// A compiled expression.
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Lit(Scalar),
    Var {
        root: Root,
        steps: Vec<Step>,
    },
    If(Vec<Expr>),
    And(Vec<Expr>),
    Or(Vec<Expr>),
    Op(Op, Vec<Expr>),
    /// `is.null(x)`: a presence test, which does not count as a read.
    IsNull(Box<Expr>),
    IsVariant {
        value: Box<Expr>,
        variant: String,
        tag: VariantTag,
    },
    /// `levels(x)`; `declared` holds the levels a factor type declares.
    Levels {
        value: Box<Expr>,
        declared: Option<Vec<String>>,
    },
    Quantifier {
        all: bool,
        collection: Box<Expr>,
        kind: Collection,
        condition: Box<Expr>,
    },
}

struct Compiler<'a> {
    lookup: &'a dyn TypeLookup,
    fields: &'a [FieldDef],
    element: Option<Ty>,
    errors: Vec<RuleError>,
}

fn pointer_segment(s: &str) -> String {
    s.replace('~', "~0").replace('/', "~1")
}

fn unit_of(t: &Ty) -> Option<&str> {
    match t {
        Ty::Number { unit } => unit.as_deref(),
        _ => None,
    }
}

impl Compiler<'_> {
    fn error(&mut self, code: &'static str, path: &str, message: String) -> (Expr, Ty) {
        self.errors.push(RuleError {
            code,
            path: path.to_string(),
            message,
        });
        (Expr::Lit(Scalar::Null), Ty::Any)
    }

    fn compile(&mut self, logic: &Value, path: &str) -> (Expr, Ty) {
        match logic {
            Value::Bool(b) => (Expr::Lit(Scalar::Bool(*b)), Ty::Bool),
            Value::Number(n) => (
                Expr::Lit(Scalar::Num(n.as_f64().unwrap_or(f64::NAN))),
                Ty::Number { unit: None },
            ),
            Value::String(t) => (Expr::Lit(Scalar::Str(t.clone())), Ty::String),
            Value::Null => (Expr::Lit(Scalar::Null), Ty::Any),
            Value::Array(items) => {
                let mut elem = None;
                let mut out = Vec::new();
                for (i, item) in items.iter().enumerate() {
                    let (e, t) = self.compile(item, &format!("{path}/{i}"));
                    if !matches!(e, Expr::Lit(_)) {
                        return self.error(
                            "rule_array_literal",
                            &format!("{path}/{i}"),
                            "an array literal holds literal values only".to_string(),
                        );
                    }
                    elem.get_or_insert(t);
                    out.push(match e {
                        Expr::Lit(v) => v,
                        _ => unreachable!(),
                    });
                }
                (
                    Expr::Lit(Scalar::Array(out)),
                    Ty::Array(Box::new(elem.unwrap_or(Ty::Any))),
                )
            }
            Value::Object(map) if map.len() == 1 => {
                let (op, args) = map.iter().next().expect("one member");
                let raw: Vec<&Value> = match args {
                    Value::Array(a) => a.iter().collect(),
                    single => vec![single],
                };
                self.operation(op, &raw, &format!("{path}/{}", pointer_segment(op)))
            }
            Value::Object(map) => self.error(
                "rule_operation",
                path,
                format!(
                    "an operation is an object with one member, found {}",
                    map.len()
                ),
            ),
        }
    }

    fn arity(
        &mut self,
        op: &str,
        args: &[&Value],
        n: std::ops::RangeInclusive<usize>,
        path: &str,
    ) -> bool {
        if n.contains(&args.len()) {
            return true;
        }
        let want = if n.start() == n.end() {
            n.start().to_string()
        } else if *n.end() == usize::MAX {
            format!("at least {}", n.start())
        } else {
            format!("{} to {}", n.start(), n.end())
        };
        self.error(
            "rule_arity",
            path,
            format!("`{op}` takes {want} operands, found {}", args.len()),
        );
        false
    }

    fn operands(&mut self, args: &[&Value], path: &str) -> Vec<(Expr, Ty)> {
        args.iter()
            .enumerate()
            .map(|(i, a)| self.compile(a, &format!("{path}/{i}")))
            .collect()
    }

    fn expect_bool(&mut self, t: &Ty, path: &str, what: &str) {
        if !matches!(t, Ty::Bool | Ty::Any | Ty::Json) {
            self.error(
                "rule_not_logical",
                path,
                format!("{what} is a logical condition, found {}", t.describe()),
            );
        }
    }

    fn expect_number(&mut self, t: &Ty, path: &str, op: &str) {
        if !matches!(t, Ty::Number { .. } | Ty::Any | Ty::Json) {
            self.error(
                "rule_type",
                path,
                format!("`{op}` takes numbers, found {}", t.describe()),
            );
        }
    }

    /// Units of two operands that must agree; returns the unit of the result.
    fn same_unit(&mut self, a: &Ty, b: &Ty, path: &str, op: &str) -> Option<String> {
        match (unit_of(a), unit_of(b)) {
            (Some(x), Some(y)) if x != y => {
                self.error(
                    "rule_unit",
                    path,
                    format!("`{op}` combines `{x}` and `{y}`; convert one operand, for example by multiplying it by a factor"),
                );
                None
            }
            (Some(x), _) | (_, Some(x)) => Some(x.to_string()),
            (None, None) => None,
        }
    }

    fn comparable(&mut self, a: &Ty, b: &Ty, path: &str, op: &str) {
        let ok = matches!(
            (a, b),
            (Ty::Any | Ty::Json, _)
                | (_, Ty::Any | Ty::Json)
                | (Ty::Number { .. }, Ty::Number { .. })
                | (Ty::Date, Ty::Date)
                | (Ty::Posixct, Ty::Posixct)
        );
        if ok {
            self.same_unit(a, b, path, op);
        } else {
            self.error(
                "rule_type",
                path,
                format!(
                    "`{op}` compares two numbers, two dates, or two date-times, found {} and {}",
                    a.describe(),
                    b.describe()
                ),
            );
        }
    }

    fn equality(&mut self, a: &Ty, b: &Ty, path: &str, op: &str) {
        fn compatible(a: &Ty, b: &Ty) -> bool {
            match (a, b) {
                (Ty::Any | Ty::Json, _) | (_, Ty::Any | Ty::Json) => true,
                (Ty::Array(x), Ty::Array(y)) => compatible(x, y),
                (Ty::Factor { .. }, Ty::Array(x)) | (Ty::Array(x), Ty::Factor { .. }) => {
                    compatible(x, &Ty::String)
                }
                (Ty::Number { .. }, Ty::Number { .. }) => true,
                (Ty::Map { value: x, .. }, Ty::Map { value: y, .. }) => compatible(x, y),
                (Ty::Matrix(x), Ty::Matrix(y)) => compatible(x, y),
                (Ty::Named(x), Ty::Named(y)) => x == y,
                // Two logicals, strings, dates, date-times, factors, or
                // tables.
                _ => std::mem::discriminant(a) == std::mem::discriminant(b),
            }
        }
        if !compatible(a, b) {
            let hint = match (a, b) {
                (Ty::Named(q), Ty::String) | (Ty::String, Ty::Named(q)) => {
                    format!(
                        "; to test an enum's variant, write `is_variant(x, \"Variant\")` for `{q}`"
                    )
                }
                _ => String::new(),
            };
            self.error(
                "rule_type",
                path,
                format!(
                    "`{op}` compares {} with {}, which are never equal{hint}",
                    a.describe(),
                    b.describe()
                ),
            );
            return;
        }
        self.same_unit(a, b, path, op);
    }

    fn var(&mut self, args: &[&Value], path: &str) -> (Expr, Ty) {
        if !self.arity("var", args, 1..=1, path) {
            return (Expr::Lit(Scalar::Null), Ty::Any);
        }
        let Some(p) = args[0].as_str() else {
            return self.error("rule_var", path, "`var` takes a path string".to_string());
        };
        let mut segments: Vec<&str> = p.split('.').collect();
        let mut steps = Vec::new();
        let (root, mut ty) = if p == ".x" || p.starts_with(".x.") {
            segments.drain(..2);
            match &self.element {
                Some(t) => (Root::Element, t.clone()),
                None => {
                    return self.error(
                        "rule_var",
                        path,
                        "`.x` is the element of an enclosing `all()` or `any()`".to_string(),
                    );
                }
            }
        } else {
            let first = segments.remove(0);
            match self.fields.iter().find(|f| f.name.as_str() == first) {
                Some(f) => {
                    steps.push(Step::Member(first.to_string()));
                    (Root::Data, Ty::of(&f.ty, f.unit.as_deref()))
                }
                None => {
                    return self.error(
                        "rule_field",
                        path,
                        format!("`{first}` is not a field in scope"),
                    );
                }
            }
        };
        for seg in segments {
            match self.step(&ty, seg, path) {
                Some((s, t)) => {
                    steps.push(s);
                    ty = t;
                }
                None => return (Expr::Lit(Scalar::Null), Ty::Any),
            }
        }
        if ty == Ty::Runtime {
            return self.error(
                "rule_type",
                path,
                format!("`{p}` is a runtime-only R value, which rules cannot read"),
            );
        }
        (Expr::Var { root, steps }, ty)
    }

    fn step(&mut self, ty: &Ty, seg: &str, path: &str) -> Option<(Step, Ty)> {
        let member = |t: Ty| Some((Step::Member(seg.to_string()), t));
        match ty {
            Ty::Any | Ty::Json => member(Ty::Any),
            Ty::Table { columns, open } => match columns.iter().find(|c| c.name.as_str() == seg) {
                // A column is a vector of its type; a factor is one already.
                Some(c) => member(match Ty::of(&c.ty, c.unit.as_deref()) {
                    f @ Ty::Factor { .. } => f,
                    t => Ty::Array(Box::new(t)),
                }),
                None if *open => member(Ty::Array(Box::new(Ty::Any))),
                None => {
                    self.error(
                        "rule_field",
                        path,
                        format!("`{seg}` is not a column of this table"),
                    );
                    None
                }
            },
            Ty::Map { value, ordered } => {
                let s = if *ordered {
                    Step::OrderedKey(seg.to_string())
                } else {
                    Step::Member(seg.to_string())
                };
                Some((s, (**value).clone()))
            }
            Ty::Named(q) => match shape(self.lookup, q) {
                None => member(Ty::Any),
                Some(Shape::Struct(fields)) => match fields.iter().find(|f| f.name.as_str() == seg)
                {
                    Some(f) => member(Ty::of(&f.ty, f.unit.as_deref())),
                    None => {
                        self.error(
                            "rule_field",
                            path,
                            format!("`{seg}` is not a field of `{q}`"),
                        );
                        None
                    }
                },
                Some(Shape::Enum { variants, .. }) => {
                    let mut found: Option<Ty> = None;
                    let mut unknown = false;
                    for (_, vf) in &variants {
                        let fields = match vf {
                            VariantFields::None => continue,
                            VariantFields::Fields(f) => *f,
                            VariantFields::Wrapped(Some(f)) => *f,
                            VariantFields::Wrapped(None) => {
                                unknown = true;
                                continue;
                            }
                        };
                        if let Some(f) = fields.iter().find(|f| f.name.as_str() == seg) {
                            let t = Ty::of(&f.ty, f.unit.as_deref());
                            match &found {
                                Some(prev) if *prev != t => {
                                    self.error(
                                        "rule_field_ambiguous",
                                        path,
                                        format!("`{seg}` has different types in different variants of `{q}`"),
                                    );
                                    return None;
                                }
                                _ => found = Some(t),
                            }
                        }
                    }
                    match found {
                        Some(t) => member(t),
                        None if unknown => member(Ty::Any),
                        None => {
                            self.error(
                                "rule_field",
                                path,
                                format!("no variant of `{q}` has a field `{seg}`"),
                            );
                            None
                        }
                    }
                }
            },
            other => {
                self.error(
                    "rule_field",
                    path,
                    format!(
                        "`{seg}` reads a field of {}, which has none",
                        other.describe()
                    ),
                );
                None
            }
        }
    }

    fn operation(&mut self, op: &str, args: &[&Value], path: &str) -> (Expr, Ty) {
        match op {
            "var" => return self.var(args, path),
            "==" | "!=" => {
                let strict = if op == "==" { "===" } else { "!==" };
                return self.error(
                    "rule_loose_equality",
                    path,
                    format!("`{op}` is JSONLogic's loose equality; R8 rules write strict equality as `{strict}`"),
                );
            }
            "is_null" => {
                if !self.arity(op, args, 1..=1, path) {
                    return (Expr::Lit(Scalar::Null), Ty::Bool);
                }
                let before = self.errors.len();
                let (e, _) = self.compile(args[0], &format!("{path}/0"));
                if self.errors.len() > before {
                    return (Expr::Lit(Scalar::Null), Ty::Bool);
                }
                if !matches!(e, Expr::Var { .. }) {
                    return self.error("rule_type", path, "`is_null` takes a field".to_string());
                }
                return (Expr::IsNull(Box::new(e)), Ty::Bool);
            }
            "is_variant" => return self.is_variant(args, path),
            "levels" => {
                if !self.arity(op, args, 1..=1, path) {
                    return (Expr::Lit(Scalar::Null), Ty::Any);
                }
                let (e, t) = self.compile(args[0], &format!("{path}/0"));
                let declared = match &t {
                    Ty::Factor { levels } => levels.clone(),
                    Ty::Any | Ty::Json => None,
                    other => {
                        return self.error(
                            "rule_type",
                            path,
                            format!("`levels` takes a factor, found {}", other.describe()),
                        );
                    }
                };
                return (
                    Expr::Levels {
                        value: Box::new(e),
                        declared,
                    },
                    Ty::Array(Box::new(Ty::String)),
                );
            }
            "all" | "any" => return self.quantifier(op == "all", args, path),
            "if" => {
                if !self.arity(op, args, 3..=usize::MAX, path) || args.len().is_multiple_of(2) {
                    return self.error(
                        "rule_arity",
                        path,
                        "`if` takes condition and value pairs and a final value".to_string(),
                    );
                }
                let parts = self.operands(args, path);
                let mut result = Ty::Any;
                for (i, (_, t)) in parts.iter().enumerate() {
                    if i % 2 == 0 && i + 1 < parts.len() {
                        self.expect_bool(t, &format!("{path}/{i}"), "an `if` condition");
                    } else if result == Ty::Any {
                        result = t.clone();
                    }
                }
                return (
                    Expr::If(parts.into_iter().map(|(e, _)| e).collect()),
                    result,
                );
            }
            "and" | "or" => {
                if !self.arity(op, args, 1..=usize::MAX, path) {
                    return (Expr::Lit(Scalar::Null), Ty::Bool);
                }
                let parts = self.operands(args, path);
                for (i, (_, t)) in parts.iter().enumerate() {
                    self.expect_bool(t, &format!("{path}/{i}"), &format!("an operand of `{op}`"));
                }
                let es = parts.into_iter().map(|(e, _)| e).collect();
                return (
                    if op == "and" {
                        Expr::And(es)
                    } else {
                        Expr::Or(es)
                    },
                    Ty::Bool,
                );
            }
            _ => {}
        }
        let (code, range) = match op {
            "===" => (Op::Eq, 2..=2),
            "!==" => (Op::Ne, 2..=2),
            "<" => (Op::Lt, 2..=2),
            "<=" => (Op::Le, 2..=2),
            ">" => (Op::Gt, 2..=2),
            ">=" => (Op::Ge, 2..=2),
            "!" => (Op::Not, 1..=1),
            "+" => (Op::Add, 2..=usize::MAX),
            "-" => (Op::Sub, 1..=2),
            "*" => (Op::Mul, 2..=usize::MAX),
            "/" => (Op::Div, 2..=2),
            "%" => (Op::Mod, 2..=2),
            "min" => (Op::Min, 1..=usize::MAX),
            "max" => (Op::Max, 1..=usize::MAX),
            "in" => (Op::In, 2..=2),
            "cat" => (Op::Cat, 1..=usize::MAX),
            "length" => (Op::Length, 1..=1),
            "subset" => (Op::Subset, 2..=2),
            "non_increasing" => (Op::NonIncreasing, 1..=1),
            "non_decreasing" => (Op::NonDecreasing, 1..=1),
            "floor" => (Op::Floor, 1..=1),
            "ceiling" => (Op::Ceiling, 1..=1),
            "round" => (Op::Round, 1..=1),
            "nrow" => (Op::Nrow, 1..=1),
            "ncol" => (Op::Ncol, 1..=1),
            "dim" => (Op::Dim, 1..=1),
            other => {
                return self.error(
                    "rule_operator",
                    path,
                    format!("`{other}` is not an operator of R8 rules"),
                );
            }
        };
        if !self.arity(op, args, range, path) {
            return (Expr::Lit(Scalar::Null), Ty::Any);
        }
        let parts = self.operands(args, path);
        let tys: Vec<Ty> = parts.iter().map(|(_, t)| t.clone()).collect();
        let result = self.type_op(code, op, &tys, path);
        (
            Expr::Op(code, parts.into_iter().map(|(e, _)| e).collect()),
            result,
        )
    }

    fn type_op(&mut self, code: Op, op: &str, t: &[Ty], path: &str) -> Ty {
        let num = Ty::Number { unit: None };
        match code {
            Op::Eq | Op::Ne => {
                self.equality(&t[0], &t[1], path, op);
                Ty::Bool
            }
            Op::Lt | Op::Le | Op::Gt | Op::Ge => {
                self.comparable(&t[0], &t[1], path, op);
                Ty::Bool
            }
            Op::Not => {
                self.expect_bool(&t[0], &format!("{path}/0"), "the operand of `!`");
                Ty::Bool
            }
            Op::Add | Op::Sub | Op::Min | Op::Max => {
                for (i, x) in t.iter().enumerate() {
                    self.expect_number(x, &format!("{path}/{i}"), op);
                }
                let mut unit = unit_of(&t[0]).map(str::to_string);
                for x in &t[1..] {
                    let a = Ty::Number { unit: unit.clone() };
                    unit = self.same_unit(&a, x, path, op);
                }
                Ty::Number { unit }
            }
            Op::Mul | Op::Div | Op::Mod => {
                for (i, x) in t.iter().enumerate() {
                    self.expect_number(x, &format!("{path}/{i}"), op);
                }
                num
            }
            Op::Floor | Op::Ceiling | Op::Round => {
                self.expect_number(&t[0], &format!("{path}/0"), op);
                Ty::Number {
                    unit: unit_of(&t[0]).map(str::to_string),
                }
            }
            Op::In => {
                if !matches!(
                    t[1],
                    Ty::Array(_) | Ty::String | Ty::Factor { .. } | Ty::Any | Ty::Json
                ) {
                    self.error(
                        "rule_type",
                        &format!("{path}/1"),
                        format!(
                            "`in` searches an array or a string, found {}",
                            t[1].describe()
                        ),
                    );
                }
                Ty::Bool
            }
            Op::Cat => {
                for (i, x) in t.iter().enumerate() {
                    if !matches!(x, Ty::String | Ty::Number { .. } | Ty::Any | Ty::Json) {
                        self.error(
                            "rule_type",
                            &format!("{path}/{i}"),
                            format!("`cat` joins strings and numbers, found {}", x.describe()),
                        );
                    }
                }
                Ty::String
            }
            Op::Length => {
                if !matches!(
                    t[0],
                    Ty::Array(_) | Ty::String | Ty::Factor { .. } | Ty::Any | Ty::Json
                ) {
                    self.error("rule_type", &format!("{path}/0"), format!("`length` takes an array, a factor, or a string, found {}; use `nrow()` for a table", t[0].describe()));
                }
                num
            }
            Op::Subset => {
                for (i, x) in t.iter().enumerate() {
                    if !matches!(x, Ty::Array(_) | Ty::Factor { .. } | Ty::Any | Ty::Json) {
                        self.error(
                            "rule_type",
                            &format!("{path}/{i}"),
                            format!("`subset` takes arrays, found {}", x.describe()),
                        );
                    }
                }
                Ty::Bool
            }
            Op::NonIncreasing | Op::NonDecreasing => {
                let ok = match &t[0] {
                    Ty::Array(e) => {
                        matches!(**e, Ty::Number { .. } | Ty::Date | Ty::Posixct | Ty::Any)
                    }
                    Ty::Any | Ty::Json => true,
                    _ => false,
                };
                if !ok {
                    self.error(
                        "rule_type",
                        &format!("{path}/0"),
                        format!(
                            "`{op}` takes an array of numbers, dates, or date-times, found {}",
                            t[0].describe()
                        ),
                    );
                }
                Ty::Bool
            }
            Op::Nrow | Op::Ncol => {
                if !matches!(t[0], Ty::Table { .. } | Ty::Matrix(_) | Ty::Any | Ty::Json) {
                    self.error(
                        "rule_type",
                        &format!("{path}/0"),
                        format!(
                            "`{op}` takes a table or a matrix, found {}",
                            t[0].describe()
                        ),
                    );
                }
                num
            }
            Op::Dim => {
                if !matches!(t[0], Ty::Matrix(_) | Ty::Any | Ty::Json) {
                    self.error(
                        "rule_type",
                        &format!("{path}/0"),
                        format!(
                            "`dim` takes a matrix or an array, found {}",
                            t[0].describe()
                        ),
                    );
                }
                Ty::Array(Box::new(num))
            }
        }
    }

    fn is_variant(&mut self, args: &[&Value], path: &str) -> (Expr, Ty) {
        if !self.arity("is_variant", args, 2..=2, path) {
            return (Expr::Lit(Scalar::Null), Ty::Bool);
        }
        let (e, t) = self.compile(args[0], &format!("{path}/0"));
        let Some(variant) = args[1].as_str() else {
            return self.error(
                "rule_type",
                &format!("{path}/1"),
                "`is_variant` takes the variant name as a string".to_string(),
            );
        };
        let tag = match &t {
            Ty::Named(q) => match shape(self.lookup, q) {
                Some(Shape::Enum { tag, variants }) => {
                    if !variants.iter().any(|(n, _)| *n == variant) {
                        return self.error(
                            "rule_variant",
                            &format!("{path}/1"),
                            format!("`{variant}` is not a variant of `{q}`"),
                        );
                    }
                    match tag {
                        Some(m) => VariantTag::Member(m.to_string()),
                        None => VariantTag::UnitOnly,
                    }
                }
                Some(Shape::Struct(_)) => {
                    return self.error(
                        "rule_type",
                        &format!("{path}/0"),
                        format!("`is_variant` takes an enum; `{q}` is a struct"),
                    );
                }
                None => VariantTag::Unknown,
            },
            Ty::Any | Ty::Json => VariantTag::Unknown,
            other => {
                return self.error(
                    "rule_type",
                    &format!("{path}/0"),
                    format!("`is_variant` takes an enum, found {}", other.describe()),
                );
            }
        };
        (
            Expr::IsVariant {
                value: Box::new(e),
                variant: variant.to_string(),
                tag,
            },
            Ty::Bool,
        )
    }

    fn quantifier(&mut self, all: bool, args: &[&Value], path: &str) -> (Expr, Ty) {
        let op = if all { "all" } else { "any" };
        if !self.arity(op, args, 2..=2, path) {
            return (Expr::Lit(Scalar::Null), Ty::Bool);
        }
        let (coll, t) = self.compile(args[0], &format!("{path}/0"));
        let (kind, elem) = match &t {
            Ty::Array(e) => (Collection::Array, (**e).clone()),
            Ty::Map {
                value,
                ordered: true,
            } => (Collection::OrderedMap, (**value).clone()),
            Ty::Map {
                value,
                ordered: false,
            } => (Collection::Hashmap, (**value).clone()),
            Ty::Any | Ty::Json => (Collection::Dynamic, Ty::Any),
            other => {
                return self.error(
                    "rule_type",
                    &format!("{path}/0"),
                    format!(
                        "`{op}` runs over a vec, an ordered map, or a hashmap, found {}",
                        other.describe()
                    ),
                );
            }
        };
        let outer = self.element.replace(elem);
        let (cond, ct) = self.compile(args[1], &format!("{path}/1"));
        self.element = outer;
        self.expect_bool(
            &ct,
            &format!("{path}/1"),
            &format!("the condition of `{op}`"),
        );
        (
            Expr::Quantifier {
                all,
                collection: Box::new(coll),
                kind,
                condition: Box::new(cond),
            },
            Ty::Bool,
        )
    }
}

/// Compiles one expression in the scope of `fields`. `what` names the
/// expression's role in a message, such as "a rule's invariant"; `logical`
/// requires a logical result.
pub fn compile(
    logic: &Logic,
    fields: &[FieldDef],
    lookup: &dyn TypeLookup,
    logical: Option<&str>,
) -> Result<Expr, Vec<RuleError>> {
    let mut c = Compiler {
        lookup,
        fields,
        element: None,
        errors: Vec::new(),
    };
    let (expr, ty) = c.compile(&logic.0, "");
    if let Some(what) = logical {
        c.expect_bool(&ty, "", what);
    }
    if c.errors.is_empty() {
        Ok(expr)
    } else {
        Err(c.errors)
    }
}

/// Compiles a computed field's expression and checks its result against the
/// target field: a number for a numeric target, in the target's unit when
/// both carry one.
pub fn compile_computed(
    logic: &Logic,
    fields: &[FieldDef],
    lookup: &dyn TypeLookup,
    target: &Ty,
) -> Result<Expr, Vec<RuleError>> {
    let mut c = Compiler {
        lookup,
        fields,
        element: None,
        errors: Vec::new(),
    };
    let (expr, ty) = c.compile(&logic.0, "");
    if let Ty::Number { .. } = target {
        c.expect_number(&ty, "", "a computed numeric field");
        c.same_unit(target, &ty, "", "the computed field's value and its target");
    }
    if c.errors.is_empty() {
        Ok(expr)
    } else {
        Err(c.errors)
    }
}
