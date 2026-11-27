//! Compilation of a JSONLogic expression against the fields in scope.
//!
//! Compiling binds every `var` path to a field and its static type,
//! checks each operator's operands, checks units, and checks variant names,
//! so a rule's mistakes are declaration errors. The result is a typed tree
//! that `eval` runs.

use super::eval::{Scalar, unwrap_option};
use super::ty::{Members, Ty, TypeLookup, VariantFields, members};
use crate::access::{valid_date, valid_posixct};
use crate::canonical::{date_days, datetime_micros};
use crate::ir::{FieldDef, FieldType, Logic, Operator};

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

/// One step of a bound path.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Step {
    /// A field of a struct or an enum value, or a member of a value of
    /// unknown type.
    Field(String),
    /// A key of a `btree_map()`.
    Key(String),
    /// A column of a table.
    Column(String),
    /// A key of an `index_map()`, whose JSON is an array of `{"name",
    /// "value"}` pairs.
    OrderedKey(String),
}

/// How a quantifier reads the elements of its collection.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Collection {
    /// A JSON array (`vec()`).
    Array,
    /// An array of `{"name", "value"}` pairs (`index_map()`); the element
    /// is the value.
    IndexMap,
    /// A JSON object (`btree_map()`); the element is each member's value.
    BtreeMap,
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
    Abs,
    Sum,
    Nchar,
    Mean,
    Sqrt,
    Log,
    Exp,
    Pow,
    Median,
    /// `n()`: the rows of a table or of a group.
    N,
    /// `count(cond)`: the rows where the condition is true.
    Count,
}

/// A column of the table whose rows an expression reads.
#[derive(Debug, Clone, PartialEq)]
pub struct Cell {
    /// The column's name.
    pub name: Box<str>,
    /// The column's type.
    pub ty: FieldType,
    /// The cells are not shown.
    pub sensitive: bool,
}

/// The columns of the table whose rows an expression reads.
pub trait RowScope {
    /// The column `name` and its unit, `None` where there is none of the name,
    /// and an error where its cells do not read.
    fn column(&self, name: &str) -> Option<Result<(Cell, Option<String>), String>>;
    /// The name nearest to `name`.
    fn nearest(&self, name: &str) -> Option<String>;
}

impl<C: crate::ir::Column> RowScope for &[C] {
    fn column(&self, name: &str) -> Option<Result<(Cell, Option<String>), String>> {
        let f = self
            .iter()
            .map(|c| c.field())
            .find(|f| f.name.as_str() == name)?;
        let cell = Cell {
            name: f.name.as_str().into(),
            ty: f.ty.clone(),
            sensitive: f.sensitive,
        };
        Some(Ok((cell, f.unit.as_deref().map(str::to_string))))
    }

    fn nearest(&self, name: &str) -> Option<String> {
        crate::name::nearest(name, self.iter().map(|c| c.field().name.as_str())).map(str::to_string)
    }
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
    /// A cell of the row: the column's place among those that the rule reads.
    Cell(usize),
    /// `mean`, `sum`, `median`, `min`, `max`, `n`, or `count` over all rows of
    /// every operand, or over the rows of each group; `at` is its place among
    /// the rule's. `skip` leaves out the missing cells (`na.rm = TRUE`).
    Aggregate {
        op: Op,
        operands: Vec<Expr>,
        at: usize,
        skip: bool,
    },
    /// `is.null(x)`: a presence test, which does not count as a read.
    IsNull(Box<Expr>),
    /// `is.na(x)`: missing, or `NaN`.
    IsNa(Box<Expr>),
    IsVariant {
        value: Box<Expr>,
        variant: String,
        tag: VariantTag,
    },
    /// `levels(x)`; `declared` holds the levels a factor type declares.
    Levels {
        value: Box<Expr>,
        declared: Option<Box<[Box<str>]>>,
    },
    Quantifier {
        all: bool,
        collection: Box<Expr>,
        kind: Collection,
        condition: Box<Expr>,
    },
}

/// What the names of an expression are.
#[derive(Clone, Copy)]
enum Names<'a> {
    /// The fields of a value.
    Fields(&'a [FieldDef]),
    /// The columns of a table, whose cells a row reads; `aggregates` admits
    /// the operators over all rows.
    Columns {
        columns: &'a dyn RowScope,
        aggregates: bool,
    },
}

struct Compiler<'a> {
    lookup: &'a dyn TypeLookup,
    names: Names<'a>,
    element: Option<Ty>,
    errors: Vec<RuleError>,
    /// The columns that the expression reads.
    cells: Vec<Cell>,
    /// The number of aggregates.
    aggregates: usize,
}

/// Whether an expression reads a cell.
fn reads_cell(e: &Expr) -> bool {
    match e {
        Expr::Cell(_) | Expr::Aggregate { .. } => true,
        Expr::If(xs) | Expr::And(xs) | Expr::Or(xs) | Expr::Op(_, xs) => xs.iter().any(reads_cell),
        Expr::IsNa(x) | Expr::IsNull(x) => reads_cell(x),
        _ => false,
    }
}

/// Whether an expression reads a cell outside an aggregate.
pub(crate) fn reads_cell_outside(e: &Expr) -> bool {
    match e {
        Expr::Cell(_) => true,
        Expr::Aggregate { .. } => false,
        Expr::If(xs) | Expr::And(xs) | Expr::Or(xs) | Expr::Op(_, xs) => {
            xs.iter().any(reads_cell_outside)
        }
        Expr::IsNa(x) | Expr::IsNull(x) => reads_cell_outside(x),
        _ => false,
    }
}

/// The levels that the type of a column declares.
fn levels(ty: &FieldType) -> Option<&[Box<str>]> {
    match unwrap_option(ty) {
        FieldType::Factor { levels, .. } => levels.as_deref(),
        _ => None,
    }
}

fn pointer_segment(s: &str) -> String {
    s.replace('~', "~0").replace('/', "~1")
}

/// The hint of a comparison of an array: `all` tests a condition on each
/// element, and a table's row rules compare paired columns.
fn elementwise_hint(a: &Ty, b: &Ty) -> &'static str {
    match (a, b) {
        (Ty::Array(_), Ty::Array(_)) => {
            "; to compare paired elements of two arrays, declare them as columns of a table and write a row rule"
        }
        (Ty::Array(_), _) | (_, Ty::Array(_)) => {
            "; to test each element of an array, write the comparison as the condition of `all` over the array"
        }
        _ => "",
    }
}

/// Whether a number holds an integer exactly; an operand of another or an
/// unknown type counts as exact, so that only a known double is flagged.
fn exact_of(t: &Ty) -> bool {
    match t {
        Ty::Number { exact, .. } => *exact,
        _ => true,
    }
}

/// A type with every number's `exact` set, for comparing the types of a
/// field in two variants, where an integer and a double are both numbers.
fn erase_exact(t: &Ty) -> Ty {
    match t {
        Ty::Number { unit, .. } => Ty::Number {
            unit: unit.clone(),
            exact: true,
        },
        Ty::Array(e) => Ty::Array(Box::new(erase_exact(e))),
        Ty::Matrix(e) => Ty::Matrix(Box::new(erase_exact(e))),
        Ty::Map { value, ordered } => Ty::Map {
            value: Box::new(erase_exact(value)),
            ordered: *ordered,
        },
        other => other.clone(),
    }
}

fn unit_of(t: &Ty) -> Option<&str> {
    match t {
        Ty::Number { unit, .. } => unit.as_deref(),
        _ => None,
    }
}

/// The compiled operator and the number of operands of an operator whose
/// operands are evaluated; `None` for the operators that compile specially.
fn evaluated(op: Operator) -> Option<(Op, std::ops::RangeInclusive<usize>)> {
    use Operator as O;
    Some(match op {
        O::StrictEq => (Op::Eq, 2..=2),
        O::StrictNe => (Op::Ne, 2..=2),
        O::Lt => (Op::Lt, 2..=2),
        O::Le => (Op::Le, 2..=2),
        O::Gt => (Op::Gt, 2..=2),
        O::Ge => (Op::Ge, 2..=2),
        O::Not => (Op::Not, 1..=1),
        O::Add => (Op::Add, 2..=usize::MAX),
        O::Sub => (Op::Sub, 1..=2),
        O::Mul => (Op::Mul, 2..=usize::MAX),
        O::Div => (Op::Div, 2..=2),
        O::Mod => (Op::Mod, 2..=2),
        O::Min => (Op::Min, 1..=usize::MAX),
        O::Max => (Op::Max, 1..=usize::MAX),
        O::In => (Op::In, 2..=2),
        O::Cat => (Op::Cat, 1..=usize::MAX),
        O::Length => (Op::Length, 1..=1),
        O::Subset => (Op::Subset, 2..=2),
        O::NonIncreasing => (Op::NonIncreasing, 1..=1),
        O::NonDecreasing => (Op::NonDecreasing, 1..=1),
        O::Floor => (Op::Floor, 1..=1),
        O::Ceiling => (Op::Ceiling, 1..=1),
        O::Round => (Op::Round, 1..=1),
        O::Nrow => (Op::Nrow, 1..=1),
        O::Ncol => (Op::Ncol, 1..=1),
        O::Dim => (Op::Dim, 1..=1),
        O::Abs => (Op::Abs, 1..=1),
        O::Sum => (Op::Sum, 1..=1),
        O::Nchar => (Op::Nchar, 1..=1),
        O::Mean => (Op::Mean, 1..=1),
        O::Pmin => (Op::Min, 2..=usize::MAX),
        O::Pmax => (Op::Max, 2..=usize::MAX),
        O::Sqrt => (Op::Sqrt, 1..=1),
        O::Log => (Op::Log, 1..=1),
        O::Exp => (Op::Exp, 1..=1),
        O::Pow => (Op::Pow, 2..=2),
        O::Median => (Op::Median, 1..=1),
        O::Var
        | O::Path
        | O::IsNa
        | O::LooseEq
        | O::LooseNe
        | O::IsNull
        | O::IsVariant
        | O::Levels
        | O::All
        | O::Any
        | O::If
        | O::And
        | O::Or
        | O::N
        | O::Count
        | O::NaRm
        | O::Col => return None,
    })
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

    fn compile(&mut self, logic: &Logic, path: &str) -> (Expr, Ty) {
        match logic {
            Logic::Bool(b) => (Expr::Lit(Scalar::Bool(*b)), Ty::Bool),
            // Rules evaluate in doubles: a number a double does not hold
            // exactly, an integer beyond 2^53, would compare as another.
            Logic::Number(n) if !n.is_exact_f64() => self.error(
                "rule_number",
                path,
                format!(
                    "{} is beyond 2^53, which a double, the number of rules, does not hold exactly",
                    n.as_f64()
                ),
            ),
            Logic::Number(n) => (
                Expr::Lit(Scalar::Num(n.as_f64())),
                Ty::Number {
                    unit: None,
                    exact: n.as_i64().is_some() || n.as_u64().is_some(),
                },
            ),
            Logic::String(t) => (Expr::Lit(Scalar::Str(t.to_string())), Ty::String),
            Logic::Null => (Expr::Lit(Scalar::Null), Ty::Any),
            Logic::Array(items) => {
                let mut elem = None;
                let mut out = Vec::new();
                for (i, item) in items.iter().enumerate() {
                    let (e, t) = self.compile(item, &format!("{path}/{i}"));
                    let Expr::Lit(v) = e else {
                        return self.error(
                            "rule_array_literal",
                            &format!("{path}/{i}"),
                            "an array literal holds literal values only".to_string(),
                        );
                    };
                    elem.get_or_insert(t);
                    out.push(v);
                }
                (
                    Expr::Lit(Scalar::Array(out)),
                    Ty::Array(Box::new(elem.unwrap_or(Ty::Any))),
                )
            }
            Logic::Var(p) => {
                let at = format!("{path}/var");
                match p.strip_prefix(".x.") {
                    Some(rest) => {
                        let names: Vec<&str> = [".x"].into_iter().chain(rest.split('.')).collect();
                        self.var_path(&names, &at)
                    }
                    None if &**p == ".x" => self.var_path(&[".x"], &at),
                    None => self.var_path(&p.split('.').collect::<Vec<_>>(), &at),
                }
            }
            Logic::Call { op, args, .. } => {
                let args: Vec<&Logic> = args.iter().collect();
                self.operation(
                    *op,
                    &args,
                    &format!("{path}/{}", pointer_segment(op.as_str())),
                )
            }
            Logic::Other(v) => match &**v {
                crate::tree::Tree::Object(m) if m.len() == 1 => {
                    let op = &*m[0].0;
                    self.error(
                        "rule_operator",
                        &format!("{path}/{}", pointer_segment(op)),
                        format!("`{op}` is not an operator of R8 rules"),
                    )
                }
                crate::tree::Tree::Object(m) => self.error(
                    "rule_operation",
                    path,
                    format!(
                        "an operation is an object with one member, found {}",
                        m.len()
                    ),
                ),
                _ => self.error(
                    "rule_operation",
                    path,
                    "an operation is an object with one member".to_string(),
                ),
            },
        }
    }

    fn arity(
        &mut self,
        op: &str,
        args: &[&Logic],
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

    fn operands(&mut self, args: &[&Logic], path: &str) -> Vec<(Expr, Ty)> {
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
                    "`{op}` compares two numbers, two dates, or two date-times, found {} and {}{}",
                    a.describe(),
                    b.describe(),
                    elementwise_hint(a, b)
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
                _ => elementwise_hint(a, b).to_string(),
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

    /// A `var` whose operand is not a string.
    fn var(&mut self, args: &[&Logic], path: &str) -> (Expr, Ty) {
        if !self.arity("var", args, 1..=1, path) {
            return (Expr::Lit(Scalar::Null), Ty::Any);
        }
        match args[0].as_str() {
            Some(_) => self.compile(
                &Logic::Var(args[0].as_str().unwrap_or_default().into()),
                path,
            ),
            None => self.error("rule_var", path, "`var` takes a path string".to_string()),
        }
    }

    /// A `path`, whose operands are the names.
    fn path(&mut self, args: &[&Logic], path: &str) -> (Expr, Ty) {
        let names: Option<Vec<&str>> = args.iter().map(|a| a.as_str()).collect();
        match names {
            Some(names) if !names.is_empty() => self.var_path(&names, path),
            _ => self.error("rule_var", path, "`path` takes names".to_string()),
        }
    }

    /// Binds the names of a path to a field and its static type.
    fn var_path(&mut self, names: &[&str], path: &str) -> (Expr, Ty) {
        let mut segments = names.to_vec();
        let mut steps = Vec::new();
        let first = segments.remove(0);
        let mut cell = None;
        let (root, mut ty) = if first == ".x"
            && (self.element.is_some() || matches!(self.names, Names::Fields(_)))
        {
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
            match self.names {
                Names::Fields(fields) => match fields.iter().find(|f| f.name.as_str() == first) {
                    Some(f) => {
                        steps.push(Step::Field(first.to_string()));
                        (Root::Data, Ty::of(&f.ty, f.unit.as_deref()))
                    }
                    None => {
                        let near =
                            crate::name::nearest(first, fields.iter().map(|f| f.name.as_str()))
                                .map(|n| format!("; the nearest field is `{n}`"))
                                .unwrap_or_default();
                        return self.error(
                            "rule_field",
                            path,
                            format!("`{first}` is not a field in scope{near}"),
                        );
                    }
                },
                Names::Columns { columns, .. } => match columns.column(first) {
                    Some(Err(message)) => return self.error("column_type", path, message),
                    Some(Ok((c, unit))) => {
                        // A factor's cell is its label.
                        let ty = match Ty::of(&c.ty, unit.as_deref()) {
                            Ty::Factor { .. } => Ty::String,
                            t => t,
                        };
                        let at = self
                            .cells
                            .iter()
                            .position(|x| x.name == c.name)
                            .unwrap_or_else(|| {
                                self.cells.push(c);
                                self.cells.len() - 1
                            });
                        cell = Some(at);
                        (Root::Data, ty)
                    }
                    None => {
                        let near = columns
                            .nearest(first)
                            .map(|n| format!("; the nearest column is `{n}`"))
                            .unwrap_or_default();
                        return self.error(
                            "rule_field",
                            path,
                            format!("`{first}` is not a column of the table{near}"),
                        );
                    }
                },
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
        if ty == Ty::HostOnly {
            return self.error(
                "rule_type",
                path,
                format!(
                    "`{}` is a host-only R value, which rules cannot read",
                    names.join(".")
                ),
            );
        }
        match cell {
            Some(at) => (Expr::Cell(at), ty),
            None => (Expr::Var { root, steps }, ty),
        }
    }

    /// Reads the string literals that are compared with a date or a date-time
    /// as one: `at` is the operand of the other type.
    fn read_dates(&mut self, parts: &mut [(Expr, Ty)], path: &str) {
        let Some(of) = parts
            .iter()
            .map(|(_, t)| t.clone())
            .find(|t| matches!(t, Ty::Date | Ty::Posixct))
        else {
            return;
        };
        let read = |text: &str| match of {
            Ty::Date => valid_date(text).then(|| Scalar::Date(date_days(text))),
            _ => {
                // The fraction of a second may be left out.
                let whole = text
                    .strip_suffix('Z')
                    .filter(|t| t.len() == 19)
                    .map(|t| format!("{t}.000000Z"));
                let text = whole.as_deref().unwrap_or(text);
                valid_posixct(text).then(|| Scalar::DateTime(datetime_micros(text)))
            }
        };
        for (i, (e, t)) in parts.iter_mut().enumerate() {
            let refused = match e {
                Expr::Lit(Scalar::Str(text)) => match read(text) {
                    Some(v) => {
                        *e = Expr::Lit(v);
                        *t = of.clone();
                        None
                    }
                    None => Some(text.clone()),
                },
                Expr::Lit(Scalar::Array(xs)) => {
                    let mut refused = None;
                    for x in xs.iter_mut() {
                        if let Scalar::Str(text) = x {
                            match read(text) {
                                Some(v) => *x = v,
                                None => refused = Some(text.clone()),
                            }
                        }
                    }
                    if matches!(t, Ty::Array(of) if **of == Ty::String) && refused.is_none() {
                        *t = Ty::Array(Box::new(of.clone()));
                    }
                    refused
                }
                _ => None,
            };
            if let Some(text) = refused {
                self.errors.push(RuleError {
                    code: "rule_date",
                    path: format!("{path}/{i}"),
                    message: if of == Ty::Date {
                        format!("\"{text}\" is compared with a date, and is not one: YYYY-MM-DD")
                    } else {
                        format!(
                            "\"{text}\" is compared with a date-time, and is not one: YYYY-MM-DDTHH:MM:SSZ"
                        )
                    },
                });
                *e = Expr::Lit(Scalar::Null);
                *t = Ty::Any;
            }
        }
    }

    /// A literal compared with a cell of a factor is one of the declared
    /// levels.
    fn labels(&mut self, parts: &[(Expr, Ty)], path: &str) {
        let Some((name, known)) = parts.iter().find_map(|(e, _)| match e {
            Expr::Cell(k) => {
                let c = &self.cells[*k];
                Some((c.name.clone(), levels(&c.ty)?.to_vec()))
            }
            _ => None,
        }) else {
            return;
        };
        for (i, (e, _)) in parts.iter().enumerate() {
            let given: &[Scalar] = match e {
                Expr::Lit(Scalar::Array(xs)) => xs,
                Expr::Lit(x @ Scalar::Str(_)) => std::slice::from_ref(x),
                _ => continue,
            };
            for x in given {
                let Scalar::Str(text) = x else { continue };
                if known.iter().any(|l| **l == **text) {
                    continue;
                }
                let listed: Vec<String> = known.iter().map(|l| format!("\"{l}\"")).collect();
                self.errors.push(RuleError {
                    code: "rule_level",
                    path: format!("{path}/{i}"),
                    message: format!(
                        "\"{text}\" is not a level of `{name}`, whose levels are {}",
                        listed.join(", ")
                    ),
                });
                return;
            }
        }
    }

    /// `mean`, `sum`, `median`, `min`, `max`, `n`, or `count` in a rule that
    /// reads rows: over all rows of every operand, or over the rows of each
    /// group. `na_rm` around the operands leaves out their missing cells.
    fn aggregate(
        &mut self,
        code: Op,
        op: &str,
        args: &[&Logic],
        admitted: bool,
        path: &str,
    ) -> (Expr, Ty) {
        let many = matches!(code, Op::Min | Op::Max);
        if !admitted {
            let cells = if many && args.len() > 1 {
                format!(
                    "; `p{op}` is the {} of the cells of a row",
                    if code == Op::Min { "least" } else { "greatest" }
                )
            } else {
                String::new()
            };
            return self.error(
                "rule_aggregate",
                path,
                format!("a rule of a table reads one row, and `{op}` reads whole columns{cells}; a rule that filters takes it"),
            );
        }
        // `na.rm = TRUE`: the operands within `na_rm`.
        let (args, skip): (Vec<&Logic>, bool) = match args {
            [
                Logic::Call {
                    op: Operator::NaRm,
                    args: inner,
                    ..
                },
            ] => {
                if code == Op::N {
                    return self.error(
                        "rule_na_rm",
                        path,
                        "`n()` counts rows, and takes no `na.rm`".to_string(),
                    );
                }
                (inner.iter().collect(), true)
            }
            _ => (args.to_vec(), false),
        };
        let path = &if skip {
            format!("{path}/0/na_rm")
        } else {
            path.to_string()
        };
        let range = match code {
            Op::N => 0..=0,
            _ if many => 1..=usize::MAX,
            _ => 1..=1,
        };
        if !self.arity(op, &args, range, path) {
            return (Expr::Lit(Scalar::Null), Ty::Any);
        }
        let parts = self.operands(&args, path);
        if !self.errors.is_empty() {
            return (Expr::Lit(Scalar::Null), Ty::Any);
        }
        if code != Op::N && !parts.iter().any(|(e, _)| reads_cell(e)) {
            return self.error(
                "rule_aggregate",
                path,
                format!("`{op}` reads whole columns, and its operands read none"),
            );
        }
        // An operand of one value for each group, such as `mean(x)`, is that
        // value; `min()` and `max()` of several operands compare it with
        // others.
        if !many && code != Op::N && !reads_cell_outside(&parts[0].0) {
            return self.error(
                "rule_aggregate",
                path,
                format!(
                    "`{op}` reads the cells of a column, and its operand is one value for the rows"
                ),
            );
        }
        let dates = many
            && matches!(parts[0].1, Ty::Date | Ty::Posixct)
            && parts.iter().all(|(_, t)| *t == parts[0].1);
        let ty = if matches!(code, Op::N | Op::Count) {
            if code == Op::Count {
                self.expect_bool(
                    &parts[0].1,
                    &format!("{path}/0"),
                    "the condition of `count`",
                );
            }
            Ty::Number {
                unit: None,
                exact: true,
            }
        } else if dates {
            parts[0].1.clone()
        } else {
            let mut unit = unit_of(&parts[0].1).map(str::to_string);
            for (i, (_, t)) in parts.iter().enumerate() {
                self.expect_number(t, &format!("{path}/{i}"), op);
                let a = Ty::Number {
                    unit: unit.clone(),
                    exact: true,
                };
                unit = self.same_unit(&a, t, path, op);
            }
            Ty::Number {
                unit,
                exact: !matches!(code, Op::Mean | Op::Median)
                    && parts.iter().all(|(_, t)| exact_of(t)),
            }
        };
        let at = self.aggregates;
        self.aggregates += 1;
        (
            Expr::Aggregate {
                op: code,
                operands: parts.into_iter().map(|(e, _)| e).collect(),
                at,
                skip,
            },
            ty,
        )
    }

    fn step(&mut self, ty: &Ty, seg: &str, path: &str) -> Option<(Step, Ty)> {
        let member = |t: Ty| Some((Step::Field(seg.to_string()), t));
        let column = |t: Ty| Some((Step::Column(seg.to_string()), t));
        match ty {
            Ty::Any | Ty::Json => member(Ty::Any),
            Ty::Table { columns, open } => match columns.iter().find(|c| c.name.as_str() == seg) {
                // A column is a vector of its type; a factor is one already.
                Some(c) => column(match Ty::of(&c.ty, c.unit.as_deref()) {
                    f @ Ty::Factor { .. } => f,
                    t => Ty::Array(Box::new(t)),
                }),
                None if *open => column(Ty::Array(Box::new(Ty::Any))),
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
                    Step::Key(seg.to_string())
                };
                Some((s, (**value).clone()))
            }
            Ty::Named(q) => match members(self.lookup, q) {
                None => member(Ty::Any),
                Some(Members::Struct(fields)) => {
                    match fields.iter().find(|f| f.name.as_str() == seg) {
                        Some(f) => member(Ty::of(&f.ty, f.unit.as_deref())),
                        None => {
                            self.error(
                                "rule_field",
                                path,
                                format!("`{seg}` is not a field of `{q}`"),
                            );
                            None
                        }
                    }
                }
                Some(Members::Enum { variants, .. }) => {
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
                                Some(prev) if erase_exact(prev) != erase_exact(&t) => {
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

    fn operation(&mut self, operator: Operator, args: &[&Logic], path: &str) -> (Expr, Ty) {
        let op = operator.as_str();
        if let Names::Columns { aggregates, .. } = self.names {
            use Operator as O;
            match operator {
                O::Mean => return self.aggregate(Op::Mean, op, args, aggregates, path),
                O::Sum => return self.aggregate(Op::Sum, op, args, aggregates, path),
                O::Min => return self.aggregate(Op::Min, op, args, aggregates, path),
                O::Max => return self.aggregate(Op::Max, op, args, aggregates, path),
                O::Median => return self.aggregate(Op::Median, op, args, aggregates, path),
                O::N => return self.aggregate(Op::N, op, args, aggregates, path),
                O::Count => return self.aggregate(Op::Count, op, args, aggregates, path),
                O::IsNull => {
                    return self.error(
                        "rule_type",
                        path,
                        "`is_null` tests a field, and a row reads cells; `is_na` tests a cell"
                            .to_string(),
                    );
                }
                O::Length
                | O::Subset
                | O::NonIncreasing
                | O::NonDecreasing
                | O::Nrow
                | O::Ncol
                | O::Dim
                | O::Levels
                | O::All
                | O::Any
                | O::IsVariant => {
                    return self.error(
                        "rule_type",
                        path,
                        format!("`{op}` reads a whole field, and a row reads cells"),
                    );
                }
                _ => {}
            }
        }
        match operator {
            Operator::N | Operator::Count => {
                return self.error(
                    "rule_aggregate",
                    path,
                    format!("`{op}` counts the rows of a table, and a rule of a value has none"),
                );
            }
            Operator::NaRm => {
                return self.error(
                    "rule_na_rm",
                    path,
                    "`na.rm = TRUE` leaves out the missing cells of the column that an aggregate reads, as in `mean(x, na.rm = TRUE)`".to_string(),
                );
            }
            Operator::Col => {
                return self.error(
                    "rule_col",
                    path,
                    "`col()` is the column that `r8map()` maps; another expression names its columns, or `col(name)` for a name held in R".to_string(),
                );
            }
            Operator::Var => return self.var(args, path),
            Operator::Path => return self.path(args, path),
            Operator::IsNa => {
                if !self.arity(op, args, 1..=1, path) {
                    return (Expr::Lit(Scalar::Null), Ty::Bool);
                }
                let (e, t) = self.compile(args[0], &format!("{path}/0"));
                if !matches!(
                    t,
                    Ty::Bool
                        | Ty::Number { .. }
                        | Ty::String
                        | Ty::Date
                        | Ty::Posixct
                        | Ty::Any
                        | Ty::Json
                ) {
                    return self.error(
                        "rule_type",
                        path,
                        format!("`is_na` takes one value, found {}", t.describe()),
                    );
                }
                return (Expr::IsNa(Box::new(e)), Ty::Bool);
            }
            Operator::LooseEq | Operator::LooseNe => {
                let strict = if operator == Operator::LooseEq {
                    "==="
                } else {
                    "!=="
                };
                return self.error(
                    "rule_loose_equality",
                    path,
                    format!("`{op}` is JSONLogic's loose equality; R8 rules write strict equality as `{strict}`"),
                );
            }
            Operator::IsNull => {
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
            Operator::IsVariant => return self.is_variant(args, path),
            Operator::Levels => {
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
            Operator::All | Operator::Any => {
                return self.quantifier(operator == Operator::All, args, path);
            }
            Operator::If => {
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
            Operator::And | Operator::Or => {
                if !self.arity(op, args, 1..=usize::MAX, path) {
                    return (Expr::Lit(Scalar::Null), Ty::Bool);
                }
                let parts = self.operands(args, path);
                for (i, (_, t)) in parts.iter().enumerate() {
                    self.expect_bool(t, &format!("{path}/{i}"), &format!("an operand of `{op}`"));
                }
                let es = parts.into_iter().map(|(e, _)| e).collect();
                return (
                    if operator == Operator::And {
                        Expr::And(es)
                    } else {
                        Expr::Or(es)
                    },
                    Ty::Bool,
                );
            }
            _ => {}
        }
        let (code, range) =
            evaluated(operator).expect("every other operator takes evaluated operands");
        if !self.arity(op, args, range, path) {
            return (Expr::Lit(Scalar::Null), Ty::Any);
        }
        let before = self.errors.len();
        let mut parts = self.operands(args, path);
        // In a table, a comparison with the missing constant is missing in
        // every row; in a value, `null` is an absent field. An operand with an
        // error compiles as the constant too.
        if self.errors.len() == before
            && matches!(self.names, Names::Columns { .. })
            && matches!(
                code,
                Op::Eq | Op::Ne | Op::Lt | Op::Le | Op::Gt | Op::Ge | Op::In
            )
            && parts
                .iter()
                .any(|(e, _)| matches!(e, Expr::Lit(Scalar::Null)))
        {
            return self.error(
                "rule_missing_constant",
                path,
                format!("`{op}` with `NA` is missing in every row; `is_na` tests a missing cell"),
            );
        }
        if matches!(
            code,
            Op::Eq | Op::Ne | Op::Lt | Op::Le | Op::Gt | Op::Ge | Op::In
        ) {
            self.read_dates(&mut parts, path);
            self.labels(&parts, path);
        }
        let tys: Vec<Ty> = parts.iter().map(|(_, t)| t.clone()).collect();
        let result = self.type_op(code, op, &tys, path);
        // Strict equality of a double that arithmetic computed fails on a
        // rounding difference: 0.1 + 0.2 is not 0.3.
        if matches!(code, Op::Eq | Op::Ne) {
            for (i, (e, t)) in parts.iter().enumerate() {
                let computed = matches!(
                    e,
                    Expr::Op(
                        Op::Add
                            | Op::Sub
                            | Op::Mul
                            | Op::Div
                            | Op::Sum
                            | Op::Mean
                            | Op::Sqrt
                            | Op::Log
                            | Op::Exp,
                        _
                    ) | Expr::Aggregate {
                        op: Op::Sum | Op::Mean,
                        ..
                    }
                );
                if computed && matches!(t, Ty::Number { exact: false, .. }) {
                    self.error(
                        "rule_float_equality",
                        &format!("{path}/{i}"),
                        format!("`{op}` compares a double computed by arithmetic, which rounding makes inexact; compare the difference with a tolerance, as in `abs(a - b) <= 1e-9`"),
                    );
                }
            }
        }
        (
            Expr::Op(code, parts.into_iter().map(|(e, _)| e).collect()),
            result,
        )
    }

    fn type_op(&mut self, code: Op, op: &str, t: &[Ty], path: &str) -> Ty {
        let num = Ty::Number {
            unit: None,
            exact: true,
        };
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
            // `min()` and `max()` of one array: its smallest or largest
            // element.
            Op::Min | Op::Max if t.len() == 1 && matches!(t[0], Ty::Array(_)) => {
                let Ty::Array(e) = &t[0] else { unreachable!() };
                self.expect_number(e, &format!("{path}/0"), op);
                match &**e {
                    n @ Ty::Number { .. } => n.clone(),
                    _ => num,
                }
            }
            Op::Add | Op::Sub | Op::Min | Op::Max => {
                for (i, x) in t.iter().enumerate() {
                    self.expect_number(x, &format!("{path}/{i}"), op);
                }
                let mut unit = unit_of(&t[0]).map(str::to_string);
                for x in &t[1..] {
                    let a = Ty::Number {
                        unit: unit.clone(),
                        exact: true,
                    };
                    unit = self.same_unit(&a, x, path, op);
                }
                Ty::Number {
                    unit,
                    exact: t.iter().all(exact_of),
                }
            }
            Op::Mul | Op::Div | Op::Mod => {
                for (i, x) in t.iter().enumerate() {
                    self.expect_number(x, &format!("{path}/{i}"), op);
                }
                Ty::Number {
                    unit: None,
                    exact: code != Op::Div && t.iter().all(exact_of),
                }
            }
            Op::Floor | Op::Ceiling | Op::Round => {
                self.expect_number(&t[0], &format!("{path}/0"), op);
                Ty::Number {
                    unit: unit_of(&t[0]).map(str::to_string),
                    exact: true,
                }
            }
            Op::Abs => {
                self.expect_number(&t[0], &format!("{path}/0"), op);
                Ty::Number {
                    unit: unit_of(&t[0]).map(str::to_string),
                    exact: exact_of(&t[0]),
                }
            }
            // The unit of a power is not one of the field's.
            Op::Pow => {
                for (i, x) in t.iter().enumerate() {
                    self.expect_number(x, &format!("{path}/{i}"), op);
                }
                Ty::Number {
                    unit: None,
                    exact: false,
                }
            }
            // The unit of a root or a logarithm is not one of the field's.
            Op::Sqrt | Op::Log | Op::Exp => {
                self.expect_number(&t[0], &format!("{path}/0"), op);
                Ty::Number {
                    unit: None,
                    exact: false,
                }
            }
            Op::Sum | Op::Mean | Op::Median => match &t[0] {
                Ty::Array(e) => {
                    self.expect_number(e, &format!("{path}/0"), op);
                    match &**e {
                        Ty::Number { unit, exact } => Ty::Number {
                            unit: unit.clone(),
                            exact: *exact && code == Op::Sum,
                        },
                        _ => num,
                    }
                }
                Ty::Any | Ty::Json => num,
                other => {
                    self.error(
                        "rule_type",
                        &format!("{path}/0"),
                        format!(
                            "`{op}` takes an array of numbers, found {}",
                            other.describe()
                        ),
                    );
                    num
                }
            },
            Op::Nchar => {
                if !matches!(t[0], Ty::String | Ty::Any | Ty::Json) {
                    self.error(
                        "rule_type",
                        &format!("{path}/0"),
                        format!("`nchar` takes a string, found {}", t[0].describe()),
                    );
                }
                num
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
            // R's length(): the number of elements of a vector or a factor.
            Op::Length => {
                match &t[0] {
                    Ty::Array(_) | Ty::Factor { .. } | Ty::Any | Ty::Json => {}
                    Ty::String => {
                        self.error(
                            "rule_type",
                            &format!("{path}/0"),
                            "`length` counts the elements of a vector or a factor, found a string; `nchar` counts a string's characters".to_string(),
                        );
                    }
                    Ty::Table { .. } => {
                        self.error(
                            "rule_type",
                            &format!("{path}/0"),
                            "`length` counts the elements of a vector or a factor, found a table; `nrow` counts its rows".to_string(),
                        );
                    }
                    other => {
                        self.error(
                            "rule_type",
                            &format!("{path}/0"),
                            format!(
                                "`length` counts the elements of a vector or a factor, found {}",
                                other.describe()
                            ),
                        );
                    }
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
            Op::N | Op::Count => num,
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

    fn is_variant(&mut self, args: &[&Logic], path: &str) -> (Expr, Ty) {
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
            Ty::Named(q) => match members(self.lookup, q) {
                Some(Members::Enum { tag, variants }) => {
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
                Some(Members::Struct(_)) => {
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

    fn quantifier(&mut self, all: bool, args: &[&Logic], path: &str) -> (Expr, Ty) {
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
            } => (Collection::IndexMap, (**value).clone()),
            Ty::Map {
                value,
                ordered: false,
            } => (Collection::BtreeMap, (**value).clone()),
            Ty::Any | Ty::Json => (Collection::Dynamic, Ty::Any),
            other => {
                return self.error(
                    "rule_type",
                    &format!("{path}/0"),
                    format!(
                        "`{op}` runs over a vec, an index map, or a btree map, found {}",
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

fn compiler<'a>(names: Names<'a>, lookup: &'a dyn TypeLookup) -> Compiler<'a> {
    Compiler {
        lookup,
        names,
        element: None,
        errors: Vec::new(),
        cells: Vec::new(),
        aggregates: 0,
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
    let mut c = compiler(Names::Fields(fields), lookup);
    let (expr, ty) = c.compile(logic, "");
    if let Some(what) = logical {
        c.expect_bool(&ty, "", what);
    }
    if c.errors.is_empty() {
        Ok(expr)
    } else {
        Err(c.errors)
    }
}

/// The compiled conditions of a rule that reads rows.
pub struct Rows {
    pub invariant: Expr,
    pub when: Option<Expr>,
    /// The columns they read, by `Expr::Cell`.
    pub cells: Vec<Cell>,
    /// The number of aggregates in them.
    pub aggregates: usize,
}

/// Compiles the conditions of a rule over the rows of a table. Error paths are
/// relative to the rule.
pub fn compile_rows(
    invariant: &Logic,
    when: Option<&Logic>,
    columns: &dyn RowScope,
    lookup: &dyn TypeLookup,
    aggregates: bool,
) -> Result<Rows, Vec<RuleError>> {
    let mut c = compiler(
        Names::Columns {
            columns,
            aggregates,
        },
        lookup,
    );
    let (expr, ty) = c.compile(invariant, "/invariant");
    c.expect_bool(&ty, "/invariant", "a rule's invariant");
    let when = when.map(|w| {
        let (e, ty) = c.compile(w, "/when");
        c.expect_bool(&ty, "/when", "a rule's `when` condition");
        e
    });
    if c.errors.is_empty() {
        Ok(Rows {
            invariant: expr,
            when,
            cells: c.cells,
            aggregates: c.aggregates,
        })
    } else {
        Err(c.errors)
    }
}

/// An expression compiled against the columns of a table: a column of its
/// rows, or of the groups of its rows where `summary`.
pub struct Calc {
    /// The expression, as `invariant`.
    pub rows: Rows,
    /// Its static type.
    pub ty: Ty,
}

/// Compiles an expression that computes a column of a table. A summary reads
/// cells within aggregates only. The paths of errors are into the
/// expression.
pub fn compile_calc(
    logic: &Logic,
    columns: &dyn RowScope,
    lookup: &dyn TypeLookup,
    summary: bool,
) -> Result<Calc, Vec<RuleError>> {
    let mut c = compiler(
        Names::Columns {
            columns,
            aggregates: true,
        },
        lookup,
    );
    let (expr, ty) = c.compile(logic, "");
    if c.errors.is_empty() {
        if summary && reads_cell_outside(&expr) {
            c.error(
                "calc_summary",
                "",
                "a summary reads the cells of its columns within aggregates, such as `mean(x)`; `r8set()` computes a column of every row".to_string(),
            );
        }
        if !matches!(
            ty,
            Ty::Bool | Ty::Number { .. } | Ty::String | Ty::Date | Ty::Posixct | Ty::Any
        ) {
            c.error(
                "calc_type",
                "",
                format!(
                    "a computed column holds logicals, numbers, strings, dates, or date-times, and the expression gives {}",
                    ty.describe()
                ),
            );
        }
    }
    if c.errors.is_empty() {
        Ok(Calc {
            rows: Rows {
                invariant: expr,
                when: None,
                cells: c.cells,
                aggregates: c.aggregates,
            },
            ty,
        })
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
    let mut c = compiler(Names::Fields(fields), lookup);
    let (expr, ty) = c.compile(logic, "");
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
