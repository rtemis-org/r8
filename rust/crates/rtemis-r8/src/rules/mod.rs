//! The rule dialect: strict JSONLogic, extended by R8's intrinsics.
//!
//! A rule states what must hold. [`CompiledRule::compile`] binds it
//! to the fields in scope, a struct's, a variant's, or a table row's,
//! and reports declaration errors: an unknown operator or field, a type or
//! unit mismatch, an unknown variant, loose equality. [`CompiledRule::check`]
//! evaluates it over a value's JSON form. A rule holds when its `when`
//! condition is `false`, or when a field its invariant or condition reads is
//! absent; `is_null()` is a presence test and does not count as a read.
//!
//! Operators: `var`, `if`, `and`, `or`, `!`, `===`, `!==`, `<`, `<=`, `>`,
//! `>=`, `+`, `-`, `*`, `/`, `%`, `min`, `max`, `in`, and `cat`. Intrinsics:
//! `length`, `subset`, `non_increasing`, `non_decreasing`, `floor`, `ceiling`,
//! `round`, `is_null`, `is_variant`, `levels`, `nrow`, `ncol`, `dim`, `all`,
//! and `any`; inside `all()` and `any()`, `.x` is the element.

mod compile;
mod diagnose;
mod eval;
mod ty;

use serde_json::Value;

use crate::access::{Access, Json};

pub use compile::RuleError;
pub use diagnose::{Diagnosis, SHOWN_CHARS, Shown};
pub(crate) use ty::dollar_type;
pub use ty::implementors;
pub use ty::{DocumentSet, Ty, TypeLookup};

use compile::{Expr, Root, Step, compile, compile_computed};
pub use eval::Scalar;
use eval::{Absent, Env, Val};

use crate::ir::{FieldDef, RuleDef};
use crate::name::Name;

/// A rule compiled against its scope.
#[derive(Debug, Clone)]
pub struct CompiledRule {
    /// The rule's ID, if it has one.
    pub id: Option<Name>,
    /// The rule's message, if it has one.
    pub message: Option<String>,
    invariant: Expr,
    when: Option<Expr>,
    /// The fields the invariant and the condition read, presence tests
    /// included: a change to any of them can change the outcome.
    reads: Box<[String]>,
}

/// The result of checking one rule against one value.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Outcome {
    /// The rule holds.
    Holds,
    /// The rule is violated; each path is relative to the value checked:
    /// empty for the value itself, and for an invariant that is an `all()`,
    /// the path of each failing element.
    Violated(Vec<Vec<crate::problem::Segment>>),
}

fn var_segments(steps: &[Step]) -> Option<Vec<crate::problem::Segment>> {
    use crate::problem::Segment;
    steps
        .iter()
        .map(|s| match s {
            Step::Field(k) => Some(Segment::Field(k.clone())),
            Step::Key(k) => Some(Segment::Key(k.clone())),
            Step::Column(k) => Some(Segment::Column(k.clone())),
            // The position of a key in an ordered map is known at run time.
            Step::OrderedKey(_) => None,
        })
        .collect()
}

impl CompiledRule {
    /// Compiles a rule in the scope of `fields`. Error paths are relative to
    /// the rule (`/invariant/...`, `/when/...`).
    pub fn compile(
        rule: &RuleDef,
        fields: &[FieldDef],
        lookup: &dyn TypeLookup,
    ) -> Result<Self, Vec<RuleError>> {
        let mut errors = Vec::new();
        let invariant = compile(&rule.invariant, fields, lookup, Some("a rule's invariant"))
            .map_err(|e| prefix(e, "/invariant"))
            .map_err(|e| errors.extend(e))
            .ok();
        let when = match &rule.when {
            Some(w) => compile(w, fields, lookup, Some("a rule's `when` condition"))
                .map(Some)
                .map_err(|e| errors.extend(prefix(e, "/when")))
                .ok(),
            None => Some(None),
        };
        match (invariant, when) {
            (Some(invariant), Some(when)) if errors.is_empty() => {
                let mut read = reads(&rule.invariant);
                if let Some(w) = &rule.when {
                    for f in reads(w) {
                        if !read.contains(&f) {
                            read.push(f);
                        }
                    }
                }
                Ok(CompiledRule {
                    id: rule.id.clone(),
                    message: rule.message.clone(),
                    invariant,
                    when,
                    reads: read.into_boxed_slice(),
                })
            }
            _ => Err(errors),
        }
    }

    /// Whether the invariant or the condition reads the field `name`, as a
    /// value or as a presence test.
    pub fn reads_field(&self, name: &str) -> bool {
        self.reads.iter().any(|f| f == name)
    }

    /// Checks the rule against a value's JSON form.
    pub fn check(&self, value: &Value) -> Result<Outcome, String> {
        self.check_access(Json::new(value), &[], &NoLookup)
    }

    /// Checks the rule against a value in any host form, whose fields are
    /// `scope`.
    pub fn check_access<'a, A: Access<'a>>(
        &self,
        value: A,
        scope: &[FieldDef],
        lookup: &dyn TypeLookup,
    ) -> Result<Outcome, String> {
        let env = Env {
            data: value,
            scope,
            lookup,
            element: None,
        };
        if let Some(w) = &self.when {
            match env.eval(w)? {
                Err(Absent) => return Ok(Outcome::Holds),
                Ok(v) => match truth(&env, v) {
                    Some(false) => return Ok(Outcome::Holds),
                    Some(true) => {}
                    None => return Err("a rule's `when` condition is a logical".to_string()),
                },
            }
        }
        match env.eval(&self.invariant)? {
            Err(Absent) => Ok(Outcome::Holds),
            Ok(v) => match truth(&env, v) {
                Some(true) => Ok(Outcome::Holds),
                Some(false) => Ok(Outcome::Violated(self.violation_paths(&env)?)),
                None => Err("a rule's invariant is a logical".to_string()),
            },
        }
    }

    fn violation_paths<'a, A: Access<'a>>(
        &self,
        env: &Env<'_, A>,
    ) -> Result<Vec<Vec<crate::problem::Segment>>, String> {
        let Expr::Quantifier {
            all: true,
            collection,
            kind,
            condition,
        } = &self.invariant
        else {
            return Ok(vec![Vec::new()]);
        };
        let Expr::Var {
            root: Root::Data,
            steps,
        } = collection.as_ref()
        else {
            return Ok(vec![Vec::new()]);
        };
        let Some(base) = var_segments(steps) else {
            return Ok(vec![Vec::new()]);
        };
        let Ok(c) = env.eval(collection)? else {
            return Ok(vec![Vec::new()]);
        };
        let mut failing = Vec::new();
        env.each_element(c, *kind, &mut |suffix, el| {
            let e = Env {
                data: env.data,
                scope: env.scope,
                lookup: env.lookup,
                element: Some(el),
            };
            if let Ok(v) = e.eval(condition)?
                && truth(&e, v) == Some(false)
            {
                let mut path = base.clone();
                path.extend(suffix.segments());
                failing.push(path);
            }
            // One path past the limit marks the list as truncated.
            Ok(failing.len() <= crate::validate::ERROR_LIMIT)
        })?;
        Ok(failing)
    }

    /// Checks a table's row rule against every row of the table's JSON form.
    pub fn check_rows(&self, table: &Value) -> Result<Vec<Vec<crate::problem::Segment>>, String> {
        let rows = table
            .get("$length")
            .and_then(Value::as_u64)
            .ok_or("a table has `$length`")? as usize;
        self.check_rows_access(Json::new(table), rows, &[], &NoLookup)
    }

    /// Checks a table's row rule against rows `0..rows` of a table in any
    /// host form, whose columns are `scope`. Each violation's path is the
    /// cell of the first column the rule reads, relative to the table, such
    /// as the column `start` and the row 4; a rule that reads no column
    /// reports the row alone.
    pub fn check_rows_access<'a, A: Access<'a>>(
        &self,
        table: A,
        rows: usize,
        scope: &[FieldDef],
        lookup: &dyn TypeLookup,
    ) -> Result<Vec<Vec<crate::problem::Segment>>, String> {
        use crate::problem::Segment;
        let first =
            first_column(&self.invariant).or_else(|| self.when.as_ref().and_then(first_column));
        let mut paths = Vec::new();
        for i in 0..rows {
            if let Outcome::Violated(_) = self.check_access(table.row(i), scope, lookup)? {
                let mut path = Vec::with_capacity(2);
                if let Some(c) = &first {
                    path.push(Segment::Column(c.clone()));
                }
                path.push(Segment::Element(i as u64));
                paths.push(path);
                // One path past the limit marks the list as truncated.
                if paths.len() > crate::validate::ERROR_LIMIT {
                    break;
                }
            }
        }
        Ok(paths)
    }
}

/// A logical result; a reference to a logical field is read in place.
fn truth<'a, A: Access<'a>>(env: &Env<'_, A>, v: Val<'_, A>) -> Option<bool> {
    match env.scalar(v) {
        Scalar::Bool(b) => Some(b),
        _ => None,
    }
}

/// A lookup that holds no type, for expressions over JSON alone.
struct NoLookup;

impl TypeLookup for NoLookup {
    fn find(&self, _name: &crate::name::QualifiedName) -> Option<&crate::ir::TypeDef> {
        None
    }
}

/// Checks a computed field's expression in the scope of `fields`; the
/// target's unit and numeric type constrain the result.
pub fn check_computed(
    logic: &crate::ir::Logic,
    fields: &[FieldDef],
    lookup: &dyn TypeLookup,
    target: &FieldDef,
) -> Result<(), Vec<RuleError>> {
    let ty = Ty::of(&target.ty, target.unit.as_deref());
    compile_computed(logic, fields, lookup, &ty).map(|_| ())
}

/// Strict equality of two JSON values, as the dialect's `===` decides it:
/// numbers by value, arrays element by element, everything else
/// structurally.
pub fn strict_equal(a: &Value, b: &Value) -> bool {
    eval::strict_eq(a, b)
}

/// A number's value: a JSON number, or a non-finite double in its string
/// form (`"Infinity"`, `"-Infinity"`, `"NaN"`).
pub fn number_value(v: &Value) -> Option<f64> {
    eval::as_number(v)
}

/// Evaluates a computed field's expression over a value's JSON form, in
/// the scope of `fields`. `Ok(None)` when the expression reads an absent
/// field.
pub fn evaluate_computed(
    logic: &crate::ir::Logic,
    fields: &[FieldDef],
    lookup: &dyn TypeLookup,
    target: &FieldDef,
    value: &Value,
) -> Result<Option<Value>, String> {
    let ty = Ty::of(&target.ty, target.unit.as_deref());
    let expr = compile_computed(logic, fields, lookup, &ty).map_err(|e| {
        e.into_iter()
            .map(|x| x.message)
            .collect::<Vec<_>>()
            .join("; ")
    })?;
    let env = Env {
        data: Json::new(value),
        scope: fields,
        lookup,
        element: None,
    };
    Ok(env.eval(&expr)?.ok().map(|v| match v {
        eval::Val::R(a, ft) if !ft.is_some_and(eval::readable) => {
            a.to_value(ft.unwrap_or(&eval::UNTYPED), lookup)
        }
        v => env.scalar(v).to_value(),
    }))
}

/// Evaluates a computed field's expression over a value in any host form,
/// in the scope of `fields`. `Ok(None)` when the expression reads an absent
/// field.
pub fn evaluate_computed_access<'a, A: Access<'a>>(
    logic: &crate::ir::Logic,
    fields: &[FieldDef],
    lookup: &dyn TypeLookup,
    target: &FieldDef,
    value: A,
) -> Result<Option<Computed<A>>, String> {
    let ty = Ty::of(&target.ty, target.unit.as_deref());
    let expr = compile_computed(logic, fields, lookup, &ty).map_err(|e| {
        e.into_iter()
            .map(|x| x.message)
            .collect::<Vec<_>>()
            .join("; ")
    })?;
    let env = Env {
        data: value,
        scope: fields,
        lookup,
        element: None,
    };
    let declared = eval::unwrap_option(&target.ty);
    Ok(env.eval(&expr)?.ok().map(|v| match v {
        // A field of the computed field's type: the value itself.
        eval::Val::R(a, Some(ft)) if eval::unwrap_option(ft) == declared => Computed::Value(a),
        v => Computed::Scalar(env.scalar(v)),
    }))
}

/// The result of a computed field's expression.
pub enum Computed<A> {
    /// A field of the value, of the computed field's declared type: the
    /// host value, which the caller stores as it is.
    Value(A),
    /// A scalar, or an array from a vector or `levels()`.
    Scalar(Scalar),
}

/// The fields a computed expression reads, by name, in the order the
/// expression names them.
pub fn reads(logic: &crate::ir::Logic) -> Vec<String> {
    use crate::ir::{Logic, Operator};
    fn push(path: &str, out: &mut Vec<String>) {
        let head = path.split('.').next().unwrap_or_default();
        if !head.starts_with('.') && !out.iter().any(|x| x == head) {
            out.push(head.to_string());
        }
    }
    fn walk(l: &Logic, out: &mut Vec<String>) {
        match l {
            Logic::Var(p) => push(p, out),
            Logic::Call {
                op: Operator::Var,
                args,
                ..
            } => {
                if let Some(p) = args.first().and_then(Logic::as_str) {
                    push(p, out);
                }
            }
            Logic::Call { args, .. } | Logic::Array(args) => args.iter().for_each(|x| walk(x, out)),
            _ => {}
        }
    }
    let mut out = Vec::new();
    walk(logic, &mut out);
    out
}

fn prefix(errors: Vec<RuleError>, p: &str) -> Vec<RuleError> {
    errors
        .into_iter()
        .map(|mut e| {
            e.path = format!("{p}{}", e.path);
            e
        })
        .collect()
}

/// The first field an expression reads, in evaluation order.
fn first_column(e: &Expr) -> Option<String> {
    match e {
        Expr::Var {
            root: Root::Data,
            steps,
        } => match steps.first() {
            Some(Step::Field(k) | Step::Column(k)) => Some(k.clone()),
            _ => None,
        },
        Expr::If(xs) | Expr::And(xs) | Expr::Or(xs) | Expr::Op(_, xs) => {
            xs.iter().find_map(first_column)
        }
        Expr::IsVariant { value, .. } | Expr::Levels { value, .. } => first_column(value),
        Expr::Quantifier {
            collection,
            condition,
            ..
        } => first_column(collection).or_else(|| first_column(condition)),
        _ => None,
    }
}
