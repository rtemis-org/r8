//! The rule dialect: strict JSONLogic, extended by R8's intrinsics.
//!
//! A rule states what must hold. [`CompiledRule::compile`] resolves it
//! against the fields in scope, a struct's, a variant's, or a table row's,
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
mod eval;
mod ty;

use serde_json::Value;

pub use compile::RuleError;
pub use ty::{DocumentSet, Ty, TypeLookup};

use compile::{Expr, Root, Step, compile, compile_computed};
use eval::{Absent, Env, elements};

use crate::ir::{FieldDef, RuleDef};
use crate::name::Name;

/// A rule compiled against its scope.
#[derive(Debug, Clone)]
pub struct CompiledRule {
    pub id: Name,
    pub message: String,
    invariant: Expr,
    when: Option<Expr>,
}

/// The result of checking one rule against one value.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Outcome {
    Holds,
    /// The rule is violated; each path is a JSON Pointer relative to the
    /// value checked: `""` for the value itself, and for an invariant that is
    /// an `all()`, the path of each failing element.
    Violated(Vec<String>),
}

fn var_pointer(steps: &[Step]) -> Option<String> {
    let mut out = String::new();
    for s in steps {
        match s {
            Step::Member(k) => {
                out.push_str(&format!("/{}", k.replace('~', "~0").replace('/', "~1")))
            }
            // The position of a key in an ordered map is known at run time.
            Step::OrderedKey(_) => return None,
        }
    }
    Some(out)
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
            (Some(invariant), Some(when)) if errors.is_empty() => Ok(CompiledRule {
                id: rule.id.clone(),
                message: rule.message.clone(),
                invariant,
                when,
            }),
            _ => Err(errors),
        }
    }

    /// Checks the rule against a value's JSON form. An `Err` is a failure the
    /// declaration checks did not exclude, such as a division by zero.
    pub fn check(&self, value: &Value) -> Result<Outcome, String> {
        let env = Env {
            data: value,
            element: None,
        };
        if let Some(w) = &self.when {
            match env.eval(w)? {
                Err(Absent) => return Ok(Outcome::Holds),
                Ok(Value::Bool(false)) => return Ok(Outcome::Holds),
                Ok(Value::Bool(true)) => {}
                Ok(v) => return Err(format!("a rule's `when` condition is a logical, found {v}")),
            }
        }
        match env.eval(&self.invariant)? {
            Err(Absent) | Ok(Value::Bool(true)) => Ok(Outcome::Holds),
            Ok(Value::Bool(false)) => Ok(Outcome::Violated(self.violation_paths(value)?)),
            Ok(v) => Err(format!("a rule's invariant is a logical, found {v}")),
        }
    }

    /// For an invariant `all(collection, condition)` over a field of the
    /// value, the paths of the failing elements; otherwise the value itself.
    fn violation_paths(&self, value: &Value) -> Result<Vec<String>, String> {
        let Expr::Quantifier {
            all: true,
            collection,
            kind,
            condition,
        } = &self.invariant
        else {
            return Ok(vec![String::new()]);
        };
        let Expr::Var {
            root: Root::Data,
            steps,
        } = collection.as_ref()
        else {
            return Ok(vec![String::new()]);
        };
        let Some(base) = var_pointer(steps) else {
            return Ok(vec![String::new()]);
        };
        let env = Env {
            data: value,
            element: None,
        };
        let Ok(c) = env.eval(collection)? else {
            return Ok(vec![String::new()]);
        };
        let mut failing = Vec::new();
        for (suffix, el) in elements(&c, *kind)? {
            let e = Env {
                data: value,
                element: Some(&el),
            };
            if let Ok(v) = e.eval(condition)?
                && v == Value::Bool(false)
            {
                failing.push(format!("{base}{suffix}"));
            }
        }
        Ok(failing)
    }

    /// Checks a table's row rule against every row of the table's JSON form.
    /// Each violation's path is the cell of the first column the rule reads,
    /// relative to the table, such as `/start/4`; a rule that reads no column
    /// reports the row number alone.
    pub fn check_rows(&self, table: &Value) -> Result<Vec<String>, String> {
        let first =
            first_column(&self.invariant).or_else(|| self.when.as_ref().and_then(first_column));
        let mut paths = Vec::new();
        for (i, row) in eval::rows(table)?.iter().enumerate() {
            if let Outcome::Violated(_) = self.check(row)? {
                paths.push(match &first {
                    Some(c) => format!("/{}/{i}", c.replace('~', "~0").replace('/', "~1")),
                    None => format!("/{i}"),
                });
            }
        }
        Ok(paths)
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
            Some(Step::Member(k)) => Some(k.clone()),
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
