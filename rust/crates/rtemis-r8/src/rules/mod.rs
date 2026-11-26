//! The rule dialect: strict JSONLogic, extended by R8's intrinsics.
//!
//! A rule states what must hold. [`CompiledRule::compile`] binds it
//! to the fields in scope, a struct's or a variant's, and [`RowRule::compile`]
//! to the columns of a table, whose rows it reads. Both report declaration
//! errors: an unknown operator or field, a type or unit mismatch, an unknown
//! variant, loose equality. [`CompiledRule::check`] evaluates a rule over a
//! value's JSON form, and [`RowRule::failing`] one over the rows of a table. A
//! rule holds when its `when` condition is `false`, or when its condition or
//! its invariant is missing: an absent field, a missing cell, and a comparison
//! with `NaN` are missing, and so is an operator with a missing operand, save
//! an `and` with a false operand and an `or` with a true one. `is_null()` is
//! a presence test, and `is_na()` tests a value; neither is missing.
//! [`RowRule::choose`] gives the rows where a rule is true.
//!
//! Operators: `var`, `path`, `if`, `and`, `or`, `!`, `===`, `!==`, `<`, `<=`,
//! `>`, `>=`, `+`, `-`, `*`, `/`, `%`, `min`, `max`, `in`, and `cat`.
//! Intrinsics: `length`, `subset`, `non_increasing`, `non_decreasing`, `abs`,
//! `floor`, `ceiling`, `round`, `sqrt`, `log`, `exp`, `sum`, `mean`, `pmin`,
//! `pmax`, `nchar`, `is_null`, `is_na`, `is_variant`, `levels`, `nrow`,
//! `ncol`, `dim`, `all`, and `any`; inside `all()` and `any()`, `.x` is the
//! element. In a rule over rows, a name is the cell of the row, and the whole
//! column in an operand of `mean`, `sum`, `min`, and `max`, which a rule that
//! chooses rows takes, and a rule of a table's declaration does not.

mod compile;
mod diagnose;
mod eval;
mod exact;
mod rows;
mod ty;

use serde_json::Value;

use crate::access::{Access, Json};

pub use compile::{Cell, RowScope, RuleError};
pub use diagnose::{Diagnosis, SHOWN_CHARS, Shown};
pub(crate) use rows::Held;
pub use rows::{NotChosen, RowError, RowRule, Tally, choose_rows};
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
    fn push(head: &str, out: &mut Vec<String>) {
        if !out.iter().any(|x| x == head) {
            out.push(head.to_string());
        }
    }
    fn var(path: &str, out: &mut Vec<String>) {
        let head = path.split('.').next().unwrap_or_default();
        // `.x` is the element of a quantifier.
        if !head.is_empty() {
            push(head, out);
        }
    }
    fn walk(l: &Logic, out: &mut Vec<String>) {
        match l {
            Logic::Var(p) => var(p, out),
            Logic::Call {
                op: op @ (Operator::Var | Operator::Path),
                args,
                ..
            } => {
                let Some(p) = args.first().and_then(Logic::as_str) else {
                    return;
                };
                match op {
                    Operator::Var => var(p, out),
                    _ if p == ".x" => {}
                    _ => push(p, out),
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
