//! The values a failed rule reads, for its violation's message: the rule is
//! evaluated again, only after it failed, so a value that passes costs
//! nothing. Values are bounded: a vector, a matrix, or a table shows its
//! length or its dimensions, a string is cut, and a field declared
//! `sensitive` is withheld.

use super::compile::{Expr, Op, Root, Step};
use super::eval::{Env, Scalar, Val, unwrap_option};
use super::{CompiledRule, TypeLookup};
use crate::access::Access;
use crate::ir::{FieldDef, FieldType, TypeDef, VariantKind};

/// The number of characters of a string that a diagnosis shows.
pub const SHOWN_CHARS: usize = 40;

/// A value a failed rule read, as a diagnosis shows it.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Shown {
    /// A number, in its shortest decimal form that reads back exactly.
    Number(String),
    /// A logical.
    Bool(bool),
    /// A string, cut at [`SHOWN_CHARS`] characters; `cut` marks a cut
    /// string.
    String {
        /// The characters shown.
        text: String,
        /// The string has more characters.
        cut: bool,
    },
    /// A date, `YYYY-MM-DD`.
    Date(String),
    /// A date-time, RFC 3339 in UTC.
    DateTime(String),
    /// A vector or a factor: its length.
    Length(u64),
    /// A matrix, an array, or a table: its dimensions (a table's rows and
    /// columns).
    Dim(Vec<u64>),
    /// A field declared `sensitive`.
    Sensitive,
    /// An absent field.
    Absent,
    /// A missing element, such as R's `NA`.
    Missing,
    /// A value of another kind than required, described: "a string".
    Kind(String),
    /// A struct, an enum, a map, or another value a diagnosis does not show.
    Other,
}

impl Shown {
    /// A number, in its shortest decimal form that reads back exactly, and
    /// `NaN`, `Inf`, and `-Inf` for the values that are not finite.
    pub fn number(x: f64) -> Shown {
        Shown::Number(if x.is_nan() {
            "NaN".to_string()
        } else if x.is_infinite() {
            (if x > 0.0 { "Inf" } else { "-Inf" }).to_string()
        } else {
            crate::emit::number_text(x)
        })
    }

    /// A string, cut at [`SHOWN_CHARS`] characters.
    pub fn string(t: &str) -> Shown {
        let mut chars = t.chars();
        let text: String = chars.by_ref().take(SHOWN_CHARS).collect();
        Shown::String {
            text,
            cut: chars.next().is_some(),
        }
    }
}

/// The values a failed rule read, with the rule's expressions.
#[derive(Debug, Clone, PartialEq, Default)]
pub struct Diagnosis {
    /// The rule's invariant, as declared, for a host to render.
    pub invariant: Option<crate::ir::Logic>,
    /// The rule's `when` condition, as declared.
    pub when: Option<crate::ir::Logic>,
    /// Each field the rule reads, as its path of field names, with its
    /// value, in the order the rule first reads them.
    pub fields: Vec<(Vec<crate::problem::Segment>, Shown)>,
    /// Each computed operand of the comparison the rule tests, such as
    /// `sum(p)` in `sum(p) <= 1`, as a JSON Pointer into the rule's
    /// invariant (`/<=/0`), with its value.
    pub operands: Vec<(String, Shown)>,
    /// The element `.x` of an `all()` that fails, for a violation at an
    /// element.
    pub element: Option<Shown>,
}

// A rule's numbers are finite, so equality of diagnoses is reflexive.
impl Eq for Diagnosis {}

/// The JSONLogic operator of a comparison.
fn comparison(op: Op) -> Option<&'static str> {
    Some(match op {
        Op::Eq => "===",
        Op::Ne => "!==",
        Op::Lt => "<",
        Op::Le => "<=",
        Op::Gt => ">",
        Op::Ge => ">=",
        _ => return None,
    })
}

/// The paths of the fields an expression reads, in order, each once; the
/// element `.x` of a quantifier and a key of an ordered map are skipped.
fn field_paths(e: &Expr, out: &mut Vec<Vec<Step>>) {
    match e {
        Expr::Var {
            root: Root::Data,
            steps,
        } => {
            if steps.is_empty() || steps.iter().any(|s| matches!(s, Step::OrderedKey(_))) {
                return;
            }
            if !out.contains(steps) {
                out.push(steps.clone());
            }
        }
        Expr::Var { .. } | Expr::Lit(_) => {}
        Expr::If(xs) | Expr::And(xs) | Expr::Or(xs) | Expr::Op(_, xs) => {
            xs.iter().for_each(|x| field_paths(x, out));
        }
        Expr::IsNull(x) => field_paths(x, out),
        Expr::IsVariant { value, .. } | Expr::Levels { value, .. } => field_paths(value, out),
        Expr::Quantifier {
            collection,
            condition,
            ..
        } => {
            field_paths(collection, out);
            field_paths(condition, out);
        }
    }
}

/// Whether the field at `path` is declared `sensitive`, or lies within one.
fn sensitive(scope: &[FieldDef], lookup: &dyn TypeLookup, path: &[Step]) -> bool {
    let mut fields: Vec<&FieldDef> = scope.iter().collect();
    for step in path {
        let seg = match step {
            Step::Field(k) | Step::Column(k) => k.as_str(),
            // A map's value is the map's field's own.
            Step::Key(_) | Step::OrderedKey(_) => continue,
        };
        let Some(f) = fields.iter().find(|f| f.name.as_str() == seg) else {
            return false;
        };
        if f.sensitive {
            return true;
        }
        fields = match unwrap_option(&f.ty) {
            FieldType::Named { name, .. } => match lookup.find(name) {
                Some(TypeDef::Struct(s)) => s.fields.iter().collect(),
                Some(TypeDef::Enum(e)) => e
                    .variants
                    .iter()
                    .flat_map(|v| match &v.kind {
                        VariantKind::Struct { fields, .. } => fields.iter().collect::<Vec<_>>(),
                        _ => Vec::new(),
                    })
                    .collect(),
                None => return false,
            },
            _ => return false,
        };
    }
    false
}

fn shown_scalar(s: Scalar) -> Shown {
    match s {
        Scalar::Null => Shown::Absent,
        Scalar::Bool(b) => Shown::Bool(b),
        Scalar::Num(x) => Shown::number(x),
        Scalar::Str(t) => Shown::string(&t),
        Scalar::Date(d) => Shown::Date(crate::canonical::date_text(d)),
        Scalar::DateTime(m) => Shown::DateTime(crate::canonical::datetime_text(m)),
        Scalar::Array(xs) => Shown::Length(xs.len() as u64),
    }
}

/// A value as a diagnosis shows it, read in place: a vector, a matrix, or
/// a table by its size alone.
fn shown<'l, 'a, A: Access<'a>>(env: &Env<'l, A>, v: Val<'l, A>) -> Shown {
    let Val::R(a, Some(ft)) = v else {
        return shown_scalar(env.scalar(v));
    };
    let ft = unwrap_option(ft);
    match ft {
        FieldType::Vec { element, .. } => a
            .vec(element)
            .map_or(Shown::Other, |n| Shown::Length(n as u64)),
        FieldType::Factor { levels, ordered } => a
            .factor(levels.as_deref(), *ordered)
            .map_or(Shown::Other, |f| Shown::Length(f.codes.len() as u64)),
        FieldType::Matrix { element, .. } | FieldType::Array { element, .. } => a
            .dense(element)
            .map_or(Shown::Other, |d| Shown::Dim(d.shape.clone())),
        FieldType::SparseMatrix { element, .. } => a
            .sparse(element)
            .map_or(Shown::Other, |s| Shown::Dim(s.shape.to_vec())),
        FieldType::Table { columns, open, .. } => {
            a.table(columns, *open).map_or(Shown::Other, |t| {
                Shown::Dim(vec![t.rows, t.columns.len() as u64])
            })
        }
        FieldType::Logical
        | FieldType::Integer(_)
        | FieldType::Double(_)
        | FieldType::Character(_)
        | FieldType::Date
        | FieldType::Posixct => shown_scalar(env.scalar(Val::R(a, Some(ft)))),
        _ => Shown::Other,
    }
}

impl CompiledRule {
    /// The values the rule reads in `value`, whose fields are `scope`, for
    /// a violation at `path`: empty for the value, or the path of a failing
    /// element of an invariant that is an `all()`, relative to the value.
    pub fn diagnose_access<'a, A: Access<'a>>(
        &self,
        value: A,
        scope: &[FieldDef],
        lookup: &dyn TypeLookup,
        path: &[crate::problem::Segment],
    ) -> Diagnosis {
        let env = Env {
            data: value,
            scope,
            lookup,
            element: None,
        };
        let mut paths = Vec::new();
        field_paths(&self.invariant, &mut paths);
        if let Some(w) = &self.when {
            field_paths(w, &mut paths);
        }
        let mut out = Diagnosis::default();
        for p in paths {
            let s = if sensitive(scope, lookup, &p) {
                Shown::Sensitive
            } else {
                let var = Expr::Var {
                    root: Root::Data,
                    steps: p.clone(),
                };
                match env.eval(&var) {
                    Ok(Ok(v)) => shown(&env, v),
                    Ok(Err(_)) => Shown::Absent,
                    Err(_) => Shown::Other,
                }
            };
            let segments = super::var_segments(&p).unwrap_or_default();
            out.fields.push((segments, s));
        }
        if path.is_empty() {
            out.operands = operands(&env, &self.invariant, "");
            return out;
        }
        // A failing element of an `all()`: its value and the computed
        // operands of the condition at that element.
        let Expr::Quantifier {
            collection,
            kind,
            condition,
            ..
        } = &self.invariant
        else {
            return out;
        };
        let Expr::Var {
            root: Root::Data,
            steps,
        } = collection.as_ref()
        else {
            return out;
        };
        let Some(base) = super::var_segments(steps) else {
            return out;
        };
        // Only an element below the collection is matched.
        let Some(rel) = path.strip_prefix(base.as_slice()) else {
            return out;
        };
        let element_sensitive = sensitive(scope, lookup, steps);
        let Ok(Ok(c)) = env.eval(collection) else {
            return out;
        };
        let _ = env.each_element(c, *kind, &mut |suffix, el| {
            if suffix.segments() != rel {
                return Ok(true);
            }
            let e = Env {
                data: env.data,
                scope: env.scope,
                lookup: env.lookup,
                element: Some(el),
            };
            if element_sensitive {
                out.element = Some(Shown::Sensitive);
            } else {
                out.element = Some(shown(&e, Val::R(el.0, el.1)));
                out.operands = operands(&e, condition, "/all/1");
            }
            Ok(false)
        });
        out
    }
}

/// The computed operands of a comparison at the top of `e`, whose pointer
/// in the invariant is `at`: each operand that is neither a field nor a
/// literal, with its value.
fn operands<'l, 'a, A: Access<'a>>(env: &Env<'l, A>, e: &Expr, at: &str) -> Vec<(String, Shown)> {
    let Expr::Op(op, args) = e else {
        return Vec::new();
    };
    let Some(name) = comparison(*op) else {
        return Vec::new();
    };
    let mut out = Vec::new();
    for (i, x) in args.iter().enumerate() {
        if matches!(x, Expr::Var { .. } | Expr::Lit(_)) {
            continue;
        }
        let s = match env.eval(x) {
            Ok(Ok(v)) => shown(env, v),
            Ok(Err(_)) => Shown::Absent,
            Err(_) => Shown::Other,
        };
        out.push((format!("{at}/{name}/{i}"), s));
    }
    out
}
