//! The field-type constructors and `rule()` of the declaration API: each reads
//! its R arguments, builds through `rtemis_r8::ir::build`, which holds the
//! checks and their messages, and writes the result as the R list of its
//! class. R's own surface, the names of its operators and functions in a
//! rule, is the only part that lives here.
//!
//! Each routine returns the built list, or a string that is the message of
//! an argument that does not build, which the R function raises as an
//! `r8_declaration_error` from its own call.
//! spec: R8/class-system#declaring-types

use extendr_api::prelude::*;
use extendr_ffi::{CAR, CDR, INTEGER, LOGICAL, PRINTNAME, R_CHAR, REAL, SEXP, SEXPTYPE, TAG};
use rtemis_r8::ir::build::{self, Bounds, BuildError};
use rtemis_r8::ir::{FieldType, Logic, Number, Operator};
use rtemis_r8::name::QualifiedName;

use crate::rview::{attr_string, inherits, is_null, sexptype, string_elt, sym_r8_name, xlen};

type Built<T> = std::result::Result<T, BuildError>;

const NA_INTEGER: i32 = i32::MIN;

/// One number, or `NULL`; integers read as doubles, and `NA` is not a number.
fn number(x: SEXP, arg: &'static str) -> Built<Option<f64>> {
    if is_null(x) {
        return Ok(None);
    }
    let v = match (sexptype(x), xlen(x)) {
        (SEXPTYPE::REALSXP, 1) if !inherits(x, "factor") => unsafe { *REAL(x) },
        (SEXPTYPE::INTSXP, 1) if !inherits(x, "factor") => match unsafe { *INTEGER(x) } {
            NA_INTEGER => f64::NAN,
            i => f64::from(i),
        },
        _ => return Err(BuildError::NotOneNumber { arg }),
    };
    if v.is_finite() {
        Ok(Some(v))
    } else {
        Err(BuildError::NotOneNumber { arg })
    }
}

/// A numeric vector, or `NULL`; `NA` reads as NaN, which the builders reject.
fn numbers(x: SEXP, arg: &'static str) -> Built<Option<Vec<f64>>> {
    if is_null(x) {
        return Ok(None);
    }
    let n = xlen(x);
    match sexptype(x) {
        SEXPTYPE::REALSXP => Ok(Some(
            unsafe { std::slice::from_raw_parts(REAL(x), n) }.to_vec(),
        )),
        SEXPTYPE::INTSXP => Ok(Some(
            unsafe { std::slice::from_raw_parts(INTEGER(x), n) }
                .iter()
                .map(|&i| {
                    if i == NA_INTEGER {
                        f64::NAN
                    } else {
                        f64::from(i)
                    }
                })
                .collect(),
        )),
        _ => Err(BuildError::NotOneNumber { arg }),
    }
}

/// A character vector without `NA`, or `NULL`.
fn strings(x: SEXP, arg: &'static str) -> Built<Option<Vec<String>>> {
    if is_null(x) {
        return Ok(None);
    }
    if sexptype(x) != SEXPTYPE::STRSXP {
        return Err(BuildError::NotStrings { arg });
    }
    (0..xlen(x))
        .map(|i| {
            string_elt(x, i)
                .map(|s| s.into_owned())
                .ok_or(BuildError::MissingString { arg })
        })
        .collect::<Built<Vec<_>>>()
        .map(Some)
}

/// One string, or `NULL`.
fn string(x: SEXP, arg: &'static str) -> Built<Option<String>> {
    if is_null(x) {
        return Ok(None);
    }
    match (sexptype(x), xlen(x)) {
        (SEXPTYPE::STRSXP, 1) => string_elt(x, 0)
            .map(|s| Some(s.into_owned()))
            .ok_or(BuildError::NotOneString { arg }),
        _ => Err(BuildError::NotOneString { arg }),
    }
}

/// One `TRUE` or `FALSE`.
fn flag(x: SEXP, arg: &'static str) -> Built<bool> {
    let given = (sexptype(x) == SEXPTYPE::LGLSXP && xlen(x) == 1).then(|| unsafe { *LOGICAL(x) });
    match given {
        Some(0) => Ok(false),
        Some(1) => Ok(true),
        _ => Err(BuildError::NotFlag { arg }),
    }
}

/// The qualified name of an R8 type object.
fn type_name(x: SEXP) -> Built<QualifiedName> {
    attr_string(x, sym_r8_name())
        .and_then(|n| QualifiedName::parse(&n).ok())
        .ok_or(BuildError::NotFieldType)
}

/// A field type argument: an `r8_field_type` or an R8 type object.
pub(crate) fn field_type(x: SEXP) -> Built<FieldType> {
    if inherits(x, "r8_field_type") {
        crate::irde::from_r(x).map_err(|_| BuildError::NotFieldType)
    } else if inherits(x, "r8_type") {
        type_name(x).map(build::named)
    } else {
        Err(BuildError::NotFieldType)
    }
}

/// The R list of a field type, of class `r8_field_type`.
fn field_type_list(t: &FieldType) -> Robj {
    let mut out = crate::irser::to_r(t).unwrap_or_else(|e| Robj::from(e.to_string()));
    let _ = out.set_class(["r8_field_type"]);
    out
}

/// The list of a built field type, or the message of the argument that does
/// not build.
fn written(t: Built<FieldType>) -> Robj {
    match t {
        Ok(t) => field_type_list(&t),
        Err(e) => Robj::from(e.to_string()),
    }
}

fn bounds(min: &Robj, max: &Robj, exclusive_min: &Robj, exclusive_max: &Robj) -> Built<Bounds> {
    unsafe {
        Ok(Bounds {
            min: number(min.get(), "min")?,
            max: number(max.get(), "max")?,
            exclusive_min: number(exclusive_min.get(), "exclusive_min")?,
            exclusive_max: number(exclusive_max.get(), "exclusive_max")?,
        })
    }
}

/// `type_integer()`.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_ft_integer(
    min: Robj,
    max: Robj,
    exclusive_min: Robj,
    exclusive_max: Robj,
    values: Robj,
) -> Robj {
    written((|| {
        let b = bounds(&min, &max, &exclusive_min, &exclusive_max)?;
        let v = numbers(unsafe { values.get() }, "values")?;
        build::integer(b, v.as_deref())
    })())
}

/// `type_double()`.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_ft_double(
    min: Robj,
    max: Robj,
    exclusive_min: Robj,
    exclusive_max: Robj,
    infinite: Robj,
    nan: Robj,
    values: Robj,
) -> Robj {
    written((|| {
        let b = bounds(&min, &max, &exclusive_min, &exclusive_max)?;
        let v = numbers(unsafe { values.get() }, "values")?;
        build::double(
            b,
            flag(unsafe { infinite.get() }, "infinite")?,
            flag(unsafe { nan.get() }, "nan")?,
            v.as_deref(),
        )
    })())
}

/// `type_character()`.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_ft_character(pattern: Robj, values: Robj) -> Robj {
    written((|| {
        let p = string(unsafe { pattern.get() }, "pattern")?;
        let v = strings(unsafe { values.get() }, "values")?;
        build::character(p.as_deref(), v)
    })())
}

/// `type_factor()`.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_ft_factor(levels: Robj, ordered: Robj) -> Robj {
    written((|| {
        let l = strings(unsafe { levels.get() }, "levels")?;
        Ok(build::factor(l, flag(unsafe { ordered.get() }, "ordered")?))
    })())
}

/// A field-type argument as the R list a container holds: an
/// `r8_field_type` as given, or the `named` field type of an R8 type object.
/// The element is not read: the checks of a container do not depend on it,
/// and the declaration reads it once.
fn element(x: SEXP) -> Built<Robj> {
    if inherits(x, "r8_field_type") {
        Ok(unsafe { Robj::from_sexp(x) })
    } else if inherits(x, "r8_type") {
        type_name(x).map(|n| field_type_list(&build::named(n)))
    } else {
        Err(BuildError::NotFieldType)
    }
}

/// A member of a container: an R object (the element type), or an IR value.
enum Member<'a> {
    Object(&'a Robj),
    Count(u64),
    Counts(&'a [u32]),
    Flag,
    Keys(&'a rtemis_r8::ir::CharacterType),
}

/// The list of a container field type, of class `r8_field_type`, from its
/// members; an absent member is left out.
fn container(kind: &str, members: &[(&str, Option<Member<'_>>)]) -> Robj {
    let mut b = crate::irser::Builder::named(1 + members.len());
    let _ = b.value("kind", kind);
    for (name, m) in members {
        let _ = match m {
            None => Ok(()),
            Some(Member::Object(x)) => {
                b.object(name, unsafe { x.get() });
                Ok(())
            }
            Some(Member::Count(n)) => b.value(name, n),
            Some(Member::Counts(d)) => b.value(name, d),
            Some(Member::Flag) => b.value(name, &true),
            Some(Member::Keys(k)) => b.value(name, *k),
        };
    }
    b.finish("r8_field_type")
}

/// `field()`: the field type, as given or as the `named` type of an R8 type
/// object, and the field's facts; `has_default` is `!missing(default)`.
/// @keywords internal
/// @noRd
#[extendr]
#[allow(clippy::too_many_arguments)]
fn r8_field(
    r#type: Robj,
    default: Robj,
    has_default: bool,
    description: Robj,
    sensitive: Robj,
    unit: Robj,
    compute: Robj,
    context: Robj,
    extensions: Robj,
) -> Robj {
    let ty = match element(unsafe { r#type.get() }) {
        Ok(t) => t,
        Err(e) => return message(e),
    };
    let read = (|| {
        let c = unsafe { compute.get() };
        let expr = if is_null(c) { None } else { Some(logic(c)?) };
        let resolve = build::resolve(expr, flag(unsafe { context.get() }, "context")?)?;
        Ok((resolve, flag(unsafe { sensitive.get() }, "sensitive")?))
    })();
    let (resolve, s) = match read {
        Ok(r) => r,
        Err(e) => return message(e),
    };
    let s = Robj::from(s);
    let has = Robj::from(has_default);
    let mut b = crate::irser::Builder::named(7 + usize::from(resolve.is_some()));
    if let Some(r) = &resolve {
        let _ = b.value("resolve", r);
    }
    unsafe {
        b.object("type", ty.get());
        b.object("default", default.get());
        b.object("has_default", has.get());
        b.object("description", description.get());
        b.object("sensitive", s.get());
        b.object("unit", unit.get());
        b.object("extensions", extensions.get());
    }
    b.finish("r8_field")
}

/// The message of a container that does not build.
fn message(e: BuildError) -> Robj {
    Robj::from(e.to_string())
}

/// `vec()`.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_ft_vec(r#type: Robj, min_length: Robj, max_length: Robj, unique: Robj) -> Robj {
    let built = (|| unsafe {
        let el = element(r#type.get())?;
        let (lo, hi) = build::lengths(
            number(min_length.get(), "min_length")?,
            number(max_length.get(), "max_length")?,
        )?;
        let unique = flag(unique.get(), "unique")?;
        Ok(container(
            "vec",
            &[
                ("element", Some(Member::Object(&el))),
                ("min_length", lo.map(Member::Count)),
                ("max_length", hi.map(Member::Count)),
                ("unique", unique.then_some(Member::Flag)),
            ],
        ))
    })();
    built.unwrap_or_else(message)
}

/// `option()`.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_ft_option(r#type: Robj) -> Robj {
    let built = (|| {
        let inner = element(unsafe { r#type.get() })?;
        Ok(container(
            "option",
            &[("inner", Some(Member::Object(&inner)))],
        ))
    })();
    built.unwrap_or_else(message)
}

/// `btree_map()` and, with `ordered = TRUE`, `index_map()`.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_ft_map(r#type: Robj, keys: Robj, ordered: bool) -> Robj {
    let built = (|| {
        let value = element(unsafe { r#type.get() })?;
        let k = unsafe { keys.get() };
        let key_type = if is_null(k) {
            None
        } else if inherits(k, "r8_field_type") {
            Some(field_type(k).map_err(|_| BuildError::Keys)?)
        } else {
            return Err(BuildError::Keys);
        };
        let constraints = build::keys(key_type)?;
        Ok(container(
            if ordered { "index_map" } else { "btree_map" },
            &[
                ("value", Some(Member::Object(&value))),
                (
                    "keys",
                    (constraints != rtemis_r8::ir::CharacterType::default())
                        .then_some(Member::Keys(&constraints)),
                ),
            ],
        ))
    })();
    built.unwrap_or_else(message)
}

/// `type_matrix()`, `type_array()` (`dim`), and `type_sparse_matrix()`, as `kind`
/// names them: `"matrix"`, `"array"`, and `"sparse_matrix"`.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_ft_matrix(r#type: Robj, nrow: Robj, ncol: Robj, dim: Robj, kind: crate::utf8::Utf8) -> Robj {
    let kind: &str = &kind;
    let built = (|| unsafe {
        let t = r#type.get();
        if kind == "matrix" && inherits(t, "r8_struct_type") {
            return Ok(field_type_list(&build::matrix_of(type_name(t)?)));
        }
        let el = element(t)?;
        if kind == "array" {
            let d = numbers(dim.get(), "dim")?
                .map(|d| {
                    d.iter()
                        .map(|&x| build::count(x, "dim"))
                        .collect::<Built<Vec<u32>>>()
                })
                .transpose()?;
            return Ok(container(
                "array",
                &[
                    ("element", Some(Member::Object(&el))),
                    ("dim", d.as_deref().map(Member::Counts)),
                ],
            ));
        }
        let count = |x: Option<f64>, arg| x.map(|v| build::count(v, arg)).transpose();
        let (nrow, ncol) = (
            count(number(nrow.get(), "nrow")?, "nrow")?,
            count(number(ncol.get(), "ncol")?, "ncol")?,
        );
        Ok(container(
            if kind == "sparse_matrix" {
                "sparse_matrix"
            } else {
                "matrix"
            },
            &[
                ("element", Some(Member::Object(&el))),
                ("nrow", nrow.map(|v| Member::Count(u64::from(v)))),
                ("ncol", ncol.map(|v| Member::Count(u64::from(v)))),
            ],
        ))
    })();
    built.unwrap_or_else(message)
}

/// `rule()`: `invariant` and `when` are the unevaluated expressions;
/// `message` and `id` are `NULL` or one string.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_rule(invariant: Robj, when: Robj, message: Robj, id: Robj) -> Robj {
    let built = (|| unsafe {
        let text = |x: &Robj, e: BuildError| -> Built<Option<String>> {
            if is_null(x.get()) {
                return Ok(None);
            }
            string(x.get(), "").ok().flatten().map(Some).ok_or(e)
        };
        let id = text(&id, BuildError::RuleId)?;
        let message = text(&message, BuildError::RuleMessage)?;
        let w = when.get();
        let when = if is_null(w) { None } else { Some(logic(w)?) };
        build::rule(
            logic(invariant.get())?,
            when,
            message.as_deref(),
            id.as_deref(),
        )
    })();
    match built {
        Ok(r) => {
            let mut out = crate::irser::to_r(&r).unwrap_or_else(|e| Robj::from(e.to_string()));
            let _ = out.set_class(["r8_rule"]);
            out
        }
        Err(e) => Robj::from(e.to_string()),
    }
}

/// The dialect expression of an R expression, as the list `rule()` holds,
/// or the message of an expression the dialect does not hold.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_logic(expr: Robj) -> Robj {
    match logic(unsafe { expr.get() }) {
        Ok(l) => crate::irser::to_r(&l).unwrap_or_else(|e| Robj::from(e.to_string())),
        Err(e) => Robj::from(e.to_string()),
    }
}

/// Dates or date-times as a rule writes them.
///
/// @param x Double: Days, or seconds, since 1970-01-01.
/// @param datetime Logical: `x` holds seconds.
/// @return Character.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_date_literals(x: Vec<f64>, datetime: bool) -> Vec<String> {
    x.into_iter()
        .map(|v| {
            if datetime {
                rtemis_r8::canonical::datetime_text((v * 1e6).round() as i64)
            } else {
                rtemis_r8::canonical::date_text(v as i64)
            }
        })
        .collect()
}

/// The R functions and operators of a rule and the operators of the dialect
/// they write, in the order `r8_rule_operators()` returns them.
const OPERATORS: &[(&str, Operator)] = &[
    ("==", Operator::StrictEq),
    ("!=", Operator::StrictNe),
    ("<", Operator::Lt),
    ("<=", Operator::Le),
    (">", Operator::Gt),
    (">=", Operator::Ge),
    ("+", Operator::Add),
    ("-", Operator::Sub),
    ("*", Operator::Mul),
    ("/", Operator::Div),
    ("%%", Operator::Mod),
    ("^", Operator::Pow),
    ("&&", Operator::And),
    ("&", Operator::And),
    ("||", Operator::Or),
    ("|", Operator::Or),
    ("!", Operator::Not),
    ("%in%", Operator::In),
    ("paste0", Operator::Cat),
    ("is.null", Operator::IsNull),
    ("is_variant", Operator::IsVariant),
    ("length", Operator::Length),
    ("nrow", Operator::Nrow),
    ("ncol", Operator::Ncol),
    ("dim", Operator::Dim),
    ("levels", Operator::Levels),
    ("floor", Operator::Floor),
    ("ceiling", Operator::Ceiling),
    ("round", Operator::Round),
    ("min", Operator::Min),
    ("max", Operator::Max),
    ("abs", Operator::Abs),
    ("sqrt", Operator::Sqrt),
    ("log", Operator::Log),
    ("exp", Operator::Exp),
    ("sum", Operator::Sum),
    ("mean", Operator::Mean),
    ("median", Operator::Median),
    ("n", Operator::N),
    ("count", Operator::Count),
    ("col", Operator::Col),
    ("pmin", Operator::Pmin),
    ("pmax", Operator::Pmax),
    ("is.na", Operator::IsNa),
    ("nchar", Operator::Nchar),
    ("all", Operator::All),
    ("any", Operator::Any),
    ("subset", Operator::Subset),
    ("non_increasing", Operator::NonIncreasing),
    ("non_decreasing", Operator::NonDecreasing),
];

fn operator(name: &str) -> Option<Operator> {
    OPERATORS
        .iter()
        .find(|(r, _)| *r == name)
        .map(|&(_, op)| op)
}

/// A message on a rule with its operators as R writes them.
pub(crate) fn in_r(message: &str) -> String {
    const AFTER: [&str; 9] = [
        " takes",
        " compares",
        " combines",
        " searches",
        " joins",
        " counts",
        " tests",
        " reads whole",
        " reads a whole",
    ];
    let mut out = message.to_string();
    let mut seen: Vec<Operator> = Vec::new();
    for &(r, op) in OPERATORS {
        // The first spelling is the one that R shows.
        if r == op.as_str() || seen.contains(&op) {
            continue;
        }
        seen.push(op);
        let name = format!("`{}`", op.as_str());
        if !out.contains(&name) {
            continue;
        }
        for after in AFTER {
            out = out.replace(&format!("{name}{after}"), &format!("`{r}`{after}"));
        }
        for before in ["operand of ", "condition of "] {
            out = out.replace(&format!("{before}{name}"), &format!("{before}`{r}`"));
        }
    }
    out
}

/// The R spelling of each operator of the dialect: the dialect operators,
/// named by the R function or operator that writes them.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_rule_operators() -> Robj {
    let mut out = Robj::from(
        OPERATORS
            .iter()
            .map(|(_, op)| op.as_str())
            .collect::<Vec<_>>(),
    );
    let _ = out.set_names(OPERATORS.iter().map(|(r, _)| *r).collect::<Vec<_>>());
    out
}

/// The functions of the dialect, as R writes them, for a message.
const KNOWN_FUNCTIONS: &str = "comparisons, arithmetic, logical operators, %in%, if, is.na(), \
is.null(), is_variant(), length(), nchar(), nrow(), ncol(), dim(), levels(), abs(), sqrt(), log(), \
exp(), floor(), ceiling(), round(), min(), max(), pmin(), pmax(), sum(), mean(), median(), n(), \
count(), all(), any(), subset(), non_increasing(), non_decreasing(), col(), NA, and val() for a \
constant from R";

/// The function of rules nearest to `name`, as R writes it.
fn nearest_function(name: &str) -> Option<String> {
    rtemis_r8::name::nearest(
        name,
        OPERATORS
            .iter()
            .map(|(r, _)| *r)
            .chain(["if", "is_variant", "val", "c"])
            .filter(|r| {
                r.chars()
                    .all(|c| c.is_alphanumeric() || c == '_' || c == '.')
            }),
    )
    .map(str::to_string)
}

/// The candidate nearest to `name`, for a message.
///
/// @param name Character: The name.
/// @param candidates Character: The candidates.
/// @return Character: The nearest, or a vector of length 0.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_nearest_name(name: crate::utf8::Utf8, candidates: crate::utf8::Utf8s) -> Vec<String> {
    let name: &str = &name;
    let candidates: Vec<String> = candidates.to_vec();
    rtemis_r8::name::nearest(name, candidates.iter().map(String::as_str))
        .map(str::to_string)
        .into_iter()
        .collect()
}

/// The name of a symbol.
fn symbol_name(x: SEXP) -> String {
    unsafe {
        let p = PRINTNAME(x);
        std::ffi::CStr::from_ptr(R_CHAR(p))
            .to_string_lossy()
            .into_owned()
    }
}

/// `deparse(x)`, for a message; `x` is quoted, so that R does not evaluate
/// it.
fn deparsed(x: SEXP) -> String {
    let robj = unsafe { Robj::from_sexp(x) };
    let quoted = lang!("quote", robj);
    call!("deparse", quoted)
        .ok()
        .and_then(|d| d.as_str_vector().map(|v| v.join(" ")))
        .unwrap_or_default()
}

/// The names of the arguments of a call, in order: `""` for an unnamed one.
fn arg_names(x: SEXP) -> Vec<String> {
    let mut out = Vec::new();
    let mut a = unsafe { CDR(x) };
    while !is_null(a) {
        let tag = unsafe { TAG(a) };
        out.push(if is_null(tag) {
            String::new()
        } else {
            symbol_name(tag)
        });
        a = unsafe { CDR(a) };
    }
    out
}

/// The arguments of a call, in order.
fn args(x: SEXP) -> Vec<SEXP> {
    let mut out = Vec::new();
    let mut a = unsafe { CDR(x) };
    while !is_null(a) {
        out.push(unsafe { CAR(a) });
        a = unsafe { CDR(a) };
    }
    out
}

/// A constant of a rule: one value of an atomic vector, not `NA`.
fn constant(x: SEXP) -> Built<Logic> {
    signed(x, false)
}

/// [`constant`], of which `negative` gives the negative, as of a number.
fn signed(x: SEXP, negative: bool) -> Built<Logic> {
    if xlen(x) != 1 {
        return Err(BuildError::Constant);
    }
    let number = |d: f64| {
        if d.is_infinite() {
            Err(BuildError::InfiniteConstant)
        } else if crate::rview::is_na_real(d) {
            Ok(Logic::Null)
        } else if d.is_nan() {
            Err(BuildError::Constant)
        } else {
            Ok(Logic::Number(Number::F64(d)))
        }
    };
    unsafe {
        match sexptype(x) {
            SEXPTYPE::INTSXP => match *INTEGER(x) {
                NA_INTEGER if !negative => Ok(Logic::Null),
                NA_INTEGER => Err(BuildError::Constant),
                i => {
                    let i = if negative { -i } else { i };
                    Ok(Logic::Number(if i >= 0 {
                        Number::U64(i as u64)
                    } else {
                        Number::I64(i64::from(i))
                    }))
                }
            },
            SEXPTYPE::REALSXP if negative => number(-*REAL(x)),
            SEXPTYPE::REALSXP => number(*REAL(x)),
            SEXPTYPE::LGLSXP if !negative => match *LOGICAL(x) {
                NA_INTEGER => Ok(Logic::Null),
                b => Ok(Logic::Bool(b != 0)),
            },
            SEXPTYPE::STRSXP if !negative => Ok(string_elt(x, 0)
                .map(|s| Logic::String(s.as_ref().into()))
                .unwrap_or(Logic::Null)),
            _ => Err(BuildError::Constant),
        }
    }
}

/// The constant that an expression writes: a value, `-1`, or `+1`; `None` for
/// another expression. R reads `-1` as a call of `-`.
fn literal(x: SEXP) -> Option<Built<Logic>> {
    if is_atomic(x) {
        return Some(constant(x));
    }
    if sexptype(x) != SEXPTYPE::LANGSXP {
        return None;
    }
    let f = unsafe { CAR(x) };
    let sign = match sexptype(f) {
        SEXPTYPE::SYMSXP => symbol_name(f),
        _ => return None,
    };
    let a = args(x);
    let [of] = a.as_slice() else { return None };
    let number = matches!(sexptype(*of), SEXPTYPE::INTSXP | SEXPTYPE::REALSXP);
    match sign.as_str() {
        "-" if number => Some(signed(*of, true)),
        "+" if number => Some(signed(*of, false)),
        "(" => literal(*of),
        _ => None,
    }
}

fn is_atomic(x: SEXP) -> bool {
    matches!(
        sexptype(x),
        SEXPTYPE::LGLSXP
            | SEXPTYPE::INTSXP
            | SEXPTYPE::REALSXP
            | SEXPTYPE::STRSXP
            | SEXPTYPE::CPLXSXP
            | SEXPTYPE::RAWSXP
    )
}

/// The names of `a`, `a$b`, `a$b$c`, in order: a symbol is one name, and `$`
/// makes a path.
fn path(x: SEXP) -> Option<Vec<String>> {
    match sexptype(x) {
        SEXPTYPE::SYMSXP => Some(vec![symbol_name(x)]),
        SEXPTYPE::LANGSXP => {
            let f = unsafe { CAR(x) };
            let a = args(x);
            if sexptype(f) == SEXPTYPE::SYMSXP && symbol_name(f) == "$" && a.len() == 2 {
                let mut names = path(a[0])?;
                names.push(match sexptype(a[1]) {
                    SEXPTYPE::SYMSXP => symbol_name(a[1]),
                    SEXPTYPE::STRSXP => string_elt(a[1], 0)?.into_owned(),
                    _ => return None,
                });
                Some(names)
            } else {
                None
            }
        }
        _ => None,
    }
}

/// The dialect expression of an R expression of a rule.
pub(crate) fn logic(x: SEXP) -> Built<Logic> {
    if is_null(x) {
        return Ok(Logic::Null);
    }
    if let Some(c) = literal(x) {
        return c;
    }
    if let Some(p) = path(x) {
        let names: Vec<&str> = p.iter().map(String::as_str).collect();
        return Ok(build::path(&names));
    }
    if sexptype(x) != SEXPTYPE::LANGSXP {
        return Err(BuildError::RuleOperand { expr: deparsed(x) });
    }
    let mut f = unsafe { CAR(x) };
    // `base::is.null()` and `R8::` before a dialect word name the same
    // functions as their bare forms.
    if sexptype(f) == SEXPTYPE::LANGSXP {
        let head = unsafe { CAR(f) };
        let a = args(f);
        if sexptype(head) == SEXPTYPE::SYMSXP
            && symbol_name(head) == "::"
            && a.len() == 2
            && sexptype(a[0]) == SEXPTYPE::SYMSXP
            && matches!(symbol_name(a[0]).as_str(), "R8" | "base")
        {
            f = a[1];
        }
    }
    if sexptype(f) != SEXPTYPE::SYMSXP {
        return Err(BuildError::RuleCallee { expr: deparsed(x) });
    }
    let name = symbol_name(f);
    let a = args(x);
    match name.as_str() {
        "int64" => match a.as_slice() {
            [t] if sexptype(*t) == SEXPTYPE::STRSXP && xlen(*t) == 1 => {
                let text = string_elt(*t, 0).ok_or(BuildError::Int64)?;
                text.parse::<i64>()
                    .map(|i| {
                        if i >= 0 {
                            Number::U64(i as u64)
                        } else {
                            Number::I64(i)
                        }
                    })
                    .or_else(|_| text.parse::<u64>().map(Number::U64))
                    .map(Logic::Number)
                    .map_err(|_| BuildError::Int64)
            }
            _ => Err(BuildError::Int64),
        },
        "json_ir" => match a.as_slice() {
            [t] if sexptype(*t) == SEXPTYPE::STRSXP && xlen(*t) == 1 => {
                let text = string_elt(*t, 0).ok_or(BuildError::JsonIr { detail: None })?;
                build::json_logic(&text)
            }
            _ => Err(BuildError::JsonIr { detail: None }),
        },
        "(" => logic(
            *a.first()
                .ok_or_else(|| BuildError::RuleOperand { expr: deparsed(x) })?,
        ),
        "c" => build::array_literal(
            a.iter()
                .map(|&v| match literal(v) {
                    Some(Err(BuildError::Constant)) | None => Err(BuildError::ArrayLiteral),
                    Some(c) => c,
                })
                .collect::<Built<Vec<_>>>()?,
        ),
        "if" => Ok(build::call(
            Operator::If,
            a.iter().map(|&v| logic(v)).collect::<Built<Vec<_>>>()?,
        )),
        "is_variant" => match a.as_slice() {
            [v, name] => build::is_variant(
                logic(*v)?,
                if is_atomic(*name) {
                    constant(*name).map_err(|_| BuildError::VariantName)?
                } else {
                    return Err(BuildError::VariantName);
                },
            ),
            _ => Err(BuildError::VariantName),
        },
        "[[" | "[" => Err(BuildError::RuleIndex { expr: deparsed(x) }),
        _ => match operator(&name) {
            Some(op) => {
                let names = arg_names(x);
                let mut operands = Vec::with_capacity(a.len());
                let mut skip = false;
                for (v, n) in a.iter().zip(&names) {
                    match n.as_str() {
                        "" => operands.push(logic(*v)?),
                        "na.rm"
                            if matches!(
                                op,
                                Operator::Sum
                                    | Operator::Mean
                                    | Operator::Median
                                    | Operator::Min
                                    | Operator::Max
                                    | Operator::Count
                            ) =>
                        {
                            skip = match literal(*v) {
                                Some(Ok(Logic::Bool(b))) => b,
                                _ => return Err(BuildError::NaRm { function: name }),
                            };
                        }
                        other => {
                            return Err(BuildError::RuleArgument {
                                function: name.clone(),
                                argument: other.to_string(),
                            });
                        }
                    }
                }
                if skip {
                    operands = vec![build::call(Operator::NaRm, operands)];
                }
                let compares = matches!(
                    op,
                    Operator::StrictEq
                        | Operator::StrictNe
                        | Operator::Lt
                        | Operator::Le
                        | Operator::Gt
                        | Operator::Ge
                        | Operator::In
                );
                if compares && operands.iter().any(|o| matches!(o, Logic::Null)) {
                    return Err(BuildError::MissingComparison { expr: deparsed(x) });
                }
                Ok(build::call(op, operands))
            }
            None => Err(BuildError::UnknownFunction {
                nearest: nearest_function(&name),
                name,
                known: KNOWN_FUNCTIONS,
            }),
        },
    }
}

extendr_module! {
    mod fieldtype;
    fn r8_nearest_name;
    fn r8_ft_integer;
    fn r8_field;
    fn r8_ft_double;
    fn r8_ft_character;
    fn r8_ft_factor;
    fn r8_ft_vec;
    fn r8_ft_option;
    fn r8_ft_map;
    fn r8_ft_matrix;
    fn r8_rule;
    fn r8_logic;
    fn r8_date_literals;
    fn r8_rule_operators;
}
