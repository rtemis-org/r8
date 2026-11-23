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
use extendr_ffi::{CAR, CDR, INTEGER, LOGICAL, PRINTNAME, R_CHAR, REAL, SEXP, SEXPTYPE};
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

/// `isTRUE(x)`.
fn is_true(x: SEXP) -> bool {
    sexptype(x) == SEXPTYPE::LGLSXP && xlen(x) == 1 && unsafe { *LOGICAL(x) } == 1
}

/// `isFALSE(x)`.
fn is_false(x: SEXP) -> bool {
    sexptype(x) == SEXPTYPE::LGLSXP && xlen(x) == 1 && unsafe { *LOGICAL(x) } == 0
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
    finite: Robj,
    values: Robj,
) -> Robj {
    written((|| {
        let b = bounds(&min, &max, &exclusive_min, &exclusive_max)?;
        let v = numbers(unsafe { values.get() }, "values")?;
        build::double(b, !is_false(unsafe { finite.get() }), v.as_deref())
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
    written(
        strings(unsafe { levels.get() }, "levels")
            .map(|l| build::factor(l, is_true(unsafe { ordered.get() }))),
    )
}

/// `type_r()`.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_ft_r(class: Robj) -> Robj {
    written(
        string(unsafe { class.get() }, "class")
            .and_then(|c| c.ok_or(BuildError::NotOneString { arg: "class" }))
            .map(|c| build::r_class(&c)),
    )
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
    let resolve = (|| {
        let c = unsafe { compute.get() };
        let expr = if is_null(c) { None } else { Some(logic(c)?) };
        build::resolve(expr, is_true(unsafe { context.get() }))
    })();
    let resolve = match resolve {
        Ok(r) => r,
        Err(e) => return message(e),
    };
    let flag = |x: &Robj| Robj::from(is_true(unsafe { x.get() }));
    let s = flag(&sensitive);
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
        Ok(container(
            "vec",
            &[
                ("element", Some(Member::Object(&el))),
                ("min_length", lo.map(Member::Count)),
                ("max_length", hi.map(Member::Count)),
                ("unique", is_true(unique.get()).then_some(Member::Flag)),
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

/// `type_matrix()`, `type_array()` (`dim`), and `type_sparse_matrix()`
/// (`sparse = TRUE`).
/// @keywords internal
/// @noRd
#[extendr]
fn r8_ft_matrix(r#type: Robj, nrow: Robj, ncol: Robj, dim: Robj, sparse: bool) -> Robj {
    let built = (|| unsafe {
        let t = r#type.get();
        if !sparse && is_null(dim.get()) && inherits(t, "r8_struct_type") {
            return Ok(field_type_list(&build::matrix_of(type_name(t)?)));
        }
        let el = element(t)?;
        if let Some(d) = numbers(dim.get(), "dim")? {
            let d = d
                .iter()
                .map(|&x| build::count(x, "dim"))
                .collect::<Built<Vec<u32>>>()?;
            return Ok(container(
                "array",
                &[
                    ("element", Some(Member::Object(&el))),
                    ("dim", Some(Member::Counts(&d))),
                ],
            ));
        }
        let count = |x: Option<f64>, arg| x.map(|v| build::count(v, arg)).transpose();
        let (nrow, ncol) = (
            count(number(nrow.get(), "nrow")?, "nrow")?,
            count(number(ncol.get(), "ncol")?, "ncol")?,
        );
        Ok(container(
            if sparse { "sparse_matrix" } else { "matrix" },
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
    ("sum", Operator::Sum),
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
const KNOWN_FUNCTIONS: &str = "comparisons, arithmetic, logical operators, %in%, if, is.null(), \
is_variant(), length(), nchar(), nrow(), ncol(), dim(), levels(), abs(), floor(), ceiling(), \
round(), min(), max(), sum(), all(), any(), subset(), non_increasing(), non_decreasing(), and \
lit() for a constant from R";

/// The function of rules nearest to `name`, as R writes it.
fn nearest_function(name: &str) -> Option<String> {
    rtemis_r8::name::nearest(
        name,
        OPERATORS
            .iter()
            .map(|(r, _)| *r)
            .chain(["if", "is_variant", "lit", "c"])
            .filter(|r| {
                r.chars()
                    .all(|c| c.is_alphanumeric() || c == '_' || c == '.')
            }),
    )
    .map(str::to_string)
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
fn constant(x: SEXP) -> Option<Logic> {
    if xlen(x) != 1 {
        return None;
    }
    unsafe {
        match sexptype(x) {
            SEXPTYPE::LGLSXP => match *LOGICAL(x) {
                NA_INTEGER => None,
                b => Some(Logic::Bool(b != 0)),
            },
            SEXPTYPE::INTSXP => match *INTEGER(x) {
                NA_INTEGER => None,
                i => Some(Logic::Number(if i >= 0 {
                    Number::U64(i as u64)
                } else {
                    Number::I64(i64::from(i))
                })),
            },
            SEXPTYPE::REALSXP => {
                let d = *REAL(x);
                d.is_finite().then_some(Logic::Number(Number::F64(d)))
            }
            SEXPTYPE::STRSXP => string_elt(x, 0).map(|s| Logic::String(s.as_ref().into())),
            _ => None,
        }
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

/// The path of `a`, `a$b`, `a$b$c`, as `a.b.c`.
fn path(x: SEXP) -> Option<String> {
    match sexptype(x) {
        SEXPTYPE::SYMSXP => Some(symbol_name(x)),
        SEXPTYPE::LANGSXP => {
            let f = unsafe { CAR(x) };
            let a = args(x);
            if sexptype(f) == SEXPTYPE::SYMSXP && symbol_name(f) == "$" && a.len() == 2 {
                let head = path(a[0])?;
                let tail = match sexptype(a[1]) {
                    SEXPTYPE::SYMSXP => symbol_name(a[1]),
                    SEXPTYPE::STRSXP => string_elt(a[1], 0)?.into_owned(),
                    _ => return None,
                };
                Some(format!("{head}.{tail}"))
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
    if is_atomic(x) {
        return constant(x).ok_or(BuildError::Constant);
    }
    if let Some(p) = path(x) {
        return Ok(build::var(p));
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
                .map(|&v| {
                    if is_atomic(v) {
                        constant(v).ok_or(BuildError::ArrayLiteral)
                    } else {
                        Err(BuildError::ArrayLiteral)
                    }
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
                    constant(*name).ok_or(BuildError::VariantName)?
                } else {
                    return Err(BuildError::VariantName);
                },
            ),
            _ => Err(BuildError::VariantName),
        },
        "[[" | "[" => Err(BuildError::RuleIndex { expr: deparsed(x) }),
        _ => match operator(&name) {
            Some(op) => Ok(build::call(
                op,
                a.iter().map(|&v| logic(v)).collect::<Built<Vec<_>>>()?,
            )),
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
    fn r8_ft_integer;
    fn r8_field;
    fn r8_ft_double;
    fn r8_ft_character;
    fn r8_ft_factor;
    fn r8_ft_r;
    fn r8_ft_vec;
    fn r8_ft_option;
    fn r8_ft_map;
    fn r8_ft_matrix;
    fn r8_rule;
    fn r8_logic;
    fn r8_rule_operators;
}
