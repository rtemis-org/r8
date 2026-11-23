# rules.R
# ::R8::
# 2026- EDG rtemis.org

# rule() captures its expressions unevaluated; `fieldtype.rs` translates an
# R expression to the dialect's JSONLogic, as a list: an operation is a named
# list with one member whose value is the list of operands, and a field is
# `list(var = "path")`. `rtemis-r8` checks the result against the type when
# the type is declared.

# %% rule ----
#' Declare a rule
#'
#' A rule states what must hold for a value of a struct, an enum variant, or
#' a table row: an R expression over its fields, captured unevaluated,
#' compiled when the type is declared, and evaluated in Rust each time a
#' value is built or a field it reads is replaced. `rule(lo < hi)` is a
#' complete rule; `message` states the requirement for the user, and `id`
#' gives the rule a stable name by which code and tests refer to it.
#'
#' @section One field:
#' A bound, a set of allowed values, a pattern, or a length on one field is
#' an argument of the field's type, checked before any rule:
#' `type_double(min = 0)`, `type_integer(values = 1:3)`,
#' `type_character(pattern = "^[A-Z]")`, `vec(type_double(), min_length = 1)`.
#' A rule on one field states what those arguments do not: `k %% 2 == 0`,
#' `abs(x) <= 1`, `nchar(name) <= 20`.
#'
#' @section Several fields:
#' A rule reads any fields by name, and nested fields with `$`:
#' `lo < hi`, `lo + hi <= 1`, `kernel$gamma <= 10`.
#' `when` limits a rule to the values for which a condition holds:
#' `when = is_variant(kernel, "Rbf")`.
#'
#' @section Optional fields:
#' A rule holds when a field it reads is absent, so a rule over an
#' `option()` field applies once the field is present. `is.null(x)` tests
#' presence and is not a read: `is.null(a) || !is.null(b)` requires `b`
#' whenever `a` is present.
#'
#' @section Vectors:
#' `all(x, <condition>)` and `any(x, <condition>)` test a condition on each
#' element of the vector `x`, with `.x` the element: `all(values, .x > 0)`.
#' A violation of an invariant that is an `all()` reports each failing
#' element's path. `length()`, `sum()`, `min()`, `max()`,
#' `non_decreasing()`, and `non_increasing()` read a whole vector, without
#' copying it. Paired elements of two vectors are columns of a
#' `type_table()`, whose `rules` apply to each row.
#'
#' @section Constants:
#' A bare name in a rule is always a field. `lit(expr)` is the value of the
#' R expression `expr`, evaluated once when `rule()` runs: `cost <= lit(max_cost)`.
#'
#' @section Operators and functions:
#' Comparisons (`==`, `!=`, `<`, `<=`, `>`, `>=`), arithmetic (`+`, `-`,
#' `*`, `/`, `%%`), logical operators (`&&`, `||`, `!`; `&` and `|` are the
#' same as `&&` and `||`), `%in%`, `if`, `is.null()`, `is_variant()`,
#' `length()`, `nchar()`, `nrow()`, `ncol()`, `dim()`, `levels()`,
#' `abs()`, `floor()`, `ceiling()`, `round()`, `min()`, `max()`, `sum()`,
#' `all()`, `any()`, `subset()`, `non_decreasing()`, `non_increasing()`,
#' `paste0()`, `c()` of constants, and `lit()`.
#'
#' Rules follow the strict semantics of R8's rule language, which is the
#' same in every host: `==` is strict equality, so an integer never equals a
#' string; strings do not order; division by zero is an error; and `==` on a
#' double computed by arithmetic or `sum()` is a declaration error, since
#' rounding makes it inexact: `abs(sum(p) - 1) <= 1e-9` states the
#' tolerance.
#'
#' @section Words that differ from R:
#' The dialect is R syntax with its own meanings; these words differ from
#' the R functions of the same name, or exist in rules only:
#'
#' | Word | In a rule |
#' |:--|:--|
#' | `is_variant(x, "V")` | The variant of the enum field `x` is `V`; R tests a value with `r8_is_variant()`. |
#' | `all(x, cond)`, `any(x, cond)` | `cond` holds for every or some element `.x` of `x`. |
#' | `subset(x, y)` | Every element of the vector `x` is an element of `y`. |
#' | `non_decreasing(x)`, `non_increasing(x)` | The vector `x` is in order. |
#' | `&`, `\|` | The same as `&&` and `\|\|`. |
#' | `==` | Strict equality: values of different types are never equal. |
#' | `lit(expr)` | The value of the R expression `expr`, a constant. |
#'
#' @section Violations:
#' A violation is an `r8_value_error` that shows the rule's ID and message,
#' when it has them, the path, the rule in R, and the values it read: the scalar fields, the
#' length or dimensions of larger fields, computed operands such as
#' `sum(p)`, and the failing elements `.x` of an `all()`. A field declared
#' `field(..., sensitive = TRUE)` shows as `<sensitive>`, and
#' `options(r8.show_values = FALSE)` shows no values. The condition's field
#' `problems` is a data frame of the problems: `code`, `path`, `rule`,
#' `message`, `rule_index` (the rule's position among its type's rules), and
#' the list column `diagnosis`.
#'
#' @param invariant Expression: What must hold.
#' @param when Optional expression: The condition under which the rule
#'   applies.
#' @param message Optional character: What holds, stated for the user, such
#'   as `"lo is below hi."`; without it, a violation shows the invariant.
#' @param id Optional character: The rule's ID, an R8 name such as
#'   `"LO_HI"`, unique among its type's rules, which a violation and
#'   `problems$rule` report.
#'
#' @return An `r8_rule`.
#'
#' @examples
#' op <- options(r8.namespace = "demo")
#' max_cost <- 100
#' Kernel <- new_enum(
#'   "Kernel",
#'   variants = list(
#'     Linear = variant(),
#'     Rbf = variant(fields = list(gamma = type_double(exclusive_min = 0)))
#'   )
#' )
#' Model <- new_struct(
#'   "Model",
#'   fields = list(
#'     cost = type_double(min = 0),
#'     kernel = Kernel,
#'     lo = type_double(),
#'     hi = type_double(),
#'     weights = vec(type_double(min = 0))
#'   ),
#'   rules = list(
#'     rule(lo < hi),
#'     rule(cost <= lit(max_cost), message = "cost is at most max_cost."),
#'     rule(abs(sum(weights) - 1) <= 1e-9, message = "weights sum to 1.", id = "WEIGHTS"),
#'     rule(kernel$gamma <= 10, when = is_variant(kernel, "Rbf"), message = "gamma is at most 10.")
#'   )
#' )
#' Model(cost = 1, kernel = Kernel@Linear, lo = 0, hi = 1, weights = c(0.5, 0.5))
#' try(Model(cost = 1, kernel = Kernel@Linear, lo = 2, hi = 1, weights = 1))
#' options(op)
#'
#' @export
rule <- function(invariant, when = NULL, message = NULL, id = NULL) {
  env <- parent.frame()
  inv <- substitute(invariant)
  if (is.character(inv)) {
    r8_abort(
      "A rule's first argument is its invariant, an R expression; the ID ",
      "and message are named: rule(lo < hi, message = \"lo is below hi.\", ",
      "id = \"LO_HI\").",
      class = "r8_declaration_error"
    )
  }
  if (is.call(inv) && identical(inv[[1L]], as.name("quote"))) {
    inv <- inv[[2L]]
  }
  w <- substitute(when)
  if (is.call(w) && identical(w[[1L]], as.name("quote"))) {
    w <- w[[2L]]
  }
  inv <- rule_expr(inv, env)
  w <- rule_expr(w, env)
  out <- r8_rule(inv, w, message, id)
  if (is.character(out)) {
    r8_abort(out, class = "r8_declaration_error")
  }
  out
} # /R8::rule


# %% lit ----
#' A constant from R in a rule
#'
#' Declaration vocabulary of the rule dialect, without the `r8_` prefix of
#' operations on values. In a rule, `lit(expr)` is the value of the R
#' expression `expr`, evaluated once, when `rule()` runs, in the environment
#' that calls `rule()`; a bare name in a rule is always a field. The value is stored in the rule as a
#' literal: one non-`NA` logical, integer, double, or string, or a vector of
#' them as the right operand of `%in%`. In a package, the value is the one
#' the expression has when the package is installed.
#'
#' Outside a rule, `lit(x)` returns `x`.
#'
#' @param x The value.
#'
#' @return `x`.
#'
#' @examples
#' max_cost <- 100
#' rule(cost <= lit(max_cost), message = "cost is at most max_cost.")
#' allowed <- c("linear", "rbf")
#' rule(kernel %in% lit(allowed), message = "kernel is linear or rbf.")
#'
#' @export
lit <- function(x) x


# %% rule_expr ----
#' Prepare a rule's R expression for translation
#'
#' Replaces each `lit()` call with its value, evaluated in `env`, and signals
#' an error for a one-argument `all()` or `any()`, which writes the
#' two-argument form.
#'
#' @param e The unevaluated expression.
#' @param env Environment: Where `rule()` is called.
#'
#' @return The expression with each `lit()` call replaced by a constant, or a
#'   call to `c()` of constants for a vector.
#'
#' @keywords internal
#' @noRd
rule_expr <- function(e, env) {
  if (!is.call(e)) {
    return(e)
  }
  head <- e[[1L]]
  if (is_rule_fn(head, "lit")) {
    return(lit_value(e, env))
  }
  for (fn in c("all", "any")) {
    if (is_rule_fn(head, fn) && length(e) == 2L) {
      elementwise_error(fn, rule_expr(e[[2L]], env))
    }
  }
  for (i in seq_along(e)[-1L]) {
    if (!is.null(e[[i]])) {
      e[[i]] <- rule_expr(e[[i]], env)
    }
  }
  e
} # /R8::rule_expr


# %% is_rule_fn ----
#' Whether a callee is a function of rules
#'
#' @param head The callee of a call.
#' @param name Character: The function's name, which matches the bare name
#'   and `R8::name`.
#'
#' @return Logical: One value.
#'
#' @keywords internal
#' @noRd
is_rule_fn <- function(head, name) {
  identical(head, as.name(name)) ||
    (is.call(head) &&
      identical(head[[1L]], as.name("::")) &&
      identical(head[[2L]], as.name("R8")) &&
      identical(head[[3L]], as.name(name)))
} # /R8::is_rule_fn


# %% lit_value ----
#' The constant a lit() call gives
#'
#' @param e The `lit()` call.
#' @param env Environment: Where `rule()` is called.
#'
#' @return One constant, or a call to `c()` of constants.
#'
#' @keywords internal
#' @noRd
lit_value <- function(e, env) {
  text <- paste(deparse(e), collapse = " ")
  if (length(e) != 2L || (!is.null(names(e)) && nzchar(names(e)[[2L]]))) {
    r8_abort(
      "`",
      text,
      "`: lit() takes one unnamed argument.",
      class = "r8_declaration_error"
    )
  }
  if (contains_lit(e[[2L]])) {
    r8_abort(
      "`",
      text,
      "`: lit() does not nest.",
      class = "r8_declaration_error"
    )
  }
  v <- eval(e[[2L]], env)
  ok <- (is.logical(v) || is.integer(v) || is.double(v) || is.character(v)) &&
    is.null(attr(v, "class", exact = TRUE)) &&
    length(v) >= 1L &&
    !anyNA(v) &&
    (!is.double(v) || all(is.finite(v)))
  if (!ok) {
    r8_abort(
      "`",
      text,
      "` is not a constant of a rule: one non-NA logical, integer, finite ",
      "double, or string, or a vector of them on the right side of %in%.",
      class = "r8_declaration_error"
    )
  }
  v <- as.vector(v)
  if (length(v) == 1L) {
    return(v)
  }
  as.call(c(as.name("c"), as.list(v)))
} # /R8::lit_value


# %% contains_lit ----
#' Whether an expression calls lit()
#'
#' @param e An expression.
#'
#' @return Logical: One value.
#'
#' @keywords internal
#' @noRd
contains_lit <- function(e) {
  if (!is.call(e)) {
    return(FALSE)
  }
  if (is_rule_fn(e[[1L]], "lit")) {
    return(TRUE)
  }
  any(vapply(as.list(e)[-1L], contains_lit, logical(1)))
} # /R8::contains_lit


# %% rule_paths ----
#' The field paths an expression reads
#'
#' @param e An expression.
#'
#' @return Character: Each path as R writes it (`kernel$gamma`), `.x`
#'   excluded.
#'
#' @keywords internal
#' @noRd
rule_paths <- function(e) {
  if (is.name(e)) {
    n <- as.character(e)
    return(if (identical(n, ".x") || !nzchar(n)) character() else n)
  }
  if (!is.call(e)) {
    return(character())
  }
  if (identical(e[[1L]], as.name("$"))) {
    return(paste(deparse(e), collapse = ""))
  }
  unique(unlist(lapply(as.list(e)[-1L], rule_paths)))
} # /R8::rule_paths


# %% replace_path ----
#' Replace a field path with .x
#'
#' @param e An expression.
#' @param p Character: The path, as `rule_paths()` writes it.
#'
#' @return The expression with each occurrence of `p` replaced by `.x`.
#'
#' @keywords internal
#' @noRd
replace_path <- function(e, p) {
  if (is.name(e) || (is.call(e) && identical(e[[1L]], as.name("$")))) {
    return(
      if (identical(paste(deparse(e), collapse = ""), p)) as.name(".x") else e
    )
  }
  if (is.call(e)) {
    for (i in seq_along(e)[-1L]) {
      if (!is.null(e[[i]])) {
        e[[i]] <- replace_path(e[[i]], p)
      }
    }
  }
  e
} # /R8::replace_path


# %% elementwise_error ----
#' Signal the error of a one-argument all() or any()
#'
#' @param fn Character: `"all"` or `"any"`.
#' @param arg The call's one argument.
#'
#' @return Does not return; signals `r8_declaration_error`, writing the
#'   two-argument form when `arg` reads one field.
#'
#' @keywords internal
#' @noRd
elementwise_error <- function(fn, arg) {
  paths <- rule_paths(arg)
  form <- if (length(paths) == 1L) {
    paste0(
      fn,
      "(",
      paths,
      ", ",
      paste(deparse(replace_path(arg, paths)), collapse = " "),
      ")"
    )
  } else {
    paste0(fn, "(x, <condition on .x>)")
  }
  r8_abort(
    "`",
    fn,
    "()` in a rule takes a vector and a condition on each of its elements, ",
    "`.x`: write `",
    form,
    "`.",
    class = "r8_declaration_error"
  )
} # /R8::elementwise_error


# %% as_rules ----
#' A rules argument as a list of rules
#'
#' @param rules One `rule()`, or a list of them.
#'
#' @return List: The rules.
#'
#' @keywords internal
#' @noRd
as_rules <- function(rules) {
  if (inherits(rules, "r8_rule")) {
    return(list(rules))
  }
  if (!is.list(rules) || is.object(rules)) {
    r8_abort(
      "`rules` is a rule() or a list of rule() calls.",
      class = "r8_declaration_error"
    )
  }
  for (i in seq_along(rules)) {
    if (!inherits(rules[[i]], "r8_rule")) {
      r8_abort(
        "`rules[[",
        i,
        "]]` is not a rule().",
        class = "r8_declaration_error"
      )
    }
  }
  rules
} # /R8::as_rules


# %% r8_is_variant ----
#' Test an enum value's variant
#'
#' `TRUE` when `x` is a value of an R8 enum whose variant is `variant`. A
#' rule tests a field's variant with the rule dialect's `is_variant(kernel,
#' "Rbf")` ([rule()]).
#'
#' @param x An R8 enum value.
#' @param variant Character: The variant name.
#'
#' @return Logical: One value.
#'
#' @examples
#' op <- options(r8.namespace = "demo")
#' Shape <- new_enum("Shape", variants = list(Circle = variant(), Square = variant()))
#' r8_is_variant(Shape@Circle, "Circle")
#' options(op)
#'
#' @export
r8_is_variant <- function(x, variant) {
  if (!inherits(x, "r8_value") || is.null(attr(x, "r8_variant"))) {
    r8_abort("`x` is an R8 enum value.", class = "r8_type_error")
  }
  identical(attr(x, "r8_variant"), variant)
} # /R8::r8_is_variant
