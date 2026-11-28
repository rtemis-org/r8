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
#' @section Rows:
#' A rule of a [new_table_type()] or a [type_table()] reads the cells of one row by the
#' names of the columns; the cell of a factor is its label. A rule also chooses the
#' rows of a table ([r8filter()]), and then `mean()`, `sum()`, `median()`, `min()`,
#' `max()`, `n()`, and `count()` read whole columns, as in [calc()]. `pmin()` and
#' `pmax()` are of the cells of a row.
#'
#' @section Missing values:
#' A comparison with a missing cell or with `NaN` is missing, and so is an operation
#' with one, except `is.na()`; `FALSE && NA` is false and `TRUE || NA` is true, as in
#' R. `x %in% c(1, 2)` is missing where `x` is, where R gives `FALSE`. A check fails where the rule is false, and a filter keeps the rows where it is
#' true.
#'
#' @section Vectors:
#' `all(x, <condition>)` and `any(x, <condition>)` test a condition on each
#' element of the vector `x`, with `.x` the element: `all(values, .x > 0)`.
#' A violation of an invariant that is an `all()` reports each failing
#' element's path. `length()`, `sum()`, `mean()`, `min()`, `max()`,
#' `non_decreasing()`, and `non_increasing()` read a whole vector, without
#' copying it. Paired elements of two vectors are columns of a
#' `type_table()`, whose `rules` apply to each row.
#'
#' @section Constants:
#' A bare name in a rule is always a field. `val(expr)` is the value of the
#' R expression `expr`, evaluated once when `rule()` runs: `cost <= val(max_cost)`.
#' `col(expr)` is the field or column whose name the R expression `expr` gives:
#' `col(m) > 0` with `m <- "auc"` is `auc > 0`. The arguments of `val()` and `col()`
#' are R; every other bare name is a field or a column. `NA` is the missing
#' constant, as a value of `if`; a comparison with `NA` is an error that names
#' `is.na()`.
#'
#' @section Operators and functions:
#' Comparisons (`==`, `!=`, `<`, `<=`, `>`, `>=`), arithmetic (`+`, `-`,
#' `*`, `/`, `%%`, `^`), logical operators (`&&`, `||`, `!`; `&` and `|` are the
#' same as `&&` and `||`), `%in%`, `if`, `is.na()`, `is.null()`, `is_variant()`,
#' `length()`, `nchar()`, `nrow()`, `ncol()`, `dim()`, `levels()`,
#' `abs()`, `sqrt()`, `log()`, `exp()`, `floor()`, `ceiling()`, `round()`, `min()`,
#' `max()`, `pmin()`, `pmax()`, `sum()`, `mean()`, `median()`, `n()`, `count()`,
#' `all()`, `any()`, `subset()`, `non_decreasing()`, `non_increasing()`,
#' `paste0()`, `c()` of constants, `NA`, `val()`, and `col()`. `na.rm = TRUE` in
#' `sum()`, `mean()`, `median()`, `min()`, `max()`, and `count()` leaves out the
#' missing cells of a table's column.
#'
#' Rules follow the strict semantics of R8's rule language, which is the
#' same in every host: `==` is strict equality, so an integer never equals a
#' string; strings do not order; a string compared with a date or a date-time is
#' one, `"2020-01-31"` or `"2020-01-31T12:00:00Z"`; a division by zero, `sqrt()` of
#' a negative number, `log()` of a number that is not positive, and a negative
#' number to a power that is not a whole number are errors; `sum()` is the exact
#' sum, rounded once, and `mean()` that sum over the count; and
#' `==` on a double computed by arithmetic, `sum()`, or `mean()` is a declaration
#' error, since rounding makes it inexact: `abs(sum(p) - 1) <= 1e-9` states the
#' tolerance.
#'
#' @section Rules of rules:
#' A rule is a value. `!`, `&`, and `|` make a rule of others, `!adult & woman`,
#' without an ID and a message.
#'
#' @section Package code:
#' `R CMD check` takes a bare name in a function for a variable that nothing binds.
#' A function of a package quotes the condition, `rule(quote(age >= val(cutoff)))`,
#' which is the same rule.
#'
#' @section Words that differ from R:
#' The dialect is R syntax with its own meanings; these words differ from
#' the R functions of the same name, or exist in rules only:
#'
#' | Word | In a rule |
#' |:--|:--|
#' | `is_variant(x, "V")` | The variant of the enum field `x` is `V`; R tests a value with `r8is_variant()`. |
#' | `all(x, cond)`, `any(x, cond)` | `cond` holds for every or some element `.x` of `x`. |
#' | `subset(x, y)` | Every element of the vector `x` is an element of `y`. |
#' | `non_decreasing(x)`, `non_increasing(x)` | The vector `x` is in order. |
#' | `&`, `\|` | The same as `&&` and `\|\|`. |
#' | `==` | Strict equality: values of different types are never equal. |
#' | `val(expr)` | The value of the R expression `expr`, a constant. |
#' | `col(expr)` | The field or column whose name the R expression `expr` gives; `col()` is the column that [r8map()] maps. |
#' | `n()`, `count(cond)` | The rows of a table or of a group, and those where `cond` is true. |
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
#'   `problems$rule` report. In a list of rules, the name of a rule is its ID:
#'   `list(order = rule(lo <= hi))`.
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
#'     rule(cost <= val(max_cost), message = "cost is at most max_cost."),
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


# %% Ops.r8_rule ----
#' Rules of other rules
#'
#' @param e1,e2 Rules.
#'
#' @return For `!`, `&`, and `|`, the rule of the conditions, without an ID and a
#'   message.
#'
#' @noRd
#' @export
Ops.r8_rule <- function(e1, e2) {
  fn <- .Generic
  if (!fn %in% c("!", "&", "|")) {
    r8_abort(
      "`",
      fn,
      "` is not an operation on rules; !, &, and | make a rule of others.",
      class = "r8_type_error"
    )
  }
  given <- if (missing(e2)) list(e1) else list(e1, e2)
  for (r in given) {
    if (!inherits(r, "r8_rule")) {
      r8_abort(
        "`",
        fn,
        "` makes a rule of two rules; a condition is a rule(), such as rule(age >= 18).",
        class = "r8_type_error"
      )
    }
    if (!is.null(r[["when"]])) {
      r8_abort(
        "`",
        fn,
        "` takes rules without `when`, which are one condition.",
        class = "r8_type_error"
      )
    }
  }
  parts <- lapply(unname(given), `[[`, "invariant")
  op <- c(`!` = "!", `&` = "and", `|` = "or")[[fn]]
  structure(
    list(invariant = stats::setNames(list(parts), op)),
    class = "r8_rule"
  )
} # /R8::Ops.r8_rule


# %% val ----
#' A constant from R in a rule
#'
#' Declaration vocabulary of the rule dialect, without the `r8_` prefix of
#' operations on values. In a rule, `val(expr)` is the value of the R
#' expression `expr`, evaluated once, when `rule()` runs, in the environment
#' that calls `rule()`; a bare name in a rule is a field or a column. The value is
#' stored in the rule as a literal: one non-`NA` logical, integer, double, string,
#' `Date`, or `POSIXct`, or a vector of them as the right operand of `%in%`. In a
#' declaration of a package, the value is the one the expression has when the
#' package is installed.
#'
#' Outside a rule, `val(x)` returns `x`.
#'
#' @param x The value.
#'
#' @return `x`.
#'
#' @examples
#' max_cost <- 100
#' rule(cost <= val(max_cost), message = "cost is at most max_cost.")
#' allowed <- c("linear", "rbf")
#' rule(kernel %in% val(allowed), message = "kernel is linear or rbf.")
#'
#' @export
val <- function(x) x


# %% rule_expr ----
#' Prepare a rule's R expression for translation
#'
#' Replaces each `val()` call with its value, evaluated in `env`, and signals
#' an error for a one-argument `all()` or `any()`, which writes the
#' two-argument form. With `calls`, a call of a name that is no word of the
#' dialect is a typed function, found in `env` and recorded in `calls` by its
#' qualified name, whose arguments `match.call()` matches to its parameters:
#' `.r8_call("ns::f", x = expr, trim = 0.2)`.
#'
#' @param e The unevaluated expression.
#' @param env Environment: Where `rule()` or `calc()` is called.
#' @param calls Optional environment: The typed functions that the expression
#'   calls, for `calc()`; `NULL` refuses them.
#'
#' @return The expression with each `val()` call replaced by a constant, or a
#'   call to `c()` of constants for a vector.
#'
#' @keywords internal
#' @noRd
rule_expr <- function(e, env, calls = NULL) {
  if (!is.call(e)) {
    return(e)
  }
  head <- e[[1L]]
  if (is_rule_fn(head, "val")) {
    return(val_value(e, env))
  }
  if (is_rule_fn(head, "col") && length(e) > 1L) {
    return(col_name(e, env))
  }
  for (fn in c("all", "any")) {
    if (is_rule_fn(head, fn) && length(e) == 2L) {
      elementwise_error(fn, rule_expr(e[[2L]], env, calls))
    }
  }
  word <- if (is.name(head)) {
    !is.null(.r8_rule_words[[as.character(head)]])
  } else {
    !(is.call(head) &&
      length(head) == 3L &&
      identical(head[[1L]], as.name("::")) &&
      !as.character(head[[2L]]) %in% c("R8", "base"))
  }
  if (!word) {
    f <- callee_function(head, env)
    if (inherits(f, "r8_function")) {
      return(typed_call(e, f, env, calls))
    }
    if (!is.null(calls) && is.function(f)) {
      untyped_call(e)
    }
  }
  for (i in seq_along(e)[-1L]) {
    if (!is.null(e[[i]])) {
      e[[i]] <- rule_expr(e[[i]], env, calls)
    }
  }
  e
} # /R8::rule_expr


# %% rule_word_names ----
#' The functions of the rule dialect, as R writes them
#'
#' A callee of one of these names, `R8::name` or `base::name`, or a callee that
#' is no name, is read or refused by the translation of a rule; another name,
#' and `pkg::name` of another package, may name a typed function.
#'
#' @return Character: The names.
#'
#' @keywords internal
#' @noRd
rule_word_names <- function() {
  c(
    names(r8_rule_operators()),
    "int64",
    "json_ir",
    "(",
    "{",
    "c",
    "if",
    "[[",
    "[",
    "$",
    "val",
    "col",
    "quote",
    ".r8_call"
  )
} # /R8::rule_word_names


# %% callee_function ----
#' The function that a callee names
#'
#' @param head The callee: a name, or `pkg::name`.
#' @param env Environment: Where `rule()` or `calc()` is called.
#'
#' @return The function, or `NULL` where the callee names none.
#'
#' @keywords internal
#' @noRd
callee_function <- function(head, env) {
  if (is.name(head)) {
    return(get0(as.character(head), envir = env, mode = "function"))
  }
  f <- tryCatch(eval(head, env), error = function(cnd) NULL)
  if (is.function(f)) f else NULL
} # /R8::callee_function


# %% untyped_call ----
#' Refuse a call of an R function in an expression of calc()
#'
#' @param e The call, whose callee is no word of the dialect and an R
#'   function that is not a typed function.
#'
#' @return Does not return.
#'
#' @keywords internal
#' @noRd
untyped_call <- function(e) {
  name <- paste(deparse(e[[1L]]), collapse = "")
  r8_abort(
    "`",
    paste(deparse(e), collapse = " "),
    "`: `",
    name,
    "()` is an R function; calc() calls the functions of the dialect and ",
    "typed functions of new_function(), such as ",
    "new_function(\"f\", function(x = arg_vec(type_double())) ..., ",
    "return_type = type_double()).",
    class = "r8_declaration_error"
  )
} # /R8::untyped_call


# %% typed_call ----
#' The call of a typed function in an expression of calc()
#'
#' @param e The call.
#' @param f Function: The typed function that it calls.
#' @param env Environment: Where `calc()` is called.
#' @param calls Optional environment: The typed functions of the expression;
#'   `NULL` refuses the call, as `rule()` does.
#'
#' @return `.r8_call(qname, ...)`, with the arguments that `match.call()`
#'   matches to the parameters of `f`, each prepared by `rule_expr()`.
#'
#' @keywords internal
#' @noRd
typed_call <- function(e, f, env, calls) {
  qname <- attr(f, "r8_function", exact = TRUE)
  text <- paste(deparse(e), collapse = " ")
  if (is.null(calls)) {
    r8_abort(
      "`",
      text,
      "`: `",
      qname,
      "()` is a typed function, which a rule does not call; an expression ",
      "of calc() in r8set(), r8summarize(), or r8map() calls it.",
      class = "r8_declaration_error"
    )
  }
  matched <- tryCatch(
    match.call(f, e, expand.dots = FALSE),
    error = function(cnd) {
      r8_abort(
        "`",
        text,
        "`: ",
        conditionMessage(cnd),
        class = "r8_declaration_error"
      )
    }
  )
  args <- as.list(matched)[-1L]
  if ("..." %in% names(args)) {
    r8_abort(
      "`",
      text,
      "`: `",
      qname,
      "()` takes `...`; an expression of calc() calls a function of named ",
      "parameters.",
      class = "r8_declaration_error"
    )
  }
  args <- lapply(args, rule_expr, env = env, calls = calls)
  if (is.null(r8_function_ir(qname)) && !is.null(attr(f, "r8_record"))) {
    register_records(list(attr(f, "r8_record")))
  }
  assign(qname, f, envir = calls)
  as.call(c(list(as.name(".r8_call"), qname), args))
} # /R8::typed_call


# %% col_name ----
#' The column a col() call names
#'
#' @param e The `col(expr)` call.
#' @param env Environment: Where `rule()` or `calc()` is called.
#'
#' @return A symbol: The column whose name `expr` gives.
#'
#' @keywords internal
#' @noRd
col_name <- function(e, env) {
  text <- paste(deparse(e), collapse = " ")
  if (length(e) != 2L || (!is.null(names(e)) && nzchar(names(e)[[2L]]))) {
    r8_abort(
      "`",
      text,
      "`: col() takes one unnamed argument, an R expression that gives a ",
      "column's name.",
      class = "r8_declaration_error"
    )
  }
  arg <- e[[2L]]
  v <- tryCatch(eval(arg, env), error = function(cnd) cnd)
  if (!is.character(v) || length(v) != 1L || is.na(v) || !nzchar(v)) {
    named <- if (is.name(arg)) as.character(arg) else NULL
    r8_abort(
      "`",
      text,
      "`: the argument of col() is an R expression that gives one column ",
      "name, such as col(m) with m <- \"auc\"",
      if (!is.null(named)) {
        paste0(
          "; the column `",
          named,
          "` itself is `",
          named,
          "`, or col(\"",
          named,
          "\")"
        )
      },
      ".",
      class = "r8_declaration_error"
    )
  }
  as.name(v)
} # /R8::col_name


# %% calc ----
#' Declare an expression
#'
#' An expression of the rule dialect as a value, as [rule()] is a condition:
#' captured unevaluated, compiled against the columns of the table it is given
#' with, and evaluated in Rust. A bare name is a column, `val(expr)` is the
#' value of an R expression, and `col(expr)` is the column whose name an R
#' expression gives. [r8set()] stores the column that an expression computes
#' for every row, [r8summarize()] the value of an expression of aggregates for
#' each group, and [r8map()] one expression over many columns, in which `col()`
#' is each column in turn. `field(option(T), compute = calc(expr))` declares a
#' field that [r8resolve()] computes from the others.
#'
#' @section Results:
#' A number is a double, except `n()`, `count()`, and a choice among integers
#' (`min()`, `max()`, `pmin()`, `pmax()`, and `if`), which are integers; a
#' logical, a string, a date, and a date-time keep their kinds, and the cell of
#' a factor is its label. A missing operand gives a missing cell, and `NA` is a
#' missing constant: `if (d == 0) NA else n / d`. An error in a row, such as a
#' division by zero, is an `r8_value_error` that counts the rows. The unit of a
#' result is the one the compiler infers: `sum()`, `mean()`, `median()`,
#' `min()`, `max()`, `+`, `-`, `abs()`, `round()`, `pmin()`, `pmax()`, and `if`
#' keep the common unit of their operands; `*`, `/`, `^`, `sqrt()`, `log()`,
#' `exp()`, `n()`, and `count()` give none.
#'
#' @section Aggregates:
#' `n()` is the number of rows, `count(cond)` the rows where `cond` is true,
#' and `sum()`, `mean()`, `median()`, `min()`, and `max()` read whole columns:
#' over all rows, or over the rows of each group with `by`. `sum()` is the exact
#' sum, rounded once; `mean()` that sum over the count; `median()` the middle
#' value, or the mean of the two middle ones. An aggregate over a missing cell
#' is missing; `na.rm = TRUE` leaves the missing cells out. With no cell left,
#' `sum()` is 0 and the others are missing.
#'
#' @section Typed functions:
#' A call of a name that is no function of the dialect calls a typed function
#' of [new_function()], found where `calc()` is called and matched to its
#' parameters as R matches a call: `calc(trimmed_mean(sbp, trim = 0.2))`. The
#' functions of the dialect come first, so a typed function named `mean` is
#' not called by `mean()`. A parameter of `vec(T)` or of a factor takes the
#' cells of its argument in the rows of each group, or of the table without
#' `by`; a scalar parameter takes a constant or `val()`; an omitted parameter
#' takes its constant default, and a default written in R is an error. The
#' function returns one logical, integer, double, string, date, or date-time,
#' or `option()` of one, and its value has no unit. Each call is checked as a
#' direct call: a missing cell for a parameter of `vec(T)` is the call's error,
#' with the group's keys, and a parameter of `vec(option(T))` receives the
#' missing cells. A function whose body is [rust()] is called from R8's Rust
#' code through the body that its package registers, with the cells of each
#' group as an R vector, and its body runs at most once for each call: a call
#' whose arguments the checks refuse runs through the function's closure, which
#' raises the error of a direct call, and an error or a refused result of the
#' body is the error that a direct call raises. An R error or an interrupt
#' inside the body continues as itself. A rule and a computed field call no
#' function.
#'
#' @param expr Expression: The expression, over the columns of a table.
#'
#' @return An `r8_calc`.
#'
#' @examples
#' calc(weight_kg / height_m^2)
#' calc(mean(sbp, na.rm = TRUE))
#' m <- "auc"
#' calc(median(col(m)))
#' calc(median(col()))
#' trimmed_mean <- new_function(
#'   "trimmed_mean",
#'   function(x = arg_vec(type_double()), trim = arg_double(default = 0)) {
#'     mean(x, trim = trim)
#'   },
#'   return_type = type_double()
#' )
#' d <- Raven(g = c("a", "b", "a", "b"), x = c(1, 2, 3, 40))
#' r8summarize(d, list(x = calc(trimmed_mean(x, trim = 0.25))), by = "g")
#'
#' @export
calc <- function(expr) {
  env <- parent.frame()
  e <- substitute(expr)
  if (is.call(e) && identical(e[[1L]], as.name("quote"))) {
    e <- e[[2L]]
  }
  calls <- new.env(parent = emptyenv())
  e <- rule_expr(e, env, calls)
  out <- r8_rule(e, NULL, NULL, NULL)
  if (is.character(out)) {
    r8_abort(out, class = "r8_declaration_error")
  }
  structure(
    list(expr = out[["invariant"]], call = e, functions = as.list(calls)),
    class = "r8_calc"
  )
} # /R8::calc


# %% check_calcs ----
#' Refuse a list of expressions that is not a named list of calc() values
#'
#' @param columns The list.
#' @param fn Character: The function that takes it, for the message.
#'
#' @return `NULL`, invisibly.
#'
#' @keywords internal
#' @noRd
check_calcs <- function(columns, fn) {
  keys <- names(columns) %||% character()
  if (
    !is.list(columns) ||
      is.object(columns) ||
      length(columns) == 0L ||
      length(keys) != length(columns) ||
      anyNA(keys) ||
      !all(nzchar(keys)) ||
      !all(vapply(columns, inherits, NA, "r8_calc"))
  ) {
    r8_abort(
      "`columns` is a named list of calc() values: ",
      fn,
      "(x, list(n = calc(n()), age = calc(mean(age)))).",
      class = "r8_argument_error"
    )
  }
  if (anyDuplicated(keys) > 0L) {
    r8_abort(
      "Two entries of `columns` are named `",
      keys[[anyDuplicated(keys)]],
      "`.",
      class = "r8_argument_error"
    )
  }
  invisible(NULL)
} # /R8::check_calcs


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


# %% val_value ----
#' The constant a val() call gives
#'
#' @param e The `val()` call.
#' @param env Environment: Where `rule()` is called.
#'
#' @return One constant, or a call to `c()` of constants.
#'
#' @keywords internal
#' @noRd
val_value <- function(e, env) {
  text <- paste(deparse(e), collapse = " ")
  if (length(e) != 2L || (!is.null(names(e)) && nzchar(names(e)[[2L]]))) {
    r8_abort(
      "`",
      text,
      "`: val() takes one unnamed argument.",
      class = "r8_declaration_error"
    )
  }
  if (contains_val(e[[2L]])) {
    r8_abort(
      "`",
      text,
      "`: val() does not nest.",
      class = "r8_declaration_error"
    )
  }
  v <- eval(e[[2L]], env)
  if (inherits(v, c("Date", "POSIXct")) && !anyNA(v)) {
    days <- as.double(v)
    if (
      all(is.finite(days)) && (!inherits(v, "Date") || all(days == round(days)))
    ) {
      v <- r8_date_literals(days, inherits(v, "POSIXct"))
    }
  }
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
      "double, string, date, or date-time, or a vector of them on the right ",
      "side of %in%.",
      class = "r8_declaration_error"
    )
  }
  v <- as.vector(v)
  if (length(v) == 1L) {
    return(v)
  }
  as.call(c(as.name("c"), as.list(v)))
} # /R8::val_value


# %% contains_val ----
#' Whether an expression calls val()
#'
#' @param e An expression.
#'
#' @return Logical: One value.
#'
#' @keywords internal
#' @noRd
contains_val <- function(e) {
  if (!is.call(e)) {
    return(FALSE)
  }
  if (is_rule_fn(e[[1L]], "val")) {
    return(TRUE)
  }
  any(vapply(as.list(e)[-1L], contains_val, logical(1)))
} # /R8::contains_val


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
#' A name in the list is the ID of its rule, in a declaration and in a check alike.
#'
#' @param rules One `rule()`, or a list of them.
#' @param arg Character: The argument's name, for messages.
#' @param class Character: The class of an error.
#'
#' @return List: The rules, without names.
#'
#' @keywords internal
#' @noRd
as_rules <- function(
  rules,
  arg = "rules",
  class = "r8_declaration_error"
) {
  if (inherits(rules, "r8_rule")) {
    return(list(rules))
  }
  if (!is.list(rules) || is.object(rules)) {
    r8_abort(
      "`",
      arg,
      "` is a rule() or a list of rule() calls.",
      class = class
    )
  }
  given <- names(rules) %||% rep("", length(rules))
  for (i in seq_along(rules)) {
    if (!inherits(rules[[i]], "r8_rule")) {
      r8_abort("`", arg, "[[", i, "]]` is not a rule().", class = class)
    }
    name <- given[[i]]
    if (is.na(name) || !nzchar(name)) {
      next
    }
    if (!is_name(name)) {
      r8_abort(
        "The name of a rule in `",
        arg,
        "` is its ID, an R8 name",
        name_reason(name),
        "; `message` holds text.",
        class = class
      )
    }
    id <- rules[[i]][["id"]]
    if (!is.null(id) && !identical(id, name)) {
      r8_abort(
        "The rule named `",
        name,
        "` in `",
        arg,
        "` has the ID `",
        id,
        "`; give one of the two.",
        class = class
      )
    }
    rules[[i]][["id"]] <- name
  }
  unname(rules)
} # /R8::as_rules
