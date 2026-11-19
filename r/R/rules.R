# rules.R
# ::R8::
# 2026- EDG rtemis.org

# An R expression compiles to a JSONLogic expression as a list: an operation
# is a named list with one member whose value is the list of operands; a
# field is `list(var = "path")`. `rtemis-r8` checks the result against the
# type when the type is declared.

# Operators of R and the JSONLogic operators they compile to.
R8_OPERATORS <- c(
  "==" = "===",
  "!=" = "!==",
  "<" = "<",
  "<=" = "<=",
  ">" = ">",
  ">=" = ">=",
  "+" = "+",
  "-" = "-",
  "*" = "*",
  "/" = "/",
  "%%" = "%",
  "&&" = "and",
  "&" = "and",
  "||" = "or",
  "|" = "or",
  "!" = "!",
  "%in%" = "in",
  "paste0" = "cat",
  "is.null" = "is_null",
  "is_variant" = "is_variant",
  "length" = "length",
  "nrow" = "nrow",
  "ncol" = "ncol",
  "dim" = "dim",
  "levels" = "levels",
  "floor" = "floor",
  "ceiling" = "ceiling",
  "round" = "round",
  "min" = "min",
  "max" = "max",
  "all" = "all",
  "any" = "any",
  "subset" = "subset",
  "non_increasing" = "non_increasing",
  "non_decreasing" = "non_decreasing"
)


# %% compile_path ----
#' The field path of a `$` chain
#'
#' @param e Language: A symbol or a `$` call.
#'
#' @return Character: The dotted path, or `NULL` when `e` is not a path.
#'
#' @keywords internal
#' @noRd
compile_path <- function(e) {
  if (is.symbol(e)) {
    return(as.character(e))
  }
  if (is.call(e) && identical(e[[1L]], as.name("$"))) {
    head <- compile_path(e[[2L]])
    if (is.null(head)) {
      return(NULL)
    }
    return(paste0(head, ".", as.character(e[[3L]])))
  }
  NULL
} # /R8::compile_path


# %% compile_rule_expr ----
#' Compile an R expression to JSONLogic
#'
#' @param e Language or constant: The expression.
#'
#' @return The JSONLogic expression as a list or a constant.
#'
#' @keywords internal
#' @noRd
compile_rule_expr <- function(e) {
  if (is.null(e)) {
    return(NULL)
  }
  if (is.atomic(e)) {
    if (length(e) != 1L || is.na(e)) {
      r8_abort(
        "A constant in a rule is one value, not NA; write an array as c(...).",
        class = "r8_declaration_error"
      )
    }
    return(e)
  }
  path <- compile_path(e)
  if (!is.null(path)) {
    return(list(var = path))
  }
  if (!is.call(e)) {
    r8_abort(
      "A rule cannot hold ",
      deparse(e),
      ".",
      class = "r8_declaration_error"
    )
  }
  fn <- e[[1L]]
  # `R8::is_variant()` and `base::is.null()` name the same functions as
  # their bare forms.
  if (
    is.call(fn) &&
      identical(fn[[1L]], as.name("::")) &&
      as.character(fn[[2L]]) %in% c("R8", "base")
  ) {
    fn <- fn[[3L]]
  }
  if (!is.symbol(fn)) {
    r8_abort(
      "A rule calls a function by name: ",
      deparse(e),
      ".",
      class = "r8_declaration_error"
    )
  }
  fn <- as.character(fn)
  args <- as.list(e)[-1L]
  if (fn == "(") {
    return(compile_rule_expr(args[[1L]]))
  }
  if (fn == "c") {
    # An array literal.
    values <- lapply(args, function(a) {
      if (!is.atomic(a) || length(a) != 1L || is.na(a)) {
        r8_abort(
          "c() in a rule holds literal values.",
          class = "r8_declaration_error"
        )
      }
      a
    })
    return(values)
  }
  if (fn == "if") {
    operands <- list(
      compile_rule_expr(args[[1L]]),
      compile_rule_expr(args[[2L]])
    )
    if (length(args) > 2L) {
      operands <- c(operands, list(compile_rule_expr(args[[3L]])))
    }
    return(list(`if` = operands))
  }
  if (fn == "is_variant") {
    # The variant is a name, written as a string.
    return(list(is_variant = list(compile_rule_expr(args[[1L]]), args[[2L]])))
  }
  op <- R8_OPERATORS[fn]
  if (is.na(op)) {
    r8_abort(
      "`",
      fn,
      "()` is not a function of R8 rules. Rules use comparisons, arithmetic, ",
      "logical operators, %in%, is.null(), is_variant(), length(), nrow(), ncol(), dim(), ",
      "levels(), floor(), ceiling(), round(), min(), max(), all(), any(), subset(), ",
      "non_increasing(), and non_decreasing().",
      class = "r8_declaration_error"
    )
  }
  operands <- lapply(args, compile_rule_expr)
  out <- list(operands)
  names(out) <- unname(op)
  out
} # /R8::compile_rule_expr


# %% rule ----
#' Declare a rule
#'
#' A rule states what must hold for a value. The invariant is an R
#' expression over the type's fields, captured unevaluated: `cost <= 100`,
#' `kernel$gamma <= 10`, `is.null(a) || !is.null(b)`. R's `==` and `!=` are
#' strict equality. A rule holds when its `when` condition is `FALSE`, or when
#' a field it reads is absent; `is.null()` tests presence and is not a read.
#' Inside `all()` and `any()`, `.x` is the element.
#'
#' @param id Character: The rule's ID, an R8 name such as `"RBF_GAMMA"`.
#' @param invariant Expression: What must hold.
#' @param message Character: What a violation means, for the user.
#' @param when Optional expression: The condition under which the rule
#'   applies.
#'
#' @return An `r8_rule`.
#'
#' @examples
#' rule("COST_MAX", cost <= 100, "cost is at most 100.")
#' rule("RBF_COST", cost <= 100, "cost is at most 100 with the Rbf kernel.",
#'   when = is_variant(kernel, "Rbf"))
#'
#' @export
rule <- function(id, invariant, message, when = NULL) {
  if (!is_name(id)) {
    r8_abort(
      "A rule's `id` is an R8 name, such as \"COST_MAX\".",
      class = "r8_declaration_error"
    )
  }
  if (
    !is.character(message) || length(message) != 1L || !nzchar(trimws(message))
  ) {
    r8_abort(
      "A rule's `message` is one non-empty string.",
      class = "r8_declaration_error"
    )
  }
  inv <- substitute(invariant)
  if (is.call(inv) && identical(inv[[1L]], as.name("quote"))) {
    inv <- inv[[2L]]
  }
  w <- substitute(when)
  if (is.call(w) && identical(w[[1L]], as.name("quote"))) {
    w <- w[[2L]]
  }
  out <- list(id = id, invariant = compile_rule_expr(inv))
  if (!is.null(w)) {
    out[["when"]] <- compile_rule_expr(w)
  }
  out[["message"]] <- message
  structure(out, class = "r8_rule")
} # /R8::rule


# %% is_variant ----
#' Test an enum value's variant
#'
#' `TRUE` when `x` is a value of an R8 enum whose variant is `variant`. In a
#' rule, `is_variant(kernel, "Rbf")` compiles to the same test over the
#' value's JSON form.
#'
#' @param x An R8 enum value.
#' @param variant Character: The variant name.
#'
#' @return Logical: One value.
#'
#' @examples
#' op <- options(r8.namespace = "demo")
#' Shape <- new_enum("Shape", variants = list(Circle = variant(), Square = variant()))
#' is_variant(Shape@Circle, "Circle")
#' options(op)
#'
#' @export
is_variant <- function(x, variant) {
  if (!inherits(x, "r8_value") || is.null(attr(x, "r8_variant"))) {
    r8_abort("`x` is an R8 enum value.", class = "r8_type_error")
  }
  identical(attr(x, "r8_variant"), variant)
} # /R8::is_variant
