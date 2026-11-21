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
#' A rule states what must hold for a value. The invariant is an R
#' expression over the type's fields, captured unevaluated: `cost <= 100`,
#' `kernel$gamma <= 10`, `is.null(a) || !is.null(b)`. R's `==` and `!=` are
#' strict equality. A rule holds when its `when` condition is `FALSE`, or when
#' a field it reads is absent; `is.null()` tests presence and is not a read.
#' Inside `all()` and `any()`, `.x` is the element. `json_ir()` gives an
#' invariant or a condition as JSONLogic text, for JSONLogic without an R
#' form.
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
  inv <- substitute(invariant)
  if (is.call(inv) && identical(inv[[1L]], as.name("quote"))) {
    inv <- inv[[2L]]
  }
  w <- substitute(when)
  if (is.call(w) && identical(w[[1L]], as.name("quote"))) {
    w <- w[[2L]]
  }
  out <- r8_rule(id, inv, message, w)
  if (is.character(out)) {
    r8_abort(out, class = "r8_declaration_error")
  }
  out
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
