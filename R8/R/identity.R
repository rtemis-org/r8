# identity.R
# ::R8::
# 2026- EDG rtemis.org

# %% r8_hash ----
#' Content hash of an R8 value
#'
#' The SHA-256 of the RFC 8785 canonical JSON of the type's name, its
#' declaration hash, and the value, written `sha256:<hex>`. Values that differ
#' only in member order or in defaults written out hash the same. A matrix,
#' an array, a sparse matrix, a table, and a `vec()` of logical, integer,
#' double, date, or date-time values enter through the digest of their
#' canonical binary form. A runtime-only field's value stays out of the hash;
#' its presence enters it. The hash is computed from the R objects in place.
#'
#' @param x An R8 value.
#'
#' @return Character: The hash.
#'
#' @examples
#' op <- options(r8.namespace = "demo")
#' Point <- new_struct("Point", fields = list(x = type_double(), y = type_double()))
#' r8_hash(Point(x = 1, y = 2))
#' options(op)
#'
#' @export
r8_hash <- function(x) {
  if (!inherits(x, "r8_value")) {
    r8_abort("`x` is an R8 value.", class = "r8_type_error")
  }
  res <- r8_value_hash(x)
  if (is.list(res)) {
    r8_abort(
      "Invalid `",
      attr(x, "r8_type"),
      "`:\n",
      problems_message(res[["errors"]]),
      class = "r8_value_error"
    )
  }
  res
} # /R8::r8_hash


# %% r8_equal ----
#' Equality of two R8 values
#'
#' `TRUE` when both values have the same type and equal fields: their
#' normalized forms are the same canonical JSON, and their runtime-only
#' fields are `identical()`. A provenance record does not enter the
#' comparison.
#'
#' @param x,y R8 values.
#'
#' @return Logical: One value.
#'
#' @examples
#' op <- options(r8.namespace = "demo")
#' Point <- new_struct("Point", fields = list(x = type_double(), y = type_double()))
#' r8_equal(Point(x = 1, y = 2), Point(x = 1L, y = 2))
#' options(op)
#'
#' @export
r8_equal <- function(x, y) {
  if (!inherits(x, "r8_value") || !inherits(y, "r8_value")) {
    r8_abort("`x` and `y` are R8 values.", class = "r8_type_error")
  }
  r8_values_equal(x, y)
} # /R8::r8_equal


# %% Ops.r8_value ----
#' Comparison of R8 values
#'
#' `x == y` and `x != y` compare two R8 values with `r8_equal()`, the
#' `PartialEq` standard trait. `<`, `<=`, `>`, and `>=` use the type's
#' `PartialOrd` impl. Other operators raise an error.
#'
#' @param e1,e2 R8 values.
#'
#' @return Logical: One value.
#'
#' @export
Ops.r8_value <- function(e1, e2) {
  # R binds .Generic in the frame of a group generic's method.
  op <- .Generic # nolint: object_usage_linter.
  if (op %in% c("==", "!=")) {
    eq <- r8_equal(e1, e2)
    return(if (op == "==") eq else !eq)
  }
  if (op %in% c("<", "<=", ">", ">=")) {
    if (!identical(attr(e1, "r8_type"), attr(e2, "r8_type"))) {
      r8_abort(
        "`",
        op,
        "` compares two values of one type.",
        class = "r8_dispatch_error"
      )
    }
    o <- r8_cmp(e1, e2)
    return(switch(
      op,
      "<" = o < 0,
      "<=" = o <= 0,
      ">" = o > 0,
      ">=" = o >= 0
    ))
  }
  r8_abort(
    "`",
    op,
    "` is not defined for R8 values; `==` and `!=` compare them, and a type that implements PartialOrd orders them.",
    class = "r8_dispatch_error"
  )
} # /R8::Ops.r8_value
