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
    value_abort("Invalid", attr(x, "r8_type"), res[["errors"]])
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


# %% ordering ----
#' The result of a `PartialOrd` impl, checked
#'
#' @param o The impl's result.
#' @param type Character: The values' type, for messages.
#'
#' @return Integer: `-1L`, `0L`, `1L`, or `NA_integer_`; any other result
#'   raises `r8_result_error`.
#'
#' @keywords internal
#' @noRd
ordering <- function(o, type) {
  if (
    (is.integer(o) || is.double(o) || is.logical(o) && is.na(o)) &&
      length(o) == 1L &&
      is.null(attributes(o)) &&
      (is.na(o) || o %in% c(-1, 0, 1))
  ) {
    return(as.integer(o))
  }
  r8_abort(
    "Invalid result of `r8_partial_cmp()` from the PartialOrd impl of `",
    type,
    "`: it returns -1L, 0L, or 1L, or NA when the values are not ordered; found ",
    paste(utils::capture.output(utils::str(o)), collapse = " "),
    " [return_type].",
    class = c("r8_result_error", "r8_type_error")
  )
} # /R8::ordering


# %% Ops.r8_value ----
#' Comparison of R8 values
#'
#' `x == y` and `x != y` compare two R8 values with `r8_equal()`, the
#' `PartialEq` standard trait. `<`, `<=`, `>`, and `>=` use the type's
#' `PartialOrd` impl, and return `FALSE` for values it does not order. Other
#' operators raise an error.
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
    o <- ordering(r8_partial_cmp(e1, e2), attr(e1, "r8_type"))
    if (is.na(o)) {
      return(FALSE)
    }
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


# %% r8_size ----
#' Size of an R8 value
#'
#' The memory an R8 value holds, in bytes. For a value of a Rust-stored type,
#' the bytes its codec reports for the state, plus the size of each field read
#' so far that is not a view of the state; a view's memory is the state's. For
#' an R-stored value, `utils::object.size()`.
#'
#' @param x An R8 value.
#'
#' @return Double: The size in bytes.
#'
#' @examples
#' op <- options(r8.namespace = "demo")
#' Resampler <- new_struct("Resampler", fields = list(n = type_integer()))
#' r8_size(Resampler(n = 10L))
#' options(op)
#'
#' @export
r8_size <- function(x) {
  if (!inherits(x, "r8_value")) {
    r8_abort("`x` is an R8 value.", class = "r8_type_error")
  }
  parts <- r8_native_parts(x)
  if (is.null(parts)) {
    return(as.numeric(utils::object.size(x)))
  }
  fields <- vapply(
    parts[["fields"]],
    function(f) as.numeric(utils::object.size(f)),
    numeric(1)
  )
  parts[["state"]] + sum(fields)
} # /R8::r8_size
