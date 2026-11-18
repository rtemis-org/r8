# identity.R
# ::R8::
# 2026- EDG rtemis.org

# %% value_identity ----
#' The canonical text, content hash, and runtime-only paths of a value
#'
#' @param x An R8 value.
#'
#' @return List: `canonical`, `hash`, and `runtime`.
#'
#' @keywords internal
#' @noRd
value_identity <- function(x) {
  if (!inherits(x, "r8_value")) {
    r8_abort("`x` is an R8 value.", class = "r8_type_error")
  }
  res <- r8_value_identity(x)
  if (!is.null(res[["errors"]])) {
    r8_abort(
      "Invalid `",
      attr(x, "r8_type"),
      "`:\n",
      problems_message(res[["errors"]]),
      class = "r8_value_error"
    )
  }
  res
} # /R8::value_identity


# %% r8_hash ----
#' Content hash of an R8 value
#'
#' The SHA-256 of the RFC 8785 canonical JSON of the type's name, its
#' declaration hash, and the value, written `sha256:<hex>`. Values that differ
#' only in member order or in defaults written out hash the same. A matrix,
#' sparse matrix, or table enters through the digest of its canonical binary
#' form. A runtime-only field's value stays out of the hash; its presence
#' enters it.
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
  value_identity(x)[["hash"]]
} # /R8::r8_hash


# %% path_get ----
#' The element of a value at a JSON Pointer
#'
#' @param x An R8 value or a list.
#' @param path Character: A JSON Pointer of field names and positions.
#'
#' @return The element.
#'
#' @keywords internal
#' @noRd
path_get <- function(x, path) {
  parts <- strsplit(sub("^/", "", path), "/", fixed = TRUE)[[1L]]
  for (p in parts) {
    p <- gsub("~1", "/", gsub("~0", "~", p, fixed = TRUE), fixed = TRUE)
    x <- unclass(x)
    x <- if (grepl("^[0-9]+$", p) && is.null(names(x))) {
      x[[as.integer(p) + 1L]]
    } else {
      x[[p]]
    }
  }
  x
} # /R8::path_get


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
  if (!identical(attr(x, "r8_type"), attr(y, "r8_type"))) {
    return(FALSE)
  }
  a <- value_identity(x)
  b <- value_identity(y)
  if (
    !identical(a[["canonical"]], b[["canonical"]]) ||
      !identical(sort(a[["runtime"]]), sort(b[["runtime"]]))
  ) {
    return(FALSE)
  }
  all(vapply(
    a[["runtime"]],
    function(p) identical(path_get(x, p), path_get(y, p)),
    logical(1)
  ))
} # /R8::r8_equal


# %% Ops.r8_value ----
#' Comparison of R8 values
#'
#' `x == y` and `x != y` compare two R8 values with `r8_equal()`, the
#' `PartialEq` standard trait; other operators raise an error.
#'
#' @param e1,e2 R8 values.
#'
#' @return Logical: One value.
#'
#' @export
Ops.r8_value <- function(e1, e2) {
  switch(
    .Generic,
    "==" = r8_equal(e1, e2),
    "!=" = !r8_equal(e1, e2),
    r8_abort(
      "`",
      .Generic,
      "` is not defined for R8 values; `==` and `!=` compare them.",
      class = "r8_dispatch_error"
    )
  )
} # /R8::Ops.r8_value
