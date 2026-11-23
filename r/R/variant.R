# variant.R
# ::R8::
# 2026- EDG rtemis.org

# The native symbols of the C routines `variant` and `is_variant`
# (`r/src/view.c`), which read an enum value's variant, bound when the package
# loads.
.r8_c_variant <- NULL
.r8_c_is_variant <- NULL


# %% r8_variant ----
#' The name of an enum value's variant
#'
#' The variant of an R8 enum value, to branch on with `switch()` or to show.
#' A struct value has no variant.
#'
#' A `switch()` without a last, unnamed arm returns `NULL` for a name that it
#' does not list; an arm that stops reports it, as in the example.
#'
#' @param x An R8 enum value.
#'
#' @return Character: The variant's name, one value.
#'
#' @seealso [r8_is_variant()], which tests for one variant.
#'
#' @examples
#' op <- options(r8.namespace = "demo")
#' Shape <- new_enum(
#'   "Shape",
#'   variants = list(
#'     Circle = variant(fields = list(r = type_double(min = 0))),
#'     Square = variant(fields = list(side = type_double(min = 0)))
#'   )
#' )
#' area <- function(x) {
#'   switch(
#'     r8_variant(x),
#'     Circle = pi * x$r^2,
#'     Square = x$side^2,
#'     stop("unsupported shape: ", r8_variant(x))
#'   )
#' }
#' area(Shape@Square(side = 2))
#' r8_variant(Shape@Circle(r = 1)) %in% c("Circle", "Square")
#' options(op)
#'
#' @export
r8_variant <- function(x) {
  .Call(.r8_c_variant, x)
} # /R8::r8_variant


# %% r8_is_variant ----
#' Test an enum value's variant
#'
#' `TRUE` when `x` is a value of an R8 enum whose variant is `variant`, a
#' variant that the enum declares. A rule tests a field's variant with the
#' rule dialect's `is_variant(kernel, "Rbf")` ([rule()]).
#'
#' The name is checked against the enum's declaration, so a misspelled name is
#' an error; for a value whose type is not registered in the session, the
#' names are compared only.
#'
#' @param x An R8 enum value.
#' @param variant Character: The variant's name, one value.
#'
#' @return Logical: One value.
#'
#' @seealso [r8_variant()], the variant's name.
#'
#' @examples
#' op <- options(r8.namespace = "demo")
#' Shape <- new_enum("Shape", variants = list(Circle = variant(), Square = variant()))
#' r8_is_variant(Shape@Circle, "Circle")
#' try(r8_is_variant(Shape@Circle, "Circel"))
#' options(op)
#'
#' @export
r8_is_variant <- function(x, variant) {
  .Call(.r8_c_is_variant, x, variant, .r8_variants)
} # /R8::r8_is_variant


# %% other_variant ----
#' `FALSE` for a variant that the value's enum declares
#'
#' The C routine `is_variant` calls it for a variant that `.r8_variants` does
#' not hold.
#'
#' @param x An object.
#' @param variant The variant that `x` is not of.
#'
#' @return `FALSE`; signals `r8_type_error` when `x` is not an enum value or
#'   `variant` is not one string, and `r8_value_error` for a name that the
#'   enum does not declare.
#'
#' @keywords internal
#' @noRd
other_variant <- function(x, variant) {
  r8_variant(x)
  if (!is.character(variant) || length(variant) != 1L || is.na(variant)) {
    r8_abort(
      "`variant` is the name of one variant; ",
      "`r8_variant(x) %in% c(...)` tests for several.",
      class = "r8_type_error"
    )
  }
  qname <- attr(x, "r8_type")
  declared <- .r8_variants[[qname]]
  if (is.null(declared)) {
    if (!r8_is_registered(qname)) {
      return(FALSE)
    }
    declared <- enum_variants(qname, r8_type_ir(qname))
  }
  if (!variant %in% names(declared)) {
    unknown_variant(variant, qname)
  }
  FALSE
} # /R8::other_variant


# %% variant_abort ----
#' Signal that an object is not an enum value
#'
#' @param x The object.
#' @param ... Not used.
#'
#' @return Does not return; signals `r8_type_error`.
#'
#' @keywords internal
#' @noRd
variant_abort <- function(x, ...) {
  r8_abort(
    "`x` is an R8 enum value",
    if (inherits(x, "r8_value")) {
      paste0("; `", attr(x, "r8_type"), "` is a struct")
    },
    ".",
    class = "r8_type_error"
  )
} # /R8::variant_abort


# %% unknown_variant ----
#' Signal that an enum declares no variant of a name
#'
#' @param name Character: The name.
#' @param qname Character: The qualified name of a registered enum.
#'
#' @return Does not return; signals `r8_value_error`.
#'
#' @keywords internal
#' @noRd
unknown_variant <- function(name, qname) {
  variants <- vapply(
    r8_type_ir(qname)[["variants"]],
    function(v) v[["name"]],
    ""
  )
  near <- r8_nearest_name(name, variants)
  r8_abort(
    "`",
    name,
    "` is not a variant of `",
    qname,
    "`",
    if (length(near) == 1L) paste0("; the nearest variant is `", near, "`"),
    ". Variants: ",
    paste(variants, collapse = ", "),
    ".",
    class = "r8_value_error"
  )
} # /R8::unknown_variant
