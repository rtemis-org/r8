# resolve.R
# ::R8::
# 2026- EDG rtemis.org

# %% pending_fields ----
#' The absent fields of a struct value that resolution fills
#'
#' @param x An R8 struct value.
#'
#' @return Named character: The policy (`"context"` or `"computed"`) of each
#'   absent field, named by the field.
#'
#' @keywords internal
#' @noRd
pending_fields <- function(x) {
  r8_pending_fields(x)
} # /R8::pending_fields


# %% r8_resolve ----
#' Resolve a value's pending fields
#'
#' Fills the absent fields of type `option()` that a struct declares with a
#' fill policy: a field of `field(option(T), context = TRUE)` from
#' `context`, and a field of `field(option(T), compute = expr)` from its
#' expression over the other fields, in dependency order. The result is a new
#' value, validated; `x` is unchanged. A context field missing from
#' `context` stays absent.
#'
#' @param x An R8 struct value.
#' @param context Named list: Values of context fields, by name.
#'
#' @return The resolved R8 value.
#'
#' @examples
#' op <- options(r8.namespace = "demo")
#' Dose <- new_struct("Dose", fields = list(
#'   mg = field(option(type_double()), context = TRUE, unit = "mg"),
#'   g = field(option(type_double()), compute = mg / 1000, unit = "g")
#' ))
#' r8_resolve(Dose(), context = list(mg = 250))$g
#' options(op)
#'
#' @export
r8_resolve <- function(x, context = list()) {
  if (!inherits(x, "r8_value") || !is.null(attr(x, "r8_variant"))) {
    r8_abort("`x` is an R8 struct value.", class = "r8_type_error")
  }
  if (!is.list(context) || (length(context) > 0L && is.null(names(context)))) {
    r8_abort(
      "`context` is a named list of context field values.",
      class = "r8_type_error"
    )
  }
  pending <- pending_fields(x)
  unknown <- setdiff(names(context), names(pending[pending == "context"]))
  if (length(unknown) > 0L) {
    r8_abort(
      "`context` names ",
      paste0("`", unknown, "`", collapse = ", "),
      ", which ",
      if (length(unknown) == 1L) {
        "is not an absent context field"
      } else {
        "are not absent context fields"
      },
      " of `",
      attr(x, "r8_type"),
      "`.",
      class = "r8_value_error"
    )
  }
  fields <- unclass(x)
  attributes(fields) <- list(names = names(fields))
  for (n in names(context)) {
    fields[n] <- list(context[[n]])
  }
  res <- r8_resolve_value(fields, attr(x, "r8_type"))
  if (!is.null(res[["errors"]])) {
    value_abort("Cannot resolve", attr(x, "r8_type"), res[["errors"]])
  }
  res[["value"]]
} # /R8::r8_resolve


# %% r8_validate ----
#' Validate an R8 value
#'
#' Checks a value against its type again, rules included: a value built by
#' its constructor is valid, and an object reassembled from parts may not
#' be. With `complete = TRUE`, a value whose context or computed fields are
#' still absent is an error that names them.
#'
#' @param x An R8 value.
#' @param complete Logical: Require every context and computed field to be
#'   present.
#'
#' @return `x`, invisibly.
#'
#' @examples
#' op <- options(r8.namespace = "demo")
#' Fit <- new_struct("FitSettings", fields = list(
#'   mtry = field(option(type_integer(min = 1L)), context = TRUE)
#' ))
#' r8_validate(Fit())
#' try(r8_validate(Fit(), complete = TRUE))
#' options(op)
#'
#' @export
r8_validate <- function(x, complete = FALSE) {
  if (!inherits(x, "r8_value")) {
    r8_abort("`x` is an R8 value.", class = "r8_type_error")
  }
  res <- r8_value_check(x)
  if (!is.null(res)) {
    value_abort("Invalid", attr(x, "r8_type"), res[["errors"]])
  }
  if (isTRUE(complete)) {
    pending <- pending_fields(x)
    if (length(pending) > 0L) {
      r8_abort(
        "`",
        attr(x, "r8_type"),
        "` has pending fields: ",
        paste0("`", names(pending), "` (", pending, ")", collapse = ", "),
        ". r8_resolve() fills them.",
        class = "r8_value_error"
      )
    }
  }
  invisible(x)
} # /R8::r8_validate
