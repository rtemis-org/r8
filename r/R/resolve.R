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


# %% r8resolve ----
#' Resolve a value's pending fields
#'
#' Fills the absent fields of type `option()` that a struct declares with a
#' fill policy: a field of `field(option(T), context = TRUE)` from
#' `context`, and a field of `field(option(T), compute = calc(expr))` from its
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
#'   g = field(option(type_double()), compute = calc(mg / 1000), unit = "g")
#' ))
#' r8resolve(Dose(), context = list(mg = 250))$g
#' options(op)
#'
#' @export
r8resolve <- function(x, context = list()) {
  if (!is_value(x) || !is.null(value_variant(x))) {
    r8_abort("`x` is an R8 struct value.", class = "r8_type_error")
  }
  if (!is.list(context) || (length(context) > 0L && is.null(names(context)))) {
    r8_abort(
      "`context` is a named list of context field values.",
      class = "r8_type_error"
    )
  }
  require_current(x)
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
      value_type(x),
      "`.",
      class = "r8_value_error"
    )
  }
  fields <- unclass(x)
  attributes(fields) <- list(names = names(fields))
  for (n in names(context)) {
    fields[n] <- list(context[[n]])
  }
  res <- r8_resolve_value(fields, value_type(x))
  if (!is.null(res[["errors"]])) {
    value_abort("Cannot resolve", value_type(x), res[["errors"]])
  }
  res[["value"]]
} # /R8::r8resolve
