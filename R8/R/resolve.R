# resolve.R
# ::R8::
# 2026- EDG rtemis.org

# %% pending_fields ----
#' The absent fields of a struct value that resolution fills
#'
#' @param x An R8 struct value.
#'
#' @return Named character: The policy (`"runtime"` or `"computed"`) of each
#'   absent field, named by the field.
#'
#' @keywords internal
#' @noRd
pending_fields <- function(x) {
  type <- .r8_types[[attr(x, "r8_type")]]
  ir <- attr(type, "r8_ir")
  if (!identical(ir[["kind"]], "struct")) {
    return(character())
  }
  out <- character()
  for (f in ir[["fields"]]) {
    policy <- f[["type"]][["resolve"]][["policy"]]
    if (!is.null(policy) && is.null(unclass(x)[[f[["name"]]]])) {
      out[[f[["name"]]]] <- policy
    }
  }
  out
} # /R8::pending_fields


# %% r8_resolve ----
#' Resolve a value's pending fields
#'
#' Fills the absent fields that a type declares with `option(T, resolve = )`:
#' a `"runtime"` field from `context`, a `"computed"` field from its
#' expression over the other fields, in dependency order. The result is a new
#' value, validated; `x` is unchanged. A runtime field missing from `context`
#' stays absent.
#'
#' @param x An R8 struct value.
#' @param context Named list: Values of runtime fields, by name.
#'
#' @return The resolved R8 value.
#'
#' @examples
#' op <- options(r8.namespace = "demo")
#' Dose <- new_struct("Dose", fields = list(
#'   mg = field(option(type_double(), resolve = "runtime"), unit = "mg"),
#'   g = field(option(type_double(), resolve = "computed", compute = mg / 1000), unit = "g")
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
      "`context` is a named list of runtime field values.",
      class = "r8_type_error"
    )
  }
  pending <- pending_fields(x)
  unknown <- setdiff(names(context), names(pending[pending == "runtime"]))
  if (length(unknown) > 0L) {
    r8_abort(
      "`context` names ",
      paste0("`", unknown, "`", collapse = ", "),
      ", which ",
      if (length(unknown) == 1L) {
        "is not an absent runtime field"
      } else {
        "are not absent runtime fields"
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
    r8_abort(
      "Cannot resolve `",
      attr(x, "r8_type"),
      "`:\n",
      problems_message(res[["errors"]]),
      class = "r8_value_error"
    )
  }
  res[["value"]]
} # /R8::r8_resolve


# %% r8_validate ----
#' Validate an R8 value
#'
#' Checks a value against its type again, rules included: a value built by
#' its constructor is valid, and an object reassembled from parts may not
#' be. With `complete = TRUE`, a value whose runtime or computed fields are
#' still absent is an error that names them.
#'
#' @param x An R8 value.
#' @param complete Logical: Require every runtime and computed field to be
#'   present.
#'
#' @return `x`, invisibly.
#'
#' @examples
#' op <- options(r8.namespace = "demo")
#' Fit <- new_struct("FitSettings", fields = list(
#'   mtry = option(type_integer(min = 1L), resolve = "runtime")
#' ))
#' r8_validate(Fit())
#' try(r8_validate(Fit(), complete = TRUE))
#' options(op)
#'
#' @export
r8_validate <- function(x, complete = FALSE) {
  value_identity(x)
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
