# values.R
# ::R8::
# 2026- EDG rtemis.org

# An R8 value in R is a named list of its fields, of classes
# c("ns::Type", "r8_value"), with the attributes `r8_type`, `r8_version`, and,
# for an enum value, `r8_variant`. Values are immutable: `$<-` and `[[<-`
# return a new value, validated.

# %% problems_message ----
#' The message of a list of problems
#'
#' @param errors List: `code`, `path`, `rule`, and `message` vectors.
#'
#' @return Character: One line per problem.
#'
#' @keywords internal
#' @noRd
problems_message <- function(errors) {
  where <- ifelse(
    nzchar(errors[["path"]]),
    paste0(" at ", errors[["path"]]),
    ""
  )
  rule <- ifelse(
    nzchar(errors[["rule"]]),
    paste0(" (rule ", errors[["rule"]], ")"),
    ""
  )
  paste0(
    "- ",
    errors[["message"]],
    rule,
    " [",
    errors[["code"]],
    where,
    "]",
    collapse = "\n"
  )
} # /R8::problems_message


# %% r8_construct ----
#' Construct a value
#'
#' @param type_name Character: The qualified type name.
#' @param fields Named list: The fields.
#' @param variant Character: The enum variant, or `""` for a struct.
#'
#' @return An R8 value.
#'
#' @keywords internal
#' @noRd
r8_construct <- function(type_name, fields, variant = "") {
  res <- r8_new_value(fields, type_name, variant)
  if (inherits(res, "r8_problems")) {
    construct_error(res, type_name, variant)
  }
  res
} # /R8::r8_construct


# %% construct_error ----
#' Signal the problems of a construction
#'
#' @param res The `r8_problems` list that `r8_new_value()` returned.
#' @param type_name Character: The qualified type name.
#' @param variant Character: The enum variant, or `""` for a struct.
#'
#' @return Does not return; signals `r8_value_error`.
#'
#' @keywords internal
#' @noRd
construct_error <- function(res, type_name, variant) {
  label <- if (nzchar(variant)) paste0(type_name, "@", variant) else type_name
  r8_abort(
    "Invalid `",
    label,
    "`:\n",
    problems_message(res[["errors"]]),
    class = c("r8_value_error")
  )
} # /R8::construct_error


# %% constructor_body ----
#' The body of a constructor
#'
#' One `.Call()` with the fields in declaration order; the constructor's
#' environment is R8's namespace, where the native routine is bound.
#'
#' @param qname Character: The qualified type name.
#' @param fields Character: The field names, in declaration order.
#' @param variant Character: The enum variant, or `""` for a struct.
#' @param required Character: The fields without a default argument.
#' @param native Logical: A Rust-native type, whose constructor first checks
#'   that its codec is registered.
#'
#' @return A call.
#'
#' @keywords internal
#' @noRd
constructor_body <- function(
  qname,
  fields,
  variant = "",
  required = character(),
  native = FALSE,
  package = ""
) {
  # A required field without an argument reaches construction as NULL, which
  # reports it as missing.
  args <- as.call(c(
    as.name("list"),
    stats::setNames(
      lapply(fields, function(n) {
        if (n %in% required) {
          bquote(if (missing(.(as.name(n)))) NULL else .(as.name(n)))
        } else {
          as.name(n)
        }
      }),
      fields
    )
  ))
  # A type declared outside a package registers its own record, which its
  # constructor carries, when the registry lacks it, then constructs again.
  body <- bquote({
    res <- .Call(wrap__r8_new_value, .(args), .(qname), .(variant))
    if (inherits(res, "r8_problems")) {
      if (
        identical(res[["errors"]][["code"]][1L], "type_unregistered") &&
          ensure_registered(sys.function())
      ) {
        res <- .Call(wrap__r8_new_value, .(args), .(qname), .(variant))
      }
      if (inherits(res, "r8_problems")) {
        construct_error(res, .(qname), .(variant))
      }
    }
    res
  })
  if (native) {
    body <- as.call(c(
      as.name("{"),
      bquote(check_codec(.(qname), .(package))),
      as.list(body)[-1L]
    ))
  }
  body
} # /R8::constructor_body


# %% check_codec ----
#' Check that the codec of a Rust-native type is registered
#'
#' @param qname Character: The qualified type name.
#'
#' @return `NULL`, invisibly; signals `r8_codec_missing` when the codec is not
#'   registered.
#'
#' @keywords internal
#' @noRd
check_codec <- function(qname, package = "") {
  if (is.null(.r8_codecs[[qname]])) {
    r8_abort(
      "`",
      qname,
      "` is a Rust-native type, whose values the codec in a package's ",
      "compiled exports builds; no codec of it is registered.",
      if (nzchar(package)) {
        paste0(
          " Loading '",
          package,
          "' registers it (its .onLoad() calls r8_register())."
        )
      },
      class = c("r8_codec_missing", "r8_value_error")
    )
  }
  invisible(NULL)
} # /R8::check_codec


# %% new_dgc ----
#' A dgCMatrix from its compressed sparse column parts
#'
#' @param i Integer: Row indices, 0-based.
#' @param p Integer: Column offsets.
#' @param x Numeric: Values.
#' @param dim Integer: The shape.
#'
#' @return A `dgCMatrix`.
#'
#' @keywords internal
#' @noRd
new_dgc <- function(i, p, x, dim) {
  if (!requireNamespace("Matrix", quietly = TRUE)) {
    r8_abort(
      "A sparse matrix field needs the package 'Matrix'.",
      class = "r8_value_error"
    )
  }
  methods::new("dgCMatrix", i = i, p = p, x = x, Dim = dim)
} # /R8::new_dgc


# %% $.r8_value ----
#' Fields of an R8 value
#'
#' `x$name` and `x[["name"]]` read a field by its exact name; an unknown name
#' is an error. `x$name <- v` and `x[["name"]] <- v` return a new value,
#' validated; `x` is unchanged.
#'
#' @param x An R8 value.
#' @param name Character: The field name.
#' @param i Character: The field name.
#' @param value The new field value.
#' @param ... Not used.
#'
#' @return The field's value, or the new R8 value.
#'
#' @name r8_value_fields
#' @export
`$.r8_value` <- function(x, name) {
  v <- .subset2(x, name)
  if (is.null(v)) field_get(x, name) else v
} # /R8::$.r8_value


# %% [[.r8_value ----
#' @rdname r8_value_fields
#' @export
`[[.r8_value` <- function(x, i, ...) {
  if (!is.character(i) || length(i) != 1L) {
    r8_abort(
      "A field is read by name: x[[\"name\"]].",
      class = "r8_value_error"
    )
  }
  field_get(x, i)
} # /R8::[[.r8_value


# %% $<-.r8_value ----
#' @rdname r8_value_fields
#' @export
`$<-.r8_value` <- function(x, name, value) {
  field_set(x, name, value)
} # /R8::$<-.r8_value


# %% [[<-.r8_value ----
#' @rdname r8_value_fields
#' @export
`[[<-.r8_value` <- function(x, i, value) {
  if (!is.character(i) || length(i) != 1L) {
    r8_abort(
      "A field is replaced by name: x[[\"name\"]] <- value.",
      class = "r8_value_error"
    )
  }
  field_set(x, i, value)
} # /R8::[[<-.r8_value


# %% field_get ----
#' Read a field
#'
#' @param x An R8 value.
#' @param name Character: The field name.
#'
#' @return The field's value.
#'
#' @keywords internal
#' @noRd
field_get <- function(x, name) {
  v <- .subset2(x, name)
  if (is.null(v) && !name %in% names(x)) {
    variant <- attr(x, "r8_variant")
    label <- paste0(
      attr(x, "r8_type"),
      if (!is.null(variant)) paste0("@", variant)
    )
    fields <- names(x)
    r8_abort(
      "`",
      name,
      "` is not a field of `",
      label,
      "`",
      if (length(fields) == 0L) {
        ", which has no fields."
      } else {
        paste0(". Fields: ", paste(fields, collapse = ", "), ".")
      },
      class = "r8_value_error"
    )
  }
  v
} # /R8::field_get


# %% field_set ----
#' Replace a field, returning a new value
#'
#' @param x An R8 value.
#' @param name Character: The field name.
#' @param value The new field value.
#'
#' @return The new R8 value.
#'
#' @keywords internal
#' @noRd
field_set <- function(x, name, value) {
  res <- .Call(wrap__r8_set_field, x, name, value)
  if (is.null(res)) {
    r8_abort(
      "`",
      name,
      "` is not a field of `",
      attr(x, "r8_type"),
      "`.",
      class = "r8_value_error"
    )
  }
  if (inherits(res, "r8_problems")) {
    variant <- attr(x, "r8_variant")
    construct_error(
      res,
      attr(x, "r8_type"),
      if (is.null(variant)) "" else variant
    )
  }
  res
} # /R8::field_set


# %% @.r8_value ----
#' `@` on an R8 value
#'
#' `@` reads the associated items of a type, not the fields of a value.
#'
#' @param object An R8 value.
#' @param name The name.
#'
#' @return Does not return.
#'
#' @rdname r8_value_fields
#' @export
`@.r8_value` <- function(object, name) {
  r8_abort(
    "`@` reads the items of a type; read the field of a value with `$`: x$",
    as.character(substitute(name)),
    ".",
    class = "r8_value_error"
  )
} # /R8::@.r8_value


# %% guards ----
# Replacement functions that would change a value without validation.
guard <- function(what) {
  force(what)
  function(x, ..., value) {
    r8_abort(
      "`",
      what,
      "` would change an R8 value without validation; values are replaced field by field with `$<-`.",
      class = "r8_value_error"
    )
  }
}

#' Guards on R8 values
#'
#' `names<-`, `length<-`, `[<-`, `dim<-`, and `levels<-` would change a value
#' without validation, so they abort; `[[` with a number aborts, as fields
#' are read by name.
#'
#' @param x An R8 value.
#' @param value Not used.
#' @param ... Not used.
#'
#' @return Does not return.
#'
#' @name r8_value_guards
#' @export
`names<-.r8_value` <- guard("names<-")

#' @rdname r8_value_guards
#' @export
`length<-.r8_value` <- guard("length<-")

#' @rdname r8_value_guards
#' @export
`[<-.r8_value` <- guard("[<-")

#' @rdname r8_value_guards
#' @export
`dim<-.r8_value` <- guard("dim<-")

#' @rdname r8_value_guards
#' @export
`levels<-.r8_value` <- guard("levels<-")


# %% to_json ----
#' Write an R8 value as JSON
#'
#' The value's document: `"$type"`, `"$types"` (the version of every type it
#' contains), then its fields in declaration order.
#'
#' @param x An R8 value.
#'
#' @return Character: The JSON document.
#'
#' @examples
#' op <- options(r8.namespace = "demo")
#' Point <- new_struct("Point", fields = list(x = type_double(), y = type_double()))
#' to_json(Point(x = 1, y = 2))
#' options(op)
#'
#' @export
to_json <- function(x) {
  if (!inherits(x, "r8_value")) {
    r8_abort("`x` is an R8 value.", class = "r8_type_error")
  }
  res <- r8_value_to_document(x)
  if (!is.null(res[["errors"]])) {
    r8_abort(
      "Cannot write `",
      attr(x, "r8_type"),
      "`:\n",
      problems_message(res[["errors"]]),
      class = "r8_value_error"
    )
  }
  res[["json"]]
} # /R8::to_json


# %% r8_from_json ----
#' Read an R8 value from JSON
#'
#' Reads a document of `type`: checks `"$type"` and the versions in
#' `"$types"`, validates the value, and returns it.
#'
#' @param type An R8 type object.
#' @param json Character: The JSON document.
#'
#' @return An R8 value.
#'
#' @examples
#' op <- options(r8.namespace = "demo")
#' Point <- new_struct("Point", fields = list(x = type_double(), y = type_double()))
#' r8_from_json(Point, to_json(Point(x = 1, y = 2)))
#' options(op)
#'
#' @export
r8_from_json <- function(type, json) {
  if (!inherits(type, "r8_type")) {
    r8_abort("`type` is an R8 type object.", class = "r8_type_error")
  }
  ensure_registered(type)
  if (isTRUE(attr(type, "r8_native"))) {
    check_codec(attr(type, "r8_name"), attr(type, "r8_package") %||% "")
  }
  res <- r8_value_from_document(
    paste(json, collapse = "\n"),
    attr(type, "r8_name")
  )
  if (!is.null(res[["errors"]])) {
    r8_abort(
      "Cannot read `",
      attr(type, "r8_name"),
      "`:\n",
      problems_message(res[["errors"]]),
      class = "r8_value_error"
    )
  }
  res[["value"]]
} # /R8::r8_from_json
