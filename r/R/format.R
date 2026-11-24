# format.R
# ::R8::
# 2026- EDG rtemis.org

# The text of declaration objects: a header that names the kind of object, in
# angle brackets, then one line per member. `format()` returns the lines, and
# `print()` writes them. Types are in the notation of the runtime
# (`r8_type_notation()`), rules and defaults as R writes them.
# spec: R8/class-system#notation-of-a-type

# %% type_text ----
#' The notation of a field type or of a type object
#'
#' @param type An `r8_field_type` or an R8 type.
#'
#' @return Character: The notation, without backticks.
#'
#' @keywords internal
#' @noRd
type_text <- function(type) {
  if (inherits(type, "r8_type")) {
    return(attr(type, "r8_name", exact = TRUE))
  }
  plain_notation(r8_type_notation(unclass(type)))
} # /R8::type_text


# %% default_text ----
#' A default as R writes it, on one line
#'
#' @param x The default.
#'
#' @return Character: The R source of the default, cut at 60 characters; for
#'   an R8 value, its type and variant.
#'
#' @keywords internal
#' @noRd
default_text <- function(x) {
  if (is_value(x)) {
    variant <- value_variant(x)
    return(paste0(
      value_type(x),
      if (!is.null(variant)) paste0("@", variant),
      if (length(attr(x, "names", exact = TRUE)) > 0L) "(...)"
    ))
  }
  if (inherits(x, "r8_json_ir")) {
    x <- unclass(x)
  }
  cut_text(deparse1(x))
} # /R8::default_text


# %% cut_text ----
#' A text of at most 60 characters
#'
#' @param text Character: The text.
#'
#' @return Character: The text, or its first 57 characters and `...`.
#'
#' @keywords internal
#' @noRd
cut_text <- function(text) {
  if (nchar(text) > 60L) paste0(substr(text, 1L, 57L), "...") else text
} # /R8::cut_text


# %% declared_default_text ----
#' The default of a declaration's field as R writes it, on one line
#'
#' @param f List: The field, of a declaration.
#'
#' @return Character: The R source of a scalar or a vector, the type and the
#'   variant of an R8 value, and the JSON text of any other default.
#'
#' @keywords internal
#' @noRd
declared_default_text <- function(f) {
  x <- f[["default"]]
  type <- f[["type"]]
  base <- if (identical(type[["kind"]], "option")) type[["inner"]] else type
  if (identical(base[["kind"]], "named")) {
    # A value of an enum is its variant's name, or an object with a tag.
    named <- if (r8_is_registered(base[["name"]])) r8_type_ir(base[["name"]])
    variant <- if (is.character(x)) x else x[[named[["tag"]] %||% ""]]
    return(paste0(
      base[["name"]],
      if (is.character(variant)) paste0("@", variant),
      if (is.list(x) && length(x) > is.character(variant)) "(...)"
    ))
  }
  if (is.character(x) && identical(base[["kind"]], "double")) {
    # A non-finite number, which JSON writes as a string.
    return(switch(x, Infinity = "Inf", `-Infinity` = "-Inf", x))
  }
  text <- if (is.character(x) && identical(base[["kind"]], "date")) {
    paste0("as.Date(", deparse1(x), ")")
  } else {
    render_default(x, type, "")
  }
  cut_text(
    if (startsWith(text, "R8::json_ir(")) unclass(r8_ir_json(x)) else text
  )
} # /R8::declared_default_text


# %% rule_text ----
#' A rule on one line, as R writes it
#'
#' @param x A rule, an `r8_rule` or a rule of a declaration.
#'
#' @return Character: The invariant, and `when` with its condition.
#'
#' @keywords internal
#' @noRd
rule_text <- function(x) {
  paste0(
    logic_text(x[["invariant"]]),
    if (!is.null(x[["when"]])) paste0(", when ", logic_text(x[["when"]]))
  )
} # /R8::rule_text


# %% rule_lines ----
#' The lines of a declaration's rules
#'
#' @param rules List: The rules.
#' @param indent Character: The indentation.
#'
#' @return Character: One line per rule.
#'
#' @keywords internal
#' @noRd
rule_lines <- function(rules, indent) {
  vapply(
    rules,
    function(r) {
      id <- r[["id"]]
      paste0(
        indent,
        "rule",
        if (!is.null(id) && !is.na(id)) paste0(" ", id),
        ": ",
        rule_text(r)
      )
    },
    ""
  )
} # /R8::rule_lines


# %% field_lines ----
#' The lines of a type's fields
#'
#' @param fields List: The fields of a declaration.
#' @param indent Character: The indentation.
#'
#' @return Character: One line per field, `name: type = default`.
#'
#' @keywords internal
#' @noRd
field_lines <- function(fields, indent) {
  vapply(
    fields,
    function(f) {
      paste0(
        indent,
        f[["name"]],
        ": ",
        plain_notation(r8_type_notation(f[["type"]])),
        if (!is.null(f[["default"]])) {
          paste0(" = ", declared_default_text(f))
        }
      )
    },
    ""
  )
} # /R8::field_lines


# %% described ----
#' A description as a line
#'
#' @param text Optional character: The description.
#'
#' @return Character: The line, or a vector of length 0.
#'
#' @keywords internal
#' @noRd
described <- function(text) {
  if (is.null(text) || !nzchar(text)) character() else paste0("  ", text)
} # /R8::described


# %% print_lines ----
#' Write the lines of an object's text
#'
#' @param x The object.
#'
#' @return `x`, invisibly.
#'
#' @keywords internal
#' @noRd
print_lines <- function(x) {
  cat(format(x), sep = "\n")
  invisible(x)
} # /R8::print_lines


# %% format.r8_type ----
#' The text of declaration objects
#'
#' `format()` gives the text of a type object, or of an object that a
#' declaration is written with, such as a field type or a rule, as lines, and
#' `print()` writes them. The first line names the kind of object in angle
#' brackets. Types are in one notation (`integer [1, Inf)`, `optional vector
#' of double`), and rules and defaults as R writes them.
#'
#' @param x The object.
#' @param ... Not used.
#'
#' @return `format()`: Character, the lines. `print()`: `x`, invisibly.
#'
#' @examples
#' op <- options(r8.namespace = "demo")
#' vec(type_double(min = 0), min_length = 1L)
#' field(type_integer(min = 1L), default = 10L, description = "Number of folds.")
#' rule(lo <= hi, message = "lo is at most hi.", id = "ORDER")
#' Interval <- new_struct(
#'   "Interval",
#'   fields = list(lo = field(type_double(), default = 0), hi = type_double()),
#'   rules = rule(lo <= hi, id = "ORDER")
#' )
#' Interval
#' format(Interval)
#' options(op)
#'
#' @name r8_format
#' @export
format.r8_type <- function(x, ...) {
  ir <- type_ir(x)
  header <- paste0(
    "<r8 ",
    ir[["kind"]],
    " ",
    attr(x, "r8_name", exact = TRUE),
    " v",
    ir[["version"]],
    if (!is.null(ir[["extends"]])) paste0(" extends ", ir[["extends"]]),
    ">"
  )
  members <- if (identical(ir[["kind"]], "struct")) {
    field_lines(ir[["fields"]], "  ")
  } else {
    unlist(lapply(ir[["variants"]], variant_lines))
  }
  c(
    header,
    described(ir[["description"]]),
    members,
    rule_lines(ir[["rules"]], "  ")
  )
} # /R8::format.r8_type


# %% variant_lines ----
#' The lines of a variant of an enum
#'
#' @param v The variant, of the enum's declaration.
#'
#' @return Character: The variant's name, then its fields and rules.
#'
#' @keywords internal
#' @noRd
variant_lines <- function(v) {
  name <- v[["name"]]
  if (identical(v[["kind"]], "unit")) {
    return(paste0("  ", name))
  }
  if (identical(v[["kind"]], "newtype")) {
    return(paste0("  ", name, ": ", v[["of"]]))
  }
  c(
    paste0("  ", name),
    field_lines(v[["fields"]], "    "),
    rule_lines(v[["rules"]], "    ")
  )
} # /R8::variant_lines


# %% print.r8_type ----
#' @rdname r8_format
#' @export
print.r8_type <- function(x, ...) print_lines(x)


# %% format.r8_field_type ----
#' @rdname r8_format
#' @export
format.r8_field_type <- function(x, ...) {
  paste("<r8 field type>", type_text(x))
} # /R8::format.r8_field_type

#' @rdname r8_format
#' @export
print.r8_field_type <- function(x, ...) print_lines(x)


# %% format.r8_host_class ----
#' @rdname r8_format
#' @export
format.r8_host_class <- function(x, ...) {
  h <- unclass(x)
  paste0(
    "<r8 host class \"",
    h[["class"]],
    "\"",
    if (nzchar(h[["package"]])) paste0(" of ", h[["package"]]),
    ", ",
    toupper(h[["system"]]),
    ">"
  )
} # /R8::format.r8_host_class

#' @rdname r8_format
#' @export
print.r8_host_class <- function(x, ...) print_lines(x)


# %% format.r8_field ----
#' @rdname r8_format
#' @export
format.r8_field <- function(x, ...) {
  f <- unclass(x)
  column <- inherits(x, "r8_column")
  policy <- f[["resolve"]][["policy"]]
  c(
    paste0(
      if (column) "<r8 column> " else "<r8 field> ",
      type_text(f[["type"]]),
      if (isTRUE(f[["has_default"]])) {
        paste0(" = ", default_text(f[["default"]]))
      }
    ),
    described(f[["description"]]),
    if (!is.null(f[["unit"]])) paste0("  unit: ", f[["unit"]]),
    if (isTRUE(f[["unique"]])) "  unique",
    if (column && isFALSE(f[["required"]])) "  may be absent",
    if (isTRUE(f[["sensitive"]])) "  sensitive",
    if (identical(policy, "computed")) {
      paste0("  computed: ", logic_text(f[["resolve"]][["expr"]]))
    } else if (!is.null(policy)) {
      "  filled from the context"
    }
  )
} # /R8::format.r8_field

#' @rdname r8_format
#' @export
print.r8_field <- function(x, ...) print_lines(x)


# %% format.r8_rule ----
#' @rdname r8_format
#' @export
format.r8_rule <- function(x, ...) {
  r <- unclass(x)
  c(
    paste0(
      "<r8 rule",
      if (!is.null(r[["id"]])) paste0(" ", r[["id"]]),
      "> ",
      rule_text(r)
    ),
    described(r[["message"]])
  )
} # /R8::format.r8_rule

#' @rdname r8_format
#' @export
print.r8_rule <- function(x, ...) print_lines(x)


# %% format.r8_variant ----
#' @rdname r8_format
#' @export
format.r8_variant <- function(x, ...) {
  v <- unclass(x)
  fields <- vapply(
    names(v[["fields"]]),
    function(n) {
      f <- v[["fields"]][[n]]
      paste0(
        "  ",
        n,
        ": ",
        if (inherits(f, "r8_field")) {
          sub("^<r8 field> ", "", format(f)[[1L]])
        } else {
          type_text(f)
        }
      )
    },
    ""
  )
  c(
    paste0(
      "<r8 variant>",
      if (!is.null(v[["of"]])) {
        paste0(" of ", type_text(v[["of"]]))
      } else if (length(fields) == 0L) {
        " unit"
      }
    ),
    described(v[["description"]]),
    unname(fields),
    rule_lines(as_rules(v[["rules"]]), "  ")
  )
} # /R8::format.r8_variant

#' @rdname r8_format
#' @export
print.r8_variant <- function(x, ...) print_lines(x)


# %% param_lines ----
#' The lines of parameters
#'
#' @param p An `r8_params`.
#'
#' @return Character: One line per parameter, `name: type = default`.
#'
#' @keywords internal
#' @noRd
param_lines <- function(p) {
  p <- unclass(p)
  vapply(
    seq_along(p[["names"]]),
    function(i) {
      type <- p[["types"]][[i]]
      paste0(
        "  ",
        p[["names"]][[i]],
        if (!is.null(type)) paste0(": ", type_text(type)),
        if (nzchar(p[["texts"]][[i]])) paste0(" = ", p[["texts"]][[i]])
      )
    },
    ""
  )
} # /R8::param_lines


# %% format.r8_params ----
#' @rdname r8_format
#' @export
format.r8_params <- function(x, ...) {
  c("<r8 params>", param_lines(x))
} # /R8::format.r8_params

#' @rdname r8_format
#' @export
print.r8_params <- function(x, ...) print_lines(x)


# %% format.r8_trait_method ----
#' @rdname r8_format
#' @export
format.r8_trait_method <- function(x, ...) {
  m <- unclass(x)
  c(
    "<r8 trait method>",
    described(m[["description"]]),
    param_lines(m[["params"]]),
    if (!is.null(m[["return_type"]])) {
      paste0("  returns ", returned_text(m[["return_type"]]))
    },
    if (length(m[["dispatch"]]) > 1L) {
      paste0("  dispatches on ", paste(m[["dispatch"]], collapse = ", "))
    },
    if (!is.null(m[["default_impl"]])) "  with a default implementation",
    if (isTRUE(m[["stochastic"]])) "  stochastic"
  )
} # /R8::format.r8_trait_method

#' @rdname r8_format
#' @export
print.r8_trait_method <- function(x, ...) print_lines(x)


# %% returned_text ----
#' The notation of a return type as a declaration holds it
#'
#' @param type An `r8_field_type`, an R8 type, the IR of one, or `"Self"`.
#'
#' @return Character: The notation.
#'
#' @keywords internal
#' @noRd
returned_text <- function(type) {
  if (is.character(type)) type else type_text(type)
} # /R8::returned_text


# %% format.r8_impl_method ----
#' @rdname r8_format
#' @export
format.r8_impl_method <- function(x, ...) {
  m <- unclass(x)
  c(
    paste0(
      "<r8 impl method> function(",
      paste(names(formals(m[["fn"]])), collapse = ", "),
      ")"
    ),
    if (!is.null(m[["return_type"]])) {
      paste0("  returns ", returned_text(m[["return_type"]]))
    }
  )
} # /R8::format.r8_impl_method

#' @rdname r8_format
#' @export
print.r8_impl_method <- function(x, ...) print_lines(x)
