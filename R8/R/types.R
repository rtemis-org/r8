# types.R
# ::R8::
# 2026- EDG rtemis.org

# Each constructor returns an `r8_field_type`: a list in the form of the IR's
# field type (`kind` and its members), which `r8_ir_json()` writes as JSON.
# Members that are JSON arrays are lists, so that a length-one array stays an
# array.

# %% new_field_type ----
#' Field type object
#'
#' @param kind Character: The IR kind.
#' @param ... Members of the field type; `NULL` members are dropped.
#'
#' @return An `r8_field_type`.
#'
#' @keywords internal
#' @noRd
new_field_type <- function(kind, ...) {
  members <- Filter(Negate(is.null), list(...))
  structure(c(list(kind = kind), members), class = "r8_field_type")
} # /R8::new_field_type


# %% as_array ----
#' A vector as a JSON array
#'
#' @param x Optional vector.
#'
#' @return A list, or `NULL`.
#'
#' @keywords internal
#' @noRd
as_array <- function(x) {
  if (is.null(x)) NULL else as.list(x)
} # /R8::as_array


# %% check_number ----
#' Checks a numeric constraint
#'
#' @param x Optional numeric: The constraint.
#' @param name Character: The argument name.
#'
#' @return `x`, invisibly.
#'
#' @keywords internal
#' @noRd
check_number <- function(x, name) {
  if (!is.null(x) && !(is.numeric(x) && length(x) == 1L && is.finite(x))) {
    r8_abort(
      "`",
      name,
      "` is one finite number.",
      class = "r8_declaration_error"
    )
  }
  invisible(x)
} # /R8::check_number


# %% type_logical ----
#' Logical field type
#'
#' A logical value, `TRUE` or `FALSE`, never `NA`.
#'
#' @return An `r8_field_type`.
#'
#' @examples
#' type_logical()
#'
#' @export
type_logical <- function() {
  new_field_type("logical")
} # /R8::type_logical


# %% type_integer ----
#' Integer field type
#'
#' An integer from -2147483647 to 2147483647. A double given for an integer
#' field converts when it is a whole number in range.
#'
#' @param min Optional Integer: Smallest admitted value.
#' @param max Optional Integer: Largest admitted value.
#' @param exclusive_min Optional Integer: The value must be above it.
#' @param exclusive_max Optional Integer: The value must be below it.
#' @param values Optional Integer: The admitted values.
#'
#' @return An `r8_field_type`.
#'
#' @examples
#' type_integer(min = 1L)
#'
#' @export
type_integer <- function(
  min = NULL,
  max = NULL,
  exclusive_min = NULL,
  exclusive_max = NULL,
  values = NULL
) {
  for (n in c("min", "max", "exclusive_min", "exclusive_max")) {
    check_number(get(n), n)
  }
  as_int <- function(x) if (is.null(x)) NULL else as.integer(x)
  new_field_type(
    "integer",
    min = as_int(min),
    max = as_int(max),
    exclusive_min = as_int(exclusive_min),
    exclusive_max = as_int(exclusive_max),
    `enum` = as_array(as_int(values))
  )
} # /R8::type_integer


# %% type_double ----
#' Double field type
#'
#' A double. The domain is finite unless `finite = FALSE`, which admits
#' `Inf`, `-Inf`, and `NaN`.
#'
#' @param min Optional Numeric: Smallest admitted value.
#' @param max Optional Numeric: Largest admitted value.
#' @param exclusive_min Optional Numeric: The value must be above it.
#' @param exclusive_max Optional Numeric: The value must be below it.
#' @param finite Logical: Restrict the domain to finite values.
#' @param values Optional Numeric: The admitted values.
#'
#' @return An `r8_field_type`.
#'
#' @examples
#' type_double(exclusive_min = 0)
#'
#' @export
type_double <- function(
  min = NULL,
  max = NULL,
  exclusive_min = NULL,
  exclusive_max = NULL,
  finite = TRUE,
  values = NULL
) {
  for (n in c("min", "max", "exclusive_min", "exclusive_max")) {
    check_number(get(n), n)
  }
  as_dbl <- function(x) if (is.null(x)) NULL else as.double(x)
  new_field_type(
    "double",
    min = as_dbl(min),
    max = as_dbl(max),
    exclusive_min = as_dbl(exclusive_min),
    exclusive_max = as_dbl(exclusive_max),
    finite = if (isFALSE(finite)) FALSE else NULL,
    `enum` = as_array(as_dbl(values))
  )
} # /R8::type_double


# %% type_character ----
#' Character field type
#'
#' A string without U+0000, never `NA`.
#'
#' @param pattern Optional Character: A regular expression in Rust `regex`
#'   syntax that the string matches.
#' @param values Optional Character: The admitted values.
#'
#' @return An `r8_field_type`.
#'
#' @examples
#' type_character(values = c("gaussian", "binomial"))
#'
#' @export
type_character <- function(pattern = NULL, values = NULL) {
  new_field_type("character", pattern = pattern, `enum` = as_array(values))
} # /R8::type_character


# %% type_raw ----
#' Raw field type
#'
#' Bytes, a raw vector in R and canonical padded base64 in JSON.
#'
#' @return An `r8_field_type`.
#'
#' @examples
#' type_raw()
#'
#' @export
type_raw <- function() {
  new_field_type("raw")
} # /R8::type_raw


# %% type_date ----
#' Date field type
#'
#' A calendar date from 0000-01-01 to 9999-12-31.
#'
#' @return An `r8_field_type`.
#'
#' @examples
#' type_date()
#'
#' @export
type_date <- function() {
  new_field_type("date")
} # /R8::type_date


# %% type_posixct ----
#' Date-time field type
#'
#' A UTC instant in microseconds, from 1827-04-16T00:06:12.629505Z to
#' 2112-09-17T23:53:47.370495Z, the range a double holds exactly.
#'
#' @return An `r8_field_type`.
#'
#' @examples
#' type_posixct()
#'
#' @export
type_posixct <- function() {
  new_field_type("posixct")
} # /R8::type_posixct


# %% type_factor ----
#' Factor field type
#'
#' A vector of categorical values.
#'
#' @param levels Optional Character: The declared levels. Without them, the
#'   levels come from the data.
#' @param ordered Logical: An ordered factor.
#'
#' @return An `r8_field_type`.
#'
#' @examples
#' type_factor(levels = c("+", "-", "*"))
#'
#' @export
type_factor <- function(levels = NULL, ordered = FALSE) {
  new_field_type(
    "factor",
    levels = as_array(levels),
    ordered = if (isTRUE(ordered)) TRUE else NULL
  )
} # /R8::type_factor


# %% type_json ----
#' JSON field type
#'
#' Any JSON value: objects as named lists, arrays as unnamed lists, numbers
#' as doubles.
#'
#' @return An `r8_field_type`.
#'
#' @examples
#' type_json()
#'
#' @export
type_json <- function() {
  new_field_type("json")
} # /R8::type_json


# %% type_r ----
#' Runtime-only R field type
#'
#' An R value of a class, held in R only: absent from JSON and from the hash.
#' A field of this type is declared inside `option()`.
#'
#' @param class Character: The R class.
#'
#' @return An `r8_field_type`.
#'
#' @examples
#' option(type_r("lm"), resolve = "runtime")
#'
#' @export
type_r <- function(class) {
  if (!is.character(class) || length(class) != 1L) {
    r8_abort("`class` is one string.", class = "r8_declaration_error")
  }
  new_field_type("r", class = class)
} # /R8::type_r


# %% vec ----
#' Vector field type
#'
#' `Vec<T>`: an atomic vector for a scalar `T`, a list otherwise.
#'
#' @param type An `r8_field_type` or an R8 type: The element type.
#' @param min_length Optional Integer: Fewest elements.
#' @param max_length Optional Integer: Most elements.
#' @param unique Logical: Elements are distinct.
#'
#' @return An `r8_field_type`.
#'
#' @examples
#' vec(type_double(min = 0), min_length = 1L)
#'
#' @export
vec <- function(type, min_length = NULL, max_length = NULL, unique = FALSE) {
  new_field_type(
    "vec",
    element = as_field_type(type),
    min_length = if (is.null(min_length)) NULL else as.integer(min_length),
    max_length = if (is.null(max_length)) NULL else as.integer(max_length),
    unique = if (isTRUE(unique)) TRUE else NULL
  )
} # /R8::vec


# %% option ----
#' Optional field type
#'
#' `Option<T>`: the value may be absent, `NULL`. `resolve` declares what
#' absence means.
#'
#' @param type An `r8_field_type` or an R8 type: The type when present.
#' @param resolve Character \{"none", "computed", "runtime"\}: `"none"`,
#'   absence is final; `"computed"`, `r8_resolve()` fills the value from
#'   `compute`; `"runtime"`, `r8_resolve()` fills it from data or the
#'   environment.
#' @param compute Optional expression: For `resolve = "computed"`, the value
#'   as an expression over the other fields, captured unevaluated and
#'   written in the rule dialect.
#'
#' @return An `r8_field_type`.
#'
#' @examples
#' option(type_integer(min = 1L), resolve = "runtime")
#' option(type_double(), resolve = "computed", compute = dose_mg / 1000)
#'
#' @export
option <- function(type, resolve = "none", compute = NULL) {
  resolve <- match.arg(resolve, c("none", "computed", "runtime"))
  expr <- substitute(compute)
  if (resolve == "computed" && is.null(expr)) {
    r8_abort(
      "A computed field gives its value in `compute`.",
      class = "r8_declaration_error"
    )
  }
  if (resolve != "computed" && !is.null(expr)) {
    r8_abort(
      "`compute` belongs to resolve = \"computed\".",
      class = "r8_declaration_error"
    )
  }
  policy <- switch(
    resolve,
    none = NULL,
    runtime = list(policy = "runtime"),
    computed = list(policy = "computed", expr = compile_rule_expr(expr))
  )
  new_field_type("option", inner = as_field_type(type), resolve = policy)
} # /R8::option


# %% hashmap ----
#' Map field type
#'
#' `HashMap<String, T>`: string keys, sorted at construction.
#'
#' @param type An `r8_field_type` or an R8 type: The value type.
#' @param keys Optional `r8_field_type`: A `type_character()` constraining
#'   the keys.
#'
#' @return An `r8_field_type`.
#'
#' @examples
#' hashmap(type_integer())
#'
#' @export
hashmap <- function(type, keys = NULL) {
  new_field_type(
    "hashmap",
    value = as_field_type(type),
    keys = key_constraints(keys)
  )
} # /R8::hashmap


# %% ordered_map ----
#' Ordered map field type
#'
#' Unique string keys in insertion order: a named list in R. Order is part of
#' the value.
#'
#' @param type An `r8_field_type` or an R8 type: The value type.
#' @param keys Optional `r8_field_type`: A `type_character()` constraining
#'   the keys.
#'
#' @return An `r8_field_type`.
#'
#' @examples
#' ordered_map(type_double())
#'
#' @export
ordered_map <- function(type, keys = NULL) {
  new_field_type(
    "ordered_map",
    value = as_field_type(type),
    keys = key_constraints(keys)
  )
} # /R8::ordered_map


# %% key_constraints ----
#' Key constraints of a map
#'
#' @param keys Optional `r8_field_type` of kind character.
#'
#' @return A list of the key constraints, or `NULL`.
#'
#' @keywords internal
#' @noRd
key_constraints <- function(keys) {
  if (is.null(keys)) {
    return(NULL)
  }
  if (!inherits(keys, "r8_field_type") || keys[["kind"]] != "character") {
    r8_abort("`keys` is a type_character().", class = "r8_declaration_error")
  }
  k <- unclass(keys)
  k[["kind"]] <- NULL
  if (length(k) == 0L) NULL else k
} # /R8::key_constraints


# %% as_field_type ----
#' A field type from a field type object or an R8 type
#'
#' @param x An `r8_field_type`, or a struct or enum type object.
#'
#' @return An `r8_field_type`.
#'
#' @keywords internal
#' @noRd
as_field_type <- function(x) {
  if (inherits(x, "r8_field_type")) {
    return(x)
  }
  if (inherits(x, "r8_type")) {
    return(new_field_type("named", name = attr(x, "r8_name")))
  }
  r8_abort(
    "A field type is a type_*(), vec(), option(), hashmap(), or ordered_map() call, or an R8 type.",
    class = "r8_declaration_error"
  )
} # /R8::as_field_type


# %% print.r8_field_type ----
#' Print a field type
#'
#' @param x An `r8_field_type`.
#' @param ... Not used.
#'
#' @return `x`, invisibly.
#'
#' @export
print.r8_field_type <- function(x, ...) {
  cat("<r8_field_type>", r8_ir_json(unclass(x)), "\n")
  invisible(x)
} # /R8::print.r8_field_type
