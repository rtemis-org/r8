# types.R
# ::R8::
# 2026- EDG rtemis.org

# Each constructor returns an `r8_field_type`: a list in the form of the IR's
# field type (`kind` and its members), which `r8_ir_json()` writes as JSON.
# Members that are JSON arrays are lists, so that a length-one array stays an
# array. The constructors with arguments build in Rust (`fieldtype.rs`, through
# `rtemis_r8::ir::build`), which checks the arguments and holds the messages;
# a routine returns the message of an argument that does not build, which
# declared() raises from the constructor's call.

# %% declared ----
#' A field type built in Rust, or the error of its arguments
#'
#' @param x An `r8_field_type`, or the message of an argument that does not
#'   build.
#'
#' @return `x`; aborts with `r8_declaration_error` for a message.
#'
#' @keywords internal
#' @noRd
declared <- function(x) {
  if (is.character(x)) {
    r8_abort(x, class = "r8_declaration_error")
  }
  x
} # /R8::declared

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
  structure(list(kind = "logical"), class = "r8_field_type")
} # /R8::type_logical


# %% type_integer ----
#' Integer field type
#'
#' An integer from -2147483647 to 2147483647. A double given for an integer
#' field converts when it is a whole number in range.
#'
#' @param min Optional Integer: Smallest admitted value.
#' @param max Optional Integer: Largest admitted value.
#' @param exclusive_min Optional Integer: The value must be above it. Given
#'   with `min`, it raises `r8_declaration_error`.
#' @param exclusive_max Optional Integer: The value must be below it. Given
#'   with `max`, it raises `r8_declaration_error`.
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
  declared(r8_ft_integer(min, max, exclusive_min, exclusive_max, values))
} # /R8::type_integer


# %% type_double ----
#' Double field type
#'
#' A double. The domain is finite unless `finite = FALSE`, which admits
#' `Inf`, `-Inf`, and `NaN`.
#'
#' @param min Optional Numeric: Smallest admitted value.
#' @param max Optional Numeric: Largest admitted value.
#' @param exclusive_min Optional Numeric: The value must be above it. Given
#'   with `min`, it raises `r8_declaration_error`.
#' @param exclusive_max Optional Numeric: The value must be below it. Given
#'   with `max`, it raises `r8_declaration_error`.
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
  declared(r8_ft_double(min, max, exclusive_min, exclusive_max, finite, values))
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
  declared(r8_ft_character(pattern, values))
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
  structure(list(kind = "raw"), class = "r8_field_type")
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
  structure(list(kind = "date"), class = "r8_field_type")
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
  structure(list(kind = "posixct"), class = "r8_field_type")
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
  declared(r8_ft_factor(levels, ordered))
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
  structure(list(kind = "json"), class = "r8_field_type")
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
  declared(r8_ft_r(class))
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
  declared(r8_ft_vec(type, min_length, max_length, unique))
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
  if (!identical(resolve, "none")) {
    resolve <- match.arg(resolve, c("none", "computed", "runtime"))
  }
  declared(r8_ft_option(type, resolve, substitute(compute)))
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
  declared(r8_ft_map(type, keys, FALSE))
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
  declared(r8_ft_map(type, keys, TRUE))
} # /R8::ordered_map


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


# %% type_matrix ----
#' Matrix field type
#'
#' A dense matrix of a scalar type, an atomic vector with `dim` in R. Given an
#' R8 struct type, a matrix whose columns are the struct's fields, all of one
#' scalar type.
#'
#' @param type An `r8_field_type` of a scalar type (or `option()` of one, for
#'   missing cells), or an R8 struct type.
#' @param nrow Optional Integer: The number of rows.
#' @param ncol Optional Integer: The number of columns.
#'
#' @return An `r8_field_type`.
#'
#' @examples
#' type_matrix(option(type_double()), nrow = 2L)
#'
#' @export
type_matrix <- function(type, nrow = NULL, ncol = NULL) {
  declared(r8_ft_matrix(type, nrow, ncol, NULL, FALSE))
} # /R8::type_matrix


# %% type_array ----
#' Array field type
#'
#' A dense array of a scalar type, an atomic vector with `dim` in R.
#'
#' @param type An `r8_field_type` of a scalar type, or `option()` of one.
#' @param dim Optional Integer: The dimensions.
#'
#' @return An `r8_field_type`.
#'
#' @examples
#' type_array(type_integer(), dim = c(2L, 3L, 4L))
#'
#' @export
type_array <- function(type, dim = NULL) {
  declared(r8_ft_matrix(type, NULL, NULL, dim, FALSE))
} # /R8::type_array


# %% type_sparse_matrix ----
#' Sparse matrix field type
#'
#' A matrix in compressed sparse column form, a `dgCMatrix` (package Matrix)
#' in R.
#'
#' @param type An `r8_field_type`: `type_double()` or `type_logical()`.
#' @param nrow Optional Integer: The number of rows.
#' @param ncol Optional Integer: The number of columns.
#'
#' @return An `r8_field_type`.
#'
#' @examples
#' type_sparse_matrix()
#'
#' @export
type_sparse_matrix <- function(type = type_double(), nrow = NULL, ncol = NULL) {
  declared(r8_ft_matrix(type, nrow, ncol, NULL, TRUE))
} # /R8::type_sparse_matrix


# %% type_table ----
#' Table field type
#'
#' A `data.frame` of equal-length columns, each a scalar type, a factor, or
#' `option()` of one, with automatic row names. An open table also accepts
#' columns its declaration does not name. A table's rules run once per row
#' and read the row's columns by name. Given an R8 struct type, a table whose
#' columns are the struct's fields.
#'
#' @param columns Named list of fields or field types, or an R8 struct type.
#' @param open Logical: Accept columns the declaration does not name.
#' @param rules List: `rule()` calls over a row's columns.
#'
#' @return An `r8_field_type`.
#'
#' @examples
#' type_table(
#'   list(start = type_integer(), width = type_integer(min = 0L)),
#'   rules = list(rule("END_FITS", start + width - 1 <= 2147483647, "The end fits in an integer."))
#' )
#'
#' @export
type_table <- function(columns = list(), open = FALSE, rules = list()) {
  if (inherits(columns, "r8_struct_type")) {
    return(structure(
      list(kind = "table_of", of = attr(columns, "r8_name")),
      class = "r8_field_type"
    ))
  }
  if (
    length(columns) > 0L &&
      (is.null(names(columns)) || any(!nzchar(names(columns))))
  ) {
    r8_abort(
      "Every field of a table is named: name = type_double() or name = field(...).",
      class = "r8_declaration_error"
    )
  }
  declared(r8_ft_table(columns, open, rules))
} # /R8::type_table
