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


# %% dyn ----
#' A field of any implementor of a trait
#'
#' `dyn(Trait)` holds a value of any R8 type that implements `Trait`, or
#' that extends a type that does, as Rust's `dyn Trait`. Its JSON form names
#' the type in the trait's tag member, or else in `"$type"`. `dyn(generic)`,
#' for a generic from [generic()] that dispatches on one argument, holds a
#' value of any type with a method of the generic. A value of a class of
#' another class system is not held, even when the class has an impl of the
#' trait; [r8_convert()] through a `From` impl turns it into an R8 value. A
#' struct and the structs that extend it are [subtype()].
#'
#' @param trait A trait object from [new_trait()], or a generic from
#'   [generic()] that dispatches on one argument.
#'
#' @return An `r8_field_type`.
#'
#' @examples
#' op <- options(r8.namespace = "demo")
#' Planar <- new_trait("Planar", methods = list(area = trait_method(params(x))))
#' Square <- new_struct("Square", fields = list(side = type_double()))
#' impl(Planar, Square, methods = list(area = function(x) x$side^2))
#' Drawing <- new_struct("Drawing", fields = list(shape = dyn(Planar)))
#' Drawing(shape = Square(side = 2))
#' options(op)
#'
#' @export
dyn <- function(trait) {
  if (inherits(trait, "r8_generic")) {
    if (length(attr(trait, "r8_dispatch")) > 1L) {
      r8_abort(
        "dyn() of a generic holds a value with a method of it; `",
        attr(trait, "r8_method"),
        "` dispatches on ",
        length(attr(trait, "r8_dispatch")),
        " arguments, so no single value has one.",
        class = "r8_declaration_error"
      )
    }
    return(structure(
      list(kind = "dyn", `trait` = attr(trait, "r8_trait")),
      class = "r8_field_type"
    ))
  }
  if (inherits(trait, "r8_trait")) {
    return(structure(
      list(kind = "dyn", `trait` = unclass(trait)[["name"]]),
      class = "r8_field_type"
    ))
  }
  if (inherits(trait, "r8_struct_type")) {
    r8_abort(
      "dyn() takes a trait, as Rust's `dyn` does; a struct and the structs ",
      "that extend it are subtype(",
      sub("^.*::", "", attr(trait, "r8_name")),
      ").",
      class = "r8_declaration_error"
    )
  }
  if (inherits(trait, "r8_enum_type")) {
    r8_abort(
      "dyn() takes a trait object; `",
      attr(trait, "r8_name"),
      "` is an enum, a closed set of variants: use it as the field type.",
      class = "r8_declaration_error"
    )
  }
  r8_abort(
    "dyn() takes a trait object, such as dyn(Planar), or a generic of one ",
    "dispatched argument.",
    class = "r8_declaration_error"
  )
} # /R8::dyn


# %% subtype ----
#' A field of a struct or of any struct that extends it
#'
#' `subtype(Struct)` holds a value of `Struct` or of a struct that extends a
#' subtype of it: a type is a subtype of itself and of every ancestor
#' through `extends`. Such a value has the struct's fields and satisfies its
#' rules, and [r8_upcast()] gives its value of `Struct`. A plain struct type
#' holds exactly that struct. Its JSON form names the value's type in
#' `"$type"`. Any implementor of a trait, related by `extends` or not, is
#' [dyn()].
#'
#' @param struct An R8 struct type object.
#'
#' @return An `r8_field_type`.
#'
#' @examples
#' op <- options(r8.namespace = "demo")
#' Range <- new_struct("Range", fields = list(lo = type_double(), hi = type_double()))
#' Window <- new_struct("Window", fields = list(label = type_character()), extends = Range)
#' Plot <- new_struct("Plot", fields = list(x = subtype(Range)))
#' Plot(x = Window(lo = 0, hi = 1, label = "w"))
#' options(op)
#'
#' @export
subtype <- function(struct) {
  if (inherits(struct, "r8_trait") || inherits(struct, "r8_generic")) {
    r8_abort(
      "subtype() takes a struct type object; any implementor of a trait is ",
      "dyn() of the trait.",
      class = "r8_declaration_error"
    )
  }
  if (!inherits(struct, "r8_struct_type")) {
    r8_abort(
      "subtype() takes a struct type object, such as subtype(Interval).",
      class = "r8_declaration_error"
    )
  }
  ensure_registered(struct)
  name <- attr(struct, "r8_name")
  a <- ancestry(name)
  if (isTRUE(a[["stale"]])) {
    stale_abort(name, a[["parent"]], "r8_declaration_error")
  }
  structure(list(kind = "subtype", of = name), class = "r8_field_type")
} # /R8::subtype


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
#' `Option<T>`: the value may be absent, `NULL` for a field and `NA` for an
#' element of a vector or a cell of a table. A struct field of this type may
#' declare who fills an absent value, with [field()]'s `compute` or
#' `context`.
#'
#' @param type An `r8_field_type` or an R8 type: The type when present.
#'
#' @return An `r8_field_type`.
#'
#' @examples
#' option(type_integer(min = 1L))
#'
#' @export
option <- function(type) {
  declared(r8_ft_option(type))
} # /R8::option


# %% btree_map ----
#' Sorted map field type
#'
#' `BTreeMap<String, T>`: unique string keys in code-point order, a named
#' vector or list in R. Two maps with the same keys and values are equal in
#' any order of construction. Canonical JSON writes the keys in RFC 8785's
#' UTF-16 order, which differs from code-point order only for keys that mix
#' characters above U+FFFF with characters from U+E000 to U+FFFF.
#'
#' @param type An `r8_field_type` or an R8 type: The value type.
#' @param keys Optional `r8_field_type`: A `type_character()` constraining
#'   the keys.
#'
#' @return An `r8_field_type`.
#'
#' @examples
#' btree_map(type_integer())
#'
#' @export
btree_map <- function(type, keys = NULL) {
  declared(r8_ft_map(type, keys, FALSE))
} # /R8::btree_map


# %% index_map ----
#' Insertion-ordered map field type
#'
#' `IndexMap<String, T>`: unique string keys in insertion order, a named
#' list in R and an array of `{"name", "value"}` pairs in JSON. Order is part
#' of the value, its equality, and its hash.
#'
#' @param type An `r8_field_type` or an R8 type: The value type.
#' @param keys Optional `r8_field_type`: A `type_character()` constraining
#'   the keys.
#'
#' @return An `r8_field_type`.
#'
#' @examples
#' index_map(type_double())
#'
#' @export
index_map <- function(type, keys = NULL) {
  declared(r8_ft_map(type, keys, TRUE))
} # /R8::index_map


# %% type_matrix ----
#' Matrix field type
#'
#' A dense matrix of a scalar type, an atomic vector with `dim` in R; with
#' `of`, a matrix whose columns are the fields of a struct, all of one scalar
#' type.
#'
#' @param type An `r8_field_type` of a scalar type, or `option()` of one for
#'   missing cells.
#' @param nrow Optional Integer: The number of rows.
#' @param ncol Optional Integer: The number of columns.
#' @param of Optional R8 struct type: The struct whose fields are the
#'   columns, in place of `type`, `nrow`, and `ncol`.
#'
#' @return An `r8_field_type`.
#'
#' @examples
#' type_matrix(option(type_double()), nrow = 2L)
#'
#' @export
type_matrix <- function(type = NULL, nrow = NULL, ncol = NULL, of = NULL) {
  if (!is.null(of)) {
    return(struct_columns(
      "matrix_of",
      of,
      "type_matrix",
      is.null(type) && is.null(nrow) && is.null(ncol)
    ))
  }
  if (inherits(type, "r8_struct_type")) {
    r8_abort(
      "A matrix whose columns are a struct's fields is type_matrix(of = ",
      sub("^.*::", "", attr(type, "r8_name")),
      ").",
      class = "r8_declaration_error"
    )
  }
  declared(r8_ft_matrix(type, nrow, ncol, NULL, FALSE))
} # /R8::type_matrix


# %% struct_columns ----
#' The field type of a matrix or table whose columns are a struct's fields
#'
#' @param kind Character: `"matrix_of"` or `"table_of"`.
#' @param of An R8 struct type object.
#' @param fn Character: The declaring function, for messages.
#' @param alone Logical: `of` is given without the arguments it replaces.
#'
#' @return An `r8_field_type`.
#'
#' @keywords internal
#' @noRd
struct_columns <- function(kind, of, fn, alone) {
  if (!inherits(of, "r8_struct_type")) {
    r8_abort(
      "`of` is an R8 struct type object.",
      class = "r8_declaration_error"
    )
  }
  if (!alone) {
    r8_abort(
      fn,
      "(of = ) takes its columns from the struct and no other argument.",
      class = "r8_declaration_error"
    )
  }
  ensure_registered(of)
  structure(
    list(kind = kind, of = attr(of, "r8_name")),
    class = "r8_field_type"
  )
} # /R8::struct_columns


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
#' and read the row's columns by name. With `of`, a table whose columns are
#' the fields of a struct.
#'
#' @param columns Named list of fields or field types.
#' @param open Logical: Accept columns the declaration does not name.
#' @param rules A `rule()`, or a list of `rule()` calls, over a row's
#'   columns.
#' @param of Optional R8 struct type: The struct whose fields are the columns,
#'   in place of `columns`, `open`, and `rules`.
#'
#' @return An `r8_field_type`.
#'
#' @examples
#' type_table(
#'   list(start = type_integer(), width = type_integer(min = 0L)),
#'   rules = list(rule(
#'     start + width - 1 <= 2147483647,
#'     message = "The end fits in an integer.",
#'     id = "END_FITS"
#'   ))
#' )
#'
#' @export
type_table <- function(
  columns = list(),
  open = FALSE,
  rules = list(),
  of = NULL
) {
  if (!is.null(of)) {
    return(struct_columns(
      "table_of",
      of,
      "type_table",
      length(columns) == 0L && isFALSE(open) && length(rules) == 0L
    ))
  }
  if (inherits(columns, "r8_struct_type")) {
    r8_abort(
      "A table whose columns are a struct's fields is type_table(of = ",
      sub("^.*::", "", attr(columns, "r8_name")),
      ").",
      class = "r8_declaration_error"
    )
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
  declared(r8_ft_table(columns, open, as_rules(rules)))
} # /R8::type_table
