# table.R
# ::R8::
# 2026- EDG rtemis.org

# Tables: the declaration of a table type, the construction of its values, and the
# operations on their columns and rows.
#
# spec: R8/raven

# %% new_table ----
#' Declare a table
#'
#' A table is a type whose values hold columns: vectors of one length, each with a
#' name. A value reads as a list of its columns, with `$`, `[[`, `names()`, and
#' `lapply()`, and as a data frame with `[i, j]`, `dim()`, `head()`, and
#' `as.data.frame()`; `lm()` and the other functions that call
#' `as.data.frame()` take one ([Raven]).
#'
#' `columns` declares the columns that every value has. With `open = TRUE`, a value
#' has other columns too, after the declared ones, of any name and of any of the
#' column types. [Raven] is the open table without declared columns.
#'
#' @param name Character: The type name, an R8 name.
#' @param columns Named list: The columns, each a field type, such as
#'   `type_double(min = 0)`, or a [column()], which takes a unit, a description, and
#'   whether the values are distinct. The cells of a column may be missing where
#'   its type is `option()`.
#' @param open Logical: If TRUE, values have other columns too.
#' @param description Character: The description.
#' @param version Integer: The version, which a migration increments.
#' @param rules List of [rule()]: The rules that every row satisfies. A rule reads
#'   the cells of its row by the names of the columns.
#'
#' @return The type object, a function that constructs values: of a data frame, of
#'   a named list of vectors, of a value of another table, or of columns given by
#'   name.
#'
#' @section Copies:
#' The columns of a data frame and of a list are not copied. The columns of a
#' data.table are, since data.table changes a column in place. An operation on the
#' columns shares them; one on the rows makes new columns. `r8_validate()` checks a
#' value again.
#'
#' @section Operations:
#' `x[i, j]` gives the rows `i` and the columns `j`, always as a table, and `x[j]`
#' gives columns. `i` is a [rule()], whose names are columns, `x[rule(age > 40), ]`, or
#' holds positions, all positive or all negative, or is a logical vector with one value
#' per row; `j` holds names or positions. [r8_filter()] and [r8_select()] are functions
#' for the two. `x$a <- value`
#' replaces a column, adds one to an open table, and, with `NULL`, removes one;
#' the new column is checked, with the rules that read it. A result without a column
#' that its type requires is a [Raven]. `rbind()` binds the rows of values of one
#' type with the same columns.
#'
#' @examples
#' op <- options(r8.namespace = "demo")
#' Vitals <- new_table("Vitals", columns = list(
#'   age = column(type_double(min = 0), unit = "a"),
#'   sbp = type_double(min = 0, max = 400)
#' ), open = TRUE)
#' v <- Vitals(data.frame(age = c(40, 52), sbp = c(120, 140), sex = c("f", "m")))
#' v
#' v$age
#' v[rule(age > 45), c("age", "sex")]
#' try(v$sbp <- c(120, -1))
#' options(op)
#'
#' @export
new_table <- function(
  name,
  columns = list(),
  open = FALSE,
  description = "",
  version = 1L,
  rules = list()
) {
  if (!is_name(name)) {
    r8_abort(
      "A type name is an R8 name, such as \"Vitals\"",
      name_reason(name),
      ".",
      class = "r8_declaration_error"
    )
  }
  if (length(columns) > 0L) {
    check_members(columns, "columns", paste0("`", name, "`"))
  }
  if (!isTRUE(open) && !isFALSE(open)) {
    r8_abort(
      "`open` of `",
      name,
      "` is TRUE or FALSE.",
      class = "r8_declaration_error"
    )
  }
  declare(
    if (open) "open_table" else "table",
    name,
    description,
    version,
    "type",
    columns,
    as_rules(rules),
    parent.frame(),
    "r8_table_type"
  )
} # /R8::new_table


# %% Raven ----
#' A table of any columns
#'
#' `Raven` is a table ([new_table()]) that declares no column and takes any: a data
#' frame without row names, a data.table, a tibble, a named list of vectors, or
#' columns given by name.
#'
#' A column is a logical, an integer, a double, a character vector, a factor, a
#' `Date`, or a `POSIXct`. Cells may be missing, and a double column holds `NaN` and
#' infinite values. The name of a column is any text that does not begin with `$`.
#'
#' @param ... One data frame or list, or the columns by name.
#' @param x An object that `Raven()` takes.
#'
#' @return A value of `R8::Raven`. `as_raven()` returns a raven as it is, and gives
#'   a value of another table as a raven with the units and the descriptions that its
#'   type declares.
#'
#' @examples
#' r <- Raven(iris)
#' dim(r)
#' r[rule(Petal.Width > mean(Petal.Width)), c("Species", "Petal.Width")]
#' coef(lm(Sepal.Length ~ Petal.Length, data = r))
#'
#' @name Raven
#' @export
Raven <- NULL

#' @rdname Raven
#' @export
as_raven <- function(x) {
  if (inherits(x, "R8::Raven") && is_value(x)) {
    return(x)
  }
  Raven(x)
} # /R8::as_raven


# %% table_value ----
#' Construct a value of a table
#'
#' @param qname Character: The qualified type name.
#' @param args List: The constructor's arguments.
#' @param type Function: The constructor.
#'
#' @return The value; signals `r8_value_error`.
#'
#' @keywords internal
#' @noRd
table_value <- function(qname, args, type) {
  given <- table_columns(args, sub("^.*::", "", qname))
  res <- r8_table_new(
    given[["columns"]],
    qname,
    given[["own"]],
    given[["facts"]]
  )
  if (
    inherits(res, "r8_problems") &&
      identical(res[["errors"]][["code"]][1L], "type_unregistered") &&
      ensure_registered(type)
  ) {
    res <- r8_table_new(
      given[["columns"]],
      qname,
      given[["own"]],
      given[["facts"]]
    )
  }
  if (inherits(res, "r8_problems")) {
    construct_error(res, qname, "", inputs = given[["columns"]])
  }
  res
} # /R8::table_value


# %% table_columns ----
#' The columns that a constructor's arguments give
#'
#' @param args List: The arguments.
#' @param name Character: The type's name, for messages.
#'
#' @return List with `columns`, a named list; `own`, whether the value copies them;
#'   and `facts`, those of a table's value, or `NULL`.
#'
#' @keywords internal
#' @noRd
table_columns <- function(args, name) {
  keys <- names(args)
  if (length(args) == 1L && (is.null(keys) || !nzchar(keys))) {
    x <- args[[1L]]
    if (is_value(x) && inherits(x, "r8_table")) {
      about <- r8_table_columns(x)
      facts <- lapply(seq_along(about[["name"]]), function(k) {
        f <- list(
          unit = about[["unit"]][[k]],
          description = about[["description"]][[k]]
        )
        f[!vapply(f, is.na, NA)]
      })
      names(facts) <- about[["name"]]
      return(list(
        columns = as.list(x),
        own = FALSE,
        facts = facts[lengths(facts) > 0L]
      ))
    }
    if (is.data.frame(x)) {
      if (.row_names_info(x) > 0L) {
        r8_abort(
          "The data frame has row names, which a table does not hold; give them ",
          "as a column, ",
          name,
          "(cbind(name = rownames(x), x, row.names = NULL)), or remove them, ",
          name,
          "(r8_conform(x)).",
          class = "r8_value_error"
        )
      }
      own <- inherits(x, "data.table")
      columns <- as.list(unclass(x))
      attributes(columns) <- list(names = names(x))
      return(list(columns = columns, own = own, facts = NULL))
    }
    if (is.list(x) && !is.object(x) && length(x) > 0L) {
      args <- x
      keys <- names(x)
    } else {
      r8_abort(
        name,
        "() takes a data frame, a named list of vectors, or columns by name, such as ",
        name,
        "(a = 1:2)",
        if (is.matrix(x)) "; as.data.frame() gives one of a matrix",
        ".",
        class = "r8_value_error"
      )
    }
  }
  if (
    length(args) > 0L && (is.null(keys) || anyNA(keys) || !all(nzchar(keys)))
  ) {
    r8_abort(
      "Every column of ",
      name,
      "() has a name: ",
      name,
      "(a = 1:2, b = c(\"x\", \"y\")).",
      class = "r8_value_error"
    )
  }
  attributes(args) <- list(names = keys)
  list(columns = args, own = FALSE, facts = NULL)
} # /R8::table_columns


# %% table_result ----
#' The result of an operation on a table
#'
#' @param res The value, or what refuses it.
#' @param x The table.
#'
#' @return `res`; signals `r8_value_error`.
#'
#' @keywords internal
#' @noRd
table_result <- function(res, x) {
  if (inherits(res, "r8_refused")) {
    message <- res[["message"]]
    substr(message, 1L, 1L) <- toupper(substr(message, 1L, 1L))
    r8_abort(message, ".", class = "r8_value_error")
  }
  if (inherits(res, "r8_problems")) {
    construct_error(res, value_type(x), "")
  }
  res
} # /R8::table_result


# %% column_closing ----
#' What an assignment that a declared column refuses may be instead
#'
#' A column of a declared type is checked when it is replaced. The result of a
#' calculation on it, such as a standardized one, is another quantity.
#'
#' @param x The table.
#' @param name Character: The column.
#' @param errors List: The problems.
#'
#' @return Character, or `NULL` for a problem that another name does not meet.
#'
#' @keywords internal
#' @noRd
column_closing <- function(x, name, errors) {
  other <- c(
    "column_length",
    "column_name",
    "column_type",
    "declaration_changed",
    "unknown_column"
  )
  if (any(errors[["code"]] %in% other)) {
    return(NULL)
  }
  about <- r8_table_columns(x)
  if (!name %in% about[["name"]][about[["declared"]]]) {
    return(NULL)
  }
  paste0(
    "`",
    name,
    "` is declared by `",
    value_type(x),
    "`. A transformed column takes another name, or as_raven(x) gives the ",
    "columns without the type."
  )
} # /R8::column_closing


# %% table_of ----
#' Check that an object is a value of a table
#'
#' @param x Object.
#' @param arg Character: Its name, for the message.
#'
#' @return `x`, invisibly; signals `r8_argument_error`.
#'
#' @keywords internal
#' @noRd
check_table <- function(x, arg = "x") {
  if (!is_value(x) || !inherits(x, "r8_table")) {
    r8_abort(
      "`",
      arg,
      "` is a value of a table, such as Raven(data).",
      class = "r8_argument_error"
    )
  }
  invisible(x)
} # /R8::check_table


# %% [.r8_table ----
#' @export
`[.r8_table` <- function(x, i, j, ..., drop = FALSE) {
  if (!is_value(x)) {
    r8_abort("`x` ", not_value_text(x), class = "r8_value_error")
  }
  if (!isFALSE(drop) || ...length() > 0L) {
    r8_abort(
      "x[i, j] of a table is a table; x$name gives a column as a vector.",
      class = "r8_value_error"
    )
  }
  env <- parent.frame()
  n <- nargs() - !missing(drop)
  if (n <= 2L) {
    if (missing(i)) {
      return(x)
    }
    cols <- index_value(i, x, substitute(i), env, "x[c(%s)]")
    if (inherits(cols, "r8_rule")) {
      r8_abort(
        "A rule chooses rows, which are asked for before a comma: x[",
        paste(deparse(substitute(i), width.cutoff = 60L), collapse = " "),
        ", ].",
        class = "r8_value_error"
      )
    }
    return(table_result(r8_table_get(x, NULL, cols), x))
  }
  rows <- if (!missing(i)) {
    row_index(x, index_value(i, x, substitute(i), env, "rule"), substitute(i))
  }
  cols <- if (!missing(j)) {
    index_value(j, x, substitute(j), env, "x[, c(%s)]")
  }
  table_result(r8_table_get(x, rows, cols), x)
} # /R8::[.r8_table


# %% index_value ----
#' The value of an index
#'
#' An index is R's: a name in it is a variable. Where R has none of a name that is
#' a column, the error gives the form that reads the column.
#'
#' @param value The index, not yet evaluated.
#' @param x The table.
#' @param e Language object: The index as written.
#' @param env Environment: The caller's.
#' @param form Character: `"rule"` for an index of rows; otherwise the form of an
#'   index of columns, with `%s` for their names.
#'
#' @return The value.
#'
#' @keywords internal
#' @noRd
index_value <- function(value, x, e, env, form) {
  tryCatch(
    value,
    error = function(cond) {
      names <- all.vars(e)
      names <- names[names %in% base::names(x)]
      lost <- names[!vapply(names, exists, NA, envir = env)]
      if (length(lost) == 0L) {
        stop(cond)
      }
      text <- paste(deparse(e, width.cutoff = 60L), collapse = " ")
      r8_abort(
        "R has no variable `",
        lost[[1L]],
        "`, which is a column of the table; ",
        if (identical(form, "rule")) {
          paste0("a condition on columns is a rule: rule(", text, ")")
        } else {
          paste0(
            "columns are asked for by name: ",
            sprintf(form, paste0("\"", lost, "\"", collapse = ", "))
          )
        },
        ".",
        class = "r8_value_error"
      )
    }
  )
} # /R8::index_value


# %% row_index ----
#' The rows that an index asks for
#'
#' @param x The table.
#' @param i The index's value.
#' @param e Language object: The index as written.
#'
#' @return Positions, a logical vector, or a list of one rule.
#'
#' @keywords internal
#' @noRd
row_index <- function(x, i, e) {
  if (inherits(i, "r8_rule")) {
    return(list(i))
  }
  if (is.logical(i) && length(i) == nrow(x) && anyNA(i)) {
    k <- sum(is.na(i))
    r8_abort(
      "The condition is missing (NA) in ",
      format(k, big.mark = ","),
      if (k == 1L) " row" else " rows",
      "; which(",
      paste(deparse(e, width.cutoff = 60L), collapse = " "),
      ") leaves ",
      if (k == 1L) "it" else "them",
      " out.",
      class = "r8_value_error"
    )
  }
  i
} # /R8::row_index


# %% r8_filter ----
#' Choose rows and columns of a table
#'
#' `r8_filter()` gives the rows of a table ([new_table()]) where a [rule()] is true,
#' and `r8_select()` gives columns. Neither changes `x`, and `r8_select()` copies no
#' column. `x[rule, ]` and `x[rule, columns]` are the same.
#'
#' A rule is a value: one rule filters every table that has the columns it names.
#' `!`, `&`, and `|` make a rule of others.
#'
#' A bare name in a rule is a column, and `val()` is the value of an R expression
#' ([val()]). The rule is checked against the columns and their types before a row is
#' read.
#'
#' @section Missing cells:
#' A row is kept where the rule is true. Where a cell is missing or `NaN`, a
#' comparison is missing, and the row is left out, by the rule and by its negation;
#' `is.na()` tests for such a cell. `FALSE && NA` is false and `TRUE || NA` true, as in
#' R.
#'
#' @section Whole columns:
#' `mean(x)`, `sum(x)`, `min(x)`, and `max(x)` read the whole column `x`, and are
#' missing where one of its cells is. `pmin(a, b)` and `pmax(a, b)` are the least and
#' the greatest cell of the row.
#'
#' @section Errors:
#' A division by zero, `sqrt()` of a negative number, and `log()` of a number that is
#' not positive are errors, unless another part decides the row: `x > 0 && log(x) > 1`
#' is false where `x` is 0.
#'
#' @param x A value of a table.
#' @param rule A [rule()] without `when`. Its `id` and `message` are not read.
#' @param columns Character or numeric: The columns, by name or by position, each
#'   once.
#'
#' @return A value of the type of `x`. `r8_select()` without a column that the type
#'   requires gives a [Raven].
#'
#' @examples
#' r <- Raven(iris)
#' wide <- rule(Petal.Width > mean(Petal.Width))
#' r8_filter(r, wide)
#' r8_filter(r, wide & rule(Species == "versicolor"))
#' r8_select(r, c("Species", "Petal.Width"))
#' r[!wide, c("Species", "Petal.Width")]
#'
#' @export
r8_filter <- function(x, rule) {
  check_table(x)
  rule <- index_value(rule, x, substitute(rule), parent.frame(), "rule")
  if (!inherits(rule, "r8_rule")) {
    r8_abort(
      "`rule` is a rule, such as rule(age >= 18).",
      class = "r8_argument_error"
    )
  }
  table_result(r8_table_get(x, list(rule), NULL), x)
} # /R8::r8_filter


# %% r8_select ----
#' @rdname r8_filter
#' @export
r8_select <- function(x, columns) {
  check_table(x)
  columns <- index_value(
    columns,
    x,
    substitute(columns),
    parent.frame(),
    "c(%s)"
  )
  positions <- is.numeric(columns) &&
    !is.object(columns) &&
    all(is.finite(columns)) &&
    all(columns >= 1)
  if (!is.character(columns) && !positions) {
    r8_abort(
      "`columns` are names, such as c(\"age\", \"sex\"), or positions, such as 1:2.",
      class = "r8_argument_error"
    )
  }
  table_result(r8_table_get(x, NULL, columns), x)
} # /R8::r8_select


# %% [<-.r8_table ----
#' @export
`[<-.r8_table` <- function(x, i, j, ..., value) {
  if (nargs() > 3L) {
    r8_abort(
      "The cells of a table are replaced by column: x$a <- replace(x$a, i, value).",
      class = "r8_value_error"
    )
  }
  NextMethod()
} # /R8::[<-.r8_table


# %% dim.r8_table ----
#' @export
dim.r8_table <- function(x) {
  # A plain integer vector, as ggplot2 requires.
  n <- length(x)
  c(if (n == 0L) 0L else length(.subset2(x, 1L)), n)
} # /R8::dim.r8_table


# %% dimnames.r8_table ----
#' @export
dimnames.r8_table <- function(x) {
  list(NULL, names(x))
} # /R8::dimnames.r8_table


# %% as.data.frame.r8_table ----
#' @export
as.data.frame.r8_table <- function(
  x,
  row.names = NULL,
  optional = FALSE,
  ...
) {
  out <- as.list(x)
  attributes(out) <- list(
    names = names(out),
    class = "data.frame",
    row.names = .set_row_names(nrow(x))
  )
  if (!is.null(row.names)) {
    attr(out, "row.names") <- row.names
  }
  out
} # /R8::as.data.frame.r8_table


# %% head.r8_table ----
#' @exportS3Method utils::head
head.r8_table <- function(x, n = 6L, ...) {
  rows <- nrow(x)
  k <- if (n < 0L) max(rows + n, 0L) else min(n, rows)
  x[seq_len(k), ]
} # /R8::head.r8_table


# %% tail.r8_table ----
#' @exportS3Method utils::tail
tail.r8_table <- function(x, n = 6L, ...) {
  rows <- nrow(x)
  k <- if (n < 0L) max(rows + n, 0L) else min(n, rows)
  x[seq.int(to = rows, length.out = k), ]
} # /R8::tail.r8_table


# %% rbind.r8_table ----
#' @export
rbind.r8_table <- function(..., deparse.level = 1) {
  xs <- unname(list(...))
  xs <- xs[!vapply(xs, is.null, NA)]
  for (x in xs) {
    if (!is_value(x) || !inherits(x, "r8_table")) {
      r8_abort(
        "rbind() of a table takes values of one table; Raven() gives one of a data frame.",
        class = "r8_value_error"
      )
    }
  }
  if (length(xs) == 1L) {
    return(xs[[1L]])
  }
  table_result(r8_table_bind(xs), xs[[1L]])
} # /R8::rbind.r8_table


# %% r8_columns ----
#' The columns of a table
#'
#' `r8_columns()` describes the columns of a value of a table ([new_table()]), and
#' `r8_set_columns()` gives a value with the unit or the description of a column that
#' its type does not declare. Those of a declared column are its type's.
#'
#' @param x A value of a table.
#' @param columns Named list: By the name of a column, a list with `unit`, a UCUM
#'   code by convention, and `description`, each one string, or `NULL` to remove it;
#'   `NULL` in place of the list removes both.
#'
#' @return `r8_columns()`, a data frame with the variables `name`, `type`, `unit`,
#'   `description`, and `declared`. `r8_set_columns()`, the new value.
#'
#' @examples
#' r <- Raven(age = c(40, 52), sex = c("f", "m"))
#' r <- r8_set_columns(
#'   r,
#'   list(age = list(unit = "a", description = "Age at admission"))
#' )
#' r
#' r8_columns(r)
#'
#' @export
r8_columns <- function(x) {
  check_table(x)
  out <- r8_table_columns(x)
  attr(out, "row.names") <- .set_row_names(length(out[["name"]]))
  class(out) <- "data.frame"
  out
} # /R8::r8_columns


# %% r8_set_columns ----
#' @rdname r8_columns
#' @export
r8_set_columns <- function(x, columns) {
  check_table(x)
  changes <- columns
  about <- r8_table_columns(x)
  keys <- names(changes)
  if (
    !is.list(changes) ||
      is.object(changes) ||
      (length(changes) > 0L && (is.null(keys) || !all(nzchar(keys))))
  ) {
    r8_abort(
      "`columns` is a list by the names of columns: ",
      "r8_set_columns(x, list(age = list(unit = \"a\"))).",
      class = "r8_argument_error"
    )
  }
  facts <- list()
  for (k in seq_along(about[["name"]])[!about[["declared"]]]) {
    f <- list(
      unit = about[["unit"]][[k]],
      description = about[["description"]][[k]]
    )
    facts[[about[["name"]][[k]]]] <- f[!vapply(f, is.na, NA)]
  }
  for (key in keys) {
    at <- match(key, about[["name"]])
    if (is.na(at)) {
      r8_abort(
        "`",
        key,
        "` is not a column of `x`.",
        class = "r8_argument_error"
      )
    }
    if (about[["declared"]][[at]]) {
      r8_abort(
        "Column `",
        key,
        "` is declared by `",
        value_type(x),
        "`, with its unit and description; as_raven(x) gives a value without them.",
        class = "r8_argument_error"
      )
    }
    facts[[key]] <- column_facts(changes[[key]], facts[[key]], key)
  }
  facts <- facts[lengths(facts) > 0L]
  r8_table_set_facts(x, if (length(facts) > 0L) facts)
} # /R8::r8_set_columns


# %% column_facts ----
#' The facts of a column after a change
#'
#' @param change List or `NULL`.
#' @param held List: The facts of the column.
#' @param key Character: The column, for messages.
#'
#' @return List.
#'
#' @keywords internal
#' @noRd
column_facts <- function(change, held, key) {
  if (is.null(change)) {
    return(list())
  }
  if (
    !is.list(change) ||
      is.null(names(change)) ||
      !all(names(change) %in% c("unit", "description"))
  ) {
    r8_abort(
      "What is set of column `",
      key,
      "` is a list with `unit` and `description`: ",
      key,
      " = list(unit = \"a\").",
      class = "r8_argument_error"
    )
  }
  for (fact in names(change)) {
    v <- change[[fact]]
    if (is.null(v)) {
      held[[fact]] <- NULL
      next
    }
    blank <- !is.character(v) ||
      length(v) != 1L ||
      is.na(v) ||
      !nzchar(v) ||
      (identical(fact, "unit") && !identical(trimws(v), as.character(v)))
    if (blank) {
      r8_abort(
        "The ",
        fact,
        " of column `",
        key,
        "` is one string that is not empty",
        if (identical(fact, "unit")) " and has no surrounding spaces",
        ".",
        class = "r8_argument_error"
      )
    }
    held[[fact]] <- as.character(v)
  }
  held
} # /R8::column_facts
