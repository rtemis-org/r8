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
#' for the two; [r8_sort()] orders rows, and [r8_set()] and the functions with it change
#' columns. `x$a <- value`
#' replaces a column, adds one to an open table, and, with `NULL`, removes one;
#' the new column is checked, with the rules that read it. A result without a column
#' that its type requires is a [Raven]. `rbind()` binds the rows of values of one
#' type with the same columns and units.
#'
#' `as.matrix()`, `t()`, `is.na()`, `summary()`, `duplicated()`, and other functions of
#' R read a table as a data frame; `unique()`, `split()`, and `na.omit()` give tables.
#' `cbind()`, `transform()`, `within()`, `subset()`, and `sort()` are errors that name
#' the function of R8. `merge()` gives a data frame.
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
  check_flag(open, "open")
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
#' @return A value of `R8::Raven`: of a raven, the same value, and of a value of
#'   another table, its columns, with the units and the descriptions that its type
#'   declares.
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
  if (
    identical(qname, "R8::Raven") &&
      length(args) == 1L &&
      is.null(names(args)) &&
      inherits(args[[1L]], "R8::Raven") &&
      is_value(args[[1L]])
  ) {
    return(args[[1L]])
  }
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
      require_current(x)
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
    require_current(x)
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
    "`. A transformed column takes another name, or Raven(x) gives the ",
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
#' a column, the error gives the form that reads the column. A name in a `rule()` is
#' no variable.
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
      if (inherits(cond, "r8_error")) {
        stop(cond)
      }
      names <- index_names(e)
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


# %% index_names ----
#' The variables of an index, without the names in its rules
#'
#' @param e Language object.
#'
#' @return Character.
#'
#' @keywords internal
#' @noRd
index_names <- function(e) {
  if (!is.call(e)) {
    return(all.vars(e))
  }
  if (is_rule_fn(e[[1L]], "rule")) {
    return(character())
  }
  unique(unlist(lapply(as.list(e), index_names)))
} # /R8::index_names


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
  columns <- table_columns_arg(x, columns, substitute(columns), parent.frame())
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


# %% r8_to_data_frame ----
#' A table as a data frame
#'
#' The columns of a table ([new_table()]) as a data frame, which shares them.
#' `as.data.frame()` of a table, which `lm()` and other functions of R call, is the
#' same.
#'
#' @param x A value of a table.
#'
#' @return A data frame.
#'
#' @examples
#' r8_to_data_frame(Raven(a = 1:3))
#'
#' @export
r8_to_data_frame <- function(x) {
  check_table(x)
  as.data.frame(x)
} # /R8::r8_to_data_frame


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


# %% base functions ----
# The functions of R that read a data frame read the columns of a table as one.

#' @export
as.matrix.r8_table <- function(x, ...) as.matrix(as.data.frame(x), ...)

#' @export
t.r8_table <- function(x) t(as.matrix(x))

#' @export
is.na.r8_table <- function(x) is.na(as.data.frame(x))

#' @export
summary.r8_table <- function(object, ...) summary(as.data.frame(object), ...)

#' @export
duplicated.r8_table <- function(x, incomparables = FALSE, ...) {
  duplicated(as.data.frame(x), incomparables = incomparables, ...)
}

#' @export
anyDuplicated.r8_table <- function(x, incomparables = FALSE, ...) {
  anyDuplicated(as.data.frame(x), incomparables = incomparables, ...)
}

#' @exportS3Method stats::model.frame
model.frame.r8_table <- function(formula, ...) {
  stats::model.frame(as.data.frame(formula), ...)
}

# Those that give rows give rows of a table.

#' @export
unique.r8_table <- function(x, incomparables = FALSE, ...) {
  x[!duplicated(x, incomparables = incomparables, ...), ]
}

#' @export
split.r8_table <- function(x, f, drop = FALSE, ...) {
  split.data.frame(x, f, drop = drop, ...)
}

#' @exportS3Method stats::na.omit
na.omit.r8_table <- function(object, ...) {
  x <- as.data.frame(object)
  object[if (length(x) > 0L) stats::complete.cases(x) else logical(), ]
}

# Those that change columns have a function of R8.

#' @export
cbind.r8_table <- function(..., deparse.level = 1) {
  r8_abort(
    "r8_set(x, list(name = value)) adds columns to a table, and r8_set(x, y) those ",
    "of another table.",
    class = "r8_value_error"
  )
}

#' @export
transform.r8_table <- function(`_data`, ...) {
  r8_abort(
    "r8_set(x, list(name = value)) adds and replaces columns of a table.",
    class = "r8_value_error"
  )
}

#' @export
within.r8_table <- function(data, expr, ...) {
  r8_abort(
    "r8_set(x, list(name = value)) adds and replaces columns of a table.",
    class = "r8_value_error"
  )
}

#' @export
sort.r8_table <- function(x, decreasing = FALSE, ...) {
  r8_abort(
    "r8_sort(x, by) orders the rows of a table.",
    class = "r8_value_error"
  )
}

#' @export
xtfrm.r8_table <- function(x) {
  r8_abort(
    "r8_sort(x, by) orders the rows of a table.",
    class = "r8_value_error"
  )
}

#' @export
subset.r8_table <- function(x, ...) {
  r8_abort(
    "r8_filter(x, rule(...)) chooses rows of a table, and r8_select(x, names) columns.",
    class = "r8_value_error"
  )
}


# %% r8_set ----
#' Change the columns of a table
#'
#' `r8_set()` gives a table ([new_table()]) with columns added or replaced,
#' `r8_drop()` without columns, and `r8_rename()` with columns under other names. Each
#' gives a new value, which shares the columns it keeps, and leaves `x` as it is.
#'
#' @section Columns of a declared table:
#' A declared column that is replaced, and a column that takes its name, is checked,
#' with the rules that read it. A table without a column that its type requires is a
#' [Raven], with the units and descriptions of the declaration.
#'
#' @section Units and descriptions:
#' A column that is added or replaced has none. A renamed column keeps its own.
#'
#' @param x A value of a table.
#' @param columns `r8_set()`: Named list, data frame, or table: The columns, each a
#'   vector with one element for each row. `r8_drop()`: Character or numeric: The
#'   columns, by name or by position, each once.
#' @param from Character or numeric: The columns, by name or by position, each once.
#' @param to Character: The new names, one for each column of `from`.
#'
#' @return A value of the type of `x`, or a [Raven].
#'
#' @examples
#' r <- Raven(weight = c(60, 82), height = c(1.62, 1.80))
#' r <- r8_set(r, list(bmi = r$weight / r$height^2))
#' r <- r8_rename(r, "weight", "mass")
#' r8_drop(r, "height")
#'
#' @export
r8_set <- function(x, columns) {
  check_table(x)
  if (is_value(columns) && inherits(columns, "r8_table")) {
    columns <- as.list(columns)
  }
  keys <- names(columns) %||% character()
  if (
    !is.list(columns) ||
      (is.object(columns) && !is.data.frame(columns)) ||
      length(keys) != length(columns) ||
      anyNA(keys) ||
      !all(nzchar(keys))
  ) {
    r8_abort(
      "`columns` is a named list: r8_set(x, list(bmi = w / h^2)).",
      class = "r8_argument_error"
    )
  }
  own <- inherits(columns, "data.table")
  columns <- as.list(unclass(columns))
  attributes(columns) <- list(names = keys)
  if (anyDuplicated(keys) > 0L) {
    r8_abort(
      "Two entries of `columns` are named `",
      keys[[anyDuplicated(keys)]],
      "`.",
      class = "r8_argument_error"
    )
  }
  absent <- vapply(columns, is.null, NA)
  if (any(absent)) {
    r8_abort(
      "Column `",
      keys[absent][[1L]],
      "` is NULL; r8_drop() gives the table without a column.",
      class = "r8_argument_error"
    )
  }
  if (own) {
    # data.table changes its columns in place.
    columns <- lapply(columns, function(v) {
      v <- rep_len(v, length(v))
      v
    })
  }
  if (length(columns) == 0L) {
    return(x)
  }
  res <- .Call(wrap__r8_set_fields, x, keys, columns)
  if (!is_value(res)) {
    if (inherits(res, "r8_problems")) {
      construct_error(
        res,
        value_type(x),
        "",
        inputs = columns,
        assignment = "r8_set()",
        closing = if (length(keys) == 1L) {
          column_closing(x, keys, res[["errors"]])
        }
      )
    }
    fields_refused(x, keys, columns, res)
  }
  res
} # /R8::r8_set


# %% r8_drop ----
#' @rdname r8_set
#' @export
r8_drop <- function(x, columns) {
  check_table(x)
  columns <- table_columns_arg(x, columns, substitute(columns), parent.frame())
  # Positions and names are read as r8_select() reads them.
  at <- match(
    names(table_result(r8_table_get(x, NULL, columns), x)),
    names(x)
  )
  table_result(r8_table_get(x, NULL, setdiff(seq_along(x), at)), x)
} # /R8::r8_drop


# %% r8_rename ----
#' @rdname r8_set
#' @export
r8_rename <- function(x, from, to) {
  check_table(x)
  from <- table_columns_arg(x, from, substitute(from), parent.frame(), "from")
  if (!is.character(to) || is.object(to) || anyNA(to)) {
    r8_abort(
      "`to` holds names, such as c(\"age\", \"sex\").",
      class = "r8_argument_error"
    )
  }
  table_result(r8_table_rename(x, from, to), x)
} # /R8::r8_rename


# %% r8_sort ----
#' Order the rows of a table
#'
#' Rows in the order of one or more columns: numbers, dates, and date-times by
#' value, logicals from `FALSE`, strings by the code points of their characters, as in
#' every host and locale, and factors by their levels. Missing cells and `NaN` come
#' last, also in a column that descends. Rows that are equal in every column of `by`
#' stay in the order they are in.
#'
#' @param x A value of a table.
#' @param by Character or numeric: The columns, the first foremost, by name or by
#'   position.
#' @param descending Optional character or numeric: The columns of `by` that descend.
#'
#' @return A value of the type of `x`.
#'
#' @examples
#' r <- Raven(site = c("b", "a", "b"), age = c(40, 52, 61))
#' r8_sort(r, c("site", "age"), descending = "age")
#'
#' @export
r8_sort <- function(x, by, descending = NULL) {
  check_table(x)
  by <- table_columns_arg(x, by, substitute(by), parent.frame(), "by")
  if (!is.null(descending)) {
    descending <- table_columns_arg(
      x,
      descending,
      substitute(descending),
      parent.frame(),
      "descending"
    )
  }
  table_result(r8_table_sort(x, by, descending %||% character()), x)
} # /R8::r8_sort


# %% table_columns_arg ----
#' The columns that an argument asks for
#'
#' @param x The table.
#' @param value The argument, not yet evaluated.
#' @param e Language object: The argument as written.
#' @param env Environment: The caller's.
#' @param arg Character: The argument's name.
#'
#' @return Names or positive positions; signals `r8_argument_error`.
#'
#' @keywords internal
#' @noRd
table_columns_arg <- function(x, value, e, env, arg = "columns") {
  value <- index_value(value, x, e, env, "c(%s)")
  positions <- is.numeric(value) &&
    !is.object(value) &&
    all(is.finite(value)) &&
    all(value >= 1)
  if (!is.character(value) && !positions) {
    r8_abort(
      "`",
      arg,
      "` are names, such as c(\"age\", \"sex\"), or positions, such as 1:2.",
      class = "r8_argument_error"
    )
  }
  value
} # /R8::table_columns_arg


# %% r8_columns ----
#' The columns of a table
#'
#' `r8_columns()` describes the columns of a value of a table ([new_table()]), and
#' `r8_annotate()` gives a value with the units or the descriptions of columns that
#' its type does not declare. Those of a declared column are its type's. A column that
#' is replaced has neither.
#'
#' @param x A value of a table.
#' @param unit,description Optional named character: The units, UCUM codes by
#'   convention, and the descriptions, by the names of columns; `NA` removes one.
#'
#' @return `r8_columns()`, a data frame with the variables `name`, `type`, `unit`,
#'   `description`, and `declared`. `r8_annotate()`, the new value.
#'
#' @examples
#' r <- Raven(age = c(40, 52), sex = c("f", "m"))
#' r <- r8_annotate(
#'   r,
#'   unit = c(age = "a"),
#'   description = c(age = "Age at admission")
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


# %% r8_annotate ----
#' @rdname r8_columns
#' @export
r8_annotate <- function(x, unit = NULL, description = NULL) {
  check_table(x)
  about <- r8_table_columns(x)
  facts <- list()
  for (k in which(!about[["declared"]])) {
    f <- list(
      unit = about[["unit"]][[k]],
      description = about[["description"]][[k]]
    )
    facts[[about[["name"]][[k]]]] <- f[!vapply(f, is.na, NA)]
  }
  given <- list(unit = unit, description = description)
  for (fact in names(given)) {
    facts <- column_facts(facts, fact, given[[fact]], about, x)
  }
  facts <- facts[lengths(facts) > 0L]
  r8_table_set_facts(x, if (length(facts) > 0L) facts)
} # /R8::r8_annotate


# %% column_facts ----
#' The facts of columns after a change
#'
#' @param held List: The facts by the names of columns.
#' @param fact Character: The fact.
#' @param given Named character, or `NULL`: The fact by the names of columns.
#' @param about List: The columns of the table.
#' @param x The table.
#'
#' @return List; signals `r8_argument_error`.
#'
#' @keywords internal
#' @noRd
column_facts <- function(held, fact, given, about, x) {
  if (is.null(given)) {
    return(held)
  }
  refuse <- function(...) r8_abort(..., class = "r8_argument_error")
  keys <- names(given)
  if (
    (!is.character(given) && !(is.logical(given) && all(is.na(given)))) ||
      is.object(given) ||
      is.null(keys) ||
      anyNA(keys) ||
      !all(nzchar(keys)) ||
      anyDuplicated(keys) > 0L
  ) {
    refuse(
      "`",
      fact,
      "` is a character vector by the names of columns: ",
      fact,
      " = c(age = ",
      if (identical(fact, "unit")) "\"a\"" else "\"Age at admission\"",
      ")."
    )
  }
  for (key in keys) {
    at <- match(key, about[["name"]])
    if (is.na(at)) {
      refuse("`", key, "` is not a column of `x`.")
    }
    if (about[["declared"]][[at]]) {
      refuse(
        "Column `",
        key,
        "` is declared by `",
        value_type(x),
        "`, with its unit and description; Raven(x) gives a value without them."
      )
    }
    v <- as.character(given[[key]])
    if (is.na(v)) {
      held[[key]][[fact]] <- NULL
      next
    }
    if (!nzchar(v) || (identical(fact, "unit") && !identical(trimws(v), v))) {
      refuse(
        "The ",
        fact,
        " of column `",
        key,
        "` is not empty",
        if (identical(fact, "unit")) " and has no surrounding spaces",
        "."
      )
    }
    of <- as.list(held[[key]])
    of[[fact]] <- v
    held[[key]] <- of
  }
  held
} # /R8::column_facts
