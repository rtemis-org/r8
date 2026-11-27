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
#' columns shares them; one on the rows makes new columns. `r8validate()` checks a
#' value again.
#'
#' @section Operations:
#' `x[i, j]` gives the rows `i` and the columns `j`, always as a table, and `x[j]`
#' gives columns. `i` is a [rule()], whose names are columns, `x[rule(age > 40), ]`, or
#' holds positions, all positive or all negative, or is a logical vector with one value
#' per row; `j` holds names or positions. [r8filter()] and [r8select()] are functions
#' for the two; [r8sort()] orders rows, and [r8set()] and the functions with it change
#' columns. `x$a <- value`
#' replaces a column, adds one to an open table, and, with `NULL`, removes one;
#' the new column is checked, with the rules that read it. A result without a column
#' that its type requires is a [Raven]. `rbind()` binds the rows of values of one
#' type with the same columns and units.
#'
#' `as.matrix()`, `t()`, `is.na()`, `summary()`, `duplicated()`, and other functions of
#' R read a table as a data frame; `unique()`, `split()`, and `na.omit()` give tables.
#' `cbind()`, `transform()`, `within()`, `subset()`, `sort()`, and `aggregate()` are
#' errors that name the function of R8. `merge()` gives a data frame.
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
          "(r8conform(x)).",
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
    return(table_result(r8_table_get(x, NULL, cols, threads_of(NULL)), x))
  }
  rows <- if (!missing(i)) {
    row_index(x, index_value(i, x, substitute(i), env, "rule"), substitute(i))
  }
  cols <- if (!missing(j)) {
    index_value(j, x, substitute(j), env, "x[, c(%s)]")
  }
  table_result(r8_table_get(x, rows, cols, threads_of(NULL)), x)
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


# %% r8filter ----
#' Choose rows and columns of a table
#'
#' `r8filter()` gives the rows of a table ([new_table()]) where a [rule()] is true,
#' and `r8select()` gives columns. Neither changes `x`, and `r8select()` copies no
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
#' @param n_threads Optional Integer [1, Inf): The threads that the work in Rust may
#'   use; `NULL` takes `getOption("r8.n_threads")`, whose default is half the logical
#'   cores, at most 2 where `R CMD check` limits the cores, and at most
#'   `OMP_THREAD_LIMIT`. A result is the same for any number of threads.
#' @param columns Character or numeric: The columns, by name or by position, each
#'   once.
#'
#' @return A value of the type of `x`. `r8select()` without a column that the type
#'   requires gives a [Raven].
#'
#' @examples
#' r <- Raven(iris)
#' wide <- rule(Petal.Width > mean(Petal.Width))
#' r8filter(r, wide)
#' r8filter(r, wide & rule(Species == "versicolor"))
#' r8select(r, c("Species", "Petal.Width"))
#' r[!wide, c("Species", "Petal.Width")]
#'
#' @export
r8filter <- function(x, rule, n_threads = NULL) {
  check_table(x)
  rule <- index_value(rule, x, substitute(rule), parent.frame(), "rule")
  if (!inherits(rule, "r8_rule")) {
    r8_abort(
      "`rule` is a rule, such as rule(age >= 18).",
      class = "r8_argument_error"
    )
  }
  table_result(r8_table_get(x, list(rule), NULL, threads_of(n_threads)), x)
} # /R8::r8filter


# %% r8select ----
#' @rdname r8filter
#' @export
r8select <- function(x, columns) {
  check_table(x)
  columns <- table_columns_arg(x, columns, substitute(columns), parent.frame())
  table_result(r8_table_get(x, NULL, columns, 1L), x)
} # /R8::r8select


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


# %% r8to_data_frame ----
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
#' r8to_data_frame(Raven(a = 1:3))
#'
#' @export
r8to_data_frame <- function(x) {
  check_table(x)
  as.data.frame(x)
} # /R8::r8to_data_frame


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
    "r8set(x, list(name = value)) adds columns to a table, and r8set(x, y) those ",
    "of another table.",
    class = "r8_value_error"
  )
}

#' @export
transform.r8_table <- function(`_data`, ...) {
  r8_abort(
    "r8set(x, list(name = value)) adds and replaces columns of a table.",
    class = "r8_value_error"
  )
}

#' @export
within.r8_table <- function(data, expr, ...) {
  r8_abort(
    "r8set(x, list(name = value)) adds and replaces columns of a table.",
    class = "r8_value_error"
  )
}

#' @export
sort.r8_table <- function(x, decreasing = FALSE, ...) {
  r8_abort(
    "r8sort(x, by) orders the rows of a table.",
    class = "r8_value_error"
  )
}

#' @export
xtfrm.r8_table <- function(x) {
  r8_abort(
    "r8sort(x, by) orders the rows of a table.",
    class = "r8_value_error"
  )
}

#' @exportS3Method stats::aggregate
aggregate.r8_table <- function(x, ...) {
  r8_abort(
    "r8summarize(x, list(name = calc(...)), by) summarizes the groups of a ",
    "table's rows.",
    class = "r8_value_error"
  )
}

#' @export
subset.r8_table <- function(x, ...) {
  r8_abort(
    "r8filter(x, rule(...)) chooses rows of a table, and r8select(x, names) columns.",
    class = "r8_value_error"
  )
}


# %% r8set ----
#' Change the columns of a table
#'
#' `r8set()` gives a table ([new_table()]) with columns added or replaced,
#' `r8drop()` without columns, and `r8rename()` with columns under other names. Each
#' gives a new value, which shares the columns it keeps, and leaves `x` as it is.
#'
#' @section Columns of a declared table:
#' A declared column that is replaced, and a column that takes its name, is checked,
#' with the rules that read it. A table without a column that its type requires is a
#' [Raven], with the units and descriptions of the declaration.
#'
#' @section Computed columns:
#' An entry of `columns` that is a [calc()] is computed in Rust over the rows of
#' `x` as given, before any column changes. An aggregate in it is over all rows,
#' or, with `by`, over the rows of each row's group: `r8set(x, list(n_ward =
#' calc(n())), by = "ward")` writes the size of each row's ward into the row.
#'
#' @section Units and descriptions:
#' A column that is added or replaced from a vector has none; one computed by
#' [calc()] has the unit that the expression infers, and no description. A renamed
#' column keeps its own.
#'
#' @param x A value of a table.
#' @param columns `r8set()`: Named list, data frame, or table: The columns, each a
#'   vector with one element for each row, or a [calc()]. `r8drop()`: Character or
#'   numeric: The columns, by name or by position, each once.
#' @param by Optional Character or numeric: The key columns whose groups the
#'   aggregates of [calc()] entries are over, by name or by position.
#' @param n_threads Optional Integer [1, Inf): The threads that the work in Rust may
#'   use; `NULL` takes `getOption("r8.n_threads")`, whose default is half the logical
#'   cores, at most 2 where `R CMD check` limits the cores, and at most
#'   `OMP_THREAD_LIMIT`. A result is the same for any number of threads.
#' @param from Character or numeric: The columns, by name or by position, each once.
#' @param to Character: The new names, one for each column of `from`.
#'
#' @return A value of the type of `x`, or a [Raven].
#'
#' @examples
#' r <- Raven(weight = c(60, 82), height = c(1.62, 1.80))
#' r <- r8set(r, list(bmi = r$weight / r$height^2))
#' r <- r8set(r, list(bmi2 = calc(weight / height^2)))
#' r <- r8rename(r, "weight", "mass")
#' r8drop(r, "height")
#'
#' @export
r8set <- function(x, columns, by = NULL, n_threads = NULL) {
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
      "`columns` is a named list: r8set(x, list(bmi = w / h^2)).",
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
      "` is NULL; r8drop() gives the table without a column.",
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
  computed <- vapply(columns, inherits, NA, "r8_calc")
  if (!is.null(by)) {
    by <- key_columns(x, by, substitute(by), parent.frame())
    if (!any(computed)) {
      r8_abort(
        "`by` groups the rows that the aggregates of calc() entries read: ",
        "r8set(x, list(n = calc(n())), by = \"ward\").",
        class = "r8_argument_error"
      )
    }
  }
  units <- NULL
  if (any(computed)) {
    res <- table_result(
      r8_table_calc(
        x,
        lapply(columns[computed], function(e) unclass(e)[["expr"]]),
        by %||% character(),
        FALSE,
        threads_of(n_threads)
      ),
      x
    )
    columns[computed] <- res[["columns"]]
    units <- stats::setNames(res[["units"]], keys[computed])
  }
  res <- .Call(wrap__r8_set_fields, x, keys, columns)
  if (!is_value(res)) {
    if (inherits(res, "r8_problems")) {
      construct_error(
        res,
        value_type(x),
        "",
        inputs = columns,
        assignment = "r8set()",
        closing = if (length(keys) == 1L) {
          column_closing(x, keys, res[["errors"]])
        }
      )
    }
    fields_refused(x, keys, columns, res)
  }
  with_units(res, units)
} # /R8::r8set


# %% threads_of ----
#' The threads of a call
#'
#' @param n_threads Optional Integer: The argument `n_threads`.
#'
#' @return Integer: The threads, from the argument, the option `r8.n_threads`, or
#'   `r8_default_threads()`.
#'
#' @keywords internal
#' @noRd
threads_of <- function(n_threads) {
  n <- n_threads %||% getOption("r8.n_threads") %||% r8_default_threads()
  if (
    !is.numeric(n) ||
      length(n) != 1L ||
      is.na(n) ||
      n < 1 ||
      n != round(n) ||
      n > .Machine[["integer.max"]]
  ) {
    r8_abort(
      "`n_threads` is a whole number of threads, at least 1, such as 2L; ",
      "options(r8.n_threads = ) sets the default.",
      class = "r8_argument_error"
    )
  }
  as.integer(n)
} # /R8::threads_of


# %% with_units ----
#' A table with the units of computed columns
#'
#' @param x A value of a table.
#' @param units Named character: The unit of each computed column, `NA` for
#'   none; `NULL` for no computed column.
#'
#' @return `x`, with the units of its extra columns that `units` names.
#'
#' @keywords internal
#' @noRd
with_units <- function(x, units) {
  if (is.null(units)) {
    return(x)
  }
  about <- r8_table_columns(x)
  extra <- about[["name"]][!about[["declared"]]]
  units <- units[!is.na(units) & names(units) %in% extra]
  if (length(units) == 0L) {
    return(x)
  }
  r8annotate(x, unit = units)
} # /R8::with_units


# %% key_columns ----
#' The names of the key columns of a table
#'
#' @param x A value of a table.
#' @param by Character or numeric: The keys, by name or by position.
#' @param e The unevaluated argument.
#' @param env Environment: Where the function is called.
#'
#' @return Character: The names, each once.
#'
#' @keywords internal
#' @noRd
key_columns <- function(x, by, e, env) {
  by <- table_columns_arg(x, by, e, env, "by")
  if (is.numeric(by)) {
    if (any(by > length(names(x))) || any(by != round(by))) {
      r8_abort(
        "`by` holds positions from 1 to ",
        length(names(x)),
        ".",
        class = "r8_argument_error"
      )
    }
    by <- names(x)[by]
  }
  if (anyNA(by) || anyDuplicated(by) > 0L) {
    r8_abort(
      "`by` names each key column once.",
      class = "r8_argument_error"
    )
  }
  by
} # /R8::key_columns


# %% r8summarize ----
#' Summarize the groups of a table's rows
#'
#' `r8summarize()` gives one row for each group of the rows of `x` by the key
#' columns `by`, or one row for the table without them: the keys, then the value
#' of each expression of `columns` for the group. Each expression is a [calc()] that
#' reads the cells of its columns within aggregates, evaluated in Rust: `n()`,
#' `count()`, `sum()`, `mean()`, `median()`, `min()`, and `max()`, alone or in an
#' expression, such as `sum(w * x) / sum(w)`.
#'
#' @section Groups:
#' A group is one combination of the values of the keys that a row has. Groups are
#' in the order of [r8sort()] of the keys: ascending, missing last, strings by code
#' point, and factors by level. A missing key is one group; a level of a factor
#' without a row has no group. The key columns keep their kinds, levels, units, and
#' descriptions.
#'
#' @section Missing cells:
#' An aggregate over a missing cell is missing, as in R; `na.rm = TRUE` in the
#' aggregate leaves the missing cells out: `mean(sbp, na.rm = TRUE)`.
#'
#' @param x A value of a table.
#' @param columns Named list of [calc()] values: The summaries, by the names of the
#'   result's columns.
#' @param by Optional Character or numeric: The key columns, by name or by
#'   position.
#' @param n_threads Optional Integer [1, Inf): The threads that the work in Rust may
#'   use; `NULL` takes `getOption("r8.n_threads")`, whose default is half the logical
#'   cores, at most 2 where `R CMD check` limits the cores, and at most
#'   `OMP_THREAD_LIMIT`. A result is the same for any number of threads.
#'
#' @return A [Raven] of the key columns and the summaries, with the units that
#'   the expressions infer.
#'
#' @examples
#' v <- Raven(
#'   ward = c("ICU", "ICU", "Cardiology"),
#'   sbp = c(142, NA, 128)
#' )
#' r8summarize(
#'   v,
#'   list(n = calc(n()), sbp = calc(mean(sbp, na.rm = TRUE))),
#'   by = "ward"
#' )
#'
#' @export
r8summarize <- function(x, columns, by = NULL, n_threads = NULL) {
  check_table(x)
  check_calcs(columns, "r8summarize")
  by <- if (is.null(by)) {
    character()
  } else {
    key_columns(x, by, substitute(by), parent.frame())
  }
  res <- table_result(
    r8_table_calc(
      x,
      lapply(columns, function(e) unclass(e)[["expr"]]),
      by,
      TRUE,
      threads_of(n_threads)
    ),
    x
  )
  summary_table(
    x,
    by,
    res[["first"]],
    res[["columns"]],
    res[["units"]],
    names(columns)
  )
} # /R8::r8summarize


# %% summary_table ----
#' The raven of a summary
#'
#' @param x A value of a table.
#' @param by Character: The key columns.
#' @param first Integer: The first row of each group, or `NULL` without keys.
#' @param columns List: The computed vectors.
#' @param units Character: Their units, `NA` for none.
#' @param names Character: Their names.
#'
#' @return A [Raven].
#'
#' @keywords internal
#' @noRd
summary_table <- function(x, by, first, columns, units, names) {
  clash <- intersect(names, by)
  if (length(clash) > 0L) {
    r8_abort(
      "The summary `",
      clash[[1L]],
      "` is named as a key column; name it otherwise.",
      class = "r8_argument_error"
    )
  }
  names(columns) <- names
  keys <- if (length(by) > 0L) {
    as.list(table_result(r8_table_get(x, first, by, 1L), x))
  }
  out <- Raven(c(keys, columns))
  # The facts of the keys, and the units of the summaries.
  facts <- list()
  if (length(by) > 0L) {
    about <- r8_table_columns(x)
    for (k in match(by, about[["name"]])) {
      f <- list(
        unit = about[["unit"]][[k]],
        description = about[["description"]][[k]]
      )
      f <- f[!vapply(f, function(v) is.na(v) || !nzchar(v), NA)]
      if (length(f) > 0L) {
        facts[[about[["name"]][[k]]]] <- f
      }
    }
  }
  for (k in which(!is.na(units))) {
    facts[[names[[k]]]] <- list(unit = units[[k]])
  }
  if (length(facts) == 0L) {
    return(out)
  }
  table_result(r8_table_set_facts(out, facts), out)
} # /R8::summary_table


# %% r8map ----
# R8 declares the generic when it loads (`declare_map()`).

#' Map an expression or a typed function over columns
#'
#' `r8map(x, f)` calls `f` once for each item of `x`, in order, and collects the
#' results in the container that `f`'s return type declares. `r8map()` is a generic
#' of R8 that dispatches on `x` and on what `f` is: a method for [Raven] applies to
#' every table.
#'
#' @section Lists and vectors:
#' The items of a list or an atomic vector are its elements, given to the first
#' parameter of a typed function ([new_function()]); `args` holds the constant
#' arguments of every call, by the names of `f`'s other parameters:
#' `r8map(folds, train_glm, args = list(data = d, outcome = "y"))`. A scalar return
#' type gives an atomic vector, and another type a list of the values, checked by
#' `f`, with the names of `x`. An error of a call names its item, and the map gives
#' nothing.
#'
#' @section Progress:
#' A map of a typed function reports its progress to a host that sets the option
#' `r8.progress` to a list of three functions: `begin(total, label, kind)`, which
#' gives a handle, `update(handle, current)`, and `end(handle, status)`, with the
#' status `"done"`, `"error"`, or `"aborted"`. The total is the calls of `f`, the
#' label its name, and the kind `"r8map"`. `begin` is called once the map has run
#' for 0.2 s, so that a shorter map reports nothing, and `update` at most about
#' every 0.05 to 0.2 s. Without the option, its default, a map reports nothing.
#'
#' @section Tables:
#' `r8map(x, f, columns, by)` gives one row for each group of the rows of `x` by
#' the key columns `by`, or one row for the table: the keys, then `f` of each column
#' of `columns`, under the column's name. Without `columns`, every column that is
#' not a key is mapped. `r8map(split(x, x[["g"]]), f)` maps `f` over the tables of
#' the groups.
#'
#' @section Typed functions:
#' A typed function of R8 ([new_function()]), with a body in R or in Rust, is
#' called on the cells of each column in each group, as a direct call is, and
#' returns one value of its declared return type:
#' `r8map(metrics, trimmed_mean, columns = metric_names, by = "param_combo_id")`.
#' Its arguments are checked as those of a direct call: a group with a missing cell
#' is an error unless the parameter's type is `option()`. Moving its body from R to
#' Rust changes no call.
#'
#' @section Expressions:
#' A [calc()] in which `col()` is the column mapped is evaluated in Rust, as the
#' summaries of [r8summarize()] are: `r8map(metrics, calc(median(col())), by =
#' "param_combo_id")`. Its results have the units that the expression infers.
#'
#' A plain R function is an error that names `new_function()`: `lapply(x[columns],
#' f)` applies one to whole columns, and `split()` gives the groups' tables.
#'
#' @param x A value of a table.
#' @param f A typed function, whose first parameter takes each item, or, for a
#'   table, a [calc()] that reads `col()`.
#' @param columns Optional Character or numeric: The columns mapped, by name or by
#'   position, each once.
#' @param by Optional Character or numeric: The key columns, by name or by
#'   position.
#' @param n_threads Optional Integer [1, Inf): The threads that the work in Rust may
#'   use; `NULL` takes `getOption("r8.n_threads")`, whose default is half the logical
#'   cores, at most 2 where `R CMD check` limits the cores, and at most
#'   `OMP_THREAD_LIMIT`. A result is the same for any number of threads. A typed function
#'   with its body in R runs on R's thread.
#'
#' @param args Optional named list: The constant arguments of every call of a typed
#'   function, by the names of its parameters other than the first.
#'
#' @return Of a table, a [Raven] of the key columns and one column for each column
#'   mapped; of a list or a vector, an atomic vector or a list, with the names of
#'   `x`.
#'
#' @examples
#' op <- options(r8.namespace = "demo")
#' m <- Raven(
#'   combo = c(1L, 1L, 2L, 2L),
#'   auc = c(0.81, 0.79, 0.85, 0.88),
#'   brier = c(0.16, 0.18, 0.14, 0.12)
#' )
#' r8map(m, calc(median(col())), by = "combo")
#' spread <- new_function(
#'   "spread",
#'   function(x = arg_vec(type_double())) max(x) - min(x),
#'   return_type = type_double()
#' )
#' r8map(m, spread, columns = "auc", by = "combo")
#' # A resample is the indices of its training rows; the data is a constant.
#' Fold <- new_struct("Fold", fields = list(train = vec(type_integer())))
#' folds <- list(a = Fold(train = 1:2), b = Fold(train = 3:4))
#' fold_auc <- new_function(
#'   "fold_auc",
#'   function(fold = arg(Fold), data = arg(Raven)) mean(data[fold$train, ]$auc),
#'   return_type = type_double()
#' )
#' r8map(folds, fold_auc, args = list(data = m))
#' options(op)
#'
#' @name r8map
#' @export
r8map <- NULL


# %% declare_map ----
#' Declare the generic r8map() and its methods for tables
#'
#' @param ns Environment: R8's namespace.
#'
#' @return `NULL`, invisibly.
#'
#' @keywords internal
#' @noRd
declare_map <- function(ns) {
  # The arguments of `params()` as a quoted call, which R CMD check does not
  # read as variables.
  declared <- quote(
    params(x, f, columns = NULL, by = NULL, args = NULL, n_threads = NULL)
  )
  r8map <- generic(
    "r8map",
    read_params(as.list(declared)[-1L], environment()),
    description = "`f` of each item of `x`, in the container of its return type.",
    dispatch = c("x", "f")
  )
  implement(r8map, Raven, host_class("r8_calc")) <- map_calc
  implement(r8map, Raven, host_class("r8_function")) <- map_typed
  implement(r8map, Raven, host_class("function")) <- map_function
  for (kind in c("list", "logical", "integer", "double", "character")) {
    implement(r8map, kind, host_class("r8_function")) <- map_items
    implement(r8map, kind, host_class("r8_calc")) <- map_items_calc
    implement(r8map, kind, host_class("function")) <- map_function
  }
  assign("r8map", r8map, envir = ns)
  invisible(NULL)
} # /R8::declare_map


# %% map_columns ----
#' The columns that a map maps
#'
#' @param x A value of a table.
#' @param columns Character, numeric, or `NULL`: The columns.
#' @param by Character: The key columns.
#'
#' @return Character: The names.
#'
#' @keywords internal
#' @noRd
map_columns <- function(x, columns, by) {
  if (is.null(columns)) {
    return(setdiff(names(x), by))
  }
  columns <- key_columns(x, columns, quote(columns), environment())
  keyed <- intersect(columns, by)
  if (length(keyed) > 0L) {
    r8_abort(
      "`",
      keyed[[1L]],
      "` is a key column and a column mapped.",
      class = "r8_argument_error"
    )
  }
  columns
} # /R8::map_columns


# %% map_calc ----
#' The method of r8map() for a table and a calc()
#'
#' @keywords internal
#' @noRd
map_calc <- function(x, f, columns, by, args, n_threads) {
  if (!is.null(args)) {
    r8_abort(
      "`args` are the constant arguments of a typed function; a calc() reads ",
      "columns and val().",
      class = "r8_argument_error"
    )
  }
  by <- if (is.null(by)) {
    character()
  } else {
    key_columns(x, by, quote(by), environment())
  }
  columns <- map_columns(x, columns, by)
  exprs <- r8_calc_mapped(unclass(f)[["expr"]], columns)
  if (inherits(exprs, "r8_refused")) {
    r8_abort(exprs[["message"]], ".", class = "r8_value_error")
  }
  res <- table_result(
    r8_table_calc(x, exprs, by, TRUE, threads_of(n_threads)),
    x
  )
  summary_table(
    x,
    by,
    res[["first"]],
    res[["columns"]],
    res[["units"]],
    columns
  )
} # /R8::map_calc


# %% map_typed ----
#' The method of r8map() for a table and a typed function
#'
#' @keywords internal
#' @noRd
map_typed <- function(x, f, columns, by, args, n_threads) {
  # A body in R runs on R's thread.
  threads_of(n_threads)
  sig <- map_signature(f, args)
  returns <- sig[["returns"]]
  if (identical(returns[["kind"]], "option")) {
    returns <- returns[["inner"]]
  }
  if (
    !returns[["kind"]] %in%
      c("logical", "integer", "double", "character", "date", "posixct")
  ) {
    r8_abort(
      "A map over the columns of a table gives one value of each column in a ",
      "column of the result, and `",
      sig[["name"]],
      "` returns another type; r8map(split(x, x[[\"g\"]]), f) maps it over ",
      "the tables of the groups.",
      class = "r8_value_error"
    )
  }
  by <- if (is.null(by)) {
    character()
  } else {
    key_columns(x, by, quote(by), environment())
  }
  columns <- map_columns(x, columns, by)
  if (length(by) > 0L) {
    g <- table_result(r8_table_groups(x, by), x)
    ids <- g[["ids"]]
    first <- g[["first"]]
  } else {
    ids <- rep.int(1L, nrow(x))
    first <- NULL
  }
  n_groups <- if (is.null(first)) 1L else length(first)
  name <- attr(f, "r8_function", exact = TRUE) %||% "f"
  # The keys of group `k`, for a message.
  group_text <- function(k) {
    if (is.null(first)) {
      return("")
    }
    shown <- vapply(
      by,
      function(b) {
        v <- x[[b]][[first[[k]]]]
        paste0(
          b,
          " = ",
          if (is.character(v) || is.factor(v)) {
            if (is.na(v)) "NA" else paste0("\"", as.character(v), "\"")
          } else {
            format(v)
          }
        )
      },
      ""
    )
    paste0(" in the group ", paste(shown, collapse = ", "))
  }
  out <- with_progress(length(columns) * n_groups, name, function(step) {
    lapply(columns, function(column) {
      parts <- split(x[[column]], factor(ids, levels = seq_len(n_groups)))
      values <- lapply(seq_along(parts), function(k) {
        v <- withCallingHandlers(
          do.call(f, c(list(parts[[k]]), sig[["args"]])),
          error = function(cnd) {
            r8_abort(
              "r8map(): `",
              name,
              "` of column `",
              column,
              "`",
              group_text(k),
              ": ",
              conditionMessage(cnd),
              class = "r8_value_error",
              data = list(parent = cnd)
            )
          }
        )
        if (length(v) != 1L || !is.atomic(v)) {
          r8_abort(
            "r8map(): `",
            name,
            "` of column `",
            column,
            "`",
            group_text(k),
            " gives ",
            length(v),
            " values; a function that r8map() maps gives one.",
            class = "r8_value_error"
          )
        }
        step()
        v
      })
      do.call(c, values)
    })
  })
  summary_table(x, by, first, out, rep(NA_character_, length(columns)), columns)
} # /R8::map_typed


# %% map_hook ----
#' The hook of the progress of maps, which a host draws
#'
#' @return The option `r8.progress`: `NULL`, its default, or a list of the
#'   functions `begin(total, label, kind)`, which gives a handle,
#'   `update(handle, current)`, and `end(handle, status)`.
#'
#' @keywords internal
#' @noRd
map_hook <- function() {
  hook <- getOption("r8.progress")
  if (
    !is.null(hook) &&
      (!is.list(hook) ||
        !all(c("begin", "update", "end") %in% names(hook)) ||
        !all(vapply(hook[c("begin", "update", "end")], is.function, NA)))
  ) {
    r8_abort(
      "The option `r8.progress` is NULL or a list of the functions `begin`, ",
      "`update`, and `end`.",
      class = "r8_argument_error"
    )
  }
  hook
} # /R8::map_hook


# %% with_progress ----
#' The steps of a map, with its progress
#'
#' Reads the option `r8.progress` once (`map_hook()`). A map reports its steps at
#' a stride that doubles while reports come within 0.05 s and halves when they
#' come after 0.2 s, and calls `begin` at the first report after 0.2 s, so that a
#' shorter map reports nothing; `end` follows with `"done"`, `"error"`, or
#' `"aborted"`.
#'
#' @param total Integer: The steps.
#' @param label Character: The name of the function mapped.
#' @param run Function: Of one argument, a function of no arguments that counts
#'   a step; gives the map's result.
#'
#' @return The result of `run`.
#'
#' @keywords internal
#' @noRd
with_progress <- function(total, label, run) {
  hook <- map_hook()
  if (is.null(hook)) {
    return(run(function() NULL))
  }
  start <- proc.time()[["elapsed"]]
  last <- start
  done <- 0L
  stride <- 1L
  next_at <- 1L
  handle <- NULL
  begun <- FALSE
  report <- function() {
    now <- proc.time()[["elapsed"]]
    gap <- now - last
    if (gap < 0.05) {
      stride <<- stride * 2L
    } else if (gap > 0.2) {
      stride <<- max(1L, stride %/% 2L)
    }
    next_at <<- done + stride
    if (!begun) {
      if (now - start < 0.2) {
        return(invisible(NULL))
      }
      handle <<- hook[["begin"]](total, label, "r8map")
      begun <<- TRUE
    }
    last <<- now
    hook[["update"]](handle, done)
    invisible(NULL)
  }
  step <- function() {
    done <<- done + 1L
    if (done >= next_at) report()
  }
  status <- "error"
  on.exit(
    if (begun) {
      # A map that completes reports its last steps.
      if (identical(status, "done")) {
        hook[["update"]](handle, done)
      }
      hook[["end"]](handle, status)
    },
    add = TRUE
  )
  out <- withCallingHandlers(
    run(step),
    interrupt = function(cnd) status <<- "aborted"
  )
  status <- "done"
  out
} # /R8::with_progress


# %% map_signature ----
#' The checked signature of a typed function that a map calls
#'
#' @param f A typed function.
#' @param args Optional named list: The constant arguments of every call.
#'
#' @return A list of `name`, the function's name, `item`, the name of the
#'   parameter that takes each item, `returns`, the declared return type, and
#'   `args`.
#'
#' @keywords internal
#' @noRd
map_signature <- function(f, args) {
  name <- attr(f, "r8_function", exact = TRUE) %||% "f"
  ir <- r8_function_ir(name)
  params <- ir[["params"]]
  if (length(params) == 0L || is.null(params[[1L]][["type"]])) {
    r8_abort(
      "r8map() gives each item to the first parameter of `",
      name,
      "`, which declares its type: function(x = arg_double(), ...).",
      class = "r8_argument_error"
    )
  }
  if (is.null(ir[["return_type"]])) {
    r8_abort(
      "r8map() collects the results of `",
      name,
      "` in the container of its return type: new_function(..., return_type = ).",
      class = "r8_argument_error"
    )
  }
  item <- params[[1L]][["name"]]
  others <- vapply(params[-1L], `[[`, "", "name")
  args <- args %||% list()
  keys <- names(args) %||% character()
  if (
    !is.list(args) ||
      is.object(args) ||
      length(keys) != length(args) ||
      anyNA(keys) ||
      !all(nzchar(keys))
  ) {
    r8_abort(
      "`args` is a named list of the constant arguments of every call: ",
      "args = list(data = d).",
      class = "r8_argument_error"
    )
  }
  if (anyDuplicated(keys) > 0L) {
    r8_abort(
      "`args` names `",
      keys[[anyDuplicated(keys)]],
      "` twice.",
      class = "r8_argument_error"
    )
  }
  if (item %in% keys) {
    r8_abort(
      "`",
      item,
      "` of `",
      name,
      "` takes each item of `x`; `args` holds the others.",
      class = "r8_argument_error"
    )
  }
  unknown <- setdiff(keys, others)
  if (length(unknown) > 0L) {
    near <- r8_nearest_name(unknown[[1L]], others)
    r8_abort(
      "`",
      name,
      "` has no parameter `",
      unknown[[1L]],
      "`",
      if (length(near) > 0L) paste0("; the nearest is `", near, "`"),
      ".",
      class = "r8_argument_error"
    )
  }
  list(name = name, item = item, returns = ir[["return_type"]], args = args)
} # /R8::map_signature


# %% map_items ----
#' The method of r8map() for a list or an atomic vector and a typed function
#'
#' @keywords internal
#' @noRd
map_items <- function(x, f, columns, by, args, n_threads) {
  if (!is.null(columns) || !is.null(by)) {
    r8_abort(
      "`columns` and `by` choose the columns and the groups of a table; ",
      "r8map() of a ",
      if (is.list(x)) "list" else "vector",
      " calls `f` on each element.",
      class = "r8_argument_error"
    )
  }
  # A body in R runs on R's thread.
  threads_of(n_threads)
  sig <- map_signature(f, args)
  constants <- sig[["args"]]
  call_f <- if (length(constants) == 0L) {
    f
  } else {
    function(item) do.call(f, c(list(item), constants))
  }
  returns <- sig[["returns"]]
  if (identical(returns[["kind"]], "option")) {
    returns <- returns[["inner"]]
  }
  kind <- returns[["kind"]]
  scalar <- list(
    logical = NA,
    integer = NA_integer_,
    double = NA_real_,
    character = NA_character_
  )
  # The item whose call runs, for the message of its error; 0 outside a call.
  at <- 0L
  failed <- function(cnd) {
    if (at == 0L) {
      return(invisible(NULL))
    }
    name <- names(x)[at]
    i <- at
    at <<- 0L
    r8_abort(
      "r8map(): `",
      sig[["name"]],
      "` of item ",
      i,
      if (!is.null(name) && !is.na(name) && nzchar(name)) {
        paste0(" (`", name, "`)")
      },
      ": ",
      conditionMessage(cnd),
      class = "r8_value_error",
      data = list(parent = cnd)
    )
  }
  out <- withCallingHandlers(
    with_progress(length(x), sig[["name"]], function(step) {
      each <- function(i) {
        at <<- i
        v <- call_f(x[[i]])
        at <<- 0L
        step()
        v
      }
      if (kind %in% names(scalar)) {
        vapply(seq_along(x), each, scalar[[kind]], USE.NAMES = FALSE)
      } else if (kind %in% c("date", "posixct")) {
        values <- lapply(seq_along(x), each)
        if (length(values) == 0L) {
          if (kind == "date") {
            as.Date(character())
          } else {
            as.POSIXct(character(), tz = "UTC")
          }
        } else {
          do.call(c, values)
        }
      } else {
        lapply(seq_along(x), each)
      }
    }),
    error = failed
  )
  names(out) <- names(x)
  out
} # /R8::map_items


# %% map_items_calc ----
#' The method of r8map() for a list or an atomic vector and a calc()
#'
#' @keywords internal
#' @noRd
map_items_calc <- function(x, f, columns, by, args, n_threads) {
  r8_abort(
    "A calc() maps over the columns of a table, with col() as each column; ",
    "a list or a vector takes a typed function: r8map(x, f) with f of new_function().",
    class = "r8_value_error"
  )
} # /R8::map_items_calc


# %% map_function ----
#' The method of r8map() for a table and an R function
#'
#' @keywords internal
#' @noRd
map_function <- function(x, f, columns, by, args, n_threads) {
  r8_abort(
    "r8map() maps a typed function of R8, declared with new_function(), whose ",
    "return type gives the container of the results, or, over the columns of a ",
    "table, calc() with col(). lapply(x, f) applies an R function, and split() ",
    "gives the tables of a table's groups.",
    class = "r8_value_error"
  )
} # /R8::map_function


# %% r8drop ----
#' @rdname r8set
#' @export
r8drop <- function(x, columns) {
  check_table(x)
  columns <- table_columns_arg(x, columns, substitute(columns), parent.frame())
  # Positions and names are read as r8select() reads them.
  at <- match(
    names(table_result(r8_table_get(x, NULL, columns, 1L), x)),
    names(x)
  )
  table_result(r8_table_get(x, NULL, setdiff(seq_along(x), at), 1L), x)
} # /R8::r8drop


# %% r8rename ----
#' @rdname r8set
#' @export
r8rename <- function(x, from, to) {
  check_table(x)
  from <- table_columns_arg(x, from, substitute(from), parent.frame(), "from")
  if (!is.character(to) || is.object(to) || anyNA(to)) {
    r8_abort(
      "`to` holds names, such as c(\"age\", \"sex\").",
      class = "r8_argument_error"
    )
  }
  table_result(r8_table_rename(x, from, to), x)
} # /R8::r8rename


# %% r8sort ----
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
#' @param n_threads Optional Integer [1, Inf): The threads that the work in Rust may
#'   use; `NULL` takes `getOption("r8.n_threads")`, whose default is half the logical
#'   cores, at most 2 where `R CMD check` limits the cores, and at most
#'   `OMP_THREAD_LIMIT`. A result is the same for any number of threads.
#'
#' @return A value of the type of `x`.
#'
#' @examples
#' r <- Raven(site = c("b", "a", "b"), age = c(40, 52, 61))
#' r8sort(r, c("site", "age"), descending = "age")
#'
#' @export
r8sort <- function(x, by, descending = NULL, n_threads = NULL) {
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
  table_result(
    r8_table_sort(x, by, descending %||% character(), threads_of(n_threads)),
    x
  )
} # /R8::r8sort


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


# %% r8columns ----
#' The columns of a table
#'
#' `r8columns()` describes the columns of a value of a table ([new_table()]), and
#' `r8annotate()` gives a value with the units or the descriptions of columns that
#' its type does not declare. Those of a declared column are its type's. A column that
#' is replaced has neither.
#'
#' @param x A value of a table.
#' @param unit,description Optional named character: The units, UCUM codes by
#'   convention, and the descriptions, by the names of columns; `NA` removes one.
#'
#' @return `r8columns()`, a data frame with the variables `name`, `type`, `unit`,
#'   `description`, and `declared`. `r8annotate()`, the new value.
#'
#' @examples
#' r <- Raven(age = c(40, 52), sex = c("f", "m"))
#' r <- r8annotate(
#'   r,
#'   unit = c(age = "a"),
#'   description = c(age = "Age at admission")
#' )
#' r
#' r8columns(r)
#'
#' @export
r8columns <- function(x) {
  check_table(x)
  out <- r8_table_columns(x)
  attr(out, "row.names") <- .set_row_names(length(out[["name"]]))
  class(out) <- "data.frame"
  out
} # /R8::r8columns


# %% r8annotate ----
#' @rdname r8columns
#' @export
r8annotate <- function(x, unit = NULL, description = NULL) {
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
} # /R8::r8annotate


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
