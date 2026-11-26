# read.R
# ::R8.io::
# 2026- EDG rtemis.org

# Files of tables: what is read, what does not read, and the report of it.
#
# spec: R8/io

# %% r8_read_table ----
#' Read a table from a file
#'
#' `r8_read_table()` reads a CSV or TSV file as a value of a table type, and
#' `r8_check_file()` reports what in the file does not read as the type or does not
#' satisfy its rules. Reading requires: it gives a value of the type, or an error that
#' lists what does not read. Checking looks: it raises no error for what it finds.
#'
#' @section With a type:
#' A column is read as it is declared. A cell whose text is not of its type is
#' collected with the other cells of the same text: 40 cells of `<0.1` in a column of
#' numbers are one decision. `recode_in` gives what such a text reads as, and
#' `na_strings_in` the texts that are missing. Both are lists by column, which a script
#' keeps beside the type and uses for the next file, where a new text is an error again.
#' The value of a recoded text is of the column's type; a text that reads as it is
#' cannot be recoded.
#'
#' A column of the file that the type does not declare is read, with
#' `infer_types`, where the type is open, and is an error where it is not.
#'
#' @section Without a type:
#' With `infer_types = "strict"`, a column has the first of these types that every
#' cell has: logical, integer, double, date, and date-time. Otherwise it is text, with
#' the cells as they are in the file. Every cell is read, and none is changed to fit:
#' a column with a number that has zeros before it, such as `00123`, is text. With
#' `infer_types = "none"`, every column is text.
#'
#' @section Texts of each type:
#' A logical is `TRUE`, `True`, or `true`, or their opposites. A number has a decimal
#' point. A date is `2024-01-31`, and a date-time has an offset,
#' `2024-01-31T10:00:00Z`. Spaces around one of these are not part of it.
#'
#' @section Missing cells:
#' A cell is missing where its text is one of `na_strings`, exactly. In a column of
#' text, a quoted `""` is an empty text, and a quoted `"NA"` is that text;
#' `na_strings_in` makes them missing.
#'
#' @section Malformed files:
#' A quote that is not closed, characters after a closing quote, a line with another
#' number of cells than the header, a name that repeats in the header or is missing,
#' and bytes that are no text in `encoding` are errors with the line.
#'
#' @param file Character: The file. One whose name ends in `.gz` is decompressed.
#' @param type Optional table type: An R8 table type ([R8::new_table()]), or an
#'   [R8::type_table()], of which the result is a [R8::Raven].
#' @param infer_types Character \{"strict", "none"\}: How the columns without a
#'   declared type are read.
#' @param na_strings Character: The texts of missing cells.
#' @param na_strings_in Optional named list: Further texts of missing cells, by column:
#'   `list(age = "-99")`.
#' @param recode_in Optional named list: The values that texts read as, by column:
#'   `list(lab = c("<0.1" = 0.1))`.
#' @param format Optional character \{"csv", "tsv"\}: The format; `NULL` takes it from
#'   the name of the file.
#' @param delimiter Optional character: The character between two cells; `NULL` takes
#'   it from the format.
#' @param encoding Character \{"UTF-8", "latin1", "windows-1252"\}: The encoding of the
#'   file. A file that begins with the mark of UTF-16 is read as such.
#'
#' @return `r8_read_table()`: A value of `type`, or a [R8::Raven]; raises
#'   `r8_value_error` with the report, in the field `report`, where a cell does not
#'   read, and `r8_file_error` for a malformed file.
#'
#' @examples
#' op <- options(r8.namespace = "demo")
#' file <- tempfile(fileext = ".csv")
#' writeLines(c("id,lab", "1,0.4", "2,<0.1", "3,12"), file)
#' r8_read_table(file)
#'
#' Labs <- R8::new_table("Labs", columns = list(
#'   id = R8::type_integer(),
#'   lab = R8::type_double(min = 0)
#' ))
#' try(r8_read_table(file, Labs))
#' r8_check_file(file, Labs)$unread
#' r8_read_table(file, Labs, recode_in = list(lab = c("<0.1" = 0.1)))
#' unlink(file)
#' options(op)
#'
#' @export
r8_read_table <- function(
  file,
  type = NULL,
  infer_types = "strict",
  na_strings = c("", "NA"),
  na_strings_in = NULL,
  recode_in = NULL,
  format = NULL,
  delimiter = NULL,
  encoding = "UTF-8"
) {
  columns <- declared_columns(type)
  read <- read_file(
    file,
    columns,
    infer_types,
    na_strings,
    na_strings_in,
    recode_in,
    format,
    delimiter,
    encoding,
    limit = 1000L
  )
  if (nrow(read[["unread"]]) > 0L || read[["lines"]][["all"]] > 0) {
    report <- if (is.null(type)) {
      NULL
    } else {
      file_report(file, read, type, columns, 1000L)
    }
    io_abort(
      unread_message(file, read, type, substitute(file), substitute(type)),
      class = "r8_value_error",
      data = list(report = report)
    )
  }
  if (is.null(type) || !inherits(type, "r8_type")) {
    out <- R8::Raven(read[["columns"]])
    if (!is.null(type)) {
      R8::r8_validate(out, type)
    }
    return(out)
  }
  type(read[["columns"]])
} # /R8.io::r8_read_table


# %% r8_check_file ----
#' @rdname r8_read_table
#'
#' @param against A table type: an R8 table type, or an [R8::type_table()].
#' @param limit Integer: The problems listed for each column and rule.
#'
#' @return `r8_check_file()`: A value of the struct `R8io::FileReport`, which extends
#'   `R8::Report` ([R8::r8_check()]) with `file`; `unread`, a table of the texts that do
#'   not read, with the number of their cells and the first row; `recoded`, a table of
#'   the texts that `recode_in` names, with their values and the number of their cells;
#'   and `lines`, the lines that have another number of cells than the header. The
#'   cells that do not read are among `problems` with the code `unread`, and a rule has
#'   no answer in their rows.
#'
#' @export
r8_check_file <- function(
  file,
  against,
  limit = 1000L,
  infer_types = "strict",
  na_strings = c("", "NA"),
  na_strings_in = NULL,
  recode_in = NULL,
  format = NULL,
  delimiter = NULL,
  encoding = "UTF-8"
) {
  if (
    !is.numeric(limit) ||
      length(limit) != 1L ||
      is.na(limit) ||
      limit < 0 ||
      limit != round(limit) ||
      limit > .Machine[["integer.max"]]
  ) {
    io_abort(
      "`limit` is a number of rows, such as 1000.",
      class = "r8_argument_error"
    )
  }
  columns <- declared_columns(against, "against")
  if (is.null(columns)) {
    type_refused("against")
  }
  read <- read_file(
    file,
    columns,
    infer_types,
    na_strings,
    na_strings_in,
    recode_in,
    format,
    delimiter,
    encoding,
    limit = as.integer(limit)
  )
  file_report(file, read, against, columns, as.integer(limit))
} # /R8.io::r8_check_file


# %% declared_columns ----
#' The columns of the type that a file is read as
#'
#' @param type The type, or `NULL`.
#' @param arg Character: The argument.
#'
#' @return What `R8::.table_type()` gives, or `NULL` without a type; signals
#'   `r8_argument_error`.
#'
#' @keywords internal
#' @noRd
declared_columns <- function(type, arg = "type") {
  if (is.null(type)) {
    return(NULL)
  }
  columns <- R8::.table_type(type)
  if (is.null(columns)) {
    type_refused(arg)
  }
  columns
} # /R8.io::declared_columns

type_refused <- function(arg) {
  io_abort(
    "`",
    arg,
    "` is a table type, such as one of new_table(), or type_table().",
    class = "r8_argument_error"
  )
} # /R8.io::type_refused


# %% check_string ----
#' Check that an argument is one string of a set
#'
#' @param x The argument.
#' @param arg Character: Its name.
#' @param values Character: The set.
#'
#' @return `x`; signals `r8_argument_error`.
#'
#' @keywords internal
#' @noRd
check_string <- function(x, arg, values) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !x %in% values) {
    said <- paste0("\"", values, "\"")
    n <- length(said)
    io_abort(
      "`",
      arg,
      "` is ",
      if (n == 2L) {
        paste(said, collapse = " or ")
      } else {
        paste0(paste(said[-n], collapse = ", "), ", or ", said[[n]])
      },
      ".",
      class = "r8_argument_error"
    )
  }
  x
} # /R8.io::check_string


# %% check_texts ----
#' Check that an argument holds texts
#'
#' @param x The argument.
#' @param arg Character: Its name.
#'
#' @return Character, in UTF-8; signals `r8_argument_error`.
#'
#' @keywords internal
#' @noRd
check_texts <- function(x, arg) {
  if (!is.character(x) || is.object(x) || anyNA(x)) {
    io_abort(
      "`",
      arg,
      "` holds texts of missing cells, such as c(\"\", \"NA\").",
      class = "r8_argument_error"
    )
  }
  enc2utf8(x)
} # /R8.io::check_texts


# %% check_by_column ----
#' Check that an argument is a named list by column
#'
#' @param x The argument, or `NULL`.
#' @param arg Character: Its name.
#' @param form Character: The form, for the message.
#'
#' @return Named list; signals `r8_argument_error`.
#'
#' @keywords internal
#' @noRd
check_by_column <- function(x, arg, form) {
  if (is.null(x)) {
    return(list())
  }
  keys <- names(x)
  if (
    !is.list(x) ||
      is.object(x) ||
      length(keys) != length(x) ||
      anyNA(keys) ||
      !all(nzchar(keys)) ||
      anyDuplicated(keys) > 0L
  ) {
    io_abort(
      "`",
      arg,
      "` is a list by the names of columns: ",
      arg,
      " = ",
      form,
      ".",
      class = "r8_argument_error"
    )
  }
  names(x) <- enc2utf8(keys)
  x
} # /R8.io::check_by_column


# %% check_recode ----
#' Check the values that texts read as
#'
#' @param x Named list, or `NULL`.
#'
#' @return Named list of named vectors; signals `r8_argument_error`.
#'
#' @keywords internal
#' @noRd
check_recode <- function(x) {
  form <- "list(lab = c(\"<0.1\" = 0.1))"
  x <- check_by_column(x, "recode_in", form)
  for (key in names(x)) {
    v <- x[[key]]
    if (
      !is.atomic(v) ||
        is.factor(v) ||
        length(v) == 0L ||
        is.null(names(v)) ||
        anyNA(names(v))
    ) {
      io_abort(
        "`recode_in` gives, for each column, values named by their texts: recode_in = ",
        form,
        ".",
        class = "r8_argument_error"
      )
    }
    if (anyNA(v)) {
      io_abort(
        "`recode_in` gives NA for ",
        quoted(names(v)[is.na(v)][[1L]]),
        " of `",
        key,
        "`; na_strings_in = list(",
        key,
        " = ",
        quoted(names(v)[is.na(v)][[1L]]),
        ") makes the cells missing.",
        class = "r8_argument_error"
      )
    }
    if (is.character(v)) {
      x[[key]] <- stats::setNames(enc2utf8(v), names(v))
    }
    names(x[[key]]) <- enc2utf8(names(v))
  }
  x
} # /R8.io::check_recode


# %% file_format ----
#' The format of a file
#'
#' @param file Character: The file.
#' @param format,delimiter The arguments.
#'
#' @return List: `delimiter`, an integer, and `compressed`; signals
#'   `r8_argument_error`.
#'
#' @keywords internal
#' @noRd
file_format <- function(file, format, delimiter) {
  if (!is.character(file) || length(file) != 1L || is.na(file)) {
    io_abort("`file` is one path.", class = "r8_argument_error")
  }
  compressed <- grepl("\\.gz$", file, ignore.case = TRUE)
  if (is.null(format)) {
    name <- tolower(sub("\\.gz$", "", file, ignore.case = TRUE))
    format <- sub("^.*\\.", "", basename(name))
    if (!format %in% c("csv", "tsv")) {
      io_abort(
        "The name of `",
        basename(file),
        "` does not end in .csv or .tsv; `format` gives the format: format = \"csv\".",
        class = "r8_argument_error"
      )
    }
  } else {
    check_string(format, "format", c("csv", "tsv"))
  }
  if (is.null(delimiter)) {
    delimiter <- if (identical(format, "tsv")) "\t" else ","
  }
  code <- if (
    is.character(delimiter) && length(delimiter) == 1L && !is.na(delimiter)
  ) {
    utf8ToInt(enc2utf8(delimiter))
  }
  if (length(code) != 1L || code > 127L || code %in% c(0L, 10L, 13L, 34L)) {
    io_abort(
      "`delimiter` is one ASCII character other than a quote and the end of a line, ",
      "such as \";\".",
      class = "r8_argument_error"
    )
  }
  list(delimiter = code, compressed = compressed)
} # /R8.io::file_format


# %% read_file ----
#' Read the columns of a file
#'
#' @param file,infer_types,na_strings,na_strings_in,recode_in,format,delimiter,encoding
#'   As [r8_read_table()] takes them.
#' @param declared What `declared_columns()` gives.
#' @param limit Integer: The cells that do not read that each column lists.
#'
#' @return What `read_delimited()` gives, with data frames; signals
#'   `r8_argument_error` and `r8_file_error`.
#'
#' @keywords internal
#' @noRd
read_file <- function(
  file,
  declared,
  infer_types,
  na_strings,
  na_strings_in,
  recode_in,
  format,
  delimiter,
  encoding,
  limit
) {
  form <- file_format(file, format, delimiter)
  check_string(infer_types, "infer_types", c("strict", "none"))
  check_string(encoding, "encoding", c("UTF-8", "latin1", "windows-1252"))
  na_strings <- check_texts(na_strings, "na_strings")
  na_strings_in <- check_by_column(
    na_strings_in,
    "na_strings_in",
    "list(age = \"-99\")"
  )
  na_strings_in <- lapply(na_strings_in, check_texts, "na_strings_in")
  recode_in <- check_recode(recode_in)
  path <- path.expand(file)
  if (!file.exists(path) || dir.exists(path)) {
    io_abort("`", file, "` is not a file.", class = "r8_file_error")
  }
  columns <- declared[["columns"]]
  res <- read_delimited(
    if (form[["compressed"]]) unpacked(path),
    path,
    form[["delimiter"]],
    encoding,
    as.character(columns[["name"]]),
    as.character(columns[["kind"]]),
    as.list(columns[["levels"]]),
    if (identical(infer_types, "strict")) "infer" else "character",
    na_strings,
    na_strings_in,
    recode_in,
    limit
  )
  if (!is.null(res[["refused"]])) {
    if (identical(res[["refused"]], "argument")) {
      io_abort(res[["message"]], ".", class = "r8_argument_error")
    }
    io_abort(
      "`",
      basename(file),
      "` does not read: ",
      res[["message"]],
      if (identical(res[["refused"]], "encoding")) {
        "; `encoding` gives the encoding of the file, such as \"latin1\""
      },
      ".",
      class = "r8_file_error"
    )
  }
  # A factor whose levels are not declared has those of its cells.
  for (k in which(columns[["kind"]] == "factor")) {
    name <- columns[["name"]][[k]]
    x <- res[["columns"]][[name]]
    if (is.character(x)) {
      res[["columns"]][[name]] <- factor(x, sort(unique(x), method = "radix"))
    }
  }
  for (part in c("unread", "cells", "recoded")) {
    res[[part]] <- list_frame(res[[part]])
  }
  if (length(recode_in) > 0L) {
    check_recoded_values(res[["recoded"]], recode_in, declared)
  }
  res
} # /R8.io::read_file


# %% list_frame ----
#' A list of vectors of one length as a data frame
#'
#' @param x Named list.
#'
#' @return Data frame.
#'
#' @keywords internal
#' @noRd
list_frame <- function(x) {
  attr(x, "row.names") <- .set_row_names(length(x[[1L]]))
  class(x) <- "data.frame"
  x
} # /R8.io::list_frame


# %% unpacked ----
#' The bytes of a compressed file
#'
#' @param path Character.
#'
#' @return Raw; signals `r8_file_error`.
#'
#' @keywords internal
#' @noRd
unpacked <- function(path) {
  con <- gzfile(path, "rb")
  on.exit(close(con))
  parts <- list()
  repeat {
    part <- tryCatch(
      readBin(con, "raw", 2^24),
      error = function(e) {
        io_abort(
          "`",
          basename(path),
          "` does not decompress: ",
          conditionMessage(e),
          class = "r8_file_error"
        )
      }
    )
    if (length(part) == 0L) {
      break
    }
    parts[[length(parts) + 1L]] <- part
  }
  if (length(parts) == 0L) raw() else do.call(c, parts)
} # /R8.io::unpacked


# %% check_recoded_values ----
#' Check that the values of texts satisfy the types of their columns
#'
#' @param recoded Data frame: What was recoded.
#' @param recode_in Named list.
#' @param declared What `declared_columns()` gives.
#'
#' @return `NULL`, invisibly; signals `r8_argument_error`.
#'
#' @keywords internal
#' @noRd
check_recoded_values <- function(recoded, recode_in, declared) {
  types <- declared[["types"]]
  for (key in names(recode_in)) {
    type <- types[[key]]
    # The levels of a factor are checked as the file is read.
    if (is.null(type) || identical(type_kind(type), "factor")) {
      next
    }
    for (text in names(recode_in[[key]])) {
      value <- unname(recode_in[[key]][text])
      ok <- tryCatch(
        {
          R8::r8_validate(value, type)
          NULL
        },
        r8_value_error = conditionMessage
      )
      if (!is.null(ok)) {
        io_abort(
          "`recode_in` gives a value that `",
          key,
          "` does not admit for ",
          quoted(text),
          ": ",
          sub("^.*\n- `value` ", "", ok),
          class = "r8_argument_error"
        )
      }
    }
  }
  invisible(NULL)
} # /R8.io::check_recoded_values


# %% unread_message ----
#' The message of a file that does not read
#'
#' @param file Character: The file.
#' @param read What `read_file()` gives.
#' @param type The type, or `NULL`.
#' @param file_expr,type_expr Language objects: The arguments as written.
#'
#' @return Character.
#'
#' @keywords internal
#' @noRd
unread_message <- function(file, read, type, file_expr, type_expr) {
  lines <- read[["lines"]]
  if (lines[["all"]] > 0) {
    shown <- utils::head(lines[["line"]], 3L)
    cells <- utils::head(lines[["cells"]], 3L)
    return(paste0(
      "`",
      basename(file),
      "` has ",
      counted(lines[["all"]], "line"),
      " with another number of cells than the ",
      length(read[["columns"]]),
      " of the header: ",
      paste0(
        "line ",
        big(shown),
        " has ",
        cells,
        collapse = ", "
      ),
      if (lines[["all"]] > length(shown)) ", ...",
      "."
    ))
  }
  unread <- read[["unread"]]
  most <- 10L
  shown <- utils::head(unread, most)
  text <- ifelse(is.na(shown[["text"]]), "other texts", quoted(shown[["text"]]))
  first <- vapply(shown[["rows"]], `[[`, 0L, 1L)
  said <- paste0(
    "  ",
    base::format(shown[["column"]]),
    "  ",
    base::format(text),
    "  ",
    counted(shown[["cells"]], "cell"),
    ", first in row ",
    big(first)
  )
  named <- unread[!is.na(unread[["text"]]), ]
  by <- split(
    named[["text"]],
    factor(named[["column"]], unique(named[["column"]]))
  )
  form <- vapply(
    names(by),
    function(k) {
      texts <- utils::head(by[[k]], 4L)
      paste0(
        k,
        " = c(",
        paste0(quoted(texts), " = ", collapse = ", "),
        if (length(by[[k]]) > 4L) ", ...",
        ")"
      )
    },
    ""
  )
  paste(
    c(
      paste0(
        "`",
        basename(file),
        "` does not read as `",
        type_name(type),
        "`: ",
        counted(sum(unread[["cells"]]), "cell"),
        " of ",
        counted(read[["rows"]], "row")
      ),
      said,
      if (nrow(unread) > most) {
        paste0("  and ", counted(nrow(unread) - most, "other text"))
      },
      paste0(
        "`recode_in` gives what each text reads as, and `na_strings_in` those that are ",
        "missing:"
      ),
      paste0("  recode_in = list(", paste(form, collapse = ", "), ")"),
      paste0(
        "r8_check_file(",
        written_as(file_expr, "file"),
        ", ",
        written_as(type_expr, "type"),
        ") gives a report."
      )
    ),
    collapse = "\n"
  )
} # /R8.io::unread_message

written_as <- function(e, other) {
  text <- deparse(e, width.cutoff = 60L)
  if (length(text) == 1L) text else other
} # /R8.io::written_as

type_kind <- function(type) {
  kind <- type[["kind"]]
  if (identical(kind, "option")) type[["inner"]][["kind"]] else kind
} # /R8.io::type_kind

type_name <- function(type) {
  name <- attr(type, "r8_name")
  if (is.null(name)) "type_table()" else name
} # /R8.io::type_name
