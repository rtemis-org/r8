# write.R
# ::R8.io::
# 2026- EDG rtemis.org

# %% r8write_table ----
#' Write a table to a file
#'
#' Writes the columns of a table as a CSV, TSV, or Parquet file, so that
#' [r8read_table()] with the type of the table reads the same values. Units and
#' descriptions are not written.
#'
#' A CSV or TSV file is UTF-8. A missing cell is empty. A text is quoted where it needs
#' to be, and where it is empty or `NA`. A number has the shortest form that reads back
#' exactly. A date and a date-time are in ISO 8601, a date-time in UTC. The cell of a
#' factor is its label.
#'
#' A Parquet file holds a logical as BOOLEAN, an integer as INT32, a double as DOUBLE,
#' a text and a factor as STRING, the factor by its labels, a date as DATE, and a
#' date-time as TIMESTAMP of microseconds adjusted to UTC. A missing cell is a null,
#' and `NaN` a value. Every column is optional, and the pages are compressed with
#' snappy.
#'
#' @param x A value of a table, or a data frame with columns that a table takes.
#' @param file Character: The file, which is replaced where it exists.
#' @param format Optional character \{"csv", "tsv", "parquet"\}: The format; `NULL`
#'   takes it from the name of the file.
#' @param delimiter Optional character: The character between two cells of a CSV or
#'   TSV file; `NULL` takes it from the format.
#'
#' @return `file`, invisibly.
#'
#' @examples
#' file <- tempfile(fileext = ".csv")
#' r8write_table(R8::Raven(a = c(1.5, NA), b = c("x", "NA")), file)
#' readLines(file)
#' r8read_table(file)
#' unlink(file)
#'
#' file <- tempfile(fileext = ".parquet")
#' r8write_table(R8::Raven(a = c(1.5, NA), b = c("x", "NA")), file)
#' r8read_table(file)
#' unlink(file)
#'
#' @export
r8write_table <- function(x, file, format = NULL, delimiter = NULL) {
  if (is.data.frame(x)) {
    x <- R8::Raven(x)
  }
  if (!inherits(x, "r8_table")) {
    io_abort(
      "`x` is a table, such as Raven(data), or a data frame.",
      class = "r8_argument_error"
    )
  }
  form <- file_format(file, format, delimiter)
  if (form[["compressed"]]) {
    io_abort(
      "A CSV or TSV file is written without compression; its name does not end in .gz.",
      class = "r8_argument_error"
    )
  }
  columns <- as.list(R8::r8to_data_frame(x))
  columns <- lapply(columns, function(v) {
    if (is.character(v)) {
      return(enc2utf8(v))
    }
    if (is.factor(v)) {
      levels(v) <- enc2utf8(levels(v))
    }
    v
  })
  names(columns) <- enc2utf8(names(columns))
  if (length(columns) == 0L) {
    io_abort(
      "`x` has no column; a file has a header with one name or more.",
      class = "r8_argument_error"
    )
  }
  res <- if (identical(form[["format"]], "parquet")) {
    write_parquet_file(columns, path.expand(file))
  } else {
    write_delimited(columns, path.expand(file), form[["delimiter"]])
  }
  if (!is.null(res)) {
    io_abort("`", file, "` is not written: ", res, ".", class = "r8_file_error")
  }
  invisible(file)
} # /R8.io::r8write_table
