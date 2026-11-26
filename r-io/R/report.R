# report.R
# ::R8.io::
# 2026- EDG rtemis.org

# The report of a file: what R8 reports of its columns and rows, and what does not
# read.
#
# spec: R8/io

# %% declare_file_report ----
# The types of a report of a file, which .onLoad() declares.
type_file_report <- NULL
file_unread_type <- NULL
file_recoded_type <- NULL
file_lines_type <- NULL

declare_file_report <- function() {
  ns <- topenv()
  of <- R8::.report_types()
  count <- R8::type_integer(min = 0L)
  row <- R8::type_integer(min = 1L)
  text <- R8::type_character()
  set <- function(name, value) assign(name, value, envir = ns)
  set(
    "file_unread_type",
    R8::new_table(
      "Unread",
      columns = list(
        column = text,
        text = R8::option(text),
        cells = count,
        first_row = row
      ),
      description = "The texts of a file that do not read; a missing text stands for the others of its column."
    )
  )
  set(
    "file_recoded_type",
    R8::new_table(
      "Recoded",
      columns = list(column = text, text = text, value = text, cells = count),
      description = "The texts of a file that read as given values."
    )
  )
  set(
    "file_lines_type",
    R8::new_table(
      "Lines",
      columns = list(line = R8::type_double(min = 1), cells = count),
      description = "The lines of a file that have another number of cells than the header."
    )
  )
  set(
    "type_file_report",
    R8::new_struct(
      "FileReport",
      extends = of[["report"]],
      fields = list(
        file = text,
        unread = file_unread_type,
        recoded = file_recoded_type,
        lines = file_lines_type,
        other_lines = R8::type_double(min = 0)
      ),
      description = "What a check of a file found."
    )
  )
  R8::impl(
    R8::Display,
    type_file_report,
    methods = list(format = function(x, ...) file_report_text(x))
  )
} # /R8.io::declare_file_report


# %% file_report ----
#' The report of a file
#'
#' @param file Character: The file.
#' @param read What `read_file()` gives.
#' @param type The type.
#' @param declared What `declared_columns()` gives.
#' @param limit Integer: The problems listed for each column and rule.
#'
#' @return A value of `R8io::FileReport`.
#'
#' @keywords internal
#' @noRd
file_report <- function(file, read, type, declared, limit) {
  of <- R8::.report_types()
  checked <- R8::r8_check(list_frame_of(read), type, limit = limit)
  unread <- read[["unread"]]
  lines <- read[["lines"]]
  rules <- as.list(checked[["rules"]])
  problems <- as.list(checked[["problems"]])
  if (nrow(unread) > 0L) {
    cells <- read[["cells"]]
    columns <- declared[["columns"]]
    # A cell that does not read is missing in the column that was checked.
    total <- tapply(
      unread[["cells"]],
      factor(unread[["column"]], unique(unread[["column"]])),
      sum
    )
    for (name in names(total)) {
      at <- match(name, rules[["rule"]])
      if (isTRUE(columns[["optional"]][match(name, columns[["name"]])])) {
        rules[["failed"]][[at]] <- as.integer(
          rules[["failed"]][[at]] + total[[name]]
        )
      }
      rows <- cells[["row"]][cells[["column"]] == name]
      # Its problems in the check are those of a missing cell.
      drop <- problems[["columns"]] == name &
        problems[["rule"]] == name &
        problems[["row"]] %in% rows
      problems <- lapply(problems, `[`, !drop)
    }
    said <- c(
      logical = "a logical",
      integer = "an integer",
      double = "a number",
      date = "a date, such as 2024-01-31",
      posixct = "a date-time, such as 2024-01-31T10:00:00Z",
      factor = "one of the levels"
    )
    kind <- columns[["kind"]][match(cells[["column"]], columns[["name"]])]
    problems <- Map(
      c,
      problems,
      list(
        rule = cells[["column"]],
        row = cells[["row"]],
        columns = cells[["column"]],
        found = paste0(cells[["column"]], " = ", quoted(cells[["text"]])),
        code = rep("unread", nrow(cells)),
        message = paste0("must be ", unname(said[kind]))
      )[names(problems)]
    )
    by <- order(match(problems[["rule"]], rules[["rule"]]), problems[["row"]])
    by <- unlist(lapply(
      split(by, match(problems[["rule"]][by], rules[["rule"]])),
      utils::head,
      limit
    ))
    problems <- lapply(problems, `[`, as.integer(by))
  }
  type_file_report(
    type = checked[["type"]],
    rows = checked[["rows"]],
    rules = of[["rules"]](rules),
    problems = of[["problems"]](problems),
    file = basename(file),
    unread = file_unread_type(
      column = unread[["column"]],
      text = unread[["text"]],
      cells = as.integer(unread[["cells"]]),
      first_row = vapply(unread[["rows"]], `[[`, 0L, 1L)
    ),
    recoded = file_recoded_type(
      column = read[["recoded"]][["column"]],
      text = read[["recoded"]][["text"]],
      value = read[["recoded"]][["value"]],
      cells = as.integer(read[["recoded"]][["cells"]])
    ),
    lines = file_lines_type(line = lines[["line"]], cells = lines[["cells"]]),
    other_lines = lines[["all"]]
  )
} # /R8.io::file_report


# %% list_frame_of ----
#' The columns that were read as a data frame
#'
#' @param read What `read_file()` gives.
#'
#' @return Data frame.
#'
#' @keywords internal
#' @noRd
list_frame_of <- function(read) {
  out <- read[["columns"]]
  attr(out, "row.names") <- .set_row_names(read[["rows"]])
  class(out) <- "data.frame"
  out
} # /R8.io::list_frame_of


# %% file_report_text ----
#' The text of a report of a file
#'
#' @param x A value of `R8io::FileReport`.
#'
#' @return Character: Lines.
#'
#' @keywords internal
#' @noRd
file_report_text <- function(x) {
  base <- format(R8::r8_upcast(x, R8::.report_types()[["report"]]))
  unread <- as.list(x[["unread"]])
  recoded <- as.list(x[["recoded"]])
  n <- length(unread[["column"]])
  most <- 10L
  i <- seq_len(min(n, most))
  text <- ifelse(
    is.na(unread[["text"]][i]),
    "other texts",
    quoted(unread[["text"]][i])
  )
  j <- seq_along(recoded[["column"]])
  c(
    base,
    if (x[["other_lines"]] > 0) {
      paste0(
        counted(x[["other_lines"]], "line"),
        " with another number of cells than the header ",
        if (x[["other_lines"]] == 1) "is" else "are",
        " left out ($lines)"
      )
    },
    if (n > 0L) "Texts that do not read ($unread)",
    if (n > 0L) {
      paste0(
        "  ",
        base::format(unread[["column"]][i]),
        "  ",
        base::format(text),
        "  ",
        counted(unread[["cells"]][i], "cell"),
        ", first in row ",
        big(unread[["first_row"]][i])
      )
    },
    if (n > most) paste0("  and ", counted(n - most, "other text")),
    if (length(j) > 0L) "Texts that read as given values ($recoded)",
    if (length(j) > 0L) {
      paste0(
        "  ",
        base::format(recoded[["column"]]),
        "  ",
        base::format(paste0(
          quoted(recoded[["text"]]),
          " as ",
          recoded[["value"]]
        )),
        "  ",
        counted(recoded[["cells"]], "cell")
      )
    }
  )
} # /R8.io::file_report_text
