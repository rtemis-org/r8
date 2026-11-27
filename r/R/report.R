# report.R
# ::R8::
# 2026- EDG rtemis.org

# Checks: r8_check() looks and reports, and r8_validate() requires.
#
# spec: R8/raven#checks

# %% r8_check ----
#' Check a table against rules or a type
#'
#' `r8_check()` holds every row of a table against rules, or against the columns and
#' the rules of a type, and reports what it finds. It refuses nothing: data that were
#' read from a file are checked as they are, before they are converted or cleaned.
#' [r8_validate()] requires, and raises an error.
#'
#' @section The report:
#' The report is an R8 value of two tables. `rules` has a row for each rule, or each
#' declared column: the rows that were checked, those that failed, those where a rule
#' has no answer since a cell is missing, and those where it is an error, such as a
#' division by zero. The counts are exact. `problems` has a row for each rule and row
#' that failed, with the columns, the values found in them, and the explanation, for at
#' most `limit` rows of each rule. A rule that names a column that the table does not
#' have is not checked: its `checked` is 0, and `problems` says why.
#'
#' @section Rules:
#' A rule ([rule()]) reads the cells of a row by the names of the columns. A rule
#' that reads whole columns only, such as `rule(mean(age) > 18)`, is checked once. A
#' name in the list is the rule's ID, as in a declaration. A list of rules is saved
#' as any R object, and is checked against any table with the columns it names.
#'
#' @param x A value of a table, or a data frame. Any other object for a type of other
#'   values, which is checked as a whole.
#' @param against A [rule()], a list of rules, or a type: an R8 type object, or a field
#'   type such as [type_table()].
#' @param limit Integer: The problems listed for each rule.
#' @param n_threads Optional Integer [1, Inf): The threads that the work in Rust may
#'   use; `NULL` takes `getOption("r8.n_threads")`, whose default is half the logical
#'   cores, at most 2 where `R CMD check` limits the cores, and at most
#'   `OMP_THREAD_LIMIT`. A result is the same for any number of threads.
#'
#' @return A value of the struct `R8::Report`, with the fields `type`, the type that the
#'   table was held against or `NULL`, `rows`, `rules`, and `problems`.
#'
#' @examples
#' visits <- data.frame(
#'   age = c(40, 152, 61),
#'   dbp = c(80, 85, 130),
#'   sbp = c(120, NA, 118)
#' )
#' report <- r8_check(
#'   visits,
#'   list(pressures = rule(dbp < sbp), age = rule(age <= 130))
#' )
#' report
#' report$problems
#' r8_filter(report$problems, rule(rule == "age"))
#'
#' @export
r8_check <- function(x, against, limit = 1000L, n_threads = NULL) {
  threads_of(n_threads)
  if (
    !is.numeric(limit) ||
      length(limit) != 1L ||
      is.na(limit) ||
      limit < 0 ||
      limit != round(limit) ||
      limit > .Machine[["integer.max"]]
  ) {
    r8_abort(
      "`limit` is a number of rows, such as 1000.",
      class = "r8_argument_error"
    )
  }
  against <- checked_against(against)
  type <- against[["type"]]
  table <- table_given(x)
  if (is.null(table) && is.null(type)) {
    r8_abort(
      "`x` is a table, such as Raven(data), or a data frame; rules read its rows.",
      class = "r8_argument_error"
    )
  }
  if (is.null(table) || (!is.null(type) && !is_table_type(type))) {
    return(object_report(x, type))
  }
  res <- r8_report(
    table[["columns"]],
    table[["value"]],
    against[["rules"]],
    type,
    as.integer(limit)
  )
  if (is.character(res)) {
    r8_abort(
      toupper(substr(res, 1L, 1L)),
      substring(res, 2L),
      ".",
      class = "r8_argument_error"
    )
  }
  table_report(res, against)
} # /R8::r8_check


# %% r8_validate ----
#' Require that a value is valid
#'
#' `r8_validate(x)` checks an R8 value against its own type again, rules included: a
#' value built by its constructor is valid, and one that other code changed in place
#' may not be. With `against`, any object is held against a type or rules. What does
#' not satisfy them is an error; [r8_check()] reports it instead.
#'
#' @param x An R8 value, or, with `against`, any object.
#' @param against Optional: A field type, such as `type_table(...)` or
#'   `vec(type_double())`, an R8 type object, a [rule()], or a list of rules.
#' @param complete Logical: Require every context and computed field of an R8 value to
#'   be present.
#'
#' @return `x`, invisibly; raises `r8_value_error` with the problems.
#'
#' @examples
#' op <- options(r8.namespace = "demo")
#' Fit <- new_struct("FitSettings", fields = list(
#'   mtry = field(option(type_integer(min = 1L)), context = TRUE)
#' ))
#' r8_validate(Fit())
#' try(r8_validate(Fit(), complete = TRUE))
#' try(r8_validate(c(1, -1), vec(type_double(min = 0))))
#' try(r8_validate(data.frame(lo = 2, hi = 1), rule(lo <= hi)))
#' options(op)
#'
#' @export
r8_validate <- function(x, against = NULL, complete = FALSE) {
  check_flag(complete, "complete", "r8_argument_error")
  if (!is.null(against)) {
    name <- substitute(x)
    name <- if (is.symbol(name)) as.character(name) else "x"
    held <- checked_against(against)
    if (is.null(held[["type"]])) {
      report <- r8_check(x, against, limit = 5L)
      if (!report_holds(report)) {
        r8_abort(
          "Invalid `",
          name,
          "`: ",
          paste(report_lines(report), collapse = "\n"),
          class = "r8_value_error",
          data = list(report = report)
        )
      }
      return(invisible(x))
    }
    table <- if (is_table_type(held[["type"]])) table_given(x)
    res <- r8_check_object(x, table[["columns"]], held[["type"]], name)
    if (!is.null(res)) {
      value_abort(
        "Invalid",
        name,
        res[["errors"]],
        inputs = stats::setNames(list(x), name)
      )
    }
    return(invisible(x))
  }
  if (!is_value(x)) {
    value_required(x)
  }
  res <- r8_value_check(x)
  if (!is.null(res)) {
    value_abort("Invalid", value_type(x), res[["errors"]])
  }
  if (complete) {
    pending <- pending_fields(x)
    if (length(pending) > 0L) {
      r8_abort(
        "`",
        value_type(x),
        "` has pending fields: ",
        paste0("`", names(pending), "` (", pending, ")", collapse = ", "),
        ". r8_resolve() fills them.",
        class = "r8_value_error"
      )
    }
  }
  invisible(x)
} # /R8::r8_validate


# %% checked_against ----
#' What an object is held against
#'
#' @param against A rule, a list of rules, or a type.
#'
#' @return List: `type`, or `rules`; signals `r8_argument_error`.
#'
#' @keywords internal
#' @noRd
checked_against <- function(against) {
  if (inherits(against, "r8_type")) {
    ensure_registered(against)
    return(list(type = against))
  }
  if (inherits(against, "r8_field_type")) {
    return(list(type = against))
  }
  if (inherits(against, "r8_rule")) {
    against <- list(against)
  }
  if (!is.list(against) || is.object(against) || length(against) == 0L) {
    r8_abort(
      "`against` is a rule(), a list of rules, or a type.",
      class = "r8_argument_error"
    )
  }
  rules <- as_rules(against, "against", "r8_argument_error")
  ids <- unlist(lapply(rules, `[[`, "id"))
  if (anyDuplicated(ids) > 0L) {
    r8_abort(
      "Two rules of `against` have the ID `",
      ids[[anyDuplicated(ids)]],
      "`.",
      class = "r8_argument_error"
    )
  }
  list(rules = rules)
} # /R8::checked_against


# %% is_table_type ----
#' Whether a type is one of tables
#'
#' @param type An R8 type object, or a field type.
#'
#' @return Logical.
#'
#' @keywords internal
#' @noRd
is_table_type <- function(type) {
  inherits(type, "r8_table_type") ||
    (inherits(type, "r8_field_type") && identical(type[["kind"]], "table"))
} # /R8::is_table_type


# %% table_given ----
#' The columns of a table given to a check
#'
#' @param x Object.
#'
#' @return List with `columns`, a list by name, and `value`, a value of a table or
#'   `NULL`; `NULL` for an object that is no table.
#'
#' @keywords internal
#' @noRd
table_given <- function(x) {
  if (inherits(x, "r8_table") && is_value(x)) {
    return(list(columns = as.list(x), value = x))
  }
  if (!is.data.frame(x)) {
    return(NULL)
  }
  columns <- as.list(unclass(x))
  attributes(columns) <- list(names = names(x))
  list(columns = columns, value = NULL)
} # /R8::table_given


# %% table_report ----
#' The report of a table's check
#'
#' @param res List: What `r8_report()` gives.
#' @param against List: As `checked_against()` gives it.
#'
#' @return A value of `R8::Report`.
#'
#' @keywords internal
#' @noRd
table_report <- function(res, against) {
  lines <- res[["lines"]]
  type <- against[["type"]]
  rules <- against[["rules"]]
  if (!is.null(type)) {
    rules <- if (inherits(type, "r8_type")) {
      r8_type_ir(attr(type, "r8_name"))[["rules"]]
    } else {
      type[["rules"]]
    }
  }
  labels <- vapply(
    seq_along(rules),
    function(k) rules[[k]][["id"]] %||% paste("rule", k),
    ""
  )
  texts <- vapply(rules, rule_text, "")
  said <- vapply(rules, function(r) r[["message"]] %||% "", "")
  of_rule <- !is.na(lines[["rule"]])
  at <- lines[["rule"]]
  name <- ifelse(of_rule, labels[at], lines[["column"]])
  requirement <- ifelse(of_rule, texts[at], column_requirement(type, lines))
  found <- res[["problems"]]
  k <- found[["line"]]
  found[["found"]] <- with_operands(found, rules, at)
  message <- found[["message"]]
  # A violation is explained by the rule's message, or else by the rule.
  plain <- found[["code"]] == "rule_violation"
  message[plain] <- ifelse(
    nzchar(said[at[k[plain]]]),
    said[at[k[plain]]],
    texts[at[k[plain]]]
  )
  failed <- found[["code"]] == "rule_error" & nzchar(found[["found"]])
  message[failed] <- paste0(texts[at[k[failed]]], ": ", message[failed])
  type_report(
    type = if (!is.null(type)) type_label(type),
    rows = as.integer(res[["rows"]]),
    rules = report_rules_type(
      rule = as.character(name),
      requirement = as.character(requirement),
      checked = as.integer(lines[["checked"]]),
      failed = as.integer(lines[["failed"]]),
      missing = as.integer(lines[["missing"]]),
      errors = as.integer(lines[["errors"]])
    ),
    problems = report_problems_type(
      rule = as.character(name[k]),
      row = as.integer(found[["row"]]),
      columns = as.character(found[["columns"]]),
      found = as.character(found[["found"]]),
      code = as.character(found[["code"]]),
      message = as.character(message)
    )
  )
} # /R8::table_report


# %% with_operands ----
#' The values found by a report, with the computed operands of its rules
#'
#' @param found List: The problems of `r8_report()`.
#' @param rules List: The rules.
#' @param at Integer: The rule of each line.
#'
#' @return Character: `"x = 10, mean(x) = 4"`, one per problem.
#'
#' @keywords internal
#' @noRd
with_operands <- function(found, rules, at) {
  out <- found[["found"]]
  of <- found[["operand_of"]]
  if (length(of) == 0L) {
    return(out)
  }
  rule <- at[found[["line"]][of]]
  place <- found[["operand_at"]]
  key <- paste(rule, place)
  first <- which(!duplicated(key))
  text <- vapply(
    first,
    function(i) {
      logic_text(logic_at(rules[[rule[[i]]]][["invariant"]], place[[i]]))
    },
    ""
  )
  values <- paste0(text[match(key, key[first])], " = ", found[["operand"]])
  joined <- vapply(split(values, of), paste, "", collapse = ", ")
  i <- as.integer(names(joined))
  out[i] <- paste0(out[i], ifelse(nzchar(out[i]), ", ", ""), joined)
  out
} # /R8::with_operands


# %% column_requirement ----
#' What a type requires of each column of a report
#'
#' @param type A table type, or `NULL`.
#' @param lines List: The lines of the report.
#'
#' @return Character: The notation of each column's type, `""` for a column that the
#'   type does not declare.
#'
#' @keywords internal
#' @noRd
column_requirement <- function(type, lines) {
  out <- rep("", length(lines[["column"]]))
  if (is.null(type)) {
    return(out)
  }
  columns <- if (inherits(type, "r8_type")) {
    r8_type_ir(attr(type, "r8_name"))[["columns"]]
  } else {
    type[["columns"]]
  }
  names <- if (is.null(names(columns))) {
    vapply(columns, `[[`, "", "name")
  } else {
    names(columns)
  }
  at <- match(lines[["column"]], names)
  for (i in which(!is.na(at))) {
    of <- columns[[at[[i]]]][["type"]]
    out[[i]] <- plain_notation(r8_type_notation(unclass(of)))
  }
  out
} # /R8::column_requirement


# %% object_report ----
#' The report of an object that is no table
#'
#' @param x Object.
#' @param type A type.
#'
#' @return A value of `R8::Report`, with one row in `rules`.
#'
#' @keywords internal
#' @noRd
object_report <- function(x, type) {
  found <- r8_report_object(x, type)
  n <- length(found[["code"]])
  name <- type_label(type)
  type_report(
    type = name,
    rows = 1L,
    rules = report_rules_type(
      rule = name,
      requirement = name,
      checked = 1L,
      failed = as.integer(n > 0L),
      missing = 0L,
      errors = 0L
    ),
    problems = report_problems_type(
      rule = rep(name, n),
      row = as.integer(found[["row"]]),
      columns = as.character(found[["path"]]),
      found = as.character(found[["found"]]),
      code = as.character(found[["code"]]),
      message = as.character(found[["message"]])
    )
  )
} # /R8::object_report


# %% type_label ----
#' A type in words
#'
#' @param type An R8 type object, or a field type.
#'
#' @return Character.
#'
#' @keywords internal
#' @noRd
type_label <- function(type) {
  if (inherits(type, "r8_type")) {
    return(attr(type, "r8_name"))
  }
  plain_notation(r8_type_notation(unclass(type)))
} # /R8::type_label


# %% report_types ----
# The types of a report, which .onLoad() declares.
type_report <- NULL
report_rules_type <- NULL
report_problems_type <- NULL

declare_report <- function() {
  ns <- topenv()
  count <- type_integer(min = 0L)
  assign(
    "report_rules_type",
    new_table(
      "ReportRules",
      columns = list(
        rule = type_character(),
        requirement = type_character(),
        checked = count,
        failed = count,
        missing = count,
        errors = count
      ),
      description = "The rules of a report, with the rows by what each is in them."
    ),
    envir = ns
  )
  assign(
    "report_problems_type",
    new_table(
      "ReportProblems",
      columns = list(
        rule = type_character(),
        row = option(type_integer(min = 1L)),
        columns = type_character(),
        found = type_character(),
        code = type_character(),
        message = type_character()
      ),
      description = "The problems of a report, by rule and row."
    ),
    envir = ns
  )
  assign(
    "type_report",
    new_struct(
      "Report",
      fields = list(
        type = option(type_character()),
        rows = type_integer(min = 0L),
        rules = report_rules_type,
        problems = report_problems_type
      ),
      description = "What a check of a table found."
    ),
    envir = ns
  )
} # /R8::declare_report


# %% report_holds ----
#' Whether every rule of a report holds
#'
#' @param x A value of `R8::Report`.
#'
#' @return Logical.
#'
#' @keywords internal
#' @noRd
report_holds <- function(x) {
  nrow(x[["problems"]]) == 0L &&
    sum(x[["rules"]][["failed"]], x[["rules"]][["errors"]]) == 0
} # /R8::report_holds


# %% report_lines ----
#' A report as lines of text
#'
#' @param x A value of `R8::Report`.
#' @param most Integer: The rules shown.
#'
#' @return Character.
#'
#' @keywords internal
#' @noRd
report_lines <- function(x, most = 20L) {
  rules <- as.list(x[["rules"]])
  problems <- as.list(x[["problems"]])
  n <- length(rules[["rule"]])
  big <- function(v) format(v, big.mark = ",", trim = TRUE)
  rows_text <- function(v) paste0(big(v), if (v == 1) " row" else " rows")
  unread <- rules[["checked"]] == 0L & rules[["rule"]] %in% problems[["rule"]]
  holds <- rules[["failed"]] == 0L & rules[["errors"]] == 0L & !unread
  of <- x[["type"]]
  word <- if (is.null(of)) c("rule", "rules") else c("check", "checks")
  held <- sum(holds)
  of_type <- if (!is.null(of) && n > 1L) paste0(" of ", of)
  head <- if (n == 1L) {
    paste("The", word[[1L]], if (held == 1L) "holds" else "does not hold")
  } else if (held == n) {
    paste0(
      if (n == 2L) "Both " else paste0("All ", big(n), " "),
      word[[2L]],
      of_type,
      " hold"
    )
  } else {
    paste0(
      big(held),
      " of ",
      big(n),
      " ",
      word[[2L]],
      of_type,
      if (held == 1L) " holds" else " hold"
    )
  }
  head <- paste0(head, " on ", rows_text(x[["rows"]]))
  shown <- seq_len(n)
  if (n > most) {
    # Those that do not hold first, and the others as a number.
    shown <- utils::head(which(!holds), most)
  }
  status <- vapply(
    shown,
    function(k) {
      at <- which(problems[["rule"]] == rules[["rule"]][[k]])
      if (unread[[k]]) {
        return(paste0("is not checked: ", problems[["message"]][at][[1L]]))
      }
      parts <- character()
      if (rules[["failed"]][[k]] > 0L) {
        parts <- c(
          parts,
          paste0(
            rows_text(rules[["failed"]][[k]]),
            if (rules[["failed"]][[k]] == 1L) " fails" else " fail"
          )
        )
      }
      if (rules[["errors"]][[k]] > 0L) {
        parts <- c(
          parts,
          paste0(rows_text(rules[["errors"]][[k]]), " with an error")
        )
      }
      if (length(parts) == 0L) {
        parts <- "holds"
      }
      if (rules[["missing"]][[k]] > 0L) {
        parts <- c(
          parts,
          paste0(rows_text(rules[["missing"]][[k]]), " without an answer")
        )
      }
      row <- problems[["row"]][at]
      row <- row[!is.na(row)]
      out <- paste(parts, collapse = ", ")
      if (length(row) > 0L) {
        failed <- rules[["failed"]][[k]] + rules[["errors"]][[k]]
        out <- paste0(
          out,
          if (failed == 1L) " (row " else " (rows ",
          paste(big(utils::head(row, 3L)), collapse = ", "),
          if (failed > 3L) ", ...",
          ")"
        )
      }
      out
    },
    ""
  )
  names <- format(rules[["rule"]][shown])
  listed <- nrow(x[["problems"]])
  total <- sum(rules[["failed"]], rules[["errors"]])
  c(
    head,
    if (length(shown) > 0L) paste0("  ", names, "  ", status),
    if (sum(!holds) > length(shown)) {
      paste0(
        "  and ",
        big(sum(!holds) - length(shown)),
        " more that do not hold"
      )
    },
    if (listed > 0L) {
      paste0(
        "  $problems lists ",
        if (listed < total) {
          paste0(big(listed), " of ", big(total))
        } else {
          big(listed)
        },
        if (listed == 1L && total <= 1L) " problem" else " problems"
      )
    }
  )
} # /R8::report_lines


# %% report_text ----
#' A report as styled text
#'
#' @param x A value of `R8::Report`.
#'
#' @return An `r8_text` object.
#'
#' @keywords internal
#' @noRd
report_text <- function(x) {
  lines <- report_lines(x)
  do.call(
    c,
    c(
      list(r8_text(r8_span(lines[[1L]], "type_name"))),
      lapply(lines[-1L], as_r8_text)
    )
  )
} # /R8::report_text


# %% .report_types ----
#' The types of a report
#'
#' For a package that reports what it found in data of another form, such as a file:
#' its report is a struct that extends `R8::Report`.
#'
#' @return Named list of types: `report`, `R8::Report`, and its tables, `rules` and
#'   `problems`.
#'
#' @examples
#' names(.report_types())
#'
#' @keywords internal
#' @export
.report_types <- function() {
  list(
    report = type_report,
    rules = report_rules_type,
    problems = report_problems_type
  )
} # /R8::.report_types


# %% .table_type ----
#' The columns of a table type
#'
#' For a package that makes tables of data of another form, such as a file.
#'
#' @param type An R8 table type, or a `type_table()`.
#'
#' @return List: `columns`, a data frame with the variables `name`, `kind` (`"logical"`,
#'   `"integer"`, `"double"`, `"character"`, `"date"`, `"posixct"`, or `"factor"`),
#'   `optional`, whether cells may be missing, `required`, whether a table has the
#'   column, and the list `levels`; `types`, the field types by name; and `open`,
#'   whether a table has other columns.
#'   `NULL` for an object that is no table type.
#'
#' @examples
#' .table_type(type_table(list(age = type_double(), sex = type_factor(c("f", "m")))))
#'
#' @keywords internal
#' @export
.table_type <- function(type) {
  if (!is_table_type(type)) {
    return(NULL)
  }
  ir <- if (inherits(type, "r8_type")) {
    ensure_registered(type)
    r8_type_ir(attr(type, "r8_name"))
  } else {
    type
  }
  columns <- ir[["columns"]]
  of <- lapply(columns, function(c) {
    t <- c[["type"]]
    optional <- identical(t[["kind"]], "option")
    list(base = if (optional) t[["inner"]] else t, optional = optional)
  })
  names <- if (is.null(names(columns))) {
    vapply(columns, `[[`, "", "name")
  } else {
    names(columns)
  }
  out <- data.frame(
    name = as.character(names),
    kind = vapply(of, function(c) c[["base"]][["kind"]], ""),
    optional = vapply(of, `[[`, NA, "optional"),
    required = vapply(columns, function(c) !isFALSE(c[["required"]]), NA),
    row.names = NULL
  )
  out[["levels"]] <- lapply(of, function(c) {
    as.character(unlist(c[["base"]][["levels"]]))
  })
  types <- lapply(columns, function(c) {
    structure(unclass(c[["type"]]), class = "r8_field_type")
  })
  names(types) <- out[["name"]]
  list(columns = out, types = types, open = isTRUE(ir[["open"]]))
} # /R8::.table_type
