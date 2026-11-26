# test-102-check.R
# ::R8.io::
# 2026- EDG rtemis.org

# r8_check_file(): the report of a file.

test_that("a report has every problem of a file, and refuses nothing", {
  T <- labs_type()
  r <- r8_check_file(file_of(labs_lines), T)
  expect_true(inherits(r, "R8io::FileReport"))
  expect_true(inherits(r, "R8::Report"))
  expect_identical(r$rows, 4L)
  expect_identical(
    as.list(R8::r8_select(r$rules, c("rule", "checked", "failed", "missing"))),
    list(
      rule = c("mrn", "age", "seen", "stage", "lab", "young"),
      checked = rep(4L, 6),
      failed = c(0L, 1L, 1L, 1L, 3L, 1L),
      missing = rep(0L, 6)
    )
  )
  p <- as.list(r$problems)
  expect_identical(
    p$rule,
    c("age", "seen", "stage", "lab", "lab", "lab", "young")
  )
  expect_identical(p$row, c(3L, 2L, 4L, 2L, 3L, 4L, 3L))
  expect_identical(
    p$code,
    c("above_max", rep("unread", 5), "rule_violation")
  )
  expect_identical(
    p$found[2:4],
    c("seen = \"2024-02-30\"", "stage = \"III\"", "lab = \"<0.1\"")
  )
  expect_identical(
    p$message[2:4],
    c(
      "must be a date, such as 2024-01-31",
      "must be one of the levels",
      "must be a number"
    )
  )
  expect_identical(
    as.list(r$unread),
    list(
      column = c("seen", "stage", "lab", "lab"),
      text = c("2024-02-30", "III", "<0.1", "hemol"),
      cells = c(1L, 1L, 2L, 1L),
      first_row = c(2L, 4L, 2L, 4L)
    )
  )
  r <- r8_check_file(
    file_of(labs_lines),
    T,
    recode_in = list(lab = c("<0.1" = 0.1, "<0.2" = 0.2)),
    na_strings_in = list(lab = "hemol")
  )
  expect_identical(
    as.list(r$recoded),
    list(
      column = c("lab", "lab"),
      text = c("<0.1", "<0.2"),
      value = c("0.1", "0.2"),
      cells = c(2L, 0L)
    )
  )
  # A missing cell of a column that has none is a problem of its own.
  expect_identical(
    as.list(R8::r8_filter(r$problems, R8::rule(rule == "lab")))[c(
      "row",
      "code"
    )],
    list(row = 4L, code = "missing_value")
  )
})

test_that("a cell that does not read has no answer in a rule", {
  T <- R8::new_table(
    "R",
    list(a = R8::option(R8::type_double()), b = R8::type_double()),
    rules = list(order = R8::rule(a < b))
  )
  r <- r8_check_file(file_of("a,b", "1,2", "x,3", "5,4", ",1"), T)
  expect_identical(
    as.list(R8::r8_select(r$rules, c("rule", "failed", "missing"))),
    list(
      rule = c("a", "b", "order"),
      failed = c(1L, 0L, 1L),
      missing = c(0L, 0L, 2L)
    )
  )
})

test_that("problems are limited, and counts are not", {
  T <- R8::new_table(
    "L",
    list(a = R8::type_double(), b = R8::type_double(max = 5))
  )
  f <- file_of("a,b", rep(c("x,9", "y,1", "1,9"), 100))
  r <- r8_check_file(f, T, limit = 3)
  expect_identical(r$rules$failed, c(200L, 200L))
  expect_identical(r$problems$row, c(1L, 2L, 4L, 1L, 3L, 4L))
  expect_identical(r$unread$cells, c(100L, 100L))
  expect_identical(nrow(r8_check_file(f, T, limit = 0)$problems), 0L)
  refuses(r8_check_file(f, T, limit = -1), "`limit`", "r8_argument_error")
})

test_that("a report prints what holds, what does not read, and what was recoded", {
  T <- labs_type()
  text <- format(r8_check_file(
    file_of(labs_lines),
    T,
    recode_in = list(lab = c("<0.1" = 0.1))
  ))
  text <- paste(text, collapse = "\n")
  for (part in c(
    "Texts that do not read ($unread)",
    "lab    \"hemol\"       1 cell, first in row 4",
    "Texts that read as given values ($recoded)",
    "lab  \"<0.1\" as 0.1  2 cells"
  )) {
    expect_match(text, part, fixed = TRUE)
  }
  good <- format(r8_check_file(file_of(labs_lines[1:2]), T))
  expect_match(paste(good, collapse = "\n"), "All 6 checks", fixed = TRUE)
  # Lines of another width are left out, and said.
  r <- r8_check_file(file_of(labs_lines[1:2], "1,2", "3"), T)
  expect_identical(as.list(r$lines), list(line = c(3, 4), cells = c(2L, 1L)))
  expect_match(
    paste(format(r), collapse = "\n"),
    "2 lines with another number of cells than the header are left out",
    fixed = TRUE
  )
})

test_that("a report is a value", {
  r <- r8_check_file(file_of(labs_lines), labs_type())
  expect_true(R8::r8_equal(
    R8::r8_from_json(type_file_report, R8::r8_to_json(r)),
    r
  ))
})
