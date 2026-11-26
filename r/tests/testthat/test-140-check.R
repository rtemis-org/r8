# test-140-check.R
# ::R8::
# 2026- EDG rtemis.org

# Checks: r8_check() holds a table against rules or a type and reports what it finds,
# and r8_validate() requires.

visits <- function() {
  data.frame(
    id = c(1L, 2L, 2L, 4L, 5L),
    age = c(40, 152, NA, 17, 61),
    sbp = c(120, 118, 155, NA, 110),
    dbp = c(80, 130, 95, 70, 0),
    ward = c("ED", "ICU", NA, "ED", "hall"),
    stay = as.Date("2026-01-01") + c(1, 2, NA, 4, 5)
  )
}

visits_type <- function(ns, open = TRUE) {
  declare_in(ns, {
    new_table(
      "Visits",
      columns = list(
        id = column(type_integer(min = 1), unique = TRUE),
        age = option(type_double(min = 0, max = 130)),
        sbp = option(type_double(min = 0)),
        dbp = type_double(min = 0),
        ward = type_factor(c("ED", "ICU"))
      ),
      open = open,
      rules = list(rule(dbp < sbp, id = "PRESSURES"), rule(ward == "ED"))
    )
  })
}

rules <- list(
  pressures = rule(dbp < sbp),
  adult = rule(age >= 18, message = "Patients are adults."),
  ratio = rule(sbp / dbp > 1),
  mean_age = rule(mean(dbp) > 10),
  typo = rule(agee > 1),
  kind = rule(ward > 1)
)

test_that("r8_check() counts what each rule is in each row, and refuses nothing", {
  x <- visits()
  r <- r8_check(x, rules)
  expect_s3_class(r, "R8::Report")
  expect_identical(r$rows, 5L)
  expect_null(r$type)
  expect_identical(r$rules$rule, names(rules))
  expect_identical(r$rules$requirement[1:2], c("dbp < sbp", "age >= 18"))
  expect_identical(r$rules$checked, c(5L, 5L, 5L, 1L, 0L, 0L))
  expect_identical(r$rules$failed, c(1L, 1L, 1L, 0L, 0L, 0L))
  expect_identical(r$rules$missing, c(1L, 1L, 1L, 0L, 0L, 0L))
  expect_identical(r$rules$errors, c(0L, 0L, 1L, 0L, 0L, 0L))
  p <- as.data.frame(r$problems)
  expect_identical(
    p$rule,
    c("pressures", "adult", "ratio", "ratio", "typo", "kind")
  )
  expect_identical(p$row, c(2L, 4L, 2L, 5L, NA, NA))
  expect_identical(p$columns[1:3], c("dbp, sbp", "age", "sbp, dbp"))
  expect_identical(p$found[1:2], c("dbp = 130, sbp = 118", "age = 17"))
  expect_identical(
    p$code,
    c(
      "rule_violation",
      "rule_violation",
      "rule_violation",
      "rule_error",
      "rule_field",
      "rule_type"
    )
  )
  expect_identical(p$message[1:2], c("dbp < sbp", "Patients are adults."))
  expect_identical(p$message[[4L]], "sbp/dbp > 1: division by zero")
  expect_match(p$message[[5L]], "the nearest column is `age`")
  expect_match(p$message[[6L]], "found a string and a number")
  # The same of a raven, and the data are as they were.
  expect_identical(r8_check(Raven(x), rules), r)
  expect_identical(x, visits())
  # The tables of a report are tables.
  expect_identical(
    r8_filter(r$problems, rule(row == 2))$rule,
    c("pressures", "ratio")
  )
  expect_true(r8_equal(r8_from_json(type_report, r8_to_json(r)), r))
})

test_that("a rule is named by the list, its id, or its place", {
  x <- visits()
  named <- function(...) r8_check(x, ...)$rules$rule
  expect_identical(named(rule(dbp >= 0)), "rule 1")
  expect_identical(named(rule(dbp >= 0, id = "LOW")), "LOW")
  expect_identical(
    named(list(
      a = rule(dbp >= 0),
      rule(id > 0),
      rule(id > 1, id = "B"),
      C = rule(id > 2, id = "C")
    )),
    c("a", "rule 2", "B", "C")
  )
  bad <- function(against, text) {
    expect_error(
      r8_check(x, against),
      text,
      fixed = TRUE,
      class = "r8_argument_error"
    )
    expect_error(
      r8_validate(x, against),
      text,
      fixed = TRUE,
      class = "r8_argument_error"
    )
  }
  bad(
    list(low = rule(dbp >= 0, id = "LOW")),
    "The rule named `low` in `against` has the ID `LOW`; give one of the two."
  )
  bad(list(`age ok` = rule(age > 1)), "is its ID, an R8 name")
  bad(list(a = rule(dbp >= 0), a = rule(id > 0)), "have the ID `a`")
  bad(list(a = rule(dbp >= 0), rule(id > 0, id = "a")), "have the ID `a`")
})

test_that("one list of rules declares, checks, and reports under the same names", {
  set <- list(pressures = rule(dbp < sbp), adult = rule(age >= 18))
  V <- declare_in("chk4", {
    new_table(
      "V",
      columns = list(
        age = option(type_double()),
        sbp = option(type_double()),
        dbp = type_double()
      ),
      open = TRUE,
      rules = set
    )
  })
  x <- visits()
  of_type <- r8_check(x, V)
  of_rules <- r8_check(x, set)
  expect_identical(tail(of_type$rules$rule, 2L), names(set))
  expect_identical(
    as.data.frame(r8_filter(of_type$problems, rule(rule %in% val(names(set))))),
    as.data.frame(of_rules$problems)
  )
  expect_error(V(age = 17, sbp = 100, dbp = 80), "Rule adult: age >= 18")
})

test_that("a condition of a rule, and a rule without an answer, are told apart", {
  x <- data.frame(a = c(1, NA, 3, 4), b = c(TRUE, TRUE, NA, FALSE))
  r <- r8_check(x, rule(a > 3, when = b))
  expect_identical(
    as.list(r$rules)[c("checked", "failed", "missing")],
    list(checked = 4L, failed = 1L, missing = 2L)
  )
})

test_that("a problem shows what its rule read", {
  x <- data.frame(x = c(1, 2, 3, 10), id = c(1L, 2L, 2L, 3L))
  r <- r8_check(x, list(s = rule(sum(x) < 10), m = rule(x <= mean(x))))
  expect_identical(r$problems$found, c("sum(x) = 16", "x = 10, mean(x) = 4"))
  expect_identical(r$problems$columns, c("x", "x"))
  r <- r8_check(x, list(s = rule(x * 2 < 10, when = id > 0)))
  expect_identical(r$problems$found, "x = 10, id = 3, x * 2 = 20")
  T <- declare_in("chk_found", {
    new_table(
      "T",
      list(
        id = column(type_integer(), unique = TRUE),
        x = column(type_double(max = 5))
      )
    )
  })
  p <- as.data.frame(r8_check(x, T)$problems)
  expect_identical(p$found, c("id = 2", "x = 10"))
  expect_identical(
    p$message,
    c("2 appears more than once", "must be at most 5")
  )
})


test_that("the problems listed are limited, and the counts are not", {
  x <- data.frame(a = seq_len(5000))
  r <- r8_check(x, list(rule(a > 4000), rule(a > 0)), limit = 10)
  expect_identical(r$rules$failed, c(4000L, 0L))
  expect_identical(r$problems$row, 1:10)
  expect_match(format(r), "lists 10 of 4,000 problems", all = FALSE)
  expect_identical(nrow(r8_check(x, rule(a > 4000), limit = 0)$problems), 0L)
  for (bad in list(-1, NA, 1.5, "a", 1:2)) {
    expect_error(
      r8_check(x, rule(a > 1), limit = bad),
      "`limit`",
      class = "r8_argument_error"
    )
  }
})

test_that("r8_check() holds data against the columns and the rules of a type", {
  V <- visits_type("chk1")
  x <- visits()
  r <- r8_check(x, V)
  expect_identical(r$type, "chk1::Visits")
  of <- as.data.frame(r$rules)
  expect_identical(
    of$rule,
    c("id", "age", "sbp", "dbp", "ward", "PRESSURES", "rule 2")
  )
  expect_identical(of$requirement[[2L]], "optional double [0, 130]")
  expect_identical(of$checked, c(5L, 5L, 5L, 5L, 0L, 5L, 0L))
  expect_identical(of$failed, c(1L, 1L, 0L, 0L, 0L, 1L, 0L))
  p <- as.data.frame(r$problems)
  expect_identical(p$row, c(3L, 2L, NA, 2L, NA))
  expect_identical(p$code[1:3], c("not_unique", "above_max", "r_type"))
  expect_identical(p$found[[2L]], "age = 152")
  expect_identical(p$message[[2L]], "must be at most 130")
  expect_match(p$message[[3L]], "must be a factor")
  expect_match(p$message[[5L]], "the cells of `ward` do not read")
  # A rule is read where its columns read, though other cells are refused.
  x$ward <- factor(x$ward, c("ED", "ICU"))
  r <- r8_check(x, V)
  expect_identical(tail(as.list(r$rules)$failed, 2L), c(1L, 1L))
  expect_identical(tail(as.list(r$rules)$missing, 2L), c(1L, 2L))
  # What a type does not have, and what it requires.
  C <- visits_type("chk2", open = FALSE)
  r <- r8_check(visits()[c("id", "age", "stay")], C)
  p <- as.data.frame(r$problems)
  expect_identical(
    p$code[p$rule %in% c("sbp", "dbp", "ward", "stay")],
    c("missing_column", "missing_column", "missing_column", "unknown_column")
  )
  # A value of the type holds.
  v <- V(
    id = 1L,
    age = 40,
    sbp = 120,
    dbp = 80,
    ward = factor("ED", c("ED", "ICU"))
  )
  expect_match(
    format(r8_check(v, V)),
    "All 7 checks of chk1::Visits hold",
    all = FALSE
  )
  # A field's type.
  t <- type_table(list(age = type_double(max = 100)), open = TRUE)
  r <- r8_check(visits(), t)
  expect_identical(r$rules$failed, 2L)
  # Counts are of rows, where a cell has two problems.
  two <- type_table(list(
    a = column(type_double(min = 0, max = 1, values = c(0, 1)))
  ))
  r <- r8_check(data.frame(a = c(-1, 0.5, 2)), two)
  expect_identical(r$rules$failed, 3L)
  expect_identical(nrow(r$problems), 5L)
})

test_that("foreign data are checked as they are", {
  # A column of a class that no table takes, names that repeat, and row names.
  x <- data.frame(a = 1:2, row.names = c("p", "q"))
  x$wait <- as.difftime(c(3, 5), units = "days")
  x$parts <- list(1, "b")
  r <- r8_check(x, list(rule(a > 0), rule(wait > 1), rule(parts == 1)))
  expect_identical(r$rules$checked, c(2L, 0L, 0L))
  expect_match(r$problems$message[[1L]], "is a `difftime`")
  expect_match(r$problems$message[[2L]], "is a list")
  twice <- setNames(data.frame(1:2, 3:4), c("a", "a"))
  expect_match(
    r8_check(twice, rule(a > 0))$problems$message,
    "two columns are named `a`"
  )
  skip_if_not_installed("data.table")
  dt <- data.table::data.table(a = c(1, -1))
  expect_identical(r8_check(dt, rule(a > 0))$problems$row, 2L)
  expect_identical(r8_check(data.frame(), rule(1 > 0))$rows, 0L)
})

test_that("an object that is no table is held against its type", {
  r <- r8_check(c(1, -1), vec(type_double(min = 0)))
  expect_identical(r$rules$failed, 1L)
  expect_identical(r$problems$row, 2L)
  expect_match(format(r), "The check does not hold", all = FALSE)
  bad <- function(expr, text) {
    expect_error(expr, text, fixed = TRUE, class = "r8_argument_error")
  }
  bad(r8_check(1, rule(a > 1)), "`x` is a table")
  bad(r8_check(visits(), "a"), "`against` is a rule()")
  bad(r8_check(visits(), list()), "`against` is a rule()")
  bad(r8_check(visits(), list(1)), "`against[[1]]` is not a rule()")
})

test_that("a report prints what holds and what does not", {
  x <- visits()
  shown <- function(...) format(r8_check(x, ...))
  expect_identical(
    shown(rules[c("pressures", "mean_age", "typo")]),
    c(
      "1 of 3 rules holds on 5 rows",
      "  pressures  1 row fails, 1 row without an answer (row 2)",
      "  mean_age   holds",
      "  typo       is not checked: `agee` is not a column of the table; the nearest column is `age`",
      "  $problems lists 2 problems"
    )
  )
  expect_identical(
    shown(list(a = rule(dbp >= 0), b = rule(id > 0))),
    c("Both rules hold on 5 rows", "  a  holds", "  b  holds")
  )
  expect_identical(shown(rule(id > 0))[[1L]], "The rule holds on 5 rows")
  many <- lapply(1:30, function(k) rule(id > val(k)))
  expect_match(shown(many), "and 10 more that do not hold", all = FALSE)
})

test_that("r8_validate() requires what r8_check() reports", {
  x <- visits()
  expect_identical(withVisible(r8_validate(x, rule(id > 0)))$visible, FALSE)
  expect_identical(r8_validate(x, rules["mean_age"]), x)
  e <- tryCatch(r8_validate(x, rules[c("adult", "mean_age")]), error = identity)
  expect_s3_class(e, "r8_value_error")
  expect_match(conditionMessage(e), "Invalid `x`: 1 of 2 rules holds on 5 rows")
  expect_match(conditionMessage(e), "adult .* 1 row fails")
  expect_s3_class(e$report, "R8::Report")
  expect_error(r8_validate(x, rules["typo"]), "is not checked")
  V <- visits_type("chk3")
  expect_error(r8_validate(x, V), "Invalid `x`", class = "r8_value_error")
  expect_error(r8_validate(x, 1), "`against`", class = "r8_argument_error")
})

test_that("r8_validate() of a table gives the problems that the type finds", {
  T <- declare_in("chk_require", {
    new_table(
      "T",
      list(
        id = column(type_integer(), unique = TRUE),
        x = type_double(max = 5)
      ),
      rules = list(order = rule(id < x))
    )
  })
  good <- data.frame(id = 1:2, x = c(2, 3))
  bad <- data.frame(id = c(1L, 1L), x = c(2, 9))
  for (f in list(identity, Raven)) {
    expect_identical(r8_validate(f(good), T), f(good))
    e <- expect_error(r8_validate(f(bad), T), class = "r8_value_error")
    expect_match(
      conditionMessage(e),
      "$x[[2]]` must be at most 5; found 9",
      fixed = TRUE
    )
    expect_match(conditionMessage(e), "1 appears more than once", fixed = TRUE)
  }
  expect_identical(r8_validate(T(good), T), T(good))
  expect_error(
    r8_validate(data.frame(id = 3L, x = 2), T),
    "order",
    class = "r8_value_error"
  )
  expect_error(
    r8_validate(1, T),
    "must be a value of",
    class = "r8_value_error"
  )
  expect_error(
    r8_validate(T(good), complete = "yes"),
    "`complete` is TRUE or FALSE.",
    class = "r8_argument_error"
  )
})


test_that("r8_to_data_frame() gives the columns as a data frame", {
  r <- Raven(a = 1:3, b = c("x", "y", "z"))
  expect_identical(
    r8_to_data_frame(r),
    data.frame(a = 1:3, b = c("x", "y", "z"))
  )
  expect_error(
    r8_to_data_frame(iris),
    "value of a table",
    class = "r8_argument_error"
  )
})
