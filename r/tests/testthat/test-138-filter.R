# test-138-filter.R
# ::R8::
# 2026- EDG rtemis.org

# Rules that choose the rows of tables: what is kept, what is refused, and rules
# as values. The conformance corpus has the semantics of each operator.

refuses <- function(expr, text) {
  expect_error(expr, text, fixed = TRUE, class = "r8_value_error")
}

# A table of the namespace of this file.
declared <- function(...) {
  op <- options(r8.namespace = "filters")
  on.exit(options(op))
  new_table(...)
}

# The rows of `x` that `rule` keeps, by a column of their numbers.
kept <- function(x, rule) r8_filter(x, rule)$row

visits <- function() {
  Raven(
    row = 1:6,
    age = c(17, 18, NA, NaN, 80, 0),
    sex = c("f", "m", "f", NA, "m", "f"),
    stage = factor(c("I", "II", NA, "I", "III", "II")),
    day = as.Date("2020-01-01") + c(0, 1, NA, 3, 4, 5),
    at = as.POSIXct("2020-01-01", tz = "UTC") + c(0, 1, 2, NA, 4.25, 5),
    smoker = c(TRUE, FALSE, NA, TRUE, FALSE, TRUE),
    n = c(1L, NA, 3L, 4L, 5L, 6L)
  )
}

test_that("r8_filter() keeps the rows where a rule is true", {
  x <- visits()
  expect_identical(kept(x, rule(age >= 18)), c(2L, 5L))
  expect_identical(kept(x, rule(!(age >= 18))), c(1L, 6L))
  expect_identical(kept(x, rule(is.na(age))), 3:4)
  expect_identical(kept(x, rule(age >= 18 || smoker)), c(1L, 2L, 4L, 5L, 6L))
  expect_identical(kept(x, rule(smoker)), c(1L, 4L, 6L))
  expect_identical(kept(x, rule(sex == "f" && n > 1)), c(3L, 6L))
  expect_identical(kept(x, rule(sex %in% c("f", "x"))), c(1L, 3L, 6L))
  expect_identical(kept(x, rule(stage == "II")), c(2L, 6L))
  expect_identical(kept(x, rule(stage != "II")), c(1L, 4L, 5L))
  expect_identical(kept(x, rule(nchar(sex) == 1)), c(1:3, 5:6))
  expect_identical(kept(x, rule(pmin(n, 4) == 4)), 4:6)
  # Filters keep the type, the order, and every column.
  expect_identical(names(r8_filter(x, rule(smoker))), names(x))
  expect_s3_class(r8_filter(x, rule(smoker)), "R8::Raven")
  expect_identical(nrow(r8_filter(x, rule(n > 100))), 0L)
  empty <- x[integer(), ]
  expect_identical(nrow(r8_filter(empty, rule(age > mean(age)))), 0L)
})

test_that("a rule on numbers keeps what R's condition keeps", {
  set.seed(1)
  n <- 3000L
  d <- data.frame(
    row = seq_len(n),
    x = rnorm(n),
    y = sample(c(NA, NaN, Inf, -Inf, 0, 1.5), n, TRUE),
    i = sample(c(NA, -3:3), n, TRUE),
    s = sample(c(NA, "a", "bb", "\u00e9"), n, TRUE),
    f = factor(sample(c(NA, "u", "v"), n, TRUE))
  )
  r <- Raven(d)
  same <- function(rule, rows) expect_identical(kept(r, rule), which(rows))
  same(rule(x > 0.5), d$x > 0.5)
  same(rule(x + y * 2 >= i), d$x + d$y * 2 >= d$i)
  same(rule(abs(x) < 1 && !is.na(i)), abs(d$x) < 1 & !is.na(d$i))
  same(rule(i > 0 || y == 0), d$i > 0 | d$y == 0)
  same(rule(!(i > 0 && y == 0)), !(d$i > 0 & d$y == 0))
  same(rule(sqrt(abs(x)) > 1), sqrt(abs(d$x)) > 1)
  same(rule(exp(x) > 2), exp(d$x) > 2)
  same(rule(log(abs(x)) > 0), log(abs(d$x)) > 0)
  same(rule(round(x) == i), round(d$x) == d$i)
  same(rule(x > mean(x)), d$x > mean(d$x))
  same(rule(x == max(x) || x == min(x)), d$x == max(d$x) | d$x == min(d$x))
  same(rule(sum(x) > x), sum(d$x) > d$x)
  same(rule(pmax(x, i) > 1), pmax(d$x, d$i) > 1)
  same(rule(s == "\u00e9"), d$s == "\u00e9")
  same(rule(s != "a"), d$s != "a")
  same(rule(f == "u"), d$f == "u")
  same(rule(if (x > 0) y > 0 else i > 0), ifelse(d$x > 0, d$y > 0, d$i > 0))
  # Strings of another encoding are the same text.
  latin <- Raven(s = iconv(c("\u00e9", "a"), "UTF-8", "latin1"))
  expect_identical(nrow(r8_filter(latin, rule(s == "\u00e9"))), 1L)
})

test_that("a column is an ALTREP vector, a view, or a vector", {
  x <- Raven(i = 1:1e5, d = as.numeric(1:1e5))
  expect_identical(kept(Raven(row = 1:1e5), rule(row > 99998)), 99999:100000)
  expect_identical(nrow(r8_filter(x, rule(i == d && d > 99990))), 10L)
  y <- Raven(a = x$i, b = x$d)
  expect_identical(nrow(r8_filter(y, rule(a == b && b > 99990))), 10L)
})

test_that("dates and date-times are compared with text and with R's values", {
  x <- visits()
  expect_identical(kept(x, rule(day >= "2020-01-04")), 4:6)
  since <- as.Date("2020-01-04")
  expect_identical(kept(x, rule(day >= val(since))), 4:6)
  expect_identical(kept(x, rule(day %in% val(since + 0:1))), 4:5)
  expect_identical(
    kept(x, rule(day == max(day))),
    integer()
  )
  expect_identical(kept(x[-3, ], rule(day == max(day))), 6L)
  at <- as.POSIXct("2020-01-01 00:00:04.25", tz = "UTC")
  expect_identical(kept(x, rule(at >= val(at))), 5:6)
  expect_identical(kept(x, rule(at > "2020-01-01T00:00:04Z")), 5:6)
  refuses(r8_filter(x, rule(day >= "2020-13-01")), "is not one: YYYY-MM-DD")
  refuses(
    r8_filter(x, rule(at >= "2020-01-01")),
    "is not one: YYYY-MM-DDTHH:MM:SSZ"
  )
  expect_error(rule(day >= val(as.Date(NA))), "not a constant of a rule")
})

test_that("val() is R's value, taken when rule() runs", {
  x <- visits()
  older <- function(x, age) r8_filter(x, rule(age >= val(age)))
  expect_identical(older(x, 80)$row, 5L)
  rules <- lapply(c(18, 80), function(a) rule(age >= val(a)))
  expect_identical(lapply(rules, function(r) kept(x, r)), list(c(2L, 5L), 5L))
  cutoff <- 18
  at <- rule(age >= val(cutoff))
  cutoff <- 80
  expect_identical(kept(x, at), c(2L, 5L))
})

test_that("a rule is a value: it is saved, combined, and applied to other tables", {
  x <- visits()
  adult <- rule(age >= 18)
  woman <- rule(sex == "f")
  expect_identical(kept(x, !adult), c(1L, 6L))
  expect_identical(kept(x, !adult & woman), c(1L, 6L))
  expect_identical(kept(x, adult | woman), c(1:3, 5:6))
  expect_identical(!(adult & woman), !(adult & woman))
  expect_identical(
    unclass(adult & woman),
    unclass(rule(age >= 18 && sex == "f"))
  )
  back <- unserialize(serialize(adult, NULL))
  expect_identical(kept(x, back), kept(x, adult))
  other <- Raven(age = c(3, 30), name = c("a", "b"))
  expect_identical(r8_filter(other, adult)$name, "b")
  # One rule checks the rows of a type and chooses those of another table.
  Adults <- declared("Adults", list(age = type_double()), rules = adult)
  expect_error(Adults(age = 3), "age >= 18")
  expect_identical(r8_filter(other, adult)$age, 30)
  # A negated rule states another requirement, so that it has no message.
  named <- rule(age >= 18, message = "age is 18 or more.", id = "ADULT")
  expect_identical(unclass(!named), unclass(!adult))
  expect_error(!rule(age > 1, when = sex == "f"), "without `when`")
  expect_error(adult & TRUE, "makes a rule of two rules")
  expect_error(adult == woman, "is not an operation on rules")
})

test_that("a rule is checked against the columns before a row is read", {
  x <- visits()
  refuses(
    r8_filter(x, rule(Age >= 18)),
    "`Age` is not a column of the table; the nearest column is `age`; if `Age` is an R variable, write `val(Age)`."
  )
  refuses(r8_filter(x, rule(sex > 2)), "`>` compares")
  refuses(r8_filter(x, rule(sex == 2)), "`==` compares a string with a number")
  refuses(r8_filter(x, rule(age + 1)), "is a logical condition")
  refuses(r8_filter(x, rule(age$x > 1)), "has none")
  refuses(r8_filter(x, rule(is.null(age))), "`is.na` tests a cell")
  refuses(r8_filter(x, rule(length(age) > 1)), "a row reads cells")
  refuses(r8_filter(x, rule(age > 1, when = n > 2)), "join the parts with `&&`")
  expect_error(
    r8_filter(x, age >= 18),
    "a condition on columns is a rule: rule(age >= 18)",
    fixed = TRUE
  )
  expect_error(
    r8_filter(x, TRUE),
    "`rule` is a rule",
    class = "r8_argument_error"
  )
  expect_error(
    r8_filter(iris, rule(a > 1)),
    "value of a table",
    class = "r8_argument_error"
  )
  # A column of a declared type that this value does not have.
  Labs <- declared(
    "Labs",
    list(a = type_double(), b = column(type_double(), required = FALSE)),
    rules = rule(b > a)
  )
  refuses(r8_filter(Labs(a = 1), rule(b > 1)), "`b` is not a column")
})

test_that("an error in a row stops a filter, unless another part decides", {
  x <- Raven(row = 1:4, a = c(1, 0, NA, 4))
  refuses(r8_filter(x, rule(1 / a > 0)), "in row 2: division by zero")
  refuses(
    r8_filter(x, rule(log(a - 1) > 0)),
    "in row 1: `log` of a number that is not positive; nor in 1 other row"
  )
  refuses(r8_filter(x, rule(sqrt(a - 2) > 0)), "`sqrt` of a negative number")
  expect_identical(kept(x, rule(a != 0 && 1 / a > 0.5)), 1L)
  expect_identical(kept(x, rule(1 / a > 0.5 && a != 0)), 1L)
})

test_that("names of columns are whole, and `$` makes a path", {
  x <- Raven(
    row = 1:2,
    Petal.Width = c(1, 2),
    `Age (years)` = c(10, 50),
    .x = c(5, 6)
  )
  expect_identical(kept(x, rule(Petal.Width > 1)), 2L)
  expect_identical(kept(x, rule(`Age (years)` > 40)), 2L)
  expect_identical(kept(x, rule(.x > 5)), 2L)
  expect_identical(format(rule(Petal.Width > 1)), "<r8 rule> Petal.Width > 1")
  expect_identical(
    format(rule(`Age (years)` > 40)),
    "<r8 rule> `Age (years)` > 40"
  )
  # A symbol with a dot is no path into a struct.
  expect_error(
    declare_in(
      "filters",
      new_struct(
        "P",
        list(kernel = type_json()),
        rules = rule(kernel.gamma > 1)
      )
    ),
    "`kernel.gamma` is not a field in scope"
  )
})

test_that("the rows of a declared table are checked by column", {
  Labs <- declared(
    "Labs2",
    list(
      a = type_double(),
      b = column(type_double(), required = FALSE),
      s = option(type_factor(c("lo", "hi")))
    ),
    rules = list(rule(b > a), rule(s == "hi" || a > 0), rule(1 / a > -1))
  )
  # A column that may be absent: its rule holds for a value without it.
  expect_s3_class(Labs(a = 1, s = factor("lo", c("lo", "hi"))), "r8_table")
  hi <- factor(c("hi", "lo"), c("lo", "hi"))
  expect_s3_class(Labs(a = 1:2 / 1, b = c(3, 4), s = hi), "r8_table")
  expect_error(Labs(a = c(1, 2), b = c(3, 2), s = hi), "found b = 2, a = 2")
  expect_error(
    Labs(a = c(1, -2), s = hi),
    "found s = \"lo\", a = -2"
  )
  expect_error(
    Labs(a = c(0, 1), s = hi),
    "division by zero. [rule_error at",
    fixed = TRUE
  )
  expect_error(
    declared("L3", list(a = type_double()), rules = rule(mean(a) > 1)),
    "reads one row"
  )
  expect_error(
    declared(
      "L3",
      list(a = type_double(), b = type_double()),
      rules = rule(min(a, b) > 1)
    ),
    "`pmin` is the least of the cells of a row"
  )
  expect_error(
    declared(
      "L3",
      list(s = type_factor(c("lo", "hi"))),
      rules = rule(s == "mid")
    ),
    "is not a level of `s`, whose levels are"
  )
})

test_that("r8_select() gives columns by name or by position", {
  x <- visits()
  expect_identical(names(r8_select(x, c("sex", "age"))), c("sex", "age"))
  expect_identical(names(r8_select(x, 2:1)), c("age", "row"))
  refuses(r8_select(x, "Age"), "the nearest column is `age`")
  refuses(r8_select(x, c("age", "age")), "twice")
  refuses(r8_select(x, 99), "there are 8 columns")
  for (bad in list(-1, TRUE, NULL, NA_real_, factor("age"), rule(age > 1))) {
    expect_error(r8_select(x, bad), "are names", class = "r8_argument_error")
  }
  expect_error(r8_select(x, age), "c(\"age\")", fixed = TRUE)
  Labs <- declared("Labs4", list(a = type_double(), b = type_double()))
  expect_s3_class(r8_select(Labs(a = 1, b = 2), "a"), "R8::Raven")
})
