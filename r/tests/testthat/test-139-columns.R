# test-139-columns.R
# ::R8::
# 2026- EDG rtemis.org

# The functions on tables that change columns and order rows: r8set(), r8drop(),
# r8rename(), and r8sort().

refuses <- function(expr, text, class = "r8_value_error") {
  expect_error(expr, text, fixed = TRUE, class = class)
}

patients <- function(ns, open = TRUE) {
  declare_in(ns, {
    new_table(
      "Patients",
      columns = list(
        weight = column(type_double(min = 0), unit = "kg", description = "W"),
        height = column(type_double(min = 0), unit = "m"),
        id = column(type_integer(), unique = TRUE, required = FALSE)
      ),
      open = open,
      rules = rule(height < 3)
    )
  })
}

test_that("r8set() adds and replaces columns, and leaves its table as it is", {
  r <- Raven(w = c(60, 82), h = c(1.6, 1.8))
  s <- r8set(r, list(bmi = r$w / r$h^2, w = c(1, 2)))
  expect_identical(names(s), c("w", "h", "bmi"))
  expect_identical(s$w, c(1, 2))
  expect_identical(s$bmi, r$w / r$h^2)
  expect_identical(r$w, c(60, 82))
  # A column of any name, those of the arguments among them.
  odd <- r8set(r, list(x = 1:2, columns = 3:4, `a b` = 5:6))
  expect_identical(odd$x, 1:2)
  expect_identical(odd$columns, 3:4)
  # A data frame or a table gives its columns.
  expect_identical(
    names(r8set(r, data.frame(a = 1:2, b = 3:4))),
    c("w", "h", "a", "b")
  )
  expect_identical(r8set(r, Raven(a = 1:2)), r8set(r, list(a = 1:2)))
  expect_identical(r8set(r, list()), r)
  bad <- function(expr, text) refuses(expr, text, "r8_argument_error")
  bad(r8set(r, c(a = 1)), "r8set(x, list(bmi = w / h^2))")
  bad(r8set(r, list(1:2)), "is a named list")
  bad(r8set(r, list(w = NULL)), "r8drop()")
  bad(r8set(iris, list(a = 1)), "a value of a table")
  refuses(r8set(r, list(a = 1)), "must have 2 cells")
  refuses(
    r8set(r, list(a = 1:2, a = 3:4)),
    "Two entries of `columns` are named `a`.",
    "r8_argument_error"
  )
  # A declared column is checked, with its rules.
  P <- patients("col1")
  p <- P(weight = c(60, 82), height = c(1.6, 1.8))
  expect_s3_class(r8set(p, list(bmi = 1:2)), "col1::Patients")
  refuses(r8set(p, list(weight = c(-1, 2))), "at least 0")
  refuses(r8set(p, list(height = c(1, 4))), "height < 3")
  # The columns of a data.table are copied.
  skip_if_not_installed("data.table")
  dt <- data.table::data.table(a = c(1, 2))
  s <- r8set(r, dt)
  data.table::set(dt, 1L, "a", -1)
  expect_identical(s$a, c(1, 2))
})

test_that("r8drop() gives a table without columns", {
  r <- Raven(a = 1:2, b = 3:4, c = 5:6)
  expect_identical(names(r8drop(r, "b")), c("a", "c"))
  expect_identical(names(r8drop(r, c("c", "a"))), "b")
  expect_identical(names(r8drop(r, 2:3)), "a")
  expect_identical(r8drop(r, character()), r)
  refuses(r8drop(r, "z"), "`z` is not a column")
  refuses(r8drop(r, c("a", "a")), "twice")
  refuses(r8drop(r, -1), "are names", "r8_argument_error")
  refuses(r8drop(r, b), "c(\"b\")")
  P <- patients("col2")
  p <- P(weight = 60, height = 1.6, extra = 1)
  expect_s3_class(r8drop(p, "extra"), "col2::Patients")
  q <- r8drop(p, "height")
  expect_s3_class(q, "R8::Raven")
  expect_identical(r8columns(q)$unit, c("kg", NA))
})

test_that("r8rename() gives columns other names, and their facts go with them", {
  r <- r8annotate(Raven(a = 1:2, b = 3:4, c = 5:6), unit = c(b = "m"))
  s <- r8rename(r, "b", "length")
  expect_identical(names(s), c("a", "length", "c"))
  expect_identical(r8columns(s)$unit, c(NA, "m", NA))
  expect_identical(s$length, r$b)
  # Names are exchanged at once, with their facts.
  x <- r8rename(r, c("a", "b"), c("b", "a"))
  expect_identical(x$b, 1:2)
  expect_identical(r8columns(x)$unit, c(NA, "m", NA))
  expect_identical(names(x), c("b", "a", "c"))
  expect_identical(names(r8rename(r, 3, "z")), c("a", "b", "z"))
  refuses(r8rename(r, "z", "y"), "`z` is not a column")
  refuses(r8rename(r, "a", "b"), "columns are named `b`")
  refuses(r8rename(r, c("a", "b"), "z"), "one name for each column")
  refuses(r8rename(r, "a", "$a"), "not a name of a column")
  refuses(r8rename(r, "a", 1), "`to` holds names", "r8_argument_error")
  # A declared column under another name: the table is a raven.
  P <- patients("col3")
  p <- P(weight = 60, height = 1.6, number = 7L)
  q <- r8rename(p, "weight", "mass")
  expect_s3_class(q, "R8::Raven")
  expect_identical(r8columns(q)$unit, c("kg", "m", NA))
  expect_identical(r8columns(q)$description, c("W", NA, NA))
  # A column that takes a declared name is checked as that column.
  expect_s3_class(r8rename(p, "number", "id"), "col3::Patients")
  twice <- P(weight = c(60, 61), height = c(1.6, 1.7), number = c(7L, 7L))
  refuses(r8rename(twice, "number", "id"), "more than once")
  # A closed table takes no other name.
  C <- patients("col4", open = FALSE)
  expect_s3_class(
    r8rename(C(weight = 60, height = 1.6, id = 1L), "id", "number"),
    "R8::Raven"
  )
})

test_that("r8sort() orders the rows, missing cells last", {
  x <- Raven(
    row = 1:6,
    d = c(2, NA, -1, NaN, 0, -0),
    i = c(3L, NA, -5L, 3L, 0L, 1L),
    s = c("b", "a", NA, "B", "é", "a"),
    f = factor(c("lo", "hi", NA, "lo", "mid", "hi"), c("lo", "mid", "hi")),
    l = c(TRUE, FALSE, NA, TRUE, FALSE, TRUE),
    t = as.Date("2020-01-01") + c(5, 1, NA, 0, 2, 2)
  )
  by <- function(...) r8sort(x, ...)$row
  expect_identical(by("d"), c(3L, 5L, 6L, 1L, 2L, 4L))
  expect_identical(by("d", descending = "d"), c(1L, 5L, 6L, 3L, 2L, 4L))
  expect_identical(by("i"), c(3L, 5L, 6L, 1L, 4L, 2L))
  expect_identical(by("i", "i"), c(1L, 4L, 6L, 5L, 3L, 2L))
  expect_identical(by("s"), c(4L, 2L, 6L, 1L, 5L, 3L))
  expect_identical(by("s", "s"), c(5L, 1L, 2L, 6L, 4L, 3L))
  expect_identical(by("f"), c(1L, 4L, 5L, 2L, 6L, 3L))
  expect_identical(by("l"), c(2L, 5L, 1L, 4L, 6L, 3L))
  expect_identical(by("t"), c(4L, 2L, 5L, 6L, 1L, 3L))
  expect_identical(by(c("l", "i"), descending = "i"), c(5L, 2L, 1L, 4L, 6L, 3L))
  expect_identical(by(character()), 1:6)
  expect_identical(nrow(r8sort(x[integer(), ], "d")), 0L)
  # One text in two encodings has one place.
  e <- Raven(
    row = 1:3,
    s = c("é", iconv("é", "UTF-8", "latin1"), "é")
  )
  expect_identical(r8sort(e, "s")$row, 1:3)
  refuses(r8sort(x, "z"), "`z` is not a column")
  refuses(r8sort(x, "d", "i"), "`i` descends, and is not among")
  refuses(r8sort(x, TRUE), "`by` are names", "r8_argument_error")
})

test_that("r8sort() gives R's order, with ties in the order they are in", {
  set.seed(1)
  n <- 5000L
  d <- data.frame(
    row = seq_len(n),
    a = sample(c(NA, NaN, Inf, -Inf, round(rnorm(50), 2)), n, TRUE),
    b = sample(c(NA, -3:3), n, TRUE),
    s = sample(c(NA, "x", "yy", "Z", "zz"), n, TRUE)
  )
  r <- Raven(d)
  same <- function(by, descending = NULL) {
    keys <- lapply(by, function(k) d[[k]])
    o <- do.call(
      order,
      c(
        keys,
        list(
          decreasing = by %in% descending,
          na.last = TRUE,
          method = "radix"
        )
      )
    )
    expect_identical(r8sort(r, by, descending)$row, o)
  }
  same("a")
  same("a", "a")
  same("b", "b")
  same("s")
  same(c("s", "a"))
  same(c("b", "s", "a"), c("s", "a"))
  # A compact sequence.
  expect_identical(r8sort(Raven(i = 5:1), "i")$i, 1:5)
})

test_that("long columns are in R's order", {
  set.seed(2)
  n <- 2e5
  d <- data.frame(
    row = seq_len(n),
    # Keys that differ in many bits, few values, one value, and nearly in order.
    a = rnorm(n) * 10^sample(-300:300, n, TRUE),
    b = sample(c(NA, NaN, Inf, -Inf, 0, 1e-300, rnorm(20)), n, TRUE),
    c = rep(1.5, n),
    d = cumsum(runif(n)) + c(runif(n - 1), -1e9),
    id = sprintf("id-%07d", sample.int(n)),
    s = sample(c(NA, "", "a", "ab", "\u00e9", strrep("x", 30)), n, TRUE)
  )
  r <- Raven(d)
  same <- function(by, descending = NULL) {
    o <- do.call(
      order,
      c(
        unname(as.list(d[by])),
        list(decreasing = by %in% descending, na.last = TRUE, method = "radix")
      )
    )
    expect_identical(r8sort(r, by, descending)$row, o)
  }
  same("a")
  same("a", "a")
  same("b")
  same(c("b", "a"), "b")
  same("c")
  same("d")
  same("id")
  same("s", "s")
  same(c("s", "a"))
  skip_on_cran()
  # Two threads.
  n <- 6e5
  x <- c(runif(n - 2), NA, NaN)
  expect_identical(r8sort(Raven(x = x, row = seq_len(n)), "x")$row, order(x))
})
