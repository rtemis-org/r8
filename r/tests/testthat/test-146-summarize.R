# test-146-summarize.R
# ::R8::
# 2026- EDG rtemis.org

# Expressions as values, columns computed from them, summaries of the groups
# of a table's rows, and maps of one expression or typed function over
# columns. The conformance corpus (conformance/summarize/) has the semantics
# of each aggregate.
# spec: R8/raven#summaries

refuses <- function(expr, text, class = "r8_value_error") {
  expect_error(expr, text, fixed = TRUE, class = class)
}

vitals <- function() {
  r8annotate(
    Raven(
      ward = factor(
        c(
          "ICU",
          "Cardiology",
          "ICU",
          "Oncology",
          "Cardiology",
          NA,
          "ICU",
          "Oncology"
        ),
        levels = c("ICU", "Cardiology", "Oncology")
      ),
      sex = c("F", "M", "M", "F", "F", "M", "F", "M"),
      age = c(71L, 58L, 64L, 49L, 77L, 35L, 82L, 60L),
      weight = c(62, 88, 79, 55, 70, 92, 58, 81),
      height = c(1.5, 2, 1.75, 1.5, 1.25, 2, 1.5, 1.75),
      sbp = c(142, 128, NA, 118, 151, 122, 160, NA),
      day = as.Date("2020-01-01") + c(2, 0, 1, NA, 4, 3, 6, 5)
    ),
    unit = c(age = "year", weight = "kg", height = "m", sbp = "mmHg")
  )
}

units_of <- function(x) {
  a <- r8columns(x)
  stats::setNames(a[["unit"]], a[["name"]])
}

test_that("calc() holds an expression of the dialect", {
  e <- calc(weight / height^2)
  expect_s3_class(e, "r8_calc")
  expect_identical(format(e), "<r8 calc> weight/height^2")
  expect_identical(
    format(calc(mean(sbp, na.rm = TRUE))),
    "<r8 calc> mean(sbp, na.rm = TRUE)"
  )
  expect_identical(
    format(calc(if (age == 35L) NA else age)),
    "<r8 calc> if (age == 35L) NA else age"
  )
  # col(m) is the column that the string m names; quote() in package code.
  m <- "auc"
  expect_identical(calc(median(col(m)))$expr, calc(median(auc))$expr)
  expect_identical(calc(quote(mean(auc)))$expr, calc(mean(auc))$expr)
  expect_identical(format(calc(median(col()))), "<r8 calc> median(col())")
  refuses(calc(mean(col(auc))), "col(\"auc\")", class = "r8_declaration_error")
  refuses(
    calc(mean(col(1))),
    "gives one column name",
    class = "r8_declaration_error"
  )
  refuses(
    calc(mean(x, trim = 0.1)),
    "takes `na.rm` and no argument `trim`",
    class = "r8_declaration_error"
  )
  refuses(
    calc(mean(x, na.rm = NA)),
    "is TRUE or FALSE",
    class = "r8_declaration_error"
  )
})

test_that("r8summarize() gives one row for each group, in the order of the keys", {
  s <- r8summarize(
    vitals(),
    list(
      n = calc(n()),
      sbp = calc(mean(sbp, na.rm = TRUE)),
      sbp_all = calc(mean(sbp)),
      age = calc(median(age))
    ),
    by = "ward"
  )
  expect_identical(class(s)[[1L]], "R8::Raven")
  # The levels, then the missing key.
  expect_identical(as.character(s$ward), c("ICU", "Cardiology", "Oncology", NA))
  expect_identical(levels(s$ward), c("ICU", "Cardiology", "Oncology"))
  expect_identical(s$n, c(3L, 2L, 2L, 1L))
  expect_identical(s$sbp, c(151, 139.5, 118, 122))
  # A mean over a missing cell is missing.
  expect_identical(s$sbp_all, c(NA, 139.5, NA, 122))
  expect_identical(s$age, c(71, 67.5, 54.5, 35))
  expect_identical(
    units_of(s),
    c(ward = NA, n = NA, sbp = "mmHg", sbp_all = "mmHg", age = "year")
  )
})

test_that("two keys order by the first, then the second; strings by code point", {
  x <- vitals()
  s <- r8summarize(
    x,
    list(n = calc(n()), w = calc(max(weight))),
    by = c("sex", "ward")
  )
  expect_identical(s$sex, c("F", "F", "F", "M", "M", "M", "M"))
  expect_identical(
    as.character(s$ward),
    c("ICU", "Cardiology", "Oncology", "ICU", "Cardiology", "Oncology", NA)
  )
  expect_identical(s$w, c(62, 70, 55, 79, 88, 81, 92))
  # Keys by position.
  expect_identical(r8summarize(x, list(n = calc(n())), by = 2L)$n, c(4L, 4L))
})

test_that("a summary without keys is one row, also of no rows", {
  x <- vitals()
  s <- r8summarize(
    x,
    list(
      n = calc(n()),
      w = calc(sum(weight)),
      first = calc(min(day, na.rm = TRUE)),
      high = calc(count(sbp > 130)),
      high_present = calc(count(sbp > 130, na.rm = TRUE)),
      bmi = calc(sum(weight) / sum(height^2))
    )
  )
  expect_identical(nrow(s), 1L)
  expect_identical(s$n, 8L)
  expect_identical(s$w, 585)
  expect_identical(s$first, as.Date("2020-01-01"))
  # A count over a missing condition is missing, as sum() in R.
  expect_identical(s$high, NA_integer_)
  expect_identical(s$high_present, 3L)
  none <- x[integer(), ]
  z <- r8summarize(
    none,
    list(n = calc(n()), w = calc(sum(weight)), m = calc(mean(weight)))
  )
  expect_identical(c(z$n, z$w, z$m), c(0, 0, NA))
  expect_identical(
    nrow(r8summarize(none, list(n = calc(n())), by = "ward")),
    0L
  )
})

test_that("a summary reads cells within aggregates, and is named apart from its keys", {
  x <- vitals()
  refuses(r8summarize(x, list(a = calc(age + 1))), "within aggregates")
  refuses(
    r8summarize(x, list(ward = calc(n())), by = "ward"),
    "named as a key",
    class = "r8_argument_error"
  )
  refuses(
    calc(n(na.rm = TRUE)),
    "takes no argument `na.rm`",
    class = "r8_declaration_error"
  )
  refuses(
    r8summarize(x, list(n = calc(n())), by = "wards"),
    "the nearest column is `ward`"
  )
  refuses(
    r8summarize(x, list(n = 1)),
    "named list of calc()",
    class = "r8_argument_error"
  )
  refuses(aggregate(x), "r8summarize(")
})

test_that("r8set() stores the columns that calc() computes, over groups with `by`", {
  x <- vitals()
  y <- r8set(
    x,
    list(
      bmi = calc(weight / height^2),
      n_ward = calc(n()),
      excess = calc(sbp - mean(sbp, na.rm = TRUE)),
      flag = calc(if (is.na(sbp)) NA else sbp > 140)
    ),
    by = "ward"
  )
  expect_identical(y$bmi, x$weight / x$height^2)
  expect_identical(y$n_ward, c(3L, 2L, 3L, 2L, 2L, 1L, 3L, 2L))
  base_mean <- ave(x$sbp, addNA(x$ward), FUN = function(v) {
    mean(v, na.rm = TRUE)
  })
  expect_equal(y$excess, x$sbp - base_mean)
  expect_identical(y$flag, c(TRUE, FALSE, NA, FALSE, TRUE, FALSE, TRUE, NA))
  # A quotient has no unit; a difference of mmHg is in mmHg.
  u <- units_of(y)
  expect_true(is.na(u[["bmi"]]))
  expect_identical(u[["excess"]], "mmHg")
  # Without `by`, an aggregate is over all rows.
  z <- r8set(x, list(share = calc(weight / sum(weight))))
  expect_equal(sum(z$share), 1)
  refuses(
    r8set(x, list(d = calc(weight / (age - age)))),
    "`d` has no value in row 1: division by zero; nor in 7 other rows"
  )
  refuses(
    r8set(x, list(r = calc((-weight)^0.5))),
    "a negative number to a power that is not a whole number"
  )
  refuses(calc(sbp == NA), "compares with NA", class = "r8_declaration_error")
  refuses(
    r8set(x, list(a = x$age), by = "ward"),
    "`by` groups the rows",
    class = "r8_argument_error"
  )
})

test_that("r8map() maps calc() over columns, with col() as each column", {
  x <- vitals()
  m <- r8map(x, calc(median(col())), columns = c("age", "weight"), by = "ward")
  expect_identical(names(m), c("ward", "age", "weight"))
  expect_identical(m$age, c(71, 67.5, 54.5, 35))
  expect_identical(m$weight, c(62, 79, 68, 92))
  expect_identical(
    units_of(m)[c("age", "weight")],
    c(age = "year", weight = "kg")
  )
  # The same call as a summary of each column.
  s <- r8summarize(
    x,
    list(age = calc(median(age)), weight = calc(median(weight))),
    by = "ward"
  )
  expect_true(r8equal(m, s))
  # Without `columns`, every column that is not a key.
  all <- r8map(
    x[c("ward", "age", "weight", "sbp")],
    calc(max(col(), na.rm = TRUE)),
    by = "ward"
  )
  expect_identical(names(all), c("ward", "age", "weight", "sbp"))
  refuses(
    r8map(x, calc(mean(age)), columns = "age"),
    "reads the column it maps as `col()`"
  )
  refuses(
    r8map(x, calc(n()), columns = "ward", by = "ward"),
    "a key column and a column mapped",
    class = "r8_argument_error"
  )
})

test_that("r8map() calls a typed function on each column of each group, as a direct call", {
  op <- options(r8.namespace = "maps")
  on.exit(options(op))
  x <- vitals()
  spread <- new_function(
    "spread",
    function(v = arg_vec(type_double())) max(v) - min(v),
    return_type = type_double()
  )
  m <- r8map(x, spread, columns = c("weight", "height"), by = "sex")
  expect_identical(m$sex, c("F", "M"))
  expect_identical(m$weight, c(15, 13))
  expect_identical(m$height, c(0.25, 0.25))
  # A group with a missing cell is the error of a direct call.
  e <- expect_error(
    r8map(x, spread, columns = "sbp", by = "sex"),
    class = "r8_value_error"
  )
  expect_match(
    conditionMessage(e),
    "column `sbp` in the group sex = \"M\"",
    fixed = TRUE
  )
  expect_error(spread(x$sbp[x$sex == "M"]), class = "r8_value_error")
  refuses(r8map(x, median, by = "ward"), "new_function()")
})

test_that("calc() calls a typed function, once over all rows or for each group", {
  op <- options(r8.namespace = "calls")
  on.exit(options(op))
  x <- vitals()
  trimmed_mean <- new_function(
    "trimmed_mean",
    function(v = arg_vec(type_double()), trim = arg_double(default = 0)) {
      mean(v, trim = trim)
    },
    return_type = type_double()
  )
  q <- attr(trimmed_mean, "r8_function")
  e <- calc(trimmed_mean(weight, trim = 0.25))
  expect_identical(
    format(e),
    paste0("<r8 calc> ", q, "(v = weight, trim = 0.25)")
  )
  s <- r8summarize(
    x,
    list(
      tm = calc(trimmed_mean(weight, trim = 0.25)),
      m = calc(trimmed_mean(weight)),
      gap = calc(trimmed_mean(weight) - mean(weight))
    ),
    by = "sex"
  )
  f <- x$weight[x$sex == "F"]
  m <- x$weight[x$sex == "M"]
  expect_identical(s$tm, c(mean(f, trim = 0.25), mean(m, trim = 0.25)))
  expect_identical(s$m, c(mean(f), mean(m)))
  expect_equal(s$gap, c(0, 0))
  # The result has no unit; an argument may be an expression of cells.
  expect_identical(unname(units_of(s)[c("tm", "m")]), c(NA_character_, NA))
  expect_identical(
    r8summarize(x, list(b = calc(trimmed_mean(weight / height^2))))$b,
    mean(x$weight / x$height^2)
  )
  # Over all rows, and over the row's group with `by`.
  y <- r8set(x, list(d = calc(weight - trimmed_mean(weight))))
  expect_equal(y$d, x$weight - mean(x$weight))
  y <- r8set(x, list(d = calc(weight - trimmed_mean(weight))), by = "sex")
  expect_equal(y$d, x$weight - ave(x$weight, x$sex))
  # In a map, col() is each column; a typed function as `f` is the map of
  # calc(f(col())).
  expect_identical(
    r8map(
      x,
      calc(trimmed_mean(col(), trim = 0.25)),
      columns = c("weight", "height"),
      by = "sex"
    ),
    r8map(
      x,
      trimmed_mean,
      columns = c("weight", "height"),
      by = "sex",
      args = list(trim = 0.25)
    )
  )
  # A name of the dialect comes first: `mean` is the dialect's.
  mean <- new_function(
    "mean",
    function(v = arg_vec(type_double())) -1,
    return_type = type_double()
  )
  expect_identical(
    r8summarize(x, list(m = calc(mean(weight))))$m,
    base::mean(x$weight)
  )
})

test_that("a call in calc() is checked as a direct call", {
  op <- options(r8.namespace = "calls")
  on.exit(options(op))
  x <- vitals()
  spread <- new_function(
    "spread",
    function(v = arg_vec(type_double())) max(v) - min(v),
    return_type = type_double()
  )
  present <- new_function(
    "present",
    function(v = arg_vec(option(type_double()))) sum(!is.na(v)),
    return_type = type_integer()
  )
  # A group with a missing cell is the error of a direct call.
  e <- expect_error(
    r8summarize(x, list(s = calc(spread(sbp))), by = "sex"),
    class = "r8_value_error"
  )
  expect_match(
    conditionMessage(e),
    paste0(
      "r8summarize(): `s` calls `",
      attr(spread, "r8_function"),
      "()` in the group sex = \"M\""
    ),
    fixed = TRUE
  )
  expect_s3_class(e$parent, "r8_argument_error")
  # A function of option() cells receives them.
  expect_identical(
    r8summarize(x, list(n = calc(present(sbp))), by = "sex")$n,
    c(4L, 2L)
  )
  # An error of the body stops the call at its group.
  fails <- new_function(
    "fails",
    function(v = arg_vec(type_double())) {
      if (length(v) > 2L) stop("too many") else 1
    },
    return_type = type_double()
  )
  e <- expect_error(
    r8summarize(x, list(f = calc(fails(weight))), by = "ward"),
    class = "r8_value_error"
  )
  expect_match(
    conditionMessage(e),
    "in the group ward = \"ICU\": too many",
    fixed = TRUE
  )
  expect_identical(conditionMessage(e$parent), "too many")
})

test_that("calc() refuses a call that the declaration of its function does not admit", {
  op <- options(r8.namespace = "calls")
  on.exit(options(op))
  x <- vitals()
  scaled <- new_function(
    "scaled",
    function(v = arg_vec(type_double()), by = arg_double(default = 1)) {
      sum(v) * by
    },
    return_type = type_double()
  )
  q <- attr(scaled, "r8_function")
  refuses(
    r8summarize(x, list(s = calc(scaled(weight, by = height)))),
    paste0("`by` of `", q, "()` takes one constant, or `val()` of an R value")
  )
  refuses(
    r8summarize(x, list(s = calc(scaled(2)))),
    paste0("`v` of `", q, "()` takes the cells of a column")
  )
  k <- 2
  expect_identical(
    r8summarize(x, list(s = calc(scaled(weight, by = val(k)))))$s,
    2 * sum(x$weight)
  )
  refuses(
    calc(scaled(weight, w = 1)),
    "unused argument (w = 1)",
    class = "r8_declaration_error"
  )
  # A default in R is not evaluated by an expression.
  later <- new_function(
    "later",
    function(v = arg_vec(type_double()), k = arg_double(default = length(v))) k,
    return_type = type_double()
  )
  refuses(
    r8summarize(x, list(s = calc(later(weight)))),
    paste0(
      "the default of `k` of `",
      attr(later, "r8_function"),
      "()` is an R expression"
    )
  )
  expect_identical(r8summarize(x, list(s = calc(later(weight, k = 3))))$s, 3)
  # One value of a scalar type.
  centered <- new_function(
    "centered",
    function(v = arg_vec(type_double())) v - mean(v),
    return_type = vec(type_double())
  )
  refuses(
    r8summarize(x, list(s = calc(centered(weight)))),
    "a function that an expression calls returns one logical, integer, double, string, date, or date-time"
  )
  refuses(
    calc(stats::sd(weight)),
    "`stats::sd()` is an R function; calc() calls the functions of the dialect and typed functions of new_function()",
    class = "r8_declaration_error"
  )
  # A rule and a computed field call no function.
  refuses(
    rule(scaled(weight) > 0),
    paste0("`", q, "()` is a typed function, which a rule does not call"),
    class = "r8_declaration_error"
  )
  expect_error(
    new_struct(
      "Dosed",
      fields = list(
        mg = vec(type_double()),
        g = field(option(type_double()), compute = calc(scaled(mg)))
      )
    ),
    "a rule and a computed field call none",
    fixed = TRUE
  )
})

test_that("r8map() applies to a declared table as to a raven", {
  op <- options(r8.namespace = "maps")
  on.exit(options(op))
  Visits <- new_table(
    "Visits",
    columns = list(
      ward = type_character(),
      age = column(type_double(), unit = "year")
    ),
    open = TRUE
  )
  v <- Visits(data.frame(
    ward = c("a", "b", "a"),
    age = c(30, 40, 50),
    w = c(1, 2, 3)
  ))
  m <- r8map(v, calc(mean(col())), by = "ward")
  expect_identical(class(m)[[1L]], "R8::Raven")
  expect_identical(m$age, c(40, 40))
  expect_identical(m$w, c(2, 2))
  expect_identical(units_of(m)[["age"]], "year")
})

test_that("a computed field takes calc()", {
  op <- options(r8.namespace = "maps")
  on.exit(options(op))
  Dose <- new_struct(
    "Dose",
    fields = list(
      mg = type_double(),
      g = field(option(type_double()), compute = calc(mg / 1000))
    )
  )
  expect_identical(r8resolve(Dose(mg = 250))$g, 0.25)
  refuses(
    field(option(type_double()), compute = mg / 1000),
    "compute = calc(mg/1000)",
    class = "r8_declaration_error"
  )
})

test_that("summaries do not depend on the order of the rows", {
  set.seed(146)
  n <- 5000L
  x <- Raven(
    g = sample(c("a", "b", "c", NA), n, TRUE),
    k = sample(1:40, n, TRUE),
    v = c(rnorm(n - 3L) * 10^sample(-5:5, n - 3L, TRUE), 1e16, 1, -1e16)
  )
  sums <- list(
    n = calc(n()),
    s = calc(sum(v)),
    m = calc(mean(v)),
    d = calc(median(v))
  )
  a <- r8summarize(x, sums, by = c("g", "k"))
  b <- r8summarize(x[sample.int(n), ], sums, by = c("g", "k"))
  expect_true(r8equal(a, b))
  # The counts are those of base R, and the medians.
  expect_identical(sum(a$n), n)
  key <- paste(x$g, x$k)
  first <- paste(a$g, a$k)
  expect_identical(a$n, as.integer(table(key)[first]))
  # R's mean of two middle values differs from R8's in the last bits.
  expect_equal(
    a$d,
    as.vector(tapply(x$v, key, median)[first]),
    tolerance = 1e-15
  )
})

test_that("sums over many groups do not depend on the order of the rows", {
  # Above 2,048 groups, each group keeps a bounded sum, and the groups that the
  # bound does not decide are summed exactly.
  set.seed(1460)
  n <- 60000L
  x <- Raven(
    k = sample.int(5000L, n, TRUE),
    v = rnorm(n) * 10^sample(-8:8, n, TRUE)
  )
  sums <- list(s = calc(sum(v)), m = calc(mean(v)))
  a <- r8summarize(x, sums, by = "k")
  b <- r8summarize(x[rev(seq_len(n)), ], sums, by = "k")
  expect_true(r8equal(a, b))
  expect_equal(a$s, as.vector(tapply(x$v, x$k, sum)))
})

test_that("a summary of a group is the summary of the table filtered to it", {
  x <- vitals()
  sums <- list(
    n = calc(n()),
    w = calc(sum(weight)),
    sbp = calc(mean(sbp, na.rm = TRUE)),
    age = calc(median(age)),
    heavy = calc(count(weight > 75))
  )
  s <- r8summarize(x, sums, by = "sex")
  for (k in seq_len(nrow(s))) {
    one <- r8summarize(r8filter(x, rule(sex == val(s$sex[[k]]))), sums)
    expect_identical(as.list(s[k, names(sums)]), as.list(one))
  }
})

test_that("a summary is the same for any number of threads", {
  set.seed(1461)
  n <- 300000L
  x <- Raven(
    k = sample.int(3000L, n, TRUE),
    v = rnorm(n) * 10^sample(-6:6, n, TRUE),
    w = sample(c(1:5, NA), n, TRUE)
  )
  sums <- list(
    s = calc(sum(v)),
    m = calc(mean(v)),
    lo = calc(min(v)),
    hi = calc(max(w, na.rm = TRUE)),
    sw = calc(sum(w))
  )
  one <- r8summarize(x, sums, by = "k", n_threads = 1L)
  for (t in c(2L, 3L, 7L)) {
    expect_true(r8equal(one, r8summarize(x, sums, by = "k", n_threads = t)))
  }
  whole <- r8summarize(x, sums, n_threads = 1L)
  expect_true(r8equal(whole, r8summarize(x, sums, n_threads = 4L)))
  expect_equal(one$s, as.vector(tapply(x$v, x$k, sum)), tolerance = 1e-12)
  refuses(
    r8summarize(x, sums, n_threads = 0L),
    "`n_threads` is a whole number",
    class = "r8_argument_error"
  )
})
