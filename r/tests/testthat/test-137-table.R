# test-137-table.R
# ::R8::
# 2026- EDG rtemis.org

# Tables: the declaration of a table, its values, the operations on their columns
# and rows, documents, and saved values.
#
# spec: R8/raven

vitals <- function(ns) {
  declare_in(ns, {
    new_table(
      "Vitals",
      columns = list(
        age = column(type_double(min = 0), unit = "a", description = "Age"),
        sbp = type_double(min = 0, max = 400),
        grp = option(type_factor(c("a", "b")))
      ),
      open = TRUE,
      rules = rule(age < 130, id = "OLD")
    )
  })
}

cases <- function(n = 3L) {
  data.frame(
    sex = rep_len(c("f", "m"), n),
    sbp = 100 + seq_len(n),
    age = 30 + seq_len(n),
    grp = factor(rep_len(c("a", NA, "b"), n), levels = c("a", "b"))
  )
}

refuses <- function(expr, text) {
  expect_error(expr, text, fixed = TRUE, class = "r8_value_error")
}

test_that("a table is declared, and its values hold its columns", {
  V <- vitals("tb1")
  expect_identical(
    format(V)[1:3],
    c(
      "<r8 table tb1::Vitals v1>",
      "  age: double [0, Inf) (unit a)  # Age",
      "  sbp: double [0, 400]"
    )[1:3] |>
      (\(x) c(x[[1L]], format(V)[2:3]))()
  )
  expect_true("  and other columns" %in% format(V))
  v <- V(cases())
  # The declared columns first, in declaration order.
  expect_identical(names(v), c("age", "sbp", "grp", "sex"))
  expect_identical(class(v), c("tb1::Vitals", "r8_table", "r8_value"))
  expect_true(inherits(v, V))
  expect_identical(dim(v), c(3L, 4L))
  expect_identical(colnames(v), names(v))
  expect_identical(nrow(v), 3L)
  expect_identical(v$age, c(31, 32, 33))
  expect_identical(v[["sex"]], c("f", "m", "f"))
  expect_identical(
    vapply(v, function(x) class(x)[[1L]], ""),
    c(
      age = "numeric",
      sbp = "numeric",
      grp = "factor",
      sex = "character"
    )
  )
  # A data frame, a list, columns by name, and a value of another table.
  same <- list(
    V(as.list(cases())),
    V(
      sex = cases()$sex,
      sbp = cases()$sbp,
      age = cases()$age,
      grp = cases()$grp
    ),
    V(Raven(v)),
    V(Raven(cases()))
  )
  for (x in same) {
    expect_identical(as.data.frame(x), as.data.frame(v))
  }
  expect_error(
    declare_in("tb1", {
      new_table("W", columns = list(x = vec(type_double())))
    }),
    "table_column",
    class = "r8_declaration_error"
  )
  expect_error(
    declare_in("tb1", {
      new_table("W", columns = list(x = field(type_double(), default = 1)))
    }),
    "column_default",
    class = "r8_declaration_error"
  )
})

test_that("construction checks a table against its declaration", {
  V <- vitals("tb2")
  d <- cases()
  with <- function(...) {
    d[names(list(...))] <- list(...)
    d
  }
  refuses(
    V(with(age = c(1, -2, 3))),
    "`age[[2]]` must be at least 0; found -2."
  )
  refuses(V(with(age = c(1, NA, 3))), "age[[2]]")
  refuses(V(with(age = c(1, 2, 131))), "Rule OLD")
  refuses(V(with(sbp = c("a", "b", "c"))), "`sbp`")
  refuses(V(with(grp = factor(c("a", "c", "b")))), "`grp[[2]]`")
  refuses(V(d[c("age", "grp")]), "`sbp` is required.")
  refuses(V(age = 1:2, sbp = 1, grp = factor(c("a", "b"))), "must have 2 cells")
  refuses(V(list(1, 2)), "Every column of Vitals() has a name")
  refuses(V(age = 1, 2), "Every column of Vitals() has a name")
  refuses(V(1:3), "takes a data frame")
  refuses(V(matrix(1:4, 2)), "as.data.frame() gives one of a matrix")
  refuses(Raven(mtcars), "cbind(name = rownames(x), x, row.names = NULL)")
  Closed <- declare_in("tb2", {
    new_table(
      "Closed",
      columns = list(x = column(type_integer(), unique = TRUE))
    )
  })
  refuses(Closed(x = 1:2, y = 1:2), "unknown_column")
  refuses(Closed(x = c(1L, 1L)), "not_unique")
})

test_that("an extra column has any name and any of the column types", {
  r <- Raven(
    `Age (years)` = c(1, NA, Inf),
    `if` = c(TRUE, NA, FALSE),
    Sepal.Length = c(NaN, 1, 2),
    `größe` = 1:3,
    s = c("x", NA, "y"),
    f = factor(c("u", NA, "v"), levels = c("v", "u"), ordered = TRUE),
    d = as.Date(c("2026-01-01", NA, "2026-02-01")),
    t = as.POSIXct(c(0, NA, 1.5), tz = "UTC")
  )
  expect_identical(
    names(r),
    c("Age (years)", "if", "Sepal.Length", "größe", "s", "f", "d", "t")
  )
  expect_identical(r$`Age (years)`, c(1, NA, Inf))
  expect_identical(levels(r$f), c("v", "u"))
  expect_identical(r8from_json(Raven, r8to_json(r)), r)
  expect_identical(Raven(iris)$Species, iris$Species)
  refuses(Raven(`$x` = 1), "does not begin with `$`")
  refuses(Raven(a = 1, a = 2), "two columns are named `a`")
  refuses(Raven(a = as.difftime(1, units = "hours")), "as.numeric(x, units")
  refuses(Raven(a = structure(1, class = "integer64")), "as.character()")
  refuses(Raven(a = list(1)), "column `a` is a list")
  refuses(Raven(a = 1i), "column_type")
  refuses(Raven(a = as.POSIXlt("2026-01-01")), "as.POSIXct()")
  refuses(Raven(a = structure(1, label = "x")), "r_attributes")
  expect_identical(dim(Raven()), c(0L, 0L))
  expect_identical(dim(Raven(data.frame())), c(0L, 0L))
})

test_that("x[i, j] gives rows and columns, always as a table", {
  V <- vitals("tb3")
  v <- V(cases(6L))
  d <- as.data.frame(v)
  same <- function(x, y) {
    row.names(y) <- NULL
    expect_identical(as.data.frame(x), y)
  }
  same(v[c(5, 1), ], d[c(5, 1), ])
  same(v[-(1:4), ], d[5:6, ])
  same(v[v$age > 33, ], d[d$age > 33, ])
  same(v[c(2L, 2L), ], d[c(2, 2), ])
  same(v[integer(), ], d[integer(), ])
  same(v[, c("age", "sex")], d[c("age", "sex")])
  same(v[, c(1, 4)], d[c(1, 4)])
  same(v[, -1], d[-1])
  same(v[c("sex", "grp")], d[c("sex", "grp")])
  same(v[2:3, "sbp"], d[2:3, "sbp", drop = FALSE])
  same(v[,], d)
  same(v[], d)
  same(head(v, 2), d[1:2, ])
  same(head(v, -4), d[1:2, ])
  same(tail(v, 2), d[5:6, ])
  # The type stays where its columns do.
  expect_s3_class(v[1:2, ], "tb3::Vitals")
  expect_s3_class(v[, c("grp", "sbp", "age")], "tb3::Vitals")
  expect_s3_class(v[, c("age", "sex")], "R8::Raven")
  refuses(v[7, ], "asks for row 7; there are 6 rows")
  refuses(v[0, ], "Positions begin at 1")
  refuses(v[c(1, NA), ], "Element 2 of the index is NA")
  refuses(v[1.5, ], "not a whole number")
  refuses(v[c(-1, 1), ], "all positive or all negative")
  refuses(v[c(TRUE, FALSE), ], "one value per row; it has 2")
  refuses(v["a", ], "by position, or with a logical vector")
  refuses(v[, "agee"], "the nearest column is `age`")
  refuses(v[, 5], "asks for column 5; there are 4 columns")
  refuses(v[, c(1, 1)], "column `age` twice")
  refuses(v[1, 1, drop = TRUE], "x$name gives a column as a vector")
  refuses(
    v[[1]],
    "column 1 is `age`: v[[\"age\"]]; names(v) gives the column names"
  )
  refuses(v$agee, "`agee` is not a column")
})

test_that("a column is replaced, added, and removed, and checked", {
  V <- vitals("tb4")
  v <- V(cases())
  u <- v
  u$age <- c(1, 2, 3)
  u$new <- c(TRUE, NA, FALSE)
  expect_identical(names(u), c("age", "sbp", "grp", "sex", "new"))
  expect_identical(v$age, c(31, 32, 33))
  u[c("age", "new")] <- list(c(4, 5, 6), NULL)
  expect_identical(u$age, c(4, 5, 6))
  expect_identical(names(u), names(v))
  u$sex <- NULL
  expect_identical(names(u), c("age", "sbp", "grp"))
  # The type of a replaced extra column is the new one's.
  u$x <- 1:3
  u$x <- letters[1:3]
  expect_identical(u$x, letters[1:3])
  refuses(v$age <- c(1, 2, -3), "`age[[3]]` must be at least 0")
  refuses(v$age <- c(1, 2, 200), "Rule OLD")
  refuses(v$age <- 1, "must have 3 cells, one per row; found length 1")
  refuses(v$age <- NULL, "`age` is required.")
  refuses(v$nope <- NULL, "`nope` is not a column")
  refuses(v$`$x` <- 1:3, "does not begin with `$`")
  refuses(v[1, 1] <- 2, "replaced by column")
  refuses(v[[1]] <- 2, "replaced by name")
  expect_identical(v$age, c(31, 32, 33))
  Closed <- declare_in("tb4", {
    new_table("Closed", columns = list(x = type_integer(), y = type_integer()))
  })
  z <- Closed(x = 1:2, y = 3:4)
  refuses(z$z <- 1:2, "whose columns are `x`, `y`")
})

test_that("rbind() binds values of one table", {
  V <- vitals("tb5")
  v <- V(cases())
  b <- rbind(v, v[1, ], v[integer(), ])
  expect_s3_class(b, "tb5::Vitals")
  expect_identical(b$sex, c("f", "m", "f", "f"))
  expect_identical(b$grp, factor(c("a", NA, "b", "a"), levels = c("a", "b")))
  expect_identical(rbind(v), v)
  refuses(rbind(v, Raven(cases())), "Raven() gives values of one type")
  refuses(rbind(v, v[, 1:3]), "has the columns")
  refuses(rbind(v, cases()), "takes values of one table")
  a <- Raven(f = factor("a"))
  refuses(rbind(a, Raven(f = factor("b"))), "levels of factor `f`")
  refuses(rbind(Raven(x = 1), Raven(x = 1L)), "another type")
  refuses(
    rbind(
      Raven(x = 1),
      r8annotate(Raven(x = 1), unit = c(x = "kg"))
    ),
    "units or descriptions"
  )
  U <- declare_in("tb5", {
    new_table("U", columns = list(id = column(type_integer(), unique = TRUE)))
  })
  refuses(rbind(U(id = 1:2), U(id = 2:3)), "not_unique")
  expect_identical(rbind(U(id = 1:2), U(id = 3L))$id, 1:3)
  refuses(U(id = 1:2)[c(1, 1), ], "Raven(x)[i, ]")
  expect_identical(Raven(U(id = 1:2))[c(1, 1), ]$id, c(1L, 1L))
})

test_that("the columns of a table have units and descriptions", {
  V <- vitals("tb6")
  v <- V(cases())
  about <- r8columns(v)
  expect_identical(about$name, names(v))
  expect_identical(about$unit, c("a", NA, NA, NA))
  expect_identical(about$description, c("Age", NA, NA, NA))
  expect_identical(about$declared, c(TRUE, TRUE, TRUE, FALSE))
  expect_identical(
    r8columns(Raven(iris))$type,
    c(rep("double", 4L), "factor")
  )
  r <- Raven(v)
  expect_identical(r8columns(r)$unit, c("a", NA, NA, NA))
  expect_identical(r8columns(r[2:1, c("sbp", "age")])$unit, c(NA, "a"))
  r <- r8annotate(
    r,
    unit = c(sbp = "mm[Hg]"),
    description = c(sbp = "Pressure")
  )
  expect_match(format(r), "[mm[Hg]]", fixed = TRUE, all = FALSE)
  expect_identical(r8columns(r)$description[1:2], c("Age", "Pressure"))
  r <- r8annotate(r, unit = c(age = NA), description = c(age = NA, sbp = NA))
  expect_identical(r8columns(r)$unit, c(NA, "mm[Hg]", NA, NA))
  expect_identical(r8columns(r)$description, rep(NA_character_, 4L))
  expect_identical(r8annotate(r), r)
  # A replaced column and a removed one take their facts along; the others keep
  # theirs.
  r <- r8annotate(r, unit = c(age = "a"))
  r$sbp <- c(1, 2, 3)
  expect_identical(r8columns(r)$unit, c("a", NA, NA, NA))
  r["age"] <- list(c(0.1, 0.2, 0.3))
  expect_identical(r8columns(r)$unit, rep(NA_character_, 4L))
  # The unit of a declared column is its type's.
  w <- v
  w$age <- w$age + 1
  expect_identical(r8columns(w)$unit[[1L]], "a")
  bad <- function(expr, text) {
    expect_error(expr, text, fixed = TRUE, class = "r8_argument_error")
  }
  bad(r8annotate(v, unit = c(age = "mo")), "is declared by `tb6::Vitals`")
  bad(r8annotate(r, unit = c(nope = "a")), "not a column")
  bad(r8annotate(r, unit = c(age = " a")), "no surrounding spaces")
  bad(r8annotate(r, description = c(age = "")), "is not empty")
  bad(r8annotate(r, unit = "a"), "unit = c(age = \"a\")")
  bad(r8annotate(r, unit = list(age = "a")), "by the names of columns")
  bad(r8annotate(r, unit = c(age = "a", age = "b")), "by the names of columns")
  bad(r8columns(cases()), "a value of a table")
})

test_that("a table is a field, an implementor, and an argument", {
  out <- declare_in("tb7", {
    V <- new_table("V", columns = list(x = type_double()))
    Size <- new_trait("Size", methods = list(size = trait_method(params(x))))
    size <- Size@size
    impl(Size, V, methods = list(size = function(x) nrow(x)))
    impl(Size, Raven, methods = list(size = function(x) -nrow(x)))
    total <- generic("total", params(x = arg(V)))
    implement(total, V) <- function(x) sum(x$x)
    Fit <- new_struct(
      "Fit",
      fields = list(
        cases = V,
        more = option(Raven),
        many = vec(Raven),
        any = option(dyn(Size))
      )
    )
    list(V = V, size = size, total = total, Fit = Fit)
  })
  v <- out[["V"]](x = c(1, 2))
  expect_identical(out[["size"]](v), 2L)
  expect_identical(out[["size"]](Raven(iris)), -150L)
  expect_identical(out[["total"]](v), 3)
  expect_error(out[["total"]](Raven(x = 1)), class = "r8_argument_error")
  f <- out[["Fit"]](
    cases = v,
    more = Raven(iris),
    many = list(Raven(a = 1:2), Raven(b = "x")),
    any = v
  )
  expect_identical(f$cases$x, c(1, 2))
  expect_identical(out[["size"]](f$any), 2L)
  expect_identical(r8from_json(out[["Fit"]], r8to_json(f)), f)
  expect_true(r8equal(f, unserialize(serialize(f, NULL))))
  expect_match(r8hash(f), "^sha256:")
  expect_match(format(f), "cases: <tb7::V> 2 x 1", fixed = TRUE, all = FALSE)
  refuses(out[["Fit"]](cases = Raven(x = 1), many = list()), "`cases`")
  refuses(out[["Fit"]](cases = data.frame(x = 1), many = list()), "`cases`")
})

test_that("equality and the hash cover the order and the units, not descriptions", {
  r <- Raven(a = c(1, NA), b = c("x", "y"))
  differs <- list(
    Raven(b = c("x", "y"), a = c(1, NA)),
    Raven(a = c(1, NaN), b = c("x", "y")),
    Raven(a = c(1L, NA), b = c("x", "y")),
    r8annotate(r, unit = c(a = "kg"))
  )
  for (x in differs) {
    expect_false(r8equal(r, x))
    expect_false(identical(r8hash(r), r8hash(x)))
  }
  d <- r8annotate(r, description = c(a = "Weight"))
  expect_true(r8equal(r, d))
  expect_identical(r8hash(r), r8hash(d))
  expect_false(identical(r, d) && identical(r8columns(r), r8columns(d)))
  for (x in c(list(r, d), differs)) {
    back <- r8from_json(Raven, r8to_json(x))
    expect_identical(back, x)
    expect_identical(r8columns(back), r8columns(x))
    expect_identical(r8columns(unserialize(serialize(x, NULL))), r8columns(x))
  }
})

test_that("a table reads as a data frame in R's own functions", {
  r <- Raven(iris)
  fit <- stats::lm(Sepal.Length ~ Petal.Length, data = iris)
  expect_equal(
    stats::coef(stats::lm(Sepal.Length ~ Petal.Length, data = r)),
    stats::coef(fit)
  )
  expect_identical(
    dim(stats::model.matrix(Sepal.Length ~ Species, data = r)),
    c(150L, 3L)
  )
  expect_identical(
    stats::aggregate(Sepal.Length ~ Species, data = r, FUN = mean),
    stats::aggregate(Sepal.Length ~ Species, data = iris, FUN = mean)
  )
  expect_identical(with(r, mean(Sepal.Width)), mean(iris$Sepal.Width))
  expect_identical(as.data.frame(r), iris)
  skip_if_not_installed("data.table")
  expect_identical(
    as.data.frame(data.table::as.data.table(r)),
    iris
  )
})

test_that("R's functions of data frames read a table or name R8's", {
  x <- Raven(
    a = c(1, 1, 2, NA),
    b = c(2, 2, 7, 1),
    g = c("u", "u", "v", "v")
  )
  d <- as.data.frame(x)
  n <- r8select(x, c("a", "b"))
  m <- as.data.frame(n)
  for (f in list(as.matrix, t, is.na, summary, dim)) {
    expect_identical(f(n), f(m))
  }
  expect_identical(apply(n, 1, sum), apply(m, 1, sum))
  expect_identical(scale(n), scale(m))
  expect_identical(duplicated(x), duplicated(d))
  expect_identical(anyDuplicated(x), anyDuplicated(d))
  expect_identical(stats::model.frame(n), stats::model.frame(m))
  expect_identical(unique(x), x[c(1, 3, 4), ])
  expect_identical(stats::na.omit(x), x[1:3, ])
  expect_identical(nrow(stats::na.omit(Raven(a = c(NA, NA_real_)))), 0L)
  parts <- split(x, x$g)
  expect_identical(parts, list(u = x[1:2, ], v = x[3:4, ]))
  expect_identical(split(x, ~g), parts)
  for (f in list(
    function() cbind(x, x),
    function() cbind(1, x),
    function() transform(x, z = a),
    function() within(x, z <- a)
  )) {
    expect_error(f(), "r8set(x, ", fixed = TRUE, class = "r8_value_error")
  }
  expect_error(subset(x, a > 1), "r8filter(", fixed = TRUE)
  for (f in list(sort, order)) {
    expect_error(f(x), "r8sort(", fixed = TRUE, class = "r8_value_error")
  }
  for (z in list(c(0, -0, 1), c(-0, 0, -1), c(0, -0))) {
    v <- Raven(z = z)$z
    expect_identical(1 / min(v), 1 / min(z))
    expect_identical(1 / max(v), 1 / max(z))
  }
})


test_that("a table has at most as many rows as R counts", {
  refuses <- function(expr) {
    expect_error(expr, "at most 2,147,483,647 rows", class = "r8_value_error")
  }
  refuses(Raven(a = seq_len(2^31)))
  x <- Raven(a = seq_len(2^30))
  refuses(rbind(x, x))
})


test_that("data.table changes no table that it is given or that it is read from", {
  skip_if_not_installed("data.table")
  n <- 1000L
  first <- function(r) .subset2(r, "a")[[1L]]
  make <- function() data.frame(a = runif(n), b = runif(n))
  dt <- data.table::as.data.table(make())
  r <- Raven(dt)
  data.table::set(dt, 1L, "a", -1)
  data.table::setnames(dt, "a", "q")
  expect_true(first(r) >= 0)
  expect_identical(names(r), c("a", "b"))
  r <- Raven(make())
  expect_error(data.table::set(r, 1L, "a", -1))
  expect_error(data.table::setnames(r, "a", "q"))
  x <- as.data.frame(r)
  data.table::setDT(x)
  data.table::set(x, 1L, "a", -1)
  x <- data.table::as.data.table(r)
  data.table::set(x, 1L, "a", -1)
  x <- data.table::data.table(a = r$a)
  data.table::set(x, 1L, "a", -1)
  expect_true(first(r) >= 0)
  expect_identical(r8validate(r), r)
  # setDT() of the value itself takes its type, and the value says so.
  x <- r
  data.table::setDT(x)
  expect_error(r8validate(r), class = "r8_error")
})

test_that("no operation on a table copies a column that it does not change", {
  skip_on_cran()
  n <- 1e6
  d <- as.data.frame(lapply(1:8, function(i) runif(n)))
  names(d) <- letters[1:8]
  few <- 2^18
  memory <- function(expr) {
    expr <- substitute(expr)
    env <- parent.frame()
    before <- gc(reset = TRUE)["Vcells", "used"]
    x <- eval(expr, env)
    8 * (gc()["Vcells", "max used"] - before)
  }
  r <- Raven(d)
  expect_lt(memory(Raven(d)), few)
  expect_lt(memory(r[, -1]), few)
  expect_lt(memory(r[, c("b", "c")]), few)
  expect_lt(memory(as.data.frame(r)), few)
  expect_lt(memory(r$a), few)
  expect_lt(
    memory({
      u <- r
      u$a <- d$b
      u$new <- d$c
      u$b <- NULL
    }),
    few
  )
  expect_lt(memory(r8validate(r)), few)
  expect_lt(memory(Raven(r)), few)
  # The rows of a table are new columns.
  expect_lt(memory(r[1:10, ]), few)
})

test_that("a table prints as a grid that says what it leaves out", {
  withr_width <- options(width = 80L)
  on.exit(options(withr_width))
  # A session that does not write UTF-8 has an ASCII sign.
  lines <- function(x) sub("\u2a09", "x", unclass(format(x)), fixed = TRUE)
  out <- lines(Raven(iris))
  expect_identical(
    out[c(1:4, 9L, 10L)],
    c(
      "<raven table> 150 rows x 5 columns",
      "     Sepal.Length  Sepal.Width  Petal.Length  Petal.Width  Species",
      "           double       double        double       double  factor",
      "  1           5.1          3.5           1.4          0.2  setosa",
      "     ... 140 rows not shown",
      "146           6.7          3.0           5.2          2.3  virginica"
    )
  )
  r <- r8annotate(
    Raven(
      a = c(1.25, NA, 100),
      b = c("x", NA, "long text that is cut somewhere")
    ),
    unit = c(a = "kg")
  )
  expect_identical(
    lines(r),
    c(
      "<raven table> 3 rows x 2 columns",
      "        a  b",
      "   double  character",
      "     [kg]",
      "     1 NA  1 NA",
      "1    1.25  x",
      "2      NA  NA",
      "3  100.00  long text that is cut..."
    )
  )
  wide <- Raven(as.data.frame(matrix(0, 2L, 40L)))
  expect_match(lines(wide), "^[0-9]+ columns not shown: V", all = FALSE)
  expect_true(all(nchar(lines(wide)) <= 80L))
  expect_match(
    lines(Raven(x = numeric(3e5))),
    "missing cells are not counted in 300,000 cells",
    all = FALSE
  )
  expect_identical(lines(Raven()), "<raven table> 0 rows x 0 columns")
  V <- vitals("grid")
  expect_match(
    lines(V(cases()))[[1L]],
    "<grid::Vitals table> 3 rows x 4 columns"
  )
  hidden <- declare_in("grid", {
    new_table("H", columns = list(x = field(type_integer(), sensitive = TRUE)))
  })
  expect_false(any(grepl("42", lines(hidden(x = 42L)))))
  # ggplot2 takes a dim() of plain integers.
  expect_identical(dim(wide), c(2L, 40L))
})

test_that("many sets of columns leave nothing behind", {
  skip_on_cran()
  w <- as.data.frame(matrix(0, 2L, 2000L))
  r <- Raven(w)
  set.seed(1)
  for (i in 1:20) {
    r[, sort(sample(2000L, 1000L))]
  }
  before <- sum(gc()[, 2L])
  for (i in 1:100) {
    r[, sort(sample(2000L, 1000L))]
  }
  expect_lt(sum(gc()[, 2L]) - before, 1)
})

test_that("a table is valid after any operations, as a full check finds", {
  V <- vitals("tb8")
  set.seed(7)
  v <- V(cases(20L))
  for (k in 1:150) {
    n <- nrow(v)
    v <- switch(
      sample(6L, 1L),
      v[sample(n, sample(3:20, 1L), replace = TRUE), ],
      {
        v$age <- runif(n, 0, 129)
        v
      },
      {
        v[[paste0("x", sample(3L, 1L))]] <- sample(
          list(
            runif(n),
            sample.int(9L, n, TRUE),
            sample(c("u", NA), n, TRUE),
            factor(sample(c("p", "q"), n, TRUE)),
            rep(NaN, n)
          ),
          1L
        )[[1L]]
        v
      },
      v[, unique(c("sbp", "age", "grp", sample(names(v), 2L)))],
      if (n < 60L) rbind(v, v[sample(n, 2L), ]) else v[1:5, ],
      {
        extra <- setdiff(names(v), c("age", "sbp", "grp"))
        if (length(extra) > 0L) {
          v[extra[[1L]]] <- list(NULL)
        }
        v
      }
    )
    expect_s3_class(v, "tb8::Vitals")
    r8validate(v)
    plain <- as.data.frame(v)
    expect_identical(as.data.frame(V(plain)), plain)
    expect_true(r8equal(v, V(plain)))
    expect_identical(r8hash(v), r8hash(V(plain)))
  }
})

test_that("a table of another declaration is refused and updated", {
  V <- vitals("tb9")
  v <- V(cases())
  V2 <- declare_in("tb9", {
    new_table(
      "Vitals",
      columns = list(
        sbp = type_double(min = 0, max = 400),
        age = column(type_double(min = 0), unit = "a"),
        grp = option(type_factor(c("a", "b")))
      ),
      open = TRUE,
      rules = rule(age < 130, id = "OLD")
    )
  })
  expect_identical(v$age, c(31, 32, 33))
  refuses(v[1, ], "another declaration")
  refuses(v$age <- c(1, 2, 3), "another declaration")
  u <- r8update(v)
  expect_identical(names(u), c("sbp", "age", "grp", "sex"))
  expect_identical(u[1, ]$sex, "f")
  declare_in("tb9", {
    new_table(
      "Vitals",
      columns = list(age = column(type_double(), unit = "mo"))
    )
  })
  expect_error(r8update(u), "was in `a`, and is in `mo`", class = "r8_error")
})

test_that("a saved table reads in a session that did not declare it", {
  V <- vitals("tb10")
  r <- r8annotate(Raven(iris), unit = c(Sepal.Length = "cm"))
  v <- V(cases())
  path <- tempfile(fileext = ".rds")
  saveRDS(list(r, v), path)
  back <- readRDS(path)
  expect_identical(back, list(r, v))
  expect_identical(r8columns(back[[1L]]), r8columns(r))
  # Damaged facts are refused.
  bytes <- serialize(r, NULL, xdr = FALSE)
  at <- which(vapply(
    seq_len(length(bytes) - 3L),
    function(i) identical(bytes[i + 0:3], charToRaw("unit")),
    NA
  ))
  bytes[at[[1L]] + 0:3] <- charToRaw("unix")
  expect_error(unserialize(bytes), "damaged")
})

test_that("an index is R's, and a rule chooses rows", {
  r <- Raven(iris)
  d <- iris
  same <- function(x, y) {
    row.names(y) <- NULL
    expect_identical(as.data.frame(x), y)
  }
  wide <- rule(Petal.Width > mean(Petal.Width))
  same(
    r[wide, c("Species", "Petal.Width")],
    d[d$Petal.Width > mean(d$Petal.Width), c("Species", "Petal.Width")]
  )
  same(r[rule(Petal.Width > 2), ], d[d$Petal.Width > 2, ])
  same(
    r[rule(Petal.Width > 2), "Species"],
    d[d$Petal.Width > 2, "Species", drop = FALSE]
  )
  same(r[!wide, 1:2], d[d$Petal.Width <= mean(d$Petal.Width), 1:2])
  # Base's forms, positions, and R's variables.
  idx <- c(5L, 2L)
  cols <- c("Species", "Sepal.Width")
  same(r[idx, cols], d[idx, cols])
  same(r[order(-r$Sepal.Length), ], d[order(-d$Sepal.Length), ])
  same(r[which(r$Petal.Width > 2), ], d[which(d$Petal.Width > 2), ])
  same(r[names(r) != "Species"], d[names(d) != "Species"])
  same(r[sapply(r, is.numeric)], d[sapply(d, is.numeric)])
  same(
    r[r$Petal.Width > 2, "Species"],
    d[d$Petal.Width > 2, "Species", drop = FALSE]
  )
  f <- function(tab, rows, columns) tab[rows, columns]
  same(f(r, 1:3, 2), d[1:3, 2, drop = FALSE])
  same(
    f(r, wide, "Species"),
    d[d$Petal.Width > mean(d$Petal.Width), "Species", drop = FALSE]
  )
})

test_that("what a call means does not depend on the table or on the session", {
  r <- Raven(age = c(1, 50), x = 1:2)
  # R's variable, though the table has a column of its name.
  age <- c(TRUE, FALSE)
  expect_identical(r[age, ]$x, 1L)
  f <- function(tab, x) tab[, x]
  expect_identical(names(f(r, "age")), "age")
  # A column, though R has a variable of its name.
  expect_identical(r[rule(age > 40), ]$x, 2L)
  g <- function(tab, age) tab[rule(age > val(age)), ]
  expect_identical(g(r, 40)$x, 2L)
  # The same line after the table has a column of a variable's name.
  keep <- 2L
  before <- as.data.frame(r[keep, "x"])
  r$keep <- c(1L, 1L)
  expect_identical(as.data.frame(r[keep, "x"]), before)
})

test_that("an index that asks for no row says so", {
  x <- Raven(a = 1:3)
  refuses <- function(expr, text) {
    expect_error(expr, text, fixed = TRUE, class = "r8_value_error")
  }
  refuses(x[list(1), ], "A list in the place of the rows holds one rule().")
  refuses(x[2^60, ], "asks for row 1152921504606847000; there are 3 rows")
  refuses(x[1e300, ], "e+300; there are 3 rows")
  refuses(x[Inf, ], "Element 1 of the index, Inf, is not a whole number")
  refuses(x[1.5, ], "Element 1 of the index, 1.5, is not a whole number")
  refuses(x[, NA], "Element 1 of the index is NA")
  d <- data.frame(a = 1, b = 2)
  names(d)[[2L]] <- NA
  refuses(Raven(d), "the name of column 2 is missing")
})


test_that("a slip in an index is an error that gives the form", {
  r <- Raven(iris)
  refuses(
    r[Petal.Width > 2, ],
    "R has no variable `Petal.Width`, which is a column of the table; a condition on columns is a rule: rule(Petal.Width > 2)."
  )
  refuses(r[Species], "columns are asked for by name: x[c(\"Species\")].")
  refuses(
    r[, c(Species, Petal.Width)],
    "x[, c(\"Species\", \"Petal.Width\")]."
  )
  wide <- rule(Petal.Width > 2)
  refuses(r[wide], "asked for before a comma: x[wide, ]")
  m <- Raven(a = c(1, NA, 3), flag = c(TRUE, NA, FALSE))
  refuses(
    m[m$a > 1, ],
    "missing (NA) in 1 row; which(m$a > 1) leaves it out."
  )
  expect_identical(m[which(m$a > 1), ]$a, 3)
  # Another error of the index is R's own.
  expect_error(r[stop("mine"), ], "mine")
  expect_error(r[nothing_here > 1, ], "nothing_here")
})
