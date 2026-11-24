# test-136-sharing.R
# ::R8::
# 2026- EDG rtemis.org

# What a value shares with the code around it: a vector or a data frame given, and
# what a read gives, are not copied, and R's own functions and data.table's change
# no value.
#
# spec: R8/class-system#sharing

sharing_types <- function(ns) {
  declare_in(ns, {
    list(
      V = new_struct(
        "V",
        fields = list(
          d = vec(type_double(min = 0)),
          i = option(vec(type_integer())),
          l = option(vec(option(type_logical()))),
          s = option(vec(type_character())),
          r = option(type_raw()),
          f = option(type_factor(c("a", "b"))),
          t = option(vec(type_posixct())),
          m = option(type_matrix(type_double()))
        )
      ),
      W = new_struct(
        "W",
        fields = list(
          tab = type_table(
            columns = list(
              a = column(type_double(min = 0)),
              b = column(type_character())
            )
          )
        )
      )
    )
  })
}

# The memory of a vector: its first address, as R writes it.
address <- function(x) {
  sub(
    "^@?([0-9a-fx]+) .*$",
    "\\1",
    utils::capture.output(.Internal(inspect(x)))[[1L]]
  )
}

is_field_view <- function(x) {
  grepl(
    "r8 field",
    utils::capture.output(.Internal(inspect(x)))[[1L]],
    fixed = TRUE
  )
}

full <- function(t, n = 100L) {
  t[["V"]](
    d = as.numeric(seq_len(n)),
    i = rep(1:2, n),
    l = rep(c(TRUE, NA), n),
    s = rep(c("x", "y"), n),
    r = as.raw(rep(1, 10L * n)),
    f = factor(rep(c("a", "b"), n), levels = c("a", "b")),
    t = as.POSIXct(seq_len(n), tz = "UTC"),
    m = matrix(as.numeric(seq_len(4L * n)), 4L)
  )
}

test_that("a vector and the columns of a data frame are shared, not copied", {
  t <- sharing_types("sh1")
  d <- as.numeric(1:1000)
  v <- t[["V"]](d = d)
  expect_identical(address(.subset2(v, "d")), address(d))
  df <- data.frame(a = d, b = rep("z", 1000L))
  w <- t[["W"]](tab = df)
  expect_identical(address(.subset2(w, "tab")[["a"]]), address(df[["a"]]))
  # A read gives a view of the vector, and a value made from it holds the vector.
  expect_true(is_field_view(v$d))
  expect_true(is_field_view(v[["d"]]))
  expect_true(is_field_view(v["d"][[1L]]))
  expect_true(is_field_view(as.list(v)[["d"]]))
  expect_true(is_field_view(w$tab$a))
  expect_identical(address(.subset2(t[["V"]](d = v$d), "d")), address(d))
  u <- v
  u$i <- 1:100
  expect_identical(address(.subset2(u, "d")), address(d))
})

test_that("a read is the field, for every kind of vector", {
  t <- sharing_types("sh2")
  v <- full(t)
  for (n in names(v)) {
    raw <- .subset2(v, n)
    x <- v[[n]]
    expect_identical(x, raw, info = n)
    expect_identical(attributes(x), attributes(raw), info = n)
    expect_identical(rev(x), rev(raw), info = n)
    expect_identical(x[c(1L, 3L, NA)], raw[c(1L, 3L, NA)], info = n)
    expect_identical(unserialize(serialize(x, NULL)), raw, info = n)
    expect_identical(c(x, x), c(raw, raw), info = n)
    expect_identical(is.na(x), is.na(raw), info = n)
    expect_identical(summary(x), summary(raw), info = n)
    expect_identical(duplicated(x), duplicated(raw), info = n)
  }
  expect_identical(sum(v$d), 5050)
  expect_identical(v$m %*% t(v$m), .subset2(v, "m") %*% t(.subset2(v, "m")))
  expect_identical(levels(v$f), c("a", "b"))
  expect_identical(order(v$d, decreasing = TRUE)[[1L]], 100L)
  expect_identical(
    t[["W"]](tab = data.frame(a = 1:100 + 0, b = "z"))$tab,
    data.frame(a = 1:100 + 0, b = "z")
  )
  # A compact sequence stays one.
  expect_identical(t[["V"]](d = 1, i = 1:1e6)$i, 1:1e6)
})

test_that("R's own assignments change a copy", {
  t <- sharing_types("sh3")
  v <- full(t)
  before <- lapply(names(v), function(n) {
    unserialize(serialize(.subset2(v, n), NULL))
  })
  d <- v$d
  d[[1L]] <- -1
  d[2:3] <- -1
  names(d) <- seq_along(d)
  s <- v$s
  s[[1L]] <- "changed"
  m <- v$m
  m[1L, 1L] <- -1
  dim(m) <- NULL
  f <- v$f
  levels(f) <- c("q", "r")
  tt <- v$t
  attr(tt, "tzone") <- "America/New_York"
  l <- v$l
  l[is.na(l)] <- FALSE
  expect_identical(unname(lapply(names(v), function(n) .subset2(v, n))), before)
  w <- t[["W"]](tab = data.frame(a = 1:100 + 0, b = "z"))
  x <- w$tab
  x$a[[1L]] <- -1
  x[["b"]] <- NULL
  names(x) <- "q"
  expect_identical(w$tab, data.frame(a = 1:100 + 0, b = "z"))
})

test_that("a data.table given is copied, and one made from a read changes no value", {
  skip_if_not_installed("data.table")
  set <- data.table::set
  t <- sharing_types("sh4")
  plain <- function() data.frame(a = 1:100 + 0, b = rep("z", 100L))
  dt <- data.table::as.data.table(plain())
  w <- t[["W"]](tab = dt)
  set(dt, 1L, "a", -1)
  set(dt, 2L, "b", "changed")
  data.table::setorderv(dt, "a", -1L)
  data.table::setnames(dt, "a", "q")
  set(dt, j = "b", value = NULL)
  expect_identical(w$tab, plain())
  # What a read gives, taken by setDT(), setattr(), and as.data.table().
  x <- w$tab
  data.table::setDT(x)
  set(x, 1L, "a", -1)
  set(x, 1L, "b", "changed")
  data.table::setorderv(x, "a", -1L)
  data.table::setnames(x, "a", "q")
  set(x, j = "b", value = NULL)
  data.table::setattr(x, "names", "p")
  expect_identical(w$tab, plain())
  x <- w$tab
  data.table::setnames(x, "a", "q")
  expect_identical(w$tab, plain())
  expect_identical(r8_validate(w), w)
  y <- data.table::as.data.table(w$tab)
  set(y, 1L, "a", -1)
  v <- full(t)
  z <- v$d
  data.table::setattr(z, "names", as.character(seq_along(z)))
  l <- list(v$d)
  data.table::setDT(l)
  set(l, 1L, 1L, -1)
  expect_identical(w$tab, plain())
  expect_identical(v$d, as.numeric(1:100))
  expect_null(names(v$d))
  # The list of a data frame given is the value's own.
  df <- plain()
  w <- t[["W"]](tab = df)
  data.table::setDT(df)
  expect_identical(class(w$tab), "data.frame")
})

test_that("r8_check() checks an object against a type, with every problem", {
  Features <- type_table(
    columns = list(
      age = column(type_double(min = 0)),
      sex = column(type_factor(c("f", "m")))
    ),
    open = TRUE
  )
  data <- data.frame(age = c(40, 52), sex = factor(c("f", "m")), more = 1:2)
  expect_identical(
    withVisible(r8_check(data, Features)),
    list(value = data, visible = FALSE)
  )
  bad <- data
  bad$age <- c(-1, NA)
  bad$sex <- factor(c("f", "x"))
  expect_error(
    r8_check(bad, Features),
    "Invalid `bad`",
    class = "r8_value_error"
  )
  expect_error(
    r8_check(bad, Features),
    "`bad\\$age\\[\\[1\\]\\]` must be at least 0"
  )
  expect_error(r8_check(1, vec(type_character())), class = "r8_value_error")
  expect_identical(r8_check(1:3, vec(type_integer(min = 1L))), 1:3)
  P <- declare_in("sh5", new_struct("P", fields = list(x = type_double())))
  expect_identical(r8_check(P(x = 1), P), P(x = 1))
  expect_error(r8_check(1, P), class = "r8_value_error")
  expect_error(r8_check(1, "double"), class = "r8_type_error")
  skip_if_not_installed("data.table")
  dt <- data.table::as.data.table(data)
  expect_identical(r8_check(dt, Features), dt)
})

test_that("a data.table of host_class() is held, and read, by reference", {
  skip_if_not_installed("data.table")
  out <- declare_in("sh9", {
    list(
      D = new_struct(
        "D",
        fields = list(data = option(host_class("data.table")))
      )
    )
  })
  dt <- data.table::data.table(a = runif(1000), b = "x")
  d <- out[["D"]](data = dt)
  expect_identical(address(d$data), address(dt))
  expect_identical(address(d$data$a), address(dt$a))
  data.table::set(d$data, 1L, "a", -1)
  expect_identical(d$data$a[1L], -1)
  expect_identical(dt$a[1L], -1)
  # A table field stores a data frame, whatever class it was given.
  t <- sharing_types("sh10")
  w <- t[["W"]](tab = data.table::data.table(a = runif(10), b = "x"))
  expect_s3_class(w$tab, "data.frame", exact = TRUE)
})
