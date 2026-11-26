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

test_that("r8_validate() checks an object against a type, with every problem", {
  Features <- type_table(
    columns = list(
      age = column(type_double(min = 0)),
      sex = column(type_factor(c("f", "m")))
    ),
    open = TRUE
  )
  data <- data.frame(age = c(40, 52), sex = factor(c("f", "m")), more = 1:2)
  expect_identical(
    withVisible(r8_validate(data, Features)),
    list(value = data, visible = FALSE)
  )
  bad <- data
  bad$age <- c(-1, NA)
  bad$sex <- factor(c("f", "x"))
  expect_error(
    r8_validate(bad, Features),
    "Invalid `bad`",
    class = "r8_value_error"
  )
  expect_error(
    r8_validate(bad, Features),
    "`bad\\$age\\[\\[1\\]\\]` must be at least 0"
  )
  expect_error(r8_validate(1, vec(type_character())), class = "r8_value_error")
  expect_identical(r8_validate(1:3, vec(type_integer(min = 1L))), 1:3)
  P <- declare_in("sh5", new_struct("P", fields = list(x = type_double())))
  expect_identical(r8_validate(P(x = 1), P), P(x = 1))
  expect_error(r8_validate(1, P), class = "r8_value_error")
  expect_error(r8_validate(1, "double"), class = "r8_argument_error")
  skip_if_not_installed("data.table")
  dt <- data.table::as.data.table(data)
  expect_identical(r8_validate(dt, Features), dt)
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

# The R memory in bytes that evaluating `expr` takes at its highest.
taken <- function(expr) {
  expr <- substitute(expr)
  env <- parent.frame()
  before <- gc(reset = TRUE)["Vcells", "used"]
  x <- eval(expr, env)
  8 * (gc()["Vcells", "max used"] - before)
}

test_that("no operation on a value copies a vector, a matrix, or a data frame", {
  skip_on_cran()
  n <- 1e6
  out <- declare_in("nc", {
    size_of <- generic("size_of", params(x))
    V <- new_struct(
      "V",
      fields = list(
        id = type_integer(),
        d = option(vec(type_double(min = 0))),
        i = option(vec(type_integer())),
        l = option(vec(type_logical())),
        s = option(vec(type_character())),
        f = option(type_factor(c("a", "b"))),
        t = option(vec(type_posixct())),
        date = option(vec(type_date())),
        m = option(type_matrix(type_double())),
        r = option(type_raw()),
        tab = option(type_table(columns = list(a = column(type_double()))))
      )
    )
    H <- new_struct(
      "H",
      fields = list(v = V, data = option(host_class("data.frame")))
    )
    P <- new_struct("P", fields = list(x = type_integer()))
    Q <- new_struct("Q", extends = P, fields = list(d = vec(type_double())))
    implement(size_of, V) <- function(x) length(x$d)
    list(V = V, H = H, P = P, Q = Q, size_of = size_of)
  })
  V <- out[["V"]]
  given <- list(
    d = runif(n),
    i = sample.int(9L, n, TRUE),
    l = runif(n) > 0.5,
    s = rep_len(c("x", "y"), n),
    f = factor(rep_len(c("a", "b"), n), levels = c("a", "b")),
    t = .POSIXct(floor(runif(n) * 1e9), tz = "America/New_York"),
    date = .Date(floor(runif(n) * 1e4)),
    m = matrix(runif(n), ncol = 4L),
    r = as.raw(rep_len(0:9, 8 * n)),
    tab = data.frame(a = runif(n))
  )
  few <- 2^18
  for (name in names(given)) {
    expect_lt(
      taken(do.call(V, c(list(id = 1L), given[name]))),
      few,
      label = paste("construction with", name)
    )
  }
  v <- do.call(V, c(list(id = 1L), given))
  for (name in names(given)) {
    expect_lt(taken(v[[name]]), few, label = paste("a read of", name))
  }
  d <- given[["d"]]
  same <- V(id = 1L, d = d + 0)
  q <- out[["Q"]](x = 1L, d = d)
  expect_lt(taken(as.list(v)), few)
  expect_lt(taken(v[c("d", "m")]), few)
  expect_lt(taken(v$tab$a), few)
  expect_lt(
    taken({
      u <- v
      u$id <- 2L
      u["i"] <- list(NULL)
    }),
    few
  )
  expect_lt(taken(V(id = 1L, d = v$d, tab = v$tab, t = v$t)), few)
  expect_lt(taken(out[["H"]](v = v, data = given[["tab"]])), few)
  expect_lt(taken(r8_validate(v)), few)
  expect_lt(taken(r8_validate(d, vec(type_double(min = 0)))), few)
  expect_lt(taken(r8_equal(V(id = 1L, d = d), same)), few)
  expect_lt(taken(r8_hash(V(id = 1L, m = given[["m"]]))), few)
  expect_lt(taken(out[["size_of"]](v)), few)
  expect_lt(taken(r8_upcast(q, out[["P"]])), few)
  expect_lt(taken(sum(v$d)), few)
  expect_lt(taken(v$d[1:10]), few)
  expect_lt(taken(colMeans(v$m)), few)
  expect_lt(
    taken({
      to <- file(nullfile(), "wb")
      serialize(v, to)
      close(to)
    }),
    few
  )
})

test_that("a vector in another time zone is shared, and read in UTC", {
  V <- declare_in("tz", {
    new_struct(
      "V",
      fields = list(
        t = vec(type_posixct()),
        tab = option(type_table(columns = list(t = column(type_posixct()))))
      )
    )
  })
  t <- .POSIXct(floor(runif(1000) * 1e9), tz = "America/New_York")
  first <- as.numeric(t[1L])
  v <- V(t = t, tab = data.frame(t = t))
  expect_identical(attr(v$t, "tzone"), "UTC")
  expect_identical(attr(t, "tzone"), "America/New_York")
  expect_identical(as.numeric(v$t), as.numeric(t))
  t[1L] <- t[1L] + 100
  x <- v$t
  x[1L] <- .POSIXct(0)
  expect_identical(as.numeric(v$t[1L]), first)
  expect_identical(as.numeric(v$tab$t[1L]), first)
  expect_identical(unserialize(serialize(v, NULL)), v)
  skip_if_not_installed("data.table")
  dt <- data.table::data.table(t = t)
  w <- V(t = t, tab = dt)
  first <- as.numeric(w$tab$t[1L])
  data.table::set(dt, 1L, "t", t[2L])
  expect_identical(as.numeric(w$tab$t[1L]), first)
})

test_that("elements and extremes of a read are those of the vector", {
  t <- sharing_types("sh11")
  V <- t[["V"]]
  set.seed(3)
  n <- 500L
  given <- list(
    d = c(runif(n - 1L), 0),
    i = sample.int(1000L, n, TRUE),
    l = sample(c(TRUE, FALSE, NA), n, TRUE)
  )
  v <- V(d = given$d, i = given$i, l = given$l)
  index <- list(
    1:10,
    c(n, 1L, n),
    -(1:10),
    c(1, NA, 3),
    c(5.9, 2),
    n + 1L,
    0L,
    c(0, 2),
    integer(),
    given$d > 0.5,
    c(TRUE, NA),
    NA,
    n:1,
    1e6,
    NULL,
    c(1, 2^31 + 5)
  )
  for (name in names(given)) {
    x <- v[[name]]
    expect_true(is_field_view(x))
    for (k in index) {
      expect_identical(x[k], given[[name]][k], info = paste(name, deparse1(k)))
    }
    for (f in list(min, max, range, sum, mean)) {
      expect_identical(f(x), f(given[[name]]), info = name)
    }
    expect_identical(min(x, na.rm = TRUE), min(given[[name]], na.rm = TRUE))
  }
  full <- full(t)
  expect_identical(full$f[c(3L, NA)], .subset2(full, "f")[c(3L, NA)])
  expect_identical(full$t[5:1], .subset2(full, "t")[5:1])
  expect_identical(full$m[2L, ], .subset2(full, "m")[2L, ])
  for (x in list(
    c(NaN, runif(40)),
    c(runif(40), NA),
    c(-Inf, runif(40), Inf),
    numeric()
  )) {
    r <- Raven(x = x, y = as.integer(is.na(x)))
    expect_identical(suppressWarnings(min(r$x)), suppressWarnings(min(x)))
    expect_identical(suppressWarnings(max(r$x)), suppressWarnings(max(x)))
    expect_identical(
      suppressWarnings(max(r$x, na.rm = TRUE)),
      suppressWarnings(max(x, na.rm = TRUE))
    )
  }
  # Rows by a logical vector, with every remainder of a block.
  for (k in c(0L, 1L, 7L, 33L)) {
    l <- rep(c(TRUE, FALSE, FALSE), length.out = k)
    r <- Raven(x = seq_len(k))
    expect_identical(r[l, ]$x, seq_len(k)[l])
  }
})
