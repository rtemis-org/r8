# test-values.R
# ::R8::
# 2026- EDG rtemis.org

svm_types <- function() {
  declare_in("rtemis", {
    Kernel <- new_enum(
      "Kernel",
      variants = list(
        Linear = variant(),
        Rbf = variant(
          fields = list(
            gamma = field(
              type_double(exclusive_min = 0),
              description = "Kernel width."
            )
          )
        )
      ),
      description = "Kernel function."
    )
    Svm <- new_struct(
      "Svm",
      fields = list(
        cost = field(
          type_double(exclusive_min = 0),
          default = 1,
          description = "Cost of a margin violation."
        ),
        kernel = field(
          Kernel,
          default = Kernel@Linear,
          description = "Kernel function."
        ),
        weights = field(
          option(vec(type_double(min = 0))),
          description = "Case weights.",
          external = TRUE
        ),
        mtry = field(
          option(type_integer(min = 1L), resolve = "runtime"),
          description = "Variables per split."
        )
      ),
      rules = list(
        rule(
          kernel$gamma <= 10,
          message = "gamma is at most 10.",
          id = "RBF_GAMMA"
        ),
        rule(
          cost <= 100,
          when = is_variant(kernel, "Rbf"),
          message = "cost is at most 100 with the Rbf kernel.",
          id = "RBF_COST"
        )
      ),
      description = "Support vector machine settings."
    )
    list(Kernel = Kernel, Svm = Svm)
  })
}

test_that("a struct value is constructed, validated, and read by field", {
  t <- svm_types()
  Svm <- t[["Svm"]]
  Kernel <- t[["Kernel"]]
  x <- Svm(cost = 2L, kernel = Kernel@Rbf(gamma = 0.5), mtry = 3)
  expect_s3_class(x, c("rtemis::Svm", "r8_value"))
  expect_identical(x$cost, 2)
  expect_identical(x[["mtry"]], 3L)
  expect_null(x$weights)
  expect_identical(attr(x$kernel, "r8_variant"), "Rbf")
  expect_identical(x$kernel$gamma, 0.5)
  expect_identical(Svm()$cost, 1)
  expect_error(x$colour, "not a field", class = "r8_value_error")
  expect_error(x@cost, "\\$", class = "r8_value_error")
  expect_error(x[[1]], class = "r8_value_error")
})

test_that("construction reports every problem with its path and rule", {
  t <- svm_types()
  Svm <- t[["Svm"]]
  Kernel <- t[["Kernel"]]
  e <- expect_error(Svm(cost = -1, mtry = 0L), class = "r8_value_error")
  expect_match(
    conditionMessage(e),
    "`cost` must be above 0; found -1. [below_min]",
    fixed = TRUE
  )
  expect_identical(e$problems$path, c("/cost", "/mtry"))
  expect_error(
    Svm(kernel = Kernel@Rbf(gamma = 20)),
    "RBF_GAMMA",
    class = "r8_value_error"
  )
  expect_error(
    Kernel@Rbf(gamma = -1),
    "`gamma` must be above 0; found -1.",
    fixed = TRUE,
    class = "r8_value_error"
  )
  expect_error(Svm(cost = NA_real_), "na_value", class = "r8_value_error")
  expect_error(Svm(cost = c(a = 1)), "r_attributes", class = "r8_value_error")
  expect_error(Svm(cost = c(1, 2)), "r_length", class = "r8_value_error")
  expect_error(Svm(mtry = 1.5), "r_type", class = "r8_value_error")
  expect_error(Svm(kernel = "Linear"), "r_type", class = "r8_value_error")
})

test_that("values are immutable: replacing a field returns a new, validated value", {
  t <- svm_types()
  x <- t[["Svm"]](cost = 2)
  y <- x
  y$cost <- 5
  expect_identical(x$cost, 2)
  expect_identical(y$cost, 5)
  expect_error(y$cost <- -1, "below_min", class = "r8_value_error")
  expect_error(names(y) <- c("a", "b", "c", "d"), class = "r8_value_error")
  expect_error(y[1] <- list(3), class = "r8_value_error")
  expect_error(length(y) <- 1, class = "r8_value_error")
})

test_that("a field write runs the rules whose invariant or condition reads the field", {
  Fit <- declare_in("writes", {
    new_struct(
      "Fit",
      fields = list(
        n_trees = type_integer(min = 1L),
        stop_at = option(type_integer(min = 1L)),
        verbose = type_logical(),
        log_file = option(type_character())
      ),
      rules = list(
        rule(
          stop_at < n_trees,
          when = !is.null(stop_at),
          message = "stop_at is below n_trees.",
          id = "STOP_BELOW_TREES"
        ),
        rule(
          verbose == TRUE,
          when = !is.null(log_file),
          message = "a log file requires verbose output.",
          id = "LOG_NEEDS_VERBOSE"
        )
      )
    )
  })
  x <- Fit(n_trees = 10L, verbose = FALSE)
  # The invariant reads n_trees; the rule holds while stop_at is absent.
  x$n_trees <- 5L
  # Only the condition's presence test and the invariant read stop_at.
  expect_error(x$stop_at <- 20L, "STOP_BELOW_TREES", class = "r8_value_error")
  x$stop_at <- 3L
  expect_error(x$n_trees <- 2L, "STOP_BELOW_TREES", class = "r8_value_error")
  # log_file appears only in a condition's presence test.
  expect_error(
    x$log_file <- "fit.log",
    "LOG_NEEDS_VERBOSE",
    class = "r8_value_error"
  )
  x$verbose <- TRUE
  x$log_file <- "fit.log"
  expect_identical(x$log_file, "fit.log")
})

test_that("a redeclared rule applies to the next field write", {
  Capped <- declare_in("redeclare", {
    new_struct(
      "Capped",
      fields = list(n = type_integer()),
      rules = list(rule(n <= 10L, message = "n is at most 10.", id = "CAP"))
    )
  })
  x <- Capped(n = 1L)
  x$n <- 8L
  Capped <- declare_in("redeclare", {
    new_struct(
      "Capped",
      fields = list(n = type_integer()),
      rules = list(rule(n <= 5L, message = "n is at most 5.", id = "CAP"))
    )
  })
  expect_error(x$n <- 8L, "CAP", class = "r8_value_error")
  x$n <- 4L
  expect_identical(x$n, 4L)
})

test_that("a document round trips to an identical value", {
  t <- svm_types()
  x <- t[["Svm"]](
    cost = 2,
    kernel = t[["Kernel"]]@Rbf(gamma = 0.5),
    weights = c(1, 2.5)
  )
  json <- to_json(x)
  expect_identical(
    json,
    '{"$type":"rtemis::Svm","$types":{"rtemis::Kernel":1,"rtemis::Svm":1},"cost":2,"kernel":{"gamma":0.5,"type":"Rbf"},"weights":[1,2.5],"mtry":null}'
  )
  expect_identical(r8_from_json(t[["Svm"]], json), x)
  expect_error(
    r8_from_json(t[["Kernel"]], json),
    "document_type",
    class = "r8_value_error"
  )
})

test_that("every field type has an R form that round trips through JSON", {
  Every <- declare_in("demo", {
    Shape <- new_enum(
      "Shape3",
      variants = list(Circle = variant(), Square = variant())
    )
    new_struct(
      "Every",
      fields = list(
        d = type_date(),
        t = type_posixct(),
        f = type_factor(levels = c("lo", "hi")),
        fd = type_factor(),
        m = type_matrix(option(type_double())),
        v = vec(option(type_integer())),
        hm = hashmap(type_double()),
        om = ordered_map(type_character()),
        j = type_json(),
        b = type_raw(),
        s = field(Shape),
        tb = type_table(
          list(
            id = field(type_character(), unique = TRUE),
            n = option(type_integer())
          ),
          open = TRUE
        ),
        model = option(type_r("lm"), resolve = "runtime")
      )
    )
  })
  Shape <- get("demo::Shape3", envir = R8:::.r8_types)
  fit <- stats::lm(dist ~ speed, data = datasets::cars)
  x <- Every(
    d = as.Date(19782L),
    t = as.POSIXct(1.0000004, origin = "1970-01-01", tz = "UTC"),
    f = factor(c("hi", "lo"), levels = c("lo", "hi")),
    fd = factor(c("b", "a", "b")),
    m = matrix(c(1, NA, 3, 4), 2, dimnames = list(c("r1", "r2"), NULL)),
    v = c(1L, NA, 3L),
    hm = c(b = 2, a = 1),
    om = list(z = "first", a = "second"),
    j = list(k = list(1, "two"), empty = stats::setNames(list(), character(0))),
    b = as.raw(c(0, 1, 250)),
    s = Shape@Square,
    tb = data.frame(id = c("a", "b"), n = c(1L, NA), score = c(0.5, 0.9)),
    model = fit
  )
  # Normalization at construction: an integer Date as a double, a date-time
  # rounded to the microsecond, map keys sorted.
  expect_identical(unclass(x$d), 19782)
  expect_identical(as.numeric(x$t), 1.0)
  expect_identical(names(x$hm), c("a", "b"))
  expect_identical(names(x$om), c("z", "a"))
  expect_identical(x$model, fit)
  y <- r8_from_json(Every, to_json(x))
  # The runtime-only field has no JSON form.
  expect_null(y$model)
  x_without <- x
  x_without$model <- NULL
  expect_identical(y, x_without)
  expect_error(
    Every(d = as.Date(19782.5)),
    "date_fraction",
    class = "r8_value_error"
  )
})

test_that("a sparse matrix field is a dgCMatrix and round trips", {
  skip_if_not_installed("Matrix")
  Counts <- declare_in(
    "demo",
    new_struct("Counts", fields = list(x = type_sparse_matrix()))
  )
  m <- Matrix::sparseMatrix(
    i = c(1L, 3L, 2L),
    j = c(1L, 1L, 2L),
    x = c(1.5, 2, 3),
    dims = c(3L, 2L)
  )
  v <- Counts(x = m)
  expect_s4_class(v$x, "dgCMatrix")
  expect_identical(as.matrix(v$x), as.matrix(m))
  expect_identical(r8_from_json(Counts, to_json(v)), v)
  expect_error(Counts(x = as.matrix(m)), "dgCMatrix", class = "r8_value_error")
})

test_that("r8_conform() reports each change and constructs the value strictly", {
  T <- declare_in("demo", {
    Inner <- new_struct("Inner", fields = list(a = type_double()))
    new_struct(
      "Conformed",
      fields = list(
        w = vec(type_double(min = 0)),
        g = type_factor(levels = c("lo", "hi")),
        when = type_posixct(),
        tb = type_table(list(id = type_character())),
        inner = field(Inner)
      )
    )
  })
  df <- data.frame(id = c("a", "b"), row.names = c("r1", "r2"))
  input <- list(
    w = stats::quantile(c(1, 2, 3)),
    g = c("hi", "lo"),
    when = as.POSIXlt("2024-01-01 12:00:00", tz = "UTC"),
    tb = df,
    inner = list(a = 1),
    colour = "red"
  )
  expect_message(x <- r8_conform(input, T), "6 change")
  expect_identical(names(x$w), NULL)
  expect_s3_class(x$g, "factor")
  expect_s3_class(x$inner, "demo::Inner")
  # Construction stays strict for what conform cannot fix.
  expect_error(
    suppressMessages(r8_conform(list(w = -1), T)),
    class = "r8_value_error"
  )
  expect_error(
    T(w = stats::quantile(c(1, 2, 3))),
    "r_attributes",
    class = "r8_value_error"
  )
})

test_that("r8_equal() compares contents, and r8_hash() identifies them", {
  t <- svm_types()
  Svm <- t[["Svm"]]
  a <- Svm(cost = 2)
  b <- Svm(cost = 2L, kernel = t[["Kernel"]]@Linear)
  expect_true(r8_equal(a, b))
  expect_true(a == b)
  expect_false(a != b)
  expect_false(r8_equal(a, Svm(cost = 3)))
  expect_identical(r8_hash(a), r8_hash(b))
  expect_false(identical(r8_hash(a), r8_hash(Svm(cost = 3))))
  expect_match(r8_hash(a), "^sha256:[0-9a-f]{64}$")
  expect_error(a < b, class = "r8_dispatch_error")
})

test_that("a runtime-only field enters equality and the hash by presence", {
  M <- declare_in(
    "demo",
    new_struct(
      "WithModel",
      fields = list(
        count = type_integer(),
        model = option(type_r("lm"), resolve = "runtime")
      )
    )
  )
  fit1 <- stats::lm(dist ~ speed, data = datasets::cars)
  fit2 <- stats::lm(dist ~ 1, data = datasets::cars)
  bare <- M(count = 1L)
  with1 <- M(count = 1L, model = fit1)
  with2 <- M(count = 1L, model = fit2)
  expect_false(r8_equal(bare, with1))
  expect_false(r8_equal(with1, with2))
  expect_true(r8_equal(with1, M(count = 1L, model = fit1)))
  # The hash records presence, not the R value.
  expect_false(identical(r8_hash(bare), r8_hash(with1)))
  expect_identical(r8_hash(with1), r8_hash(with2))
})

test_that("matrices compare, hash, and write their dimension names", {
  M <- declare_in(
    "demo",
    new_struct("NamedMatrix", fields = list(m = type_matrix(type_double())))
  )
  n <- 10000L
  rows <- paste0("r", seq_len(n))
  x <- matrix(seq_len(2L * n) / 2, n, dimnames = list(rows, c("a", "b")))
  y <- x
  rownames(y)[n] <- "other"
  z <- x
  rownames(z)[n] <- NA
  vx <- M(m = x)
  expect_true(r8_equal(vx, M(m = x)))
  expect_false(r8_equal(vx, M(m = y)))
  expect_false(r8_equal(vx, M(m = z)))
  expect_false(identical(r8_hash(vx), r8_hash(M(m = y))))
  expect_identical(r8_from_json(M, to_json(vx)), vx)
})

test_that("sizes print in IEC units, and an R-native value shows none", {
  f <- R8:::r8_format_size
  expect_identical(f(120), "120 B")
  expect_identical(f(1024), "1.00 KiB")
  expect_identical(f(8000048), "7.63 MiB")
  expect_identical(f(800000048), "763 MiB")
  P <- declare_in("size_demo", {
    new_struct("P", fields = list(x = type_double()))
  })
  expect_identical(format(P(x = 1))[[1L]], "<size_demo::P>")
  expect_identical(utils::capture.output(str(P(x = 1)))[[1L]], "<size_demo::P>")
})

test_that("a constructor forces its arguments in order and reports a missing one", {
  T <- declare_in("frame", {
    new_struct(
      "T",
      fields = list(
        a = type_integer(),
        b = option(type_integer()),
        c = field(type_double(), default = 0.5)
      )
    )
  })
  seen <- character()
  v <- T(
    a = {
      seen <- c(seen, "a")
      1L
    },
    b = {
      seen <- c(seen, "b")
      2L
    }
  )
  expect_identical(seen, c("a", "b"))
  expect_identical(v$c, 0.5)
  expect_null(T(a = 1L)$b)
  e <- expect_error(T(b = 2L), class = "r8_value_error")
  expect_match(conditionMessage(e), "missing_field")
  expect_error(T(a = stop("raised while forcing")), "raised while forcing")
  E <- declare_in("frame", {
    new_enum(
      "E",
      variants = list(
        V = variant(fields = list(x = type_double(), y = option(type_double())))
      )
    )
  })
  expect_identical(E@V(x = 2)$x, 2)
  expect_error(E@V(), "missing_field", class = "r8_value_error")
  # A required argument missing through a wrapper is a missing field.
  make <- function(a) T(a = a)
  expect_error(make(), "missing_field", class = "r8_value_error")
  # A field's declared default applies before the rules run.
  Shape <- declare_in(
    "frame",
    new_enum("Shape", variants = list(Wide = variant(), Tall = variant()))
  )
  Rect <- declare_in("frame", {
    new_struct(
      "Rect",
      fields = list(
        w = type_double(),
        h = type_double(),
        shape = field(Shape, default = Shape@Wide)
      ),
      rules = list(rule(
        w >= h,
        when = is_variant(shape, "Wide"),
        message = "wide",
        id = "WIDE"
      ))
    )
  })
  expect_error(Rect(w = 1, h = 2), "WIDE", class = "r8_value_error")
})
