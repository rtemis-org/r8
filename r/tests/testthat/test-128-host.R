# test-128-host.R
# ::R8::
# 2026- EDG rtemis.org

# Methods for classes of R, such as data.frame, matrix, and lm.
#
# spec: R8/class-system#host-classes

test_that("host_class() records S3, implicit, and S4 classes", {
  h <- host_class("difftime")
  expect_s3_class(h, "r8_host_class")
  expect_identical(unclass(h)[["package"]], "")
  expect_identical(unclass(h)[["system"]], "s3")
  expect_identical(unclass(host_class("NULL"))[["system"]], "implicit")
  expect_identical(unclass(host_class("matrix"))[["package"]], "")
  expect_error(
    host_class("matrix", system = "s3"),
    "is an implicit class",
    class = "r8_declaration_error"
  )
  expect_error(
    host_class("frame", system = "implicit"),
    "is not one",
    class = "r8_declaration_error"
  )
  expect_error(
    host_class("pkg::A"),
    "has no `::`",
    class = "r8_declaration_error"
  )
  s4 <- host_class("dgCMatrix", package = "Matrix", system = "s4")
  expect_identical(unclass(s4)[["system"]], "s4")
  expect_output(print(s4), "dgCMatrix\" of Matrix, S4")
  expect_error(host_class("ANY"), "left out", class = "r8_declaration_error")
  expect_error(
    host_class("r8_value"),
    "its type and its ancestors",
    class = "r8_declaration_error"
  )
  expect_error(
    host_class("GRanges", system = "s4"),
    "names its package",
    class = "r8_declaration_error"
  )
  expect_error(
    host_class("matrix", package = "Matrix"),
    "belongs to no package",
    class = "r8_declaration_error"
  )
})

test_that("an R8 generic dispatches on S3 and implicit classes", {
  declare_in("hc1", {
    describe <- generic("describe", params(x))
    implement(describe, "data.frame") <- function(x) "a data frame"
    implement(describe, "tbl") <- function(x) "a tibble"
    implement(describe, "matrix") <- function(x) "a matrix"
    implement(describe, "NULL") <- function(x) "nothing"
    implement(describe, host_class("integer")) <- function(x) "integers"
    expect_identical(describe(data.frame(a = 1)), "a data frame")
    # The class nearer the front of the class vector is more specific.
    tbl <- structure(list(a = 1), class = c("tbl_df", "tbl", "data.frame"))
    expect_identical(describe(tbl), "a tibble")
    expect_identical(describe(diag(2)), "a matrix")
    expect_identical(describe(NULL), "nothing")
    expect_identical(describe(1:3), "integers")
    expect_error(
      describe(1.5),
      "does not implement",
      class = "r8_dispatch_error"
    )
    expect_identical(r8_method(describe, "tbl")(tbl), "a tibble")
    positions <- R8:::impl_tuple(list("data.frame"), "x", "describe")
    expect_identical(as.vector(positions), "\"data.frame\"")
    expect_identical(
      attr(positions, "ir")[[1L]],
      list(kind = "host_class", host = "r", class = "data.frame", system = "s3")
    )
    positions <- R8:::impl_tuple(list("NULL"), "x", "describe")
    expect_identical(attr(positions, "ir")[[1L]][["system"]], "implicit")
  })
})

test_that("the dispatch cache keys a value by its whole chain", {
  declare_in("hc2", {
    which_one <- generic("which_one", params(x))
    implement(which_one, "b") <- function(x) "b"
    implement(which_one, "c") <- function(x) "c"
    implement(which_one, "integer") <- function(x) "integer"
    implement(which_one, "double") <- function(x) "double"
    # One first class, two chains.
    expect_identical(which_one(structure(list(), class = c("a", "b"))), "b")
    expect_identical(which_one(structure(list(), class = c("a", "c"))), "c")
    # An integer and a double matrix.
    expect_identical(which_one(matrix(1L)), "integer")
    expect_identical(which_one(matrix(1)), "double")
    expect_identical(which_one(matrix(1L)), "integer")
    index <- R8:::.r8_dispatch[["hc2::which_one|which_one"]]
    expect_true(exists("class:a\x1fb", envir = index, inherits = FALSE))
    expect_true(exists("matrix:double", envir = index, inherits = FALSE))
    expect_identical(
      R8:::dispatch_key(structure(list(), class = c("a", "b"))),
      "class:a\x1fb"
    )
    expect_null(R8:::dispatch_key(quote(f(x))))
    expect_null(r8_clear_dispatch())
    expect_false(exists("hc2::which_one|which_one", envir = R8:::.r8_dispatch))
  })
})

test_that("host classes take part in multiple dispatch", {
  declare_in("hc3", {
    Sel <- new_struct("Sel", fields = list(cols = vec(type_character())))
    exc <- generic("exc", params(x, idx), dispatch = c("x", "idx"))
    implement(exc, "data.frame", "character") <- function(x, idx) {
      x[, setdiff(names(x), idx), drop = FALSE]
    }
    implement(exc, "data.frame", "numeric") <- function(x, idx) {
      x[, -idx, drop = FALSE]
    }
    implement(exc, "data.frame", Sel) <- function(x, idx) {
      x[, setdiff(names(x), idx$cols), drop = FALSE]
    }
    implement(exc, idx = "NULL") <- function(x, idx) x
    df <- data.frame(a = 1, b = 2, c = 3)
    expect_named(exc(df, "b"), c("a", "c"))
    expect_named(exc(df, 1L), c("b", "c"))
    expect_named(exc(df, 2), c("a", "c"))
    expect_named(exc(df, Sel(cols = c("a", "b"))), "c")
    expect_named(exc(df, NULL), c("a", "b", "c"))
    expect_named(exc(as.list(df), NULL), c("a", "b", "c"))
    expect_identical(
      r8_method(exc, "data.frame", Sel)(df, Sel(cols = "a")),
      df[, c("b", "c"), drop = FALSE]
    )
    # Each impl more specific in one position, at the call.
    pick <- generic("pick", params(x, y), dispatch = c("x", "y"))
    implement(pick, "tbl") <- function(x, y) "tbl, any"
    implement(pick, y = "character") <- function(x, y) "any, character"
    tbl <- structure(list(), class = c("tbl", "data.frame"))
    expect_identical(pick(tbl, 1), "tbl, any")
    expect_error(
      pick(tbl, "a"),
      "dispatch_ambiguous",
      class = "r8_dispatch_error"
    )
    implement(pick, "tbl", "character") <- function(x, y) "tbl, character"
    expect_identical(pick(tbl, "a"), "tbl, character")
  })
})

test_that("R8 values and host objects match their own positions only", {
  declare_in("hc4", {
    A <- new_struct("A", fields = list(a = type_double()))
    kind <- generic("kind", params(x))
    implement(kind, A) <- function(x) "A"
    implement(kind, "list") <- function(x) "list"
    expect_identical(kind(A(a = 1)), "A")
    expect_identical(kind(list(1)), "list")
    # A class vector that names an R8 type is not a value of that type.
    forged <- structure(list(), class = "hc4::A")
    expect_error(
      kind(forged),
      "does not implement",
      class = "r8_dispatch_error"
    )
    B <- new_struct("B", fields = list(b = type_double()))
    expect_error(
      kind(B(b = 1)),
      "does not implement",
      class = "r8_dispatch_error"
    )
    expect_error(
      implement(kind, "hc4::A") <- function(x) "A",
      "by its type object",
      class = "r8_declaration_error"
    )
    expect_error(
      implement(kind, NULL) <- function(x) "x",
      "\"NULL\"",
      class = "r8_declaration_error"
    )
    expect_error(
      implement(summary, "data.frame") <- function(object, ...) NULL,
      "belongs to the package of the generic or of the class",
      class = "r8_declaration_error"
    )
  })
})

test_that("an impl on a host class follows the orphan rule", {
  declare_in("hc5", {
    Describe <- new_trait(
      "Describe",
      methods = list(describe = trait_method(params(x)))
    )
  })
  declare_in("hc6", {
    Describe <- R8:::.r8_traits[["hc5::Describe"]]
    expect_error(
      impl(Describe, "data.frame", methods = list(describe = function(x) "df")),
      "impl_orphan",
      class = "r8_declaration_error"
    )
    impl(
      Describe,
      host_class("frame", package = "hc6"),
      methods = list(describe = function(x) "frame")
    )
    describe <- Describe@describe
    expect_identical(describe(structure(list(), class = "frame")), "frame")
  })
})

test_that("an impl on a host class takes an existing S3 method", {
  declare_in("hc7", {
    Summ <- new_trait(
      "Summ",
      methods = list(
        summary = trait_method(params(object, "...")),
        label = trait_method(params(x))
      )
    )
    # data.frame has summary.data.frame in base, which the impl takes.
    impl(Summ, "data.frame", methods = list(label = function(x) "df"))
    df <- data.frame(a = 1:3)
    expect_identical(summary(df), base:::summary.data.frame(df))
    expect_identical(Summ@label(df), "df")
    expect_error(
      impl(
        Summ,
        "data.frame",
        methods = list(
          summary = function(object, ...) "mine",
          label = function(x) "df"
        )
      ),
      "host_method_exists",
      class = "r8_declaration_error"
    )
    expect_identical(summary(df), base:::summary.data.frame(df))
    # A class without a method of the generic takes the impl's.
    impl(
      Summ,
      "hc7frame",
      methods = list(
        summary = function(object, ...) c("hc7frame", r8_next_impl()),
        label = function(x) "hc7frame"
      )
    )
    x <- structure(list(a = 1:3), class = c("hc7frame", "list"))
    # r8_next_impl() continues to the next S3 method, summary.default.
    expect_identical(
      summary(x),
      c("hc7frame", summary(unclass(x)))
    )
  })
})

test_that("an impl on a host class reaches the next R8 impl of its chain", {
  declare_in("hc8", {
    describe <- generic("describe", params(x))
    implement(describe, "data.frame") <- function(x) "df"
    implement(describe, "tbl") <- function(x) paste("tbl of", r8_next_impl())
    tbl <- structure(list(a = 1), class = c("tbl", "data.frame"))
    expect_identical(describe(tbl), "tbl of df")
  })
})

test_that("a dyn() field admits R8 values only", {
  declare_in("hc9", {
    Describe <- new_trait(
      "Describe",
      methods = list(describe = trait_method(params(x)))
    )
    impl(Describe, "data.frame", methods = list(describe = function(x) "df"))
    A <- new_struct("A", fields = list(a = type_double()))
    impl(Describe, A, methods = list(describe = function(x) "A"))
    Report <- new_struct("Report", fields = list(body = dyn(Describe)))
    expect_s3_class(Report(body = A(a = 1)), "r8_value")
    expect_error(Report(body = data.frame(a = 1)), class = "r8_value_error")
  })
})

test_that("r8_convert() reads the class vector as .class2() gives it", {
  declare_in("hc10", {
    Pct <- new_struct("Pct", fields = list(p = type_double()))
    impl(
      From("numeric"),
      Pct,
      methods = list(from = function(x) Pct(p = as.vector(x) * 100))
    )
    expect_identical(r8_convert(0.5, Pct)$p, 50)
    expect_identical(r8_convert(1L, Pct)$p, 100)
    expect_identical(r8_convert(matrix(1), Pct)$p, 100)
  })
})

test_that("S4 classes are host classes by name and by class object", {
  skip_if_not_installed("Matrix")
  declare_in("hc11", {
    describe <- generic("describe", params(x))
    implement(
      describe,
      methods::getClass("CsparseMatrix", where = asNamespace("Matrix"))
    ) <-
      function(x) "compressed sparse"
    implement(
      describe,
      host_class("dgCMatrix", package = "Matrix", system = "s4")
    ) <-
      function(x) "dgC"
    m <- Matrix::sparseMatrix(i = 1:2, j = 1:2, x = c(1, 2))
    expect_identical(describe(m), "dgC")
    l <- Matrix::sparseMatrix(i = 1:2, j = 1:2, x = c(TRUE, TRUE))
    expect_identical(describe(l), "compressed sparse")
    expect_error(
      implement(
        describe,
        host_class("noSuchClass", package = "Matrix", system = "s4")
      ) <-
        function(x) NULL,
      "host_class_s4",
      class = "r8_declaration_error"
    )
    expect_error(
      implement(describe, methods::getClass("data.frame")) <- function(x) NULL,
      "registered with S4",
      class = "r8_declaration_error"
    )
  })
})

test_that("dyn() of a generic holds a value with a method of it", {
  declare_in("hc12", {
    describe <- generic("describe", params(x))
    A <- new_struct("A", fields = list(a = type_double()))
    B <- new_struct("B", fields = list(b = type_double()))
    implement(describe, A) <- function(x) "A"
    implement(describe, "data.frame") <- function(x) "df"
    Report <- new_struct("Report", fields = list(body = dyn(describe)))
    r <- Report(body = A(a = 1))
    expect_identical(describe(r$body), "A")
    expect_error(Report(body = B(b = 1)), class = "r8_value_error")
    expect_error(Report(body = data.frame()), class = "r8_value_error")
    back <- r8_from_json(Report, r8_to_json(r))
    expect_true(r8_equal(back, r))
    pair <- generic("pair", params(x, y), dispatch = c("x", "y"))
    expect_error(
      dyn(pair),
      "dispatches on 2 arguments",
      class = "r8_declaration_error"
    )
  })
})

test_that("an impl on a host class continues to an internal generic's default", {
  declare_in("hc13", {
    Len <- new_trait(
      "Len",
      methods = list(
        length = trait_method(params(x)),
        label = trait_method(params(x))
      )
    )
    impl(
      Len,
      "hc13frame",
      methods = list(
        length = function(x) 100L + r8_next_impl(),
        label = function(x) "frame"
      )
    )
    expect_identical(length(structure(list(1, 2), class = "hc13frame")), 102L)
    # POSIXlt has length.POSIXlt in base.
    expect_error(
      impl(
        Len,
        "POSIXlt",
        methods = list(length = function(x) 1L, label = function(x) "lt")
      ),
      "host_method_exists",
      class = "r8_declaration_error"
    )
    lt <- as.POSIXlt("2026-01-01", tz = "UTC")
    expect_identical(length(lt), 1L)
  })
})

test_that("a list with the class of a value is refused by dispatch", {
  declare_in("hc14", {
    kind <- generic("kind", params(x))
    implement(kind, "list") <- function(x) "list"
    bad <- structure(list(), class = c("hc14::A", "r8_value"))
    expect_error(kind(bad), "is not one", class = "r8_dispatch_error")
  })
})

test_that("a symbol or a call dispatches as a value and is not evaluated", {
  declare_in("hostlang", {
    kind <- generic("kind", params(x))
    implement(kind, "call") <- function(x) "a call"
    implement(kind, "name") <- function(x) "a name"
    expect_identical(kind(quote(stop("evaluated"))), "a call")
    expect_identical(kind(quote(not_bound)), "a name")
    expect_identical(kind(quote(not_bound)), "a name")
    pair <- generic("pair", params(x, y), dispatch = c("x", "y"))
    implement(pair, "call", "name") <- function(x, y) "a call and a name"
    expect_identical(
      pair(quote(stop("evaluated")), quote(not_bound)),
      "a call and a name"
    )
    `tag<-` <- generic("tag<-", params(x, value))
    implement(`tag<-`, "name") <- function(x, value) as.name(value)
    implement(`tag<-`, "call") <- function(x, value) as.name(value)
    s <- quote(not_bound)
    tag(s) <- "other"
    expect_identical(s, quote(other))
    e <- quote(stop("evaluated"))
    expect_error(
      tag(e) <- "other",
      "replacement_type",
      class = "r8_result_error"
    )
  })
})
