# test-117-altrep.R
# ::R8::
# 2026- EDG rtemis.org

# Vectors that R has not written out in memory, such as 1:1e9, stay so.
#
# R8 reads ALTREP vectors by element or by region and leaves them
# unmaterialized: a compact sequence stays compact, including one inside the
# wrapper that base R puts around an ALTREP vector given attributes.

# Whether `x` is or wraps an unexpanded compact sequence.
compact <- function(x) {
  lines <- utils::capture.output(.Internal(inspect(x)))
  any(grepl("(compact)", lines, fixed = TRUE))
}

test_that("a rule over a compact sequence leaves it compact", {
  S <- declare_in(
    "altrep",
    new_struct(
      "S",
      fields = list(x = vec(type_integer())),
      rules = list(rule(
        all(x, .x > 0),
        message = "every element is positive",
        id = "pos"
      ))
    )
  )
  x <- 1:100000
  expect_true(compact(x))
  s <- S(x = x)
  expect_true(compact(x))
  expect_error(S(x = 0:99999), class = "r8_value_error")
})

test_that("operations on a field holding a compact sequence leave it compact", {
  V <- declare_in(
    "altrep",
    new_struct("V", fields = list(x = vec(type_integer())))
  )
  x <- 1:100000
  v <- V(x = x)
  invisible(r8_hash(v))
  invisible(r8_validate(v))
  expect_true(r8_equal(v, V(x = 1:100000)))
  invisible(r8_to_json(v))
  expect_true(compact(x))
})

test_that("a matrix over a wrapped compact sequence stays compact", {
  M <- declare_in(
    "altrep",
    new_struct("M", fields = list(m = type_matrix(type_integer())))
  )
  x <- 1:100000
  dim(x) <- c(100L, 1000L)
  expect_true(compact(x))
  m <- M(m = x)
  invisible(r8_hash(m))
  invisible(r8_validate(m))
  expect_true(r8_equal(m, M(m = x)))
  expect_true(compact(x))
})

test_that("a factor over a compact sequence stays compact", {
  Fc <- declare_in(
    "altrep",
    new_struct("Fc", fields = list(f = type_factor()))
  )
  n <- 100000L
  x <- structure(
    seq_len(n),
    levels = as.character(seq_len(n)),
    class = "factor"
  )
  expect_true(compact(x))
  v <- Fc(f = x)
  invisible(r8_hash(v))
  invisible(r8_validate(v))
  expect_true(r8_equal(v, Fc(f = x)))
  invisible(r8_to_json(v))
  expect_true(compact(x))
})

test_that("a sparse matrix with compact slots leaves them compact", {
  skip_if_not_installed("Matrix")
  S <- declare_in(
    "altrep",
    new_struct("S", fields = list(x = type_sparse_matrix()))
  )
  n <- 100000L
  m <- Matrix::sparseMatrix(i = seq_len(n), j = seq_len(n), x = rep(1, n))
  # Slot assignment keeps the compact sequences that new() expands.
  m@p <- 0:n
  m@i <- 0:(n - 1L)
  expect_true(compact(m@p) && compact(m@i))
  v <- S(x = m)
  invisible(r8_hash(v))
  invisible(r8_validate(v))
  expect_true(r8_equal(v, S(x = m)))
  invisible(r8_to_json(v))
  expect_true(compact(m@p) && compact(m@i))
})

test_that("%in% and subset() over a compact sequence leave it compact", {
  W <- declare_in(
    "altrep",
    new_struct(
      "W",
      fields = list(x = vec(type_integer()), k = type_integer()),
      rules = list(
        rule(k %in% x, message = "x holds k", id = "has_k"),
        rule(
          subset(x, c(1L, 2L, 3L)),
          message = "x holds 1, 2, and 3 only",
          id = "small"
        )
      )
    )
  )
  x <- 1:100000
  e <- expect_error(W(x = x, k = 0L), class = "r8_value_error")
  expect_match(conditionMessage(e), "has_k")
  expect_match(conditionMessage(e), "small")
  expect_true(compact(x))
  expect_s3_class(W(x = 1:3, k = 2L), "r8_value")
})

test_that("a compact sequence validates from its ends, and a violation names its item", {
  Idx <- declare_in("altrep_bounds", {
    new_struct(
      "Idx",
      fields = list(i = vec(type_integer(min = 1L, max = 1000000L)))
    )
  })
  expect_identical(length(Idx(i = 1:1000000)$i), 1000000L)
  expect_identical(Idx(i = 1000000:1)$i[[1L]], 1000000L)
  e <- expect_error(Idx(i = 0:9), class = "r8_value_error")
  expect_identical(e$problems$path, "/i/0")
  expect_match(
    conditionMessage(e),
    "`i[[1]]` must be at least 1; found 0.",
    fixed = TRUE
  )
  e <- expect_error(Idx(i = 2:1000001), class = "r8_value_error")
  expect_identical(e$problems$path, "/i/999999")
  expect_identical(e$problems$where, "i[[1000000]]")
  # A closed set of values needs every item.
  Few <- declare_in("altrep_bounds", {
    new_struct(
      "Few",
      fields = list(i = vec(type_integer(values = c(1L, 2L, 4L))))
    )
  })
  e <- expect_error(Few(i = 1:4), class = "r8_value_error")
  expect_match(
    conditionMessage(e),
    "`i[[3]]` must be one of 1, 2, or 4; found 3.",
    fixed = TRUE
  )
})
