# test-altrep.R
# ::R8::
# 2026- EDG rtemis.org

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
      rules = list(rule("pos", all(x, .x > 0), "every element is positive"))
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
  invisible(to_json(v))
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
  invisible(to_json(v))
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
  invisible(to_json(v))
  expect_true(compact(m@p) && compact(m@i))
})

test_that("%in% and subset() over a compact sequence leave it compact", {
  W <- declare_in(
    "altrep",
    new_struct(
      "W",
      fields = list(x = vec(type_integer()), k = type_integer()),
      rules = list(
        rule("has_k", k %in% x, "x holds k"),
        rule("small", subset(x, c(1L, 2L, 3L)), "x holds 1, 2, and 3 only")
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
