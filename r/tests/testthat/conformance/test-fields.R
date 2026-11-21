# test-fields.R
# ::R8::
# 2026- EDG rtemis.org

# Fields of a `Conform` value: construction, reading, defaults, problems,
# replacement, equality, and r8_conform(), for R-native and Rust-native
# values alike (helper-conformance.R).
# spec: R8/class-system#conformance-tests

test_that("a value constructs and reads each field", {
  p <- conformance_provider()
  x <- conform_value(p)
  expect_s3_class(x, c(paste0(p[["namespace"]], "::Conform"), "r8_value"))
  expect_identical(x$ratio, 0.5)
  expect_identical(x$count, 3L)
  expect_identical(x$flag, TRUE)
  expect_identical(x$label, "abc")
  expect_identical(x$seed, 1L)
  expect_identical(x$weights, c(0.5, 1.5))
  expect_identical(x$index, c(1L, 2L))
  expect_identical(x$bytes, as.raw(1:3))
  expect_identical(x$mask, c(TRUE, FALSE))
  expect_identical(x[["label"]], "abc")
  expect_identical(
    names(x),
    c(
      "ratio",
      "count",
      "flag",
      "label",
      "seed",
      "weights",
      "index",
      "bytes",
      "mask"
    )
  )
  expect_error(x$colour, "not a field", class = "r8_value_error")
})

test_that("a default fills an omitted field, and an option may be absent", {
  p <- conformance_provider()
  x <- conform_value(p, count = NULL, seed = NULL)
  expect_identical(x$count, 10L)
  expect_null(x$seed)
})

test_that("construction reports every problem with its path and rule", {
  p <- conformance_provider()
  e <- expect_error(
    conform_value(p, ratio = 2, count = 0L, label = "ABC"),
    class = "r8_value_error"
  )
  expect_match(conditionMessage(e), "above_max at /ratio")
  expect_match(conditionMessage(e), "below_min at /count")
  expect_match(conditionMessage(e), "pattern_mismatch at /label")
  expect_error(
    conform_value(p, seed = 5L),
    "seed_below_count",
    class = "r8_value_error"
  )
  expect_error(
    conform_value(p, weights = 1),
    "weights_match_index",
    class = "r8_value_error"
  )
  expect_error(
    conform_value(p, ratio = NA_real_),
    "na_value",
    class = "r8_value_error"
  )
  expect_error(
    conform_value(p, ratio = c(0.1, 0.2)),
    "r_length",
    class = "r8_value_error"
  )
  expect_error(
    conform_value(p, count = 1.5),
    "r_type",
    class = "r8_value_error"
  )
  expect_error(
    conform_value(p, flag = "yes"),
    "r_type",
    class = "r8_value_error"
  )
  expect_error(
    conform_value(p, label = c(a = "abc")),
    "r_attributes",
    class = "r8_value_error"
  )
})

test_that("replacing a field returns a new, validated value", {
  p <- conformance_provider()
  x <- conform_value(p)
  y <- x
  y$count <- 4L
  expect_identical(x$count, 3L)
  expect_identical(y$count, 4L)
  expect_error(y$ratio <- 2, "above_max", class = "r8_value_error")
  expect_error(y$seed <- 10L, "seed_below_count", class = "r8_value_error")
  expect_error(y[1] <- list(3), class = "r8_value_error")
  expect_error(names(y) <- letters[1:9], class = "r8_value_error")
  expect_identical(y$count, 4L)
})

test_that("equality and the hash follow the contents", {
  p <- conformance_provider()
  a <- conform_value(p)
  b <- conform_value(p)
  c <- conform_value(p, ratio = 0.25)
  expect_true(r8_equal(a, b))
  expect_identical(r8_hash(a), r8_hash(b))
  expect_false(r8_equal(a, c))
  expect_false(identical(r8_hash(a), r8_hash(c)))
})

test_that("r8_conform() converts what it can and constructs strictly", {
  p <- conformance_provider()
  input <- conform_args(count = 3, index = c(1, 2), colour = "red")
  expect_message(x <- r8_conform(input, p[["Conform"]]), "change")
  expect_identical(x$count, 3L)
  expect_identical(x$index, c(1L, 2L))
  expect_true(r8_equal(x, conform_value(p)))
  expect_error(
    suppressMessages(r8_conform(conform_args(ratio = 2), p[["Conform"]])),
    class = "r8_value_error"
  )
})
