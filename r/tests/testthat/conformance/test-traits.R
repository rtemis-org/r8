# test-traits.R
# ::R8::
# 2026- EDG rtemis.org

# Traits and generics declared in R for `Probe`, and the impls of R8's
# standard traits that the provider declares, for R-stored and Rust-stored
# values alike (helper-conformance.R).
# spec: R8/class-system#conformance-tests

test_that("a trait and a generic declared in R dispatch on the value", {
  p <- conformance_provider()
  Probe <- p[["Probe"]]
  x <- conform_value(p)
  Size <- declare_in("conftest", {
    Size <- new_trait(
      "Size",
      methods = list(total = trait_method(params(x)))
    )
    impl(Size, Probe, methods = list(total = function(x) sum(x$weights)))
    Size
  })
  expect_identical(Size@total(x), 2)
  weight_mean <- declare_in("conftest", {
    weight_mean <- generic("weight_mean", params(x))
    implement(weight_mean, Probe) <- function(x) mean(x$weights)
    weight_mean
  })
  expect_identical(weight_mean(x), 1)
})

test_that("Display gives format(), print(), and a nested value's text", {
  p <- conformance_provider()
  x <- conform_value(p)
  expect_identical(format(x), "Probe abc (3)")
  expect_output(print(x, output_type = "plain"), "^Probe abc \\(3\\)$")
  expect_identical(r8_layout(list(c = x))[[2L]], "  c: Probe abc (3)")
})

test_that("PartialOrd orders values", {
  p <- conformance_provider()
  x <- conform_value(p)
  y <- conform_value(p, ratio = 0.25)
  expect_true(x > y)
  expect_false(x < y)
  expect_true(x <= x)
  expect_identical(r8_partial_cmp(x, y), 1L)
  expect_identical(r8_partial_cmp(y, x), -1L)
  expect_identical(r8_partial_cmp(x, 1), NA_integer_)
})

test_that("Shape gives dim(), nrow(), and ncol()", {
  p <- conformance_provider()
  x <- conform_value(p)
  expect_identical(dim(x), c(2L, 2L))
  expect_identical(nrow(x), 2L)
  expect_identical(ncol(x), 2L)
})

test_that("the default str() shows the layout", {
  p <- conformance_provider()
  x <- conform_value(p)
  expect_output(
    str(x),
    sprintf("<%s::Probe>", p[["namespace"]]),
    fixed = TRUE
  )
  expect_output(str(x), "weights: double[2] 0.5 1.5", fixed = TRUE)
})
