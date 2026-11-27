# test-workers.R
# ::R8::
# 2026- EDG rtemis.org

# A `Probe` value crosses to mirai and future workers and back. A worker
# loads the type's package, so the test runs with the Rust-stored provider,
# whose package is installed (helper-conformance.R).
# spec: R8/class-system#conformance-tests

test_that("a value crosses to mirai and future workers and back", {
  p <- conformance_provider()
  skip_unless(p, "workers")
  skip_if_not_installed("mirai")
  skip_if_not_installed("future")
  x <- conform_value(p)
  mirai::daemons(1)
  on.exit(mirai::daemons(0), add = TRUE)
  m <- mirai::mirai(
    {
      eval(parse(text = setup))
      list(x$count, x$weights * 2, x, R8::r8hash(x))
    },
    x = x,
    setup = p[["worker_setup"]]
  )[]
  expect_identical(m[[1L]], 3L)
  expect_identical(m[[2L]], c(1, 3))
  expect_true(r8equal(m[[3L]], x))
  expect_identical(m[[4L]], r8hash(x))
  future::plan(future::multisession, workers = 1)
  on.exit(future::plan(future::sequential), add = TRUE)
  f <- future::value(future::future(
    {
      eval(parse(text = setup))
      y <- x
      y$count <- 2L
      y
    },
    globals = list(x = x, setup = p[["worker_setup"]]),
    packages = character()
  ))
  expect_identical(f$count, 2L)
  expect_identical(x$count, 3L)
  expect_true(r8equal(f, conform_value(p, count = 2L)))
})
