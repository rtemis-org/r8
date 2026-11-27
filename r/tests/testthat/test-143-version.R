# test-143-version.R
# ::R8::
# 2026- EDG rtemis.org

# The version of the runtime that the package is built with.

test_that("r8runtime_version() returns one version string", {
  v <- r8runtime_version()
  expect_type(v, "character")
  expect_length(v, 1L)
  expect_match(v, "^[0-9]+\\.[0-9]+\\.[0-9]+")
})
