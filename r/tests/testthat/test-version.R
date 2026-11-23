# test-version.R
# ::R8::
# 2026- EDG rtemis.org

test_that("r8_runtime_version() returns one version string", {
  v <- r8_runtime_version()
  expect_type(v, "character")
  expect_length(v, 1L)
  expect_match(v, "^[0-9]+\\.[0-9]+\\.[0-9]+")
})
