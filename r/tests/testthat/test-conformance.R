# test-conformance.R
# ::R8::
# 2026- EDG rtemis.org

# Runs the conformance tests (tests/testthat/conformance) against `Probe`
# declared in R. test-packages.R runs them against the Rust-stored
# `pkgr::Probe` of r8pkgR.
# spec: R8/class-system#conformance-tests

test_that("the conformance tests pass against an R-stored type", {
  res <- as.data.frame(testthat::test_dir(
    test_path("conformance"),
    reporter = "silent",
    stop_on_failure = FALSE,
    stop_on_warning = FALSE
  ))
  failed <- res[["test"]][res[["failed"]] > 0L | res[["error"]]]
  expect_identical(failed, character(), info = paste(failed, collapse = "\n"))
  # Workers load the type's package, which an R-stored type declared here
  # has none of.
  expect_identical(
    res[["test"]][res[["skipped"]]],
    "a value crosses to mirai and future workers and back"
  )
  expect_gt(sum(res[["nb"]]), 60L)
})
