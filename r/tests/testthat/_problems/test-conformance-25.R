# Extracted from test-conformance.R:25

# test -------------------------------------------------------------------------
res <- as.data.frame(testthat::test_dir(
  test_path("conformance"),
  reporter = "silent",
  stop_on_failure = FALSE,
  stop_on_warning = FALSE
))
failed <- res[["test"]][res[["failed"]] > 0L | res[["error"]]]
expect_identical(failed, character(), info = paste(failed, collapse = "\n"))
expect_identical(
  res[["test"]][res[["skipped"]]],
  "a value crosses to mirai and future workers and back"
)
expect_gt(sum(res[["nb"]]), 60L)
