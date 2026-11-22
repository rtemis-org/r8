# Extracted from test-conformance.R:18

# test -------------------------------------------------------------------------
res <- as.data.frame(testthat::test_dir(
  test_path("conformance"),
  reporter = "silent",
  stop_on_failure = FALSE,
  stop_on_warning = FALSE
))
failed <- res[["test"]][res[["failed"]] > 0L | res[["error"]]]
expect_identical(failed, character(), info = paste(failed, collapse = "\n"))
