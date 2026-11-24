# test-144-scenarios.R
# ::R8::
# 2026- EDG rtemis.org

# The written list of the scenarios is that of the test files.

test_that("every scenario has an id of its own, in order, and a group", {
  s <- scenario_table()
  expect_identical(anyDuplicated(s$id), 0L)
  expect_true(all(substr(s$id, 1L, 1L) %in% names(SCENARIO_GROUPS)))
  for (g in names(SCENARIO_GROUPS)) {
    n <- as.integer(substring(s$id[substr(s$id, 1L, 1L) == g], 2L))
    expect_identical(n, seq_along(n))
  }
})

test_that("the text of a scenario names no internal of R8", {
  s <- scenario_table()
  text <- paste(s$title, s$situation, s$risk, s$result)
  internal <- grepl(
    "r8_type|r8_variant\\b|attribute `|R8:::|SEXP|codec|registry",
    text
  )
  expect_identical(s$id[internal], character())
})

test_that("tests/scenarios.md is written from the test files", {
  path <- test_path("..", "scenarios.md")
  skip_if_not(file.exists(path), "tests/scenarios.md is not here")
  expect_identical(readLines(path), scenario_text())
})
