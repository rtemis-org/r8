# Extracted from test-rules.R:227

# test -------------------------------------------------------------------------
declare_in("rulesshow", {
  S <- new_struct(
    "S",
    fields = list(
      lo = type_double(),
      hi = type_double(),
      values = vec(type_double()),
      secret = field(type_double(), sensitive = TRUE)
    ),
    rules = list(
      rule(lo < hi, message = "lo is below hi.", id = "LO_HI"),
      rule(all(values, .x > 0), message = "values are positive.", id = "POS"),
      rule(secret < 10, message = "secret is below 10.", id = "SECRET")
    )
  )
  e <- tryCatch(
    S(lo = 2, hi = 1, values = c(1, -1, 3, -2, -5, -6, -7), secret = 12),
    error = identity
  )
  m <- conditionMessage(e)
  expect_match(m, "  lo < hi, with lo = 2, hi = 1", fixed = TRUE)
  expect_match(
    m,
    "[rule_violation at /values/1, /values/3, /values/4, and 2 more]",
    fixed = TRUE
  )
  expect_match(
    m,
    "all(values, .x > 0), with length(values) = 7, .x = -1 at /values/1",
    fixed = TRUE
  )
  expect_match(m, "secret = <sensitive>", fixed = TRUE)
  expect_false(grepl("12", m, fixed = TRUE))
  expect_identical(
    e$problems$rule,
    c("LO_HI", rep("POS", 5L), "SECRET")
  )
  expect_identical(e$problems$diagnosis[[1L]]$fields, c("lo = 2", "hi = 1"))
  op <- options(r8.show_values = FALSE)
  on.exit(options(op), add = TRUE)
  e <- tryCatch(
    S(lo = 2, hi = 1, values = 1, secret = 1),
    error = identity
  )
  expect_match(conditionMessage(e), "  lo < hi\n?$")
  expect_null(e$problems$diagnosis)
})
