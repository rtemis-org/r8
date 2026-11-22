# Extracted from test-rules.R:13

# test -------------------------------------------------------------------------
declare_in("rulesone", {
  S <- new_struct(
    "S",
    fields = list(lo = type_double(), hi = type_double()),
    rules = rule(lo < hi, message = "lo is below hi.", id = "LO_HI")
  )
  expect_s3_class(S(lo = 1, hi = 2), "r8_value")
  expect_error(S(lo = 2, hi = 1), "rule LO_HI", class = "r8_value_error")
  expect_error(
    new_struct(
      "T",
      fields = list(lo = type_double()),
      rules = list(rule(lo > 0, message = "m", id = "A"), "lo > 0")
    ),
    "`rules[[2]]` is not a rule()",
    fixed = TRUE,
    class = "r8_declaration_error"
  )
})
