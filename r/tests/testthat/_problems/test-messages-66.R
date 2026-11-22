# Extracted from test-messages.R:66

# test -------------------------------------------------------------------------
declare_in("msgunknown", {
  P <- new_struct(
    "P",
    fields = list(values = vec(type_double()), lo = type_double())
  )
  e <- tryCatch(r8_from_json(P, '{"valeus":[1],"lo":1}'), error = identity)
  expect_match(
    conditionMessage(e),
    "`valeus` is not a field of `msgunknown::P`; the nearest field is `values`.",
    fixed = TRUE
  )
  e <- tryCatch(
    r8_from_json(P, '{"values":[1],"lo":1,"zz":2}'),
    error = identity
  )
  expect_match(
    conditionMessage(e),
    "`zz` is not a field of `msgunknown::P`, whose fields are `values` and `lo`.",
    fixed = TRUE
  )
})
