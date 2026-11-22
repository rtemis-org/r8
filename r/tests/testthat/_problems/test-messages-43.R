# Extracted from test-messages.R:43

# test -------------------------------------------------------------------------
declare_in("msgform", {
  K <- new_enum(
    "K",
    variants = list(
      Linear = variant(),
      Rbf = variant(fields = list(gamma = type_double()))
    )
  )
  P <- new_struct(
    "P",
    fields = list(
      x = type_double(max = 0),
      s = type_character(values = c("linear", "rbf")),
      w = vec(type_double(min = 0)),
      m = hashmap(type_integer(min = 0L)),
      k = K
    )
  )
  e <- tryCatch(
    P(x = 4, s = "rbff", w = c(1, -2), m = list(a = 1L, b = -1L), k = K@Linear),
    error = identity
  )
  m <- conditionMessage(e)
  expect_match(m, "Invalid `msgform::P`: 4 problems\n", fixed = TRUE)
  expect_match(m, "- `x` must be at most 0; found 4. [above_max]", fixed = TRUE)
  expect_match(
    m,
    "- `s` must be one of \"linear\" or \"rbf\"; found \"rbff\"; the nearest value is \"rbf\". [not_in_enum]",
    fixed = TRUE
  )
  expect_match(
    m,
    "- `w[[2]]` must be at least 0; found -2. [below_min]",
    fixed = TRUE
  )
  expect_match(
    m,
    "- `m[[\"b\"]]` must be at least 0; found -1. [below_min]",
    fixed = TRUE
  )
  p <- e$problems
  expect_identical(p$where, c("x", "s", "w[[2]]", "m[[\"b\"]]"))
  expect_identical(p$path, c("/x", "/s", "/w/1", "/m/b"))
  expect_identical(p$suggestion, c(NA, "rbf", NA, NA))
  json <- to_json(P(x = 0, s = "rbf", w = 1, m = list(), k = K@Rbf(gamma = 1)))
  bad <- sub('"type":"Rbf"', '"type":"Rbff"', json, fixed = TRUE)
  e <- tryCatch(r8_from_json(P, bad), error = identity)
  expect_match(
    conditionMessage(e),
    "`k` must be a variant of `msgform::K`: \"Linear\" or \"Rbf\"; found \"Rbff\"; the nearest value is \"Rbf\".",
    fixed = TRUE
  )
})
