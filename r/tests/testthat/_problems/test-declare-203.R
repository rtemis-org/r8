# Extracted from test-declare.R:203

# test -------------------------------------------------------------------------
expect_error(
  type_double(min = 0, exclusive_min = 0),
  "bound the same side",
  class = "r8_declaration_error"
)
expect_error(
  type_double(max = 1, exclusive_max = 2),
  "bound the same side",
  class = "r8_declaration_error"
)
expect_error(
  type_integer(min = 1L, exclusive_min = 0L),
  "bound the same side",
  class = "r8_declaration_error"
)
T <- declare_in("bounds", {
  new_struct(
    "Range",
    fields = list(
      p = type_double(exclusive_min = 0, max = 1),
      n = type_integer(min = 1L, exclusive_max = 10L)
    )
  )
})
expect_identical(T(p = 1, n = 9L)$n, 9L)
expect_error(T(p = 0, n = 1L), "is not above 0", class = "r8_value_error")
