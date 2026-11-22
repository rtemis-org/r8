# Extracted from test-altrep.R:138

# prequel ----------------------------------------------------------------------
compact <- function(x) {
  lines <- utils::capture.output(.Internal(inspect(x)))
  any(grepl("(compact)", lines, fixed = TRUE))
}

# test -------------------------------------------------------------------------
Idx <- declare_in("altrep_bounds", {
  new_struct(
    "Idx",
    fields = list(i = vec(type_integer(min = 1L, max = 1000000L)))
  )
})
expect_identical(length(Idx(i = 1:1000000)$i), 1000000L)
expect_identical(Idx(i = 1000000:1)$i[[1L]], 1000000L)
e <- expect_error(Idx(i = 0:9), class = "r8_value_error")
expect_match(conditionMessage(e), "/i/0")
