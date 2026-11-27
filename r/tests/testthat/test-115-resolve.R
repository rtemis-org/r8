# test-115-resolve.R
# ::R8::
# 2026- EDG rtemis.org

# Fields that are filled when a value is resolved: computed from other
# fields, or given by the context of a call.

dose_type <- function() {
  declare_in(
    "demo",
    new_struct(
      "DoseR",
      fields = list(
        mg = field(
          option(type_double(min = 0)),
          context = TRUE,
          unit = "mg"
        ),
        g = field(
          option(type_double()),
          compute = calc(mg / 1000),
          unit = "g"
        ),
        kg = field(
          option(type_double()),
          compute = calc(g / 1000),
          unit = "kg"
        ),
        model = field(option(host_class("lm")), context = TRUE)
      )
    )
  )
}

test_that("r8_resolve() fills context fields from context and computed fields in order", {
  Dose <- dose_type()
  x <- Dose()
  expect_null(x$g)
  y <- r8_resolve(x, context = list(mg = 2500))
  expect_identical(y$mg, 2500)
  expect_identical(y$g, 2.5)
  expect_identical(y$kg, 0.0025)
  expect_null(x$mg)
  fit <- stats::lm(dist ~ speed, data = datasets::cars)
  z <- r8_resolve(x, context = list(mg = 1, model = fit))
  expect_identical(z$model, fit)
})

test_that("resolution reports what it cannot fill", {
  Dose <- dose_type()
  expect_error(r8_resolve(Dose()), "computed_absent", class = "r8_value_error")
  expect_error(
    r8_resolve(Dose(), context = list(mg = -1)),
    "below_min",
    class = "r8_value_error"
  )
  expect_error(
    r8_resolve(Dose(), context = list(g = 1)),
    "not an absent context field",
    class = "r8_value_error"
  )
})

test_that("r8_validate(complete = TRUE) names the pending fields", {
  Dose <- dose_type()
  x <- Dose()
  expect_invisible(r8_validate(x))
  e <- expect_error(r8_validate(x, complete = TRUE), class = "r8_value_error")
  expect_match(conditionMessage(e), "`mg` \\(context\\)")
  expect_match(conditionMessage(e), "`g` \\(computed\\)")
  done <- r8_resolve(
    x,
    context = list(
      mg = 1,
      model = stats::lm(dist ~ speed, data = datasets::cars)
    )
  )
  expect_invisible(r8_validate(done, complete = TRUE))
})
