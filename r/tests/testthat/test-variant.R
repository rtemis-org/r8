# test-variant.R
# ::R8::
# 2026- EDG rtemis.org

# The name of an enum value's variant, and the test of one.
# spec: R8/class-system#values-in-r

resampler <- function(ns) {
  declare_in(ns, {
    Split <- new_struct(
      "Split",
      fields = list(train = vec(type_integer(min = 1L)))
    )
    new_enum(
      "Resampler",
      variants = list(
        KFold = variant(
          fields = list(n = field(type_integer(min = 2L), default = 10L))
        ),
        Bootstrap = variant(
          fields = list(n = field(type_integer(min = 1L), default = 25L))
        ),
        Custom = variant(of = Split),
        Loocv = variant()
      )
    )
  })
}

test_that("r8_variant() names the variant of a struct, newtype, and unit variant", {
  Resampler <- resampler("variant1")
  expect_identical(r8_variant(Resampler@KFold()), "KFold")
  expect_identical(r8_variant(Resampler@Custom(train = 1L)), "Custom")
  expect_identical(r8_variant(Resampler@Loocv), "Loocv")
  # The result is the caller's: changing it leaves the value's variant.
  cv <- Resampler@KFold()
  v <- r8_variant(cv)
  v[1L] <- "Bootstrap"
  expect_identical(r8_variant(cv), "KFold")
  expect_true(r8_variant(cv) %in% c("KFold", "Loocv"))
  # A field write keeps the variant.
  cv$n <- 5L
  expect_identical(r8_variant(cv), "KFold")
  # A value read from a document, and one that crossed serialize().
  expect_identical(
    r8_variant(r8_from_json(Resampler, r8_to_json(Resampler@Loocv))),
    "Loocv"
  )
  expect_identical(r8_variant(unserialize(serialize(cv, NULL))), "KFold")
})

test_that("r8_variant() refuses a struct value and any other object", {
  Resampler <- resampler("variant2")
  Point <- declare_in(
    "variant2",
    new_struct("Point", fields = list(x = type_double()))
  )
  e <- expect_error(r8_variant(Point(x = 1)), class = "r8_type_error")
  expect_identical(
    conditionMessage(e),
    "`x` is an R8 enum value; `variant2::Point` is a struct."
  )
  e <- expect_error(r8_variant(1), class = "r8_type_error")
  expect_identical(conditionMessage(e), "`x` is an R8 enum value.")
  others <- list(
    NULL,
    "KFold",
    NA,
    list(),
    mean,
    quote(a),
    quote(stop("evaluated")),
    globalenv(),
    Resampler,
    Resampler@KFold,
    variant(),
    # An object with the attribute and without the class of a value.
    unclass(Resampler@KFold()),
    structure(1, r8_variant = "KFold"),
    # The class without the attribute, and an attribute that names no variant.
    structure(list(), class = "r8_value"),
    structure(list(), class = "r8_value", r8_variant = 1L),
    structure(list(), class = "r8_value", r8_variant = c("A", "B"))
  )
  for (x in others) {
    expect_error(r8_variant(x), class = "r8_type_error")
    expect_error(r8_is_variant(x, "KFold"), class = "r8_type_error")
  }
})

test_that("r8_is_variant() tests one variant that the enum declares", {
  Resampler <- resampler("variant3")
  cv <- Resampler@KFold()
  expect_true(r8_is_variant(cv, "KFold"))
  expect_false(r8_is_variant(cv, "Bootstrap"))
  expect_false(r8_is_variant(cv, "Custom"))
  expect_true(r8_is_variant(Resampler@Loocv, "Loocv"))
  expect_true(r8_is_variant(Resampler@Custom(train = 1L), "Custom"))
  # A name the enum does not declare, with the nearest one.
  e <- expect_error(r8_is_variant(cv, "KFol"), class = "r8_value_error")
  expect_identical(
    conditionMessage(e),
    paste0(
      "`KFol` is not a variant of `variant3::Resampler`; the nearest variant ",
      "is `KFold`. Variants: KFold, Bootstrap, Custom, Loocv."
    )
  )
  e <- expect_error(r8_is_variant(cv, ""), class = "r8_value_error")
  expect_no_match(conditionMessage(e), "nearest")
  # `variant` is one string.
  for (v in list(
    c("KFold", "Bootstrap"),
    character(),
    NA_character_,
    NA,
    1,
    NULL,
    factor("KFold"),
    quote(KFold),
    list("KFold")
  )) {
    expect_error(r8_is_variant(cv, v), class = "r8_type_error")
  }
  # The type object's `@` reports an unknown variant with the same text.
  e <- expect_error(Resampler@KFol, class = "r8_value_error")
  expect_match(conditionMessage(e), "the nearest variant is `KFold`")
})

test_that("r8_is_variant() follows a declaration made again, and an unregistered type", {
  Resampler <- resampler("variant4")
  cv <- Resampler@KFold()
  expect_error(r8_is_variant(cv, "Jackknife"), class = "r8_value_error")
  declare_in(
    "variant4",
    new_enum(
      "Resampler",
      variants = list(KFold = variant(), Jackknife = variant())
    )
  )
  expect_false(r8_is_variant(cv, "Jackknife"))
  expect_error(r8_is_variant(cv, "Loocv"), class = "r8_value_error")
  # A value whose type no declaration of the session names.
  x <- structure(
    list(),
    class = c("nowhere::Shape", "r8_value"),
    r8_type = "nowhere::Shape",
    r8_version = 1L,
    r8_variant = "Circle"
  )
  expect_identical(r8_variant(x), "Circle")
  expect_true(r8_is_variant(x, "Circle"))
  expect_false(r8_is_variant(x, "Square"))
  expect_error(r8_is_variant(x, 1), class = "r8_type_error")
})
