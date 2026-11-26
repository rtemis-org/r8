# test-116-idioms.R
# ::R8::
# 2026- EDG rtemis.org

# R's own functions on values and type objects: as.list(), `[`, inherits(),
# lapply(), and others give the fields, several fields, and a value's type.
# spec: R8/class-system#values-in-r

idiom_types <- function(ns) {
  declare_in(ns, {
    Interval <- new_struct(
      "Interval",
      fields = list(lo = type_double(), hi = type_double()),
      rules = list(rule(lo <= hi))
    )
    Window <- new_struct(
      "Window",
      fields = list(label = option(type_character())),
      extends = Interval
    )
    Holder <- new_struct("Holder", fields = list(interval = Interval))
    Shape <- new_enum(
      "Shape",
      variants = list(
        Circle = variant(fields = list(r = type_double())),
        Empty = variant()
      )
    )
    environment()
  })
}

test_that("as.list() gives a value's fields as a plain list", {
  t <- idiom_types("idiom1")
  w <- t$Window(lo = 1, hi = 2)
  expect_identical(as.list(w), list(lo = 1, hi = 2, label = NULL))
  expect_identical(
    lapply(w, class),
    list(lo = "numeric", hi = "numeric", label = "NULL")
  )
  expect_identical(
    vapply(w, is.null, NA),
    c(lo = FALSE, hi = FALSE, label = TRUE)
  )
  expect_identical(Filter(Negate(is.null), as.list(w)), list(lo = 1, hi = 2))
  expect_identical(
    Map(function(n, v) n, names(w), as.list(w)),
    list(lo = "lo", hi = "hi", label = "label")
  )
  # A field that holds a value keeps it, and the list is the caller's.
  h <- t$Holder(interval = t$Interval(lo = 0, hi = 1))
  l <- as.list(h)
  expect_s3_class(l$interval, "idiom1::Interval")
  l$interval <- 1
  expect_s3_class(h$interval, "idiom1::Interval")
  # The fields of a variant, and none of a unit variant.
  expect_identical(as.list(t$Shape@Circle(r = 2)), list(r = 2))
  expect_identical(
    as.list(t$Shape@Empty),
    structure(list(), names = character())
  )
  expect_identical(lapply(t$Shape@Empty, class), as.list(t$Shape@Empty))
  # A value is built again from its fields.
  expect_identical(do.call(t$Window, as.list(w)), w)
})

test_that("x[names] reads fields by name", {
  t <- idiom_types("idiom2")
  w <- t$Window(lo = 1, hi = 2)
  expect_identical(w["lo"], list(lo = 1))
  expect_identical(w[c("label", "lo")], list(label = NULL, lo = 1))
  expect_identical(w[character()], structure(list(), names = character()))
  e <- expect_error(w["nope"], class = "r8_value_error")
  expect_identical(
    conditionMessage(e),
    "`nope` is not a field of `idiom2::Window`. Fields: lo, hi, label."
  )
  expect_error(w[c("lo", "nope")], "`nope`", class = "r8_value_error")
  expect_error(w[NA_character_], class = "r8_value_error")
  e <- expect_error(t$Shape@Empty["r"], class = "r8_value_error")
  expect_match(conditionMessage(e), "which has no fields", fixed = TRUE)
  expect_error(w[], class = "r8_value_error")
  expect_error(w["lo", "hi"], class = "r8_value_error")
  expect_error(w["lo", drop = TRUE], class = "r8_value_error")
  for (i in list(1, 1L, -1, TRUE, NULL, factor("lo"), list("lo"), quote(lo))) {
    expect_error(w[i], class = "r8_value_error")
  }
  expect_error(head(w, 1), class = "r8_value_error")
  # R's other reads of a value are as they were.
  expect_identical(names(w), c("lo", "hi", "label"))
  expect_length(w, 3L)
  expect_identical(unlist(w), c(lo = 1, hi = 2))
  expect_identical(with(w, hi - lo), 1)
  expect_identical(all.equal(w, w), TRUE)
  expect_match(all.equal(w, t$Window(lo = 1, hi = 3)), "hi")
  # An S3 method of `[` for a type's class takes precedence.
  p <- declare_in(
    "idiom2",
    new_struct("Pair", fields = list(a = type_double(), b = type_double()))
  )(a = 3, b = 4)
  registerS3method(
    "[",
    "idiom2::Pair",
    function(x, i, ...) c(x$a, x$b)[i]
  )
  expect_identical(p[2], 4)
  expect_identical(p[1:2], c(3, 4))
  expect_identical(as.list(p), list(a = 3, b = 4))
})

test_that("inherits() takes a type object", {
  t <- idiom_types("idiom3")
  w <- t$Window(lo = 1, hi = 2)
  expect_true(inherits(w, t$Window))
  expect_true(inherits(w, t$Interval))
  expect_false(inherits(t$Interval(lo = 1, hi = 2), t$Window))
  expect_false(inherits(w, t$Shape))
  expect_true(inherits(t$Shape@Empty, t$Shape))
  expect_true(inherits(t$Shape@Circle(r = 1), t$Shape))
  for (x in list(1, NULL, "idiom3::Window", list(), mean, t$Window)) {
    expect_false(inherits(x, t$Window))
  }
  expect_identical(inherits(w, t$Interval, which = TRUE), 2L)
  # The qualified name, as a string, says the same.
  expect_true(inherits(w, "idiom3::Interval"))
  expect_false(inherits(unclass(w), t$Window))
})
