# test-traits.R
# ::R8::
# 2026- EDG rtemis.org

test_that("a trait method dispatches on the type of its first argument", {
  out <- declare_in("tr", {
    Area <- new_trait(
      "Area",
      methods = list(
        area = trait_method(
          function(shape, ...) NULL,
          description = "The area."
        ),
        label = trait_method(function(shape) NULL, default = function(shape) {
          "a shape"
        })
      )
    )
    Square <- new_struct("Square", fields = list(side = type_double(min = 0)))
    Circle <- new_struct("Circle", fields = list(r = type_double(min = 0)))
    impl(
      Area,
      Square,
      methods = list(
        area = function(shape, ...) shape$side^2,
        label = function(shape) "a square"
      )
    )
    impl(
      Area,
      Circle,
      methods = list(area = function(shape, ...) pi * shape$r^2)
    )
    list(Area = Area, Square = Square, Circle = Circle)
  })
  area <- out[["Area"]]@area
  label <- out[["Area"]]@label
  expect_identical(area(out[["Square"]](side = 3)), 9)
  expect_equal(area(out[["Circle"]](r = 1)), pi)
  expect_identical(label(out[["Square"]](side = 1)), "a square")
  expect_identical(label(out[["Circle"]](r = 1)), "a shape")
  expect_error(area(1), "does not implement", class = "r8_dispatch_error")
  expect_error(out[["Area"]]@volume, class = "r8_dispatch_error")
})

test_that("a method named as an existing S3 generic becomes its method", {
  T <- declare_in("tr", {
    Show <- new_trait(
      "Show",
      methods = list(format = trait_method(function(x, ...) NULL))
    )
    Point <- new_struct(
      "Point",
      fields = list(x = type_double(), y = type_double())
    )
    impl(
      Show,
      Point,
      methods = list(format = function(x, ...) sprintf("(%g, %g)", x$x, x$y))
    )
    Point
  })
  expect_identical(format(T(x = 1, y = 2)), "(1, 2)")
})

test_that("plain functions, mismatched signatures, and orphan impls are refused", {
  expect_error(
    declare_in(
      "tr",
      new_trait("Rows", methods = list(nrow = trait_method(function(x) NULL)))
    ),
    "not a generic",
    class = "r8_declaration_error"
  )
  expect_error(
    declare_in(
      "tr",
      new_trait(
        "Fmt",
        methods = list(format = trait_method(function(self) NULL))
      )
    ),
    "begin with",
    class = "r8_declaration_error"
  )
  parts <- declare_in(
    "tr",
    list(
      Tr = new_trait(
        "Orphan",
        methods = list(go = trait_method(function(x) NULL))
      ),
      Ty = new_struct("Mine", fields = list(a = type_double()))
    )
  )
  expect_error(
    declare_in(
      "elsewhere",
      impl(parts[["Tr"]], parts[["Ty"]], methods = list(go = function(x) 1))
    ),
    "orphan rule",
    class = "r8_declaration_error"
  )
  expect_error(
    declare_in("tr", impl(parts[["Tr"]], parts[["Ty"]])),
    "no default",
    class = "r8_declaration_error"
  )
})

test_that("an impl of the same trait for the same type replaces it", {
  parts <- declare_in(
    "tr",
    list(
      Tr = new_trait(
        "Again",
        methods = list(go = trait_method(function(x) NULL))
      ),
      Ty = new_struct("Twice", fields = list(a = type_double()))
    )
  )
  declare_in(
    "tr",
    impl(parts[["Tr"]], parts[["Ty"]], methods = list(go = function(x) 1))
  )
  declare_in(
    "tr",
    impl(parts[["Tr"]], parts[["Ty"]], methods = list(go = function(x) 2))
  )
  go <- parts[["Tr"]]@go
  expect_identical(go(parts[["Ty"]](a = 0)), 2)
})
