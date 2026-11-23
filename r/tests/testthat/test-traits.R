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
        label = trait_method(
          function(shape) NULL,
          default_impl = function(shape) {
            "a shape"
          }
        )
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
      methods = list(summary = trait_method(function(object, ...) NULL))
    )
    Point <- new_struct(
      "Point",
      fields = list(x = type_double(), y = type_double())
    )
    impl(
      Show,
      Point,
      methods = list(
        summary = function(object, ...) sprintf("(%g, %g)", object$x, object$y)
      )
    )
    Point
  })
  expect_identical(summary(T(x = 1, y = 2)), "(1, 2)")
})

test_that("the methods of R8 and its standard traits are reserved", {
  for (m in c("print", "format", "str", "$", "as.list")) {
    expect_error(
      declare_in(
        "tr",
        new_trait(
          "Reserved",
          methods = stats::setNames(
            list(trait_method(function(x, ...) NULL)),
            m
          )
        )
      ),
      "reserved for R8",
      class = "r8_declaration_error"
    )
  }
})

test_that("two traits of one namespace declare no method of one name", {
  declare_in(
    "dupgen",
    new_trait("Fit", methods = list(fit = trait_method(function(x) NULL)))
  )
  expect_error(
    declare_in(
      "dupgen",
      new_trait("Train", methods = list(fit = trait_method(function(x) NULL)))
    ),
    "duplicate_generic",
    class = "r8_declaration_error"
  )
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
        "Summ",
        methods = list(summary = trait_method(function(self) NULL))
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
        methods = list(again = trait_method(function(x) NULL))
      ),
      Ty = new_struct("Twice", fields = list(a = type_double()))
    )
  )
  declare_in(
    "tr",
    impl(parts[["Tr"]], parts[["Ty"]], methods = list(again = function(x) 1))
  )
  declare_in(
    "tr",
    impl(parts[["Tr"]], parts[["Ty"]], methods = list(again = function(x) 2))
  )
  again <- parts[["Tr"]]@again
  expect_identical(again(parts[["Ty"]](a = 0)), 2)
})

test_that("dispatch follows impls changed after a call, and serves defaults and other classes", {
  out <- declare_in("trc", {
    Size <- new_trait(
      "Size",
      methods = list(
        size = trait_method(function(x) NULL, default_impl = function(x) {
          "default"
        })
      )
    )
    Box <- new_struct("Box", fields = list(w = type_double()))
    impl(Size, Box, methods = list(size = function(x) "first"))
    list(Size = Size, Box = Box)
  })
  size <- out[["Size"]]@size
  b <- out[["Box"]](w = 1)
  expect_identical(size(b), "first")
  expect_identical(size(b), "first")
  declare_in(
    "trc",
    impl(
      out[["Size"]],
      out[["Box"]],
      methods = list(size = function(x) "second")
    )
  )
  expect_identical(size(b), "second")
  # An object of another class takes the trait's default implementation, through
  # the cache by its class, on every call.
  other <- structure(list(), class = "trc_other")
  expect_identical(size(other), "default")
  expect_identical(size(other), "default")
  expect_identical(size(1), "default")
  # The generic's body holds constants only.
  bound <- all.names(body(size))
  expect_true(all(c(".r8_c_find", ".r8_dispatch") %in% bound))
  expect_false(any(vapply(as.list(body(size)), is.environment, logical(1))))
})

test_that("a trait's required fields are checked against each implementor", {
  declare_in("req1", {
    Measured <- new_trait(
      "Measured",
      methods = list(span = trait_method(function(x) NULL)),
      required_fields = list(
        lo = type_double(),
        hi = field(option(type_double()), description = "The upper end.")
      )
    )
    Range <- new_struct(
      "Range",
      fields = list(lo = type_double(), hi = type_double())
    )
    impl(Measured, Range, methods = list(span = function(x) x$hi - x$lo))
    Window <- new_struct(
      "Window",
      fields = list(label = type_character()),
      extends = Range
    )
    Report <- new_struct("Report", fields = list(m = dyn(Measured)))
    r <- Report(m = Window(lo = 1, hi = 3, label = "w"))
    expect_identical(Measured@span(r$m), 2)
    Point <- new_struct("Point", fields = list(x = type_double()))
    expect_error(
      impl(Measured, Point, methods = list(span = function(x) 0)),
      "required_field_missing",
      class = "r8_declaration_error"
    )
    Ints <- new_struct(
      "Ints",
      fields = list(lo = type_integer(), hi = type_double())
    )
    expect_error(
      impl(Measured, Ints, methods = list(span = function(x) 0)),
      "required_field_type",
      class = "r8_declaration_error"
    )
    expect_error(
      new_trait(
        "Bad",
        methods = list(f = trait_method(function(x) NULL)),
        required_fields = list(a = field(type_double(), default = 1))
      ),
      "required_field_form",
      class = "r8_declaration_error"
    )
    # A redeclared struct keeps the required fields of its impls.
    expect_error(
      new_struct("Range", fields = list(hi = type_double())),
      "required_field_missing",
      class = "r8_declaration_error"
    )
  })
})
