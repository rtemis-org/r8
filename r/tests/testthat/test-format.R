# test-format.R
# ::R8::
# 2026- EDG rtemis.org

# The text of declaration objects and type objects.
# spec: R8/class-system#notation-of-a-type

lines <- function(x) {
  out <- utils::capture.output(shown <- withVisible(print(x)))
  # print() writes the lines of format(), and returns the object invisibly.
  expect_identical(out, format(x))
  expect_false(shown$visible)
  expect_identical(shown$value, x)
  out
}

format_types <- function(ns) {
  declare_in(ns, {
    Interval <- new_struct(
      "Interval",
      fields = list(lo = type_double(), hi = type_double()),
      rules = rule(lo <= hi, message = "lo is at most hi.", id = "ORDER")
    )
    Window <- new_struct(
      "Window",
      fields = list(label = type_character()),
      extends = Interval
    )
    Resampler <- new_enum(
      "Resampler",
      description = "How cases are split.",
      variants = list(
        KFold = variant(
          fields = list(
            n = field(type_integer(min = 2L), default = 10L),
            seed = option(type_integer(min = 0L))
          ),
          rules = list(rule(n < 100L))
        ),
        Custom = variant(of = Interval),
        Loocv = variant()
      )
    )
    Loss <- new_enum(
      "Loss",
      variants = list(Squared = variant(), Absolute = variant())
    )
    Fit <- new_trait("Fit", methods = list(fit = trait_method(params(x))))
    environment()
  })
}

test_that("a field type prints its notation", {
  t <- format_types("fmt1")
  expect_identical(
    lines(type_integer(min = 1L)),
    "<r8 field type> integer [1, Inf)"
  )
  expect_identical(
    lines(vec(type_double(min = 0, max = 1))),
    "<r8 field type> vector of double [0, 1]"
  )
  expect_identical(
    lines(option(t$Interval)),
    "<r8 field type> optional fmt1::Interval"
  )
  expect_identical(
    lines(subtype(t$Interval)),
    "<r8 field type> subtype of fmt1::Interval"
  )
  expect_identical(
    lines(dyn(t$Fit)),
    "<r8 field type> implementor of fmt1::Fit"
  )
  expect_identical(lines(host_class("lm")), "<r8 host class \"lm\", S3>")
})

test_that("a field and a column print their type, default, and facts", {
  t <- format_types("fmt2")
  expect_identical(
    lines(field(
      type_integer(min = 1L),
      default = 10L,
      description = "Number of folds."
    )),
    c("<r8 field> integer [1, Inf) = 10L", "  Number of folds.")
  )
  expect_identical(
    lines(field(
      option(type_double()),
      compute = a / 2,
      unit = "g",
      sensitive = TRUE
    )),
    c(
      "<r8 field> optional double",
      "  unit: g",
      "  sensitive",
      "  computed: a/2"
    )
  )
  expect_identical(
    lines(field(option(type_double()), context = TRUE)),
    c("<r8 field> optional double", "  filled from the context")
  )
  expect_identical(
    lines(field(t$Resampler, default = t$Resampler@KFold())),
    "<r8 field> fmt2::Resampler = fmt2::Resampler@KFold(...)"
  )
  expect_identical(
    lines(field(t$Loss, default = t$Loss@Squared)),
    "<r8 field> fmt2::Loss = fmt2::Loss@Squared"
  )
  expect_identical(
    lines(column(type_character(), unique = TRUE, required = FALSE)),
    c("<r8 column> character", "  unique", "  may be absent")
  )
  long <- lines(field(type_character(), default = strrep("x", 90)))
  expect_match(long, "x\\.\\.\\.$")
  expect_lte(nchar(long), 100L)
})

test_that("a rule and a variant print as R writes them", {
  t <- format_types("fmt3")
  expect_identical(
    lines(rule(lo <= hi, message = "lo is at most hi.", id = "ORDER")),
    c("<r8 rule ORDER> lo <= hi", "  lo is at most hi.")
  )
  expect_identical(
    lines(rule(n > 0, when = is_variant(kind, "A"))),
    "<r8 rule> n > 0, when is_variant(kind, \"A\")"
  )
  expect_identical(lines(variant()), "<r8 variant> unit")
  expect_identical(
    lines(variant(of = t$Interval)),
    "<r8 variant> of fmt3::Interval"
  )
  expect_identical(
    lines(variant(
      fields = list(
        a = t$Interval,
        b = field(type_integer(), default = 3L),
        c = option(type_double())
      ),
      rules = rule(b > 1L),
      description = "A variant."
    )),
    c(
      "<r8 variant>",
      "  A variant.",
      "  a: fmt3::Interval",
      "  b: integer = 3L",
      "  c: optional double",
      "  rule: b > 1L"
    )
  )
})

test_that("parameters and methods print their types, defaults, and results", {
  t <- format_types("fmt4")
  p <- params(
    x,
    n = arg_integer(min = 1L, default = 3L),
    k = length(x),
    z = arg(t$Interval),
    "..."
  )
  body <- c(
    "  x",
    "  n: integer [1, Inf) = 3L",
    "  k = length(x)",
    "  z: fmt4::Interval",
    "  ..."
  )
  expect_identical(lines(p), c("<r8 params>", body))
  expect_identical(
    lines(trait_method(
      p,
      description = "Draws.",
      return_type = type_double(),
      default_impl = function(x, n, k, z, ...) 1
    )),
    c(
      "<r8 trait method>",
      "  Draws.",
      body,
      "  returns double",
      "  with a default implementation"
    )
  )
  expect_identical(
    lines(trait_method(params(x, value), return_type = "Self")),
    c("<r8 trait method>", "  x", "  value", "  returns Self")
  )
  expect_identical(
    lines(impl_method(function(x, n) 1, return_type = t$Interval)),
    c("<r8 impl method> function(x, n)", "  returns fmt4::Interval")
  )
})

test_that("a type object prints its fields, defaults, and rules", {
  t <- format_types("fmt5")
  expect_identical(
    lines(t$Window),
    c(
      "<r8 struct fmt5::Window v1 extends fmt5::Interval>",
      "  lo: double",
      "  hi: double",
      "  label: character",
      "  rule ORDER: lo <= hi"
    )
  )
  expect_identical(
    lines(t$Resampler),
    c(
      "<r8 enum fmt5::Resampler v1>",
      "  How cases are split.",
      "  KFold",
      "    n: integer [2, Inf) = 10L",
      "    seed: optional integer [0, Inf)",
      "    rule: n < 100L",
      "  Custom: fmt5::Interval",
      "  Loocv"
    )
  )
  Every <- declare_in(
    "fmt5",
    new_struct(
      "Every",
      fields = list(
        a = field(vec(type_double()), default = c(0.1, 0.2)),
        b = field(type_date(), default = as.Date("2026-01-02")),
        c = field(t$Resampler, default = t$Resampler@Loocv),
        d = field(t$Resampler, default = t$Resampler@KFold(n = 3L)),
        e = field(t$Loss, default = t$Loss@Absolute),
        f = field(t$Interval, default = t$Interval(lo = 0, hi = 1)),
        g = field(btree_map(type_integer()), default = c(k = 1L)),
        h = field(type_double(finite = FALSE), default = -Inf),
        i = field(option(type_logical()), default = TRUE)
      )
    )
  )
  expect_identical(
    sub("^.* = ", "", lines(Every)[-1L]),
    c(
      "c(0.1, 0.2)",
      "as.Date(\"2026-01-02\")",
      "fmt5::Resampler@Loocv",
      "fmt5::Resampler@KFold(...)",
      "fmt5::Loss@Absolute",
      "fmt5::Interval(...)",
      "{\"k\":1}",
      "-Inf",
      "TRUE"
    )
  )
})
