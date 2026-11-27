# test-129-standard.R
# ::R8::
# 2026- EDG rtemis.org

# The standard traits: the text of a value (Display, Debug, and Print),
# order (PartialOrd), dimensions (Shape), and conversion (From).

money <- function() {
  declare_in(
    "demo",
    new_struct("MoneyS", fields = list(cents = type_integer()))
  )
}

test_that("Display and Debug give text, with defaults for every value", {
  Money <- money()
  m <- Money(cents = 150L)
  expect_identical(format(m)[1L], "<demo::MoneyS>")
  expect_output(print(m), "cents: 150")
  declare_in(
    "demo",
    impl(
      Display,
      Money,
      methods = list(format = function(x, ...) sprintf("$%.2f", x$cents / 100))
    )
  )
  expect_identical(format(m), "$1.50")
  expect_output(print(m), "\\$1.50")
  expect_output(str(m), "cents")
})

test_that("PartialOrd orders values; without it, ordering is an error", {
  Money <- money()
  a <- Money(cents = 100L)
  b <- Money(cents = 200L)
  expect_error(a < b, "does not implement", class = "r8_dispatch_error")
  declare_in(
    "demo",
    impl(
      PartialOrd,
      Money,
      methods = list(partial_cmp = function(x, y) sign(x$cents - y$cents))
    )
  )
  expect_true(a < b)
  expect_true(b >= a)
  expect_false(a > b)
  expect_identical(r8partial_cmp(a, b), -1)
})

test_that("PartialOrd's result is -1L, 0L, 1L, or NA, and NA orders nothing", {
  declare_in("po2", {
    Score <- new_struct("Score", fields = list(v = type_double(nan = TRUE)))
    impl(
      PartialOrd,
      Score,
      methods = list(partial_cmp = function(x, y) {
        if (is.nan(x$v) || is.nan(y$v)) {
          NA_integer_
        } else {
          as.integer(sign(x$v - y$v))
        }
      })
    )
    a <- Score(v = 1)
    n <- Score(v = NaN)
    expect_true(a < Score(v = 2))
    expect_false(a < n)
    expect_false(a >= n)
    expect_false(n == a)
    Bad <- new_struct("Bad", fields = list(v = type_double()))
    impl(PartialOrd, Bad, methods = list(partial_cmp = function(x, y) "a"))
    expect_error(
      Bad(v = 1) < Bad(v = 2),
      "return_type",
      class = "r8_result_error"
    )
    Far <- new_struct("Far", fields = list(v = type_double()))
    impl(
      PartialOrd,
      Far,
      methods = list(partial_cmp = function(x, y) x$v - y$v)
    )
    expect_error(Far(v = 1) < Far(v = 5), "found", class = "r8_result_error")
  })
})

test_that("Shape reports dimensions, which nrow() and ncol() follow", {
  Grid <- declare_in(
    "demo",
    new_struct("Grid", fields = list(r = type_integer(), c = type_integer()))
  )
  declare_in(
    "demo",
    impl(Shape, Grid, methods = list(dim = function(x) c(x$r, x$c)))
  )
  g <- Grid(r = 3L, c = 4L)
  expect_identical(dim(g), c(3L, 4L))
  expect_identical(nrow(g), 3L)
  expect_identical(length(g), 2L)
})

test_that("From converts through r8convert(), Type@from(), and r8converter()", {
  Money <- money()
  declare_in(
    "demo",
    impl(
      From("numeric"),
      Money,
      methods = list(from = function(x) {
        Money(cents = as.integer(round(x * 100)))
      })
    )
  )
  declare_in(
    "demo",
    impl(
      From("character"),
      Money,
      methods = list(from = function(x) {
        Money(cents = as.integer(sub("c$", "", x)))
      })
    )
  )
  expect_identical(r8convert(1.5, Money)$cents, 150L)
  expect_identical(r8convert(2L, Money)$cents, 200L)
  expect_identical(Money@from("75c")$cents, 75L)
  as_money <- r8converter(Money)
  expect_identical(as_money(0.1)$cents, 10L)
  m <- Money(cents = 5L)
  expect_identical(r8convert(m, Money), m)
  expect_error(
    r8convert(TRUE, Money),
    "no From impl",
    class = "r8_dispatch_error"
  )
  Count <- declare_in(
    "demo",
    new_struct("Count", fields = list(n = type_integer()))
  )
  expect_error(
    r8convert(5, Count),
    'no From impl for "double", "numeric"\\.$',
    class = "r8_dispatch_error"
  )
  expect_error(Money@nothing, class = "r8_dispatch_error")
  expect_match(r8declaration(Money), "MoneyS")
})

test_that("a converter is the generic of the conversions to its target", {
  declare_in("cv1", {
    Range <- new_struct(
      "Range",
      fields = list(lo = type_double(), hi = type_double())
    )
    Target <- new_struct("Target", fields = list(width = type_double()))
    as_target <- r8converter(Target)
    expect_s3_class(as_target, "r8_converter")
    implement(as_target, Range) <- function(x) Target(width = x$hi - x$lo)
    implement(as_target, "character") <- function(x) Target(width = nchar(x))
    expect_identical(as_target(Range(lo = 1, hi = 4))$width, 3)
    expect_identical(r8convert("abcd", Target)$width, 4)
    expect_identical(Target@from("ab")$width, 2)
    expect_output(print(as_target), "<r8 converter cv1::Target>")
    expect_output(print(as_target), "Methods for: character, cv1::Range")
    expect_identical(
      body(r8method(as_target, "character")),
      quote(Target(width = nchar(x)))
    )
    expect_error(
      implement(as_target, "integer") <- function(x, y) NULL,
      "r8converter\\(Target\\).*function\\(x\\)",
      class = "r8_declaration_error"
    )
    expect_error(
      implement(as_target, 1) <- function(x) NULL,
      "an R8 type object, a class name",
      class = "r8_declaration_error"
    )
    implement(as_target, "numeric") <- function(x) Range(lo = 0, hi = x)
    expect_error(
      as_target(1),
      "returned a value of `cv1::Range`",
      class = "r8_type_error"
    )
  })
})

test_that("a From impl of an ancestor converts a descendant, which it receives unchanged", {
  declare_in("cv2", {
    Range <- new_struct(
      "Range",
      fields = list(lo = type_double(), hi = type_double())
    )
    Window <- new_struct(
      "Window",
      fields = list(label = type_character()),
      extends = Range
    )
    Frame <- new_struct(
      "Frame",
      fields = list(depth = type_double()),
      extends = Window
    )
    Target <- new_struct("Target", fields = list(what = type_character()))
    as_target <- r8converter(Target)
    implement(as_target, Range) <- function(x) Target(what = class(x)[1L])
    f <- Frame(lo = 1, hi = 2, label = "f", depth = 3)
    expect_identical(as_target(f)$what, "cv2::Frame")
    implement(as_target, Window) <- function(x) Target(what = "window impl")
    expect_identical(as_target(f)$what, "window impl")
    expect_identical(as_target(Range(lo = 1, hi = 2))$what, "cv2::Range")
    impl(
      From("character"),
      Range,
      methods = list(from = function(x) Range(lo = 0, hi = 1))
    )
    expect_error(
      r8convert("a", Window),
      "no From impl",
      class = "r8_dispatch_error"
    )
    implement(as_target, Frame) <- function(x) r8next_impl()
    expect_error(
      as_target(f),
      "r8convert\\(r8upcast\\(x, Parent\\), Target\\)",
      class = "r8_dispatch_error"
    )
    Holder <- new_struct("Holder", fields = list(r = Range))
    expect_error(
      Holder(r = f),
      "descendant `cv2::Frame`; r8upcast\\(\\)",
      class = "r8_value_error"
    )
  })
})

test_that("a From impl names its source by type object and follows the orphan rule with the source", {
  Target <- declare_in(
    "cv3",
    new_struct("Target", fields = list(n = type_double()))
  )
  expect_error(
    declare_in(
      "cv3",
      impl(From("cv3::Rnage"), Target, methods = list(from = function(x) x))
    ),
    "by its type object",
    class = "r8_declaration_error"
  )
  Local <- declare_in(
    "cv4",
    new_struct("Local", fields = list(n = type_double()))
  )
  expect_no_error(declare_in(
    "cv4",
    impl(
      From(Local),
      Target,
      methods = list(from = function(x) Target(n = x$n))
    )
  ))
  expect_identical(r8convert(Local(n = 2), Target)$n, 2)
  expect_error(
    declare_in(
      "cv5",
      impl(
        From("character"),
        Target,
        methods = list(from = function(x) Target(n = 1))
      )
    ),
    "orphan rule",
    class = "r8_declaration_error"
  )
})

test_that("Enum@from converts, and from is a reserved variant name", {
  declare_in("cv6", {
    Kind <- new_enum("Kind", variants = list(A = variant(), B = variant()))
    as_kind <- r8converter(Kind)
    implement(as_kind, "character") <- function(x) {
      if (x == "a") Kind@A else Kind@B
    }
    expect_identical(R8:::value_variant(Kind@from("a")), "A")
    expect_identical(R8:::value_variant(as_kind("b")), "B")
    expect_error(
      new_enum("K2", variants = list(from = variant())),
      "variant name `from` is reserved",
      class = "r8_declaration_error"
    )
  })
})
