# test-standard.R
# ::R8::
# 2026- EDG rtemis.org

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
      methods = list(cmp = function(x, y) x$cents - y$cents)
    )
  )
  expect_true(a < b)
  expect_true(b >= a)
  expect_false(a > b)
  expect_identical(r8_cmp(a, b), -100L)
})

test_that("Dim reports dimensions, which nrow() and ncol() follow", {
  Grid <- declare_in(
    "demo",
    new_struct("Grid", fields = list(r = type_integer(), c = type_integer()))
  )
  declare_in(
    "demo",
    impl(Dim, Grid, methods = list(dim = function(x) c(x$r, x$c)))
  )
  g <- Grid(r = 3L, c = 4L)
  expect_identical(dim(g), c(3L, 4L))
  expect_identical(nrow(g), 3L)
  expect_identical(length(g), 2L)
})

test_that("From converts through r8_convert(), Type@from(), and r8_converter()", {
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
  expect_identical(r8_convert(1.5, Money)$cents, 150L)
  expect_identical(r8_convert(2L, Money)$cents, 200L)
  expect_identical(Money@from("75c")$cents, 75L)
  as_money <- r8_converter(Money)
  expect_identical(as_money(0.1)$cents, 10L)
  m <- Money(cents = 5L)
  expect_identical(r8_convert(m, Money), m)
  expect_error(
    r8_convert(TRUE, Money),
    "no From impl",
    class = "r8_dispatch_error"
  )
  Count <- declare_in(
    "demo",
    new_struct("Count", fields = list(n = type_integer()))
  )
  expect_error(
    r8_convert(5, Count),
    'no From impl for "numeric"\\.$',
    class = "r8_dispatch_error"
  )
  expect_error(Money@nothing, class = "r8_dispatch_error")
  expect_match(r8_declaration(Money), "MoneyS")
})
