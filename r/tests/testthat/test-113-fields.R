# test-113-fields.R
# ::R8::
# 2026- EDG rtemis.org

# Several fields replaced at once, x[names] <- list(...), with one check of
# the rules between them.
#
# spec: R8/class-system#values-in-r

field_types <- function(ns) {
  declare_in(ns, {
    Interval <- new_struct(
      "Interval",
      fields = list(
        lo = type_double(),
        hi = type_double(),
        unit = option(type_character()),
        n = field(type_integer(min = 1L), default = 3L)
      ),
      rules = list(
        rule(lo <= hi, message = "lo is at most hi.", id = "ORDER"),
        rule(n < 100L, id = "SIZE")
      )
    )
    Window <- new_struct(
      "Window",
      fields = list(label = type_character()),
      extends = Interval,
      rules = list(rule(label != "", id = "LABEL"))
    )
    Same <- new_struct(
      "Same",
      fields = list(n = type_integer(), m = type_integer()),
      rules = list(rule(n == m))
    )
    Shape <- new_enum(
      "Shape",
      variants = list(
        Box = variant(
          fields = list(w = type_double(), h = type_double()),
          rules = list(rule(w >= h))
        ),
        Pair = variant(of = Same),
        Empty = variant()
      )
    )
    environment()
  })
}

test_that("x[names] <- list() replaces fields and runs their rules once, after all", {
  t <- field_types("fields1")
  x <- t$Interval(lo = 1, hi = 2)
  y <- x
  y[c("lo", "hi")] <- list(5, 9)
  expect_identical(y, t$Interval(lo = 5, hi = 9))
  # The value it was read from is unchanged.
  expect_identical(x$lo, 1)
  # The order of the names is the author's, and each is replaced.
  y[c("hi", "lo")] <- list(2, 1)
  expect_identical(y, x)
  # A rule that no order of single writes passes.
  s <- t$Same(n = 1L, m = 1L)
  expect_error(s$n <- 2L, class = "r8_value_error")
  s[c("n", "m")] <- list(2L, 2L)
  expect_identical(s, t$Same(n = 2L, m = 2L))
  # A list with the fields' names, as from tuning.
  best <- list(lo = 0, n = 7L)
  y[names(best)] <- best
  expect_identical(y[names(best)], best)
  # base R's replace() is the form in an expression.
  expect_identical(
    replace(x, c("lo", "hi"), list(5, 9)),
    t$Interval(lo = 5, hi = 9)
  )
  expect_identical(`[<-`(x, "lo", list(0))$lo, 0)
  # None.
  y <- x
  y[character()] <- list()
  expect_identical(y, x)
})

test_that("new values convert, take defaults, and may be absent, as in a single write", {
  t <- field_types("fields2")
  x <- t$Interval(lo = 1, hi = 2, unit = "cm", n = 9L)
  # A whole double for an integer, and an integer for a double.
  x[c("n", "hi")] <- list(4, 8L)
  expect_identical(x$n, 4L)
  expect_identical(x$hi, 8)
  # NULL is the default, or absent.
  x[c("n", "unit")] <- list(NULL, NULL)
  expect_identical(x$n, 3L)
  expect_null(x$unit)
  e <- expect_error(
    x[c("lo", "hi")] <- list(NULL, NULL),
    class = "r8_value_error"
  )
  expect_match(conditionMessage(e), "2 problems", fixed = TRUE)
  expect_match(conditionMessage(e), "`lo`")
  expect_match(conditionMessage(e), "`hi`")
})

test_that("every problem is in one error that names the assignment", {
  t <- field_types("fields3")
  x <- t$Window(lo = 1, hi = 2, label = "w")
  e <- expect_error(
    x[c("lo", "n", "label")] <- list("a", 0L, 1),
    class = "r8_value_error"
  )
  expect_match(
    conditionMessage(e),
    "Invalid `fields3::Window` from `x[c(\"lo\", \"n\", \"label\")] <- value`: 3 problems",
    fixed = TRUE
  )
  expect_setequal(e$problems$code, c("r_type", "below_min", "r_type"))
  # The rules of the parent and of the child that read a replaced field.
  e <- expect_error(
    x[c("lo", "label")] <- list(5, ""),
    class = "r8_value_error"
  )
  expect_setequal(e$problems$rule, c("ORDER", "LABEL"))
  # A rule that reads no replaced field does not run; a type problem comes
  # before the rules.
  e <- expect_error(
    x[c("lo", "n")] <- list(5, "a"),
    class = "r8_value_error"
  )
  expect_identical(e$problems$code, "r_type")
  e <- expect_error(x["lo"] <- list(5), class = "r8_value_error")
  expect_match(conditionMessage(e), "from `x[\"lo\"] <- value`", fixed = TRUE)
  expect_identical(x, t$Window(lo = 1, hi = 2, label = "w"))
  # A long assignment is cut.
  e <- expect_error(
    x[c("lo", "hi", "unit", "n", "label")] <- list(1, 0, "a", 1L, "b"),
    class = "r8_value_error"
  )
  expect_match(
    conditionMessage(e),
    "from `x[c(\"lo\", \"hi\", \"unit\", ...)] <- value`",
    fixed = TRUE
  )
})

test_that("a child keeps its type, and an enum value its variant", {
  t <- field_types("fields4")
  w <- t$Window(lo = 1, hi = 2, label = "w")
  w[c("lo", "hi")] <- list(5, 9)
  expect_identical(w, t$Window(lo = 5, hi = 9, label = "w"))
  b <- t$Shape@Box(w = 2, h = 1)
  b[c("w", "h")] <- list(1, 0.5)
  expect_identical(b, t$Shape@Box(w = 1, h = 0.5))
  expect_error(b[c("w", "h")] <- list(1, 2), class = "r8_value_error")
  p <- t$Shape@Pair(n = 1L, m = 1L)
  p[c("n", "m")] <- list(3L, 3L)
  expect_identical(p, t$Shape@Pair(n = 3L, m = 3L))
  u <- t$Shape@Empty
  u[character()] <- list()
  expect_identical(u, t$Shape@Empty)
  expect_error(u["w"] <- list(1), "no fields", class = "r8_value_error")
})

test_that("an assignment states its fields by name and their values in a list", {
  t <- field_types("fields5")
  x <- t$Interval(lo = 1, hi = 2)
  refused <- function(expr, message) {
    expect_error(expr, message, fixed = TRUE, class = "r8_value_error")
  }
  refused(x["nope"] <- list(1), "`nope` is not a field of `fields5::Interval`")
  refused(x[c("lo", NA)] <- list(1, 2), "is not a field")
  refused(x[c("lo", "lo")] <- list(1, 2), "The field `lo` is replaced twice.")
  refused(x[1] <- list(1), "Fields are replaced by name")
  refused(x[TRUE] <- list(1), "Fields are replaced by name")
  refused(x[] <- list(1), "Fields are replaced by name")
  expect_error(x["lo", "hi"] <- list(1), "unused argument")
  refused(x["lo"] <- 1, "takes a plain list")
  refused(x[c("lo", "hi")] <- c(1, 2), "takes a plain list")
  refused(x["lo"] <- NULL, "takes a plain list")
  refused(x[c("lo", "hi")] <- x, "takes a plain list")
  refused(x[c("lo", "hi")] <- data.frame(a = 1, b = 2), "takes a plain list")
  refused(x[c("lo", "hi")] <- list(1), "found 2 fields and 1 value")
  refused(x["lo"] <- list(1, 2), "found 1 field and 2 values")
  refused(x[c("lo", "hi")] <- list(hi = 1, lo = 2), "found hi, lo")
  refused(x[c("lo", "hi")] <- list(lo = 1, 2), "in their order: lo, hi")
  expect_identical(x, t$Interval(lo = 1, hi = 2))
  # A symbol or a call as a new value is refused as data, not evaluated.
  expect_error(
    x[c("lo", "hi")] <- list(quote(stop("evaluated")), 2),
    class = "r8_value_error"
  )
})

test_that("a field asked for by position is an error that gives the call by name", {
  P <- declare_in("pos", {
    new_struct(
      "P",
      fields = list(
        id = type_integer(),
        age = type_double(),
        s = type_character()
      )
    )
  })
  r <- P(id = 1L, age = 40, s = "a")
  says <- function(expr, text) {
    expect_error(expr, text, fixed = TRUE, class = "r8_value_error")
  }
  says(r[[1]], "field 1 is `id`: r[[\"id\"]]; names(r) gives the field names.")
  says(r[[3L]], "field 3 is `s`: r[[\"s\"]]")
  says(list(r)[[1]][[2]], "field 2 is `age`: x[[\"age\"]]")
  says(r[2], "field 2 is `age`: r[\"age\"];")
  says(r[c(1, 3)], "fields 1, 3 are `id`, `s`: r[c(\"id\", \"s\")];")
  says(r[[4]], "`r` has 3 fields: `id`, `age`, `s`.")
  says(r[c(1, 9)], "`r` has 3 fields")
  says(r[[2]] <- 1, "field 2 is `age`: x[[\"age\"]] <- value.")
  for (i in list(-1L, 0, 1.5, NA, TRUE, NULL, list("id"), c("id", "age"))) {
    says(r[[i]], "A field is read by name: r[[\"id\"]].")
  }
  says(r[-1], "Fields are read by name: r[c(\"id\", \"age\")].")
  says(r[1:2, ], "Fields are read by name: r[c(\"id\", \"age\")].")
  says(r[], "Fields are read by name")
  # What goes through a list by position goes through as.list().
  says(Map(identity, r), "names(x) gives the field names")
  expect_identical(Map(identity, as.list(r)), as.list(r))
})
