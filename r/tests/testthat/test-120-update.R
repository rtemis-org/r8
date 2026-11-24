# test-120-update.R
# ::R8::
# 2026- EDG rtemis.org

# Values of another declaration of their type than the registered one: what
# reads them, what refuses them, and what r8_update() does with them. The
# scenarios tell the same of users' work.
#
# spec: R8/class-system#values-of-another-declaration

other <- "r8_other_declaration_error"

test_that("a declaration whose differences have no part in a value is the same", {
  struct <- function(a) {
    declare_in("up1", new_struct("S", fields = list(a = a)))
  }
  x <- struct(type_double())(a = 1)
  for (f in list(
    field(type_double(), default = 2),
    field(type_double(), description = "Other."),
    field(type_double(), sensitive = TRUE),
    field(type_double(), extensions = list("up1:note" = 1))
  )) {
    S <- struct(f)
    expect_identical(r8_validate(x), x)
    expect_identical(r8_update(x), x)
    y <- x
    y$a <- 3
    expect_identical(y$a, 3)
  }
})

test_that("a value of another declaration is read, shown, and not used", {
  S <- declare_in("up2", {
    S <- new_struct("S", fields = list(a = type_double(), b = type_character()))
    implement(format, S) <- function(x, ...) paste("the text of", x$b)
    S
  })
  x <- S(a = 1, b = "b")
  expect_identical(format(x), "the text of b")
  declare_in("up2", {
    new_struct(
      "S",
      fields = list(a = field(type_double(), unit = "kg"), b = type_character())
    )
  })
  # What looks.
  expect_identical(x$a, 1)
  expect_identical(x[["b"]], "b")
  expect_identical(x["a"], list(a = 1))
  expect_identical(as.list(x), list(a = 1, b = "b"))
  expect_identical(names(x), c("a", "b"))
  expect_identical(lapply(x, identity), list(a = 1, b = "b"))
  expect_identical(attributes(x)$class, c("up2::S", "r8_value"))
  shown <- format(x)
  expect_match(shown, "a: 1", all = FALSE)
  expect_match(shown, "`a` was without a unit, and is in `kg`", all = FALSE)
  expect_output(print(x), "Made under another declaration of `up2::S`")
  expect_output(str(x), "a: 1")
  # What uses.
  expect_error(r8_validate(x), "declaration_changed", class = "r8_value_error")
  expect_error(r8_hash(x), "declaration_changed", class = "r8_value_error")
  expect_error(r8_to_json(x), "declaration_changed", class = "r8_value_error")
  expect_error(r8_resolve(x), "declaration_changed", class = other)
  expect_error(r8_equal(x, x), "declaration_changed", class = other)
  expect_error(x == x, "declaration_changed", class = other)
  expect_error(x$a <- 2, "declaration_changed", class = "r8_value_error")
  expect_error(
    x["a"] <- list(2),
    "declaration_changed",
    class = "r8_value_error"
  )
  expect_error(r8_update(x), "may mean something else", class = other)
})

test_that("a function that takes or returns a type refuses a value of another declaration", {
  out <- declare_in("up3", {
    A <- new_struct("A", fields = list(a = type_double()))
    B <- new_struct("B", fields = list(b = type_double()), extends = A)
    one <- generic("one", params(x))
    implement(one, A) <- function(x) x$a
    two <- generic("two", params(x, y), dispatch = c("x", "y"))
    implement(two, A, A) <- function(x, y) x$a + y$a
    list(A = A, B = B, one = one, two = two)
  })
  a <- out$A(a = 1)
  b <- out$B(a = 1, b = 2)
  expect_identical(c(out$one(a), out$one(b), out$two(a, b)), c(1, 1, 2))
  declare_in("up3", {
    B <- new_struct(
      "B",
      fields = list(b = field(type_double(), unit = "m")),
      extends = out$A
    )
  })
  expect_identical(out$one(a), 1)
  expect_error(out$one(b), "`b` was without a unit", class = other)
  expect_error(out$two(a, b), "`b` was without a unit", class = other)
  expect_error(r8_upcast(b, out$A), "`b` was without a unit", class = other)
  # An index that held the method of the value's type is not used.
  expect_error(out$one(b), class = other)
})

test_that("r8_update() converts the values that a value holds", {
  types <- function(unit = NULL, ...) {
    fields <- list(x = field(type_double(), unit = unit), ...)
    declare_in("up4", {
      P <- new_struct("P", fields = fields)
      K <- new_enum(
        "K",
        variants = list(
          One = variant(fields = list(p = P)),
          Nothing = variant()
        )
      )
      H <- new_struct(
        "H",
        fields = list(
          p = P,
          ps = vec(P),
          map = index_map(P),
          k = K,
          maybe = option(P),
          when = type_date(),
          m = type_matrix(type_double())
        )
      )
      list(P = P, K = K, H = H)
    })
  }
  t <- types()
  p <- function(x) t$P(x = x)
  h <- t$H(
    p = p(1),
    ps = list(p(2), p(3)),
    map = list(a = p(4)),
    k = t$K@One(p = p(5)),
    maybe = NULL,
    when = as.Date("2026-01-01"),
    m = matrix(c(1, 2, 3, 4), 2)
  )
  before <- as.list(h)
  t <- types(note = option(type_character()))
  expect_error(
    r8_validate(h),
    "`note` was added as an optional field (in `up4::P`)",
    fixed = TRUE
  )
  u <- r8_update(h)
  expect_identical(u$p, t$P(x = 1, note = NULL))
  expect_identical(u$ps, list(t$P(x = 2), t$P(x = 3)))
  expect_identical(u$map$a, t$P(x = 4))
  expect_identical(u$k, t$K@One(p = t$P(x = 5)))
  expect_identical(u$when, before$when)
  expect_identical(u$m, before$m)
  expect_identical(r8_validate(u), u)
  # Lists, as of readRDS().
  l <- r8_update(list(h, n = list(1, p = before$p), "a", NULL))
  expect_identical(l[[1L]], u)
  expect_identical(l$n$p, t$P(x = 1))
  expect_null(l[[4L]])
  t <- types(unit = "mg")
  expect_error(r8_update(h), "(in `up4::P`)", fixed = TRUE, class = other)
})

test_that("r8_update() takes an R8 value or a list", {
  expect_error(r8_update(1), class = "r8_type_error")
  expect_error(
    r8_update(structure(list(), class = c("a::B", "r8_value"))),
    "is not one",
    class = "r8_type_error"
  )
  expect_identical(r8_update(list(1, "a")), list(1, "a"))
})

test_that("a value of an enum is updated unless its variant was removed", {
  kinds <- function(names) {
    variants <- lapply(names, function(v) variant())
    names(variants) <- names
    declare_in("up5", new_enum("K", variants = variants))
  }
  K <- kinds(c("A", "B", "C"))
  a <- K@A
  c <- K@C
  K <- kinds(c("A", "B", "D"))
  expect_error(
    r8_validate(a),
    "the variant `C` was removed",
    class = "r8_value_error"
  )
  expect_identical(r8_update(a), K@A)
  expect_error(r8_update(c), "its variant was removed", class = other)
})

test_that("a declaration is read back whatever its length and letters", {
  wide <- function(n, unit) {
    fields <- lapply(seq_len(n), function(i) {
      field(
        type_character(values = c("alpha", "\u03b2eta", "\U0001F600")),
        unit = unit
      )
    })
    names(fields) <- paste0("field_number_", seq_len(n))
    declare_in("up6", new_struct("Wide", fields = fields))
  }
  for (n in c(1L, 300L)) {
    Wide <- wide(n, "\u00b5g/m\u00b3")
    x <- do.call(
      Wide,
      setNames(rep(list("\u03b2eta"), n), paste0("field_number_", seq_len(n)))
    )
    x <- unserialize(serialize(x, NULL))
    expect_identical(r8_validate(x), x)
    Wide <- wide(n, "mg")
    expect_error(
      r8_validate(x),
      "`field_number_1` was in `\u00b5g/m\u00b3`, and is in `mg`",
      class = "r8_value_error"
    )
  }
  # The declaration is written once for any number of values.
  one <- length(serialize(list(x), NULL))
  expect_lt(length(serialize(rep(list(x), 100L), NULL)) / 100, one)
})

test_that("a value whose declaration this R8 does not read is shown and not used", {
  skip_without_oracle()
  declare_in("up7", new_struct("S", fields = list(a = type_double())))
  x <- oracle_any_value(list(a = 1), "up7::S", "", "sha256:later")
  expect_identical(x$a, 1)
  expect_match(format(x), "does not read", all = FALSE)
  expect_error(r8_update(x), "does not read", class = other)
  expect_error(r8_to_json(x), "does not read", class = "r8_value_error")
})

test_that("the declaration again makes its values usable again", {
  struct <- function(unit) {
    declare_in(
      "up8",
      new_struct("S", fields = list(a = field(type_double(), unit = unit)))
    )
  }
  x <- struct("kg")(a = 1)
  struct("g")
  expect_error(r8_validate(x), class = "r8_value_error")
  struct("kg")
  expect_identical(r8_validate(x), x)
})

test_that("equality meets a value of another declaration in a field that names no type", {
  out <- declare_in("up9", {
    Sized <- new_trait("Sized", methods = list(size = trait_method(params(x))))
    X <- new_struct("X", fields = list(n = type_double(min = 0)))
    impl(Sized, X, methods = list(size = function(x) x$n))
    D <- new_struct("D", fields = list(d = dyn(Sized)))
    list(X = X, D = D)
  })
  h <- out$D(d = out$X(n = 1))
  X <- declare_in("up9", {
    new_struct("X", fields = list(n = field(type_double(min = 0), unit = "mg")))
  })
  expect_error(r8_equal(h, out$D(d = X(n = 1))), "is in `mg`", class = other)
  expect_error(h == out$D(d = X(n = 1)), "is in `mg`", class = other)
  expect_error(r8_update(h), "is in `mg`", class = other)
  expect_error(r8_validate(h), class = "r8_value_error")
})

test_that("a constructor builds a default that is a value from the registered declaration", {
  out <- declare_in("up10", {
    K <- new_enum("K", variants = list(A = variant(), B = variant()))
    P <- new_struct("P", fields = list(x = type_double()))
    H <- new_struct(
      "H",
      fields = list(
        k = field(K, default = K@A),
        ps = field(vec(P), default = list(P(x = 1)))
      )
    )
    list(K = K, P = P, H = H)
  })
  a <- out$K@A
  # The constructor is the one of before, as in a package that was not installed
  # again.
  H <- out$H
  declare_in("up10", {
    new_enum("K", variants = list(A = variant(), B = variant(), C = variant()))
    new_struct("P", fields = list(x = type_double(), z = option(type_double())))
  })
  h <- H()
  expect_identical(r8_validate(h), h)
  expect_identical(h$k, out$K@A)
  expect_identical(names(h$ps[[1L]]), c("x", "z"))
  expect_error(H(k = a), "the variant `C` was added", class = "r8_value_error")
})

test_that("r8_convert() refuses a value of another declaration of its target", {
  S <- declare_in("up11", new_struct("S", fields = list(a = type_double())))
  x <- S(a = 1)
  S <- declare_in("up11", {
    new_struct("S", fields = list(a = field(type_double(), unit = "kg")))
  })
  expect_error(r8_convert(x, S), "is in `kg`", class = other)
})

test_that("a value of a stale type refuses a write and a comparison", {
  out <- declare_in("up12", {
    P <- new_struct("P", fields = list(a = type_double()))
    C <- new_struct("C", fields = list(b = type_double()), extends = P)
    list(P = P, C = C)
  })
  x <- out$C(a = 1, b = 2)
  y <- out$C(a = 1, b = 2)
  suppressWarnings(declare_in("up12", {
    new_struct("P", fields = list(a = type_double(min = 5)))
  }))
  expect_error(x$b <- 9, "type_stale", class = "r8_value_error")
  expect_error(r8_equal(x, y), "type_stale", class = "r8_value_error")
})

test_that("factor levels that are added or removed are checked, and reordered ones refused", {
  struct <- function(levels) {
    declare_in("up13", new_struct("S", fields = list(f = type_factor(levels))))
  }
  S <- struct(c("a", "b"))
  x <- S(f = factor("b", levels = c("a", "b")))
  S <- struct(c("a", "b", "c"))
  expect_identical(r8_update(x), S(f = factor("b", levels = c("a", "b", "c"))))
  struct("b")
  expect_identical(levels(r8_update(x)$f), "b")
  struct("a")
  expect_error(r8_update(x), class = "r8_value_error")
  struct(c("b", "a"))
  expect_error(r8_update(x), "may mean something else", class = other)
})

test_that("an update that the new declaration does not admit names its type", {
  S <- declare_in("up14", new_struct("S", fields = list(a = type_double())))
  x <- S(a = -1)
  declare_in("up14", new_struct("S", fields = list(a = type_double(min = 0))))
  expect_error(
    r8_update(x),
    "Cannot update to `up14::S`: 1 problem",
    fixed = TRUE
  )
})

test_that("a value of another declaration inside another shows without its own text", {
  out <- declare_in("up15", {
    Sized <- new_trait("Sized", methods = list(size = trait_method(params(x))))
    X <- new_struct("X", fields = list(n = type_double()))
    impl(Sized, X, methods = list(size = function(x) x$n))
    implement(format, X) <- function(x, ...) "its own text"
    D <- new_struct("D", fields = list(d = dyn(Sized)))
    list(X = X, D = D)
  })
  h <- out$D(d = out$X(n = 1))
  expect_match(format(h), "its own text", all = FALSE)
  declare_in("up15", {
    new_struct("X", fields = list(n = field(type_double(), unit = "mg")))
  })
  expect_no_match(format(h), "its own text")
})
