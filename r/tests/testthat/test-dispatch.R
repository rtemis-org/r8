# test-dispatch.R
# ::R8::
# 2026- EDG rtemis.org

# spec: R8/class-system#multiple-dispatch

pair_types <- function(ns) {
  declare_in(ns, {
    A <- new_struct("A", fields = list(a = type_double()))
    A1 <- new_struct("A1", fields = list(a1 = type_double()), extends = A)
    A2 <- new_struct("A2", fields = list(a2 = type_double()), extends = A1)
    B <- new_struct("B", fields = list(b = type_double()))
    B1 <- new_struct("B1", fields = list(b1 = type_double()), extends = B)
    C <- new_struct("C", fields = list(c = type_double()))
    list(A = A, A1 = A1, A2 = A2, B = B, B1 = B1, C = C)
  })
}

test_that("a generic dispatches on the arguments it names, through extends", {
  ty <- pair_types("md1")
  list2env(ty, environment())
  declare_in("md1", {
    combine <- generic("combine", c("x", "y", "..."), dispatch = c("x", "y"))
    expect_identical(attr(combine, "r8_dispatch"), c("x", "y"))
    expect_output(print(combine), "Dispatches on: x, y")
    implement(combine, A, B) <- function(x, y, ...) "A, B"
    implement(combine, A1, B) <- function(x, y, ...) "A1, B"
    implement(combine, A, B1) <- function(x, y, ...) "A, B1"
    implement(combine, A1, B1) <- function(x, y, ...) "A1, B1"
    implement(combine, C) <- function(x, y, ...) "C, any"
    a <- A(a = 1)
    a1 <- A1(a = 1, a1 = 2)
    a2 <- A2(a = 1, a1 = 2, a2 = 3)
    b <- B(b = 1)
    b1 <- B1(b = 1, b1 = 2)
    expect_identical(combine(a, b), "A, B")
    expect_identical(combine(a1, b), "A1, B")
    expect_identical(combine(a2, b1), "A1, B1")
    expect_identical(combine(a, b1), "A, B1")
    expect_identical(combine(C(c = 1), 42), "C, any")
    expect_identical(combine(C(c = 1), NULL), "C, any")
    # The second call of each hits the cache.
    expect_identical(combine(a2, b1), "A1, B1")
    expect_error(
      combine(b, a),
      "applies to \\(md1::B, md1::A\\)",
      class = "r8_dispatch_error"
    )
    expect_identical(r8_method(combine, A1, B1)(a1, b1), "A1, B1")
    expect_identical(r8_method(combine, x = C)(1, 2), "C, any")
    expect_output(print(combine), "\\(md1::A1, md1::B1\\)")
  })
})

test_that("an omitted or named position matches any value", {
  ty <- pair_types("md2")
  list2env(ty, environment())
  declare_in("md2", {
    place <- generic("place", c("x", "at", "z"), dispatch = c("x", "z"))
    implement(place, z = C) <- function(x, at, z) "any, C"
    implement(place, A, z = C) <- function(x, at, z) "A, C"
    implement(place, A) <- function(x, at, z) "A, any"
    implement(place, A1, C) <- function(x, at, z) "A1, C"
    expect_identical(place(B(b = 1), 0, C(c = 1)), "any, C")
    expect_identical(place(A(a = 1), 0, C(c = 1)), "A, C")
    expect_identical(place(A(a = 1), 0, B(b = 1)), "A, any")
    expect_identical(place(A2(a = 1, a1 = 2, a2 = 3), 0, C(c = 1)), "A1, C")
    expect_error(
      implement(place, at = A) <- function(x, at, z) NULL,
      "`at` is not one of them",
      class = "r8_declaration_error"
    )
    expect_error(
      implement(place, A, B, C) <- function(x, at, z) NULL,
      "names 3 types",
      class = "r8_declaration_error"
    )
  })
})

test_that("an ambiguous call names the method to declare", {
  ty <- pair_types("md3")
  list2env(ty, environment())
  declare_in("md3", {
    combine <- generic("combine", c("x", "y"), dispatch = c("x", "y"))
    implement(combine, A1, B) <- function(x, y) "A1, B"
    implement(combine, A, B1) <- function(x, y) "A, B1"
    amb <- r8_ambiguities(combine)
    expect_identical(nrow(amb), 1L)
    expect_identical(amb[["meet"]], "(md3::A1, md3::B1)")
    a1 <- A1(a = 1, a1 = 2)
    b1 <- B1(b = 1, b1 = 2)
    expect_error(
      combine(a1, b1),
      "implement it for \\(md3::A1, md3::B1\\).*dispatch_ambiguous",
      class = "r8_dispatch_error"
    )
    # The error is not cached: declaring the meet resolves the call.
    implement(combine, A1, B1) <- function(x, y) "A1, B1"
    expect_identical(combine(a1, b1), "A1, B1")
    expect_identical(nrow(r8_ambiguities(combine)), 0L)
  })
})

test_that("r8_next_impl() continues to the unique less specific method or the one named", {
  ty <- pair_types("md4")
  list2env(ty, environment())
  declare_in("md4", {
    combine <- generic("combine", c("x", "y"), dispatch = c("x", "y"))
    implement(combine, A, B) <- function(x, y) "A, B"
    implement(combine, A1, B) <- function(x, y) paste("A1, B >", r8_next_impl())
    implement(combine, A, B1) <- function(x, y) "A, B1"
    implement(combine, A1, B1) <- function(x, y) r8_next_impl()
    a1 <- A1(a = 1, a1 = 2)
    expect_identical(combine(a1, B(b = 1)), "A1, B > A, B")
    expect_error(
      combine(a1, B1(b = 1, b1 = 2)),
      "after the running impl.*r8_next_impl\\(Type, ...\\)",
      class = "r8_dispatch_error"
    )
    implement(combine, A1, B1) <- function(x, y) r8_next_impl(A, B1)
    expect_identical(combine(a1, B1(b = 1, b1 = 2)), "A, B1")
    implement(combine, A1, B1) <- function(x, y) r8_next_impl(C, B1)
    expect_error(
      combine(a1, B1(b = 1, b1 = 2)),
      "has no impl for \\(md4::C, md4::B1\\)",
      class = "r8_dispatch_error"
    )
    implement(combine, A, B) <- function(x, y) r8_next_impl()
    expect_error(
      combine(A(a = 1), B(b = 1)),
      "has no next impl",
      class = "r8_dispatch_error"
    )
  })
})

test_that("a trait's methods dispatch on several arguments, with one impl per tuple", {
  ty <- pair_types("md5")
  list2env(ty, environment())
  declare_in("md5", {
    Pair <- new_trait(
      "Pair",
      methods = list(
        join = trait_method(function(x, y) NULL, dispatch = c("x", "y")),
        label = trait_method(
          function(p, q) NULL,
          dispatch = c("p", "q"),
          default = function(p, q) "default label"
        )
      )
    )
    impl(
      Pair,
      list(A, B),
      methods = list(
        join = function(x, y) "A+B",
        label = function(p, q) "A, B label"
      )
    )
    impl(Pair, list(A1, B), methods = list(join = function(x, y) "A1+B"))
    join <- Pair@join
    label <- Pair@label
    a1 <- A1(a = 1, a1 = 2)
    expect_identical(join(a1, B(b = 1)), "A1+B")
    # The impl for (A1, B) provides every method; `label` takes the default.
    expect_identical(label(a1, B(b = 1)), "default label")
    expect_identical(label(A(a = 1), B(b = 1)), "A, B label")
    expect_error(
      new_trait(
        "Bad",
        methods = list(
          one = trait_method(function(x, y) NULL, dispatch = c("x", "y")),
          two = trait_method(function(x) NULL)
        )
      ),
      "dispatch_arity",
      class = "r8_declaration_error"
    )
    expect_error(
      new_trait(
        "Tagged",
        tag = "kind",
        methods = list(
          pair = trait_method(function(x, y) NULL, dispatch = c("x", "y"))
        )
      ),
      "dispatch_tag",
      class = "r8_declaration_error"
    )
    expect_error(
      new_struct("Holder", fields = list(p = dyn(Pair))),
      "dyn_dispatch",
      class = "r8_declaration_error"
    )
  })
})

test_that("dispatch arguments are checked at declaration", {
  declare_in("md6", {
    expect_error(
      generic("f", c("x", "..."), dispatch = c("x", "...")),
      "`...` is not one",
      class = "r8_declaration_error"
    )
    expect_error(
      generic("f", c("x", "y"), dispatch = "z"),
      "`z` is not one",
      class = "r8_declaration_error"
    )
    expect_error(
      generic("f", c("a", "b", "c", "d"), dispatch = c("a", "b", "c", "d")),
      "at most 3",
      class = "r8_declaration_error"
    )
    expect_error(
      generic("lab<-", c("x", "y", "value"), dispatch = c("x", "y")),
      "first argument only",
      class = "r8_declaration_error"
    )
    expect_error(
      new_trait(
        "S",
        methods = list(
          merge = trait_method(function(x, y, ...) NULL, dispatch = c("x", "y"))
        )
      ),
      "dispatches on its first argument only",
      class = "r8_declaration_error"
    )
    # A dispatched argument is forced; a missing one is R's own error.
    g <- generic("g", c("x", "y"), dispatch = c("x", "y"))
    expect_error(g(1), "argument \"y\" is missing")
  })
})

test_that("an impl of another namespace follows the orphan rule over its positions", {
  ty <- pair_types("md7")
  list2env(ty, environment())
  combine <- declare_in(
    "md7",
    generic("combine", c("x", "y"), dispatch = c("x", "y"))
  )
  Mine <- declare_in(
    "md8",
    new_struct("Mine", fields = list(m = type_double()))
  )
  expect_no_error(declare_in("md8", {
    implement(combine, A, Mine) <- function(x, y) "A, Mine"
  }))
  expect_identical(combine(A(a = 1), Mine(m = 1)), "A, Mine")
  expect_error(
    declare_in("md8", {
      implement(combine, y = Mine) <- function(x, y) NULL
    }),
    "orphan rule",
    class = "r8_declaration_error"
  )
  # The trait's namespace may not declare a tuple that md8 declares.
  expect_error(
    declare_in("md7", {
      implement(combine, A, Mine) <- function(x, y) "again"
    }),
    "duplicate_impl",
    class = "r8_declaration_error"
  )
  expect_identical(combine(A(a = 1), Mine(m = 1)), "A, Mine")
})

test_that("values without a class attribute hit the dispatch cache", {
  declare_in("md9", {
    Show <- new_trait(
      "Show",
      methods = list(
        show_it = trait_method(function(x) NULL, default = function(x) {
          "default"
        })
      )
    )
    show_it <- Show@show_it
    expect_identical(show_it(1), "default")
    expect_identical(show_it(matrix(1:4, 2)), "default")
    index <- R8:::.r8_dispatch[["md9::Show|show_it"]]
    expect_true(exists("type:double", envir = index, inherits = FALSE))
    expect_true(exists("class:matrix", envir = index, inherits = FALSE))
  })
})
