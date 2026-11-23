# test-dispatch-extends.R
# ::R8::
# 2026- EDG rtemis.org

# spec: R8/class-system#inheritance

chain_types <- function(ns) {
  declare_in(ns, {
    A <- new_struct("A", fields = list(a = type_double()))
    B <- new_struct("B", fields = list(b = type_double()), extends = A)
    C <- new_struct("C", fields = list(c = type_double()), extends = B)
    list(A = A, B = B, C = C)
  })
}

test_that("a parent's impl runs on the child, and generics in it dispatch on the child", {
  ty <- chain_types("dx1")
  A <- ty[["A"]]
  B <- ty[["B"]]
  C <- ty[["C"]]
  declare_in("dx1", {
    label <- generic("label", "x")
    describe <- generic("describe", "x")
    implement(label, A) <- function(x) "an A"
    implement(label, C) <- function(x) "a C"
    implement(describe, A) <- function(x) {
      y <- x
      y$a <- 10
      list(label = label(x), class = class(y)[1L])
    }
    x <- C(a = 1, b = 2, c = 3)
    out <- describe(x)
    expect_identical(out[["label"]], "a C")
    expect_identical(out[["class"]], "dx1::C")
    expect_identical(label(B(a = 1, b = 2)), "an A")
    # The cached impl serves the second call.
    expect_identical(describe(x)[["label"]], "a C")
  })
})

test_that("an impl is the unit of precedence through extends", {
  ty <- chain_types("dx2")
  A <- ty[["A"]]
  B <- ty[["B"]]
  C <- ty[["C"]]
  declare_in("dx2", {
    Shape <- new_trait(
      "Shape",
      methods = list(
        name = trait_method(function(x) NULL),
        size = trait_method(function(x) NULL, default_impl = function(x) {
          "default size"
        })
      )
    )
    impl(
      Shape,
      A,
      methods = list(name = function(x) "A", size = function(x) "A size")
    )
    impl(Shape, B, methods = list(name = function(x) "B"))
    x <- C(a = 1, b = 2, c = 3)
    expect_identical(Shape@name(x), "B")
    # B's impl omits size: the trait's default, not A's.
    expect_identical(Shape@size(x), "default size")
  })
})

test_that("r8_next_impl() continues from the provider, level by level", {
  ty <- chain_types("dx3")
  A <- ty[["A"]]
  B <- ty[["B"]]
  C <- ty[["C"]]
  declare_in("dx3", {
    path <- generic("path", c("x", "sep"))
    implement(path, A) <- function(x, sep) "A"
    implement(path, B) <- function(x, sep) paste("B", r8_next_impl(), sep = sep)
    implement(path, C) <- function(x, sep) paste("C", r8_next_impl(), sep = sep)
    expect_identical(path(C(a = 1, b = 2, c = 3), sep = "/"), "C/B/A")
    expect_identical(path(B(a = 1, b = 2), sep = "-"), "B-A")
    implement(path, A) <- function(x, sep) r8_next_impl()
    expect_error(
      path(A(a = 1), sep = "/"),
      "has no next impl",
      class = "r8_dispatch_error"
    )
    expect_error(
      r8_next_impl(),
      "from the body of an impl",
      class = "r8_dispatch_error"
    )
  })
})

test_that("r8_upcast() and r8_convert() return the value of an ancestor", {
  ty <- chain_types("dx4")
  A <- ty[["A"]]
  B <- ty[["B"]]
  C <- ty[["C"]]
  declare_in("dx4", {
    x <- C(a = 1, b = 2, c = 3)
    a <- r8_upcast(x, A)
    expect_identical(class(a), c("dx4::A", "r8_value"))
    expect_true(r8_equal(a, A(a = 1)))
    expect_no_error(r8_validate(a))
    b <- r8_convert(x, B)
    expect_identical(names(unclass(b)), c("a", "b"))
    expect_identical(r8_upcast(x, C), x)
    expect_identical(r8_ancestors(C), c("dx4::B", "dx4::A"))
    expect_identical(r8_ancestors(x), c("dx4::B", "dx4::A"))
    expect_identical(r8_ancestors(A), character())
    expect_error(r8_upcast(a, C), "not an ancestor", class = "r8_type_error")
    expect_error(
      impl(From(C), A, methods = list(from = function(x) A(a = x$a))),
      "reserved",
      class = "r8_declaration_error"
    )
  })
})

test_that("a stale child refuses ancestors, upcast, and forwarded dispatch", {
  declare_in("dx5", {
    A <- new_struct("A", fields = list(a = type_double()))
    B <- new_struct("B", fields = list(b = type_double()), extends = A)
    label <- generic("label", "x")
    implement(label, A) <- function(x) "an A"
    b <- B(a = 1, b = 2)
    expect_warning(
      A <- new_struct("A", fields = list(a = type_double(), z = type_double())),
      class = "r8_stale_warning"
    )
    expect_error(r8_ancestors(B), "type_stale", class = "r8_type_error")
    expect_error(r8_upcast(b, A), "type_stale", class = "r8_type_error")
    expect_error(label(b), "type_stale", class = "r8_dispatch_error")
  })
})
