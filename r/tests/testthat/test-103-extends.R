# test-103-extends.R
# ::R8::
# 2026- EDG rtemis.org

# Structs that extend another struct.
#
# A child holds the fields and the rules of its parent, then its own. These
# tests check what it inherits, the defaults that it changes, and the values
# of each in the place of the other.
#
# spec: R8/class-system#inheritance

test_that("a child holds its parent's fields and rules, then its own", {
  declare_in("ext1", {
    A <- new_struct(
      "A",
      fields = list(a = field(type_double(), default = 1)),
      rules = rule(a > 0, message = "a is positive.", id = "A_POS")
    )
    B <- new_struct(
      "B",
      fields = list(b = type_double()),
      rules = rule(b > 0, message = "b is positive.", id = "B_POS"),
      extends = A,
      defaults = list(a = 2)
    )
    C <- new_struct("C", fields = list(c = type_character()), extends = B)
    expect_identical(names(formals(C)), c("a", "b", "c"))
    expect_identical(formals(C)[["a"]], 2)
    x <- C(b = 3, c = "x")
    expect_identical(class(x), c("ext1::C", "ext1::B", "ext1::A", "r8_value"))
    expect_identical(R8:::value_type(x), "ext1::C")
    expect_true(inherits(x, "ext1::A"))
    expect_identical(x$a, 2)
    # The parent's rules apply to the child.
    expect_error(C(a = -1, b = 3, c = "x"), "A_POS", class = "r8_value_error")
    expect_error(C(b = -3, c = "x"), "B_POS", class = "r8_value_error")
    ir <- R8:::type_ir(C)
    expect_identical(ir[["extends"]], "ext1::B")
    expect_identical(
      vapply(ir[["fields"]], function(f) f[["inherited_from"]] %||% "", ""),
      c("ext1::A", "ext1::B", "")
    )
    # A JSON round trip keeps the child's type.
    y <- r8from_json(C, r8to_json(x))
    expect_true(r8equal(x, y))
    expect_identical(class(y), class(x))
  })
})

test_that("a field of a named type admits that type only", {
  declare_in("ext2", {
    A <- new_struct("A", fields = list(a = type_double()))
    B <- new_struct("B", fields = list(b = type_double()), extends = A)
    H <- new_struct("H", fields = list(x = A))
    expect_s3_class(H(x = A(a = 1)), "r8_value")
    expect_error(H(x = B(a = 1, b = 2)), class = "r8_value_error")
  })
})

test_that("extends and defaults refuse what a child cannot declare", {
  declare_in("ext3", {
    A <- new_struct(
      "A",
      fields = list(
        a = field(type_double(), default = 1),
        o = field(option(type_double()), default = 1)
      )
    )
    expect_error(
      new_struct("B", extends = "A"),
      "is an R8 struct type",
      class = "r8_declaration_error"
    )
    expect_error(
      new_struct("B", defaults = list(a = 2)),
      "set `extends =`",
      class = "r8_declaration_error"
    )
    expect_error(
      new_struct("B", fields = list(a = type_double()), extends = A),
      "duplicate_field",
      class = "r8_declaration_error"
    )
    expect_error(
      new_struct("B", extends = A, defaults = list(z = 1)),
      "inherited_unknown",
      class = "r8_declaration_error"
    )
    expect_error(
      new_struct("B", extends = A, defaults = list(a = NULL)),
      "default_removed",
      class = "r8_declaration_error"
    )
    expect_error(
      new_struct("B", extends = A, defaults = list(a = 1, a = 2)),
      "names `a` twice",
      class = "r8_declaration_error"
    )
    B <- new_struct("B", extends = A, defaults = list(o = NULL))
    expect_null(B()$o)
    expect_identical(A()$o, 1)
  })
})

test_that("a struct whose defaults violate its rules is refused", {
  declare_in("ext4", {
    expect_error(
      new_struct(
        "Range",
        fields = list(
          lo = field(type_double(), default = 5),
          hi = field(type_double(), default = 1)
        ),
        rules = rule(lo < hi, message = "lo is below hi.", id = "ORDER")
      ),
      "default_violates_rule",
      class = "r8_declaration_error"
    )
  })
})

test_that("a parent declared again leaves its children stale until they are declared again", {
  declare_in("ext5", {
    A <- new_struct("A", fields = list(a = type_double()))
    B <- new_struct("B", fields = list(b = type_double()), extends = A)
    impl(Display, B, list(format = function(x, ...) "a B"))
    b <- B(a = 1, b = 2)
    expect_warning(
      A <- new_struct("A", fields = list(a = type_double(), z = type_double())),
      "`ext5::B` stale",
      class = "r8_stale_warning"
    )
    expect_error(
      B(a = 1, b = 2),
      "type_stale|declare `ext5::B` again",
      class = "r8_value_error"
    )
    # An existing value reads its fields and dispatches on its own impl.
    expect_identical(b$b, 2)
    expect_identical(format(b), "a B")
    B <- new_struct("B", fields = list(b = type_double()), extends = A)
    expect_s3_class(B(a = 1, b = 2, z = 3), "r8_value")
    expect_false("ext5::B" %in% R8:::r8_stale_types(character()))
  })
})
