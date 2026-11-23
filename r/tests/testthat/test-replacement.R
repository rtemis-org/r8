# test-replacement.R
# ::R8::
# 2026- EDG rtemis.org

# spec: R8/class-system#replacement-methods

range_types <- function(ns) {
  declare_in(ns, {
    Range <- new_struct(
      "Range",
      fields = list(
        lo = type_double(),
        hi = type_double(),
        tag = type_character()
      )
    )
    Window <- new_struct(
      "Window",
      fields = list(label = type_character()),
      extends = Range
    )
    list(Range = Range, Window = Window)
  })
}

test_that("a parent's replacement method on a child returns the child", {
  ty <- range_types("rp1")
  Range <- ty[["Range"]]
  Window <- ty[["Window"]]
  declare_in("rp1", {
    `tag<-` <- generic("tag<-", c("x", "value"))
    implement(`tag<-`, Range) <- function(x, value) {
      x$tag <- value
      x
    }
    w <- Window(lo = 1, hi = 2, tag = "a", label = "w")
    w0 <- w
    tag(w) <- "b"
    expect_identical(class(w), c("rp1::Window", "rp1::Range", "r8_value"))
    expect_identical(w$tag, "b")
    expect_identical(w$label, "w")
    expect_identical(w0$tag, "a")
  })
})

test_that("a replacement method that returns another type raises r8_type_error", {
  ty <- range_types("rp2")
  Range <- ty[["Range"]]
  Window <- ty[["Window"]]
  declare_in("rp2", {
    `tag<-` <- generic("tag<-", c("x", "value"))
    implement(`tag<-`, Range) <- function(x, value) {
      Range(lo = x$lo, hi = x$hi, tag = value)
    }
    r <- Range(lo = 1, hi = 2, tag = "a")
    tag(r) <- "b"
    expect_identical(r$tag, "b")
    w <- Window(lo = 1, hi = 2, tag = "a", label = "w")
    expect_error(
      tag(w) <- "c",
      "from the method for `rp2::Range`, returned a value of `rp2::Range` for a value of `rp2::Window`.*replacement_type",
      class = "r8_type_error"
    )
    expect_identical(class(w)[1L], "rp2::Window")
    implement(`tag<-`, Window) <- function(x, value) "text"
    expect_error(
      tag(w) <- "c",
      "an object of class \"character\"",
      class = "r8_type_error"
    )
  })
})

test_that("an enum's replacement method may change the variant", {
  declare_in("rp3", {
    Status <- new_enum(
      "Status",
      variants = list(Running = variant(), Done = variant())
    )
    Other <- new_enum("Other", variants = list(Z = variant()))
    `status<-` <- generic("status<-", c("x", "value"))
    implement(`status<-`, Status) <- function(x, value) {
      if (identical(value, "done")) Status@Done else Other@Z
    }
    s <- Status@Running
    status(s) <- "done"
    expect_identical(attr(s, "r8_variant"), "Done")
    expect_error(status(s) <- "other", "rp3::Other", class = "r8_type_error")
  })
})

test_that("a host object's replacement result keeps its class vector", {
  declare_in("rp4", {
    Note <- new_trait(
      "Note",
      methods = list(
        `note<-` = trait_method(
          function(x, value) NULL,
          default_impl = function(x, value) {
            if (identical(value, "drop")) {
              return(unclass(x))
            }
            attr(x, "note") <- value
            x
          }
        )
      )
    )
    `note<-` <- Note@`note<-`
    d <- data.frame(a = 1)
    note(d) <- "kept"
    expect_identical(attr(d, "note"), "kept")
    expect_error(note(d) <- "drop", "replacement_type", class = "r8_type_error")
    m <- matrix(1:4, 2)
    note(m) <- "kept"
    expect_identical(attr(m, "note"), "kept")
  })
})

test_that("r8_next_impl() in a replacement impl passes the current arguments", {
  ty <- range_types("rp5")
  Range <- ty[["Range"]]
  Window <- ty[["Window"]]
  declare_in("rp5", {
    `tag<-` <- generic("tag<-", c("x", "value"))
    implement(`tag<-`, Range) <- function(x, value) {
      x$tag <- value
      x
    }
    implement(`tag<-`, Window) <- function(x, value) {
      x$label <- toupper(value)
      r8_next_impl()
    }
    w <- Window(lo = 1, hi = 2, tag = "a", label = "w")
    tag(w) <- "b"
    expect_identical(c(w$tag, w$label), c("b", "B"))
    implement(`tag<-`, Window) <- function(x, value) {
      x <- Range(lo = 0, hi = 1, tag = "r")
      r8_next_impl()
    }
    expect_error(
      tag(w) <- "c",
      "neither `rp5::Window` nor one of its descendants",
      class = "r8_dispatch_error"
    )
  })
})

test_that("an S3 replacement generic's method is checked and continues with r8_next_impl()", {
  ty <- range_types("rp6")
  Range <- ty[["Range"]]
  Window <- ty[["Window"]]
  declare_in("rp6", {
    Units <- new_trait(
      "Units",
      methods = list(`units<-` = trait_method(function(x, value) NULL))
    )
    impl(
      Units,
      Range,
      methods = list(`units<-` = function(x, value) {
        x$tag <- value
        x
      })
    )
    impl(
      Units,
      Window,
      methods = list(`units<-` = function(x, value) {
        x$label <- "u"
        r8_next_impl()
      })
    )
    w <- Window(lo = 1, hi = 2, tag = "a", label = "w")
    units(w) <- "cm"
    expect_identical(c(w$tag, w$label), c("cm", "u"))
    impl(Units, Window, methods = list(`units<-` = function(x, value) 1))
    expect_error(units(w) <- "cm", "replacement_type", class = "r8_type_error")
  })
})

test_that("a Print impl continues with r8_next_impl()", {
  ty <- range_types("rp7")
  Range <- ty[["Range"]]
  Window <- ty[["Window"]]
  declare_in("rp7", {
    impl(
      Print,
      Range,
      methods = list(print = function(x, ...) cat("a range\n"))
    )
    impl(
      Print,
      Window,
      methods = list(print = function(x, ...) {
        cat("a window, ")
        r8_next_impl()
      })
    )
    expect_output(
      print(Window(lo = 1, hi = 2, tag = "a", label = "w")),
      "a window, a range"
    )
  })
})

test_that("generic() declares replacement generics under the trait with_f", {
  declare_in("rp8", {
    `labels<-` <- generic("labels<-", c("x", "value"))
    expect_identical(attr(`labels<-`, "r8_trait"), "rp8::with_labels")
    expect_identical(attr(`labels<-`, "r8_method"), "labels<-")
    expect_output(print(`labels<-`), "<r8 generic rp8::labels<->")
    expect_output(print(`labels<-`), "labels\\(x\\) <- value")
    expect_error(
      generic("with_labels"),
      "share the trait `rp8::with_labels`",
      class = "r8_declaration_error"
    )
    expect_error(
      new_trait(
        "with_labels",
        methods = list(f = trait_method(function(x) NULL))
      ),
      "trait of the replacement generic",
      class = "r8_declaration_error"
    )
    expect_error(
      generic("lab<-", "x"),
      "the last argument `value`",
      class = "r8_declaration_error"
    )
    expect_error(
      generic("lab<-", c("value", "x")),
      "the last argument `value`",
      class = "r8_declaration_error"
    )
    expect_error(
      generic("levels<-", c("x", "value")),
      "reserved",
      class = "r8_declaration_error"
    )
    expect_error(
      generic("units<-", c("x", "value")),
      "implement\\(`units<-`, Type\\)",
      class = "r8_declaration_error"
    )
  })
})

test_that("new_trait() reserves the replacement names that implement() refuses", {
  for (m in c("names<-", "length<-", "dim<-", "levels<-", "[<-")) {
    methods <- stats::setNames(
      list(trait_method(function(x, value) NULL)),
      m
    )
    expect_error(
      declare_in("rp9", new_trait("Bad", methods = methods)),
      "reserved",
      class = "r8_declaration_error"
    )
  }
})

test_that("implement() on an S3 replacement generic checks the method's result", {
  ty <- range_types("rp10")
  Range <- ty[["Range"]]
  declare_in("rp10", {
    implement(`units<-`, Range) <- function(x, value) {
      x$tag <- value
      x
    }
    r <- Range(lo = 1, hi = 2, tag = "a")
    units(r) <- "cm"
    expect_identical(r$tag, "cm")
    implement(`units<-`, Range) <- function(x, value) value
    expect_error(
      units(r) <- "m",
      "The replacement method of `base::units<-`",
      class = "r8_type_error"
    )
  })
})
