# test-119-representation.R
# ::R8::
# 2026- EDG rtemis.org

# A value as an R object: its identity and fields are in an ALTREP object, where
# R code does not reach, and its attributes are its names and its class.
#
# spec: R8/class-system#values-in-r

representation_types <- function(ns) {
  declare_in(ns, {
    Point <- new_struct(
      "Point",
      fields = list(x = type_double(min = 0), y = type_double())
    )
    Shape <- new_enum(
      "Shape",
      variants = list(
        Round = variant(fields = list(size = type_double())),
        Square = variant(fields = list(size = type_double())),
        Nothing = variant()
      )
    )
    Empty <- new_struct("Empty", fields = list())
    list(Point = Point, Shape = Shape, Empty = Empty)
  })
}

is_one <- function(x) {
  inherits(tryCatch(r8validate(x), error = identity), "r8_value")
}

test_that("the attributes of a value are its names and its class", {
  t <- representation_types("rep1")
  p <- t[["Point"]](x = 1, y = 2)
  expect_identical(
    attributes(p),
    list(names = c("x", "y"), class = c("rep1::Point", "r8_value"))
  )
  expect_identical(
    attributes(t[["Shape"]]@Round(size = 1)),
    list(
      names = "size",
      class = c("rep1::Shape@Round", "rep1::Shape", "r8_value")
    )
  )
  expect_identical(
    class(t[["Shape"]]@Nothing),
    c("rep1::Shape@Nothing", "rep1::Shape", "r8_value")
  )
})

test_that("identical() tells the variants of an enum apart", {
  t <- representation_types("rep2")
  Shape <- t[["Shape"]]
  expect_identical(Shape@Round(size = 1), Shape@Round(size = 1))
  expect_false(identical(Shape@Round(size = 1), Shape@Square(size = 1)))
  expect_false(Shape@Square(size = 1) %in% list(Shape@Round(size = 1)))
  expect_length(
    unique(list(
      Shape@Round(size = 1),
      Shape@Square(size = 1),
      Shape@Round(size = 1)
    )),
    2L
  )
})

test_that("a value stays one wherever R code moves it", {
  t <- representation_types("rep3")
  p <- t[["Point"]](x = 1, y = 2)
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path))
  saveRDS(p, path)
  e <- new.env()
  e$p <- p
  ways <- list(
    copy = {
      q <- p
      q
    },
    list = list(1, p)[[2L]],
    modified = {
      l <- list(p, 1)
      l[[2L]] <- 2
      l[[1L]]
    },
    lapply = lapply(list(p), identity)[[1L]],
    map = Map(function(v) v, list(p))[[1L]],
    rbind = do.call(rbind, list(data.frame(i = 1, v = I(list(p)))))$v[[1L]],
    data.frame = data.frame(i = 1:2, v = I(list(p, p)))[2L, "v"][[1L]],
    split = split(I(list(p, p)), 1:2)[[1L]][[1L]],
    modifyList = utils::modifyList(list(a = 1, b = p), list(a = 2))$b,
    unclassed = {
      u <- unclass(p)
      class(u) <- class(p)
      u
    },
    structure = structure(p, note = "x"),
    subclass = structure(p, class = c("mine", class(p))),
    environment = e$p,
    serialize = unserialize(serialize(p, NULL)),
    ascii = unserialize(serialize(p, NULL, ascii = TRUE)),
    rds = readRDS(path),
    nested = unserialize(serialize(list(a = p, b = list(p)), NULL))$b[[1L]],
    cloned = local({
      f <- function(v) {
        attr(v, "x") <- 1
        v
      }
      f(p)
    })
  )
  for (w in names(ways)) {
    expect_true(is_one(ways[[w]]), info = w)
    expect_identical(as.list(ways[[w]]), list(x = 1, y = 2), info = w)
    expect_true(r8equal(ways[[w]], p), info = w)
  }
  expect_true(is_one(unserialize(serialize(t[["Empty"]](), NULL))))
  expect_true(is_one(unserialize(serialize(t[["Shape"]]@Nothing, NULL))))
})

test_that("an object that R code makes from the parts of a value is not one", {
  t <- representation_types("rep4")
  p <- t[["Point"]](x = 1, y = 2)
  q <- unclass(p)
  q$x <- -1
  class(q) <- class(p)
  renamed <- unclass(p)
  names(renamed) <- c("y", "x")
  class(renamed) <- class(p)
  copied <- list(x = 1, y = 2)
  attributes(copied) <- attributes(p)
  shorter <- unclass(p)[1L]
  class(shorter) <- class(p)
  text <- eval(parse(text = deparse(p)))
  ways <- list(
    written = q,
    renamed = renamed,
    attributes = copied,
    structure = structure(list(x = 1, y = 2), class = class(p)),
    mostattributes = local({
      l <- list(x = 1, y = 2)
      mostattributes(l) <- attributes(p)
      l
    }),
    shorter = shorter,
    deparsed = text,
    unclassed = unclass(p),
    version2 = unserialize(serialize(p, NULL, version = 2))
  )
  for (w in names(ways)) {
    expect_false(is_one(ways[[w]]), info = w)
    expect_error(r8to_json(ways[[w]]), class = "r8_error", info = w)
  }
  # The value is as it was.
  expect_true(is_one(p))
  expect_identical(p$x, 1)
})

test_that("an object that is not a value prints as a list, with what it is", {
  p <- representation_types("rep5")[["Point"]](x = 1, y = 2)
  copied <- structure(list(x = 1, y = 2), class = class(p))
  expect_match(format(copied)[[1L]], "is not one")
  expect_output(print(copied), "\\$x")
  expect_output(str(copied), "is not one")
  expect_identical(copied$x, 1)
  expect_identical(as.list(copied), list(x = 1, y = 2))
})

test_that("a saved value is small, and its declaration is written once", {
  Point <- representation_types("rep6")[["Point"]]
  size <- function(n) {
    length(serialize(lapply(seq_len(n), function(i) Point(x = i, y = 1)), NULL))
  }
  # The bytes of each value after the first.
  expect_lt((size(1000L) - size(1L)) / 999, 250)
})
