# test-701-misuse.R
# ::R8::
# 2026- EDG rtemis.org

# Scenarios of misuse and damage: a value that R code takes apart, and a saved
# value whose bytes are damaged.
#
# R lets any code change the attributes of an object. These tests state what R8
# finds out, and where.

misuse_type <- function(ns) {
  declare_in(ns, {
    new_struct(
      "Point",
      fields = list(x = type_double(min = 0), y = type_double())
    )
  })
}

scenario(
  "F2",
  "a typed argument refuses a value that was taken apart and rebuilt",
  situation = "A user removes the class of a value, puts in an invalid field, and restores the class.",
  risk = "A function that takes the type receives an invalid value.",
  result = "The call is an error that says what the object is: the type and the fields of a value are in the object, where R code does not reach, and a write of an element that R8 does not make leaves a list.",
  code = {
    Point <- misuse_type("f2")
    f <- declare_in("f2", {
      area <- generic("area", params(x, corner = arg(Point)))
      implement(area, Point) <- function(x, corner) x$x * corner$y
      area
    })
    p <- Point(x = 1, y = 2)
    forged <- unclass(p)
    forged$x <- -1
    class(forged) <- class(p)
    expect_error(f(forged, p), "is not one", class = "r8_error")
    expect_error(f(p, forged), "dput", class = "r8_error")
    copied <- list(x = -1, y = 2)
    attributes(copied) <- attributes(p)
    expect_error(f(copied, p), "is not one", class = "r8_error")
    expect_error(f(p, copied), "dput", class = "r8_error")
    expect_identical(f(p, p), 2)
    # A value that was only looked at is a value.
    same <- unclass(p)
    class(same) <- class(p)
    expect_identical(f(same, same), 2)
  }
)

scenario(
  "F3",
  "a value whose fields or class R code changed is not written or compared",
  situation = "A user changes a field of a value, its class, or its attributes with R's own functions.",
  risk = "The value is written to a file or compared as a valid one, or as a value of another type.",
  result = "A list in place of the value is an error, and a class that R code gives a value changes nothing that R8 reads.",
  code = {
    Point <- misuse_type("f3")
    Other <- declare_in(
      "f3",
      new_struct("Other", fields = list(a = type_character()))
    )
    p <- Point(x = 1, y = 2)
    forged <- unclass(p)
    forged$x <- -1
    class(forged) <- class(p)
    expect_error(r8_validate(forged), class = "r8_error")
    expect_error(r8_to_json(forged), class = "r8_error")
    expect_error(r8_hash(forged), class = "r8_error")
    expect_error(forged$x <- 1, "is not one", class = "r8_error")
    expect_match(format(forged)[[1L]], "is not one")
    renamed <- p
    class(renamed) <- c("f3::Other", "r8_value")
    attr(renamed, "r8_type") <- "f3::Other"
    expect_identical(r8_to_json(renamed), r8_to_json(p))
    expect_true(r8_equal(renamed, p))
    Holder <- declare_in("f3", new_struct("Holder", fields = list(o = Other)))
    expect_error(Holder(o = renamed), class = "r8_value_error")
  }
)

scenario(
  "F4",
  "R's functions that change a vector are refused on a value",
  situation = "A user calls names<-, length<-, or an assignment by position on a value.",
  risk = "A field is renamed, dropped, or replaced without a check.",
  result = "Each is an error, and the value is as it was.",
  code = {
    p <- misuse_type("f4")(x = 1, y = 2)
    expect_error(names(p) <- c("y", "x"), class = "r8_error")
    expect_error(length(p) <- 1L, class = "r8_error")
    expect_error(p[1] <- list(5), class = "r8_error")
    expect_error(p[[1]] <- 5, class = "r8_error")
    expect_identical(as.list(p), list(x = 1, y = 2))
  }
)

scenario(
  "F5",
  "a saved value of Rust whose bytes are damaged is an error",
  situation = "A file that holds a value of a type whose values Rust holds is damaged in the bytes that the Rust code wrote. Damage to R's own part of a file is R's to find.",
  risk = "The Rust code reads past the bytes, and R crashes.",
  result = "Reading the value is an error, or gives a value that R8 checks.",
  how = "installed",
  code = {
    lib <- install_test_packages()
    out <- run_r(
      lib,
      c(
        "suppressPackageStartupMessages(library(r8pkgR))",
        "values <- list(",
        "  Signal(name = 'abcdef', values = as.numeric(1:5)),",
        "  Resampler@KFold(k = 3L, seed = 1L),",
        "  Resampler@Fixed(train = 1:4),",
        "  Panel(range = Interval(lo = 0, hi = 1), frame = Interval(lo = 0, hi = 1), figure = Square(side = 1))",
        ")",
        "set.seed(2)",
        "read <- function(b) tryCatch({ x <- unserialize(b); for (f in names(x)) x[[f]]; R8::r8_validate(x); 'valid' }, error = function(e) 'error')",
        "res <- character()",
        "for (x in values) {",
        "  b <- serialize(x, NULL, xdr = FALSE)",
        "  # The state's bytes are the raw vectors of the stream: type 24, then the length.",
        "  at <- which(vapply(seq_len(length(b) - 7L), function(i) b[i] == as.raw(24) && all(b[i + 1:3] == 0), NA))",
        "  for (h in at) {",
        "    n <- readBin(b[h + 4:7], 'integer', endian = 'little')",
        "    if (is.na(n) || n < 1L || h + 7L + n > length(b)) next",
        "    for (i in 1:200) {",
        "      d <- b",
        "      k <- h + 7L + sample(n, sample(min(3L, n), 1L))",
        "      d[k] <- as.raw(sample(0:255, length(k), TRUE))",
        "      res <- c(res, read(d))",
        "    }",
        "  }",
        "}",
        "cat('done', length(res) >= 800, all(res %in% c('valid', 'error')), any(res == 'error'), '\\n')"
      )
    )
    expect_true(
      "done TRUE TRUE TRUE " %in% out,
      info = paste(out, collapse = "\n")
    )
  }
)
