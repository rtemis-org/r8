# test-public.R
# ::R8::
# 2026- EDG rtemis.org

# The examples of the help pages and the test packages use R8's functions, and
# none of its internals.

INTERNALS <- c(
  "an attribute of a value or a type" = "attr\\([^)]*\"r8_",
  "a function of R8's namespace" = "R8:::",
  "a value without its class" = "unclass\\(",
  "a class of every value or type" = "\"r8_(value|type)\""
)

# The lines of `text` that use an internal of R8, with the reason.
internals_used <- function(text, where) {
  unlist(lapply(names(INTERNALS), function(why) {
    at <- grep(INTERNALS[[why]], text)
    if (length(at) > 0L) {
      paste0(where, ": ", why, ": ", trimws(text[at]))
    }
  }))
}

# The lines of roxygen2 examples in an R file.
example_lines <- function(path) {
  inside <- FALSE
  keep <- vapply(
    readLines(path, warn = FALSE),
    function(l) {
      if (grepl("^#' @", l)) {
        inside <<- grepl("^#' @examples", l)
        return(FALSE)
      }
      inside <<- inside && startsWith(l, "#'")
      inside
    },
    NA
  )
  names(keep)[keep]
}

test_that("the patterns find each internal", {
  used <- internals_used(
    c(
      "attr(x, \"r8_variant\")",
      "R8:::type_ir(x)",
      "unclass(x)$a",
      "inherits(x, \"r8_value\")",
      "r8_variant(x)"
    ),
    "here"
  )
  expect_length(used, 4L)
})

test_that("examples and test packages use R8's public functions", {
  sources <- list.files(test_path("..", "..", "R"), "[.]R$", full.names = TRUE)
  skip_if(length(sources) == 0L, "the package's sources are not here")
  found <- unlist(lapply(sources, function(f) {
    internals_used(example_lines(f), basename(f))
  }))
  packages <- list.files(
    test_path("packages"),
    "[.]R$",
    recursive = TRUE,
    full.names = TRUE
  )
  found <- c(
    found,
    unlist(lapply(packages, function(f) {
      internals_used(readLines(f, warn = FALSE), basename(f))
    }))
  )
  expect_identical(as.character(found), character())
})
