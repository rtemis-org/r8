# helper-declare.R
# ::R8::
# 2026- EDG rtemis.org

# Evaluates declarations outside any package namespace, so that they belong
# to the namespace `ns` through the option `r8.namespace`.
declare_in <- function(ns, expr) {
  op <- options(r8.namespace = ns)
  on.exit(options(op))
  # Outside any namespace, with the caller's variables visible.
  env <- list2env(
    as.list(parent.frame()),
    envir = new.env(parent = globalenv())
  )
  eval(substitute(expr), envir = env)
}

# The RFC 8785 text of a fixture file in tests/testthat/fixtures.
fixture_canonical <- function(file) {
  text <- paste(
    readLines(test_path("fixtures", file), warn = FALSE),
    collapse = "\n"
  )
  R8:::r8_canonical_json(text)
}
