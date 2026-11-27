# test-json.R
# ::R8::
# 2026- EDG rtemis.org

# Documents and serialization of a `Probe` value, for R-stored and
# Rust-stored values alike (helper-conformance.R).
# spec: R8/class-system#conformance-tests

test_that("a document round trips to an equal value", {
  p <- conformance_provider()
  x <- conform_value(p)
  json <- r8to_json(x)
  expect_match(
    json,
    sprintf('"$type":"%s::Probe"', p[["namespace"]]),
    fixed = TRUE
  )
  y <- r8from_json(p[["Probe"]], json)
  expect_true(r8equal(x, y))
  expect_identical(r8to_json(y), json)
  expect_identical(r8hash(y), r8hash(x))
})

test_that("a document outside the type reports its problems", {
  p <- conformance_provider()
  json <- r8to_json(conform_value(p))
  bad <- sub('"ratio":0.5', '"ratio":2', json, fixed = TRUE)
  e <- expect_error(r8from_json(p[["Probe"]], bad), class = "r8_value_error")
  expect_identical(paste(e$problems$code, e$problems$path), "above_max /ratio")
  ruled <- sub('"seed":1', '"seed":7', json, fixed = TRUE)
  expect_error(
    r8from_json(p[["Probe"]], ruled),
    "seed_below_count",
    class = "r8_value_error"
  )
})

test_that("serialize() and saveRDS() round trip to an equal value", {
  p <- conformance_provider()
  x <- conform_value(p)
  expect_true(r8equal(unserialize(serialize(x, NULL)), x))
  # Version 2 of R's format, which R has not written by default since 3.6.0,
  # holds a list of the fields.
  expect_error(
    r8validate(unserialize(serialize(x, NULL, version = 2))),
    "is not one",
    class = "r8_type_error"
  )
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path))
  saveRDS(x, path)
  y <- readRDS(path)
  expect_true(r8equal(y, x))
  expect_identical(y$weights, x$weights)
})

test_that("fields of every R8 field type round trip through JSON", {
  p <- conformance_provider()
  skip_unless(p, "every_field_type")
  x <- p[["Every"]](
    level = factor(c("hi", "lo"), levels = c("lo", "hi")),
    day = as.Date("2026-11-20"),
    at = as.POSIXct(0, origin = "1970-01-01", tz = "UTC"),
    m = matrix(c(1, 2, 3, 4), 2),
    tb = data.frame(id = c("a", "b")),
    map = c(a = 1, b = 2),
    doc = list(k = list(1, "two"))
  )
  expect_true(r8equal(r8from_json(p[["Every"]], r8to_json(x)), x))
})
