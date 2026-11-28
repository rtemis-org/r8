# helper-files.R
# ::R8.io::
# 2026- EDG rtemis.org

# A file of lines, removed with the test.
file_of <- function(..., ext = ".csv", env = parent.frame()) {
  file <- tempfile(fileext = ext)
  con <- file(file, "wb")
  writeLines(enc2utf8(c(...)), con, useBytes = TRUE)
  close(con)
  do.call(on.exit, list(call("unlink", file), add = TRUE), envir = env)
  file
}

labs_type <- function() {
  R8::new_table_type(
    "Labs",
    columns = list(
      mrn = R8::column(R8::type_character(), unique = TRUE),
      age = R8::type_integer(min = 0L, max = 120L),
      seen = R8::option(R8::type_date()),
      stage = R8::type_factor(c("I", "II")),
      lab = R8::type_double(min = 0)
    ),
    rules = list(young = R8::rule(age < 100))
  )
}

labs_lines <- c(
  "mrn,age,seen,stage,lab",
  "00123,40,2024-01-31,I,1.5",
  "00456,52,2024-02-30,II,<0.1",
  "00789,130,,II,<0.1",
  "00790,61,NA,III,hemol"
)

refuses <- function(expr, text, class = "r8_value_error") {
  testthat::expect_error(expr, text, fixed = TRUE, class = class)
}
