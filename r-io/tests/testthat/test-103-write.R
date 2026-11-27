# test-103-write.R
# ::R8.io::
# 2026- EDG rtemis.org

# r8write_table(): what is written reads back.

test_that("a table that is written reads back as it was", {
  set.seed(1)
  n <- 200
  hole <- function(x) replace(x, sample(n, 20), NA)
  texts <- c(
    "",
    "NA",
    "a,b",
    "\"q\"",
    "x\ny",
    "\r",
    " s ",
    "café",
    "007",
    "TRUE"
  )
  T <- R8::new_table(
    "Back",
    list(
      d = R8::option(R8::type_double(infinite = TRUE, nan = TRUE)),
      i = R8::option(R8::type_integer()),
      l = R8::option(R8::type_logical()),
      s = R8::option(R8::type_character()),
      f = R8::option(R8::type_factor(texts)),
      u = R8::option(R8::type_date()),
      t = R8::option(R8::type_posixct())
    )
  )
  x <- T(
    d = hole(c(NaN, Inf, -Inf, -0, 1e300, 1e-300, 1 / 3, stats::rnorm(n - 7))),
    i = hole(sample(
      c(-.Machine$integer.max, .Machine$integer.max, 0L, 7L),
      n,
      TRUE
    )),
    l = hole(sample(c(TRUE, FALSE), n, TRUE)),
    s = hole(sample(texts, n, TRUE)),
    f = factor(hole(sample(texts, n, TRUE)), texts),
    u = hole(as.Date("1900-01-01") + sample(1e5, n)),
    t = hole(as.POSIXct(round(stats::runif(n, -1e9, 4e9), 6), tz = "UTC"))
  )
  for (ext in c(".csv", ".tsv", ".parquet")) {
    f <- tempfile(fileext = ext)
    expect_identical(r8write_table(x, f), f)
    expect_true(R8::r8equal(r8read_table(f, T), x))
    unlink(f)
  }
})

test_that("a file is written as it is read", {
  f <- tempfile(fileext = ".csv")
  on.exit(unlink(f))
  r8write_table(data.frame(a = c(1.5, NA), b = c("x", "NA")), f)
  expect_identical(readLines(f), c("a,b", "1.5,x", ",\"NA\""))
  r8write_table(R8::Raven(a = c(1L, NA)), f, delimiter = ";")
  expect_identical(r8read_table(f)$a, c(1L, NA))
  latin1 <- "caf\xe9"
  Encoding(latin1) <- "latin1"
  r8write_table(R8::Raven(a = latin1), f)
  expect_identical(r8read_table(f)$a, "café")
  refuses(r8write_table(1, f), "`x` is a table", "r8_argument_error")
  refuses(
    r8write_table(R8::Raven(a = 1), "x.txt"),
    ".csv, .tsv, or .parquet",
    "r8_argument_error"
  )
  refuses(
    r8write_table(R8::Raven(a = 1), "x.csv.gz"),
    "compression",
    "r8_argument_error"
  )
  refuses(r8write_table(R8::Raven(), f), "no column", "r8_argument_error")
  refuses(
    r8write_table(
      R8::Raven(a = 1),
      file.path(tempdir(), "no-such-dir", "x.csv")
    ),
    "is not written",
    "r8_file_error"
  )
})
