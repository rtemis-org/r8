# test-104-parquet.R
# ::R8.io::
# 2026- EDG rtemis.org

# Parquet files: those of other writers, typed values under a declared type, and
# clock readings of a time zone.

fixture <- function(name) testthat::test_path("fixtures", name)

test_that("a Parquet file of arrow reads as the types it stores", {
  x <- r8_read_table(fixture("arrow_zstd.parquet"))
  expect_s3_class(x, "r8_table")
  df <- R8::r8_to_data_frame(x)
  expect_identical(df$i, c(1L, NA, 3L))
  expect_identical(df$d, c(1.5, NaN, NA))
  # A string is a quoted text: "" is a text.
  expect_identical(df$s, c("a", NA, ""))
  expect_identical(df$b, c(TRUE, NA, FALSE))
  expect_identical(df$dt, as.Date(c("2020-01-31", NA, "1969-12-31")))
  # The factor of R is a text without a type, and INT64 is an integer where it fits.
  expect_identical(df$f, c("lo", "hi", NA))
  expect_identical(df$i64, c(1L, 2L, NA))
  expect_identical(df$big, c(2^40, 1, 2))
  ts <- as.POSIXct("2020-01-31 10:00:00", tz = "UTC")
  expect_identical(df$ts, ts + c(0, NA, 0.5))
  # A timestamp without a zone is the clock reading of `time_zone`.
  expect_identical(df$naive[[1L]], ts)
  ny <- r8_read_table(
    fixture("arrow_zstd.parquet"),
    time_zone = "America/New_York"
  )
  expect_identical(R8::r8_to_data_frame(ny)$naive[[1L]], ts + 5 * 3600)
})

test_that("a value of another type is reported by its text and recoded", {
  T <- R8::new_table(
    "Counts",
    columns = list(
      d = R8::option(R8::type_integer()),
      dec = R8::option(R8::type_integer())
    ),
    open = TRUE
  )
  e <- expect_error(
    r8_read_table(fixture("arrow_zstd.parquet"), T),
    class = "r8_value_error"
  )
  expect_match(conditionMessage(e), "\"1.5\"", fixed = TRUE)
  expect_match(conditionMessage(e), "\"1.25\"", fixed = TRUE)
  expect_match(
    conditionMessage(e),
    "recode_in = list(d = c(\"1.5\" = , \"NaN\" = ), dec = c(\"1.25\" = ))",
    fixed = TRUE
  )
  x <- r8_read_table(
    fixture("arrow_zstd.parquet"),
    T,
    recode_in = list(d = c("1.5" = 2L), dec = c("1.25" = 1L)),
    na_strings_in = list(d = "NaN")
  )
  expect_identical(x$d, c(2L, NA, NA))
  expect_identical(x$dec, c(1L, NA, 3L))
  report <- r8_check_file(fixture("arrow_zstd.parquet"), T)
  expect_identical(report$unread$text, c("1.5", "NaN", "1.25"))
})

test_that("decimals and clock readings of duckdb read", {
  df <- R8::r8_to_data_frame(r8_read_table(fixture("duck_flat.parquet")))
  expect_identical(df$dec, c(2.5, -0.01))
  expect_identical(df$d0, c(7L, NA))
  expect_identical(
    df$ts,
    as.POSIXct(c("2020-01-01 10:00:00", NA), tz = "UTC")
  )
})

test_that("what a Parquet file cannot hold, and the arguments of CSV, are refused", {
  refuses(
    r8_read_table(fixture("duck_nested.parquet")),
    "`lst`",
    "r8_file_error"
  )
  refuses(
    r8_read_table(fixture("duck_flat.parquet"), delimiter = ";"),
    "`delimiter` is an argument of a CSV or TSV file",
    "r8_argument_error"
  )
  refuses(
    r8_read_table(fixture("duck_flat.parquet"), encoding = "latin1"),
    "`encoding` is an argument of a CSV or TSV file",
    "r8_argument_error"
  )
  refuses(
    r8_read_table(fixture("duck_flat.parquet"), time_zone = "Mars/Olympus"),
    "\"Mars/Olympus\" is not one",
    "r8_argument_error"
  )
  f <- file_of("a,b", "1,2")
  refuses(
    r8_read_table(f, format = "parquet"),
    "does not read",
    "r8_file_error"
  )
})

test_that("a CSV date-time without an offset is the clock reading of the zone", {
  f <- file_of(
    "at",
    "2024-06-01 08:15:00",
    "2024-03-10 02:30:00"
  )
  utc <- r8_read_table(f)$at
  expect_identical(
    utc,
    as.POSIXct(c("2024-06-01 08:15:00", "2024-03-10 02:30:00"), tz = "UTC")
  )
  T <- R8::new_table("At", columns = list(at = R8::type_posixct()))
  # New York's clock skips 02:30 on 2024-03-10.
  e <- expect_error(
    r8_read_table(f, T, time_zone = "America/New_York"),
    class = "r8_value_error"
  )
  expect_match(conditionMessage(e), "\"2024-03-10 02:30:00\"", fixed = TRUE)
})

test_that("nanoparquet reads what R8.io writes", {
  skip_if_not_installed("nanoparquet")
  x <- R8::Raven(
    l = c(TRUE, NA),
    i = c(1L, NA),
    d = c(NaN, NA),
    s = c("", NA),
    f = factor(c("b", NA), c("a", "b")),
    u = as.Date(c("2020-01-31", NA)),
    t = as.POSIXct(c("2020-01-31 10:00:00.25", NA), tz = "UTC")
  )
  f <- tempfile(fileext = ".parquet")
  on.exit(unlink(f))
  r8_write_table(x, f)
  y <- as.data.frame(nanoparquet::read_parquet(f))
  expect_identical(y$l, c(TRUE, NA))
  expect_identical(y$i, c(1L, NA))
  expect_identical(y$d, c(NaN, NA))
  expect_identical(y$s, c("", NA))
  expect_identical(y$f, c("b", NA))
  expect_equal(y$u, as.Date(c("2020-01-31", NA)))
  expect_equal(as.numeric(y$t), as.numeric(x$t))
})
