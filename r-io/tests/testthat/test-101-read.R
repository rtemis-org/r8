# test-101-read.R
# ::R8.io::
# 2026- EDG rtemis.org

# r8read_table(): what is read, what is inferred, and what is refused. The tests of
# the crate rtemis-r8-io have the texts of each type.

test_that("a file without a type has the types that all its cells have", {
  f <- file_of(
    "mrn,age,x,flag,seen,at,note,none",
    "00123,40,1,TRUE,2024-01-31,2024-01-31T10:00:00Z,\"a, b\",",
    "00456,,2.5,false,2024-02-29,2024-01-31 11:00:00+01:00,,NA",
    "00789,NA,,,,,\"\","
  )
  x <- r8read_table(f)
  expect_identical(
    as.list(x),
    list(
      mrn = c("00123", "00456", "00789"),
      age = c(40L, NA, NA),
      x = c(1, 2.5, NA),
      flag = c(TRUE, FALSE, NA),
      seen = as.Date(c("2024-01-31", "2024-02-29", NA)),
      at = as.POSIXct(c(1706695200, 1706695200, NA), tz = "UTC"),
      note = c("a, b", NA, ""),
      none = c(NA, NA, NA)
    )
  )
  x <- r8read_table(f, infer_types = "none")
  expect_true(all(vapply(as.list(x), is.character, NA)))
  expect_identical(x$age, c("40", NA, NA))
  # A cell that does not fit leaves the column the text of the file.
  f <- file_of("a,b,c", "1,TRUE,2024-01-31", "2.50,FALSE,2024-02-30", "x,1,")
  x <- r8read_table(f)
  expect_identical(
    as.list(x),
    list(
      a = c("1", "2.50", "x"),
      b = c("TRUE", "FALSE", "1"),
      c = c("2024-01-31", "2024-02-30", NA)
    )
  )
  expect_identical(dim(r8read_table(file_of("a,b"))), c(0L, 2L))
})

test_that("a file is read as a declared type", {
  T <- labs_type()
  f <- file_of(labs_lines[1:2])
  x <- r8read_table(f, T)
  expect_true(inherits(x, T))
  expect_identical(x$mrn, "00123")
  expect_identical(x$stage, factor("I", c("I", "II")))
  expect_identical(x$lab, 1.5)
  # A declared number reads a text that inference leaves as it is.
  T2 <- R8::new_table_type(
    "T2",
    list(a = R8::type_double(), b = R8::type_integer())
  )
  x <- r8read_table(file_of("a,b", "007,+5", "1e2,40.0"), T2)
  expect_identical(as.list(x), list(a = c(7, 100), b = c(5L, 40L)))
  # The other columns of an open type are inferred.
  T3 <- R8::new_table_type("T3", list(a = R8::type_character()), open = TRUE)
  x <- r8read_table(file_of("a,b", "1,2"), T3)
  expect_identical(as.list(x), list(a = "1", b = 2L))
  expect_identical(
    r8read_table(file_of("a,b", "1,2"), T3, infer_types = "none")$b,
    "2"
  )
  # A type_table() gives a raven that satisfies it.
  x <- r8read_table(
    f,
    R8::type_table(list(lab = R8::type_double()), open = TRUE)
  )
  expect_true(inherits(x, R8::Raven))
  refuses(
    r8read_table(
      f,
      R8::type_table(list(age = R8::type_double(max = 5)), open = TRUE)
    ),
    "must be at most 5"
  )
  refuses(r8read_table(file_of("a,c", "1,2"), T2), "c")
  # A factor without levels has those of its cells, in code-point order.
  T4 <- R8::new_table_type("T4", list(g = R8::type_factor()))
  expect_identical(
    levels(r8read_table(file_of("g", "b", "a", "B"), T4)$g),
    c("B", "a", "b")
  )
})

test_that("texts that do not read are listed once, with the call that converts them", {
  T <- labs_type()
  f <- file_of(labs_lines)
  e <- expect_error(r8read_table(f, T), class = "r8_value_error")
  text <- conditionMessage(e)
  expect_match(text, "5 cells of 4 rows", fixed = TRUE)
  expect_match(
    text,
    "lab    \"<0.1\"        2 cells, first in row 2",
    fixed = TRUE
  )
  expect_match(
    text,
    "recode_in = list(seen = c(\"2024-02-30\" = ), stage = c(\"III\" = ), lab = c(\"<0.1\" = , \"hemol\" = ))",
    fixed = TRUE
  )
  expect_match(text, "r8check_file(f, T) gives a report.", fixed = TRUE)
  expect_true(inherits(e[["report"]], "R8io::FileReport"))
  codes <- list(
    seen = c("2024-02-30" = as.Date("2024-03-01")),
    stage = c("III" = "II"),
    lab = c("<0.1" = 0.1)
  )
  # What reads is then checked against the type.
  refuses(
    r8read_table(f, T, recode_in = codes, na_strings_in = list(lab = "hemol")),
    "must be at most 120"
  )
  x <- r8read_table(
    file_of(labs_lines[-c(4, 5)]),
    T,
    recode_in = codes,
    na_strings_in = list(lab = "hemol")
  )
  expect_identical(x$lab, c(1.5, 0.1))
  expect_identical(x$seen, as.Date(c("2024-01-31", "2024-03-01")))
  # A text of the next file is another decision.
  refuses(
    r8read_table(
      file_of(labs_lines[1:3], "00900,3,,I,<0.2"),
      T,
      recode_in = codes
    ),
    "\"<0.2\"  1 cell, first in row 3"
  )
})

test_that("a value that a text reads as is one that the column admits", {
  T <- labs_type()
  f <- file_of(labs_lines)
  argument <- function(recode, text, ...) {
    refuses(
      r8read_table(f, T, recode_in = recode, ...),
      text,
      "r8_argument_error"
    )
  }
  argument(list(lab = c("<0.1" = -1)), "must be at least 0; found -1")
  argument(
    list(lab = c("<0.1" = "low")),
    "a value of another type for \"<0.1\""
  )
  argument(list(lab = c("1.5" = 2)), "which reads as it is")
  argument(list(lab = c("<0.1" = NA)), "na_strings_in = list(lab = \"<0.1\")")
  argument(list(lab = c("NA" = 1)), "makes missing")
  argument(
    list(lab = c("hemol" = 1)),
    "makes missing",
    na_strings_in = list(lab = "hemol")
  )
  argument(list(lab = c("x" = 1, "x" = 2)), "two values")
  argument(list(lab = 1), "values named by their texts")
  argument(list(1), "by the names of columns")
  argument(list(labs = c("x" = 1)), "`labs`, which is not a column of the file")
  argument(list(stage = c("III" = "IV")), "another type")
  refuses(
    r8read_table(f, recode_in = list(lab = c("<0.1" = 1))),
    "whose type is not declared",
    "r8_argument_error"
  )
  # A text that does not occur is no error.
  expect_identical(
    nrow(r8read_table(
      file_of(labs_lines[1:2]),
      T,
      recode_in = list(lab = c("x" = 1))
    )),
    1L
  )
})

test_that("missing cells are the texts that are given", {
  f <- file_of("a,s", "-99,x", "1,-99", ",", "NA,NA", "unk,\"NA\"", "2,\"\"")
  x <- r8read_table(
    f,
    na_strings = c("", "NA", "unk"),
    na_strings_in = list(a = "-99")
  )
  expect_identical(x$a, c(NA, 1L, NA, NA, NA, 2L))
  expect_identical(x$s, c("x", "-99", NA, NA, "NA", ""))
  x <- r8read_table(f, na_strings_in = list(s = c("NA", "")))
  expect_identical(x$s, c("x", "-99", NA, NA, NA, NA))
  expect_identical(r8read_table(f, na_strings = character())$s[[4]], "NA")
  T <- R8::new_table_type(
    "M",
    list(a = R8::type_integer(), s = R8::option(R8::type_character()))
  )
  refuses(
    r8read_table(f, T, na_strings_in = list(a = c("-99", "unk"))),
    "must be a value"
  )
  refuses(
    r8read_table(f, na_strings = NA),
    "`na_strings`",
    "r8_argument_error"
  )
  refuses(
    r8read_table(f, na_strings_in = list(z = "1")),
    "`z`, which is not a column of the file",
    "r8_argument_error"
  )
  refuses(
    r8read_table(f, na_strings_in = c(a = "1")),
    "by the names of columns",
    "r8_argument_error"
  )
})

test_that("a malformed file is an error that gives the line", {
  broken <- function(lines, text, ...) {
    refuses(r8read_table(file_of(lines), ...), text, "r8_file_error")
  }
  broken(c("a,b", "1,\"2"), "cell in line 2 is not closed")
  broken(c("a,b", "\"1\"x,2"), "line 2 has characters after its closing quote")
  broken(c("a,a", "1,2"), "two columns are named `a`")
  broken(c("a,", "1,2"), "column 2 has no name")
  broken(character(), "no header")
  refuses(
    r8read_table(file_of("a,b", "1,2", "3", "4,5,6")),
    "2 lines with another number of cells than the 2 of the header: line 3 has 1, line 4 has 3."
  )
  f <- tempfile(fileext = ".csv")
  on.exit(unlink(f))
  writeBin(c(charToRaw("a\ncaf"), as.raw(0xe9), charToRaw("\n")), f)
  refuses(r8read_table(f), "byte 4 of line 2 is not valid", "r8_file_error")
  expect_identical(r8read_table(f, encoding = "latin1")$a, "café")
  writeBin(
    c(
      as.raw(c(0xff, 0xfe)),
      iconv("a\n€\n", "UTF-8", "UTF-16LE", toRaw = TRUE)[[1]]
    ),
    f
  )
  expect_identical(r8read_table(f)$a, "€")
  writeBin(charToRaw("a\n\x80\n"), f)
  expect_identical(r8read_table(f, encoding = "windows-1252")$a, "€")
})

test_that("the format is the name's, or the argument's", {
  expect_identical(
    dim(r8read_table(file_of("a\tb", "1\t2", ext = ".tsv"))),
    c(1L, 2L)
  )
  f <- file_of("a;b", "1;2", ext = ".txt")
  refuses(
    r8read_table(f),
    "does not end in .csv, .tsv, or .parquet",
    "r8_argument_error"
  )
  expect_identical(
    as.list(r8read_table(f, format = "csv", delimiter = ";")),
    list(a = 1L, b = 2L)
  )
  f <- tempfile(fileext = ".csv.gz")
  on.exit(unlink(f))
  con <- gzfile(f, "w")
  writeLines(c("a,b", "1,2"), con)
  close(con)
  expect_identical(as.list(r8read_table(f)), list(a = 1L, b = 2L))
  argument <- function(text, ...) {
    refuses(r8read_table(...), text, "r8_argument_error")
  }
  argument("`delimiter` is one ASCII character", "x.csv", delimiter = ";;")
  argument("`delimiter`", "x.csv", delimiter = "é")
  argument("`delimiter`", "x.csv", delimiter = "\"")
  argument(
    "`format` is \"csv\", \"tsv\", or \"parquet\".",
    "x.csv",
    format = "xls"
  )
  argument(
    "`infer_types` is \"strict\" or \"none\".",
    "x.csv",
    infer_types = TRUE
  )
  argument("`encoding` is ", "x.csv", encoding = "utf8")
  argument("`file` is one path.", c("a", "b"))
  argument("is a table type", "x.csv", type = 1)
  argument("is a table type", "x.csv", type = R8::vec(R8::type_double()))
  refuses(r8read_table("no-such.csv"), "is not a file", "r8_file_error")
})

test_that("a long read ends at a time limit", {
  skip_on_cran()
  f <- tempfile(fileext = ".csv")
  on.exit(unlink(f))
  n <- 1e6
  writeLines(c("a,b,c", paste(seq_len(n), "x", stats::runif(n), sep = ",")), f)
  read <- function() {
    setTimeLimit(elapsed = 0.01, transient = TRUE)
    on.exit(setTimeLimit())
    r8read_table(f)
  }
  expect_error(read(), "time limit")
  expect_identical(nrow(r8read_table(f)), as.integer(n))
})
