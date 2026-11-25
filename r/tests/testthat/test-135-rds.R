# test-135-rds.R
# ::R8::
# 2026- EDG rtemis.org

# Files with a checksum: `r8_write_rds()` and `r8_read_rds()`.

test_that("an object is read as it was saved, with each compression", {
  Point <- declare_in("rds1", {
    new_struct("Point", fields = list(x = type_double(), y = type_double()))
  })
  file <- tempfile(fileext = ".rds")
  on.exit(unlink(file))
  for (compress in list(TRUE, FALSE, "gzip", "bzip2", "xz")) {
    for (x in list(
      Point(x = 1, y = 2),
      list(a = 1, Point(x = 3, y = 4)),
      NULL,
      1:1e5
    )) {
      expect_identical(r8_write_rds(x, file, compress), file)
      expect_identical(r8_read_rds(file), x)
      expect_identical(readRDS(file), x)
    }
  }
})

test_that("a file with a damaged byte is an error, whichever byte it is", {
  Point <- declare_in("rds2", {
    new_struct("Point", fields = list(x = type_double(), y = type_double()))
  })
  file <- tempfile(fileext = ".rds")
  on.exit(unlink(file))
  x <- Point(x = 1, y = 2)
  for (compress in list(FALSE, "gzip", "bzip2", "xz")) {
    r8_write_rds(x, file, compress)
    whole <- readBin(file, "raw", file.size(file))
    read <- function(bytes) {
      writeBin(bytes, file)
      tryCatch(r8_read_rds(file), r8_file_error = function(e) "refused")
    }
    for (i in seq_along(whole)) {
      bytes <- whole
      bytes[[i]] <- xor(bytes[[i]], as.raw(0x11))
      out <- read(bytes)
      # A byte of a compression's header may have no part in the content.
      expect_true(identical(out, "refused") || identical(out, x), info = i)
    }
    for (n in c(0L, 1L, 20L, length(whole) %/% 2L, length(whole) - 1L)) {
      out <- read(whole[seq_len(n)])
      expect_true(identical(out, "refused") || identical(out, x), info = n)
    }
  }
})

test_that("a file that r8_write_rds() did not write is refused, with what reads it", {
  file <- tempfile(fileext = ".rds")
  on.exit(unlink(file))
  saveRDS(list(1), file)
  expect_error(
    r8_read_rds(file),
    "readRDS\\(\\) reads",
    class = "r8_file_error"
  )
  expect_error(
    r8_read_rds(tempfile()),
    "is not a file",
    class = "r8_file_error"
  )
  expect_error(r8_read_rds(tempdir()), "is not a file", class = "r8_file_error")
  expect_error(r8_read_rds(1), class = "r8_type_error")
  expect_error(r8_write_rds(1, NA_character_), class = "r8_type_error")
  expect_error(r8_write_rds(1, file, "zip"), class = "r8_type_error")
})
