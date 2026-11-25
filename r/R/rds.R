# rds.R
# ::R8::
# 2026- EDG rtemis.org

# R's reader of serialized objects checks no sum, and a damaged file may end the
# session (R 4.6.2): five of the 291 bytes of a list of two numbers, 29 of the 405
# of an integer sequence, and 31 of the 776 of two R8 values do, each changed
# alone. The checksum is read before R's reader is called.

# The bytes between the serialized object and its SHA-256.
RDS_MAGIC <- as.raw(c(0x52, 0x38, 0x53, 0x00)) # "R8S\0"

# %% r8_write_rds ----
#' Write and read an object with a checksum
#'
#' `r8_write_rds()` writes an RDS file that ends in the SHA-256 of its content, and
#' `r8_read_rds()` compares the two before R reads the object. `readRDS()` reads
#' the file too.
#'
#' R reads a serialized object without a check: a file that a disk or a transfer
#' damaged gives another object, an error of R's own, or a crash of the session.
#' `r8_read_rds()` raises an error that names the file.
#'
#' @param x An object: an R8 value, or any other.
#' @param file Character: The path of the file.
#' @param compress Logical or Character \{"gzip", "bzip2", "xz"\}: The
#'   compression, as that of `saveRDS()`.
#'
#' @return `r8_write_rds()`: `file`, invisibly. `r8_read_rds()`: The object. A file
#'   that is damaged, or that `r8_write_rds()` did not write, raises `r8_file_error`.
#'
#' @examples
#' op <- options(r8.namespace = "demo")
#' Point <- new_struct("Point", fields = list(x = type_double(), y = type_double()))
#' file <- tempfile(fileext = ".rds")
#' r8_write_rds(Point(x = 1, y = 2), file)
#' r8_read_rds(file)
#' unlink(file)
#' options(op)
#'
#' @export
r8_write_rds <- function(x, file, compress = TRUE) {
  check_path(file)
  open <- if (isFALSE(compress)) {
    base::file
  } else if (isTRUE(compress) || identical(compress, "gzip")) {
    gzfile
  } else if (identical(compress, "bzip2")) {
    bzfile
  } else if (identical(compress, "xz")) {
    xzfile
  } else {
    r8_abort(
      "`compress` is TRUE, FALSE, \"gzip\", \"bzip2\", or \"xz\".",
      class = "r8_type_error"
    )
  }
  bytes <- serialize(x, NULL)
  con <- open(file, "wb")
  on.exit(close(con))
  for (part in list(bytes, RDS_MAGIC, r8_sha256(bytes))) {
    writeBin(part, con)
  }
  invisible(file)
} # /R8::r8_write_rds


# %% r8_read_rds ----
#' @rdname r8_write_rds
#' @export
r8_read_rds <- function(file) {
  check_path(file)
  if (!file.exists(file) || dir.exists(file)) {
    r8_abort("`", file, "` is not a file.", class = "r8_file_error")
  }
  bytes <- read_bytes(file)
  n <- length(bytes)
  tail <- length(RDS_MAGIC) + 32L
  at <- seq.int(n - tail + 1L, length.out = length(RDS_MAGIC))
  if (n <= tail || !identical(bytes[at], RDS_MAGIC)) {
    r8_abort(
      "`",
      file,
      "` has no checksum: it is damaged at its end, or r8_write_rds() did not ",
      "write it. Restore it from a copy, or write it again; readRDS() reads a ",
      "file of saveRDS() without a check.",
      class = "r8_file_error"
    )
  }
  content <- bytes[seq_len(n - tail)]
  if (!identical(r8_sha256(content), bytes[seq.int(n - 31L, n)])) {
    r8_abort(
      "`",
      file,
      "` is damaged: its content is not that of its checksum. Restore it from a ",
      "copy, or write it again.",
      class = "r8_file_error"
    )
  }
  unserialize(content)
} # /R8::r8_read_rds


# %% check_path ----
#' Check the path of a file
#'
#' @param file The path.
#'
#' @return `NULL`, invisibly.
#'
#' @keywords internal
#' @noRd
check_path <- function(file) {
  if (
    !is.character(file) || length(file) != 1L || is.na(file) || !nzchar(file)
  ) {
    r8_abort("`file` is one path.", class = "r8_type_error")
  }
  invisible(NULL)
} # /R8::check_path


# %% read_bytes ----
#' The bytes of a file, decompressed
#'
#' @param file Character: The path.
#'
#' @return Raw: The bytes that a damaged compression gives before its end.
#'
#' @keywords internal
#' @noRd
read_bytes <- function(file) {
  parts <- list()
  tryCatch(
    {
      # gzfile() reads each compression, and a file without one.
      con <- gzfile(file, "rb")
      on.exit(close(con))
      repeat {
        part <- suppressWarnings(readBin(con, "raw", 1e7))
        if (length(part) == 0L) {
          break
        }
        parts[[length(parts) + 1L]] <- part
      }
    },
    error = function(e) NULL
  )
  do.call(c, c(list(raw()), parts))
} # /R8::read_bytes
