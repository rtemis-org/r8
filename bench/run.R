# run.R
# ::r8 bench::
# 2026- EDG rtemis.org

# Runs every row of the benchmark, each row and system in a fresh
# `Rscript --vanilla` under /usr/bin/time, and appends the measurements,
# with the peak resident memory of each process, to
# bench/results/<commit>.jsonl. The acceptance corpus runs first, and a
# mismatch stops the run. Usage: Rscript bench/run.R <library> <commit>,
# [<library with alloc-stats>], where the library holds the R8 release
# build, r8benchR, and the packages of syspkg.R and namespace.R, and the
# second library holds a build of R8 with the feature `alloc-stats`, for
# the memory rows; bench.sh prepares both.
# spec: R8/class-system#benchmark

args <- commandArgs(trailingOnly = TRUE)
lib <- normalizePath(args[[1L]])
sha <- args[[2L]]
lib_alloc <- if (length(args) > 2L) normalizePath(args[[3L]]) else NULL
bench_dir <- normalizePath("bench")
source(file.path(bench_dir, "namespace.R"))
source(file.path(bench_dir, "meta.R"))
out <- file.path(bench_dir, "results", paste0(sha, ".jsonl"))
dir.create(dirname(out), showWarnings = FALSE)
writeLines(
  jsonlite::toJSON(bench_meta(sha, lib), auto_unbox = TRUE, pretty = TRUE, na = "null"),
  file.path(bench_dir, "results", paste0(sha, ".meta.json"))
)
rscript <- file.path(R.home("bin"), "Rscript")
lib_path <- function(first = NULL) paste(c(first, lib, .libPaths()), collapse = .Platform$path.sep)

# The peak resident memory in bytes from the output of /usr/bin/time:
# `-l` on macOS reports bytes; `-v` on Linux reports kilobytes.
peak_rss <- function(err) {
  mac <- grep("maximum resident set size", err, value = TRUE)
  if (length(mac)) {
    return(as.numeric(sub("^\\s*([0-9]+).*$", "\\1", mac[[1L]])))
  }
  linux <- grep("Maximum resident set size", err, value = TRUE)
  if (length(linux)) {
    return(1024 * as.numeric(sub(".*:\\s*([0-9]+)\\s*$", "\\1", linux[[1L]])))
  }
  NA_real_
}

run_row <- function(row, system, mode = "", repeat_n = 1L, first_lib = NULL) {
  libs <- lib_path(first_lib)
  records <- list()
  for (r in seq_len(repeat_n)) {
    so <- tempfile()
    se <- tempfile()
    flag <- if (Sys.info()[["sysname"]] == "Darwin") "-l" else "-v"
    status <- system2(
      "/usr/bin/time",
      c(flag, rscript, "--vanilla", shQuote(file.path(bench_dir, "rows", paste0(row, ".R"))), shQuote(system), shQuote(mode)),
      stdout = so,
      stderr = se,
      env = c(paste0("R_LIBS=", libs), paste0("R8_BENCH_DIR=", bench_dir))
    )
    lines <- readLines(so, warn = FALSE)
    err <- readLines(se, warn = FALSE)
    if (!identical(status, 0L)) {
      stop(sprintf("row %s, %s %s failed:\n%s", row, system, mode, paste(c(lines, err), collapse = "\n")))
    }
    json <- grep("^\\{", lines, value = TRUE)
    rss <- jsonlite::toJSON(
      list(row = row, system = system, mode = mode, type = "process", metric = "peak_rss", value = peak_rss(err), unit = "B"),
      auto_unbox = TRUE, digits = NA
    )
    records <- c(records, json, rss)
  }
  stamp <- function(j) {
    d <- jsonlite::fromJSON(j)
    d$commit <- sha
    d$date <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
    jsonlite::toJSON(d, auto_unbox = TRUE, digits = NA)
  }
  cat(vapply(records, stamp, ""), file = out, sep = "\n", append = TRUE)
  message(sprintf("%-12s %-8s %-8s done", row, system, mode))
  invisible(records)
}

# A directory the rows' processes share, for the document of the JSON
# read's peak memory.
Sys.setenv(R8_BENCH_TMP = local({
  d <- tempfile("r8bench")
  dir.create(d)
  d
}))

systems <- c("S3", "S4", "RC", "R6", "S7", "R8", "R8 Rust")
r_systems <- setdiff(systems, "R8 Rust")

# The gate: every system passes the acceptance corpus, and every system
# stores the valid GBM's fields identically.
stored <- character()
for (s in systems) {
  d <- lapply(run_row("corpus", s), jsonlite::fromJSON)
  metric <- function(m) Filter(function(x) identical(x$metric, m), d)[[1L]]$value
  if (metric("mismatches") != 0) stop(sprintf("%s fails the acceptance corpus in %d cases", s, metric("mismatches")))
  stored[[s]] <- metric("stored_fields")
}
if (length(unique(stored)) != 1L) {
  stop("the systems store the valid GBM's fields differently:\n", paste(names(stored), stored, collapse = "\n"))
}

for (s in systems) run_row("values", s)
for (s in systems) run_row("cold", s, repeat_n = 5L)
for (s in r_systems) run_row("types", s, repeat_n = 5L)
if (!is.null(lib_alloc)) for (s in r_systems) run_row("types", s, "memory", first_lib = lib_alloc)
for (s in systems) for (m in c("", "compact")) run_row("big", s, m)
for (s in systems) for (m in c("value", "control")) run_row("peak", s, m)
for (s in c("R8", "R8 Rust")) for (m in c("json_write", "json", "json_control")) run_row("peak", s, m)
run_row("table", "R8")
for (m in c("vec", "table", "rows")) run_row("peak", "R8", m)
for (s in NAMESPACE_SYSTEMS) for (m in c("pkg", "control")) run_row("namespace", s, m)
run_row("s7_baseline", "S7")
message("results: ", out)
