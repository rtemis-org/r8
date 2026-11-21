# meta.R
# ::r8 bench::
# 2026- EDG rtemis.org

# Writes the provenance of a run, bench/results/<commit>.meta.json: the
# commit, the date, the machine, the operating system, and the versions of
# R, rustc, and the packages the benchmark loads. run.R calls it at the
# start of a run; `just bench-publish` copies it with the results.
# Usage: Rscript bench/meta.R <commit> [<library>].
# spec: R8/class-system#benchmark

bench_meta <- function(sha, lib = NULL) {
  if (!is.null(lib)) .libPaths(c(lib, .libPaths()))
  run <- function(cmd, args) {
    tryCatch(trimws(system2(cmd, args, stdout = TRUE, stderr = FALSE))[1L], error = function(e) NA_character_)
  }
  darwin <- Sys.info()[["sysname"]] == "Darwin"
  pkgs <- c("R8", "S7", "R6", "bench", "lobstr", "rtemis.core", "jsonlite")
  list(
    commit = sha,
    date = format(Sys.time(), "%Y-%m-%d"),
    machine = if (darwin) run("sysctl", c("-n", "machdep.cpu.brand_string")) else run("uname", "-m"),
    memory_gb = if (darwin) as.numeric(run("sysctl", c("-n", "hw.memsize"))) / 2^30 else NA_real_,
    os = if (darwin) paste("macOS", run("sw_vers", "-productVersion")) else run("uname", c("-sr")),
    r = paste(R.version$major, R.version$minor, sep = "."),
    rustc = sub("^rustc ([^ ]+).*$", "\\1", run("rustc", "--version")),
    packages = as.list(vapply(pkgs, function(p) {
      tryCatch(as.character(utils::packageVersion(p)), error = function(e) NA_character_)
    }, ""))
  )
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  meta <- bench_meta(args[[1L]], if (length(args) > 1L) args[[2L]])
  out <- file.path("bench", "results", paste0(args[[1L]], ".meta.json"))
  dir.create(dirname(out), showWarnings = FALSE)
  writeLines(jsonlite::toJSON(meta, auto_unbox = TRUE, pretty = TRUE, na = "null"), out)
  message("provenance: ", out)
}
