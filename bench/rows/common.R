# common.R
# ::r8 bench::
# 2026- EDG rtemis.org

# The preamble of every row: the system and mode from the command line, the
# system's types declared, and `emit()`, which writes one measurement as a
# JSON line on standard output. run.R runs each row in a fresh process.
# spec: R8/class-system#benchmark

ARGS <- commandArgs(trailingOnly = TRUE)
SYSTEM <- ARGS[[1L]]
MODE <- if (length(ARGS) > 1L) ARGS[[2L]] else ""
BENCH_DIR <- Sys.getenv("R8_BENCH_DIR", "bench")
ROW <- sub("\\.R$", "", basename(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE))))

source(file.path(BENCH_DIR, "types.R"))

# The package that holds a system's types (syspkg.R), and the packages it
# loads; R8 with Rust-stored values takes its types from r8benchR.
system_packages <- function(system) {
  switch(
    system,
    "S3" = character(),
    "S4" = "methods",
    "RC" = "methods",
    "R6" = "R6",
    "S7" = "S7",
    "R8" = "R8",
    "R8 Rust" = c("R8", "r8benchR")
  )
}

# Attaches the system's package and returns the expressions of its rows.
load_system <- function(system = SYSTEM) {
  for (p in system_packages(system)) {
    suppressPackageStartupMessages(library(p, character.only = TRUE))
  }
  if (system == "R8 Rust") {
    return(declare_system(system, globalenv()))
  }
  pkg <- paste0("benchsys", system)
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
  get(".ops", envir = asNamespace(pkg))
}

emit <- function(type, metric, value, unit) {
  cat(
    jsonlite::toJSON(
      list(row = ROW, system = SYSTEM, mode = MODE, type = type, metric = metric, value = value, unit = unit),
      auto_unbox = TRUE,
      digits = NA
    ),
    "\n",
    sep = ""
  )
}

# The median time and the R allocations of evaluating `expr` in the global
# environment, from bench::mark(), after two evaluations: the first call of
# a function byte-compiles it, which allocates once.
mark <- function(type, metric, expr, min_iterations = 200L) {
  if (is.null(expr)) {
    return(invisible(NULL))
  }
  for (i in 1:2) eval(expr, globalenv())
  b <- bench::mark(exprs = list(expr), env = globalenv(), min_iterations = min_iterations, check = FALSE, filter_gc = FALSE)
  emit(type, metric, as.numeric(b$median), "s")
  emit(type, paste0(metric, "_alloc"), as.numeric(b$mem_alloc), "B")
}

# The size of one value or type beyond the first: the size of a list of
# 101, less the size of a list of one, over 100, so objects the values share
# count once.
marginal_size <- function(make) {
  xs <- lapply(1:101, function(i) make(i))
  (as.numeric(lobstr::obj_size(xs)) - as.numeric(lobstr::obj_size(xs[1L]))) / 100
}
