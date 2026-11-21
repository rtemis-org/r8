# types.R
# ::r8 bench::
# 2026- EDG rtemis.org

# The default mode: the time to declare 200 GBM types in one process; run.R
# repeats it in fresh processes for the median, on the release build. Mode
# "memory": the memory 100 declarations add, over 100, in R (gc() cell
# counts) and, for R8, in Rust (the counting allocator of the build with
# the feature `alloc-stats`, r8_alloc_stats()); run.R runs it on that build.
# spec: R8/class-system#benchmark

source(file.path(Sys.getenv("R8_BENCH_DIR", "bench"), "rows", "common.R"))
for (p in system_packages(SYSTEM)) suppressPackageStartupMessages(library(p, character.only = TRUE))
if (SYSTEM == "R8") options(r8.namespace = "many")

if (MODE != "memory") {
  t <- system.time(types <- lapply(1:200, function(i) gbm_type(SYSTEM, paste0("GBMT_", i))))[["elapsed"]]
  emit("GBM", "declare_200", t, "s")
} else {
  # R memory in bytes: a cons cell takes 56 bytes and a vector cell 8.
  r_bytes <- function() {
    invisible(gc())
    g <- gc()
    56 * g["Ncells", "used"] + 8 * g["Vcells", "used"]
  }
  native_bytes <- function() {
    if (SYSTEM != "R8") return(0)
    s <- R8:::r8_alloc_stats()
    if (is.null(s)) stop("the memory rows run on a build of R8 with the feature `alloc-stats`")
    s[[1L]]
  }
  # One declaration first, so that one-time allocations fall before the
  # measured ones.
  invisible(gbm_type(SYSTEM, "GBMS_0"))
  r0 <- r_bytes()
  n0 <- native_bytes()
  kept <- lapply(1:100, function(i) gbm_type(SYSTEM, paste0("GBMS_", i)))
  r1 <- r_bytes()
  n1 <- native_bytes()
  emit("GBM", "memory_type_r", (r1 - r0) / 100, "B")
  emit("GBM", "memory_type_native", (n1 - n0) / 100, "B")
}
