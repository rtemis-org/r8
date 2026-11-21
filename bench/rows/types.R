# types.R
# ::r8 bench::
# 2026- EDG rtemis.org

# The time to declare 200 GBM types in one process, and the memory per type:
# the R memory that 100 declarations add, from gc() cell counts, over 100;
# run.R adds the peak resident memory of 1,000 declarations, which counts
# R8's registry in Rust (peak.R).
# spec: R8/class-system#benchmark

source(file.path(Sys.getenv("R8_BENCH_DIR", "bench"), "rows", "common.R"))
for (p in system_packages(SYSTEM)) suppressPackageStartupMessages(library(p, character.only = TRUE))
if (SYSTEM == "R8") options(r8.namespace = "many")

t <- system.time(types <- lapply(1:200, function(i) gbm_type(SYSTEM, paste0("GBMT_", i))))[["elapsed"]]
emit("GBM", "declare_200", t, "s")

# R memory in bytes: a cons cell takes 56 bytes and a vector cell 8.
r_bytes <- function() {
  invisible(gc())
  g <- gc()
  56 * g["Ncells", "used"] + 8 * g["Vcells", "used"]
}
before <- r_bytes()
kept <- lapply(1:100, function(i) gbm_type(SYSTEM, paste0("GBMS_", i)))
emit("GBM", "memory_type", (r_bytes() - before) / 100, "B")
