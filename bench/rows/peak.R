# peak.R
# ::r8 bench::
# 2026- EDG rtemis.org

# A process for the peak resident memory that run.R reads with
# /usr/bin/time -l. Mode "value" builds one GBM of 10^6 weights and feature
# indices; mode "control" loads the same packages and builds the same input
# only. The R8 modes "vec" and "table" build 10^6 values of a two-double
# struct as `vec(S)` and as `type_table(S)`, and "rows" their input only.
# Mode "types" declares 1,000 GBM types, and "types_control" loads the
# system's packages only.
# spec: R8/class-system#benchmark

source(file.path(Sys.getenv("R8_BENCH_DIR", "bench"), "rows", "common.R"))
if (MODE %in% c("types", "types_control")) {
  # 1,000 GBM types declared in the session, against the system's packages
  # alone: memory in R and, for R8, in its registry in Rust.
  for (p in system_packages(SYSTEM)) suppressPackageStartupMessages(library(p, character.only = TRUE))
  if (SYSTEM == "R8") options(r8.namespace = "many")
  if (MODE == "types") kept <- lapply(1:1000, function(i) gbm_type(SYSTEM, paste0("GBMP_", i)))
  invisible(gc())
  emit("process", "done", 1, "")
  quit(save = "no")
}
ops <- load_system()
n <- 1e6L
if (MODE %in% c("value", "control")) {
  args <- gbm_args(n)
  if (MODE == "value") x <- do.call(GBM, args)
} else {
  source(file.path(BENCH_DIR, "table.R"))
  input <- table_input(n)
  if (MODE == "vec") x <- table_as_vec(input)
  if (MODE == "table") x <- table_as_table(input)
}
invisible(gc())
emit("process", "done", 1, "")
