# peak.R
# ::r8 bench::
# 2026- EDG rtemis.org

# A process for the peak resident memory that run.R reads with
# /usr/bin/time -l. Mode "value" builds one GBM of 10^6 weights and feature
# indices; mode "control" loads the same packages and builds the same input
# only. The R8 modes "vec" and "table" build 10^6 values of a two-double
# struct as `vec(S)` and as `type_table(S)`, and "rows" their input only.
# spec: R8/class-system#benchmark

source(file.path(Sys.getenv("R8_BENCH_DIR", "bench"), "rows", "common.R"))
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
