# types.R
# ::r8 bench::
# 2026- EDG rtemis.org

# Memory per type, the marginal size of the 2nd to 101st GBM type, and the
# time to declare 200 GBM types in one process; run.R repeats the row in
# fresh processes for the median.
# spec: R8/class-system#benchmark

source(file.path(Sys.getenv("R8_BENCH_DIR", "bench"), "rows", "common.R"))
invisible(load_system())
if (SYSTEM == "R8") options(r8.namespace = "many")

t <- system.time(types <- lapply(1:200, function(i) gbm_type(SYSTEM, paste0("GBMT_", i))))[["elapsed"]]
emit("GBM", "declare_200", t, "s")
emit("GBM", "memory_type", marginal_size(function(i) {
  name <- paste0("GBMS_", i)
  ty <- gbm_type(SYSTEM, name)
  switch(SYSTEM, "S4" = methods::getClass(name), ty)
}), "B")
