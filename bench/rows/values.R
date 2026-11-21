# values.R
# ::r8 bench::
# 2026- EDG rtemis.org

# The operations on one value: construction, field read, field write with
# validation, whole-value validation, dispatch, JSON write and read where
# the system has one, the size of saveRDS(), and the memory per value, for
# GBM with vectors of 10, Wide20, and GBMRun.
# spec: R8/class-system#benchmark

source(file.path(Sys.getenv("R8_BENCH_DIR", "bench"), "rows", "common.R"))
ops <- load_system()
args <- gbm_args(10L)
x <- do.call(GBM, args)
w <- Wide20()

mark("GBM", "construct", ops$construct)
mark("Wide20", "construct", ops$construct_wide)
mark("GBM", "read", ops$read)
mark("GBM", "write", ops$write)
mark("Wide20", "write", ops$write_wide)
mark("GBM", "validate", ops$validate)
mark("GBM", "dispatch", ops$dispatch)
mark("GBM", "dispatch_generic", ops$dispatch_generic)

if (SYSTEM %in% c("R8", "R8 Rust")) {
  json <- R8::to_json(x)
  mark("GBM", "json_write", quote(R8::to_json(x)))
  mark("GBM", "json_read", quote(R8::r8_from_json(GBM, json)))
  emit("GBM", "json_size", nchar(json, type = "bytes"), "B")
}

rds_size <- function(v) {
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path))
  saveRDS(v, path)
  file.size(path)
}
emit("GBM", "saverds_size", rds_size(x), "B")
emit("Wide20", "saverds_size", rds_size(w), "B")
emit("GBM", "memory_value", marginal_size(function(i) do.call(GBM, gbm_args(10L))), "B")
emit("Wide20", "memory_value", marginal_size(function(i) Wide20()), "B")
# The memory a Rust-native value holds in Rust, its state's bytes, which
# obj_size() does not see: r8_size() of a value whose fields are unread.
if (SYSTEM == "R8 Rust") {
  emit("GBM", "memory_value_rust", R8::r8_size(do.call(GBM, gbm_args(10L))), "B")
  emit("Wide20", "memory_value_rust", R8::r8_size(Wide20()), "B")
}

if (SYSTEM != "R8 Rust") {
  source(file.path(BENCH_DIR, "corpus.R"))
  run <- gbm_run(5L, 5L)
  assign("run", run, envir = globalenv())
  mark("GBMRun", "construct", quote(gbm_run(5L, 5L)))
  emit("GBMRun", "saverds_size", rds_size(run), "B")
  emit("GBMRun", "memory_value", marginal_size(function(i) gbm_run(5L, 5L)), "B")
}
