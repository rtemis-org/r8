# values.R
# ::r8 bench::
# 2026- EDG rtemis.org

# The operations on one value: construction with every argument supplied
# and with defaults, a field read, the three variants of a field write and,
# for RC and R6, a write that keeps the original, whole-value validation,
# dispatch, JSON write and read where the system has one, the size of
# saveRDS(), and the memory per value, for GBM with vectors of 10, Wide20,
# and GBMRun.
# spec: R8/class-system#benchmark

source(file.path(Sys.getenv("R8_BENCH_DIR", "bench"), "rows", "common.R"))
ops <- load_system()
args <- gbm_args(10L)
x <- do.call(GBM, args)
# RC and R6 writes change their value in place: the write rows use `xw`, so
# the other rows read `x` unchanged.
xw <- do.call(GBM, args)
w <- Wide20(f1 = 0.5)

mark("GBM", "construct", ops$construct)
mark("Wide20", "construct", ops$construct_wide)
mark("Wide20", "construct_defaults", ops$construct_wide_defaults)
mark("GBM", "read", ops$read)
mark("GBM", "write_idiom", ops$write_idiom)
mark("GBM", "write_field_rules", ops$write_field_rules)
mark("GBM", "write_all", ops$write_all)
mark("GBM", "write_keep", ops$write_keep)
mark("Wide20", "write_field_rules", ops$write_wide)
mark("GBM", "validate", ops$validate)
mark("GBM", "dispatch", ops$dispatch)

if (SYSTEM %in% c("R8", "R8 Rust")) {
  json <- R8::r8to_json(x)
  mark("GBM", "json_write", quote(R8::r8to_json(x)))
  mark("GBM", "json_read", quote(R8::r8from_json(GBM, json)))
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
# Memory per value: each value from distinct supplied values, so that no two
# share a vector or a prototype's defaults.
emit("GBM", "memory_value", marginal_size(function(i) do.call(GBM, gbm_args(10L))), "B")
emit("Wide20", "memory_value", marginal_size(function(i) {
  do.call(Wide20, as.list(stats::setNames(stats::runif(20L), WIDE_FIELDS)))
}), "B")
# The memory a Rust-stored value holds in Rust, its state's bytes, which
# obj_size() does not see: r8size() of a value whose fields are unread.
if (SYSTEM == "R8 Rust") {
  emit("GBM", "memory_value_rust", R8::r8size(do.call(GBM, gbm_args(10L))), "B")
  emit("Wide20", "memory_value_rust", R8::r8size(Wide20(f1 = 0.5)), "B")
}

if (SYSTEM != "R8 Rust") {
  source(file.path(BENCH_DIR, "corpus.R"))
  run <- gbm_run(5L, 5L)
  mark("GBMRun", "construct", quote(gbm_run(5L, 5L)))
  emit("GBMRun", "saverds_size", rds_size(run), "B")
  emit("GBMRun", "memory_value", marginal_size(function(i) gbm_run(5L, 5L)), "B")
}
