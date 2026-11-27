# big.R
# ::r8 bench::
# 2026- EDG rtemis.org

# A GBM whose weights and feature indices hold 10^6 elements: construction,
# the three variants of a field write, and whole-value validation, which
# scale with the data, and, for R8 and R8 Rust, the JSON write and read of
# the value. Mode "compact" passes the feature indices as a compact sequence
# (ALTREP), which R's min() and anyNA() answer without a pass; the default
# mode passes them held in memory.
# spec: R8/class-system#benchmark

source(file.path(Sys.getenv("R8_BENCH_DIR", "bench"), "rows", "common.R"))
ops <- load_system()
compact <- identical(MODE, "compact")
type <- if (compact) "GBM 1e6 compact" else "GBM 1e6"
args <- gbm_args(1e6L, compact = compact)
x <- do.call(GBM, args)
xw <- do.call(GBM, args)

mark(type, "construct", ops$construct, min_iterations = 20L)
if (!compact) {
  mark(type, "write_idiom", ops$write_idiom, min_iterations = 20L)
  mark(type, "write_field_rules", ops$write_field_rules, min_iterations = 20L)
  mark(type, "write_all", ops$write_all, min_iterations = 20L)
  mark(type, "write_keep", ops$write_keep, min_iterations = 20L)
}
mark(type, "validate", ops$validate, min_iterations = 20L)
if (!compact && SYSTEM %in% c("R8", "R8 Rust")) {
  json <- R8::r8to_json(x)
  mark(type, "json_write", quote(R8::r8to_json(x)), min_iterations = 10L)
  mark(type, "json_read", quote(R8::r8from_json(GBM, json)), min_iterations = 10L)
  emit(type, "json_size", nchar(json, type = "bytes"), "B")
}
