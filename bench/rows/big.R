# big.R
# ::r8 bench::
# 2026- EDG rtemis.org

# A GBM whose weights and feature indices hold 10^6 elements: construction,
# a field write, and whole-value validation, which scale with the data.
# spec: R8/class-system#benchmark

source(file.path(Sys.getenv("R8_BENCH_DIR", "bench"), "rows", "common.R"))
ops <- load_system()
args <- gbm_args(1e6L)
x <- do.call(GBM, args)

mark("GBM 1e6", "construct", ops$construct, min_iterations = 20L)
mark("GBM 1e6", "write", ops$write, min_iterations = 20L)
mark("GBM 1e6", "validate", ops$validate, min_iterations = 20L)
