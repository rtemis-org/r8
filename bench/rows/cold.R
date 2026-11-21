# cold.R
# ::r8 bench::
# 2026- EDG rtemis.org

# The first construction of a GBM after the system's package loads, which
# pays for byte-compilation, method caches, and lazy rule compilation; run.R
# repeats the row in fresh processes for the median.
# spec: R8/class-system#benchmark

source(file.path(Sys.getenv("R8_BENCH_DIR", "bench"), "rows", "common.R"))
ops <- load_system()
args <- gbm_args(10L)
t0 <- bench::hires_time()
x <- eval(ops$construct)
emit("GBM", "construct_first", as.numeric(bench::hires_time() - t0), "s")
