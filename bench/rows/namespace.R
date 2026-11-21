# namespace.R
# ::r8 bench::
# 2026- EDG rtemis.org

# Mode "pkg": loads the package of 200 GBM types of the system (namespace.R)
# and reports three endpoints: the time of loadNamespace(); that time with
# every binding read, which S7, R6, and RC need before their class objects
# exist in memory; and that time with one value constructed of each type,
# which R8 needs before it compiles each type's rules. It reports the cons
# cells at each endpoint and the median of 20 full gc() runs. Mode
# "control" loads the system's own packages only, and reports the median
# gc() time, which report.R subtracts.
# spec: R8/class-system#benchmark

source(file.path(Sys.getenv("R8_BENCH_DIR", "bench"), "rows", "common.R"))
source(file.path(BENCH_DIR, "namespace.R"))
for (p in system_packages(SYSTEM)) suppressPackageStartupMessages(loadNamespace(p))
cells <- function() {
  invisible(gc())
  gc()["Ncells", "used"]
}
gc_median <- function() stats::median(replicate(20L, system.time(gc(full = TRUE))[["elapsed"]]))

if (MODE == "pkg") {
  pkg <- namespace_package(SYSTEM)
  args <- gbm_args(10L)
  n0 <- cells()
  t1 <- system.time(ns <- loadNamespace(pkg))[["elapsed"]]
  n1 <- cells()
  t2 <- system.time(types <- mget(sprintf("GBM_%d", 1:200), envir = ns))[["elapsed"]]
  n2 <- cells()
  t3 <- system.time(values <- lapply(types, function(ty) gbm_value(SYSTEM, ty, args)))[["elapsed"]]
  n3 <- cells()
  emit("GBM x 200", "load_time", t1, "s")
  emit("GBM x 200", "bindings_time", t1 + t2, "s")
  emit("GBM x 200", "values_time", t1 + t2 + t3, "s")
  emit("GBM x 200", "ncells_load", n1 - n0, "cells")
  emit("GBM x 200", "ncells_bindings", n2 - n0, "cells")
  emit("GBM x 200", "ncells_values", n3 - n0, "cells")
}
emit("GBM x 200", "gc_full", gc_median(), "s")
