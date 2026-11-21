# namespace.R
# ::r8 bench::
# 2026- EDG rtemis.org

# Mode "pkg": loads the package of 200 GBM types of the system (namespace.R)
# and reports the time of loadNamespace(), the cons cells it adds, the cons
# cells after every binding is read, and the median of 20 full gc() runs.
# Mode "control": loads the system's own packages only, and reports the
# median gc() time, which report.R subtracts.
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
  n0 <- cells()
  t <- system.time(ns <- loadNamespace(pkg))[["elapsed"]]
  n1 <- cells()
  for (nm in ls(ns, all.names = TRUE)) invisible(get(nm, envir = ns))
  n2 <- cells()
  emit("GBM x 200", "load_time", t, "s")
  emit("GBM x 200", "ncells_load", n1 - n0, "cells")
  emit("GBM x 200", "ncells_bindings", n2 - n0, "cells")
}
emit("GBM x 200", "gc_full", gc_median(), "s")
