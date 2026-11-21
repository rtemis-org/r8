# table.R
# ::r8 bench::
# 2026- EDG rtemis.org

# R8 only: 10^6 values of a two-double struct as `vec(S)` and as
# `type_table(S)`: the time to build each, and its size.
# spec: R8/class-system#benchmark

source(file.path(Sys.getenv("R8_BENCH_DIR", "bench"), "rows", "common.R"))
invisible(load_system())
source(file.path(BENCH_DIR, "table.R"))
types <- table_types()
input <- table_input(1e6L)
tv <- system.time(v <- table_as_vec(input, types))[["elapsed"]]
tt <- system.time(t <- table_as_table(input, types))[["elapsed"]]
emit("vec(S) 1e6", "construct", tv, "s")
emit("type_table(S) 1e6", "construct", tt, "s")
emit("vec(S) 1e6", "size", as.numeric(lobstr::obj_size(v)), "B")
emit("type_table(S) 1e6", "size", as.numeric(lobstr::obj_size(t)), "B")
