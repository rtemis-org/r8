# s7_baseline.R
# ::r8 bench::
# 2026- EDG rtemis.org

# The S7 baseline of notes.md (2026-11-17, S7 0.2.2), measured again with
# the installed S7: a class of 20 properties from rtemis.core's
# prop_number().
# spec: R8/class-system#benchmark

source(file.path(Sys.getenv("R8_BENCH_DIR", "bench"), "rows", "common.R"))
suppressPackageStartupMessages({ library(rtemis.core); library(S7) })
props <- stats::setNames(
  lapply(1:20, function(i) prop_number(0.1, min = 0, max = 1, description = paste("Field", i))),
  paste0("f", 1:20)
)
Cls <- new_class("Cls", properties = props)
x <- Cls()
emit("Cls", "memory_property", as.numeric(lobstr::obj_size(props[[1]])), "B")
emit("Cls", "memory_class", as.numeric(lobstr::obj_size(Cls)), "B")
emit("Cls", "memory_value", as.numeric(lobstr::obj_size(x)), "B")
mark("Cls", "construct", quote(Cls()))
mark("Cls", "read", quote(x@f1))
mark("Cls", "write", quote({ y <- x; y@f1 <- 0.5; y }))
