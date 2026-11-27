# peak.R
# ::r8 bench::
# 2026- EDG rtemis.org

# A process for the peak resident memory that run.R reads with
# /usr/bin/time -l. Mode "value" builds one GBM of 10^6 weights and feature
# indices; mode "control" loads the same packages and builds the same input
# only. The R8 modes "vec" and "table" build 10^6 values of a two-double
# struct as `vec(S)` and as `type_table(S)`, and "rows" their input only.
# The R8 and R8 Rust modes for a JSON read: "json_write" writes the document
# of the GBM of 10^6 weights and feature indices to a file in the directory
# `R8_BENCH_TMP`, which run.R shares between processes; "json" reads the
# file's text and then the value; "json_control" reads the text only. The
# document is written in its own process, so that building the value is not
# part of the read's peak.
# spec: R8/class-system#benchmark

source(file.path(Sys.getenv("R8_BENCH_DIR", "bench"), "rows", "common.R"))
ops <- load_system()
n <- 1e6L
if (MODE %in% c("value", "control")) {
  args <- gbm_args(n)
  if (MODE == "value") x <- do.call(GBM, args)
} else if (MODE %in% c("json_write", "json", "json_control")) {
  path <- file.path(Sys.getenv("R8_BENCH_TMP"), paste0("gbm-", gsub(" ", "_", SYSTEM), ".json"))
  if (MODE == "json_write") {
    writeLines(R8::r8to_json(do.call(GBM, gbm_args(n))), path)
  } else {
    text <- readLines(path)
    if (MODE == "json") x <- R8::r8from_json(GBM, text)
  }
} else {
  source(file.path(BENCH_DIR, "table.R"))
  input <- table_input(n)
  if (MODE == "vec") x <- table_as_vec(input)
  if (MODE == "table") x <- table_as_table(input)
}
invisible(gc())
emit("process", "done", 1, "")
