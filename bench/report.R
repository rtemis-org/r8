# report.R
# ::r8 bench::
# 2026- EDG rtemis.org

# Writes the Markdown tables of one run from bench/results/<commit>.jsonl:
# medians over repeated processes, control processes subtracted from gc()
# times and peak resident memory. Usage: Rscript bench/report.R <commit>.
# spec: R8/class-system#benchmark

sha <- commandArgs(trailingOnly = TRUE)[[1L]]
lines <- readLines(file.path("bench", "results", paste0(sha, ".jsonl")))
d <- do.call(rbind, lapply(lines, function(l) {
  x <- jsonlite::fromJSON(l)
  x$value <- suppressWarnings(as.numeric(x$value))
  as.data.frame(x[c("row", "system", "mode", "type", "metric", "value", "unit")])
}))
systems <- c("S3", "S4", "RC", "R6", "S7", "R8", "R8 Rust")
r_systems <- setdiff(systems, "R8 Rust")

value_of <- function(row, system, type, metric, mode = "") {
  v <- d$value[d$row == row & d$system == system & d$type == type & d$metric == metric & d$mode == mode]
  if (length(v)) stats::median(v) else NA_real_
}

fmt <- function(v, unit) {
  if (is.na(v)) return("")
  switch(
    unit,
    us = if (v == 0) "< 0.01" else formatC(v * 1e6, digits = 3, format = "fg"),
    ms = formatC(v * 1e3, digits = 3, format = "fg"),
    s = formatC(v, digits = 3, format = "fg"),
    B = format(round(v), big.mark = ","),
    MB = formatC(v / 2^20, digits = 3, format = "fg"),
    n = format(round(v), big.mark = ",")
  )
}

table_of <- function(title, rows, cols = systems, note = NULL) {
  cat("\n### ", title, "\n\n", sep = "")
  if (!is.null(note)) cat(note, "\n\n", sep = "")
  cat("| | ", paste(cols, collapse = " | "), " |\n", sep = "")
  cat("|---|", paste(rep("---:", length(cols)), collapse = "|"), "|\n", sep = "")
  for (r in rows) {
    cells <- vapply(cols, function(s) fmt(r$f(s), r$unit), "")
    cat("| ", r$label, " | ", paste(cells, collapse = " | "), " |\n", sep = "")
  }
}

row_spec <- function(label, unit, f) list(label = label, unit = unit, f = f)
vals <- function(type, metric) function(s) value_of("values", s, type, metric)
peak_delta <- function(row, s, mode, control, type = "process") {
  value_of(row, s, type, "peak_rss", mode) - value_of(row, s, type, "peak_rss", control)
}

table_of("GBM, vectors of 10 (median time, µs; sizes, bytes)", list(
  row_spec("Construction, every argument supplied", "us", vals("GBM", "construct")),
  row_spec("First construction after load (median of 5 processes)", "us", function(s) value_of("cold", s, "GBM", "construct_first")),
  row_spec("Field read", "us", vals("GBM", "read")),
  row_spec("Field write, the system's idiom", "us", vals("GBM", "write_idiom")),
  row_spec("Field write, the changed field and the rules", "us", vals("GBM", "write_field_rules")),
  row_spec("Field write, every field checked again", "us", vals("GBM", "write_all")),
  row_spec("Field write keeping the original (RC $copy(), R6 $clone())", "us", vals("GBM", "write_keep")),
  row_spec("Whole-value validation", "us", vals("GBM", "validate")),
  row_spec("Dispatch (RC and R6: a method call)", "us", vals("GBM", "dispatch")),
  row_spec("JSON write", "us", vals("GBM", "json_write")),
  row_spec("JSON read", "us", vals("GBM", "json_read")),
  row_spec("Memory per value, B", "B", vals("GBM", "memory_value")),
  row_spec("Memory per value held in Rust, B", "B", vals("GBM", "memory_value_rust")),
  row_spec("saveRDS() size, B", "B", vals("GBM", "saverds_size"))
), note = paste(
  "In the idiom, S3, S4, RC, and R6 check every field again. S7 and R8 check the changed field and the rules; R8 checks the rules that read the field.",
  "RC and R6 write in place; the other systems return a new value and keep the original."
))
table_of("R allocations of the GBM operations (bytes per evaluation, after two)", list(
  row_spec("Construction", "B", vals("GBM", "construct_alloc")),
  row_spec("Field read", "B", vals("GBM", "read_alloc")),
  row_spec("Field write, the changed field and the rules", "B", vals("GBM", "write_field_rules_alloc")),
  row_spec("Validation", "B", vals("GBM", "validate_alloc")),
  row_spec("Dispatch", "B", vals("GBM", "dispatch_alloc"))
))
table_of("Wide20, twenty doubles", list(
  row_spec("Construction, f1 supplied, µs", "us", vals("Wide20", "construct")),
  row_spec("Construction, every field by default, µs", "us", vals("Wide20", "construct_defaults")),
  row_spec("Field write, the changed field, µs", "us", vals("Wide20", "write_field_rules")),
  row_spec("Memory per value, B", "B", vals("Wide20", "memory_value")),
  row_spec("Memory per value held in Rust, B", "B", vals("Wide20", "memory_value_rust")),
  row_spec("saveRDS() size, B", "B", vals("Wide20", "saverds_size"))
))
table_of("GBMRun, a GBM with a Resampler, a date, a factor, a matrix, and 5 folds", list(
  row_spec("Construction, µs", "us", vals("GBMRun", "construct")),
  row_spec("Memory per value, B", "B", vals("GBMRun", "memory_value")),
  row_spec("saveRDS() size, B", "B", vals("GBMRun", "saverds_size"))
), r_systems)
big <- function(metric, mode = "", type = "GBM 1e6") function(s) value_of("big", s, type, metric, mode)
table_of("GBM with vectors of 10^6 (median time, ms)", list(
  row_spec("Construction", "ms", big("construct")),
  row_spec("Construction, feature indices a compact sequence (ALTREP)", "ms", big("construct", "compact", "GBM 1e6 compact")),
  row_spec("Field write, the system's idiom", "ms", big("write_idiom")),
  row_spec("Field write, the changed field and the rules", "ms", big("write_field_rules")),
  row_spec("Field write, every field checked again", "ms", big("write_all")),
  row_spec("Field write keeping the original", "ms", big("write_keep")),
  row_spec("Whole-value validation", "ms", big("validate")),
  row_spec("Whole-value validation, compact feature indices", "ms", big("validate", "compact", "GBM 1e6 compact")),
  row_spec("Peak resident memory beyond the input, MB", "MB", function(s) peak_delta("peak", s, "value", "control"))
))
ns <- function(metric, mode = "pkg") function(s) value_of("namespace", s, "GBM x 200", metric, mode)
table_of("Types: 200 GBM types", list(
  row_spec("Declaring 200 types in a session, s (median of 5 processes)", "s", function(s) value_of("types", s, "GBM", "declare_200")),
  row_spec("Memory per type in R, B", "B", function(s) value_of("types", s, "GBM", "memory_type_r", "memory")),
  row_spec("Memory per type in Rust (R8's registry), B", "B", function(s) value_of("types", s, "GBM", "memory_type_native", "memory")),
  row_spec("Memory per type, total, B", "B", function(s) {
    value_of("types", s, "GBM", "memory_type_r", "memory") + value_of("types", s, "GBM", "memory_type_native", "memory")
  }),
  row_spec("loadNamespace() of a package of 200 types, s", "s", ns("load_time")),
  row_spec("Loading, every binding read, s", "s", ns("bindings_time")),
  row_spec("Loading, every binding read, a value of each type, s", "s", ns("values_time")),
  row_spec("Cons cells after loading", "n", ns("ncells_load")),
  row_spec("Cons cells, every binding read", "n", ns("ncells_bindings")),
  row_spec("Cons cells, a value of each type", "n", ns("ncells_values")),
  row_spec("Full gc() beyond the control, ms", "ms", function(s) ns("gc_full")(s) - ns("gc_full", "control")(s)),
  row_spec("Peak resident memory beyond the control, MB", "MB", function(s) peak_delta("namespace", s, "pkg", "control"))
), r_systems, note = paste(
  "R8 checks each declaration (field types, defaults against their types, rules, references); the other systems check none.",
  "S7, R6, and RC lazy-load their class objects, which reading the bindings brings into memory; R8 registers its types as its package loads and compiles a type's rules at its first value."
))
cat("\n### R8: 10^6 values of a two-double struct\n\n")
cat("| | `vec(S)` | `type_table(S)` |\n|---|---:|---:|\n")
cat("| Construction, s | ", fmt(value_of("table", "R8", "vec(S) 1e6", "construct"), "s"), " | ",
    fmt(value_of("table", "R8", "type_table(S) 1e6", "construct"), "s"), " |\n", sep = "")
cat("| Size (obj_size()), MB | ", fmt(value_of("table", "R8", "vec(S) 1e6", "size"), "MB"), " | ",
    fmt(value_of("table", "R8", "type_table(S) 1e6", "size"), "MB"), " |\n", sep = "")
cat("| Peak resident memory beyond the input, MB | ", fmt(peak_delta("peak", "R8", "vec", "rows"), "MB"), " | ",
    fmt(peak_delta("peak", "R8", "table", "rows"), "MB"), " |\n", sep = "")
cat("\n### The S7 baseline of 2026-11-17, measured again\n\n")
cat("| | Value |\n|---|---:|\n")
s7 <- function(m) value_of("s7_baseline", "S7", "Cls", m)
cat("| One property declaration, B | ", fmt(s7("memory_property"), "B"), " |\n", sep = "")
cat("| The class, B | ", fmt(s7("memory_class"), "B"), " |\n", sep = "")
cat("| An instance, the class reachable from it included, B | ", fmt(s7("memory_value"), "B"), " |\n", sep = "")
cat("| `Cls()`, µs | ", fmt(s7("construct"), "us"), " |\n", sep = "")
cat("| `x@f1`, µs | ", fmt(s7("read"), "us"), " |\n", sep = "")
cat("| `y <- x; y@f1 <- 0.5`, µs | ", fmt(s7("write"), "us"), " |\n", sep = "")
