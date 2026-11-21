# namespace.R
# ::r8 bench::
# 2026- EDG rtemis.org

# Writes, for each system, a package that declares 200 GBM types at its top
# level, `GBM_1` to `GBM_200`, for the namespace rows: the package holds
# checks.R and types.R, and declares each type with gbm_type().
# spec: R8/class-system#benchmark

NAMESPACE_SYSTEMS <- c("S3", "S4", "RC", "R6", "S7", "R8")

namespace_package <- function(system) paste0("bench", system, "200")

write_namespace_package <- function(system, dir, n = 200L, bench_dir = "bench") {
  pkg <- namespace_package(system)
  root <- file.path(dir, pkg)
  dir.create(file.path(root, "R"), recursive = TRUE, showWarnings = FALSE)
  imports <- switch(system, "S3" = character(), "S4" = "methods", "RC" = "methods", "R6" = "R6", "S7" = "S7", "R8" = "R8")
  writeLines(c(
    paste0("Package: ", pkg),
    "Title: 200 Types for the R8 Benchmark",
    "Version: 0.0.1",
    "Description: Declares 200 GBM types for the namespace rows of the R8 benchmark.",
    "License: BSD_3_clause",
    "Encoding: UTF-8",
    if (length(imports)) paste0("Imports: ", paste(imports, collapse = ", ")),
    if (system == "R8") paste0("Config/R8/namespace: ", tolower(pkg))
  ), file.path(root, "DESCRIPTION"))
  writeLines(c(
    "exportPattern(\"^GBM_\")",
    if (system %in% c("S4", "RC")) "import(methods)"
  ), file.path(root, "NAMESPACE"))
  file.copy(file.path(bench_dir, "checks.R"), file.path(root, "R", "checks.R"), overwrite = TRUE)
  file.copy(file.path(bench_dir, "types.R"), file.path(root, "R", "types.R"), overwrite = TRUE)
  decls <- sprintf(
    "GBM_%d <- gbm_type(\"%s\", \"GBM_%d\", where = environment())",
    seq_len(n), system, seq_len(n)
  )
  writeLines(c("# The types of the namespace rows.", "", decls), file.path(root, "R", "zz-types.R"))
  if (system == "R8") {
    writeLines(c(".onLoad <- function(libname, pkgname) {", "  R8::r8_register()", "}"), file.path(root, "R", "zzz.R"))
  }
  root
}
