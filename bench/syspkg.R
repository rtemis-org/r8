# syspkg.R
# ::r8 bench::
# 2026- EDG rtemis.org

# Writes, for each system, a package that declares the benchmark types at
# its top level, `benchsysS3` to `benchsysR8`, so that every system's types
# are package code: byte-compiled at install, with S3 methods registered,
# and serialized with references to the namespace. The package holds
# checks.R and types.R, and keeps the expressions of the rows as `.ops`.
# spec: R8/class-system#benchmark

SYSTEM_PACKAGES <- c("S3", "S4", "RC", "R6", "S7", "R8")

system_package <- function(system) paste0("benchsys", system)

write_system_package <- function(system, dir, bench_dir = "bench") {
  pkg <- system_package(system)
  root <- file.path(dir, pkg)
  dir.create(file.path(root, "R"), recursive = TRUE, showWarnings = FALSE)
  imports <- switch(system, "S3" = character(), "S4" = "methods", "RC" = "methods", "R6" = "R6", "S7" = "S7", "R8" = "R8")
  writeLines(c(
    paste0("Package: ", pkg),
    "Title: The Types of the R8 Benchmark",
    "Version: 0.0.1",
    "Description: Declares the types of the R8 benchmark in one class system.",
    "License: BSD_3_clause",
    "Encoding: UTF-8",
    if (length(imports)) paste0("Imports: ", paste(imports, collapse = ", ")),
    if (system == "R8") "Config/R8/namespace: bench"
  ), file.path(root, "DESCRIPTION"))
  writeLines(c(
    "exportPattern(\"^[^\\\\.]\")",
    if (system %in% c("S4", "RC")) "import(methods)",
    if (system == "S3") c("S3method(\"$<-\", s3_GBM)", "S3method(complexity, s3_GBM)")
  ), file.path(root, "NAMESPACE"))
  file.copy(file.path(bench_dir, "checks.R"), file.path(root, "R", "checks.R"), overwrite = TRUE)
  file.copy(file.path(bench_dir, "types.R"), file.path(root, "R", "types.R"), overwrite = TRUE)
  writeLines(c(
    "# The benchmark's types in this package's namespace, and the expressions",
    "# of the rows.",
    "",
    sprintf(".ops <- declare_system(\"%s\", where = environment())", system)
  ), file.path(root, "R", "zz-declare.R"))
  if (system == "R8") {
    writeLines(c(".onLoad <- function(libname, pkgname) {", "  R8::r8register()", "}"), file.path(root, "R", "zzz.R"))
  }
  root
}
