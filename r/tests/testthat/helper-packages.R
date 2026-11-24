# helper-packages.R
# ::R8::
# 2026- EDG rtemis.org

# The tests that install packages (test-15x, and the scenarios of packages) run when
# R8_PACKAGE_TESTS is set (`just test-packages`): the installation takes
# minutes, and uses the installed R8.

run_r <- function(lib, code) {
  script <- tempfile(fileext = ".R")
  writeLines(
    c(
      sprintf(
        ".libPaths(c(%s, .libPaths()))",
        paste(deparse(lib), collapse = "")
      ),
      R_STORED,
      code
    ),
    script
  )
  # R_LIBS reaches the worker processes that mirai and future start.
  libs <- paste(c(lib, .libPaths()), collapse = .Platform$path.sep)
  suppressWarnings(system2(
    file.path(R.home("bin"), "Rscript"),
    script,
    stdout = TRUE,
    stderr = TRUE,
    env = paste0("R_LIBS=", libs)
  ))
}

# The library of the installed test packages, installed once per run of this
# file.
.test_packages <- new.env(parent = emptyenv())

install_test_packages <- function() {
  if (!is.null(.test_packages[["lib"]])) {
    return(.test_packages[["lib"]])
  }
  lib <- tempfile("r8lib")
  dir.create(lib)
  for (p in c("r8pkgA", "r8pkgB", "r8pkgAbi", "r8pkgR", "r8pkgS")) {
    out <- system2(
      file.path(R.home("bin"), "R"),
      c(
        "CMD",
        "INSTALL",
        paste0("--library=", lib),
        test_path("packages", p)
      ),
      stdout = TRUE,
      stderr = TRUE
    )
    if (!dir.exists(file.path(lib, p))) {
      stop(paste(out, collapse = "\n"))
    }
  }
  .test_packages[["lib"]] <- lib
  lib
}

# A library with a copy of the installed package `pkg` that imports nothing,
# as a package does that names another one with `::` only.
without_imports <- function(lib, pkg) {
  alone <- tempfile("r8alone")
  dir.create(alone)
  file.copy(file.path(lib, pkg), alone, recursive = TRUE)
  path <- file.path(alone, pkg, "Meta", "nsInfo.rds")
  info <- readRDS(path)
  info[["imports"]] <- list()
  saveRDS(info, path)
  alone
}
