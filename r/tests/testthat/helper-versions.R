# helper-versions.R
# ::R8::
# 2026- EDG rtemis.org

# Small packages at several versions, each version in a library of its own, for
# the scenarios in which one package is updated and another is not. The package
# `pa` declares a type, and `pb` uses it.

# Writes the source of a package that declares R8 types in R.
#   code     Character: The lines of its R code.
#   exports  Character: The names it exports.
#   imports  Character: The packages it imports, beside R8.
write_package <- function(
  dir,
  name,
  code,
  exports,
  version = "1.0.0",
  imports = character()
) {
  path <- file.path(dir, name)
  unlink(path, recursive = TRUE)
  dir.create(file.path(path, "R"), recursive = TRUE)
  writeLines(
    c(
      paste("Package:", name),
      "Title: Test Package",
      paste("Version:", version),
      "Description: A test package.",
      "License: BSD_3_clause + file LICENSE",
      "Encoding: UTF-8",
      paste("Imports:", paste(c("R8", imports), collapse = ", ")),
      paste("Config/R8/namespace:", name)
    ),
    file.path(path, "DESCRIPTION")
  )
  writeLines("R8", file.path(path, "LICENSE"))
  writeLines(
    c(
      sprintf("import(%s)", imports),
      sprintf("export(%s)", exports)
    ),
    file.path(path, "NAMESPACE")
  )
  writeLines(
    c(code, ".onLoad <- function(libname, pkgname) R8::r8_register()"),
    file.path(path, "R", "types.R")
  )
  path
}

# Adds Rust code to the source of a package: `lib` holds the lines of its
# lib.rs, and `crates` the directories of the crates that it depends on, named
# by crate.
write_rust <- function(path, lib, crates = character()) {
  name <- basename(path)
  root <- normalizePath(
    test_path("..", "..", "..", "rust", "crates"),
    mustWork = FALSE
  )
  if (!dir.exists(root)) {
    skip("the crates of the checkout are not here")
  }
  src <- file.path(path, "src", "rust", "src")
  dir.create(src, recursive = TRUE)
  writeLines(lib, file.path(src, "lib.rs"))
  writeLines(
    c(
      "[package]",
      sprintf("name = \"%s\"", name),
      "version = \"0.0.1\"",
      "edition = \"2024\"",
      "[workspace]",
      "[lib]",
      "crate-type = [\"staticlib\", \"lib\"]",
      "[dependencies]",
      sprintf(
        "rtemis-r8 = { path = \"%s/rtemis-r8\", features = [\"macros\"] }",
        root
      ),
      sprintf("rtemis-r8-api = { path = \"%s/rtemis-r8-api\" }", root),
      sprintf("%s = { path = \"%s\" }", names(crates), crates)
    ),
    file.path(path, "src", "rust", "Cargo.toml")
  )
  target <- file.path(
    Sys.getenv("R8_TARGET_DIR", file.path(tempdir(), "r8target")),
    "versions"
  )
  writeLines(
    c(
      sprintf("TARGET_DIR = %s", target),
      sprintf("STATLIB = $(TARGET_DIR)/release/lib%s.a", name),
      sprintf("PKG_LIBS = -L$(TARGET_DIR)/release -l%s", name),
      "all: $(SHLIB)",
      ".PHONY: $(STATLIB)",
      "$(SHLIB): $(STATLIB)",
      "$(STATLIB):",
      paste0(
        "\tPATH=\"$(PATH):$(HOME)/.cargo/bin\" cargo build --offline --lib ",
        "--release --manifest-path=./rust/Cargo.toml --target-dir $(TARGET_DIR)"
      )
    ),
    file.path(path, "src", "Makevars")
  )
  writeLines(
    c(
      "#include <stddef.h>",
      "#include <R_ext/Rdynload.h>",
      sprintf("void r8_init_%s(void *dll);", name),
      sprintf("void R_init_%s(DllInfo *dll) {", name),
      "  R_registerRoutines(dll, NULL, NULL, NULL, NULL);",
      "  R_useDynamicSymbols(dll, FALSE);",
      sprintf("  r8_init_%s(dll);", name),
      "}"
    ),
    file.path(path, "src", "entrypoint.c")
  )
  cat(
    sprintf("useDynLib(%s, .registration = TRUE)\n", name),
    file = file.path(path, "NAMESPACE"),
    append = TRUE
  )
  path
}

# Installs the package at `path` into a new library, which also finds the
# packages of `libs`, and returns the library.
install_version <- function(path, libs = character(), lib = tempfile("lib")) {
  dir.create(lib, showWarnings = FALSE)
  out <- suppressWarnings(system2(
    file.path(R.home("bin"), "R"),
    c("CMD", "INSTALL", "--no-docs", paste0("--library=", lib), path),
    stdout = TRUE,
    stderr = TRUE,
    env = paste0(
      "R_LIBS=",
      paste(c(lib, libs, .libPaths()), collapse = .Platform$path.sep)
    )
  ))
  if (!dir.exists(file.path(lib, basename(path)))) {
    stop(paste(out, collapse = "\n"))
  }
  lib
}

# The R declarations of the types in the Rust code of the package at `path`.
generate_types <- function(path, libs = character()) {
  code <- sprintf(
    "pkgbuild::compile_dll(%s, quiet = TRUE); R8::r8_document_rust(%s)",
    deparse(path),
    deparse(path)
  )
  out <- run_r(libs, code)
  if (!file.exists(file.path(path, "R", "r8-types.R"))) {
    stop(paste(out, collapse = "\n"))
  }
  path
}

# Runs `code` in a fresh session whose libraries are `libs`, and returns the
# lines that its say() writes, by label.
said <- function(libs, code) {
  out <- run_r(
    libs,
    c(
      "say <- function(label, e) {",
      "  text <- tryCatch(",
      "    paste(format(e), collapse = ' '),",
      "    error = function(e) paste('error:', conditionMessage(e)),",
      "    warning = function(w) paste('warning:', conditionMessage(w))",
      "  )",
      "  cat(sprintf('[%s] %s\\n', label, gsub('\\n', ' ', text)))",
      "}",
      code
    )
  )
  m <- regmatches(out, regexec("^\\[([^]]+)\\] (.*)$", out))
  hit <- lengths(m) == 3L
  lines <- vapply(m[hit], `[[`, "", 3L)
  names(lines) <- vapply(m[hit], `[[`, "", 2L)
  attr(lines, "output") <- paste(out, collapse = "\n")
  lines
}

# Expects the line of each label to match its regular expression.
expect_said <- function(said, ...) {
  want <- c(...)
  for (label in names(want)) {
    expect_match(
      unname(said[label]),
      want[[label]],
      info = attr(said, "output"),
      label = label
    )
  }
}
