# config.R
# ::R8::
# 2026- EDG rtemis.org

# Writes src/Makevars (or src/Makevars.win) from its template, replacing each
# @PLACEHOLDER@ with the value of the corresponding variable whose name starts
# with `.` (for example @CRAN_FLAGS@ with `.cran_flags`).

# Checks the Rust toolchain against SystemRequirements.
source("tools/msrv.R")

# Build mode from the DEBUG and NOT_CRAN environment variables.
env_debug <- Sys.getenv("DEBUG")
env_not_cran <- Sys.getenv("NOT_CRAN")

# Vendored crates enable an offline build.
vendor_exists <- file.exists("src/rust/vendor.tar.xz")

is_not_cran <- env_not_cran != ""
is_debug <- env_debug != ""

if (is_debug) {
  # A debug build is never a CRAN build; CRAN builds use the release profile.
  is_not_cran <- TRUE
  message("Creating DEBUG build.")
}

if (!is_not_cran) {
  message("Building for CRAN.")
}

# CRAN builds (NOT_CRAN unset) with vendored crates run offline on at most two
# jobs.
.cran_flags <- ifelse(
  !is_not_cran && vendor_exists,
  "-j 2 --offline",
  ""
)

# R8_CRATES_PATH, when set, points to the `crates` directory of an r8
# checkout (for example /path/to/r8/crates). Cargo then builds `rtemis-r8` from
# that source in place of the crates.io release named in Cargo.toml. The
# justfile sets it to the checkout that holds this package; CRAN builds leave
# it unset.
crates_path <- Sys.getenv("R8_CRATES_PATH")
.cargo_config <- if (nzchar(crates_path)) {
  core_path <- normalizePath(
    file.path(crates_path, "rtemis-r8"),
    winslash = "/",
    mustWork = TRUE
  )
  message("Using the rtemis-r8 crate at ", core_path, ".")
  sprintf("--config 'patch.crates-io.rtemis-r8.path=\"%s\"'", core_path)
} else {
  ""
}

# With a local rtemis-r8, the Makevars save Cargo.lock before each cargo
# command and restore it after, since the patch rewrites the lock's rtemis-r8
# entry without its crates.io source.
patched <- nzchar(.cargo_config)
.save_lock <- ifelse(patched, "cp rust/Cargo.lock rust/Cargo.lock.saved", "")
.restore_lock <- ifelse(
  patched,
  "cp rust/Cargo.lock.saved rust/Cargo.lock;",
  ""
)
.drop_lock <- ifelse(patched, "rm -f rust/Cargo.lock.saved", "")

# R8_ORACLE, when set, builds the differential oracle (src/rust/src/oracle.rs)
# that the tests compare native construction with. The `document` binary builds
# without it, so R/extendr-wrappers.R is the same in every build.
.features <- ifelse(nzchar(Sys.getenv("R8_ORACLE")), "--features oracle", "")
if (nzchar(.features)) {
  message("Building with the differential oracle.")
}

# R8_TARGET_DIR, when set on a Unix build, is the Cargo target directory,
# outside the package directory: an installer that copies the package directory
# (pak::local_install()) copies no build cache, and the cache persists between
# builds. The justfile sets it. Unset, as on CRAN, the target directory is
# src/rust/target, which a release build removes after linking.
target_dir <- if (.Platform[["OS.type"]] == "unix") {
  Sys.getenv("R8_TARGET_DIR")
} else {
  ""
}
.target_dir <- if (nzchar(target_dir)) {
  normalizePath(target_dir, winslash = "/", mustWork = FALSE)
} else {
  "./rust/target"
}

# DEBUG selects the dev profile; otherwise the release profile.
.profile <- ifelse(is_debug, "", "--release")
.clean_targets <- ifelse(is_debug || nzchar(target_dir), "", "$(TARGET_DIR)")
# `make clean` (R CMD INSTALL --preclean) removes the target directory only
# when it is the package's own src/rust/target.
.clean_target_dir <- ifelse(nzchar(target_dir), "", "$(TARGET_DIR)")

# Target triple of webR.
webr_target <- "wasm32-unknown-emscripten"

# TRUE when R runs as webR.
is_wasm <- identical(R.version$platform, webr_target)

if (is_wasm) {
  message("Building for WebR")
}

# Library directory under the target directory: [<webR target>/]<debug|release>,
# for @LIBDIR@.
target_libpath <- if (is_wasm) "wasm32-unknown-emscripten" else NULL
cfg <- if (is_debug) "debug" else "release"

.libdir <- paste(c(target_libpath, cfg), collapse = "/")

# @TARGET@: an explicit --target for webR builds only.
.target <- ifelse(is_wasm, paste0("--target=", webr_target), "")

# WebAssembly builds abort on panic.
.panic_exports <- ifelse(
  is_wasm,
  "CARGO_PROFILE_DEV_PANIC=\"abort\" CARGO_PROFILE_RELEASE_PANIC=\"abort\" ",
  ""
)

# @RUN_DOCUMENT@: "yes" runs the `document` binary after the library build,
# which regenerates R/extendr-wrappers.R. Development builds (NOT_CRAN or
# DEBUG set) take "yes". CRAN builds (NOT_CRAN unset) take "no" and install the committed
# R/extendr-wrappers.R; the binary builds in the dev profile with other
# RUSTFLAGS, which recompiles every dependency, and its build scripts fail to
# run on win-builder (os error 193). webR builds take "no", because a binary
# cross-compiled to WebAssembly cannot run on the build host.
.run_document <- ifelse(is_wasm || !is_not_cran, "no", "yes")

# Template and output for the current platform.
is_windows <- .Platform[["OS.type"]] == "windows"

mv_fp <- ifelse(
  is_windows,
  "src/Makevars.win.in",
  "src/Makevars.in"
)

mv_ofp <- ifelse(
  is_windows,
  "src/Makevars.win",
  "src/Makevars"
)

# The output is rewritten on every configure run.
if (file.exists(mv_ofp)) {
  message("Cleaning previous `", mv_ofp, "`.")
  invisible(file.remove(mv_ofp))
}

mv_txt <- readLines(mv_fp)

new_txt <- gsub("@CRAN_FLAGS@", .cran_flags, mv_txt) |>
  gsub("@CARGO_CONFIG@", .cargo_config, x = _, fixed = TRUE) |>
  gsub("@FEATURES@", .features, x = _, fixed = TRUE) |>
  gsub("@TARGET_DIR@", .target_dir, x = _, fixed = TRUE) |>
  gsub("@PROFILE@", .profile, x = _) |>
  gsub("@CLEAN_TARGET_DIR@", .clean_target_dir, x = _, fixed = TRUE) |>
  gsub("@CLEAN_TARGET@", .clean_targets, x = _) |>
  gsub("@LIBDIR@", .libdir, x = _) |>
  gsub("@TARGET@", .target, x = _) |>
  gsub("@PANIC_EXPORTS@", .panic_exports, x = _) |>
  gsub("@RUN_DOCUMENT@", .run_document, x = _, fixed = TRUE) |>
  gsub("@SAVE_LOCK@", .save_lock, x = _, fixed = TRUE) |>
  gsub("@RESTORE_LOCK@", .restore_lock, x = _, fixed = TRUE) |>
  gsub("@DROP_LOCK@", .drop_lock, x = _, fixed = TRUE)

message("Writing `", mv_ofp, "`.")
con <- file(mv_ofp, open = "wb")
writeLines(new_txt, con, sep = "\n")
close(con)

message("`tools/config.R` has finished.")
