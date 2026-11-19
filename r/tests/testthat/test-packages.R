# test-packages.R
# ::R8::
# 2026- EDG rtemis.org

# Installs packages that declare R8 types at their top level, and one that
# implements traits in C through R8's C interface, and checks them in fresh R
# sessions: registration at load, construction with rules, dispatch across
# packages and into compiled code, documents, saveRDS(), and readRDS() in a
# session without the packages. The installation takes minutes and uses the installed
# R8, so the test runs when R8_PACKAGE_TESTS is set (`just test-packages`).

run_r <- function(lib, code) {
  script <- tempfile(fileext = ".R")
  writeLines(
    c(sprintf(".libPaths(c(%s, .libPaths()))", deparse(lib)), code),
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

install_test_packages <- function() {
  lib <- tempfile("r8lib")
  dir.create(lib)
  for (p in c("r8pkgA", "r8pkgB", "r8pkgC")) {
    out <- system2(
      file.path(R.home("bin"), "R"),
      c(
        "CMD",
        "INSTALL",
        "--no-test-load",
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
  lib
}

test_that("packages declaring R8 types register, dispatch, and persist", {
  skip_if(
    !nzchar(Sys.getenv("R8_PACKAGE_TESTS")),
    "set R8_PACKAGE_TESTS to install the test packages"
  )
  lib <- install_test_packages()
  rds <- tempfile(fileext = ".rds")
  out <- run_r(
    lib,
    c(
      "suppressPackageStartupMessages({library(r8pkgA); library(r8pkgB)})",
      "x <- Rect(w = 3, h = 2)",
      "cat('area', area(x), '\\n')",
      "cat('circle', round(area(Circle(r = 1)), 4), '\\n')",
      "cat('class', class(x)[1], '\\n')",
      "r <- tryCatch(Rect(w = 1, h = 2), error = function(e) conditionMessage(e))",
      "cat('rule', grepl('WIDE', r), '\\n')",
      "y <- R8::r8_from_json(Rect, R8::to_json(x))",
      "cat('roundtrip', identical(x, y), '\\n')",
      sprintf("saveRDS(x, %s)", deparse(rds)),
      "cat('hash', R8::r8_hash(x), '\\n')"
    )
  )
  expect_true("area 6 " %in% out, info = paste(out, collapse = "\n"))
  expect_true("circle 3.1416 " %in% out, info = paste(out, collapse = "\n"))
  expect_true("class pkga::Rect " %in% out, info = paste(out, collapse = "\n"))
  expect_true("rule TRUE " %in% out, info = paste(out, collapse = "\n"))
  expect_true("roundtrip TRUE " %in% out, info = paste(out, collapse = "\n"))
  hash <- sub("^hash ", "", grep("^hash ", out, value = TRUE))
  # A value read in a session without the packages is a classed list.
  out2 <- run_r(
    lib,
    c(
      sprintf("x <- readRDS(%s)", deparse(rds)),
      "cat('fields', paste(names(unclass(x)), collapse = ','), '\\n')"
    )
  )
  expect_true(
    "fields w,h,shape " %in% out2,
    info = paste(out2, collapse = "\n")
  )
  # Read again with the packages loaded, it is the same value.
  out3 <- run_r(
    lib,
    c(
      "suppressPackageStartupMessages(library(r8pkgA))",
      sprintf("x <- readRDS(%s)", deparse(rds)),
      "cat('hash', R8::r8_hash(x), '\\n')",
      "cat('area', area(x), '\\n')"
    )
  )
  expect_true(
    paste0("hash ", hash) %in% out3,
    info = paste(out3, collapse = "\n")
  )
  expect_true("area 6 " %in% out3, info = paste(out3, collapse = "\n"))
})


test_that("values cross to mirai and future workers and back", {
  skip_if(
    !nzchar(Sys.getenv("R8_PACKAGE_TESTS")),
    "set R8_PACKAGE_TESTS to install the test packages"
  )
  skip_if_not_installed("mirai")
  skip_if_not_installed("future")
  lib <- install_test_packages()
  out <- run_r(
    lib,
    c(
      "suppressPackageStartupMessages(library(r8pkgA))",
      "x <- Rect(w = 3, h = 2)",
      "mirai::daemons(1)",
      "# A worker that has not loaded the defining package receives a classed list.",
      "m1 <- mirai::mirai(list(loaded = 'r8pkgA' %in% loadedNamespaces(), w = unclass(x)[['w']]), x = x)",
      "r1 <- mirai::collect_mirai(m1)",
      "cat('mirai-unloaded', r1$loaded, r1$w, '\\n')",
      "# A worker that loads it dispatches and constructs; the value comes back equal.",
      "m2 <- mirai::mirai({ library(r8pkgA); list(a = area(x), v = Rect(w = 4, h = 1)) }, x = x)",
      "r2 <- mirai::collect_mirai(m2)",
      "cat('mirai-area', r2$a, '\\n')",
      "cat('mirai-value', R8::r8_equal(r2$v, Rect(w = 4, h = 1)), identical(r2$v, Rect(w = 4, h = 1)), '\\n')",
      "mirai::daemons(0)",
      "future::plan(future::multisession, workers = 1)",
      "f <- future::future({ library(r8pkgA); list(a = area(x), v = Rect(w = 5, h = 5)) }, globals = list(x = x), packages = character())",
      "r3 <- future::value(f)",
      "cat('future-area', r3$a, '\\n')",
      "cat('future-value', R8::r8_hash(r3$v) == R8::r8_hash(Rect(w = 5, h = 5)), '\\n')",
      "future::plan(future::sequential)"
    )
  )
  info <- paste(out, collapse = "\n")
  expect_true("mirai-unloaded FALSE 3 " %in% out, info = info)
  expect_true("mirai-area 6 " %in% out, info = info)
  expect_true("mirai-value TRUE TRUE " %in% out, info = info)
  expect_true("future-area 6 " %in% out, info = info)
  expect_true("future-value TRUE " %in% out, info = info)
})

test_that("load_all() twice replaces a package's declarations", {
  skip_if(
    !nzchar(Sys.getenv("R8_PACKAGE_TESTS")),
    "set R8_PACKAGE_TESTS to install the test packages"
  )
  skip_if_not_installed("pkgload")
  lib <- tempfile("r8lib")
  dir.create(lib)
  path <- normalizePath(test_path("packages", "r8pkgA"))
  out <- run_r(
    lib,
    c(
      sprintf("pkgload::load_all(%s, quiet = TRUE)", deparse(path)),
      sprintf("pkgload::load_all(%s, quiet = TRUE)", deparse(path)),
      "x <- Rect(w = 3, h = 2)",
      "cat('area', area(x), '\\n')",
      "cat('json', nchar(R8::to_json(x)) > 0, '\\n')"
    )
  )
  info <- paste(out, collapse = "\n")
  expect_true("area 6 " %in% out, info = info)
  expect_true("json TRUE " %in% out, info = info)
})

test_that("impls in C register through R8.h and dispatch", {
  skip_if(
    !nzchar(Sys.getenv("R8_PACKAGE_TESTS")),
    "set R8_PACKAGE_TESTS to install the test packages"
  )
  lib <- install_test_packages()
  out <- run_r(
    lib,
    c(
      "suppressPackageStartupMessages(library(r8pkgC))",
      "p <- Point(x = 3, y = 4)",
      "cat('magnitude', magnitude(p), '\\n')",
      "cat('ratio', ratio(p, 2), '\\n')",
      "e <- tryCatch(ratio(p, 0), error = function(e) conditionMessage(e))",
      "cat('error', grepl('[zero_divisor]', e, fixed = TRUE), '\\n')",
      "cat('display', format(p), '\\n')",
      "cat('print', utils::capture.output(print(p)), '\\n')",
      "cat('again', R8::r8_register('r8pkgC'), magnitude(Point(x = 6, y = 8)), '\\n')"
    )
  )
  info <- paste(out, collapse = "\n")
  expect_true("magnitude 5 " %in% out, info = info)
  expect_true("ratio 1.5 " %in% out, info = info)
  expect_true("error TRUE " %in% out, info = info)
  expect_true("display Point(3, 4) " %in% out, info = info)
  expect_true("print Point(3, 4) " %in% out, info = info)
  expect_true("again 5 10 " %in% out, info = info)
})

test_that("roxygen2 documents R8 declarations and the package passes check", {
  skip_if(
    !nzchar(Sys.getenv("R8_PACKAGE_TESTS")),
    "set R8_PACKAGE_TESTS to install the test packages"
  )
  skip_if_not_installed("roxygen2")
  # A copy, so the generated NAMESPACE, Rd files, and Collate field stay out
  # of the source tree and every run generates them anew.
  dir <- tempfile("r8doc")
  dir.create(dir)
  file.copy(test_path("packages", "r8pkgDoc"), dir, recursive = TRUE)
  pkg <- file.path(dir, "r8pkgDoc")
  out <- run_r(
    character(),
    c(
      "options(warn = 1)",
      sprintf("roxygen2::roxygenise(%s)", deparse(pkg))
    )
  )
  info <- paste(out, collapse = "\n")
  expect_false(any(grepl("warning", out, ignore.case = TRUE)), info = info)
  expect_setequal(
    list.files(file.path(pkg, "man")),
    c(
      "Method.Rd",
      "Resample.Rd",
      "Resampler.Rd",
      "Summarize.Rd",
      "as_resampler.Rd",
      "describe.Rd",
      "n_resamples.Rd"
    )
  )
  ns <- readLines(file.path(pkg, "NAMESPACE"))
  expect_true("export(resample)" %in% ns, info = paste(ns, collapse = "\n"))
  expect_true("export(describe)" %in% ns, info = paste(ns, collapse = "\n"))
  rd <- function(name) {
    paste(readLines(file.path(pkg, "man", name)), collapse = "\n")
  }
  # The struct's usage from its constructor's formals; the enum's from the
  # roclet, one line per variant.
  expect_match(
    rd("Resampler.Rd"),
    "Resampler(method, n_resamples, seed = NULL)",
    fixed = TRUE
  )
  expect_match(rd("Method.Rd"), "\\special{Method@KFold(k)}", fixed = TRUE)
  expect_match(rd("Method.Rd"), "\\special{Method@Bootstrap}", fixed = TRUE)
  # The methods of the package's generic go to the generic's Rd file, the
  # methods of base generics to the type's; one method of each has no roxygen
  # block.
  expect_match(
    rd("describe.Rd"),
    "## R8 method for struct 'Resampler'\n\\special{describe(x, ..., verbose = FALSE)}",
    fixed = TRUE
  )
  expect_match(
    rd("describe.Rd"),
    "## R8 method for enum 'Method'\n\\special{describe(x, ...)}",
    fixed = TRUE
  )
  expect_match(
    rd("describe.Rd"),
    "\\item{verbose}{Logical: Include the seed.}",
    fixed = TRUE
  )
  expect_match(
    rd("Resampler.Rd"),
    "## R8 method for struct 'Resampler'\n\\special{toString(x, ...)}",
    fixed = TRUE
  )
  expect_match(
    rd("Method.Rd"),
    "## R8 method for enum 'Method'\n\\special{print(x, ...)}",
    fixed = TRUE
  )
  # A method of the package's own S3 generic, in the generic's Rd file.
  expect_match(
    rd("n_resamples.Rd"),
    "## R8 method for struct 'Resampler'\n\\special{n_resamples(x, ...)}",
    fixed = TRUE
  )
  # A second run writes nothing.
  again <- run_r(
    character(),
    sprintf("roxygen2::roxygenise(%s)", deparse(pkg))
  )
  expect_false(
    any(grepl("^Writing", again)),
    info = paste(again, collapse = "\n")
  )
  rbin <- file.path(R.home("bin"), "R")
  old <- setwd(dir)
  on.exit(setwd(old), add = TRUE)
  built <- system2(
    rbin,
    c("CMD", "build", "r8pkgDoc"),
    stdout = TRUE,
    stderr = TRUE
  )
  tarball <- list.files(dir, pattern = "^r8pkgDoc_.*[.]tar[.]gz$")
  expect_length(tarball, 1L)
  check <- suppressWarnings(system2(
    rbin,
    c("CMD", "check", "--no-manual", tarball),
    stdout = TRUE,
    stderr = TRUE
  ))
  expect_true(
    "Status: OK" %in% check,
    info = paste(c(built, check), collapse = "\n")
  )
  # In a fresh session, r8_register() restores the generic's methods, the S3
  # methods of a base generic and of the package's own S3 generic, and the
  # Print impl.
  used <- run_r(
    file.path(dir, "r8pkgDoc.Rcheck"),
    c(
      "library(r8pkgDoc)",
      "r <- Resampler(method = Method@Bootstrap, n_resamples = 2L, seed = 1L)",
      "cat(describe(r, verbose = TRUE), '\\n')",
      "cat(describe(Method@Bootstrap), '\\n')",
      "cat(toString(r), '\\n')",
      "cat(n_resamples(r), '\\n')",
      "print(Method@Bootstrap)"
    )
  )
  expect_identical(
    trimws(used),
    c(
      "2 resamples, seed 1",
      "a resampling method",
      "Resampler(2)",
      "2",
      "A resampling method"
    ),
    info = paste(used, collapse = "\n")
  )
})
