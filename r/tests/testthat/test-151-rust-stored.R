# test-151-rust-stored.R
# ::R8::
# 2026- EDG rtemis.org

# Types whose values Rust holds, declared in a package's Rust code.
#
# A value behaves in R as any R8 value does. These tests check construction,
# reading and replacing fields, the memory that a value takes and its
# release, saving, parallel workers, and the conformance tests of every
# field type.

test_that("Rust-stored values read, change, persist, and cross workers", {
  skip_if(
    !nzchar(Sys.getenv("R8_PACKAGE_TESTS")),
    "set R8_PACKAGE_TESTS to install the test packages"
  )
  skip_if_not_installed("mirai")
  skip_if_not_installed("future")
  lib <- install_test_packages()
  rds <- tempfile(fileext = ".rds")
  out <- run_r(
    lib,
    c(
      "suppressPackageStartupMessages(library(r8pkgR))",
      "inspect <- function(x) utils::capture.output(.Internal(inspect(x, 0)))[1]",
      "# A compact sequence is copied into Rust, whose memory a large field views.",
      "s <- Signal(name = 'a', values = as.numeric(1:3))",
      "cat('rust_stored', grepl('r8 rust-stored pkgr::Signal', inspect(s)), '\\n')",
      "v <- s$values",
      "cat('small', grepl('r8', inspect(v)), v, '\\n')",
      "big <- Signal(name = 'a', values = as.numeric(seq_len(1e6)))",
      "b <- big$values",
      "cat('view', grepl('r8 view', inspect(b)), grepl('in place', inspect(b)), '\\n')",
      "used <- function(f) { before <- gc(reset = TRUE)['Vcells', 'used']; f(); gc()['Vcells', 'max used'] - before < 5e5 }",
      "cat('in_place', used(function() sum(big$values)), used(function() big$values[1:5]), '\\n')",
      "o <- order(big$values)",
      "cat('own_copy', grepl('in place', inspect(big$values)), '\\n')",
      "w <- b; w[1] <- 99",
      "cat('big_copy', w[1], big$values[1], R8::r8_hash(big) == R8::r8_hash(Signal(name = 'a', values = as.numeric(seq_len(1e6)))), '\\n')",
      "w <- v; w[1] <- 99",
      "cat('copy', w[1], s$values[1], '\\n')",
      "cat('equal', R8::r8_equal(s, Signal(name = 'a', values = c(1, 2, 3))), '\\n')",
      "cat('hash', R8::r8_hash(s) == R8::r8_hash(Signal(name = 'a', values = c(1, 2, 3))), '\\n')",
      "r <- R8::r8_from_json(Signal, R8::r8_to_json(s))",
      "cat('json', grepl('r8 rust-stored', inspect(r)), R8::r8_equal(r, s), '\\n')",
      "t <- s; t$name <- 'b'",
      "cat('set', t$name, s$name, t$values, '\\n')",
      "e <- tryCatch({u <- s; u$values <- 'x'}, error = function(e) 'r_type')",
      "cat('set_error', e, '\\n')",
      "x <- unserialize(serialize(s, NULL))",
      "cat('serialize', grepl('r8 rust-stored', inspect(x)), R8::r8_equal(x, s), '\\n')",
      sprintf("saveRDS(s, %s)", deparse(rds)),
      "cat('size', R8::r8_size(s) > 3 * 8, '\\n')",
      "y <- Signal(name = 'm', values = as.numeric(seq_len(1e6)))",
      "before <- R8::r8_size(y); shown <- format(R8::r8_layout(y))[1]",
      "cat('header', shown, R8::r8_size(y) == before, '\\n')",
      "invisible(y$name); invisible(y$values)",
      "cat('str', utils::capture.output(str(y))[1], '\\n')",
      "rm(y)",
      "rm(s, u, v, w, r, t, x, b, big); invisible(gc()); invisible(gc())",
      "live <- function() R8:::r8_rust_stored_memory()[1]",
      "cat('dropped', live(), '\\n')",
      "peak <- 0",
      "for (i in 1:40) { y <- Signal(name = 'b', values = as.numeric(seq_len(1e6))); peak <- max(peak, live()) }",
      "cat('bounded', peak <= 9 * 8e6, '\\n')"
    )
  )
  info <- paste(out, collapse = "\n")
  expect_true("rust_stored TRUE " %in% out, info = info)
  expect_true("small FALSE 1 2 3 " %in% out, info = info)
  expect_true("view TRUE TRUE " %in% out, info = info)
  expect_true("in_place TRUE TRUE " %in% out, info = info)
  expect_true("own_copy TRUE " %in% out, info = info)
  expect_true("big_copy 99 1 TRUE " %in% out, info = info)
  expect_true("copy 99 1 " %in% out, info = info)
  expect_true("equal TRUE " %in% out, info = info)
  expect_true("hash TRUE " %in% out, info = info)
  expect_true("json TRUE TRUE " %in% out, info = info)
  expect_true("set b a 1 2 3 " %in% out, info = info)
  expect_true("set_error r_type " %in% out, info = info)
  expect_true("serialize TRUE TRUE " %in% out, info = info)
  expect_true("size TRUE " %in% out, info = info)
  # print() shows the state's bytes and reads nothing; str() adds the fields
  # read that are not views of the state.
  expect_true("header <pkgr::Signal> 7.63 MiB TRUE " %in% out, info = info)
  expect_true("str <pkgr::Signal> 7.63 MiB + 112 B read " %in% out, info = info)
  expect_true("dropped 0 " %in% out, info = info)
  expect_true("bounded TRUE " %in% out, info = info)
  # A session that reads the value before loading the codec's package.
  out <- run_r(
    lib,
    c(
      sprintf("s <- readRDS(%s)", deparse(rds)),
      "cat('alone', s$name, s$values, '\\n')",
      "invisible(serialize(s, NULL))",
      "suppressPackageStartupMessages(library(r8pkgR))",
      "cat('loaded', s$name, s$values, '\\n')",
      "mirai::daemons(1)",
      "m <- mirai::mirai({ library(r8pkgR); list(sum(s$values), Signal(name = 'w', values = s$values * 2)) }, s = s)[]",
      "cat('mirai', m[[1]], m[[2]]$name, m[[2]]$values, '\\n')",
      "invisible(mirai::daemons(0))",
      "future::plan(future::multisession, workers = 1)",
      "f <- future::value(future::future({ library(r8pkgR); Signal(name = 'f', values = s$values + 1) }, globals = list(s = s)))",
      "cat('future', f$name, f$values, '\\n')",
      "future::plan(future::sequential)"
    )
  )
  info <- paste(out, collapse = "\n")
  expect_true("alone a 1 2 3 " %in% out, info = info)
  expect_true("loaded a 1 2 3 " %in% out, info = info)
  expect_true("mirai 6 w 2 4 6 " %in% out, info = info)
  expect_true("future f 2 3 4 " %in% out, info = info)
})

test_that("Rust-stored enums construct, read, change, persist, and cross workers", {
  skip_if(
    !nzchar(Sys.getenv("R8_PACKAGE_TESTS")),
    "set R8_PACKAGE_TESTS to install the test packages"
  )
  skip_if_not_installed("mirai")
  skip_if_not_installed("future")
  lib <- install_test_packages()
  rds <- tempfile(fileext = ".rds")
  out <- run_r(
    lib,
    c(
      "suppressPackageStartupMessages(library(r8pkgR))",
      "inspect <- function(x) utils::capture.output(.Internal(inspect(x, 0)))[1]",
      "rust_stored <- function(x) grepl('r8 rust-stored pkgr::Resampler', inspect(x))",
      "k <- Resampler@KFold(k = 5L, seed = 1L)",
      "cat('kfold', rust_stored(k), R8::r8_variant(k), names(k), k$k, format(k), '\\n')",
      "idx <- as.integer(c(2, 4, 6))",
      "f <- Resampler@Fixed(train = idx)",
      "cat('fixed', rust_stored(f), identical(f$train, idx), format(f), '\\n')",
      "l <- Resampler@Loocv",
      "cat('unit', rust_stored(l), length(l), R8::r8_variant(l), format(l), '\\n')",
      "c1 <- Resampler@Custom(train = c(1L, 2L), test = 3L)",
      "cat('newtype', rust_stored(c1), names(c1), c1$test, format(c1), '\\n')",
      "k2 <- k; k2$k <- 10L",
      "cat('set', rust_stored(k2), k2$k, k$k, R8::r8_variant(k2), '\\n')",
      "k3 <- k; k3[c('k', 'seed')] <- list(7L, 2L)",
      "cat('several', rust_stored(k3), k3$k, k3$seed, k$k, k$seed, R8::r8_variant(k3), '\\n')",
      "e <- tryCatch(R8::r8_is_variant(k, 'KFol'), r8_value_error = conditionMessage)",
      "cat('is_variant', R8::r8_is_variant(k, 'KFold'), R8::r8_is_variant(k, 'Loocv'), grepl('nearest variant is `KFold`', e, fixed = TRUE), '\\n')",
      "e <- tryCatch(R8::r8_variant(Signal(name = 'a', values = 1)), r8_type_error = conditionMessage)",
      "cat('struct', grepl('`pkgr::Signal` is a struct', e, fixed = TRUE), '\\n')",
      "e <- tryCatch(k$train, error = function(e) 'no_field')",
      "cat('variant_field', e, '\\n')",
      "r <- R8::r8_from_json(Resampler, R8::r8_to_json(c1))",
      "cat('json', rust_stored(r), R8::r8_equal(r, c1), '\\n')",
      "u <- unserialize(serialize(f, NULL)); v <- unserialize(serialize(l, NULL))",
      "cat('serialize', rust_stored(u), R8::r8_equal(u, f), rust_stored(v), R8::r8_variant(v), '\\n')",
      "cat('hash', R8::r8_hash(k) == R8::r8_hash(Resampler@KFold(k = 5L, seed = 1L)), '\\n')",
      "e <- tryCatch(Resampler@KFold(k = 'a', seed = 1L), error = function(e) 'r_type')",
      "cat('invalid', e, '\\n')",
      sprintf("saveRDS(list(f, l, c1), %s)", deparse(rds))
    )
  )
  info <- paste(out, collapse = "\n")
  expect_true(
    "kfold TRUE KFold k seed 5 KFold (k = 5, seed = 1) " %in% out,
    info = info
  )
  expect_true("fixed TRUE TRUE Fixed (3 training cases) " %in% out, info = info)
  expect_true("unit TRUE 0 Loocv LOOCV " %in% out, info = info)
  expect_true(
    "newtype TRUE train test 3 Custom (2 train, 1 test) " %in% out,
    info = info
  )
  expect_true("set TRUE 10 5 KFold " %in% out, info = info)
  expect_true("several TRUE 7 2 5 1 KFold " %in% out, info = info)
  expect_true("is_variant TRUE FALSE TRUE " %in% out, info = info)
  expect_true("struct TRUE " %in% out, info = info)
  expect_true("variant_field no_field " %in% out, info = info)
  expect_true("json TRUE TRUE " %in% out, info = info)
  expect_true("serialize TRUE TRUE TRUE Loocv " %in% out, info = info)
  expect_true("hash TRUE " %in% out, info = info)
  expect_true("invalid r_type " %in% out, info = info)
  # A session that reads the values before loading the codec's package, and
  # workers in both directions.
  out <- run_r(
    lib,
    c(
      sprintf("x <- readRDS(%s)", deparse(rds)),
      # The variant is read without the type's package, and a name that is no
      # variant is refused by the declaration that the value carries.
      "cat('before', R8::r8_variant(x[[2]]), R8::r8_is_variant(x[[2]], 'Loocv'), tryCatch(R8::r8_is_variant(x[[2]], 'Lookv'), r8_value_error = function(e) 'refused'), 'r8pkgR' %in% loadedNamespaces(), '\\n')",
      "suppressPackageStartupMessages(library(r8pkgR))",
      "cat('after', tryCatch(R8::r8_is_variant(x[[2]], 'Lookv'), r8_value_error = function(e) 'refused'), '\\n')",
      "cat('loaded', format(x[[1]]), format(x[[2]]), x[[3]]$train, '\\n')",
      "mirai::daemons(1)",
      "m <- mirai::mirai({ library(r8pkgR); list(format(f), Resampler@Bootstrap(n = 3L), R8::r8_variant(f)) }, f = x[[1]])[]",
      "cat('mirai', m[[1]], format(m[[2]]), m[[3]], R8::r8_variant(m[[2]]), '\\n')",
      "invisible(mirai::daemons(0))",
      "future::plan(future::multisession, workers = 1)",
      "g <- future::value(future::future({ library(r8pkgR); c1$train }, globals = list(c1 = x[[3]])))",
      "cat('future', g, '\\n')",
      "future::plan(future::sequential)"
    )
  )
  info <- paste(out, collapse = "\n")
  expect_true("before Loocv TRUE refused FALSE " %in% out, info = info)
  expect_true("after refused " %in% out, info = info)
  expect_true(
    "loaded Fixed (3 training cases) LOOCV 1 2 " %in% out,
    info = info
  )
  expect_true(
    "mirai Fixed (3 training cases) Bootstrap (n = 3) Fixed Bootstrap " %in%
      out,
    info = info
  )
  expect_true("future 1 2 " %in% out, info = info)
})

test_that("the conformance tests pass against the Rust-stored pkgr::Probe", {
  skip_if(
    !nzchar(Sys.getenv("R8_PACKAGE_TESTS")),
    "set R8_PACKAGE_TESTS to install the test packages"
  )
  lib <- install_test_packages()
  dir <- normalizePath(test_path("conformance"))
  results <- tempfile(fileext = ".rds")
  out <- run_r(
    lib,
    c(
      "suppressPackageStartupMessages({ library(R8); library(r8pkgR); library(testthat) })",
      "options(r8.conformance = 'rust')",
      sprintf(
        "res <- testthat::test_dir(%s, reporter = 'silent', stop_on_failure = FALSE, stop_on_warning = FALSE)",
        deparse(dir)
      ),
      sprintf("saveRDS(as.data.frame(res), %s)", deparse(results))
    )
  )
  info <- paste(out, collapse = "\n")
  expect_true(file.exists(results), info = info)
  res <- readRDS(results)
  failed <- res[["test"]][res[["failed"]] > 0L | res[["error"]]]
  expect_identical(
    failed,
    character(),
    info = paste(c(failed, info), collapse = "\n")
  )
  # The field types without a Rust field type. A Rust field type added later
  # fails this expectation until its test leaves the list.
  expect_identical(
    res[["test"]][res[["skipped"]]],
    "fields of every R8 field type round trip through JSON"
  )
  expect_gt(sum(res[["nb"]]), 60L)
  # The R mirror of the conformance tests declares the type r8pkgR declares
  # in Rust, apart from the namespace.
  out <- run_r(
    lib,
    c(
      "suppressPackageStartupMessages({ library(R8); library(r8pkgR) })",
      sprintf("source(%s)", deparse(file.path(dir, "helper-conformance.R"))),
      "mirror <- conformance_r_types()[['Probe']]",
      "rust <- gsub('pkgr', 'conf', r8_declaration(r8pkgR::Probe), fixed = TRUE)",
      "cat('mirror', identical(rust, r8_declaration(mirror)), '\\n')"
    )
  )
  expect_true("mirror TRUE " %in% out, info = paste(out, collapse = "\n"))
})
