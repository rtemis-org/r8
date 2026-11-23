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
    c(
      sprintf(
        ".libPaths(c(%s, .libPaths()))",
        paste(deparse(lib), collapse = "")
      ),
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
  for (p in c("r8pkgA", "r8pkgB", "r8pkgC", "r8pkgR")) {
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
  .test_packages[["lib"]] <- lib
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
      "y <- R8::r8_from_json(Rect, R8::r8_to_json(x))",
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
    "fields w,h,orientation " %in% out2,
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

test_that("a type declared outside a package registers on first use in a worker", {
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
      "options(r8.namespace = 'scratch')",
      "K <- R8::new_enum('K', variants = list(Linear = R8::variant(), Rbf = R8::variant(fields = list(gamma = R8::type_double()))))",
      "S <- R8::new_struct('S', fields = list(cost = R8::type_double(min = 0), kernel = R8::field(K, default = K@Linear)))",
      "cat('record', is.raw(attr(S, 'r8_record')), is.null(attr(S, 'r8_ir')), '\n')",
      "mirai::daemons(1)",
      "# The worker has no declaration of S or K; S's record holds both.",
      "m <- mirai::mirai({ v <- S(cost = 2); list(cost = v$cost, kernel = attr(v$kernel, 'r8_variant')) }, S = S)",
      "r <- mirai::collect_mirai(m)",
      "cat('mirai', r$cost, r$kernel, '\n')",
      "m2 <- mirai::mirai(tryCatch(S(cost = -1), r8_value_error = function(e) 'refused'), S = S)",
      "cat('mirai-invalid', mirai::collect_mirai(m2), '\n')",
      "m3 <- mirai::mirai(K@Rbf(gamma = 0.5)$gamma, K = K)",
      "cat('mirai-variant', mirai::collect_mirai(m3), '\n')",
      "mirai::daemons(0)",
      "future::plan(future::multisession, workers = 1)",
      "f <- future::future(S(cost = 3)$cost, globals = list(S = S), packages = character())",
      "cat('future', future::value(f), '\n')",
      "future::plan(future::sequential)"
    )
  )
  info <- paste(out, collapse = "\n")
  expect_true("record TRUE TRUE " %in% out, info = info)
  expect_true("mirai 2 Linear " %in% out, info = info)
  expect_true("mirai-invalid refused " %in% out, info = info)
  expect_true("mirai-variant 0.5 " %in% out, info = info)
  expect_true("future 3 " %in% out, info = info)
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
      "cat('json', nchar(R8::r8_to_json(x)) > 0, '\\n')"
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
      "cat('again', R8::r8_register('r8pkgC'), magnitude(Point(x = 6, y = 8)), '\\n')",
      "u <- tryCatch(ratio(p, NA_real_), error = function(e) conditionMessage(e))",
      "cat('unwind', u, '\\n')",
      "cat('status', suppressMessages(protect_status()), '\\n')",
      "cat('after', ratio(p, 2), '\\n')",
      "# A closure kept after its package unloads refuses the call.",
      "keys <- grep('|magnitude|', ls(R8:::.r8_impls), fixed = TRUE, value = TRUE)",
      "f <- get(keys[[1L]], envir = R8:::.r8_impls)",
      "unloadNamespace('r8pkgC')",
      "e <- tryCatch(f(p), error = function(e) conditionMessage(e))",
      "cat('unloaded', grepl('unloaded', e), '\\n')"
    )
  )
  info <- paste(out, collapse = "\n")
  expect_true("magnitude 5 " %in% out, info = info)
  expect_true("ratio 1.5 " %in% out, info = info)
  expect_true("error TRUE " %in% out, info = info)
  expect_true("display Point(3, 4) " %in% out, info = info)
  expect_true("print Point(3, 4) " %in% out, info = info)
  expect_true("again 7 10 " %in% out, info = info)
  # An R error raised under R8Api.protect reaches the caller after the impl
  # function returns.
  expect_true("unwind `by` is NA " %in% out, info = info)
  expect_true("status 2 " %in% out, info = info)
  expect_true("after 1.5 " %in% out, info = info)
  expect_true("unloaded TRUE " %in% out, info = info)
})

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
      "suppressPackageStartupMessages(library(r8pkgC))",
      "inspect <- function(x) utils::capture.output(.Internal(inspect(x, 0)))[1]",
      "s <- Signal(name = 'a', values = c(1.5, 2, 3))",
      "cat('rust_stored', grepl('r8 rust-stored pkgc::Signal', inspect(s)), '\\n')",
      "v <- s$values",
      "cat('view', grepl('r8 view', inspect(v)), grepl('in place', inspect(v)), v, '\\n')",
      "w <- v; w[1] <- 99",
      "cat('copy', w[1], s$values[1], '\\n')",
      "cat('equal', R8::r8_equal(s, Signal(name = 'a', values = c(1.5, 2, 3))), '\\n')",
      "cat('hash', R8::r8_hash(s) == R8::r8_hash(Signal(name = 'a', values = c(1.5, 2, 3))), '\\n')",
      "r <- R8::r8_from_json(Signal, R8::r8_to_json(s))",
      "cat('json', grepl('r8 rust-stored', inspect(r)), R8::r8_equal(r, s), '\\n')",
      "t <- s; t$name <- 'b'",
      "cat('set', t$name, s$name, t$values, '\\n')",
      "e <- tryCatch({u <- s; u$values <- 'x'}, error = function(e) 'r_type')",
      "cat('set_error', e, '\\n')",
      "cat('state', unlist(signal_peek(t)), '\\n')",
      "m <- signal_make('made', 3)",
      "cat('made', m$name, m$values, '\\n')",
      "x <- unserialize(serialize(s, NULL))",
      "cat('serialize', grepl('r8 rust-stored', inspect(x)), R8::r8_equal(x, s), '\\n')",
      sprintf("saveRDS(s, %s)", deparse(rds)),
      "cat('size', R8::r8_size(s) > 3 * 8, '\\n')",
      "y <- Signal(name = 'm', values = as.numeric(seq_len(1e6)))",
      "before <- R8::r8_size(y); shown <- format(y)[1]",
      "cat('header', shown, R8::r8_size(y) == before, '\\n')",
      "invisible(y$name); invisible(y$values)",
      "cat('str', utils::capture.output(str(y))[1], '\\n')",
      "rm(y)",
      "rm(s, u, v, w, r, t, m, x); invisible(gc())",
      "cat('dropped', live_signals(), '\\n')",
      "peak <- 0",
      "for (i in 1:40) { y <- Signal(name = 'b', values = as.numeric(seq_len(1e6))); peak <- max(peak, live_signals()) }",
      "cat('bounded', peak <= 9, '\\n')"
    )
  )
  info <- paste(out, collapse = "\n")
  expect_true("rust_stored TRUE " %in% out, info = info)
  expect_true("view TRUE TRUE 1.5 2 3 " %in% out, info = info)
  expect_true("copy 99 1.5 " %in% out, info = info)
  expect_true("equal TRUE " %in% out, info = info)
  expect_true("hash TRUE " %in% out, info = info)
  expect_true("json TRUE TRUE " %in% out, info = info)
  expect_true("set b a 1.5 2 3 " %in% out, info = info)
  expect_true("set_error r_type " %in% out, info = info)
  expect_true("state b 1.5 " %in% out, info = info)
  expect_true("made made 0 1 2 " %in% out, info = info)
  expect_true("serialize TRUE TRUE " %in% out, info = info)
  expect_true("size TRUE " %in% out, info = info)
  # print() shows the state's bytes and reads nothing; str() adds the fields
  # read that are not views of the state.
  expect_true("header <pkgc::Signal> 7.63 MiB TRUE " %in% out, info = info)
  expect_true("str <pkgc::Signal> 7.63 MiB + 112 B read " %in% out, info = info)
  expect_true("dropped 0 " %in% out, info = info)
  expect_true("bounded TRUE " %in% out, info = info)
  # A session that reads the value before loading the codec's package.
  out <- run_r(
    lib,
    c(
      sprintf("s <- readRDS(%s)", deparse(rds)),
      "e <- tryCatch(s$values, error = function(e) conditionMessage(e))",
      "cat('lazy', grepl('library(r8pkgC)', e, fixed = TRUE), '\\n')",
      "suppressPackageStartupMessages(library(r8pkgC))",
      "cat('loaded', s$name, s$values, '\\n')",
      "mirai::daemons(1)",
      "m <- mirai::mirai({ library(r8pkgC); list(sum(s$values), Signal(name = 'w', values = s$values * 2)) }, s = s)[]",
      "cat('mirai', m[[1]], m[[2]]$name, m[[2]]$values, '\\n')",
      "invisible(mirai::daemons(0))",
      "future::plan(future::multisession, workers = 1)",
      "f <- future::value(future::future({ library(r8pkgC); Signal(name = 'f', values = s$values + 1) }, globals = list(s = s)))",
      "cat('future', f$name, f$values, '\\n')",
      "future::plan(future::sequential)"
    )
  )
  info <- paste(out, collapse = "\n")
  expect_true("lazy TRUE " %in% out, info = info)
  expect_true("loaded a 1.5 2 3 " %in% out, info = info)
  expect_true("mirai 6.5 w 3 4 6 " %in% out, info = info)
  expect_true("future f 2.5 3 4 " %in% out, info = info)
})

test_that("types and impls in Rust register, dispatch, persist, and cross workers", {
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
      "suppressPackageStartupMessages(library(r8pkgR))",
      "inspect <- function(x) utils::capture.output(.Internal(inspect(x, 0)))[1]",
      "x <- c(3, 4)",
      "s <- Signal(name = 'a', values = x)",
      "cat('rust_stored', grepl('r8 rust-stored pkgr::Signal', inspect(s)), identical(attr(Signal, 'r8_storage'), 'rust'), '\\n')",
      "cat('fields', s$name, s$values, '\\n')",
      "cat('magnitude', magnitude(s), '\\n')",
      "cat('ratio', ratio(s, 2), '\\n')",
      "e <- tryCatch(ratio(s, 0), error = function(e) conditionMessage(e))",
      "cat('error', grepl('[zero_divisor]', e, fixed = TRUE), '\\n')",
      "cat('display', format(s), '\\n')",
      "a <- Signal(name = 'a', values = c(0, 0)); b <- Signal(name = 'b', values = c(3, 4))",
      "rn <- unserialize(serialize(b, NULL, version = 2))",
      "e <- tryCatch(distance(a, 1), error = function(e) conditionMessage(e))",
      "cat('distance', distance(a, b), distance(a, rn), grepl('`y` is not a `pkgr::Signal`', e, fixed = TRUE), '\\n')",
      "r <- R8::r8_from_json(Signal, R8::r8_to_json(s))",
      "cat('json', R8::r8_equal(r, s), '\\n')",
      "u <- unserialize(serialize(s, NULL))",
      "cat('serialize', R8::r8_equal(u, s), '\\n')",
      "mirai::daemons(1)",
      "m <- mirai::mirai({ library(r8pkgR); list(magnitude(s), Signal(name = 'w', values = s$values * 2)) }, s = s)[]",
      "cat('mirai', m[[1]], m[[2]]$name, m[[2]]$values, '\\n')",
      "invisible(mirai::daemons(0))",
      "future::plan(future::multisession, workers = 1)",
      "f <- future::value(future::future({ library(r8pkgR); Signal(name = 'f', values = s$values + 1) }, globals = list(s = s)))",
      "cat('future', f$name, f$values, '\\n')",
      "future::plan(future::sequential)"
    )
  )
  info <- paste(out, collapse = "\n")
  expect_true("rust_stored TRUE TRUE " %in% out, info = info)
  expect_true("fields a 3 4 " %in% out, info = info)
  expect_true("magnitude 5 " %in% out, info = info)
  expect_true("ratio 1.5 " %in% out, info = info)
  expect_true("error TRUE " %in% out, info = info)
  expect_true("display Signal a (2 values) " %in% out, info = info)
  # A `&Signal` parameter takes a Rust-stored value or an R-stored one.
  expect_true("distance 5 5 TRUE " %in% out, info = info)
  expect_true("json TRUE " %in% out, info = info)
  expect_true("serialize TRUE " %in% out, info = info)
  expect_true("mirai 5 w 6 8 " %in% out, info = info)
  expect_true("future f 4 5 " %in% out, info = info)
  # R/r8-types.R is current with the Rust declarations, and a stale copy is
  # detected and regenerated.
  pkg <- test_path("packages", "r8pkgR")
  out <- run_r(
    lib,
    sprintf(
      "R8::r8_document_rust(%s, check = TRUE); cat('current\\n')",
      deparse(normalizePath(pkg))
    )
  )
  expect_true("current" %in% out, info = paste(out, collapse = "\n"))
  copy <- tempfile("r8pkgR")
  dir.create(file.path(copy, "R"), recursive = TRUE)
  dir.create(file.path(copy, "src"))
  file.copy(file.path(pkg, "DESCRIPTION"), copy)
  file.copy(file.path(pkg, "src", "r8pkgR.so"), file.path(copy, "src"))
  generated <- readLines(file.path(pkg, "R", "r8-types.R"))
  writeLines(
    sub("storage = \"rust\"", "storage = \"r\"", generated),
    file.path(copy, "R", "r8-types.R")
  )
  out <- run_r(
    lib,
    c(
      sprintf("p <- %s", deparse(copy)),
      "e <- tryCatch(R8::r8_document_rust(p, check = TRUE), error = function(e) conditionMessage(e))",
      "cat('stale', grepl('differs from the declarations', e), '\\n')",
      "R8::r8_document_rust(p)",
      "cat('written\\n')"
    )
  )
  expect_true("stale TRUE " %in% out, info = paste(out, collapse = "\n"))
  expect_true(R8:::same_code(
    readLines(file.path(copy, "R", "r8-types.R")),
    generated
  ))
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
      "cat('kfold', rust_stored(k), attr(k, 'r8_variant'), names(k), k$k, format(k), '\\n')",
      "idx <- as.integer(c(2, 4, 6))",
      "f <- Resampler@Fixed(train = idx)",
      "cat('fixed', rust_stored(f), identical(f$train, idx), format(f), '\\n')",
      "l <- Resampler@Loocv",
      "cat('unit', rust_stored(l), length(l), attr(l, 'r8_variant'), format(l), '\\n')",
      "c1 <- Resampler@Custom(train = c(1L, 2L), test = 3L)",
      "cat('newtype', rust_stored(c1), names(c1), c1$test, format(c1), '\\n')",
      "k2 <- k; k2$k <- 10L",
      "cat('set', rust_stored(k2), k2$k, k$k, attr(k2, 'r8_variant'), '\\n')",
      "e <- tryCatch(k$train, error = function(e) 'no_field')",
      "cat('variant_field', e, '\\n')",
      "r <- R8::r8_from_json(Resampler, R8::r8_to_json(c1))",
      "cat('json', rust_stored(r), R8::r8_equal(r, c1), '\\n')",
      "u <- unserialize(serialize(f, NULL)); v <- unserialize(serialize(l, NULL))",
      "cat('serialize', rust_stored(u), R8::r8_equal(u, f), rust_stored(v), attr(v, 'r8_variant'), '\\n')",
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
      "suppressPackageStartupMessages(library(r8pkgR))",
      "cat('loaded', format(x[[1]]), format(x[[2]]), x[[3]]$train, '\\n')",
      "mirai::daemons(1)",
      "m <- mirai::mirai({ library(r8pkgR); list(format(f), Resampler@Bootstrap(n = 3L)) }, f = x[[1]])[]",
      "cat('mirai', m[[1]], format(m[[2]]), '\\n')",
      "invisible(mirai::daemons(0))",
      "future::plan(future::multisession, workers = 1)",
      "g <- future::value(future::future({ library(r8pkgR); c1$train }, globals = list(c1 = x[[3]])))",
      "cat('future', g, '\\n')",
      "future::plan(future::sequential)"
    )
  )
  info <- paste(out, collapse = "\n")
  expect_true(
    "loaded Fixed (3 training cases) LOOCV 1 2 " %in% out,
    info = info
  )
  expect_true(
    "mirai Fixed (3 training cases) Bootstrap (n = 3) " %in% out,
    info = info
  )
  expect_true("future 1 2 " %in% out, info = info)
})

test_that("an enum declared in R takes a codec in C, and a misshaped codec is refused", {
  skip_if(
    !nzchar(Sys.getenv("R8_PACKAGE_TESTS")),
    "set R8_PACKAGE_TESTS to install the test packages"
  )
  lib <- install_test_packages()
  out <- run_r(
    lib,
    c(
      "suppressPackageStartupMessages(library(r8pkgC))",
      "inspect <- function(x) utils::capture.output(.Internal(inspect(x, 0)))[1]",
      "rust_stored <- function(x) grepl('r8 rust-stored pkgc::Resampler', inspect(x))",
      "k <- Resampler@KFold(k = 3L)",
      "k2 <- k; k2$k <- 7L",
      "cat('kfold', rust_stored(k), k$k, rust_stored(k2), k2$k, '\\n')",
      "l <- Resampler@Loocv",
      "cat('unit', rust_stored(l), length(l), attr(l, 'r8_variant'), '\\n')",
      "r <- R8::r8_from_json(Resampler, R8::r8_to_json(k))",
      "cat('json', rust_stored(r), R8::r8_equal(r, k), '\\n')",
      "u <- unserialize(serialize(l, NULL))",
      "cat('serialize', rust_stored(u), attr(u, 'r8_variant'), '\\n')",
      "e <- tryCatch(R8:::r8_register_exports(r8pkgC:::bad_exports(1), 'r8pkgC', 'pkgc'), error = function(e) conditionMessage(e))",
      "cat('misshaped', grepl('does not match its type', e), '\\n')",
      "st <- function(k) R8:::r8_exports_compare(r8pkgC:::bad_exports(k), 'pkgc')$status",
      "cat('pins', st(2), st(3), '\\n')",
      "chk <- function(k) tryCatch(R8:::check_exports('r8pkgC', R8:::r8_exports_compare(r8pkgC:::bad_exports(k), 'pkgc'), 'pkgc::Signal'), error = function(e) conditionMessage(e))",
      "h <- R8:::r8_exports_compare(r8pkgC:::bad_exports(2), 'pkgc')$hash",
      "cat('stale', grepl(h, chk(2), fixed = TRUE), grepl('reads fields by position', chk(2)), '\\n')",
      "cat('unpinned', grepl('sets no `declaration_hash`', chk(3)), grepl(h, chk(3), fixed = TRUE), '\\n')",
      "ri <- function(k) tryCatch(R8:::r8_register_exports(r8pkgC:::bad_impls(k), 'r8pkgC', 'pkgc'), error = function(e) conditionMessage(e))",
      "cat('impls', grepl('two impl functions `magnitude`', ri(1)), grepl('`ratio` of `pkgc::Norm` for `pkgc::Point` is NULL', ri(2)), '\\n')",
      "s <- Signal(name = 'a', values = 1)",
      "cat('after', grepl('r8 rust-stored pkgc::Signal', inspect(s)), '\\n')"
    )
  )
  info <- paste(out, collapse = "\n")
  expect_true("kfold TRUE 3 TRUE 7 " %in% out, info = info)
  expect_true("unit TRUE 0 Loocv " %in% out, info = info)
  expect_true("json TRUE TRUE " %in% out, info = info)
  expect_true("serialize TRUE Loocv " %in% out, info = info)
  # The refused table leaves the registered codecs as they were.
  expect_true("misshaped TRUE " %in% out, info = info)
  # A codec written in C pins its type's declaration hash; a stale or missing
  # pin names the current hash.
  expect_true("pins stale unpinned " %in% out, info = info)
  expect_true("stale TRUE TRUE " %in% out, info = info)
  expect_true("unpinned TRUE TRUE " %in% out, info = info)
  expect_true("impls TRUE TRUE " %in% out, info = info)
  expect_true("after TRUE " %in% out, info = info)
})

test_that("the generator names a type of another namespace by its package's export", {
  skip_if(
    !nzchar(Sys.getenv("R8_PACKAGE_TESTS")),
    "set R8_PACKAGE_TESTS to install the test packages"
  )
  lib <- install_test_packages()
  out <- run_r(
    lib,
    c(
      "p <- tempfile(); dir.create(p)",
      "desc <- function(imports) writeLines(c('Package: x', paste0('Imports: ', imports)), file.path(p, 'DESCRIPTION'))",
      "field_t <- list(name = 'orientation', type = list(kind = 'named', name = 'pkga::Orientation'), description = '')",
      "doc <- list(namespace = 'model', types = list(list(kind = 'struct', name = 'Fit', description = '', version = 1L, fields = list(field_t))))",
      "desc('R8, r8pkgA (>= 0.0.1)')",
      "refs <- R8:::foreign_refs(doc, p)",
      "assign('map', refs, envir = R8:::.r8_render_refs)",
      "cat('ref', refs[['pkga::Orientation']], R8:::render_type(field_t$type, 'model'), '\\n')",
      "op <- options(r8.namespace = 'model')",
      "Fit <- R8::new_struct('Fit', fields = list(orientation = r8pkgA::Orientation))",
      "options(op)",
      "ir <- R8:::type_ir(Fit)",
      "lines <- R8:::render_struct(ir, 'model', rust_stored = FALSE)",
      "cat('rendered', any(grepl('orientation = r8pkgA::Orientation', lines, fixed = TRUE)), '\\n')",
      "desc('R8')",
      "e <- tryCatch(R8:::foreign_refs(doc, p), error = function(e) conditionMessage(e))",
      "cat('unimported', grepl('listed in Imports', e), '\\n')",
      "doc$types[[1]]$fields[[1]]$type$name <- 'pkga::Absent'",
      "desc('r8pkgA')",
      "e <- tryCatch(R8:::foreign_refs(doc, p), error = function(e) conditionMessage(e))",
      "cat('unexported', grepl('exports no type object', e), '\\n')"
    )
  )
  info <- paste(out, collapse = "\n")
  expect_true(
    "ref r8pkgA::Orientation r8pkgA::Orientation " %in% out,
    info = info
  )
  expect_true("rendered TRUE " %in% out, info = info)
  expect_true("unimported TRUE " %in% out, info = info)
  expect_true("unexported TRUE " %in% out, info = info)
})

test_that("the roclet r8_rd documents the methods a package implements in Rust", {
  skip_if(
    !nzchar(Sys.getenv("R8_PACKAGE_TESTS")),
    "set R8_PACKAGE_TESTS to install the test packages"
  )
  skip_if_not_installed("roxygen2")
  lib <- install_test_packages()
  # A copy of r8pkgR whose crates are the absolute paths of this checkout's.
  copy <- file.path(tempfile("roclet"), "r8pkgR")
  dir.create(dirname(copy))
  file.copy(test_path("packages", "r8pkgR"), dirname(copy), recursive = TRUE)
  unlink(Sys.glob(file.path(copy, "src", c("*.o", "*.so"))))
  cargo <- file.path(copy, "src", "rust", "Cargo.toml")
  crates <- normalizePath(test_path("..", "..", "..", "rust", "crates"))
  writeLines(
    gsub(
      "../../../../../../../rust/crates",
      crates,
      readLines(cargo),
      fixed = TRUE
    ),
    cargo
  )
  out <- run_r(
    lib,
    c(
      sprintf("roxygen2::roxygenise(%s, roclets = 'R8::r8_rd')", deparse(copy)),
      sprintf(
        "rd <- readLines(file.path(%s, 'man', 'Signal.Rd'))",
        deparse(copy)
      ),
      "cat('signal', sum(grepl(\"R8 method for struct 'Signal'\", rd, fixed = TRUE)), any(grepl('special{distance(x, y)}', rd, fixed = TRUE)), '\\n')",
      sprintf(
        "rd <- readLines(file.path(%s, 'man', 'Resampler.Rd'))",
        deparse(copy)
      ),
      "cat('resampler', any(grepl(\"R8 method for enum 'Resampler'\", rd, fixed = TRUE)), any(grepl('special{format(x, ...)}', rd, fixed = TRUE)), '\\n')"
    )
  )
  info <- paste(out, collapse = "\n")
  expect_true("signal 4 TRUE " %in% out, info = info)
  expect_true("resampler TRUE TRUE " %in% out, info = info)
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
      "label_of.Rd",
      "n_resamples.Rd",
      "pair_with.Rd",
      "seed-set.Rd"
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
  # A replacement generic's method, written as its call, and a converter's
  # method for a class, in the Rd files of the generic and the converter.
  expect_match(
    rd("seed-set.Rd"),
    "## R8 method for struct 'Resampler'\n\\special{seed(x) <- value}",
    fixed = TRUE
  )
  expect_match(
    rd("as_resampler.Rd"),
    "## R8 method for class 'character'\n\\special{as_resampler(x)}",
    fixed = TRUE
  )
  # The methods of a generic that dispatches on two arguments.
  expect_match(
    rd("pair_with.Rd"),
    "## R8 method for struct 'Resampler', enum 'Method'\n\\special{pair_with(x, y)}",
    fixed = TRUE
  )
  expect_match(
    rd("pair_with.Rd"),
    "## R8 method for any, struct 'Resampler'\n\\special{pair_with(x, y)}",
    fixed = TRUE
  )
  expect_match(
    rd("pair_with.Rd"),
    "## R8 method for class 'data.frame', struct 'Resampler'\n\\special{pair_with(x, y)}",
    fixed = TRUE
  )
  # The formals of a generic and a method's return type, which is no type of
  # the method's tuple.
  expect_match(
    rd("label_of.Rd"),
    "label_of(x, sep = default_sep(), n = 2L, method = NULL, ...)",
    fixed = TRUE
  )
  expect_match(
    rd("label_of.Rd"),
    "## R8 method for struct 'Resampler'\n\\special{label_of(x, sep, n, method, ...)}",
    fixed = TRUE
  )
  # The types of the parameters, a documented type as a link, and the return
  # types of the generic and of the method.
  for (text in c(
    "\\item{sep}{Character: The separator.}",
    "\\item{n}{Integer [1, Inf): The number of marks.}",
    "\\item{method}{Optional \\code{\\link{Method}}: The method.}",
    "\\value{\nCharacter.\n\nThe method for struct 'Resampler': Character.\n}"
  )) {
    expect_match(rd("label_of.Rd"), text, fixed = TRUE)
  }
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
      "print(Method@Bootstrap)",
      "seed(r) <- 9L",
      "cat(r$seed, n_resamples(as_resampler('abc')), '\\n')",
      "cat(pair_with(r, Method@Bootstrap), '|', pair_with(1, r), '|', pair_with(data.frame(), r), '\\n')",
      "cat(label_of(r), label_of(r, n = 3L, sep = '+'), '\\n')",
      "cat(class(tryCatch(label_of(r, n = 0), error = identity))[[1L]], label_of(r, n = 1), '\\n')"
    )
  )
  expect_identical(
    trimws(used),
    c(
      "2 resamples, seed 1",
      "a resampling method",
      "Resampler(2)",
      "2",
      "A resampling method",
      "9 3",
      "a resampler and a method | anything and a resampler | a data frame and a resampler",
      "r-r r+r+r",
      "r8_argument_error r"
    ),
    info = paste(used, collapse = "\n")
  )
  # The same methods in mirai and future workers, which load the package and
  # register its declarations there.
  skip_if_not_installed("mirai")
  skip_if_not_installed("future")
  task <- paste0(
    "{ library(r8pkgDoc); c(describe(r, verbose = TRUE), describe(Method@Bootstrap), ",
    "toString(r), n_resamples(r), utils::capture.output(print(Method@Bootstrap)), ",
    "pair_with(r, Method@Bootstrap)) }"
  )
  workers <- run_r(
    file.path(dir, "r8pkgDoc.Rcheck"),
    c(
      "library(r8pkgDoc)",
      "r <- Resampler(method = Method@Bootstrap, n_resamples = 2L, seed = 1L)",
      "mirai::daemons(1)",
      sprintf("m <- mirai::mirai(%s, r = r)", task),
      "cat('mirai', mirai::collect_mirai(m), sep = '|')",
      "cat('\\n')",
      "mirai::daemons(0)",
      "future::plan(future::multisession, workers = 1)",
      sprintf(
        "f <- future::future(%s, globals = list(r = r), packages = character())",
        task
      ),
      "cat('future', future::value(f), sep = '|')",
      "cat('\\n')",
      "future::plan(future::sequential)",
      # A generic sent to a worker that has not loaded its package.
      "mirai::daemons(1)",
      "m <- mirai::mirai({ before <- 'r8pkgDoc' %in% loadedNamespaces(); c(before, f(r), g(r), class(tryCatch(f(r, sep = 1), error = identity))[[1L]]) }, f = label_of, g = describe, r = r)",
      "cat('sent', mirai::collect_mirai(m), sep = '|')",
      "cat('\\n')",
      "mirai::daemons(0)"
    )
  )
  expected <- "2 resamples, seed 1|a resampling method|Resampler(2)|2|A resampling method|a resampler and a method"
  info <- paste(workers, collapse = "\n")
  expect_true(paste0("mirai|", expected) %in% workers, info = info)
  expect_true(paste0("future|", expected) %in% workers, info = info)
  # Unserializing the generic loads its package.
  expect_true(
    "sent|TRUE|r-r|2 resamples|r8_argument_error" %in% workers,
    info = info
  )
})

test_that("a package that masks a function its generics call is refused at load", {
  skip_if(
    !nzchar(Sys.getenv("R8_PACKAGE_TESTS")),
    "set R8_PACKAGE_TESTS to install the test packages"
  )
  dir <- tempfile("r8mask")
  dir.create(dir)
  file.copy(test_path("packages", "r8pkgA"), dir, recursive = TRUE)
  # A file after the generics'.
  writeLines(
    ".Call <- function(...) stop(\"masked\")",
    file.path(dir, "r8pkgA", "R", "zzzz.R")
  )
  lib <- file.path(dir, "library")
  dir.create(lib)
  out <- suppressWarnings(system2(
    file.path(R.home("bin"), "R"),
    c("CMD", "INSTALL", paste0("--library=", lib), file.path(dir, "r8pkgA")),
    stdout = TRUE,
    stderr = TRUE
  ))
  expect_true(
    any(grepl("base_masked", out, fixed = TRUE)),
    info = paste(out, collapse = "\n")
  )
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


test_that("a struct extends another package's struct, and a changed parent refuses it", {
  skip_if(
    !nzchar(Sys.getenv("R8_PACKAGE_TESTS")),
    "set R8_PACKAGE_TESTS to install the test packages"
  )
  lib <- install_test_packages()
  out <- run_r(
    lib,
    c(
      "suppressPackageStartupMessages({library(r8pkgA); library(r8pkgB)})",
      "x <- Panel(w = 3, h = 2, label = 'p')",
      "cat('class', paste(class(x), collapse = ','), '\\n')",
      "r <- tryCatch(Panel(w = 1, h = 2, label = 'p'), error = function(e) conditionMessage(e))",
      "cat('rule', grepl('WIDE', r), '\\n')",
      # pkga::Rect declared again with another field, as a reloaded r8pkgA
      # would: Panel is stale, and r8pkgA's load warns of it.
      "ir <- R8:::r8_type_ir('pkga::Rect')",
      "ir$fields <- c(ir$fields, list(list(name = 'd', type = list(kind = 'double'))))",
      "invisible(R8:::r8_declare_type(ir, 'pkga'))",
      "w <- tryCatch(R8:::stale_elsewhere('r8pkgA'), warning = function(w) w)",
      "cat('warning', inherits(w, 'r8_stale_warning'), grepl('pkgb::Panel', conditionMessage(w)), '\\n')",
      "e <- tryCatch(Panel(w = 3, h = 2, label = 'p'), error = function(e) conditionMessage(e))",
      "cat('stale', grepl('declare `pkgb::Panel` again', e, fixed = TRUE), '\\n')"
    )
  )
  info <- paste(out, collapse = "\n")
  expect_true("class pkgb::Panel,pkga::Rect,r8_value " %in% out, info = info)
  expect_true("rule TRUE " %in% out, info = info)
  expect_true("warning TRUE TRUE " %in% out, info = info)
  expect_true("stale TRUE " %in% out, info = info)
  # r8pkgB installed against an earlier r8pkgA is refused at load.
  src <- tempfile("r8pkgA2")
  dir.create(src)
  file.copy(test_path("packages", "r8pkgA"), src, recursive = TRUE)
  types <- file.path(src, "r8pkgA", "R", "types.R")
  writeLines(
    sub(
      "h = R8::type_double(min = 0),",
      "h = R8::type_double(min = 0),\n    d = R8::field(R8::type_double(), default = 0),",
      readLines(types),
      fixed = TRUE
    ),
    types
  )
  lib2 <- tempfile("r8lib2")
  dir.create(lib2)
  system2(
    file.path(R.home("bin"), "R"),
    c(
      "CMD",
      "INSTALL",
      "--no-test-load",
      paste0("--library=", lib2),
      file.path(src, "r8pkgA")
    ),
    stdout = TRUE,
    stderr = TRUE
  )
  out <- run_r(
    c(lib2, lib),
    c(
      "library(r8pkgA)",
      "r <- tryCatch(library(r8pkgB), error = function(e) conditionMessage(e))",
      "cat('refused', grepl('extends_stale', paste(r, collapse = ' ')), '\\n')"
    )
  )
  expect_true("refused TRUE " %in% out, info = paste(out, collapse = "\n"))
})
