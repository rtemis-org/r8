# test-153-documentation.R
# ::R8::
# 2026- EDG rtemis.org

# The documentation of packages that declare R8 types: help pages from
# roxygen2, and the R declarations of types declared in Rust.

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
      # A struct that extends a struct of another namespace, whose declaration
      # the registry holds.
      "op <- options(r8.namespace = 'model')",
      "Box <- R8::new_struct('Box', fields = list(depth = R8::type_double()), extends = r8pkgA::Rect)",
      "options(op)",
      "doc2 <- list(namespace = 'model', types = list(R8:::type_ir(Box)))",
      "assign('map', R8:::foreign_refs(doc2, p), envir = R8:::.r8_render_refs)",
      "lines <- R8:::render_struct(doc2$types[[1]], 'model', rust_stored = FALSE)",
      "cat('extends', any(grepl('extends = r8pkgA::Rect', lines, fixed = TRUE)), any(grepl('defaults', lines)), '\\n')",
      "desc('R8')",
      "e <- tryCatch(R8:::foreign_refs(doc, p), error = function(e) conditionMessage(e))",
      "cat('unimported', grepl('listed in Imports', e), '\\n')",
      "doc$types[[1]]$fields[[1]]$type$name <- 'pkga::Absent'",
      "desc('r8pkgA')",
      "e <- tryCatch(R8:::foreign_refs(doc, p), error = function(e) conditionMessage(e))",
      "cat('unexported', grepl('exports no type or trait object', e), '\\n')"
    )
  )
  info <- paste(out, collapse = "\n")
  expect_true(
    "ref r8pkgA::Orientation r8pkgA::Orientation " %in% out,
    info = info
  )
  expect_true("rendered TRUE " %in% out, info = info)
  expect_true("extends TRUE FALSE " %in% out, info = info)
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
      "cat('resampler', any(grepl(\"R8 method for enum 'Resampler'\", rd, fixed = TRUE)), any(grepl('special{format(x, ...)}', rd, fixed = TRUE)), '\\n')",
      # Methods on several positions, and a replacement method.
      sprintf(
        "rd <- function(f) readLines(file.path(%s, 'man', f))",
        deparse(copy)
      ),
      "has <- function(f, text) any(grepl(text, rd(f), fixed = TRUE))",
      "cat('positions', has('meets.Rd', \"R8 method for struct 'Signal', any\"), has('meets.Rd', \"R8 method for struct 'Interval', struct 'Band'\"), has('meets.Rd', 'special{meets(x, y, by)}'), '\\n')",
      "cat('replace', has('label-set.Rd', 'special{label(x) <- value}'), '\\n')",
      # A generic and a trait declared in Rust.
      "cat('declared', has('shifted.Rd', 'item{by}{Double'), has('shifted.Rd', 'The distance'), has('Figure.Rd', 'A plane figure'), has('scaled.Rd', 'special{scaled(x, by)}'), '\\n')",
      "cat('from', has('Interval.Rd', 'special{from(x)}'), '\\n')"
    )
  )
  info <- paste(out, collapse = "\n")
  expect_true("signal 4 TRUE " %in% out, info = info)
  expect_false(any(startsWith(out, "\u2716")), info = info)
  expect_true("resampler TRUE TRUE " %in% out, info = info)
  expect_true("positions TRUE TRUE TRUE " %in% out, info = info)
  expect_true("replace TRUE " %in% out, info = info)
  expect_true("declared TRUE TRUE TRUE TRUE " %in% out, info = info)
  # A conversion has no entry of its own.
  expect_true("from FALSE " %in% out, info = info)
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
      "clip_resamples.Rd",
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
  # A typed function: its usage, the types of its parameters, and its return type.
  for (text in c(
    "clip_resamples(r, most = 10L)",
    "\\item{r}{\\code{\\link{Resampler}}: The resampler.}",
    "\\item{most}{Integer [1, Inf): The most resamples.}",
    "\\value{\nInteger.\n}"
  )) {
    expect_match(rd("clip_resamples.Rd"), text, fixed = TRUE)
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
      "cat(class(tryCatch(label_of(r, n = 0), error = identity))[[1L]], label_of(r, n = 1), '\\n')",
      "cat(clip_resamples(r), clip_resamples(r, most = 1), class(tryCatch(clip_resamples(1), error = identity))[[1L]], '\\n')"
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
      "r8_argument_error r",
      "2 1 r8_argument_error"
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
      # A typed function sent to a worker that has not loaded its package.
      "m <- mirai::mirai({ before <- 'r8pkgDoc' %in% loadedNamespaces(); c(before, f(r, most = 1), class(tryCatch(f(1), error = identity))[[1L]]) }, f = clip_resamples, r = r)",
      "cat('function', mirai::collect_mirai(m), sep = '|')",
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
  expect_true("function|TRUE|1|r8_argument_error" %in% workers, info = info)
})
