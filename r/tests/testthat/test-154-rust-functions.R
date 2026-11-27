# test-154-rust-functions.R
# ::R8::
# 2026- EDG rtemis.org

# Typed functions whose bodies are in Rust: the functions of r8pkgR that
# R/functions.R declares with rust(), and src/rust/src/functions.rs holds.

test_that("a function whose body is in Rust computes, checks, and errs as its declaration says", {
  skip_if(
    !nzchar(Sys.getenv("R8_PACKAGE_TESTS")),
    "set R8_PACKAGE_TESTS to install the test packages"
  )
  lib <- install_test_packages()
  out <- run_r(
    lib,
    c(
      "suppressPackageStartupMessages(library(r8pkgR))",
      "say <- function(...) cat(..., '\\n')",
      "clip_r <- function(x, lo, hi) pmin(pmax(x, lo), hi)",
      "level_r <- function(x, lo = 0, hi) { y <- numeric(length(x)); s <- 0; for (i in seq_along(x)) { s <- min(max(s + x[[i]], lo), hi); y[[i]] <- s }; y }",
      "set.seed(1)",
      "same <- vapply(c(0, 1, 10, 1000), function(n) { x <- 3 * stats::rnorm(n); identical(clip(x, -1, 1), clip_r(x, -1, 1)) && identical(level(x, hi = 2), level_r(x, hi = 2)) && identical(level(x, -1, 2), level_r(x, -1, 2)) }, NA)",
      "say('same', all(same))",
      # The check converts an integer vector, as for a body in R.
      "say('converted', identical(clip(1:3, 0L, 2), c(1, 2, 2)))",
      "e <- tryCatch(clip(1, 2, 1), error = identity)",
      "say('rule', class(e)[1], e$problems$code)",
      "e <- tryCatch(clip('a', 0, 1), error = identity)",
      "say('type', class(e)[1], e$problems$code)",
      "e <- tryCatch(clip(1, 0), error = conditionMessage)",
      "say('missing', grepl('\"hi\" is missing', e, fixed = TRUE))",
      # Each borrowed type, a default written in R, an R value, and `...`.
      "say('tally', tally(c(1L, 2L, 3L), flags = c(TRUE, FALSE, TRUE), extra = 1, more = 2))",
      "say('named', tally(1:2, label = 's', data = mtcars, na.rm = TRUE))",
      "say('utf8', tally(1L, label = '\\u00e9') == tally(1L, label = iconv('\\u00e9', 'UTF-8', 'latin1')))",
      "e <- tryCatch(tally(1L, flags = c(TRUE, NA)), error = identity)",
      "say('na', class(e)[1], e$problems$code)",
      # The errors of the body, and the check of its result.
      "say('fraction', fraction(1, 4))",
      "e <- tryCatch(fraction(1, 0), error = identity)",
      "say('error', paste(class(e), collapse = ','), e$code, deparse1(conditionCall(e)), conditionMessage(e))",
      "e <- tryCatch(suppressWarnings(fraction(-1, 2)), error = identity)",
      "say('panic', class(e)[1], e$code)",
      "e <- tryCatch(fraction(3, 2), error = identity)",
      "say('result', class(e)[1], e$problems$code)",
      # The code of a jump without a jump is an error, and R8 continues no jump.
      "e <- tryCatch(unwound(), error = conditionMessage)",
      "say('unwound', grepl('[unwind_without_jump]', e, fixed = TRUE))",
      "say('after', fraction(1, 4))",
      "say('print', any(grepl(\"body in Rust, package 'r8pkgR'\", utils::capture.output(print(clip)), fixed = TRUE)))",
      # Calls from documents reach the body.
      "say('document', identical(R8::r8call_from_json(level, '{\"$function\": \"pkgr::level\", \"arguments\": {\"x\": [1, 2, 3], \"hi\": 2.5}}'), c(1, 2.5, 2.5)))",
      "say('written', R8::r8call_to_json(level, c(1, 2), hi = 3))"
    )
  )
  info <- paste(out, collapse = "\n")
  for (line in c(
    "same TRUE ",
    "converted TRUE ",
    "rule r8_argument_error rule_violation ",
    "type r8_argument_error r_type ",
    "missing TRUE ",
    "tally n: sum 6, flags 2, bytes 3, na.rm false, k 3, 2 more, data NULL ",
    "named s: sum 3, flags none, bytes 3, na.rm true, k 2, 0 more, data an R value ",
    "utf8 TRUE ",
    "na r8_argument_error missing_value ",
    "fraction 0.25 ",
    "error r8_compiled_error,r8_error,error,condition zero_whole fraction(1, 0) `whole` is not 0 [zero_whole] ",
    "panic r8_panic panic ",
    "result r8_result_error above_max ",
    "unwound TRUE ",
    "after 0.25 ",
    "print TRUE ",
    "document TRUE ",
    "written {\"$function\":\"pkgr::level\",\"$types\":{},\"arguments\":{\"hi\":3,\"x\":[1,2]}} "
  )) {
    expect_true(line %in% out, info = paste(line, info, sep = "\n"))
  }
})

test_that("a body moved to Rust leaves the declaration, its schema, and its record as they were", {
  skip_if(
    !nzchar(Sys.getenv("R8_PACKAGE_TESTS")),
    "set R8_PACKAGE_TESTS to install the test packages"
  )
  lib <- install_test_packages()
  out <- run_r(
    lib,
    c(
      "suppressPackageStartupMessages({ library(R8); library(r8pkgR) })",
      "say <- function(...) cat(..., '\\n')",
      # The same declaration with a body in R, in another namespace.
      "op <- options(r8.namespace = 'other')",
      "level_r <- new_function('level', function(x = arg_vec(type_double(), description = 'Inflows, in order.'), lo = arg_double(default = 0, description = 'The floor.'), hi = arg_double(description = 'The capacity.')) x, rules = list(order = rule(lo <= hi)), return_type = vec(type_double()), description = 'Holds a running sum between `lo` and `hi`.')",
      "say('ir', identical(R8:::r8_function_ir('pkgr::level'), R8:::r8_function_ir('other::level')))",
      "schema <- function(f) { s <- jsonlite::fromJSON(r8schema(f), simplifyVector = FALSE); s$title <- NULL; s[['x-r8']]$`function` <- NULL; s }",
      "say('schema', identical(schema(level), schema(level_r)))",
      "say('formals', identical(formals(level), formals(level_r)))",
      # The Rust signature that r8rust_signature() writes is that of the
      # compiled body, which load accepted.
      sprintf(
        "src <- readLines(%s)",
        deparse(normalizePath(test_path(
          "packages",
          "r8pkgR",
          "src",
          "rust",
          "src",
          "functions.rs"
        )))
      ),
      "params <- function(text) { t <- gsub('[[:space:]]', '', paste(text, collapse = '')); n <- regmatches(t, regexpr('fn[a-z_]+\\\\([^{]*\\\\{', t)); sub(',\\\\)', ')', n) }",
      "source_fn <- function(name) { t <- gsub('[[:space:]]', '', paste(src, collapse = '')); m <- regmatches(t, regexpr(paste0('fn', name, '\\\\([^{]*\\\\{'), t)); sub(',\\\\)', ')', m) }",
      "for (f in list(clip = clip, level = level, tally = tally, fraction = fraction)) { name <- sub('^.*::', '', attr(f, 'r8_function')); g <- params(strsplit(r8rust_signature(f), '\\n')[[1]]); s <- source_fn(name); say('signature', name, identical(g, s) || identical(sub('->(.*)\\\\{$', '->r8::Result<\\\\1>{', g), s)) }"
    )
  )
  info <- paste(out, collapse = "\n")
  for (line in c(
    "ir TRUE ",
    "schema TRUE ",
    "formals TRUE ",
    "signature clip TRUE ",
    "signature level TRUE ",
    "signature tally TRUE ",
    "signature fraction TRUE "
  )) {
    expect_true(line %in% out, info = paste(line, info, sep = "\n"))
  }
})

test_that("load compares each Rust body with its declaration", {
  skip_if(
    !nzchar(Sys.getenv("R8_PACKAGE_TESTS")),
    "set R8_PACKAGE_TESTS to install the test packages"
  )
  lib <- install_test_packages()
  pkg <- normalizePath(test_path("packages", "r8pkgR"))
  out <- run_r(
    lib,
    c(
      sprintf("dyn.load(%s)", deparse(file.path(pkg, "src", "r8pkgR.so"))),
      "library(R8)",
      "say <- function(...) cat(..., '\\n')",
      "ptr <- .Call(R8:::.r8_native[['exports']], 'r8pkgR')",
      "op <- options(r8.namespace = 'pkgr')",
      sprintf("source(%s)", deparse(file.path(pkg, "R", "r8-types.R"))),
      # The declarations of R/functions.R with bodies in R, as a session
      # outside the package declares them.
      "rust_names <- c('pkgr::clip', 'pkgr::level', 'pkgr::tally', 'pkgr::fraction', 'pkgr::unwound')",
      "check <- function(in_rust = rust_names, in_r = character()) tryCatch({ R8:::check_functions('r8pkgR', R8:::r8_exports_functions(ptr, 'pkgr'), in_rust, in_r); 'agree' }, error = conditionMessage)",
      "clip <- new_function('clip', function(x = arg_vec(type_double()), lo = arg_double(), hi = arg_double()) x)",
      "level <- new_function('level', function(x = arg_vec(type_double()), lo = arg_double(default = 0), hi = arg_double()) x)",
      "tally <- new_function('tally', function(x = arg_vec(type_integer()), label = arg_character(default = 'n'), flags = arg_option(vec(type_logical())), bytes = arg_raw(default = as.raw(1:3)), na.rm = arg_logical(default = FALSE), data = NULL, k = arg_integer(default = length(x)), ...) x)",
      "fraction <- new_function('fraction', function(part = arg_double(), whole = arg_double()) part)",
      "unwound <- new_function('unwound', function() 1)",
      "say('first', check())",
      "e <- check(c(rust_names, 'pkgr::spread'))",
      "say('absent', grepl('`pkgr::spread()` has the body rust(), and the package\\'s Rust code exports no function `spread`', e, fixed = TRUE))",
      "say('unlisted', grepl('exports the function `tally`, which no new_function() of the package declares', check(setdiff(rust_names, 'pkgr::tally')), fixed = TRUE))",
      "say('in_r', grepl('`pkgr::clip()` has a body in R, and the package\\'s Rust code exports a body of it', check(setdiff(rust_names, 'pkgr::clip'), 'pkgr::clip'), fixed = TRUE))",
      "level <- new_function('level', function(x = arg_vec(type_integer()), lo = arg_double(default = 0), hi = arg_double()) x)",
      "clip <- new_function('clip', function(x = arg_vec(type_double()), low = arg_double(), hi = arg_double()) x)",
      "fraction <- new_function('fraction', function(part = arg_double(), whole = arg_double()) part, return_type = type_integer())",
      "e <- check()",
      "say(e)"
    )
  )
  info <- paste(out, collapse = "\n")
  for (line in c(
    "first agree ",
    "absent TRUE ",
    "unlisted TRUE ",
    "in_r TRUE ",
    "- `pkgr::level()` in Rust: its parameter `x` is vector of double, and in the declaration vector of integer. Its Rust signature, from r8rust_signature():",
    "- `pkgr::clip()` in Rust: it takes the parameters (x, lo, hi), and the declaration (x, low, hi). Its Rust signature, from r8rust_signature():",
    "- `pkgr::fraction()` in Rust: it returns double, and the declaration integer. Its Rust signature, from r8rust_signature():",
    "        x: &[i32],",
    "    ) -> i32 {"
  )) {
    expect_true(line %in% out, info = paste(line, info, sep = "\n"))
  }
  expect_true(
    any(grepl("[function_signature]", out, fixed = TRUE)),
    info = info
  )
})

test_that("a function whose body is in Rust needs its package's registration", {
  skip_if(
    !nzchar(Sys.getenv("R8_PACKAGE_TESTS")),
    "set R8_PACKAGE_TESTS to install the test packages"
  )
  lib <- install_test_packages()
  out <- run_r(
    lib,
    c(
      "suppressPackageStartupMessages(library(r8pkgR))",
      "say <- function(...) cat(..., '\\n')",
      "f <- r8pkgR::clip",
      "say('before', identical(f(c(-2, 2), -1, 1), c(-1, 1)))",
      "unloadNamespace('r8pkgR')",
      "invisible(gc())",
      "e <- tryCatch(f(c(-2, 2), -1, 1), error = identity)",
      "say('after', class(e)[1], grepl(\"package 'r8pkgR' has not registered it\", conditionMessage(e), fixed = TRUE))",
      "suppressPackageStartupMessages(library(r8pkgR))",
      "say('again', identical(f(c(-2, 2), -1, 1), c(-1, 1)))"
    )
  )
  info <- paste(out, collapse = "\n")
  for (line in c(
    "before TRUE ",
    "after r8_dispatch_error TRUE ",
    "again TRUE "
  )) {
    expect_true(line %in% out, info = paste(line, info, sep = "\n"))
  }
})
