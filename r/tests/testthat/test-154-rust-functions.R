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
      "for (f in list(clip = clip, level = level, tally = tally, fraction = fraction, spread = spread, portion = portion)) { name <- sub('^.*::', '', attr(f, 'r8_function')); g <- params(strsplit(r8rust_signature(f), '\\n')[[1]]); s <- source_fn(name); say('signature', name, identical(g, s) || identical(sub('->(.*)\\\\{$', '->r8::Result<\\\\1>{', g), s)) }"
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
    "signature fraction TRUE ",
    "signature spread TRUE ",
    "signature portion TRUE "
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
      "rust_names <- c('pkgr::clip', 'pkgr::level', 'pkgr::tally', 'pkgr::fraction', 'pkgr::unwound', 'pkgr::spread', 'pkgr::portion', 'pkgr::counted', 'pkgr::count_of', 'pkgr::filled')",
      "check <- function(in_rust = rust_names, in_r = character()) tryCatch({ R8:::check_functions('r8pkgR', R8:::r8_exports_functions(ptr, 'pkgr'), in_rust, in_r); 'agree' }, error = conditionMessage)",
      "clip <- new_function('clip', function(x = arg_vec(type_double()), lo = arg_double(), hi = arg_double()) x)",
      "level <- new_function('level', function(x = arg_vec(type_double()), lo = arg_double(default = 0), hi = arg_double()) x)",
      "tally <- new_function('tally', function(x = arg_vec(type_integer()), label = arg_character(default = 'n'), flags = arg_option(vec(type_logical())), bytes = arg_raw(default = as.raw(1:3)), na.rm = arg_logical(default = FALSE), data = NULL, k = arg_integer(default = length(x)), ...) x)",
      "fraction <- new_function('fraction', function(part = arg_double(), whole = arg_double()) part)",
      "unwound <- new_function('unwound', function() 1)",
      "spread <- new_function('spread', function(x = arg_vec(type_double())) 0, return_type = type_double())",
      "portion <- new_function('portion', function(x = arg_vec(type_double()), whole = arg_vec(type_double()), scale = arg_double(default = 1)) 0, return_type = type_double(min = 0))",
      "counted <- new_function('counted', function(x = arg_vec(type_double())) 0, return_type = type_double())",
      "count_of <- new_function('count_of', function() 0L, return_type = type_integer())",
      "filled <- new_function('filled', function(x = arg_vec(type_double()), n = arg_integer()) 0, return_type = type_double())",
      "say('first', check())",
      "e <- check(c(rust_names, 'pkgr::midrange'))",
      "say('absent', grepl('`pkgr::midrange()` has the body rust(), and the package\\'s Rust code exports no function `midrange`', e, fixed = TRUE))",
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

test_that("an expression of calc() calls a body in Rust through its entry, as a direct call", {
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
      "set.seed(1)",
      "n <- 1000L",
      "x <- Raven(g = sample(c('a', 'b', 'c'), n, TRUE), k = sample(1:5, n, TRUE), v = stats::rnorm(n), w = stats::runif(n))",
      "rows <- split(seq_len(n), x$g)",
      "direct <- function(f) unname(vapply(rows, f, 0))",
      # Each group's value is that of a direct call: cells of an integer
      # column converted, a constant, a default, and an expression of cells.
      "s <- r8summarize(x, list(s = calc(spread(v)), i = calc(spread(k)), p = calc(portion(w, whole = v + 10, scale = 2)), q = calc(portion(w, whole = w))), by = 'g')",
      "say('groups', identical(s$g, c('a', 'b', 'c')))",
      "say('spread', identical(s$s, direct(function(r) spread(x$v[r]))))",
      "say('integer', identical(s$i, direct(function(r) spread(x$k[r]))))",
      "say('portion', identical(s$p, direct(function(r) portion(x$w[r], whole = x$v[r] + 10, scale = 2))))",
      "say('default', identical(s$q, rep(1, 3)))",
      "say('qualified', identical(r8summarize(x, list(s = calc(r8pkgR::spread(v))), by = 'g')$s, s$s))",
      # Over all rows, over each row's group, and over each column.
      "y <- r8set(x, list(d = calc(v - spread(v))))",
      "say('rows', identical(y$d, x$v - spread(x$v)))",
      "y <- r8set(x, list(d = calc(spread(v))), by = 'g')",
      "say('by', identical(y$d, s$s[match(x$g, s$g)]))",
      "m <- r8map(x, calc(spread(col())), columns = c('v', 'w'), by = 'g')",
      "say('map', identical(m, r8map(x, spread, columns = c('v', 'w'), by = 'g')))",
      # The entry computes the value; the function's closure is not called.
      "probe <- spread",
      "body(probe) <- quote(stop('the closure ran'))",
      "attributes(probe) <- attributes(spread)",
      "say('entry', identical(r8summarize(x, list(s = calc(probe(v))), by = 'g')$s, s$s))",
      # A call that the checks or the body refuse raises the error of a
      # direct call, at its group.
      "parent <- function(e) paste(class(e)[1], class(e$parent)[1], if (is.null(e$parent$problems)) e$parent$code else e$parent$problems$code)",
      "e <- tryCatch(r8summarize(x, list(p = calc(portion(w, whole = v - v))), by = 'g'), error = identity)",
      "say('body', parent(e), grepl('`p` calls `pkgr::portion()` in the group g = \"a\": `whole` sums to 0 [zero_whole]', conditionMessage(e), fixed = TRUE))",
      "e <- tryCatch(r8summarize(x, list(p = calc(portion(w, whole = w, scale = -1))), by = 'g'), error = identity)",
      "say('rule', parent(e))",
      "e <- tryCatch(r8summarize(x, list(p = calc(portion(v - 10, whole = w))), by = 'g'), error = identity)",
      "say('result', parent(e))",
      "z <- Raven(g = c('a', 'a', 'b'), v = c(1, NA, 2))",
      "e <- tryCatch(r8summarize(z, list(s = calc(spread(v))), by = 'g'), error = identity)",
      "say('missing', parent(e), grepl('in the group g = \"a\"', conditionMessage(e), fixed = TRUE))",
      # The body runs at most once: a group whose body returns an error stops
      # the calls, and the error is that of a direct call.
      "x2 <- Raven(g = c('a', 'b', 'b', 'c'), v = c(1, 2, -1, 3))",
      "before <- count_of()",
      "e <- tryCatch(r8summarize(x2, list(n = calc(counted(v))), by = 'g'), error = identity)",
      "say('once', count_of() - before, parent(e), grepl('in the group g = \"b\": `x` holds a negative value [negative]', conditionMessage(e), fixed = TRUE))",
      "direct <- tryCatch(counted(c(2, -1)), error = identity)",
      "say('direct', identical(class(e$parent), class(direct)), identical(conditionMessage(e$parent), conditionMessage(direct)))",
      "e <- tryCatch(suppressWarnings(r8summarize(x2, list(f = calc(fraction(-1, 2))), by = 'g')), error = identity)",
      "say('panic', class(e$parent)[1])",
      "e <- tryCatch(r8summarize(x2, list(u = calc(unwound())), by = 'g'), error = identity)",
      "say('unwound', class(e)[1], grepl('[unwind_without_jump]', conditionMessage(e$parent), fixed = TRUE))",
      # An R error inside the body's R8Api.protect continues as itself, inside
      # the error that names the column and the group.
      "heap <- gc()[2L, 4L]",
      "invisible(mem.maxVSize(ceiling(heap) + 20))",
      "big <- as.integer(ceiling((heap + 100) * 2^20 / 8))",
      "e <- tryCatch(r8summarize(x2, list(m = calc(filled(v, n = val(big)))), by = 'g'), error = identity)",
      "say('jump', class(e)[1], class(e$parent)[1], grepl('`m` calls `pkgr::filled()` in the group g = \"a\": vector memory limit', conditionMessage(e), fixed = TRUE))",
      "invisible(mem.maxVSize(Inf))",
      "say('after', identical(r8summarize(x2, list(m = calc(filled(v, n = 10L))), by = 'g')$m, c(1, 2, 1)))",
      # Without its package's registration, the call is the closure's.
      "f <- r8pkgR::spread",
      "unloadNamespace('r8pkgR')",
      "invisible(gc())",
      "e <- tryCatch(r8summarize(x, list(s = calc(f(v))), by = 'g'), error = identity)",
      "say('unloaded', class(e)[1], class(e$parent)[1])"
    )
  )
  info <- paste(out, collapse = "\n")
  for (line in c(
    "groups TRUE ",
    "spread TRUE ",
    "integer TRUE ",
    "portion TRUE ",
    "default TRUE ",
    "qualified TRUE ",
    "rows TRUE ",
    "by TRUE ",
    "map TRUE ",
    "entry TRUE ",
    "body r8_value_error r8_compiled_error zero_whole TRUE ",
    "rule r8_value_error r8_argument_error rule_violation ",
    "result r8_value_error r8_result_error below_min ",
    "missing r8_value_error r8_argument_error missing_value TRUE ",
    "once 2 r8_value_error r8_compiled_error negative TRUE ",
    "direct TRUE TRUE ",
    "panic r8_panic ",
    "unwound r8_value_error TRUE ",
    "jump r8_value_error simpleError TRUE ",
    "after TRUE ",
    "unloaded r8_value_error r8_dispatch_error "
  )) {
    expect_true(line %in% out, info = paste(line, info, sep = "\n"))
  }
})

test_that("r8explain() states where a body in Rust runs, and calls none", {
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
      "x <- Raven(g = c('a', 'a', 'b'), v = c(1, 2, 4), w = c(0.5, 1, 2))",
      "before <- count_of()",
      "p <- r8explain(r8summarize(x, list(n = calc(counted(v))), by = 'g'))",
      "s <- as.data.frame(p)",
      "say('none', count_of() - before)",
      "say('rust', p$engine, s$engine[s$stage == 'call'], s$language[s$stage == 'call'])",
      "fn <- p$nodes[[1]]$meta$functions[[1]]",
      "say('package', fn$name, fn$package, identical(fn$version, as.character(packageVersion('r8pkgR'))))",
      "p <- r8explain(r8map(x, spread, columns = c('v', 'w'), by = 'g'))",
      "say('map', p$headline)",
      "op <- options(r8.namespace = 'mixed')",
      "half <- new_function('half', function(x = arg_vec(type_double())) sum(x) / 2, return_type = type_double())",
      "options(op)",
      "p <- r8explain(r8summarize(x, list(s = calc(spread(v)), h = calc(half(v))), by = 'g'))",
      "say('mixed', p$engine, is.null(p$nodes[[1]]$meta$functions[[2]]$package))",
      "p <- r8explain(r8map(list(c(1, 2), c(3, 5)), spread))",
      "say('list', p$engine, p$nodes[[1]]$meta$path)"
    )
  )
  info <- paste(out, collapse = "\n")
  for (line in c(
    "none 0 ",
    "rust rust rust rust ",
    "package pkgr::counted r8pkgR TRUE ",
    "map r8map(): pkgr::spread over 2 columns and 2 groups, 4 calls, in Rust ",
    "mixed rust+r TRUE ",
    "list rust+r host "
  )) {
    expect_true(line %in% out, info = paste(line, info, sep = "\n"))
  }
})
