# test-702-interface.R
# ::R8::
# 2026- EDG rtemis.org

# The interface between R8 and the compiled code of a package.
#
# Every package in Rust meets it through the crate rtemis-r8-api. These
# tests give R8 tables that the crate does not write, and check that each is
# refused with a message and that nothing it registered before changes.

scenario(
  "F1",
  "compiled code that is not the crate's is refused",
  situation = "A package gives R8 a table of compiled code that is malformed, or that another version of the interface wrote.",
  risk = "R crashes, or calls another function than the table names.",
  result = "Each table is refused with a message, and what R8 registered before stays as it was.",
  how = "installed",
  code = {
    lib <- install_test_packages()
    refused <- c(
      "two impl functions `magnitude`" = 1,
      "`magnitude` of `pkgr::Norm` for `pkgr::Signal` is NULL" = 2,
      "defines `ratio`, which has no default" = 3,
      "names no type that it is for" = 4,
      "of the kind 9" = 5,
      "the methods of a class of R are written in R" = 6,
      "holds no method `norm`" = 7,
      "the impl entry of `magnitude` states no signature" = 8,
      "reinstall the package against the installed R8" = 9,
      "the impl entries have size" = 10,
      "has no declaration record" = 11,
      "names no module" = 12,
      "names another module than the exports table" = 13,
      "sets no `from_reader`" = 14,
      "does not match its type" = 15
    )
    out <- run_r(
      lib,
      c(
        "suppressPackageStartupMessages(library(r8pkgR))",
        "register <- function(k) R8:::r8_register_exports(r8pkgAbi:::table(k), 'r8pkgR', 'pkgr')",
        "says <- function(k) tryCatch({ register(k); 'none' }, error = function(e) conditionMessage(e))",
        sprintf("cat('%d', says(%d), '\\n')", refused, refused),
        "e <- tryCatch(R8:::r8_exports_compare(r8pkgAbi:::table(16), 'pkgr'), error = function(e) conditionMessage(e))",
        "cat('undeclared', e, '\\n')",
        "# A refused table leaves the registered entries as they were.",
        "s <- Signal(name = 'a', values = c(3, 4))",
        "cat('after', magnitude(s), '\\n')",
        "# An R error raised under R8Api.protect reaches the caller after the impl",
        "# function returns.",
        "invisible(register(17))",
        "f <- R8:::r8_impl_fn('pkgr::Norm', 'pkgr::Signal', 'magnitude')",
        "cat('unwind', tryCatch(.Call(R8:::.r8_c_invoke, f, list(s)), error = function(e) conditionMessage(e)), '\\n')",
        "cat('status', suppressMessages(r8pkgAbi:::protect_status()), '\\n')",
        "invisible(R8::r8_register('r8pkgR'))",
        "cat('again', magnitude(s), '\\n')",
        "# A closure kept after its package unloads refuses the call.",
        "keys <- grep('|magnitude|', ls(R8:::.r8_impls), fixed = TRUE, value = TRUE)",
        "f <- get(keys[[1L]], envir = R8:::.r8_impls)",
        "unloadNamespace('r8pkgR')",
        "e <- tryCatch(f(s), error = function(e) conditionMessage(e))",
        "cat('unloaded', grepl('unloaded', e), '\\n')"
      )
    )
    info <- paste(out, collapse = "\n")
    for (text in names(refused)) {
      line <- grep(paste0("^", refused[[text]], " "), out, value = TRUE)
      expect_true(
        any(grepl(text, line, fixed = TRUE)),
        info = paste(text, info)
      )
    }
    expect_true(
      any(grepl("`pkgr::None` has no declaration", out, fixed = TRUE)),
      info = info
    )
    expect_true("after 5 " %in% out, info = info)
    expect_true("unwind `by` is NA " %in% out, info = info)
    expect_true("status 2 " %in% out, info = info)
    expect_true("again 5 " %in% out, info = info)
    expect_true("unloaded TRUE " %in% out, info = info)
  }
)

test_that("R8Api builds a value from fields and replaces the fields of one", {
  skip_if(
    !nzchar(Sys.getenv("R8_PACKAGE_TESTS")),
    "set R8_PACKAGE_TESTS to install the test packages"
  )
  skip_without_oracle()
  lib <- install_test_packages()
  out <- run_r(
    lib,
    c(
      "suppressPackageStartupMessages({ library(R8); loadNamespace('r8pkgR') })",
      "say <- function(...) cat(..., '\\n')",
      "of <- r8pkgAbi:::of_fields; with <- r8pkgAbi:::with_fields",
      "hash <- function(q) .Call('wrap__r8_declaration_hash', q, PACKAGE = 'R8')",
      "x <- of('pkgr::Interval', list(1, 2, NULL), hash('pkgr::Interval'))",
      "say('struct', class(x)[1], x$lo, x$hi, r8pkgR::width(x), r8_equal(x, r8pkgR::Interval(lo = 1, hi = 2)))",
      "k <- of('pkgr::Resampler', list(3L, 1L), hash('pkgr::Resampler'), 'KFold')",
      "say('enum', r8_variant(k), k$k, format(k))",
      "s <- of('pkgr::Split', list(1:2, 3L), hash('pkgr::Split'))",
      "say('r_stored', class(s)[1], s$test, is.null(R8:::r8_rust_stored_parts(s)))",
      "h <- hash('pkgr::Interval')",
      "say('codes', of('pkgr::Interval', list(1, 2, NULL), 'sha256:0'), of('pkgr::None', list(), h), of('None', list(), h), of('pkgr::Interval', list(1, 2), h), of('pkgr::Resampler', list(), hash('pkgr::Resampler'), 'None'))",
      "y <- with(x, list(hi = 9, unit = 'cm'))",
      "say('replaced', class(y)[1], y$lo, y$hi, y$unit, x$hi)",
      "say('rule', with(x, list(hi = 0)))",
      "say('type', with(x, list(hi = 'a')))",
      "say('field', with(x, list(top = 1)))",
      "say('list', with(x, list(1))[1], with(x, 1)[1])"
    )
  )
  info <- paste(out, collapse = "\n")
  expect_true("struct pkgr::Interval 1 2 1 TRUE " %in% out, info = info)
  expect_true("enum KFold 3 KFold (k = 3, seed = 1) " %in% out, info = info)
  expect_true("r_stored pkgr::Split 3 TRUE " %in% out, info = info)
  expect_true(
    "codes declaration_changed unknown_type unknown_type invalid_argument unknown_variant " %in%
      out,
    info = info
  )
  expect_true("replaced pkgr::Interval 1 9 cm 2 " %in% out, info = info)
  expect_true(
    "rule invalid_value the rule `ordered` is violated " %in% out,
    info = info
  )
  expect_true(any(startsWith(out, "type invalid_value `/hi`")), info = info)
  expect_true(any(startsWith(out, "field unknown_field")), info = info)
  expect_true("list invalid_argument invalid_argument " %in% out, info = info)
})
