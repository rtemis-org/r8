# test-152-rust-impls.R
# ::R8::
# 2026- EDG rtemis.org

# Methods written in Rust, called from R.

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
      "e <- tryCatch(ratio(s, 0), error = identity)",
      "cat('error', inherits(e, 'r8_compiled_error') && inherits(e, 'r8_error'), e$code, deparse1(conditionCall(e)), '\\n')",
      "e <- tryCatch(sapply(list(s), ratio, by = 0), error = identity)",
      "cat('call', deparse1(conditionCall(e)), '\\n')",
      "cat('display', format(s), '\\n')",
      "a <- Signal(name = 'a', values = c(0, 0)); b <- Signal(name = 'b', values = c(3, 4))",
      "rn <- r_stored(b)",
      "l <- as.list(s)",
      "cat('as_list', identical(l, list(name = 'a', values = x)), grepl('VECSXP', inspect(l)), !grepl('r8 rust-stored', inspect(l)), identical(s['values'], list(values = x)), lengths(lapply(s, as.vector)), '\\n')",
      "e <- tryCatch(s['value'], r8_value_error = conditionMessage)",
      "cat('unknown', grepl('Fields: name, values', e), '\\n')",
      "e <- tryCatch(distance(a, 1), r8_argument_error = conditionMessage)",
      "cat('distance', distance(a, b), distance(a, rn), grepl('`y` must be a value of `pkgr::Signal`', e, fixed = TRUE), '\\n')",
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
  # The error of a compiled method reports the generic's call.
  # An error of a compiled method is classed, with its code.
  expect_true("error TRUE zero_divisor ratio(s, 0) " %in% out, info = info)
  expect_true("call FUN(X[[i]], ...) " %in% out, info = info)
  expect_true("display Signal a (2 values) " %in% out, info = info)
  # The fields of a Rust-stored value, as an ordinary list.
  expect_true("as_list TRUE TRUE TRUE TRUE 1 2 " %in% out, info = info)
  expect_true("unknown TRUE " %in% out, info = info)
  # A `&Signal` parameter takes a Rust-stored value or an R-stored one; the
  # trait types it, so R8 refuses another object before the method runs.
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

test_that("Rust-stored structs extend one another, and compiled impls apply to descendants", {
  skip_if(
    !nzchar(Sys.getenv("R8_PACKAGE_TESTS")),
    "set R8_PACKAGE_TESTS to install the test packages"
  )
  skip_if_not_installed("mirai")
  lib <- install_test_packages()
  rds <- tempfile(fileext = ".rds")
  out <- run_r(
    lib,
    c(
      "suppressPackageStartupMessages({ library(R8); library(r8pkgR) })",
      "say <- function(...) cat(..., '\\n')",
      "msg <- function(x) tryCatch(x, error = function(e) conditionMessage(e))",
      "inspect <- function(x) utils::capture.output(.Internal(inspect(x, 0)))[1]",
      "stored <- function(x, t) grepl(paste0('r8 rust-stored pkgr::', t), inspect(x))",
      "s <- Strip(hi = 4, label = 's', weights = c(1, 2, 3))",
      "b <- Band(label = 'b', weights = c(1, 2))",
      "say('class', class(s))",
      "say('inherits', inherits(s, Strip), inherits(s, Interval), inherits(b, Strip), inherits(b, Interval), inherits(s, Signal))",
      "say('stored', stored(s, 'Strip'), stored(b, 'Band'))",
      "say('fields', s$lo, s$hi, is.null(s$unit), s$label, s$weights, s$number)",
      "say('defaults', b$lo, b$hi, b$unit)",
      "e <- msg(Band(lo = 2, label = '', weights = 1))",
      "say('rules', grepl('Rule ordered', e), grepl('Rule labeled', e))",
      "b2 <- b; b2$lo <- 0.25; b2$label <- 'c'",
      "say('write', b2$lo, b2$label, b2$weights, b$lo, b$label)",
      "say('rule_on_write', grepl('Rule ordered', msg(b2$hi <- -1)))",
      "weights <- b$weights; b3 <- b; b3[c('lo', 'hi', 'label')] <- list(5, 9, 'd')",
      "say('several', stored(b3, 'Band'), b3$lo, b3$hi, b3$label, identical(b3$weights, weights), b$lo, b$hi, b$label)",
      "e <- msg(b3[c('lo', 'hi')] <- list(9, 1))",
      "say('several_rule', grepl('Rule ordered', e), grepl('from `x[c(\"lo\", \"hi\")] <- value`', e, fixed = TRUE), b3$lo)",
      "say('width', width(s), width(b), width(Interval(lo = 1, hi = 3)))",
      "say('again', width(s), width(b))",
      "say('total', total(b), total(s))",
      "say('not_up', grepl('does not implement `pkgr::total`', msg(total(Interval(lo = 1)))))",
      "x <- middle(s)",
      "say('middle', class(x)[1], x$lo, x$hi)",
      "x <- shifted(s, 1); y <- shifted(b, 1); z <- shifted(Interval(lo = 1, hi = 3), 1)",
      "say('shifted', class(x)[1], x$lo, x$hi, x$label, x$weights, x$number, class(y)[1], y$lo, y$label, class(z)[1], z$lo)",
      "say('stored', stored(x, 'Strip'), stored(y, 'Band'), stored(z, 'Interval'), grepl('[not_finite]', msg(shifted(s, Inf)), fixed = TRUE))",
      "say('made', grepl('must be finite', msg(shifted(Interval(lo = 1e308, hi = 1.5e308), 1.5e308)), fixed = TRUE))",
      "no <- function(y) grepl('Invalid arguments of `overlap()`: 1 problem\\n- `y` must be a value', msg(overlap(s, y)), fixed = TRUE)",
      "say('argument', overlap(s, Interval(lo = 1, hi = 9)), no(s), no(1), no(NULL))",
      "say('format', format(s))",
      "say('other', grepl('`x` is not a `pkgr::Interval`', msg(r8_method(width, Interval)(Signal(name = 'a', values = 1))), fixed = TRUE))",
      "u <- r8_upcast(s, Interval); v <- r8_upcast(s, Band)",
      "say('upcast', class(u)[1], stored(u, 'Interval'), u$hi, class(v)[1], stored(v, 'Band'), v$weights, v$label)",
      "say('convert', class(r8_convert(s, Interval))[1], grepl('not an ancestor', msg(r8_upcast(u, Band))))",
      "say('equal', r8_equal(v, Band(lo = 0, hi = 4, unit = NULL, label = 's', weights = c(1, 2, 3))))",
      "j <- r8_from_json(Strip, r8_to_json(s))",
      "say('json', stored(j, 'Strip'), r8_equal(j, s), shares(s), shares(j))",
      "w <- unserialize(serialize(s, NULL))",
      "say('serialize', stored(w, 'Strip'), r8_equal(w, s), width(w))",
      "w <- r_stored(s)",
      "say('r_stored', stored(w, 'Strip'), width(w), total(w), stored(r8_upcast(w, Band), 'Band'))",
      sprintf("saveRDS(s, %s)", deparse(rds)),
      "mirai::daemons(1)",
      "m <- mirai::mirai({ library(r8pkgR); list(width(s), total(s), class(R8::r8_upcast(s, Band))[1]) }, s = s)[]",
      "say('mirai', unlist(m))",
      "invisible(mirai::daemons(0))"
    )
  )
  info <- paste(out, collapse = "\n")
  expect_true(
    "class pkgr::Strip pkgr::Band pkgr::Interval r8_value " %in% out,
    info = info
  )
  expect_true("stored TRUE TRUE " %in% out, info = info)
  expect_true("inherits TRUE TRUE FALSE TRUE FALSE " %in% out, info = info)
  expect_true("fields 0 4 TRUE s 1 2 3 1 " %in% out, info = info)
  expect_true("defaults 0 1 cm " %in% out, info = info)
  expect_true("rules TRUE TRUE " %in% out, info = info)
  expect_true("write 0.25 c 1 2 0 b " %in% out, info = info)
  expect_true("rule_on_write TRUE " %in% out, info = info)
  expect_true("several TRUE 5 9 d TRUE 0 1 b " %in% out, info = info)
  expect_true("several_rule TRUE TRUE 5 " %in% out, info = info)
  # The impl for Interval applies to its descendants, at a first and a later call.
  expect_true("width 4 1 2 " %in% out, info = info)
  expect_true("again 4 1 " %in% out, info = info)
  # The R impl for Strip continues to the compiled impl for Band.
  expect_true("total 3 7 " %in% out, info = info)
  expect_true("not_up TRUE " %in% out, info = info)
  # An impl for the parent returns the parent; an impl for each type, written
  # once over the parent's interface trait, returns the type; a `&Interval`
  # parameter takes an Interval exactly.
  expect_true("middle pkgr::Interval 1 3 " %in% out, info = info)
  expect_true(
    "shifted pkgr::Strip 1 5 s 1 2 3 1 pkgr::Band 1 b pkgr::Interval 2 " %in%
      out,
    info = info
  )
  expect_true("stored TRUE TRUE TRUE TRUE " %in% out, info = info)
  # A value that Rust code makes is validated by its type: here a sum that
  # overflows to Inf, which a double field does not admit.
  expect_true("made TRUE " %in% out, info = info)
  expect_true("argument 3 TRUE TRUE TRUE " %in% out, info = info)
  expect_true("format [0, 4] " %in% out, info = info)
  expect_true("other TRUE " %in% out, info = info)
  expect_true(
    "upcast pkgr::Interval TRUE 4 pkgr::Band TRUE 1 2 3 s " %in% out,
    info = info
  )
  expect_true("convert pkgr::Interval TRUE " %in% out, info = info)
  expect_true("equal TRUE " %in% out, info = info)
  # A clone of a Buffer, borrowed from R or owned by Rust, shares its memory.
  expect_true("json TRUE TRUE TRUE TRUE " %in% out, info = info)
  expect_true("serialize TRUE TRUE 4 " %in% out, info = info)
  # A value read back as R-stored takes the same impls.
  expect_true("r_stored FALSE 4 7 TRUE " %in% out, info = info)
  expect_true("mirai 4 7 pkgr::Band " %in% out, info = info)
  # A state is lent to the compiled code that built it, and no other.
  out <- run_r(
    lib,
    c(
      "suppressPackageStartupMessages({ library(R8); library(r8pkgR) })",
      "say <- function(...) cat(..., '\\n')",
      "lend <- r8pkgAbi:::lend; wrap <- r8pkgAbi:::wrap",
      "c1 <- Signal(name = 'a', values = 1)",
      sprintf("s <- readRDS(%s)", deparse(rds)),
      "say('exact', lend(s, 'pkgr::Strip'), lend(s, 'pkgr::Band'), lend(c1, 'pkgr::Strip'), lend(1, 'pkgr::Strip'))",
      "say('upcast', lend(s, 'pkgr::Band', TRUE), lend(s, 'pkgr::Interval', TRUE), lend(s, 'pkgr::Signal', TRUE))",
      "say('r_stored', lend(r_stored(c1), 'pkgr::Signal'))",
      "say('pin', r8pkgAbi:::pin(raw(4)))",
      "say('wrap', wrap('pkgr::Signal'), wrap('pkgr::None'))"
    )
  )
  info <- paste(out, collapse = "\n")
  expect_true(
    "exact not_pinned type_mismatch type_mismatch type_mismatch " %in% out,
    info = info
  )
  expect_true(
    "upcast not_pinned not_pinned type_mismatch " %in% out,
    info = info
  )
  expect_true("r_stored not_pinned " %in% out, info = info)
  expect_true("wrap other_module unknown_type " %in% out, info = info)
  expect_true("pin bad_record " %in% out, info = info)
  # A child whose R declaration declares the same fields without `extends`
  # has the same declaration hash, and is reported.
  pkg <- test_path("packages", "r8pkgR")
  copy <- tempfile("r8pkgR")
  dir.create(file.path(copy, "R"), recursive = TRUE)
  file.copy(file.path(pkg, "R", "r8-types.R"), file.path(copy, "R"))
  out <- run_r(
    lib,
    c(
      sprintf(
        "dyn.load(%s)",
        deparse(normalizePath(file.path(pkg, "src", "r8pkgR.so")))
      ),
      "library(R8)",
      "ptr <- .Call(R8:::.r8_native[['exports']], 'r8pkgR')",
      "op <- options(r8.namespace = 'pkgr')",
      sprintf("source('%s')", file.path(copy, "R", "r8-types.R")),
      "flat <- R8:::r8_type_ir('pkgr::Strip')",
      "st <- function() { x <- R8:::r8_exports_compare(ptr, 'pkgr'); x$status[x$type_name == 'pkgr::Strip'] }",
      "cat('before', st(), '\\n')",
      "Strip <- new_struct('Strip', fields = list(",
      "  lo = field(type_double(), default = 0, description = 'The lower end.'),",
      "  hi = field(type_double(), default = 1, description = 'The upper end.'),",
      "  unit = field(option(type_character()), description = 'The unit of the ends.'),",
      "  label = field(type_character(), description = 'The label.'),",
      "  weights = field(vec(type_double()), description = 'The weights.'),",
      "  number = field(type_integer(min = 1L), default = 1L, description = 'The number.')),",
      "  description = 'A numbered band without a default unit.',",
      "  rules = list(rule(lo <= hi, id = 'ordered'), rule(label != '', id = 'labeled')), storage = 'rust')",
      "cat('flat', st(), '\\n')"
    )
  )
  info <- paste(out, collapse = "\n")
  expect_true("before same " %in% out, info = info)
  expect_true("flat differs " %in% out, info = info)
})

test_that("compiled impls dispatch on several arguments, replace, and convert", {
  skip_if(
    !nzchar(Sys.getenv("R8_PACKAGE_TESTS")),
    "set R8_PACKAGE_TESTS to install the test packages"
  )
  skip_if_not_installed("mirai")
  lib <- install_test_packages()
  out <- run_r(
    lib,
    c(
      "suppressPackageStartupMessages({ library(R8); library(r8pkgR); library(r8pkgS) })",
      "say <- function(...) cat(..., '\\n')",
      "cls <- function(x) tryCatch({ x; 'none' }, error = function(e) class(e)[1])",
      "msg <- function(x) tryCatch(x, error = function(e) conditionMessage(e))",
      "i <- Interval(lo = 1, hi = 2); b <- Band(label = 'b', weights = c(1, 5))",
      "s <- Strip(hi = 4, label = 's', weights = 3)",
      "g <- Signal(name = 'g', values = c(4, 2, 8)); t <- Tile(lo = 1, hi = 3, number = 7L)",
      # The nearest impl in each position, each value lent as its type.
      "say('pairs', meets(i, i, 1), '|', meets(i, b, 1), '|', meets(b, i, 1), '|', meets(s, s, 1))",
      "say('again', meets(i, i, 1), '|', meets(i, b, 1), '|', meets(b, i, 1), '|', meets(s, s, 1))",
      # Any value, an R one among them.
      "say('any', meets(g, 1, 2), '|', meets(g, NULL, 2), '|', meets(g, g, 2))",
      "say('none', cls(meets(i, 1, 2)), cls(meets(1, i, 2)))",
      # Another package's types, read by their fields, and its own.
      "say('other', meets(t, i, 1), '|', meets(t, s, 1), '|', meets(t, t, 1), '|', meets(i, t, 1))",
      "w <- r_stored(b)",
      "say('r_stored', meets(i, w, 1), '|', meets(w, i, 1))",
      "say('method', is.function(r8_method(meets, Interval, Band)), r8_method(meets, Interval, Interval)(i, i, 3))",
      "say('ambiguities', nrow(r8_ambiguities(meets)))",
      # A replacement method of a family keeps the type; one of a parent does
      # not serve a child.
      "label(b) <- 'new'; label(s) <- 'strip'",
      "say('replace', class(b)[1], b$label, b$weights, class(s)[1], s$label, s$number)",
      "upper(i) <- 9",
      "say('upper', i$hi, cls(upper(b) <- 9), grepl('[below_lo]', msg(upper(i) <- 0), fixed = TRUE))",
      "say('argument', cls(label(b) <- 1))",
      # A conversion, of a descendant too, and its error.
      "x <- r8_convert(b, Signal)",
      "say('convert', format(r8_convert(g, Interval)), format(Interval@from(g)), class(x)[1], x$name, x$values)",
      "say('child', format(r8_convert(s, Signal)), cls(r8_convert(i, Signal)))",
      "say('empty', grepl('[no_values]', msg(r8_convert(Signal(name = 'e', values = numeric()), Interval)), fixed = TRUE))",
      "m <- mirai::mirai({ library(r8pkgS); list(r8pkgR::meets(t, i, 1), r8pkgR::meets(i, t, 1), format(R8::r8_convert(g, r8pkgR::Interval))) }, t = t, i = r8pkgR::Interval(lo = 1, hi = 2), g = g)[]",
      "say('mirai', unlist(m))"
    )
  )
  info <- paste(out, collapse = "\n")
  pairs <- paste(
    "interval 2 interval 1 1 | interval 2 band b 1 |",
    "interval 1 interval 1 1 | interval 4 band s 1 "
  )
  expect_true(paste("pairs", pairs) %in% out, info = info)
  expect_true(paste("again", pairs) %in% out, info = info)
  expect_true(
    "any signal g any false 2 | signal g any false 2 | signal g any false 2 " %in%
      out,
    info = info
  )
  expect_true(
    "none r8_dispatch_error r8_dispatch_error " %in% out,
    info = info
  )
  expect_true(
    "other tile 7 interval 2 | tile 7 band 1 | tile 7 tile 7 | interval 2 interval 1 1 " %in%
      out,
    info = info
  )
  expect_true(
    "r_stored interval 2 band b 1 | interval 1 interval 1 1 " %in% out,
    info = info
  )
  expect_true("method TRUE interval 2 interval 1 3 " %in% out, info = info)
  expect_true("ambiguities 0 " %in% out, info = info)
  expect_true(
    "replace pkgr::Band new 1 5 pkgr::Strip strip 1 " %in% out,
    info = info
  )
  expect_true("upper 9 r8_result_error TRUE " %in% out, info = info)
  expect_true("argument r8_argument_error " %in% out, info = info)
  expect_true(
    "convert [2, 8] [2, 8] pkgr::Signal new 1 5 " %in% out,
    info = info
  )
  expect_true(
    "child Signal strip (1 values) r8_dispatch_error " %in% out,
    info = info
  )
  expect_true("empty TRUE " %in% out, info = info)
  expect_true(
    "mirai tile 7 interval 2 interval 2 interval 1 1 [2, 8] " %in% out,
    info = info
  )
})

test_that("the signatures of compiled impls are those of their traits", {
  skip_if(
    !nzchar(Sys.getenv("R8_PACKAGE_TESTS")),
    "set R8_PACKAGE_TESTS to install the test packages"
  )
  lib <- install_test_packages()
  pkg <- normalizePath(test_path("packages", "r8pkgR"))
  session <- c(
    sprintf("dyn.load(%s)", deparse(file.path(pkg, "src", "r8pkgR.so"))),
    "library(R8)",
    "ptr <- .Call(R8:::.r8_native[['exports']], 'r8pkgR')",
    "op <- options(r8.namespace = 'pkgr')",
    sprintf("source(%s)", deparse(file.path(pkg, "R", "r8-types.R")))
  )
  in_r <- sprintf(
    "source(%s)",
    vapply(
      file.path(pkg, "R", c("interval.R", "norm.R")),
      deparse,
      character(1)
    )
  )
  out <- run_r(
    lib,
    c(
      session,
      in_r,
      "cat('agree', length(R8:::r8_exports_signatures(ptr, 'r8pkgR')), '\\n')",
      # Other declarations of the generics and the trait that R declares.
      "width <- generic('width', params(x, digits = arg_integer()))",
      "middle <- generic('middle', params(x), return_type = type_double())",
      "overlap <- generic('overlap', params(x, y))",
      "`upper<-` <- generic('upper<-', params(x, value = arg_integer()))",
      "total <- generic('total', params(x), return_type = type_double(min = 0))",
      "Norm <- new_trait('Norm', methods = list(magnitude = trait_method(params(x)), ratio = trait_method(params(x, to = arg_double())), distance = trait_method(params(x, y = arg(Band)))))",
      "p <- R8:::r8_exports_signatures(ptr, 'r8pkgR')",
      "cat('members', sort(vapply(p, function(x) x$member, '')), '\\n')",
      "e <- tryCatch(R8:::check_signatures(p, 'r8pkgR'), error = identity)",
      "cat('class', class(e)[1], '\\n')",
      "cat(conditionMessage(e), '\\n')",
      # Another declaration of a generic that Rust declares.
      "status <- function() { x <- R8:::r8_exports_compare(ptr, 'pkgr'); x$status[x$type_name == 'pkgr::shifted'] }",
      "cat('before', status(), '\\n')",
      "shifted <- generic('shifted', params(x, by = arg_double(infinite = TRUE, default = 2)), return_type = 'Self')",
      "cat('after', status(), '\\n')",
      "shifted <- generic('shifted', params(x, by = arg_double(infinite = TRUE, default = 1)), description = 'Another text.', return_type = 'Self')",
      "cat('described', status(), '\\n')",
      "e <- tryCatch(R8:::check_exports('p', list(type_name = 'p::f', status = 'missing', codec = FALSE, hash = ''), character()), error = conditionMessage)",
      "cat('absent', grepl('`p::f` is declared in the compiled exports and has no R declaration', e, fixed = TRUE), '\\n')"
    )
  )
  info <- paste(out, collapse = "\n")
  expect_true("agree 0 " %in% out, info = info)
  expect_true(
    "members distance middle overlap ratio upper<- width " %in% out,
    info = info
  )
  expect_true("class r8_declaration_error " %in% out, info = info)
  for (line in c(
    "- `width` of `pkgr::width` for `pkgr::Interval`: it takes the parameters (x), and the declaration (x, digits). Its parameters are declared in R as params(x).",
    "- `middle` of `pkgr::middle` for `pkgr::Interval`: it returns `pkgr::Interval`, and the declaration double. Its parameters are declared in R as params(x).",
    "- `overlap` of `pkgr::overlap` for `pkgr::Interval`: its parameter `y` is `pkgr::Interval`, and in the declaration without a type. Its parameters are declared in R as params(x, y = arg(Interval)).",
    "- `upper<-` of `pkgr::with_upper` for `pkgr::Interval`: its parameter `value` is double, and in the declaration integer. Its parameters are declared in R as params(x, value = arg_double()).",
    "- `ratio` of `pkgr::Norm` for `pkgr::Signal`: it takes the parameters (x, by), and the declaration (x, to). Its parameters are declared in R as params(x, by = arg_double()).",
    "- `distance` of `pkgr::Norm` for `pkgr::Signal`: its parameter `y` is `pkgr::Signal`, and in the declaration `pkgr::Band`. Its parameters are declared in R as params(x, y = arg(Signal))."
  )) {
    expect_true(line %in% out, info = paste(line, info, sep = "\n"))
  }
  expect_true(
    any(grepl("[impl_signature]", out, fixed = TRUE)),
    info = info
  )
  expect_true("before same " %in% out, info = info)
  expect_true("after differs " %in% out, info = info)
  expect_true("described same " %in% out, info = info)
  expect_true("absent TRUE " %in% out, info = info)
  # The result type of a method that returns a narrower type than its generic.
  out <- run_r(
    lib,
    c(
      "suppressPackageStartupMessages({ library(R8); library(r8pkgR) })",
      "say <- function(...) cat(..., '\\n')",
      "i <- jsonlite::fromJSON(r8_declaration(Interval), simplifyVector = FALSE)$impls",
      "own <- Filter(function(x) identical(x$trait, 'pkgr::middle'), i)[[1]]$methods[[1]]",
      "say('own', own$name, own$return_type$name)",
      "x <- Interval(lo = 0, hi = 4); say('default', shifted(x)$lo, shifted(x, 2)$lo)",
      "b <- Band(lo = 0, hi = 4, label = 'b', weights = 1)",
      "say('pattern', class(tryCatch(label(b) <- 'B', error = identity))[1])",
      "say('print', format(args(refit))[1])"
    )
  )
  info <- paste(out, collapse = "\n")
  expect_true("default 1 2 " %in% out, info = info)
  expect_true("pattern r8_argument_error " %in% out, info = info)
})
