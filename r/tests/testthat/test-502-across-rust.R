# test-502-across-rust.R
# ::R8::
# 2026- EDG rtemis.org

# Rust code of one package that uses the types of another.
#
# The crate of a package may depend on the crate of another. Each package
# holds its own compiled copy of the types that they share. These tests
# check a struct that extends one of the other package, fields that hold
# values of types the code does not know, and reads of another package's
# values whose declaration has changed since the code was compiled.

scenario(
  "D4",
  "Rust code of one package uses the types and traits of another",
  situation = "The Rust crate of package pb depends on that of pa, declares a struct that extends one of pa, and implements traits of pa for it.",
  risk = "Each package holds its own compiled copy of the types, whose memory the other must not read.",
  result = "A value is read in place by the package whose code built it, and by its fields in the other. Methods of each package apply to the values of both.",
  how = "installed",
  code = {
    skip_if_not_installed("mirai")
    lib <- install_test_packages()
    out <- run_r(
      lib,
      c(
        "suppressPackageStartupMessages({ library(R8); library(r8pkgR); library(r8pkgS) })",
        "say <- function(...) cat(..., '\\n')",
        "msg <- function(x) tryCatch(x, error = function(e) conditionMessage(e))",
        "t <- Tile(lo = 1, hi = 3, number = 2L)",
        "say('class', class(t))",
        # The impl of r8pkgR's crate for every struct that extends Interval.
        "u <- shifted(t, 2)",
        "say('shifted', class(u)[1], u$lo, u$hi, u$number)",
        # r8pkgR's compiled impls for Interval read a value of r8pkgS by its fields.
        "v <- r8upcast(t, Interval); m <- middle(t)",
        "say('ancestor', width(t), format(t), class(v)[1], v$hi, class(m)[1], m$lo)",
        # An impl of a trait of r8pkgR's crate, and a parameter of r8pkgR's type.
        "say('total', total(t), grepl('does not implement', msg(total(v))))",
        "say('gap', gap(t, Interval(lo = 7, hi = 8)), grepl('Invalid arguments of `gap()`', msg(gap(t, t)), fixed = TRUE))",
        "say('own', grepl('does not implement `pkgs::gap`', msg(gap(v, v)), fixed = TRUE))",
        "m <- mirai::mirai({ library(r8pkgS); list(r8pkgR::width(t), class(r8pkgR::shifted(t, 1))[1], inherits(t, r8pkgR::Interval), names(as.list(t))[4]) }, t = t)[]",
        "say('mirai', unlist(m))"
      )
    )
    info <- paste(out, collapse = "\n")
    expect_true(
      "class pkgs::Tile pkgr::Interval r8_value " %in% out,
      info = info
    )
    expect_true("shifted pkgs::Tile 3 5 2 " %in% out, info = info)
    expect_true(
      "ancestor 2 [1, 3] pkgr::Interval 3 pkgr::Interval 1.5 " %in% out,
      info = info
    )
    expect_true("total 5 TRUE " %in% out, info = info)
    expect_true("gap 4 TRUE " %in% out, info = info)
    expect_true("own TRUE " %in% out, info = info)
    expect_true("mirai 2 pkgs::Tile TRUE number " %in% out, info = info)
    out <- run_r(
      lib,
      sprintf(
        "R8::r8document_rust(%s, check = TRUE); cat('current\\n')",
        deparse(normalizePath(test_path("packages", "r8pkgS")))
      )
    )
    expect_true("current" %in% out, info = paste(out, collapse = "\n"))
  }
)

scenario(
  "D5",
  "a value of Rust code holds values of types that the code does not know",
  situation = "A struct in the Rust code of pa has a field for any interval. A user puts in an interval that pb, or the user, declared later.",
  risk = "The code treats the value as one that it knows, or loses the fields that it does not.",
  result = "The value is kept and given back unchanged, its known part is read, and the fields of a saved value read without the packages that declare them.",
  how = "installed",
  code = {
    skip_if_not_installed("mirai")
    lib <- install_test_packages()
    rds <- c(tempfile(fileext = ".rds"), tempfile(fileext = ".rds"))
    session <- c(
      "suppressPackageStartupMessages({ library(R8); library(r8pkgR); library(r8pkgS) })",
      "say <- function(...) cat(..., '\\n')",
      "msg <- function(x) tryCatch(x, error = function(e) conditionMessage(e))",
      "strip <- Strip(lo = 1, hi = 4, label = 's', weights = c(1, 2), number = 7L)",
      "tile <- Tile(lo = 1, hi = 3, number = 2L)",
      "frame <- Interval(lo = 0, hi = 2)",
      "p <- Panel(range = strip, inset = strip, frame = frame, method = Resampler@KFold(k = 3L, seed = 1L), figure = Square(side = 2))",
      # A value of r8pkgS's type, which r8pkgR's code does not know.
      "q <- Panel(range = tile, frame = frame, figure = Square(side = 1))"
    )
    out <- run_r(
      lib,
      c(
        session,
        "say('contents', contents(p))",
        "say('unknown', contents(q))",
        # A held value is the value itself, and a nested one a value of its type.
        "say('read', identical(p$range, strip), class(q$range)[1], class(p$frame)[1], p$frame$hi, r8variant(p$method), is.null(q$method), is.null(q$inset))",
        "w <- q; w$range <- strip; w$frame <- Interval(lo = 0, hi = 10); w$method <- Resampler@Loocv",
        "say('write', contents(w), r8variant(w$method), contents(q))",
        "say('held', class(held(p))[1], class(held(q))[1])",
        # Another value of an ancestor keeps the type and its other fields.
        "m <- moved(p, 2, 9); n <- moved(q, 0, 1)",
        "say('moved', class(m$range)[1], m$range$lo, m$range$hi, m$range$label, m$range$number, contents(m))",
        "say('moved unknown', class(n$range)[1], n$range$hi, n$range$number, class(held(n))[1])",
        # The rules of the held type are checked by the method that makes the value.
        "say('invalid', msg(moved(q, 5, 1)))",
        "b <- banded(p, 'new')",
        "say('new', class(b$range)[1], b$range$label, b$range$hi, b$range$weights, class(b$figure)[1], b$figure$side)",
        "say('json', vapply(list(p, q, m), function(x) { y <- r8from_json(Panel, r8to_json(x)); r8equal(x, y) && r8hash(x) == r8hash(y) && contents(x) == contents(y) }, NA))",
        "say('tag', grepl('\"figure\":{\"side\":2,\"name\":\"square\",\"figure\":\"Square\"}', r8to_json(p), fixed = TRUE))",
        "say('serialize', vapply(list(p, q, m), function(x) { y <- unserialize(serialize(x, NULL)); r8equal(x, y) && contents(x) == contents(y) }, NA))",
        "say('r_stored', contents(r_stored(q)))",
        # An implementor declared in R, and a type that implements nothing.
        "op <- options(r8.namespace = 'demo')",
        "Circle <- new_struct('Circle', fields = list(r = type_double(), name = type_character()))",
        "impl(Figure, Circle, methods = list(area = function(x) x$r, scaled = function(x, by) { x$r <- x$r * by; x }))",
        "x <- Panel(range = frame, frame = frame, figure = Circle(r = 3, name = 'c')); y <- r8from_json(Panel, r8to_json(x))",
        "say('implementor', contents(x), area(x$figure), r8equal(x, y), contents(y))",
        "say('refused', grepl('not_implementor', msg(Panel(range = frame, frame = frame, figure = strip))), grepl('not_subtype', msg(Panel(range = Square(side = 1), frame = frame, figure = Square(side = 1)))), grepl('invalid', msg(Panel(range = frame, frame = strip, figure = Square(side = 1))), ignore.case = TRUE))",
        # Parameters of a method in Rust: an open value, an optional value, and an optional vector.
        "r <- refit(p, tile, Circle(r = 2, name = 'd'))",
        "say('refit', contents(r), r$frame$hi, refit(p, strip, Square(side = 1), Interval(lo = 0, hi = 4), c(1, 2))$frame$hi)",
        "say('refit_refused', vapply(list(quote(refit(p, 1, Square(side = 1))), quote(refit(p, tile, tile)), quote(refit(p, tile, Square(side = 1), strip)), quote(refit(p, tile, Square(side = 1), weights = -1))), function(e) class(tryCatch(eval(e), error = identity))[1], ''))",
        # A trait declared in Rust, with a required field, an impl in R, and one in Rust.
        "say('scaled', scaled(Square(side = 2), 3)$side, scaled(Circle(r = 2, name = 'c'), 2)$r, Square(side = 1)$name, class(tryCatch(scaled(Square(side = 1), 0), error = identity))[1])",
        "say('required', grepl('required_field_missing', msg(impl(Figure, new_struct('Bare', fields = list(r = type_double())), methods = list(area = function(x) 1, scaled = function(x, by) x)))))",
        # Fields of r8pkgR's types in a type of r8pkgS, whose library does not build them.
        "g <- Grid(cell = frame, method = Resampler@Custom(train = 1:2, test = 3L), extent = Band(lo = 0, hi = 8, label = 'b', weights = 1), figure = Square(side = 3), note = tile)",
        "say('noted', noted(g$note), grepl('not_implementor', msg(g$note <- strip)))",
        "h <- r8from_json(Grid, r8to_json(g)); u <- unserialize(serialize(g, NULL))",
        "say('grid', cells(g), cells(h), cells(u), r8equal(g, h), r8equal(g, u))",
        "say('nested', class(g$cell)[1], h$cell$hi, r8variant(u$method), h$method$train, width(h$cell), format(u$method), area(h$figure))",
        "g$method <- Resampler@Loocv; say('unit', format(g$method), format(r8from_json(Grid, r8to_json(g))$method))",
        "say('result', class(lower(tile))[1], lower(tile)$hi, width(lower(tile)))",
        "k <- mirai::mirai({ library(r8pkgS); list(r8pkgR::contents(q), class(q$range)[1], r8pkgR::moved(q, 1, 5)) }, q = q)[]",
        "say('mirai', k[[1]], k[[2]], contents(k[[3]]))",
        sprintf(
          "saveRDS(q, %s); saveRDS(moved(q, 1, 2), %s)",
          deparse(rds[[1]]),
          deparse(rds[[2]])
        )
      )
    )
    info <- paste(out, collapse = "\n")
    expect_true("contents strip 7; s 3; square 4; 5 " %in% out, info = info)
    expect_true("unknown pkgs::Tile; none; square 1; 4 " %in% out, info = info)
    expect_true(
      "read TRUE pkgs::Tile pkgr::Interval 2 KFold TRUE TRUE " %in% out,
      info = info
    )
    expect_true(
      "write strip 7; none; square 1; 13 Loocv pkgs::Tile; none; square 1; 4 " %in%
        out,
      info = info
    )
    expect_true("held pkgr::Strip pkgs::Tile " %in% out, info = info)
    expect_true(
      "moved pkgr::Strip 2 9 s 7 strip 7; s 3; square 4; 9 " %in% out,
      info = info
    )
    expect_true(
      "moved unknown pkgs::Tile 1 2 pkgs::Tile " %in% out,
      info = info
    )
    expect_true(
      "invalid the rule `ordered` is violated [invalid_value] " %in% out,
      info = info
    )
    expect_true(
      "new pkgr::Band new 2 1 2 pkgr::Square 2 " %in% out,
      info = info
    )
    expect_true("json TRUE TRUE TRUE " %in% out, info = info)
    expect_true("tag TRUE " %in% out, info = info)
    expect_true("serialize TRUE TRUE TRUE " %in% out, info = info)
    expect_true("r_stored pkgs::Tile; none; square 1; 4 " %in% out, info = info)
    expect_true(
      "implementor pkgr::Interval; none; demo::Circle; 4 3 TRUE pkgr::Interval; none; demo::Circle; 4 " %in%
        out,
      info = info
    )
    expect_true("refused TRUE TRUE TRUE " %in% out, info = info)
    expect_true(
      "refit pkgs::Tile; s 3; demo::Circle; 4 2 7 " %in% out,
      info = info
    )
    expect_true(
      paste("refit_refused", strrep("r8_argument_error ", 4L)) %in% out,
      info = info
    )
    expect_true("scaled 6 4 square r8_argument_error " %in% out, info = info)
    expect_true("required TRUE " %in% out, info = info)
    expect_true("grid 4 b 1 4 b 1 4 b 1 TRUE TRUE " %in% out, info = info)
    expect_true(
      "nested pkgr::Interval 2 Custom 1 2 2 Custom (2 train, 1 test) 9 " %in%
        out,
      info = info
    )
    expect_true("noted tile 2 TRUE " %in% out, info = info)
    expect_true("unit LOOCV LOOCV " %in% out, info = info)
    expect_true("result pkgr::Interval 2 1 " %in% out, info = info)
    expect_true(
      "mirai pkgs::Tile; none; square 1; 4 pkgs::Tile pkgs::Tile; none; square 1; 6 " %in%
        out,
      info = info
    )
    # The fields of a value read before its packages are loaded; a method is given
    # the value once the packages that declare its parts are.
    out <- run_r(
      lib,
      c(
        "say <- function(...) cat(..., '\\n')",
        "msg <- function(x) tryCatch(x, error = function(e) conditionMessage(e))",
        sprintf(
          "q <- readRDS(%s); m <- readRDS(%s)",
          deparse(rds[[1]]),
          deparse(rds[[2]])
        ),
        "say('first', q$frame$hi, q$range$number)",
        "suppressPackageStartupMessages(library(r8pkgR))",
        "say('second', q$frame$hi); say('third', msg(contents(q)))",
        "suppressPackageStartupMessages(library(r8pkgS))",
        "say('then', contents(q), q$range$number, m$range$hi)"
      )
    )
    info <- paste(out, collapse = "\n")
    expect_true("first 2 2 " %in% out, info = info)
    expect_true("second 2 " %in% out, info = info)
    expect_true(any(grepl("^third .*pkgs::Tile", out)), info = info)
    expect_true(
      "then pkgs::Tile; none; square 1; 4 2 2 " %in% out,
      info = info
    )
    # Each operation that reaches R from a codec, under R's torture of the
    # collector.
    out <- run_r(
      lib,
      c(
        session,
        "g <- Grid(cell = frame, method = Resampler@Custom(train = 1:2, test = 3L), extent = Band(lo = 0, hi = 8, label = 'b', weights = 1), figure = Square(side = 3))",
        "pj <- r8to_json(p); gj <- r8to_json(g)",
        "run <- function() list(contents(moved(p, 2, 9)), moved(q, 2, 9)$range$number, banded(p, 'n')$range$weights, r8from_json(Panel, pj)$range$weights, r8from_json(Grid, gj)$method$train, unserialize(serialize(p, NULL))$range$weights, contents(unserialize(serialize(p, NULL))), unserialize(serialize(g, NULL))$extent$weights, lower(tile)$hi, r8from_json(Grid, gj)$cell$hi)",
        "a <- run(); gctorture2(1); b <- run(); gctorture2(0)",
        "say('torture', identical(a, b), unlist(b[-1]))"
      )
    )
    expect_true(
      "torture TRUE 2 1 2 1 2 1 2 1 2 strip 7; s 3; square 4; 5 1 2 2 " %in%
        out,
      info = paste(out, collapse = "\n")
    )
  }
)
