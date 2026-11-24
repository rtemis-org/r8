# test-501-across.R
# ::R8::
# 2026- EDG rtemis.org

# A type of one package used in another, both current.
#
# These tests install two packages, one of which uses the types of the
# other: as the parent of a struct, the type of a field, the argument of a
# generic, and the first argument of a method.

scenario(
  "D1",
  "a struct extends a struct of another package, until that struct changes",
  situation = "Package pb declares a struct that extends a struct of pa. Later pa changes the parent, and pb is not installed again.",
  risk = "The child keeps the fields and rules of the earlier parent, and methods of pa meet a value of another shape.",
  result = "Methods of pa apply to the child. After the change, pb is refused when it loads, and asks to be installed again.",
  how = "installed",
  code = {
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
  }
)

scenario(
  "D2",
  "a type of one package is a field, an argument, and the owner of a method in another",
  situation = "Package pb has a struct with a field of a type of pa, a generic with an argument of the type, and a method of a generic of pa for its own struct.",
  risk = "A value that is not of the type goes into the field or the argument, or a call runs the method of the other package.",
  result = "The field and the argument take values of the type only, and a call runs the method of the type of its argument.",
  how = "installed",
  code = {
    first <- pa_first()
    out <- said(
      c(first$pb, first$pa),
      c(
        "library(pb)",
        "h <- Holder(p = pa::Point(x = 6, y = 2), label = 'a')",
        "say('field', c(h$p$x, R8::r8_variant(h$kind)))",
        "say('argument', first(h, pa::Point(x = 1, y = 0)))",
        "say('method', c(pa::mag(h), pa::mag(h$p)))",
        "say('wrong field', Holder(p = h, label = 'a'))",
        "say('wrong argument', first(h, h))",
        "say('wrong call', pa::mag(3))"
      )
    )
    expect_said(
      out,
      field = "^6 A$",
      argument = "^7$",
      method = "^106 +6$",
      "wrong field" = "^error: .*`p`",
      "wrong argument" = "^error: .*`p`",
      "wrong call" = "^error: .*does not implement"
    )
  }
)

scenario(
  "D3",
  "one package converts the values of another",
  situation = "Package pc declares a type, and a conversion to it from a type of pa.",
  risk = "The conversion is not found, or runs for a value of another type.",
  result = "The conversion runs for values of pa's type, and any other value is an error.",
  how = "installed",
  code = {
    first <- pa_first()
    pc <- install_version(
      write_package(
        tempfile("src"),
        "pc",
        c(
          "Polar <- R8::new_struct('Polar', fields = list(r = R8::type_double(min = 0)))",
          "R8::impl(R8::From(pa::Point), Polar, list(from = function(x) Polar(r = sqrt(x$x^2 + x$y^2))))"
        ),
        "Polar",
        imports = "pa"
      ),
      libs = first$pa
    )
    out <- said(
      c(pc, first$pa),
      c(
        "library(pc)",
        "say('convert', R8::r8_convert(pa::Point(x = 3, y = 4), Polar)$r)",
        "say('from', Polar@from(pa::Point(x = 3, y = 4))$r)",
        "say('other', R8::r8_convert(pa::Kind@A, Polar))"
      )
    )
    expect_said(out, convert = "^5$", from = "^5$", other = "^error: ")
  }
)

scenario(
  "D6",
  "two packages that use a third agree on its type",
  situation = "Packages pb and pc both use a type of pa, and a script passes values from one to the other.",
  risk = "Each has its own copy of the type, and refuses the values of the other.",
  result = "Both use the one type of pa, in either order of loading.",
  how = "installed",
  code = {
    first <- pa_first()
    pc <- install_version(
      write_package(
        tempfile("src"),
        "pc",
        c(
          "make <- function() pa::Point(x = 1, y = 2)",
          "Pair <- R8::new_struct('Pair', fields = list(a = pa::Point, b = pa::Point))"
        ),
        c("make", "Pair"),
        imports = "pa"
      ),
      libs = first$pa
    )
    libs <- c(pc, first$pb, first$pa)
    for (order in list(c("pb", "pc"), c("pc", "pb"))) {
      out <- said(
        libs,
        c(
          sprintf("library(%s)", order),
          "h <- Holder(p = make(), label = 'a')",
          "say('used', first(h, make()))",
          "say('pair', Pair(a = h$p, b = make())$b$y)"
        )
      )
      expect_said(out, used = "^2$", pair = "^2$")
    }
  }
)
