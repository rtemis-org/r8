# test-301-package.R
# ::R8::
# 2026- EDG rtemis.org

# Packages that declare R8 types: they register when they load.
#
# A package declares its types when it is installed, and registers them in
# each session that loads it. These tests check a package that loads, one
# that implements the trait of a package that it has to load first, one that
# is refused, and one that a developer loads again and again.

scenario(
  "B1",
  "a package loads the package whose trait it implements",
  situation = "Package pb implements a trait of package pa, and imports from it. A user loads pb in a session that has not loaded pa.",
  risk = "R registers the methods of pb before the trait exists, or pb fails with an error about a package that the user did not mention.",
  result = "pa loads first, by the import. A package that names pa with :: and imports nothing from it is refused, with the line to add to its NAMESPACE.",
  how = "installed",
  code = {
    lib <- install_test_packages()
    out <- run_r(
      lib,
      c(
        "suppressPackageStartupMessages(library(r8pkgB))",
        "cat('loaded', 'r8pkgA' %in% loadedNamespaces(), round(r8pkgA::area(Circle(r = 1)), 4), '\\n')"
      )
    )
    expect_true(
      "loaded TRUE 3.1416 " %in% out,
      info = paste(out, collapse = "\n")
    )
    # Without the import, the error names it, for an impl in R and a compiled
    # one; the generic's name is the name its package exports.
    refused <- function(pkg, libs, code = character()) {
      out <- run_r(
        c(without_imports(lib, pkg), libs),
        c(
          code,
          sprintf(
            "m <- tryCatch(loadNamespace('%s'), error = function(e) conditionMessage(e))",
            pkg
          ),
          "cat('message', gsub('\\n', ' ', m), '\\n')"
        )
      )
      sub("^message ", "", grep("^message", out, value = TRUE))
    }
    expect_match(
      refused("r8pkgB", lib),
      paste0(
        "Package 'r8pkgB' implements `pkga::Area`, which is not registered: ",
        "package 'r8pkgA' declares the namespace `pkga` and is not loaded. ",
        "Import from it in NAMESPACE, so that it loads first: ",
        "importFrom(r8pkgA, Area)."
      ),
      fixed = TRUE
    )
    expect_match(
      refused("r8pkgS", lib),
      paste0(
        "Package 'r8pkgS' implements `pkgr::shifted`, which is not registered: ",
        "package 'r8pkgR' declares the namespace `pkgr` and is not loaded. ",
        "Import from it in NAMESPACE, so that it loads first: ",
        "importFrom(r8pkgR, shifted)."
      ),
      fixed = TRUE
    )
    # A refused package leaves the registry as it was, and loads once the other
    # package is.
    out <- run_r(
      c(without_imports(lib, "r8pkgS"), lib),
      c(
        "try(loadNamespace('r8pkgS'), silent = TRUE)",
        "cat('left', R8:::r8_is_registered('pkgs::Tile'), '\\n')",
        "loadNamespace('r8pkgR'); loadNamespace('r8pkgS')",
        "cat('then', class(r8pkgR::shifted(r8pkgS::Tile(lo = 1, number = 1L), 1))[1], '\\n')"
      )
    )
    info <- paste(out, collapse = "\n")
    expect_true("left FALSE " %in% out, info = info)
    expect_true("then pkgs::Tile " %in% out, info = info)
  }
)

scenario(
  "B2",
  "a package registers its types when it loads",
  situation = "A package declares structs, enums, traits, and methods. A user loads it, builds values, saves them, and reads them in a session that has not loaded the package.",
  risk = "The types of the session disagree with those of the package, or a saved value loses what it is.",
  result = "Loading the package registers its types and methods once. A saved value read without the package keeps its data, and is a full value again when the package loads.",
  how = "installed",
  code = {
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
    expect_true(
      "class pkga::Rect " %in% out,
      info = paste(out, collapse = "\n")
    )
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
  }
)

scenario(
  "B3",
  "a developer loads a package again and again",
  situation = "A developer changes a package and runs load_all() after each change.",
  risk = "Each load adds a second copy of every type and method, or is refused as a duplicate.",
  result = "Each load replaces the declarations of the one before.",
  how = "installed",
  code = {
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
  }
)

scenario(
  "B4",
  "a package that masks a function of R is refused",
  situation = "A package defines a function named as one of R that its generics call, such as `.Call`.",
  risk = "The generics of the package call its function, and fail far from the cause.",
  result = "The package is refused when it loads, with the name of the function.",
  how = "installed",
  code = {
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
  }
)

scenario(
  "B5",
  "a package that fails to load leaves the session as it was",
  situation = "A package declares a type, and then a method for a generic that does not exist. A user tries to load it, installs a corrected version, and loads that.",
  risk = "The types of the failed load stay registered, and the corrected package is refused, or meets the types of the first.",
  result = "The first load is an error and registers nothing. The corrected package loads.",
  how = "installed",
  code = {
    type <- "Point <- R8::new_struct('Point', fields = list(%s = R8::type_double()))"
    good <- install_version(write_package(
      tempfile("src"),
      "pa",
      sprintf(type, "y"),
      "Point"
    ))
    bad <- tempfile("lib")
    path <- write_package(
      tempfile("src"),
      "pa",
      c(
        sprintf(type, "x"),
        ".onLoad <- function(libname, pkgname) { R8::r8_register(); stop('the load fails') }",
        "invisible(NULL)"
      ),
      "Point"
    )
    # The last definition of .onLoad() is the one that the package has.
    code <- readLines(file.path(path, "R", "types.R"))
    writeLines(
      code[-length(code)],
      file.path(path, "R", "types.R")
    )
    dir.create(bad)
    suppressWarnings(system2(
      file.path(R.home("bin"), "R"),
      c(
        "CMD",
        "INSTALL",
        "--no-docs",
        "--no-test-load",
        paste0("--library=", bad),
        path
      ),
      stdout = FALSE,
      stderr = FALSE
    ))
    out <- said(
      character(),
      c(
        sprintf("say('first', library(pa, lib.loc = %s))", deparse(bad)),
        "say('loaded', 'pa' %in% loadedNamespaces())",
        sprintf(
          "say('second', { library(pa, lib.loc = %s); names(Point(y = 1)) })",
          deparse(good)
        )
      )
    )
    expect_said(
      out,
      first = "^error: .*the load fails",
      loaded = "^FALSE$",
      second = "^y$"
    )
  }
)

scenario(
  "B6",
  "a package that is unloaded and loaded again works",
  situation = "A user unloads a package and loads it again, as after an update in the same session.",
  risk = "The second load is refused as a duplicate, or the methods of the first answer.",
  result = "The package loads again, and its methods are those of the second load.",
  how = "installed",
  code = {
    first <- pa_first()
    out <- said(
      first$pa,
      c(
        "library(pa)",
        "p <- Point(x = 3, y = 2)",
        "unloadNamespace('pa')",
        "say('kept', unlist(as.list(p)))",
        "library(pa)",
        "say('again', c(mag(p), mag(Point(x = 4, y = 2))))"
      )
    )
    expect_said(out, kept = "^3 2$", again = "^3 4$")
  }
)

scenario(
  "B7",
  "the types and methods of an unloaded package are not used",
  situation = "A user unloads a package, and calls a generic of another package for a value of its type.",
  risk = "Code of a package that is not loaded answers.",
  result = "The call is an error that names the package to load.",
  how = "installed",
  open = "R8 keeps the declarations and the methods in R of an unloaded package."
)
