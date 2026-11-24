# test-601-versions.R
# ::R8::
# 2026- EDG rtemis.org

# Scenarios of updates: one package is updated on CRAN or r-universe, and the
# packages that use its types are not.
#
# Package pa declares a type. Package pb uses it: as the type of a field, as the
# argument of a generic, and in a method of a generic of pa. Each test installs
# pb for pa 1.0.0, then installs pa 2.0.0 with one change into another library,
# and runs a fresh session with both, as after install.packages("pa").
# `helper-pa.R` holds the packages.

scenario(
  "E1",
  "a package is updated and lists the fields of a type in another order",
  situation = "pa 2.0.0 lists the field y before x. pb, built for pa 1.0.0, is not installed again.",
  risk = "R code of pb, which names the fields, reads another one.",
  result = "pb loads and works. A value that the user saved is read by name, and is used once r8_update() has converted it. A document that holds its declarations is read, and one that does not is refused.",
  how = "installed",
  code = {
    out <- with_pa(
      "reordered",
      c(
        "say('new', first(Holder(p = pa::Point(x = 6, y = 2), label = 'a'), pa::Point(x = 1, y = 0)))",
        "say('read', c(p$x, p$y))",
        "say('method', pa::mag(h))",
        "say('updated', { h <- R8::r8_update(h); c(pa::mag(h), h$p$y, names(h$p)) })",
        "say('document', { x <- R8::r8_from_json(pa::Point, pd); c(x$x, x$y) })",
        "say('short', R8::r8_from_json(pa::Point, pj))",
        "say('any', { x <- R8::r8_from_json(pa::Point, pj, declaration = 'any'); c(x$x, x$y) })",
        "say('write', { p$y <- 9; p })"
      )
    )
    expect_said(
      out,
      load = "^loaded$",
      new = "^7$",
      read = "^3 2$",
      method = "^error: .*in the order x, y, and are in the order y, x \\(in `pa::Point`\\)",
      updated = "^103 +2 +y +x",
      document = "^3 2$",
      short = "^error: .*another declaration of `pa::Point`.*declaration = \"any\"",
      any = "^3 2$",
      write = "^error: .*declaration_changed"
    )
  }
)

scenario(
  "E2",
  "a package is updated and adds an optional field to a type",
  situation = "pa 2.0.0 adds a field of type option() to its type. pb is not installed again, and the user reads a saved value and a document.",
  risk = "Code of pb that does not know the field fails.",
  result = "pb loads and works, and a document that holds its declarations is read. A value saved before has no such field: reading it is an error that says so, and r8_update() gives the value with the field absent.",
  how = "installed",
  code = {
    out <- with_pa(
      "optional",
      c(
        "say('new', first(Holder(p = pa::Point(x = 6, y = 2), label = 'a'), pa::Point(x = 1, y = 0)))",
        "say('document', is.null(R8::r8_from_json(pa::Point, pd)$z))",
        "say('short', R8::r8_from_json(pa::Point, pj))",
        "say('holder', R8::r8_from_json(Holder, hd)$p$x)",
        "say('saved', pa::mag(p))",
        "say('field', p$z)",
        "say('updated', { u <- R8::r8_update(list(p, h)); c(pa::mag(u[[1]]), pa::mag(u[[2]]), is.null(u[[1]]$z)) })"
      )
    )
    expect_said(
      out,
      load = "^loaded$",
      new = "^7$",
      document = "^TRUE$",
      short = "^error: .*declaration_changed",
      holder = "^3$",
      saved = "^error: .*`z` was added as an optional field; r8_update\\(\\) gives",
      field = "^error: `z` is not a field of `pa::Point`. Fields: x, y",
      updated = "^ *3 103 +1$"
    )
  }
)

scenario(
  "E3",
  "a package is updated and adds a required field to a type",
  situation = "pa 2.0.0 adds a required field to its type. pb builds the type without it, and the user reads a saved value and a document.",
  risk = "Values without the field go around.",
  result = "Building a value without the field, reading the document, and checking the saved value are errors that name the field.",
  how = "installed",
  code = {
    out <- with_pa(
      "required",
      c(
        "say('new', pa::Point(x = 6, y = 2))",
        "say('document', R8::r8_from_json(pa::Point, pj))",
        "say('validate', R8::r8_validate(p))",
        "say('nested', R8::r8_validate(h))",
        "say('write', R8::r8_to_json(p))",
        "say('field', Holder(p = p, label = 'b'))",
        "say('update', R8::r8_update(p))"
      )
    )
    added <- "^error: .*`z` was added without a default.*declaration_changed"
    expect_said(
      out,
      load = "^loaded$",
      new = "^error: .*z",
      document = "^error: .*`z`.*missing_field",
      validate = added,
      nested = added,
      write = added,
      field = added,
      update = added
    )
  }
)

scenario(
  "E4",
  "a package is updated and removes a field of a type",
  situation = "pa 2.0.0 removes the field y. pb and the user's files still have it.",
  risk = "The field is dropped without notice, or read as another.",
  result = "Building a value with the field, reading the document, and checking the saved value are errors that name it.",
  how = "installed",
  code = {
    out <- with_pa(
      "removed",
      c(
        "say('new', pa::Point(x = 6, y = 2))",
        "say('document', R8::r8_from_json(pa::Point, pj))",
        "say('validate', R8::r8_validate(p))",
        "say('update', R8::r8_update(p))"
      )
    )
    expect_said(
      out,
      load = "^loaded$",
      new = "^error: .*y",
      document = "^error: .*`y`.*unknown_field",
      validate = "^error: .*`y` was removed.*declaration_changed",
      update = "^error: .*`y` was removed.*declaration_changed"
    )
  }
)

scenario(
  "E5",
  "a package is updated and tightens a bound of a type",
  situation = "pa 2.0.0 requires x to be at least 5. The user's saved value has 3.",
  risk = "A value outside the bound goes around.",
  result = "Building such a value, reading the document, and checking the saved value are errors that give the bound and the value found.",
  how = "installed",
  code = {
    out <- with_pa(
      "tightened",
      c(
        "say('new', pa::Point(x = 3, y = 2))",
        "say('document', R8::r8_from_json(pa::Point, pj))",
        "say('validate', R8::r8_validate(p))",
        "say('field', Holder(p = p, label = 'b'))",
        "say('update', R8::r8_update(p))"
      )
    )
    expect_said(
      out,
      load = "^loaded$",
      new = "^error: .*at least 5; found 3",
      document = "^error: .*at least 5; found 3",
      validate = "^error: .*\\[0, Inf\\), and is double \\[5, Inf\\)",
      field = "^error: .*\\[0, Inf\\), and is double \\[5, Inf\\)",
      update = "^error: .*at least 5; found 3"
    )
  }
)

scenario(
  "E6",
  "a package is updated and changes the type of a field",
  situation = "pa 2.0.0 declares x as a string where it was a number.",
  risk = "Arithmetic of pb meets a string, or a number is read as text.",
  result = "Reading the document and checking the saved value are errors that name the field and both kinds.",
  how = "installed",
  code = {
    out <- with_pa(
      "retyped",
      c(
        "say('document', R8::r8_from_json(pa::Point, pj))",
        "say('validate', R8::r8_validate(p))",
        "say('update', R8::r8_update(p))"
      )
    )
    expect_said(
      out,
      load = "^loaded$",
      document = "^error: .*`x`.*type_mismatch",
      validate = "^error: .*`x` was double \\[0, Inf\\), and is character",
      update = "^error: .*`x` was double \\[0, Inf\\), and is character"
    )
  }
)

scenario(
  "E7",
  "a package is updated and an enum loses a variant",
  situation = "A user saved a value of the variant C of pa 2.0.0, and reads it where pa 1.0.0 is installed, which has A and B.",
  risk = "Code that handles every variant meets one that it does not know.",
  result = "Checking the value, writing it, and reading its document are errors that name the variant.",
  how = "installed",
  code = {
    first <- pa_first()
    newer <- pa_library("first", kinds = c("A", "B", "C"))
    rds <- tempfile(fileext = ".rds")
    json <- tempfile(fileext = ".json")
    said(
      newer,
      c(
        sprintf("saveRDS(pa::Kind@C, %s)", deparse(rds)),
        sprintf("writeLines(R8::r8_to_json(pa::Kind@C), %s)", deparse(json))
      )
    )
    out <- said(
      first$pa,
      c(
        "library(pa)",
        sprintf("k <- readRDS(%s)", deparse(rds)),
        "say('validate', R8::r8_validate(k))",
        "say('write', R8::r8_to_json(k))",
        sprintf(
          "say('read', R8::r8_from_json(Kind, readLines(%s)))",
          deparse(json)
        )
      )
    )
    expect_said(
      out,
      validate = "^error: .*the variant `C` was removed",
      write = "^error: .*the variant `C` was removed",
      read = "^error: .*C"
    )
  }
)

scenario(
  "E8",
  "a package is updated and an enum gains a variant, which another has as a default",
  situation = "pa 2.0.0 adds the variant C to its enum. pb has a field of the enum with the default A, and is not installed again, so its constructor holds a value made before. The user has a saved value of the enum too.",
  risk = "pb cannot build its own type with its own default.",
  result = "pb builds its type: the default is a value of the new declaration. The saved value is used once r8_update() has converted it.",
  how = "installed",
  code = {
    first <- pa_first()
    rds <- tempfile(fileext = ".rds")
    said(first$pa, sprintf("saveRDS(pa::Kind@B, %s)", deparse(rds)))
    out <- with_pa(
      "first",
      c(
        "n <- Holder(p = pa::Point(x = 6, y = 2), label = 'a')",
        "say('default', c(R8::r8_variant(n$kind), R8::r8_equal(n$kind, pa::Kind@A)))",
        sprintf("k <- readRDS(%s)", deparse(rds)),
        "say('saved', Holder(p = n$p, label = 'b', kind = k))",
        "say('updated', R8::r8_variant(Holder(p = n$p, label = 'b', kind = R8::r8_update(k))$kind))"
      ),
      kinds = c("A", "B", "C")
    )
    expect_said(
      out,
      load = "^loaded$",
      default = "^A +TRUE$",
      saved = "^error: .*the variant `C` was added",
      updated = "^B$"
    )
  }
)

scenario(
  "E9",
  "a package is updated and raises the version of a type",
  situation = "pa 2.0.0 raises the version of its type to 2, the author's statement that earlier values mean something else. The user reads a document and a saved value of version 1.",
  risk = "The earlier value is taken as one of the new version.",
  result = "The document is refused, with both versions.",
  how = "installed",
  code = {
    out <- with_pa(
      "first",
      "say('document', R8::r8_from_json(pa::Point, pj))",
      version = 2L
    )
    expect_said(out, load = "^loaded$", document = "^error: .*version_older")
  }
)

scenario(
  "E10",
  "a saved value of a lower version of its type is refused as its document is",
  situation = "pa 2.0.0 raises the version of its type to 2. The user reads a value saved with saveRDS() at version 1.",
  risk = "The value is taken as one of version 2, and written as such.",
  result = "Every use of the value, and r8_update(), are errors that give both versions.",
  how = "installed",
  code = {
    out <- with_pa(
      "first",
      c(
        "say('read', p$x)",
        "say('use', pa::mag(p))",
        "say('write', R8::r8_to_json(p))",
        "say('update', R8::r8_update(p))"
      ),
      version = 2L
    )
    both <- "^error: .*the version of `pa::Point` was 1, and is 2"
    expect_said(out, read = "^3$", use = both, write = both, update = both)
  }
)

scenario(
  "E11",
  "a package is updated and changes the unit of a field",
  situation = "pa 2.0.0 gives a field per kilogram of body weight, and its author forgets to raise the version of the type. The field has the same name and the same bounds. The user reads a value that was saved before.",
  risk = "A dose of 500 mg is taken as 500 mg/kg. Every other check of the value passes.",
  result = "The value prints with a line that names both units. Every use of it, and r8_update(), are errors that name them.",
  how = "installed",
  code = {
    out <- with_pa(
      "unit",
      c(
        "say('print', format(p))",
        "say('use', pa::mag(p))",
        "say('argument', first(h, p))",
        "say('equal', R8::r8_equal(p, pa::Point(x = 3, y = 2)))",
        "say('hash', R8::r8_hash(p))",
        "say('holder', R8::r8_to_json(h))",
        "say('update', R8::r8_update(h))"
      )
    )
    unit <- "`x` was without a unit, and is in `mg/kg`"
    expect_said(
      out,
      print = unit,
      use = paste0("^error: .*", unit),
      argument = paste0("^error: .*", unit),
      equal = paste0("^error: .*", unit),
      hash = paste0("^error: .*", unit),
      holder = paste0("^error: .*", unit),
      update = paste0("^error: .*", unit)
    )
  }
)

scenario(
  "E12",
  "a document of an earlier unit is refused",
  situation = "As E11, for a JSON document that was written before.",
  risk = "A dose of 500 mg is read as 500 mg/kg.",
  result = "The document is refused: one that holds its declarations with both units, and one that does not by the hash of its declaration. A document that a person wrote, which names no declaration, is read.",
  how = "installed",
  code = {
    out <- with_pa(
      "unit",
      c(
        "say('full', R8::r8_from_json(pa::Point, pd))",
        "say('short', R8::r8_from_json(pa::Point, pj))",
        "say('holder', R8::r8_from_json(Holder, hd))",
        "say('hand', R8::r8_from_json(pa::Point, '{\"$type\": \"pa::Point\", \"$types\": {\"pa::Point\": 1}, \"x\": 1, \"y\": 2}')$x)"
      )
    )
    unit <- "^error: .*`x` was without a unit, and is in `mg/kg`"
    expect_said(
      out,
      full = unit,
      short = "^error: .*another declaration of `pa::Point`",
      holder = unit,
      hand = "^1$"
    )
  }
)

scenario(
  "E13",
  "a package is updated and rewords its type",
  situation = "pa 2.0.0 changes descriptions, the message of a rule, and the default of a field.",
  risk = "Values that a user saved are refused without need.",
  result = "The saved values and documents are used as before: none of these has a part in a value.",
  how = "installed",
  code = {
    first <- install_version(write_package(
      tempfile("src"),
      "pd",
      "P <- R8::new_struct('P', fields = list(a = R8::field(R8::type_double(), default = 1, description = 'A.'), b = R8::type_double()), rules = R8::rule(a <= b, message = 'a is at most b.', id = 'AB'))",
      "P"
    ))
    later <- install_version(write_package(
      tempfile("src"),
      "pd",
      "P <- R8::new_struct('P', fields = list(a = R8::field(R8::type_double(), default = 2, description = 'The lower end.'), b = R8::type_double()), rules = R8::rule(a <= b, message = 'The ends are in order.', id = 'ORDER'))",
      "P",
      version = "2.0.0"
    ))
    rds <- tempfile(fileext = ".rds")
    json <- said(
      first,
      c(
        sprintf("saveRDS(pd::P(b = 5), %s)", deparse(rds)),
        "say('json', R8::r8_to_json(pd::P(b = 5)))"
      )
    )[["json"]]
    out <- said(
      later,
      c(
        sprintf("x <- readRDS(%s)", deparse(rds)),
        "say('equal', R8::r8_equal(x, pd::P(a = 1, b = 5)))",
        sprintf(
          "say('document', R8::r8_equal(x, R8::r8_from_json(pd::P, %s)))",
          deparse(json)
        ),
        "say('write', { x$a <- 3; x$a })",
        "say('rule', { x$a <- 9 })"
      )
    )
    expect_said(
      out,
      equal = "^TRUE$",
      document = "^TRUE$",
      write = "^3$",
      rule = "^error: .*The ends are in order"
    )
  }
)

scenario(
  "E14",
  "a package is built for a newer version of another than the one installed",
  situation = "pb was built for pa 2.0.0, whose type has a field z, and the session has pa 1.0.0, since R does not check the versions that a package was built with.",
  risk = "pb reads a field that the type does not have.",
  result = "Building a value with the field, and reading the field, are errors that name it.",
  how = "installed",
  code = {
    first <- pa_first()
    newer <- pa_library("required")
    pc <- install_version(
      write_package(
        tempfile("src"),
        "pc",
        c(
          "make <- function() pa::Point(x = 1, y = 2, z = 3)",
          "read <- function(p) p$z"
        ),
        c("make", "read"),
        imports = "pa"
      ),
      libs = newer
    )
    out <- said(
      c(pc, first$pa),
      c(
        "say('load', { library(pc); 'loaded' })",
        "say('make', make())",
        "say('read', read(pa::Point(x = 1, y = 2)))"
      )
    )
    expect_said(
      out,
      load = "^loaded$",
      make = "^error: .*z",
      read = "^error: `z` is not a field of `pa::Point`. Fields: x, y"
    )
  }
)

scenario(
  "E15",
  "a package is updated while a session has it loaded",
  situation = "A user installs pa 2.0.0, with the fields in another order, into the library of a session that has pa 1.0.0 loaded, and goes on working. A worker that starts then loads pa 2.0.0.",
  risk = "The session mixes the two declarations.",
  result = "The session keeps the declaration that it loaded, and a new session has the new one. A value that crosses is read by name, and is used in the new session once r8_update() has converted it.",
  how = "installed",
  code = {
    lib <- pa_library("first", "1.0.0")
    path <- write_package(
      tempfile("src"),
      "pa",
      pa_code("reordered"),
      c("Point", "Kind", "mag"),
      version = "2.0.0"
    )
    rds <- tempfile(fileext = ".rds")
    out <- said(
      lib,
      c(
        "library(pa)",
        "p <- Point(x = 6, y = 2)",
        sprintf(
          "say('install', system2(file.path(R.home('bin'), 'R'), c('CMD', 'INSTALL', '--no-docs', '--library=%s', %s), stdout = FALSE, stderr = FALSE))",
          lib,
          deparse(path)
        ),
        "say('session', { q <- p; q$y <- 9; c(q$x, q$y, Point(x = 1, y = 2)$y) })",
        sprintf("saveRDS(p, %s)", deparse(rds)),
        sprintf(
          "say('new', system2(file.path(R.home('bin'), 'Rscript'), c('-e', shQuote(\"library(pa, lib.loc = '%s'); p <- readRDS('%s'); cat(p$x, p$y, names(Point(x = 1, y = 2)), mag(R8::r8_update(p)))\")), stdout = TRUE))",
          lib,
          rds
        )
      )
    )
    expect_said(
      out,
      install = "^0$",
      session = "^6 9 2$",
      new = "^6 2 y x 6$"
    )
  }
)

scenario(
  "E16",
  "three packages, of which the first is updated",
  situation = "pc uses a type of pb that has a field of a type of pa. pa 2.0.0 removes a field of its type; pb and pc are not installed again.",
  risk = "pc, which does not name pa, meets values of another shape.",
  result = "Checking a saved value of pb's type in pc, and reading its document, are errors that name the field of pa's type.",
  how = "installed",
  code = {
    first <- pa_first()
    pc <- install_version(
      write_package(
        tempfile("src"),
        "pc",
        c(
          "Box <- R8::new_struct('Box', fields = list(h = pb::Holder))",
          "check <- function(h) Box(h = h)"
        ),
        c("Box", "check"),
        imports = "pb"
      ),
      libs = c(first$pb, first$pa)
    )
    newer <- pa_library("removed")
    out <- said(
      c(pc, first$pb, newer),
      c(
        "say('load', { library(pc); 'loaded' })",
        sprintf("h <- readRDS(%s)", deparse(first$h_rds)),
        "say('check', check(h))",
        sprintf(
          "say('document', R8::r8_from_json(pb::Holder, readLines(%s)))",
          deparse(first$h_json)
        ),
        "say('new', check(pb::Holder(p = pa::Point(x = 1), label = 'a'))$h$p$x)"
      )
    )
    expect_said(
      out,
      load = "^loaded$",
      check = "^error: .*`h`.*`y` was removed \\(in `pa::Point`\\)",
      document = "^error: .*`p\\$y`.*unknown_field|^error: .*y.*unknown_field",
      new = "^1$"
    )
  }
)

scenario(
  "E17",
  "R8 is updated, and the packages are not installed again",
  situation = "A user updates R8. Packages with compiled code were built with the earlier R8.",
  risk = "R8 reads their tables and records in another layout, and crashes.",
  result = "A package whose compiled code was built for an earlier interface is refused when it loads, and asks to be installed again.",
  how = "installed",
  code = {
    lib <- install_test_packages()
    out <- run_r(
      lib,
      c(
        "suppressPackageStartupMessages(library(r8pkgR))",
        "says <- function(k) tryCatch({ R8:::r8_register_exports(r8pkgAbi:::table(k), 'r8pkgR', 'pkgr'); 'none' }, error = conditionMessage)",
        "cat('table', says(9), '\\n')",
        "cat('entries', says(10), '\\n')",
        "cat('after', magnitude(Signal(name = 'a', values = c(3, 4))), '\\n')"
      )
    )
    info <- paste(out, collapse = "\n")
    expect_true(any(grepl("^table .*reinstall the package", out)), info = info)
    expect_true(
      any(grepl("^entries .*the impl entries have size", out)),
      info = info
    )
    expect_true("after 5 " %in% out, info = info)
  }
)
