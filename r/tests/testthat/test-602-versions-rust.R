# test-602-versions-rust.R
# ::R8::
# 2026- EDG rtemis.org

# Scenarios of updates of a package with Rust code, whose types the Rust code of
# another package uses.
#
# The crate of pb depends on the crate of pa, so pb holds its own compiled copy
# of pa's type. A user who updates pa, and not pb, has two copies that differ.
# Each test builds and installs both, changes pa, and installs pa alone.

# The Rust code of `pa`: `fields` declares the fields of its Point, and `rules`
# the attributes of its rules.
pa_rust <- function(fields, rules = character()) {
  c(
    "use rtemis_r8::R8;",
    "use rtemis_r8_api as r8;",
    "/// A point.",
    "#[derive(R8, Clone)]",
    "#[r8(namespace = \"pa\", version = 1, storage = \"rust\")]",
    rules,
    sprintf("pub struct Point { %s }", fields),
    "/// The x of a point.",
    "#[r8::methods(trait = \"pa::own_x\")]",
    "pub trait OwnX { fn own_x(&self) -> f64; }",
    "impl OwnX for Point { fn own_x(&self) -> f64 { self.x } }",
    "r8::exports! { package = \"pa\", namespace = \"pa\", types = [Point], traits = [OwnX], impls = [Point: OwnX] }"
  )
}

# The Rust code of `pb`, which reads a Point as an argument and in a field.
PB_RUST <- c(
  "use rtemis_r8::R8;",
  "use rtemis_r8_api as r8;",
  "use pa::Point;",
  "/// A holder of a point.",
  "#[derive(R8, Clone)]",
  "#[r8(namespace = \"pb\", version = 1, storage = \"rust\")]",
  "pub struct Holder { pub by: f64, pub p: Option<Point> }",
  "/// The x of a point, scaled.",
  "#[r8::methods(trait = \"pb::scaled\")]",
  "pub trait Scaled { fn scaled(&self, p: &Point) -> f64; }",
  "impl Scaled for Holder { fn scaled(&self, p: &Point) -> f64 { self.by * p.x } }",
  "/// The weight.",
  "#[r8::methods(trait = \"pb::weight\")]",
  "pub trait Weight { fn weight(&self) -> f64; }",
  "impl Weight for Holder { fn weight(&self) -> f64 { self.by } }",
  "r8::exports! { package = \"pb\", namespace = \"pb\", types = [Holder], traits = [Scaled, Weight], impls = [Holder: Scaled, Holder: Weight] }"
)

# A library with `pa` whose Rust code is `rust`, and the path of its source.
pa_rust_library <- function(rust) {
  path <- write_package(
    tempfile("src"),
    "pa",
    character(),
    c("Point", "own_x")
  )
  write_rust(path, rust)
  generate_types(path)
  list(lib = install_version(path), path = path)
}

# The first version of both, built once per run.
.rust_first <- new.env(parent = emptyenv())

rust_first <- function() {
  skip_unless_packages()
  skip_if_not_installed("pkgbuild")
  if (is.null(.rust_first$pb)) {
    pa <- pa_rust_library(pa_rust("#[r8(min = 0)] pub x: f64, pub y: f64"))
    path <- write_package(
      tempfile("src"),
      "pb",
      character(),
      c("Holder", "scaled", "weight"),
      imports = "pa"
    )
    write_rust(
      path,
      PB_RUST,
      c(pa = file.path(pa$path, "src", "rust"))
    )
    generate_types(path, pa$lib)
    .rust_first$pb <- install_version(path, libs = pa$lib)
    .rust_first$pa <- pa$lib
  }
  .rust_first
}

# What a session with the first pb says of a Point of `pa`, its library.
rust_session <- function(pa) {
  said(
    c(rust_first()$pb, pa),
    c(
      "say('load', { library(pb); 'loaded' })",
      "suppressWarnings(library(pb))",
      "p <- pa::Point(x = 6, y = 2)",
      "h <- Holder(by = 10, p = NULL)",
      "say('own', pa::own_x(p))",
      "say('argument', scaled(h, p))",
      "say('field', Holder(by = 1, p = p)$by)",
      "say('other', weight(h))"
    )
  )
}

scenario(
  "E18",
  "Rust code reads a type of another package, both current",
  situation = "The Rust code of pb takes a value of a type in the Rust code of pa. Both packages are those that were built together.",
  risk = "The code reads memory that another package laid out.",
  result = "The value is read by its fields, and every call gives the number that the source says.",
  how = "installed",
  code = {
    expect_said(
      rust_session(rust_first()$pa),
      load = "^loaded$",
      own = "^6$",
      argument = "^60$",
      field = "^1$",
      other = "^10$"
    )
  }
)

scenario(
  "E19",
  "a package with Rust code is updated and reorders the fields of a type",
  situation = "pa 2.0.0 lists the field y before x in its Rust code, and is installed. pb, compiled against pa 1.0.0, is not installed again.",
  risk = "The Rust code of pb reads y where it means x: the answer is wrong, without an error.",
  result = "The fields are found by name, and the answer is right.",
  how = "installed",
  code = {
    rust_first()
    pa <- pa_rust_library(pa_rust("pub y: f64, #[r8(min = 0)] pub x: f64"))
    expect_said(
      rust_session(pa$lib),
      load = "^loaded$",
      own = "^6$",
      argument = "^60$",
      field = "^1$"
    )
  }
)

scenario(
  "E20",
  "a package with Rust code is updated and adds a field, a bound, and a rule",
  situation = "pa 2.0.0 adds an optional field to its type, a bound to y, and a rule between x and y. pb is not installed again.",
  risk = "pb is refused although every value that it meets is one that it was written for.",
  result = "pb works as before.",
  how = "installed",
  code = {
    rust_first()
    pa <- pa_rust_library(pa_rust(
      "pub z: Option<f64>, #[r8(min = 0)] pub x: f64, #[r8(min = 0)] pub y: f64",
      "#[r8(rule(id = \"order\", invariant = r#\"{\">=\": [{\"var\": \"x\"}, {\"var\": \"y\"}]}\"#))]"
    ))
    expect_said(
      rust_session(pa$lib),
      load = "^loaded$",
      argument = "^60$",
      field = "^1$"
    )
  }
)

scenario(
  "E21",
  "a package with Rust code is updated and drops a bound",
  situation = "pa 2.0.0 drops the lower bound of x. The Rust code of pb was compiled when x was at least 0, and may divide by it or index with it.",
  risk = "The code meets a value that it was never written for.",
  result = "pb loads with a warning that names the bound. Its functions that read the type are errors, and the others work.",
  how = "installed",
  code = {
    rust_first()
    pa <- pa_rust_library(pa_rust("pub x: f64, pub y: f64"))
    expect_said(
      rust_session(pa$lib),
      load = "^warning: .*'pb' 1.0.0 was compiled against another declaration of `pa::Point` of 'pa' 1.0.0.*`x` is compiled as double \\[0, Inf\\), and declared as double",
      own = "^6$",
      argument = "^error: .*declaration_changed",
      field = "^error: .*declaration_changed",
      other = "^10$"
    )
  }
)

scenario(
  "E22",
  "a package with Rust code is updated and changes a unit",
  situation = "pa 2.0.0 declares x in millimeters, where pa 1.0.0 gave no unit.",
  risk = "pb takes the number as it did before.",
  result = "pb loads with a warning that names the unit, and its functions that read the type are errors.",
  how = "installed",
  code = {
    rust_first()
    pa <- pa_rust_library(pa_rust(
      "#[r8(min = 0, unit = \"mm\")] pub x: f64, pub y: f64"
    ))
    expect_said(
      rust_session(pa$lib),
      load = "^warning: .*`x` is compiled without a unit, and declared in `mm`",
      argument = "^error: .*declaration_changed",
      other = "^10$"
    )
  }
)

scenario(
  "E23",
  "a saved value of Rust is read after its package changed the type",
  situation = "A user saves a value whose fields Rust holds. pa 2.0.0 reorders the fields, at the same version of the type, and the user reads the value.",
  risk = "The Rust code takes one field for another: the numbers are wrong.",
  result = "The fields read by their names, without pa too. The Rust code of pa 2.0.0 is not given the value, and r8update() makes one that it takes.",
  how = "installed",
  code = {
    first <- rust_first()
    rds <- tempfile(fileext = ".rds")
    said(
      first$pa,
      sprintf("saveRDS(pa::Point(x = 6, y = 2), %s)", deparse(rds))
    )
    pa <- pa_rust_library(pa_rust("pub y: f64, #[r8(min = 0)] pub x: f64"))
    code <- c(
      sprintf("p <- readRDS(%s)", deparse(rds)),
      "say('alone', c(p$x, p$y))",
      "library(pa)",
      "say('read', c(p$x, p$y))",
      "say('validate', R8::r8validate(p)$x)",
      "say('set', {q <- p; q$x <- 1; q$x})",
      "say('update', {u <- R8::r8update(p); c(u$x, u$y, R8::r8validate(u)$x)})"
    )
    expect_said(
      said(first$pa, code),
      alone = "^6 2$",
      read = "^6 2$",
      validate = "^6$",
      set = "^1$"
    )
    expect_said(
      said(pa$lib, code),
      alone = "^6 2$",
      read = "^6 2$",
      validate = "^error: ",
      set = "^error: ",
      update = "^6 2 6$"
    )
  }
)

scenario(
  "E24",
  "Rust code reads a changed type of another package only where that is safe",
  situation = "The Rust code of pb was compiled when a type of pa had other fields or bounds. pa declares the type again, as it does under load_all().",
  risk = "The code reads another field than it means, or a value that its checks never admitted.",
  result = "Fields are found by name. A change that admits no new value is read; any other is an error that names it, and a warning when pb loads.",
  how = "installed",
  code = {
    lib <- install_test_packages()
    # r8pkgS reads a Signal of r8pkgR as an argument (peak), a Resampler in a
    # nested field, and a Square, which no field and no signature of it names
    # (side, downcast_ref). r8pkgR declares them again as it does under load_all().
    out <- run_r(
      lib,
      c(
        "suppressPackageStartupMessages({ library(R8); library(r8pkgR); library(r8pkgS) })",
        "say <- function(...) cat(..., '\\n')",
        "says <- function(e) tryCatch({ force(e); 'read' }, error = function(e) gsub('\n', ' ', conditionMessage(e)))",
        "of <- function(...) {",
        "  f <- list(name = type_character(), values = vec(type_double()))",
        "  new <- list(...)",
        "  f[names(new)] <- new",
        "  f",
        "}",
        "declare <- function(call) invisible(eval(call, asNamespace('r8pkgR')))",
        "signal <- function(fields, rules = list(), version = 1L) {",
        "  declare(bquote(R8::new_struct('Signal', fields = .(fields), rules = .(rules), version = .(version), storage = 'rust')))",
        "}",
        "x <- Signal(name = 'a', values = c(1, 5))",
        "grid <- function(...) Grid(cell = Interval(lo = 0, hi = 1), extent = Interval(lo = 0, hi = 1), ...)",
        "g <- grid(method = Resampler@Loocv, figure = Square(side = 3))",
        "say('compiled', peak(g, x), side(g))",
        "# An R-stored value of the registered declaration.",
        "value <- function(type, ..., variant = '') .Call('wrap__r8_any_value', list(...), type, variant, '', PACKAGE = 'R8')",
        "s <- function(...) value('pkgr::Signal', ...)",
        "a <- 'a'; v <- c(1, 5)",
        "say('r_stored', peak(g, s(name = a, values = v)))",
        "# Refinements of the compiled declaration.",
        "signal(of(note = option(type_character()), seed = type_integer()))",
        "say('added', says(peak(g, s(name = a, values = v, note = NULL, seed = 1L))))",
        "say('earlier', says(peak(g, x)))",
        "signal(rev(of()))",
        "say('reordered', peak(g, s(values = v, name = a)))",
        "signal(of(name = type_character(pattern = '^a'), values = vec(type_double(min = 0), min_length = 1)), list(rule(name != 'b')))",
        "say('tightened', says(peak(g, s(name = a, values = v))))",
        "# Others.",
        "signal(of(values = vec(type_double(infinite = TRUE, nan = TRUE))))",
        "say('loosened', says(peak(g, s(name = a, values = v))))",
        "signal(of(values = field(vec(type_double()), unit = 'cm')))",
        "say('unit', says(peak(g, s(name = a, values = v))))",
        "signal(of()['values'])",
        "say('field', says(peak(g, s(values = v))))",
        "signal(of(), version = 2L)",
        "say('version', says(peak(g, s(name = a, values = v))))",
        "# The declaration that the code was compiled against admits the reads again.",
        "signal(of())",
        "say('again', says(peak(g, x)))",
        "# A variant that the enum adds is refused in its values alone.",
        "method <- Resampler@Bootstrap(n = 2L)",
        "declare(quote(R8::new_enum('Resampler', variants = list(KFold = variant(fields = list(k = type_integer(), seed = type_integer())), Bootstrap = variant(fields = list(n = type_integer())), Fixed = variant(fields = list(train = vec(type_integer()))), Custom = variant(of = Split), Loocv = variant(), Nested = variant()), storage = 'rust')))",
        "say('earlier variant', says(grid(method = method)))",
        "say('variant', class(grid(method = value('pkgr::Resampler', n = 2L, variant = 'Bootstrap')))[1])",
        "# A value of a type that is declared again is not built by its earlier code.",
        "e <- tryCatch(Resampler@Bootstrap(n = 2L), error = conditionMessage)",
        "say('own', e)",
        "kept <- grid(method = value('pkgr::Resampler', variant = 'Loocv'), figure = Square(side = 3))",
        "square <- function(type) declare(bquote(R8::new_struct('Square', fields = list(color = type_character(), side = .(type), name = type_character()), storage = 'rust')))",
        "square(type_double(min = 0))",
        "say('body earlier', says(side(kept)))",
        "held <- function() grid(method = value('pkgr::Resampler', variant = 'Loocv'), figure = value('pkgr::Square', color = 'red', side = 3, name = 'b'))",
        "say('body added', says(side(held())))",
        "square(type_double())",
        "say('body loosened', says(side(held())))",
        "w <- tryCatch(r8register('r8pkgS'), warning = identity)",
        "say('quiet', is.numeric(w))",
        "signal(of(name = option(type_character())))",
        "w <- tryCatch(r8register('r8pkgS'), warning = identity)",
        "say('class', class(w)[1])",
        "cat(conditionMessage(w), '\\n')"
      )
    )
    info <- paste(out, collapse = "\n")
    expect_true("compiled 5 3 " %in% out, info = info)
    expect_true("r_stored 5 " %in% out, info = info)
    expect_true(
      any(grepl("^earlier .*`seed` was added without a default", out)),
      info = info
    )
    expect_true("added read " %in% out, info = info)
    expect_true("reordered 5 " %in% out, info = info)
    expect_true("tightened read " %in% out, info = info)
    expect_true(
      any(grepl(
        "^loosened .*'r8pkgS' was compiled against another declaration of `pkgr::Signal` than the registered one: `values` is compiled as vector of double, and declared as vector of .*Install versions of 'r8pkgS' and of package 'r8pkgR' that were built for each other \\[declaration_changed\\]",
        out
      )),
      info = info
    )
    expect_true(
      any(grepl(
        "^unit .*`values` is compiled without a unit, and declared in `cm`",
        out
      )),
      info = info
    )
    expect_true(
      any(grepl("^field .*`name` is compiled, and not declared", out)),
      info = info
    )
    expect_true(
      any(grepl("^version .*is compiled as 1, and declared as 2", out)),
      info = info
    )
    expect_true("again read " %in% out, info = info)
    expect_true("variant pkgs::Grid " %in% out, info = info)
    expect_true(
      any(grepl(
        "^own `pkgr::Resampler` was declared again after its package",
        out
      )),
      info = info
    )
    expect_true(
      any(grepl("^earlier variant .*the variant `Nested` was added", out)),
      info = info
    )
    expect_true(
      any(grepl("^body earlier .*`color` was added without a default", out)),
      info = info
    )
    expect_true("body added read " %in% out, info = info)
    expect_true(
      any(grepl(
        "^body loosened .*`side` is compiled as double \\[0, Inf\\), and declared as double",
        out
      )),
      info = info
    )
    expect_true("quiet TRUE " %in% out, info = info)
    expect_true("class r8_declaration_warning " %in% out, info = info)
    expect_true(
      "- `name` is compiled as character, and declared as optional character" %in%
        out,
      info = info
    )
  }
)
