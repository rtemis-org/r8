# helper-pa.R
# ::R8::
# 2026- EDG rtemis.org

# The packages of the scenarios across versions. `pa` declares `Point`, `Kind`,
# and the generic `mag`; `pb`, built for the first version of `pa`, has a struct
# with a field of each, a generic with an argument of `Point`, and a method of
# `mag`.

# The declaration of `Point` in each version of `pa`, as the user of a package
# meets it: what its NEWS would say.
PA_FIELDS <- c(
  first = "x = R8::type_double(min = 0), y = R8::type_double()",
  reordered = "y = R8::type_double(), x = R8::type_double(min = 0)",
  optional = "x = R8::type_double(min = 0), y = R8::type_double(), z = R8::option(R8::type_double())",
  required = "x = R8::type_double(min = 0), y = R8::type_double(), z = R8::type_double()",
  removed = "x = R8::type_double(min = 0)",
  tightened = "x = R8::type_double(min = 5), y = R8::type_double()",
  unit = "x = R8::field(R8::type_double(min = 0), unit = 'mg/kg'), y = R8::type_double()",
  retyped = "x = R8::type_character(), y = R8::type_double()"
)

pa_code <- function(change, version = 1L, kinds = c("A", "B")) {
  c(
    sprintf(
      "Point <- R8::new_struct('Point', fields = list(%s), version = %dL)",
      PA_FIELDS[[change]],
      version
    ),
    sprintf(
      "Kind <- R8::new_enum('Kind', variants = list(%s))",
      paste(kinds, "= R8::variant()", collapse = ", ")
    ),
    "mag <- R8::generic('mag', R8::params(x))",
    "R8::implement(mag, Point) <- function(x) abs(x$x)"
  )
}

# A library with `pa` at a version whose `Point` has the change `change`.
pa_library <- function(change, package_version = "2.0.0", ...) {
  install_version(write_package(
    tempfile("src"),
    "pa",
    pa_code(change, ...),
    c("Point", "Kind", "mag"),
    version = package_version
  ))
}

# A library with `pb`, built for the `pa` of the library `pa`.
pb_library <- function(pa) {
  install_version(
    write_package(
      tempfile("src"),
      "pb",
      c(
        "Holder <- R8::new_struct('Holder', fields = list(p = pa::Point, label = R8::type_character(), kind = R8::field(pa::Kind, default = pa::Kind@A)))",
        "first <- R8::generic('first', R8::params(h, p = R8::arg(pa::Point)), return_type = R8::type_double())",
        "R8::implement(first, Holder) <- function(h, p) p$x + h$p$x",
        "R8::implement(mag, Holder) <- function(x) 100 + pa::mag(x$p)"
      ),
      c("Holder", "first"),
      imports = "pa"
    ),
    libs = pa
  )
}

# The first version of both, installed once per run, and the values and
# documents that a user saved with them: the paths `p`, `h`, `k` (RDS), and
# `pj` and `hj` (documents).
.pa_first <- new.env(parent = emptyenv())

pa_first <- function() {
  if (is.null(.pa_first$pa)) {
    .pa_first$pa <- pa_library("first", "1.0.0")
    .pa_first$pb <- pb_library(.pa_first$pa)
    dir <- tempfile("saved")
    dir.create(dir)
    for (f in c("p.rds", "h.rds", "p.json", "h.json")) {
      assign(sub("[.]", "_", f), file.path(dir, f), envir = .pa_first)
    }
    out <- said(
      c(.pa_first$pb, .pa_first$pa),
      c(
        "library(pb)",
        "p <- pa::Point(x = 3, y = 2)",
        "h <- Holder(p = p, label = 'a')",
        sprintf("saveRDS(p, %s)", deparse(.pa_first$p_rds)),
        sprintf("saveRDS(h, %s)", deparse(.pa_first$h_rds)),
        sprintf("writeLines(R8::r8_to_json(p), %s)", deparse(.pa_first$p_json)),
        sprintf("writeLines(R8::r8_to_json(h), %s)", deparse(.pa_first$h_json)),
        "say('saved', TRUE)"
      )
    )
    if (!identical(unname(out["saved"]), "TRUE")) {
      stop(attr(out, "output"))
    }
  }
  .pa_first
}

# Runs `code` with `pb` as built for the first `pa`, and `pa` at a version with
# the change `change`, after reading what the user saved: `p` and `h`, and the
# documents `pj` and `hj`.
with_pa <- function(change, code, ...) {
  first <- pa_first()
  said(
    c(first$pb, pa_library(change, ...)),
    c(
      "say('load', { library(pb); 'loaded' })",
      sprintf("p <- readRDS(%s)", deparse(first$p_rds)),
      sprintf("h <- readRDS(%s)", deparse(first$h_rds)),
      sprintf("pj <- readLines(%s)", deparse(first$p_json)),
      sprintf("hj <- readLines(%s)", deparse(first$h_json)),
      code
    )
  )
}
