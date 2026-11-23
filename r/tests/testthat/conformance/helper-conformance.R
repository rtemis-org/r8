# helper-conformance.R
# ::R8::
# 2026- EDG rtemis.org

# The provider of the conformance tests: the type `Conform`, the impls of
# R8's standard traits for it, and the kinds of test the run supports, chosen
# by `getOption("r8.conformance")`. "native" takes `pkgr::Conform`, declared
# and implemented in Rust in the test package r8pkgR; any other value
# declares the same type, with impls of the same results, in R in the
# namespace `conf`. test-conformance.R runs this directory with the R
# provider, and test-packages.R with the Rust-stored one.
# spec: R8/class-system#conformance-tests

# Evaluates `expr` with the option `r8.namespace` set to `ns`, outside any
# package namespace, with the caller's variables visible.
declare_in <- function(ns, expr) {
  op <- options(r8.namespace = ns)
  on.exit(options(op))
  env <- list2env(
    as.list(parent.frame()),
    envir = new.env(parent = globalenv())
  )
  eval(substitute(expr), envir = env)
}

# `pkgr::Conform` declared in R, in the namespace `conf`: the declaration
# that r8_document_rust() writes for r8pkgR, without `storage = "rust"`; and
# `conf::Every`, a field of each type that has no Rust field type. Its
# impls give the results of r8pkgR's Rust impls.
conformance_r_types <- function() {
  declare_in("conf", {
    Conform <- new_struct(
      "Conform",
      fields = list(
        ratio = field(
          type_double(min = 0, max = 1),
          description = "A fraction."
        ),
        count = field(
          type_integer(min = 1L),
          default = 10L,
          description = "A count."
        ),
        flag = field(type_logical(), description = "A flag."),
        label = field(
          type_character(pattern = "^[a-z]+$"),
          description = "A lowercase label."
        ),
        seed = field(option(type_integer()), description = "An optional seed."),
        weights = field(vec(type_double()), description = "Weights."),
        index = field(vec(type_integer()), description = "Case indices."),
        bytes = field(type_raw(), description = "Bytes."),
        mask = field(vec(type_logical()), description = "A mask.")
      ),
      description = "A field of every Rust field type, with constraints, a default, and rules.",
      version = 1L,
      rules = list(
        rule(
          seed < count,
          when = json_ir('{"!==":[{"var":"seed"},null]}'),
          message = "The seed is below the count.",
          id = "seed_below_count"
        ),
        rule(
          json_ir(
            '{"===":[{"length":{"var":"weights"}},{"length":{"var":"index"}}]}'
          ),
          message = "There is one weight per index.",
          id = "weights_match_index"
        )
      )
    )
    impl(
      Display,
      Conform,
      methods = list(format = function(x, ...) {
        sprintf("Conform %s (%d)", x$label, x$count)
      })
    )
    impl(
      PartialOrd,
      Conform,
      methods = list(partial_cmp = function(x, y) {
        if (!inherits(y, "conf::Conform")) {
          return(NA_integer_)
        }
        as.integer(sign(x$ratio - y$ratio))
      })
    )
    impl(
      Shape,
      Conform,
      methods = list(dim = function(x) c(length(x$index), 2L))
    )
    Every <- new_struct(
      "Every",
      fields = list(
        level = type_factor(levels = c("lo", "hi")),
        day = type_date(),
        at = type_posixct(),
        m = type_matrix(type_double()),
        tb = type_table(list(id = type_character())),
        map = btree_map(type_double()),
        doc = type_json()
      )
    )
    list(Conform = Conform, Every = Every)
  })
}

# The type, its namespace, the kinds of test the run supports, and the code
# a worker runs before it reads a value.
conformance_provider <- function() {
  if (identical(getOption("r8.conformance"), "native")) {
    return(list(
      Conform = getExportedValue("r8pkgR", "Conform"),
      namespace = "pkgr",
      supports = "workers",
      worker_setup = "library(r8pkgR)"
    ))
  }
  types <- conformance_r_types()
  list(
    Conform = types[["Conform"]],
    Every = types[["Every"]],
    namespace = "conf",
    supports = "every_field_type"
  )
}

# Skips a test that needs a kind the provider does not support.
skip_unless <- function(p, kind) {
  if (!kind %in% p[["supports"]]) {
    testthat::skip(paste0("the provider does not support `", kind, "`"))
  }
}

# The arguments of a valid `Conform`; an argument given as NULL is omitted.
conform_args <- function(...) {
  utils::modifyList(
    list(
      ratio = 0.5,
      count = 3L,
      flag = TRUE,
      label = "abc",
      seed = 1L,
      weights = c(0.5, 1.5),
      index = c(1L, 2L),
      bytes = as.raw(1:3),
      mask = c(TRUE, FALSE)
    ),
    list(...)
  )
}

# A `Conform` value of the provider.
conform_value <- function(p, ...) {
  do.call(p[["Conform"]], conform_args(...))
}
