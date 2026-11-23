# test-generic.R
# ::R8::
# 2026- EDG rtemis.org

# The R-familiar layer: generic(), implement<-, and r8_method().
# spec: R8/class-system#r-familiar-layer

gbm_types <- function(ns) {
  declare_in(ns, {
    GBM <- new_struct("GBM", fields = list(n_trees = type_integer(min = 1L)))
    Kernel <- new_enum(
      "Kernel",
      variants = list(
        Linear = variant(),
        Rbf = variant(fields = list(gamma = type_double()))
      )
    )
    list(GBM = GBM, Kernel = Kernel)
  })
}

test_that("generic() declares a generic and implement() its methods", {
  ty <- gbm_types("gen1")
  GBM <- ty[["GBM"]]
  Kernel <- ty[["Kernel"]]
  out <- declare_in("gen1", {
    describe <- generic("describe", params(x), description = "Describes x.")
    d0 <- describe
    implement(describe, GBM) <- function(x) paste(x$n_trees, "trees")
    implement(describe, Kernel) <- function(x) "a kernel"
    list(describe = describe, unchanged = identical(describe, d0))
  })
  describe <- out[["describe"]]
  expect_true(out[["unchanged"]])
  expect_s3_class(describe, "r8_generic")
  expect_identical(names(formals(describe)), "x")
  expect_identical(attr(describe, "r8_trait"), "gen1::describe")
  expect_identical(describe(GBM(n_trees = 5L)), "5 trees")
  expect_identical(describe(Kernel@Rbf(gamma = 0.1)), "a kernel")
  expect_identical(describe(Kernel@Linear), "a kernel")
  expect_error(describe(1), "does not implement", class = "r8_dispatch_error")
  ir <- unclass(R8:::.r8_traits[["gen1::describe"]])[["ir"]]
  expect_true(ir[["generic"]])
  expect_identical(ir[["methods"]][[1L]][["name"]], "describe")
  expect_identical(unlist(ir[["methods"]][[1L]][["params"]]), "x")
  expect_identical(r8_method(describe, GBM)(GBM(n_trees = 2L)), "2 trees")
  expect_output(print(describe), "<r8 generic gen1::describe>")
  expect_output(print(describe), "Methods for: gen1::GBM, gen1::Kernel")
})

test_that("generic() takes its arguments as given", {
  g <- declare_in("gen2", generic("score", params(model, data)))
  expect_identical(names(formals(g)), c("model", "data"))
  g2 <- declare_in("gen2", generic("rescore", params(model, "...", data)))
  expect_identical(names(formals(g2)), c("model", "...", "data"))
})

test_that("generic() refuses names and arguments outside the rules", {
  for (bad in c("describe.gbm", "describe.x<-", "1st", "")) {
    expect_error(
      declare_in("gen3", generic(bad, params(x))),
      "rule of trait names",
      class = "r8_declaration_error"
    )
  }
  expect_error(
    declare_in("gen3", generic("default", params(x))),
    "name_host_keyword",
    class = "r8_declaration_error"
  )
  expect_error(
    declare_in("gen3", generic("g", params("..."))),
    "first parameter is the value dispatched on",
    class = "r8_declaration_error"
  )
  expect_error(
    declare_in("gen3", generic("g", params(x, x))),
    "`x` is declared twice",
    class = "r8_declaration_error"
  )
  expect_error(
    declare_in("gen3", generic("summary", params(object))),
    "implement\\(summary, Type\\)",
    class = "r8_declaration_error"
  )
  expect_error(
    declare_in("gen3", generic("nrow", params(x))),
    "name the generic otherwise",
    class = "r8_declaration_error"
  )
})

test_that("a generic shares the namespace's names with types and trait methods", {
  declare_in("gen4", {
    new_struct("describe", fields = list(a = type_double()))
    new_trait(
      "Explain",
      methods = list(explain = trait_method(params(x)))
    )
  })
  expect_error(
    declare_in("gen4", generic("describe", params(x))),
    "duplicate_type",
    class = "r8_declaration_error"
  )
  expect_error(
    declare_in("gen4", generic("explain", params(x))),
    "duplicate_generic",
    class = "r8_declaration_error"
  )
})

test_that("implement() lowers format, str, dim, and r8_partial_cmp to standard traits", {
  GBM <- gbm_types("gen5")[["GBM"]]
  declare_in("gen5", {
    implement(format, GBM) <- function(x, ...) paste0("GBM(", x$n_trees, ")")
    implement(str, GBM) <- function(object, ...) cat("GBM str\n")
    implement(dim, GBM) <- function(x) c(x$n_trees, 2L)
    implement(r8_partial_cmp, GBM) <- function(x, y) sign(x$n_trees - y$n_trees)
  })
  g <- GBM(n_trees = 3L)
  expect_identical(format(g), "GBM(3)")
  expect_output(print(g), "^GBM\\(3\\)$")
  expect_output(str(g), "GBM str")
  expect_identical(nrow(g), 3L)
  expect_true(g < GBM(n_trees = 4L))
  expect_identical(r8_method(format, GBM)(g), "GBM(3)")
})

test_that("implement(print) is a Print impl, stated in a message", {
  GBM <- gbm_types("gen6")[["GBM"]]
  expect_message(
    declare_in("gen6", {
      implement(print, GBM) <- function(x, ...) {
        cat("A GBM of", x$n_trees, "trees\n")
      }
    }),
    "implements R8's `Print` trait"
  )
  g <- GBM(n_trees = 7L)
  expect_output(out <- withVisible(print(g)), "^A GBM of 7 trees$")
  expect_false(out[["visible"]])
  expect_identical(out[["value"]], g)
  expect_identical(format(g)[[1L]], "<gen6::GBM>")
  expect_true(is.function(r8_method(print, GBM)))
})

test_that("implement() on another S3 generic registers a method for the qualified class", {
  GBM <- gbm_types("gen7")[["GBM"]]
  declare_in("gen7", {
    implement(summary, GBM) <- function(object, ...) object$n_trees * 2L
  })
  expect_identical(summary(GBM(n_trees = 4L)), 8L)
  expect_error(
    declare_in("gen7", {
      implement(summary, GBM) <- function(x, ...) NULL
    }),
    "function\\(object, \\.\\.\\.\\)",
    class = "r8_declaration_error"
  )
  expect_error(
    declare_in("gen7b", {
      implement(summary, GBM) <- function(object, ...) NULL
    }),
    "orphan rule",
    class = "r8_declaration_error"
  )
})

test_that("implement() registers methods of an S3 generic that R8's namespace cannot reach", {
  GBM <- gbm_types("gen8")[["GBM"]]
  # A generic in an environment of its own, as a generic of a package R8 does
  # not import is in that package's namespace.
  home <- new.env(parent = globalenv())
  evalq(tidy_up <- function(x, ...) UseMethod("tidy_up"), home)
  tidy_up <- home$tidy_up
  on.exit(
    rm(
      list = "tidy_up.gen8::GBM",
      envir = globalenv()[[".__S3MethodsTable__."]]
    ),
    add = TRUE
  )
  declare_in("gen8", {
    implement(tidy_up, GBM) <- function(x, ...) x$n_trees + 1L
  })
  expect_identical(tidy_up(GBM(n_trees = 9L)), 10L)
  expect_identical(r8_method(tidy_up, GBM)(GBM(n_trees = 1L)), 2L)
})

test_that("implement() takes the method of a trait", {
  GBM <- gbm_types("gen9")[["GBM"]]
  fit <- declare_in("gen9", {
    Fit <- new_trait(
      "Fit",
      methods = list(fit = trait_method(params(spec, x)))
    )
    fit <- Fit@fit
    implement(fit, GBM) <- function(spec, x) sum(x) + spec$n_trees
    fit
  })
  expect_identical(fit(GBM(n_trees = 1L), 1:3), 7L)
})

test_that("implement() refuses the functions R8 owns, operators, and non-generics", {
  GBM <- gbm_types("gen10")[["GBM"]]
  refuse <- function(expr, pattern) {
    expect_error(
      eval.parent(substitute(expr)),
      pattern,
      class = "r8_declaration_error"
    )
  }
  env <- new.env(parent = globalenv())
  env$GBM <- GBM
  try_impl <- function(g) {
    env$g <- g
    evalq(implement(g, GBM) <- function(x, ...) NULL, env)
  }
  refuse(try_impl(`$`), "reads the fields")
  refuse(try_impl(`[[<-`), "validates every change")
  refuse(try_impl(`==`), "r8_equal")
  refuse(try_impl(`<`), "implement\\(r8_partial_cmp, GBM\\)")
  refuse(try_impl(`+`), "operators")
  refuse(try_impl(Ops), "operators")
  refuse(try_impl(nrow), "implement\\(dim, GBM\\)")
  refuse(try_impl(r8_convert), "implement\\(as_target, GBM\\)")
  refuse(try_impl(r8_converter), "r8_converter\\(Target\\)")
  refuse(try_impl(function(x) x), "not a generic")
  refuse(try_impl(methods::show), "S4 generic")
  refuse(try_impl(1), "takes a generic")
})


test_that("a method has the generic's parameters, and adds its own only after ...", {
  ty <- gbm_types("gen9")
  GBM <- ty[["GBM"]]
  declare_in("gen9", {
    exact <- generic("exact", params(x))
    expect_error(
      implement(exact, GBM) <- function(x, ...) NULL,
      "takes exactly the generic's parameters: function(x)",
      fixed = TRUE,
      class = "r8_declaration_error"
    )
    expect_error(
      implement(exact, GBM) <- function(x, digits) NULL,
      "takes exactly",
      class = "r8_declaration_error"
    )
    implement(exact, GBM) <- function(x) x$n_trees
    x <- GBM(n_trees = 3L)
    expect_identical(exact(x), 3L)
    # An argument the generic does not declare is an error at the call.
    expect_error(exact(x, digits = 2))
    open <- generic("open_ended", params(x, "..."))
    expect_error(
      implement(open, GBM) <- function(x, digits = 1) NULL,
      "`...` included",
      class = "r8_declaration_error"
    )
    implement(open, GBM) <- function(x, ..., digits = 1) x$n_trees * digits
    expect_identical(open(x, digits = 2L), 6L)
    # impl() checks a method's arguments as implement() does.
    Size <- new_trait(
      "Size",
      methods = list(size = trait_method(params(x)))
    )
    expect_error(
      impl(Size, GBM, methods = list(size = function(x, ...) 1L)),
      "gen9::Size@size",
      class = "r8_declaration_error"
    )
    expect_error(
      trait_method(params(x), default_impl = function(x, y) NULL),
      "the default",
      class = "r8_declaration_error"
    )
  })
})
