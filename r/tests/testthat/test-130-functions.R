# test-130-functions.R
# ::R8::
# 2026- EDG rtemis.org

# Typed functions: new_function(), the check of arguments and results in the
# function's own frame, rules over parameters, and the declaration.
#
# spec: R8/class-system#typed-functions

test_that("a typed function checks and converts its arguments", {
  clip <- declare_in("fn1", {
    new_function(
      "clip",
      function(
        x = arg_vec(type_double()),
        lo = arg_double(),
        hi = arg_double(default = 1)
      ) {
        pmin(pmax(x, lo), hi)
      }
    )
  })
  expect_true(inherits(clip, "r8_function"))
  expect_identical(names(formals(clip)), c("x", "lo", "hi"))
  expect_identical(clip(c(-1, 0.5, 3), lo = 0L), c(0, 0.5, 1))
  e <- expect_error(clip("a", lo = "b"), class = "r8_argument_error")
  expect_match(
    conditionMessage(e),
    "Invalid arguments of `clip()`: 2 problems",
    fixed = TRUE
  )
  expect_match(conditionMessage(e), "`lo` must be", fixed = TRUE)
  expect_error(clip(1), "lo", class = "error")
  # A whole double is the integer of an integer parameter.
  n_of <- declare_in("fn1", {
    new_function("n_of", function(n = arg_integer(min = 1L)) n)
  })
  expect_identical(n_of(2), 2L)
  expect_error(n_of(0L), "must be at least 1", class = "r8_argument_error")
  # A default is named where it fails.
  first <- declare_in("fn1", {
    new_function(
      "first",
      function(
        x = arg_vec(type_double()),
        n = arg_integer(min = 1L, default = length(x))
      ) {
        x[seq_len(n)]
      }
    )
  })
  expect_identical(first(c(3, 4)), c(3, 4))
  expect_error(
    first(numeric()),
    "`n`, its default `length(x)`, must be at least 1",
    fixed = TRUE,
    class = "r8_argument_error"
  )
})

test_that("the body runs in the function's own frame", {
  fit <- declare_in("fn2", {
    new_function(
      "fit",
      function(
        data = arg(host_class("data.frame")),
        y = arg_character(),
        n = arg_integer(default = 3L),
        ...
      ) {
        list(
          label = deparse(substitute(data)),
          call = match.call(),
          sys = sys.call(),
          missing_n = missing(n),
          caller = parent.frame(),
          nargs = nargs(),
          dots = list(...)
        )
      }
    )
  })
  visits <- data.frame(a = 1)
  out <- fit(visits, "a", extra = 2)
  expect_identical(out$label, "visits")
  expect_identical(out$call, quote(fit(data = visits, y = "a", extra = 2)))
  expect_identical(out$sys, quote(fit(visits, "a", extra = 2)))
  expect_true(out$missing_n)
  expect_identical(out$caller, environment())
  expect_identical(out$nargs, 3L)
  expect_identical(out$dots, list(extra = 2))
  seen <- new.env()
  done <- declare_in("fn2", {
    new_function("done", function(x = arg_double()) {
      on.exit(assign("ran", TRUE, envir = seen))
      x
    })
  })
  expect_identical(done(1), 1)
  expect_true(seen$ran)
})

test_that("a return type checks each return() and the last value", {
  pick <- declare_in("fn3", {
    new_function(
      "pick",
      function(x = arg_double()) {
        inner <- function() return("inner")
        if (x > 10) {
          return(invisible(10L))
        }
        if (x < 0) {
          return("negative")
        }
        q <- quote(return("quoted"))
        if (identical(inner(), "inner")) x * 2
      },
      return_type = type_double()
    )
  })
  expect_identical(pick(1), 2)
  out <- withVisible(pick(20))
  expect_identical(out, list(value = 10, visible = FALSE))
  e <- expect_error(pick(-1), class = "r8_result_error")
  expect_match(
    conditionMessage(e),
    "Invalid result of `pick()`: 1 problem",
    fixed = TRUE
  )
  none <- declare_in("fn3", {
    new_function(
      "none",
      function(x = arg_double()) NULL,
      return_type = type_double()
    )
  })
  expect_error(none(1), class = "r8_result_error")
})

test_that("the check of a result wraps the value, and the body runs as written", {
  run <- declare_in("fn_value", {
    new_function(
      "run",
      function(x = arg_vec(type_double())) {
        y <- numeric(length(x))
        for (i in seq_along(x)) {
          y[[i]] <- 2 * x[[i]]
        }
        y
      },
      return_type = vec(type_double())
    )
  })
  expect_identical(run(c(1, 2)), c(2, 4))
  # After the check of the arguments, the body's statements, then the check of
  # its last value alone.
  b <- body(run)[[3L]]
  expect_identical(b[[1L]], as.name("{"))
  expect_identical(b[[2L]], quote(y <- numeric(length(x))))
  expect_identical(b[[3L]][[1L]], as.name("for"))
  expect_identical(b[[4L]][[2L]], quote(.r8_v <- withVisible(y)))
  branch <- declare_in("fn_value", {
    new_function(
      "branch",
      function(x = arg_double()) {
        if (x > 0) {
          invisible(x)
        } else if (x < -5) {
          "low"
        } else {
          x * 2
        }
      },
      return_type = type_double()
    )
  })
  expect_identical(withVisible(branch(1)), list(value = 1, visible = FALSE))
  expect_identical(withVisible(branch(-1)), list(value = -2, visible = TRUE))
  expect_error(branch(-6), class = "r8_result_error")
})

test_that("rules hold over the typed parameters", {
  span <- declare_in("fn4", {
    new_function(
      "span",
      function(
        lo = arg_double(),
        hi = arg_double(),
        w = arg_option(type_double())
      ) {
        hi - lo
      },
      rules = list(
        order = rule(lo <= hi, message = "lo is at most hi."),
        rule(w > 0)
      )
    )
  })
  expect_identical(span(1, 3), 2)
  expect_identical(span(1, 3, w = 1), 2)
  e <- expect_error(span(3, 1, w = -1), class = "r8_argument_error")
  text <- conditionMessage(e)
  expect_match(text, "2 problems", fixed = TRUE)
  expect_match(text, "lo is at most hi.", fixed = TRUE)
  expect_match(text, "found lo = 3, hi = 1", fixed = TRUE)
  expect_error(
    declare_in("fn4", {
      new_function("bad", function(x = arg_double(), y) x, rules = rule(x < y))
    }),
    "rule_field",
    class = "r8_declaration_error"
  )
})

test_that("typed = \"strict\" asks a type of every parameter", {
  expect_error(
    declare_in("fn5", {
      new_function("s", function(x, n = arg_integer(), ...) n, typed = "strict")
    }),
    "parameter `x` of the strictly typed function `s` has no type",
    fixed = TRUE,
    class = "r8_declaration_error"
  )
  s <- declare_in("fn5", {
    new_function("s", function(n = arg_integer()) n, typed = "strict")
  })
  expect_identical(s(1), 1L)
  plain <- declare_in("fn5", new_function("plain", function(x, y = 2) x + y))
  expect_identical(plain(1), 3)
  none <- declare_in("fn5", new_function("none", function() 1))
  expect_identical(none(), 1)
})

test_that("a declaration refuses what is no typed function", {
  refuses <- function(expr, text) {
    expect_error(expr, text, fixed = TRUE, class = "r8_declaration_error")
  }
  refuses(new_function("2a", function(x) x), "R8 name")
  refuses(new_function("f", sum), "`fn` is a function")
  refuses(new_function("f", "x"), "`fn` is a function")
  refuses(new_function("f", function(x) x, typed = "all"), "`typed` is")
  refuses(
    new_function("f", function(x) x, return_type = "Self"),
    "A return type is a field type"
  )
  refuses(
    new_function("f", function(n = arg_integer(min = 1L, default = 0L)) n),
    "default"
  )
  refuses(
    new_function("f", function(x = arg_vec(arg_double())) x),
    "declares a parameter"
  )
  declare_in("fn6", new_struct("Taken", fields = list(a = type_double())))
  refuses(
    declare_in("fn6", new_function("Taken", function(x) x)),
    "duplicate_function"
  )
  declare_in("fn6", generic("shared", params(x)))
  refuses(
    declare_in("fn6", new_function("shared", function(x) x)),
    "duplicate_function"
  )
})

test_that("a typed function prints its parameters and registers its record", {
  clip <- declare_in("fn7", {
    new_function(
      "clip",
      function(x = arg_vec(type_double()), lo = arg_double(default = 0)) x,
      rules = list(order = rule(lo <= 1)),
      return_type = vec(type_double())
    )
  })
  expect_output(
    print(clip),
    paste(
      "<r8 function fn7::clip>",
      "clip(x, lo)",
      "  x: vector of double",
      "  lo: double = 0",
      "  returns vector of double",
      "  rule order: lo <= 1",
      sep = "\n"
    ),
    fixed = TRUE
  )
  ir <- r8_function_ir("fn7::clip")
  expect_identical(ir[["name"]], "clip")
  expect_identical(length(ir[["params"]]), 2L)
  # Outside a package, the function carries its record, which a process that
  # has not registered it registers at the first call.
  expect_true(is.raw(attr(clip, "r8_record")))
})

test_that("a typed function registers its record in another process", {
  # The other process loads the installed R8.
  skip_if_not(nzchar(Sys.getenv("R8_PACKAGE_TESTS")))
  clip <- declare_in("fn7", {
    new_function("clip", function(x = arg_vec(type_double())) x)
  })
  bytes <- serialize(clip, NULL)
  out <- run_r(
    character(),
    c(
      sprintf(
        "f <- unserialize(as.raw(c(%s)))",
        paste0("0x", as.character(bytes), collapse = ",")
      ),
      "cat(f(c(1, 2)), class(tryCatch(f('a'), error = identity))[[1L]], '\\n')"
    )
  )
  expect_identical(
    trimws(out),
    "1 2 r8_argument_error",
    info = paste(out, collapse = "\n")
  )
})


test_that("a generic and a trait method check rules over their parameters", {
  out <- declare_in("fn8", {
    S <- new_struct("S", fields = list(a = type_double()))
    span <- generic(
      "span",
      params(x, lo = arg_double(), hi = arg_double()),
      rules = list(order = rule(lo <= hi, message = "lo is at most hi."))
    )
    implement(span, S) <- function(x, lo, hi) x$a * (hi - lo)
    Fit <- new_trait(
      "Fit",
      methods = list(
        fit = trait_method(
          params(x, n = arg_integer(), most = arg_integer()),
          rules = rule(n <= most)
        )
      )
    )
    impl(Fit, S, methods = list(fit = function(x, n, most) n))
    list(S = S, span = span, fit = Fit@fit)
  })
  s <- out$S(a = 2)
  expect_identical(out$span(s, 1, 3), 4)
  e <- expect_error(out$span(s, 3, 1), class = "r8_argument_error")
  expect_match(conditionMessage(e), "lo is at most hi.", fixed = TRUE)
  expect_identical(out$fit(s, 1L, 2L), 1L)
  expect_error(out$fit(s, 3L, 2L), "n <= most", class = "r8_argument_error")
  expect_error(
    declare_in("fn8", {
      generic("bad", params(x, lo = arg_double(), hi), rules = rule(lo < hi))
    }),
    "rule_field",
    class = "r8_declaration_error"
  )
})

test_that("a rule's diagnosis writes each value once, and no empty value", {
  f <- declare_in("fn_diag", {
    new_function(
      "f",
      function(a = arg_vec(type_double()), b = arg_vec(type_double())) 1,
      rules = list(r1 = rule(length(a) == length(b)))
    )
  })
  e <- expect_error(f(c(1, 2), 1), class = "r8_argument_error")
  expect_match(
    conditionMessage(e),
    "found length(a) = 2, length(b) = 1. [rule_violation]",
    fixed = TRUE
  )
  g <- declare_in("fn_diag", {
    new_function(
      "g",
      function(a = arg_factor(), b = arg_factor()) 1,
      rules = list(lv = rule(levels(a) == levels(b)))
    )
  })
  e <- expect_error(g(factor("x"), factor("y")), class = "r8_argument_error")
  expect_false(grepl("= ,", conditionMessage(e), fixed = TRUE))
})


test_that("a return() inside local() or evalq() returns from that call", {
  f <- declare_in("fn_local", {
    new_function(
      "f",
      function(x = arg_double()) {
        y <- local({
          return("a")
        })
        z <- evalq(return(2), new.env())
        x + z
      },
      return_type = type_double()
    )
  })
  expect_identical(f(1), 3)
})

test_that("a default that its type converts is not missing in the body", {
  # Binding the converted value clears R's missing flag of the parameter.
  f <- declare_in("fn_missing", {
    new_function(
      "f",
      function(n = arg_integer(default = length(letters) / 2)) missing(n)
    )
  })
  expect_false(f())
  g <- declare_in("fn_missing", {
    new_function("g", function(n = arg_integer(default = 3L)) missing(n))
  })
  expect_true(g())
})

test_that("a body of rust() is the whole body, in a package", {
  # Outside a package, no Rust code holds the body.
  expect_error(
    declare_in("fn_rust", {
      new_function("f", function(x = arg_double()) rust())
    }),
    "is declared outside a package",
    class = "r8_declaration_error"
  )
  expect_error(
    declare_in("fn_rust", {
      new_function("g", function(x = arg_double()) {
        y <- x + 1
        R8::rust()
      })
    }),
    "rust() is the whole body",
    fixed = TRUE,
    class = "r8_declaration_error"
  )
  # A refused declaration leaves no function.
  expect_null(r8_function_ir("fn_rust::f"))
  expect_null(r8_function_ir("fn_rust::g"))
  expect_error(rust(), "called by itself", class = "r8_declaration_error")
  # A function of another package of the name `rust`, and rust() in quoted code,
  # are R code of the body.
  own <- declare_in("fn_rust", {
    rust <- function(x) x * 2
    list(
      calls = new_function("calls", function(x = arg_double()) rust(x)),
      quoted = new_function("quoted", function(x = arg_double()) {
        list(x, quote(R8::rust()))
      })
    )
  })
  expect_identical(own$calls(2), 4)
  expect_identical(own$quoted(1)[[2L]], quote(R8::rust()))
  expect_false(rust_body(function(x) x))
  expect_true(rust_body(function(x) rust()))
  expect_true(rust_body(function(x) {
    R8::rust()
  }))
})

test_that("r8rust_signature() writes the Rust function of a declaration", {
  level <- declare_in("fn_signature", {
    new_function(
      "level",
      function(
        x = arg_vec(type_double()),
        lo = arg_double(default = 0),
        hi = arg_double(),
        na.rm = arg_logical(default = FALSE),
        data = NULL,
        ...
      ) {
        x
      },
      rules = list(rule(lo <= hi)),
      return_type = vec(type_double()),
      description = "Holds a running sum between `lo` and `hi`."
    )
  })
  text <- r8rust_signature(level)
  expect_s3_class(text, "r8_rust_text")
  lines <- strsplit(text, "\n", fixed = TRUE)[[1L]]
  expect_identical(
    lines[[1L]],
    "/// Holds a running sum between `lo` and `hi`."
  )
  expect_true("/// - lo <= hi" %in% lines)
  expect_true(
    "/// `data` and `...` are R values, so this body runs in R alone." %in%
      lines
  )
  expect_identical(
    lines[match("#[r8::function]", lines) + 0:9],
    c(
      "#[r8::function]",
      "fn level(",
      "    x: &[f64],",
      "    lo: f64,",
      "    hi: f64,",
      "    #[r8(name = \"na.rm\")] na_rm: bool,",
      "    data: r8::Sexp,",
      "    dots: r8::Dots,",
      ") -> Vec<f64> {",
      "    todo!()"
    )
  )
  expect_output(print(text), "fn level(", fixed = TRUE)
  expect_error(r8rust_signature(sum), class = "r8_type_error")
})
