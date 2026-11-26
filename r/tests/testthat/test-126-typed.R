# test-126-typed.R
# ::R8::
# 2026- EDG rtemis.org

# Generics whose arguments and results have types, which each call checks.

# The message of the error that `expr` raises.
message_of <- function(expr) {
  conditionMessage(tryCatch(expr, error = identity))
}

test_that("a typed argument is checked and converted as in construction", {
  f <- declare_in("ty1", {
    A <- new_struct("A")
    f <- generic(
      "f",
      params(
        x,
        n = arg_integer(min = 0L, default = 1L),
        tol = arg_double(exclusive_min = 0, default = 1e-3),
        flag = arg_logical(default = FALSE),
        loss = arg_character(values = c("sq", "log"), default = "sq"),
        id = arg_character(pattern = "^[a-z]+$", default = "a")
      )
    )
    implement(f, A) <- function(x, n, tol, flag, loss, id) {
      list(n = n, tol = tol, flag = flag, loss = loss, id = id)
    }
    list(f = f, a = A())
  })
  a <- f[["a"]]
  f <- f[["f"]]
  expect_identical(
    f(a),
    list(n = 1L, tol = 1e-3, flag = FALSE, loss = "sq", id = "a")
  )
  # A whole double for an integer, an integer for a double, in either order.
  r <- f(a, tol = 2L, n = 10)
  expect_identical(r[["n"]], 10L)
  expect_identical(r[["tol"]], 2)
  expect_identical(
    f(a, n = 0L, flag = TRUE, loss = "log", id = "bc")[["loss"]],
    "log"
  )
  # Through the validator: a compact sequence of length one, a non-ASCII string.
  expect_identical(f(a, n = seq_len(1))[["n"]], 1L)

  refused <- function(expr, pattern) {
    expect_error(expr, pattern, class = "r8_argument_error")
  }
  refused(f(a, n = -1L), "`n` must be at least 0; found -1. \\[below_min\\]")
  refused(f(a, n = -1), "below_min")
  refused(f(a, n = 1.5), "`n`")
  refused(f(a, n = NA), "`n`")
  refused(f(a, n = NULL), "`n` must be a value of type integer")
  refused(f(a, n = 1:2), "r_length")
  refused(f(a, n = c(a = 1L)), "r_attributes")
  refused(f(a, n = "1"), "r_type")
  refused(f(a, tol = Inf), "`tol` must be")
  refused(f(a, tol = NA_real_), "na_value")
  refused(f(a, tol = Sys.Date()), "r_attributes")
  refused(f(a, flag = NA), "na_value")
  refused(f(a, flag = 1), "r_type")
  refused(f(a, loss = "hinge"), "`loss` must be one of \"sq\" or \"log\"")
  refused(f(a, id = "A1"), "`id`")
})

test_that("every problem of every argument is in one condition", {
  f <- declare_in("ty2", {
    A <- new_struct("A")
    f <- generic(
      "f",
      params(x, n = arg_integer(min = 0L), tol = arg_double(min = 0))
    )
    implement(f, A) <- function(x, n, tol) NULL
    list(f = f, a = A())
  })
  a <- f[["a"]]
  f <- f[["f"]]
  e <- tryCatch(f(a, n = -1L, tol = "a"), error = identity)
  expect_s3_class(
    e,
    c("r8_argument_error", "r8_value_error", "r8_error"),
    exact = FALSE
  )
  expect_identical(
    class(e)[1:3],
    c("r8_argument_error", "r8_value_error", "r8_error")
  )
  lines <- strsplit(conditionMessage(e), "\n")[[1L]]
  expect_identical(lines[[1L]], "Invalid arguments of `f()`: 2 problems")
  expect_identical(
    lines[[2L]],
    "- `n` must be at least 0; found -1. [below_min]"
  )
  expect_match(lines[[3L]], "^- `tol` must be")
  expect_identical(e[["problems"]][["path"]], c("/n", "/tol"))
  expect_match(message_of(f(a, n = -1L, tol = 1)), "f\\(\\)`: 1 problem\n")
})

test_that("a default that fails is named by its expression", {
  f <- declare_in("ty3", {
    A <- new_struct("A", fields = list(v = vec(type_double())))
    f <- generic(
      "f",
      params(
        x,
        n = arg_integer(min = 1L, default = length(x$v)),
        w = arg_vec(type_double(min = 0), default = x$v - 1)
      )
    )
    implement(f, A) <- function(x, n, w) n
    list(f = f, A = A)
  })
  A <- f[["A"]]
  f <- f[["f"]]
  expect_identical(f(A(v = c(1, 2))), 2L)
  expect_match(
    message_of(f(A(v = numeric()))),
    "`n`, its default `length(x$v)`, must be at least 1; found 0.",
    fixed = TRUE
  )
  expect_match(
    message_of(f(A(v = c(1, 0)))),
    "`w[[2]]`, of its default `x$v - 1`, must be at least 0; found -1.",
    fixed = TRUE
  )
  # An argument given is named alone.
  expect_match(
    message_of(f(A(v = c(1, 2)), n = 0L)),
    "- `n` must be at least 1; found 0.",
    fixed = TRUE
  )
})

test_that("a missing argument and an error while forcing are R's own", {
  f <- declare_in("ty4", {
    A <- new_struct("A")
    f <- generic("f", params(x, n = arg_integer(), lazy))
    implement(f, A) <- function(x, n, lazy) n
    list(f = f, a = A())
  })
  a <- f[["a"]]
  f <- f[["f"]]
  expect_error(f(a), "argument \"n\" is missing, with no default")
  expect_error(f(n = 1L), "argument \"x\" is missing, with no default")
  e <- tryCatch(f(a, n = stop("while forcing")), error = identity)
  expect_s3_class(e, "simpleError")
  expect_identical(conditionMessage(e), "while forcing")
  # An untyped argument stays lazy.
  expect_identical(f(a, 2L, stop("not forced")), 2L)
  g <- function(m) f(a, n = m)
  expect_error(g(), "argument \"m\" is missing, with no default")
})

test_that("typed and dispatched arguments are forced in the order of the formals", {
  f <- declare_in("ty5", {
    A <- new_struct("A")
    f <- generic(
      "f",
      params(a = arg_integer(), x, b = arg_integer(), y),
      dispatch = "x"
    )
    implement(f, A) <- function(a, x, b, y) c(a, b)
    list(f = f, a = A())
  })
  order <- character()
  say <- function(name, value) {
    order <<- c(order, name)
    value
  }
  expect_identical(
    f[["f"]](
      y = say("y", 1),
      b = say("b", 2L),
      x = say("x", f[["a"]]),
      a = say("a", 1L)
    ),
    c(1L, 2L)
  )
  expect_identical(order, c("a", "x", "b"))
})

test_that("R8 types, subtype(), and dyn() are checked exactly", {
  t <- declare_in("ty6", {
    Range <- new_struct(
      "Range",
      fields = list(lo = type_double(), hi = type_double())
    )
    Window <- new_struct(
      "Window",
      fields = list(label = type_character()),
      extends = Range
    )
    Other <- new_struct("Other")
    Planar <- new_trait(
      "Planar",
      methods = list(area = trait_method(params(x)))
    )
    Square <- new_struct("Square", fields = list(side = type_double()))
    Tile <- new_struct("Tile", extends = Square)
    impl(Planar, Square, methods = list(area = function(x) x$side^2))
    A <- new_struct("A")
    f <- generic(
      "f",
      params(
        x,
        exact = arg_option(Range),
        sub = arg_option(subtype(Range)),
        shape = arg_option(dyn(Planar))
      )
    )
    implement(f, A) <- function(x, exact, sub, shape) "ok"
    list(
      f = f,
      a = A(),
      Range = Range,
      Window = Window,
      Other = Other,
      Square = Square,
      Tile = Tile
    )
  })
  f <- t[["f"]]
  a <- t[["a"]]
  range <- t[["Range"]](lo = 0, hi = 1)
  window <- t[["Window"]](lo = 0, hi = 1, label = "w")
  refused <- function(expr, pattern) {
    expect_error(expr, pattern, class = "r8_argument_error")
  }
  expect_identical(f(a, exact = range), "ok")
  refused(f(a, exact = window), "r8_upcast")
  refused(f(a, exact = 1), "`exact` must be a value of `ty6::Range`")
  # Each twice: through the validator, then by the admitted type.
  for (i in 1:2) {
    expect_identical(f(a, sub = range), "ok")
    expect_identical(f(a, sub = window), "ok")
    expect_identical(f(a, shape = t[["Square"]](side = 1)), "ok")
    expect_identical(f(a, shape = t[["Tile"]](side = 1)), "ok")
    refused(f(a, sub = t[["Other"]]()), "not_subtype")
    refused(f(a, shape = t[["Other"]]()), "not_implementor")
    refused(f(a, shape = list(side = 1)), "`shape`")
  }
  expect_identical(f(a, NULL, NULL, NULL), "ok")
})

test_that("containers, matrices, tables, and host classes go to the validator", {
  t <- declare_in("ty7", {
    A <- new_struct("A")
    f <- generic(
      "f",
      params(
        x,
        v = arg_vec(type_double(min = 0), default = c(1, 2)),
        m = arg_matrix(type_double(), ncol = 2L, default = diag(2)),
        g = arg_factor(
          levels = c("a", "b"),
          default = factor("a", levels = c("a", "b"))
        ),
        d = arg_table(list(k = type_integer()), default = data.frame(k = 1L)),
        w = arg_btree_map(type_double(), default = c(a = 1)),
        fn = arg(host_class("function"), identity),
        df = arg(host_class("data.frame"), data.frame())
      )
    )
    implement(f, A) <- function(x, v, m, g, d, w, fn, df) {
      list(v = v, m = m, w = w, class = class(df))
    }
    list(f = f, a = A())
  })
  f <- t[["f"]]
  a <- t[["a"]]
  refused <- function(expr, pattern) {
    expect_error(expr, pattern, class = "r8_argument_error")
  }
  expect_identical(f(a)[["m"]], diag(2))
  # The stored form: an integer vector for doubles, a list for a map.
  expect_identical(f(a, v = 1:3)[["v"]], c(1, 2, 3))
  expect_identical(f(a, w = list(b = 2, a = 1))[["w"]], c(a = 1, b = 2))
  expect_identical(
    f(
      a,
      df = structure(data.frame(), class = c("tbl_df", "tbl", "data.frame"))
    )[["class"]],
    c("tbl_df", "tbl", "data.frame")
  )
  expect_no_error(f(a, fn = sum))
  refused(f(a, v = c(1, -1)), "`v\\[\\[2\\]\\]` must be at least 0")
  refused(f(a, m = diag(3)), "dimension_mismatch")
  refused(f(a, g = "a"), "`g`")
  refused(f(a, d = data.frame(k = "a")), "`d\\$k`")
  refused(
    f(a, fn = "sum"),
    "must be an object of class \"function\"; found a character vector"
  )
  refused(f(a, df = diag(2)), "must be an object of class \"data.frame\"")
})

test_that("a typed generic of several dispatched arguments checks, then dispatches", {
  t <- declare_in("ty8", {
    Range <- new_struct("Range", fields = list(lo = type_double()))
    Wide <- new_struct("Wide", extends = Range)
    Grid <- new_struct("Grid")
    snap <- generic(
      "snap",
      params(
        x = arg(subtype(Range)),
        n = arg_integer(default = 1L),
        grid,
        "..."
      ),
      dispatch = c("x", "grid")
    )
    implement(snap, Range, Grid) <- function(x, n, grid, ..., k = 0L) {
      paste("grid", n, k)
    }
    implement(snap, Wide) <- function(x, n, grid, ...) paste("wide", n)
    list(snap = snap, r = Range(lo = 0), w = Wide(lo = 0), g = Grid())
  })
  snap <- t[["snap"]]
  expect_identical(snap(t[["r"]], grid = t[["g"]], n = 2, k = 3L), "grid 2 3")
  expect_identical(snap(t[["w"]], 1L, 7), "wide 1")
  expect_error(snap(1, 1L, t[["g"]]), "`x`", class = "r8_argument_error")
  expect_error(snap(t[["r"]], 1L, 7), class = "r8_dispatch_error")
  expect_error(
    snap(t[["w"]], 1L, t[["g"]]),
    "dispatch_ambiguous",
    class = "r8_dispatch_error"
  )
})

test_that("a typed replacement generic checks its value", {
  t <- declare_in("ty9", {
    A <- new_struct("A", fields = list(n = type_integer()))
    `n_of<-` <- generic("n_of<-", params(x, value = arg_integer(min = 0L)))
    implement(`n_of<-`, A) <- function(x, value) {
      x$n <- value
      x
    }
    list(set = `n_of<-`, A = A)
  })
  `n_of<-` <- t[["set"]]
  a <- t[["A"]](n = 1L)
  n_of(a) <- 5
  expect_identical(a$n, 5L)
  expect_error(
    n_of(a) <- -1L,
    "Invalid arguments of `n_of<-\\(\\)`: 1 problem\n- `value` must be at least 0",
    class = "r8_argument_error"
  )
})

test_that("the contract belongs to the generic's call", {
  t <- declare_in("ty10", {
    A <- new_struct("A")
    B <- new_struct("B", extends = A)
    f <- generic(
      "f",
      params(x, n = arg_integer(min = 0L)),
      return_type = type_integer()
    )
    implement(f, A) <- function(x, n) "not an integer"
    implement(f, B) <- function(x, n) {
      n <- -5L
      inner <- r8_next_impl()
      nchar(inner) + n
    }
    list(f = f, A = A, B = B)
  })
  f <- t[["f"]]
  # r8_next_impl() passes a value outside the type and returns a result
  # outside the return type; the call's own result is checked.
  expect_identical(f(t[["B"]](), 1L), 9L)
  expect_identical(r8_method(f, t[["A"]])(t[["A"]](), -1L), "not an integer")
  expect_error(f(t[["A"]](), 1L), class = "r8_result_error")
})

test_that("a result is checked, converted, and keeps its visibility", {
  t <- declare_in("ty11", {
    A <- new_struct("A")
    f <- generic("f", params(x, value), return_type = type_integer(min = 0L))
    implement(f, A) <- function(x, value) value
    quiet <- generic("quiet", params(x), return_type = type_double())
    implement(quiet, A) <- function(x) invisible(1)
    both <- generic(
      "both",
      params(x, n = arg_integer()),
      return_type = type_double()
    )
    implement(both, A) <- function(x, n) n
    maybe <- generic("maybe", params(x), return_type = option(type_double()))
    implement(maybe, A) <- function(x) NULL
    list(f = f, quiet = quiet, both = both, maybe = maybe, a = A())
  })
  f <- t[["f"]]
  a <- t[["a"]]
  expect_identical(f(a, 3L), 3L)
  expect_identical(f(a, 3), 3L)
  expect_true(withVisible(f(a, 3L))[["visible"]])
  expect_identical(
    withVisible(t[["quiet"]](a)),
    list(value = 1, visible = FALSE)
  )
  expect_identical(t[["both"]](a, 2), 2)
  expect_null(t[["maybe"]](a))
  e <- tryCatch(f(a, -1L), error = identity)
  expect_identical(
    class(e)[1:3],
    c("r8_result_error", "r8_type_error", "r8_error")
  )
  expect_identical(
    conditionMessage(e),
    paste0(
      "Invalid result of `f()` from its method for `ty11::A`: 1 problem\n",
      "- The value must be at least 0; found -1. [below_min]"
    )
  )
  expect_error(
    f(a, NULL),
    "The value must be a value of type .*; found NULL",
    class = "r8_result_error"
  )
  expect_error(f(a, stop("in the method")), "in the method")
})

test_that("a method's own return type is the one checked", {
  t <- declare_in("ty12", {
    Model <- new_struct("Model", fields = list(rss = type_double()))
    Linear <- new_struct("Linear", extends = Model)
    Tree <- new_struct("Tree", extends = Model)
    A <- new_struct("A")
    B <- new_struct("B")
    C <- new_struct("C")
    Fit <- new_trait(
      "Fit",
      methods = list(
        fit = trait_method(
          params(x, out),
          default_impl = function(x, out) out,
          return_type = subtype(Model)
        )
      )
    )
    impl(
      Fit,
      A,
      methods = list(
        fit = impl_method(function(x, out) out, return_type = Linear)
      )
    )
    impl(Fit, B, methods = list(fit = function(x, out) out))
    impl(Fit, C)
    impl(
      Fit,
      "data.frame",
      methods = list(
        fit = impl_method(function(x, out) out, return_type = Tree)
      )
    )
    list(fit = Fit@fit, A = A, B = B, C = C, Linear = Linear, Tree = Tree)
  })
  fit <- t[["fit"]]
  linear <- t[["Linear"]](rss = 1)
  tree <- t[["Tree"]](rss = 1)
  expect_identical(fit(t[["A"]](), linear), linear)
  expect_error(
    fit(t[["A"]](), tree),
    "from its method for `ty12::A`.*`ty12::Linear`",
    class = "r8_result_error"
  )
  expect_identical(fit(t[["B"]](), tree), tree)
  expect_error(
    fit(t[["B"]](), 1),
    "from its method for `ty12::B`",
    class = "r8_result_error"
  )
  expect_identical(fit(data.frame(), tree), tree)
  expect_error(
    fit(data.frame(), linear),
    "from its method for `\"data.frame\"`",
    class = "r8_result_error"
  )
  expect_error(
    fit(t[["C"]](), 1),
    "from the trait's default implementation",
    class = "r8_result_error"
  )
})

test_that("Self is the type of the first dispatched argument", {
  t <- declare_in("ty13", {
    A <- new_struct("A", fields = list(n = type_integer()))
    B <- new_struct("B", extends = A)
    same <- generic("same", params(x, out), return_type = "Self")
    implement(same, A) <- function(x, out) out
    implement(same, "numeric") <- function(x, out) out
    implement(same, "name") <- function(x, out) out
    list(same = same, A = A, B = B)
  })
  same <- t[["same"]]
  a <- t[["A"]](n = 1L)
  b <- t[["B"]](n = 1L)
  expect_identical(same(a, a), a)
  expect_identical(same(b, b), b)
  expect_error(
    same(b, a),
    "must be a value of `ty13::B`, the type of the first dispatched argument; found a value of `ty13::A`. \\[return_type\\]",
    class = "r8_result_error"
  )
  expect_identical(same(1, 2), 2)
  expect_error(
    same(1, "a"),
    "found an object of class \"character\"",
    class = "r8_result_error"
  )
  # A symbol is a value.
  expect_identical(same(quote(not_bound), quote(other)), quote(other))
  expect_error(same(quote(not_bound), 1), class = "r8_result_error")
})

test_that("a declaration made again replaces the signature", {
  t <- declare_in("ty14", {
    A <- new_struct("A")
    f <- generic("f", params(x, n = arg_integer(max = 5L)))
    implement(f, A) <- function(x, n) n
    list(f = f, A = A)
  })
  f <- t[["f"]]
  a <- t[["A"]]()
  expect_identical(f(a, 5L), 5L)
  expect_error(f(a, 6L), class = "r8_argument_error")
  again <- function() {
    declare_in("ty14", generic("f", params(x, n = arg_integer(max = 9L))))
  }
  invisible(again())
  expect_identical(f(a, 6L), 6L)
  # While a call forces its arguments.
  wider <- function() {
    declare_in("ty14", generic("f", params(x, n = arg_integer(max = 99L))))
    50L
  }
  expect_identical(f(a, wider()), 50L)
  fewer <- function() {
    declare_in("ty14", generic("f", params(x, n)))
    "untyped"
  }
  expect_identical(f(a, fewer()), "untyped")
  # A result type declared while the method runs.
  g <- declare_in("ty14", {
    A <- t[["A"]]
    g <- generic("g", params(x, k), return_type = type_integer())
    implement(g, A) <- function(x, k) k()
    g
  })
  widen <- function() {
    declare_in(
      "ty14",
      generic("g", params(x, k), return_type = type_character())
    )
    "a string"
  }
  expect_identical(g(a, widen), "a string")
})

test_that("a typed generic of many parameters is checked", {
  n <- 60L
  nms <- paste0("p", seq_len(n))
  p <- do.call(
    params,
    c(
      list(quote(x)),
      stats::setNames(
        lapply(seq_len(n), function(i) {
          bquote(arg_integer(min = .(i), default = .(i)))
        }),
        nms
      )
    )
  )
  f <- declare_in("ty15", {
    A <- new_struct("A")
    f <- generic("f", p)
    implement(f, A) <- as.function(c(
      stats::setNames(rep(list(quote(expr = )), n + 1L), c("x", nms)),
      as.call(c(list(as.name("sum")), lapply(nms, as.name)))
    ))
    list(f = f, a = A())
  })
  expect_identical(f[["f"]](f[["a"]]), sum(seq_len(n)))
  expect_error(
    f[["f"]](f[["a"]], p60 = 1L),
    "`p60` must be at least 60",
    class = "r8_argument_error"
  )
})

test_that("a method sees the argument as given or converted", {
  t <- declare_in("ty16", {
    A <- new_struct("A")
    f <- generic("f", params(x, n = arg_integer(default = 1L), "..."))
    implement(f, A) <- function(x, n, ...) {
      list(
        missing = missing(n),
        n = n,
        dots = list(...),
        parent = parent.frame()
      )
    }
    list(f = f, a = A())
  })
  f <- t[["f"]]
  a <- t[["a"]]
  expect_false(f(a)[["missing"]])
  expect_false(f(a, 2)[["missing"]])
  expect_identical(do.call(f, list(a, n = 3))[["n"]], 3L)
  expect_identical((a |> f(n = 4L, k = 2))[["dots"]], list(k = 2))
  # The caller's variable keeps its own value.
  n <- 7
  f(a, n)
  expect_identical(n, 7)
})

test_that("a field of a host class holds an object of the class only", {
  t <- declare_in("ty17", {
    S <- new_struct(
      "S",
      fields = list(
        fit = option(host_class("lm")),
        fn = option(host_class("function")),
        m = option(host_class("matrix")),
        num = option(host_class("numeric")),
        sym = option(host_class("name")),
        call = option(host_class("call")),
        cond = option(host_class("if"))
      )
    )
    S
  })
  fit <- stats::lm(dist ~ speed, datasets::cars)
  expect_s3_class(t(fit = fit)$fit, "lm")
  expect_no_error(t(fit = stats::glm(dist ~ speed, data = datasets::cars)))
  expect_no_error(t(
    fn = sum,
    m = diag(2),
    num = 1L,
    sym = quote(not_bound),
    call = quote(stop("evaluated")),
    cond = quote(if (a) b)
  ))
  refused <- function(expr, pattern) {
    expect_error(expr, pattern, class = "r8_value_error")
  }
  refused(
    t(fit = 1),
    "`fit` must be an object of class \"lm\"; found a numeric vector. \\[r_type\\]"
  )
  refused(t(fit = data.frame()), "found an object of class \"data.frame\"")
  refused(t(fn = "sum"), "\"function\"")
  refused(t(m = array(1, c(1, 1, 1))), "\"matrix\"")
  refused(t(num = "1"), "\"numeric\"")
  refused(t(call = quote(x)), "\"call\"")
})

test_that("a class of an S4 object's superclasses is its class", {
  skip_if_not_installed("Matrix")
  t <- declare_in("ty18", {
    A <- new_struct("A")
    f <- generic(
      "f",
      params(
        x,
        m = arg(host_class("Matrix", package = "Matrix", system = "s4"))
      )
    )
    implement(f, A) <- function(x, m) "ok"
    list(f = f, a = A())
  })
  m <- Matrix::Matrix(c(1, 0, 0, 1), 2, sparse = TRUE)
  expect_identical(t[["f"]](t[["a"]], m), "ok")
  expect_error(
    t[["f"]](t[["a"]], diag(2)),
    "\"Matrix\"",
    class = "r8_argument_error"
  )
})

test_that("a parameter's name is not one of R8's", {
  expect_error(
    params(x, .r8_v = 1),
    "reserved",
    class = "r8_declaration_error"
  )
  expect_error(params(x, .r8_f), "reserved", class = "r8_declaration_error")
})

test_that("a From impl's result of another type is a result error", {
  t <- declare_in("ty19", {
    A <- new_struct("A")
    B <- new_struct("B")
    impl(From("numeric"), A, methods = list(from = function(x) B()))
    A
  })
  expect_error(r8_convert(1, t), "From", class = "r8_result_error")
})

test_that("print() of a generic and of a trait shows types, defaults, and return types", {
  t <- declare_in("ty20", {
    Model <- new_struct("Model")
    Linear <- new_struct("Linear", extends = Model)
    A <- new_struct("A")
    B <- new_struct("B")
    train <- generic(
      "train",
      params(
        x,
        y,
        n = arg_integer(min = 0L, default = 1),
        loss = arg_character(values = c("sq", "log")),
        w = arg_option(vec(type_double(min = 0))),
        k = length(x),
        "..."
      ),
      return_type = subtype(Model)
    )
    implement(train, A, return_type = Linear) <- function(
      x,
      y,
      n,
      loss,
      w,
      k,
      ...
    ) {
      Linear()
    }
    implement(train, B) <- function(x, y, n, loss, w, k, ...) Model()
    plain <- generic("plain", params(x, y))
    same <- generic("same", params(x, y), return_type = "Self")
    Fit <- new_trait(
      "Fit",
      methods = list(
        fit = trait_method(
          params(x, n = arg_double(default = 1)),
          return_type = Model
        ),
        note = trait_method(params(x))
      )
    )
    list(train = train, plain = plain, same = same, Fit = Fit)
  })
  expect_identical(
    utils::capture.output(print(t[["train"]])),
    c(
      "<r8 generic ty20::train>",
      "train(x, y, n, loss, w, k, ...)",
      "  n: integer [0, Inf) = 1L",
      "  loss: character {\"sq\", \"log\"}",
      "  w: optional vector of double [0, Inf) = NULL",
      "  k = length(x)",
      "  returns subtype of ty20::Model",
      "Methods for: ty20::A (returns ty20::Linear), ty20::B"
    )
  )
  expect_identical(
    utils::capture.output(print(t[["plain"]])),
    c("<r8 generic ty20::plain>", "plain(x, y)")
  )
  expect_identical(
    utils::capture.output(print(t[["same"]]))[[3L]],
    "  returns the type of x"
  )
  expect_identical(
    utils::capture.output(print(t[["Fit"]])),
    c(
      "<r8 trait ty20::Fit>",
      "  fit(x, n)",
      "    n: double = 1",
      "    returns ty20::Model",
      "  note(x)"
    )
  )
})
