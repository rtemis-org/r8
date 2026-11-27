# test-125-params.R
# ::R8::
# 2026- EDG rtemis.org

# The parameters of generics and trait methods, with their types and defaults:
# params(), arg() and its shorthands, return types, and impl_method().

# The IR of the one method of the trait of a generic.
method_ir <- function(g) {
  R8:::r8_trait_ir(attr(g, "r8_trait"))[["methods"]][[1L]]
}

test_that("params() reads names, types, defaults, and dots", {
  p <- params(
    x,
    y,
    n = arg_integer(min = 0L, default = 1L),
    m = length(x),
    "..."
  )
  expect_s3_class(p, "r8_params")
  expect_identical(p[["names"]], c("x", "y", "n", "m", "..."))
  expect_identical(p[["formals"]][["n"]], 1L)
  expect_identical(p[["formals"]][["m"]], quote(length(x)))
  expect_identical(p[["formals"]][["x"]], quote(expr = ))
  expect_identical(p[["types"]][[3L]], type_integer(min = 0L))
  expect_null(p[["types"]][[4L]])
  expect_output(print(p), "m = length(x)", fixed = TRUE)

  # Nothing but the types is evaluated.
  expect_no_error(params(
    x,
    n = stop("evaluated"),
    k = arg_double(default = stop("evaluated"))
  ))
})

test_that("the IR of typed parameters holds types, JSON defaults, and host defaults", {
  g <- declare_in("par1", {
    generic(
      "fit",
      params(
        x,
        tol = arg_double(min = 0, default = 1e-3),
        n = arg_integer(default = length(x)),
        ids = arg_vec(type_integer(), default = c(1L, -2L)),
        weights = arg_option(vec(type_double())),
        label = arg(option(type_character())),
        seed = 1L,
        "..."
      )
    )
  })
  ir <- method_ir(g)[["params"]]
  expect_identical(ir[[1L]], "x")
  expect_identical(ir[[2L]][["type"]][["min"]], 0)
  expect_identical(ir[[2L]][["default"]], 0.001)
  expect_identical(
    ir[[3L]][["host_default"]],
    list(host = "r", text = "length(x)")
  )
  expect_identical(ir[[4L]][["default"]], list(1L, -2L))
  # A `NULL` default is a member of the IR; a required parameter has none.
  expect_true("default" %in% names(ir[[5L]]))
  expect_null(ir[[5L]][["default"]])
  expect_false("default" %in% names(ir[[6L]]))
  expect_identical(
    ir[[7L]],
    list(
      name = "seed",
      host_default = list(host = "r", text = "1L")
    )
  )
  expect_identical(ir[[8L]], "...")
  # A constant default is the formal's value; any other stays an expression.
  expect_identical(
    formals(g)[["ids"]],
    c(1L, -2L)
  )
  formals(g)[["ids"]] <- quote(ids_value)
  expect_identical(
    formals(g),
    as.pairlist(alist(
      x = ,
      tol = 1e-3,
      n = length(x),
      ids = ids_value,
      weights = NULL,
      label = ,
      seed = 1L,
      ... =
    ))
  )
})

test_that("every type constructor has a shorthand with its arguments", {
  shorthand <- c(
    arg_logical = "type_logical",
    arg_integer = "type_integer",
    arg_double = "type_double",
    arg_character = "type_character",
    arg_raw = "type_raw",
    arg_date = "type_date",
    arg_posixct = "type_posixct",
    arg_factor = "type_factor",
    arg_json = "type_json",
    arg_matrix = "type_matrix",
    arg_array = "type_array",
    arg_sparse_matrix = "type_sparse_matrix",
    arg_table = "type_table",
    arg_vec = "vec",
    arg_option = "option",
    arg_btree_map = "btree_map",
    arg_index_map = "index_map"
  )
  types <- grep("^type_", getNamespaceExports("R8"), value = TRUE)
  expect_setequal(
    setdiff(shorthand, c("vec", "option", "btree_map", "index_map")),
    types
  )
  for (n in names(shorthand)) {
    f <- get(n, envir = asNamespace("R8"))
    constructor <- get(shorthand[[n]], envir = asNamespace("R8"))
    expect_identical(R8:::arg_constructor(f), shorthand[[n]])
    expect_identical(
      names(formals(f)),
      c(names(formals(constructor)), "default", "description")
    )
  }
  expect_identical(R8:::arg_constructor(arg), "arg")
  expect_null(R8:::arg_constructor(type_double))
  expect_null(R8:::arg_constructor(sum))
})

test_that("arg() is recognized as a function, under any name", {
  integer_arg <- arg_integer
  p <- params(x, a = R8::arg_double(min = 0), b = integer_arg(max = 3L))
  expect_identical(p[["types"]][[2L]], type_double(min = 0))
  expect_identical(p[["types"]][[3L]], type_integer(max = 3L))
  # Another function of the same name declares no type.
  local({
    arg_double <- function(...) 7
    p <- params(x, a = arg_double())
    expect_null(p[["types"]][[2L]])
  })
})

test_that("a type in a variable is a parameter's type", {
  Positive <- type_double(exclusive_min = 0)
  Point <- declare_in(
    "par2",
    new_struct("Point", fields = list(x = type_double()))
  )
  p <- params(x, a = arg(Positive, 1), b = arg(Point), c = arg_vec(Positive))
  expect_identical(p[["types"]][[2L]], Positive)
  expect_identical(p[["formals"]][["a"]], 1)
  expect_identical(p[["types"]][[4L]], vec(Positive))
  g <- declare_in("par2", generic("shift", params(x, by = arg(Point))))
  expect_identical(
    method_ir(g)[["params"]][[2L]][["type"]],
    list(kind = "named", name = "par2::Point")
  )
})

test_that("params() refuses what declares no parameter", {
  refused <- function(expr, message) {
    expect_error(expr, message, class = "r8_declaration_error")
  }
  refused(params(), "at least one parameter")
  refused(params("..."), "first parameter is the value dispatched on")
  refused(params(x, x), "`x` is declared twice")
  refused(params(x, ), "Argument 2 of params\\(\\) is empty")
  refused(params(x, y = ), "Parameter `y`.*bare name")
  refused(params(x, "y"), "bare, params\\(y\\)")
  refused(params(x, length(y)), "has no name")
  refused(params(x, arg_integer()), "n = arg_integer\\(\\)")
  refused(params(x, ... = arg_double()), "takes no type and no default")
  refused(params(x, `a b` = 1), "a syntactic R name")
  refused(params(x, n = arg()), "a type, a default, or a description")
  refused(params(x, n = arg_double(0, 1, , , , , , 5)), "`default` follows")
  refused(params(x, n = arg(type_double(), 1, "d")), "`description` follows")
  refused(params(x, quote), "`quote` is reserved")
  refused(params(x, environment = 1), "`environment` is reserved")
  refused(params(x, a = R8::Raven), "is a type")
  refused(
    params(x, v = arg_vec(type_integer(), default = c(1L, TRUE))),
    "mixes integer and logical"
  )
  f <- function(...) params(x, ...)
  refused(f(1), "`...` as the string")
})

test_that("params() refuses a type outside arg() and arg() outside a parameter", {
  refused <- function(expr, message) {
    expect_error(expr, message, class = "r8_declaration_error")
  }
  Prob <- type_double(min = 0, max = 1)
  Point <- declare_in(
    "par3",
    new_struct("Point", fields = list(x = type_double()))
  )
  Fits <- declare_in(
    "par3",
    new_trait("Fits", methods = list(fits = trait_method(params(x))))
  )
  refused(params(x, n = type_integer()), "n = arg\\(type_integer\\(\\)\\)")
  refused(params(x, n = Prob), "n = arg\\(Prob\\)")
  refused(params(x, n = Point), "arg\\(default = Point\\)")
  refused(params(x, n = R8::vec(Prob)), "is a type")
  refused(params(x, n = arg(Prob, Point)), "is a type")
  refused(
    params(x, n = arg_vec(arg_double())),
    "`arg_double\\(\\)` declares a parameter"
  )
  refused(params(x, n = c(arg_double(), 1)), "stand alone")
  refused(
    params(x, n = arg_double(default = arg_double())),
    "its default calls arg"
  )
  refused(params(x, n = arg(Fits)), "arg\\(dyn\\(Fits\\)\\)")
  refused(params(x, n = arg("double")), "its type is a field type")
  refused(params(x, n = arg_double(lo = 0)), "Parameter `n`.*unused argument")
  refused(
    params(x, n = arg_double(min = 0, exclusive_min = 0)),
    "Parameter `n`.*same side"
  )
  # A default that is a type, and one that reads another parameter.
  p <- params(x, type = arg(default = Point))
  expect_identical(p[["formals"]][["type"]], quote(Point))
  expect_no_error(params(x, Point, n = Point))
})

test_that("a constant default is checked against its type at the declaration", {
  refused <- function(expr, message) {
    expect_error(
      declare_in("par4", expr),
      message,
      class = "r8_declaration_error"
    )
  }
  refused(
    generic("f1", params(x, n = arg_integer(min = 0L, default = -1L))),
    "default of `n` is invalid.*default_invalid"
  )
  refused(
    generic("f1", params(x, n = arg_integer(default = NULL))),
    "default of `n` is invalid"
  )
  refused(
    generic("f1", params(x, n = arg_option(type_integer(), default = NA))),
    "default of `n` is invalid"
  )
  # An R value of the type: the JSON form of a date is not a Date.
  refused(
    generic("f1", params(x, d = arg_date(default = "2026-01-01"))),
    "default of `d` is invalid"
  )
  g <- declare_in("par4", {
    generic(
      "f2",
      params(
        x,
        d = arg_date(default = as.Date("2026-01-01")),
        w = arg_double(infinite = TRUE, default = -Inf),
        v = arg_vec(option(type_double()), default = c(1, NA)),
        h = arg(host_class("function"), NULL)
      )
    )
  })
  ir <- method_ir(g)[["params"]]
  expect_identical(
    ir[[2L]][["host_default"]][["text"]],
    "as.Date(\"2026-01-01\")"
  )
  expect_identical(ir[[3L]][["default"]], "-Infinity")
  expect_identical(ir[[4L]][["default"]], list(1L, NULL))
  expect_identical(ir[[5L]][["host_default"]][["text"]], "NULL")
})

test_that("arg() outside a declaration is R's own default or missing argument", {
  f <- function(
    x = arg(type_double()),
    n = arg_integer(min = 0L, default = length(x))
  ) {
    if (missing(n)) c(n, 1L) else n
  }
  expect_identical(f(c(1, 2, 3)), c(3L, 1L))
  expect_identical(f(1, n = 7L), 7L)
  expect_error(f(), "argument \"x\" is missing, with no default")
  cond <- tryCatch(f(), error = identity)
  expect_identical(conditionCall(cond), quote(f()))
  g <- function(x, w = arg_option(type_double())) w
  expect_null(g(1))
  expect_error(arg_double(), "params\\(", class = "r8_declaration_error")
})

test_that("generic() and trait_method() take params() only", {
  expect_error(
    declare_in("par5", generic("f", c("x", "..."))),
    "params\\(x, \"...\"\\)",
    class = "r8_declaration_error"
  )
  expect_error(
    declare_in("par5", generic("f", "x")),
    "params\\(x\\)",
    class = "r8_declaration_error"
  )
  expect_error(declare_in("par5", generic("f", args = "x")), "unused argument")
  expect_error(
    trait_method(function(x, y) NULL),
    "params\\(x, y\\)",
    class = "r8_declaration_error"
  )
})

test_that("a method's formals carry no arg()", {
  declare_in("par6", {
    A <- new_struct("A", fields = list(a = type_double()))
    f <- generic("f", params(x, n, "..."))
    expect_error(
      implement(f, A) <- function(x, n = arg_integer(), ...) n,
      "declares `n` with arg\\(\\)",
      class = "r8_declaration_error"
    )
    expect_error(
      trait_method(params(x), default_impl = function(x = arg_double()) x),
      "declares `x` with arg\\(\\)",
      class = "r8_declaration_error"
    )
  })
})

test_that("a dispatched parameter's type bounds its methods", {
  ir_of <- method_ir
  declare_in("par7", {
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
    width <- generic("width", params(x = arg(subtype(Range))))
    implement(width, Range) <- function(x) x$hi - x$lo
    expect_identical(width(Window(lo = 0, hi = 2, label = "w")), 2)
    expect_error(
      implement(width, Other) <- function(x) 0,
      "dispatch_param_type",
      class = "r8_declaration_error"
    )
    expect_error(
      generic("bad", params(x = arg_double())),
      "dispatch_param_type",
      class = "r8_declaration_error"
    )
    combine <- generic(
      "combine",
      params(
        x = arg(Range),
        y = arg(subtype(Range)),
        n = arg_integer(default = 1L)
      ),
      dispatch = c("x", "y")
    )
    ir <- ir_of(combine)[["params"]]
    expect_true(ir[[1L]][["dispatch"]])
    expect_identical(
      ir[[2L]][["type"]],
      list(kind = "subtype", of = "par7::Range")
    )
    expect_null(ir[[3L]][["dispatch"]])
  })
})

test_that("return types are declared by the generic and narrowed by a method", {
  ir_of <- method_ir
  declare_in("par8", {
    Model <- new_struct("Model", fields = list(rss = type_double()))
    Linear <- new_struct("Linear", extends = Model)
    Other <- new_struct("Other")
    A <- new_struct("A")
    B <- new_struct("B")
    fit <- generic("fit", params(x), return_type = subtype(Model))
    expect_identical(
      ir_of(fit)[["return_type"]],
      list(kind = "subtype", of = "par8::Model")
    )
    implement(fit, A, return_type = Linear) <- function(x) Linear(rss = 1)
    expect_s3_class(fit(A()), "par8::Linear")
    expect_error(
      implement(fit, B, return_type = Other) <- function(x) Other(),
      "return_type_unfit",
      class = "r8_declaration_error"
    )
    expect_error(
      implement(fit, B, return_type = "Self") <- function(x) x,
      "A return type is",
      class = "r8_declaration_error"
    )
    same <- generic("same", params(x), return_type = "Self")
    expect_identical(ir_of(same)[["return_type"]], "Self")
    expect_error(
      generic("bad", params(x), return_type = "Model"),
      "A return type is.*\"Self\"",
      class = "r8_declaration_error"
    )
    expect_error(
      generic("tag<-", params(x, value), return_type = "Self"),
      "return_type_replacement",
      class = "r8_declaration_error"
    )
    expect_error(
      implement(format, A, return_type = type_character()) <- function(x, ...) {
        "a"
      },
      "this generic declares none",
      class = "r8_declaration_error"
    )
  })
})

test_that("impl_method() gives an impl's method its own return type", {
  declare_in("par9", {
    Model <- new_struct("Model", fields = list(rss = type_double()))
    Linear <- new_struct("Linear", extends = Model)
    A <- new_struct("A")
    B <- new_struct("B")
    Train <- new_trait(
      "Train",
      methods = list(
        train = trait_method(params(spec), return_type = subtype(Model)),
        note = trait_method(params(spec), default_impl = function(spec) "note")
      )
    )
    impl(
      Train,
      A,
      methods = list(
        train = impl_method(
          function(spec) Linear(rss = 2),
          return_type = Linear
        )
      )
    )
    expect_identical(Train@train(A())$rss, 2)
    expect_identical(Train@note(A()), "note")
    expect_error(
      impl(
        Train,
        B,
        methods = list(
          train = function(spec) Model(rss = 1),
          note = impl_method(function(spec) "b", return_type = type_character())
        )
      ),
      "return_type_unfit",
      class = "r8_declaration_error"
    )
    expect_error(
      impl_method(1, Linear),
      "`fn` is a function",
      class = "r8_declaration_error"
    )
    expect_error(
      impl_method(function(spec) 1, NULL),
      "its own return type",
      class = "r8_declaration_error"
    )
  })
})

test_that("a default is evaluated in the generic's frame and where params() is called", {
  out <- declare_in("par10", {
    A <- new_struct("A", fields = list(v = vec(type_double())))
    my_option <- "declared here"
    setup_x <- function() "made here"
    f <- generic(
      "f",
      params(
        x,
        n = length(x$v),
        opt = my_option,
        cfg = setup_x(),
        env = parent.frame(),
        was_missing = missing(n),
        type = c("avg", "all")
      )
    )
    implement(f, A) <- function(x, n, opt, cfg, env, was_missing, type) {
      type <- match.arg(type, c("avg", "all"))
      list(
        n = n,
        opt = opt,
        cfg = cfg,
        env = env,
        was_missing = was_missing,
        type = type
      )
    }
    list(A = A, f = f)
  })
  a <- out[["A"]](v = c(1, 2, 3))
  r <- out[["f"]](a)
  expect_identical(r[["n"]], 3L)
  expect_identical(r[["opt"]], "declared here")
  expect_identical(r[["cfg"]], "made here")
  expect_identical(r[["env"]], environment())
  expect_true(r[["was_missing"]])
  expect_identical(r[["type"]], "avg")
  expect_false(out[["f"]](a, n = 9L)[["was_missing"]])
  expect_identical(out[["f"]](a, opt = "given")[["opt"]], "given")
})

test_that("a constant default is a value, whatever masks c() where it is called", {
  declare_in("par11", {
    A <- new_struct("A")
    f <- generic("f", params(x, v = c(1, 2), w = -1))
    c <- function(...) "masked"
    implement(f, A) <- function(x, v, w) list(v, w)
    expect_identical(f(A()), list(base::c(1, 2), -1))
    # A call of the masking function is an expression, evaluated at the call.
    g <- generic("g", params(x, v = c(1, 2)))
    implement(g, A) <- function(x, v) v
    expect_identical(g(A()), "masked")
  })
})

test_that("a generic is refused where a function its body calls is masked", {
  expect_error(
    declare_in("par12", {
      .Call <- function(...) stop("masked .Call()")
      generic("f", params(x))
    }),
    "`.Call` is masked.*base_masked",
    class = "r8_declaration_error"
  )
  expect_error(
    declare_in("par12", {
      quote <- function(...) NULL
      new_trait("T1", methods = list(t1 = trait_method(params(x))))
    }),
    "`quote` is masked",
    class = "r8_declaration_error"
  )
  # A variable of the same name masks no function.
  expect_no_error(declare_in("par12", {
    quote <- 1
    generic("h", params(x))
  }))
})

test_that("a method gives no default to a parameter of its R8 generic", {
  declare_in("par13", {
    A <- new_struct("A")
    describe <- generic("describe", params(x, verbosity, "..."))
    expect_error(
      implement(describe, A) <- function(x, verbosity = 1L, ...) verbosity,
      "`verbosity = 1L` in its params\\(\\) \\[method_default\\]",
      class = "r8_declaration_error"
    )
    # Its own arguments after the dots have defaults.
    implement(describe, A) <- function(x, verbosity, ..., digits = 3L) digits
    expect_identical(describe(A(), 1L), 3L)

    Sized <- new_trait(
      "Sized",
      methods = list(
        size = trait_method(params(x, n)),
        summary = trait_method(params(object, "..."))
      )
    )
    expect_error(
      impl(
        Sized,
        A,
        methods = list(
          size = function(x, n = 2) n,
          summary = function(object, ...) "s"
        )
      ),
      "`par13::Sized@size` gives `n` a default",
      class = "r8_declaration_error"
    )
    # The default, in the generic's params(), beside the parameter's type.
    sized <- generic(
      "sized",
      params(x, k = length(x), n, m = arg_integer(min = 1L))
    )
    e <- expect_error(
      implement(sized, A) <- function(x, k, n = 2, m) n,
      class = "r8_declaration_error"
    )
    expect_identical(
      conditionMessage(e),
      paste0(
        "The method of `sized` gives `n` a default, which belongs to the ",
        "generic: `n = 2` in its params() [method_default]."
      )
    )
    e <- expect_error(
      implement(sized, A) <- function(x, k, n, m = 1L) n,
      class = "r8_declaration_error"
    )
    expect_match(
      conditionMessage(e),
      "`default = 1L` in the type of `m` in its params()",
      fixed = TRUE
    )
    # R applies the defaults of a method of an S3 generic.
    impl(
      Sized,
      A,
      methods = list(
        size = function(x, n) n,
        summary = function(object, ..., digits = 4L) digits
      )
    )
    expect_identical(summary(A()), 4L)
    expect_error(
      new_trait(
        "Bad",
        methods = list(
          bad = trait_method(params(x, n), default_impl = function(x, n = 1) n)
        )
      ),
      "`par13::Bad@bad` gives `n` a default",
      class = "r8_declaration_error"
    )
    implement(toString, A) <- function(x, ..., width = 10L) paste(width)
    expect_identical(toString(A()), "10")
  })
})

test_that("a trait method that maps an S3 generic takes names only", {
  refused <- function(method, message) {
    expect_error(
      declare_in(
        "par14",
        new_trait("Report", methods = list(summary = method))
      ),
      message,
      class = "r8_declaration_error"
    )
  }
  refused(
    trait_method(params(object, "...", digits = 3)),
    "`digits` takes no default \\[mapped_generic\\]"
  )
  refused(
    trait_method(params(object, "...", digits = arg_integer())),
    "`digits` takes no type"
  )
  refused(
    trait_method(params(object, "..."), return_type = type_character()),
    "declares no return type"
  )
})

test_that("a dispatched parameter may be named by a prefix of an argument of implement()", {
  declare_in("par15", {
    Graph <- new_struct("Graph")
    Node <- new_struct("Node")
    connect <- generic("connect", params(g, v), dispatch = c("g", "v"))
    implement(connect, g = Graph) <- function(g, v) "any vertex"
    implement(connect, v = Node, g = Graph) <- function(g, v) "a node"
    expect_identical(connect(Graph(), 1), "any vertex")
    expect_identical(connect(Graph(), Node()), "a node")
    expect_identical(r8method(connect, g = Graph)(1, 2), "any vertex")
    pair <- generic(
      "pair",
      params(generic, val),
      dispatch = c("generic", "val")
    )
    implement(pair, generic = Graph, val = Node) <- function(generic, val) {
      "pair"
    }
    expect_identical(pair(Graph(), Node()), "pair")
    for (bad in c("return_type", "value")) {
      p <- do.call(params, list(quote(x), as.name(bad)))
      expect_error(
        generic("bad", p, dispatch = c("x", bad)),
        "names an argument of implement\\(\\)",
        class = "r8_declaration_error"
      )
    }
    # A parameter that is not dispatched has any name.
    expect_no_error(generic("fine", params(x, value, return_type)))
    expect_error(
      r8method(generic = connect, Graph),
      "without an argument name",
      class = "r8_dispatch_error"
    )
    expect_error(
      r8method(connect),
      "then the types",
      class = "r8_dispatch_error"
    )
  })
})
