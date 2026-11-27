# test-141-generate.R
# ::R8::
# 2026- EDG rtemis.org

# The R declarations that R8 writes for types declared in Rust.
#
# The generator of R/r8-types.R renders IR as R declaration calls. Each test
# declares types in R, renders their IR, declares the rendering again in the
# same namespace, and compares the IR: the round trip from IR to R to IR.

# Renders the declarations of `types` (in order) from their IR, declares the
# rendering in `ns`, with the objects of `traits`, by name, in reach, and returns
# the canonical IR before and after.
round_trip <- function(ns, types, traits = list()) {
  before <- vapply(
    types,
    function(t) R8:::canonical_ir(R8:::type_ir(t)),
    character(1)
  )
  lines <- unlist(lapply(types, function(t) {
    ir <- R8:::type_ir(t)
    if (ir[["kind"]] == "struct") {
      R8:::render_struct(ir, ns, rust_stored = FALSE)
    } else {
      R8:::render_enum(ir, ns)
    }
  }))
  text <- unlist(strsplit(lines, "\n", fixed = TRUE))
  env <- list2env(traits, parent = globalenv())
  op <- options(r8.namespace = ns)
  on.exit(options(op))
  eval(parse(text = text, keep.source = FALSE), envir = env)
  after <- vapply(
    types,
    function(t) {
      name <- R8:::type_ir(t)[["name"]]
      R8:::canonical_ir(R8:::type_ir(env[[name]]))
    },
    character(1)
  )
  list(before = unname(before), after = unname(after), text = text)
}

test_that("every field type renders as R that declares the same IR", {
  types <- declare_in("gen_types", {
    P <- new_struct("P", fields = list(x = type_double(), y = type_double()))
    K <- new_enum(
      "K",
      variants = list(
        Linear = variant(description = "No kernel parameters."),
        Rbf = variant(fields = list(gamma = type_double(exclusive_min = 0))),
        Wrap = variant(of = P)
      ),
      tag = "kind",
      version = 2L
    )
    S <- new_struct(
      "S",
      fields = list(
        l = type_logical(),
        i = type_integer(min = -1L, max = 10L),
        ie = type_integer(exclusive_min = -2L, exclusive_max = 11L),
        iv = type_integer(values = c(1L, 2L, 3L)),
        d = type_double(exclusive_min = 0, infinite = TRUE, nan = TRUE),
        dv = type_double(values = c(0.5, 1.25)),
        ch = type_character(pattern = "^[a-z]+$"),
        cv = type_character(values = c("a", "b")),
        raw = type_raw(),
        date = type_date(),
        time = type_posixct(),
        f = type_factor(levels = c("lo", "hi"), ordered = TRUE),
        js = type_json(),
        r = option(host_class("lm")),
        v = vec(
          type_double(min = 0),
          min_length = 1L,
          max_length = 5L,
          unique = TRUE
        ),
        o = option(type_integer()),
        o_context = field(option(type_double()), context = TRUE),
        o_computed = field(option(type_double()), compute = calc(d * 2)),
        h = btree_map(type_double(), keys = type_character(pattern = "^k")),
        om = index_map(type_character()),
        p = P,
        k = K,
        m = type_matrix(option(type_double()), nrow = 2L, ncol = 3L),
        mp = type_matrix(of = P),
        a = type_array(type_integer(), dim = c(2L, 3L, 4L)),
        sm = type_sparse_matrix(type_logical(), nrow = 4L),
        tb = type_table(
          list(
            start = type_integer(),
            width = column(type_integer(min = 0L), unique = TRUE),
            label = column(type_character(), required = FALSE)
          ),
          open = TRUE,
          rules = list(rule(
            start + width <= 100L,
            message = "The end fits.",
            id = "END_FITS"
          ))
        ),
        tp = type_table(of = P)
      ),
      description = "Every field type."
    )
    list(P, K, S)
  })
  rt <- round_trip("gen_types", types)
  expect_identical(
    rt[["after"]],
    rt[["before"]],
    info = paste(rt[["text"]], collapse = "\n")
  )
})

test_that("a struct that extends another renders with its own members", {
  types <- declare_in("gen_extends", {
    Range <- new_struct(
      "Range",
      fields = list(
        lo = field(type_double(), description = "The lower end."),
        hi = field(type_double(), default = 10),
        unit = option(type_character())
      ),
      rules = rule(lo <= hi, id = "ordered")
    )
    Window <- new_struct(
      "Window",
      fields = list(label = type_character()),
      rules = rule(label != "", id = "labeled"),
      extends = Range,
      defaults = list(lo = 0, unit = "s")
    )
    Pane <- new_struct(
      "Pane",
      fields = list(
        within = subtype(Range),
        around = option(vec(subtype(Window)))
      ),
      extends = Window,
      defaults = list(unit = NULL)
    )
    Plain <- new_struct("Plain", extends = Range)
    list(Range, Window, Pane, Plain)
  })
  rt <- round_trip("gen_extends", types)
  expect_identical(
    rt[["after"]],
    rt[["before"]],
    info = paste(rt[["text"]], collapse = "\n")
  )
  text <- paste(rt[["text"]], collapse = "\n")
  expect_match(text, "extends = Range,\n  defaults = list(lo = 0", fixed = TRUE)
  expect_match(text, "defaults = list(unit = NULL)", fixed = TRUE)
  expect_match(text, "R8::subtype(Range)", fixed = TRUE)
  # The roxygen block documents the inherited arguments of the constructor.
  expect_match(text, "#' @param lo The lower end.\n#' @param hi", fixed = TRUE)
  # A type is declared after its parent and the structs whose subtypes it
  # holds.
  irs <- lapply(types, R8:::type_ir)
  order <- vapply(
    R8:::type_order(rev(irs), "gen_extends"),
    function(t) t[["name"]],
    ""
  )
  expect_identical(order, c("Range", "Plain", "Window", "Pane"))
})

test_that("a field of any implementor of a trait renders as dyn()", {
  ns <- declare_in("gen_dyn", {
    Figure <- new_trait(
      "Figure",
      methods = list(area = trait_method(params(x)))
    )
    width <- generic("width", params(x))
    Panel <- new_struct(
      "Panel",
      fields = list(
        figure = dyn(Figure),
        wide = option(dyn(width)),
        all = vec(dyn(Figure), min_length = 1L)
      )
    )
    Frame <- new_enum(
      "Frame",
      variants = list(Of = variant(fields = list(figure = dyn(Figure))))
    )
    list(types = list(Panel, Frame), traits = list(Figure, width))
  })
  traits <- stats::setNames(ns[["traits"]], c("Figure", "width"))
  rt <- round_trip("gen_dyn", ns[["types"]], traits)
  expect_identical(rt[["after"]], rt[["before"]])
  text <- paste(rt[["text"]], collapse = "\n")
  expect_match(text, "R8::dyn(Figure)", fixed = TRUE)
  expect_match(text, "R8::option(R8::dyn(width))", fixed = TRUE)
  irs <- lapply(ns[["types"]], R8:::type_ir)
  expect_setequal(
    R8:::dyn_traits(irs),
    c("gen_dyn::Figure", "gen_dyn::width")
  )
  # A trait of another namespace is found among the imported packages.
  expect_error(
    R8:::render_type(
      list(kind = "dyn", trait = "elsewhere::Figure"),
      "gen_dyn"
    ),
    "`elsewhere::Figure` belongs to another namespace",
    class = "r8_declaration_error"
  )
})

test_that("a trait and a generic render as R that declares the same IR", {
  traits <- declare_in("gen_traits", {
    P <- new_struct("P", fields = list(a = type_double()), version = 1L)
    Figure <- new_trait(
      "Figure",
      methods = list(
        area = trait_method(
          params(x),
          description = "The area.",
          return_type = type_double(min = 0)
        ),
        scaled = trait_method(
          params(x, by = arg_double(min = 0, default = 1), "..."),
          return_type = "Self"
        )
      ),
      description = "A figure.",
      tag = "kind",
      required_fields = list(
        name = type_character(),
        size = field(option(type_integer()), description = "The size.")
      )
    )
    tune <- generic(
      "tune",
      params(
        object,
        p = arg(P),
        any,
        n = arg_integer(default = 10L),
        q = arg_option(P),
        r = arg(option(type_double())),
        s = arg_option(type_character(), default = "a"),
        v = arg_vec(type_double(), min_length = 1L, default = c(1, 2)),
        w = arg_date(),
        f = arg(dyn(Figure)),
        g = arg(subtype(P)),
        flag = arg_logical(default = TRUE)
      ),
      description = "Tunes a model.",
      return_type = subtype(P)
    )
    near <- generic("near", params(x, y, by), dispatch = c("x", "y"))
    `size<-` <- generic("size<-", params(x, value = arg_integer(min = 1L)))
    list(
      objects = list(P = P),
      names = c("Figure", "tune", "near", "with_size")
    )
  })
  ir_of <- function(n) R8:::r8_trait_ir(paste0("gen_traits::", n))
  before <- lapply(traits[["names"]], ir_of)
  text <- unlist(lapply(before, R8:::render_trait, ns = "gen_traits"))
  text <- unlist(strsplit(text, "\n", fixed = TRUE))
  env <- list2env(traits[["objects"]], parent = globalenv())
  op <- options(r8.namespace = "gen_traits")
  on.exit(options(op))
  eval(parse(text = text, keep.source = FALSE), envir = env)
  after <- lapply(traits[["names"]], ir_of)
  expect_identical(
    vapply(after, R8:::canonical_ir, ""),
    vapply(before, R8:::canonical_ir, ""),
    info = paste(text, collapse = "\n")
  )
  code <- paste(text, collapse = "\n")
  for (part in c(
    "p = R8::arg(P), any, n = R8::arg_integer(default = 10L)",
    "q = R8::arg_option(P), r = R8::arg(R8::option(R8::type_double()))",
    "s = R8::arg_option(R8::type_character(), default = \"a\")",
    "v = R8::arg_vec(R8::type_double(), min_length = 1L, default = c(1, 2))",
    "w = R8::arg_date(), f = R8::arg(R8::dyn(Figure))",
    "scaled = R8::trait_method(R8::params(x, by = R8::arg_double(min = 0, default = 1), \"...\"), return_type = \"Self\")",
    "dispatch = c(\"x\", \"y\")",
    "`size<-` <- R8::generic(",
    "area <- Figure@area",
    "#' @return A value of the type of `x`."
  )) {
    expect_match(code, part, fixed = TRUE)
  }
  # A declaration follows those it names.
  types <- list(R8:::type_ir(traits[["objects"]][["P"]]))
  order <- vapply(
    R8:::type_order(c(rev(before), types), "gen_traits"),
    function(t) t[["name"]],
    ""
  )
  expect_lt(match("P", order), match("tune", order))
  expect_lt(match("Figure", order), match("tune", order))
})

test_that("a change to an inherited field without an R form is refused", {
  ir <- declare_in("gen_changed", {
    Range <- new_struct("Range", fields = list(lo = type_double()))
    Window <- new_struct("Window", extends = Range)
    R8:::type_ir(Window)
  })
  ir[["fields"]][[1L]][["sensitive"]] <- TRUE
  ir[["fields"]][[1L]][["description"]] <- "The lower end."
  expect_error(
    R8:::render_struct(ir, "gen_changed", rust_stored = FALSE),
    "`gen_changed::Window` changes `sensitive` and `description` of the inherited field `lo`",
    class = "r8_declaration_error"
  )
})

test_that("a declaration read back from the registry as R lists declares the same type", {
  types <- declare_in("ir_lists", {
    K <- new_enum(
      "K",
      variants = list(
        Linear = variant(),
        Rbf = variant(
          fields = list(gamma = type_double(exclusive_min = 0, max = 10))
        )
      ),
      tag = "kind"
    )
    S <- new_struct(
      "S",
      fields = list(
        i = type_integer(min = 1L, exclusive_max = 9L),
        f = type_factor(levels = c("lo", "hi"), ordered = TRUE),
        v = vec(type_double(min = 0), min_length = 1L, unique = TRUE),
        o = option(type_character(pattern = "^[a-z]+$")),
        h = btree_map(type_integer()),
        k = field(K, default = K@Linear, description = "A kernel."),
        tb = type_table(list(
          start = type_integer(),
          label = column(type_character(), required = FALSE)
        ))
      ),
      rules = list(rule(
        i < 5L,
        when = !is.null(o),
        message = "i is small.",
        id = "I_SMALL"
      ))
    )
    list(K, S)
  })
  for (t in types) {
    before <- r8_declaration(t)
    problems <- R8:::r8_declare_type(R8:::type_ir(t), "ir_lists")
    expect_length(problems[["code"]], 0L)
    expect_identical(r8_declaration(t), before)
  }
})

test_that("field facts, defaults, and rules render as R that declares the same IR", {
  types <- declare_in("gen_facts", {
    K <- new_enum("K", variants = list(Linear = variant(), Poly = variant()))
    P <- new_struct("P", fields = list(x = type_double(), y = type_double()))
    S <- new_struct(
      "S",
      fields = list(
        cost = field(
          type_double(exclusive_min = 0),
          default = 1.5,
          description = "Cost; 100% is the maximum.",
          unit = "mg",
          sensitive = TRUE
        ),
        n = field(type_integer(), default = 2L),
        name = field(type_character(), default = "a \"b\""),
        w = field(vec(type_double()), default = c(0.1, 1 / 3)),
        k = field(K, default = K@Poly),
        origin = field(P, default = json_ir('{"x": 0, "y": 0}')),
        tags = field(
          option(vec(type_character())),
          extensions = list("rtemis:tunable" = TRUE)
        )
      ),
      rules = list(
        rule(
          cost <= 100 && cost > 0,
          message = "cost is in (0, 100].",
          id = "COST_MAX"
        ),
        rule(
          n >= 2L,
          when = is_variant(k, "Poly"),
          message = "Poly takes n >= 2.",
          id = "POLY"
        ),
        rule(
          !is.null(name) || name %in% c("a", "b"),
          message = "name is set.",
          id = "NAME"
        ),
        rule(
          json_ir(
            '{"and": [{">": [{"var": "n"}, 0]}, {"<": [{"var": "n"}, 9]}, {"!==": [{"var": "name"}, ""]}]}'
          ),
          message = "Three conditions.",
          id = "ALL"
        )
      )
    )
    list(K, P, S)
  })
  rt <- round_trip("gen_facts", types)
  expect_identical(
    rt[["after"]],
    rt[["before"]],
    info = paste(rt[["text"]], collapse = "\n")
  )
  text <- paste(rt[["text"]], collapse = "\n")
  # R forms where R has one; json_ir() where it does not.
  expect_match(text, "default = K@Poly", fixed = TRUE)
  expect_match(text, "cost <= 100 && cost > 0", fixed = TRUE)
  expect_match(text, "when = is_variant(k, \"Poly\")", fixed = TRUE)
  expect_match(text, "default = R8::json_ir('{\"x\":0,\"y\":0}')", fixed = TRUE)
  expect_match(text, "R8::json_ir('{\"and\":[", fixed = TRUE)
  expect_match(text, "100\\% is the maximum", fixed = TRUE)
})

test_that("JSONLogic renders as R when R compiles back to it, and as json_ir() otherwise", {
  r <- function(json) R8:::render_logic(R8:::r8_json_parse(json))
  expect_identical(r('{"<=": [{"var": "a.b"}, 1.5]}'), "a$b <= 1.5")
  expect_identical(
    r('{"if": [{"is_null": [{"var": "a"}]}, true, {">": [{"var": "a"}, 2]}]}'),
    "if (is.null(a)) TRUE else a > 2L"
  )
  expect_identical(r('{"*": [{"+": [{"var": "a"}, 1]}, 2]}'), "(a + 1L) * 2L")
  expect_identical(
    r('{"in": [{"var": "k"}, ["x", "y"]]}'),
    "k %in% c(\"x\", \"y\")"
  )
  # Three operands of `and`, and an integer beyond 2^53, have no R form; the
  # escape keeps the integer as written.
  expect_identical(
    r('{"and": [true, false, true]}'),
    "R8::json_ir('{\"and\":[true,false,true]}')"
  )
  expect_identical(
    r('{">": [{"var": "a"}, 9007199254740993]}'),
    "R8::json_ir('{\">\":[{\"var\":\"a\"},9007199254740993]}')"
  )
})

test_that("json_ir() carries JSON into the IR as written", {
  r <- rule(
    json_ir('{">": [{"var": "a"}, 9007199254740993]}'),
    message = "m",
    id = "BIG"
  )
  expect_identical(
    R8:::r8_ir_json(unclass(r)),
    '{"id":"BIG","invariant":{">":[{"var":"a"},9007199254740993]},"message":"m"}'
  )
  expect_error(json_ir("{not json"), class = "r8_declaration_error")
  expect_error(json_ir(c("1", "2")), class = "r8_declaration_error")
  expect_output(print(json_ir("[1, 2]")), "<r8 json_ir> [1, 2]", fixed = TRUE)
})

test_that("a Rust-stored type constructs values once its codec is registered", {
  S <- declare_in("gen_rust", {
    new_struct("S", fields = list(x = type_double()), storage = "rust")
  })
  expect_identical(attr(S, "r8_storage"), "rust")
  expect_error(S(x = 1), class = "r8_codec_missing_error")
  expect_error(r8_from_json(S, '{"x": 1}'), class = "r8_codec_missing_error")
})

test_that("registration lists every difference between R declarations and the exports", {
  compared <- list(
    type_name = c("p::A", "p::B", "p::C", "p::D"),
    status = c("missing", "differs", "same", "same"),
    codec = c(TRUE, FALSE, TRUE, FALSE)
  )
  err <- expect_error(
    R8:::check_exports("p", compared, rust_stored = c("p::D")),
    class = "r8_declaration_error"
  )
  msg <- conditionMessage(err)
  expect_match(
    msg,
    "`p::A` is declared in the compiled exports and has no R declaration.",
    fixed = TRUE
  )
  expect_match(msg, "The R declaration of `p::B` differs", fixed = TRUE)
  expect_match(
    msg,
    "`p::C` has a codec in the compiled exports; its R declaration takes storage = \"rust\".",
    fixed = TRUE
  )
  expect_match(
    msg,
    "`p::D` is declared with storage = \"rust\"; the compiled exports carry no codec for it.",
    fixed = TRUE
  )
  expect_match(msg, "R8::r8_document_rust()", fixed = TRUE)
  expect_null(R8:::check_exports(
    "p",
    list(type_name = "p::A", status = "same", codec = TRUE),
    rust_stored = "p::A"
  ))
  expect_error(
    R8:::check_exports("p", NULL, rust_stored = "p::A"),
    class = "r8_declaration_error"
  )
})

test_that("a Rust-stored enum's variants and unit values wait for its codec", {
  E <- declare_in("gen_rust_enum", {
    new_enum(
      "E",
      variants = list(
        A = variant(fields = list(x = type_double())),
        B = variant()
      ),
      storage = "rust"
    )
  })
  expect_identical(attr(E, "r8_storage"), "rust")
  expect_error(E@A(x = 1), class = "r8_codec_missing_error")
  expect_error(E@B, class = "r8_codec_missing_error")
  lines <- R8:::render_enum(
    R8:::type_ir(E),
    "gen_rust_enum",
    rust_stored = TRUE
  )
  expect_true("  storage = \"rust\"" %in% lines)
})

test_that("generated code compares by tokens, so formatting keeps it current", {
  a <- c(
    "# r8-types.R",
    "S <- R8::new_struct(\"S\", fields = list(x = R8::type_double()))"
  )
  b <- c(
    "# r8-types.R",
    "S <- R8::new_struct(",
    "  \"S\",",
    "  fields = list(x = R8::type_double())",
    ")"
  )
  expect_true(R8:::same_code(a, b))
  expect_false(R8:::same_code(a, sub("double", "integer", b)))
  expect_false(R8:::same_code(a, sub("r8-types.R", "other.R", b)))
  expect_false(R8:::same_code(character(), b))
  expect_false(R8:::same_code("S <- (", b))
})
