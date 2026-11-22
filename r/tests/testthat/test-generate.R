# test-generate.R
# ::R8::
# 2026- EDG rtemis.org

# The generator of R/r8-types.R renders IR as R declaration calls. Each test
# declares types in R, renders their IR, declares the rendering again in the
# same namespace, and compares the IR: the round trip from IR to R to IR.

# Renders the declarations of `types` (in order) from their IR, declares the
# rendering in `ns`, and returns the canonical IR before and after.
round_trip <- function(ns, types) {
  before <- vapply(
    types,
    function(t) R8:::canonical_ir(R8:::type_ir(t)),
    character(1)
  )
  lines <- unlist(lapply(types, function(t) {
    ir <- R8:::type_ir(t)
    if (ir[["kind"]] == "struct") {
      R8:::render_struct(ir, ns, native = FALSE)
    } else {
      R8:::render_enum(ir, ns)
    }
  }))
  text <- unlist(strsplit(lines, "\n", fixed = TRUE))
  env <- new.env(parent = globalenv())
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
        d = type_double(exclusive_min = 0, finite = FALSE),
        dv = type_double(values = c(0.5, 1.25)),
        ch = type_character(pattern = "^[a-z]+$"),
        cv = type_character(values = c("a", "b")),
        raw = type_raw(),
        date = type_date(),
        time = type_posixct(),
        f = type_factor(levels = c("lo", "hi"), ordered = TRUE),
        js = type_json(),
        r = option(type_r("lm")),
        v = vec(
          type_double(min = 0),
          min_length = 1L,
          max_length = 5L,
          unique = TRUE
        ),
        o = option(type_integer()),
        o_runtime = option(type_double(), resolve = "runtime"),
        o_computed = option(
          type_double(),
          resolve = "computed",
          compute = d * 2
        ),
        h = hashmap(type_double(), keys = type_character(pattern = "^k")),
        om = ordered_map(type_character()),
        p = P,
        k = K,
        m = type_matrix(option(type_double()), nrow = 2L, ncol = 3L),
        mp = type_matrix(P),
        a = type_array(type_integer(), dim = c(2L, 3L, 4L)),
        sm = type_sparse_matrix(type_logical(), nrow = 4L),
        tb = type_table(
          list(
            start = type_integer(),
            width = field(type_integer(min = 0L), unique = TRUE),
            label = field(type_character(), optional = TRUE)
          ),
          open = TRUE,
          rules = list(rule(
            start + width <= 100L,
            message = "The end fits.",
            id = "END_FITS"
          ))
        ),
        tp = type_table(P)
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
        h = hashmap(type_integer()),
        k = field(K, default = K@Linear, description = "A kernel."),
        tb = type_table(list(
          start = type_integer(),
          label = field(type_character(), optional = TRUE)
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
        w = field(vec(type_double()), default = c(0.1, 1 / 3), external = TRUE),
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
  expect_output(print(json_ir("[1, 2]")), "<r8_json_ir> [1, 2]", fixed = TRUE)
})

test_that("a native type constructs values once its codec is registered", {
  S <- declare_in("gen_native", {
    new_struct("S", fields = list(x = type_double()), native = TRUE)
  })
  expect_true(attr(S, "r8_native"))
  expect_error(S(x = 1), class = "r8_codec_missing")
  expect_error(r8_from_json(S, '{"x": 1}'), class = "r8_codec_missing")
})

test_that("registration lists every difference between R declarations and the exports", {
  compared <- list(
    type_name = c("p::A", "p::B", "p::C", "p::D"),
    status = c("missing", "differs", "same", "same"),
    codec = c(TRUE, FALSE, TRUE, FALSE)
  )
  err <- expect_error(
    R8:::check_exports("p", compared, native = c("p::D")),
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
    "`p::C` has a codec in the compiled exports; its R declaration takes native = TRUE.",
    fixed = TRUE
  )
  expect_match(
    msg,
    "`p::D` is declared with native = TRUE; the compiled exports carry no codec for it.",
    fixed = TRUE
  )
  expect_match(msg, "R8::r8_document_native()", fixed = TRUE)
  expect_null(R8:::check_exports(
    "p",
    list(type_name = "p::A", status = "same", codec = TRUE),
    native = "p::A"
  ))
  expect_error(
    R8:::check_exports("p", NULL, native = "p::A"),
    class = "r8_declaration_error"
  )
})

test_that("a native enum's variants and unit values wait for its codec", {
  E <- declare_in("gen_native_enum", {
    new_enum(
      "E",
      variants = list(
        A = variant(fields = list(x = type_double())),
        B = variant()
      ),
      native = TRUE
    )
  })
  expect_true(attr(E, "r8_native"))
  expect_error(E@A(x = 1), class = "r8_codec_missing")
  expect_error(E@B, class = "r8_codec_missing")
  lines <- R8:::render_enum(
    R8:::type_ir(E),
    "gen_native_enum",
    native = TRUE
  )
  expect_true("  native = TRUE" %in% lines)
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
