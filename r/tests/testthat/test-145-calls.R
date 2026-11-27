# test-145-calls.R
# ::R8::
# 2026- EDG rtemis.org

# Descriptions of parameters, the JSON Schema of a function's arguments, and
# documents of a call: r8check_call(), r8call_from_json(), and
# r8call_to_json().
#
# spec: R8/class-system#typed-functions

clip_of <- function(ns) {
  declare_in(ns, {
    Range <- new_struct(
      "Range",
      fields = list(lo = type_double(), hi = type_double())
    )
    new_function(
      "clip",
      function(
        x = arg_vec(type_double(), description = "The values."),
        lo = arg_double(default = 0, description = "The lower bound."),
        hi = arg_double(),
        n = arg_integer(default = length(x)),
        w = arg(option(Range)),
        verbose = arg_logical(default = FALSE, description = "Print progress.")
      ) {
        list(call = sys.call(), value = pmin(pmax(x, lo), hi), n = n)
      },
      rules = list(order = rule(lo <= hi), count = rule(n >= 1L)),
      description = "Clips values."
    )
  })
}

doc_of <- function(ns, args) {
  paste0('{"$function": "', ns, '::clip", "arguments": ', args, "}")
}

test_that("a parameter's description is in its declaration", {
  clip <- clip_of("call1")
  params <- r8_function_ir("call1::clip")[["params"]]
  of <- function(n) {
    Filter(function(p) is.list(p) && identical(p[["name"]], n), params)[[1L]]
  }
  expect_identical(of("x")[["description"]], "The values.")
  expect_identical(of("lo")[["description"]], "The lower bound.")
  out <- capture.output(print(clip))
  expect_true("    The lower bound." %in% out)
  # An untyped parameter with a description alone.
  g <- declare_in("call1", {
    new_function("g", function(x, v = arg(description = "A value.")) v)
  })
  v <- r8_function_ir("call1::g")[["params"]][[2L]]
  expect_null(v[["type"]])
  expect_identical(v[["description"]], "A value.")
  expect_true("  v" %in% capture.output(print(g)))
  expect_identical(g(1, 2), 2)
  e <- expect_error(r8schema(g), class = "r8_type_error")
  expect_match(conditionMessage(e), "`x`, `v`", fixed = TRUE)
  # params() reads the same.
  p <- params(x, n = arg_integer(description = "Draws."))
  expect_identical(unclass(p)[["descriptions"]], c("", "Draws."))
  expect_true("    Draws." %in% capture.output(print(p)))
  expect_error(
    params(x, n = arg_integer(description = 1)),
    "one string",
    class = "r8_declaration_error"
  )
  # Outside a declaration, arg() still gives R's error for a missing argument.
  f <- function(v = arg(description = "A value.")) v
  expect_error(f(), "missing", class = "error")
})

test_that("r8schema() writes the schema of a function's arguments", {
  f <- declare_in("call2", {
    new_function(
      "f",
      function(
        x = arg_vec(type_double(), description = "The values."),
        lo = arg_double(default = 0),
        n = arg_integer(default = length(x)),
        z = arg_option(type_double())
      ) {
        x
      },
      description = "F."
    )
  })
  # The schema's text without its white space.
  s <- gsub("[[:space:]]", "", r8schema(f))
  has <- function(text) expect_true(grepl(text, s, fixed = TRUE), info = text)
  has('"title":"call2::f"')
  has('"description":"F."')
  has('"required":["x"]')
  has('"additionalProperties":false')
  has('"description":"Thevalues."')
  has('"lo":{"type":"number","default":0}')
  has('"x-r8":{"host_default":{"host":"r","text":"length(x)"}}')
  has('"z":{"anyOf":[{"type":"number"},{"type":"null"}],"default":null}')
  dots <- declare_in("call2", {
    new_function("dots", function(x = arg_double(), ...) x)
  })
  expect_error(r8schema(dots), "`...`", class = "r8_type_error")
})

test_that("r8check_call() reports every problem of a document", {
  clip <- clip_of("call3")
  ok <- r8check_call(
    clip,
    doc_of("call3", '{"x": [1, 2], "hi": 1, "n": 2, "w": null}')
  )
  expect_identical(nrow(ok), 0L)
  bad <- r8check_call(
    clip,
    doc_of("call3", '{"x": [1, "a"], "lo": "b", "zz": 1}')
  )
  expect_setequal(
    paste(bad[["code"]], bad[["path"]]),
    c(
      "unknown_argument /arguments/zz",
      "type_mismatch /arguments/x/1",
      "type_mismatch /arguments/lo",
      "missing_argument /arguments/hi",
      "missing_argument /arguments/w"
    )
  )
  # The rules, with a constant default of an omitted argument; a rule over an
  # argument whose default is written in R is unchecked.
  rules <- r8check_call(
    clip,
    doc_of("call3", '{"x": [1], "hi": -1, "w": null}')
  )
  expect_identical(rules[["code"]], "rule_violation")
  expect_identical(rules[["rule"]], "order")
  expect_identical(attr(rules, "unchecked")[["rule"]], "count")
  # A call that holds has no rows, and its unchecked rules apart.
  held <- r8check_call(clip, doc_of("call3", '{"x": [1], "hi": 1, "w": null}'))
  expect_identical(nrow(held), 0L)
  expect_identical(attr(held, "unchecked")[["code"]], "rule_unchecked")
  # A name from the document is written with its control characters escaped
  # and cut.
  long <- r8check_call(
    clip,
    paste0(
      '{"$function": "call3::',
      strrep("a", 5000),
      '\\u001b", "arguments": {}}'
    )
  )
  expect_lt(nchar(long[["message"]]), 300L)
  expect_false(grepl("\033", long[["message"]], fixed = TRUE))
  # A function that the call does not admit.
  other <- r8check_call(clip, '{"$function": "call3::other", "arguments": {}}')
  expect_identical(other[["code"]], "unknown_function")
  expect_error(r8check_call(list(), "{}"), class = "r8_type_error")
})

test_that("r8call_from_json() runs a call that holds and raises one that does not", {
  clip <- clip_of("call4")
  out <- r8call_from_json(
    clip,
    doc_of("call4", '{"x": [-1, 0.5, 3], "hi": 1, "w": {"lo": 0, "hi": 1}}')
  )
  expect_identical(out[["value"]], c(0, 0.5, 1))
  # Omitted arguments take their defaults, and the call shows names, not values.
  expect_identical(out[["n"]], 3L)
  expect_identical(deparse(out[["call"]]), "clip(x = x, hi = hi, w = w)")
  e <- expect_error(
    r8call_from_json(clip, doc_of("call4", '{"x": [1], "lo": 2, "hi": 1}')),
    class = "r8_argument_error"
  )
  expect_match(
    conditionMessage(e),
    "Invalid arguments of `clip()`",
    fixed = TRUE
  )
  expect_identical(e[["problems"]][["code"]], "missing_argument")
  # A rule that the document could not decide is checked by the function.
  e <- expect_error(
    r8call_from_json(clip, doc_of("call4", '{"x": [], "hi": 1, "w": null}')),
    class = "r8_argument_error"
  )
  expect_match(conditionMessage(e), "count", fixed = TRUE)
  # null is NULL: a parameter with a default that is not option() refuses it.
  e <- expect_error(
    r8call_from_json(
      clip,
      doc_of("call4", '{"x": [1], "lo": null, "hi": 1, "w": null}')
    ),
    class = "r8_argument_error"
  )
  expect_identical(e[["problems"]][["path"]], "/arguments/lo")
})

test_that("r8call_to_json() writes a document that reads back", {
  clip <- clip_of("call5")
  Range <- declare_in("call5", {
    new_struct("Range", fields = list(lo = type_double(), hi = type_double()))
  })
  doc <- r8call_to_json(clip, c(-1, 3), hi = 1, w = Range(lo = 0, hi = 1))
  expect_s3_class(doc, "json")
  expect_identical(
    unclass(doc),
    paste0(
      '{"$function":"call5::clip","$types":{"call5::Range":1},',
      '"arguments":{"hi":1,"w":{"hi":1,"lo":0},"x":[-1,3]}}'
    )
  )
  expect_identical(r8call_from_json(clip, doc)[["value"]], c(0, 1))
  # Arguments by position, every parameter included.
  expect_match(
    r8call_to_json(clip, 1, 0, 1, 1L, NULL, TRUE),
    '"verbose":true',
    fixed = TRUE
  )
  expect_error(
    r8call_to_json(clip, 1, lo = 2, hi = 1, n = 1L, w = NULL),
    "order",
    class = "r8_argument_error"
  )
  e <- expect_error(
    r8call_to_json(clip, "a", hi = 1, w = NULL),
    class = "r8_argument_error"
  )
  expect_identical(e[["problems"]][["path"]], "/arguments/x")
  expect_error(
    r8call_to_json(clip, 1, zz = 2),
    "unused argument",
    class = "r8_argument_error"
  )
  # A parameter named `f` is given by name.
  g <- declare_in("call5", new_function("g", function(f = arg_double()) f))
  expect_match(r8call_to_json(g, f = 1), '"arguments":{"f":1}', fixed = TRUE)
})

test_that("the roclet writes a described parameter's @param", {
  skip_if_not_installed("roxygen2")
  clip <- clip_of("call6")
  env <- new.env()
  assign("clip", clip, envir = env)
  block <- roxygen2::parse_text(
    "#' Clip\n#'\n#' @param hi Double: The upper bound.\nclip <- clip\n",
    env = env
  )[[1L]]
  out <- generic_block(block, list(block), env)
  params <- Filter(function(t) identical(t$tag, "param"), out$tags)
  texts <- stats::setNames(
    vapply(params, function(t) t$val$description, ""),
    vapply(params, function(t) t$val$name, "")
  )
  expect_identical(texts[["lo"]], "Double: The lower bound.")
  expect_identical(texts[["x"]], "Vector of double: The values.")
  expect_identical(texts[["verbose"]], "Logical: Print progress.")
  block <- roxygen2::parse_text(
    "#' Clip\n#'\n#' @param lo The bound.\nclip <- clip\n",
    env = env
  )[[1L]]
  expect_error(
    generic_block(block, list(block), env),
    "described parameter takes no @param",
    class = "r8_declaration_error"
  )
})

test_that("the schema writes each rule's text, as R writes the rule", {
  f <- declare_in("call7", {
    new_function(
      "f",
      function(
        lo = arg_double(),
        hi = arg_double(),
        v = arg_vec(type_double())
      ) {
        1
      },
      rules = list(
        order = rule(lo <= hi, message = "lo is at most hi."),
        rule(length(v) >= 1L && (lo + 1) * 2 <= hi),
        rule(all(v, .x > 0) || !is.null(v))
      )
    )
  })
  s <- gsub("[[:space:]]+", " ", r8schema(f))
  expect_match(s, '"text": "lo <= hi"', fixed = TRUE)
  expect_match(s, '"message": "lo is at most hi."', fixed = TRUE)
  # The text in Rust is the text that R writes of the same rule, but for the
  # integer suffix: the IR holds a number, which every host reads alike.
  for (r in r8_function_ir("call7::f")[["rules"]]) {
    expect_identical(
      r8_rule_text(r[["invariant"]]),
      gsub("([0-9])L\\b", "\\1", logic_text(r[["invariant"]]))
    )
  }
})
