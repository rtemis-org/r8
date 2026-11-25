# test-102-rules.R
# ::R8::
# 2026- EDG rtemis.org

# Rules: conditions that a value as a whole satisfies, such as lo <= hi.
#
# These tests check how a rule is written in R, how an error in one is
# reported, and that rules on one field, across fields, and under a
# condition hold or report a violation.

test_that("rules takes one rule() or a list of them", {
  declare_in("rulesone", {
    S <- new_struct(
      "S",
      fields = list(lo = type_double(), hi = type_double()),
      rules = rule(lo < hi, message = "lo is below hi.", id = "LO_HI")
    )
    expect_s3_class(S(lo = 1, hi = 2), "r8_value")
    expect_error(
      S(lo = 2, hi = 1),
      "Rule LO_HI: lo is below hi.",
      class = "r8_value_error"
    )
    expect_error(
      new_struct(
        "T",
        fields = list(lo = type_double()),
        rules = list(rule(lo > 0, message = "m", id = "A"), "lo > 0")
      ),
      "`rules[[2]]` is not a rule()",
      fixed = TRUE,
      class = "r8_declaration_error"
    )
  })
})

test_that("a one-argument all() or any() writes the two-argument form", {
  expect_error(
    rule(all(values > 0), message = "m", id = "POS"),
    "write `all(values, .x > 0)`",
    fixed = TRUE,
    class = "r8_declaration_error"
  )
  expect_error(
    rule(any(kernel$gammas >= 1), message = "m", id = "ANY"),
    "write `any(kernel$gammas, .x >= 1)`",
    fixed = TRUE,
    class = "r8_declaration_error"
  )
  expect_error(
    rule(all(a < b), message = "m", id = "PAIR"),
    "`all(x, <condition on .x>)`",
    fixed = TRUE,
    class = "r8_declaration_error"
  )
})

test_that("val() stores the value of an R expression, evaluated where rule() is called", {
  make <- function(cap) {
    rule(cost <= val(cap), message = "cost is at most cap.", id = "CAP")
  }
  r <- make(100)
  expect_identical(r[["invariant"]][["<="]][[2L]], 100)
  allowed <- c("linear", "rbf")
  r <- rule(kernel %in% val(allowed), message = "m", id = "K")
  expect_identical(unlist(r[["invariant"]][["in"]][[2L]]), allowed)
  expect_identical(
    rule(kernel %in% R8::val(allowed), message = "m", id = "K")[["invariant"]],
    r[["invariant"]]
  )
  r <- rule(cost <= 1, when = n > val(length(allowed)), message = "m", id = "W")
  expect_identical(r[["when"]][[">"]][[2L]], 2L)
  na <- NA_real_
  expect_error(
    rule(cost <= val(na), message = "m", id = "NA"),
    "is not a constant of a rule",
    class = "r8_declaration_error"
  )
  expect_error(
    rule(cost <= val(mean), message = "m", id = "F"),
    "is not a constant of a rule",
    class = "r8_declaration_error"
  )
  expect_error(
    rule(cost <= val(val(1)), message = "m", id = "N"),
    "val() does not nest",
    fixed = TRUE,
    class = "r8_declaration_error"
  )
  expect_error(
    rule(cost <= val(x = 1), message = "m", id = "A"),
    "one unnamed argument",
    class = "r8_declaration_error"
  )
  expect_identical(val(3), 3)
})

test_that("a name that is not a field names the nearest field and val()", {
  declare_in("rulesnear", {
    expect_error(
      new_struct(
        "S",
        fields = list(values = vec(type_double())),
        rules = rule(length(valeus) > 0, message = "m", id = "A")
      ),
      "the nearest field is `values`; if `valeus` is an R variable, write `val(valeus)`",
      fixed = TRUE,
      class = "r8_declaration_error"
    )
  })
  expect_error(
    rule(lenght(values) > 0, message = "m", id = "A"),
    "the nearest is `length()`",
    fixed = TRUE,
    class = "r8_declaration_error"
  )
})

test_that("abs(), sum(), nchar(), and min() and max() of a vector evaluate", {
  declare_in("rulesfns", {
    S <- new_struct(
      "S",
      fields = list(
        x = type_double(),
        p = vec(type_double()),
        k = vec(type_integer()),
        name = type_character()
      ),
      rules = list(
        rule(abs(x) <= 1, message = "x lies in [-1, 1].", id = "ABS"),
        rule(abs(sum(p) - 1) <= 1e-9, message = "p sums to 1.", id = "SUM"),
        rule(
          min(k) >= 0 && max(k) <= 9,
          message = "k lies in [0, 9].",
          id = "RANGE"
        ),
        rule(
          nchar(name) <= 3,
          message = "name has at most 3 characters.",
          id = "NCHAR"
        )
      )
    )
    ok <- list(x = -0.5, p = c(0.1, 0.2, 0.7), k = c(0L, 9L), name = "aéb")
    expect_s3_class(do.call(S, ok), "r8_value")
    bad <- function(...) {
      args <- utils::modifyList(ok, list(...))
      tryCatch(do.call(S, args), error = function(e) e$problems$rule)
    }
    expect_identical(bad(x = 1.5), "ABS")
    expect_identical(bad(p = c(0.5, 0.6)), "SUM")
    expect_identical(bad(p = numeric()), "SUM")
    expect_identical(bad(k = c(3L, -1L)), "RANGE")
    expect_identical(bad(name = "abcd"), "NCHAR")
  })
})

test_that("strict equality of computed doubles is a declaration error", {
  declare_in("rulesfloat", {
    expect_error(
      new_struct(
        "S",
        fields = list(a = type_double(), b = type_double()),
        rules = rule(a + b == 1, message = "m", id = "SUM")
      ),
      "abs(a - b) <= 1e-9",
      fixed = TRUE,
      class = "r8_declaration_error"
    )
    expect_s3_class(
      new_struct(
        "T",
        fields = list(
          a = type_integer(),
          b = type_integer(),
          c = type_double()
        ),
        rules = list(
          rule(a + b == 3L, message = "m", id = "INT"),
          rule(c == 0.5, message = "m", id = "LIT")
        )
      ),
      "r8_struct_type"
    )
  })
})

test_that("a declaration error in a rule shows the R expression", {
  declare_in("rulesdecl", {
    expect_error(
      new_struct(
        "S",
        fields = list(values = vec(type_double()), lo = type_double()),
        rules = list(
          rule(lo > 0, message = "m", id = "A"),
          rule(values > lo, message = "m", id = "B")
        )
      ),
      "in `values > lo`",
      fixed = TRUE,
      class = "r8_declaration_error"
    )
  })
})

test_that("a violation shows the rule in R and the values it read", {
  declare_in("rulesshow", {
    S <- new_struct(
      "S",
      fields = list(
        lo = type_double(),
        hi = type_double(),
        values = vec(type_double()),
        secret = field(type_double(), sensitive = TRUE)
      ),
      rules = list(
        rule(lo < hi, message = "lo is below hi.", id = "LO_HI"),
        rule(all(values, .x > 0), message = "values are positive.", id = "POS"),
        rule(secret < 10, message = "secret is below 10.", id = "SECRET")
      )
    )
    e <- tryCatch(
      S(lo = 2, hi = 1, values = c(1, -1, 3, -2, -5, -6, -7), secret = 12),
      error = identity
    )
    m <- conditionMessage(e)
    expect_match(m, "  lo < hi; found lo = 2, hi = 1", fixed = TRUE)
    expect_match(
      m,
      "[rule_violation at values[[2]], values[[4]], values[[5]], and 2 more]",
      fixed = TRUE
    )
    expect_match(
      m,
      "all(values, .x > 0); found length(values) = 7, .x = -1 at values[[2]]",
      fixed = TRUE
    )
    expect_match(m, "secret = <sensitive>", fixed = TRUE)
    expect_false(grepl("12", m, fixed = TRUE))
    expect_identical(
      e$problems$rule,
      c("LO_HI", rep("POS", 5L), "SECRET")
    )
    expect_identical(e$problems$diagnosis[[1L]]$fields, c("lo = 2", "hi = 1"))
    op <- options(r8.show_values = FALSE)
    on.exit(options(op), add = TRUE)
    e <- tryCatch(
      S(lo = 2, hi = 1, values = 1, secret = 1),
      error = identity
    )
    expect_match(conditionMessage(e), "  lo < hi\n?$")
    expect_null(e$problems$diagnosis)
  })
})

test_that("the generator renders the element .x of all() as R", {
  r <- rule(all(train, all(.x, .x <= n_cases)), message = "m", id = "A")
  expect_identical(
    R8:::render_logic(r[["invariant"]]),
    "all(train, all(.x, .x <= n_cases))"
  )
  r <- rule(all(items, .x$w > 0), message = "m", id = "B")
  expect_identical(R8:::render_logic(r[["invariant"]]), "all(items, .x$w > 0)")
})

test_that("a rule needs neither an ID nor a message", {
  declare_in("rulesbare", {
    U <- new_struct(
      "U",
      fields = list(lo = type_double(), hi = type_double()),
      rules = list(
        rule(lo < hi),
        rule(lo > 0, message = "lo is positive."),
        rule(hi < 10, id = "HI_MAX")
      )
    )
    e <- tryCatch(U(lo = -2, hi = -3), error = identity)
    m <- conditionMessage(e)
    expect_match(
      m,
      "- Rule: lo < hi; found lo = -2, hi = -3. [rule_violation]",
      fixed = TRUE
    )
    expect_match(
      m,
      "- Rule: lo is positive. [rule_violation]\n  lo > 0; found lo = -2",
      fixed = TRUE
    )
    expect_identical(e$problems$rule, c(NA_character_, NA_character_))
    expect_identical(e$problems$rule_index, c(0L, 1L))
    e <- tryCatch(U(lo = 1, hi = 20), error = identity)
    expect_match(
      conditionMessage(e),
      "- Rule HI_MAX: hi < 10; found hi = 20. [rule_violation]",
      fixed = TRUE
    )
    expect_identical(e$problems$rule, "HI_MAX")
    expect_identical(e$problems$rule_index, 2L)
    expect_error(
      new_struct(
        "V",
        fields = list(a = type_double()),
        rules = list(rule(a > 0), rule(a > 0, message = "a is positive."))
      ),
      "rule 1 repeats rule 0",
      class = "r8_declaration_error"
    )
  })
  expect_error(
    rule("LO_HI", lo < hi, "lo is below hi."),
    'rule(lo < hi, message = "lo is below hi.", id = "LO_HI")',
    fixed = TRUE,
    class = "r8_declaration_error"
  )
  expect_error(
    rule(lo < hi, message = " "),
    "non-empty",
    class = "r8_declaration_error"
  )
  expect_error(
    rule(lo < hi, id = "lo hi"),
    "R8 name",
    class = "r8_declaration_error"
  )
})

test_that("the generator writes a rule's optional members by name", {
  r <- rule(lo < hi, when = lo > 0, message = "lo is below hi.", id = "LO_HI")
  expect_identical(
    R8:::render_rules(list(r), ""),
    'list(R8::rule(lo < hi, when = lo > 0, message = "lo is below hi.", id = "LO_HI"))'
  )
  expect_identical(
    R8:::render_rules(list(rule(lo < hi)), ""),
    "list(R8::rule(lo < hi))"
  )
})

test_that("the name of a rule in a list is its ID", {
  ids <- function(type) {
    vapply(
      r8_type_ir(attr(type, "r8_name"))[["rules"]],
      function(r) r[["id"]] %||% "",
      ""
    )
  }
  fields <- list(lo = type_double(), hi = type_double())
  S <- declare_in("rn1", {
    new_struct(
      "S",
      fields = fields,
      rules = list(
        order = rule(lo <= hi),
        rule(lo > 0),
        rule(hi < 9, id = "TOP")
      )
    )
  })
  expect_identical(ids(S), c("order", "", "TOP"))
  expect_error(S(lo = 2, hi = 1), "Rule order: lo <= hi")
  # One rule in two types, with a name of each.
  shared <- rule(lo <= hi)
  A <- declare_in("rn2", {
    new_struct("A", fields = fields, rules = list(a = shared))
  })
  B <- declare_in("rn2", {
    new_table(
      "B",
      columns = fields,
      rules = list(b = shared, shared_too = rule(lo > 0))
    )
  })
  expect_identical(ids(A), "a")
  expect_identical(ids(B), c("b", "shared_too"))
  expect_null(shared[["id"]])
  # Every declaration of rules reads them so.
  expect_identical(
    type_table(fields, rules = list(t = shared))$rules[[1L]]$id,
    "t"
  )
  E <- declare_in("rn3", {
    new_enum(
      "E",
      variants = list(V = variant(fields = fields, rules = list(v = shared)))
    )
  })
  expect_identical(
    r8_type_ir("rn3::E")[["variants"]][[1L]][["rules"]][[1L]][["id"]],
    "v"
  )
  bad <- function(rules, text) {
    fields <- list(lo = type_double(), hi = type_double())
    expect_error(
      declare_in("rn4", new_struct("S", fields = fields, rules = rules)),
      text,
      fixed = TRUE,
      class = "r8_declaration_error"
    )
  }
  bad(
    list(order = rule(lo <= hi, id = "LO_HI")),
    "The rule named `order` in `rules` has the ID `LO_HI`; give one of the two."
  )
  bad(list(`lo hi` = rule(lo <= hi)), "is its ID, an R8 name")
  bad(list(a = rule(lo <= hi), a = rule(lo > 0)), "duplicate_rule")
  bad(list(a = rule(lo <= hi), rule(lo > 0, id = "a")), "duplicate_rule")
  # A child's rule does not take the ID of its parent's.
  P <- declare_in("rn5", {
    new_struct("P", fields = fields, rules = list(order = rule(lo <= hi)))
  })
  expect_error(
    declare_in("rn5", {
      new_struct("C", extends = P, rules = list(order = rule(lo < hi)))
    }),
    class = "r8_declaration_error"
  )
  # A name is no part of a value: one made before is of the type after.
  x <- declare_in("rn6", {
    T <- new_struct("T", fields = fields, rules = rule(lo <= hi))
    T(lo = 1, hi = 2)
  })
  T <- declare_in("rn6", {
    new_struct("T", fields = fields, rules = list(order = rule(lo <= hi)))
  })
  expect_true(r8_equal(x, T(lo = 1, hi = 2)))
})
