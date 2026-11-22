# test-messages.R
# ::R8::
# 2026- EDG rtemis.org

# Validation messages: the subject in R syntax, the requirement, the value
# found, the nearest name, hints for R objects, and the header.
# spec: R8/class-system#validation-messages

test_that("a constraint names the field in R, the requirement, and the value found", {
  declare_in("msgform", {
    K <- new_enum(
      "K",
      variants = list(
        Linear = variant(),
        Rbf = variant(fields = list(gamma = type_double()))
      )
    )
    P <- new_struct(
      "P",
      fields = list(
        x = type_double(max = 0),
        s = type_character(values = c("linear", "rbf")),
        w = vec(type_double(min = 0)),
        m = hashmap(type_integer(min = 0L)),
        k = K
      )
    )
    e <- tryCatch(
      P(
        x = 4,
        s = "rbff",
        w = c(1, -2),
        m = list(a = 1L, b = -1L),
        k = K@Linear
      ),
      error = identity
    )
    m <- conditionMessage(e)
    expect_match(m, "Invalid `msgform::P`: 4 problems\n", fixed = TRUE)
    expect_match(
      m,
      "- `x` must be at most 0; found 4. [above_max]",
      fixed = TRUE
    )
    expect_match(
      m,
      "- `s` must be one of \"linear\" or \"rbf\"; found \"rbff\"; the nearest value is \"rbf\". [not_in_enum]",
      fixed = TRUE
    )
    expect_match(
      m,
      "- `w[[2]]` must be at least 0; found -2. [below_min]",
      fixed = TRUE
    )
    expect_match(
      m,
      "- `m[[\"b\"]]` must be at least 0; found -1. [below_min]",
      fixed = TRUE
    )
    p <- e$problems
    expect_identical(p$where, c("x", "s", "w[[2]]", "m[[\"b\"]]"))
    expect_identical(p$path, c("/x", "/s", "/w/1", "/m/b"))
    expect_identical(p$suggestion, c(NA, "rbf", NA, NA))
    json <- to_json(P(
      x = 0,
      s = "rbf",
      w = 1,
      m = list(a = 1L),
      k = K@Rbf(gamma = 1)
    ))
    bad <- sub('"type":"Rbf"', '"type":"Rbff"', json, fixed = TRUE)
    e <- tryCatch(r8_from_json(P, bad), error = identity)
    expect_match(
      conditionMessage(e),
      "`k` must be a variant of `msgform::K`: \"Linear\" or \"Rbf\"; found \"Rbff\"; the nearest value is \"Rbf\".",
      fixed = TRUE
    )
  })
})

test_that("an unknown field names the nearest field, or lists the fields", {
  declare_in("msgunknown", {
    P <- new_struct(
      "P",
      fields = list(values = vec(type_double()), lo = type_double())
    )
    json <- to_json(P(values = 1, lo = 1))
    e <- tryCatch(
      r8_from_json(P, sub('"values"', '"valeus"', json, fixed = TRUE)),
      error = identity
    )
    expect_match(
      conditionMessage(e),
      "`valeus` is not a field of `msgunknown::P`; the nearest field is `values`.",
      fixed = TRUE
    )
    e <- tryCatch(
      r8_from_json(P, sub('"lo":1', '"lo":1,"zz":2', json, fixed = TRUE)),
      error = identity
    )
    expect_match(
      conditionMessage(e),
      "`zz` is not a field of `msgunknown::P`, whose fields are `values` and `lo`.",
      fixed = TRUE
    )
  })
})

test_that("a sensitive field's value is withheld from constraint messages", {
  declare_in("msgsensitive", {
    P <- new_struct(
      "P",
      fields = list(
        token = field(type_character(pattern = "^[a-z]+$"), sensitive = TRUE),
        dose = field(type_double(max = 10), sensitive = TRUE)
      )
    )
    e <- tryCatch(P(token = "SECRET-123", dose = 99), error = identity)
    m <- conditionMessage(e)
    expect_false(grepl("SECRET", m, fixed = TRUE))
    expect_false(grepl("99", m, fixed = TRUE))
    expect_match(
      m,
      "`token` must match \"^[a-z]+$\"; found <sensitive>.",
      fixed = TRUE
    )
    expect_match(
      m,
      "`dose` must be at most 10; found <sensitive>.",
      fixed = TRUE
    )
  })
})

test_that("a long string is cut in a message", {
  declare_in("msgcut", {
    P <- new_struct("P", fields = list(s = type_character(pattern = "^a$")))
    e <- tryCatch(P(s = strrep("b", 100)), error = identity)
    expect_match(
      conditionMessage(e),
      paste0("found \"", strrep("b", 40), "...\""),
      fixed = TRUE
    )
  })
})

test_that("an R object gets a hint for a lossless conversion", {
  declare_in("msghints", {
    P <- new_struct(
      "P",
      fields = list(n = type_integer(), s = type_character())
    )
    e <- tryCatch(P(n = NA_integer_, s = "a"), error = identity)
    expect_match(
      conditionMessage(e),
      "`n` must be a value; found NA. `option()` admits a missing value. [na_value]",
      fixed = TRUE
    )
    expect_identical(e$problems$hint, "`option()` admits a missing value.")
    e <- tryCatch(P(n = 1L, s = factor("a")), error = identity)
    expect_match(
      conditionMessage(e),
      "`as.character()` gives its labels.",
      fixed = TRUE
    )
    e <- tryCatch(P(n = 1:2, s = "a"), error = identity)
    expect_match(
      conditionMessage(e),
      "`n` must be one value; found length 2.",
      fixed = TRUE
    )
    e <- tryCatch(P(n = 1L, s = 1), error = identity)
    expect_match(
      conditionMessage(e),
      "`s` must be a character vector; found a numeric vector.",
      fixed = TRUE
    )
  })
})

test_that("a closing line says when rules were not evaluated, and $<- names its assignment", {
  declare_in("msgskipped", {
    P <- new_struct(
      "P",
      fields = list(lo = type_double(max = 5), hi = type_double()),
      rules = rule(lo < hi)
    )
    e <- tryCatch(P(lo = 9, hi = 10), error = identity)
    expect_match(
      conditionMessage(e),
      "\nRules were not evaluated: 1 field is invalid.",
      fixed = TRUE
    )
    p <- P(lo = 1, hi = 2)
    e <- tryCatch(p$lo <- 3, error = identity)
    expect_match(
      conditionMessage(e),
      "Invalid `msgskipped::P` from `x$lo <- value`: 1 problem\n- Rule: lo < hi; found lo = 3, hi = 2.",
      fixed = TRUE
    )
    e <- tryCatch(p$lo <- 8, error = identity)
    expect_match(
      conditionMessage(e),
      "- `lo` must be at most 5; found 8. [above_max]",
      fixed = TRUE
    )
  })
})

test_that("a rule violation in a map names the key or the item in R", {
  declare_in("msgmappaths", {
    H <- new_struct(
      "H",
      fields = list(m = hashmap(type_double())),
      rules = rule(all(m, .x > 0), id = "POS")
    )
    e <- tryCatch(H(m = list(a = 1, `2` = -1, `a/b` = -2)), error = identity)
    expect_identical(e$problems$where, c('m[["2"]]', 'm[["a/b"]]'))
    expect_identical(e$problems$path, c("/m/2", "/m/a~1b"))
    expect_match(conditionMessage(e), '.x = -1 at m[["2"]]', fixed = TRUE)
    O <- new_struct(
      "O",
      fields = list(om = ordered_map(type_double())),
      rules = rule(all(om, .x > 0), id = "POS")
    )
    e <- tryCatch(O(om = list(a = 1, b = -1)), error = identity)
    expect_identical(e$problems$where, "om[[2]]")
    expect_identical(e$problems$path, "/om/1/value")
    expect_match(conditionMessage(e), ".x = -1 at om[[2]]", fixed = TRUE)
  })
})

test_that("list() is the empty map, and NULL stays absent", {
  declare_in("msgemptymap", {
    M <- new_struct(
      "M",
      fields = list(
        h = hashmap(type_double()),
        o = ordered_map(type_character()),
        oh = option(hashmap(type_double()))
      )
    )
    v <- M(h = list(), o = list())
    expect_length(v$h, 0L)
    expect_identical(names(v$o), character())
    expect_null(v$oh)
    expect_true(r8_equal(v, r8_from_json(M, to_json(v))))
    v$h <- list()
    expect_length(v$h, 0L)
    for (x in list(numeric(), character())) {
      expect_error(
        M(h = x, o = list()),
        "`list()` is the empty map",
        fixed = TRUE
      )
    }
    expect_error(M(h = NULL, o = list()), "`h` is required.", fixed = TRUE)
  })
})
