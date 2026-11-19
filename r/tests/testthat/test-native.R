# test-native.R
# ::R8::
# 2026- EDG rtemis.org

# The differential test of construction: r8_new_value() validates R objects
# in place and keeps them where they are in their stored forms;
# r8_new_value_json() is the reference, which encodes to JSON, validates, and
# decodes. Both give identical values for valid input, and the same codes,
# paths, and rule IDs for invalid input.

# Constructs `fields` as a value of `type` on both paths and compares them.
# Errors of the value's meaning (bounds, levels, rules, and the others) are
# compared by code, path, and rule ID. Errors of the R form (`r_*` and
# `na_value`) have no counterpart in the JSON form, and the reference's
# encoder may report one problem twice; for these, both paths report the
# same fields.
same_construction <- function(type, fields, variant = "") {
  qname <- attr(type, "r8_name")
  native <- R8:::r8_new_value(fields, qname, variant)
  ref <- R8:::r8_new_value_json(fields, qname, variant)
  if (is.null(ref[["errors"]])) {
    expect_false(inherits(native, "r8_problems"), info = qname)
    expect_identical(native, ref[["value"]], info = qname)
    same_identity(native)
    return(invisible(native))
  }
  expect_s3_class(native, "r8_problems")
  ne <- native[["errors"]]
  re <- ref[["errors"]]
  r_form <- function(code) grepl("^r_", code) | code == "na_value"
  key <- function(e, keep) {
    sort(paste(e[["code"]], e[["path"]], e[["rule"]])[keep])
  }
  expect_identical(
    key(ne, !r_form(ne[["code"]])),
    key(re, !r_form(re[["code"]])),
    info = qname
  )
  field_of <- function(path) unique(sub("^(/[^/]*).*$", "\\1", path))
  expect_identical(
    sort(field_of(ne[["path"]][r_form(ne[["code"]])])),
    sort(field_of(re[["path"]][r_form(re[["code"]])])),
    info = qname
  )
  invisible(native)
}

# The canonical text and content hash written from the R objects equal
# those of the value's document read through the JSON form.
same_identity <- function(x) {
  native <- R8:::r8_value_identity(x)
  ref <- R8:::r8_document_identity(to_json(x), attr(x, "r8_type"))
  expect_identical(
    native[["canonical"]],
    ref[["canonical"]],
    info = attr(x, "r8_type")
  )
  expect_identical(native[["hash"]], ref[["hash"]], info = attr(x, "r8_type"))
  expect_true(r8_equal(x, r8_from_json(get_type(x), to_json(x))))
}

get_type <- function(x) R8:::.r8_types[[attr(x, "r8_type")]]

test_that("scalars construct identically on both paths", {
  S <- declare_in(
    "native",
    new_struct(
      "Scalars",
      fields = list(
        l = type_logical(),
        i = type_integer(min = 0L),
        d = type_double(min = 0, max = 10),
        s = type_character(pattern = "^[a-z]+$"),
        dt = type_date(),
        ct = type_posixct(),
        o = option(type_double()),
        dflt = field(type_integer(), default = 7L)
      )
    )
  )
  base <- list(
    l = TRUE,
    i = 3L,
    d = 2.5,
    s = "abc",
    dt = as.Date("2024-01-02"),
    ct = as.POSIXct("2024-01-02 03:04:05", tz = "UTC"),
    o = NULL,
    dflt = NULL
  )
  same_construction(S, base)
  # Accepted forms that the stored form converts.
  same_construction(S, modifyList(base, list(i = 3, d = 2L)))
  same_construction(
    S,
    modifyList(base, list(dt = structure(19724L, class = "Date")))
  )
  same_construction(
    S,
    modifyList(
      base,
      list(ct = as.POSIXct(1704164645.1234567, tz = "America/New_York"))
    )
  )
  # Invalid values.
  same_construction(S, modifyList(base, list(i = -1L)))
  same_construction(S, modifyList(base, list(d = 11)))
  same_construction(S, modifyList(base, list(s = "ABC")))
  same_construction(S, modifyList(base, list(l = NA)))
  same_construction(S, modifyList(base, list(i = 2.5)))
  same_construction(S, modifyList(base, list(d = c(1, 2))))
  same_construction(S, modifyList(base, list(d = c(a = 1))))
  same_construction(S, modifyList(base, list(dt = 19724)))
  same_construction(S, modifyList(base, list(l = NULL)))
})

test_that("vectors construct identically and keep the input", {
  V <- declare_in(
    "native",
    new_struct(
      "Vectors",
      fields = list(
        d = vec(type_double(min = 0)),
        i = vec(type_integer()),
        s = vec(type_character()),
        od = vec(option(type_double())),
        u = vec(type_integer(), unique = TRUE),
        n = vec(type_double(), min_length = 2L),
        dt = vec(type_date())
      )
    )
  )
  x <- runif(100)
  base <- list(
    d = x,
    i = 1:3,
    s = c("a", "b"),
    od = c(1, NA, 3),
    u = c(1L, 2L, 3L),
    n = c(1, 2),
    dt = as.Date(c("2024-01-01", "2024-02-01"))
  )
  v <- same_construction(V, base)
  # A field in its stored form is the input object itself.
  expect_identical(.Internal(address(unclass(v)[["d"]])), .Internal(address(x)))
  same_construction(V, modifyList(base, list(d = 1:3, i = c(1, 2))))
  same_construction(V, modifyList(base, list(d = c(1, -1, -2))))
  same_construction(V, modifyList(base, list(d = c(1, NA))))
  same_construction(V, modifyList(base, list(u = c(1L, 2L, 1L))))
  same_construction(V, modifyList(base, list(n = 1)))
  same_construction(V, modifyList(base, list(i = c(1.5, 2))))
  same_construction(V, modifyList(base, list(s = 1:2)))
})

test_that("factors, maps, and JSON construct identically", {
  F <- declare_in(
    "native",
    new_struct(
      "Collections",
      fields = list(
        f = type_factor(levels = c("lo", "mid", "hi")),
        fd = type_factor(),
        h = hashmap(type_double()),
        hl = hashmap(vec(type_integer())),
        om = ordered_map(type_character()),
        j = type_json()
      )
    )
  )
  base <- list(
    f = factor(c("lo", "hi"), levels = c("lo", "mid", "hi")),
    fd = factor(c("b", "a")),
    h = c(b = 2, a = 1),
    hl = list(z = 1:2, y = 3L),
    om = c(second = "x", first = "y"),
    j = list(a = 1L, b = list(TRUE, "s"))
  )
  same_construction(F, base)
  same_construction(F, modifyList(base, list(f = factor(c("hi", "lo")))))
  same_construction(F, modifyList(base, list(h = list(b = 2, a = 1L))))
  same_construction(F, modifyList(base, list(f = factor(c("lo", "top")))))
  same_construction(F, modifyList(base, list(h = c(1, 2))))
  same_construction(F, modifyList(base, list(j = list(a = c(1, 2)))))
})

test_that("matrices and tables construct identically", {
  M <- declare_in(
    "native",
    new_struct(
      "Arrays",
      fields = list(
        m = type_matrix(type_double(), ncol = 2L),
        a = type_array(type_integer())
      )
    )
  )
  base <- list(
    m = matrix(as.numeric(1:6), ncol = 2),
    a = array(1:8, dim = c(2, 2, 2))
  )
  same_construction(M, base)
  same_construction(
    M,
    modifyList(
      base,
      list(
        m = matrix(
          1:6,
          ncol = 2,
          dimnames = list(r = letters[1:3], c = c("x", "y"))
        )
      )
    )
  )
  same_construction(
    M,
    modifyList(base, list(m = matrix(as.numeric(1:6), ncol = 3)))
  )
  T <- declare_in(
    "native",
    new_struct(
      "Tables",
      fields = list(
        t = type_table(
          columns = list(
            x = type_double(min = 0),
            g = type_factor(levels = c("a", "b")),
            o = option(type_integer()),
            d = type_date()
          ),
          rules = list(rule("pos", x < 100, "x is below 100"))
        ),
        open = type_table(columns = list(id = type_integer()), open = TRUE)
      )
    )
  )
  df <- data.frame(
    x = c(1, 2),
    g = factor(c("a", "b")),
    o = c(1L, NA),
    d = as.Date(c("2024-01-01", "2024-01-02"))
  )
  tbase <- list(t = df, open = data.frame(id = 1:2, extra = c("p", "q")))
  same_construction(T, tbase)
  df2 <- df
  df2$x <- 1:2
  same_construction(T, modifyList(tbase, list(t = df2)))
  df3 <- df
  df3$x <- c(1, 200)
  same_construction(T, modifyList(tbase, list(t = df3)))
  df4 <- df
  df4$x <- c(-1, 2)
  same_construction(T, modifyList(tbase, list(t = df4)))
  same_construction(T, modifyList(tbase, list(t = df[, c("x", "g", "d")])))
})

test_that("nested values, enums, and rules construct identically", {
  Inner <- declare_in(
    "native",
    new_struct("Inner", fields = list(v = type_double()))
  )
  Kind <- declare_in(
    "native",
    new_enum(
      "Kind",
      variants = list(
        Plain = variant(),
        Scaled = variant(fields = list(factor = type_double(exclusive_min = 0)))
      )
    )
  )
  Outer <- declare_in(
    "native",
    new_struct(
      "Outer",
      fields = list(
        inner = Inner,
        items = vec(Inner),
        kind = Kind,
        lo = type_double(),
        hi = type_double()
      ),
      rules = list(rule("ordered", lo <= hi, "lo is at most hi"))
    )
  )
  base <- list(
    inner = Inner(v = 1),
    items = list(Inner(v = 2), Inner(v = 3)),
    kind = Kind@Scaled(factor = 2),
    lo = 1,
    hi = 2
  )
  same_construction(Outer, base)
  same_construction(Outer, modifyList(base, list(lo = 3)))
  same_construction(Outer, modifyList(base, list(kind = Kind@Plain)))
  same_construction(Outer, modifyList(base, list(inner = list(v = 1))))
  same_construction(Kind, list(factor = 2), "Scaled")
  same_construction(Kind, list(factor = -1), "Scaled")
})

test_that("$<- validates the field and the rules that read it", {
  R <- declare_in(
    "native",
    new_struct(
      "Ranged",
      fields = list(
        lo = type_double(),
        hi = type_double(),
        big = vec(type_double())
      ),
      rules = list(rule("ordered", lo <= hi, "lo is at most hi"))
    )
  )
  x <- runif(10)
  r <- R(lo = 1, hi = 2, big = x)
  r$lo <- 1.5
  expect_identical(r$lo, 1.5)
  # The other fields are shared with the original value.
  expect_identical(
    .Internal(address(unclass(r)[["big"]])),
    .Internal(address(x))
  )
  expect_error(r$lo <- 3, class = "r8_value_error")
  expect_error(r$lo <- "a", class = "r8_value_error")
  expect_error(r$nope <- 1, class = "r8_value_error")
  r$hi <- 5L
  expect_identical(r$hi, 5)
})

test_that("r8_equal() compares normalized forms in place", {
  E <- declare_in(
    "native",
    new_struct(
      "Eq",
      fields = list(
        d = vec(option(type_double(finite = FALSE))),
        h = hashmap(type_integer()),
        n = field(type_double(), default = 1)
      )
    )
  )
  a <- E(d = c(0, NA, NaN), h = c(a = 1L, b = 2L))
  expect_true(r8_equal(a, E(d = c(-0, NA, NaN), h = c(b = 2L, a = 1L))))
  expect_true(r8_equal(
    a,
    E(d = c(0L, NA, NaN), h = list(a = 1L, b = 2L), n = 1)
  ))
  expect_false(r8_equal(a, E(d = c(0, NaN, NaN), h = c(a = 1L, b = 2L))))
  expect_false(r8_equal(a, E(d = c(0, NA, NaN), h = c(a = 1L, b = 3L))))
  expect_false(r8_equal(a, E(d = c(0, NA, NaN), h = c(a = 1L, b = 2L), n = 2)))
  expect_identical(
    r8_hash(a),
    r8_hash(E(d = c(-0, NA, NaN), h = c(b = 2L, a = 1L)))
  )
  same_identity(a)
})
