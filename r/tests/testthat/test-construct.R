# test-construct.R
# ::R8::
# 2026- EDG rtemis.org

# The differential test of construction: r8_new_value() validates R objects
# in place and keeps them where they are in their stored forms;
# the differential oracle (helper-oracle.R) is the reference, which encodes to
# JSON, validates, and decodes. Both give identical values for valid input, and the same codes,
# paths, and rule IDs for invalid input.

# Constructs `fields` as a value of `type` on both paths and compares them.
# Errors of the value's meaning (bounds, levels, rules, and the others) are
# compared by code, path, and rule ID. Errors of the R form (`r_*` and
# `na_value`) have no counterpart in the JSON form, and the reference's
# encoder may report one problem twice; for these, both paths report the
# same fields.
same_construction <- function(type, fields, variant = "") {
  skip_without_oracle()
  qname <- attr(type, "r8_name")
  in_place <- R8:::r8_new_value(fields, qname, variant)
  ref <- oracle_new_value(fields, qname, variant)
  if (is.null(ref[["errors"]])) {
    expect_false(inherits(in_place, "r8_problems"), info = qname)
    expect_identical(in_place, ref[["value"]], info = qname)
    same_identity(in_place)
    return(invisible(in_place))
  }
  expect_s3_class(in_place, "r8_problems")
  ne <- in_place[["errors"]]
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
  invisible(in_place)
}

# The canonical text and content hash written from the R objects equal
# those of the value's document read through the JSON form.
same_identity <- function(x) {
  skip_without_oracle()
  in_place <- R8:::r8_value_identity(x)
  ref <- oracle_document_identity(r8_to_json(x), attr(x, "r8_type"))
  expect_identical(
    in_place[["canonical"]],
    ref[["canonical"]],
    info = attr(x, "r8_type")
  )
  expect_identical(in_place[["hash"]], ref[["hash"]], info = attr(x, "r8_type"))
  expect_true(r8_equal(x, r8_from_json(get_type(x), r8_to_json(x))))
}

get_type <- function(x) R8:::.r8_types[[attr(x, "r8_type")]]

test_that("scalars construct identically on both paths", {
  S <- declare_in(
    "construct",
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
    "construct",
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
    "construct",
    new_struct(
      "Collections",
      fields = list(
        f = type_factor(levels = c("lo", "mid", "hi")),
        fd = type_factor(),
        h = btree_map(type_double()),
        hl = btree_map(vec(type_integer())),
        om = index_map(type_character()),
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

test_that("a factor code outside the levels is reported where it occurs", {
  F <- declare_in(
    "construct",
    new_struct(
      "Codes",
      fields = list(
        f = type_factor(levels = c("lo", "hi")),
        fd = type_factor()
      )
    )
  )
  # structure() sets the codes and levels without checking them.
  bad <- structure(c(1L, 3L, 0L), levels = c("lo", "hi"), class = "factor")
  base <- list(f = factor("lo", levels = c("lo", "hi")), fd = factor("a"))
  in_place <- same_construction(F, modifyList(base, list(f = bad)))
  expect_identical(in_place[["errors"]][["code"]], rep("invalid_code", 2L))
  expect_identical(in_place[["errors"]][["path"]], c("/f/1", "/f/2"))
  in_place <- same_construction(F, modifyList(base, list(fd = bad)))
  expect_identical(
    in_place[["errors"]][["path"]],
    c("/fd/codes/1", "/fd/codes/2")
  )
})

test_that("matrices and tables construct identically", {
  M <- declare_in(
    "construct",
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
    "construct",
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
          rules = list(rule(x < 100, message = "x is below 100", id = "pos"))
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
    "construct",
    new_struct("Inner", fields = list(v = type_double()))
  )
  Kind <- declare_in(
    "construct",
    new_enum(
      "Kind",
      variants = list(
        Plain = variant(),
        Scaled = variant(fields = list(factor = type_double(exclusive_min = 0)))
      )
    )
  )
  Outer <- declare_in(
    "construct",
    new_struct(
      "Outer",
      fields = list(
        inner = Inner,
        items = vec(Inner),
        kind = Kind,
        lo = type_double(),
        hi = type_double()
      ),
      rules = list(rule(lo <= hi, message = "lo is at most hi", id = "ordered"))
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
    "construct",
    new_struct(
      "Ranged",
      fields = list(
        lo = type_double(),
        hi = type_double(),
        big = vec(type_double())
      ),
      rules = list(rule(lo <= hi, message = "lo is at most hi", id = "ordered"))
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
    "construct",
    new_struct(
      "Eq",
      fields = list(
        d = vec(option(type_double(finite = FALSE))),
        h = btree_map(type_integer()),
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

test_that("== compares maps and matrices in place and keeps string literals", {
  P <- declare_in(
    "construct",
    new_struct(
      "Pairs",
      fields = list(
        a = btree_map(type_double()),
        b = btree_map(type_double()),
        m = type_matrix(type_double()),
        n = type_matrix(type_double()),
        s = type_character()
      ),
      rules = list(
        rule(a == b, message = "a equals b.", id = "SAME_MAP"),
        rule(m == n, message = "m equals n.", id = "SAME_MATRIX"),
        rule(
          s != "NaN",
          message = "s is not the text NaN.",
          id = "NOT_NAN_TEXT"
        )
      )
    )
  )
  base <- list(
    a = c(x = 1, y = 2),
    b = c(x = 1, y = 2),
    m = matrix(as.numeric(1:4), 2),
    n = matrix(as.numeric(1:4), 2),
    s = "Infinity"
  )
  same_construction(P, base)
  rules_of <- function(fields) {
    sort(R8:::r8_new_value(fields, attr(P, "r8_name"), "")[["errors"]][[
      "rule"
    ]])
  }
  bad <- modifyList(
    base,
    list(b = c(x = 1, y = 3), n = matrix(as.numeric(4:1), 2), s = "NaN")
  )
  same_construction(P, bad)
  expect_identical(rules_of(bad), c("NOT_NAN_TEXT", "SAME_MAP", "SAME_MATRIX"))
})

test_that("a computed field of a matrix type is the matrix it names", {
  C <- declare_in(
    "construct",
    new_struct(
      "Computed",
      fields = list(
        m = type_matrix(type_double()),
        n = field(option(type_matrix(type_double())), compute = m)
      )
    )
  )
  m <- matrix(as.numeric(1:6), 3)
  x <- r8_resolve(C(m = m))
  expect_identical(x$n, m)
})

test_that("a dgCMatrix out of compressed sparse column form is reported", {
  skip_if_not_installed("Matrix")
  S <- declare_in(
    "construct",
    new_struct("Sparse", fields = list(x = type_sparse_matrix()))
  )
  m <- Matrix::sparseMatrix(
    i = c(1L, 3L, 2L),
    j = c(1L, 1L, 2L),
    x = c(1.5, 2, 3),
    dims = c(3L, 2L)
  )
  same_construction(S, list(x = m))
  # Slot assignment skips the validity method of Matrix.
  unsorted <- m
  unsorted@i <- c(2L, 0L, 1L)
  beyond <- m
  beyond@i <- c(0L, 3L, 1L)
  offsets <- m
  offsets@p <- c(0L, 2L, 2L)
  for (bad in list(unsorted, beyond, offsets)) {
    in_place <- same_construction(S, list(x = bad))
    expect_identical(in_place[["errors"]][["code"]], "sparse_form")
    expect_identical(in_place[["errors"]][["path"]], "/x")
  }
})
