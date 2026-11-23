# test-text.R
# ::R8::
# 2026- EDG rtemis.org

# spec: R8/class-system#text-output

# Runs `code` with the options `opts` set.
with_opts <- function(opts, code) {
  op <- options(opts)
  on.exit(options(op))
  code
}

# Runs `code` with the environment variables `vars` set, `""` unsetting one.
with_env <- function(vars, code) {
  old <- Sys.getenv(names(vars), unset = NA, names = TRUE)
  on.exit({
    for (n in names(old)) {
      if (is.na(old[[n]])) {
        Sys.unsetenv(n)
      } else {
        do.call(Sys.setenv, as.list(old[n]))
      }
    }
  })
  for (n in names(vars)) {
    if (nzchar(vars[[n]])) {
      do.call(Sys.setenv, as.list(vars[n]))
    } else {
      Sys.unsetenv(n)
    }
  }
  code
}

gbm_types <- function() {
  declare_in("demo", {
    Split <- new_struct(
      "SplitT",
      fields = list(train = vec(type_integer()), test = vec(type_integer()))
    )
    GBM <- new_struct(
      "GBMT",
      fields = list(
        n_trees = type_integer(),
        shrinkage = type_double(),
        coef = vec(type_double()),
        split = Split
      )
    )
    list(Split = Split, GBM = GBM)
  })
}

gbm_value <- function(types) {
  types$GBM(
    n_trees = 500L,
    shrinkage = 0.01,
    coef = c(0.12, -0.5, 1 / 3, 1.5, 2, 3, 4, 5, 6),
    split = types$Split(train = 1:10, test = 11:12)
  )
}

test_that("the default layout names the type, then each field, nested values indented", {
  x <- gbm_value(gbm_types())
  expect_identical(
    format(x),
    c(
      "<demo::GBMT>",
      "  n_trees: 500",
      "  shrinkage: 0.01",
      "  coef: double[9] 0.12 -0.5 0.3333333 1.5 2 3 4 5 ...",
      "  split: <demo::SplitT>",
      "    train: integer[10] 1 2 3 4 5 6 7 8 ...",
      "    test: integer[2] 11 12"
    )
  )
  t <- r8_layout(x, head = 2L)
  expect_s3_class(t, "r8_text")
  expect_identical(attr(t, "r8_depth"), c(0L, 1L, 1L, 1L, 1L, 2L, 2L))
  expect_identical(t[[4L]], "  coef: double[9] 0.12 -0.5 ...")
  spans <- attr(t, "r8_spans")
  first <- spans$line == 1L
  expect_identical(
    spans$role[first],
    c("bracket", "muted", "type_name", "bracket")
  )
  coef <- spans$line == 4L
  expect_identical(
    spans$role[coef],
    c("field_name", "type_number", "summary", "scalar")
  )
  expect_identical(
    spans$role[spans$line == 6L][2:3],
    c("type_integer", "summary")
  )
  ansi <- with_env(c(COLORTERM = ""), format(x, output_type = "ansi"))
  expect_match(
    ansi[[4L]],
    "\033[38;5;32mdouble\033[0m\033[38;5;244m[9]\033[0m",
    fixed = TRUE
  )
  expect_error(r8_layout(1), class = "r8_text_error")
})

test_that("each output type reduces to the plain text", {
  x <- gbm_value(gbm_types())
  with_env(c(COLORTERM = "truecolor"), {
    expect_match(
      format(x, output_type = "ansi")[[1L]],
      "\033[38;2;108;163;160m<",
      fixed = TRUE
    )
  })
  plain <- format(x)
  ansi <- format(x, output_type = "ansi")
  expect_identical(gsub("\033\\[[0-9;]*m", "", ansi), plain)
  expect_match(ansi[[1L]], "\033[", fixed = TRUE)
  html <- format(x, output_type = "html")
  expect_match(
    html[[1L]],
    "^<pre class=\"r8\"><span class=\"r8-line\" data-depth=\"0\">"
  )
  expect_match(html[[6L]], "data-depth=\"2\"", fixed = TRUE)
  unescaped <- gsub("&gt;", ">", gsub("&lt;", "<", gsub("<[^>]+>", "", html)))
  expect_identical(unescaped, plain)
  expect_identical(
    format(x, output_type = "markdown"),
    c("```", plain, "```")
  )
  expect_length(format(x, output_type = "markdown", inline = TRUE), 1L)
  expect_error(format(x, output_type = "rtf"), class = "r8_text_error")
})

test_that("r8_span() and r8_text() join pieces on one line, and c() stacks lines", {
  t <- r8_text("Model ", r8_span("GBM", "type_name"), " has ", 500L, " trees")
  expect_identical(as.character(t), "Model GBM has 500 trees")
  expect_identical(nchar(t), 23L)
  spans <- attr(t, "r8_spans")
  expect_identical(spans$start, c(1L, 7L, 10L, 15L, 18L))
  expect_identical(spans$end, c(6L, 9L, 14L, 17L, 23L))
  expect_identical(spans$role[[2L]], "type_name")
  s <- c(t, r8_text(r8_span("12 s", "summary")))
  expect_identical(as.character(s), c("Model GBM has 500 trees", "12 s"))
  expect_identical(attr(s, "r8_spans")$line, c(1L, 1L, 1L, 1L, 1L, 2L))
  expect_identical(
    format(r8_span("é<b>", "strong"), output_type = "html"),
    "<pre class=\"r8\"><span class=\"r8-line\" data-depth=\"0\"><span class=\"r8-strong\" style=\"font-weight:bold\">é&lt;b&gt;</span></span></pre>"
  )
  expect_identical(format(r8_text()), character())
  expect_error(r8_span("a", "red"), class = "r8_text_error")
  expect_error(r8_span("a\nb", "strong"), class = "r8_text_error")
  expect_error(r8_span(c("a", "b"), "strong"), class = "r8_text_error")
})

test_that("text changed outside R8 encodes without its roles", {
  t <- toupper(r8_text("a", r8_span("b", "strong")))
  expect_warning(out <- format(t, output_type = "ansi"), "plain copy")
  expect_identical(out, "AB")
})

test_that("a nested value whose type implements Display shows the impl's text", {
  types <- gbm_types()
  x <- gbm_value(types)
  declare_in(
    "demo",
    impl(
      Display,
      types$Split,
      methods = list(format = function(x, ...) {
        r8_text(
          r8_span(paste(length(x$train), "train"), "highlight"),
          " / ",
          length(x$test),
          " test"
        )
      })
    )
  )
  expect_identical(format(x)[[5L]], "  split: 10 train / 2 test")
  expect_identical(format(x$split), "10 train / 2 test")
  expect_match(format(x$split, output_type = "ansi"), "\033[1;", fixed = TRUE)
})

test_that("a Print impl returns text in the output type print() determines", {
  types <- gbm_types()
  x <- gbm_value(types)
  declare_in(
    "demo",
    impl(
      Print,
      types$GBM,
      methods = list(print = function(x, ...) {
        r8_text(r8_span("GBM", "highlight"), " in ", r8_output_type())
      })
    )
  )
  expect_identical(
    capture.output(print(x, output_type = "plain")),
    "GBM in plain"
  )
  with_env(c(COLORTERM = ""), {
    expect_identical(
      capture.output(print(x, output_type = "ansi")),
      "\033[1;38;5;73mGBM\033[0m in ansi"
    )
  })
  with_opts(list(r8.output_type = "markdown"), {
    expect_identical(capture.output(print(x)), "GBM in markdown")
  })
  expect_identical(format(x)[[1L]], "<demo::GBMT>")
})

test_that("r8_output_type() determines the choice, the setting, and the environment", {
  expect_identical(r8_output_type("html"), "html")
  with_opts(list(r8.output_type = "markdown"), {
    expect_identical(r8_output_type(), "markdown")
    expect_identical(r8_output_type("plain"), "plain")
  })
  with_env(c(R8_OUTPUT_TYPE = "html", NO_COLOR = ""), {
    expect_identical(r8_output_type(), "html")
  })
  with_env(c(R8_OUTPUT_TYPE = "", NO_COLOR = "1"), {
    expect_identical(r8_output_type(), "plain")
  })
  expect_error(r8_output_type("rtf"), class = "r8_text_error")
})

test_that("r8_theme() replaces the styles of the roles it names", {
  th <- r8_theme(
    type_name = list(color = "#FF0000"),
    bracket = list(color = NA)
  )
  expect_s3_class(th, "r8_theme")
  expect_identical(th[["type_name"]][["color"]], "#FF0000")
  expect_identical(th[["type_name"]][["index"]], 196L)
  expect_true(th[["type_name"]][["bold"]])
  expect_identical(th[["bracket"]][["color"]], NA_character_)
  x <- gbm_value(gbm_types())
  out <- with_env(
    c(COLORTERM = ""),
    format(x, output_type = "ansi", theme = th)
  )
  expect_match(
    out[[1L]],
    "^<\033\\[38;5;244mdemo::\033\\[0m\033\\[1;38;5;196mGBMT\033\\[0m>$"
  )
  with_opts(list(r8.theme = th), {
    expect_identical(
      with_env(c(COLORTERM = ""), format(x, output_type = "ansi")),
      out
    )
  })
  expect_error(r8_theme(colour = list(bold = TRUE)), "not a role")
  expect_error(r8_theme(bracket = list(color = "teal")), "hex code")
  expect_error(r8_theme(bracket = list(weight = 2)), "not a member")
  expect_error(r8_theme(list(bold = TRUE)), class = "r8_text_error")
  expect_error(format(x, theme = list()), class = "r8_text_error")
})

test_that("a list lays out its elements with types inferred from R objects", {
  # A type of its own: an earlier test implements Display for SplitT.
  Split <- declare_in(
    "demo",
    new_struct(
      "SplitL",
      fields = list(train = vec(type_integer()), test = vec(type_integer()))
    )
  )
  x <- list(
    model = "GBM",
    n_trees = 500L,
    auc = c(0.81, 0.84, 0.79),
    split = Split(train = 1:3, test = 4L),
    list(a = 1, deep = list(b = list(c = 1))),
    when = as.Date("2026-11-20"),
    named = c(a = 1, b = 2),
    fn = mean,
    NULL
  )
  t <- r8_layout(x)
  expect_identical(
    as.character(t),
    c(
      "<list> 9 elements",
      "  model: \"GBM\"",
      "  n_trees: 500",
      "  auc: double[3] 0.81 0.84 0.79",
      "  split: <demo::SplitL>",
      "    train: integer[3] 1 2 3",
      "    test: integer[1] 4",
      "  [[5]]: list[2]",
      "    a: 1",
      "    deep: list[1]",
      "      b: list[1]",
      "  when: 2026-11-20",
      "  named: double[2] 1 2",
      "  fn: function",
      "  [[9]]: NULL"
    )
  )
  spans <- attr(t, "r8_spans")
  expect_identical(
    spans$role[spans$line == 8L][1:3],
    c("muted", "type_other", "summary")
  )
  expect_identical(attr(t, "r8_depth")[9:11], c(2L, 2L, 3L))
  expect_identical(as.character(r8_layout(list())), "<list> 0 elements")
  expect_identical(
    as.character(r8_layout(unname(as.list(1:2)))),
    c("<list> 2 elements", "  [[1]]: 1", "  [[2]]: 2")
  )
  expect_error(r8_layout(lm(mpg ~ wt, mtcars)), class = "r8_text_error")
})

test_that("a data frame lays out its columns as vectors, within the limits", {
  t <- r8_layout(iris)
  expect_identical(t[[1L]], "<data.frame> 150 x 5")
  expect_identical(
    t[[6L]],
    "  Species: factor[150] setosa setosa setosa setosa setosa setosa setosa setosa ... (3 levels)"
  )
  one <- r8_layout(data.frame(a = 1))
  expect_identical(one[[2L]], "  a: double[1] 1")
  wide <- r8_layout(as.data.frame(matrix(0, 2, 5000)))
  expect_length(wide, 34L)
  expect_identical(wide[[34L]], "  ... 4,968 more")
  nested <- r8_layout(list(df = head(mtcars, 2L)))
  expect_identical(
    nested[[2L]],
    "  df: table[2 x 11] mpg cyl disp hp drat wt qsec vs ..."
  )
  long <- r8_layout(lapply(1:40, function(i) as.list(1:10)))
  expect_length(long, 129L)
  expect_identical(long[[129L]], "    ...")
  tb <- structure(
    list(a = 1:3),
    class = c("tbl_df", "tbl", "data.frame"),
    row.names = c(NA, -3L)
  )
  expect_identical(r8_layout(tb)[[1L]], "<tbl_df> 3 x 1")
})
