# test-133-integrate.R
# ::R8::
# 2026- EDG rtemis.org

# The text of values in documents and notebooks: knitr, repr, and pillar.
#
# spec: R8/class-system#text-output

# Runs `code` with knitr's pandoc target set to `to`.
with_pandoc_to <- function(to, code) {
  old <- knitr::opts_knit$get("rmarkdown.pandoc.to")
  knitr::opts_knit$set(rmarkdown.pandoc.to = to)
  on.exit(knitr::opts_knit$set(rmarkdown.pandoc.to = old))
  code
}

gbm_integrate <- function() {
  declare_in(
    "demo",
    new_struct("GBMI", fields = list(n_trees = type_integer()))
  )
}

test_that("knitr receives html, inline markdown, or plain text by target", {
  skip_if_not_installed("knitr")
  GBM <- gbm_integrate()
  x <- GBM(n_trees = 500L)
  html <- with_pandoc_to("html", knit_print.r8_value(x))
  expect_s3_class(html, "knit_asis")
  expect_match(html, "```{=html}\n<pre class=\"r8\">", fixed = TRUE)
  expect_match(html, "r8-type_name", fixed = TRUE)
  inline <- knit_print.r8_value(x, inline = TRUE)
  expect_identical(as.character(inline), "`<demo::GBMI>; n_trees: 500`")
  expect_output(with_pandoc_to("gfm", knit_print.r8_value(x)), "n_trees: 500")
  t <- r8_text("a", r8_span("b", "strong"))
  expect_match(
    with_pandoc_to("html", knit_print.r8_text(t)),
    "r8-strong",
    fixed = TRUE
  )
})

test_that("a Print impl's text reaches documents and notebooks", {
  GBM <- declare_in(
    "demo",
    new_struct("GBMP", fields = list(n_trees = type_integer()))
  )
  declare_in(
    "demo",
    impl(
      Print,
      GBM,
      methods = list(print = function(x, ...) {
        r8_text(r8_span("GBM", "highlight"), " in ", r8_output_type())
      })
    )
  )
  x <- GBM(n_trees = 1L)
  expect_match(
    repr_html.r8_value(x),
    "r8-user_text\">html</span>",
    fixed = TRUE
  )
  expect_identical(repr_text.r8_value(x), "GBM in plain")
  Cat <- declare_in(
    "demo",
    new_struct("GBMC", fields = list(n_trees = type_integer()))
  )
  declare_in(
    "demo",
    impl(
      Print,
      Cat,
      methods = list(print = function(x, ...) cat("trees:", x$n_trees, "\n"))
    )
  )
  y <- Cat(n_trees = 2L)
  expect_silent(out <- repr_text.r8_value(y))
  expect_identical(out, "trees: 2 ")
})

test_that("repr gives html and plain text, and pillar the type or Display text", {
  GBM <- gbm_integrate()
  x <- GBM(n_trees = 500L)
  expect_match(repr_html.r8_value(x), "^<pre class=\"r8\">")
  expect_identical(repr_text.r8_value(x), "<demo::GBMI>\n  n_trees: 500")
  expect_identical(repr_text.r8_text(r8_text("a")), "a")
  expect_identical(obj_sum.r8_value(x), "demo::GBMI")
  Named <- declare_in(
    "demo",
    new_struct("GBMD", fields = list(n_trees = type_integer()))
  )
  declare_in(
    "demo",
    impl(
      Display,
      Named,
      methods = list(format = function(x, ...) paste0("GBM(", x$n_trees, ")"))
    )
  )
  expect_identical(obj_sum.r8_value(Named(n_trees = 3L)), "GBM(3)")
})

test_that("the classes mode marks spans by class, styled by r8_css()", {
  GBM <- gbm_integrate()
  x <- GBM(n_trees = 500L)
  classes <- format(x, output_type = "html", html_style = "classes")
  expect_false(any(grepl("style=", classes, fixed = TRUE)))
  expect_match(
    classes[[1L]],
    "<span class=\"r8-type_name\">GBMI</span>",
    fixed = TRUE
  )
  op <- options(r8.html_style = "classes")
  on.exit(options(op))
  expect_identical(format(x, output_type = "html"), classes)
  options(op)
  expect_error(format(x, html_style = "classes"), class = "r8_text_error")
  expect_error(
    format(x, output_type = "html", html_style = "css"),
    class = "r8_text_error"
  )
  css <- r8_css()
  expect_match(
    css,
    ".r8 .r8-bracket { color:var(--r8-bracket,#6CA3A0) }",
    fixed = TRUE
  )
  expect_match(
    css,
    "@media (prefers-color-scheme: light) {\n  .r8 .r8-bracket { color:var(--r8-bracket,#0F6A66) }",
    fixed = TRUE
  )
  expect_match(css, "@media (prefers-color-scheme: dark) {", fixed = TRUE)
  custom <- r8_css(
    r8_theme(bracket = list(color = "#FF0000")),
    style_element = TRUE
  )
  expect_match(custom, "^<style>\n")
  expect_match(
    custom,
    ".r8 .r8-bracket { color:var(--r8-bracket,#FF0000) }",
    fixed = TRUE
  )
  expect_false(grepl("  .r8 .r8-bracket", custom, fixed = TRUE))
  expect_match(
    custom,
    "  .r8 .r8-variant { color:var(--r8-variant,#0F6A66) }",
    fixed = TRUE
  )
  expect_error(r8_css(list()), class = "r8_text_error")
})

test_that("repr dispatches to R8's methods once repr loads", {
  skip_if_not_installed("repr")
  GBM <- gbm_integrate()
  x <- GBM(n_trees = 500L)
  expect_identical(repr::repr_html(x), repr_html.r8_value(x))
  expect_identical(repr::repr_text(x), "<demo::GBMI>\n  n_trees: 500")
  expect_match(repr::repr_html(r8_text("a")), "^<pre class=\"r8\">")
})
