# integrate.R
# ::R8::
# 2026- EDG rtemis.org

# Text output in documents, notebooks, and tibbles: methods for generics of
# knitr, repr, and pillar, registered when those packages load.
# spec: R8/class-system#text-output

# %% shown_text ----
#' The text a value shows in a document or a notebook
#'
#' An `r8_text` object is its own text. An R8 value shows the text its
#' `Print` impl returns, with `r8_output_type()` reporting `output_type`
#' while the impl runs, or, from an impl that writes with `cat()`, the lines
#' it writes; a value without a `Print` impl shows its `Display` text or its
#' default layout.
#'
#' @param x An R8 value or an `r8_text` object.
#' @param output_type Character: The output type of the target.
#'
#' @return An `r8_text` object.
#'
#' @keywords internal
#' @noRd
shown_text <- function(x, output_type) {
  if (inherits(x, "r8_text")) {
    return(x)
  }
  f <- .r8_impls[[paste("R8::Print", "print", attr(x, "r8_type"), sep = "|")]]
  if (is.null(f)) {
    return(value_text(x))
  }
  out <- NULL
  written <- utils::capture.output(
    out <- with_print_state(c(output_type, "24bit"), NULL, f(x))
  )
  if (inherits(out, "r8_text")) out else as_r8_text(written)
} # /R8::shown_text


# %% html_target ----
#' Whether knitr writes an HTML document
#'
#' @return Logical.
#'
#' @keywords internal
#' @noRd
html_target <- function() {
  knitr::is_html_output(
    excludes = c(
      "markdown",
      "markdown_strict",
      "gfm",
      "commonmark",
      "epub",
      "epub3"
    )
  )
} # /R8::html_target


# %% knit_text ----
#' The knitr output of an R8 value or text
#'
#' Inline code receives the one-line markdown encoding; an HTML target, the
#' html encoding in a raw HTML block; every other target, plain text through
#' `knitr::normal_print()`.
#'
#' @param x An R8 value or an `r8_text` object.
#' @param inline Logical: The value is the result of inline code.
#'
#' @return The output of `knitr::asis_output()` or `knitr::normal_print()`.
#'
#' @keywords internal
#' @noRd
knit_text <- function(x, inline) {
  if (isTRUE(inline)) {
    return(knitr::asis_output(
      format(shown_text(x, "markdown"), output_type = "markdown", inline = TRUE)
    ))
  }
  if (html_target()) {
    html <- paste(
      format(shown_text(x, "html"), output_type = "html"),
      collapse = "\n"
    )
    return(knitr::asis_output(paste0("\n```{=html}\n", html, "\n```\n")))
  }
  knitr::normal_print(x)
} # /R8::knit_text


# %% knit_print.r8_value ----
#' Text output in documents, notebooks, and tibbles
#'
#' R8 registers methods for knitr, repr, and pillar when those packages
#' load. In R Markdown and Quarto, an HTML document receives a value's HTML
#' encoding, inline code its one-line markdown encoding, and every other
#' document plain text. In Jupyter, `repr_html()` gives the HTML encoding and
#' `repr_text()` plain text. A tibble column of R8 values shows the first
#' line of each value's `Display` text, or its type, `<pkgr::Resampler@Bootstrap>`.
#' A value with a `Print` impl shows the text the impl returns.
#'
#' @param x,obj An R8 value or an `r8_text` object.
#' @param options List: knitr's chunk options.
#' @param inline Logical: The value is the result of inline code.
#' @param ... Not used.
#'
#' @return `knit_print()`: knitr output. `repr_html()` and `repr_text()`:
#'   Character, one string. `obj_sum()`: Character, one string.
#'
#' @name r8_documents
#' @keywords internal
#' @exportS3Method knitr::knit_print
knit_print.r8_value <- function(x, options = list(), inline = FALSE, ...) {
  knit_text(x, inline)
} # /R8::knit_print.r8_value


# %% knit_print.r8_text ----
#' @rdname r8_documents
#' @exportS3Method knitr::knit_print
knit_print.r8_text <- function(x, options = list(), inline = FALSE, ...) {
  knit_text(x, inline)
} # /R8::knit_print.r8_text


# %% repr_html.r8_value ----
#' @rdname r8_documents
#' @exportS3Method repr::repr_html
repr_html.r8_value <- function(obj, ...) {
  paste(format(shown_text(obj, "html"), output_type = "html"), collapse = "\n")
} # /R8::repr_html.r8_value


# %% repr_html.r8_text ----
#' @rdname r8_documents
#' @exportS3Method repr::repr_html
repr_html.r8_text <- function(obj, ...) {
  paste(format(obj, output_type = "html"), collapse = "\n")
} # /R8::repr_html.r8_text


# %% repr_text.r8_value ----
#' @rdname r8_documents
#' @exportS3Method repr::repr_text
repr_text.r8_value <- function(obj, ...) {
  paste(format(shown_text(obj, "plain")), collapse = "\n")
} # /R8::repr_text.r8_value


# %% repr_text.r8_text ----
#' @rdname r8_documents
#' @exportS3Method repr::repr_text
repr_text.r8_text <- function(obj, ...) {
  paste(format(obj), collapse = "\n")
} # /R8::repr_text.r8_text


# %% obj_sum.r8_value ----
#' @rdname r8_documents
#' @exportS3Method pillar::obj_sum
obj_sum.r8_value <- function(x) {
  # pillar wraps the summary in angle brackets: a value without a Display
  # impl gives its type, `pkgr::Resampler@Bootstrap`, which shows as
  # `<pkgr::Resampler@Bootstrap>`.
  if (is.null(display_impl(x))) {
    variant <- attr(x, "r8_variant")
    return(paste0(
      attr(x, "r8_type"),
      if (!is.null(variant)) paste0("@", variant)
    ))
  }
  lines <- format(x)
  if (length(lines) == 0L) "" else lines[[1L]]
} # /R8::obj_sum.r8_value
