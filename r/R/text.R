# text.R
# ::R8::
# 2026- EDG rtemis.org

# Text output: `r8_text` objects, the default layout of a value, the theme,
# the output type, and the format() and print() methods of values and text.
# The Rust core lays out and encodes text (src/rust/src/text.rs).
# spec: R8/class-system#text-output

# The output type, color depth, and theme of the running print(), which
# r8_output_type() reports to a Print impl.
.r8_print <- new.env(parent = emptyenv())

# %% new_r8_text ----
#' An `r8_text` object from its parts
#'
#' @param parts List: `lines`, `line`, `start`, `end`, `role`, and `depth`.
#'
#' @return An `r8_text` object.
#'
#' @keywords internal
#' @noRd
new_r8_text <- function(parts) {
  lines <- as.character(parts[["lines"]])
  structure(
    lines,
    class = "r8_text",
    r8_spans = list(
      line = as.integer(parts[["line"]]),
      start = as.integer(parts[["start"]]),
      end = as.integer(parts[["end"]]),
      role = as.character(parts[["role"]])
    ),
    r8_depth = as.integer(parts[["depth"]]),
    r8_plain = lines
  )
} # /R8::new_r8_text


# %% text_parts ----
#' The parts of an `r8_text` object
#'
#' The lines, spans, and depths that the core encodes. Text whose lines
#' differ from its stored plain copy, after `sub()` or `toupper()`, keeps its
#' lines and drops its spans and depths, with a warning.
#'
#' @param x An `r8_text` object.
#'
#' @return List: `lines`, `line`, `start`, `end`, `role`, and `depth`.
#'
#' @keywords internal
#' @noRd
text_parts <- function(x) {
  lines <- enc2utf8(as.character(x))
  lines[is.na(lines)] <- "NA"
  spans <- attr(x, "r8_spans")
  depth <- attr(x, "r8_depth")
  intact <- !is.null(spans) &&
    length(depth) == length(lines) &&
    identical(lines, attr(x, "r8_plain"))
  if (!intact) {
    if (!is.null(spans)) {
      warning(
        "The text of an `r8_text` object differs from its plain copy; ",
        "its roles are dropped.",
        call. = FALSE
      )
    }
    spans <- list(
      line = integer(),
      start = integer(),
      end = integer(),
      role = character()
    )
    depth <- integer(length(lines))
  }
  list(
    lines = lines,
    line = spans[["line"]],
    start = spans[["start"]],
    end = spans[["end"]],
    role = spans[["role"]],
    depth = depth
  )
} # /R8::text_parts


# %% as_r8_text ----
#' Text as an `r8_text` object
#'
#' An `r8_text` object is kept; an R8 value gives its text (`value_text()`);
#' other objects are coerced to character, each line of each element a line
#' of user text; `NULL` gives no lines.
#'
#' @param x An object.
#'
#' @return An `r8_text` object.
#'
#' @keywords internal
#' @noRd
as_r8_text <- function(x) {
  if (inherits(x, "r8_text")) {
    return(x)
  }
  if (is_value(x)) {
    return(value_text(x))
  }
  text <- enc2utf8(as.character(x))
  invalid <- !is.na(text) & !validUTF8(text)
  text[invalid] <- iconv(text[invalid], "UTF-8", "UTF-8", sub = "\ufffd")
  lines <- as.character(unlist(strsplit(text, "\n", fixed = TRUE)))
  lines[is.na(lines)] <- "NA"
  n <- nchar(lines)
  shown <- which(n > 0)
  new_r8_text(list(
    lines = lines,
    line = shown,
    start = rep(1L, length(shown)),
    end = n[shown],
    role = rep("user_text", length(shown)),
    depth = integer(length(lines))
  ))
} # /R8::as_r8_text


# %% join_parts ----
#' Two texts' parts joined on one line
#'
#' The first line of `b` continues the last line of `a`; the other lines of
#' `b` follow.
#'
#' @param a,b List: The parts of an `r8_text` object.
#'
#' @return List: The parts of the joined text.
#'
#' @keywords internal
#' @noRd
join_parts <- function(a, b) {
  n <- length(a[["lines"]])
  if (n == 0L) {
    return(b)
  }
  if (length(b[["lines"]]) == 0L) {
    return(a)
  }
  offset <- nchar(a[["lines"]][[n]])
  first <- b[["line"]] == 1L
  list(
    lines = c(
      a[["lines"]][-n],
      paste0(a[["lines"]][[n]], b[["lines"]][[1L]]),
      b[["lines"]][-1L]
    ),
    line = c(a[["line"]], b[["line"]] + n - 1L),
    start = c(a[["start"]], b[["start"]] + ifelse(first, offset, 0L)),
    end = c(a[["end"]], b[["end"]] + ifelse(first, offset, 0L)),
    role = c(a[["role"]], b[["role"]]),
    depth = c(a[["depth"]], b[["depth"]][-1L])
  )
} # /R8::join_parts


# %% stack_parts ----
#' Texts' parts stacked, one after another
#'
#' @param parts List: The parts of `r8_text` objects.
#'
#' @return List: The parts of the stacked text.
#'
#' @keywords internal
#' @noRd
stack_parts <- function(parts) {
  offsets <- cumsum(c(0L, vapply(parts, function(p) length(p[["lines"]]), 0L)))
  list(
    lines = unlist(lapply(parts, `[[`, "lines")),
    line = unlist(Map(
      function(p, o) p[["line"]] + o,
      parts,
      offsets[-length(offsets)]
    )),
    start = unlist(lapply(parts, `[[`, "start")),
    end = unlist(lapply(parts, `[[`, "end")),
    role = unlist(lapply(parts, `[[`, "role")),
    depth = unlist(lapply(parts, `[[`, "depth"))
  )
} # /R8::stack_parts


# %% r8_span ----
#' Text with a role
#'
#' Marks text with a role, which the theme styles: `r8_span()` makes one line
#' of one span, which `r8_text()` joins with other text and `c()` stacks.
#' Text output names roles, never colors, so the theme restyles every
#' encoding: ANSI, HTML, and markdown, which keeps `strong` and `emphasis`.
#'
#' The roles are `bracket`, `type_name`, `variant`, `field_name`, `scalar`,
#' `summary`, `user_text`, `strong`, `emphasis`, `muted`, `highlight`, and
#' `plain`, and the roles of base-type names after rtemislive's column types:
#' `type_number` (double), `type_integer`, `type_categorical` (factor),
#' `type_string` (character), `type_boolean` (logical), `type_datetime` (Date
#' and POSIXct), and `type_other`.
#'
#' @param text Character: One string, without line breaks.
#' @param role Character: The role.
#'
#' @return An `r8_text` object: a character vector of plain lines, with the
#'   spans of its roles and the depth of each line as attributes.
#'
#' @author EDG
#' @export
#'
#' @examples
#' r8_text("GBM ", r8_span("trained", "highlight"), " in 12 s")
r8_span <- function(text, role) {
  if (!is.character(text) || length(text) != 1L || is.na(text)) {
    r8_abort("`text` is one string.", class = "r8_text_error")
  }
  if (grepl("\n", text, fixed = TRUE)) {
    r8_abort(
      "`text` holds a line break; stack lines with c().",
      class = "r8_text_error"
    )
  }
  if (!is.character(role) || length(role) != 1L || !role %in% r8_roles()) {
    r8_abort(
      "`role` is one of ",
      paste0("\"", r8_roles(), "\"", collapse = ", "),
      ".",
      class = "r8_text_error"
    )
  }
  n <- nchar(text)
  styled <- n > 0L && role != "plain"
  new_r8_text(list(
    lines = text,
    line = if (styled) 1L else integer(),
    start = if (styled) 1L else integer(),
    end = if (styled) n else integer(),
    role = if (styled) role else character(),
    depth = 0L
  ))
} # /R8::r8_span


# %% r8_text ----
#' Text for output
#'
#' Joins pieces into text: character as user text, spans from `r8_span()`,
#' R8 values as their text, and other `r8_text` objects. Pieces join on one
#' line: the first line of each piece continues the last line of the text
#' before it, and its other lines follow. `c()` stacks `r8_text` objects as
#' lines.
#'
#' An `r8_text` object is a character vector of plain lines, so `nchar()`
#' gives the plain width and `cat()` writes plain text. Its attributes hold
#' the spans of its roles, the depth of each line, and a copy of the plain
#' lines; text that `sub()` or `toupper()` changes encodes without roles.
#' `format()` encodes it as plain text unless its caller names another
#' output type, and `print()` writes it in the output type that
#' `r8_output_type()` determines.
#'
#' @param ... Pieces: character, `r8_text` objects, and R8 values.
#'
#' @return An `r8_text` object.
#'
#' @author EDG
#' @export
#'
#' @examples
#' t <- r8_text("GBM ", r8_span("trained", "highlight"))
#' c(t, r8_text(r8_span("12 s", "summary")))
#' format(t, output_type = "html")
r8_text <- function(...) {
  pieces <- list(...)
  parts <- list(
    lines = character(),
    line = integer(),
    start = integer(),
    end = integer(),
    role = character(),
    depth = integer()
  )
  for (p in pieces) {
    if (!is.null(p)) {
      parts <- join_parts(parts, text_parts(as_r8_text(p)))
    }
  }
  new_r8_text(parts)
} # /R8::r8_text


# %% c.r8_text ----
#' @rdname r8_text
#' @export
c.r8_text <- function(...) {
  pieces <- Filter(Negate(is.null), list(...))
  new_r8_text(stack_parts(lapply(pieces, function(p) {
    text_parts(as_r8_text(p))
  })))
} # /R8::c.r8_text


# %% display_types ----
#' The types that implement `Display`
#'
#' @return Character: Qualified type names.
#'
#' @keywords internal
#' @noRd
display_types <- function() {
  prefix <- "R8::Display|format|"
  keys <- ls(.r8_impls, all.names = TRUE)
  keys <- keys[startsWith(keys, prefix)]
  substring(keys, nchar(prefix) + 1L)
} # /R8::display_types


# %% display_impl ----
#' The `format` function of a type's `Display` impl
#'
#' @param x An R8 value.
#'
#' @return Function, or `NULL` when the type has no `Display` impl.
#'
#' @keywords internal
#' @noRd
display_impl <- function(x) {
  .r8_impls[[paste("R8::Display", "format", value_type(x), sep = "|")]]
} # /R8::display_impl


# %% value_text ----
#' The text of an R8 value
#'
#' The text of its type's `Display` impl, or its default layout.
#'
#' @param x An R8 value.
#' @param summary Character or `NULL`: Text after the default layout's
#'   header.
#'
#' @return An `r8_text` object.
#'
#' @keywords internal
#' @noRd
value_text <- function(x, summary = NULL) {
  if (!is_value(x)) {
    return(as_r8_text(c(
      paste("This object", not_value_text(x)),
      utils::capture.output(print(unclass(x)))
    )))
  }
  other <- r8_value_other(x)
  if (!is.null(other)) {
    note <- sub("^made", "Made", other[["message"]])
    # The fields of a saved Rust-stored value read under its own declaration only.
    layout <- tryCatch(
      layout_value(x, summary = summary),
      error = function(e) r8_text("<", value_type(x), ">")
    )
    return(c(
      layout,
      r8_text(r8_span(paste0(note, "."), "summary"))
    ))
  }
  f <- display_impl(x)
  if (!is.null(f)) {
    return(as_r8_text(f(x)))
  }
  if (inherits(x, "R8::Report")) {
    return(report_text(x))
  }
  layout_value(x, summary = summary)
} # /R8::value_text


# %% layout_value ----
#' The default layout of an R8 value
#'
#' Reads a duplicate of the value, so that the layout fills no field cache
#' of `x`. The header of a Rust-stored value shows the size of its state.
#'
#' @param x An R8 value.
#' @param summary Character or `NULL`: Text after the header, in place of
#'   the size.
#' @param head Integer: The number of items a vector's head shows.
#'
#' @return An `r8_text` object.
#'
#' @keywords internal
#' @noRd
layout_value <- function(x, summary = NULL, head = 8L) {
  if (is.null(summary)) {
    parts <- r8_rust_stored_parts(x)
    if (!is.null(parts)) {
      summary <- r8_format_size(parts[["state"]])
    }
  }
  copy <- unclass(x)
  class(copy) <- oldClass(x)
  new_r8_text(r8_layout_value(
    copy,
    display_types(),
    display_parts,
    summary,
    as.integer(head),
    getOption("width", 80L),
    isTRUE(l10n_info()[["UTF-8"]])
  ))
} # /R8::layout_value


# %% r8_layout ----
#' The default layout of an R8 value, a list, or a data frame
#'
#' The text that `print()` shows for a value whose type has no `Display` or
#' `Print` impl: a header naming the type, and each field on its own line,
#' a nested value's fields indented under it. A vector shows its element
#' type, its length, and its first `head` items; a factor, a matrix, a table,
#' and a sparse matrix show their shape. The header of a Rust-stored value
#' shows the size of its state in IEC binary units. A nested value whose
#' type implements `Display` shows the impl's text. The layout reads no
#' vector whole.
#'
#' A list or a data frame lays out the same way, with each element's type
#' inferred from the R object: the header names the class and the number of
#' elements, or rows x columns (`<data.frame> 150 x 5`), and each element or
#' column follows on its own line. A nested list shows as `list[3]` with its
#' elements indented, to a depth of three; an element without a name shows
#' its position, `[[2]]`. A level shows at most 32 elements, and the layout
#' at most 128 lines.
#'
#' A `Display` or `Print` impl can build on the layout with `r8_text()` and
#' `c()`.
#'
#' @param x An R8 value, a list, or a data frame.
#' @param head Integer: The number of items a vector's head shows.
#'
#' @return An `r8_text` object.
#'
#' @author EDG
#' @export
#'
#' @examples
#' GBM <- new_struct("GBM", fields = list(
#'   n_trees = field(type_integer()),
#'   shrinkage = field(type_double())
#' ))
#' r8_layout(GBM(n_trees = 500L, shrinkage = 0.01))
#' r8_layout(list(model = "GBM", n_trees = 500L, auc = c(0.81, 0.84, 0.79)))
#' r8_layout(iris)
r8_layout <- function(x, head = 8L) {
  if (!is.numeric(head) || length(head) != 1L || is.na(head) || head < 0) {
    r8_abort("`head` is one non-negative integer.", class = "r8_text_error")
  }
  if (is_value(x)) {
    return(layout_value(x, head = head))
  }
  if (is.data.frame(x) || (is.list(x) && !is.object(x))) {
    return(new_r8_text(r8_layout_object(
      x,
      display_types(),
      display_parts,
      as.integer(head)
    )))
  }
  r8_abort(
    "`x` is an R8 value, a list, or a data frame.",
    class = "r8_text_error"
  )
} # /R8::r8_layout


# %% display_parts ----
#' The parts of the `Display` text of a nested R8 value
#'
#' @param v An R8 value whose type implements `Display`.
#'
#' @return List: The parts of an `r8_text` object.
#'
#' @keywords internal
#' @noRd
display_parts <- function(v) {
  text_parts(as_r8_text(display_impl(v)(v)))
} # /R8::display_parts


# %% host_kind ----
#' The kind of host R runs in, for the output type of print()
#'
#' @return Character: `positron`, `rstudio`, `rstudio_color`,
#'   `no_ansi_gui`, or `other`.
#'
#' @keywords internal
#' @noRd
host_kind <- function() {
  gui <- .Platform[["GUI"]]
  if (identical(gui, "Positron")) {
    "positron"
  } else if (identical(gui, "RStudio")) {
    if (nzchar(Sys.getenv("RSTUDIO_CONSOLE_COLOR"))) {
      "rstudio_color"
    } else {
      "rstudio"
    }
  } else if (gui %in% c("AQUA", "Rgui")) {
    "no_ansi_gui"
  } else {
    "other"
  }
} # /R8::host_kind


# %% check_output_type ----
#' Checks an `output_type` argument
#'
#' @param output_type Character or `NULL`.
#'
#' @return `NULL`, invisibly.
#'
#' @keywords internal
#' @noRd
check_output_type <- function(output_type) {
  types <- c("plain", "ansi", "html", "markdown")
  if (
    !is.null(output_type) &&
      !(is.character(output_type) &&
        length(output_type) == 1L &&
        output_type %in% types)
  ) {
    r8_abort(
      "`output_type` is \"plain\", \"ansi\", \"html\", or \"markdown\".",
      class = "r8_text_error"
    )
  }
  invisible(NULL)
} # /R8::check_output_type


# %% determine_output ----
#' The output type and color depth of print()
#'
#' @param output_type Character or `NULL`: The output type the caller
#'   names.
#'
#' @return Character: The output type and the color depth, `256` or `24bit`.
#'
#' @keywords internal
#' @noRd
determine_output <- function(output_type = NULL) {
  check_output_type(output_type)
  r8_determine_output(list(
    explicit = output_type,
    option = getOption("r8.output_type"),
    env = Sys.getenv("R8_OUTPUT_TYPE"),
    no_color = nzchar(Sys.getenv("NO_COLOR")),
    knitr = isTRUE(getOption("knitr.in.progress")),
    host = host_kind(),
    terminal = isatty(stdout()),
    truecolor = Sys.getenv("COLORTERM") %in% c("truecolor", "24bit")
  ))
} # /R8::determine_output


# %% r8_output_type ----
#' The output type of print()
#'
#' Determines the output type as `print()` does, in this order: the
#' `output_type` argument; `getOption("r8.output_type")`; the environment
#' variable `R8_OUTPUT_TYPE`; `"plain"` when `NO_COLOR` is set or knitr
#' runs; `"ansi"` in Positron, in RStudio with console color, and when
#' standard output is a terminal, other than in R.app and Rgui; `"plain"`
#' otherwise. Inside a running `print()` of an R8 value or text, it returns
#' that call's output type, so a `Print` impl that writes with `cat()`
#' writes the type the call determined.
#'
#' @param output_type Character or `NULL`: `"plain"`, `"ansi"`, `"html"`,
#'   or `"markdown"`.
#'
#' @return Character: The output type.
#'
#' @author EDG
#' @export
#'
#' @examples
#' r8_output_type()
#' r8_output_type("html")
r8_output_type <- function(output_type = NULL) {
  if (is.null(output_type) && !is.null(.r8_print[["output_type"]])) {
    return(.r8_print[["output_type"]])
  }
  determine_output(output_type)[[1L]]
} # /R8::r8_output_type


# %% r8_theme ----
#' A theme for text output
#'
#' R8's theme with the styles of the named roles replaced. Each argument is
#' a role, whose value is a list of any of `color` (a hex code such as
#' `"#6CA3A0"`, or `NA` for the host's default color), `bold`, and `italic`;
#' the members it leaves out keep R8's style. ANSI output in 256 colors uses
#' the nearest color of the 256-color palette. `options(r8.theme = )` sets
#' the theme of `format()` and `print()`, and their `theme` argument
#' overrides it.
#'
#' R8's theme shows brackets, variants, and highlighted text in rtemis teal
#' (`#6CA3A0`). Type names and strong text are bold. Summaries and muted text
#' are gray (`#808080`). Emphasis is italic.
#' The names of base types take rtemislive's colors of column types, at a
#' lightness that reads on light and dark backgrounds: double `#2E81CC`,
#' integer `#008C92`, factor `#128F5A`, character `#CC566E`, logical
#' `#866BD8`, and Date and POSIXct `#C056AC`.
#'
#' @param ... Styles by role, each a list of `color`, `bold`, and `italic`.
#'
#' @return An `r8_theme` object: a style per role.
#'
#' @author EDG
#' @export
#'
#' @examples
#' th <- r8_theme(type_name = list(color = "#0F6A66"), muted = list(italic = TRUE))
#' th[["type_name"]]
r8_theme <- function(...) {
  overrides <- list(...)
  if (
    length(overrides) > 0L &&
      (is.null(names(overrides)) || any(!nzchar(names(overrides))))
  ) {
    r8_abort(
      "Each argument of r8_theme() names a role.",
      class = "r8_text_error"
    )
  }
  structure(
    r8_theme_table(overrides),
    class = "r8_theme",
    r8_named = as.character(names(overrides))
  )
} # /R8::r8_theme


# %% format.r8_theme ----
#' @rdname r8_theme
#' @param x An `r8_theme` object.
#' @export
format.r8_theme <- function(x, ...) {
  x <- unclass(x)
  width <- max(nchar(names(x)))
  vapply(
    names(x),
    function(role) {
      s <- x[[role]]
      style <- c(
        if (!is.na(s[["color"]])) {
          sprintf("%s (%d)", s[["color"]], s[["index"]])
        },
        if (isTRUE(s[["bold"]])) "bold",
        if (isTRUE(s[["italic"]])) "italic"
      )
      sprintf(
        "%-*s %s",
        width,
        role,
        if (length(style)) paste(style, collapse = " ") else "default"
      )
    },
    character(1),
    USE.NAMES = FALSE
  )
} # /R8::format.r8_theme


# %% print.r8_theme ----
#' @rdname r8_theme
#' @export
print.r8_theme <- function(x, ...) {
  cat("<r8 theme>", format(x), sep = "\n")
  invisible(x)
} # /R8::print.r8_theme


# %% r8_css ----
#' The stylesheet of R8's html classes mode
#'
#' HTML that `format(x, output_type = "html", html_style = "classes")`
#' writes marks each span with a class per role, `r8-<role>`, inside a `pre`
#' element of class `r8`; this stylesheet styles those classes. It holds the
#' rules of the theme, then, under `@media (prefers-color-scheme: light)` and
#' `(prefers-color-scheme: dark)`, the light and dark variants of R8's
#' theme for the roles the theme does not name: rtemis green in place of
#' teal on light pages, and rtemislive's light and dark colors of base types.
#' Each color is a CSS variable, `--r8-<role>`, which a page can set.
#'
#' @param theme An `r8_theme` object, or `NULL` for `getOption("r8.theme")`
#'   or R8's theme.
#' @param style_element Logical: Wrap the stylesheet in a `<style>` element.
#'
#' @return Character: The stylesheet, one string.
#'
#' @author EDG
#' @export
#'
#' @examples
#' cat(r8_css())
#' cat(r8_css(
#'   r8_theme(type_name = list(color = "#0F6A66")),
#'   style_element = TRUE
#' ))
r8_css <- function(theme = NULL, style_element = FALSE) {
  theme <- theme %||% getOption("r8.theme")
  if (!is.null(theme) && !inherits(theme, "r8_theme")) {
    r8_abort(
      "A theme is an `r8_theme` object, from r8_theme().",
      class = "r8_text_error"
    )
  }
  named <- if (is.null(theme)) {
    character()
  } else {
    as.character(attr(theme, "r8_named"))
  }
  out <- r8_css_text(if (is.null(theme)) NULL else unclass(theme), named)
  if (isTRUE(style_element)) paste0("<style>\n", out, "\n</style>") else out
} # /R8::r8_css


# %% determine_theme ----
#' The theme table of format() and print()
#'
#' @param theme An `r8_theme` object or `NULL`.
#'
#' @return List or `NULL`: The theme table, `NULL` for R8's theme.
#'
#' @keywords internal
#' @noRd
determine_theme <- function(theme) {
  theme <- theme %||% .r8_print[["theme"]] %||% getOption("r8.theme")
  if (is.null(theme)) {
    return(NULL)
  }
  if (!inherits(theme, "r8_theme")) {
    r8_abort(
      "A theme is an `r8_theme` object, from r8_theme().",
      class = "r8_text_error"
    )
  }
  unclass(theme)
} # /R8::determine_theme


# %% encode_text ----
#' Text encoded as one output type
#'
#' @param x Text: an `r8_text` object or anything `as_r8_text()` takes.
#' @param output_type Character: The output type.
#' @param theme An `r8_theme` object or `NULL`.
#' @param depth Character or `NULL`: The color depth of `ansi`; `NULL`
#'   determines it.
#' @param inline Logical: markdown writes one line.
#' @param html_style Character or `NULL`: `"inline"` or `"classes"`; `NULL`
#'   determines it.
#'
#' @return Character: One string, lines joined by line breaks.
#'
#' @keywords internal
#' @noRd
encode_text <- function(
  x,
  output_type,
  theme = NULL,
  depth = NULL,
  inline = FALSE,
  html_style = NULL
) {
  check_output_type(output_type)
  html_style <- determine_html_style(html_style, output_type)
  depth <- depth %||%
    .r8_print[["depth"]] %||%
    determine_output(output_type)[[2L]]
  r8_text_encode(
    text_parts(as_r8_text(x)),
    output_type,
    determine_theme(theme),
    depth,
    identical(html_style, "classes"),
    isTRUE(inline)
  )
} # /R8::encode_text


# %% determine_html_style ----
#' The style of html output
#'
#' @param html_style Character or `NULL`: `"inline"` or `"classes"`.
#' @param output_type Character: The output type.
#'
#' @return Character: `"inline"` or `"classes"`.
#'
#' @keywords internal
#' @noRd
determine_html_style <- function(html_style, output_type) {
  if (!is.null(html_style) && !identical(output_type, "html")) {
    r8_abort(
      "`html_style` applies to the output type \"html\" only.",
      class = "r8_text_error"
    )
  }
  style <- html_style %||% getOption("r8.html_style") %||% "inline"
  if (
    !(is.character(style) &&
      length(style) == 1L &&
      style %in% c("inline", "classes"))
  ) {
    r8_abort(
      "`html_style` is \"inline\" or \"classes\".",
      class = "r8_text_error"
    )
  }
  style
} # /R8::determine_html_style


# %% with_print_state ----
#' Runs `expr` with the output type of a running print()
#'
#' @param determined Character: The output type and color depth.
#' @param theme An `r8_theme` object or `NULL`.
#' @param expr The code to run.
#'
#' @return The value of `expr`.
#'
#' @keywords internal
#' @noRd
with_print_state <- function(determined, theme, expr) {
  old <- as.list(.r8_print)
  on.exit({
    rm(list = ls(.r8_print, all.names = TRUE), envir = .r8_print)
    list2env(old, envir = .r8_print)
  })
  .r8_print[["output_type"]] <- determined[[1L]]
  .r8_print[["depth"]] <- determined[[2L]]
  .r8_print[["theme"]] <- theme
  expr
} # /R8::with_print_state


# %% format.r8_text ----
#' Text output of R8 values and text
#'
#' `format()` encodes text as plain text unless its caller names another
#' output type, so that files, snapshots, and `paste()` receive no markup.
#' `print()` writes the output type that `r8_output_type()` determines. The
#' text of an R8 value is its type's `Display` impl's, or its default layout
#' (`r8_layout()`); a `Print` impl replaces the output of `print()`, and a
#' `Debug` impl the output of `str()`. The default `str()` adds to the size
#' of a Rust-stored value the fields read into R that are not views of its
#' state, which together make `r8_size(x)`: `<pkgr::Signal> 7.63 MiB + 112 B
#' read`.
#'
#' @param x,object An R8 value or an `r8_text` object.
#' @param output_type Character: `"plain"`, `"ansi"`, `"html"`, or
#'   `"markdown"`; `NULL` in `print()` determines it with `r8_output_type()`.
#' @param theme An `r8_theme` object, or `NULL` for `getOption("r8.theme")`
#'   or R8's theme.
#' @param inline Logical: markdown writes one line.
#' @param html_style Character or `NULL`: `"inline"`, which styles each span
#'   through its own `style` attribute, or `"classes"`, which marks spans by
#'   class only, for the stylesheet of `r8_css()`, where the theme then
#'   applies. `NULL` reads `getOption("r8.html_style")`, then `"inline"`;
#'   `print()`, knitr, and Jupyter read the option. Only for the output type
#'   `"html"`.
#' @param ... Passed to a `Display` impl by `format()`, and to a `Print`
#'   impl by `print()`.
#'
#' @return `format()`: Character, one element per line. `print()`: `x`,
#'   invisibly. `str()`: `NULL`, invisibly.
#'
#' @name r8_value_text
#' @export
#'
#' @examples
#' GBM <- new_struct("GBM", fields = list(n_trees = type_integer()))
#' x <- GBM(n_trees = 500L)
#' format(x)
#' format(x, output_type = "html")
#' print(x, output_type = "plain")
#' str(x)
format.r8_text <- function(
  x,
  output_type = "plain",
  theme = NULL,
  inline = FALSE,
  html_style = NULL,
  ...
) {
  out <- encode_text(
    x,
    output_type,
    theme,
    inline = inline,
    html_style = html_style
  )
  if (length(x) == 0L) character() else strsplit(out, "\n", fixed = TRUE)[[1L]]
} # /R8::format.r8_text


# %% print.r8_text ----
#' @rdname r8_value_text
#' @export
print.r8_text <- function(x, output_type = NULL, theme = NULL, ...) {
  determined <- if (
    is.null(output_type) && !is.null(.r8_print[["output_type"]])
  ) {
    c(.r8_print[["output_type"]], .r8_print[["depth"]])
  } else {
    determine_output(output_type)
  }
  if (length(x) > 0L) {
    cat(
      encode_text(x, determined[[1L]], theme, determined[[2L]]),
      "\n",
      sep = ""
    )
  }
  invisible(x)
} # /R8::print.r8_text


# %% format.r8_value ----
#' @rdname r8_value_text
#' @export
format.r8_value <- function(
  x,
  output_type = "plain",
  theme = NULL,
  inline = FALSE,
  html_style = NULL,
  ...
) {
  format(
    value_text(x),
    output_type = output_type,
    theme = theme,
    inline = inline,
    html_style = html_style
  )
} # /R8::format.r8_value


# %% print.r8_value ----
#' @rdname r8_value_text
#' @export
print.r8_value <- function(x, output_type = NULL, theme = NULL, ...) {
  print(value_text(x), output_type = output_type, theme = theme)
  invisible(x)
} # /R8::print.r8_value


# %% str.r8_value ----
#' @rdname r8_value_text
#' @export
str.r8_value <- function(object, ...) {
  if (!is_value(object)) {
    print(value_text(object))
    return(invisible(NULL))
  }
  parts <- r8_rust_stored_parts(object)
  summary <- NULL
  if (!is.null(parts)) {
    # The fields read, counted before the layout, which reads a duplicate.
    read <- sum(vapply(
      parts[["fields"]],
      function(f) as.numeric(utils::object.size(f)),
      numeric(1)
    ))
    summary <- r8_format_size(parts[["state"]])
    if (read > 0) {
      summary <- paste0(summary, " + ", r8_format_size(read), " read")
    }
  }
  print(layout_value(object, summary = summary))
  invisible(NULL)
} # /R8::str.r8_value


# %% display_method ----
#' The S3 format method of a `Display` impl
#'
#' Encodes the impl's text as `format.r8_value()` does.
#'
#' @param f Function: The impl's `format` method.
#'
#' @return Function.
#'
#' @keywords internal
#' @noRd
display_method <- function(f) {
  force(f)
  function(
    x,
    output_type = "plain",
    theme = NULL,
    inline = FALSE,
    html_style = NULL,
    ...
  ) {
    format(
      if (is.null(r8_value_other(x))) as_r8_text(f(x, ...)) else value_text(x),
      output_type = output_type,
      theme = theme,
      inline = inline,
      html_style = html_style
    )
  }
} # /R8::display_method


# %% print_method ----
#' The S3 print method of a `Print` impl
#'
#' Determines the output type, which `r8_output_type()` reports while the impl
#' runs, calls the impl, writes the `r8_text` object it returns in that
#' type, and returns the value invisibly. An impl that writes with `cat()`
#' and returns anything else writes once.
#'
#' @param f Function: The impl's `print` method.
#'
#' @return Function.
#'
#' @keywords internal
#' @noRd
print_method <- function(f) {
  force(f)
  function(x, output_type = NULL, theme = NULL, ...) {
    if (!is.null(r8_value_other(x))) {
      print(value_text(x), output_type = output_type, theme = theme)
      return(invisible(x))
    }
    determined <- determine_output(output_type)
    with_print_state(determined, theme, {
      out <- f(x, ...)
      if (inherits(out, "r8_text")) {
        print(out)
      }
    })
    invisible(x)
  }
} # /R8::print_method
