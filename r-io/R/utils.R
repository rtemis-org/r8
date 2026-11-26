# utils.R
# ::R8.io::
# 2026- EDG rtemis.org

# %% io_abort ----
#' Raise an error
#'
#' @param ... Character: Message parts, pasted without a separator.
#' @param class Character: Condition classes, before `r8_error`.
#' @param data List: Further fields of the condition.
#'
#' @return Does not return.
#'
#' @keywords internal
#' @noRd
io_abort <- function(..., class, data = list()) {
  stop(structure(
    class = c(class, "r8_error", "error", "condition"),
    c(list(message = paste0(...), call = NULL), data)
  ))
} # /R8.io::io_abort


# %% big ----
#' Numbers with marks, and nouns in number
#'
#' @param n Numeric: Counts.
#' @param noun Character: The noun of one.
#'
#' @return Character.
#'
#' @keywords internal
#' @noRd
big <- function(n) format(n, big.mark = ",", trim = TRUE, scientific = FALSE)

counted <- function(n, noun) {
  paste0(big(n), " ", noun, ifelse(n == 1, "", "s"))
} # /R8.io::counted


# %% quoted ----
#' Texts in quotes, cut where they are long
#'
#' @param x Character.
#'
#' @return Character.
#'
#' @keywords internal
#' @noRd
quoted <- function(x) {
  long <- !is.na(x) & nchar(x) > 40L
  x[long] <- paste0(substr(x[long], 1L, 37L), "...")
  encodeString(x, quote = "\"")
} # /R8.io::quoted
