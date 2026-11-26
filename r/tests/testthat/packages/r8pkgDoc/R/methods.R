#' @include traits.R
#' @importFrom R8 implement<-
NULL

#' Describe a value
#'
#' @param x A value of a type with a method of `describe()`.
#' @param ... Arguments of methods.
#'
#' @return Character: The description.
#'
#' @examples
#' describe(Resampler(method = Method@Bootstrap, n_resamples = 2L))
#' @export
describe <- R8::generic(
  "describe",
  R8::params(x, "..."),
  description = "Describes x."
)

#' @param verbose Logical: Include the seed.
implement(describe, Resampler) <- function(x, ..., verbose = FALSE) {
  out <- paste(x$n_resamples, "resamples")
  if (verbose && !is.null(x$seed)) {
    out <- paste0(out, ", seed ", x$seed)
  }
  out
}

implement(describe, Method) <- function(x, ...) {
  "a resampling method"
}

#' @param x A `Resampler` value.
implement(toString, Resampler) <- function(x, ...) {
  paste0("Resampler(", x$n_resamples, ")")
}

implement(print, Method) <- function(x, ...) {
  cat("A resampling method\n")
}

#' Count the resamples of a value
#'
#' An S3 generic of this package.
#'
#' @param x A value.
#' @param ... Arguments of methods.
#'
#' @return Integer: The number of resamples.
#'
#' @examples
#' n_resamples(Resampler(method = Method@Bootstrap, n_resamples = 4L))
#' @export
n_resamples <- function(x, ...) {
  UseMethod("n_resamples")
}

implement(n_resamples, Resampler) <- function(x, ...) {
  x$n_resamples
}

#' Replace the seed of a value
#'
#' @param x A value of a type with a method of `seed<-`.
#' @param value Integer: The seed.
#'
#' @return A value of the type of `x`, with the seed replaced.
#'
#' @examples
#' r <- Resampler(method = Method@Bootstrap, n_resamples = 2L)
#' seed(r) <- 7L
#' r$seed
#' @export
`seed<-` <- R8::generic(
  "seed<-",
  R8::params(x, value),
  description = "Replaces the seed."
)

implement(`seed<-`, Resampler) <- function(x, value) {
  x$seed <- value
  x
}

implement(as_resampler, "character") <- function(x) {
  Resampler(method = Method@Bootstrap, n_resamples = nchar(x))
}

#' Pair two values
#'
#' @param x A value.
#' @param y A value.
#'
#' @return Character: The pair's description.
#'
#' @examples
#' pair_with(Resampler(method = Method@Bootstrap, n_resamples = 2L), Method@Bootstrap)
#' @export
pair_with <- R8::generic("pair_with", R8::params(x, y), dispatch = c("x", "y"))

implement(pair_with, Resampler, Method) <- function(x, y) {
  "a resampler and a method"
}

implement(pair_with, y = Resampler) <- function(x, y) {
  "anything and a resampler"
}

implement(pair_with, "data.frame", Resampler) <- function(x, y) {
  "a data frame and a resampler"
}

#' Label a value
#'
#' @param x A value of a type with a method of `label_of()`.
#' @param sep The separator.
#' @param n The number of marks.
#' @param method The method.
#' @param ... Arguments of methods.
#'
#' @examples
#' label_of(Resampler(method = Method@Bootstrap, n_resamples = 2L))
#' @export
label_of <- R8::generic(
  "label_of",
  R8::params(
    x,
    sep = R8::arg_character(default = default_sep()),
    n = R8::arg_integer(min = 1L, default = 2L),
    method = R8::arg_option(Method),
    "..."
  ),
  description = "Labels x.",
  return_type = R8::type_character()
)

# An unexported function that a default calls.
default_sep <- function() "-"

implement(label_of, Resampler, return_type = R8::type_character()) <- function(
  x,
  sep,
  n,
  method,
  ...
) {
  paste(rep("r", n), collapse = sep)
}
