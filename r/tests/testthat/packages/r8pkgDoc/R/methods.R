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
describe <- R8::generic("describe", c("x", "..."), description = "Describes x.")

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
