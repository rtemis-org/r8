# traits.R
# ::r8pkgDoc::
# 2026- EDG rtemis.org

#' @include types.R
NULL

#' Resample
#'
#' The trait of types that draw resamples, and its generic `resample()`.
#'
#' @format An R8 trait with the method `resample`.
#' @export
Resample <- R8::new_trait(
  "Resample",
  methods = list(
    resample = R8::trait_method(
      R8::params(x, n_cases, "..."),
      description = "Draws the training sets of n_cases cases."
    )
  )
)

#' @rdname Resample
#' @param x A value whose type implements `Resample`.
#' @param n_cases Integer: The number of cases.
#' @param ... Passed to the impl.
#'
#' @return `resample()`: List of integer vectors, the training cases of each
#'   resample.
#'
#' @examples
#' resample(Resampler(method = Method@Bootstrap, n_resamples = 2L), 10L)
#' @export
resample <- Resample@resample

R8::impl(
  Resample,
  Resampler,
  methods = list(
    resample = function(x, n_cases, ...) {
      lapply(seq_len(x$n_resamples), function(i) {
        sample.int(n_cases, n_cases, replace = TRUE)
      })
    }
  )
)

#' Summaries of resamplers
#'
#' A trait whose method is an S3 method of `summary()`.
#'
#' @format An R8 trait with the method `summary`.
#' @export
Summarize <- R8::new_trait(
  "Summarize",
  methods = list(
    summary = R8::trait_method(
      R8::params(object, "..."),
      description = "A one-line summary."
    )
  )
)

R8::impl(
  Summarize,
  Resampler,
  methods = list(
    summary = function(object, ...) {
      paste(object$n_resamples, "resamples")
    }
  )
)

R8::impl(
  R8::Display,
  Resampler,
  methods = list(
    format = function(x, ...) paste0("Resampler: ", x$n_resamples, " resamples")
  )
)

R8::impl(
  R8::From("numeric"),
  Resampler,
  methods = list(
    from = function(x) {
      Resampler(method = Method@Bootstrap, n_resamples = as.integer(x))
    }
  )
)

#' Convert to a resampler
#'
#' @param x Numeric: The number of bootstrap resamples.
#'
#' @return A `Resampler` value.
#'
#' @examples
#' as_resampler(3)
#' @export
as_resampler <- R8::r8_converter(Resampler)
