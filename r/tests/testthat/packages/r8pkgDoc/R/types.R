# types.R
# ::r8pkgDoc::
# 2026- EDG rtemis.org

#' Resampling method
#'
#' The method a resampler draws its resamples by.
#'
#' @format An R8 enum with the variants `KFold` and `Bootstrap`.
#' @export
Method <- R8::new_enum(
  "Method",
  variants = list(
    KFold = R8::variant(fields = list(k = R8::type_integer(min = 2L))),
    Bootstrap = R8::variant()
  )
)

#' Resampler
#'
#' A resampling configuration.
#'
#' @param method `Method`: The resampling method.
#' @param n_resamples Integer: The number of resamples.
#' @param seed Optional Integer: The random seed.
#'
#' @return A `Resampler` value.
#'
#' @examples
#' Resampler(method = Method@KFold(k = 5L), n_resamples = 5L)
#' @export
Resampler <- R8::new_struct(
  "Resampler",
  fields = list(
    method = R8::field(Method),
    n_resamples = R8::type_integer(min = 1L),
    seed = R8::option(R8::type_integer())
  )
)
