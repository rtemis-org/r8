# functions.R
# ::r8pkgDoc::
# 2026- EDG rtemis.org

#' @include types.R
NULL

#' Limit the resamples of a value
#'
#' @param r The resampler.
#' @param most The most resamples.
#'
#' @examples
#' clip_resamples(Resampler(method = Method@Bootstrap, n_resamples = 20L), most = 5L)
#' @export
clip_resamples <- R8::new_function(
  "clip_resamples",
  function(
    r = R8::arg(Resampler),
    most = R8::arg_integer(min = 1L, default = 10L)
  ) {
    min(r$n_resamples, most)
  },
  return_type = R8::type_integer()
)
