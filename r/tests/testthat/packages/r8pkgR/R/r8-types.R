# r8-types.R
# ::r8pkgR::
# R8::r8_document_native() writes this file from the R8 declarations in
# the package's Rust code, and replaces it when they change.

#' Signal
#'
#' A named series of values.
#'
#' The struct `pkgr::Signal`, declared in Rust.
#'
#' @param name The name.
#' @param values The values.
#' @return A `pkgr::Signal` value.
#' @export
Signal <- R8::new_struct(
  "Signal",
  fields = list(
    name = R8::field(R8::type_character(), description = "The name."),
    values = R8::field(R8::vec(R8::type_double()), description = "The values.")
  ),
  description = "A named series of values.",
  version = 1L,
  native = TRUE
)

#' Split
#'
#' A train and test split of case indices.
#'
#' The struct `pkgr::Split`, declared in Rust.
#'
#' @param train The indices of the training cases.
#' @param test The indices of the test cases.
#' @return A `pkgr::Split` value.
#' @export
Split <- R8::new_struct(
  "Split",
  fields = list(
    train = R8::field(
      R8::vec(R8::type_integer()),
      description = "The indices of the training cases."
    ),
    test = R8::field(
      R8::vec(R8::type_integer()),
      description = "The indices of the test cases."
    )
  ),
  description = "A train and test split of case indices.",
  version = 1L
)

#' Resampler
#'
#' A resampling method.
#'
#' The enum `pkgr::Resampler`, declared in Rust.
#'
#' @return The type object of `pkgr::Resampler`.
#' @export
Resampler <- R8::new_enum(
  "Resampler",
  variants = list(
    KFold = R8::variant(
      fields = list(
        k = R8::field(R8::type_integer(), description = "The number of folds."),
        seed = R8::field(R8::type_integer(), description = "The random seed.")
      ),
      description = "K-fold cross-validation."
    ),
    Bootstrap = R8::variant(
      fields = list(
        n = R8::field(
          R8::type_integer(),
          description = "The number of resamples."
        )
      ),
      description = "Bootstrap resampling."
    ),
    Fixed = R8::variant(
      fields = list(
        train = R8::field(
          R8::vec(R8::type_integer()),
          description = "The indices of the training cases."
        )
      ),
      description = "A fixed set of training cases."
    ),
    Custom = R8::variant(of = Split, description = "A given split."),
    Loocv = R8::variant(description = "Leave-one-out cross-validation.")
  ),
  description = "A resampling method.",
  tag = "type",
  version = 1L,
  native = TRUE
)
