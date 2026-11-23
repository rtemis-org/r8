# r8-types.R
# ::r8pkgR::
# R8::r8_document_rust() writes this file from the R8 declarations in
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
  storage = "rust"
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
  storage = "rust"
)

#' Conform
#'
#' A field of every Rust field type, with constraints, a default, and rules.
#'
#' The struct `pkgr::Conform`, declared in Rust.
#'
#' @param ratio A fraction.
#' @param count A count.
#' @param flag A flag.
#' @param label A lowercase label.
#' @param seed An optional seed.
#' @param weights Weights.
#' @param index Case indices.
#' @param bytes Bytes.
#' @param mask A mask.
#' @return A `pkgr::Conform` value.
#' @export
Conform <- R8::new_struct(
  "Conform",
  fields = list(
    ratio = R8::field(
      R8::type_double(min = 0, max = 1),
      description = "A fraction."
    ),
    count = R8::field(
      R8::type_integer(min = 1L),
      default = 10L,
      description = "A count."
    ),
    flag = R8::field(R8::type_logical(), description = "A flag."),
    label = R8::field(
      R8::type_character(pattern = "^[a-z]+$"),
      description = "A lowercase label."
    ),
    seed = R8::field(
      R8::option(R8::type_integer()),
      description = "An optional seed."
    ),
    weights = R8::field(R8::vec(R8::type_double()), description = "Weights."),
    index = R8::field(
      R8::vec(R8::type_integer()),
      description = "Case indices."
    ),
    bytes = R8::field(R8::type_raw(), description = "Bytes."),
    mask = R8::field(R8::vec(R8::type_logical()), description = "A mask.")
  ),
  description = "A field of every Rust field type, with constraints, a default, and rules.",
  version = 1L,
  rules = list(
    R8::rule(
      seed < count,
      when = R8::json_ir('{"!==":[{"var":"seed"},null]}'),
      message = "The seed is below the count.",
      id = "seed_below_count"
    ),
    R8::rule(
      R8::json_ir(
        '{"===":[{"length":{"var":"weights"}},{"length":{"var":"index"}}]}'
      ),
      message = "There is one weight per index.",
      id = "weights_match_index"
    )
  ),
  storage = "rust"
)
