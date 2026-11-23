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

#' Probe
#'
#' A field of every Rust field type, with constraints, a default, and rules.
#'
#' The struct `pkgr::Probe`, declared in Rust.
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
#' @return A `pkgr::Probe` value.
#' @export
Probe <- R8::new_struct(
  "Probe",
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

#' Interval
#'
#' A numeric interval.
#'
#' The struct `pkgr::Interval`, declared in Rust.
#'
#' @param lo The lower end.
#' @param hi The upper end.
#' @param unit The unit of the ends.
#' @return A `pkgr::Interval` value.
#' @export
Interval <- R8::new_struct(
  "Interval",
  fields = list(
    lo = R8::field(R8::type_double(), description = "The lower end."),
    hi = R8::field(
      R8::type_double(),
      default = 1,
      description = "The upper end."
    ),
    unit = R8::field(
      R8::option(R8::type_character()),
      description = "The unit of the ends."
    )
  ),
  description = "A numeric interval.",
  version = 1L,
  rules = list(
    R8::rule(lo <= hi, id = "ordered")
  ),
  storage = "rust"
)

#' Band
#'
#' A labeled interval with weights.
#'
#' The struct `pkgr::Band`, declared in Rust.
#'
#' @param lo The lower end.
#' @param hi The upper end.
#' @param unit The unit of the ends.
#' @param label The label.
#' @param weights The weights.
#' @return A `pkgr::Band` value.
#' @export
Band <- R8::new_struct(
  "Band",
  fields = list(
    label = R8::field(R8::type_character(), description = "The label."),
    weights = R8::field(
      R8::vec(R8::type_double()),
      description = "The weights."
    )
  ),
  description = "A labeled interval with weights.",
  version = 1L,
  rules = list(
    R8::rule(label != "", id = "labeled")
  ),
  extends = Interval,
  defaults = list(lo = 0, unit = "cm"),
  storage = "rust"
)

#' Strip
#'
#' A numbered band without a default unit.
#'
#' The struct `pkgr::Strip`, declared in Rust.
#'
#' @param lo The lower end.
#' @param hi The upper end.
#' @param unit The unit of the ends.
#' @param label The label.
#' @param weights The weights.
#' @param number The number.
#' @return A `pkgr::Strip` value.
#' @export
Strip <- R8::new_struct(
  "Strip",
  fields = list(
    number = R8::field(
      R8::type_integer(min = 1L),
      default = 1L,
      description = "The number."
    )
  ),
  description = "A numbered band without a default unit.",
  version = 1L,
  extends = Band,
  defaults = list(unit = NULL),
  storage = "rust"
)
