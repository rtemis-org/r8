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

#' Square
#'
#' A square.
#'
#' The struct `pkgr::Square`, declared in Rust.
#'
#' @param side The side.
#' @param name The name.
#' @return A `pkgr::Square` value.
#' @export
Square <- R8::new_struct(
  "Square",
  fields = list(
    side = R8::field(R8::type_double(min = 0), description = "The side."),
    name = R8::field(
      R8::type_character(),
      default = "square",
      description = "The name."
    )
  ),
  description = "A square.",
  version = 1L,
  storage = "rust"
)

#' Figure
#'
#' A plane figure with a name of its own.
#'
#' The trait `pkgr::Figure`, declared in Rust.
#'
#' @return The trait object.
#' @export
Figure <- R8::new_trait(
  "Figure",
  methods = list(
    area = R8::trait_method(
      R8::params(x),
      description = "The area.",
      return_type = R8::type_double()
    ),
    scaled = R8::trait_method(
      R8::params(x, by = R8::arg_double(exclusive_min = 0)),
      description = "The figure scaled by a factor.",
      return_type = "Self"
    )
  ),
  description = "A plane figure with a name of its own.",
  tag = "figure",
  required_fields = list(
    name = R8::field(R8::type_character(), description = "The name.")
  )
)

#' area
#'
#' The area.
#'
#' A generic of the trait `pkgr::Figure`.
#'
#' @param x A value, whose type selects the method.
#' @return The result of the method: double.
#' @export
area <- Figure@area

#' scaled
#'
#' The figure scaled by a factor.
#'
#' A generic of the trait `pkgr::Figure`.
#'
#' @param x A value, whose type selects the method.
#' @param by The factor.
#' @return A value of the type of `x`.
#' @export
scaled <- Figure@scaled

#' Panel
#'
#' A panel of a plot.
#'
#' The struct `pkgr::Panel`, declared in Rust.
#'
#' @param range The range.
#' @param inset The band of an inset.
#' @param frame The frame.
#' @param method The resampling method.
#' @param figure The figure.
#' @return A `pkgr::Panel` value.
#' @export
Panel <- R8::new_struct(
  "Panel",
  fields = list(
    range = R8::field(R8::subtype(Interval), description = "The range."),
    inset = R8::field(
      R8::option(R8::subtype(Band)),
      description = "The band of an inset."
    ),
    frame = R8::field(Interval, description = "The frame."),
    method = R8::field(
      R8::option(Resampler),
      description = "The resampling method."
    ),
    figure = R8::field(R8::dyn(Figure), description = "The figure.")
  ),
  description = "A panel of a plot.",
  version = 1L,
  storage = "rust"
)

#' shifted
#'
#' Moves a value.
#'
#' The generic `pkgr::shifted`, declared in Rust.
#'
#' @param x A value, whose type selects the method.
#' @param by The distance, in the unit of the value.
#' @return A value of the type of `x`.
#' @export
shifted <- R8::generic(
  "shifted",
  R8::params(x, by = R8::arg_double(infinite = TRUE, default = 1)),
  description = "Moves a value.",
  return_type = "Self"
)

#' meets
#'
#' How two values meet. The result names the impl and the values that it read.
#'
#' The generic `pkgr::meets`, declared in Rust.
#'
#' @param x A value, whose type selects the method.
#' @param y The other value.
#' @param by A number.
#' @return The result of the method: character.
#' @export
meets <- R8::generic(
  "meets",
  R8::params(x, y, by = R8::arg_double()),
  description = "How two values meet. The result names the impl and the values that it read.",
  dispatch = c("x", "y"),
  return_type = R8::type_character()
)

#' label<-
#'
#' Another label. The impl for `Band` and every struct that extends it keeps the type.
#'
#' The generic `pkgr::with_label`, declared in Rust.
#'
#' @param x A value, whose type selects the method.
#' @param value The label.
#' @return The value with the replacement, of its type.
#' @export
`label<-` <- R8::generic(
  "label<-",
  R8::params(x, value = R8::arg_character(pattern = "^[a-z]*$")),
  description = "Another label. The impl for `Band` and every struct that extends it keeps the type."
)

#' contents
#'
#' What the fields of a panel hold.
#'
#' The generic `pkgr::contents`, declared in Rust.
#'
#' @param x A value, whose type selects the method.
#' @return The result of the method: character.
#' @export
contents <- R8::generic(
  "contents",
  R8::params(x),
  description = "What the fields of a panel hold.",
  return_type = R8::type_character()
)

#' moved
#'
#' The panel with its range moved, which keeps the type of the range.
#'
#' The generic `pkgr::moved`, declared in Rust.
#'
#' @param x A value, whose type selects the method.
#' @param lo The lower end.
#' @param hi The upper end.
#' @return The result of the method: pkgr::Panel.
#' @export
moved <- R8::generic(
  "moved",
  R8::params(x, lo = R8::arg_double(), hi = R8::arg_double()),
  description = "The panel with its range moved, which keeps the type of the range.",
  return_type = Panel
)

#' banded
#'
#' The panel over a new band, and of a new square.
#'
#' The generic `pkgr::banded`, declared in Rust.
#'
#' @param x A value, whose type selects the method.
#' @param label The parameter `label`.
#' @return The result of the method: pkgr::Panel.
#' @export
banded <- R8::generic(
  "banded",
  R8::params(x, label = R8::arg_character()),
  description = "The panel over a new band, and of a new square.",
  return_type = Panel
)

#' held
#'
#' The value of the range.
#'
#' The generic `pkgr::held`, declared in Rust.
#'
#' @param x A value, whose type selects the method.
#' @return The result of the method: subtype of pkgr::Interval.
#' @export
held <- R8::generic(
  "held",
  R8::params(x),
  description = "The value of the range.",
  return_type = R8::subtype(Interval)
)

#' refit
#'
#' The panel with other values of its open fields, where they are given.
#'
#' The generic `pkgr::refit`, declared in Rust.
#'
#' @param x A value, whose type selects the method.
#' @param range The range.
#' @param figure The figure.
#' @param frame The frame.
#' @param weights Weights, whose sum is added to the upper end of the frame.
#' @return The result of the method: pkgr::Panel.
#' @export
refit <- R8::generic(
  "refit",
  R8::params(
    x,
    range = R8::arg(R8::subtype(Interval)),
    figure = R8::arg(R8::dyn(Figure)),
    frame = R8::arg_option(Interval),
    weights = R8::arg_option(R8::vec(R8::type_double(min = 0)))
  ),
  description = "The panel with other values of its open fields, where they are given.",
  return_type = Panel
)
