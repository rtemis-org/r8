# functions.R
# ::r8pkgR::
# 2026- EDG rtemis.org

# Typed functions whose bodies are in Rust (src/rust/src/functions.rs).
# `spread()`, `portion()`, `counted()`, and `filled()` return one value, which
# the expressions of R8::calc() call once for each group; `count_of()` gives the
# calls of `counted()` so far.

clip <- R8::new_function(
  "clip",
  function(
    x = R8::arg_vec(R8::type_double(), description = "The values."),
    lo = R8::arg_double(description = "The lower bound."),
    hi = R8::arg_double(description = "The upper bound.")
  ) {
    R8::rust()
  },
  rules = list(order = R8::rule(lo <= hi)),
  return_type = R8::vec(R8::type_double()),
  description = "Holds each element between `lo` and `hi`."
)

level <- R8::new_function(
  "level",
  function(
    x = R8::arg_vec(R8::type_double(), description = "Inflows, in order."),
    lo = R8::arg_double(default = 0, description = "The floor."),
    hi = R8::arg_double(description = "The capacity.")
  ) {
    R8::rust()
  },
  rules = list(order = R8::rule(lo <= hi)),
  return_type = R8::vec(R8::type_double()),
  description = "Holds a running sum between `lo` and `hi`."
)

tally <- R8::new_function(
  "tally",
  function(
    x = R8::arg_vec(R8::type_integer()),
    label = R8::arg_character(default = "n"),
    flags = R8::arg_option(R8::vec(R8::type_logical())),
    bytes = R8::arg_raw(default = as.raw(1:3)),
    na.rm = R8::arg_logical(default = FALSE),
    data = NULL,
    k = R8::arg_integer(default = length(x)),
    ...
  ) {
    R8::rust()
  },
  return_type = R8::type_character()
)

fraction <- R8::new_function(
  "fraction",
  function(part = R8::arg_double(), whole = R8::arg_double()) R8::rust(),
  return_type = R8::type_double(max = 1)
)

unwound <- R8::new_function(
  "unwound",
  function() R8::rust(),
  return_type = R8::type_double()
)

spread <- R8::new_function(
  "spread",
  function(x = R8::arg_vec(R8::type_double(), description = "The values.")) {
    R8::rust()
  },
  return_type = R8::type_double(),
  description = "The largest value less the smallest."
)

portion <- R8::new_function(
  "portion",
  function(
    x = R8::arg_vec(R8::type_double(), description = "The parts."),
    whole = R8::arg_vec(R8::type_double(), description = "The wholes."),
    scale = R8::arg_double(
      default = 1,
      description = "The factor of the ratio."
    )
  ) {
    R8::rust()
  },
  rules = list(scale = R8::rule(scale > 0)),
  return_type = R8::type_double(min = 0),
  description = "`scale` times the sum of `x` over the sum of `whole`."
)

counted <- R8::new_function(
  "counted",
  function(x = R8::arg_vec(R8::type_double(), description = "The values.")) {
    R8::rust()
  },
  return_type = R8::type_double(),
  description = "The sum of `x`, counting its calls; an error for a negative value."
)

count_of <- R8::new_function(
  "count_of",
  function() R8::rust(),
  return_type = R8::type_integer(),
  description = "The calls of `counted()` so far."
)

filled <- R8::new_function(
  "filled",
  function(
    x = R8::arg_vec(R8::type_double(), description = "The values."),
    n = R8::arg_integer(
      description = "The length of the vector that R allocates."
    )
  ) {
    R8::rust()
  },
  return_type = R8::type_double(),
  description = "The length of `x`, after R allocates a vector of `n` doubles."
)
