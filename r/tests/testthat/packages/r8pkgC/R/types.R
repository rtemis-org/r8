# types.R
# ::r8pkgC::
# 2026- EDG rtemis.org

# The struct and the trait are declared in R; their impls are in C
# (src/exports.c), registered through R8's C interface.

Point <- R8::new_struct(
  "Point",
  fields = list(x = R8::type_double(), y = R8::type_double())
)

Norm <- R8::new_trait(
  "Norm",
  methods = list(
    magnitude = R8::trait_method(function(x) NULL),
    ratio = R8::trait_method(function(x, by) NULL)
  )
)
magnitude <- Norm@magnitude
ratio <- Norm@ratio

# A Rust-native type whose codec is in C (src/signal.c); the declaration
# equals the one in the exports' IR document.
Signal <- R8::new_struct(
  "Signal",
  fields = list(
    name = R8::type_character(),
    values = R8::vec(R8::type_double())
  ),
  native = TRUE
)

# The number of live Signal states.
live_signals <- function() .Call(pkgc_live_signals)

# A Signal's name and first value, read in C through R8Api.value_state.
signal_peek <- function(x) .Call(pkgc_signal_peek, x)

# A Signal of `n` values 0, 1, ..., built in C through R8Api.new_value.
signal_make <- function(name, n) .Call(pkgc_signal_make, name, as.integer(n))

# The status of R8Api.protect for a callback that raises an R error.
protect_status <- function() .Call(pkgc_protect_status)

# A Rust-native enum whose codec is in C (src/resampler.c).
Resampler <- R8::new_enum(
  "Resampler",
  variants = list(
    KFold = R8::variant(fields = list(k = R8::type_integer())),
    Loocv = R8::variant()
  ),
  native = TRUE
)

# An exports table whose struct codec sets the enum members, which
# registration refuses.
bad_exports <- function() .Call(pkgc_bad_exports)
