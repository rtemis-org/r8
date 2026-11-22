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

# A Rust-native type whose codec is in C (src/signal.c), which pins the
# hash of this declaration.
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

# Exports tables that registration refuses: a codec of Signal that sets the
# enum members (1), pins another declaration hash (2), or pins none (3).
bad_exports <- function(kind = 1L) .Call(pkgc_bad_exports, as.integer(kind))

# Exports tables whose impl entries registration refuses: a repeated method
# (1) or a NULL function (2).
bad_impls <- function(kind) .Call(pkgc_bad_impls, as.integer(kind))
