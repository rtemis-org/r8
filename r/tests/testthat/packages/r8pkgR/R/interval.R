# interval.R
# ::r8pkgR::
# 2026- EDG rtemis.org

# The generics declared in R whose impls for Interval and Band are in Rust
# (src/rust); the others are declared in Rust (R/r8-types.R).

width <- R8::generic("width", R8::params(x), return_type = R8::type_double())
middle <- R8::generic(
  "middle",
  R8::params(x),
  return_type = R8::subtype(Interval)
)
overlap <- R8::generic("overlap", R8::params(x, y = R8::arg(Interval)))
total <- R8::generic("total", R8::params(x))
shares <- R8::generic("shares", R8::params(x))
`upper<-` <- R8::generic("upper<-", R8::params(x, value = R8::arg_double()))

# An impl in R, which continues to the compiled impl of its parent.
R8::implement(total, Strip) <- function(x) x[["number"]] + R8::r8_next_impl()
