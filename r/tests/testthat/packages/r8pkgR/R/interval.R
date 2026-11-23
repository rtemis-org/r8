# interval.R
# ::r8pkgR::
# 2026- EDG rtemis.org

# The generics whose impls for Interval and Band are in Rust (src/rust).

width <- R8::generic("width", R8::params(x), return_type = R8::type_double())
middle <- R8::generic("middle", R8::params(x))
shifted <- R8::generic("shifted", R8::params(x, by))
overlap <- R8::generic("overlap", R8::params(x, y))
total <- R8::generic("total", R8::params(x))
shares <- R8::generic("shares", R8::params(x))

# An impl in R, which continues to the compiled impl of its parent.
R8::implement(total, Strip) <- function(x) x[["number"]] + R8::r8_next_impl()
