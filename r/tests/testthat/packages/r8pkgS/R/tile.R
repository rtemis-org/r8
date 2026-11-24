# tile.R
# ::r8pkgS::
# 2026- EDG rtemis.org

# The generics declared in R whose impls for Tile are in Rust, with a parameter
# and a result of a type of r8pkgR.

gap <- R8::generic("gap", R8::params(x, y = R8::arg(r8pkgR::Interval)))
lower <- R8::generic("lower", R8::params(x), return_type = r8pkgR::Interval)
