# tile.R
# ::r8pkgS::
# 2026- EDG rtemis.org

# The generic whose impl for Tile is in Rust (src/rust), with a parameter of a
# type of r8pkgR.

gap <- R8::generic("gap", R8::params(x, y))
