# types.R
# ::r8pkgB::
# 2026- EDG rtemis.org

Circle <- R8::new_struct("Circle", fields = list(r = R8::type_double(min = 0)))

# An impl of another package's trait for this package's type: allowed in the
# type's namespace.
R8::impl(
  r8pkgA::Area,
  Circle,
  methods = list(area = function(x, ...) pi * x$r^2)
)
