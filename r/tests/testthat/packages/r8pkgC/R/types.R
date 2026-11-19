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
