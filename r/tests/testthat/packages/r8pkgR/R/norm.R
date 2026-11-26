# norm.R
# ::r8pkgR::
# 2026- EDG rtemis.org

# The trait is declared in R; its impl for Signal is in Rust (src/rust),
# registered through the package's exports.

Norm <- R8::new_trait(
  "Norm",
  methods = list(
    magnitude = R8::trait_method(
      R8::params(x),
      return_type = R8::type_double()
    ),
    ratio = R8::trait_method(
      R8::params(x, by = R8::arg_double()),
      return_type = R8::type_double()
    ),
    distance = R8::trait_method(
      R8::params(x, y = R8::arg(Signal)),
      return_type = R8::type_double()
    )
  )
)
magnitude <- Norm@magnitude
ratio <- Norm@ratio
distance <- Norm@distance
