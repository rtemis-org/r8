# norm.R
# ::r8pkgR::
# 2026- EDG rtemis.org

# The trait is declared in R; its impl for Signal is in Rust (src/rust),
# registered through the package's exports.

Norm <- R8::new_trait(
  "Norm",
  methods = list(
    magnitude = R8::trait_method(R8::params(x)),
    ratio = R8::trait_method(R8::params(x, by)),
    distance = R8::trait_method(R8::params(x, y))
  )
)
magnitude <- Norm@magnitude
ratio <- Norm@ratio
distance <- Norm@distance
