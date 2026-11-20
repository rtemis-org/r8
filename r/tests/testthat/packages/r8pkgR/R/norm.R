# norm.R
# ::r8pkgR::
# 2026- EDG rtemis.org

# The trait is declared in R; its impl for Signal is in Rust (src/rust),
# registered through the package's exports.

Norm <- R8::new_trait(
  "Norm",
  methods = list(
    magnitude = R8::trait_method(function(x) NULL),
    ratio = R8::trait_method(function(x, by) NULL)
  )
)
magnitude <- Norm@magnitude
ratio <- Norm@ratio
