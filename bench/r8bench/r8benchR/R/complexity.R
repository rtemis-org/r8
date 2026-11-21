# complexity.R
# ::r8benchR::
# 2026- EDG rtemis.org

# The trait of the benchmark's dispatch row, declared in R; its impl for GBM
# is in Rust (src/rust), registered through the package's exports.

Complexity <- R8::new_trait(
  "Complexity",
  methods = list(complexity = R8::trait_method(function(x) NULL))
)
complexity <- Complexity@complexity
