# noted.R
# ::r8pkgS::
# 2026- EDG rtemis.org

# The trait of the field `note` of Grid, which precedes the types declared in
# Rust (R/r8-types.R). Its impl for Tile is in Rust.

Noted <- R8::new_trait(
  "Noted",
  methods = list(
    noted = R8::trait_method(
      R8::params(x),
      return_type = R8::type_character()
    )
  )
)
noted <- Noted@noted
