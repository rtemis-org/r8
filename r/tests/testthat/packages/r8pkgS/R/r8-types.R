# r8-types.R
# ::r8pkgS::
# R8::r8_document_rust() writes this file from the R8 declarations in
# the package's Rust code, and replaces it when they change.

#' Tile
#'
#' A numbered interval.
#'
#' The struct `pkgs::Tile`, declared in Rust.
#'
#' @param lo The lower end.
#' @param hi The upper end.
#' @param unit The unit of the ends.
#' @param number The number.
#' @return A `pkgs::Tile` value.
#' @export
Tile <- R8::new_struct(
  "Tile",
  fields = list(
    number = R8::field(R8::type_integer(min = 1L), description = "The number.")
  ),
  description = "A numbered interval.",
  version = 1L,
  extends = r8pkgR::Interval,
  storage = "rust"
)
