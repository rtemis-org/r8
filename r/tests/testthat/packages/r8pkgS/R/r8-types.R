# r8-types.R
# ::r8pkgS::
# R8::r8document_rust() writes this file from the R8 declarations in
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

#' Grid
#'
#' A grid of cells.
#'
#' The struct `pkgs::Grid`, declared in Rust.
#'
#' @param cell The cell, a value of r8pkgR's type.
#' @param method The resampling method, a value of r8pkgR's enum.
#' @param extent The extent.
#' @param figure The figure.
#' @param note A value with a note.
#' @return A `pkgs::Grid` value.
#' @export
Grid <- R8::new_struct(
  "Grid",
  fields = list(
    cell = R8::field(
      r8pkgR::Interval,
      description = "The cell, a value of r8pkgR's type."
    ),
    method = R8::field(
      r8pkgR::Resampler,
      description = "The resampling method, a value of r8pkgR's enum."
    ),
    extent = R8::field(
      R8::subtype(r8pkgR::Interval),
      description = "The extent."
    ),
    figure = R8::field(
      R8::option(R8::dyn(r8pkgR::Figure)),
      description = "The figure."
    ),
    note = R8::field(
      R8::option(R8::dyn(Noted)),
      description = "A value with a note."
    )
  ),
  description = "A grid of cells.",
  version = 1L,
  storage = "rust"
)

#' cells
#'
#' The number of cells in the extent, and the band that the extent is.
#'
#' The generic `pkgs::cells`, declared in Rust.
#'
#' @param x A value, whose type selects the method.
#' @return The result of the method: character.
#' @export
cells <- R8::generic(
  "cells",
  R8::params(x),
  description = "The number of cells in the extent, and the band that the extent is.",
  return_type = R8::type_character()
)

#' peak
#'
#' The peak of a signal, a value of r8pkgR's type, in the extent of a grid.
#'
#' The generic `pkgs::peak`, declared in Rust.
#'
#' @param x A value, whose type selects the method.
#' @param y The signal.
#' @return The result of the method: double.
#' @export
peak <- R8::generic(
  "peak",
  R8::params(x, y = R8::arg(r8pkgR::Signal)),
  description = "The peak of a signal, a value of r8pkgR's type, in the extent of a grid.",
  return_type = R8::type_double()
)

#' side
#'
#' The side of the square that the figure of a grid is, if it is one; the type is named in no field and no signature of this crate.
#'
#' The generic `pkgs::side`, declared in Rust.
#'
#' @param x A value, whose type selects the method.
#' @return The result of the method: optional double.
#' @export
side <- R8::generic(
  "side",
  R8::params(x),
  description = "The side of the square that the figure of a grid is, if it is one; the type is named in no field and no signature of this crate.",
  return_type = R8::option(R8::type_double())
)
