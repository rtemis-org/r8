# r8-types.R
# ::r8pkgR::
# R8::r8_document_native() writes this file from the R8 declarations in
# the package's Rust code, and replaces it when they change.

#' Signal
#'
#' A named series of values.
#'
#' The struct `pkgr::Signal`, declared in Rust.
#'
#' @param name The name.
#' @param values The values.
#' @return A `pkgr::Signal` value.
#' @export
Signal <- R8::new_struct(
  "Signal",
  fields = list(
    name = R8::field(R8::type_character(), description = "The name."),
    values = R8::field(R8::vec(R8::type_double()), description = "The values.")
  ),
  description = "A named series of values.",
  version = 1L,
  native = TRUE
)
