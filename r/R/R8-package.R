# R8-package.R
# ::R8::
# 2026- EDG rtemis.org

#' @name R8-package
#'
#' @title R8: Structs, Enums, and Traits with a 'Rust' Runtime
#'
#' @description
#' A type system for R whose runtime is the 'rtemis-r8' 'Rust' crate, shared
#' with other host languages.
#'
#' @section Runtime:
#' [r8_core_version()].
#'
#' @section Names:
#' A function that a declaration is written with takes no prefix:
#' [new_struct()], [type_double()], [field()], [rule()], [impl()],
#' [generic()], and the standard traits. A function that operates on values,
#' types, or packages takes the prefix `r8_`: [r8_to_json()], [r8_hash()],
#' [r8_validate()], [r8_register()]. S3 classes, condition classes, and value
#' attributes take the prefix `r8_`; `R8_` prefixes the constants of the C
#' header `R8.h` and the environment variable `R8_OUTPUT_TYPE`.
#'
#' @section Conditions:
#' Every error R8 signals has the class `r8_error`, after one of:
#' - `r8_declaration_error`: a declaration of a type, trait, impl, or generic
#'   that does not pass its checks, or a package whose declarations do not
#'   register.
#' - `r8_type_error`: an argument of the wrong kind, such as a type object
#'   where a value is expected.
#' - `r8_value_error`: a value that does not satisfy its type; its subclass
#'   `r8_codec_missing` is a Rust-native type whose codec is not registered.
#' - `r8_dispatch_error`: a generic or trait method with no impl for the
#'   value's type.
#' - `r8_text_error`: text output given invalid arguments.
#'
#' Every warning R8 signals has the class `r8_warning`; `r8_stale_warning` is
#' a declaration that leaves structs stale, which extend an earlier
#' declaration of their parent.
#'
#' @keywords internal
"_PACKAGE"
