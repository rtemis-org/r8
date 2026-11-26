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
#' [r8_runtime_version()].
#'
#' @section Names:
#' A function that a declaration is written with takes no prefix:
#' [new_struct()], [type_double()], [field()], [rule()], [impl()],
#' [generic()], [params()], [arg()], and the standard traits. A function that operates on values,
#' types, or packages takes the prefix `r8_`: [r8_to_json()], [r8_hash()],
#' [r8_validate()], [r8_register()]. S3 classes, condition classes, and value
#' attributes take the prefix `r8_`; `R8_` prefixes the environment
#' variable `R8_OUTPUT_TYPE`.
#'
#' @section Conditions:
#' Every error R8 signals has the class `r8_error`, after one of:
#' - `r8_declaration_error`: a declaration of a type, trait, impl, or generic
#'   that does not pass its checks, or a package whose declarations do not
#'   register.
#' - `r8_type_error`: an argument of the wrong kind, such as a type object
#'   where a value is expected; its subclass `r8_result_error` is a method's
#'   result that does not satisfy the return type, or a replacement method or
#'   `From` impl that returns a value of another type.
#' - `r8_value_error`: a value that does not satisfy its type; its subclasses
#'   are `r8_argument_error`, the arguments of a generic's call that do not
#'   satisfy their types, `r8_codec_missing_error`, a Rust-stored type whose
#'   codec is not registered, and `r8_unknown_type_error`, a document that
#'   names a type no loaded package declares.
#' - `r8_dispatch_error`: a generic or trait method with no impl for the
#'   value's type, a conversion without a `From` impl, or `r8_next_impl()`
#'   without a next impl.
#' - `r8_other_declaration_error`, beside `r8_value_error` or
#'   `r8_dispatch_error`: a value made under another declaration of its type
#'   than the registered one, which [r8_update()] converts or refuses.
#' - `r8_method_error`: an error that a package's compiled method returned,
#'   with its code in the field `code` and the generic's call; its subclass
#'   `r8_panic` is a panic of the method.
#' - `r8_file_error`: a damaged file ([r8_read_rds()]).
#' - `r8_text_error`: text output given invalid arguments.
#'
#' Every warning R8 signals has the class `r8_warning`:
#'
#' - `r8_stale_warning`: a declaration that leaves structs stale, which extend an
#'   earlier declaration of their parent.
#' - `r8_declaration_warning`: a package whose compiled code was compiled against
#'   another declaration of a type of another package, and may not read it
#'   ([r8_register()]).
#'
#' @section Types that other packages use:
#' A package does not change a type that other packages use without coordinating
#' the change with them, with a minimum version of the declaring package in their
#' `DESCRIPTION`: see the section "Changing a type that other packages use" of
#' [new_struct()].
#'
#' @keywords internal
"_PACKAGE"
