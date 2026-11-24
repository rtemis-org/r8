# helper-oracle.R
# ::R8::
# 2026- EDG rtemis.org

# The differential oracle (src/rust/src/oracle.rs): construction and identity
# through the JSON form. A build with R8_ORACLE set registers its routines;
# R/extendr-wrappers.R holds no wrapper for them, so the tests call them by
# name.

# Whether the installed R8 carries the oracle. With R8_ORACLE set, a build
# without it is an error, so that the differential tests run wherever the
# oracle is expected.
has_oracle <- function() {
  found <- is.loaded("wrap__r8_new_value_json", PACKAGE = "R8")
  if (!found && nzchar(Sys.getenv("R8_ORACLE"))) {
    stop("R8_ORACLE is set, and R8 was built without the differential oracle.")
  }
  found
}

skip_without_oracle <- function() {
  skip_if_not(has_oracle(), "R8 was built without the differential oracle")
}

oracle_new_value <- function(fields, type_name, variant) {
  .Call("wrap__r8_new_value_json", fields, type_name, variant, PACKAGE = "R8")
}

# An R-stored value of any type and fields, as another declaration of the type
# made it, or of a type whose values Rust holds.
oracle_any_value <- function(fields, type, variant = "", declaration = "") {
  .Call(
    "wrap__r8_any_value",
    fields,
    type,
    variant,
    declaration,
    PACKAGE = "R8"
  )
}

# The source of `r_stored()`, for the R sessions of the package tests: an
# R-stored value with the fields of `x`.
R_STORED <- c(
  "r_stored <- function(x, fields = as.list(x)) {",
  "  .Call('wrap__r8_any_value', fields, R8:::value_type(x), '', '', PACKAGE = 'R8')",
  "}"
)

oracle_document_identity <- function(text, type_name) {
  .Call("wrap__r8_document_identity", text, type_name, PACKAGE = "R8")
}
