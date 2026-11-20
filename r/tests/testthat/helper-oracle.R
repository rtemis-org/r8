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

oracle_document_identity <- function(text, type_name) {
  .Call("wrap__r8_document_identity", text, type_name, PACKAGE = "R8")
}
