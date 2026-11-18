# utils.R
# ::R8::
# 2026- EDG rtemis.org

# %% r8_abort ----
#' Raise an R8 error
#'
#' @param ... Character: Message parts, pasted without a separator.
#' @param class Character: Condition classes, before `r8_error`.
#' @param call Optional call to report.
#'
#' @return Does not return.
#'
#' @keywords internal
#' @noRd
r8_abort <- function(..., class = character(), call = NULL) {
  cond <- structure(
    class = c(class, "r8_error", "error", "condition"),
    list(message = paste0(...), call = call)
  )
  stop(cond)
} # /R8::r8_abort


# %% is_name ----
#' An R8 name
#'
#' @param x Object.
#'
#' @return Logical: `x` is one string matching `[A-Za-z][A-Za-z0-9_]*`.
#'
#' @keywords internal
#' @noRd
is_name <- function(x) {
  is.character(x) &&
    length(x) == 1L &&
    !is.na(x) &&
    grepl("^[A-Za-z][A-Za-z0-9_]*$", x)
} # /R8::is_name


# %% declaring_namespace ----
#' The namespace of declarations made in an environment
#'
#' Code in a package declares types in the namespace of the `DESCRIPTION`
#' field `Config/R8/namespace`, which defaults to the package name. Code
#' outside a package uses the option `r8.namespace`, which defaults to
#' `"global"`.
#'
#' @param env Environment: Where the declaration is made.
#'
#' @return Character: The namespace.
#'
#' @keywords internal
#' @noRd
declaring_namespace <- function(env) {
  top <- topenv(env)
  if (isNamespace(top)) {
    pkg <- getNamespaceName(top)
    declared <- utils::packageDescription(pkg, fields = "Config/R8/namespace")
    ns <- if (is.character(declared) && !is.na(declared)) declared else pkg
    if (!is_name(ns)) {
      r8_abort(
        "Package '",
        pkg,
        "' declares R8 types; its namespace `",
        ns,
        "` is not an R8 name. Set `Config/R8/namespace` in its DESCRIPTION.",
        class = "r8_declaration_error"
      )
    }
    return(ns)
  }
  ns <- getOption("r8.namespace", "global")
  if (!is_name(ns)) {
    r8_abort(
      "The option `r8.namespace` is not an R8 name: `",
      ns,
      "`.",
      class = "r8_declaration_error"
    )
  }
  ns
} # /R8::declaring_namespace
