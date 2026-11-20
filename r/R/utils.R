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


# %% declaring_package ----
#' The package whose code makes a declaration
#'
#' @param env Environment: Where the declaration is made.
#'
#' @return Character: The package name, or `""` outside a package.
#'
#' @keywords internal
#' @noRd
declaring_package <- function(env) {
  top <- topenv(env)
  if (isNamespace(top)) unname(getNamespaceName(top)) else ""
} # /R8::declaring_package


# %% claim_namespace ----
#' Claim a namespace for the declaring package, or abort
#'
#' @param namespace Character: The namespace.
#' @param package Character: The declaring package, or `""`.
#'
#' @return `NULL`, invisibly.
#'
#' @keywords internal
#' @noRd
claim_namespace <- function(namespace, package) {
  owner <- r8_claim_namespace(namespace, package)
  if (nzchar(owner)) {
    r8_abort(
      "The R8 namespace `",
      namespace,
      "` belongs to the package '",
      owner,
      "'",
      if (nzchar(package)) {
        paste0("; package '", package, "' declares into it")
      } else {
        ""
      },
      ". Declare into another namespace (`Config/R8/namespace` or the option `r8.namespace`).",
      class = "r8_declaration_error"
    )
  }
  invisible(NULL)
} # /R8::claim_namespace


# %% record_declaration ----
#' Record a declaration in the declaring package's namespace
#'
#' A package's top-level declarations run when the package is installed, in
#' another R session; `r8_register()` replays the record when the package
#' loads. Declarations outside a package, or after the namespace is locked,
#' are not recorded.
#'
#' @param env Environment: Where the declaration is made.
#' @param entry List: The declaration, with its `kind`.
#'
#' @return `NULL`, invisibly.
#'
#' @keywords internal
#' @noRd
record_declaration <- function(env, entry) {
  top <- topenv(env)
  if (!isNamespace(top) || environmentIsLocked(top)) {
    return(invisible(NULL))
  }
  if (!exists(".__r8__", envir = top, inherits = FALSE)) {
    assign(".__r8__", new.env(parent = emptyenv()), envir = top)
    top[[".__r8__"]][["entries"]] <- list()
  }
  table <- top[[".__r8__"]]
  table[["entries"]] <- c(table[["entries"]], list(entry))
  invisible(NULL)
} # /R8::record_declaration


# %% %||% ----
#' The left value, or the right one when the left is `NULL`
#'
#' Base R defines it from R 4.4.0; R8 supports R 4.3.
#'
#' @param x,y Values.
#'
#' @return `x`, or `y` when `x` is `NULL`.
#'
#' @keywords internal
#' @noRd
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
} # /R8::%||%
