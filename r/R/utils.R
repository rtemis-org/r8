# utils.R
# ::R8::
# 2026- EDG rtemis.org

# %% r8_abort ----
#' Raise an R8 error
#'
#' @param ... Character: Message parts, pasted without a separator.
#' @param class Character: Condition classes, before `r8_error`.
#' @param call Optional call to report.
#' @param data List: Further fields of the condition, such as `problems`.
#'
#' @return Does not return.
#'
#' @keywords internal
#' @noRd
r8_abort <- function(..., class = character(), call = NULL, data = list()) {
  cond <- structure(
    class = c(class, "r8_error", "error", "condition"),
    c(list(message = paste0(...), call = call), data)
  )
  stop(cond)
} # /R8::r8_abort


# %% r8_warn ----
#' Raise an R8 warning
#'
#' @param ... Character: Message parts, pasted without a separator.
#' @param class Character: Condition classes, before `r8_warning`.
#'
#' @return `NULL`, invisibly.
#'
#' @keywords internal
#' @noRd
r8_warn <- function(..., class = character()) {
  cond <- structure(
    class = c(class, "r8_warning", "warning", "condition"),
    list(message = paste0(...), call = NULL)
  )
  warning(cond)
  invisible(NULL)
} # /R8::r8_warn


# %% is_name ----
#' An R8 name
#'
#' @param x Object.
#'
#' @return Logical: `x` is one string that the core's name grammar accepts:
#'   a letter, then letters, digits, and underscores, and not a reserved word
#'   of R or Rust (`rtemis_r8::name::Name::new()`).
#'
#' @keywords internal
#' @noRd
is_name <- function(x) {
  !nzchar(r8_name_problem(x))
} # /R8::is_name


# %% name_reason ----
#' Why an object is not an R8 name, for a message
#'
#' @param x Object.
#'
#' @return Character: `" (<reason>)"`, or `""` for a name.
#'
#' @keywords internal
#' @noRd
name_reason <- function(x) {
  r <- r8_name_problem(x)
  if (nzchar(r)) paste0(" (", r, ")") else ""
} # /R8::name_reason


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
    # Read once per loaded namespace and kept in the package's table.
    table <- get0(".__r8__", envir = top, inherits = FALSE)
    if (!is.null(table[["namespace"]])) {
      return(table[["namespace"]])
    }
    pkg <- getNamespaceName(top)
    declared <- utils::packageDescription(pkg, fields = "Config/R8/namespace")
    ns <- if (is.character(declared) && !is.na(declared)) declared else pkg
    if (!is_name(ns)) {
      r8_abort(
        "Package '",
        pkg,
        "' declares R8 types; its namespace `",
        ns,
        "` is not an R8 name",
        name_reason(ns),
        ". Set `Config/R8/namespace` in its DESCRIPTION.",
        class = "r8_declaration_error"
      )
    }
    if (!environmentIsLocked(top)) {
      if (is.null(table)) {
        table <- new.env(parent = emptyenv())
        table[["entries"]] <- list()
        assign(".__r8__", table, envir = top)
      }
      table[["namespace"]] <- ns
    }
    return(ns)
  }
  ns <- getOption("r8.namespace", "global")
  if (!is_name(ns)) {
    r8_abort(
      "The option `r8.namespace` is not an R8 name: `",
      ns,
      "`",
      name_reason(ns),
      ".",
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
    namespace_taken(namespace, package, owner)
  }
  invisible(NULL)
} # /R8::claim_namespace


# %% namespace_taken ----
#' Abort for a namespace that another package owns
#'
#' @param namespace Character: The namespace.
#' @param package Character: The declaring package, or `""`.
#' @param owner Character: The package that owns the namespace.
#'
#' @return Does not return; aborts with `r8_declaration_error`.
#'
#' @keywords internal
#' @noRd
namespace_taken <- function(namespace, package, owner) {
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
} # /R8::namespace_taken


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
#' @return Logical: `TRUE` when the declaration is recorded, invisibly.
#'
#' @keywords internal
#' @noRd
record_declaration <- function(env, entry) {
  top <- topenv(env)
  if (!isNamespace(top) || environmentIsLocked(top)) {
    return(invisible(FALSE))
  }
  if (!exists(".__r8__", envir = top, inherits = FALSE)) {
    assign(".__r8__", new.env(parent = emptyenv()), envir = top)
    top[[".__r8__"]][["entries"]] <- list()
  }
  table <- top[[".__r8__"]]
  table[["entries"]] <- c(table[["entries"]], list(entry))
  invisible(TRUE)
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
