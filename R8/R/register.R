# register.R
# ::R8::
# 2026- EDG rtemis.org

# %% r8_register ----
#' Register a package's R8 declarations
#'
#' A package's top-level `new_struct()`, `new_enum()`, `new_trait()`, and
#' `impl()` calls run when the package is installed, and record themselves in
#' the package's namespace. `r8_register()`, called from the package's
#' `.onLoad()`, registers them in the session: it checks each declaration
#' again and makes its types, traits, and impls available. Registering a
#' package again replaces its declarations.
#'
#' @param pkgname Optional Character: The package; by default, the package
#'   whose code calls `r8_register()`.
#'
#' @return The number of declarations registered, invisibly.
#'
#' @examples
#' \dontrun{
#' .onLoad <- function(libname, pkgname) {
#'   R8::r8_register()
#' }
#' }
#'
#' @export
r8_register <- function(pkgname = NULL) {
  ns <- if (is.null(pkgname)) topenv(parent.frame()) else asNamespace(pkgname)
  if (!isNamespace(ns)) {
    r8_abort(
      "r8_register() is called from a package's .onLoad().",
      class = "r8_declaration_error"
    )
  }
  pkg <- getNamespaceName(ns)
  table <- if (exists(".__r8__", envir = ns, inherits = FALSE)) {
    ns[[".__r8__"]]
  } else {
    NULL
  }
  entries <- if (is.null(table)) list() else table[["entries"]]
  for (e in entries) {
    claim_namespace(e[["namespace"]], pkg)
    switch(
      e[["kind"]],
      type = register_type(e[["type"]], e[["namespace"]]),
      trait = register_trait(e[["trait"]], e[["namespace"]]),
      impl = register_impl(e)
    )
  }
  invisible(length(entries))
} # /R8::r8_register
