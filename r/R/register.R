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
      impl = register_impl(e),
      s3method = register_s3_method(e)
    )
  }
  n_compiled <- register_exports(pkg)
  invisible(length(entries) + n_compiled)
} # /R8::r8_register


# %% register_exports ----
#' Register a package's compiled exports
#'
#' Fetches the function the package registered with
#' `R_RegisterCCallable(pkg, "r8_exports", fn)`, when it registered one, and
#' registers the `R8Exports` table it returns (`R8.h`): the IR document, the
#' codecs of Rust-native types, and the impl functions, each of which becomes
#' the R method of its trait for its type.
#'
#' @param pkg Character: The package.
#'
#' @return The number of impl functions registered.
#'
#' @keywords internal
#' @noRd
register_exports <- function(pkg) {
  # R_GetCCallable() raises an error for a package that registered no
  # exports.
  ptr <- tryCatch(
    .Call(.r8_native[["exports"]], pkg),
    error = function(e) NULL
  )
  if (is.null(ptr)) {
    return(0L)
  }
  info <- r8_exports_info(ptr)
  claim_namespace(info[["namespace"]], pkg)
  fns <- r8_register_exports(ptr, pkg)
  for (i in seq_along(fns[["method"]])) {
    compiled_method(
      fns[["trait_name"]][[i]],
      fns[["type_name"]][[i]],
      fns[["method"]][[i]]
    )
  }
  length(fns[["method"]])
} # /R8::register_exports


# %% compiled_method ----
#' The R method of an impl function
#'
#' A function with the formals of the trait's method that passes its
#' arguments, in the declared order, to the impl function; it is registered
#' as the trait's method for the type, as `impl()` registers an R method.
#'
#' @param trait_name,type_name,method Character: The impl.
#'
#' @return `NULL`, invisibly.
#'
#' @keywords internal
#' @noRd
compiled_method <- function(trait_name, type_name, method) {
  trait <- .r8_traits[[trait_name]]
  if (is.null(trait)) {
    r8_abort(
      "The trait `",
      trait_name,
      "` of a compiled impl is not registered.",
      class = "r8_declaration_error"
    )
  }
  t <- unclass(trait)
  decl <- Filter(
    function(m) identical(m[["name"]], method),
    t[["ir"]][["methods"]]
  )
  params <- unlist(decl[[1L]][["params"]])
  args <- lapply(params, function(p) {
    if (identical(p, "...")) quote(list(...)) else as.name(p)
  })
  formals <- stats::setNames(
    rep(list(quote(expr = )), length(params)),
    params
  )
  fn <- as.function(
    c(
      formals,
      bquote(call_impl(
        .(trait_name),
        .(type_name),
        .(method),
        .(as.call(c(as.name("list"), args)))
      ))
    ),
    envir = asNamespace("R8")
  )
  generic <- t[["generics"]][[method]]
  assign(
    paste(trait_name, method, type_name, sep = "|"),
    fn,
    envir = .r8_impls
  )
  if (!inherits(generic, "r8_generic")) {
    if (identical(trait_name, "R8::Print")) {
      fn <- print_method(fn)
    }
    registerS3method(method, type_name, fn, envir = s3_envir(generic))
  }
  invisible(NULL)
} # /R8::compiled_method


# %% call_impl ----
#' Call a compiled impl function
#'
#' Passes the arguments to the impl function of `method` of the trait for the
#' type, through R8's C routine `invoke`, which raises the impl's error and
#' continues a jump that `R8Api.protect` caught.
#'
#' @param trait_name,type_name,method Character: The impl.
#' @param args List: The arguments, in the declared order.
#'
#' @return The impl function's result.
#'
#' @keywords internal
#' @noRd
call_impl <- function(trait_name, type_name, method, args) {
  .Call(
    .r8_native[["invoke"]],
    r8_impl_fn(trait_name, type_name, method),
    args
  )
} # /R8::call_impl
