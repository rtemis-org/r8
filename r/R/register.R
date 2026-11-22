# register.R
# ::R8::
# 2026- EDG rtemis.org

# The methods each package implements in compiled code, by package: a data
# frame of `trait_name`, `type_name`, and `method`, which the roclet `r8_rd`
# documents.
.r8_compiled_methods <- new.env(parent = emptyenv())

# By package, the external pointers to compiled functions that the closures
# of its compiled methods hold (`compiled_method()`), cleared when the
# package registers again or unloads (`clear_compiled()`).
.r8_compiled_ptrs <- new.env(parent = emptyenv())

# The packages whose `onUnload` hook clears their pointers.
.r8_unload_hooks <- new.env(parent = emptyenv())


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
#' A package with compiled exports (`R8.h`) registers them after its R
#' declarations. Every type of the exports has an R declaration with the same
#' declaration hash, the type of every codec is declared with
#' `native = TRUE`, and every type declared with `native = TRUE` has a codec;
#' otherwise registration signals `r8_declaration_error` and lists each
#' difference. `r8_document_native()` generates the R declarations of types
#' declared in Rust.
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
  for (ns_name in unique(vapply(entries, function(e) e[["namespace"]], ""))) {
    claim_namespace(ns_name, pkg)
  }
  # The records were checked when the package was installed; they register
  # without the per-type check, and check_namespaces() checks every namespace
  # the package owns whole. A failure restores the registry.
  r8_registry_save()
  done <- FALSE
  on.exit(if (done) r8_registry_release() else r8_registry_restore())
  # Consecutive type records register in one call, before the next entry of
  # another kind, so the declarations replay in their order.
  pending <- list()
  for (e in entries) {
    if (identical(e[["kind"]], "type")) {
      pending[[length(pending) + 1L]] <- e[["record"]]
      next
    }
    register_records(pending, pkg, check = FALSE)
    pending <- list()
    switch(
      e[["kind"]],
      trait = register_trait(e[["trait"]], e[["namespace"]]),
      impl = register_impl(e),
      s3method = register_s3_method(e)
    )
  }
  register_records(pending, pkg, check = FALSE)
  n_compiled <- register_exports(pkg, entries)
  clear_variants()
  check_namespaces(pkg)
  done <- TRUE
  invisible(length(entries) + n_compiled)
} # /R8::r8_register


# %% check_namespaces ----
#' Check the namespaces a package owns, whole
#'
#' Each declaration is checked alone when it is registered; the check of a
#' whole namespace, once per load, reports what those checks cannot see,
#' such as a rule that reads a field of a type declared again without that
#' field.
#'
#' @param pkg Character: The package.
#'
#' @return `NULL`, invisibly.
#'
#' @keywords internal
#' @noRd
check_namespaces <- function(pkg) {
  p <- r8_check_namespaces(r8_package_namespaces(pkg))
  if (length(p[["code"]]) > 0L) {
    r8_abort(
      "The declarations of package '",
      pkg,
      "' do not agree:\n",
      paste0(
        "- ",
        p[["message"]],
        " [",
        p[["code"]],
        " at ",
        p[["namespace"]],
        ":",
        p[["path"]],
        "]",
        collapse = "\n"
      ),
      class = "r8_declaration_error"
    )
  }
  invisible(NULL)
} # /R8::check_namespaces


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
#' @param entries List: The package's recorded declarations.
#'
#' @return The number of impl functions registered.
#'
#' @keywords internal
#' @noRd
register_exports <- function(pkg, entries = list()) {
  native <- unlist(lapply(entries, function(e) {
    if (identical(e[["kind"]], "type") && isTRUE(e[["native"]])) {
      e[["name"]]
    }
  }))
  # R_GetCCallable() raises an error for a package that registered no
  # exports.
  ptr <- tryCatch(
    .Call(.r8_native[["exports"]], pkg),
    error = function(e) NULL
  )
  if (is.null(ptr)) {
    check_exports(pkg, NULL, native)
    return(0L)
  }
  info <- r8_exports_info(ptr)
  claim_namespace(info[["namespace"]], pkg)
  compared <- r8_exports_compare(ptr)
  check_exports(pkg, compared, native)
  fns <- r8_register_exports(ptr, pkg)
  assign(
    pkg,
    data.frame(
      trait_name = as.character(fns[["trait_name"]]),
      type_name = as.character(fns[["type_name"]]),
      method = as.character(fns[["method"]])
    ),
    envir = .r8_compiled_methods
  )
  for (n in ls(.r8_codecs)) {
    if (identical(.r8_codecs[[n]], pkg)) {
      rm(list = n, envir = .r8_codecs)
    }
  }
  for (n in compared[["type_name"]][compared[["codec"]]]) {
    assign(n, pkg, envir = .r8_codecs)
  }
  clear_compiled(pkg)
  if (is.null(.r8_unload_hooks[[pkg]])) {
    setHook(
      packageEvent(pkg, "onUnload"),
      function(...) clear_compiled(pkg)
    )
    assign(pkg, TRUE, envir = .r8_unload_hooks)
  }
  for (i in seq_along(fns[["method"]])) {
    compiled_method(
      fns[["trait_name"]][[i]],
      fns[["type_name"]][[i]],
      fns[["method"]][[i]],
      pkg
    )
  }
  length(fns[["method"]])
} # /R8::register_exports


# %% clear_compiled ----
#' Clear the pointers to a package's compiled functions
#'
#' A closure that still holds one, after the package's library unloads or
#' the package registers again, raises an error when called.
#'
#' @param pkg Character: The package.
#'
#' @return `NULL`, invisibly.
#'
#' @keywords internal
#' @noRd
clear_compiled <- function(pkg) {
  clear_dispatch()
  ptrs <- .r8_compiled_ptrs[[pkg]]
  if (!is.null(ptrs)) {
    r8_clear_external_ptrs(ptrs)
    rm(list = pkg, envir = .r8_compiled_ptrs)
  }
  invisible(NULL)
} # /R8::clear_compiled


# %% check_exports ----
#' Check a package's compiled exports against its R declarations
#'
#' @param pkg Character: The package.
#' @param compared Optional list: `r8_exports_compare()` of the package's
#'   exports, or `NULL` for a package without exports.
#' @param native Character: The qualified names the package declares with
#'   `native = TRUE`.
#'
#' @return `NULL`, invisibly; signals `r8_declaration_error` listing each
#'   difference.
#'
#' @keywords internal
#' @noRd
check_exports <- function(pkg, compared, native) {
  names <- compared[["type_name"]]
  status <- compared[["status"]]
  codec <- compared[["codec"]]
  problems <- c(
    sprintf(
      "`%s` is declared in the compiled exports and has no R declaration.",
      names[status == "missing"]
    ),
    sprintf(
      "The R declaration of `%s` differs from its declaration in the compiled exports.",
      names[status == "differs"]
    ),
    sprintf(
      "`%s` has a codec in the compiled exports; its R declaration takes native = TRUE.",
      names[codec & status != "missing" & !names %in% native]
    ),
    sprintf(
      "`%s` is declared with native = TRUE; the compiled exports carry no codec for it.",
      setdiff(native, names[codec])
    )
  )
  if (length(problems) > 0L) {
    regenerate <- any(status %in% c("missing", "differs"))
    r8_abort(
      "The R8 declarations of package '",
      pkg,
      "' do not match its compiled exports:\n",
      paste0("- ", problems, collapse = "\n"),
      if (regenerate) {
        paste0(
          "\nR8::r8_document_native() regenerates R/r8-types.R from the ",
          "declarations in Rust."
        )
      },
      class = "r8_declaration_error"
    )
  }
  invisible(NULL)
} # /R8::check_exports


# %% compiled_method ----
#' The R method of an impl function
#'
#' A function with the formals of the trait's method that passes its
#' arguments, in the declared order, to the impl function; it is registered
#' as the trait's method for the type, as `impl()` registers an R method.
#'
#' @param trait_name,type_name,method Character: The impl.
#' @param pkg Character: The package whose exports hold the function.
#'
#' @return `NULL`, invisibly.
#'
#' @keywords internal
#' @noRd
compiled_method <- function(trait_name, type_name, method, pkg) {
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
    trait_ir(t)[["methods"]]
  )
  params <- unlist(decl[[1L]][["params"]])
  args <- lapply(params, function(p) {
    if (identical(p, "...")) quote(list(...)) else as.name(p)
  })
  formals <- stats::setNames(
    rep(list(quote(expr = )), length(params)),
    params
  )
  # The function is resolved once, here; its pointer, which the body holds,
  # is cleared when the package registers again or unloads.
  ptr <- r8_impl_fn(trait_name, type_name, method)
  .r8_compiled_ptrs[[pkg]] <- c(.r8_compiled_ptrs[[pkg]], list(ptr))
  fn <- as.function(
    c(
      formals,
      call(
        ".Call",
        quote(.r8_c_invoke),
        ptr,
        as.call(c(as.name("list"), args))
      )
    ),
    envir = asNamespace("R8")
  )
  generic <- t[["generics"]][[method]]
  set_impl(trait_name, method, type_name, fn)
  if (!inherits(generic, "r8_generic")) {
    if (identical(trait_name, "R8::Print")) {
      fn <- print_method(fn)
    } else if (identical(trait_name, "R8::Display")) {
      fn <- display_method(fn)
    }
    registerS3method(method, type_name, fn, envir = s3_envir(generic))
  }
  invisible(NULL)
} # /R8::compiled_method
