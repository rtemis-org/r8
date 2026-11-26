# register.R
# ::R8::
# 2026- EDG rtemis.org

# The methods each package implements in compiled code, by package: a data
# frame of `trait_name`, `tuple` (the key of the positions), and `method`,
# which the roclet `r8_rd` documents.
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
#' A package with compiled exports, the types and the methods of its Rust code,
#' registers them after its R declarations. Every type of the exports has an R declaration with the same
#' declaration hash, the type of every codec is declared with
#' `storage = "rust"`, and every type declared with `storage = "rust"` has a codec;
#' otherwise registration signals `r8_declaration_error` and lists each
#' difference. `r8_document_rust()` generates the R declarations of types
#' declared in Rust. Each impl declaration of the exports is built from its
#' impl entries, grouped by trait and positions.
#'
#' @section Types that other packages use:
#' The compiled code of a package holds the declarations of the types of other
#' packages that it was compiled against. It reads the fields of a value of such
#' a type, by name, when every value that the registered declaration admits is
#' one that the compiled declaration admits: fields may be added or reordered,
#' bounds tightened, and rules added. A field removed or renamed, another type
#' or unit, a bound loosened, a rule removed, and another `version` are a
#' warning of class `r8_declaration_warning` when the package loads, and an error
#' when its code reads a value. A struct that extends a struct of another
#' package is refused at load when the parent's fields or rules differ.
#'
#' A package does not change a type that other packages use without
#' coordinating the change with them: see the section "Changing a type that
#' other packages use" of [new_struct()].
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
  # The records were checked when the package was installed; they register
  # without the per-type check, and check_namespaces() checks every namespace
  # the package owns whole. A failure restores the registry, the namespace
  # claims included.
  token <- r8_registry_save()
  done <- FALSE
  on.exit(if (done) r8_registry_release(token) else r8_registry_restore(token))
  for (ns_name in unique(vapply(entries, function(e) e[["namespace"]], ""))) {
    claim_namespace(ns_name, pkg)
  }
  # Consecutive type records register in one call, before the next entry of
  # another kind, so the declarations replay in their order.
  pending <- list()
  for (e in entries) {
    if (e[["kind"]] %in% c("type", "function")) {
      pending[[length(pending) + 1L]] <- e[["record"]]
      next
    }
    if (identical(e[["kind"]], "trait")) {
      # A file after the generic's may define a function that masks one.
      generics <- unclass(e[["trait"]])[["generics"]]
      for (m in names(generics)) {
        if (inherits(generics[[m]], "r8_generic")) {
          check_base_names(environment(generics[[m]]), m)
        }
      }
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
  exports <- register_exports(pkg, entries)
  clear_variants()
  check_namespaces(pkg)
  done <- TRUE
  stale_elsewhere(pkg)
  warn_changed(exports[["changed"]], pkg)
  invisible(length(entries) + exports[["n"]])
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
  # The namespaces of the packages `pkg` imports, against whose impls an
  # ambiguity is the package's.
  imported <- if (isNamespaceLoaded(pkg)) names(getNamespaceImports(pkg))
  imports <- unique(unlist(lapply(
    setdiff(imported, c("base", pkg)),
    r8_package_namespaces
  )))
  p <- r8_check_namespaces(r8_package_namespaces(pkg), as.character(imports))
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


# %% stale_elsewhere ----
#' Warn of the structs of other packages that a package's load leaves stale
#'
#' A struct of another package that extends a type of `pkg` and disagrees
#' with its new declaration stays stale until its own package is loaded
#' again.
#'
#' @param pkg Character: The package.
#'
#' @return `NULL`, invisibly.
#'
#' @keywords internal
#' @noRd
stale_elsewhere <- function(pkg) {
  own <- r8_package_namespaces(pkg)
  if (length(own) == 0L) {
    return(invisible(NULL))
  }
  stale <- r8_stale_types(own)
  if (length(stale) > 0L) {
    owners <- vapply(
      sub("::.*$", "", stale),
      function(n) {
        o <- r8_namespace_owner(n)
        if (nzchar(o)) paste0("package '", o, "'") else "the console"
      },
      ""
    )
    r8_warn(
      "Package '",
      pkg,
      "' leaves ",
      paste0("`", stale, "` (", owners, ")", collapse = ", "),
      " stale: ",
      if (length(stale) == 1L) "it extends" else "each extends",
      " an earlier declaration of a type of '",
      pkg,
      "'. Load or reinstall the package that declares ",
      if (length(stale) == 1L) "it" else "each",
      " again.",
      class = "r8_stale_warning"
    )
  }
  invisible(NULL)
} # /R8::stale_elsewhere


# %% register_exports ----
#' Register a package's compiled exports
#'
#' Fetches the function the package registered with
#' `R_RegisterCCallable(pkg, "r8_exports", fn)`, when it registered one, and
#' registers the `R8Exports` table it returns (`R8.h`): the codecs of
#' Rust-stored types, checked against the package's R declarations, and the
#' impl functions, whose entries make the impl declarations and each of which
#' becomes the R method of its trait for its type.
#'
#' @param pkg Character: The package.
#' @param entries List: The package's recorded declarations.
#'
#' @return The number of impl functions registered.
#'
#' @keywords internal
#' @noRd
register_exports <- function(pkg, entries = list()) {
  rust_stored <- unlist(lapply(entries, function(e) {
    if (identical(e[["kind"]], "type") && isTRUE(e[["rust_stored"]])) {
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
    check_exports(pkg, NULL, rust_stored)
    return(list(n = 0L))
  }
  namespace <- declaring_namespace(asNamespace(pkg))
  claim_namespace(namespace, pkg)
  compared <- r8_exports_compare(ptr, namespace)
  check_exports(pkg, compared, rust_stored)
  for (name in r8_exports_traits(ptr)) {
    check_trait_registered(name, pkg)
  }
  check_signatures(r8_exports_signatures(ptr, pkg), pkg)
  fns <- r8_register_exports(ptr, pkg, namespace)
  assign(
    pkg,
    data.frame(
      trait_name = as.character(fns[["trait_name"]]),
      tuple = as.character(fns[["tuple"]]),
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
      fns[["tuple"]][[i]],
      fns[["method"]][[i]],
      pkg
    )
  }
  list(n = length(fns[["method"]]), changed = fns[["changed"]])
} # /R8::register_exports


# %% warn_changed ----
#' Warn of types that a package's compiled code may not read
#'
#' The compiled code of a package reads the fields of a value of another
#' package's type when the registered declaration of the type refines the one
#' that the code was compiled against. A read that R8 refuses is an error; the
#' warning names what differs when the package loads.
#'
#' @param changed List: `type_name` and `message` of each difference.
#' @param pkg Character: The package.
#'
#' @return `NULL`, invisibly.
#'
#' @keywords internal
#' @noRd
warn_changed <- function(changed, pkg) {
  types <- unique(changed[["type_name"]])
  if (length(types) == 0L) {
    return(invisible(NULL))
  }
  owners <- vapply(sub("::.*$", "", types), r8_namespace_owner, character(1L))
  versioned <- function(p) {
    paste0("'", p, "' ", as.character(getNamespaceVersion(p)))
  }
  lines <- vapply(
    seq_along(types),
    function(i) {
      of <- changed[["message"]][changed[["type_name"]] == types[[i]]]
      paste0(
        "`",
        types[[i]],
        "` of ",
        if (nzchar(owners[[i]])) versioned(owners[[i]]) else "another package",
        "\n",
        paste0("- ", of, collapse = "\n")
      )
    },
    character(1L)
  )
  r8_warn(
    "Package ",
    versioned(pkg),
    " was compiled against another declaration of ",
    paste(lines, collapse = "\n"),
    "\nIts compiled methods that read ",
    if (length(types) == 1L) "a value of it" else "a value of these types",
    " raise an error. Install versions of '",
    pkg,
    "' and of ",
    paste0("'", unique(owners[nzchar(owners)]), "'", collapse = " and "),
    " that were built for each other.",
    class = "r8_declaration_warning"
  )
} # /R8::warn_changed


# %% check_signatures ----
#' Check the signatures of a package's compiled impls
#'
#' @param problems List: `r8_exports_signatures()` of the package's exports.
#' @param pkg Character: The package.
#'
#' @return `NULL`, invisibly; signals `r8_declaration_error` listing each
#'   difference, with the R declaration that a method's signature states.
#'
#' @keywords internal
#' @noRd
check_signatures <- function(problems, pkg) {
  if (length(problems) == 0L) {
    return(invisible(NULL))
  }
  .r8_render_refs[["bare"]] <- TRUE
  on.exit(.r8_render_refs[["bare"]] <- NULL)
  lines <- vapply(
    problems,
    function(p) {
      paste0(
        "- `",
        p[["member"]],
        "` of `",
        p[["of"]],
        "`",
        if (nzchar(p[["target"]])) paste0(" for ", p[["target"]]),
        ": ",
        paste(p[["messages"]], collapse = "; "),
        ".",
        if (!is.null(p[["signature"]])) {
          sig <- p[["signature"]]
          # The result of a method is of one impl; the declaration is of all.
          sig[["return_type"]] <- NULL
          paste0(
            " Its parameters are declared in R as ",
            gsub("R8::", "", render_params(sig[["params"]], ""), fixed = TRUE),
            "."
          )
        }
      )
    },
    character(1)
  )
  r8_abort(
    "The compiled impls of package '",
    pkg,
    "' do not have the signatures that their traits declare:\n",
    paste(lines, collapse = "\n"),
    "\nThe Rust method or the declaration of its trait changes [impl_signature].",
    class = "r8_declaration_error"
  )
} # /R8::check_signatures


# %% check_trait_registered ----
#' Check that a trait that a package implements is registered
#'
#' A package that names another package's trait as `pkg::Trait` and imports
#' nothing from that package evaluates the name when it is installed, and
#' loads without the other package.
#'
#' @param name Character: The trait's qualified name.
#' @param pkg Character: The package that implements it.
#'
#' @return `NULL`, invisibly; signals `r8_declaration_error` for a trait that
#'   is not registered, with the import that loads the package that declares
#'   it.
#'
#' @keywords internal
#' @noRd
check_trait_registered <- function(name, pkg) {
  if (!is.null(.r8_traits[[name]])) {
    return(invisible(NULL))
  }
  namespace <- sub("::.*$", "", name)
  owner <- r8_namespace_owner(namespace)
  provider <- r8_namespace_providers()[[namespace]]
  r8_abort(
    if (nzchar(pkg)) {
      paste0("Package '", pkg, "' implements `")
    } else {
      "An impl of `"
    },
    name,
    "`, which is not registered: ",
    if (nzchar(owner)) {
      paste0(
        "package '",
        owner,
        "' ",
        utils::packageVersion(owner),
        " is loaded and declares no such trait."
      )
    } else if (length(provider) > 0L) {
      paste0(
        "package '",
        provider[[1L]],
        "' declares the namespace `",
        namespace,
        "` and is not loaded. Import from it in NAMESPACE, so that it loads first: ",
        import_directive(provider[[1L]], sub("^.*::", "", name)),
        "."
      )
    } else {
      paste0("no installed package declares the namespace `", namespace, "`.")
    },
    class = "r8_declaration_error"
  )
} # /R8::check_trait_registered


# %% import_directive ----
#' The NAMESPACE directive that imports from a package
#'
#' @param pkg Character: An installed package, which is not loaded.
#' @param name Character: The name to import.
#'
#' @return Character: `importFrom()` of `name` when the package's NAMESPACE
#'   exports it by name, and `import()` of the package otherwise.
#'
#' @keywords internal
#' @noRd
import_directive <- function(pkg, name) {
  exports <- tryCatch(
    parseNamespaceFile(pkg, dirname(system.file(package = pkg)))[["exports"]],
    error = function(e) character()
  )
  if (name %in% exports) {
    paste0("importFrom(", pkg, ", ", name, ")")
  } else {
    paste0("import(", pkg, ")")
  }
} # /R8::import_directive


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
#' @param rust_stored Character: The qualified names the package declares with
#'   `storage = "rust"`.
#'
#' @return `NULL`, invisibly; signals `r8_declaration_error` listing each
#'   difference.
#'
#' @keywords internal
#' @noRd
check_exports <- function(pkg, compared, rust_stored) {
  names <- compared[["type_name"]]
  status <- compared[["status"]]
  codec <- compared[["codec"]]
  declared <- status != "missing"
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
      "`%s` has a codec in the compiled exports; its R declaration takes storage = \"rust\".",
      names[codec & declared & !names %in% rust_stored]
    ),
    sprintf(
      "`%s` is declared with storage = \"rust\"; the compiled exports carry no codec for it.",
      setdiff(rust_stored, names[codec])
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
          "\nR8::r8_document_rust() regenerates R/r8-types.R from the ",
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
#' as the trait's method for the positions, as `impl()` registers an R method,
#' and for `R8::From` as the conversion of its source to its type.
#'
#' @param trait_name,tuple,method Character: The impl: its trait, the key of
#'   its positions (`tuple_key()`), and the method.
#' @param pkg Character: The package whose exports hold the function.
#'
#' @return `NULL`, invisibly.
#'
#' @keywords internal
#' @noRd
compiled_method <- function(trait_name, tuple, method, pkg) {
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
  params <- param_names(decl[[1L]][["params"]])
  args <- lapply(params, function(p) {
    if (identical(p, "...")) quote(list(...)) else as.name(p)
  })
  formals <- stats::setNames(
    rep(list(quote(expr = )), length(params)),
    params
  )
  # The function is looked up once, here; its pointer, which the body holds,
  # is cleared when the package registers again or unloads.
  ptr <- r8_impl_fn(trait_name, tuple, method)
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
  if (identical(trait_name, "R8::From")) {
    # The type, then the source.
    put_impl("R8::From", sub("^[^,]*,", "", tuple), sub(",.*$", "", tuple), fn)
    return(invisible(NULL))
  }
  register_method(t, method, tuple, fn)
} # /R8::compiled_method
