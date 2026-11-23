# traits.R
# ::R8::
# 2026- EDG rtemis.org

# Impls of R8 traits written in R, by "trait|method|type". A generic made by
# a trait looks up the impl for the qualified type of its first argument.
.r8_impls <- new.env(parent = emptyenv())

# The dispatch cache: by `"Trait|method"`, an environment of the resolved
# impl of the method by qualified type, or by `class:` and the class of an
# object of another class; `r8_find_impl()` fills it from `.r8_impls` on a
# miss of the C routine `find`. A generic reads it in R8's namespace, so a
# generic sent to a worker resolves the worker's impls. `put_impl()` drops a
# method's index when one of its impls changes; unloading a package and
# declaring a type clear the cache.
.r8_dispatch <- new.env(parent = emptyenv())

# The types with an impl of a trait, by `"Trait|type"`: the unit of
# precedence through `extends`, so that the nearest type with an impl
# provides every method, and a method its impl omits takes the trait's
# default.
.r8_impl_of <- new.env(parent = emptyenv())

# The ancestors of each type through `extends`, by qualified type, as
# `r8_type_ancestors()` returns them; cleared with the dispatch cache and
# when records register.
.r8_ancestry <- new.env(parent = emptyenv())

# The native symbol of the C routine `find` (`r/src/view.c`), bound when the
# package loads.
.r8_c_find <- NULL

# The native symbol of the C routine `replaced` (`r/src/view.c`), which a
# replacement generic calls on its impl's result, bound when the package
# loads.
.r8_c_replaced <- NULL

# The native symbols of the C routines `find2` and `find3`, which find the
# impl for two and three dispatched arguments, bound when the package loads.
.r8_c_find2 <- NULL
.r8_c_find3 <- NULL

# Trait objects by qualified name, for the impls of compiled code.
.r8_traits <- new.env(parent = emptyenv())

# Packages whose generics a trait method of the same name becomes a method
# of, beside the imports of the declaring package.
BASE_PACKAGES <- c("base", "stats", "utils", "methods", "graphics", "grDevices")


# %% trait_method ----
#' Declare a trait method
#'
#' @param fn Function: Its formals are the method's signature; the first
#'   formal is the value the method dispatches on.
#' @param description Character: What the method does.
#' @param default Optional function: A default implementation, used for a
#'   type whose impl does not define the method.
#' @param stochastic Logical: The method draws random numbers.
#' @param dispatch Optional Character: The arguments the method dispatches
#'   on, by name, at most three and none of them `...`; the first argument
#'   by default. Every method of a trait dispatches on the same number of
#'   arguments, and an impl names a type for any of them (`impl()`).
#'
#' @return An `r8_trait_method`.
#'
#' @examples
#' trait_method(function(spec, x, y) NULL, description = "Trains a model.")
#' trait_method(function(x, y, ...) NULL, dispatch = c("x", "y"))
#'
#' @export
trait_method <- function(
  fn,
  description = "",
  default = NULL,
  stochastic = FALSE,
  dispatch = NULL
) {
  if (!is.function(fn) || length(formals(fn)) == 0L) {
    r8_abort(
      "A trait method's signature is a function with at least one argument.",
      class = "r8_declaration_error"
    )
  }
  if (!is.null(default)) {
    check_method_formals(default, names(formals(fn)), "the default")
  }
  structure(
    list(
      fn = fn,
      description = description,
      default = default,
      stochastic = isTRUE(stochastic),
      dispatch = dispatch
    ),
    class = "r8_trait_method"
  )
} # /R8::trait_method


# %% existing_generic ----
#' The existing function of a name in the base packages or the imports
#'
#' @param name Character: The method name.
#' @param env Environment: Where the trait is declared.
#'
#' @return A list with `fn` and `package`, or `NULL`.
#'
#' @keywords internal
#' @noRd
existing_generic <- function(name, env) {
  pkgs <- BASE_PACKAGES
  top <- topenv(env)
  if (isNamespace(top)) {
    imports <- utils::packageDescription(
      getNamespaceName(top),
      fields = "Imports"
    )
    if (is.character(imports) && !is.na(imports)) {
      pkgs <- c(pkgs, trimws(sub("\\(.*", "", strsplit(imports, ",")[[1L]])))
    }
  }
  for (p in pkgs) {
    if (
      requireNamespace(p, quietly = TRUE) && name %in% getNamespaceExports(p)
    ) {
      return(list(fn = getExportedValue(p, name), package = p))
    }
  }
  NULL
} # /R8::existing_generic


# %% is_s3_generic ----
#' A function that dispatches S3 methods
#'
#' @param f Function.
#'
#' @return Logical.
#'
#' @keywords internal
#' @noRd
is_s3_generic <- function(f) {
  !is.null(s3_generic_info(f)) || is_s4_generic(f)
} # /R8::is_s3_generic


# %% s3_envir ----
#' The environment in which an S3 generic's name is found
#'
#' @param f Function: An S3 generic.
#'
#' @return Environment: The namespace of the generic's package.
#'
#' @keywords internal
#' @noRd
s3_envir <- function(f) {
  info <- s3_generic_info(f)
  if (!is.null(info) && nzchar(info[["package"]])) {
    return(asNamespace(info[["package"]]))
  }
  env <- environment(f)
  if (is.null(env)) baseenv() else env
} # /R8::s3_envir


# Methods reserved for R8 and its standard traits, with the declaration that
# defines each for a type.
RESERVED_METHODS <- c(
  print = "impl(Print, Type, ...) or implement(print, Type)",
  format = "impl(Display, Type, ...) or implement(format, Type)",
  str = "impl(Debug, Type, ...) or implement(str, Type)",
  `==` = "",
  `$` = "",
  `[[` = "",
  `$<-` = "",
  `[[<-` = "",
  `[<-` = "",
  `names<-` = "",
  `length<-` = "",
  `dim<-` = "",
  `levels<-` = "",
  `@` = "",
  as.list = ""
)


# %% make_generic ----
#' A new generic for a trait method
#'
#' The generic finds the impl for its first argument and calls it with its
#' formals. A replacement generic, whose method name ends in `<-`, passes the
#' impl's result to the C routine `replaced`, which returns it when it is a
#' value of the first argument's type.
#'
#' @param trait_name Character: The trait's qualified name.
#' @param method Character: The method name.
#' @param fn Function: The signature.
#' @param dispatch Optional Character: The dispatched arguments, in formal
#'   order; `NULL` for the first.
#'
#' @return The generic, with its dispatched arguments as the attribute
#'   `r8_dispatch`.
#'
#' @keywords internal
#' @noRd
make_generic <- function(trait_name, method, fn, dispatch = NULL) {
  args <- names(formals(fn))
  dispatch <- dispatch %||% args[[1L]]
  first <- as.name(args[[1L]])
  call_args <- lapply(args, function(a) {
    if (a == "...") quote(...) else as.name(a)
  })
  names(call_args) <- ifelse(args == "...", "", args)
  key <- as.name(paste(trait_name, method, sep = "|"))
  # Constants only: the method's key as a symbol and namespace bindings, so
  # a generic sent to a worker dispatches through the worker's impls.
  d <- lapply(dispatch, as.name)
  find <- switch(
    length(d),
    bquote(.Call(.r8_c_find, .(d[[1L]]), quote(.(key)), .r8_dispatch)),
    bquote(.Call(
      .r8_c_find2,
      .(d[[1L]]),
      .(d[[2L]]),
      quote(.(key)),
      .r8_dispatch
    )),
    bquote(.Call(
      .r8_c_find3,
      .(d[[1L]]),
      .(d[[2L]]),
      .(d[[3L]]),
      quote(.(key)),
      .r8_dispatch
    ))
  )
  body <- as.call(c(list(call("(", find)), call_args))
  if (endsWith(method, "<-")) {
    body <- bquote(.Call(.r8_c_replaced, .(first), .(body), quote(.(key))))
  }
  g <- as.function(c(formals(fn), body), envir = asNamespace("R8"))
  structure(
    g,
    class = c("r8_generic", "function"),
    r8_trait = trait_name,
    r8_method = method,
    r8_dispatch = dispatch
  )
} # /R8::make_generic


# %% replacement_method ----
#' The S3 method of a replacement impl, with the result check
#'
#' Calls the impl and passes its result to the C routine `replaced`, as a
#' replacement generic of R8 does.
#'
#' @param fn Function: The impl, with its `r8_provider` stamp.
#' @param key Character: `"Trait|method"`.
#'
#' @return Function: The S3 method, with `fn`'s formals.
#'
#' @keywords internal
#' @noRd
replacement_method <- function(fn, key) {
  args <- names(formals(fn))
  call_args <- lapply(args, function(a) {
    if (a == "...") quote(...) else as.name(a)
  })
  names(call_args) <- ifelse(args == "...", "", args)
  body <- bquote(.Call(
    .r8_c_replaced,
    .(as.name(args[[1L]])),
    .(as.call(c(list(quote(.r8_impl)), call_args))),
    quote(.(as.name(key)))
  ))
  env <- new.env(parent = asNamespace("R8"))
  assign(".r8_impl", fn, envir = env)
  as.function(c(formals(fn), body), envir = env)
} # /R8::replacement_method


# %% replacement_abort ----
#' Signal that a replacement method returned a value of another type
#'
#' Called by the C routine `replaced`.
#'
#' @param key Character: `"Trait|method"`.
#' @param x The value the method received.
#' @param out The value it returned.
#'
#' @return Does not return.
#'
#' @keywords internal
#' @noRd
replacement_abort <- function(key, x, out) {
  at <- regexpr("|", key, fixed = TRUE)
  trait_name <- substr(key, 1L, at - 1L)
  method <- substr(key, at + 1L, nchar(key))
  r8 <- inherits(x, "r8_value")
  type <- if (r8) attr(x, "r8_type") else class(x)[1L]
  fn <- tryCatch(
    impl_in_chain(trait_name, method, type, r8 = r8),
    error = function(e) NULL
  )
  provider <- attr(fn, "r8_provider")[3L]
  got <- if (inherits(out, "r8_value")) {
    paste0("a value of `", attr(out, "r8_type"), "`")
  } else {
    paste0(
      "an object of class ",
      paste0("\"", class(out), "\"", collapse = ", ")
    )
  }
  t <- .r8_traits[[trait_name]]
  # A generic is named by its method, which differs from its trait's name
  # for a replacement generic.
  what <- if (is.null(t)) {
    # The method of an S3 generic that implement() declares.
    paste0("method of `", trait_name, "`")
  } else if (isTRUE(trait_ir(t)[["generic"]])) {
    paste0("generic `", sub("::.*$", "", trait_name), "::", method, "`")
  } else {
    paste0("method `", method, "` of `", trait_name, "`")
  }
  r8_abort(
    "The replacement ",
    what,
    if (!is.null(provider) && !is.na(provider) && nzchar(provider)) {
      paste0(
        if (startsWith(what, "generic")) {
          ", from the method for `"
        } else {
          ", from the impl for `"
        },
        provider,
        "`,"
      )
    } else if (!is.null(provider) && !is.na(provider)) {
      ", from the trait's default,"
    },
    " returned ",
    got,
    " for a value of `",
    type,
    "`; a replacement method returns a value of its argument's type, ",
    "such as x after `x$field <- value`",
    if (
      r8 &&
        !is.null(provider) &&
        !is.na(provider) &&
        nzchar(provider) &&
        !identical(provider, type)
    ) {
      paste0(", or declare the method for `", type, "`")
    },
    " [replacement_type].",
    class = "r8_type_error"
  )
} # /R8::replacement_abort


# %% dispatch_resolve ----
#' Resolve the impl of a trait method for a call on a miss of the cache
#'
#' Called by the C routines `find`, `find2`, and `find3` when the dispatch
#' cache has no impl for the types of the dispatched arguments;
#' `r8_find_impl()` finds the impl and stores it.
#'
#' @param key Character: `"Trait|method"`.
#' @param ... The values of the dispatched arguments, in order.
#'
#' @return The impl function.
#'
#' @keywords internal
#' @noRd
dispatch_resolve <- function(key, ...) {
  at <- regexpr("|", key, fixed = TRUE)
  r8_find_impl(
    substr(key, 1L, at - 1L),
    substr(key, at + 1L, nchar(key)),
    list(...)
  )
} # /R8::dispatch_resolve


# %% cache_impl ----
#' Store a resolved impl in the dispatch cache
#'
#' Under the dispatch key of each dispatched argument (`dispatch_key()`),
#' one environment per position: the keys the C routines `find`, `find2`,
#' and `find3` read.
#'
#' @param key Character: `"Trait|method"`.
#' @param args List: The values of the dispatched arguments.
#' @param impl Function: The impl.
#'
#' @return `NULL`, invisibly.
#'
#' @keywords internal
#' @noRd
cache_impl <- function(key, args, impl) {
  keys <- lapply(args, dispatch_key)
  if (any(vapply(keys, is.null, logical(1)))) {
    return(invisible(NULL))
  }
  node <- .r8_dispatch[[key]]
  if (is.null(node)) {
    node <- new.env(parent = emptyenv())
    assign(key, node, envir = .r8_dispatch)
  }
  n <- length(keys)
  for (k in keys[-n]) {
    nxt <- node[[k]]
    if (!is.environment(nxt)) {
      nxt <- new.env(parent = emptyenv())
      assign(k, nxt, envir = node)
    }
    node <- nxt
  }
  assign(keys[[n]], impl, envir = node)
  invisible(NULL)
} # /R8::cache_impl


# %% put_impl ----
#' Record an impl and drop its method's cached dispatch
#'
#' @param trait_name Character: The trait's qualified name.
#' @param method Character: The method name.
#' @param type Character: The qualified type name, a class, or `""` for the
#'   trait's default.
#' @param fn Function: The impl.
#'
#' @return `NULL`, invisibly.
#'
#' @keywords internal
#' @noRd
put_impl <- function(trait_name, method, type, fn) {
  # The provider, which r8_next_impl() reads from the running impl.
  attr(fn, "r8_provider") <- c(trait_name, method, type)
  assign(paste(trait_name, method, type, sep = "|"), fn, envir = .r8_impls)
  if (nzchar(type)) {
    mark_impl(trait_name, type)
  }
  key <- paste(trait_name, method, sep = "|")
  if (exists(key, envir = .r8_dispatch, inherits = FALSE)) {
    rm(list = key, envir = .r8_dispatch)
  }
  invisible(NULL)
} # /R8::put_impl


# %% clear_dispatch ----
#' Clear the dispatch cache
#'
#' @return `NULL`, invisibly.
#'
#' @keywords internal
#' @noRd
clear_dispatch <- function() {
  rm(list = ls(.r8_dispatch, all.names = TRUE), envir = .r8_dispatch)
  clear_ancestry()
  invisible(NULL)
} # /R8::clear_dispatch


# %% r8_clear_dispatch ----
#' Clear R8's dispatch cache
#'
#' R8 caches the method each generic resolved for the chain of its
#' arguments' types, keyed by each argument's type or class vector, and
#' clears the cache when an impl is declared. An S4 class's superclasses
#' belong to its class definition, so `methods::setIs()` or
#' `methods::setClassUnion()` on an existing class changes the chain of
#' objects of that class without an impl's declaration; a session that does
#' so after a generic of an impl on the new superclass ran calls
#' `r8_clear_dispatch()`.
#'
#' @return `NULL`, invisibly.
#'
#' @examples
#' r8_clear_dispatch()
#'
#' @export
r8_clear_dispatch <- function() {
  clear_dispatch()
} # /R8::r8_clear_dispatch


# %% clear_ancestry ----
#' Clear the memo of ancestors
#'
#' @return `NULL`, invisibly.
#'
#' @keywords internal
#' @noRd
clear_ancestry <- function() {
  rm(list = ls(.r8_ancestry, all.names = TRUE), envir = .r8_ancestry)
  invisible(NULL)
} # /R8::clear_ancestry


# %% mark_impl ----
#' Record that a type has an impl of a trait
#'
#' @param trait_name Character: The trait's qualified name.
#' @param type Character: The qualified type name or class.
#'
#' @return `NULL`, invisibly.
#'
#' @keywords internal
#' @noRd
mark_impl <- function(trait_name, type) {
  assign(paste(trait_name, type, sep = "|"), TRUE, envir = .r8_impl_of)
  invisible(NULL)
} # /R8::mark_impl


# %% ancestry ----
#' The ancestors of a type, memoized
#'
#' @param type Character: The qualified type name.
#'
#' @return List: `ancestors`, `stale`, and `parent`, as
#'   `r8_type_ancestors()` returns them.
#'
#' @keywords internal
#' @noRd
ancestry <- function(type) {
  a <- .r8_ancestry[[type]]
  if (is.null(a)) {
    # A class of another system has no ancestors through `extends`.
    a <- if (grepl("::", type, fixed = TRUE)) {
      r8_type_ancestors(type)
    } else {
      list(ancestors = character(), stale = FALSE, parent = "")
    }
    assign(type, a, envir = .r8_ancestry)
  }
  a
} # /R8::ancestry


# %% stale_abort ----
#' Signal that a stale type refuses an operation
#'
#' @param type Character: The qualified type name.
#' @param parent Character: Its parent.
#' @param class Character: The condition class, before `r8_error`.
#'
#' @return Does not return.
#'
#' @keywords internal
#' @noRd
stale_abort <- function(type, parent, class) {
  r8_abort(
    "`",
    type,
    "` was declared against an earlier declaration of `",
    parent,
    "`; declare `",
    type,
    "` again, or reload or reinstall the package that declares it [type_stale].",
    class = class
  )
} # /R8::stale_abort


# %% impl_in_chain ----
#' The impl of a trait method for a type, through `extends`
#'
#' The nearest of `type` and its ancestors with an impl of the trait
#' provides the method, or the trait's default when that impl omits it;
#' without such a type, the trait's default applies. With `after`, the search
#' starts at the parent of `after`, for `r8_next_impl()`.
#'
#' @param trait_name Character: The trait's qualified name.
#' @param method Character: The method name.
#' @param type Character: The qualified type name, or a class.
#' @param r8 Logical: `type` is an R8 type, whose ancestors are searched.
#' @param after Optional Character: The type whose impl is running.
#'
#' @return The impl function, or `NULL`.
#'
#' @keywords internal
#' @noRd
impl_in_chain <- function(trait_name, method, type, r8 = TRUE, after = NULL) {
  chain <- type
  own <- is.null(after) &&
    exists(
      paste(trait_name, type, sep = "|"),
      envir = .r8_impl_of,
      inherits = FALSE
    )
  if (r8 && !own) {
    a <- ancestry(type)
    if (isTRUE(a[["stale"]])) {
      stale_abort(type, a[["parent"]], "r8_dispatch_error")
    }
    chain <- c(type, a[["ancestors"]])
    if (!is.null(after)) {
      at <- match(after, chain)
      chain <- if (is.na(at)) character() else chain[-seq_len(at)]
    }
  } else if (!is.null(after)) {
    chain <- character()
  }
  default <- .r8_impls[[paste(trait_name, method, "", sep = "|")]]
  for (t in chain) {
    if (
      exists(
        paste(trait_name, t, sep = "|"),
        envir = .r8_impl_of,
        inherits = FALSE
      )
    ) {
      fn <- .r8_impls[[paste(trait_name, method, t, sep = "|")]] %||% default
      if (!identical(t, type) && is_compiled_impl(fn)) {
        r8_abort(
          "The compiled impl of `",
          trait_name,
          "` for `",
          t,
          "` takes a value of `",
          t,
          "`, not of its descendant `",
          type,
          "`; implement `",
          trait_name,
          "` for `",
          type,
          "`.",
          class = "r8_dispatch_error"
        )
      }
      return(fn)
    }
  }
  # After the running impl, the default follows unless it is running.
  if (!is.null(after) && !nzchar(after)) {
    return(NULL)
  }
  default
} # /R8::impl_in_chain


# %% is_compiled_impl ----
#' Whether an impl calls a compiled method
#'
#' @param fn Function: The impl.
#'
#' @return Logical.
#'
#' @keywords internal
#' @noRd
is_compiled_impl <- function(fn) {
  b <- body(fn)
  is.call(b) &&
    identical(b[[1L]], quote(.Call)) &&
    identical(b[[2L]], quote(.r8_c_invoke))
} # /R8::is_compiled_impl


# %% r8_find_impl ----
#' The impl of a trait method for the values of its dispatched arguments
#'
#' Resolves through the core's rule of dispatch (`resolve_tuple()`), over the
#' chain of each argument: an R8 value's type and its ancestors, or another
#' object's class vector as `.class2()` gives it.
#'
#' @param trait_name Character: The trait's qualified name.
#' @param method Character: The method name.
#' @param args List: The values of the dispatched arguments.
#'
#' @return The impl function.
#'
#' @keywords internal
#' @noRd
r8_find_impl <- function(trait_name, method, args) {
  key <- paste(trait_name, method, sep = "|")
  impl <- resolve_tuple(trait_name, method, args)
  if (!is.null(impl)) {
    cache_impl(key, args, impl)
    return(impl)
  }
  if (length(args) == 1L) {
    r8_abort(
      "`",
      value_type_text(args[[1L]]),
      "` does not implement `",
      trait_name,
      "` (method `",
      method,
      "`).",
      class = "r8_dispatch_error"
    )
  }
  r8_abort(
    "No impl of `",
    trait_name,
    "` (method `",
    method,
    "`) applies to (",
    paste(vapply(args, value_type_text, character(1)), collapse = ", "),
    "), and the trait has no default.",
    class = "r8_dispatch_error"
  )
} # /R8::r8_find_impl


# %% value_type_text ----
#' The type of a value, for messages
#'
#' @param x The value.
#'
#' @return Character: Its R8 type, or its first class.
#'
#' @keywords internal
#' @noRd
value_type_text <- function(x) {
  if (inherits(x, "r8_value")) attr(x, "r8_type") else class(x)[1L]
} # /R8::value_type_text


# %% new_trait ----
#' Declare a trait
#'
#' A trait declares methods that types implement with `impl()`. A method is a
#' generic that dispatches on the type of its first argument, reached as
#' `Trait@method`; a package binds and exports it under its name, such as
#' `fit <- Fit@fit`. A method named as an S3 generic of the base packages or
#' of the package's imports, such as `summary` or `format`, becomes an S3
#' method of that generic, and its signature begins with the generic's
#' arguments.
#'
#' @param name Character: The trait name.
#' @param methods Named list: The methods, `trait_method()` calls.
#' @param description Character: What the trait is.
#' @param tag Optional Character: The member that names an implementor in
#'   JSON.
#'
#' @return The trait object, of class `r8_trait`.
#'
#' @examples
#' op <- options(r8.namespace = "demo")
#' Fit <- new_trait("Fit", methods = list(fit = trait_method(function(spec, x, y) NULL)))
#' fit <- Fit@fit
#' options(op)
#'
#' @export
new_trait <- function(name, methods = list(), description = "", tag = NULL) {
  if (!is_name(name)) {
    r8_abort(
      "A trait name is an R8 name, such as \"Fit\"",
      name_reason(name),
      ".",
      class = "r8_declaration_error"
    )
  }
  reserved <- intersect(names(methods), names(RESERVED_METHODS))
  if (length(reserved) > 0L) {
    how <- RESERVED_METHODS[[reserved[[1L]]]]
    r8_abort(
      "The method name `",
      reserved[[1L]],
      "` is reserved for R8",
      if (nzchar(how)) paste0("; a type defines it with ", how) else "",
      ".",
      class = "r8_declaration_error"
    )
  }
  env <- parent.frame()
  namespace <- declaring_namespace(env)
  claim_namespace(namespace, declaring_package(env))
  held <- .r8_traits[[paste0(namespace, "::", name)]]
  if (!is.null(held) && isTRUE(trait_ir(held)[["generic"]])) {
    held_method <- trait_ir(held)[["methods"]][[1L]][["name"]]
    if (endsWith(held_method, "<-")) {
      r8_abort(
        "`",
        namespace,
        "::",
        name,
        "` is the trait of the replacement generic `",
        held_method,
        "`; name the trait otherwise.",
        class = "r8_declaration_error"
      )
    }
  }
  trait <- with_record(
    build_trait(name, methods, description, tag, namespace, env),
    namespace
  )
  record_declaration(
    env,
    list(kind = "trait", trait = trait, namespace = namespace)
  )
  trait
} # /R8::new_trait


# %% build_trait ----
#' A trait object, without registering it
#'
#' @param name Character: The trait name.
#' @param methods Named list: `trait_method()` calls.
#' @param description Character: What the trait is.
#' @param tag Optional Character: The tag member.
#' @param namespace Character: The trait's namespace.
#' @param env Environment: Where the trait is declared, for existing
#'   generics.
#'
#' @return The trait object.
#'
#' @keywords internal
#' @noRd
build_trait <- function(name, methods, description, tag, namespace, env) {
  qname <- paste0(namespace, "::", name)
  check_members(methods, "methods", paste0("`", qname, "`"))
  if (
    length(methods) == 0L ||
      is.null(names(methods)) ||
      any(!nzchar(names(methods)))
  ) {
    r8_abort(
      "A trait has named methods: name = trait_method(...).",
      class = "r8_declaration_error"
    )
  }
  generics <- list()
  methods_ir <- list()
  for (m in names(methods)) {
    tm <- methods[[m]]
    if (!inherits(tm, "r8_trait_method")) {
      r8_abort(
        "Method `",
        m,
        "` is a trait_method() call.",
        class = "r8_declaration_error"
      )
    }
    existing <- existing_generic(m, env)
    if (!is.null(existing)) {
      if (!is_s3_generic(existing[["fn"]])) {
        r8_abort(
          "`",
          m,
          "` is a function of '",
          existing[["package"]],
          "' that is not a generic; ",
          "name the method otherwise, such as `",
          m,
          "_of`.",
          class = "r8_declaration_error"
        )
      }
      gen_args <- names(formals(existing[["fn"]]))
      own <- names(formals(tm[["fn"]]))
      if (
        !is.null(gen_args) && !identical(own[seq_along(gen_args)], gen_args)
      ) {
        r8_abort(
          "Method `",
          m,
          "` is an S3 method of `",
          existing[["package"]],
          "::",
          m,
          "`, so its arguments begin with (",
          paste(gen_args, collapse = ", "),
          ").",
          class = "r8_declaration_error"
        )
      }
    }
    own_args <- names(formals(tm[["fn"]]))
    dispatch <- check_dispatch(
      tm[["dispatch"]],
      own_args,
      paste0(qname, "@", m),
      replacement = endsWith(m, "<-")
    )
    if (!is.null(existing)) {
      if (length(dispatch) > 1L) {
        r8_abort(
          "Method `",
          m,
          "` is an S3 method of `",
          existing[["package"]],
          "::",
          m,
          "`, which dispatches on its first argument only.",
          class = "r8_declaration_error"
        )
      }
      generics[[m]] <- existing[["fn"]]
    } else {
      generics[[m]] <- make_generic(qname, m, tm[["fn"]], dispatch)
    }
    method_ir <- list(
      name = m,
      description = tm[["description"]],
      params = dispatch_params(own_args, dispatch)
    )
    if (tm[["stochastic"]]) {
      method_ir[["stochastic"]] <- TRUE
    }
    if (!is.null(tm[["default"]])) {
      method_ir[["has_default"]] <- TRUE
    }
    methods_ir[[length(methods_ir) + 1L]] <- method_ir
  }
  ir <- list(name = name, description = description, methods = methods_ir)
  if (!is.null(tag)) {
    ir[["tag"]] <- tag
  }
  trait <- structure(
    list(
      name = qname,
      ir = ir,
      generics = generics,
      defaults = Filter(
        Negate(is.null),
        lapply(methods, function(tm) tm[["default"]])
      )
    ),
    class = "r8_trait"
  )
  trait
} # /R8::build_trait


# %% register_trait ----
#' Check a trait's declaration and register it, with its defaults
#'
#' @param trait A trait object.
#' @param namespace Character: Its namespace.
#'
#' @return `NULL`, invisibly.
#'
#' @keywords internal
#' @noRd
register_trait <- function(trait, namespace) {
  t <- unclass(trait)
  if (!is.null(t[["record"]])) {
    register_records(list(t[["record"]]))
  } else {
    problems <- r8_declare_trait(t[["ir"]], namespace)
    if (length(problems[["code"]]) > 0L) {
      declaration_failed(t[["name"]], problems)
    }
  }
  assign(t[["name"]], trait, envir = .r8_traits)
  for (m in names(t[["defaults"]])) {
    set_impl(t[["name"]], m, "", t[["defaults"]][[m]])
  }
  invisible(NULL)
} # /R8::register_trait


# %% @.r8_trait ----
#' @rdname new_trait
#' @param object A trait object.
#' @export
`@.r8_trait` <- function(object, name) {
  name <- as.character(substitute(name))
  g <- unclass(object)[["generics"]][[name]]
  if (is.null(g)) {
    r8_abort(
      "`",
      name,
      "` is not a method of `",
      unclass(object)[["name"]],
      "`.",
      class = "r8_dispatch_error"
    )
  }
  g
} # /R8::@.r8_trait


# %% impl ----
#' Implement a trait for a type
#'
#' Defines a trait's methods for a type, or for a tuple of types when its
#' methods dispatch on several arguments; a method the impl omits takes the
#' trait's default. Rust's orphan rule applies: an impl outside the trait's
#' namespace names a type of its own namespace, with only types before it.
#' Implementing the same trait for the same types again replaces the impl;
#' an impl that another namespace declares is refused.
#'
#' @param trait A trait object.
#' @param type An R8 type object; for a trait whose methods dispatch on
#'   several arguments, a list of them, one per dispatched argument the impl
#'   specializes, by position or by name, as `list(A, B)` or `list(y = B)`.
#' @param methods Named list: One function per method.
#' @param tag_value Optional Character: The value of the trait's tag member
#'   for this type; defaults to the type's name.
#'
#' @return `NULL`, invisibly.
#'
#' @examples
#' op <- options(r8.namespace = "demo")
#' Describe <- new_trait("Describe", methods = list(describe = trait_method(function(x) NULL)))
#' Point <- new_struct("Point", fields = list(x = type_double(), y = type_double()))
#' impl(Describe, Point, methods = list(describe = function(x) sprintf("(%g, %g)", x$x, x$y)))
#' describe <- Describe@describe
#' describe(Point(x = 1, y = 2))
#' options(op)
#'
#' @export
impl <- function(trait, type, methods = list(), tag_value = NULL) {
  impl_declare(trait, type, methods, tag_value, parent.frame())
} # /R8::impl


# %% impl_declare ----
#' Check, register, and record an impl
#'
#' @param trait A trait object.
#' @param type An R8 type object, or a list of them, by name or by position, one per
#'   dispatched argument the impl specializes.
#' @param methods Named list: One function per method.
#' @param tag_value Optional Character: The tag value.
#' @param env Environment: Where the impl is declared.
#' @param dispatch Optional Character: The dispatched arguments of the
#'   method declared, which name the positions of `type`; by default those of
#'   the trait's first method.
#'
#' @return `NULL`, invisibly.
#'
#' @keywords internal
#' @noRd
impl_declare <- function(
  trait,
  type,
  methods,
  tag_value,
  env,
  dispatch = NULL
) {
  if (!inherits(trait, "r8_trait")) {
    r8_abort("`trait` is a trait object.", class = "r8_declaration_error")
  }
  t <- unclass(trait)
  from <- inherits(trait, "r8_from_trait")
  # The dispatched arguments, from the trait's first method.
  # The dispatched arguments of the method declared, or else of the trait's
  # first method.
  dispatch <- dispatch %||%
    if (from) {
      "x"
    } else {
      dispatch_names(trait_ir(t)[["methods"]][[1L]][["params"]])
    }
  types <- if (is_position(type)) {
    list(type)
  } else if (is.list(type) && !is.object(type)) {
    type
  } else {
    r8_abort(
      "`type` is an R8 type object or a class, or a list of them, one per ",
      "dispatched argument of `",
      t[["name"]],
      "` (",
      paste(dispatch, collapse = ", "),
      ").",
      class = "r8_declaration_error"
    )
  }
  positions <- impl_tuple(types, dispatch, t[["name"]])
  type_name <- tuple_key(positions)
  namespace <- declaring_namespace(env)
  claim_namespace(namespace, declaring_package(env))
  source <- if (from) t[["source"]] else ""
  source_ns <- if (grepl("::", source, fixed = TRUE)) {
    sub("::.*", "", source)
  }
  if (!is.null(source_ns) && !r8_is_registered(source)) {
    r8_abort(
      "The source of From(\"",
      source,
      "\") is not a registered type; a qualified source names a declared ",
      "type, and a class name has no `::`.",
      class = "r8_declaration_error"
    )
  }
  check_members(methods, "methods", "an impl")
  fns <- methods
  methods_ir <- trait_ir(t)[["methods"]]
  declared <- vapply(
    methods_ir,
    function(m) m[["name"]],
    character(1)
  )
  unknown <- setdiff(names(fns), declared)
  if (length(unknown) > 0L) {
    r8_abort(
      "`",
      paste(unknown, collapse = "`, `"),
      "` is not a method of `",
      t[["name"]],
      "`.",
      class = "r8_declaration_error"
    )
  }
  adopted <- character()
  if (length(positions) == 1L && is_host_key(positions[[1L]])) {
    adopted <- host_methods(t, host_key_class(positions[[1L]]), fns)
  }
  for (m in names(fns)) {
    params <- param_names(methods_ir[[match(m, declared)]][["params"]])
    label <- if (isTRUE(trait_ir(t)[["generic"]])) {
      m
    } else {
      paste0(t[["name"]], "@", m)
    }
    check_method_formals(fns[[m]], params, label)
  }
  for (m in declared) {
    has_default <- isTRUE(Filter(
      function(x) x[["name"]] == m,
      methods_ir
    )[[1L]][["has_default"]])
    if (is.null(fns[[m]]) && !has_default && !m %in% adopted) {
      r8_abort(
        "The impl of `",
        t[["name"]],
        "` for `",
        type_name,
        "` defines `",
        m,
        "`, which has no default.",
        class = "r8_declaration_error"
      )
    }
  }
  ir <- c(
    list(`trait` = t[["name"]]),
    tuple_ir(positions),
    list(methods = as.list(union(names(fns), adopted)))
  )
  if (length(ir[["type_args"]]) == 0L) {
    ir[["type_args"]] <- NULL
  }
  if (!is.null(tag_value)) {
    ir[["tag_value"]] <- tag_value
  }
  if (inherits(trait, "r8_from_trait")) {
    # The source is an R8 type, named with its namespace, or an R class.
    ir[["type_args"]] <- list(
      if (!is.null(source_ns)) {
        list(kind = "type", name = t[["source"]])
      } else {
        list(
          kind = "host_class",
          host = "r",
          class = t[["source"]],
          system = "s3"
        )
      }
    )
    if (!is.null(source_ns) && type_name %in% ancestry(source)[["ancestors"]]) {
      r8_abort(
        "`",
        t[["source"]],
        "` extends `",
        type_name,
        "`, so r8_convert() upcasts it; an impl of From(\"",
        t[["source"]],
        "\") for `",
        type_name,
        "` is reserved.",
        class = "r8_declaration_error"
      )
    }
  }
  # The entry names its trait, which registers before the impl.
  entry <- list(
    kind = "impl",
    trait_name = t[["name"]],
    type_name = type_name,
    fns = fns,
    record = r8_impl_record(ir, namespace),
    namespace = namespace
  )
  if (inherits(trait, "r8_from_trait")) {
    entry[["source"]] <- t[["source"]]
  }
  if (length(adopted) > 0L) {
    entry[["adopted"]] <- adopted
  }
  s4 <- Filter(
    function(p) identical(p[["system"]], "s4"),
    attr(positions, "ir")
  )
  if (length(s4) > 0L) {
    entry[["s4_classes"]] <- s4
  }
  register_impl(entry)
  record_declaration(env, entry)
  invisible(NULL)
} # /R8::impl_declare


# %% register_impl ----
#' Register an impl: its R methods for dispatch and its declaration
#'
#' @param entry List: `trait`, `type_name`, `fns`, `ir`, and `namespace`.
#'
#' @return `NULL`, invisibly.
#'
#' @keywords internal
#' @noRd
register_impl <- function(entry) {
  for (p in entry[["s4_classes"]]) {
    check_s4_class(p[["class"]], p[["package"]])
  }
  # The registry checks the impl first, so a refused impl leaves the R
  # functions of the impls as they were.
  register_records(list(entry[["record"]]))
  t <- if (!is.null(entry[["source"]])) {
    unclass(From(entry[["source"]]))
  } else {
    unclass(.r8_traits[[entry[["trait_name"]]]])
  }
  if (!is.null(entry[["source"]])) {
    put_impl(
      "R8::From",
      entry[["source"]],
      entry[["type_name"]],
      entry[["fns"]][["from"]]
    )
    return(invisible(NULL))
  }
  # An impl that defines no method still takes precedence over the impls of
  # the type's ancestors.
  mark_impl(t[["name"]], entry[["type_name"]])
  # A method of an existing generic that the host class already has.
  for (m in entry[["adopted"]]) {
    set_impl(
      t[["name"]],
      m,
      entry[["type_name"]],
      existing_method(
        t[["generics"]][[m]],
        m,
        host_key_class(entry[["type_name"]])
      )
    )
  }
  for (m in names(entry[["fns"]])) {
    generic <- t[["generics"]][[m]]
    fn <- entry[["fns"]][[m]]
    set_impl(t[["name"]], m, entry[["type_name"]], fn)
    if (!inherits(generic, "r8_generic")) {
      # An S3 method is registered for the class itself; a host class's key
      # holds the name in double quotes.
      s3_class <- if (is_host_key(entry[["type_name"]])) {
        host_key_class(entry[["type_name"]])
      } else {
        entry[["type_name"]]
      }
      # The S3 method is the stamped impl, from which r8_next_impl() reads
      # its provider.
      fn <- .r8_impls[[paste(t[["name"]], m, entry[["type_name"]], sep = "|")]]
      if (identical(t[["name"]], "R8::Print")) {
        fn <- print_method(fn)
      } else if (identical(t[["name"]], "R8::Display")) {
        fn <- display_method(fn)
      } else if (endsWith(m, "<-")) {
        fn <- replacement_method(fn, paste(t[["name"]], m, sep = "|"))
      }
      registerS3method(m, s3_class, fn, envir = s3_envir(generic))
    }
  }
  invisible(NULL)
} # /R8::register_impl


# %% check_s4_class ----
#' Check that a package defines an S4 class
#'
#' Checks `host_class(class, package, system = "s4")` when the package's
#' namespace is loaded: at the impl's declaration, and in `r8_register()`
#' at the declaring package's load.
#'
#' @param class Character: The class name.
#' @param package Character: The package.
#'
#' @return `NULL`, invisibly; a package that defines no S4 class of the name,
#'   or defines it as an S3 class registered with S4, raises
#'   `r8_declaration_error`.
#'
#' @keywords internal
#' @noRd
check_s4_class <- function(class, package) {
  if (!isNamespaceLoaded(package)) {
    return(invisible(NULL))
  }
  def <- methods::getClassDef(class, package = package)
  if (
    is.null(def) ||
      !identical(def@package, package) ||
      methods::extends(def, "oldClass")
  ) {
    r8_abort(
      "'",
      package,
      "' defines no S4 class `",
      class,
      "`; an S3 class is named as a string, \"",
      class,
      "\" [host_class_s4].",
      class = "r8_declaration_error"
    )
  }
  invisible(NULL)
} # /R8::check_s4_class


# %% existing_method ----
#' The method of an existing generic for exactly a class, outside R8
#'
#' @param generic Function: An S3 generic of another package.
#' @param method Character: Its name.
#' @param class Character: The class.
#'
#' @return Function, or `NULL` when the class has no such method or R8
#'   registered it.
#'
#' @keywords internal
#' @noRd
existing_method <- function(generic, method, class) {
  fn <- utils::getS3method(
    method,
    class,
    optional = TRUE,
    envir = s3_envir(generic)
  )
  if (is.null(fn) || !is.null(attr(fn, "r8_provider"))) NULL else fn
} # /R8::existing_method


# %% host_methods ----
#' The methods of an impl on a host class that the class already has
#'
#' A trait method that maps an existing S3 generic, for a class that already
#' has a method of that generic outside R8, takes that method: the impl
#' omits it, and supplying one raises `r8_declaration_error`, since
#' registering it would replace another package's method for every caller.
#' A trait method that maps an S4 generic is not implemented for host
#' classes.
#'
#' @param t List: The unclassed trait.
#' @param class Character: The host class.
#' @param fns Named list: The impl's functions.
#'
#' @return Character: The names of the methods the class already has.
#'
#' @keywords internal
#' @noRd
host_methods <- function(t, class, fns) {
  adopted <- character()
  for (m in names(t[["generics"]])) {
    generic <- t[["generics"]][[m]]
    if (inherits(generic, "r8_generic")) {
      next
    }
    if (is_s4_generic(generic)) {
      r8_abort(
        "`",
        m,
        "` of `",
        t[["name"]],
        "` is an S4 generic; impls of S4 generics on host classes are not ",
        "declared by R8.",
        class = "r8_declaration_error"
      )
    }
    existing <- existing_method(generic, m, class)
    if (is.null(existing)) {
      next
    }
    if (!is.null(fns[[m]])) {
      env <- environment(existing)
      r8_abort(
        "`",
        class,
        "` has a method of `",
        m,
        "`",
        if (!is.null(env)) paste0(" in '", environmentName(topenv(env)), "'"),
        "; the impl of `",
        t[["name"]],
        "` for `",
        class,
        "` omits `",
        m,
        "` and takes that method, since registering another would replace ",
        "it for every caller [host_method_exists].",
        class = "r8_declaration_error"
      )
    }
    adopted <- c(adopted, m)
  }
  adopted
} # /R8::host_methods


# %% set_impl ----
#' Record the impl of a trait method for a type
#'
#' @param trait_name Character: The trait's qualified name.
#' @param method Character: The method name.
#' @param type Character: The qualified type name, or `""` for the trait's
#'   default.
#' @param fn Function: The impl.
#'
#' @return `NULL`, invisibly.
#'
#' @keywords internal
#' @noRd
set_impl <- function(trait_name, method, type, fn) {
  put_impl(trait_name, method, type, fn)
} # /R8::set_impl


# %% with_record ----
#' Register a trait and keep its declaration as a record
#'
#' @param trait A trait object holding its declaration as R lists.
#' @param namespace Character: Its namespace.
#'
#' @return The trait object, holding its declaration record in place of the
#'   lists.
#'
#' @keywords internal
#' @noRd
with_record <- function(trait, namespace) {
  register_trait(trait, namespace)
  t <- unclass(trait)
  t[["record"]] <- r8_trait_record(t[["name"]])
  t[["ir"]] <- NULL
  structure(t, class = class(trait))
} # /R8::with_record


# %% trait_ir ----
#' The declaration of a registered trait
#'
#' @param trait A trait object, or its unclassed list.
#'
#' @return List: The declaration, in the form the declaration functions build,
#'   from the registry.
#'
#' @keywords internal
#' @noRd
trait_ir <- function(trait) {
  r8_trait_ir(unclass(trait)[["name"]])
} # /R8::trait_ir


# %% print.r8_trait ----
#' Print a trait object
#'
#' @param x A trait object.
#' @param ... Not used.
#'
#' @return `x`, invisibly.
#'
#' @export
print.r8_trait <- function(x, ...) {
  t <- unclass(x)
  cat("<r8 trait ", t[["name"]], ">\n", sep = "")
  for (m in trait_ir(t)[["methods"]]) {
    cat(
      "  ",
      m[["name"]],
      "(",
      paste(param_names(m[["params"]]), collapse = ", "),
      ")\n",
      sep = ""
    )
  }
  invisible(x)
} # /R8::print.r8_trait
