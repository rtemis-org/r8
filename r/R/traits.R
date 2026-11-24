# traits.R
# ::R8::
# 2026- EDG rtemis.org

# Impls of R8 traits written in R, by "trait|method|type". A generic made by
# a trait looks up the impl for the qualified type of its first argument.
.r8_impls <- new.env(parent = emptyenv())

# The dispatch cache: by `"Trait|method"`, an environment of the selected
# impl of the method by qualified type, or by `class:` and the class of an
# object of another class; `r8_find_impl()` fills it from `.r8_impls` on a
# miss of the C routine `find`. A generic reads it in R8's namespace, so a
# generic sent to a worker selects among the worker's impls. `put_impl()` drops a
# method's index when one of its impls changes; unloading a package and
# declaring a type clear the cache.
# Its parent is R8's namespace, in which the C routine `find_n()` calls
# `dispatch_select()`; every lookup in it reads its own frame only.
.r8_dispatch <- new.env(parent = environment())

# The types with an impl of a trait, by `"Trait|type"`: the unit of precedence
# through `extends`, so that the nearest type with an impl provides every
# method, and a method its impl omits takes the trait's default
# implementation.
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

# The native symbols of the C routines `call`, which checks the arguments of
# a typed generic's call and finds the impl, and `result`, which checks its
# result, bound when the package loads.
.r8_c_call <- NULL
.r8_c_result <- NULL

# Trait objects by qualified name, for the impls of compiled code.
.r8_traits <- new.env(parent = emptyenv())

# Packages whose generics a trait method of the same name becomes a method
# of, beside the imports of the declaring package.
BASE_PACKAGES <- c("base", "stats", "utils", "methods", "graphics", "grDevices")


# %% trait_method ----
#' Declare a trait method
#'
#' @param params The method's parameters, from [params()]; the first is the
#'   value the method dispatches on.
#' @param description Character: What the method does.
#' @param default_impl Optional function: The method's default
#'   implementation, used for a type whose impl does not define the method.
#' @param stochastic Logical: The method draws random numbers.
#' @param dispatch Optional Character: The arguments the method dispatches
#'   on, by name, at most three and none of them `...`; the first argument
#'   by default. Every method of a trait dispatches on the same number of
#'   arguments, and an impl names a type for any of them (`impl()`).
#' @param return_type Optional: The type of the value that every impl's
#'   method returns, an `r8_field_type`, an R8 type, or `"Self"`, the type of
#'   the first dispatched argument. An impl may declare a type that fits it
#'   ([impl_method()]). A replacement method returns a value of its
#'   argument's type and declares none.
#'
#' @return An `r8_trait_method`.
#'
#' @seealso [params()], [impl()].
#'
#' @examples
#' trait_method(params(spec, x, y), description = "Trains a model.")
#' trait_method(params(x, y, "..."), dispatch = c("x", "y"))
#' trait_method(
#'   params(spec, x, verbosity = arg_integer(min = 0L, default = 1L)),
#'   return_type = type_double()
#' )
#'
#' @export
trait_method <- function(
  params,
  description = "",
  default_impl = NULL,
  stochastic = FALSE,
  dispatch = NULL,
  return_type = NULL
) {
  p <- as_params(params, "trait_method()")
  if (!is.null(default_impl)) {
    check_method_formals(
      default_impl,
      p[["names"]],
      "the default implementation"
    )
  }
  structure(
    list(
      params = p,
      description = description,
      default_impl = default_impl,
      stochastic = isTRUE(stochastic),
      dispatch = dispatch,
      return_type = return_type_ir(return_type, self = TRUE)
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
    if (requireNamespace(p, quietly = TRUE) && exports_name(p, name)) {
      return(list(fn = getExportedValue(p, name), package = p))
    }
  }
  NULL
} # /R8::existing_generic


# %% exports_name ----
#' Whether a loaded package exports a name
#'
#' Looks the name up in the package's table of exports, where
#' `getNamespaceExports()` lists the whole table.
#'
#' @param package Character: The package.
#' @param name Character: The name.
#'
#' @return Logical.
#'
#' @keywords internal
#' @noRd
exports_name <- function(package, name) {
  table <- if (identical(package, "base")) {
    baseenv()
  } else {
    getNamespaceInfo(package, "exports")
  }
  exists(name, envir = table, inherits = FALSE)
} # /R8::exports_name


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
#' The generic finds the impl for its dispatched arguments and calls it with
#' its formals. A replacement generic, whose method name ends in `<-`, passes the
#' impl's result to the C routine `replaced`, which returns it when it is a
#' value of the first argument's type.
#'
#' @param trait_name Character: The trait's qualified name.
#' @param method Character: The method name.
#' A generic with typed parameters passes its frame to the C routine `call`,
#' which forces and checks them before it finds the impl; one with a return
#' type passes the impl's result to the C routine `result`, and returns it
#' invisibly when the impl did. A generic without types calls neither.
#'
#' @param p An `r8_params`: The parameters.
#' @param dispatch Character: The dispatched arguments, in formal order.
#' @param return_type The return type's IR, or `NULL`.
#'
#' @return The generic, with its dispatched arguments as the attribute
#'   `r8_dispatch`.
#'
#' @keywords internal
#' @noRd
make_generic <- function(trait_name, method, p, dispatch, return_type = NULL) {
  args <- p[["names"]]
  first <- as.name(args[[1L]])
  call_args <- lapply(args, function(a) {
    if (a == "...") quote(...) else as.name(a)
  })
  names(call_args) <- ifelse(args == "...", "", args)
  key <- as.name(paste(trait_name, method, sep = "|"))
  # The generic is enclosed by the environment that calls params(), where its
  # defaults are evaluated, and reaches R8 through R8's namespace, a constant
  # of its body, which serializes by reference. Unserialized in a worker, the
  # generic of a package loads the package and dispatches through the
  # worker's impls.
  ns <- asNamespace("R8")
  at <- function(name) call("$", ns, as.name(name))
  typed <- !all(vapply(p[["types"]], is.null, logical(1)))
  find <- as.call(c(
    list(quote(.Call)),
    if (typed) {
      list(at(".r8_c_call"), quote(environment()))
    } else {
      c(
        at(c(".r8_c_find", ".r8_c_find2", ".r8_c_find3")[[length(dispatch)]]),
        lapply(dispatch, as.name)
      )
    },
    list(call("quote", key), at(".r8_dispatch"))
  ))
  body <- as.call(c(list(find), call_args))
  if (endsWith(method, "<-")) {
    body <- as.call(list(
      quote(.Call),
      at(".r8_c_replaced"),
      first,
      body,
      call("quote", key)
    ))
  } else if (!is.null(return_type)) {
    checked <- as.call(list(
      quote(.Call),
      at(".r8_c_result"),
      quote(.r8_v),
      quote(.r8_f),
      call("quote", key),
      as.name(dispatch[[1L]]),
      at(".r8_dispatch")
    ))
    body <- call(
      "{",
      call("<-", quote(.r8_f), find),
      call(
        "<-",
        quote(.r8_v),
        call("withVisible", as.call(c(list(quote(.r8_f)), call_args)))
      ),
      call("if", checked, quote(.r8_v[[1L]]), quote(invisible(.r8_v[[1L]])))
    )
  }
  check_base_names(p[["env"]], method)
  structure(
    as.function(c(p[["formals"]], body), envir = p[["env"]]),
    class = c("r8_generic", "function"),
    r8_trait = trait_name,
    r8_method = method,
    r8_dispatch = dispatch
  )
} # /R8::make_generic


# The functions of base R that a generic's body calls.
GENERIC_BODY_CALLS <- c(
  ".Call",
  "$",
  "quote",
  "environment",
  "withVisible",
  "invisible",
  "[[",
  "{",
  "<-",
  "if"
)


# %% check_base_names ----
#' Refuse an environment that masks the functions a generic's body calls
#'
#' A generic's body calls these functions of base R from the environment that
#' encloses the generic, where the byte compiler inlines them.
#'
#' @param env Environment: The generic's environment.
#' @param method Character: The generic's or trait method's name.
#'
#' @return `NULL`, invisibly.
#'
#' @keywords internal
#' @noRd
check_base_names <- function(env, method) {
  for (n in GENERIC_BODY_CALLS) {
    if (!identical(get(n, envir = env, mode = "function"), get(n, baseenv()))) {
      r8_abort(
        "`",
        n,
        "` is masked where the generic `",
        method,
        "` is declared; a generic calls base R's `",
        n,
        "`, so the function that masks it takes another name ",
        "[base_masked].",
        class = "r8_declaration_error"
      )
    }
  }
  invisible(NULL)
} # /R8::check_base_names


# %% check_method_defaults ----
#' Refuse a method's default for a parameter of its R8 generic
#'
#' A generic calls its method with every parameter of its own, so a default
#' of the method for one of them is never used. A method's own arguments
#' after `...`, and a method of an S3 generic that R8 does not own, keep
#' their defaults.
#'
#' @param fn Function: The method.
#' @param params Character: The generic's parameters.
#' @param label Character: The generic's or trait method's name.
#' @param typed Character: The parameters with a type.
#'
#' @return `NULL`, invisibly.
#'
#' @keywords internal
#' @noRd
check_method_defaults <- function(fn, params, label, typed) {
  fmls <- formals(fn)
  for (p in setdiff(params, "...")) {
    if (!identical(fmls[[p]], quote(expr = ))) {
      default <- deparse1(fmls[[p]])
      r8_abort(
        "The method of `",
        label,
        "` gives `",
        p,
        "` a default, which belongs to the generic: ",
        if (p %in% typed) {
          paste0("`default = ", default, "` in the type of `", p, "`")
        } else {
          paste0("`", p, " = ", default, "`")
        },
        " in its params() [method_default].",
        class = "r8_declaration_error"
      )
    }
  }
  invisible(NULL)
} # /R8::check_method_defaults


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


# %% checked_s3_method ----
#' The S3 method of an impl for an R8 type, with the check of its value
#'
#' S3 dispatch reads the class vector of a value; the check, the first
#' statement of the method, reads its declaration.
#'
#' @param fn Function: The method.
#'
#' @return Function: `fn` with the check before its body, its formals,
#'   environment, and attributes kept.
#'
#' @keywords internal
#' @noRd
checked_s3_method <- function(fn) {
  first <- names(formals(fn))[[1L]]
  if (identical(first, "...")) {
    return(fn)
  }
  check <- as.call(list(
    quote(.Call),
    call("$", asNamespace("R8"), quote(.r8_c_require)),
    as.name(first)
  ))
  kept <- attributes(fn)
  kept[["srcref"]] <- NULL
  body(fn) <- call("{", check, body(fn))
  attributes(fn) <- kept
  fn
} # /R8::checked_s3_method


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
  r8 <- is_value(x)
  type <- if (r8) value_type(x) else class(x)[1L]
  fn <- tryCatch(
    impl_in_chain(trait_name, method, type, r8 = r8),
    error = function(e) NULL
  )
  provider <- attr(fn, "r8_provider")[3L]
  got <- if (is_value(out)) {
    paste0("a value of `", value_type(out), "`")
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
      ", from the trait's default implementation,"
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
    class = c("r8_result_error", "r8_type_error")
  )
} # /R8::replacement_abort


# %% dispatch_select ----
#' Resolve the impl of a trait method for a call on a miss of the cache
#'
#' Called by the C routines `find`, `find2`, and `find3` when the dispatch
#' cache has no impl for the types of the dispatched arguments;
#' `r8_find_impl()` finds the impl and stores it.
#'
#' @param key Character: `"Trait|method"`.
#' @param trait_name Character: The trait's qualified name, the part of
#'   `key` before `|`.
#' @param method Character: The method name, the part after it.
#' @param ... The values of the dispatched arguments, in order.
#'
#' @return The impl function.
#'
#' @keywords internal
#' @noRd
dispatch_select <- function(key, trait_name, method, ...) {
  r8_find_impl(trait_name, method, list(...), key)
} # /R8::dispatch_select


# %% cache_impl ----
#' Store a selected impl in the dispatch cache
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
  if (length(args) == 1L) {
    k <- dispatch_key(args[[1L]])
    if (!is.null(k)) {
      node <- .r8_dispatch[[key]]
      if (is.null(node)) {
        node <- new.env(parent = emptyenv())
        assign(key, node, envir = .r8_dispatch)
      }
      assign(k, impl, envir = node)
    }
    return(invisible(NULL))
  }
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
#'   trait's default implementation.
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
#' R8 caches the method each generic selected for the chain of its
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
#' The nearest of `type` and its ancestors with an impl of the trait provides
#' the method, or the trait's default implementation when that impl omits it;
#' without such a type, the trait's default implementation applies. With
#' `after`, the search starts at the parent of `after`, for `r8_next_impl()`.
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
      return(
        .r8_impls[[paste(trait_name, method, t, sep = "|")]] %||% default
      )
    }
  }
  # After the running impl, the default follows unless it is running.
  if (!is.null(after) && !nzchar(after)) {
    return(NULL)
  }
  default
} # /R8::impl_in_chain


# %% r8_find_impl ----
#' The impl of a trait method for the values of its dispatched arguments
#'
#' Resolves through the core's rule of dispatch (`select_tuple()`), over the
#' chain of each argument: an R8 value's type and its ancestors, or another
#' object's class vector as `.class2()` gives it.
#'
#' @param trait_name Character: The trait's qualified name.
#' @param method Character: The method name.
#' @param args List: The values of the dispatched arguments.
#' @param key Character: `"Trait|method"`, the method's key in the dispatch
#'   cache.
#'
#' @return The impl function.
#'
#' @keywords internal
#' @noRd
r8_find_impl <- function(
  trait_name,
  method,
  args,
  key = paste(trait_name, method, sep = "|")
) {
  impl <- select_tuple(trait_name, method, args)
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
  if (is_value(x)) value_type(x) else class(x)[1L]
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
#' @param tag Optional Character: The member of a `dyn()` value's JSON form
#'   that holds the implementor's type, by its tag value; without it, the
#'   member `"$type"` holds the qualified type name. An enum's `tag` is
#'   another member, which holds a variant's name.
#' @param required_fields Named list: The fields every implementor has, each a
#'   field type, an R8 type object, or `field(type, description = )`. An
#'   implementor is a struct whose field of each name has a type that fits the
#'   required one: the same type; `T` or `option(T)` for `option(T)`; a subtype
#'   of the struct for `subtype()`; an implementor of the trait for `dyn()`. A
#'   constraint narrows nothing, so a field of `type_double(min = 0)` fits
#'   `type_double(min = 0)` only. Code that receives a `dyn()` value of the
#'   trait reads these fields.
#'
#' @return The trait object, of class `r8_trait`.
#'
#' @examples
#' op <- options(r8.namespace = "demo")
#' Fit <- new_trait("Fit", methods = list(fit = trait_method(params(spec, x, y))))
#' fit <- Fit@fit
#' options(op)
#'
#' @export
new_trait <- function(
  name,
  methods = list(),
  description = "",
  tag = NULL,
  required_fields = list()
) {
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
    build_trait(
      name,
      methods,
      description,
      tag,
      namespace,
      env,
      required_fields
    ),
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
#' @param required_fields Named list: The required fields, as `new_trait()`
#'   takes them.
#'
#' @return The trait object.
#'
#' @keywords internal
#' @noRd
build_trait <- function(
  name,
  methods,
  description,
  tag,
  namespace,
  env,
  required_fields = list()
) {
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
    p <- tm[["params"]]
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
      own <- p[["names"]]
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
    dispatch <- check_dispatch(
      tm[["dispatch"]],
      p[["names"]],
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
      check_mapped(tm, m, existing[["package"]])
      generics[[m]] <- existing[["fn"]]
    } else {
      if (!is.null(tm[["default_impl"]])) {
        check_method_defaults(
          tm[["default_impl"]],
          p[["names"]],
          paste0(qname, "@", m),
          p[["names"]][!vapply(p[["types"]], is.null, NA)]
        )
      }
    }
    method_ir <- list(
      name = m,
      description = tm[["description"]],
      params = params_ir(p, dispatch)
    )
    method_ir[["return_type"]] <- tm[["return_type"]]
    if (is.null(existing)) {
      generics[[m]] <- make_generic(
        qname,
        m,
        stored_defaults(p),
        dispatch,
        tm[["return_type"]]
      )
    }
    if (tm[["stochastic"]]) {
      method_ir[["stochastic"]] <- TRUE
    }
    if (!is.null(tm[["default_impl"]])) {
      method_ir[["has_default_impl"]] <- TRUE
    }
    methods_ir[[length(methods_ir) + 1L]] <- method_ir
  }
  ir <- list(name = name, description = description, methods = methods_ir)
  if (length(required_fields) > 0L) {
    check_members(required_fields, "required_fields", paste0("`", qname, "`"))
    ir[["required_fields"]] <- declared(r8_required_fields(required_fields))
  }
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
        lapply(methods, function(tm) tm[["default_impl"]])
      )
    ),
    class = "r8_trait"
  )
  trait
} # /R8::build_trait


# %% check_mapped ----
#' Refuse what a trait method that maps an existing generic does not use
#'
#' R calls the existing generic, with its own formals, and each method with
#' its own defaults, so such a method's parameters are names, without types
#' and defaults, and it declares no return type.
#'
#' @param tm An `r8_trait_method`.
#' @param method Character: Its name.
#' @param package Character: The existing generic's package.
#'
#' @return `NULL`, invisibly.
#'
#' @keywords internal
#' @noRd
check_mapped <- function(tm, method, package) {
  p <- tm[["params"]]
  typed <- !vapply(p[["types"]], is.null, logical(1))
  given <- typed | nzchar(p[["texts"]])
  if (!any(given) && is.null(tm[["return_type"]])) {
    return(invisible(NULL))
  }
  i <- match(TRUE, given)
  r8_abort(
    "Method `",
    method,
    "` is an S3 method of `",
    package,
    "::",
    method,
    "`, whose own arguments and whose methods' own defaults apply; ",
    if (is.na(i)) {
      "it declares no return type"
    } else {
      paste0(
        "its parameter `",
        p[["names"]][[i]],
        "` takes no ",
        if (typed[[i]]) "type" else "default"
      )
    },
    " [mapped_generic].",
    class = "r8_declaration_error"
  )
} # /R8::check_mapped


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
#' trait's default implementation. Rust's orphan rule applies: an impl outside
#' the trait's namespace names a type of its own namespace, with only types
#' before it. Implementing the same trait for the same types again replaces the
#' impl; an impl that another namespace declares is refused.
#'
#' @param trait A trait object.
#' @param target What the trait is implemented for: an R8 type object or a
#'   class ([host_class()]); for a trait whose methods dispatch on several
#'   arguments, a list of them, one per dispatched argument the impl
#'   specializes, by position or by name, as `list(A, B)` or `list(y = B)`.
#' @param methods Named list: One function per method, or [impl_method()] for
#'   a method with its own return type.
#' @param tag_value Optional Character: The value of the trait's tag member
#'   for this type; defaults to the type's name.
#'
#' @return `NULL`, invisibly.
#'
#' @examples
#' op <- options(r8.namespace = "demo")
#' Describe <- new_trait("Describe", methods = list(describe = trait_method(params(x))))
#' Point <- new_struct("Point", fields = list(x = type_double(), y = type_double()))
#' impl(Describe, Point, methods = list(describe = function(x) sprintf("(%g, %g)", x$x, x$y)))
#' describe <- Describe@describe
#' describe(Point(x = 1, y = 2))
#' options(op)
#'
#' @export
impl <- function(trait, target, methods = list(), tag_value = NULL) {
  impl_declare(trait, target, methods, tag_value, parent.frame())
} # /R8::impl


# %% impl_method ----
#' Declare a method of an impl with its own return type
#'
#' In the `methods` of [impl()], a function with the type of the value it
#' returns, which fits the return type of the trait's method:
#' `impl(Train, GLMNET, methods = list(train = impl_method(fn, return_type =
#' GLMNETModel)))`, where `train` is declared with
#' `return_type = subtype(Model)`. The same type fits; a struct, or
#' `subtype()` of it, fits `subtype()` of a struct it extends; a type with an
#' impl of the trait fits `dyn()` of it; and `T` or `option(T)` fits
#' `option(T)`. A constraint narrows nothing: `type_double(min = 0)` fits
#' `type_double(min = 0)` only. `implement(generic, Type, return_type = T) <-
#' fn` declares the same for a generic.
#'
#' @param fn Function: The method.
#' @param return_type An `r8_field_type` or an R8 type: The type of the value
#'   it returns.
#'
#' @return An `r8_impl_method`.
#'
#' @seealso [impl()], [implement<-].
#'
#' @examples
#' op <- options(r8.namespace = "demo")
#' Model <- new_struct("Model", fields = list(rss = type_double()))
#' LinearModel <- new_struct("LinearModel", extends = Model)
#' Linear <- new_struct("Linear")
#' Train <- new_trait("Train", methods = list(
#'   train = trait_method(params(spec, y), return_type = subtype(Model))
#' ))
#' impl(Train, Linear, methods = list(
#'   train = impl_method(
#'     function(spec, y) LinearModel(rss = sum((y - mean(y))^2)),
#'     return_type = LinearModel
#'   )
#' ))
#' Train@train(Linear(), c(1, 2, 3))
#' options(op)
#'
#' @export
impl_method <- function(fn, return_type) {
  if (!is.function(fn)) {
    r8_abort("`fn` is a function.", class = "r8_declaration_error")
  }
  if (is.null(return_type)) {
    r8_abort(
      "impl_method() gives a method its own return type; a method without ",
      "one is the function itself.",
      class = "r8_declaration_error"
    )
  }
  structure(
    list(fn = fn, return_type = return_type_ir(return_type, self = FALSE)),
    class = "r8_impl_method"
  )
} # /R8::impl_method


# %% impl_declare ----
#' Check, register, and record an impl
#'
#' @param trait A trait object.
#' @param type An R8 type object, or a list of them, by name or by position, one per
#'   dispatched argument the impl specializes.
#' @param methods Named list: One function or `impl_method()` per method.
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
  source_type <- from && identical(t[["source_ir"]][["kind"]], "named")
  check_members(methods, "methods", "an impl")
  typed <- vapply(methods, inherits, logical(1), "r8_impl_method")
  return_types <- lapply(methods[typed], function(m) m[["return_type"]])
  fns <- methods
  fns[typed] <- lapply(methods[typed], function(m) m[["fn"]])
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
    params_ir <- methods_ir[[match(m, declared)]][["params"]]
    params <- param_names(params_ir)
    label <- if (isTRUE(trait_ir(t)[["generic"]])) {
      m
    } else {
      paste0(t[["name"]], "@", m)
    }
    check_method_formals(fns[[m]], params, label)
    # A trait method that maps an S3 generic is called by R, which applies
    # the method's own defaults.
    mapped <- t[["generics"]][[m]]
    if (is.null(mapped) || inherits(mapped, "r8_generic")) {
      typed <- vapply(
        params_ir,
        function(p) is.list(p) && !is.null(p[["type"]]),
        NA
      )
      check_method_defaults(fns[[m]], params, label, params[typed])
    }
  }
  for (m in declared) {
    has_default_impl <- isTRUE(Filter(
      function(x) x[["name"]] == m,
      methods_ir
    )[[1L]][["has_default_impl"]])
    if (is.null(fns[[m]]) && !has_default_impl && !m %in% adopted) {
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
    list(
      methods = lapply(union(names(fns), adopted), function(m) {
        if (is.null(return_types[[m]])) {
          m
        } else {
          list(name = m, return_type = return_types[[m]])
        }
      })
    )
  )
  if (length(ir[["type_args"]]) == 0L) {
    ir[["type_args"]] <- NULL
  }
  if (!is.null(tag_value)) {
    ir[["tag_value"]] <- tag_value
  }
  if (inherits(trait, "r8_from_trait")) {
    # The source is an R8 type or a host class.
    ir[["type_args"]] <- list(t[["source_ir"]])
    if (source_type && type_name %in% ancestry(source)[["ancestors"]]) {
      r8_abort(
        "`",
        t[["source"]],
        "` extends `",
        type_name,
        "`, so r8_convert() upcasts it; an impl of From(",
        sub("^.*::", "", t[["source"]]),
        ") for `",
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
    if (identical(t[["source_ir"]][["system"]], "s4")) {
      entry[["s4_classes"]] <- list(t[["source_ir"]])
    }
  }
  if (length(adopted) > 0L) {
    entry[["adopted"]] <- adopted
  }
  s4 <- Filter(
    function(p) identical(p[["system"]], "s4"),
    attr(positions, "ir")
  )
  if (length(s4) > 0L) {
    entry[["s4_classes"]] <- c(entry[["s4_classes"]], s4)
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
  check_trait_registered(
    entry[["trait_name"]],
    r8_namespace_owner(entry[["namespace"]])
  )
  for (p in entry[["s4_classes"]]) {
    check_s4_class(p[["class"]], p[["package"]])
  }
  # The registry checks the impl first, so a refused impl leaves the R
  # functions of the impls as they were.
  register_records(list(entry[["record"]]))
  if (!is.null(entry[["source"]])) {
    put_impl(
      "R8::From",
      entry[["source"]],
      entry[["type_name"]],
      entry[["fns"]][["from"]]
    )
    return(invisible(NULL))
  }
  t <- unclass(.r8_traits[[entry[["trait_name"]]]])
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
    register_method(t, m, entry[["type_name"]], entry[["fns"]][[m]])
  }
  invisible(NULL)
} # /R8::register_impl


# %% register_method ----
#' Register the function of an impl's method for dispatch
#'
#' The function is the trait's method for the positions; for a method that
#' maps an S3 generic, it is the S3 method for the class too.
#'
#' @param t List: The trait, unclassed.
#' @param m Character: The method.
#' @param key Character: The key of the positions (`tuple_key()`).
#' @param fn Function: The function.
#'
#' @return `NULL`, invisibly.
#'
#' @keywords internal
#' @noRd
register_method <- function(t, m, key, fn) {
  generic <- t[["generics"]][[m]]
  set_impl(t[["name"]], m, key, fn)
  if (!inherits(generic, "r8_generic")) {
    # An S3 method is registered for the class itself; a host class's key
    # holds the name in double quotes.
    s3_class <- if (is_host_key(key)) host_key_class(key) else key
    # The S3 method is the stamped impl, from which r8_next_impl() reads its
    # provider.
    fn <- .r8_impls[[paste(t[["name"]], m, key, sep = "|")]]
    if (identical(t[["name"]], "R8::Print")) {
      fn <- print_method(fn)
    } else if (identical(t[["name"]], "R8::Display")) {
      fn <- display_method(fn)
    } else if (endsWith(m, "<-")) {
      fn <- replacement_method(fn, paste(t[["name"]], m, sep = "|"))
    }
    if (!is_host_key(key) && !t[["name"]] %in% c("R8::Print", "R8::Display")) {
      fn <- checked_s3_method(fn)
    }
    registerS3method(m, s3_class, fn, envir = s3_envir(generic))
  }
  invisible(NULL)
} # /R8::register_method


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
#'   default implementation.
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
    lines <- signature_lines(m, t[["generics"]][[m[["name"]]]], "    ")
    if (length(lines) > 0L) {
      cat(lines, sep = "\n")
    }
  }
  invisible(x)
} # /R8::print.r8_trait
