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

# The native symbol of the C routine `find` (`r/src/view.c`), bound when the
# package loads.
.r8_c_find <- NULL

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
#'
#' @return An `r8_trait_method`.
#'
#' @examples
#' trait_method(function(spec, x, y) NULL, description = "Trains a model.")
#'
#' @export
trait_method <- function(
  fn,
  description = "",
  default = NULL,
  stochastic = FALSE
) {
  if (!is.function(fn) || length(formals(fn)) == 0L) {
    r8_abort(
      "A trait method's signature is a function with at least one argument.",
      class = "r8_declaration_error"
    )
  }
  structure(
    list(
      fn = fn,
      description = description,
      default = default,
      stochastic = isTRUE(stochastic)
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
  `@` = "",
  as.list = ""
)


# %% make_generic ----
#' A new generic for a trait method
#'
#' @param trait_name Character: The trait's qualified name.
#' @param method Character: The method name.
#' @param fn Function: The signature.
#'
#' @return The generic.
#'
#' @keywords internal
#' @noRd
make_generic <- function(trait_name, method, fn) {
  args <- names(formals(fn))
  first <- as.name(args[[1L]])
  call_args <- lapply(args, function(a) {
    if (a == "...") quote(...) else as.name(a)
  })
  names(call_args) <- ifelse(args == "...", "", args)
  key <- as.name(paste(trait_name, method, sep = "|"))
  # Constants only: the method's key as a symbol and namespace bindings, so
  # a generic sent to a worker dispatches through the worker's impls.
  find <- bquote(.Call(.r8_c_find, .(first), quote(.(key)), .r8_dispatch))
  body <- as.call(c(list(call("(", find)), call_args))
  g <- as.function(c(formals(fn), body), envir = asNamespace("R8"))
  structure(
    g,
    class = c("r8_generic", "function"),
    r8_trait = trait_name,
    r8_method = method
  )
} # /R8::make_generic


# %% dispatch_resolve ----
#' Resolve the impl of a trait method for a value on a miss of the cache
#'
#' Called by the C routine `find` when the dispatch cache has no impl for the
#' value's type; `r8_find_impl()` finds the impl and stores it.
#'
#' @param key Character: `"Trait|method"`.
#' @param x The value dispatched on.
#'
#' @return The impl function.
#'
#' @keywords internal
#' @noRd
dispatch_resolve <- function(key, x) {
  at <- regexpr("|", key, fixed = TRUE)
  r8_find_impl(substr(key, 1L, at - 1L), substr(key, at + 1L, nchar(key)), x)
} # /R8::dispatch_resolve


# %% cache_impl ----
#' Store a resolved impl in the dispatch cache
#'
#' Under the value's qualified type, or under `class:` and the first class of
#' an object with a class attribute: the keys the C routine `find` reads.
#'
#' @param key Character: `"Trait|method"`.
#' @param x The value dispatched on.
#' @param impl Function: The impl.
#'
#' @return `NULL`, invisibly.
#'
#' @keywords internal
#' @noRd
cache_impl <- function(key, x, impl) {
  type <- attr(x, "r8_type", exact = TRUE)
  name <- if (is.character(type) && length(type) == 1L) {
    type
  } else if (!is.null(attr(x, "class", exact = TRUE))) {
    paste0("class:", class(x)[[1L]])
  }
  if (!is.null(name)) {
    index <- .r8_dispatch[[key]]
    if (is.null(index)) {
      index <- new.env(parent = emptyenv())
      assign(key, index, envir = .r8_dispatch)
    }
    assign(name, impl, envir = index)
  }
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
  assign(paste(trait_name, method, type, sep = "|"), fn, envir = .r8_impls)
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
  invisible(NULL)
} # /R8::clear_dispatch


# %% r8_find_impl ----
#' The impl of a trait method for a value
#'
#' @param trait_name Character: The trait's qualified name.
#' @param method Character: The method name.
#' @param x The value dispatched on.
#'
#' @return The impl function.
#'
#' @keywords internal
#' @noRd
r8_find_impl <- function(trait_name, method, x) {
  type <- if (inherits(x, "r8_value")) attr(x, "r8_type") else class(x)[1L]
  impl <- .r8_impls[[paste(trait_name, method, type, sep = "|")]]
  if (is.null(impl)) {
    impl <- .r8_impls[[paste(trait_name, method, "", sep = "|")]]
  }
  if (!is.null(impl)) {
    cache_impl(paste(trait_name, method, sep = "|"), x, impl)
    return(impl)
  }
  r8_abort(
    "`",
    type,
    "` does not implement `",
    trait_name,
    "` (method `",
    method,
    "`).",
    class = "r8_dispatch_error"
  )
} # /R8::r8_find_impl


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
      "A trait name is an R8 name, such as \"Fit\".",
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
      generics[[m]] <- existing[["fn"]]
    } else {
      generics[[m]] <- make_generic(qname, m, tm[["fn"]])
    }
    method_ir <- list(
      name = m,
      description = tm[["description"]],
      params = as.list(names(formals(tm[["fn"]])))
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
#' Defines a trait's methods for a type; a method the impl omits takes the
#' trait's default. Rust's orphan rule applies: an impl is declared in the
#' namespace of the trait or of the type. Implementing the same trait for the
#' same type again replaces the impl.
#'
#' @param trait A trait object.
#' @param type An R8 type object.
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
#' @param type An R8 type object.
#' @param methods Named list: One function per method.
#' @param tag_value Optional Character: The tag value.
#' @param env Environment: Where the impl is declared.
#'
#' @return `NULL`, invisibly.
#'
#' @keywords internal
#' @noRd
impl_declare <- function(trait, type, methods, tag_value, env) {
  if (!inherits(trait, "r8_trait")) {
    r8_abort("`trait` is a trait object.", class = "r8_declaration_error")
  }
  if (!inherits(type, "r8_type")) {
    r8_abort("`type` is an R8 type object.", class = "r8_declaration_error")
  }
  t <- unclass(trait)
  type_name <- attr(type, "r8_name")
  namespace <- declaring_namespace(env)
  claim_namespace(namespace, declaring_package(env))
  trait_ns <- sub("::.*", "", t[["name"]])
  type_ns <- sub("::.*", "", type_name)
  if (!namespace %in% c(trait_ns, type_ns)) {
    r8_abort(
      "An impl of `",
      t[["name"]],
      "` for `",
      type_name,
      "` is declared in the namespace of the trait or of the type ",
      "(Rust's orphan rule); this code is in `",
      namespace,
      "`.",
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
  for (m in declared) {
    has_default <- isTRUE(Filter(
      function(x) x[["name"]] == m,
      methods_ir
    )[[1L]][["has_default"]])
    if (is.null(fns[[m]]) && !has_default) {
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
  ir <- list(
    `trait` = t[["name"]],
    target = list(kind = "type", name = type_name),
    methods = as.list(names(fns))
  )
  if (!is.null(tag_value)) {
    ir[["tag_value"]] <- tag_value
  }
  if (inherits(trait, "r8_from_trait")) {
    ir[["type_args"]] <- list(t[["source"]])
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
    register_records(list(entry[["record"]]))
    return(invisible(NULL))
  }
  for (m in names(entry[["fns"]])) {
    generic <- t[["generics"]][[m]]
    fn <- entry[["fns"]][[m]]
    set_impl(t[["name"]], m, entry[["type_name"]], fn)
    if (!inherits(generic, "r8_generic")) {
      if (identical(t[["name"]], "R8::Print")) {
        fn <- print_method(fn)
      } else if (identical(t[["name"]], "R8::Display")) {
        fn <- display_method(fn)
      }
      registerS3method(m, entry[["type_name"]], fn, envir = s3_envir(generic))
    }
  }
  register_records(list(entry[["record"]]))
  invisible(NULL)
} # /R8::register_impl


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
  cat("<trait ", t[["name"]], ">\n", sep = "")
  for (m in trait_ir(t)[["methods"]]) {
    cat(
      "  ",
      m[["name"]],
      "(",
      paste(unlist(m[["params"]]), collapse = ", "),
      ")\n",
      sep = ""
    )
  }
  invisible(x)
} # /R8::print.r8_trait
