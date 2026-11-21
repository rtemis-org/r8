# traits.R
# ::R8::
# 2026- EDG rtemis.org

# Impls of R8 traits written in R, by "trait|method|type". A generic made by
# a trait looks up the impl for the qualified type of its first argument.
.r8_impls <- new.env(parent = emptyenv())

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
  body <- bquote({
    impl <- r8_find_impl(.(trait_name), .(method), .(first))
    .(as.call(c(list(quote(impl)), call_args)))
  })
  g <- as.function(c(formals(fn), body), envir = asNamespace("R8"))
  structure(
    g,
    class = c("r8_generic", "function"),
    r8_trait = trait_name,
    r8_method = method
  )
} # /R8::make_generic


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
  if (!is.null(impl)) {
    return(impl)
  }
  default <- .r8_impls[[paste(trait_name, method, "", sep = "|")]]
  if (!is.null(default)) {
    return(default)
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
  trait <- build_trait(name, methods, description, tag, namespace, env)
  register_trait(trait, namespace)
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
  problems <- r8_declare_trait(t[["ir"]], namespace)
  if (length(problems[["code"]]) > 0L) {
    r8_abort(
      "Declaration of `",
      t[["name"]],
      "` failed:\n",
      paste0(
        "- ",
        problems[["message"]],
        " [",
        problems[["code"]],
        "]",
        collapse = "\n"
      ),
      class = "r8_declaration_error"
    )
  }
  assign(t[["name"]], trait, envir = .r8_traits)
  for (m in names(t[["defaults"]])) {
    assign(
      paste(t[["name"]], m, "", sep = "|"),
      t[["defaults"]][[m]],
      envir = .r8_impls
    )
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
  declared <- vapply(
    t[["ir"]][["methods"]],
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
      t[["ir"]][["methods"]]
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
  entry <- list(
    kind = "impl",
    trait = trait,
    type_name = type_name,
    fns = fns,
    ir = ir,
    namespace = namespace
  )
  if (inherits(trait, "r8_from_trait")) {
    entry[["source"]] <- t[["source"]]
    entry[["ir"]][["type_args"]] <- list(t[["source"]])
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
  t <- unclass(entry[["trait"]])
  if (!is.null(entry[["source"]])) {
    assign(
      paste("R8::From", entry[["source"]], entry[["type_name"]], sep = "|"),
      entry[["fns"]][["from"]],
      envir = .r8_impls
    )
    r8_declare_impl(entry[["ir"]], entry[["namespace"]])
    return(invisible(NULL))
  }
  for (m in names(entry[["fns"]])) {
    generic <- t[["generics"]][[m]]
    fn <- entry[["fns"]][[m]]
    assign(
      paste(t[["name"]], m, entry[["type_name"]], sep = "|"),
      fn,
      envir = .r8_impls
    )
    if (!inherits(generic, "r8_generic")) {
      if (identical(t[["name"]], "R8::Print")) {
        fn <- print_method(fn)
      } else if (identical(t[["name"]], "R8::Display")) {
        fn <- display_method(fn)
      }
      registerS3method(m, entry[["type_name"]], fn, envir = s3_envir(generic))
    }
  }
  r8_declare_impl(entry[["ir"]], entry[["namespace"]])
  invisible(NULL)
} # /R8::register_impl


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
  for (m in t[["ir"]][["methods"]]) {
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
