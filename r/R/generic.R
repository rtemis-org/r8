# generic.R
# ::R8::
# 2026- EDG rtemis.org

# The R-familiar layer: `generic()` declares a generic, the trait of one
# method named as the generic, and `implement<-` declares its method for one
# type, an impl of that trait. spec: R8/class-system#r-familiar-layer

# The functions of the internal S3 generics of base R: the primitive and
# internal generics, the members of the group generics, and the subset
# operators.
internal_s3_generics <- function() {
  c(
    .S3PrimitiveGenerics,
    .internalGenerics,
    ls(.GenericArgsEnv, all.names = TRUE),
    "[",
    "[[",
    "$",
    "[<-",
    "[[<-",
    "$<-",
    "@",
    "@<-"
  )
} # /R8::internal_s3_generics


# %% s3_generic_info ----
#' The name and package of an S3 generic
#'
#' A closure is an S3 generic when its body calls `UseMethod()`; a primitive
#' or a closure of base R is one when it is an internal generic. The test is
#' by identity: a function bound under another name keeps the name of the
#' generic it is.
#'
#' @param f Function.
#'
#' @return A list with `name` and `package`, or `NULL`.
#'
#' @keywords internal
#' @noRd
s3_generic_info <- function(f) {
  if (!is.function(f)) {
    return(NULL)
  }
  if (is.primitive(f)) {
    name <- sub("^\\.Primitive\\(\"(.*)\"\\)$", "\\1", deparse(f)[[1L]])
    if (name %in% internal_s3_generics()) {
      return(list(name = name, package = "base"))
    }
    return(NULL)
  }
  std <- utils::isS3stdGeneric(f)
  if (isTRUE(std)) {
    env <- environment(f)
    package <- if (isNamespace(env)) getNamespaceName(env) else ""
    return(list(name = names(std), package = unname(package)))
  }
  for (name in .internalGenerics) {
    if (identical(f, get0(name, envir = baseenv(), inherits = FALSE))) {
      return(list(name = name, package = "base"))
    }
  }
  NULL
} # /R8::s3_generic_info


# %% is_s4_generic ----
#' An S4 generic
#'
#' @param f Function.
#'
#' @return Logical.
#'
#' @keywords internal
#' @noRd
is_s4_generic <- function(f) {
  methods::is(f, "genericFunction")
} # /R8::is_s4_generic


# %% generic_formals ----
#' The formal argument names of a function, primitives included
#'
#' @param f Function.
#'
#' @return Character.
#'
#' @keywords internal
#' @noRd
generic_formals <- function(f) {
  if (is.primitive(f)) {
    a <- args(f)
    if (is.null(a)) character() else names(formals(a))
  } else {
    names(formals(f))
  }
} # /R8::generic_formals


# %% check_method_formals ----
#' Abort unless a method's formals fit its generic's
#'
#' A method of a generic without `...` has exactly the generic's formals, in
#' order. A method of a generic with `...` has the generic's formals in
#' order, `...` included, and may add named arguments after them.
#'
#' @param fn Function: The method.
#' @param gen_args Character: The generic's formal argument names.
#' @param label Character: The generic's name, for the message.
#'
#' @return `NULL`, invisibly.
#'
#' @keywords internal
#' @noRd
check_method_formals <- function(fn, gen_args, label) {
  if (!is.function(fn)) {
    r8_abort(
      "The method of `",
      label,
      "` is a function.",
      class = "r8_declaration_error"
    )
  }
  own <- names(formals(fn)) %||% character()
  dots <- "..." %in% gen_args
  fits <- if (dots) {
    identical(own[seq_along(gen_args)], gen_args)
  } else {
    identical(own, gen_args)
  }
  if (!fits) {
    r8_abort(
      "The method of `",
      label,
      if (dots) {
        "` takes the generic's arguments first, `...` included, and may add named arguments: function("
      } else {
        "` takes exactly the generic's arguments: function("
      },
      paste(gen_args, collapse = ", "),
      ").",
      class = "r8_declaration_error"
    )
  }
  invisible(NULL)
} # /R8::check_method_formals


# %% generic ----
#' Declare a generic
#'
#' A generic is a function that dispatches on the R8 type of its first
#' argument to the method that `implement()` declares for that type. A
#' package assigns it to the generic's name, documents it with roxygen2, and
#' exports it. The generic is a trait with one method named as the generic,
#' `ns::describe`, which Rust and the C interface implement as any trait.
#'
#' A replacement generic, `generic("labels<-", c("x", "value"))`, is called
#' as `labels(x) <- value`, which rebinds `x` to the method's result; the
#' original value is unchanged. Its last argument is `value`, and each
#' method returns a value of the type of `x`: for a child type, the child,
#' such as `x` after `x$label <- value`; any other result raises
#' `r8_type_error`. Its trait is `ns::with_labels`, the method's name in the
#' bindings other than R.
#'
#' @param name Character: The generic's name, an R8 name such as
#'   `"describe"`: a letter, then letters, digits, or underscores; or such a
#'   name followed by `<-` for a replacement generic, such as `"labels<-"`.
#'   It is the name the generic is assigned to.
#' @param args Character: The formal argument names, in order. The first is
#'   the value dispatched on. A generic with `...` lets each method add named
#'   arguments after the generic's; a generic without it calls its methods
#'   with exactly its arguments, and a call with another argument is an
#'   error. Removing `...` from a generic breaks the methods that add
#'   arguments.
#' @param description Character: What the generic does.
#'
#' @return `generic()`: The generic, a function of class `r8_generic`. `print()`: `x`, invisibly.
#'
#' @seealso [implement<-] declares a method; [r8_method()] returns one.
#'
#' @examples
#' op <- options(r8.namespace = "models")
#' GBM <- new_struct("GBM", fields = list(n_trees = type_integer(min = 1L)))
#' describe <- generic("describe", "x")
#' implement(describe, GBM) <- function(x) {
#'   paste("Gradient boosting,", x$n_trees, "trees")
#' }
#' describe(GBM(n_trees = 100L))
#'
#' `n_trees<-` <- generic("n_trees<-", c("x", "value"))
#' implement(`n_trees<-`, GBM) <- function(x, value) {
#'   x$n_trees <- value
#'   x
#' }
#' m <- GBM(n_trees = 100L)
#' n_trees(m) <- 200L
#' m$n_trees
#' options(op)
#'
#' @export
generic <- function(name, args = "x", description = "") {
  replacement <- is.character(name) &&
    length(name) == 1L &&
    !is.na(name) &&
    endsWith(name, "<-")
  base <- if (replacement) substr(name, 1L, nchar(name) - 2L) else name
  if (!is_name(base)) {
    r8_abort(
      "A generic's name follows the rule of trait names in new_trait(): ",
      "a letter, then letters, digits, or underscores, such as \"describe\", ",
      "or such a name followed by `<-` for a replacement generic",
      name_reason(base),
      ".",
      class = "r8_declaration_error"
    )
  }
  if (
    !is.character(args) ||
      length(args) == 0L ||
      anyNA(args) ||
      anyDuplicated(args) > 0L ||
      !all(args == "..." | make.names(args) == args)
  ) {
    r8_abort(
      "`args` are the generic's formal argument names, distinct syntactic ",
      "names such as c(\"x\", \"data\").",
      class = "r8_declaration_error"
    )
  }
  if (identical(args[[1L]], "...")) {
    r8_abort(
      "The first argument of a generic is the value it dispatches on, a name.",
      class = "r8_declaration_error"
    )
  }
  if (
    replacement &&
      (length(args) < 2L || !identical(args[[length(args)]], "value"))
  ) {
    r8_abort(
      "A replacement generic takes the value it replaces in and the ",
      "replacement, the last argument `value`: generic(\"",
      name,
      "\", c(\"x\", \"value\")).",
      class = "r8_declaration_error"
    )
  }
  if (name %in% names(RESERVED_METHODS)[!nzchar(RESERVED_METHODS)]) {
    r8_abort(
      "`",
      name,
      "` is reserved for R8 and its standard traits.",
      class = "r8_declaration_error"
    )
  }
  env <- parent.frame()
  existing <- existing_generic(name, env)
  if (!is.null(existing)) {
    if (!is.null(s3_generic_info(existing[["fn"]]))) {
      r8_abort(
        "`",
        name,
        "` is a generic of '",
        existing[["package"]],
        "'; declare its method for a type with implement(",
        if (make.names(name) == name) name else paste0("`", name, "`"),
        ", Type) <- function(",
        paste(generic_formals(existing[["fn"]]), collapse = ", "),
        ") ...",
        class = "r8_declaration_error"
      )
    }
    r8_abort(
      "`",
      name,
      "` is a function of '",
      existing[["package"]],
      "'; name the generic otherwise.",
      class = "r8_declaration_error"
    )
  }
  namespace <- declaring_namespace(env)
  claim_namespace(namespace, declaring_package(env))
  # The trait of a replacement generic `f<-` is named `with_f`, the
  # method's identifier in the other bindings.
  trait_name <- if (replacement) paste0("with_", base) else name
  qname <- paste0(namespace, "::", trait_name)
  check_generic_trait(qname, name)
  formals <- stats::setNames(rep(list(quote(expr = )), length(args)), args)
  g <- make_generic(qname, name, as.function(c(formals, list(NULL))))
  ir <- list(
    name = trait_name,
    description = description,
    methods = list(list(
      name = name,
      description = description,
      params = as.list(args)
    )),
    generic = TRUE
  )
  trait <- with_record(
    structure(
      list(
        name = qname,
        ir = ir,
        generics = stats::setNames(list(g), name),
        defaults = list()
      ),
      class = "r8_trait"
    ),
    namespace
  )
  record_declaration(
    env,
    list(kind = "trait", trait = trait, namespace = namespace)
  )
  g
} # /R8::generic


# %% check_generic_trait ----
#' Refuse a generic whose trait name another generic's trait holds
#'
#' The trait of the generic `f` is `ns::f` and that of `f<-` is
#' `ns::with_f`, so the generics `with_f` and `f<-` would share one trait.
#'
#' @param qname Character: The qualified name of the generic's trait.
#' @param method Character: The generic's name.
#'
#' @return `NULL`, invisibly.
#'
#' @keywords internal
#' @noRd
check_generic_trait <- function(qname, method) {
  t <- .r8_traits[[qname]]
  if (is.null(t)) {
    return(invisible(NULL))
  }
  ir <- trait_ir(t)
  other <- ir[["methods"]][[1L]][["name"]]
  if (isTRUE(ir[["generic"]]) && !identical(other, method)) {
    r8_abort(
      "The generics `",
      method,
      "` and `",
      other,
      "` share the trait `",
      qname,
      "`: the trait of a replacement generic `f<-` is `with_f`; ",
      "name the generic otherwise.",
      class = "r8_declaration_error"
    )
  }
  invisible(NULL)
} # /R8::check_generic_trait


# %% classify_generic ----
#' What `implement()` declares for a generic
#'
#' Classifies a generic by identity with known functions: a generic of R8
#' (from `generic()` or a trait); a generic that a standard trait maps; an S3
#' generic of another package; or a function `implement()` refuses.
#'
#' @param g Function: The generic.
#' @param type_label Character: The type's name, for messages.
#'
#' @return A list: `kind` `"trait"` with `trait` and `method`, or `kind`
#'   `"s3"` with `name`, `package` (`""` for a generic outside a package),
#'   and `fn`, the generic.
#'
#' @keywords internal
#' @noRd
classify_generic <- function(g, type_label = "Type") {
  if (!is.function(g)) {
    r8_abort(
      "implement() takes a generic, written as its name: ",
      "implement(describe, ",
      type_label,
      ") <- function(x) ...",
      class = "r8_declaration_error"
    )
  }
  if (inherits(g, "r8_generic")) {
    trait <- .r8_traits[[attr(g, "r8_trait")]]
    if (is.null(trait)) {
      r8_abort(
        "The trait `",
        attr(g, "r8_trait"),
        "` of this generic is not registered; load its package.",
        class = "r8_declaration_error"
      )
    }
    return(list(kind = "trait", trait = trait, method = attr(g, "r8_method")))
  }
  standard <- list(
    list(fn = base::format, trait = "R8::Display", method = "format"),
    list(fn = utils::str, trait = "R8::Debug", method = "str"),
    list(fn = base::dim, trait = "R8::Dim", method = "dim"),
    list(fn = base::print, trait = "R8::Print", method = "print")
  )
  for (s in standard) {
    if (identical(g, s[["fn"]])) {
      return(list(
        kind = "trait",
        trait = .r8_traits[[s[["trait"]]]],
        method = s[["method"]]
      ))
    }
  }
  refused <- list(
    list(
      fns = list(base::`$`, base::`[[`, base::`@`, base::as.list),
      reason = "R8 reads the fields of a value through it"
    ),
    list(
      fns = list(
        base::`$<-`,
        base::`[[<-`,
        base::`[<-`,
        base::`names<-`,
        base::`length<-`,
        base::`dim<-`,
        base::`levels<-`
      ),
      reason = "R8 validates every change to a value through it"
    ),
    list(
      fns = list(base::`==`, base::`!=`),
      reason = "equality of values is r8_equal(), with which r8_hash() agrees"
    ),
    list(
      fns = list(base::`<`, base::`<=`, base::`>`, base::`>=`),
      reason = paste0(
        "comparisons use the PartialOrd trait; implement(r8_cmp, ",
        type_label,
        ") <- function(x, y) ... defines the order"
      )
    ),
    list(
      fns = c(
        lapply(
          c("+", "-", "*", "/", "^", "%%", "%/%", "&", "|", "!", "%*%"),
          get,
          envir = baseenv()
        ),
        list(methods::Ops, methods::Arith, methods::Compare, methods::Logic)
      ),
      reason = "operators on R8 values are not implemented by types"
    )
  )
  for (r in refused) {
    if (any(vapply(r[["fns"]], identical, logical(1), g))) {
      r8_abort(
        "implement() does not declare this method: ",
        r[["reason"]],
        ".",
        class = "r8_declaration_error"
      )
    }
  }
  if (is_s4_generic(g)) {
    r8_abort(
      "`",
      g@generic,
      "` is an S4 generic; implement() declares methods of S3 generics and ",
      "of R8 generics.",
      class = "r8_declaration_error"
    )
  }
  info <- s3_generic_info(g)
  if (!is.null(info)) {
    return(list(
      kind = "s3",
      name = info[["name"]],
      package = info[["package"]],
      fn = g
    ))
  }
  if (identical(g, r8_convert) || identical(g, r8_converter)) {
    r8_abort(
      "A conversion is a method of the target's converter: as_target <- ",
      "r8_converter(Target), then implement(as_target, ",
      type_label,
      ") <- function(x) ... declares the conversion from ",
      type_label,
      " to Target, as impl(From(source), Target, ...) does.",
      class = "r8_declaration_error"
    )
  }
  if (identical(g, base::nrow) || identical(g, base::ncol)) {
    r8_abort(
      "nrow() and ncol() follow dim(); implement(dim, ",
      type_label,
      ") <- function(x) ... defines them.",
      class = "r8_declaration_error"
    )
  }
  r8_abort(
    "This function is not a generic; declare one with generic(), such as ",
    "describe <- generic(\"describe\", \"x\").",
    class = "r8_declaration_error"
  )
} # /R8::classify_generic


# %% implement<- ----
#' Declare the method of a generic for a type
#'
#' `implement(generic, Type) <- function(x) ...` declares the method that
#' `generic` calls for values of `Type`, and returns `generic` unchanged.
#' `generic` is written as its name. The function has exactly the generic's
#' arguments, or, for a generic with `...`, the generic's arguments first,
#' `...` included, then any named arguments of its own. By the generic:
#'
#' - A generic from `generic()`, or a trait method such as `Fit@fit`: an
#'   impl of its trait.
#' - `format`, `str`, and `dim`: an impl of the standard trait `Display`,
#'   `Debug`, or `Dim`; `r8_cmp`: of `PartialOrd`, which `<`, `<=`, `>`, and
#'   `>=` use.
#' - `print`: an impl of the standard trait `Print`; `print()` calls the
#'   function and returns the value invisibly, and `format()` keeps its own
#'   text. The declaration states this in a message.
#' - Another S3 generic of base R or of an imported package, such as
#'   `summary` or `predict`: an S3 method for the qualified class of `Type`.
#' - The functions through which R8 reads, changes, and compares values
#'   (`$`, `[[`, `@`, `as.list`, `$<-`, `[[<-`, `[<-`, `names<-`,
#'   `length<-`, `dim<-`, `levels<-`, `==`, `!=`), the comparison and other
#'   operators, S4 generics, and functions that are not generics raise
#'   `r8_declaration_error`. For `r8_convert` and `r8_converter`, the error
#'   names the converter's form. A converter from `r8_converter(Target)` is
#'   the generic of the conversions to `Target`: `implement(as_target,
#'   Source)` and `implement(as_target, "character")` declare the conversion
#'   from an R8 type or a class, an impl of `From(source)` for `Target`, and
#'   the method takes exactly `(x)`.
#'
#' An impl is declared in the namespace of its generic or of its type.
#'
#' In a package, `r8_rd()` documents the method in the generic's Rd file.
#'
#' @param generic A generic, written as its name.
#' @param type An R8 type object.
#' @param value Function: The method.
#'
#' @return `generic`, unchanged.
#'
#' @seealso [generic()], [r8_method()].
#'
#' @examples
#' op <- options(r8.namespace = "models")
#' Resampler <- new_struct(
#'   "Resampler",
#'   fields = list(n_resamples = type_integer(min = 1L))
#' )
#' implement(format, Resampler) <- function(x, ...) {
#'   paste("Resampler,", x$n_resamples, "resamples")
#' }
#' implement(summary, Resampler) <- function(object, ...) {
#'   object$n_resamples
#' }
#' r <- Resampler(n_resamples = 10L)
#' format(r)
#' summary(r)
#' options(op)
#'
#' @name implement
#' @export
`implement<-` <- function(generic, type, value) {
  if (inherits(generic, "r8_converter")) {
    declare_conversion(generic, type, value, parent.frame())
    return(generic)
  }
  if (!inherits(type, "r8_type")) {
    r8_abort("`type` is an R8 type object.", class = "r8_declaration_error")
  }
  type_name <- attr(type, "r8_name")
  bare <- sub("^.*::", "", type_name)
  env <- parent.frame()
  target <- classify_generic(generic, bare)
  if (identical(target[["kind"]], "s3")) {
    declare_s3_method(target, type, value, env)
    return(generic)
  }
  t <- unclass(target[["trait"]])
  m <- target[["method"]]
  decl <- Filter(
    function(d) identical(d[["name"]], m),
    trait_ir(t)[["methods"]]
  )
  impl_declare(
    target[["trait"]],
    type,
    stats::setNames(list(value), m),
    NULL,
    env
  )
  if (identical(t[["name"]], "R8::Print")) {
    message(
      "implement(print, ",
      bare,
      ") implements R8's `Print` trait: print() calls this function and ",
      "returns its argument invisibly. format() and the `Display` trait keep ",
      "their own text; implement(format, ",
      bare,
      ") sets it."
    )
  }
  generic
} # /R8::implement<-


# %% declare_s3_method ----
#' Declare an S3 method of another package's generic for an R8 type
#'
#' @param target List: `name` and `package` of the generic.
#' @param type An R8 type object.
#' @param fn Function: The method.
#' @param env Environment: Where the method is declared.
#'
#' @return `NULL`, invisibly.
#'
#' @keywords internal
#' @noRd
declare_s3_method <- function(target, type, fn, env) {
  type_name <- attr(type, "r8_name")
  namespace <- declaring_namespace(env)
  claim_namespace(namespace, declaring_package(env))
  if (!identical(namespace, sub("::.*", "", type_name))) {
    r8_abort(
      "A method of `",
      target[["package"]],
      "::",
      target[["name"]],
      "` for `",
      type_name,
      "` is declared in the namespace of the type (Rust's orphan rule); ",
      "this code is in `",
      namespace,
      "`.",
      class = "r8_declaration_error"
    )
  }
  check_method_formals(fn, generic_formals(target[["fn"]]), target[["name"]])
  entry <- list(
    kind = "s3method",
    name = target[["name"]],
    package = target[["package"]],
    type_name = type_name,
    fn = fn,
    namespace = namespace
  )
  if (!nzchar(target[["package"]])) {
    # A generic outside a package, found in its own environment; such code
    # is not recorded for r8_register().
    entry[["envir"]] <- environment(target[["fn"]])
  }
  register_s3_method(entry)
  record_declaration(env, entry)
  invisible(NULL)
} # /R8::declare_s3_method


# %% register_s3_method ----
#' Register an S3 method of another package's generic
#'
#' @param entry List: `name`, `package`, `type_name`, and `fn`, and `envir`
#'   when `package` is `""`.
#'
#' @return `NULL`, invisibly.
#'
#' @keywords internal
#' @noRd
register_s3_method <- function(entry) {
  trait_name <- paste0(entry[["package"]], "::", entry[["name"]])
  put_impl(trait_name, entry[["name"]], entry[["type_name"]], entry[["fn"]])
  # The S3 method is the stamped impl, from which r8_next_impl() reads its
  # provider; a replacement method's result is checked.
  fn <- .r8_impls[[paste(
    trait_name,
    entry[["name"]],
    entry[["type_name"]],
    sep = "|"
  )]]
  if (endsWith(entry[["name"]], "<-")) {
    fn <- replacement_method(fn, paste(trait_name, entry[["name"]], sep = "|"))
  }
  if (nzchar(entry[["package"]])) {
    registerS3method(
      entry[["name"]],
      entry[["type_name"]],
      fn,
      envir = asNamespace(entry[["package"]])
    )
  } else {
    # UseMethod() finds the methods of a generic outside a package in the S3
    # table of the generic's top environment, where registerS3method() writes
    # for a generic of the global environment.
    top <- topenv(entry[["envir"]])
    table <- get0(".__S3MethodsTable__.", envir = top, inherits = FALSE)
    if (is.null(table)) {
      table <- new.env(hash = TRUE, parent = baseenv())
      assign(".__S3MethodsTable__.", table, envir = top)
    }
    assign(
      paste0(entry[["name"]], ".", entry[["type_name"]]),
      fn,
      envir = table
    )
  }
  invisible(NULL)
} # /R8::register_s3_method


# %% r8_method ----
#' The method of a generic for a type
#'
#' Returns the function that `implement()` or `impl()` declared for `type`,
#' for tests and inspection.
#'
#' @param generic A generic, as `implement()` takes it.
#' @param type An R8 type object.
#'
#' @return Function. A generic without a method for `type` raises
#'   `r8_dispatch_error`.
#'
#' @examples
#' op <- options(r8.namespace = "models")
#' GBM <- new_struct("GBM", fields = list(n_trees = type_integer(min = 1L)))
#' describe <- generic("describe", "x")
#' implement(describe, GBM) <- function(x) "Gradient boosting"
#' r8_method(describe, GBM)
#' options(op)
#'
#' @export
r8_method <- function(generic, type) {
  if (inherits(generic, "r8_converter")) {
    source <- if (inherits(type, "r8_type")) attr(type, "r8_name") else type
    target <- attr(converter_target(generic), "r8_name")
    fn <- .r8_impls[[paste("R8::From", source, target, sep = "|")]]
    if (is.null(fn)) {
      r8_abort(
        "`",
        target,
        "` has no conversion from `",
        source,
        "`.",
        class = "r8_dispatch_error"
      )
    }
    attr(fn, "r8_provider") <- NULL
    return(fn)
  }
  if (!inherits(type, "r8_type")) {
    r8_abort("`type` is an R8 type object.", class = "r8_dispatch_error")
  }
  type_name <- attr(type, "r8_name")
  target <- classify_generic(generic, sub("^.*::", "", type_name))
  key <- if (identical(target[["kind"]], "s3")) {
    paste(
      paste0(target[["package"]], "::", target[["name"]]),
      target[["name"]],
      type_name,
      sep = "|"
    )
  } else {
    paste(
      unclass(target[["trait"]])[["name"]],
      target[["method"]],
      type_name,
      sep = "|"
    )
  }
  fn <- .r8_impls[[key]]
  if (is.null(fn)) {
    r8_abort(
      "`",
      type_name,
      "` has no method of this generic.",
      class = "r8_dispatch_error"
    )
  }
  attr(fn, "r8_provider") <- NULL
  fn
} # /R8::r8_method


# %% usage_line ----
#' The call of a generic, as its usage
#'
#' A replacement generic `f<-` is called as `f(x) <- value`.
#'
#' @param method Character: The method's name.
#' @param args Character: Its formal argument names.
#'
#' @return Character: The usage.
#'
#' @keywords internal
#' @noRd
usage_line <- function(method, args) {
  if (endsWith(method, "<-") && length(args) >= 2L) {
    base <- substr(method, 1L, nchar(method) - 2L)
    return(paste0(
      base,
      "(",
      paste(args[-length(args)], collapse = ", "),
      ") <- ",
      args[[length(args)]]
    ))
  }
  paste0(method, "(", paste(args, collapse = ", "), ")")
} # /R8::usage_line


# %% print.r8_generic ----
#' @rdname generic
#' @param x A generic of class `r8_generic`.
#' @param ... Not used.
#' @export
print.r8_generic <- function(x, ...) {
  trait <- attr(x, "r8_trait")
  method <- attr(x, "r8_method")
  t <- .r8_traits[[trait]]
  is_generic <- !is.null(t) && isTRUE(trait_ir(t)[["generic"]])
  cat(
    "<r8 generic ",
    if (is_generic) {
      paste0(sub("::.*$", "", trait), "::", method)
    } else {
      paste0(trait, "@", method)
    },
    ">\n",
    sep = ""
  )
  cat(usage_line(method, names(formals(x))), "\n", sep = "")
  prefix <- paste(trait, method, "", sep = "|")
  keys <- ls(.r8_impls, all.names = TRUE)
  types <- substring(keys[startsWith(keys, prefix)], nchar(prefix) + 1L)
  types <- types[nzchar(types)]
  if (length(types) > 0L) {
    cat("Methods for: ", paste(sort(types), collapse = ", "), "\n", sep = "")
  }
  invisible(x)
} # /R8::print.r8_generic
