# params.R
# ::R8::
# 2026- EDG rtemis.org

# The parameters of a generic or trait method. `params()` reads its call
# unevaluated, so a default stays an R expression, evaluated at each call
# that omits the argument; `arg()` and its shorthands, one per type
# constructor, declare a parameter's type and default.
# spec: R8/class-system#parameters

#' @include types.R
NULL


# %% arg_default ----
#' The value of an argument that a call omits
#'
#' The body of `arg()` and its shorthands, for a function that R calls
#' without a declaration, such as `function(x = arg(type_double())) x`: the
#' default, or R's own error for a missing argument without one.
#'
#' @param default The default, evaluated in the calling function's frame.
#' @param constructor The type constructor of the shorthand, or `arg`; its
#'   name identifies the function (`arg_constructor()`) and is never
#'   evaluated.
#'
#' @return `default`.
#'
#' @keywords internal
#' @noRd
arg_default <- function(default, constructor) {
  if (!missing(default)) {
    return(default)
  }
  # The frames of the shorthand and of the function whose parameter it
  # declares. sys.parent(n) counts contexts, which a forced promise adds to;
  # parent.frame(n) counts callers.
  frames <- sys.frames()
  at <- function(env) {
    Position(function(f) identical(f, env), frames, right = TRUE)
  }
  frame <- parent.frame(2L)
  outer <- at(frame)
  if (!is.na(outer)) {
    given <- sys.call(at(parent.frame(1L)))
    # A call from code that keeps its source carries its reference.
    attr(given, "srcref") <- NULL
    fmls <- formals(sys.function(outer))
    for (n in names(fmls)) {
      if (
        identical(fmls[[n]], given) &&
          eval(as.call(list(quote(missing), as.name(n))), frame)
      ) {
        f <- as.function(c(
          stats::setNames(list(quote(expr = )), n),
          list(as.name(n))
        ))
        cond <- tryCatch(f(), error = identity)
        cond[["call"]] <- sys.call(outer)
        stop(cond)
      }
    }
  }
  r8_abort(
    "arg() and its shorthands declare a parameter in params(), such as ",
    "params(x, n = arg_integer(min = 0L)).",
    class = "r8_declaration_error"
  )
} # /R8::arg_default


# %% arg_shorthand ----
#' The shorthand of `arg()` for a type constructor
#'
#' @param constructor Character: The constructor's name.
#'
#' @return Function: The constructor's formals, then `default` and
#'   `description`.
#'
#' @keywords internal
#' @noRd
arg_shorthand <- function(constructor) {
  f <- get(constructor, envir = topenv(), mode = "function")
  as.function(
    c(
      formals(f),
      alist(default = , description = ""),
      as.call(list(
        quote(arg_default),
        quote(default),
        as.name(constructor)
      ))
    ),
    envir = environment(f)
  )
} # /R8::arg_shorthand


# %% arg ----
#' Declare a parameter's type and default
#'
#' Inside [params()], `arg(type, default)` gives a parameter of a generic or
#' trait method its type and, when `default` is given, its default, an R
#' expression evaluated at each call that omits the argument. Each type
#' constructor has a shorthand, with the constructor's arguments and then a
#' named `default`: `arg_double(min = 0, default = 1)` is
#' `arg(type_double(min = 0), 1)`.
#'
#' A parameter without a default is required. `arg_option(type)` has the
#' default `NULL`: the argument may be omitted and is then absent;
#' `arg(option(type))` is required and may be `NULL`. `arg(default = expr)`
#' gives a parameter without a type a default that is itself a type, such as
#' `arg(default = GBM)`.
#'
#' `description` is named after `default`: `arg_integer(min = 1L, default =
#' 10L, description = "The number of folds.")`. `arg(description = "...")`
#' alone describes a parameter without a type or a default.
#'
#' Each call of the generic checks and converts its typed arguments
#' ([generic()]).
#'
#' A type means the same in a parameter as in a field: `arg(Range)` admits a
#' value of exactly `Range`, `arg(subtype(Range))` also the structs that
#' extend it, `arg(dyn(Fit))` any implementor of the trait `Fit`, and
#' `arg(host_class("data.frame"))` an object of that class.
#'
#' @param type An `r8_field_type` or an R8 type: The parameter's type; for
#'   `arg_vec()`, `arg_option()`, `arg_btree_map()`, `arg_index_map()`,
#'   `arg_matrix()`, `arg_array()`, and `arg_sparse_matrix()`, the type of
#'   the elements or of a present value.
#' @param default Optional expression: The default.
#' @param description Character: What the parameter is, one string, which the
#'   declaration keeps: [r8_schema()] writes it, and the roclet [r8_rd()]
#'   writes it as the parameter's `@param`.
#' @param min,max,exclusive_min,exclusive_max,values Optional Numeric: The
#'   bounds and admitted values, as [type_integer()] and [type_double()] take
#'   them; `values` of `arg_character()` as [type_character()] takes it.
#' @param infinite,nan Logical: As [type_double()] takes them.
#' @param pattern Optional Character: As [type_character()] takes it.
#' @param levels Optional Character: As [type_factor()] takes it.
#' @param ordered Logical: As [type_factor()] takes it.
#' @param nrow,ncol Optional Integer: As [type_matrix()] takes them.
#' @param of Optional R8 struct type: As [type_matrix()] and [type_table()]
#'   take it.
#' @param dim Optional Integer: As [type_array()] takes it.
#' @param columns Named list: As [type_table()] takes it.
#' @param open Logical: As [type_table()] takes it.
#' @param rules List: As [type_table()] takes it.
#' @param min_length,max_length Optional Integer: As [vec()] takes them.
#' @param unique Logical: As [vec()] takes it.
#' @param keys Optional `r8_field_type`: As [btree_map()] takes it.
#'
#' @return In [params()], a parameter's declaration. In a function that R
#'   calls without a declaration, such as `function(x = arg_double()) x`,
#'   the default, or R's error for a missing argument.
#'
#' @seealso [params()].
#'
#' @examples
#' params(x, verbosity = arg_integer(min = 0L, default = 1L))
#' params(x, n = arg_integer(default = length(x)), weights = arg_option(vec(type_double())))
#'
#' @export
arg <- function(type, default, description = "") {
  arg_default(default, arg)
} # /R8::arg


#' @rdname arg
#' @export
arg_logical <- arg_shorthand("type_logical")

#' @rdname arg
#' @export
arg_integer <- arg_shorthand("type_integer")

#' @rdname arg
#' @export
arg_double <- arg_shorthand("type_double")

#' @rdname arg
#' @export
arg_character <- arg_shorthand("type_character")

#' @rdname arg
#' @export
arg_raw <- arg_shorthand("type_raw")

#' @rdname arg
#' @export
arg_date <- arg_shorthand("type_date")

#' @rdname arg
#' @export
arg_posixct <- arg_shorthand("type_posixct")

#' @rdname arg
#' @export
arg_factor <- arg_shorthand("type_factor")

#' @rdname arg
#' @export
arg_json <- arg_shorthand("type_json")

#' @rdname arg
#' @export
arg_matrix <- arg_shorthand("type_matrix")

#' @rdname arg
#' @export
arg_array <- arg_shorthand("type_array")

#' @rdname arg
#' @export
arg_sparse_matrix <- arg_shorthand("type_sparse_matrix")

#' @rdname arg
#' @export
arg_table <- arg_shorthand("type_table")

#' @rdname arg
#' @export
arg_vec <- arg_shorthand("vec")

#' @rdname arg
#' @export
arg_option <- function(type, default = NULL, description = "") {
  arg_default(default, option)
}

#' @rdname arg
#' @export
arg_btree_map <- arg_shorthand("btree_map")

#' @rdname arg
#' @export
arg_index_map <- arg_shorthand("index_map")


# %% arg_constructor ----
#' The type constructor of `arg()` or a shorthand
#'
#' Recognizes the function itself, under any name.
#'
#' @param f An object.
#'
#' @return Character: The constructor's name, `"arg"` for `arg()`; `NULL` for
#'   any other object.
#'
#' @keywords internal
#' @noRd
arg_constructor <- function(f) {
  if (!is.function(f) || !identical(environment(f), topenv())) {
    return(NULL)
  }
  b <- body(f)
  if (is.call(b) && identical(b[[1L]], quote(`{`)) && length(b) == 2L) {
    b <- b[[2L]]
  }
  if (is.call(b) && identical(b[[1L]], quote(arg_default))) {
    as.character(b[[3L]])
  }
} # /R8::arg_constructor


# The functions that make a field type, and `field()`.
TYPE_CONSTRUCTORS <- c(
  "type_logical",
  "type_integer",
  "type_double",
  "type_character",
  "type_raw",
  "type_date",
  "type_posixct",
  "type_factor",
  "type_json",
  "type_matrix",
  "type_array",
  "type_sparse_matrix",
  "type_table",
  "vec",
  "option",
  "btree_map",
  "index_map",
  "dyn",
  "subtype",
  "host_class",
  "field"
)


# %% called_function ----
#' The function that a call names
#'
#' @param head The call's first element.
#' @param env Environment: Where the call is written.
#'
#' @return Function, or `NULL`.
#'
#' @keywords internal
#' @noRd
called_function <- function(head, env) {
  if (is.function(head)) {
    return(head)
  }
  if (is.name(head)) {
    return(get0(as.character(head), envir = env, mode = "function"))
  }
  if (
    is.call(head) &&
      (identical(head[[1L]], quote(`::`)) ||
        identical(head[[1L]], quote(`:::`)))
  ) {
    return(tryCatch(eval(head, env), error = function(e) NULL))
  }
  NULL
} # /R8::called_function


# %% calls_arg ----
#' Whether an expression calls `arg()` or a shorthand
#'
#' @param e An expression.
#' @param env Environment: Where it is written.
#'
#' @return Logical.
#'
#' @keywords internal
#' @noRd
calls_arg <- function(e, env) {
  if (!is.call(e)) {
    return(FALSE)
  }
  if (!is.null(arg_constructor(called_function(e[[1L]], env)))) {
    return(TRUE)
  }
  for (i in seq_along(e)[-1L]) {
    if (is.call(e[[i]]) && calls_arg(e[[i]], env)) {
      return(TRUE)
    }
  }
  FALSE
} # /R8::calls_arg


# %% is_visible_type ----
#' Whether a default expression is visibly a type
#'
#' A call of a type constructor, or the name of a type, a field type, or a
#' trait.
#'
#' @param e The default expression.
#' @param env Environment: Where it is written.
#' @param params Character: The parameters' names, which a default reads
#'   before the environment's.
#'
#' @return Logical.
#'
#' @keywords internal
#' @noRd
is_visible_type <- function(e, env, params) {
  x <- e
  if (is.call(e)) {
    f <- called_function(e[[1L]], env)
    if (!is.function(f) || !identical(environment(f), topenv())) {
      return(FALSE)
    }
    ns <- topenv()
    return(any(vapply(
      TYPE_CONSTRUCTORS,
      function(n) identical(f, get(n, envir = ns, inherits = FALSE)),
      logical(1)
    )))
  }
  if (is.name(e)) {
    if (as.character(e) %in% params) {
      return(FALSE)
    }
    x <- get0(as.character(e), envir = env)
  }
  inherits(x, c("r8_type", "r8_field_type", "r8_trait"))
} # /R8::is_visible_type


# %% constant_value ----
#' The value of a syntactic constant
#'
#' A number, a string, a logical, `NULL`, a negated number, or `c()` of
#' unnamed constants, with `-` and `c` those of base R.
#'
#' @param e An expression.
#' @param env Environment: Where it is written.
#'
#' @return A list of the value, or `NULL` for any other expression.
#'
#' @keywords internal
#' @noRd
constant_value <- function(e, env) {
  if (is.null(e)) {
    return(list(NULL))
  }
  is_constant <- function(x) {
    (is.numeric(x) || is.logical(x) || is.character(x)) &&
      length(x) == 1L &&
      is.null(attributes(x))
  }
  is_signed <- function(x) {
    is.call(x) &&
      length(x) == 2L &&
      identical(called_function(x[[1L]], env), base::`-`) &&
      is.numeric(x[[2L]]) &&
      is_constant(x[[2L]])
  }
  if (is_constant(e) || is_signed(e)) {
    return(list(eval(e, baseenv())))
  }
  if (
    is.call(e) &&
      length(e) > 1L &&
      is.null(names(e)) &&
      identical(called_function(e[[1L]], env), base::c) &&
      all(vapply(
        as.list(e)[-1L],
        function(x) is_constant(x) || is_signed(x),
        logical(1)
      ))
  ) {
    return(list(eval(e, baseenv())))
  }
  NULL
} # /R8::constant_value


# %% param_abort ----
#' Refuse a parameter
#'
#' @param name Character: The parameter's name.
#' @param ... Character: The reason.
#'
#' @return Does not return.
#'
#' @keywords internal
#' @noRd
param_abort <- function(name, ...) {
  r8_abort(
    "Parameter `",
    name,
    "`: ",
    ...,
    class = "r8_declaration_error"
  )
} # /R8::param_abort


# %% params ----
#' Declare the parameters of a generic or trait method
#'
#' `params()` reads its call unevaluated. Each argument declares one
#' parameter, in order:
#'
#' - `x`, a bare name: a parameter without a type or a default.
#' - `n = arg_integer(min = 0L, default = 1L)`, a call of [arg()] or one of
#'   its shorthands: a typed parameter.
#' - `n = length(x)`, any other expression: a parameter without a type whose
#'   default is the expression.
#' - `"..."`, a string: the dots, through which methods add arguments of
#'   their own.
#'
#' A default is evaluated at each call that omits the argument, in the
#' generic's frame, where the other parameters are visible, and then in the
#' environment that calls `params()`; nothing but the types is evaluated when
#' `params()` runs. A constant default, such as `1L`, `"a"`, or `c(1, 2)`, of
#' a typed parameter is checked against its type at the declaration and
#' belongs to the declaration that other languages read; a call from another
#' language supplies an argument whose default is any other R expression.
#'
#' @param ... The parameters. The first is a name, the value that the generic
#'   or method dispatches on.
#'
#' @return An `r8_params`, for [generic()] and [trait_method()].
#'
#' @seealso [arg()], [generic()], [trait_method()].
#'
#' @examples
#' params(x)
#' params(x, y, n = arg_integer(min = 1L, default = 10L), "...")
#' params(x, n = arg_integer(default = length(x)))
#'
#' @export
params <- function(...) {
  read_params(as.list(sys.call())[-1L], parent.frame())
} # /R8::params


# %% read_params ----
#' The parameters that the arguments of `params()` declare
#'
#' @param exprs List: The unevaluated arguments.
#' @param env Environment: Where `params()` is called.
#' @param generic Logical: The parameters are a generic's, whose first is
#'   dispatched on; a function's may be none, and begin with `...`.
#'
#' @return An `r8_params`.
#'
#' @keywords internal
#' @noRd
read_params <- function(exprs, env, generic = TRUE) {
  n <- length(exprs)
  if (n == 0L && generic) {
    r8_abort(
      "params() declares at least one parameter, the value dispatched on: ",
      "params(x).",
      class = "r8_declaration_error"
    )
  }
  given <- names(exprs) %||% rep("", n)
  nms <- character(n)
  for (i in seq_len(n)) {
    nms[[i]] <- param_name(given[[i]], exprs, i, env)
  }
  if (generic && identical(nms[[1L]], "...")) {
    r8_abort(
      "The first parameter is the value dispatched on, a name; `...` ",
      "follows it.",
      class = "r8_declaration_error"
    )
  }
  if (anyDuplicated(nms) > 0L) {
    r8_abort(
      "Parameter `",
      nms[[anyDuplicated(nms)]],
      "` is declared twice.",
      class = "r8_declaration_error"
    )
  }
  fmls <- stats::setNames(rep(list(quote(expr = )), n), nms)
  types <- vector("list", n)
  constants <- vector("list", n)
  texts <- character(n)
  descriptions <- character(n)
  for (i in which(nzchar(given))) {
    p <- read_param(nms[[i]], exprs[[i]], env, nms)
    if (!is.null(p[["type"]])) {
      types[[i]] <- p[["type"]]
    }
    descriptions[[i]] <- p[["description"]] %||% ""
    if (is.null(p[["default"]])) {
      next
    }
    e <- p[["default"]][[1L]]
    if (is_visible_type(e, env, nms) && !isTRUE(p[["type_default"]])) {
      param_abort(
        nms[[i]],
        "its default `",
        deparse1(e),
        "` is a type; a type is given as ",
        nms[[i]],
        " = arg(",
        deparse1(e),
        "), and a default that is a type as ",
        nms[[i]],
        " = arg(default = ",
        deparse1(e),
        ")."
      )
    }
    fmls[i] <- list(e)
    texts[[i]] <- deparse1(e, collapse = "\n")
    constant <- constant_value(e, env)
    if (!is.null(constant)) {
      # The formal holds the value, as the IR does.
      fmls[i] <- constant
      constants[[i]] <- constant
    }
  }
  structure(
    list(
      names = nms,
      formals = fmls,
      types = types,
      constants = constants,
      texts = texts,
      descriptions = descriptions,
      env = env
    ),
    class = "r8_params"
  )
} # /R8::read_params


# %% param_name ----
#' The name of a parameter of `params()`
#'
#' @param given Character: The argument's name, or `""`.
#' @param exprs List: The unevaluated arguments.
#' @param i Integer: The argument's position.
#' @param env Environment: Where `params()` is called.
#'
#' @return Character.
#'
#' @keywords internal
#' @noRd
param_name <- function(given, exprs, i, env) {
  empty <- identical(exprs[[i]], quote(expr = ))
  if (nzchar(given)) {
    if (identical(given, "...")) {
      r8_abort(
        "`...` takes no type and no default; params() takes it as the ",
        "string \"...\".",
        class = "r8_declaration_error"
      )
    }
    if (startsWith(given, ".r8_")) {
      r8_abort(
        "`",
        given,
        "` is reserved: the names that begin with `.r8_` are those of R8 in ",
        "a generic's frame.",
        class = "r8_declaration_error"
      )
    }
    if (make.names(given) != given) {
      r8_abort(
        "`",
        given,
        "` is not a parameter's name, a syntactic R name.",
        class = "r8_declaration_error"
      )
    }
    if (empty) {
      param_abort(
        given,
        "a parameter without a type or a default is its bare name, params(",
        given,
        ")."
      )
    }
    return(given)
  }
  if (empty) {
    r8_abort(
      "Argument ",
      i,
      " of params() is empty.",
      class = "r8_declaration_error"
    )
  }
  e <- exprs[[i]]
  if (identical(e, "...")) {
    return("...")
  }
  if (identical(e, quote(...))) {
    r8_abort(
      "params() takes `...` as the string \"...\".",
      class = "r8_declaration_error"
    )
  }
  if (is.name(e)) {
    return(param_name(as.character(e), list(TRUE), 1L, env))
  }
  if (is.character(e) && length(e) == 1L && !is.na(e)) {
    r8_abort(
      "params() takes a parameter's name bare, params(",
      e,
      "), and \"...\" as a string.",
      class = "r8_declaration_error"
    )
  }
  r8_abort(
    "Argument ",
    i,
    " of params(), `",
    deparse1(e),
    "`, has no name; a parameter with a type or a default is named, such as ",
    if (calls_arg(e, env)) paste0("n = ", deparse1(e)) else "n = length(x)",
    ".",
    class = "r8_declaration_error"
  )
} # /R8::param_name


# %% read_param ----
#' A named parameter of `params()`
#'
#' @param name Character: The parameter's name.
#' @param e Its expression.
#' @param env Environment: Where `params()` is called.
#' @param params Character: The parameters' names.
#'
#' @return List: `type`, a field type or an R8 type object, or `NULL`;
#'   `default`, a list of the default expression, or `NULL`; and
#'   `type_default`, `TRUE` for `arg(default = )` without a type, whose
#'   default may be a type; and `description`, a string.
#'
#' @keywords internal
#' @noRd
read_param <- function(name, e, env, params) {
  constructor <- if (is.call(e)) {
    arg_constructor(called_function(e[[1L]], env))
  }
  if (is.null(constructor)) {
    if (calls_arg(e, env)) {
      param_abort(
        name,
        "arg() and its shorthands stand alone as a parameter's declaration, ",
        "not inside `",
        deparse1(e),
        "`."
      )
    }
    return(list(default = list(e)))
  }
  f <- called_function(e[[1L]], env)
  matched <- tryCatch(
    as.list(match.call(f, e, envir = env))[-1L],
    error = function(cond) param_abort(name, conditionMessage(cond))
  )
  given <- vapply(
    seq_along(matched),
    function(i) !identical(matched[[i]], quote(expr = )),
    logical(1)
  )
  matched <- matched[given]
  default <- if ("default" %in% names(matched)) {
    matched["default"]
  } else if (!identical(formals(f)[["default"]], quote(expr = ))) {
    unname(formals(f)["default"])
  }
  matched <- matched[names(matched) != "default"]
  description <- if ("description" %in% names(matched)) {
    param_description(name, matched[["description"]], env)
  } else {
    ""
  }
  matched <- matched[names(matched) != "description"]
  if (!is.null(default)) {
    default <- list(default[[1L]])
    if (calls_arg(default[[1L]], env)) {
      param_abort(
        name,
        "its default calls arg(), which declares a parameter."
      )
    }
  }
  if (identical(constructor, "arg") && length(matched) == 0L) {
    if (is.null(default) && !nzchar(description)) {
      param_abort(
        name,
        "arg() gives a type, a default, or a description; a parameter without ",
        "them is its bare name, params(",
        name,
        ")."
      )
    }
    return(list(
      default = default,
      type_default = TRUE,
      description = description
    ))
  }
  for (i in seq_along(matched)) {
    if (is.call(matched[[i]]) && calls_arg(matched[[i]], env)) {
      param_abort(
        name,
        "`",
        deparse1(matched[[i]]),
        "` declares a parameter; a type is type_double(), vec(), and so on."
      )
    }
  }
  type <- tryCatch(
    if (identical(constructor, "arg")) {
      eval(matched[["type"]], env)
    } else {
      eval(
        as.call(c(list(get(constructor, envir = topenv())), matched)),
        env
      )
    },
    error = function(cond) param_abort(name, conditionMessage(cond))
  )
  check_param_type(name, type)
  list(type = type, default = default, description = description)
} # /R8::read_param


# %% param_description ----
#' The description of a parameter
#'
#' @param name Character: The parameter's name.
#' @param e The expression of `description`.
#' @param env Environment: Where the parameter is declared.
#'
#' @return Character: One string.
#'
#' @keywords internal
#' @noRd
param_description <- function(name, e, env) {
  text <- tryCatch(
    eval(e, env),
    error = function(cond) param_abort(name, conditionMessage(cond))
  )
  if (!is.character(text) || length(text) != 1L || is.na(text)) {
    param_abort(
      name,
      "its description is one string, such as description = \"The number ",
      "of folds.\"."
    )
  }
  enc2utf8(text)
} # /R8::param_description


# %% check_param_type ----
#' Refuse a parameter's type that is not one
#'
#' @param name Character: The parameter's name.
#' @param type The type.
#'
#' @return `NULL`, invisibly.
#'
#' @keywords internal
#' @noRd
check_param_type <- function(name, type) {
  if (inherits(type, "r8_type")) {
    ensure_registered(type)
  } else if (inherits(type, "r8_trait")) {
    param_abort(
      name,
      "a value of any implementor of a trait is dyn() of the trait: arg(dyn(",
      sub("^.*::", "", unclass(type)[["name"]]),
      "))."
    )
  } else if (!inherits(type, "r8_field_type")) {
    param_abort(
      name,
      "its type is a field type, such as type_double(), subtype(), dyn(), or ",
      "host_class(), or an R8 type."
    )
  }
  invisible(NULL)
} # /R8::check_param_type


# %% params_ir ----
#' The IR parameters of a generic or trait method
#'
#' Parameters without types and defaults are written in R, so the standard
#' traits are built when R8 is installed, before its shared object loads.
#'
#' @param p An `r8_params`.
#' @param dispatch Character: The dispatched parameters' names.
#'
#' @return List: The IR `params`.
#'
#' @keywords internal
#' @noRd
params_ir <- function(p, dispatch) {
  descriptions <- p[["descriptions"]] %||% character(length(p[["names"]]))
  if (
    !any(nzchar(p[["texts"]])) &&
      !any(nzchar(descriptions)) &&
      all(vapply(p[["types"]], is.null, NA))
  ) {
    return(dispatch_params(p[["names"]], dispatch))
  }
  declared(r8_params_ir(
    p[["names"]],
    p[["types"]],
    p[["constants"]],
    p[["texts"]],
    descriptions,
    dispatch
  ))
} # /R8::params_ir


# %% stored_defaults ----
#' Parameters with their constant defaults in stored form
#'
#' A constant default of a typed parameter, which `params_ir()` has checked,
#' becomes the formal's default in its stored form, such as `1L` for `1` and
#' `type_integer()`, which a call then passes without a conversion.
#'
#' @param p An `r8_params`.
#'
#' @return An `r8_params`.
#'
#' @keywords internal
#' @noRd
stored_defaults <- function(p) {
  for (i in seq_along(p[["names"]])) {
    if (!is.null(p[["types"]][[i]]) && !is.null(p[["constants"]][[i]])) {
      value <- p[["constants"]][[i]][[1L]]
      if (!is.null(value)) {
        p[["formals"]][i] <- list(r8_stored_form(value, p[["types"]][[i]]))
      }
    }
  }
  p
} # /R8::stored_defaults


# %% return_type_ir ----
#' The IR of a return type
#'
#' @param x A field type, an R8 type object, or `"Self"`; `NULL` for none.
#' @param self Logical: `"Self"` is admitted.
#'
#' @return The IR, or `NULL`.
#'
#' @keywords internal
#' @noRd
return_type_ir <- function(x, self) {
  if (is.null(x) || (self && identical(x, "Self"))) {
    return(x)
  }
  if (inherits(x, "r8_type")) {
    ensure_registered(x)
    return(list(kind = "named", name = attr(x, "r8_name")))
  }
  if (inherits(x, "r8_field_type")) {
    return(unclass(x))
  }
  r8_abort(
    "A return type is a field type, such as type_double(), subtype(), dyn(), ",
    "or host_class(), an R8 type",
    if (self) ", or \"Self\", the type of the first dispatched argument",
    ".",
    class = "r8_declaration_error"
  )
} # /R8::return_type_ir


# %% as_params ----
#' The parameters given to `generic()` or `trait_method()`
#'
#' @param x The argument.
#' @param fn Character: The function, for messages.
#'
#' @return `x`, an `r8_params`.
#'
#' @keywords internal
#' @noRd
as_params <- function(x, fn) {
  if (inherits(x, "r8_params")) {
    return(x)
  }
  names <- if (is.character(x)) {
    x
  } else if (is.function(x)) {
    names(formals(x))
  }
  r8_abort(
    fn,
    " takes its parameters as params()",
    if (length(names) > 0L) {
      paste0(
        ": params(",
        paste(ifelse(names == "...", "\"...\"", names), collapse = ", "),
        ")"
      )
    } else {
      ", such as params(x, n = arg_integer())"
    },
    ".",
    class = "r8_declaration_error"
  )
} # /R8::as_params
