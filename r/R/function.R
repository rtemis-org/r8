# function.R
# ::R8::
# 2026- EDG rtemis.org

# Typed functions: a function whose formals declare the types of its parameters,
# registered under a qualified name, which checks its arguments and its result in
# its own frame.
#
# spec: R8/class-system#typed-functions

# %% new_function ----
#' Declare a typed function
#'
#' `new_function()` declares a function whose parameters have types. The types are
#' written in the function's own formals, with [arg()] and its shorthands, such as
#' `n = arg_integer(min = 1L, default = 10L)`, and each call checks and converts its
#' arguments before the body runs, as a typed generic's call does ([generic()]). The
#' function is registered under its name in the declaring package's namespace, as a
#' type is.
#'
#' The body runs in the function's own frame, so `substitute()`, `match.call()`,
#' `sys.call()`, `parent.frame()`, `missing()`, and `on.exit()` mean what they mean in
#' a plain function. With `return_type`, each `return()` of the body, and its last
#' value, pass the check of the result; a `return()` in a function defined in the body
#' belongs to that function.
#'
#' @section Rules:
#' `rules` hold over the typed parameters, which a rule reads by name: `rule(lo <=
#' hi)`. They are checked once each argument satisfies its type. A rule that reads an
#' optional parameter holds where its argument is `NULL`.
#'
#' @section Typed and untyped parameters:
#' With `typed = "mixed"`, a parameter may have no type, and the function may take
#' `...`; a call checks the parameters that have types. With `typed = "strict"`,
#' every parameter has a type and there is no `...`, which the declaration checks.
#'
#' @section Outside R8:
#' The function given is plain R: `arg()` gives its default, or R's error for a
#' missing argument, and checks nothing.
#'
#' @param name Character: The function's name, an R8 name, under which it is
#'   registered.
#' @param fn Function: The function, whose formals declare the parameters.
#' @param rules A [rule()], or a list of rules, over the typed parameters.
#' @param return_type Optional: The type of the value that the function returns: a
#'   field type, such as `type_double()`, or an R8 type.
#' @param description Character: What the function does.
#' @param typed Character \{"mixed", "strict"\}: Whether every parameter has a type.
#'
#' @return The function, of class `r8_function`, with the formals of `fn`.
#'
#' @examples
#' op <- options(r8.namespace = "demo")
#' clip <- new_function(
#'   "clip",
#'   function(x = arg_vec(type_double()), lo = arg_double(), hi = arg_double()) {
#'     pmin(pmax(x, lo), hi)
#'   },
#'   rules = list(order = rule(lo <= hi)),
#'   return_type = vec(type_double())
#' )
#' clip(c(-1, 0.5, 3), lo = 0, hi = 1)
#' try(clip(c(-1, 0.5, 3), lo = 1, hi = 0))
#' clip
#' options(op)
#'
#' @export
new_function <- function(
  name,
  fn,
  rules = list(),
  return_type = NULL,
  description = "",
  typed = "mixed"
) {
  if (!is_name(name)) {
    r8_abort(
      "A function's name is an R8 name, such as \"clip\"",
      name_reason(name),
      ".",
      class = "r8_declaration_error"
    )
  }
  if (!is.function(fn) || is.primitive(fn) || inherits(fn, "r8_function")) {
    r8_abort(
      "`fn` is a function, such as function(x = arg_double()) x.",
      class = "r8_declaration_error"
    )
  }
  if (
    !is.character(typed) ||
      length(typed) != 1L ||
      !typed %in% c("mixed", "strict")
  ) {
    r8_abort(
      "`typed` is \"mixed\" or \"strict\".",
      class = "r8_declaration_error"
    )
  }
  if (!is.character(description) || length(description) != 1L) {
    r8_abort("`description` is one string.", class = "r8_declaration_error")
  }
  env <- parent.frame()
  namespace <- declaring_namespace(env)
  claim_namespace(namespace, declaring_package(env))
  qname <- paste0(namespace, "::", name)
  p <- function_params(fn)
  ir <- list(
    name = name,
    description = description,
    params = if (length(p[["names"]]) > 0L) {
      params_ir(p, character())
    } else {
      list()
    }
  )
  ir[["return_type"]] <- return_type_ir(return_type, self = FALSE)
  ir[["rules"]] <- as_rules(rules)
  if (identical(typed, "strict")) {
    ir[["typed"]] <- "strict"
  }
  problems <- r8_declare_function(ir, namespace)
  if (length(problems[["code"]]) > 0L) {
    # A rule of a function reads its typed parameters.
    problems[["message"]] <- sub(
      "is not a field in scope; the nearest field",
      "is not a typed parameter; the nearest",
      sub(
        "is not a field in scope",
        "is not a typed parameter",
        problems[["message"]],
        fixed = TRUE
      ),
      fixed = TRUE
    )
    declaration_failed(qname, problems, list(rules = ir[["rules"]]))
  }
  check_base_names(environment(fn), name)
  f <- make_function(qname, fn, stored_defaults(p), ir)
  record <- r8_function_record(qname)
  recorded <- record_declaration(
    env,
    list(
      kind = "function",
      record = record,
      namespace = namespace,
      name = qname
    )
  )
  if (!recorded) {
    attr(f, "r8_record") <- record
  }
  f
} # /R8::new_function


# %% function_params ----
#' The parameters that the formals of a function declare
#'
#' @param fn Function.
#'
#' @return An `r8_params`, as `params()` gives it.
#'
#' @keywords internal
#' @noRd
function_params <- function(fn) {
  fmls <- formals(fn)
  nms <- names(fmls) %||% character()
  exprs <- lapply(seq_along(fmls), function(i) {
    if (identical(nms[[i]], "...")) {
      return("...")
    }
    if (identical(fmls[[i]], quote(expr = ))) {
      return(as.name(nms[[i]]))
    }
    fmls[[i]]
  })
  bare <- vapply(
    seq_along(fmls),
    function(i) {
      identical(nms[[i]], "...") || identical(fmls[[i]], quote(expr = ))
    },
    NA
  )
  names(exprs) <- ifelse(bare, "", nms)
  read_params(exprs, environment(fn), generic = FALSE)
} # /R8::function_params


# %% make_function ----
#' The closure of a typed function
#'
#' The closure has the formals of `fn`, with constant defaults in their stored
#' form, and its body: before it, the C routine `check`, which forces, checks, and
#' converts the typed arguments in the frame; with a return type, each `return()`
#' and the last value pass through the C routine `result`.
#'
#' @param qname Character: The qualified name.
#' @param fn Function: The function given.
#' @param p An `r8_params`: Its parameters.
#' @param ir List: The declaration.
#'
#' @return The closure, of class `r8_function`.
#'
#' @keywords internal
#' @noRd
make_function <- function(qname, fn, p, ir) {
  ns <- asNamespace("R8")
  at <- function(name) call("$", ns, as.name(name))
  key <- call("quote", as.name(qname))
  typed <- !all(vapply(p[["types"]], is.null, NA)) || length(ir[["rules"]]) > 0L
  body <- body(fn)
  if (!is.null(ir[["return_type"]])) {
    checked <- function(e) {
      call(
        "{",
        call("<-", quote(.r8_v), call("withVisible", e)),
        call(
          "if",
          as.call(list(
            quote(.Call),
            at(".r8_c_result"),
            quote(.r8_v),
            NULL,
            key,
            NULL,
            at(".r8_dispatch")
          )),
          quote(.r8_v[[1L]]),
          quote(invisible(.r8_v[[1L]]))
        )
      )
    }
    body <- checked(returns_checked(body, checked))
  }
  if (typed) {
    check <- as.call(list(
      quote(.Call),
      at(".r8_c_check"),
      quote(environment()),
      key,
      at(".r8_dispatch")
    ))
    body <- call("{", check, body)
  }
  out <- as.function(c(p[["formals"]], body), envir = environment(fn))
  structure(
    out,
    class = c("r8_function", "function"),
    r8_function = qname,
    r8_source = fn
  )
} # /R8::make_function


# %% returns_checked ----
#' A body whose `return()` calls pass a check
#'
#' Each call of `return` in `e` takes `check()` of its value, except one in a
#' function that the body defines, in `quote()`, `bquote()`, or `substitute()`, or in
#' a formula.
#'
#' @param e Language object: The body, or a part of it.
#' @param check Function: The check of a value's expression.
#'
#' @return The expression.
#'
#' @keywords internal
#' @noRd
returns_checked <- function(e, check) {
  if (!is.call(e)) {
    return(e)
  }
  head <- e[[1L]]
  if (is.name(head)) {
    fn <- as.character(head)
    if (fn %in% c("function", "quote", "bquote", "substitute", "~")) {
      return(e)
    }
    if (identical(fn, "return")) {
      value <- if (length(e) > 1L) returns_checked(e[[2L]], check)
      return(call("return", check(value)))
    }
  }
  for (i in seq_along(e)[-1L]) {
    if (!is.null(e[[i]]) && !identical(e[[i]], quote(expr = ))) {
      e[[i]] <- returns_checked(e[[i]], check)
    }
  }
  e
} # /R8::returns_checked


# %% function_unregistered ----
#' Register the declaration that a typed function carries
#'
#' Called by the C routine `check` for a function whose declaration the session
#' has not registered, such as one that a worker process received.
#'
#' @param key Character: The qualified name.
#' @param env Environment: The function's frame.
#' @param ... Not used.
#'
#' @return `NULL`, invisibly; signals `r8_dispatch_error` for a function without a
#'   record.
#'
#' @keywords internal
#' @noRd
function_unregistered <- function(key, env, ...) {
  frame <- Position(
    function(f) identical(f, env),
    sys.frames(),
    right = TRUE
  )
  f <- if (!is.na(frame)) sys.function(frame)
  record <- attr(f, "r8_record")
  if (is.null(record)) {
    ns <- sub("::.*$", "", key)
    if (requireNamespace(ns, quietly = TRUE)) {
      return(invisible(NULL))
    }
    r8_abort(
      "`",
      key,
      "()` is not registered; load the package that declares it.",
      class = "r8_dispatch_error"
    )
  }
  register_records(list(record))
  invisible(NULL)
} # /R8::function_unregistered


# %% print.r8_function ----
#' @rdname new_function
#' @param x A typed function.
#' @param ... Not used.
#' @export
print.r8_function <- function(x, ...) {
  qname <- attr(x, "r8_function")
  name <- sub("^.*::", "", qname)
  ir <- r8_function_ir(qname)
  cat("<r8 function ", qname, ">\n", sep = "")
  cat(usage_line(name, names(formals(x))), "\n", sep = "")
  if (!is.null(ir)) {
    lines <- signature_lines(ir, x, "  ")
    if (length(lines) > 0L) {
      cat(lines, sep = "\n")
    }
    for (r in ir[["rules"]]) {
      cat(
        "  rule",
        if (!is.null(r[["id"]])) paste0(" ", r[["id"]]),
        ": ",
        rule_text(r),
        "\n",
        sep = ""
      )
    }
  }
  invisible(x)
} # /R8::print.r8_function
