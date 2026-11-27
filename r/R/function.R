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
#' @section A body in Rust:
#' A function of a package whose body is exactly [rust()] has its body in the
#' package's Rust code: a function marked `#[r8::function]` of the same name,
#' listed in `functions` of the package's `r8::exports!`. The declaration stays
#' the function's: its parameters, types, defaults, descriptions, rules, return
#' type, help page, and [r8_schema()]. Each call checks and converts the
#' arguments, evaluates every parameter, a default written in R included, and
#' passes the values to the Rust function in the order of the parameters, `...`
#' as a list; the result is checked as an R body's is. An error that the Rust
#' function returns is an `r8_compiled_error`. When the package loads, R8
#' compares the Rust function's signature with the declaration
#' ([r8_rust_signature()] writes it).
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
  # Before the declaration, so that a refused body leaves none.
  in_rust <- rust_body(fn)
  if (in_rust && !nzchar(declaring_package(env))) {
    r8_abort(
      "A function whose body is rust() is declared in a package, whose Rust code holds the body; `",
      qname,
      "()` is declared outside a package.",
      class = "r8_declaration_error"
    )
  }
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
  f <- make_function(qname, fn, stored_defaults(p), ir, rust = in_rust)
  record <- r8_function_record(qname)
  recorded <- record_declaration(
    env,
    list(
      kind = "function",
      record = record,
      namespace = namespace,
      name = qname,
      rust = in_rust
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
#' and the expression that gives the body's value pass through the C routine
#' `result` (`value_checked()`).
#'
#' A function whose body is in Rust has the body of the C routine `native`
#' alone, which checks the arguments, calls the compiled body, and checks the
#' result; the closure's attribute `r8_body` is `"rust"`.
#'
#' @param qname Character: The qualified name.
#' @param fn Function: The function given.
#' @param p An `r8_params`: Its parameters.
#' @param ir List: The declaration.
#' @param rust Logical: Whether the body is in Rust.
#'
#' @return The closure, of class `r8_function`.
#'
#' @keywords internal
#' @noRd
make_function <- function(qname, fn, p, ir, rust = FALSE) {
  ns <- asNamespace("R8")
  at <- function(name) call("$", ns, as.name(name))
  key <- call("quote", as.name(qname))
  if (rust) {
    out <- as.function(
      c(
        p[["formals"]],
        as.call(list(
          quote(.Call),
          at(".r8_c_native"),
          quote(environment()),
          key,
          at(".r8_dispatch")
        ))
      ),
      envir = environment(fn)
    )
    return(structure(
      out,
      class = c("r8_function", "function"),
      r8_function = qname,
      r8_source = fn,
      r8_body = "rust"
    ))
  }
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
    body <- value_checked(returns_checked(body, checked), checked)
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
#' function that the body defines, in `quote()`, `bquote()`, `substitute()`,
#' `local()`, or `evalq()`, or in a formula.
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
    # local() and evalq() evaluate their expression in a frame of their own,
    # whose return() returns from that frame.
    if (
      fn %in%
        c("function", "quote", "bquote", "substitute", "~", "local", "evalq")
    ) {
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


# %% rust ----
#' The body of a typed function written in Rust
#'
#' `rust()` is the whole body of a function given to [new_function()] whose body
#' is in the declaring package's Rust code: a Rust function of the same name,
#' marked `#[r8::function]` and listed in `functions` of the package's
#' `r8::exports!`. The declaration in R holds the parameters, their types,
#' defaults, and descriptions, the rules, and the return type; the Rust function
#' computes the result. [r8_rust_signature()] writes the Rust function's
#' signature from the declaration.
#'
#' @return Does not return: called outside a typed function, it signals
#'   `r8_declaration_error`.
#'
#' @examples
#' # In a package whose Rust code holds `fn level(x: &[f64], lo: f64, hi: f64)`:
#' # level <- new_function(
#' #   "level",
#' #   function(x = arg_vec(type_double()), lo = arg_double(), hi = arg_double()) rust(),
#' #   rules = list(rule(lo <= hi)),
#' #   return_type = vec(type_double())
#' # )
#' try(rust())
#'
#' @export
rust <- function() {
  r8_abort(
    "rust() is the body of a function given to new_function() in a package, whose Rust code holds the function's body; called by itself, it computes nothing.",
    class = "r8_declaration_error"
  )
} # /R8::rust


# %% rust_body ----
#' Whether a function's body is `rust()`
#'
#' The body is `rust()` or `R8::rust()`, alone or in braces, where `rust` alone
#' names R8's function, or no function, in the environment of `fn`. A call of
#' R8's `rust()` anywhere else in the body, outside `quote()`, `bquote()`, a
#' function, and a formula, is a declaration error.
#'
#' @param fn Function: The function given to `new_function()`.
#'
#' @return Logical.
#'
#' @keywords internal
#' @noRd
rust_body <- function(fn) {
  # `rust` alone is R8's when it names R8's function, or no function, where
  # `fn` is defined; a package may define a function of its own of that name.
  bare <- get0("rust", envir = environment(fn), mode = "function")
  heads <- c(
    list(
      quote(R8::rust),
      call(":::", as.name("R8"), as.name("rust"))
    ),
    if (is.null(bare) || identical(bare, rust)) list(quote(rust))
  )
  is_rust <- function(e) {
    is.call(e) && any(vapply(heads, identical, NA, e[[1L]]))
  }
  b <- body(fn)
  if (is.call(b) && identical(b[[1L]], as.name("{")) && length(b) == 2L) {
    b <- b[[2L]]
  }
  if (is_rust(b) && length(b) == 1L) {
    return(TRUE)
  }
  calls_rust <- function(e) {
    if (!is.call(e)) {
      return(FALSE)
    }
    if (is_rust(e)) {
      return(TRUE)
    }
    # Code that is not evaluated as the body's own.
    if (
      is.name(e[[1L]]) &&
        as.character(e[[1L]]) %in% c("quote", "bquote", "function", "~")
    ) {
      return(FALSE)
    }
    any(vapply(as.list(e), calls_rust, NA))
  }
  if (calls_rust(body(fn))) {
    r8_abort(
      "rust() is the whole body of a function whose body is in Rust, such as function(x = arg_double()) rust(); R code around a Rust body is a function of its own that calls the function whose body is rust().",
      class = "r8_declaration_error"
    )
  }
  FALSE
} # /R8::rust_body


# %% r8_rust_signature ----
#' The Rust signature of a typed function
#'
#' `r8_rust_signature()` writes the Rust function that the body of a typed
#' function moves into: `#[r8::function]`, the function's name, each parameter
#' with the Rust type of its declared type, the result's type, the description
#' as a doc comment, the checks that R8 makes before and after the body as a
#' comment, and the body `todo!()`. A vector of doubles, integers, logicals, or
#' bytes is a slice borrowed for the call, `&[f64]`, and a `Vec` as a result; a
#' string is `&str`, and `String` as a result; a parameter without a type, or of
#' a type without a Rust form here, is `r8::Sexp`, and `...` is `r8::Dots`. The
#' function then takes the body `rust()` ([new_function()]).
#'
#' @param f A typed function, of [new_function()].
#'
#' @return Character: The Rust text, of class `r8_rust_text`, which prints as
#'   the text.
#'
#' @examples
#' op <- options(r8.namespace = "demo")
#' level <- new_function(
#'   "level",
#'   function(x = arg_vec(type_double()), lo = arg_double(), hi = arg_double()) {
#'     y <- numeric(length(x))
#'     s <- 0
#'     for (i in seq_along(x)) {
#'       s <- min(max(s + x[[i]], lo), hi)
#'       y[[i]] <- s
#'     }
#'     y
#'   },
#'   rules = list(rule(lo <= hi)),
#'   return_type = vec(type_double()),
#'   description = "Holds a running level between `lo` and `hi`."
#' )
#' r8_rust_signature(level)
#' options(op)
#'
#' @export
r8_rust_signature <- function(f) {
  if (!inherits(f, "r8_function")) {
    r8_abort(
      "`f` is a typed function of new_function().",
      class = "r8_type_error"
    )
  }
  qname <- attr(f, "r8_function")
  if (is.null(r8_function_ir(qname)) && !is.null(attr(f, "r8_record"))) {
    register_records(list(attr(f, "r8_record")))
  }
  structure(r8_rust_signature_text(qname), class = "r8_rust_text")
} # /R8::r8_rust_signature


# %% print.r8_rust_text ----
#' @rdname r8_rust_signature
#' @param x Character: Rust text.
#' @param ... Not used.
#' @export
print.r8_rust_text <- function(x, ...) {
  cat(x)
  invisible(x)
} # /R8::print.r8_rust_text


# %% value_checked ----
#' A body whose value passes a check
#'
#' Applies `check()` to the expression that gives the value of `e`: the last
#' expression of a block, and each branch of an `if` with an `else`. The rest of
#' the body runs in the function's frame as written: an expression given to
#' `withVisible()` is a promise, whose loops run slower than the frame's own code.
#' A `return()` stays as `returns_checked()` wrote it.
#'
#' @param e Language object: The body, or a part of it.
#' @param check Function: The check of a value's expression.
#'
#' @return The expression.
#'
#' @keywords internal
#' @noRd
value_checked <- function(e, check) {
  if (is.call(e) && is.name(e[[1L]])) {
    head <- as.character(e[[1L]])
    if (identical(head, "{") && length(e) > 1L) {
      n <- length(e)
      e[[n]] <- value_checked(e[[n]], check)
      return(e)
    }
    if (identical(head, "if") && length(e) == 4L) {
      e[[3L]] <- value_checked(e[[3L]], check)
      e[[4L]] <- value_checked(e[[4L]], check)
      return(e)
    }
    if (identical(head, "return")) {
      return(e)
    }
  }
  check(e)
} # /R8::value_checked


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
    # The package that declares the function: the namespace of its closure,
    # or the owner of its R8 namespace, whose name may differ from the
    # package's (`Config/R8/namespace`).
    pkg <- if (is.function(f)) declaring_package(environment(f)) else ""
    if (!nzchar(pkg)) {
      pkg <- r8_namespace_owner(sub("::.*$", "", key))
    }
    if (nzchar(pkg) && requireNamespace(pkg, quietly = TRUE)) {
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
  if (identical(attr(x, "r8_body"), "rust")) {
    cat(
      "  body in Rust, package '",
      declaring_package(environment(x)),
      "'\n",
      sep = ""
    )
  }
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
