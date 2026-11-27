# call.R
# ::R8::
# 2026- EDG rtemis.org

# Documents of a call of a typed function: {"$function": "ns::f", "arguments":
# {...}}, with "$types" optional. The caller names the functions that a document
# may call; the check runs in Rust over the document, and the arguments reach the
# function in their stored form.
#
# spec: R8/class-system#typed-functions

#' @include function.R
NULL


# %% call_functions ----
#' The functions that a document of a call may name
#'
#' @param f A typed function, or a list of them.
#'
#' @return Named list: The functions, by qualified name, each registered.
#'
#' @keywords internal
#' @noRd
call_functions <- function(f) {
  if (inherits(f, "r8_function")) {
    q <- attr(f, "r8_function")
    if (length(function_undocumented(f)) == 0L) {
      return(stats::setNames(list(f), q))
    }
  }
  fs <- if (inherits(f, "r8_function")) list(f) else f
  if (
    !is.list(fs) ||
      length(fs) == 0L ||
      !all(vapply(fs, inherits, NA, what = "r8_function"))
  ) {
    r8_abort(
      "`f` is a typed function of new_function(), or a list of them.",
      class = "r8_type_error"
    )
  }
  names(fs) <- vapply(fs, attr, "", which = "r8_function")
  for (q in names(fs)) {
    undocumented <- function_undocumented(fs[[q]])
    if (length(undocumented) > 0L) {
      r8_abort(
        "`",
        q,
        "()` has parameters that a document cannot hold: ",
        paste0("`", undocumented, "`", collapse = ", "),
        ". A parameter of a document has a type with a JSON form: not `...`, ",
        "and not host_class() outside option().",
        class = "r8_type_error"
      )
    }
  }
  fs
} # /R8::call_functions


# %% function_undocumented ----
#' The parameters of a typed function that a document cannot hold
#'
#' Registers the declaration that the function carries, for one declared
#' outside a package in another process.
#'
#' @param f A typed function.
#'
#' @return Character: The names of the parameters without a type, `...`, and
#'   those of a type `host_class()` outside `option()`.
#'
#' @keywords internal
#' @noRd
function_undocumented <- function(f) {
  q <- attr(f, "r8_function")
  out <- r8_function_undocumented(q)
  if (is.null(out)) {
    record <- attr(f, "r8_record")
    if (is.null(record)) {
      r8_abort(
        "`",
        q,
        "()` is not registered; load the package that declares it.",
        class = "r8_dispatch_error"
      )
    }
    register_records(list(record))
    out <- r8_function_undocumented(q)
  }
  out
} # /R8::function_undocumented


# %% read_call ----
#' Read a document of a call of one of the functions given
#'
#' @param fs Named list: The functions, by qualified name.
#' @param json Character: The document.
#' @param load Optional Character: Packages to load.
#'
#' @return List: As `r8_read_call()` returns it.
#'
#' @keywords internal
#' @noRd
read_call <- function(fs, json, load) {
  json <- document_text(json)
  load_packages(load)
  r8_read_call(json, names(fs))
} # /R8::read_call


# %% r8check_call ----
#' Check a document of a call of a typed function
#'
#' Reads a document of a call, `{"$function": "ns::f", "arguments": {...}}`, and
#' checks it without running the function: its form, that `"$function"` names one
#' of the functions `f`, each argument against its parameter's type, the versions
#' of `"$types"` when the document holds it, and, when every argument holds, the
#' function's rules, with the defaults written as constants of the arguments it
#' omits. A rule that reads an omitted argument whose default is written in R,
#' which the function evaluates when it runs, is listed in the attribute
#' `unchecked`, as `rule_unchecked`.
#'
#' @param f A typed function of [new_function()], or a list of them: the
#'   functions that the document may call. The document never selects other
#'   code.
#' @param json Character: The document, one string or its lines.
#' @param load Optional Character: Packages to load before the check, as
#'   [r8from_json()] takes them.
#'
#' @return A data frame of the problems, as the condition of
#'   [r8call_from_json()] holds them (`code`, `path`, `rule`, `message`, and
#'   others), with no rows for a call that holds. Paths are JSON Pointers into
#'   the document, such as `/arguments/lo`. The attribute `unchecked` holds, in
#'   the same form, the rules that read an omitted argument whose default is
#'   written in R, which the function checks when it runs.
#'
#' @seealso [r8call_from_json()], [r8call_to_json()], and [r8schema()].
#'
#' @examples
#' op <- options(r8.namespace = "demo")
#' clip <- new_function(
#'   "clip",
#'   function(x = arg_vec(type_double()), lo = arg_double(), hi = arg_double()) {
#'     pmin(pmax(x, lo), hi)
#'   },
#'   rules = list(order = rule(lo <= hi))
#' )
#' r8check_call(clip, '{"$function": "demo::clip", "arguments": {"x": [1], "lo": 2, "hi": 1}}')
#' options(op)
#'
#' @export
r8check_call <- function(f, json, load = NULL) {
  fs <- call_functions(f)
  res <- read_call(fs, json, load)
  errors <- res[["errors"]]
  undocumented_abort(errors, fs)
  unchecked <- res[["unchecked"]]
  if (!is.null(errors)) {
    at <- errors[["code"]] == "rule_unchecked"
    unchecked <- lapply(errors, `[`, at)
    errors <- lapply(errors, `[`, !at)
  }
  structure(
    problems_data(errors),
    unchecked = problems_data(unchecked)
  )
} # /R8::r8check_call


# %% r8call_from_json ----
#' Call a typed function from a document of a call
#'
#' Checks a document of a call as [r8check_call()] does, raises every problem in
#' one `r8_argument_error`, and otherwise calls the function that `"$function"`
#' names with the arguments in their stored form. An argument that the document
#' omits takes its default, and `null` is `NULL`. The call is evaluated in an
#' environment that binds the arguments by name, whose parent is the frame of the
#' caller of `r8call_from_json()`: `sys.call()` in the function's body shows the
#' arguments' names, and `parent.frame()` is that environment.
#'
#' @inheritParams r8check_call
#'
#' @return The function's result.
#'
#' @examples
#' op <- options(r8.namespace = "demo")
#' clip <- new_function(
#'   "clip",
#'   function(x = arg_vec(type_double()), lo = arg_double(default = 0), hi = arg_double()) {
#'     pmin(pmax(x, lo), hi)
#'   }
#' )
#' r8call_from_json(clip, '{"$function": "demo::clip", "arguments": {"x": [-1, 0.5, 3], "hi": 1}}')
#' options(op)
#'
#' @export
r8call_from_json <- function(f, json, load = NULL) {
  fs <- call_functions(f)
  res <- read_call(fs, json, load)
  errors <- res[["errors"]]
  if (!is.null(errors)) {
    undocumented_abort(errors, fs)
    # The function checks a rule that the document could not decide when it
    # runs.
    errors <- lapply(errors, `[`, errors[["code"]] != "rule_unchecked")
    q <- if (length(fs) == 1L) names(fs) else "call"
    value_abort(
      "Invalid arguments of",
      paste0(sub("^.*::", "", q), "()"),
      errors,
      class = c("r8_argument_error", "r8_value_error")
    )
  }
  # The call names its arguments by symbols bound in its own environment, so
  # sys.call() and match.call() in the body show the call, not the values.
  env <- list2env(res[["arguments"]], parent = parent.frame())
  assign(res[["name"]], fs[[res[["function"]]]], envir = env)
  eval(res[["call"]], env)
} # /R8::r8call_from_json


# %% r8call_to_json ----
#' Write a document of a call of a typed function
#'
#' Writes the document of a call of `f` with the arguments given, checked as
#' [r8check_call()] checks a document: `{"$function": "ns::f", "$types": {...},
#' "arguments": {...}}`, in RFC 8785 form, with `"$types"` holding the version of
#' each named type that the arguments contain. The arguments are matched to the
#' parameters as R matches a call, and only the arguments given are written.
#'
#' @param ... The typed function of [new_function()], unnamed and first, then the
#'   arguments, as the call of the function takes them.
#'
#' @return Character: The document, of class `json`; an invalid argument raises
#'   `r8_argument_error`.
#'
#' @examples
#' op <- options(r8.namespace = "demo")
#' clip <- new_function(
#'   "clip",
#'   function(x = arg_vec(type_double()), lo = arg_double(default = 0), hi = arg_double()) {
#'     pmin(pmax(x, lo), hi)
#'   }
#' )
#' doc <- r8call_to_json(clip, c(-1, 0.5, 3), hi = 1)
#' doc
#' r8call_from_json(clip, doc)
#' options(op)
#'
#' @export
r8call_to_json <- function(...) {
  # The function is the first argument, without a name, so that a parameter
  # of any name, `f` included, is given by name.
  nms <- ...names()
  f <- if (...length() > 0L && (is.null(nms) || !nzchar(nms[[1L]]))) ..1
  if (!inherits(f, "r8_function")) {
    r8_abort(
      "The first argument, without a name, is a typed function of ",
      "new_function(): r8call_to_json(f, x = 1).",
      class = "r8_type_error"
    )
  }
  fs <- call_functions(f)
  q <- names(fs)
  given <- list(...)[-1L]
  # The arguments, named by their parameters as R matches a call.
  proto <- as.function(c(formals(f), list(NULL)))
  args <- tryCatch(
    as.list(match.call(proto, as.call(c(list(quote(f)), given))))[-1L],
    error = function(cond) {
      r8_abort(
        "Invalid arguments of `",
        sub("^.*::", "", q),
        "()`: ",
        conditionMessage(cond),
        "; its parameters are ",
        paste0("`", names(formals(f)), "`", collapse = ", "),
        ".",
        class = c("r8_argument_error", "r8_value_error")
      )
    }
  )
  res <- r8_write_call(q, args)
  if (!is.null(res[["errors"]])) {
    value_abort(
      "Invalid arguments of",
      paste0(sub("^.*::", "", q), "()"),
      res[["errors"]],
      inputs = args,
      class = c("r8_argument_error", "r8_value_error")
    )
  }
  structure(res[["json"]], class = "json")
} # /R8::r8call_to_json


# %% undocumented_abort ----
#' Raise the error of a function that a document cannot call
#'
#' @param errors List: The problems of a document.
#' @param fs Named list: The functions given.
#'
#' @return `NULL`, invisibly, when no problem is `function_undocumented`.
#'
#' @keywords internal
#' @noRd
undocumented_abort <- function(errors, fs) {
  at <- which(errors[["code"]] == "function_undocumented")
  if (length(at) > 0L) {
    r8_abort(errors[["message"]][[at[[1L]]]], ".", class = "r8_type_error")
  }
  invisible(NULL)
} # /R8::undocumented_abort
