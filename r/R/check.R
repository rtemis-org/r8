# check.R
# ::R8::
# 2026- EDG rtemis.org

# The errors of the check of a typed generic's call, which the C routines
# `call` and `result` (`r/src/view.c`) raise through R.
# spec: R8/class-system#checks-at-a-call

# %% key_method ----
#' The method of a key
#'
#' @param key Character: `"Trait|method"`.
#'
#' @return Character: The method's name.
#'
#' @keywords internal
#' @noRd
key_method <- function(key) {
  if (!grepl("|", key, fixed = TRUE)) {
    # A function's key is its qualified name.
    return(sub("^.*::", "", key))
  }
  substr(key, regexpr("|", key, fixed = TRUE) + 1L, nchar(key))
} # /R8::key_method


# %% signature_abort ----
#' Signal that a typed generic's trait is not registered
#'
#' @param key Character: `"Trait|method"`.
#' @param ... Not used.
#'
#' @return Does not return; signals `r8_dispatch_error`.
#'
#' @keywords internal
#' @noRd
signature_abort <- function(key, ...) {
  r8_abort(
    "`",
    sub("|", "` is not a registered trait (method `", key, fixed = TRUE),
    "`); load the package that declares it.",
    class = "r8_dispatch_error"
  )
} # /R8::signature_abort


# %% argument_abort ----
#' Signal the problems of a call's arguments
#'
#' Names an argument that the call omitted by its default: "`n`, its default
#' `length(x)`, must be at least 1; found 0."
#'
#' @param key Character: `"Trait|method"`, or a function's qualified name.
#' @param res The problems, of class `r8_problems`.
#' @param env Environment: The frame of the generic or the function.
#'
#' @return Does not return; signals `r8_argument_error`.
#'
#' @keywords internal
#' @noRd
argument_abort <- function(key, res, env) {
  errors <- res[["errors"]]
  params <- sub("^/([^/]*).*$", "\\1", errors[["path"]])
  frame <- Position(
    function(f) identical(f, env),
    sys.frames(),
    right = TRUE
  )
  fmls <- if (!is.na(frame)) formals(sys.function(frame))
  for (i in seq_along(params)) {
    p <- params[[i]]
    # A rule over several parameters has no parameter of its own.
    if (
      !nzchar(p) ||
        !p %in% names(fmls) ||
        is.null(fmls) ||
        !eval(as.call(list(quote(missing), as.name(p))), env) ||
        identical(fmls[[p]], quote(expr = ))
    ) {
      next
    }
    where <- paste0("`", errors[["where"]][[i]], "`")
    if (startsWith(errors[["message"]][[i]], where)) {
      errors[["message"]][[i]] <- paste0(
        where,
        if (identical(errors[["where"]][[i]], p)) ", its" else ", of its",
        " default `",
        deparse1(fmls[[p]]),
        "`,",
        substr(
          errors[["message"]][[i]],
          nchar(where) + 1L,
          nchar(errors[["message"]][[i]])
        )
      )
    }
  }
  inputs <- mget(
    unique(params[nzchar(params)]),
    envir = env,
    ifnotfound = list(NULL)
  )
  value_abort(
    "Invalid arguments of",
    paste0(key_method(key), "()"),
    errors,
    inputs = inputs,
    class = c("r8_argument_error", "r8_value_error")
  )
} # /R8::argument_abort


# %% provider_text ----
#' The source of a result, for messages
#'
#' @param tuple Character: The tuple key of the impl, `""` for the trait's
#'   default implementation.
#'
#' @return Character.
#'
#' @keywords internal
#' @noRd
provider_text <- function(tuple) {
  if (!nzchar(tuple)) {
    return("the trait's default implementation")
  }
  positions <- strsplit(tuple, ",", fixed = TRUE)[[1L]]
  text <- paste0("`", positions, "`", collapse = ", ")
  paste0(
    "its method for ",
    if (length(positions) > 1L) paste0("(", text, ")") else text
  )
} # /R8::provider_text


# %% result_abort ----
#' Signal the problems of a method's result
#'
#' @param key Character: `"Trait|method"`.
#' @param tuple Character: The tuple key of the impl.
#' @param res The problems, of class `r8_problems`.
#'
#' @return Does not return; signals `r8_result_error`.
#'
#' @keywords internal
#' @noRd
result_abort <- function(key, tuple, res) {
  value_abort(
    "Invalid result of",
    paste0(key_method(key), "()"),
    res[["errors"]],
    source = if (grepl("|", key, fixed = TRUE)) provider_text(tuple),
    class = c("r8_result_error", "r8_type_error")
  )
} # /R8::result_abort


# %% self_abort ----
#' Signal that a method that returns `Self` returned another type
#'
#' @param key Character: `"Trait|method"`.
#' @param x The call's first dispatched argument.
#' @param out List: The result, and the impl's provider.
#'
#' @return Does not return; signals `r8_result_error`.
#'
#' @keywords internal
#' @noRd
self_abort <- function(key, x, out) {
  value <- out[[1L]]
  result_abort(
    key,
    out[[2L]][[3L]],
    list(
      errors = list(
        code = "return_type",
        path = "",
        rule = NA_character_,
        rule_index = NA_integer_,
        message = paste0(
          "The value must be a value of `",
          value_type_text(x),
          "`, the type of the first dispatched argument; found ",
          if (is_value(value)) {
            paste0("a value of `", value_type(value), "`")
          } else {
            paste0(
              "an object of class ",
              paste0("\"", class(value), "\"", collapse = ", ")
            )
          },
          "."
        ),
        where = "",
        suggestion = NA_character_
      )
    )
  )
} # /R8::self_abort


# %% method_abort ----
#' Signal the error that a compiled method returned
#'
#' @param code Character: The error's code, `"panic"` for a panic.
#' @param message Character: The method's message.
#' @param call The generic's call, or `NULL`.
#'
#' @return Does not return; signals `r8_method_error`, or `r8_panic` for a
#'   panic.
#'
#' @keywords internal
#' @noRd
method_abort <- function(code, message, call) {
  panic <- identical(code, "panic")
  r8_abort(
    if (panic) {
      paste0(
        "the compiled method panicked, which is a defect of its package: ",
        message
      )
    } else {
      message
    },
    " [",
    code,
    "]",
    class = c(if (panic) "r8_panic", "r8_method_error"),
    call = call,
    data = list(code = code)
  )
} # /R8::method_abort
