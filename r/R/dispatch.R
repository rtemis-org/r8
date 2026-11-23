# dispatch.R
# ::R8::
# 2026- EDG rtemis.org

# Multiple dispatch in R: the dispatched arguments of a generic or trait
# method, the tuples of types an impl names, the keys of the dispatch tables,
# and the resolution of a call through the core's resolver.
# spec: R8/class-system#multiple-dispatch

# The most dispatched arguments a generic takes, one C routine per count.
MAX_DISPATCH <- 3L


# %% param_names ----
#' The names of an IR method's parameters
#'
#' @param params List: The `params` of an IR method, each a name or a list
#'   with `name`.
#'
#' @return Character.
#'
#' @keywords internal
#' @noRd
param_names <- function(params) {
  vapply(
    params,
    function(p) if (is.list(p)) p[["name"]] else p,
    character(1)
  )
} # /R8::param_names


# %% dispatch_names ----
#' The parameters an IR method dispatches on
#'
#' Those marked `dispatch`, or else the first.
#'
#' @param params List: The `params` of an IR method.
#'
#' @return Character.
#'
#' @keywords internal
#' @noRd
dispatch_names <- function(params) {
  marked <- vapply(
    params,
    function(p) is.list(p) && isTRUE(p[["dispatch"]]),
    logical(1)
  )
  names <- param_names(params)
  if (any(marked)) names[marked] else names[1L]
} # /R8::dispatch_names


# %% check_dispatch ----
#' Check the dispatched arguments of a generic or trait method
#'
#' @param dispatch Optional Character: The names; `NULL` for the first
#'   argument.
#' @param args Character: The formal argument names.
#' @param label Character: The generic or method, for messages.
#' @param replacement Logical: A replacement generic or method.
#'
#' @return Character: The dispatched argument names, in formal order.
#'
#' @keywords internal
#' @noRd
check_dispatch <- function(dispatch, args, label, replacement = FALSE) {
  if (is.null(dispatch)) {
    return(args[[1L]])
  }
  if (
    !is.character(dispatch) ||
      length(dispatch) == 0L ||
      anyNA(dispatch) ||
      anyDuplicated(dispatch) > 0L
  ) {
    r8_abort(
      "`dispatch` names the arguments of `",
      label,
      "` it dispatches on, distinct names such as c(\"x\", \"y\").",
      class = "r8_declaration_error"
    )
  }
  unknown <- setdiff(dispatch, setdiff(args, "..."))
  if (length(unknown) > 0L) {
    r8_abort(
      "`",
      label,
      "` dispatches on its named arguments (",
      paste(setdiff(args, "..."), collapse = ", "),
      "); `",
      paste(unknown, collapse = "`, `"),
      "` is not one.",
      class = "r8_declaration_error"
    )
  }
  if (length(dispatch) > MAX_DISPATCH) {
    r8_abort(
      "`",
      label,
      "` dispatches on at most ",
      MAX_DISPATCH,
      " arguments.",
      class = "r8_declaration_error"
    )
  }
  if (replacement && length(dispatch) > 1L) {
    r8_abort(
      "The replacement generic `",
      label,
      "` dispatches on its first argument only.",
      class = "r8_declaration_error"
    )
  }
  args[args %in% dispatch]
} # /R8::check_dispatch


# %% dispatch_params ----
#' The IR parameters of a signature with its dispatched arguments
#'
#' A signature that dispatches on its first argument alone carries no mark.
#'
#' @param args Character: The formal argument names.
#' @param dispatch Character: The dispatched argument names.
#'
#' @return List: The IR `params`.
#'
#' @keywords internal
#' @noRd
dispatch_params <- function(args, dispatch) {
  if (identical(dispatch, args[[1L]])) {
    return(as.list(args))
  }
  lapply(args, function(a) {
    if (a %in% dispatch) list(name = a, dispatch = TRUE) else a
  })
} # /R8::dispatch_params


# %% tuple_key ----
#' The key of an impl's tuple of positions
#'
#' @param positions Character: Each position's qualified type name, or
#'   `"any"`.
#'
#' @return Character: The positions joined by commas; a single position is
#'   its name, the key of single dispatch.
#'
#' @keywords internal
#' @noRd
tuple_key <- function(positions) {
  paste(positions, collapse = ",")
} # /R8::tuple_key


# %% impl_tuple ----
#' The tuple of positions that an `implement()` or `impl()` call names
#'
#' Types are matched to the dispatched arguments as R matches a call's
#' arguments: by name, then the unnamed ones in order; an argument not given
#' is any.
#'
#' @param types List: R8 type objects, each with a name or without one.
#' @param dispatch Character: The dispatched argument names.
#' @param label Character: The generic or trait, for messages.
#'
#' @return Character: One position per dispatched argument, a qualified type
#'   name or `"any"`.
#'
#' @keywords internal
#' @noRd
impl_tuple <- function(types, dispatch, label) {
  if (length(types) == 0L) {
    r8_abort(
      "An impl of `",
      label,
      "` names a type for at least one of its dispatched arguments (",
      paste(dispatch, collapse = ", "),
      ").",
      class = "r8_declaration_error"
    )
  }
  nms <- names(types) %||% rep("", length(types))
  named <- nms[nzchar(nms)]
  bad <- setdiff(named, dispatch)
  if (length(bad) > 0L || anyDuplicated(named) > 0L) {
    r8_abort(
      "`",
      label,
      "` dispatches on ",
      paste0("`", dispatch, "`", collapse = ", "),
      if (length(bad) > 0L) {
        paste0("; `", paste(bad, collapse = "`, `"), "` is not one of them")
      } else {
        "; each is named at most once"
      },
      ".",
      class = "r8_declaration_error"
    )
  }
  out <- stats::setNames(rep("any", length(dispatch)), dispatch)
  out[named] <- vapply(types[nzchar(nms)], position_name, character(1))
  free <- setdiff(dispatch, named)
  unnamed <- types[!nzchar(nms)]
  if (length(unnamed) > length(free)) {
    r8_abort(
      "`",
      label,
      "` dispatches on ",
      length(dispatch),
      " arguments (",
      paste(dispatch, collapse = ", "),
      "); this impl names ",
      length(types),
      " types.",
      class = "r8_declaration_error"
    )
  }
  out[free[seq_along(unnamed)]] <- vapply(unnamed, position_name, character(1))
  unname(out)
} # /R8::impl_tuple


# %% position_name ----
#' The qualified name of an R8 type object in an impl's position
#'
#' @param type An R8 type object.
#'
#' @return Character.
#'
#' @keywords internal
#' @noRd
position_name <- function(type) {
  if (!inherits(type, "r8_type")) {
    r8_abort(
      "A dispatched position names an R8 type object; classes of other ",
      "systems in a dispatched position come with impls on host classes.",
      class = "r8_declaration_error"
    )
  }
  ensure_registered(type)
  attr(type, "r8_name")
} # /R8::position_name


# %% tuple_ir ----
#' The `target` and `type_args` of an impl's IR from its positions
#'
#' @param positions Character: Qualified type names or `"any"`.
#'
#' @return List: `target` and `type_args`.
#'
#' @keywords internal
#' @noRd
tuple_ir <- function(positions) {
  pos <- lapply(positions, function(p) {
    if (identical(p, "any")) {
      list(kind = "any")
    } else {
      list(kind = "type", name = p)
    }
  })
  list(target = pos[[1L]], type_args = pos[-1L])
} # /R8::tuple_ir


# %% dispatch_key ----
#' The key of a value in a dispatch index
#'
#' The keys the C routines `find`, `find2`, and `find3` read: the value's
#' `r8_type`; `class:` and the first element of its class attribute; for an
#' object without one, `class:matrix` or `class:array` with a `dim`
#' attribute, and otherwise `type:` and its `typeof()`.
#'
#' @param x The value.
#'
#' @return Character, or `NULL` for a class name too long to key.
#'
#' @keywords internal
#' @noRd
dispatch_key <- function(x) {
  type <- attr(x, "r8_type", exact = TRUE)
  if (is.character(type) && length(type) == 1L) {
    return(type)
  }
  cls <- attr(x, "class", exact = TRUE)
  if (!is.null(cls)) {
    key <- paste0("class:", cls[[1L]])
    return(if (nchar(key, type = "bytes") < 256L) key)
  }
  d <- attr(x, "dim", exact = TRUE)
  if (!is.null(d)) {
    return(if (length(d) == 2L) "class:matrix" else "class:array")
  }
  paste0("type:", typeof(x))
} # /R8::dispatch_key


# %% resolve_tuple ----
#' The impl of a trait method for the values of its dispatched arguments
#'
#' Resolves the call through the core's resolver over the registered impls
#' (`r8_dispatch_resolve()`): the most specific applicable impl provides the
#' method, or the trait's default when that impl omits it or none applies.
#'
#' @param trait_name Character: The trait's qualified name.
#' @param method Character: The method name.
#' @param args List: The values of the dispatched arguments.
#' @param above Optional Character: The tuple key of the running impl, for
#'   `r8_next_impl()`.
#'
#' @return The impl function, with the tuple key of its provider as the
#'   attribute `r8_tuple`, or `NULL` when neither an impl nor a default
#'   applies. An ambiguous call or a stale argument raises
#'   `r8_dispatch_error`.
#'
#' @keywords internal
#' @noRd
resolve_tuple <- function(trait_name, method, args, above = NULL) {
  types <- vapply(
    args,
    function(a) {
      if (inherits(a, "r8_value")) attr(a, "r8_type") else NA_character_
    },
    character(1)
  )
  above_positions <- if (!is.null(above)) {
    strsplit(above, ",", fixed = TRUE)[[1L]]
  }
  r <- r8_dispatch_resolve(trait_name, types, above_positions)
  default <- .r8_impls[[paste(trait_name, method, "", sep = "|")]]
  switch(
    r[["kind"]],
    impl = {
      key <- tuple_key(r[["tuple"]])
      fn <- .r8_impls[[paste(trait_name, method, key, sep = "|")]] %||% default
      if (!is.null(fn)) {
        attr(fn, "r8_tuple") <- key
      }
      fn
    },
    default = if (is.null(above) || nzchar(above)) default,
    ambiguous = r8_abort(
      "The call of `",
      trait_name,
      "` (method `",
      method,
      "`) on (",
      paste(
        ifelse(is.na(types), "a value of no R8 type", types),
        collapse = ", "
      ),
      ") is ambiguous",
      if (!is.null(above)) " after the running impl",
      ": the impls for (",
      paste(r[["tuple"]], collapse = ", "),
      ") and for (",
      paste(r[["other"]], collapse = ", "),
      ") are each more specific in one position; ",
      if (is.null(above)) {
        paste0("implement it for (", paste(r[["meet"]], collapse = ", "), ")")
      } else {
        "name the impl to continue to, as r8_next_impl(Type, ...)"
      },
      " [dispatch_ambiguous].",
      class = "r8_dispatch_error"
    ),
    stale = stale_abort(
      r[["stale"]],
      ancestry(r[["stale"]])[["parent"]],
      "r8_dispatch_error"
    )
  )
} # /R8::resolve_tuple


# %% r8_ambiguities ----
#' The ambiguous pairs of impls of a generic or trait
#'
#' Lists the pairs of registered impls of a generic or trait that are each
#' more specific in one dispatched position, with no impl at their meet, the
#' tuple whose impl resolves them. A package's tests call it to check that
#' its impls, with those of the packages it uses, leave no call ambiguous.
#'
#' @param x A generic of class `r8_generic`, or a trait object.
#'
#' @return A data frame with one row per pair: `first` and `second`, the two
#'   impls' tuples, `meet`, the tuple to implement, each written as
#'   `"(ns::A, ns::B)"`, and `namespaces`, the namespaces that declare them.
#'   Zero rows when no call is ambiguous.
#'
#' @examples
#' op <- options(r8.namespace = "demo")
#' A <- new_struct("A", fields = list(a = type_double()))
#' A1 <- new_struct("A1", fields = list(a1 = type_double()), extends = A)
#' combine <- generic("combine", c("x", "y"), dispatch = c("x", "y"))
#' implement(combine, A1, A) <- function(x, y) "A1, A"
#' implement(combine, A, A1) <- function(x, y) "A, A1"
#' r8_ambiguities(combine)
#' options(op)
#'
#' @export
r8_ambiguities <- function(x) {
  trait_name <- if (inherits(x, "r8_generic")) {
    attr(x, "r8_trait")
  } else if (inherits(x, "r8_trait")) {
    unclass(x)[["name"]]
  } else {
    r8_abort(
      "`x` is a generic of class `r8_generic` or a trait object.",
      class = "r8_type_error"
    )
  }
  r8_dispatch_ambiguities(trait_name)
} # /R8::r8_ambiguities
