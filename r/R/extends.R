# extends.R
# ::R8::
# 2026- EDG rtemis.org

# spec: R8/class-system#inheritance

# %% r8_ancestors ----
#' The ancestors of a struct
#'
#' The parents of a struct through `extends`, the nearest first. A child's
#' values belong to the class of each ancestor, and an impl of an ancestor
#' applies to them where the child has none.
#'
#' @param type An R8 struct type object, or a value of one.
#'
#' @return Character: The qualified names of the ancestors; empty for a
#'   struct without `extends`. A stale struct, which extends an earlier
#'   declaration of its parent, raises `r8_type_error`.
#'
#' @examples
#' op <- options(r8.namespace = "demo")
#' Range <- new_struct("Range", fields = list(lo = type_double(), hi = type_double()))
#' Window <- new_struct("Window", fields = list(label = type_character()), extends = Range)
#' r8_ancestors(Window)
#' options(op)
#'
#' @export
r8_ancestors <- function(type) {
  name <- type_name_of(type)
  a <- ancestry(name)
  if (isTRUE(a[["stale"]])) {
    stale_abort(name, a[["parent"]], "r8_type_error")
  }
  a[["ancestors"]]
} # /R8::r8_ancestors


# %% type_name_of ----
#' The qualified name of a struct type object or of a value's type
#'
#' @param type An R8 struct type object, or a value of one.
#'
#' @return Character: The qualified name.
#'
#' @keywords internal
#' @noRd
type_name_of <- function(type) {
  if (inherits(type, "r8_struct_type")) {
    ensure_registered(type)
    return(attr(type, "r8_name"))
  }
  if (inherits(type, "r8_value") && is.null(attr(type, "r8_variant"))) {
    return(attr(type, "r8_type"))
  }
  r8_abort(
    "`type` is an R8 struct type object or a value of one.",
    class = "r8_type_error"
  )
} # /R8::type_name_of


# %% r8_upcast ----
#' The value of an ancestor that a child's value holds
#'
#' A child holds its ancestors' fields first, so the value of an ancestor is
#' those fields with the ancestor's type. The fields are the child's own
#' objects, not copies. The result is valid: the child's rules include the
#' ancestor's, which read the ancestor's fields only. `r8_convert(x, Parent)`
#' returns the same value.
#'
#' @param x A value of a struct with `extends`.
#' @param type An R8 struct type object: `x`'s type or one of its ancestors.
#'
#' @return A value of `type`; `x` itself when `type` is its type.
#'
#' @examples
#' op <- options(r8.namespace = "demo")
#' Range <- new_struct("Range", fields = list(lo = type_double(), hi = type_double()))
#' Window <- new_struct("Window", fields = list(label = type_character()), extends = Range)
#' r8_upcast(Window(lo = 1, hi = 2, label = "w"), Range)
#' options(op)
#'
#' @export
r8_upcast <- function(x, type) {
  if (!inherits(x, "r8_value") || !is.null(attr(x, "r8_variant"))) {
    r8_abort("`x` is an R8 struct value.", class = "r8_type_error")
  }
  if (!inherits(type, "r8_struct_type")) {
    r8_abort("`type` is an R8 struct type object.", class = "r8_type_error")
  }
  ensure_registered(type)
  target <- attr(type, "r8_name")
  source <- attr(x, "r8_type")
  if (identical(source, target)) {
    return(x)
  }
  a <- ancestry(source)
  if (isTRUE(a[["stale"]])) {
    stale_abort(source, a[["parent"]], "r8_type_error")
  }
  if (!target %in% a[["ancestors"]]) {
    r8_abort(
      "`",
      target,
      "` is not an ancestor of `",
      source,
      "`, whose ancestors are ",
      if (length(a[["ancestors"]]) == 0L) {
        "none"
      } else {
        paste0("`", a[["ancestors"]], "`", collapse = ", ")
      },
      ".",
      class = "r8_type_error"
    )
  }
  if (isTRUE(attr(type, "r8_native"))) {
    r8_abort(
      "`",
      source,
      "` is Rust-native; its value of `",
      target,
      "` is the embedded parent that `#[r8(extends)]` declares in Rust.",
      class = "r8_type_error"
    )
  }
  r8_upcast_value(x, target)
} # /R8::r8_upcast


# %% r8_next_impl ----
#' Call the next impl of the running method
#'
#' Called from the body of an impl, `r8_next_impl()` calls the impl that the
#' running one takes precedence over: that of the nearest ancestor, after
#' the type that provides the running impl, with an impl of the trait, or
#' else the trait's default. For a method that dispatches on several
#' arguments, the next impl is the most specific applicable impl less
#' specific than the running one; when two are, each more specific in one
#' position, `r8_next_impl(Type, ...)` names the one to continue to. It
#' passes the current values of the running impl's formals, so an impl may
#' change its arguments first, as with `NextMethod()`; generics in the next
#' impl dispatch on the values passed. Every impl that R8 registers
#' continues this way, an S3 method declared with `implement()` or `impl()`
#' included.
#'
#' @param ... Optional R8 type objects: the impl to continue to, one per
#'   dispatched argument, by position or by name, as `implement()` takes
#'   them; it applies to the current arguments.
#'
#' @return The value of the next impl. Without a next impl, with two next
#'   impls, and for a first argument whose type is not the provider of the
#'   running impl or one of its descendants, raises `r8_dispatch_error`.
#'
#' @examples
#' op <- options(r8.namespace = "demo")
#' Range <- new_struct("Range", fields = list(lo = type_double(), hi = type_double()))
#' Window <- new_struct("Window", fields = list(label = type_character()), extends = Range)
#' span_text <- generic("span_text", "x")
#' implement(span_text, Range) <- function(x) paste(x$lo, "to", x$hi)
#' implement(span_text, Window) <- function(x) paste(x$label, r8_next_impl())
#' span_text(Window(lo = 1, hi = 2, label = "w"))
#' options(op)
#'
#' @export
r8_next_impl <- function(...) {
  chosen <- list(...)
  caller <- sys.function(sys.parent())
  provider <- attr(caller, "r8_provider")
  if (is.null(provider)) {
    r8_abort(
      "r8_next_impl() is called from the body of an impl that R8 registers: ",
      "a method declared with implement() or impl().",
      class = "r8_dispatch_error"
    )
  }
  if (identical(provider[[1L]], "R8::From")) {
    r8_abort(
      "A From impl has no next impl; convert the value of an ancestor ",
      "explicitly, as r8_convert(r8_upcast(x, Parent), Target).",
      class = "r8_dispatch_error"
    )
  }
  frame <- parent.frame()
  formal_names <- names(formals(caller))
  trait_name <- provider[[1L]]
  method <- provider[[2L]]
  after <- provider[[3L]]
  dispatch <- method_dispatch(trait_name, method) %||% formal_names[[1L]]
  values <- lapply(dispatch, get, envir = frame)
  fn <- if (length(chosen) > 0L) {
    chosen_impl(trait_name, method, dispatch, values, chosen)
  } else if (length(dispatch) > 1L) {
    if (!nzchar(after)) {
      NULL
    } else {
      resolve_tuple(trait_name, method, values, above = after)
    }
  } else {
    next_in_chain(trait_name, method, values[[1L]], after)
  }
  if (
    is.null(fn) &&
      length(chosen) == 0L &&
      length(dispatch) == 1L &&
      is_host_key(after) &&
      maps_s3_generic(trait_name, method)
  ) {
    # The S3 method after the class, from the frame UseMethod() called.
    dispatch_frame <- sys.frame(sys.parent())
    if (!exists(".Generic", envir = dispatch_frame, inherits = FALSE)) {
      dispatch_frame <- sys.frame(sys.parent(2L))
    }
    return(do.call("NextMethod", list(), envir = dispatch_frame))
  }
  if (is.null(fn)) {
    r8_abort(
      "The impl of `",
      trait_name,
      "` (method `",
      method,
      "`) for (",
      paste(vapply(values, value_type_text, character(1)), collapse = ", "),
      ") has no next impl: no impl after `",
      if (nzchar(after)) {
        gsub(",", ", ", after, fixed = TRUE)
      } else {
        "the trait's default"
      },
      "` applies.",
      class = "r8_dispatch_error"
    )
  }
  args <- list()
  for (n in formal_names) {
    if (identical(n, "...")) {
      args <- c(args, eval(quote(list(...)), frame))
    } else if (!eval(call("missing", as.name(n)), frame)) {
      args[n] <- list(get(n, envir = frame))
    }
  }
  do.call(fn, args)
} # /R8::r8_next_impl


# %% maps_s3_generic ----
#' Whether a trait method is the method of an existing S3 generic
#'
#' @param trait_name Character: The trait's qualified name.
#' @param method Character: The method name.
#'
#' @return Logical.
#'
#' @keywords internal
#' @noRd
maps_s3_generic <- function(trait_name, method) {
  t <- .r8_traits[[trait_name]]
  if (is.null(t)) {
    return(FALSE)
  }
  g <- unclass(t)[["generics"]][[method]]
  is.function(g) && !inherits(g, "r8_generic") && !is_s4_generic(g)
} # /R8::maps_s3_generic


# %% method_dispatch ----
#' The dispatched arguments of a trait method
#'
#' @param trait_name Character: The trait's qualified name.
#' @param method Character: The method name.
#'
#' @return Character, or `NULL` for a method of an S3 generic.
#'
#' @keywords internal
#' @noRd
method_dispatch <- function(trait_name, method) {
  t <- .r8_traits[[trait_name]]
  if (is.null(t)) {
    return(NULL)
  }
  for (m in trait_ir(t)[["methods"]]) {
    if (identical(m[["name"]], method)) {
      return(dispatch_names(m[["params"]]))
    }
  }
  NULL
} # /R8::method_dispatch


# %% next_in_chain ----
#' The next impl of a method that dispatches on one argument
#'
#' @param trait_name Character: The trait's qualified name.
#' @param method Character: The method name.
#' @param x The value of the dispatched argument.
#' @param after Character: The type that provides the running impl, or
#'   `""` for the trait's default.
#'
#' @return The impl function, or `NULL`.
#'
#' @keywords internal
#' @noRd
next_in_chain <- function(trait_name, method, x, after) {
  r8 <- inherits(x, "r8_value")
  type <- value_type_text(x)
  if (nzchar(after)) {
    chain <- chain_keys(x)
    if (!after %in% chain) {
      r8_abort(
        "r8_next_impl() continues the impl for `",
        after,
        "` on a value of `",
        type,
        "`, which is neither `",
        after,
        "` nor one of its descendants.",
        class = "r8_dispatch_error"
      )
    }
  } else {
    return(NULL)
  }
  if (is.null(.r8_traits[[trait_name]])) {
    # The method of an S3 generic that implement() declares for an R8 type.
    return(impl_in_chain(trait_name, method, type, r8 = r8, after = after))
  }
  resolve_tuple(trait_name, method, list(x), above = after)
} # /R8::next_in_chain


# %% chain_keys ----
#' The position keys a value matches, most specific first
#'
#' @param x The value.
#'
#' @return Character: For an R8 value, its type and its ancestors; for
#'   another object, its class vector as `.class2()` gives it, each class in
#'   double quotes, as host-class positions are keyed.
#'
#' @keywords internal
#' @noRd
chain_keys <- function(x) {
  if (inherits(x, "r8_value")) {
    type <- attr(x, "r8_type")
    if (is.null(attr(x, "r8_variant"))) c(type, r8_ancestors(x)) else type
  } else {
    paste0("\"", .class2(x), "\"")
  }
} # /R8::chain_keys


# %% chosen_impl ----
#' The impl that `r8_next_impl(Type, ...)` names
#'
#' @param trait_name Character: The trait's qualified name.
#' @param method Character: The method name.
#' @param dispatch Character: The dispatched arguments.
#' @param values List: Their current values.
#' @param chosen List: The types named.
#'
#' @return The impl function, or the trait's default when the impl omits the
#'   method. An impl that is not declared, or that does not apply to the
#'   current arguments, raises `r8_dispatch_error`.
#'
#' @keywords internal
#' @noRd
chosen_impl <- function(trait_name, method, dispatch, values, chosen) {
  positions <- impl_tuple(chosen, dispatch, trait_name)
  key <- tuple_key(positions)
  if (
    !exists(
      paste(trait_name, key, sep = "|"),
      envir = .r8_impl_of,
      inherits = FALSE
    )
  ) {
    r8_abort(
      "`",
      trait_name,
      "` has no impl for (",
      paste(positions, collapse = ", "),
      ").",
      class = "r8_dispatch_error"
    )
  }
  for (i in seq_along(positions)) {
    p <- positions[[i]]
    v <- values[[i]]
    if (!identical(p, "any") && !p %in% chain_keys(v)) {
      r8_abort(
        "The impl of `",
        trait_name,
        "` for (",
        paste(positions, collapse = ", "),
        ") does not apply to `",
        dispatch[[i]],
        "`, a value of `",
        value_type_text(v),
        "`.",
        class = "r8_dispatch_error"
      )
    }
  }
  .r8_impls[[paste(trait_name, method, key, sep = "|")]] %||%
    .r8_impls[[paste(trait_name, method, "", sep = "|")]]
} # /R8::chosen_impl
