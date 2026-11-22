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
#' else the trait's default. It passes the current values of the running
#' impl's formals, so an impl may change its arguments first, as with
#' `NextMethod()`; generics in the next impl dispatch on the value passed.
#' Every impl that R8 registers continues this way, an S3 method declared
#' with `implement()` or `impl()` included.
#'
#' @return The value of the next impl. Without a next impl, and for a first
#'   argument whose type is not the provider of the running impl or one of
#'   its descendants, raises `r8_dispatch_error`.
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
r8_next_impl <- function() {
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
  x <- get(formal_names[[1L]], envir = frame)
  r8 <- inherits(x, "r8_value")
  type <- if (r8) attr(x, "r8_type") else class(x)[1L]
  after <- provider[[3L]]
  if (nzchar(after)) {
    chain <- if (r8 && is.null(attr(x, "r8_variant"))) {
      c(type, r8_ancestors(x))
    } else {
      class(x)
    }
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
  }
  fn <- impl_in_chain(
    provider[[1L]],
    provider[[2L]],
    type,
    r8 = r8,
    after = after
  )
  if (is.null(fn)) {
    r8_abort(
      "The impl of `",
      provider[[1L]],
      "` (method `",
      provider[[2L]],
      "`) for `",
      type,
      "` has no next impl: no ancestor after `",
      if (nzchar(after)) after else "the trait's default",
      "` implements it.",
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
