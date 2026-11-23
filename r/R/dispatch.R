# dispatch.R
# ::R8::
# 2026- EDG rtemis.org

# Multiple dispatch in R: the dispatched arguments of a generic or trait
# method, the tuples of types an impl names, the keys of the dispatch tables,
# and the selection of a call's impl by the runtime's rule of dispatch.
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
  # The arguments of `implement<-` after the types it names.
  reserved <- intersect(dispatch %||% args[[1L]], c("return_type", "value"))
  if (length(reserved) > 0L) {
    r8_abort(
      "`",
      label,
      "` dispatches on `",
      reserved[[1L]],
      "`, which names an argument of implement(); name the parameter ",
      "otherwise.",
      class = "r8_declaration_error"
    )
  }
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
#' @param types List: R8 type objects or host classes (`position_of()`), each
#'   with a name or without one.
#' @param dispatch Character: The dispatched argument names.
#' @param label Character: The generic or trait, for messages.
#'
#' @return Character: One position key per dispatched argument, a qualified
#'   type name, a host class's name in double quotes, or `"any"`, with the
#'   positions' IR as the attribute `ir`.
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
  ir <- stats::setNames(
    rep(list(list(kind = "any")), length(dispatch)),
    dispatch
  )
  for (n in named) {
    p <- position_of(types[[match(n, nms)]])
    out[[n]] <- p[["key"]]
    ir[[n]] <- p[["ir"]]
  }
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
  for (k in seq_along(unnamed)) {
    p <- position_of(unnamed[[k]])
    out[[free[[k]]]] <- p[["key"]]
    ir[[free[[k]]]] <- p[["ir"]]
  }
  structure(unname(out), ir = unname(ir))
} # /R8::impl_tuple


# The implicit classes: the names that `.class2()` derives from an object
# without a class attribute, from its type and `dim` attribute.
IMPLICIT_CLASSES <- c(
  "NULL",
  "integer",
  "double",
  "numeric",
  "character",
  "logical",
  "complex",
  "raw",
  "list",
  "function",
  "matrix",
  "array",
  "environment",
  "name",
  "call"
)


# %% host_class ----
#' A class of another class system
#'
#' Names a class that R8 did not declare: an S3 class, an implicit class
#' such as `"matrix"` or `"NULL"`, or an S4 class. It is a dispatched
#' position of an impl of an R8 generic or trait, and a field type inside
#' `option()`. A dispatched position matches an object whose class vector,
#' as [.class2()] reports it, contains the class; the nearer the class is to
#' the front of that vector, the more specific the impl. In `implement()`,
#' `impl()`, `r8_method()`, and `r8_next_impl()`, a string is shorthand for
#' `host_class()` of an S3 or implicit class, and an S4 class object, as
#' `methods::getClass()` returns it, for `host_class()` of that S4 class.
#'
#' An impl on a host class is declared by the package of the generic or
#' trait, or by the package that `package` names.
#'
#' @param class Character: The class name.
#' @param package Character: The package that defines the class; `""` for an
#'   S3 class of no package named and for an implicit class.
#' @param system Character: The class system. `"s3"`: a name in an object's
#'   `class` attribute. `"implicit"`: a name that `.class2()` derives from
#'   an object without a class attribute, from its type and `dim` attribute:
#'   `NULL`, `integer`, `double`, `numeric`, `character`, `logical`,
#'   `complex`, `raw`, `list`, `function`, `matrix`, `array`,
#'   `environment`, `name`, or `call`; a string naming one of these is
#'   implicit. `"s4"`: a class defined with `methods::setClass()`, which
#'   requires `package`; `r8_register()` checks at load that the package
#'   defines an S4 class of that name.
#'
#' @return An object of class `r8_host_class`, which is also a field type:
#'   `option(host_class("lm"))` declares a field that holds an object whose
#'   class vector contains `"lm"`, host-only, absent from documents and from
#'   the content hash.
#'
#' @examples
#' op <- options(r8.namespace = "hosts")
#' shape_text <- generic("shape_text", params(x))
#' implement(shape_text, "data.frame") <- function(x) paste(nrow(x), "rows")
#' implement(shape_text, host_class("matrix")) <- function(x) {
#'   paste(nrow(x), "by", ncol(x))
#' }
#' shape_text(data.frame(a = 1:3))
#' shape_text(diag(2))
#' field(option(host_class("lm")), context = TRUE)
#' options(op)
#'
#' @export
host_class <- function(
  class,
  package = "",
  system = c("s3", "s4", "implicit")
) {
  given <- !missing(system)
  system <- match.arg(system)
  if (
    !is.character(class) ||
      length(class) != 1L ||
      is.na(class) ||
      !nzchar(class)
  ) {
    r8_abort(
      "`class` is a class name, a string such as \"data.frame\".",
      class = "r8_declaration_error"
    )
  }
  if (!is.character(package) || length(package) != 1L || is.na(package)) {
    r8_abort(
      "`package` is a package name, or \"\" for an S3 class of no package.",
      class = "r8_declaration_error"
    )
  }
  if (grepl("::", class, fixed = TRUE)) {
    r8_abort(
      "`",
      class,
      "` is written as an R8 type's qualified name; a position or source ",
      "names an R8 type by its type object, and a class name has no `::`.",
      class = "r8_declaration_error"
    )
  }
  if (identical(class, "ANY")) {
    r8_abort(
      "A position that matches any value is left out of the impl, as ",
      "implement(f, y = B) for a method that specializes `y` only.",
      class = "r8_declaration_error"
    )
  }
  if (identical(class, "r8_value")) {
    r8_abort(
      "A host class names a class of another class system; an R8 value ",
      "matches the positions of its type and its ancestors.",
      class = "r8_declaration_error"
    )
  }
  if (identical(system, "s4") && !nzchar(package)) {
    r8_abort(
      "An S4 class names its package: host_class(\"",
      class,
      "\", package = \"pkg\", system = \"s4\").",
      class = "r8_declaration_error"
    )
  }
  if (class %in% IMPLICIT_CLASSES) {
    if (given && !identical(system, "implicit")) {
      r8_abort(
        "`",
        class,
        "` is an implicit class: host_class(\"",
        class,
        "\", system = \"implicit\"), or the string \"",
        class,
        "\".",
        class = "r8_declaration_error"
      )
    }
    system <- "implicit"
  } else if (identical(system, "implicit")) {
    r8_abort(
      "The implicit classes are ",
      paste(IMPLICIT_CLASSES, collapse = ", "),
      "; `",
      class,
      "` is not one.",
      class = "r8_declaration_error"
    )
  }
  if (identical(system, "implicit") && nzchar(package)) {
    r8_abort(
      "An implicit class belongs to no package; host_class(\"",
      class,
      "\") names it.",
      class = "r8_declaration_error"
    )
  }
  structure(
    list(
      kind = "host_class",
      host = "r",
      package = package,
      class = class,
      system = system
    ),
    class = c("r8_host_class", "r8_field_type")
  )
} # /R8::host_class


# %% print.r8_host_class ----
#' @export
print.r8_host_class <- function(x, ...) {
  h <- unclass(x)
  cat(
    "<R8 host class \"",
    h[["class"]],
    "\"",
    if (nzchar(h[["package"]])) paste0(" of ", h[["package"]]),
    ", ",
    toupper(h[["system"]]),
    ">\n",
    sep = ""
  )
  invisible(x)
} # /R8::print.r8_host_class


# %% as_host_class ----
#' The host class that a string or an S4 class object names
#'
#' @param x A string, an `r8_host_class`, or an S4 class object.
#'
#' @return An `r8_host_class`, or `NULL` when `x` names no host class.
#'
#' @keywords internal
#' @noRd
as_host_class <- function(x) {
  if (inherits(x, "r8_host_class")) {
    return(x)
  }
  if (methods::is(x, "classRepresentation")) {
    if (methods::extends(x, "oldClass")) {
      r8_abort(
        "`",
        x@className,
        "` is an S3 class registered with S4; name it as a string, \"",
        x@className,
        "\".",
        class = "r8_declaration_error"
      )
    }
    return(host_class(x@className, x@package, "s4"))
  }
  if (is.character(x) && length(x) == 1L && !is.na(x)) {
    return(host_class(x))
  }
  NULL
} # /R8::as_host_class


# %% is_position ----
#' Whether an object names one dispatched position
#'
#' @param x An object.
#'
#' @return Logical: `TRUE` for an R8 type object, a string, an
#'   `r8_host_class`, and an S4 class object.
#'
#' @keywords internal
#' @noRd
is_position <- function(x) {
  inherits(x, c("r8_type", "r8_host_class")) ||
    (is.character(x) && length(x) == 1L) ||
    methods::is(x, "classRepresentation")
} # /R8::is_position


# %% position_of ----
#' The dispatched position that an R8 type object or a host class names
#'
#' @param type An R8 type object, a string naming a class, an
#'   `r8_host_class`, or an S4 class object.
#'
#' @return List: `key`, the position as R8's tables key it (a qualified type
#'   name, or a host class's name in double quotes), and `ir`, the position
#'   in the IR.
#'
#' @keywords internal
#' @noRd
position_of <- function(type) {
  if (inherits(type, "r8_type")) {
    ensure_registered(type)
    name <- attr(type, "r8_name")
    return(list(key = name, ir = list(kind = "named", name = name)))
  }
  if (is.null(type)) {
    r8_abort(
      "A dispatched position names an R8 type object or a class; the class ",
      "of NULL is written as the string \"NULL\".",
      class = "r8_declaration_error"
    )
  }
  h <- as_host_class(type)
  if (is.null(h)) {
    r8_abort(
      "A dispatched position names an R8 type object or a class: a string ",
      "such as \"data.frame\", host_class(), or an S4 class object.",
      class = "r8_declaration_error"
    )
  }
  h <- unclass(h)
  ir <- list(kind = "host_class", host = "r", class = h[["class"]])
  if (nzchar(h[["package"]])) {
    ir[["package"]] <- h[["package"]]
  }
  ir[["system"]] <- h[["system"]]
  list(key = paste0("\"", h[["class"]], "\""), ir = ir)
} # /R8::position_of


# %% is_host_key ----
#' Whether a position key names a host class
#'
#' @param key Character: Position keys.
#'
#' @return Logical.
#'
#' @keywords internal
#' @noRd
is_host_key <- function(key) {
  startsWith(key, "\"")
} # /R8::is_host_key


# %% host_key_class ----
#' The class name of a host class's position key
#'
#' @param key Character: A position key, `"\"data.frame\""`.
#'
#' @return Character: The class name.
#'
#' @keywords internal
#' @noRd
host_key_class <- function(key) {
  substr(key, 2L, nchar(key) - 1L)
} # /R8::host_key_class


# %% tuple_ir ----
#' The `target` and `type_args` of an impl's IR from its positions
#'
#' @param positions Character: Position keys, from `impl_tuple()`, which
#'   carries each position's IR as the attribute `ir`.
#'
#' @return List: `target` and `type_args`.
#'
#' @keywords internal
#' @noRd
tuple_ir <- function(positions) {
  pos <- attr(positions, "ir")
  list(target = pos[[1L]], type_args = pos[-1L])
} # /R8::tuple_ir


# %% dispatch_key ----
#' The key of a value in a dispatch index
#'
#' The keys the C routines `find`, `find2`, and `find3` read, each of which
#' determines the value's chain in a position: the value's `r8_type`; for an
#' object with a class attribute, `class:` and the whole class vector joined
#' by the byte 0x1F, followed for an S4 object by 0x1F, `@`, and the class's
#' package; for an object without one, `type:`, `matrix:`, or `array:` by
#' its `dim` attribute, and its `typeof()`.
#'
#' @param x The value.
#'
#' @return Character, or `NULL` for a language object, whose class depends
#'   on its head, and for a key longer than 255 bytes: such values are not
#'   cached.
#'
#' @keywords internal
#' @noRd
dispatch_key <- function(x) {
  type <- attr(x, "r8_type", exact = TRUE)
  if (is.character(type) && length(type) == 1L) {
    return(type)
  }
  cls <- attr(x, "class", exact = TRUE)
  key <- if (!is.null(cls)) {
    k <- paste0("class:", paste(cls, collapse = "\x1f"))
    if (isS4(x)) {
      k <- paste0(k, "\x1f@", attr(cls, "package", exact = TRUE) %||% "")
    }
    k
  } else if (identical(typeof(x), "language")) {
    return(NULL)
  } else {
    d <- attr(x, "dim", exact = TRUE)
    prefix <- if (is.null(d)) {
      "type:"
    } else if (length(d) == 2L) {
      "matrix:"
    } else {
      "array:"
    }
    paste0(prefix, typeof(x))
  }
  if (nchar(key, type = "bytes") < 256L) key
} # /R8::dispatch_key


# %% select_tuple ----
#' The impl of a trait method for the values of its dispatched arguments
#'
#' Selects the impl through the runtime's rule of dispatch over the registered
#' impls (`r8_dispatch_select()`): the most specific applicable impl provides
#' the method, or the trait's default implementation when that impl omits it or
#' none applies.
#'
#' @param trait_name Character: The trait's qualified name.
#' @param method Character: The method name.
#' @param args List: The values of the dispatched arguments.
#' @param above Optional Character: The tuple key of the running impl, for
#'   `r8_next_impl()`.
#'
#' @return The impl function, or `NULL` when neither an impl nor a default
#'   applies. An ambiguous call or a stale argument raises
#'   `r8_dispatch_error`.
#'
#' @keywords internal
#' @noRd
select_tuple <- function(trait_name, method, args, above = NULL) {
  if (length(args) == 1L) {
    x <- args[[1L]]
    if (inherits(x, "r8_value")) {
      r8 <- TRUE
      types <- attr(x, "r8_type") %||% NA_character_
      if (!is.character(types) || length(types) != 1L) {
        r8_abort(
          "A value of class `r8_value` carries its type's qualified name as ",
          "the attribute `r8_type`, one string.",
          class = "r8_dispatch_error"
        )
      }
      chains <- list(NULL)
    } else {
      r8 <- FALSE
      types <- NA_character_
      chains <- list(.class2(x))
    }
  } else {
    r8 <- vapply(args, inherits, logical(1), "r8_value")
    types <- ifelse(
      r8,
      vapply(args, function(a) attr(a, "r8_type") %||% NA_character_, ""),
      NA_character_
    )
    chains <- lapply(seq_along(args), function(i) {
      if (!r8[[i]]) .class2(args[[i]])
    })
  }
  above_positions <- if (!is.null(above)) {
    strsplit(above, ",", fixed = TRUE)[[1L]]
  }
  r <- r8_dispatch_select(trait_name, types, chains, above_positions)
  if (is.character(r)) {
    key <- if (length(r) == 1L) r else tuple_key(r)
    fn <- .r8_impls[[paste(trait_name, method, key, sep = "|")]] %||%
      .r8_impls[[paste(trait_name, method, "", sep = "|")]]
    if (
      length(args) == 1L &&
        r8[[1L]] &&
        !identical(key, types[[1L]]) &&
        is_compiled_impl(fn)
    ) {
      r8_abort(
        "The compiled impl of `",
        trait_name,
        "` for `",
        key,
        "` takes a value of `",
        key,
        "`, not of its descendant `",
        types[[1L]],
        "`; implement `",
        trait_name,
        "` for `",
        types[[1L]],
        "`.",
        class = "r8_dispatch_error"
      )
    }
    return(fn)
  }
  if (is.null(r)) {
    return(
      if (is.null(above) || nzchar(above)) {
        .r8_impls[[paste(trait_name, method, "", sep = "|")]]
      }
    )
  }
  switch(
    r[["kind"]],
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
} # /R8::select_tuple


# %% r8_ambiguities ----
#' The ambiguous pairs of impls of a generic or trait
#'
#' Lists the pairs of registered impls of a generic or trait that are each
#' more specific in one dispatched position, with no impl at their meet, the
#' tuple whose impl disambiguates them. A package's tests call it to check that
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
#' combine <- generic("combine", params(x, y), dispatch = c("x", "y"))
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
