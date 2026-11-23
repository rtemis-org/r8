# zzz.R
# ::R8::
# 2026- EDG rtemis.org

# The standard traits, declared by R8 in its namespace `R8`. They are built
# when R8 is installed and registered when it loads; this file is sourced
# last, after the functions it uses.

# %% Display ----
#' Standard traits
#'
#' Traits that R8 declares in its namespace `R8`, which a type implements
#' with `impl()` like any trait:
#'
#' - `Display`: `format(x, ...)`, which `print()` shows for a type without
#'   a `Print` impl.
#' - `Print`: `print(x, ...)`, which `print()` calls before it returns `x`
#'   invisibly; `format()` keeps the `Display` text.
#' - `Debug`: `str(object, ...)`.
#' - `Dim`: `dim(x)`; `nrow()` and `ncol()` follow.
#' - `PartialOrd`: `cmp(x, y)`, which returns a negative number, zero, or a
#'   positive number as `x` sorts before, with, or after `y`; `<`, `<=`, `>`,
#'   and `>=` use it, and `r8_cmp()` calls it.
#' - `From(source)`: `from(x)`, which converts a value of the class or R8
#'   type `source` to the implementing type; `r8_convert()`, `Type@from()`,
#'   and `r8_converter()` use it.
#'
#' R8 provides `format()`, `print()`, and `str()` for every value, `==` and
#' `r8_hash()` through `r8_equal()`, `r8_to_json()` and `r8_from_json()`, and a
#' type whose required fields have defaults is constructed with no
#' arguments.
#'
#' @param source Character: The class or qualified R8 type converted from.
#' @param x,y Values of one R8 type, for `r8_cmp()`.
#'
#' @return A trait object. `From()` returns one of the internal class
#'   `r8_from_trait`, which holds its type parameter `source`.
#'
#' @examples
#' op <- options(r8.namespace = "demo")
#' Money <- new_struct("Money", fields = list(cents = type_integer()))
#' impl(Display, Money, methods = list(format = function(x, ...) sprintf("$%.2f", x$cents / 100)))
#' impl(PartialOrd, Money, methods = list(cmp = function(x, y) x$cents - y$cents))
#' impl(From("numeric"), Money, methods = list(
#'   from = function(x) Money(cents = as.integer(round(x * 100)))
#' ))
#' m <- r8_convert(1.5, Money)
#' print(m)
#' m < Money(cents = 200L)
#' options(op)
#'
#' @name standard_traits
#' @export
Display <- build_trait(
  "Display",
  list(
    format = trait_method(
      function(x, ...) NULL,
      description = "The text that print() shows."
    )
  ),
  "A value's text for users.",
  NULL,
  "R8",
  environment()
)

#' @rdname standard_traits
#' @export
Debug <- build_trait(
  "Debug",
  list(
    str = trait_method(
      function(object, ...) NULL,
      description = "The structure, for developers."
    )
  ),
  "A value's structure for developers.",
  NULL,
  "R8",
  environment()
)

#' @rdname standard_traits
#' @export
Dim <- build_trait(
  "Dim",
  list(dim = trait_method(function(x) NULL, description = "The dimensions.")),
  "A value's dimensions.",
  NULL,
  "R8",
  environment()
)

#' @rdname standard_traits
#' @export
PartialOrd <- build_trait(
  "PartialOrd",
  list(
    cmp = trait_method(
      function(x, y) NULL,
      description = "The order of x relative to y."
    )
  ),
  "An order between values of a type.",
  NULL,
  "R8",
  environment()
)

#' @rdname standard_traits
#' @export
Print <- build_trait(
  "Print",
  list(
    print = trait_method(
      function(x, ...) NULL,
      description = "Writes the value to the console."
    )
  ),
  "A value's console output, in place of its format().",
  NULL,
  "R8",
  environment()
)

#' @rdname standard_traits
#' @export
r8_cmp <- unclass(PartialOrd)[["generics"]][["cmp"]]

# The declaration of `From`, one trait whose impls carry their source as a
# type argument.
FROM_IR <- list(
  name = "From",
  description = "Conversion from a source class or type.",
  type_params = 1L,
  methods = list(list(
    name = "from",
    description = "Converts x to the implementing type.",
    params = list("x")
  ))
)

#' @rdname standard_traits
#' @export
From <- function(source) {
  if (!is.character(source) || length(source) != 1L || !nzchar(source)) {
    r8_abort(
      "`source` is one class or qualified type name.",
      class = "r8_declaration_error"
    )
  }
  structure(
    list(
      name = "R8::From",
      ir = FROM_IR,
      generics = list(),
      defaults = list(),
      source = source
    ),
    class = c("r8_from_trait", "r8_trait")
  )
} # /R8::From


# %% r8_convert ----
#' Convert a value to an R8 type
#'
#' Returns `x` when it is a value of `type`, and its upcast when `type` is an
#' ancestor of its type (`r8_upcast()`). Otherwise calls the `From(source)`
#' impl of `type` for the nearest source with one: for an R8 value, its type,
#' then its ancestors through `extends`, nearest first; for any other object,
#' its classes, then `"numeric"` for a number. The impl receives `x`
#' unchanged, and its result must be a value of `type`. Conversions are not
#' chained.
#'
#' @param x The value to convert.
#' @param type An R8 type object.
#'
#' @return A value of `type`. A source without an impl raises
#'   `r8_dispatch_error`, and an impl's result of another type
#'   `r8_type_error`.
#'
#' @examples
#' op <- options(r8.namespace = "demo")
#' Pct <- new_struct("Pct", fields = list(p = type_double(min = 0, max = 100)))
#' impl(From("numeric"), Pct, methods = list(from = function(x) Pct(p = x * 100)))
#' r8_convert(0.25, Pct)$p
#' options(op)
#'
#' @export
r8_convert <- function(x, type) {
  if (!inherits(type, "r8_type")) {
    r8_abort("`type` is an R8 type object.", class = "r8_type_error")
  }
  target <- attr(type, "r8_name")
  r8 <- inherits(x, "r8_value")
  if (r8 && identical(attr(x, "r8_type"), target)) {
    return(x)
  }
  struct <- r8 && is.null(attr(x, "r8_variant"))
  ancestors <- if (struct) r8_ancestors(x) else character()
  # A child converts to an ancestor by upcast.
  if (target %in% ancestors) {
    return(r8_upcast(x, type))
  }
  sources <- if (r8) {
    c(attr(x, "r8_type"), ancestors)
  } else {
    unique(c(class(x), if (is.numeric(x)) "numeric"))
  }
  for (s in sources) {
    fn <- .r8_impls[[paste("R8::From", s, target, sep = "|")]]
    if (!is.null(fn)) {
      if (r8 && !identical(s, attr(x, "r8_type")) && is_compiled_impl(fn)) {
        r8_abort(
          "The compiled From(\"",
          s,
          "\") impl of `",
          target,
          "` takes a value of `",
          s,
          "`, not of its descendant `",
          attr(x, "r8_type"),
          "`; convert r8_upcast(x, ",
          sub("^.*::", "", s),
          "), or declare From(\"",
          attr(x, "r8_type"),
          "\").",
          class = "r8_dispatch_error"
        )
      }
      out <- fn(x)
      if (
        !inherits(out, "r8_value") || !identical(attr(out, "r8_type"), target)
      ) {
        r8_abort(
          "The From(\"",
          s,
          "\") impl of `",
          target,
          "` returned ",
          if (inherits(out, "r8_value")) {
            paste0("a value of `", attr(out, "r8_type"), "`")
          } else {
            paste0(
              "an object of class ",
              paste0("\"", class(out), "\"", collapse = ", ")
            )
          },
          "; a From impl returns a value of its target.",
          class = "r8_type_error"
        )
      }
      return(out)
    }
  }
  r8_abort(
    "`",
    target,
    "` has no From impl for ",
    paste0("\"", sources, "\"", collapse = ", "),
    ".",
    class = "r8_dispatch_error"
  )
} # /R8::r8_convert


# %% r8_converter ----
#' A conversion function for an R8 type
#'
#' Returns `function(x) r8_convert(x, type)`, which a type's package exports
#' under a name of its choice, such as `as_granges()`. The converter is also
#' the generic of the conversions to `type`: `implement(as_target, Source)
#' <- function(x) ...` declares the conversion from the R8 type `Source`, and
#' `implement(as_target, "character") <- function(x) ...` the conversion from
#' a class, each an impl of `From(source)` for `type`.
#'
#' @param type An R8 type object.
#'
#' @return `r8_converter()`: A function of class `r8_converter`.
#'   `print()`: `x`, invisibly.
#'
#' @examples
#' op <- options(r8.namespace = "demo")
#' Pct <- new_struct("Pct2", fields = list(p = type_double(min = 0, max = 100)))
#' as_pct <- r8_converter(Pct)
#' implement(as_pct, "numeric") <- function(x) Pct(p = x * 100)
#' as_pct(0.5)$p
#' as_pct
#' options(op)
#'
#' @export
r8_converter <- function(type) {
  if (!inherits(type, "r8_type")) {
    r8_abort("`type` is an R8 type object.", class = "r8_type_error")
  }
  force(type)
  structure(
    function(x) r8_convert(x, type),
    class = c("r8_converter", "function")
  )
} # /R8::r8_converter


# %% converter_target ----
#' The target type object of a converter
#'
#' @param f A function of class `r8_converter`.
#'
#' @return An R8 type object.
#'
#' @keywords internal
#' @noRd
converter_target <- function(f) {
  get("type", envir = environment(f), inherits = FALSE)
} # /R8::converter_target


# %% print.r8_converter ----
#' @rdname r8_converter
#' @param x A function of class `r8_converter`.
#' @param ... Not used.
#' @export
print.r8_converter <- function(x, ...) {
  target <- attr(converter_target(x), "r8_name")
  cat("<r8 converter ", target, ">\n", sep = "")
  keys <- ls(.r8_impls, all.names = TRUE)
  keys <- keys[
    startsWith(keys, "R8::From|") & endsWith(keys, paste0("|", target))
  ]
  sources <- substr(
    keys,
    nchar("R8::From|") + 1L,
    nchar(keys) - nchar(target) - 1L
  )
  if (length(sources) > 0L) {
    cat("Methods for: ", paste(sort(sources), collapse = ", "), "\n", sep = "")
  }
  invisible(x)
} # /R8::print.r8_converter


# %% declare_conversion ----
#' Declare a conversion through a converter
#'
#' `implement(converter, source) <- fn` lowers to `impl(From(source),
#' Target, methods = list(from = fn))`.
#'
#' @param converter A function of class `r8_converter`.
#' @param source An R8 type object, or Character: a class name.
#' @param fn Function: The conversion, of one argument `x`.
#' @param env Environment: The declaring environment.
#'
#' @return `NULL`, invisibly.
#'
#' @keywords internal
#' @noRd
declare_conversion <- function(converter, source, fn, env) {
  target <- converter_target(converter)
  name <- if (inherits(source, "r8_type")) {
    ensure_registered(source)
    attr(source, "r8_name")
  } else if (
    is.character(source) &&
      length(source) == 1L &&
      !is.na(source) &&
      nzchar(source)
  ) {
    source
  } else {
    r8_abort(
      "A converter's method is declared for the class of `x`: an R8 type ",
      "object or a class name, such as implement(as_",
      tolower(sub("^.*::", "", attr(target, "r8_name"))),
      ", \"character\") <- function(x) ...",
      class = "r8_declaration_error"
    )
  }
  check_method_formals(
    fn,
    "x",
    paste0("r8_converter(", sub("^.*::", "", attr(target, "r8_name")), ")")
  )
  impl_declare(From(name), target, list(from = fn), NULL, env)
} # /R8::declare_conversion


# %% @.r8_struct_type ----
#' Items of a struct type
#'
#' `Type@from(x)` converts `x` with the type's `From` impls, as
#' `r8_converter(Type)(x)` does.
#'
#' @param object A struct type object.
#' @param name The item's name.
#'
#' @return The item.
#'
#' @rdname new_struct
#' @export
`@.r8_struct_type` <- function(object, name) {
  name <- as.character(substitute(name))
  if (identical(name, "from")) {
    return(r8_converter(object))
  }
  r8_abort(
    "`",
    name,
    "` is not an item of `",
    attr(object, "r8_name"),
    "`.",
    class = "r8_dispatch_error"
  )
} # /R8::@.r8_struct_type


# The native symbols of R8's C routines that R calls through `.Call()`
# (`r8_native_symbols()`), set when the package loads.
.r8_native <- new.env(parent = emptyenv())

# The native symbol of the C routine `invoke`, bound when the package loads,
# which a compiled method's wrapper calls.
.r8_c_invoke <- NULL

# %% .onLoad ----
.onLoad <- function(libname, pkgname) {
  list2env(r8_native_symbols(), envir = .r8_native)
  ns <- topenv()
  assign(".r8_c_find", .r8_native[["find"]], envir = ns)
  assign(".r8_c_replaced", .r8_native[["replaced"]], envir = ns)
  assign(".r8_c_find2", .r8_native[["find2"]], envir = ns)
  assign(".r8_c_find3", .r8_native[["find3"]], envir = ns)
  assign(".r8_c_invoke", .r8_native[["invoke"]], envir = ns)
  claim_namespace("R8", "R8")
  for (t in list(Display, Debug, Dim, PartialOrd, Print)) {
    register_trait(t, "R8")
  }
  register_trait(
    structure(
      list(
        name = "R8::From",
        ir = FROM_IR,
        generics = list(),
        defaults = list()
      ),
      class = "r8_trait"
    ),
    "R8"
  )
}
