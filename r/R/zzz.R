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
#' `r8_hash()` through `r8_equal()`, `to_json()` and `r8_from_json()`, and a
#' type whose required fields have defaults is constructed with no
#' arguments.
#'
#' @param source Character: The class or qualified R8 type converted from.
#' @param x,y Values of one R8 type, for `r8_cmp()`.
#'
#' @return A trait object.
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
#' Calls the `From(source)` impl of `type` for the class of `x`, or for its
#' R8 type when `x` is an R8 value, and checks that the result is a value of
#' `type`.
#'
#' @param x The value to convert.
#' @param type An R8 type object.
#'
#' @return A value of `type`.
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
  if (inherits(x, "r8_value") && identical(attr(x, "r8_type"), target)) {
    return(x)
  }
  sources <- if (inherits(x, "r8_value")) {
    attr(x, "r8_type")
  } else {
    unique(c(class(x), if (is.numeric(x)) "numeric"))
  }
  for (s in sources) {
    fn <- .r8_impls[[paste("R8::From", s, target, sep = "|")]]
    if (!is.null(fn)) {
      out <- fn(x)
      if (
        !inherits(out, "r8_value") || !identical(attr(out, "r8_type"), target)
      ) {
        r8_abort(
          "The From(\"",
          s,
          "\") impl of `",
          target,
          "` returned another kind of value.",
          class = "r8_dispatch_error"
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
#' under a name of its choice, such as `as_granges()`.
#'
#' @param type An R8 type object.
#'
#' @return Function.
#'
#' @examples
#' op <- options(r8.namespace = "demo")
#' Pct <- new_struct("Pct2", fields = list(p = type_double(min = 0, max = 100)))
#' impl(From("numeric"), Pct, methods = list(from = function(x) Pct(p = x * 100)))
#' as_pct <- r8_converter(Pct)
#' as_pct(0.5)$p
#' options(op)
#'
#' @export
r8_converter <- function(type) {
  force(type)
  function(x) r8_convert(x, type)
} # /R8::r8_converter


# %% @.r8_struct_type ----
#' Items of a struct type
#'
#' `Type@from(x)` converts `x` with the type's `From` impls.
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

# %% .onLoad ----
.onLoad <- function(libname, pkgname) {
  list2env(r8_native_symbols(), envir = .r8_native)
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
