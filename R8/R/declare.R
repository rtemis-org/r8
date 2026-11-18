# declare.R
# ::R8::
# 2026- EDG rtemis.org

# Declared types, by qualified name, for this session. The registry of
# declarations lives in R8's shared object; this environment holds the R
# type objects that refer to it.
.r8_types <- new.env(parent = emptyenv())


# %% field ----
#' Declare a field
#'
#' The facts of one field of a struct, a struct variant, or a table. A bare
#' field type is the short form of `field(type)`.
#'
#' @param type An `r8_field_type` or an R8 type: The field's type.
#' @param default Optional: The default value, used when the field is absent.
#' @param description Character: What the field holds.
#' @param sensitive Logical: Mask the value in printing, events, errors, and
#'   differences.
#' @param unit Optional Character: A UCUM unit code, such as `"mg"`.
#' @param external Logical: `write_json()` writes the value to an Arrow IPC
#'   file.
#' @param unique Logical: A table column whose values are distinct.
#' @param optional Logical: A table column that may be absent.
#' @param extensions Optional named list: Namespaced extensions, such as
#'   `list("rtemis:tunable" = TRUE)`, which R8 stores and does not interpret.
#'
#' @return An `r8_field`.
#'
#' @examples
#' field(type_double(exclusive_min = 0), default = 1, description = "Cost.")
#'
#' @export
field <- function(
  type,
  default = NULL,
  description = "",
  sensitive = FALSE,
  unit = NULL,
  external = FALSE,
  unique = FALSE,
  optional = FALSE,
  extensions = NULL
) {
  structure(
    list(
      type = as_field_type(type),
      default = default,
      has_default = !missing(default),
      description = description,
      sensitive = isTRUE(sensitive),
      unit = unit,
      external = isTRUE(external),
      unique = isTRUE(unique),
      optional = isTRUE(optional),
      extensions = extensions
    ),
    class = "r8_field"
  )
} # /R8::field


# %% as_field ----
#' A field from a field object or a field type
#'
#' @param x An `r8_field`, an `r8_field_type`, or an R8 type.
#'
#' @return An `r8_field`.
#'
#' @keywords internal
#' @noRd
as_field <- function(x) {
  if (inherits(x, "r8_field")) x else field(x)
} # /R8::as_field


# %% default_json ----
#' A default value in its JSON form, as a list for `r8_ir_json()`
#'
#' @param x The default value.
#' @param type An `r8_field_type`.
#'
#' @return The value in the form `r8_ir_json()` writes as the field's JSON.
#'
#' @keywords internal
#' @noRd
default_json <- function(x, type) {
  kind <- type[["kind"]]
  if (is.null(x)) {
    return(NULL)
  }
  if (kind == "option") {
    return(default_json(x, type[["inner"]]))
  }
  if (kind == "vec") {
    return(lapply(as.list(x), default_json, type = type[["element"]]))
  }
  if (kind == "named") {
    return(value_json(x))
  }
  x
} # /R8::default_json


# %% value_json ----
#' The JSON form of an R8 enum value, for a default
#'
#' @param x An R8 value.
#'
#' @return A string for an enum of unit variants; otherwise a named list
#'   holding the tag member and the variant's fields.
#'
#' @keywords internal
#' @noRd
value_json <- function(x) {
  if (!inherits(x, "r8_value")) {
    r8_abort(
      "The default of a field of an R8 type is a value of that type.",
      class = "r8_declaration_error"
    )
  }
  type <- .r8_types[[attr(x, "r8_type")]]
  variant <- attr(x, "r8_variant")
  if (is.null(variant)) {
    r8_abort(
      "Struct values as defaults are constructed with the type.",
      class = "r8_declaration_error"
    )
  }
  ir <- attr(type, "r8_ir")
  unit_only <- all(vapply(
    ir[["variants"]],
    function(v) v[["shape"]] == "unit",
    logical(1)
  ))
  if (unit_only) {
    return(variant)
  }
  out <- list(variant)
  names(out) <- ir[["tag"]]
  out
} # /R8::value_json


# %% field_ir ----
#' The IR of a field
#'
#' @param name Character: The field name.
#' @param f An `r8_field`.
#'
#' @return A list in the form of the IR's field declaration.
#'
#' @keywords internal
#' @noRd
field_ir <- function(name, f) {
  out <- list(name = name, type = unclass(strip_classes(f[["type"]])))
  if (isTRUE(f[["has_default"]])) {
    out[["default"]] <- default_json(f[["default"]], f[["type"]])
  }
  out[["description"]] <- f[["description"]]
  if (f[["sensitive"]]) {
    out[["sensitive"]] <- TRUE
  }
  if (!is.null(f[["unit"]])) {
    out[["unit"]] <- f[["unit"]]
  }
  if (f[["external"]]) {
    out[["external"]] <- TRUE
  }
  if (f[["unique"]]) {
    out[["unique"]] <- TRUE
  }
  if (f[["optional"]]) {
    out[["optional"]] <- TRUE
  }
  if (!is.null(f[["extensions"]])) {
    out[["extensions"]] <- f[["extensions"]]
  }
  out
} # /R8::field_ir


# %% strip_classes ----
#' A field type without its classes, recursively
#'
#' @param x An `r8_field_type` or a list.
#'
#' @return A plain list.
#'
#' @keywords internal
#' @noRd
strip_classes <- function(x) {
  if (is.list(x)) {
    x <- lapply(unclass(x), strip_classes)
  }
  x
} # /R8::strip_classes


# %% fields_ir ----
#' The IR of the fields given to a constructor
#'
#' @param fields Named list: Fields or field types.
#' @param what Character: Where the fields belong, for messages.
#'
#' @return A list of field declarations.
#'
#' @keywords internal
#' @noRd
fields_ir <- function(fields, what) {
  if (
    length(fields) > 0L &&
      (is.null(names(fields)) || any(!nzchar(names(fields))))
  ) {
    r8_abort(
      "Every field of ",
      what,
      " is named: name = type_double() or name = field(...).",
      class = "r8_declaration_error"
    )
  }
  unname(Map(function(n, f) field_ir(n, as_field(f)), names(fields), fields))
} # /R8::fields_ir


# %% check_members ----
#' Checks a named list of members
#'
#' @param x List: The members.
#' @param what Character: The argument name.
#' @param owner Character: What the members belong to, for messages.
#'
#' @return `x`, invisibly.
#'
#' @keywords internal
#' @noRd
check_members <- function(x, what, owner) {
  if (!is.list(x)) {
    r8_abort(
      "`",
      what,
      "` of ",
      owner,
      " is a named list.",
      class = "r8_declaration_error"
    )
  }
  if (length(x) > 0L) {
    n <- names(x)
    if (is.null(n) || any(!nzchar(n))) {
      r8_abort(
        "Every member of `",
        what,
        "` of ",
        owner,
        " is named: list(name = ...).",
        class = "r8_declaration_error"
      )
    }
    dup <- unique(n[duplicated(n)])
    if (length(dup) > 0L) {
      r8_abort(
        "`",
        what,
        "` of ",
        owner,
        " names `",
        paste(dup, collapse = "`, `"),
        "` twice.",
        class = "r8_declaration_error"
      )
    }
  }
  invisible(x)
} # /R8::check_members


# %% declare ----
#' Check and register a declaration, and make its type object
#'
#' @param ir List: The IR declaration.
#' @param namespace Character: The namespace.
#' @param class Character: The type object's class.
#' @param formals List: The constructor's formals.
#'
#' @return The type object.
#'
#' @keywords internal
#' @noRd
declare <- function(ir, env, class, formals) {
  namespace <- declaring_namespace(env)
  package <- declaring_package(env)
  claim_namespace(namespace, package)
  qname <- paste0(namespace, "::", ir[["name"]])
  field_names <- names(formals)
  ctor <- as.function(
    c(
      formals,
      bquote(r8_construct(
        .(qname),
        mget(.(field_names), envir = environment())
      ))
    ),
    envir = asNamespace("R8")
  )
  type <- structure(
    ctor,
    class = c(class, "r8_type", "function"),
    r8_name = qname,
    r8_ir = ir
  )
  register_type(type, namespace)
  record_declaration(
    env,
    list(kind = "type", type = type, namespace = namespace)
  )
  type
} # /R8::declare


# %% register_type ----
#' Check a type's declaration and register it
#'
#' @param type A type object.
#' @param namespace Character: Its namespace.
#'
#' @return `NULL`, invisibly.
#'
#' @keywords internal
#' @noRd
register_type <- function(type, namespace) {
  ir <- attr(type, "r8_ir")
  problems <- r8_declare_type(r8_ir_json(ir), namespace)
  if (length(problems[["code"]]) > 0L) {
    lines <- paste0(
      "- ",
      problems[["message"]],
      " [",
      problems[["code"]],
      ifelse(
        nzchar(problems[["path"]]),
        paste0(" at ", problems[["path"]]),
        ""
      ),
      "]"
    )
    r8_abort(
      "Declaration of `",
      namespace,
      "::",
      ir[["name"]],
      "` failed:\n",
      paste(lines, collapse = "\n"),
      class = "r8_declaration_error"
    )
  }
  assign(attr(type, "r8_name"), type, envir = .r8_types)
  invisible(NULL)
} # /R8::register_type


# %% new_struct ----
#' Declare a struct
#'
#' A product type: named fields, each with a type and per-field facts, and
#' rules over them. The type object constructs values: `Svm(cost = 2)`.
#' Types declared in a package belong to its namespace (the `DESCRIPTION`
#' field `Config/R8/namespace`, or the package name); types declared at the
#' console belong to the option `r8.namespace`, `"global"` by default.
#'
#' @param name Character: The type name, an R8 name such as `"Svm"`.
#' @param fields Named list: The fields, `field(...)` calls or bare field
#'   types.
#' @param description Character: What the type is.
#' @param version Integer: The type's version, from 1.
#' @param rules List: `rule()` calls over the fields.
#'
#' @return The type object, of class `r8_struct_type`.
#'
#' @examples
#' op <- options(r8.namespace = "demo")
#' Point <- new_struct("Point", fields = list(x = type_double(), y = type_double()),
#'   description = "A point.")
#' options(op)
#'
#' @export
new_struct <- function(
  name,
  fields = list(),
  description = "",
  version = 1L,
  rules = list()
) {
  if (!is_name(name)) {
    r8_abort(
      "A type name is an R8 name, such as \"Svm\".",
      class = "r8_declaration_error"
    )
  }
  check_members(fields, "fields", paste0("`", name, "`"))
  ir <- list(
    kind = "struct",
    name = name,
    description = description,
    version = as.integer(version),
    fields = fields_ir(fields, paste0("`", name, "`"))
  )
  if (length(rules) > 0L) {
    ir[["rules"]] <- lapply(rules, function(r) unclass(r))
  }
  formals <- lapply(fields, function(f) {
    f <- as_field(f)
    if (isTRUE(f[["has_default"]])) f[["default"]] else NULL
  })
  declare(ir, parent.frame(), "r8_struct_type", formals)
} # /R8::new_struct


# %% variant ----
#' Declare an enum variant
#'
#' A unit variant has no fields; a struct variant has named fields; a newtype
#' variant holds one value of a struct, `variant(of = GRanges)`.
#'
#' @param fields Named list: The fields of a struct variant.
#' @param description Character: What the variant is.
#' @param rules List: `rule()` calls over the variant's fields.
#' @param of Optional R8 struct type: The struct a newtype variant holds.
#'
#' @return An `r8_variant`.
#'
#' @examples
#' variant(fields = list(
#'   gamma = field(type_double(exclusive_min = 0), description = "Kernel width.")
#' ))
#'
#' @export
variant <- function(
  fields = list(),
  description = "",
  rules = list(),
  of = NULL
) {
  check_members(fields, "fields", "a variant")
  structure(
    list(
      fields = fields,
      description = description,
      rules = rules,
      of = of
    ),
    class = "r8_variant"
  )
} # /R8::variant


# %% new_enum ----
#' Declare an enum
#'
#' A sum type of unit, struct, and newtype variants. An enum of unit variants
#' is written as the variant name; an enum with another variant as an object
#' whose `tag` member names the variant. `Kernel@Linear` is a unit variant's
#' value.
#'
#' @param name Character: The type name.
#' @param variants Named list: The variants, `variant()` calls.
#' @param description Character: What the type is.
#' @param tag Character: The member that names the variant in JSON.
#' @param version Integer: The type's version, from 1.
#'
#' @return `new_enum()`: the type object, of class `r8_enum_type`.
#'   `Kernel@Linear`: the unit variant's value, an R8 value.
#'
#' @examples
#' op <- options(r8.namespace = "demo")
#' Kernel <- new_enum("Kernel", variants = list(
#'   Linear = variant(),
#'   Rbf = variant(fields = list(gamma = type_double(exclusive_min = 0)))
#' ))
#' Kernel@Linear
#' options(op)
#'
#' @export
new_enum <- function(
  name,
  variants = list(),
  description = "",
  tag = "type",
  version = 1L
) {
  if (!is_name(name)) {
    r8_abort(
      "A type name is an R8 name, such as \"Kernel\".",
      class = "r8_declaration_error"
    )
  }
  check_members(variants, "variants", paste0("`", name, "`"))
  if (
    length(variants) == 0L ||
      is.null(names(variants)) ||
      any(!nzchar(names(variants)))
  ) {
    r8_abort(
      "An enum has named variants: Name = variant(...).",
      class = "r8_declaration_error"
    )
  }
  variants_ir <- unname(Map(
    function(n, v) {
      if (!inherits(v, "r8_variant")) {
        r8_abort(
          "Variant `",
          n,
          "` is a variant() call.",
          class = "r8_declaration_error"
        )
      }
      out <- list(name = n, description = v[["description"]])
      if (!is.null(v[["of"]])) {
        out[["shape"]] <- "newtype"
        out[["of"]] <- attr(v[["of"]], "r8_name")
      } else if (length(v[["fields"]]) == 0L) {
        out[["shape"]] <- "unit"
      } else {
        out[["shape"]] <- "struct"
        out[["fields"]] <- fields_ir(v[["fields"]], paste0("variant `", n, "`"))
        if (length(v[["rules"]]) > 0L) {
          out[["rules"]] <- lapply(v[["rules"]], unclass)
        }
      }
      out
    },
    names(variants),
    variants
  ))
  ir <- list(
    kind = "enum",
    name = name,
    description = description,
    tag = tag,
    version = as.integer(version),
    variants = variants_ir
  )
  declare(ir, parent.frame(), "r8_enum_type", list())
} # /R8::new_enum


# %% @.r8_enum_type ----
#' @param object An enum type object.
#' @param name The variant's name.
#'
#' @rdname new_enum
#' @export
`@.r8_enum_type` <- function(object, name) {
  name <- as.character(substitute(name))
  ir <- attr(object, "r8_ir")
  v <- Filter(function(x) x[["name"]] == name, ir[["variants"]])
  if (length(v) == 0L) {
    r8_abort(
      "`",
      name,
      "` is not a variant of `",
      attr(object, "r8_name"),
      "`.",
      class = "r8_value_error"
    )
  }
  qname <- attr(object, "r8_name")
  if (v[[1L]][["shape"]] == "unit") {
    return(r8_construct(qname, list(), name))
  }
  # A struct or newtype variant: its constructor, with the fields as formals.
  fields <- if (v[[1L]][["shape"]] == "struct") {
    vapply(v[[1L]][["fields"]], function(f) f[["name"]], character(1))
  } else {
    names(formals(.r8_types[[v[[1L]][["of"]]]]))
  }
  defaults <- stats::setNames(rep(list(NULL), length(fields)), fields)
  as.function(
    c(
      defaults,
      bquote(r8_construct(
        .(qname),
        mget(.(fields), envir = environment()),
        .(name)
      ))
    ),
    envir = asNamespace("R8")
  )
} # /R8::@.r8_enum_type


# %% r8_declaration ----
#' Declaration of a type
#'
#' The IR document of a type: its namespace and its declaration, as JSON
#' text.
#'
#' @param type An R8 type object.
#'
#' @return Character: The IR document.
#'
#' @examples
#' op <- options(r8.namespace = "demo")
#' Point <- new_struct("Point", fields = list(x = type_double(), y = type_double()))
#' r8_declaration(Point)
#' options(op)
#'
#' @export
r8_declaration <- function(type) {
  if (!inherits(type, "r8_type")) {
    r8_abort("`type` is an R8 type object.", class = "r8_type_error")
  }
  r8_type_document(attr(type, "r8_name"))
} # /R8::r8_declaration


# %% print.r8_type ----
#' Print a type object
#'
#' @param x An R8 type object.
#' @param ... Not used.
#'
#' @return `x`, invisibly.
#'
#' @export
print.r8_type <- function(x, ...) {
  ir <- attr(x, "r8_ir")
  cat(
    "<",
    ir[["kind"]],
    " ",
    attr(x, "r8_name"),
    " v",
    ir[["version"]],
    ">\n",
    sep = ""
  )
  if (nzchar(ir[["description"]])) {
    cat("  ", ir[["description"]], "\n", sep = "")
  }
  items <- if (ir[["kind"]] == "struct") ir[["fields"]] else ir[["variants"]]
  for (it in items) {
    detail <- if (ir[["kind"]] == "struct") {
      it[["type"]][["kind"]]
    } else {
      it[["shape"]]
    }
    cat("  ", it[["name"]], ": ", detail, "\n", sep = "")
  }
  invisible(x)
} # /R8::print.r8_type


# %% print.r8_value ----
#' Print an R8 value
#'
#' @param x An R8 value.
#' @param ... Not used.
#'
#' @return `x`, invisibly.
#'
#' @export
print.r8_value <- function(x, ...) {
  variant <- attr(x, "r8_variant")
  cat(
    "<",
    attr(x, "r8_type"),
    if (!is.null(variant)) paste0("@", variant),
    ">\n",
    sep = ""
  )
  invisible(x)
} # /R8::print.r8_value
