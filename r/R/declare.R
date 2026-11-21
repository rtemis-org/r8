# declare.R
# ::R8::
# 2026- EDG rtemis.org

# Declared types, by qualified name, for this session. The registry of
# declarations lives in R8's shared object; this environment holds the R
# type objects that refer to it.
.r8_types <- new.env(parent = emptyenv())

# Each enum's variants, by qualified name: a unit variant's value and a
# struct or newtype variant's constructor, built on first access and cleared
# whenever a declaration changes the registry (`clear_variants()`).
.r8_variants <- new.env(parent = emptyenv())

# The registered codecs of Rust-native types: qualified name to the package
# whose exports carry the codec.
.r8_codecs <- new.env(parent = emptyenv())


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


# %% json_ir ----
#' IR as JSON text
#'
#' JSON text that a declaration carries into its IR as written: a rule's
#' `invariant` or `when` as JSONLogic, or a field's `default` as the value's
#' JSON form. It holds what the R forms do not express, such as an `and` of
#' three operands, a default that is an enum variant with fields or a struct,
#' and a number that a double does not hold exactly. `r8_document_native()`
#' writes it where a Rust declaration has no R form.
#'
#' @param text Character: One JSON text.
#'
#' @return An `r8_json_ir`: the text, of that class.
#'
#' @examples
#' rule("COST_RANGE", json_ir('{"and": [{">": [{"var": "cost"}, 0]},
#'   {"<=": [{"var": "cost"}, 100]}, {"!==": [{"var": "name"}, ""]}]}'),
#'   "cost is in (0, 100] and name is not empty.")
#' field(type_double(), default = json_ir("0.5"))
#'
#' @export
json_ir <- function(text) {
  if (!is.character(text) || length(text) != 1L || is.na(text)) {
    r8_abort("`text` is one string of JSON.", class = "r8_declaration_error")
  }
  tryCatch(
    r8_canonical_json(text),
    error = function(e) {
      r8_abort(
        "The text of json_ir() is JSON: ",
        conditionMessage(e),
        class = "r8_declaration_error"
      )
    }
  )
  structure(text, class = "r8_json_ir")
} # /R8::json_ir


# %% print.r8_json_ir ----
#' Print IR as JSON text
#'
#' @param x An `r8_json_ir`.
#' @param ... Not used.
#'
#' @return `x`, invisibly.
#'
#' @export
print.r8_json_ir <- function(x, ...) {
  cat("<r8_json_ir>", unclass(x), "\n")
  invisible(x)
} # /R8::print.r8_json_ir


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
  if (is.null(x) || inherits(x, "r8_json_ir")) {
    return(x)
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
  variant <- attr(x, "r8_variant")
  if (is.null(variant)) {
    r8_abort(
      "Struct values as defaults are constructed with the type.",
      class = "r8_declaration_error"
    )
  }
  ir <- r8_type_ir(attr(x, "r8_type"))
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
  # Nested field types keep their classes, which the deserializer that reads
  # the declaration into Rust ignores.
  out <- list(name = name, type = unclass(f[["type"]]))
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


# %% ctor_formals ----
#' The formals of a constructor, from the IR fields
#'
#' A required field, neither an option nor with a default, has no default
#' argument; every other field defaults to `NULL`, which construction reads as
#' the field's default or as none.
#'
#' @param fields List: The IR fields.
#'
#' @return List: The formals.
#'
#' @keywords internal
#' @noRd
ctor_formals <- function(fields) {
  names <- vapply(fields, function(f) f[["name"]], character(1))
  out <- stats::setNames(rep(list(NULL), length(fields)), names)
  for (f in fields) {
    if (
      !identical(f[["type"]][["kind"]], "option") && is.null(f[["default"]])
    ) {
      out[f[["name"]]] <- list(quote(expr = ))
    }
  }
  out
} # /R8::ctor_formals


# %% required_formals ----
#' The names of formals without a default
#'
#' @param formals List: Formals.
#'
#' @return Character.
#'
#' @keywords internal
#' @noRd
required_formals <- function(formals) {
  names(formals)[vapply(
    formals,
    function(a) identical(a, quote(expr = )),
    logical(1)
  )]
} # /R8::required_formals


# %% declare ----
#' Check and register a declaration, and make its type object
#'
#' @param ir List: The IR declaration.
#' @param namespace Character: The namespace.
#' @param class Character: The type object's class.
#' @param formals List: The constructor's formals.
#' @param native Logical: A Rust-native type.
#'
#' @return The type object.
#'
#' @keywords internal
#' @noRd
declare <- function(ir, env, class, formals, native = FALSE) {
  namespace <- declaring_namespace(env)
  package <- declaring_package(env)
  claim_namespace(namespace, package)
  qname <- paste0(namespace, "::", ir[["name"]])
  ctor <- as.function(
    c(
      formals,
      constructor_body(
        qname,
        formals,
        native = native,
        package = package
      )
    ),
    envir = asNamespace("R8")
  )
  register_ir(ir, qname, namespace)
  # In a package, the record goes to the package's table and the type object
  # carries its name; outside a package, the type object carries the record,
  # so that another process registers the type on first use.
  record <- r8_type_record(qname)
  recorded <- record_declaration(
    env,
    list(
      kind = "type",
      record = record,
      namespace = namespace,
      name = qname,
      native = if (native) TRUE
    )
  )
  type <- structure(
    ctor,
    class = c(class, "r8_type", "function"),
    r8_name = qname,
    r8_record = if (!recorded) record,
    r8_native = if (native) TRUE,
    r8_package = if (native) package
  )
  assign(qname, type, envir = .r8_types)
  type
} # /R8::declare


# %% register_ir ----
#' Check a type's declaration and register it
#'
#' @param ir List: The declaration, as the declaration functions build it.
#' @param qname Character: The qualified name.
#' @param namespace Character: The namespace.
#'
#' @return `NULL`, invisibly.
#'
#' @keywords internal
#' @noRd
register_ir <- function(ir, qname, namespace) {
  problems <- r8_declare_type(ir, namespace)
  if (length(problems[["code"]]) > 0L) {
    declaration_failed(qname, problems)
  }
  clear_variants()
  invisible(NULL)
} # /R8::register_ir


# %% register_records ----
#' Register the declarations of records
#'
#' @param records List: Raw vectors, each a declaration record.
#' @param source Character: The package the records come from, or `""` for a
#'   type object's own record.
#' @param check Logical: Check each declaration; `FALSE` at package load,
#'   where `check_namespaces()` checks the package's namespaces whole.
#'
#' @return `NULL`, invisibly.
#'
#' @keywords internal
#' @noRd
register_records <- function(records, source = "", check = TRUE) {
  if (length(records) == 0L) {
    return(invisible(NULL))
  }
  problems <- r8_declare_records(records, source, check)
  if (length(problems[["code"]]) > 0L) {
    declaration_failed(problems[["name"]][[1L]], problems)
  }
  clear_variants()
  invisible(NULL)
} # /R8::register_records


# %% ensure_registered ----
#' Register a type object's own record when the registry lacks its type
#'
#' A type declared outside a package carries its record, so that a process
#' that did not declare it, such as a `future` or `mirai` worker, registers it
#' on first use.
#'
#' @param type An R8 type object.
#'
#' @return Logical: `TRUE` when the type is registered, invisibly.
#'
#' @keywords internal
#' @noRd
ensure_registered <- function(type) {
  qname <- attr(type, "r8_name")
  if (is.null(qname)) {
    return(invisible(FALSE))
  }
  if (r8_is_registered(qname)) {
    return(invisible(TRUE))
  }
  record <- attr(type, "r8_record")
  if (is.null(record)) {
    return(invisible(FALSE))
  }
  register_records(list(record))
  invisible(r8_is_registered(qname))
} # /R8::ensure_registered


# %% declaration_failed ----
#' Signal the problems of a failed declaration
#'
#' @param qname Character: The qualified name of the declaration.
#' @param problems List: `code`, `path`, and `message` of each problem.
#'
#' @return Does not return; signals `r8_declaration_error`.
#'
#' @keywords internal
#' @noRd
declaration_failed <- function(qname, problems) {
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
    qname,
    "` failed:\n",
    paste(lines, collapse = "\n"),
    class = "r8_declaration_error"
  )
} # /R8::declaration_failed


# %% clear_variants ----
#' Clear the variants built for enum types
#'
#' @return `NULL`, invisibly.
#'
#' @keywords internal
#' @noRd
clear_variants <- function() {
  rm(list = ls(.r8_variants, all.names = TRUE), envir = .r8_variants)
  invisible(NULL)
} # /R8::clear_variants


# %% type_ir ----
#' The declaration of a registered type
#'
#' @param type An R8 type object.
#'
#' @return List: The declaration, in the form the declaration functions build,
#'   from the registry.
#'
#' @keywords internal
#' @noRd
type_ir <- function(type) {
  ensure_registered(type)
  r8_type_ir(attr(type, "r8_name"))
} # /R8::type_ir


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
#' @param native Logical: A Rust-native type, whose values the codec in the
#'   package's compiled exports builds and holds (`R8.h`). Its constructor
#'   signals `r8_codec_missing` until the codec is registered, and
#'   `r8_register()` checks that the exports carry the codec. A package whose
#'   types are declared in Rust generates these declarations with
#'   `r8_document_native()`.
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
  rules = list(),
  native = FALSE
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
  formals <- ctor_formals(ir[["fields"]])
  for (n in names(fields)) {
    f <- as_field(fields[[n]])
    if (
      isTRUE(f[["has_default"]]) &&
        !is.null(f[["default"]]) &&
        !inherits(f[["default"]], "r8_json_ir")
    ) {
      formals[n] <- list(f[["default"]])
    }
  }
  declare(
    ir,
    parent.frame(),
    "r8_struct_type",
    formals,
    native = isTRUE(native)
  )
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
#' @param native Logical: A Rust-native type, whose values, unit variants
#'   included, the codec in the package's compiled exports builds and holds
#'   (`R8.h`). Its variant constructors and unit variants signal
#'   `r8_codec_missing` until the codec is registered.
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
  version = 1L,
  native = FALSE
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
  declare(ir, parent.frame(), "r8_enum_type", list(), native = isTRUE(native))
} # /R8::new_enum


# %% @.r8_enum_type ----
#' @param object An enum type object.
#' @param name The variant's name.
#'
#' @rdname new_enum
#' @export
`@.r8_enum_type` <- function(object, name) {
  name <- as.character(substitute(name))
  qname <- attr(object, "r8_name")
  variants <- .r8_variants[[qname]] %||% enum_variants(qname, object)
  v <- variants[[name]]
  if (is.null(v)) {
    r8_abort(
      "`",
      name,
      "` is not a variant of `",
      qname,
      "`.",
      class = "r8_value_error"
    )
  }
  if (!inherits(v, "r8_variant_spec")) {
    return(v)
  }
  built <- build_variant(
    v,
    qname,
    name,
    isTRUE(attr(object, "r8_native")),
    attr(object, "r8_package") %||% ""
  )
  variants[[name]] <- built
  built
} # /R8::@.r8_enum_type


# %% enum_variants ----
#' The variants of an enum, to be built on first access
#'
#' @param qname Character: The enum's qualified name.
#' @param object The enum type object.
#'
#' @return Environment: Each variant's declaration, of class
#'   `r8_variant_spec`, by name; kept in `.r8_variants`.
#'
#' @keywords internal
#' @noRd
enum_variants <- function(qname, object) {
  env <- new.env(parent = emptyenv())
  for (v in type_ir(object)[["variants"]]) {
    env[[v[["name"]]]] <- structure(v, class = "r8_variant_spec")
  }
  .r8_variants[[qname]] <- env
  env
} # /R8::enum_variants


# %% build_variant ----
#' Build a variant: a unit variant's value, or a variant's constructor
#'
#' @param v The variant's declaration, of class `r8_variant_spec`.
#' @param qname Character: The enum's qualified name.
#' @param name Character: The variant's name.
#' @param native Logical: The enum is Rust-native.
#' @param package Character: The package that declares a Rust-native enum.
#'
#' @return The unit variant's value, or the constructor of a struct or
#'   newtype variant.
#'
#' @keywords internal
#' @noRd
build_variant <- function(v, qname, name, native, package = "") {
  v <- unclass(v)
  if (v[["shape"]] == "unit") {
    if (native) {
      check_codec(qname, package)
    }
    return(r8_construct(qname, list(), name))
  }
  # A struct or newtype variant: its constructor, with the fields as formals;
  # a field with a default receives NULL, which construction fills.
  formals <- if (v[["shape"]] == "struct") {
    ctor_formals(v[["fields"]])
  } else {
    ctor_formals(r8_type_ir(v[["of"]])[["fields"]])
  }
  as.function(
    c(
      formals,
      constructor_body(
        qname,
        formals,
        name,
        native = native,
        package = package
      )
    ),
    envir = asNamespace("R8")
  )
} # /R8::build_variant


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
  ensure_registered(type)
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
  ir <- type_ir(x)
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
