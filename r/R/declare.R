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

# The registered codecs of Rust-stored types: qualified name to the package
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
#' @param unit Optional Character: A unit code, by convention UCUM, such as
#'   `"mg"`.
#' @param compute Optional expression: For a field of type `option()`, its
#'   value as an expression over the other fields, captured unevaluated and
#'   written in the rule dialect, which `r8_resolve()` fills.
#' @param context Logical: For a field of type `option()`, `r8_resolve(x,
#'   context)` fills the value from `context`. At most one of `compute` and
#'   `context` is given.
#' @param extensions Optional named list: Namespaced extensions, such as
#'   `list("rtemis:tunable" = TRUE)`, which R8 stores and does not interpret.
#'
#' @return An `r8_field`.
#'
#' @examples
#' field(type_double(exclusive_min = 0), default = 1, description = "Cost.")
#' field(option(type_double()), compute = dose_mg / 1000)
#' field(option(type_integer(min = 1L)), context = TRUE)
#'
#' @export
field <- function(
  type,
  default = NULL,
  description = "",
  sensitive = FALSE,
  unit = NULL,
  compute = NULL,
  context = FALSE,
  extensions = NULL
) {
  declared(r8_field(
    type,
    default,
    !missing(default),
    description,
    sensitive,
    unit,
    substitute(compute),
    context,
    extensions
  ))
} # /R8::field


# %% column ----
#' Declare a table column
#'
#' The facts of one column of [type_table()]: those of a field, and whether
#' the column's values are distinct and whether every table holds it. A
#' column that may be absent differs from a present column with missing
#' cells, which is `option()`. A bare field type or a [field()] is the short
#' form of a column that is required and not unique.
#'
#' @param type An `r8_field_type`: The column's type, a scalar, a factor, or
#'   `option()` of one.
#' @param default Optional: The default value.
#' @param description Character: What the column holds.
#' @param sensitive Logical: Mask the values in printing, events, errors,
#'   and differences.
#' @param unit Optional Character: A unit code, by convention UCUM, such as
#'   `"mg"`.
#' @param unique Logical: The column's values are distinct.
#' @param required Logical: Every table holds the column.
#' @param extensions Optional named list: Namespaced extensions, which R8
#'   stores and does not interpret.
#'
#' @return An `r8_column`.
#'
#' @examples
#' type_table(list(
#'   id = column(type_character(), unique = TRUE),
#'   note = column(option(type_character()), required = FALSE)
#' ))
#'
#' @export
column <- function(
  type,
  default = NULL,
  description = "",
  sensitive = FALSE,
  unit = NULL,
  unique = FALSE,
  required = TRUE,
  extensions = NULL
) {
  f <- declared(r8_field(
    type,
    default,
    !missing(default),
    description,
    sensitive,
    unit,
    NULL,
    FALSE,
    extensions
  ))
  structure(
    c(unclass(f), list(unique = isTRUE(unique), required = isTRUE(required))),
    class = c("r8_column", "r8_field")
  )
} # /R8::column


# %% json_ir ----
#' IR as JSON text
#'
#' Declaration vocabulary, without the `r8_` prefix of operations on values:
#' JSON text that a declaration carries into its IR as written: a rule's
#' `invariant` or `when` as JSONLogic, or a field's `default` as the value's
#' JSON form. It holds what the R forms do not express, such as an `and` of
#' three operands, a default that is an enum variant with fields or a struct,
#' and a number that a double does not hold exactly. `r8_document_rust()`
#' writes it where a Rust declaration has no R form.
#'
#' @param text Character: One JSON text.
#'
#' @return An `r8_json_ir`: the text, of that class.
#'
#' @examples
#' rule(json_ir('{"and": [{">": [{"var": "cost"}, 0]},
#'   {"<=": [{"var": "cost"}, 100]}, {"!==": [{"var": "name"}, ""]}]}'),
#'   message = "cost is in (0, 100] and name is not empty.", id = "COST_RANGE")
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


# %% int64 ----
#' An integer beyond R's range
#'
#' Declaration vocabulary, without the `r8_` prefix of operations on values:
#' an integer of more than 2^53 in magnitude, which neither an R integer nor a
#' double holds exactly, given as its decimal digits, for a rule's constant or
#' a default. R8 writes such an integer of a declaration in this form.
#'
#' @param text Character: The decimal digits of one integer, with an optional
#'   leading `-`.
#'
#' @return An `r8_int64`: the text, of that class.
#'
#' @examples
#' int64("9007199254740993")
#'
#' @export
int64 <- function(text) {
  if (
    !is.character(text) ||
      length(text) != 1L ||
      is.na(text) ||
      !grepl("^-?[0-9]+$", text)
  ) {
    r8_abort(
      "`text` is the decimal digits of one integer.",
      class = "r8_declaration_error"
    )
  }
  structure(text, class = "r8_int64")
} # /R8::int64


# %% print.r8_int64 ----
#' Print an integer beyond R's range
#'
#' @param x An `r8_int64`.
#' @param ... Not used.
#'
#' @return `x`, invisibly.
#'
#' @export
print.r8_int64 <- function(x, ...) {
  cat("<r8 int64>", unclass(x), "\n")
  invisible(x)
} # /R8::print.r8_int64


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
  cat("<r8 json_ir>", unclass(x), "\n")
  invisible(x)
} # /R8::print.r8_json_ir


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
#' @param rust_stored Logical: A Rust-stored type.
#'
#' @return The type object.
#'
#' @keywords internal
#' @noRd
declare <- function(
  kind,
  name,
  description,
  version,
  tag,
  members,
  rules,
  env,
  class,
  rust_stored = FALSE,
  extends = NULL,
  defaults = list()
) {
  namespace <- declaring_namespace(env)
  package <- declaring_package(env)
  qname <- paste0(namespace, "::", name)
  out <- r8_declare(
    kind,
    name,
    description,
    as.integer(version),
    tag,
    members,
    rules,
    namespace,
    package,
    if (is.null(extends)) "" else attr(extends, "r8_name"),
    defaults,
    if (is.null(extends)) list() else as.list(formals(extends))
  )
  if (nzchar(out[["owner"]])) {
    namespace_taken(namespace, package, out[["owner"]])
  }
  if (length(out[["problems"]][["code"]]) > 0L) {
    declaration_failed(
      qname,
      out[["problems"]],
      list(rules = rules, members = members)
    )
  }
  clear_variants()
  clear_dispatch()
  stale <- out[["stale"]]
  if (length(stale) > 0L && !nzchar(package)) {
    stale_warning(qname, stale)
  }
  # A struct's formals and its required fields come from r8_declare(); an
  # enum's constructor takes no arguments.
  formals <- out[["formals"]] %||% list()
  ctor <- as.function(
    c(
      formals,
      constructor_body(
        qname,
        formals,
        rust_stored = rust_stored,
        package = package,
        required = out[["required"]]
      )
    ),
    envir = asNamespace("R8")
  )
  # In a package, the record goes to the package's table and the type object
  # carries its name; outside a package, the type object carries the record,
  # so that another process registers the type on first use.
  record <- out[["record"]]
  recorded <- record_declaration(
    env,
    list(
      kind = "type",
      record = record,
      namespace = namespace,
      name = qname,
      rust_stored = if (rust_stored) TRUE
    )
  )
  type <- structure(
    ctor,
    class = c(class, "r8_type", "function"),
    r8_name = qname,
    r8_record = if (!recorded) record,
    r8_storage = if (rust_stored) "rust",
    r8_package = if (rust_stored) package
  )
  assign(qname, type, envir = .r8_types)
  type
} # /R8::declare


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
  clear_ancestry()
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
#' @param decl Optional list: The declaration's `rules` and `members`, from
#'   which a problem in a rule shows the R expression at its path.
#'
#' @return Does not return; signals `r8_declaration_error`.
#'
#' @keywords internal
#' @noRd
declaration_failed <- function(qname, problems, decl = NULL) {
  msg <- problems[["message"]]
  # A name that is not a field may be an R variable, whose value lit() reads.
  pattern <- "^`([A-Za-z][A-Za-z0-9_.]*)` is not a field in scope"
  hint <- problems[["code"]] == "rule_field" & grepl(pattern, msg)
  name <- regmatches(msg[hint], regexpr("^`[^`]+`", msg[hint]))
  msg[hint] <- paste0(
    msg[hint],
    "; if ",
    name,
    " is an R variable, write `lit(",
    gsub("`", "", name),
    ")`"
  )
  where <- if (is.null(decl)) {
    character(length(msg))
  } else {
    vapply(problems[["path"]], problem_expr, "", decl = decl)
  }
  lines <- paste0(
    "- ",
    msg,
    " [",
    problems[["code"]],
    ifelse(
      nzchar(problems[["path"]]),
      paste0(" at ", problems[["path"]]),
      ""
    ),
    "]",
    ifelse(nzchar(where), paste0("\n  in `", where, "`"), "")
  )
  r8_abort(
    "Declaration of `",
    qname,
    "` failed:\n",
    paste(lines, collapse = "\n"),
    class = "r8_declaration_error"
  )
} # /R8::declaration_failed


# %% problem_expr ----
#' The R expression at the path of a problem in a rule
#'
#' @param path Character: The problem's JSON Pointer into the declaration,
#'   such as `/rules/0/invariant/>/0` or `/variants/1/rules/0/when`.
#' @param decl List: The declaration's `rules` and `members`.
#'
#' @return Character: The R expression at the path, or `""` for a path
#'   outside a rule's expression.
#'
#' @keywords internal
#' @noRd
problem_expr <- function(path, decl) {
  m <- regmatches(
    path,
    regexec(
      "^(?:/variants/([0-9]+))?/rules/([0-9]+)(/(?:invariant|when)(?:/.*)?)$",
      path,
      perl = TRUE
    )
  )[[1L]]
  if (length(m) == 0L) {
    return("")
  }
  rules <- if (nzchar(m[[2L]])) {
    decl[["members"]][[as.integer(m[[2L]]) + 1L]][["rules"]]
  } else {
    decl[["rules"]]
  }
  r <- rules[[as.integer(m[[3L]]) + 1L]]
  # A path ending at an operator names the operation: its operands' list
  # sits under the operator's key.
  pointer <- m[[4L]]
  # A field shows with the operation that reads it.
  if (grepl("/[0-9]+/var$", pointer)) {
    pointer <- sub("/[^/]+/[0-9]+/var$", "", pointer)
  }
  last <- sub("^.*/", "", pointer)
  if (!grepl("^[0-9]+$", last) && !last %in% c("invariant", "when")) {
    pointer <- sub("/[^/]*$", "", pointer)
  }
  x <- tryCatch(logic_at(r, pointer), error = function(e) NULL)
  if (is.null(x)) "" else logic_text(x)
} # /R8::problem_expr


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
#' rules over them. The type object constructs values, `Svm(cost = 2)`, and
#' `inherits(x, Svm)` tests for a value of the type or of a struct that extends
#' it. Types declared in a package belong to its namespace (the `DESCRIPTION`
#' field `Config/R8/namespace`, or the package name); types declared at the
#' console belong to the option `r8.namespace`, `"global"` by default.
#'
#' @param name Character: The type name, an R8 name such as `"Svm"`.
#' @param fields Named list: The fields, `field(...)` calls or bare field
#'   types.
#' @param description Character: What the type is.
#' @param version Integer: The type's version, from 1.
#' @param rules A `rule()`, or a list of `rule()` calls, over the fields.
#'   A child's rules add to the rules it inherits.
#' @param extends Optional R8 struct type: The parent. The child holds the
#'   parent's fields, then its own `fields`, and the parent's rules, then its
#'   own `rules`; its values belong to the parent's class, and impls of the
#'   parent apply to them where the child has none.
#' @param defaults Named list: The child's defaults of inherited fields, by
#'   field name. `NULL` removes the default of an `option()` field.
#' @param storage Character \{"r", "rust"\}: Where values hold their fields.
#'   `"rust"`: a Rust-stored type, whose values the codec in the package's
#'   compiled exports builds and holds (`R8.h`). Its constructor signals
#'   `r8_codec_missing_error` until the codec is registered, and `r8_register()`
#'   checks that the exports carry the codec. A package whose types are declared
#'   in Rust generates these declarations with `r8_document_rust()`.
#'
#' @return The type object, of class `r8_struct_type`.
#'
#' @examples
#' op <- options(r8.namespace = "demo")
#' Point <- new_struct("Point", fields = list(x = type_double(), y = type_double()),
#'   description = "A point.")
#' Range <- new_struct("Range", fields = list(lo = type_double(), hi = type_double()),
#'   rules = rule(lo <= hi, message = "lo is at most hi."))
#' Window <- new_struct("Window", fields = list(label = type_character()),
#'   extends = Range)
#' w <- Window(lo = 1, hi = 2, label = "w")
#' w
#' inherits(w, Range)
#' options(op)
#'
#' @export
new_struct <- function(
  name,
  fields = list(),
  description = "",
  version = 1L,
  rules = list(),
  extends = NULL,
  defaults = list(),
  storage = c("r", "rust")
) {
  rust_stored <- identical(match.arg(storage), "rust")
  if (!is_name(name)) {
    r8_abort(
      "A type name is an R8 name, such as \"Svm\"",
      name_reason(name),
      ".",
      class = "r8_declaration_error"
    )
  }
  check_members(fields, "fields", paste0("`", name, "`"))
  if (length(defaults) > 0L) {
    check_members(defaults, "defaults", paste0("`", name, "`"))
  }
  if (!is.null(extends)) {
    if (!inherits(extends, "r8_struct_type")) {
      r8_abort(
        "`extends` of `",
        name,
        "` is an R8 struct type, such as `extends = Range`.",
        class = "r8_declaration_error"
      )
    }
    ensure_registered(extends)
    parent_rust <- identical(attr(extends, "r8_storage"), "rust")
    if (!identical(rust_stored, parent_rust)) {
      r8_abort(
        "`",
        name,
        "` and its parent `",
        attr(extends, "r8_name"),
        "` are both Rust-stored or both R-stored; set `storage = \"",
        if (parent_rust) "rust" else "r",
        "\"`.",
        class = "r8_declaration_error"
      )
    }
  } else if (length(defaults) > 0L) {
    r8_abort(
      "`defaults` of `",
      name,
      "` changes inherited fields; set `extends =`, or give the defaults in `fields`.",
      class = "r8_declaration_error"
    )
  }
  declare(
    "struct",
    name,
    description,
    version,
    "type",
    fields,
    as_rules(rules),
    parent.frame(),
    "r8_struct_type",
    rust_stored = isTRUE(rust_stored),
    extends = extends,
    defaults = defaults
  )
} # /R8::new_struct


# %% stale_warning ----
#' Warn that a declaration leaves structs stale
#'
#' @param qname Character: The qualified name of the declared type.
#' @param stale Character: The qualified names of the stale structs.
#'
#' @return `NULL`, invisibly.
#'
#' @keywords internal
#' @noRd
stale_warning <- function(qname, stale) {
  r8_warn(
    "The declaration of `",
    qname,
    "` leaves ",
    paste0("`", stale, "`", collapse = ", "),
    " stale: ",
    if (length(stale) == 1L) "it extends" else "each extends",
    " an earlier declaration. Declare ",
    if (length(stale) == 1L) "it" else "them",
    " again, parent before child.",
    class = "r8_stale_warning"
  )
} # /R8::stale_warning


# %% variant ----
#' Declare an enum variant
#'
#' A unit variant has no fields; a struct variant has named fields; a newtype
#' variant holds one value of a struct, `variant(of = GRanges)`.
#'
#' @param fields Named list: The fields of a struct variant.
#' @param description Character: What the variant is.
#' @param rules A `rule()`, or a list of `rule()` calls, over the variant's
#'   fields.
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
  rules <- as_rules(rules)
  problem <- r8_variant_problem(
    !is.null(of),
    length(fields) > 0L,
    length(rules) > 0L
  )
  if (nzchar(problem)) {
    r8_abort(problem, class = "r8_declaration_error")
  }
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
#' @param tag Character: The member of an object variant's JSON form that
#'   holds the variant's name. A trait's `tag` is another member, which holds
#'   an implementor's type.
#' @param version Integer: The type's version, from 1.
#' @param storage Character \{"r", "rust"\}: Where values hold their fields.
#'   `"rust"`: a Rust-stored type, whose values, unit variants included, the
#'   codec in the package's compiled exports builds and holds (`R8.h`). Its
#'   variant constructors and unit variants signal `r8_codec_missing_error`
#'   until the codec is registered.
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
  storage = c("r", "rust")
) {
  rust_stored <- identical(match.arg(storage), "rust")
  if (!is_name(name)) {
    r8_abort(
      "A type name is an R8 name, such as \"Kernel\"",
      name_reason(name),
      ".",
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
  declare(
    "enum",
    name,
    description,
    version,
    tag,
    variants,
    list(),
    parent.frame(),
    "r8_enum_type",
    rust_stored = isTRUE(rust_stored)
  )
} # /R8::new_enum


# %% @.r8_enum_type ----
#' @param object An enum type object.
#' @param name The variant's name.
#'
#' @rdname new_enum
#' @export
`@.r8_enum_type` <- function(object, name) {
  name <- as.character(substitute(name))
  # `from` is a reserved variant name.
  if (identical(name, "from")) {
    return(r8_converter(object))
  }
  qname <- attr(object, "r8_name")
  variants <- .r8_variants[[qname]] %||% enum_variants(qname, type_ir(object))
  v <- variants[[name]]
  if (is.null(v)) {
    unknown_variant(name, qname)
  }
  if (!inherits(v, "r8_variant_spec")) {
    return(v)
  }
  built <- build_variant(
    v,
    qname,
    name,
    identical(attr(object, "r8_storage"), "rust"),
    attr(object, "r8_package") %||% ""
  )
  variants[[name]] <- built
  built
} # /R8::@.r8_enum_type


# %% enum_variants ----
#' The variants of an enum, to be built on first access
#'
#' @param qname Character: The enum's qualified name.
#' @param ir List: The enum's declaration.
#'
#' @return Environment: Each variant's declaration, of class
#'   `r8_variant_spec`, by name; kept in `.r8_variants`.
#'
#' @keywords internal
#' @noRd
enum_variants <- function(qname, ir) {
  env <- new.env(parent = emptyenv())
  for (v in ir[["variants"]]) {
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
#' @param rust_stored Logical: The enum is Rust-stored.
#' @param package Character: The package that declares a Rust-stored enum.
#'
#' @return The unit variant's value, or the constructor of a struct or
#'   newtype variant.
#'
#' @keywords internal
#' @noRd
build_variant <- function(v, qname, name, rust_stored, package = "") {
  v <- unclass(v)
  if (v[["kind"]] == "unit") {
    if (rust_stored) {
      check_codec(qname, package)
    }
    return(r8_construct(qname, list(), name))
  }
  # A struct or newtype variant: its constructor, with the fields as formals;
  # a field with a default receives NULL, which construction fills.
  formals <- if (v[["kind"]] == "struct") {
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
        rust_stored = rust_stored,
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


# %% nameOfClass.r8_type ----
#' The class of a type's values
#'
#' `inherits(x, Type)` calls it: a value's classes are its type's qualified
#' name, then those of its type's ancestors.
#'
#' @param x An R8 type object.
#'
#' @return Character: The qualified name of the type.
#'
#' @exportS3Method base::nameOfClass
#' @keywords internal
#' @noRd
nameOfClass.r8_type <- function(x) {
  attr(x, "r8_name", exact = TRUE)
} # /R8::nameOfClass.r8_type
