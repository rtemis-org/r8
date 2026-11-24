# values.R
# ::R8::
# 2026- EDG rtemis.org

# The installed packages that declare each R8 namespace, by the library
# paths read, for the message of an unknown type.
.r8_providers <- new.env(parent = emptyenv())

# An R8 value in R is a named list of its fields, an ALTREP object
# (`src/value.c`) whose data slots hold the fields and the identity of the
# value: its type, its variant, and its declaration. Its attributes are `names`
# and `class`, c("ns::Type", "r8_value"), with "ns::Type@Variant" first for an
# enum value, which S3 methods read and R8 does not. Values are immutable:
# `$<-`, `[[<-`, and `[<-` return a new value, validated.

# The native symbols of the C routines `type`, `variant_of`, `is_value`,
# `names`, and `require`, bound when the package loads.
.r8_c_type <- NULL
.r8_c_variant_of <- NULL
.r8_c_is_value <- NULL
.r8_c_names <- NULL
.r8_c_require <- NULL

# The native symbols of the C routines `get` and `fields`, which read fields.
.r8_c_get <- NULL
.r8_c_fields <- NULL


# %% is_value ----
#' Whether an object is an R8 value
#'
#' The identity decides, which R code does not reach; `inherits(x,
#' "r8_value")` reads the class vector, which any object can have.
#'
#' @param x An object.
#'
#' @return Logical.
#'
#' @keywords internal
#' @noRd
is_value <- function(x) .Call(.r8_c_is_value, x)


# %% value_type ----
#' The type of an R8 value, its variant, and the names of its fields
#'
#' @param x An object.
#'
#' @return `value_type()`: Character, the qualified name. `value_variant()`:
#'   Character, the variant of an enum value. `value_names()`: Character. Each
#'   is `NULL` for an object that is not an R8 value, and `value_variant()` for
#'   a struct's value.
#'
#' @keywords internal
#' @noRd
value_type <- function(x) .Call(.r8_c_type, x)

value_variant <- function(x) .Call(.r8_c_variant_of, x)

value_names <- function(x) .Call(.r8_c_names, x)


# %% not_value_text ----
#' What an object is that has the class of an R8 value and is not one
#'
#' @param x An object that inherits from `r8_value`.
#'
#' @return Character: A sentence without its subject.
#'
#' @keywords internal
#' @noRd
not_value_text <- function(x) {
  paste0(
    "has the class of an R8 value and is not one: it is a list, as `dput()`, ",
    "copied attributes, and a write of its elements in C code give. ",
    "Construct the value with its type."
  )
} # /R8::not_value_text


# %% require_current ----
#' Check that a value is of the registered declaration of its type
#'
#' @param x An R8 value.
#'
#' @return `NULL`, invisibly; a value of another declaration raises
#'   `r8_other_declaration_error`.
#'
#' @keywords internal
#' @noRd
require_current <- function(x) invisible(.Call(.r8_c_require, x))


# %% other_abort ----
#' Signal that a value is of another declaration than the registered one
#'
#' Called by the C routine `require`.
#'
#' @param x The value.
#' @param ... Not used.
#' @param what Character: The value, as the message names it.
#' @param class Character: Further condition classes.
#'
#' @return `NULL` for a value whose type is not registered; otherwise does not
#'   return.
#'
#' @keywords internal
#' @noRd
other_abort <- function(
  x,
  ...,
  what = "The value",
  class = "r8_value_error"
) {
  other <- r8_value_other(x)
  if (!is.null(other)) {
    r8_abort(
      what,
      " was ",
      other[["message"]],
      " [declaration_changed].",
      class = c("r8_other_declaration_error", class)
    )
  }
  invisible(NULL)
} # /R8::other_abort


# %% r8_update ----
#' Convert a value to the registered declaration of its type
#'
#' A value belongs to the declaration of its type that it was made under, which
#' it carries with it. After the declaration changes, when a package is updated
#' or a type is declared again at the console, a value made before, such as one
#' read from a file, can be looked at. It prints, gives its fields, and is refused
#' where it is used: as a field or an argument, in a method call, and in an assignment,
#' comparison, hash, or document. `r8_update()` gives its value under the
#' registered declaration where its fields mean the same under both, validated.
#'
#' The fields mean the same when each is declared under the same name, as the
#' same kind of type, in the same unit, and with its factor levels in the same
#' order, and every field that the registered declaration adds is optional or has
#' a default, which it takes. A change of bounds, of rules, or of the order of
#' fields, and a factor level that is added or removed, do not change the meaning; a
#' value that the registered declaration
#' does not admit is an error that lists its problems. A value of another
#' version of its type is refused, since raising a version states that the
#' meaning changed. The values that the fields hold are converted in the same
#' way.
#'
#' A type whose values Rust holds (`storage = "rust"`) reads a value from a file
#' under the declaration it was saved with; such a value is made again from its
#' source.
#'
#' @param x An R8 value, or a list that holds R8 values.
#'
#' @return The value or the list, in which every R8 value is of the registered
#'   declaration of its type; `x` itself when each is. A value whose fields may
#'   mean something else raises `r8_other_declaration_error`.
#'
#' @examples
#' op <- options(r8.namespace = "demo")
#' Visit <- new_struct("Visit", fields = list(age = type_integer()))
#' v <- Visit(age = 40L)
#' Visit <- new_struct("Visit", fields = list(
#'   age = type_integer(min = 0L),
#'   site = option(type_character())
#' ))
#' try(r8_to_json(v))
#' r8_update(v)
#' options(op)
#'
#' @export
r8_update <- function(x) {
  if (!is_value(x) && (!is.list(x) || inherits(x, "r8_value"))) {
    value_required(x)
  }
  res <- r8_updated_value(x)
  if (!is.null(res[["message"]])) {
    r8_abort(
      "Cannot update the value: ",
      res[["message"]],
      " [",
      res[["code"]] %||% "declaration_changed",
      "].",
      class = c(
        if (is.null(res[["code"]])) "r8_other_declaration_error",
        "r8_value_error"
      )
    )
  }
  if (!is.null(res[["errors"]])) {
    value_abort("Cannot update to", res[["of"]], res[["errors"]])
  }
  res[["value"]]
} # /R8::r8_update


# %% value_required ----
#' Signal that an argument is not an R8 value
#'
#' @param x The argument, `x` of the caller.
#'
#' @return Does not return; signals `r8_type_error`.
#'
#' @keywords internal
#' @noRd
value_required <- function(x) {
  if (inherits(x, "r8_value")) {
    r8_abort("`x` ", not_value_text(x), class = "r8_type_error")
  }
  r8_abort("`x` is an R8 value.", class = "r8_type_error")
} # /R8::value_required

# %% problems_message ----
#' The message of a list of problems
#'
#' One entry per problem; the violations of one rule at several paths, such
#' as the failing elements of an `all()`, form one entry. A rule violation
#' adds a line with the rule's expression in R and, unless the option
#' `r8.show_values` is `FALSE`, the values the rule read: its fields, its
#' computed operands, and the failing elements `.x`, as the core collects
#' them for the first violations of each rule.
#'
#' @param errors List: `code`, `path`, `rule`, `message`, and optionally
#'   `diagnosis` vectors.
#'
#' @return Character: The message lines, joined.
#'
#' @keywords internal
#' @noRd
problems_message <- function(errors) {
  n <- length(errors[["code"]])
  diagnosis <- errors[["diagnosis"]]
  if (is.null(diagnosis)) {
    diagnosis <- vector("list", n)
  }
  rule <- errors[["rule"]] %||% rep(NA_character_, n)
  index <- errors[["rule_index"]] %||% rep(NA_integer_, n)
  show <- !isFALSE(getOption("r8.show_values"))
  is_rule <- errors[["code"]] == "rule_violation"
  key <- ifelse(
    is_rule,
    paste(rule, index, errors[["message"]], sep = "\r"),
    paste0("\r", seq_len(n))
  )
  out <- character()
  for (k in unique(key)) {
    i <- which(key == k)
    first <- i[[1L]]
    paths <- errors[["path"]][i]
    if (!is.null(errors[["where"]])) {
      paths <- errors[["where"]][i]
    }
    where <- paths_text(paths[nzchar(paths)])
    bracket <- paste0(
      " [",
      errors[["code"]][[first]],
      if (nzchar(where)) paste0(" at ", where),
      "]"
    )
    if (!is_rule[[first]]) {
      id <- rule[[first]]
      hint <- errors[["hint"]][[first]]
      code_only <- !is.null(errors[["where"]])
      out <- c(
        out,
        paste0(
          "- ",
          errors[["message"]][[first]],
          if (!is.null(hint) && !is.na(hint)) paste0(" ", hint),
          if (!is.na(id)) paste0(" (rule ", id, ")"),
          if (code_only) {
            paste0(" [", errors[["code"]][[first]], "]")
          } else {
            bracket
          }
        )
      )
      next
    }
    has <- !vapply(diagnosis[i], is.null, logical(1))
    head <- paste0(
      "- Rule",
      if (!is.na(rule[[first]])) paste0(" ", rule[[first]]),
      ":"
    )
    line <- if (any(has)) {
      diagnosis_line(diagnosis[i][has], paths[has], show)
    } else {
      NULL
    }
    message <- errors[["message"]][[first]]
    if (nzchar(message)) {
      out <- c(
        out,
        paste0(head, " ", message, bracket),
        if (!is.null(line)) paste0("  ", line)
      )
    } else {
      out <- c(
        out,
        paste0(
          head,
          " ",
          if (is.null(line)) "the rule does not hold" else line,
          ".",
          bracket
        )
      )
    }
  }
  paste(out, collapse = "\n")
} # /R8::problems_message


# %% paths_text ----
#' The paths of one entry of a problem message
#'
#' @param paths Character: JSON Pointers.
#'
#' @return Character: The first three, and the count of the others.
#'
#' @keywords internal
#' @noRd
paths_text <- function(paths) {
  if (length(paths) <= 3L) {
    return(paste(paths, collapse = ", "))
  }
  paste0(
    paste(paths[1:3], collapse = ", "),
    ", and ",
    length(paths) - 3L,
    " more"
  )
} # /R8::paths_text


# %% diagnosis_line ----
#' The line of a rule violation's diagnosis
#'
#' @param d List: The diagnoses of one entry, as `errors_list()` writes them.
#' @param paths Character: The path of each diagnosis.
#' @param show Logical: Show the values.
#'
#' @return Character: The rule's expression and, with `show`, the values
#'   after "found".
#'
#' @keywords internal
#' @noRd
diagnosis_line <- function(d, paths, show) {
  first <- d[[1L]]
  expr <- logic_text(first[["invariant"]])
  if (!is.null(first[["when"]])) {
    expr <- paste0(expr, ", when ", logic_text(first[["when"]]))
  }
  if (!show) {
    return(expr)
  }
  ops <- first[["operands"]]
  values <- c(
    first[["fields"]],
    if (length(ops) > 0L) {
      paste0(
        vapply(
          names(ops),
          function(p) logic_text(logic_at(first[["invariant"]], p)),
          character(1)
        ),
        " = ",
        unname(ops)
      )
    }
  )
  elements <- unlist(lapply(seq_along(d), function(j) {
    if (!is.null(d[[j]][["element"]])) {
      paste0(d[[j]][["element"]], " at ", paths[[j]])
    }
  }))
  if (length(elements) == 0L && length(d) > 1L && nzchar(paths[[1L]])) {
    values <- c(values, paste0("at ", paths[[1L]]))
  }
  values <- c(values, elements)
  paste0(
    expr,
    if (length(values) > 0L) paste0("; found ", paste(values, collapse = ", "))
  )
} # /R8::diagnosis_line


# %% logic_text ----
#' A rule expression as R text
#'
#' @param x A JSONLogic expression, as the lists `rule()` builds.
#'
#' @return Character: The R expression, or the JSON text of an expression
#'   without an R form.
#'
#' @keywords internal
#' @noRd
logic_text <- function(x) {
  e <- tryCatch(logic_to_r(x), error = function(e) NULL)
  if (is.null(e)) {
    return(r8_ir_json(x))
  }
  paste(deparse(e, width.cutoff = 500L), collapse = " ")
} # /R8::logic_text


# %% logic_at ----
#' The subexpression at a JSON Pointer
#'
#' @param x A JSONLogic expression, as the lists `rule()` builds.
#' @param pointer Character: A JSON Pointer into `x`, such as `"/<=/0"`.
#'
#' @return The subexpression.
#'
#' @keywords internal
#' @noRd
logic_at <- function(x, pointer) {
  for (seg in strsplit(sub("^/", "", pointer), "/", fixed = TRUE)[[1L]]) {
    seg <- gsub("~1", "/", gsub("~0", "~", seg, fixed = TRUE), fixed = TRUE)
    x <- if (grepl("^[0-9]+$", seg) && is.null(names(x))) {
      x[[as.integer(seg) + 1L]]
    } else {
      x[[seg]]
    }
  }
  x
} # /R8::logic_at


# %% problems_data ----
#' The problems of a value error, for its condition object
#'
#' @param errors List: As `problems_message()` takes it.
#'
#' @return A data frame: `code`, `path`, `rule`, `rule_index`, `message`,
#'   `where` (the path in R), `suggestion`, and `hint`, and, unless
#'   the option `r8.show_values` is `FALSE`, the list column `diagnosis`.
#'
#' @keywords internal
#' @noRd
problems_data <- function(errors) {
  n <- length(errors[["code"]])
  out <- data.frame(
    code = errors[["code"]],
    path = errors[["path"]],
    rule = errors[["rule"]] %||% rep(NA_character_, n),
    rule_index = errors[["rule_index"]] %||% rep(NA_integer_, n),
    message = errors[["message"]],
    where = errors[["where"]] %||% rep(NA_character_, n),
    suggestion = errors[["suggestion"]] %||% rep(NA_character_, n),
    hint = errors[["hint"]] %||% rep(NA_character_, n)
  )
  if (
    !isFALSE(getOption("r8.show_values")) && !is.null(errors[["diagnosis"]])
  ) {
    out[["diagnosis"]] <- errors[["diagnosis"]]
  }
  out
} # /R8::problems_data


# %% problem_keys ----
#' The entry of each problem in a message
#'
#' @param errors List: As `problems_message()` takes it.
#'
#' @return Character: One key per problem; the violations of one rule share
#'   a key.
#'
#' @keywords internal
#' @noRd
problem_keys <- function(errors) {
  n <- length(errors[["code"]])
  ifelse(
    errors[["code"]] == "rule_violation",
    paste(
      errors[["rule"]] %||% rep(NA_character_, n),
      errors[["rule_index"]] %||% rep(NA_integer_, n),
      errors[["message"]],
      sep = "\r"
    ),
    paste0("\r", seq_len(n))
  )
} # /R8::problem_keys


# %% coercion_hints ----
#' Hints for the R objects behind a value's problems
#'
#' A hint states the lossless conversion of a common R mistake: a whole
#' double for an integer field, a factor for a character field, more than
#' one value for a scalar field, and `NA` for a field that is not an
#' `option()`.
#'
#' @param errors List: As `problems_message()` takes it, with `where`.
#' @param inputs Optional named list: The R objects given for the fields.
#'
#' @return Character: One hint per problem, `NA` where none applies.
#'
#' @keywords internal
#' @noRd
coercion_hints <- function(errors, inputs) {
  n <- length(errors[["code"]])
  out <- rep(NA_character_, n)
  if (is.null(inputs) || is.null(errors[["where"]])) {
    return(out)
  }
  for (i in seq_len(n)) {
    where <- errors[["where"]][[i]]
    code <- errors[["code"]][[i]]
    msg <- errors[["message"]][[i]]
    if (!nzchar(where) || !where %in% names(inputs)) {
      next
    }
    obj <- inputs[[where]]
    out[[i]] <- if (code %in% c("missing_value", "na_value")) {
      "`option()` admits a missing value."
    } else if (
      code %in% c("type_mismatch", "r_type", "r_attributes") && is.factor(obj)
    ) {
      "`as.character()` gives its labels."
    } else if (code == "r_attributes") {
      "`r8_conform()` removes them."
    } else if (
      code %in%
        c("type_mismatch", "r_type") &&
        grepl("integer", msg, fixed = TRUE) &&
        is.double(obj) &&
        length(obj) == 1L &&
        is.finite(obj) &&
        obj == round(obj) &&
        abs(obj) <= .Machine[["integer.max"]]
    ) {
      paste0("The integer is `", format(obj, scientific = FALSE), "L`.")
    } else {
      NA_character_
    }
  }
  out
} # /R8::coercion_hints


# %% rules_skipped ----
#' The number of invalid fields that kept a type's rules from running
#'
#' @param errors List: As `problems_message()` takes it.
#' @param type_name Optional character: The qualified type.
#' @param variant Character: The enum variant, or `""`.
#'
#' @return Integer: The number of invalid top-level fields when the type
#'   has rules and none was evaluated, and 0 otherwise.
#'
#' @keywords internal
#' @noRd
rules_skipped <- function(errors, type_name, variant = "") {
  if (is.null(type_name) || any(startsWith(errors[["code"]], "rule_"))) {
    return(0L)
  }
  ir <- tryCatch(r8_type_ir(type_name), error = function(e) NULL)
  rules <- if (nzchar(variant)) {
    ir[["variants"]][[variant]][["rules"]]
  } else {
    ir[["rules"]]
  }
  if (length(rules) == 0L) {
    return(0L)
  }
  where <- errors[["where"]] %||% sub("^/", "", errors[["path"]])
  length(unique(sub("[$[].*$", "", where[nzchar(where)])))
} # /R8::rules_skipped


# %% value_abort ----
#' Signal a value's problems
#'
#' @param what Character: The failed operation, such as `"Invalid"`.
#' @param label Character: The type, or the type and variant.
#' @param errors List: As `problems_message()` takes it.
#' @param inputs Optional named list: The R objects given for the fields,
#'   from which coercion hints are written.
#' @param assignment Optional character: The assignment that caused the
#'   problems, such as `x$n <- value`.
#' @param type_name Optional character: The qualified type, whose rules a
#'   closing line reports as not evaluated when fields are invalid.
#' @param variant Character: The enum variant, or `""`.
#' @param source Optional character: Where the value comes from, after the
#'   label.
#' @param class Character: The condition's classes, before `r8_error`.
#'
#' @return Does not return; signals an error of class `class`, whose field
#'   `problems` holds `problems_data(errors)`.
#'
#' @keywords internal
#' @noRd
value_abort <- function(
  what,
  label,
  errors,
  inputs = NULL,
  assignment = NULL,
  type_name = NULL,
  variant = "",
  source = NULL,
  class = "r8_value_error"
) {
  errors[["hint"]] <- coercion_hints(errors, inputs)
  n <- length(unique(problem_keys(errors)))
  skipped <- rules_skipped(errors, type_name, variant)
  r8_abort(
    what,
    " `",
    label,
    "`",
    if (!is.null(assignment)) paste0(" from `", assignment, "`"),
    if (!is.null(source)) paste0(" from ", source),
    ": ",
    n,
    if (n == 1L) " problem" else " problems",
    "\n",
    problems_message(errors),
    if (skipped > 0L) {
      paste0(
        "\nRules were not evaluated: ",
        skipped,
        if (skipped == 1L) " field is invalid." else " fields are invalid."
      )
    },
    class = class,
    data = list(problems = problems_data(errors))
  )
} # /R8::value_abort


# %% r8_construct ----
#' Construct a value
#'
#' @param type_name Character: The qualified type name.
#' @param fields Named list: The fields.
#' @param variant Character: The enum variant, or `""` for a struct.
#'
#' @return An R8 value.
#'
#' @keywords internal
#' @noRd
r8_construct <- function(type_name, fields, variant = "") {
  res <- r8_new_value(fields, type_name, variant)
  if (inherits(res, "r8_problems")) {
    construct_error(res, type_name, variant)
  }
  res
} # /R8::r8_construct


# %% construct_error ----
#' Signal the problems of a construction
#'
#' @param res The `r8_problems` list that `r8_new_value()` returned.
#' @param type_name Character: The qualified type name.
#' @param variant Character: The enum variant, or `""` for a struct.
#' @param inputs Optional named list: The R objects given for the fields.
#' @param assignment Optional character: The assignment that caused the
#'   problems.
#'
#' @return Does not return; signals `r8_value_error`.
#'
#' @keywords internal
#' @noRd
construct_error <- function(
  res,
  type_name,
  variant,
  inputs = NULL,
  assignment = NULL
) {
  label <- if (nzchar(variant)) paste0(type_name, "@", variant) else type_name
  value_abort(
    "Invalid",
    label,
    res[["errors"]],
    inputs = inputs,
    assignment = assignment,
    type_name = type_name,
    variant = variant
  )
} # /R8::construct_error


# %% constructor_body ----
#' The body of a constructor
#'
#' One `.Call()` of R8's C routine `construct` with the call's frame, the
#' field names, and which fields are required, constant vectors: the routine
#' forces the arguments in the order of the formals, an error raised while
#' forcing one propagates, a formal's default (a field's declared default)
#' applies to an argument not supplied, and a missing required argument
#' reaches construction as an absent field. The constructor's environment is
#' R8's namespace, where `.r8_native` is bound.
#'
#' @param qname Character: The qualified type name.
#' @param formals List: The constructor's formals, one per field in
#'   declaration order; a formal without a default is a required field.
#' @param variant Character: The enum variant, or `""` for a struct.
#' @param rust_stored Logical: The type is Rust-stored, whose codec is checked
#'   first.
#' @param package Character: The package that declares a Rust-stored type.
#' @param required Optional Logical: Which fields are required, one per
#'   formal; `NULL` reads them from `formals`, where a required field has no
#'   default argument.
#'
#' @return The body, a call.
#'
#' @keywords internal
#' @noRd
constructor_body <- function(
  qname,
  formals,
  variant = "",
  rust_stored = FALSE,
  package = "",
  required = NULL
) {
  fields <- names(formals)
  if (is.null(required)) {
    required <- fields %in% required_formals(formals)
  }
  steps <- list(
    call(
      "<-",
      quote(.r8_value_),
      call(
        ".Call",
        quote(.r8_native[["construct"]]),
        quote(environment()),
        as.character(fields),
        required,
        qname,
        variant
      )
    ),
    call(
      "if",
      quote(inherits(.r8_value_, "r8_problems")),
      call(
        "construct_failed",
        quote(.r8_value_),
        quote(environment()),
        quote(sys.function()),
        qname,
        variant
      ),
      quote(.r8_value_)
    )
  )
  if (rust_stored) {
    steps <- c(list(call("check_codec", qname, package)), steps)
  }
  as.call(c(list(as.name("{")), steps))
} # /R8::constructor_body


# %% construct_failed ----
#' Handle a construction that returned problems
#'
#' A type declared outside a package registers its own record, which its
#' constructor carries, when the registry lacks it, then constructs again
#' from the same frame, whose promises are already forced; any other problem
#' raises the construction error.
#'
#' @param res The problems, of class `r8_problems`.
#' @param env Environment: The constructor's frame.
#' @param fn Function: The constructor.
#' @param qname Character: The qualified type name.
#' @param variant Character: The enum variant, or `""`.
#'
#' @return The value constructed on the second attempt; otherwise signals
#'   `r8_value_error`.
#'
#' @keywords internal
#' @noRd
construct_failed <- function(res, env, fn, qname, variant) {
  if (
    identical(res[["errors"]][["code"]][1L], "type_unregistered") &&
      ensure_registered(fn)
  ) {
    f <- formals(fn)
    res <- .Call(
      .r8_native[["construct"]],
      env,
      names(f),
      names(f) %in% required_formals(f),
      qname,
      variant
    )
    if (!inherits(res, "r8_problems")) {
      return(res)
    }
  }
  # The arguments given, for hints; a missing argument is skipped.
  given <- names(formals(fn))
  inputs <- tryCatch(
    mget(given, envir = env, ifnotfound = list(NULL)),
    error = function(e) NULL
  )
  construct_error(res, qname, variant, inputs = inputs)
} # /R8::construct_failed


# %% check_codec ----
#' Check that the codec of a Rust-stored type is registered
#'
#' @param qname Character: The qualified type name.
#'
#' @return `NULL`, invisibly; signals `r8_codec_missing_error` when the codec is
#'   not registered.
#'
#' @keywords internal
#' @noRd
check_codec <- function(qname, package = "") {
  if (is.null(.r8_codecs[[qname]])) {
    r8_abort(
      "`",
      qname,
      "` is a Rust-stored type, whose values the codec in a package's ",
      "compiled exports builds; no codec of it is registered.",
      if (nzchar(package)) {
        paste0(
          " Loading '",
          package,
          "' registers it (its .onLoad() calls r8_register())."
        )
      },
      class = c("r8_codec_missing_error", "r8_value_error")
    )
  }
  invisible(NULL)
} # /R8::check_codec


# %% new_dgc ----
#' A dgCMatrix from its compressed sparse column parts
#'
#' @param i Integer: Row indices, 0-based.
#' @param p Integer: Column offsets.
#' @param x Numeric: Values.
#' @param dim Integer: The shape.
#'
#' @return A `dgCMatrix`.
#'
#' @keywords internal
#' @noRd
new_dgc <- function(i, p, x, dim) {
  if (!requireNamespace("Matrix", quietly = TRUE)) {
    r8_abort(
      "A sparse matrix field needs the package 'Matrix'.",
      class = "r8_value_error"
    )
  }
  methods::new("dgCMatrix", i = i, p = p, x = x, Dim = dim)
} # /R8::new_dgc


# %% $.r8_value ----
#' Fields of an R8 value
#'
#' `x$name` and `x[["name"]]` read a field by its exact name, and
#' `x[c("a", "b")]` several, as a plain named list; an unknown name is an
#' error. `as.list(x)` gives every field as a plain named list, in declaration
#' order and with `NULL` for an absent field, so that `lapply()`, `vapply()`,
#' and `Filter()` take a value.
#'
#' `x$name <- v` and `x[["name"]] <- v` return a new value, validated; `x` is
#' unchanged. `x[c("a", "b")] <- list(v, w)` replaces several fields: each is
#' validated, then the rules that read any of them run once, on the new value,
#' so a rule between the fields holds for the result and not for each step.
#' The list has one element per name, and its own names, when it has them, are
#' the fields'.
#'
#' @param x An R8 value.
#' @param name Character: The field name.
#' @param i Character: The field name; for `x[i]`, one name or more, each once
#'   in an assignment.
#' @param value The new field value; for `x[i]`, a list of new field values.
#' @param ... Not used.
#'
#' @return The field's value, a named list of fields, or the new R8 value.
#'
#' @examples
#' op <- options(r8.namespace = "demo")
#' Interval <- new_struct(
#'   "Interval",
#'   fields = list(lo = type_double(), hi = type_double())
#' )
#' x <- Interval(lo = 0, hi = 2)
#' x$lo
#' x[c("lo", "hi")]
#' vapply(x, class, "")
#' x$lo <- 1
#' x
#' x[c("lo", "hi")] <- list(5, 9)
#' x
#' options(op)
#'
#' @name r8_value_fields
#' @export
`$.r8_value` <- function(x, name) {
  v <- .Call(.r8_c_get, x, name)
  if (is.null(v)) field_get(x, name) else v
} # /R8::$.r8_value


# %% [[.r8_value ----
#' @rdname r8_value_fields
#' @export
`[[.r8_value` <- function(x, i, ...) {
  if (!is.character(i) || length(i) != 1L) {
    r8_abort(
      "A field is read by name: x[[\"name\"]].",
      class = "r8_value_error"
    )
  }
  v <- .Call(.r8_c_get, x, i)
  if (is.null(v)) field_get(x, i) else v
} # /R8::[[.r8_value


# %% [.r8_value ----
#' @rdname r8_value_fields
#' @export
`[.r8_value` <- function(x, i, ...) {
  if (missing(i) || !is.character(i) || ...length() > 0L) {
    r8_abort(
      "Fields are read by name: x[c(\"a\", \"b\")].",
      class = "r8_value_error"
    )
  }
  unknown <- is.na(match(i, attr(x, "names", exact = TRUE)))
  if (any(unknown)) {
    unknown_field(x, i[unknown][[1L]])
  }
  .Call(.r8_c_fields, x, match(i, attr(x, "names", exact = TRUE)))
} # /R8::[.r8_value


# %% as.list.r8_value ----
#' @rdname r8_value_fields
#' @export
as.list.r8_value <- function(x, ...) {
  .Call(.r8_c_fields, x, NULL)
} # /R8::as.list.r8_value


# %% $<-.r8_value ----
#' @rdname r8_value_fields
#' @export
`$<-.r8_value` <- function(x, name, value) {
  field_set(x, name, value)
} # /R8::$<-.r8_value


# %% [[<-.r8_value ----
#' @rdname r8_value_fields
#' @export
`[[<-.r8_value` <- function(x, i, value) {
  if (!is.character(i) || length(i) != 1L) {
    r8_abort(
      "A field is replaced by name: x[[\"name\"]] <- value.",
      class = "r8_value_error"
    )
  }
  field_set(x, i, value)
} # /R8::[[<-.r8_value


# %% [<-.r8_value ----
#' @rdname r8_value_fields
#' @export
`[<-.r8_value` <- function(x, i, value) {
  if (missing(i)) {
    fields_refused(x, NULL, value)
  }
  if (!is_value(x)) {
    r8_abort("`x` ", not_value_text(x), class = "r8_value_error")
  }
  res <- .Call(wrap__r8_set_fields, x, i, value)
  if (!is_value(res)) {
    fields_refused(x, i, value, res)
  }
  res
} # /R8::[<-.r8_value


# %% field_get ----
#' Read a field that the C routine `get` gives as `NULL`
#'
#' @param x An R8 value.
#' @param name Character: The field name.
#'
#' @return `NULL` for a field of `x`; an unknown name raises `r8_value_error`.
#'
#' @keywords internal
#' @noRd
field_get <- function(x, name) {
  # `names()` dispatches; the attribute does not.
  if (is.na(match(name, attr(x, "names", exact = TRUE)))) {
    unknown_field(x, name)
  }
  NULL
} # /R8::field_get


# %% unknown_field ----
#' Signal that a value has no field of a name
#'
#' @param x An R8 value.
#' @param name Character: The name.
#'
#' @return Does not return; signals `r8_value_error`.
#'
#' @keywords internal
#' @noRd
unknown_field <- function(x, name) {
  variant <- value_variant(x)
  fields <- attr(x, "names", exact = TRUE)
  r8_abort(
    "`",
    name,
    "` is not a field of `",
    value_type(x),
    if (!is.null(variant)) paste0("@", variant),
    "`",
    if (length(fields) == 0L) {
      ", which has no fields."
    } else {
      paste0(". Fields: ", paste(fields, collapse = ", "), ".")
    },
    class = "r8_value_error"
  )
} # /R8::unknown_field


# %% field_set ----
#' Replace a field, returning a new value
#'
#' @param x An R8 value.
#' @param name Character: The field name.
#' @param value The new field value.
#'
#' @return The new R8 value.
#'
#' @keywords internal
#' @noRd
field_set <- function(x, name, value) {
  if (!is_value(x)) {
    r8_abort("`x` ", not_value_text(x), class = "r8_value_error")
  }
  res <- .Call(wrap__r8_set_field, x, name, value)
  if (is.null(res)) {
    r8_abort(
      "`",
      name,
      "` is not a field of `",
      value_type(x),
      "`.",
      class = "r8_value_error"
    )
  }
  if (inherits(res, "r8_problems")) {
    variant <- value_variant(x)
    construct_error(
      res,
      value_type(x),
      if (is.null(variant)) "" else variant,
      inputs = structure(list(value), names = name),
      assignment = paste0("x$", name, " <- value")
    )
  }
  res
} # /R8::field_set


# %% fields_refused ----
#' Signal why an assignment of several fields is refused
#'
#' @param x An R8 value.
#' @param fields The index of the assignment.
#' @param values The new values.
#' @param res Optional `r8_problems`: The problems of the new value, for an
#'   assignment of fields by name from a list.
#'
#' @return Does not return; signals `r8_value_error`.
#'
#' @keywords internal
#' @noRd
fields_refused <- function(x, fields, values, res = NULL) {
  refuse <- function(...) r8_abort(..., class = "r8_value_error")
  if (!is.character(fields)) {
    refuse("Fields are replaced by name: x[c(\"a\", \"b\")] <- list(v, w).")
  }
  unknown <- is.na(match(fields, attr(x, "names", exact = TRUE)))
  if (any(unknown)) {
    unknown_field(x, fields[unknown][[1L]])
  }
  if (anyDuplicated(fields)) {
    refuse(
      "The field `",
      fields[[anyDuplicated(fields)]],
      "` is replaced twice."
    )
  }
  if (!is.vector(values, "list")) {
    refuse(
      "x[names] <- value takes a plain list, such as list(v, w); ",
      "x$name <- v replaces one field."
    )
  }
  if (length(values) != length(fields)) {
    refuse(
      "x[names] <- value takes one new value per field; found ",
      length(fields),
      if (length(fields) == 1L) " field and " else " fields and ",
      length(values),
      if (length(values) == 1L) " value." else " values."
    )
  }
  if (!is.null(names(values)) && !identical(names(values), fields)) {
    refuse(
      "The names of the list are the fields it replaces, in their order: ",
      paste(fields, collapse = ", "),
      "; found ",
      paste(names(values), collapse = ", "),
      "."
    )
  }
  shown <- if (length(fields) > 4L) c(fields[1:3], "...") else fields
  index <- paste(
    ifelse(shown == "...", "...", paste0("\"", shown, "\"")),
    collapse = ", "
  )
  construct_error(
    res,
    value_type(x),
    value_variant(x) %||% "",
    inputs = structure(values, names = fields),
    assignment = paste0(
      "x[",
      if (length(fields) == 1L) index else paste0("c(", index, ")"),
      "] <- value"
    )
  )
} # /R8::fields_refused


# %% @.r8_value ----
#' `@` on an R8 value
#'
#' `@` reads the associated items of a type, not the fields of a value.
#'
#' @param object An R8 value.
#' @param name The name.
#'
#' @return Does not return.
#'
#' @rdname r8_value_fields
#' @export
`@.r8_value` <- function(object, name) {
  r8_abort(
    "`@` reads the items of a type; read the field of a value with `$`: x$",
    as.character(substitute(name)),
    ".",
    class = "r8_value_error"
  )
} # /R8::@.r8_value


# %% guards ----
# Replacement functions that would change a value without validation.
guard <- function(what) {
  force(what)
  function(x, ..., value) {
    r8_abort(
      "`",
      what,
      "` would change an R8 value without validation; `$<-` and `[<-` replace fields.",
      class = "r8_value_error"
    )
  }
}

#' Guards on R8 values
#'
#' `names<-`, `length<-`, `dim<-`, and `levels<-` would change a value without
#' validation, so they abort; `[[` and `[` with a number abort, as fields are
#' read by name.
#'
#' @param x An R8 value.
#' @param value Not used.
#' @param ... Not used.
#'
#' @return Does not return.
#'
#' @name r8_value_guards
#' @export
`names<-.r8_value` <- guard("names<-")

#' @rdname r8_value_guards
#' @export
`length<-.r8_value` <- guard("length<-")

#' @rdname r8_value_guards
#' @export
`dim<-.r8_value` <- guard("dim<-")

#' @rdname r8_value_guards
#' @export
`levels<-.r8_value` <- guard("levels<-")


# %% r8_to_json ----
#' Write an R8 value as JSON
#'
#' The value's document: `"$type"`, `"$types"` (the version of every type it
#' contains), `"$declarations"` (the hash of what the declaration of each says about
#' a value), then its fields in declaration order.
#'
#' @param x An R8 value.
#' @param declarations Logical: Write the declarations of the types too, as IR
#'   documents in `"$ir"`, so that [r8_from_json()] can tell what differs after a
#'   declaration changes, and read the document where its fields mean the same.
#'
#' @return Character: The JSON document.
#'
#' @examples
#' op <- options(r8.namespace = "demo")
#' Point <- new_struct("Point", fields = list(x = type_double(), y = type_double()))
#' r8_to_json(Point(x = 1, y = 2))
#' options(op)
#'
#' @export
r8_to_json <- function(x, declarations = FALSE) {
  if (!is_value(x)) {
    value_required(x)
  }
  if (!isTRUE(declarations) && !isFALSE(declarations)) {
    r8_abort("`declarations` is TRUE or FALSE.", class = "r8_type_error")
  }
  res <- r8_value_to_document(x, declarations)
  if (!is.null(res[["errors"]])) {
    value_abort("Cannot write", value_type(x), res[["errors"]])
  }
  res[["json"]]
} # /R8::r8_to_json


# %% r8_from_json ----
#' Read an R8 value from JSON
#'
#' Reads a document of `type`: checks `"$type"`, the versions in `"$types"`, and
#' the declarations in `"$declarations"`, validates the value, and returns it. A type the document names
#' is looked up among the registered types only; the document never selects code
#' to load.
#'
#' @param type An R8 type object.
#' @param json Character: The JSON document.
#' @param load Optional Character: Packages to load, with `loadNamespace()`,
#'   before the read, such as the package that declares an implementor of a
#'   `dyn()` field.
#' @param declaration Character \{"registered", "any"\}: `"registered"` refuses a
#'   document that was written under another declaration of a type than the
#'   registered one, unless it holds the declarations
#'   (`r8_to_json(declarations = TRUE)`) and its fields mean the same under both, as
#'   [r8_update()] states. `"any"` reads it as one of the registered declaration.
#'   A document without `"$declarations"`, as one written by hand, claims none.
#'
#' @return An R8 value. A document that names types no loaded package declares
#'   raises `r8_unknown_type_error`, which names each namespace and the
#'   installed packages that declare it.
#'
#' @examples
#' op <- options(r8.namespace = "demo")
#' Point <- new_struct("Point", fields = list(x = type_double(), y = type_double()))
#' r8_from_json(Point, r8_to_json(Point(x = 1, y = 2)))
#' options(op)
#'
#' @export
r8_from_json <- function(type, json, load = NULL, declaration = "registered") {
  if (!inherits(type, "r8_type")) {
    r8_abort("`type` is an R8 type object.", class = "r8_type_error")
  }
  if (!identical(declaration, "registered") && !identical(declaration, "any")) {
    r8_abort(
      "`declaration` is \"registered\" or \"any\".",
      class = "r8_type_error"
    )
  }
  json <- document_text(json)
  load_packages(load)
  ensure_registered(type)
  if (identical(attr(type, "r8_storage"), "rust")) {
    check_codec(attr(type, "r8_name"), attr(type, "r8_package") %||% "")
  }
  res <- r8_value_from_document(
    json,
    attr(type, "r8_name"),
    identical(declaration, "any")
  )
  if (!is.null(res[["errors"]])) {
    if ("unknown_type" %in% res[["errors"]][["code"]]) {
      unknown_type_abort(json)
    }
    value_abort(
      "Cannot read",
      attr(type, "r8_name"),
      declaration_hint(res[["errors"]])
    )
  }
  res[["value"]]
} # /R8::r8_from_json


# %% document_text ----
#' The text of a document
#'
#' @param json Character: The document, or its lines.
#'
#' @return Character: One string, in UTF-8; another object, and a text that is not
#'   valid UTF-8, raise `r8_type_error`.
#'
#' @keywords internal
#' @noRd
document_text <- function(json) {
  if (!is.character(json) || anyNA(json)) {
    r8_abort(
      "`json` is the text of a document: one string, or its lines, such as ",
      "readLines(file) gives.",
      class = "r8_type_error"
    )
  }
  json <- paste(enc2utf8(as.vector(json)), collapse = "\n")
  if (!validUTF8(json)) {
    r8_abort(
      "`json` is not valid UTF-8. Read the file with its encoding, such as ",
      "readLines(file, encoding = \"latin1\"), and pass the text to enc2utf8().",
      class = "r8_type_error"
    )
  }
  json
} # /R8::document_text


# %% declaration_hint ----
#' What reads a document of another declaration, in its problems
#'
#' @param errors List: The problems of a document.
#'
#' @return List: `errors`, with a sentence after the message of a document that
#'   does not hold its declarations.
#'
#' @keywords internal
#' @noRd
declaration_hint <- function(errors) {
  at <- errors[["code"]] == "declaration_changed" &
    errors[["path"]] == "/$declarations"
  errors[["message"]][at] <- paste0(
    errors[["message"]][at],
    "; `r8_to_json(x, declarations = TRUE)` writes one that does, and ",
    "`r8_from_json(declaration = \"any\")` reads it as one of the registered"
  )
  errors
} # /R8::declaration_hint


# %% load_packages ----
#' Load the packages that `load =` names
#'
#' @param load Optional Character: Package names.
#'
#' @return `NULL`, invisibly.
#'
#' @keywords internal
#' @noRd
load_packages <- function(load) {
  if (is.null(load)) {
    return(invisible(NULL))
  }
  if (!is.character(load) || anyNA(load)) {
    r8_abort(
      "`load` names packages, such as load = \"rtemis.ml\".",
      class = "r8_type_error"
    )
  }
  for (p in load) {
    if (!requireNamespace(p, quietly = TRUE)) {
      r8_abort(
        "Package '",
        p,
        "' of `load` is not installed.",
        class = "r8_type_error"
      )
    }
  }
  invisible(NULL)
} # /R8::load_packages


# %% unknown_type_abort ----
#' Signal the unregistered types a document names
#'
#' Reads the document's `"$type"` and `"$types"`, and names, for each
#' unregistered type, its namespace and the installed packages that declare
#' it (`Config/R8/namespace`, or the package name for a package that imports
#' R8).
#'
#' @param json Character: The JSON document.
#'
#' @return Does not return; signals `r8_unknown_type_error`.
#'
#' @keywords internal
#' @noRd
unknown_type_abort <- function(json) {
  doc <- r8_json_parse(paste(json, collapse = "\n"))
  named <- unique(c(
    if (is.character(doc[["$type"]])) doc[["$type"]],
    names(doc[["$types"]])
  ))
  missing <- named[!vapply(named, r8_is_registered, logical(1))]
  if (length(missing) == 0L) {
    return(invisible(NULL))
  }
  namespaces <- unique(sub("::.*$", "", missing))
  providers <- r8_namespace_providers()
  lines <- vapply(
    namespaces,
    function(ns) {
      types <- missing[startsWith(missing, paste0(ns, "::"))]
      owner <- r8_namespace_owner(ns)
      pkgs <- providers[[ns]] %||% character()
      what <- paste0("`", types, "`", collapse = ", ")
      if (nzchar(owner)) {
        paste0(
          what,
          ": package '",
          owner,
          "' ",
          utils::packageVersion(owner),
          " is loaded and declares no such type."
        )
      } else if (length(pkgs) > 0L) {
        paste0(
          what,
          ": namespace `",
          ns,
          "` is declared by the installed ",
          if (length(pkgs) == 1L) "package " else "packages ",
          paste0("'", pkgs, "'", collapse = ", "),
          "; read with load = \"",
          pkgs[[1L]],
          "\"."
        )
      } else {
        paste0(what, ": no installed package declares namespace `", ns, "`.")
      }
    },
    ""
  )
  cond <- structure(
    class = c(
      "r8_unknown_type_error",
      "r8_value_error",
      "r8_error",
      "error",
      "condition"
    ),
    list(
      message = paste0(
        "The document names types that no loaded package declares:\n",
        paste0("- ", lines, collapse = "\n")
      ),
      call = NULL,
      namespace = namespaces,
      packages = providers[namespaces]
    )
  )
  stop(cond)
} # /R8::unknown_type_abort


# %% r8_namespace_providers ----
#' The installed packages that declare each R8 namespace
#'
#' Reads the `DESCRIPTION` of every installed package once per library path
#' set: a package declares the namespace of its `Config/R8/namespace` field,
#' or its own name when it imports or depends on R8.
#'
#' @return Named list: By namespace, the package names.
#'
#' @keywords internal
#' @noRd
r8_namespace_providers <- function() {
  key <- paste(.libPaths(), collapse = "\n")
  cached <- .r8_providers[[key]]
  if (!is.null(cached)) {
    return(cached)
  }
  out <- list()
  for (lib in .libPaths()) {
    for (pkg in list.files(lib)) {
      d <- file.path(lib, pkg, "DESCRIPTION")
      if (!file.exists(d)) {
        next
      }
      f <- tryCatch(
        read.dcf(
          d,
          fields = c("Package", "Config/R8/namespace", "Imports", "Depends")
        ),
        error = function(e) NULL
      )
      if (is.null(f)) {
        next
      }
      ns <- f[1L, "Config/R8/namespace"]
      uses_r8 <- any(grepl("\\bR8\\b", c(f[1L, "Imports"], f[1L, "Depends"])))
      if (is.na(ns)) {
        if (!uses_r8) {
          next
        }
        ns <- f[1L, "Package"]
      }
      out[[ns]] <- unique(c(out[[ns]], f[1L, "Package"]))
    }
  }
  assign(key, out, envir = .r8_providers)
  out
} # /R8::r8_namespace_providers


# %% r8_schema ----
#' The JSON Schema of a type's values
#'
#' Writes the JSON Schema (draft 2020-12) that the documents of `type`
#' satisfy, for tools outside R8: each named type it reaches is in `$defs`,
#' and the root admits the document envelope. R8's facts that JSON Schema
#' cannot state, such as rules and units, are annotations under `x-r8`.
#'
#' @param type An R8 type object.
#' @param closed Logical: Write each `dyn()` field as the closed list of the
#'   implementors registered in the session, each with its tag value as a
#'   constant, and each `subtype()` field as the struct and its registered
#'   descendants, each with `"$type"` as a constant, in place of an object
#'   that names its type. The annotation `x-r8.closed` is then
#'   `"registered"`.
#'
#' @return Character: The schema as JSON text.
#'
#' @examples
#' op <- options(r8.namespace = "demo")
#' Point <- new_struct("Point", fields = list(x = type_double(), y = type_double()))
#' cat(r8_schema(Point))
#' options(op)
#'
#' @export
r8_schema <- function(type, closed = FALSE) {
  if (!inherits(type, "r8_type")) {
    r8_abort("`type` is an R8 type object.", class = "r8_type_error")
  }
  ensure_registered(type)
  r8_type_schema(attr(type, "r8_name"), isTRUE(closed))
} # /R8::r8_schema
