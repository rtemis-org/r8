# values.R
# ::R8::
# 2026- EDG rtemis.org

# The installed packages that declare each R8 namespace, by the library
# paths read, for the message of an unresolved type.
.r8_providers <- new.env(parent = emptyenv())

# An R8 value in R is a named list of its fields, of classes
# c("ns::Type", "r8_value"), with the attributes `r8_type`, `r8_version`, and,
# for an enum value, `r8_variant`. Values are immutable: `$<-` and `[[<-`
# return a new value, validated.

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
#'
#' @return Does not return; signals `r8_value_error`, whose field
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
  variant = ""
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
    class = "r8_value_error",
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
#' @return `NULL`, invisibly; signals `r8_codec_missing` when the codec is not
#'   registered.
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
      class = c("r8_codec_missing", "r8_value_error")
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
#' `x$name` and `x[["name"]]` read a field by its exact name; an unknown name
#' is an error. `x$name <- v` and `x[["name"]] <- v` return a new value,
#' validated; `x` is unchanged.
#'
#' @param x An R8 value.
#' @param name Character: The field name.
#' @param i Character: The field name.
#' @param value The new field value.
#' @param ... Not used.
#'
#' @return The field's value, or the new R8 value.
#'
#' @name r8_value_fields
#' @export
`$.r8_value` <- function(x, name) {
  v <- .subset2(x, name)
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
  v <- .subset2(x, i)
  if (is.null(v)) field_get(x, i) else v
} # /R8::[[.r8_value


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


# %% field_get ----
#' Read a field
#'
#' @param x An R8 value.
#' @param name Character: The field name.
#'
#' @return The field's value.
#'
#' @keywords internal
#' @noRd
field_get <- function(x, name) {
  v <- .subset2(x, name)
  # `names()` dispatches; the attribute does not.
  if (is.null(v) && is.na(match(name, attr(x, "names", exact = TRUE)))) {
    variant <- attr(x, "r8_variant")
    label <- paste0(
      attr(x, "r8_type"),
      if (!is.null(variant)) paste0("@", variant)
    )
    fields <- names(x)
    r8_abort(
      "`",
      name,
      "` is not a field of `",
      label,
      "`",
      if (length(fields) == 0L) {
        ", which has no fields."
      } else {
        paste0(". Fields: ", paste(fields, collapse = ", "), ".")
      },
      class = "r8_value_error"
    )
  }
  v
} # /R8::field_get


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
  res <- .Call(wrap__r8_set_field, x, name, value)
  if (is.null(res)) {
    r8_abort(
      "`",
      name,
      "` is not a field of `",
      attr(x, "r8_type"),
      "`.",
      class = "r8_value_error"
    )
  }
  if (inherits(res, "r8_problems")) {
    variant <- attr(x, "r8_variant")
    construct_error(
      res,
      attr(x, "r8_type"),
      if (is.null(variant)) "" else variant,
      inputs = structure(list(value), names = name),
      assignment = paste0("x$", name, " <- value")
    )
  }
  res
} # /R8::field_set


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
      "` would change an R8 value without validation; values are replaced field by field with `$<-`.",
      class = "r8_value_error"
    )
  }
}

#' Guards on R8 values
#'
#' `names<-`, `length<-`, `[<-`, `dim<-`, and `levels<-` would change a value
#' without validation, so they abort; `[[` with a number aborts, as fields
#' are read by name.
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
`[<-.r8_value` <- guard("[<-")

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
#' contains), then its fields in declaration order.
#'
#' @param x An R8 value.
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
r8_to_json <- function(x) {
  if (!inherits(x, "r8_value")) {
    r8_abort("`x` is an R8 value.", class = "r8_type_error")
  }
  res <- r8_value_to_document(x)
  if (!is.null(res[["errors"]])) {
    value_abort("Cannot write", attr(x, "r8_type"), res[["errors"]])
  }
  res[["json"]]
} # /R8::r8_to_json


# %% r8_from_json ----
#' Read an R8 value from JSON
#'
#' Reads a document of `type`: checks `"$type"` and the versions in
#' `"$types"`, validates the value, and returns it. A type the document names
#' resolves among the registered types only; the document never selects code
#' to load.
#'
#' @param type An R8 type object.
#' @param json Character: The JSON document.
#' @param load Optional Character: Packages to load, with `loadNamespace()`,
#'   before the read, such as the package that declares an implementor of a
#'   `dyn()` field.
#'
#' @return An R8 value. A document that names types no loaded package
#'   declares raises `r8_unresolved_type`, which names each namespace and the
#'   installed packages that declare it.
#'
#' @examples
#' op <- options(r8.namespace = "demo")
#' Point <- new_struct("Point", fields = list(x = type_double(), y = type_double()))
#' r8_from_json(Point, r8_to_json(Point(x = 1, y = 2)))
#' options(op)
#'
#' @export
r8_from_json <- function(type, json, load = NULL) {
  if (!inherits(type, "r8_type")) {
    r8_abort("`type` is an R8 type object.", class = "r8_type_error")
  }
  load_packages(load)
  ensure_registered(type)
  if (identical(attr(type, "r8_storage"), "rust")) {
    check_codec(attr(type, "r8_name"), attr(type, "r8_package") %||% "")
  }
  res <- r8_value_from_document(
    paste(json, collapse = "\n"),
    attr(type, "r8_name")
  )
  if (!is.null(res[["errors"]])) {
    if ("unresolved_type" %in% res[["errors"]][["code"]]) {
      unresolved_abort(json)
    }
    value_abort("Cannot read", attr(type, "r8_name"), res[["errors"]])
  }
  res[["value"]]
} # /R8::r8_from_json


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


# %% unresolved_abort ----
#' Signal the unregistered types a document names
#'
#' Reads the document's `"$type"` and `"$types"`, and names, for each
#' unregistered type, its namespace and the installed packages that declare
#' it (`Config/R8/namespace`, or the package name for a package that imports
#' R8).
#'
#' @param json Character: The JSON document.
#'
#' @return Does not return; signals `r8_unresolved_type`.
#'
#' @keywords internal
#' @noRd
unresolved_abort <- function(json) {
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
      "r8_unresolved_type",
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
} # /R8::unresolved_abort


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
#' @param implementors Logical: Write each `dyn()` field as the closed list
#'   of the implementors registered in the session, each with its tag value
#'   as a constant, in place of an object that names its type.
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
r8_schema <- function(type, implementors = FALSE) {
  if (!inherits(type, "r8_type")) {
    r8_abort("`type` is an R8 type object.", class = "r8_type_error")
  }
  ensure_registered(type)
  r8_type_schema(attr(type, "r8_name"), isTRUE(implementors))
} # /R8::r8_schema
