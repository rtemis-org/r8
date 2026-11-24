# conform.R
# ::R8::
# 2026- EDG rtemis.org

# %% conform_value ----
#' Conform one value to a field type
#'
#' @param x The value.
#' @param type An IR field type (a list).
#' @param path Character: The field's path, for the report.
#' @param changes Environment: Collects the changes in `changes$log`.
#'
#' @return The conformed value.
#'
#' @keywords internal
#' @noRd
conform_value <- function(x, type, path, changes) {
  note <- function(what) {
    changes[["log"]] <- c(changes[["log"]], paste0(path, ": ", what))
  }
  if (is.null(x)) {
    return(NULL)
  }
  kind <- type[["kind"]]
  if (kind == "option") {
    return(conform_value(x, type[["inner"]], path, changes))
  }
  if (inherits(x, "Rle")) {
    x <- methods::as(x, "vector")
    note("expanded an Rle")
  }
  scalar_kinds <- c(
    "logical",
    "integer",
    "double",
    "character",
    "date",
    "posixct"
  )
  if (
    kind %in%
      scalar_kinds ||
      (kind == "vec" &&
        type[["element"]][["kind"]] %in% c(scalar_kinds, "option"))
  ) {
    if (inherits(x, "POSIXlt")) {
      x <- as.POSIXct(x)
      note("converted POSIXlt to POSIXct")
    }
    keep <- if (
      kind %in% c("date", "posixct") || inherits(x, c("Date", "POSIXct"))
    ) {
      c("class", "tzone")
    } else {
      character()
    }
    extra <- setdiff(names(attributes(x)), keep)
    if (length(extra) > 0L) {
      attributes(x) <- attributes(x)[intersect(names(attributes(x)), keep)]
      note(paste0("removed the attributes ", paste(extra, collapse = ", ")))
    }
    return(x)
  }
  if (kind == "factor") {
    if (is.character(x) && !is.null(type[["levels"]])) {
      x <- factor(
        x,
        levels = unlist(type[["levels"]]),
        ordered = isTRUE(type[["ordered"]])
      )
      note("converted a character vector to a factor with the declared levels")
    }
    extra <- setdiff(names(attributes(x)), c("levels", "class"))
    if (length(extra) > 0L) {
      attributes(x) <- attributes(x)[c("levels", "class")]
      note(paste0("removed the attributes ", paste(extra, collapse = ", ")))
    }
    return(x)
  }
  if (kind == "table" || kind == "table_of") {
    if (!identical(class(x), "data.frame") && inherits(x, "data.frame")) {
      cls <- class(x)[1L]
      x <- as.data.frame(x)
      class(x) <- "data.frame"
      note(paste0("converted a ", cls, " to a data.frame"))
    }
    if (
      is.data.frame(x) &&
        !identical(attr(x, "row.names"), seq_len(nrow(x))) &&
        is.character(attr(x, "row.names"))
    ) {
      rownames(x) <- NULL
      note("reset the row names")
    }
    return(x)
  }
  if (kind == "vec" && is.list(x)) {
    if (!is.null(names(x))) {
      x <- unname(x)
      note("removed the names")
    }
    return(lapply(seq_along(x), function(i) {
      conform_value(
        x[[i]],
        type[["element"]],
        paste0(path, "/", i - 1L),
        changes
      )
    }))
  }
  if (kind == "named" && is.list(x) && !is_value(x)) {
    ir <- tryCatch(r8_type_ir(type[["name"]]), error = function(e) NULL)
    if (identical(ir[["kind"]], "struct")) {
      note(paste0("constructed a `", type[["name"]], "` from a list"))
      return(conform_struct(x, type[["name"]], path, changes))
    }
  }
  x
} # /R8::conform_value


# %% conform_struct ----
#' Conform the fields of a struct and construct it
#'
#' @param x Named list: The fields.
#' @param qname Character: The struct's qualified name.
#' @param path Character: The struct's path.
#' @param changes Environment: Collects the changes.
#'
#' @return An R8 value.
#'
#' @keywords internal
#' @noRd
conform_struct <- function(x, qname, path, changes) {
  ir <- r8_type_ir(qname)
  fields <- list()
  for (f in ir[["fields"]]) {
    name <- f[["name"]]
    if (!is.null(x[[name]])) {
      fields[[name]] <- conform_value(
        x[[name]],
        f[["type"]],
        paste0(path, "/", name),
        changes
      )
    }
  }
  unknown <- setdiff(
    names(x),
    vapply(ir[["fields"]], function(f) f[["name"]], character(1))
  )
  if (length(unknown) > 0L) {
    changes[["log"]] <- c(
      changes[["log"]],
      paste0(
        path,
        ": dropped the members ",
        paste(unknown, collapse = ", "),
        ", which are not fields"
      )
    )
  }
  names <- vapply(ir[["fields"]], function(f) f[["name"]], character(1))
  r8_construct(
    qname,
    stats::setNames(lapply(names, function(n) fields[[n]]), names)
  )
} # /R8::conform_struct


# %% r8_conform ----
#' Conform an R object to a type
#'
#' Turns an ordinary R object into a valid input for a struct type and
#' constructs the value: it removes names and other attributes from scalar
#' and vector fields, converts a character vector to a factor with declared
#' levels, `POSIXlt` to `POSIXct`, a tibble or data.table to a data.frame,
#' resets row names, expands an `Rle`, constructs a nested struct from a
#' named list, and drops members that are not fields. It reports each change
#' in one message. Construction itself stays strict: a value `r8_conform()`
#' cannot make valid raises the same errors as the type's constructor.
#'
#' @param x Named list: The fields of a value.
#' @param type An R8 struct type object.
#'
#' @return The R8 value.
#'
#' @examples
#' op <- options(r8.namespace = "demo")
#' Weights <- new_struct("Weights", fields = list(w = vec(type_double(min = 0))))
#' q <- stats::quantile(c(1, 2, 3))
#' x <- r8_conform(list(w = q), Weights)
#' options(op)
#'
#' @export
r8_conform <- function(x, type) {
  if (!inherits(type, "r8_struct_type")) {
    r8_abort("`type` is an R8 struct type object.", class = "r8_type_error")
  }
  if (!is.list(x) || is.null(names(x))) {
    r8_abort("`x` is a named list of fields.", class = "r8_type_error")
  }
  changes <- new.env(parent = emptyenv())
  changes[["log"]] <- character()
  value <- conform_struct(x, attr(type, "r8_name"), "", changes)
  if (length(changes[["log"]]) > 0L) {
    message(
      "r8_conform() made ",
      length(changes[["log"]]),
      " change(s) for `",
      attr(type, "r8_name"),
      "`:\n",
      paste0("  ", changes[["log"]], collapse = "\n")
    )
  }
  value
} # /R8::r8_conform
