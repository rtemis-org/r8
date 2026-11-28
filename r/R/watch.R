# watch.R
# ::R8::
# 2026- EDG rtemis.org

# The state of a call of a table function that r8explain() explains, or whose
# run's plan r8bench() or a progress hook with `stages()` collects: the table
# functions entered, the call's start, and the plan that its run leaves.
# spec: R8/raven#benchmarks

# %% .r8_watch ----
# `explaining`, set by r8explain(); `collecting`, set by r8bench(); `hooked`,
# set by map_progress() for a hook with `stages()`; each is `TRUE` or `NULL`,
# tested by `is.null()`, a primitive: `isTRUE()` is a closure, whose calls cost
# about 1 us of a map of 50 us at the size of rtemis's tuner. `depth`, the table
# functions of R8 entered; `start`, the seconds of `r8_clock()` at the first's
# entry; `call`, its call; `label`, the label of its map; `pending`, the plan
# that its run left; and `plans`, the plans that r8bench() collects.
.r8_watch <- new.env(parent = emptyenv())


# %% watching ----
#' Whether a call of a table function is explained or its plan collected
#'
#' @return Logical.
#'
#' @keywords internal
#' @noRd
watching <- function() {
  !is.null(.r8_watch[["explaining"]]) ||
    !is.null(.r8_watch[["collecting"]]) ||
    !is.null(.r8_watch[["hooked"]])
} # /R8::watching


# %% explaining ----
#' Whether r8explain() runs the current call
#'
#' @return Logical.
#'
#' @keywords internal
#' @noRd
explaining <- function() {
  !is.null(.r8_watch[["explaining"]]) && identical(.r8_watch[["depth"]], 1L)
} # /R8::explaining


# %% collecting ----
#' Whether the current call's run leaves its plan for a collector
#'
#' @return Logical.
#'
#' @keywords internal
#' @noRd
collecting <- function() {
  (!is.null(.r8_watch[["collecting"]]) || !is.null(.r8_watch[["hooked"]])) &&
    identical(.r8_watch[["depth"]], 1L)
} # /R8::collecting


# %% watch_enter ----
#' Count the entry of a table function while a call is watched
#'
#' A table function of R8 calls it on entry, and `watch_leave()` on exit: the
#' first that it enters is the call watched, at depth 1, whose start and call
#' it records; a table function that another calls runs as it would.
#'
#' @return Logical: Whether a call is watched, so that the caller leaves.
#'
#' @keywords internal
#' @noRd
watch_enter <- function() {
  if (!watching()) {
    return(FALSE)
  }
  depth <- (.r8_watch[["depth"]] %||% 0L) + 1L
  .r8_watch[["depth"]] <- depth
  if (depth == 1L) {
    .r8_watch[["start"]] <- r8_clock()
    .r8_watch[["call"]] <- table_call()
    .r8_watch[["label"]] <- NULL
    .r8_watch[["pending"]] <- NULL
  }
  TRUE
} # /R8::watch_enter


# %% watch_leave ----
#' Count the exit of a table function while a call is watched
#'
#' The exit of the call watched delivers a plan that its run left and that no
#' return delivered, as the plan of a call that failed.
#'
#' @return `NULL`, invisibly.
#'
#' @keywords internal
#' @noRd
watch_leave <- function() {
  if (!watching()) {
    return(invisible(NULL))
  }
  if (identical(.r8_watch[["depth"]], 1L)) {
    watch_done("failed")
  }
  .r8_watch[["depth"]] <- (.r8_watch[["depth"]] %||% 1L) - 1L
  invisible(NULL)
} # /R8::watch_leave


# %% table_call ----
#' The call of the table function that the user wrote
#'
#' @return The innermost call of `r8map()`, `r8summarize()`, or `r8set()` on
#'   the stack, or `NULL`.
#'
#' @keywords internal
#' @noRd
table_call <- function() {
  calls <- sys.calls()
  names <- c("r8map", "r8summarize", "r8set")
  for (i in rev(seq_along(calls))) {
    head <- calls[[i]][[1L]]
    if (is.call(head) && identical(head[[1L]], as.name("::"))) {
      head <- head[[3L]]
    }
    if (is.name(head) && as.character(head) %in% names) {
      return(calls[[i]])
    }
  }
  NULL
} # /R8::table_call


# %% watch_trace ----
#' The trace that the binding takes for a run whose plan is collected
#'
#' @param fn Character: The table function.
#' @param columns Character: The columns computed, for a label.
#'
#' @return `NULL` without a collector, or a list of `name`, `label`, and
#'   `before`, the seconds from the call's start.
#'
#' @keywords internal
#' @noRd
watch_trace <- function(fn, columns) {
  if (!collecting()) {
    return(NULL)
  }
  list(
    name = fn,
    label = .r8_watch[["label"]] %||% paste(columns, collapse = ", "),
    before = r8_clock() - (.r8_watch[["start"]] %||% 0)
  )
} # /R8::watch_trace


# %% watch_pending ----
#' Keep the plan that a run left, for its call's end
#'
#' @param res The list that `r8_table_calc()` gives.
#' @param functions Optional list: The typed functions, by qualified name.
#' @param trace List: The trace that `watch_trace()` gave.
#'
#' @return `NULL`, invisibly.
#'
#' @keywords internal
#' @noRd
watch_pending <- function(res, functions, trace) {
  if (is.list(res) && !is.null(res[["plan"]])) {
    .r8_watch[["pending"]] <- list(
      nodes = res[["plan"]],
      core_end = r8_clock(),
      before = trace[["before"]],
      functions = functions
    )
  }
  invisible(NULL)
} # /R8::watch_pending


# %% watch_items ----
#' Time the calls of a map over items, for a collector of its plan
#'
#' A map over the items of a list or a vector makes its calls in R, which times
#' them: the plan of `r8_list_plan()`, whose `call` stage runs from this call to
#' the call of the function that it gives.
#'
#' @param name Character: The typed function's qualified name.
#' @param items Integer: The items.
#' @param functions List: The typed function, by its qualified name.
#'
#' @return `NULL` without a collector, or a function of `item` and `message`,
#'   called once the calls end, which stamps the `call` stage completed, or,
#'   with the item from 1 and the message of its error, failed.
#'
#' @keywords internal
#' @noRd
watch_items <- function(name, items, functions) {
  if (!collecting()) {
    return(NULL)
  }
  start <- .r8_watch[["start"]] %||% r8_clock()
  before <- r8_clock() - start
  .r8_watch[["pending"]] <- list(
    nodes = r8_list_plan("r8map", name, items)[["nodes"]],
    before = before,
    functions = functions
  )
  function(item = NULL, message = NULL) {
    pending <- .r8_watch[["pending"]]
    if (is.null(pending)) {
      return(invisible(NULL))
    }
    pending[["core_end"]] <- r8_clock()
    end <- pending[["core_end"]] - start
    nodes <- pending[["nodes"]]
    at <- match("r8.call", vapply(nodes, `[[`, "", "kind"))
    nodes[[at]][["t_start"]] <- before
    nodes[[at]][["t_end"]] <- end
    nodes[[at]][["meta"]][["busy"]] <- end - before
    if (is.null(message)) {
      nodes[[at]][["status"]] <- "completed"
    } else {
      nodes[[at]][["status"]] <- "failed"
      nodes[[at]][["meta"]][["row"]] <- as.integer(item)
      nodes[[at]][["meta"]][["message"]] <- message
      nodes[[1L]][["status"]] <- "failed"
    }
    pending[["nodes"]] <- nodes
    .r8_watch[["pending"]] <- pending
    invisible(NULL)
  }
} # /R8::watch_items


# %% watch_done ----
#' End the plan that a run left, and give it to its collectors
#'
#' Stamps `check` from the call's start to the core's, `assemble` from the
#' core's return to this call for a call that completed, and the call's node;
#' the status of a call that the core marked as failed stays. r8bench() keeps
#' the plan.
#'
#' @param status Character \{"completed", "failed", "aborted"\}: How the call
#'   ended.
#'
#' @return The `r8_plan`, or `NULL` without a plan.
#'
#' @keywords internal
#' @noRd
watch_done <- function(status) {
  pending <- .r8_watch[["pending"]]
  if (is.null(pending)) {
    return(NULL)
  }
  .r8_watch[["pending"]] <- NULL
  start <- .r8_watch[["start"]] %||% pending[["core_end"]]
  now <- r8_clock() - start
  nodes <- pending[["nodes"]]
  kinds <- vapply(nodes, `[[`, "", "kind")
  root <- nodes[[1L]]
  root[["t_start"]] <- 0
  root[["t_end"]] <- now
  if (!identical(root[["status"]], "failed")) {
    root[["status"]] <- status
  }
  nodes[[1L]] <- root
  check <- match("r8.check", kinds)
  if (!is.na(check)) {
    before <- pending[["before"]] %||% 0
    nodes[[check]][["t_start"]] <- 0
    nodes[[check]][["t_end"]] <- before
    nodes[[check]][["meta"]][["busy"]] <- before
    nodes[[check]][["status"]] <- "completed"
  }
  assemble <- match("r8.assemble", kinds)
  if (!is.na(assemble) && identical(root[["status"]], "completed")) {
    core_end <- pending[["core_end"]] - start
    nodes[[assemble]][["t_start"]] <- core_end
    nodes[[assemble]][["t_end"]] <- now
    nodes[[assemble]][["meta"]][["busy"]] <- now - core_end
    nodes[[assemble]][["status"]] <- "completed"
  }
  plan <- plan_of(nodes, .r8_watch[["call"]], pending[["functions"]])
  if (isTRUE(.r8_watch[["collecting"]])) {
    .r8_watch[["plans"]] <- c(.r8_watch[["plans"]], list(plan))
  }
  plan
} # /R8::watch_done
