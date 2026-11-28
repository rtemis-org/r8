# explain.R
# ::R8::
# 2026- EDG rtemis.org

# The plan of a call of a table function before it runs: r8explain(), and the
# plan's text, flowchart, and table of stages.
# spec: R8/raven#plans

# %% explained ----
#' End the call that r8explain() runs, with its plan
#'
#' A refused plan raises the error that the call raises.
#'
#' @param plan List: The plan that the binding gives, or an `r8_refused`.
#' @param x Optional: The table, for the error of a refused plan.
#' @param functions Optional list: The typed functions that the call calls, by
#'   qualified name.
#'
#' @return Does not return: signals an `r8_explained` condition.
#'
#' @keywords internal
#' @noRd
explained <- function(plan, x = NULL, functions = NULL) {
  if (inherits(plan, "r8_refused")) {
    table_result(plan, x)
  }
  stop(structure(
    class = c("r8_explained", "condition"),
    list(
      message = "r8explain(): the plan of the call.",
      call = NULL,
      plan = plan,
      functions = functions
    )
  ))
} # /R8::explained


# %% r8explain ----
#' The plan of a call of a table function, before it runs
#'
#' `r8explain(call)` takes a call of `r8map()`, `r8summarize()`, or `r8set()`,
#' unevaluated, and returns its plan: the stages of the call, where each runs, in
#' Rust or in R, the typed function that each calls and the language of its body,
#' the calls, and the stages that each reads. It evaluates the call's arguments in
#' the caller's frame, runs the function's checks, compiles its expressions against
#' the table, and numbers the groups of the rows, then stops before it evaluates a
#' row or calls a typed function.
#'
#' @section Stages:
#' A call over the columns of a table has the stages `check`, the checks of its
#' arguments, in R; `compile`, of its expressions against the table's columns;
#' `group`, of the rows by the key columns; `aggregate`, of the aggregates of the
#' dialect, which may run on worker threads; a `call` stage for each call of a typed
#' function in each expression, in the language of the function's body, with an
#' ordinal where an expression calls one function twice; `evaluate`, of the rows of
#' an expression that reads cells outside its aggregates and calls; and `assemble`,
#' of the result, in R. A stage's `inputs` are the stages whose results it reads: a
#' call whose arguments read an aggregate reads `aggregate`, `evaluate` reads the
#' aggregates and calls of the expressions of rows, and `assemble` reads the stages
#' that no other stage reads. A map over the items of a list or a vector has
#' `check`, `call`, and `assemble`, and R makes its calls. The engine of the call,
#' `"rust"`, `"r"`, or `"rust+r"`, is that of its evaluation and its calls, as the
#' messages of `r8map()` state it. Where a table function's checks call another,
#' the first that R8 enters is the call explained.
#'
#' @section What a plan does not state:
#' What the body of a typed function in R does inside, and whether a body in Rust
#' calls R. `r8rust_signature()` writes the signature of a body in Rust for a
#' function whose body is in R.
#'
#' @param call A call of `r8map()`, `r8summarize()`, or `r8set()`, unevaluated.
#' @param x An `r8_plan`.
#' @param form Character \{"text", "mermaid"\}: The plan's headline and stages, or
#'   its Mermaid flowchart.
#' @param row.names,optional Not used.
#' @param ... Not used.
#'
#' @return An `r8_plan`: a list of `engine`, the languages of the call's work,
#'   `"rust"`, `"r"`, or `"rust+r"`; `headline`, the call's summary in the words
#'   of `r8map()`'s start line; and `nodes`, the call's node, then one node for
#'   each stage, each with the members of rtemis's session records: `node_id`
#'   (`"s1"`, `"s2"`, ...), `parent_id`, `kind` (the function, then `"r8.check"`,
#'   `"r8.compile"`, ...), `label`, `status` (`"pending"`), `t_start`, `t_end`,
#'   and `meta`. The call's `meta` holds the call, R8's version, the record's
#'   format, the plan hash, the threads, the platform, and each typed function's
#'   name and body's language with its body's identity: the package and its
#'   version, or, for a function of no package, `body_hash`, the SHA-256 of its
#'   body as R deparses it. The plan hash covers the table type's declaration, the
#'   expressions, the keys, and the typed functions' declarations, and not their
#'   bodies, so that it is the same for a body in R and in Rust. An equal plan
#'   hash, equal body identities, and equal inputs give an equal result. A stage's
#'   `meta` holds its engine, the stages that it reads (`inputs`), whether it may
#'   run on worker threads, and its counts; in the plan of a run, `busy`, the
#'   seconds that it worked, and, for a stage that failed, `message` and, where
#'   it has them, `group` and `row`, the group and its first row, from 1, or,
#'   for a map over items, `row`, the item.
#'   `as.data.frame()` gives one row for each stage:
#'   `node_id`, `stage`, `label`, `engine`, `function` and `language`, of the
#'   typed function that a `call` stage calls, `calls`, `threads`, `inputs`,
#'   `status`, and, for the plan of a run that [r8bench()] collects, `t_start`,
#'   `t_end`, and `busy`, seconds, which are `NA` before a run. `format(plan)`
#'   gives the lines that `print()` writes, and `format(plan, "mermaid")` the
#'   plan's Mermaid flowchart, as one string, in which each stage's class is its
#'   engine, `rust` or `r`.
#'
#' @export
#'
#' @examples
#' op <- options(r8.namespace = "demo")
#' m <- Raven(
#'   combo = c(1L, 1L, 2L, 2L),
#'   auc = c(0.81, 0.79, 0.85, 0.88),
#'   brier = c(0.16, 0.18, 0.14, 0.12)
#' )
#' spread <- new_function(
#'   "spread",
#'   function(x = arg_vec(type_double())) max(x) - min(x),
#'   return_type = type_double()
#' )
#' plan <- r8explain(r8map(m, spread, by = "combo"))
#' plan
#' plan$engine
#' as.data.frame(plan)
#' cat(format(plan, "mermaid"))
#' options(op)
r8explain <- function(call) {
  e <- substitute(call)
  env <- parent.frame()
  if (!is.call(e)) {
    r8_abort(
      "r8explain() takes a call of a table function, unevaluated: ",
      "r8explain(r8map(x, f, by = \"g\")).",
      class = "r8_argument_error"
    )
  }
  fn <- eval(e[[1L]], env)
  known <- list(r8map = r8map, r8summarize = r8summarize, r8set = r8set)
  which <- names(known)[vapply(known, identical, NA, fn)]
  if (length(which) == 0L) {
    r8_abort(
      "r8explain() explains a call of r8map(), r8summarize(), or r8set(); `",
      paste(deparse(e[[1L]]), collapse = " "),
      "` is another function.",
      class = "r8_argument_error"
    )
  }
  # The arguments evaluate before the call, so that a table function in an
  # argument runs and only the outer call is explained. The call reads them as
  # variables of a frame of its own, so that its call holds names and not the
  # values of whole tables.
  args <- as.list(e)[-1L]
  frame <- new.env(parent = env)
  held <- paste0(".r8_arg", seq_along(args))
  for (i in seq_along(args)) {
    assign(held[[i]], eval(args[[i]], env), envir = frame)
  }
  assign(".r8_fn", fn, envir = frame)
  run <- as.call(c(
    list(as.name(".r8_fn")),
    stats::setNames(lapply(held, as.name), names(args))
  ))
  depth <- .r8_watch[["depth"]]
  .r8_watch[["explaining"]] <- TRUE
  .r8_watch[["depth"]] <- 0L
  on.exit(
    {
      .r8_watch[["explaining"]] <- NULL
      .r8_watch[["depth"]] <- depth
      .r8_watch[["label"]] <- NULL
    },
    add = TRUE
  )
  ended <- tryCatch(
    {
      eval(run, frame)
      NULL
    },
    r8_explained = identity
  )
  if (is.null(ended)) {
    r8_abort(
      "r8explain(): the call of ",
      which,
      "() ended before its plan.",
      class = "r8_value_error"
    )
  }
  plan_of(ended[["plan"]][["nodes"]], e, ended[["functions"]])
} # /R8::r8explain


# %% plan_of ----
#' The plan of a call, with the facts of R in the call's node
#'
#' @param nodes List: The nodes that the binding gives.
#' @param call Call: The call explained.
#' @param functions Optional list: The typed functions that the call calls, by
#'   qualified name.
#'
#' @return An `r8_plan`.
#'
#' @keywords internal
#' @noRd
plan_of <- function(nodes, call, functions = NULL) {
  meta <- nodes[[1L]][["meta"]]
  if (!is.null(call)) {
    meta[["call"]] <- paste(deparse(call, width.cutoff = 500L), collapse = " ")
  }
  meta[["r8_version"]] <- as.character(getNamespaceVersion("R8"))
  meta[["platform"]] <- R.version[["platform"]]
  # A body's identity: the package in whose namespace its closure is defined,
  # with the package's version, or, for a function of no package, such as one
  # of the global environment, the SHA-256 of the closure's body as R deparses
  # it.
  meta[["functions"]] <- lapply(meta[["functions"]], function(f) {
    f[c("package", "version", "body_hash")] <- NULL
    closure <- functions[[f[["name"]]]]
    if (!is.function(closure)) {
      return(f)
    }
    home <- topenv(environment(closure))
    if (isNamespace(home)) {
      f[["package"]] <- unname(getNamespaceName(home))
      f[["version"]] <- as.character(getNamespaceVersion(home))
    } else {
      f[["body_hash"]] <- r8_text_hash(paste(
        deparse(body(closure), width.cutoff = 500L),
        collapse = "\n"
      ))
    }
    f
  })
  nodes[[1L]][["meta"]] <- meta
  structure(
    list(
      engine = meta[["engine"]],
      headline = r8_plan_text(nodes, "headline"),
      nodes = nodes
    ),
    class = "r8_plan"
  )
} # /R8::plan_of


# %% as.data.frame.r8_plan ----
#' @rdname r8explain
#' @export
as.data.frame.r8_plan <- function(x, row.names = NULL, optional = FALSE, ...) {
  stages <- x[["nodes"]][-1L]
  meta <- function(name, empty) {
    vapply(stages, function(s) s[["meta"]][[name]] %||% empty, empty)
  }
  out <- list(
    node_id = vapply(stages, `[[`, "", "node_id"),
    stage = sub("^r8[.]", "", vapply(stages, `[[`, "", "kind")),
    label = vapply(stages, `[[`, "", "label"),
    engine = meta("engine", NA_character_),
    `function` = meta("function", NA_character_),
    language = meta("language", NA_character_),
    calls = as.integer(vapply(
      stages,
      function(s) as.numeric(s[["meta"]][["calls"]] %||% NA_real_),
      0
    )),
    threads = meta("threads", NA),
    inputs = vapply(
      stages,
      function(s) paste(unlist(s[["meta"]][["inputs"]]), collapse = ", "),
      ""
    ),
    status = vapply(stages, `[[`, "", "status"),
    t_start = vapply(stages, function(s) s[["t_start"]] %||% NA_real_, 0),
    t_end = vapply(stages, function(s) s[["t_end"]] %||% NA_real_, 0),
    busy = vapply(
      stages,
      function(s) as.numeric(s[["meta"]][["busy"]] %||% NA_real_),
      0
    )
  )
  as.data.frame(out, stringsAsFactors = FALSE, optional = TRUE)
} # /R8::as.data.frame.r8_plan


# %% format.r8_plan ----
#' @rdname r8explain
#' @export
format.r8_plan <- function(x, form = c("text", "mermaid"), ...) {
  form <- match.arg(form)
  if (form == "mermaid") {
    return(r8_plan_text(x[["nodes"]], "mermaid"))
  }
  stages <- as.data.frame(x)
  texts <- r8_plan_text(x[["nodes"]], "stages")
  lines <- paste0(
    "  ",
    format(stages[["node_id"]]),
    "  ",
    format(ifelse(stages[["engine"]] == "rust", "Rust", "R")),
    "  ",
    texts
  )
  in_r <- unique(stages[["function"]][
    stages[["stage"]] == "call" & stages[["language"]] == "r"
  ])
  c(
    paste0("<r8 plan> ", x[["headline"]]),
    lines,
    if (length(in_r) > 0L) {
      paste0(
        "  A body in R runs on R's thread; ",
        paste0(
          "r8rust_signature(",
          sub("^.*::", "", in_r),
          ")",
          collapse = ", "
        ),
        " writes the signature of its body in Rust."
      )
    },
    "  The plan does not state what a body in R does inside, or whether a body in Rust calls R."
  )
} # /R8::format.r8_plan


# %% print.r8_plan ----
#' @rdname r8explain
#' @export
print.r8_plan <- function(x, ...) print_lines(x)
