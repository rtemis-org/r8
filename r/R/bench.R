# bench.R
# ::R8::
# 2026- EDG rtemis.org

# r8bench(): the resources that an expression takes, and the plans of the
# table functions of R8 that it runs.
# spec: R8/raven#benchmarks

# %% r8bench ----
#' Measure the resources that an expression takes
#'
#' `r8bench(expr)` evaluates `expr` once, in the caller's frame, and returns a
#' record of the resources that it took: the wall time, the CPU time of R's
#' process, the most memory that the process held, the cores that it may use, and,
#' for each call of `r8map()`, `r8summarize()`, or `r8set()` that `expr` runs, the
#' plan of the call filled by its run: each stage's status, its first start and
#' last end in seconds from the call's start, and the seconds it was busy, and a
#' failed stage's group, first row, and message. The record prints as one line.
#'
#' @section Measures:
#' The CPU time, in user and in system mode, is that of all the threads of R's
#' process, from `getrusage()` or `GetProcessTimes()`; `utilization` is its ratio
#' to the wall time, the threads busy on average. The wall time and the times of
#' the stages are read from a monotonic clock. `rss_peak` is the most memory that
#' the process held during `expr`, read about every 10 ms by a thread of Rust that
#' calls no function of R, which starts before the times measured and stops after
#' them; `process_peak_before` and `process_peak_after` are the operating system's
#' high-water mark of the process, which a peak during `expr` raises and which
#' macOS does not reset. `cores_available` is the cores that the operating system
#' gives the process, and at most `SLURM_CPUS_PER_TASK` in a Slurm job. With
#' `gc = TRUE`, R's heap is collected before `expr`, outside the time measured,
#' and `r_heap_peak` is the most memory that R's heap held during it. The plans
#' are those that [r8explain()] gives, filled by the run. A time under 1 s prints
#' in milliseconds.
#'
#' @param expr An expression, evaluated once in the caller's frame.
#' @param gc Logical: Collect R's heap before `expr` and measure the most that it
#'   held during it.
#' @param x An `r8_bench`.
#' @param ... Not used.
#'
#' @return An `r8_bench`: a list of `wall`, `cpu_user`, and `cpu_system`, seconds;
#'   `utilization`; `n_threads`, the most threads that the plans' work may use, or
#'   the default of `n_threads` without a plan; `cores_available`; `rss_start` and
#'   `rss_peak`, bytes; `process_peak_before` and `process_peak_after`, bytes;
#'   `r_heap_peak`, bytes, or `NULL` without `gc = TRUE`; `rust_heap_peak`, `NULL`;
#'   `where`, the engine of the plans' work, `"rust"`, `"r"`, or `"rust+r"`, and the
#'   calls of typed functions with their bodies in Rust and in R, or `NULL` without a
#'   plan; `size`, the most rows and groups of the plans and their calls; `plans`, a
#'   list of `r8_plan`; `platform`; and `r8_version`. A figure that the operating
#'   system does not give is `NA`. An error of `expr` is raised after the
#'   measurement ends.
#'
#' @export
#'
#' @examples
#' op <- options(r8.namespace = "demo")
#' m <- Raven(
#'   combo = rep(1:20, each = 10),
#'   auc = seq(0.7, 0.9, length.out = 200)
#' )
#' b <- r8bench(r8map(m, calc(mean(col())), by = "combo"))
#' b
#' b$wall
#' as.data.frame(b$plans[[1L]])
#' options(op)
r8bench <- function(expr, gc = FALSE) {
  if (!is.logical(gc) || length(gc) != 1L || is.na(gc)) {
    r8_abort("`gc` is TRUE or FALSE.", class = "r8_argument_error")
  }
  if (isTRUE(.r8_watch[["collecting"]])) {
    r8_abort(
      "r8bench() measures one expression at a time; an r8bench() inside ",
      "another is refused.",
      class = "r8_argument_error"
    )
  }
  if (gc) {
    # Collected before the time measured; the most memory used is counted
    # from here.
    base::gc(reset = TRUE)
  }
  .r8_watch[["collecting"]] <- TRUE
  .r8_watch[["plans"]] <- list()
  sampler <- NULL
  on.exit(
    {
      .r8_watch[["collecting"]] <- NULL
      .r8_watch[["plans"]] <- NULL
      if (!is.null(sampler)) {
        r8_sampler_stop(sampler)
      }
    },
    add = TRUE
  )
  # The sampler's start and stop lie outside the times measured.
  sampler <- r8_sampler_start(10L)
  before <- r8_usage()
  started <- r8_clock()
  failed <- tryCatch(
    {
      force(expr)
      NULL
    },
    error = identity
  )
  wall <- r8_clock() - started
  after <- r8_usage()
  rss_peak <- r8_sampler_stop(sampler)
  sampler <- NULL
  r_heap_peak <- if (gc) {
    g <- base::gc()
    at <- match("max used", colnames(g)) + 1L
    sum(g[, at]) * 2^20
  }
  plans <- .r8_watch[["plans"]]
  if (!is.null(failed)) {
    stop(failed)
  }
  cpu_user <- after[["user"]] - before[["user"]]
  cpu_system <- after[["system"]] - before[["system"]]
  structure(
    list(
      wall = wall,
      cpu_user = cpu_user,
      cpu_system = cpu_system,
      utilization = if (wall > 0) (cpu_user + cpu_system) / wall else NA_real_,
      n_threads = bench_threads(plans),
      cores_available = r8_cores_available(),
      rss_start = before[["rss"]],
      rss_peak = rss_peak,
      process_peak_before = before[["peak"]],
      process_peak_after = after[["peak"]],
      r_heap_peak = r_heap_peak,
      rust_heap_peak = NULL,
      where = bench_where(plans),
      size = bench_size(plans),
      plans = plans,
      platform = R.version[["platform"]],
      r8_version = getNamespaceVersion("R8")[["version"]]
    ),
    class = "r8_bench"
  )
} # /R8::r8bench


# %% bench_threads ----
#' The most threads that the work of a benchmark's plans may use
#'
#' @param plans List of `r8_plan`.
#'
#' @return Integer: Of the plans' calls, or the default of `n_threads`.
#'
#' @keywords internal
#' @noRd
bench_threads <- function(plans) {
  if (length(plans) == 0L) {
    return(threads_of(NULL))
  }
  as.integer(max(vapply(
    plans,
    function(p) as.numeric(p[["nodes"]][[1L]][["meta"]][["n_threads"]] %||% 1),
    0
  )))
} # /R8::bench_threads


# %% bench_where ----
#' Where the work of a benchmark's plans ran
#'
#' @param plans List of `r8_plan`.
#'
#' @return `NULL` without a plan, or a list of `engine` and of `calls_rust` and
#'   `calls_r`, the calls of typed functions by the language of their bodies.
#'
#' @keywords internal
#' @noRd
bench_where <- function(plans) {
  if (length(plans) == 0L) {
    return(NULL)
  }
  engines <- vapply(plans, `[[`, "", "engine")
  langs <- unique(unlist(strsplit(engines, "+", fixed = TRUE)))
  stages <- do.call(rbind, lapply(plans, as.data.frame))
  calls <- stages[stages[["stage"]] == "call", , drop = FALSE]
  count <- function(lang) {
    as.integer(sum(calls[["calls"]][calls[["language"]] == lang], na.rm = TRUE))
  }
  list(
    engine = if (length(langs) == 2L) "rust+r" else langs,
    calls_rust = count("rust"),
    calls_r = count("r")
  )
} # /R8::bench_where


# %% bench_size ----
#' The size of a benchmark's plans
#'
#' @param plans List of `r8_plan`.
#'
#' @return `NULL` without a plan, or a list of `rows` and `groups`, the most of
#'   the plans' calls, and `calls`, their sum.
#'
#' @keywords internal
#' @noRd
bench_size <- function(plans) {
  if (length(plans) == 0L) {
    return(NULL)
  }
  field <- function(name) {
    vapply(
      plans,
      function(p) {
        as.numeric(p[["nodes"]][[1L]][["meta"]][[name]] %||% NA_real_)
      },
      0
    )
  }
  most <- function(v) if (all(is.na(v))) NA_real_ else max(v, na.rm = TRUE)
  list(
    rows = most(field("rows")),
    groups = most(field("groups")),
    calls = sum(field("calls"), na.rm = TRUE)
  )
} # /R8::bench_size


# %% format.r8_bench ----
#' @rdname r8bench
#' @export
format.r8_bench <- function(x, ...) {
  r8_bench_line(
    x[["wall"]],
    x[["cpu_user"]] + x[["cpu_system"]],
    as.integer(x[["n_threads"]]),
    as.numeric(x[["rss_peak"]]),
    x[["where"]][["engine"]] %||% ""
  )
} # /R8::format.r8_bench


# %% print.r8_bench ----
#' @rdname r8bench
#' @export
print.r8_bench <- function(x, ...) print_lines(x)
