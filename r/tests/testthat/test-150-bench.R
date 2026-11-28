# test-150-bench.R
# ::R8::
# 2026- EDG rtemis.org

# The resources that an expression takes, as r8bench() measures them, and the
# plans that the runs of its table functions leave.
# spec: R8/raven#benchmarks

declared <- function(expr) {
  op <- options(r8.namespace = "benches")
  on.exit(options(op))
  expr
}

set.seed(2026L)
m <- Raven(
  combo = rep(1:20, each = 10L),
  auc = runif(200L),
  brier = runif(200L)
)
spread <- declared(new_function(
  "spread",
  function(x = arg_vec(type_double())) max(x) - min(x),
  return_type = type_double()
))

test_that("a benchmark records the resources of its expression", {
  b <- r8bench(r8map(m, spread, by = "combo"))
  expect_s3_class(b, "r8_bench")
  expect_identical(
    names(b),
    c(
      "wall",
      "cpu_user",
      "cpu_system",
      "utilization",
      "n_threads",
      "cores_available",
      "rss_start",
      "rss_peak",
      "process_peak_before",
      "process_peak_after",
      "r_heap_peak",
      "rust_heap_peak",
      "where",
      "size",
      "plans",
      "platform",
      "r8_version"
    )
  )
  expect_gte(b$wall, 0)
  expect_gte(b$cpu_user + b$cpu_system, 0)
  expect_gte(b$cores_available, 1L)
  if (!is.na(b$rss_peak)) {
    expect_gt(b$rss_peak, 0)
  }
  expect_null(b$r_heap_peak)
  expect_identical(b$where$engine, "rust+r")
  expect_identical(b$where$calls_r, 40L)
  expect_identical(b$size$calls, 40)
  expect_match(
    format(b),
    "^r8bench\\(\\): [0-9.]+ m?s, CPU [0-9.]+ m?s \\([0-9.]+ of [0-9]+ threads?\\).*, in Rust and R$"
  )
  expect_identical(capture.output(print(b)), format(b))
  expect_null(R8:::.r8_watch[["collecting"]])
})

test_that("a run's plan is the plan that r8explain() gives, filled", {
  b <- r8bench(r8map(m, spread, by = "combo"))
  expect_length(b$plans, 1L)
  run <- b$plans[[1L]]
  dry <- r8explain(r8map(m, spread, by = "combo"))
  same <- c(
    "node_id",
    "stage",
    "label",
    "engine",
    "function",
    "language",
    "calls",
    "inputs"
  )
  expect_identical(as.data.frame(run)[same], as.data.frame(dry)[same])
  expect_identical(
    run$nodes[[1L]]$meta$plan_hash,
    dry$nodes[[1L]]$meta$plan_hash
  )
  expect_identical(run$nodes[[1L]]$status, "completed")
  expect_identical(unique(as.data.frame(run)$status), "completed")
  expect_identical(
    run$nodes[[1L]]$meta$call,
    "r8map(m, spread, by = \"combo\")"
  )
  stages <- as.data.frame(run)
  expect_true(all(stages$t_start <= stages$t_end))
  expect_true(all(stages$busy >= 0))
  expect_identical(run$nodes[[1L]]$t_start, 0)
  expect_gte(run$nodes[[1L]]$t_end, max(stages$t_end))
})

test_that("each table function of the expression leaves its plan", {
  b <- r8bench({
    y <- r8set(m, list(share = calc(auc / sum(auc))), by = "combo")
    r8summarize(y, list(s = calc(sum(share))), by = "combo")
  })
  expect_identical(
    vapply(b$plans, function(p) p$nodes[[1L]]$kind, ""),
    c("r8set", "r8summarize")
  )
  stages <- as.data.frame(b$plans[[1L]])
  expect_identical(
    stages$stage,
    c("check", "compile", "group", "aggregate", "evaluate", "assemble")
  )
  expect_identical(b$where$engine, "rust")
  # An expression without a table function of R8 has no plan.
  plain <- r8bench(sum(1:10))
  expect_length(plain$plans, 0L)
  expect_null(plain$where)
  expect_match(format(plain), "\\)(, memory peak [^,]+)?$")
})

test_that("a map over the items of a list leaves its plan", {
  plus <- declared(new_function(
    "plus",
    function(x = arg_double()) {
      if (x > 2) stop("over two") else x + 1
    },
    return_type = type_double()
  ))
  b <- r8bench(r8map(list(1, 2), plus))
  stages <- as.data.frame(b$plans[[1L]])
  expect_identical(stages$stage, c("check", "call", "assemble"))
  expect_identical(stages$status, rep("completed", 3L))
  expect_identical(stages$calls[[2L]], 2L)
  expect_true(all(stages$t_start <= stages$t_end))
  expect_identical(b$where$engine, "r")
  expect_identical(b$where$calls_r, 2L)
  # The plan of a map that fails holds the item and the message; r8bench()
  # raises the error.
  op <- options(
    r8.progress = list(
      begin = function(total, label, kind) "h",
      update = function(handle, current) NULL,
      end = function(handle, status) NULL,
      stages = function(handle, plan) got <<- plan
    )
  )
  on.exit(options(op), add = TRUE)
  got <- NULL
  slow_plus <- declared(new_function(
    "slow_plus",
    function(x = arg_double()) {
      Sys.sleep(0.1)
      if (x > 2) stop("over two") else x + 1
    },
    return_type = type_double()
  ))
  # The hook begins after 0.2 s, by the third call.
  expect_error(r8map(list(1, 2, 2, 3), slow_plus, verbosity = 0L), "over two")
  call <- got$nodes[[3L]]
  expect_identical(call$status, "failed")
  expect_identical(call$meta$row, 4L)
  expect_identical(call$meta$message, "over two")
  expect_identical(got$nodes[[1L]]$status, "failed")
  expect_identical(got$nodes[[4L]]$status, "pending")
})

test_that("r8bench() collects R's heap on request, and refuses a nested call", {
  b <- r8bench(numeric(1e5), gc = TRUE)
  expect_gt(b$r_heap_peak, 0)
  expect_error(
    r8bench(r8bench(1)),
    "one expression at a time",
    class = "r8_argument_error"
  )
  expect_error(r8bench(1, gc = NA), "`gc`", class = "r8_argument_error")
  expect_error(r8bench(stop("boom")), "boom")
  expect_null(R8:::.r8_watch[["collecting"]])
  expect_null(R8:::.r8_watch[["plans"]])
})

test_that("a progress hook with stages() takes the run's plan before end()", {
  # Each call takes 0.02 s, so that the map runs past the 0.2 s at which a
  # hook begins.
  m2 <- Raven(combo = rep(1:20, each = 10L), v = c(rep(0, 190), rep(1, 10)))
  slow <- declared(new_function(
    "slow",
    function(x = arg_vec(type_double())) {
      Sys.sleep(0.02)
      if (x[[1L]] > 0.5) stop("too high") else sum(x)
    },
    return_type = type_double()
  ))
  seen <- character()
  got <- NULL
  op <- options(
    r8.progress = list(
      begin = function(total, label, kind) "h",
      update = function(handle, current) NULL,
      end = function(handle, status) seen <<- c(seen, paste("end", status)),
      stages = function(handle, plan) {
        seen <<- c(seen, "stages")
        got <<- plan
      }
    )
  )
  on.exit(options(op), add = TRUE)
  r8map(
    Raven(combo = m2$combo, v = rep(0, 200L)),
    slow,
    by = "combo",
    verbosity = 0L
  )
  expect_identical(seen, c("stages", "end completed"))
  expect_s3_class(got, "r8_plan")
  expect_identical(got$nodes[[1L]]$status, "completed")
  expect_null(R8:::.r8_watch[["hooked"]])
  # A map that fails gives its plan before end(): the failed call holds the
  # group, its first row, the message, and the time it ran, and the stages
  # after it stay pending.
  seen <- character()
  expect_error(r8map(m2, slow, by = "combo", verbosity = 0L), "too high")
  expect_identical(seen, c("stages", "end failed"))
  stages <- as.data.frame(got)
  expect_identical(got$nodes[[1L]]$status, "failed")
  expect_identical(
    stages$status,
    c("completed", "completed", "completed", "failed", "pending")
  )
  call <- got$nodes[[match("call", stages$stage) + 1L]]
  expect_identical(call$meta$group, 20L)
  expect_identical(call$meta$row, 191L)
  expect_identical(call$meta$message, "too high")
  expect_gte(call$t_end, call$t_start)
  expect_null(R8:::.r8_watch[["pending"]])
  # `stages` is a function.
  options(
    r8.progress = list(
      begin = function(total, label, kind) "h",
      update = function(handle, current) NULL,
      end = function(handle, status) NULL,
      stages = "plan"
    )
  )
  expect_error(
    r8map(m2, slow, by = "combo"),
    "optionally `message` and `stages`",
    class = "r8_argument_error"
  )
})
