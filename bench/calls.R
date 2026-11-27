# calls.R
# ::r8 bench::
# 2026- EDG rtemis.org

# Calls of typed functions in the expressions of calc(): `spread()` of the
# test package r8pkgR, whose body is in Rust, against the same function with
# its body in R and against the dialect's `max(col()) - min(col())`, each as
# `r8map(v, calc(...), by = "g")` over 10 columns of 10^6 rows at 100, 10^4,
# and 10^5 groups, so that 10^3, 10^5, and 10^6 calls of the function run;
# `r8map(v, spread, by = "g")`, the plain form of the map ("plain"), and the
# same with a progress hook that does nothing ("hooked").
# Times are medians of `reps` runs, in ms, one run for the body in R, and
# medians of bench::mark() in microseconds at the size of rtemis's tuner. Run
# against installed release builds of R8 and r8pkgR:
# `R_LIBS=<library of R8>:<library of r8pkgR> Rscript bench/calls.R`.
# spec: R8/raven#summaries

suppressPackageStartupMessages({
  library(R8)
  library(r8pkgR)
})

ms <- function(f, reps = 3L) {
  f()
  stats::median(replicate(reps, system.time(f())[["elapsed"]])) * 1000
}

op <- options(r8.namespace = "bench")
spread_r <- new_function(
  "spread_r",
  function(x = arg_vec(type_double())) max(x) - min(x),
  return_type = type_double()
)
options(op)

# 10 columns of n rows in g groups.
columns <- function(n, g) {
  set.seed(1L)
  d <- as.data.frame(matrix(rnorm(n * 10L), n, 10L, dimnames = list(NULL, paste0("x", 1:10))))
  d$g <- sample.int(g, n, TRUE)
  Raven(d)
}

by_groups <- function(n, g) {
  v <- columns(n, g)
  cols <- paste0("x", 1:10)
  rust <- calc(spread(col()))
  in_r <- calc(spread_r(col()))
  dialect <- calc(max(col()) - min(col()))
  hook <- list(begin = function(total, label, kind) 1L, update = function(h, n) NULL, end = function(h, s) NULL)
  times <- c(
    rust = ms(\() r8map(v, rust, columns = cols, by = "g", n_threads = 1L)),
    plain = ms(\() r8map(v, spread, columns = cols, by = "g", n_threads = 1L)),
    hooked = {
      op <- options(r8.progress = hook)
      on.exit(options(op))
      ms(\() r8map(v, spread, columns = cols, by = "g", n_threads = 1L))
    },
    r = ms(\() r8map(v, in_r, columns = cols, by = "g", n_threads = 1L), 1L),
    dialect = ms(\() r8map(v, dialect, columns = cols, by = "g", n_threads = 1L))
  )
  data.frame(rows = n, groups = g, calls = 10L * g, as.list(round(times, 1)))
}

# rtemis's tuner: 20 combinations of 10 resamples, 3 metrics.
tuner <- function() {
  set.seed(4L)
  v <- Raven(
    combo = rep(1:20, each = 10L),
    auc = runif(200L),
    balanced_accuracy = runif(200L),
    brier = runif(200L)
  )
  b <- bench::mark(
    rust = r8map(v, calc(spread(col())), by = "combo"),
    r = r8map(v, calc(spread_r(col())), by = "combo"),
    dialect = r8map(v, calc(max(col()) - min(col())), by = "combo"),
    check = FALSE,
    min_iterations = 200L
  )
  data.frame(expression = as.character(b$expression), median_us = round(as.numeric(b$median) * 1e6, 1))
}

main <- function() {
  cat("R8", as.character(packageVersion("R8")), "\n\n")
  print(do.call(rbind, lapply(c(1e2L, 1e4L, 1e5L), \(g) by_groups(1e6L, g))), row.names = FALSE)
  cat("\n")
  print(tuner(), row.names = FALSE)
}

if (sys.nframe() == 0L) main()
