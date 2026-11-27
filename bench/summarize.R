# summarize.R
# ::r8 bench::
# 2026- EDG rtemis.org

# Grouped summaries, columns computed by calc(), and maps over columns in R8,
# against data.table on one thread and on its default threads, collapse,
# dplyr, base R, and DuckDB; R8 on one thread (R8) and on two (R8_2). Times are
# medians of `reps` runs, in ms, and of
# bench::mark() in microseconds for the size of rtemis's tuner. Run against an
# installed release build of R8: `R_LIBS=<library> Rscript bench/summarize.R`.
# spec: R8/raven#summaries

suppressPackageStartupMessages({
  library(R8)
  library(data.table)
  library(collapse)
  library(dplyr)
  library(duckdb)
})

ms <- function(f, reps = 5L) {
  f()
  stats::median(replicate(reps, system.time(f())[["elapsed"]])) * 1000
}

threads <- getDTthreads()
row_of <- function(case, rows, groups, times) {
  data.frame(case = case, rows = rows, groups = groups, as.list(round(times, 1)), check.names = FALSE)
}

# Summaries of n, mean(x), and sum(y) by a key.
by_key <- function(n, g, key = c("integer", "factor", "string", "sparse", "two")) {
  key <- match.arg(key)
  set.seed(1L)
  k <- sample.int(g, n, TRUE)
  d <- data.frame(x = rnorm(n), y = runif(n))
  by <- "k"
  if (key == "integer") d$k <- k
  if (key == "factor") d$k <- factor(k)
  if (key == "string") d$k <- sprintf("g%07d", k)
  if (key == "sparse") d$k <- k * 104729L
  if (key == "two") {
    d$k <- k %% 100L
    d$j <- k %/% 100L
    by <- c("k", "j")
  }
  v <- Raven(d)
  dt <- as.data.table(d)
  con <- dbConnect(duckdb())
  on.exit(dbDisconnect(con, shutdown = TRUE))
  duckdb_register(con, "d", d)
  sums <- list(n = calc(n()), mx = calc(mean(x)), sy = calc(sum(y)))
  reps <- if (n > 1e6) 3L else 5L
  keys <- paste(by, collapse = ", ")
  times <- c(
    R8 = ms(\() r8summarize(v, sums, by = by, n_threads = 1L), reps),
    R8_2 = ms(\() r8summarize(v, sums, by = by, n_threads = 2L), reps),
    dt1 = {
      setDTthreads(1L)
      ms(\() dt[, .(n = .N, mx = mean(x), sy = sum(y)), keyby = by], reps)
    },
    dt = {
      setDTthreads(threads)
      ms(\() dt[, .(n = .N, mx = mean(x), sy = sum(y)), keyby = by], reps)
    },
    collapse = ms(\() fsummarise(fgroup_by(d, by), n = fnobs(x), mx = fmean(x), sy = fsum(y)), reps),
    dplyr = if (g <= 1e4) {
      ms(\() summarise(d, n = n(), mx = mean(x), sy = sum(y), .by = all_of(by)), reps)
    } else NA,
    base = if (key == "integer" && g <= 1e5) {
      ms(\() list(tapply(d$x, d$k, mean), tapply(d$y, d$k, sum), tabulate(d$k)), reps)
    } else NA,
    duckdb = ms(\() dbGetQuery(con, sprintf(
      "select %s, count(*) n, avg(x) mx, sum(y) sy from d group by %s order by %s", keys, keys, keys
    )), reps)
  )
  row_of(paste0("n, mean, sum by ", key), n, g, times)
}

# The median by an integer key, and the summaries of a column with 5% of its
# cells missing, under the default and under na.rm = TRUE.
medians_and_missing <- function(n, g) {
  set.seed(2L)
  d <- data.frame(k = sample.int(g, n, TRUE), x = rnorm(n))
  d$x[sample.int(n, n %/% 20L)] <- NA
  v <- Raven(d)
  dt <- as.data.table(d)
  setDTthreads(1L)
  med <- c(
    R8 = ms(\() r8summarize(v, list(m = calc(median(x, na.rm = TRUE))), by = "k", n_threads = 1L), 3L),
    dt1 = ms(\() dt[, .(m = median(x, na.rm = TRUE)), keyby = k], 3L),
    collapse = ms(\() fmedian(d$x, d$k), 3L)
  )
  setDTthreads(threads)
  strict <- c(
    R8 = ms(\() r8summarize(v, list(m = calc(mean(x)), s = calc(sum(x))), by = "k", n_threads = 1L), 3L),
    R8_2 = ms(\() r8summarize(v, list(m = calc(mean(x)), s = calc(sum(x))), by = "k", n_threads = 2L), 3L),
    R8_na_rm = ms(\() r8summarize(v, list(m = calc(mean(x, na.rm = TRUE)), s = calc(sum(x, na.rm = TRUE))), by = "k", n_threads = 1L), 3L),
    dt1 = {
      setDTthreads(1L)
      ms(\() dt[, .(m = mean(x, na.rm = TRUE), s = sum(x, na.rm = TRUE)), keyby = k], 3L)
    }
  )
  setDTthreads(threads)
  list(
    row_of("median, 5% missing, na.rm", n, g, med),
    row_of("mean and sum, 5% missing, strict and na.rm", n, g, strict)
  )
}

# The size of each row's group in each row, and a column of arithmetic.
in_rows <- function(n, g) {
  set.seed(3L)
  d <- data.frame(k = sample.int(g, n, TRUE), w = runif(n, 40, 120), h = runif(n, 1.4, 2))
  v <- Raven(d)
  dt <- as.data.table(d)
  setDTthreads(1L)
  count <- c(
    R8 = ms(\() r8set(v, list(nk = calc(n())), by = "k", n_threads = 1L), 3L),
    dt1 = ms(\() copy(dt)[, nk := .N, by = k], 3L),
    dplyr = ms(\() mutate(d, nk = n(), .by = k), 3L),
    base = ms(\() ave(seq_len(n), d$k, FUN = length), 3L)
  )
  bmi <- c(
    R8 = ms(\() r8set(v, list(bmi = calc(w / h^2)), n_threads = 1L), 3L),
    base = ms(\() d$w / d$h^2, 3L),
    dt1 = ms(\() copy(dt)[, bmi := w / h^2], 3L)
  )
  setDTthreads(threads)
  list(
    row_of("n() of each row's group", n, g, count),
    row_of("calc(w / h^2), a column", n, NA, bmi)
  )
}

# rtemis's tuner: 20 combinations of 10 resamples, 3 metrics.
tuner <- function() {
  set.seed(4L)
  d <- data.frame(
    combo = rep(1:20, each = 10L),
    auc = runif(200L),
    balanced_accuracy = runif(200L),
    brier = runif(200L)
  )
  v <- Raven(d)
  dt <- as.data.table(d)
  setDTthreads(1L)
  op <- options(r8.namespace = "bench")
  on.exit(options(op))
  spread <- new_function(
    "spread",
    function(x = arg_vec(type_double())) max(x) - min(x),
    return_type = type_double()
  )
  b <- bench::mark(
    R8_map_calc = r8map(v, calc(mean(col())), by = "combo", n_threads = 1L),
    R8_map_median = r8map(v, calc(median(col())), by = "combo"),
    R8_map_typed = r8map(v, spread, by = "combo"),
    dt1_mean = dt[, lapply(.SD, mean), keyby = combo],
    dt1_median = dt[, lapply(.SD, median), keyby = combo],
    collapse = fmean(d[-1L], d$combo),
    aggregate = aggregate(d[-1L], d["combo"], FUN = mean),
    check = FALSE,
    min_iterations = 50L
  )
  setDTthreads(threads)
  data.frame(expression = as.character(b$expression), median_us = round(as.numeric(b$median) * 1e6, 1))
}

main <- function() {
  cat("R8", as.character(packageVersion("R8")), "; data.table", as.character(packageVersion("data.table")),
      "with", threads, "threads; collapse", as.character(packageVersion("collapse")),
      "; dplyr", as.character(packageVersion("dplyr")), "; duckdb", as.character(packageVersion("duckdb")), "\n\n")
  grid <- rbind(
    by_key(1e6, 10L), by_key(1e6, 1e3L), by_key(1e6, 1e5L),
    by_key(1e7, 10L), by_key(1e7, 1e3L), by_key(1e7, 1e5L), by_key(1e7, 1e6L)
  )
  print(grid, row.names = FALSE)
  cat("\n")
  keys <- do.call(rbind, lapply(c("factor", "string", "sparse", "two"), function(k) by_key(1e7, 1e3L, k)))
  keys <- rbind(keys, by_key(1e7, 1e5L, "string"))
  print(keys, row.names = FALSE)
  cat("\n")
  for (t in c(medians_and_missing(1e7, 1e3L), in_rows(1e7, 1e3L))) {
    print(t, row.names = FALSE)
    cat("\n")
  }
  cat("\n")
  print(tuner(), row.names = FALSE)
}

if (sys.nframe() == 0L) main()
