# test-149-rtemis-grouped.R
# ::R8::
# 2026- EDG rtemis.org

# The grouped work of rtemis as R8 calls, against base R: the tuner's summary
# of each metric by its combination of hyperparameters, and the ROC curve's
# count of each curve's vertices and its down-sample of each curve.
# spec: R8/raven#summaries

test_that("the tuner's summary of each metric by combination is a map", {
  set.seed(2026L)
  metric_names <- c("auc", "balanced_accuracy", "brier")
  # 4 combinations of 5 resamples each; a resample that failed has no metrics.
  df <- data.frame(
    param_combo_id = rep(1:4, each = 5L),
    auc = runif(20L, 0.7, 0.9),
    balanced_accuracy = runif(20L, 0.6, 0.8),
    brier = runif(20L, 0.1, 0.2)
  )
  df[7L, metric_names] <- NA
  metrics <- Raven(df)
  # The tuner's configuration names the function of the summary.
  summaries <- list(mean = calc(mean(col())), median = calc(median(col())))
  for (fn in names(summaries)) {
    by_combo <- r8map(metrics, summaries[[fn]], by = "param_combo_id")
    expect_identical(by_combo$param_combo_id, 1:4)
    expect_identical(names(by_combo), c("param_combo_id", metric_names))
    for (m in metric_names) {
      expected <- as.vector(tapply(df[[m]], df$param_combo_id, match.fun(fn)))
      expect_equal(by_combo[[m]], expected, info = paste(fn, m))
    }
    # The combination with a failed resample has no summary.
    expect_true(is.na(by_combo$auc[[2L]]))
  }
  # With `na.rm`, the failed resample is left out.
  by_combo <- r8map(
    metrics,
    calc(mean(col(), na.rm = TRUE)),
    by = "param_combo_id"
  )
  expect_equal(
    by_combo$auc,
    as.vector(tapply(df$auc, df$param_combo_id, mean, na.rm = TRUE))
  )
})

test_that("the ROC curve's vertices are counted and each curve down-sampled", {
  set.seed(2026L)
  keys <- c("sample", "resample", "class")
  # One curve for each sample, resample, and class, of 3 to 40 vertices.
  grid <- expand.grid(
    sample = c("training", "test"),
    resample = 1:3,
    class = c("a", "b"),
    stringsAsFactors = FALSE
  )
  sizes <- sample(3:40, nrow(grid), replace = TRUE)
  df <- do.call(
    rbind,
    lapply(seq_len(nrow(grid)), function(i) {
      n <- sizes[[i]]
      data.frame(
        grid[rep(i, n), ],
        fpr = sort(runif(n)),
        tpr = sort(runif(n)),
        row.names = NULL
      )
    })
  )
  max_points <- 10L
  curves <- Raven(df)
  # Counted before the down-sample, so that a reader of the table can tell.
  curves <- r8set(curves, list(n_vertices = calc(n())), by = keys)
  expect_identical(
    curves$n_vertices,
    as.integer(ave(
      seq_len(nrow(df)),
      df$sample,
      df$resample,
      df$class,
      FUN = length
    ))
  )
  # Each curve keeps its first and last vertex and at most `max_points`.
  thin <- function(g) {
    g[unique(round(seq(1, nrow(g), length.out = min(nrow(g), max_points)))), ]
  }
  groups <- list(curves$sample, curves$resample, curves$class)
  kept <- do.call(rbind, lapply(split(curves, groups, drop = TRUE), thin))
  expect_s3_class(kept, "r8_table")
  df$n_vertices <- as.integer(ave(
    seq_len(nrow(df)),
    df$sample,
    df$resample,
    df$class,
    FUN = length
  ))
  expected <- do.call(
    rbind,
    lapply(split(df, as.list(df[keys]), drop = TRUE), thin)
  )
  rownames(expected) <- NULL
  expect_identical(as.data.frame(kept), expected)
  expect_true(all(table(kept$sample, kept$resample, kept$class) <= max_points))
})
