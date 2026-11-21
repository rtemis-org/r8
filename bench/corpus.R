# corpus.R
# ::r8 bench::
# 2026- EDG rtemis.org

# The acceptance corpus, which gates the timing: each system accepts the
# valid value of each type, converts a whole double to an integer, and
# rejects one invalid value per constraint and per rule, and an invalid
# write. `run_corpus(system, ops)` returns one row per case, with the
# expected and the observed acceptance.
# spec: R8/class-system#benchmark

corpus_cases <- function() {
  a <- gbm_args()
  with_args <- function(...) utils::modifyList(a, list(...))
  # An omitted option: modifyList() drops a NULL member.
  without <- function(name, ...) {
    out <- with_args(...)
    out[[name]] <- NULL
    out
  }
  list(
    list(case = "GBM valid", accept = TRUE, make = function() do.call(GBM, a)),
    list(case = "GBM converts a whole double", accept = TRUE, make = function() {
      x <- do.call(GBM, with_args(n_trees = 500, weights = rep(1L, 10L)))
      stopifnot(is.integer(get_field(x, "n_trees")), is.double(get_field(x, "weights")))
      x
    }),
    list(case = "GBM omits the options", accept = TRUE, make = function() {
      do.call(GBM, without("num_class", objective = "binary"))
    }),
    list(case = "learning_rate at 0", accept = FALSE, make = function() do.call(GBM, with_args(learning_rate = 0))),
    list(case = "learning_rate above 1", accept = FALSE, make = function() do.call(GBM, with_args(learning_rate = 1.5))),
    list(case = "subsample NA", accept = FALSE, make = function() do.call(GBM, with_args(subsample = NA_real_))),
    list(case = "colsample not finite", accept = FALSE, make = function() do.call(GBM, with_args(colsample = Inf))),
    list(case = "learning_rate a string", accept = FALSE, make = function() do.call(GBM, with_args(learning_rate = "0.1"))),
    list(case = "n_trees at 0", accept = FALSE, make = function() do.call(GBM, with_args(n_trees = 0L))),
    list(case = "n_trees not whole", accept = FALSE, make = function() do.call(GBM, with_args(n_trees = 5.5))),
    list(case = "n_trees two values", accept = FALSE, make = function() do.call(GBM, with_args(n_trees = c(1L, 2L)))),
    list(case = "max_depth above 32", accept = FALSE, make = function() do.call(GBM, with_args(max_depth = 33L))),
    list(case = "objective outside its pattern", accept = FALSE, make = function() do.call(GBM, with_args(objective = "ranking"))),
    list(case = "num_class below 2", accept = FALSE, make = function() do.call(GBM, with_args(num_class = 1L))),
    list(case = "early_stopping_rounds at 0", accept = FALSE, make = function() do.call(GBM, with_args(early_stopping_rounds = 0L))),
    list(case = "seed not whole", accept = FALSE, make = function() do.call(GBM, with_args(seed = 1.5))),
    list(case = "verbose NA", accept = FALSE, make = function() do.call(GBM, with_args(verbose = NA))),
    list(case = "weights below 0", accept = FALSE, make = function() do.call(GBM, with_args(weights = c(-1, rep(1, 9L))))),
    list(case = "weights NA", accept = FALSE, make = function() do.call(GBM, with_args(weights = c(NA, rep(1, 9L))))),
    list(case = "feature_idx at 0", accept = FALSE, make = function() do.call(GBM, with_args(feature_idx = 0:9))),
    list(case = "feature_idx not whole", accept = FALSE, make = function() do.call(GBM, with_args(feature_idx = c(1.5, 2:10)))),
    list(case = "weights with names", accept = FALSE, make = function() do.call(GBM, with_args(weights = stats::setNames(rep(1, 10L), letters[1:10])))),
    list(case = "rule: early stopping at n_trees", accept = FALSE, make = function() do.call(GBM, with_args(early_stopping_rounds = 500L))),
    list(case = "rule: num_class without multiclass", accept = FALSE, make = function() do.call(GBM, with_args(objective = "binary"))),
    list(case = "rule: multiclass without num_class", accept = FALSE, make = function() do.call(GBM, without("num_class"))),
    list(case = "write n_trees at 0", accept = FALSE, make = function() set_field(do.call(GBM, a), "n_trees", 0L)),
    list(case = "write breaking a rule", accept = FALSE, make = function() set_field(do.call(GBM, a), "n_trees", 20L)),
    list(case = "Wide20 valid", accept = TRUE, make = function() Wide20()),
    list(case = "Wide20 f1 above 1", accept = FALSE, make = function() Wide20(f1 = 1.5)),
    list(case = "GBMRun valid", accept = TRUE, make = function() gbm_run(3L, 3L)),
    list(case = "GBMRun outcome with fewer levels", accept = TRUE, make = function() gbm_run(3L, 3L, outcome = factor("control"))),
    list(case = "GBMRun outcome outside its levels", accept = FALSE, make = function() gbm_run(3L, 3L, outcome = factor("other"))),
    list(case = "GBMRun created not a Date", accept = FALSE, make = function() gbm_run(3L, 3L, created = "2026-11-20")),
    list(case = "GBMRun importance columns", accept = FALSE, make = function() gbm_run(3L, 3L, importance = matrix(0, 2, 3))),
    list(case = "Resampler train_p at 1", accept = FALSE, make = function() resampler(train_p = 1)),
    list(case = "rule: one fold per resample", accept = FALSE, make = function() gbm_run(3L, 2L))
  )
}

resampler <- function(...) {
  do.call(Resampler, utils::modifyList(list(type = "kfold", n = 3L, train_p = 0.8, stratify = TRUE), list(...)))
}

# A GBMRun with `n` resamples and `k` folds.
gbm_run <- function(n, k, ...) {
  args <- utils::modifyList(
    list(
      gbm = do.call(GBM, gbm_args()),
      resampler = resampler(n = n),
      created = as.Date("2026-11-20"),
      outcome = factor(c("control", "case", "case"), levels = c("control", "case")),
      importance = matrix(c(0.5, 0.3, 0.2, 0.1, 0.1, 0.1), 3, 2),
      folds = lapply(seq_len(k), function(i) resampler())
    ),
    list(...)
  )
  do.call(GBMRun, args)
}

# A field of a value, and a value with a field replaced, in each system's
# idiom.
get_field <- function(x, name) {
  if (isS4(x) && !is(x, "envRefClass")) methods::slot(x, name) else if (inherits(x, "S7_object")) S7::prop(x, name) else x[[name]]
}

set_field <- function(x, name, value) {
  if (is(x, "envRefClass")) {
    x$set(name, value)
  } else if (inherits(x, "R6")) {
    x[[name]] <- value
  } else if (inherits(x, "S7_object")) {
    S7::prop(x, name) <- value
  } else if (isS4(x)) {
    methods::slot(x, name) <- value
    methods::validObject(x)
  } else {
    x[[name]] <- value
    if (inherits(x, "s3_GBM")) x <- `$<-`(x, name, value)
  }
  x
}

run_corpus <- function(system) {
  cases <- corpus_cases()
  if (identical(system, "R8 Rust")) {
    cases <- Filter(function(k) !grepl("GBMRun|Resampler|fold", k$case), cases)
  }
  do.call(rbind, lapply(cases, function(k) {
    observed <- tryCatch({ k$make(); TRUE }, error = function(e) FALSE)
    data.frame(system = system, case = k$case, expected = k$accept, observed = observed)
  }))
}
