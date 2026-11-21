# corpus.R
# ::r8 bench::
# 2026- EDG rtemis.org

# The acceptance corpus, which gates the timing. Each system accepts the
# valid value of each type, converts a whole double to an integer at
# construction and at a write, rejects one invalid value per constraint and
# per rule and an invalid write, keeps a value unchanged after a failed
# write, validates a valid value, and evaluates every expression a row
# times. `run_corpus(system, ops)` returns one row per case, with the
# expected and the observed acceptance; `stored_fields()` gives the stored
# fields of the valid GBM as JSON, which run.R compares across systems.
# spec: R8/class-system#benchmark

# A field of a value, in each system's idiom.
get_field <- function(x, name) {
  if (inherits(x, "envRefClass") || inherits(x, "R6")) {
    x[[name]]
  } else if (inherits(x, "S7_object")) {
    S7::prop(x, name)
  } else if (isS4(x)) {
    methods::slot(x, name)
  } else {
    x[[name]]
  }
}

corpus_cases <- function(ops) {
  a <- gbm_args()
  with_args <- function(...) utils::modifyList(a, list(...))
  # An omitted option: modifyList() drops a NULL member.
  without <- function(name, ...) {
    out <- with_args(...)
    out[[name]] <- NULL
    out
  }
  gbm <- function(args) do.call(GBM, args)
  write <- function(x, name, value) ops$write_fr_fn(x, name, value)
  list(
    list(case = "GBM valid", accept = TRUE, make = function() gbm(a)),
    list(case = "GBM converts a whole double", accept = TRUE, make = function() {
      x <- gbm(with_args(n_trees = 500, weights = rep(1L, 10L)))
      stopifnot(is.integer(get_field(x, "n_trees")), is.double(get_field(x, "weights")))
      x
    }),
    list(case = "GBM omits the options", accept = TRUE, make = function() gbm(without("num_class", objective = "binary"))),
    list(case = "learning_rate at 0", accept = FALSE, make = function() gbm(with_args(learning_rate = 0))),
    list(case = "learning_rate above 1", accept = FALSE, make = function() gbm(with_args(learning_rate = 1.5))),
    list(case = "subsample NA", accept = FALSE, make = function() gbm(with_args(subsample = NA_real_))),
    list(case = "colsample not finite", accept = FALSE, make = function() gbm(with_args(colsample = Inf))),
    list(case = "learning_rate a string", accept = FALSE, make = function() gbm(with_args(learning_rate = "0.1"))),
    list(case = "n_trees at 0", accept = FALSE, make = function() gbm(with_args(n_trees = 0L))),
    list(case = "n_trees not whole", accept = FALSE, make = function() gbm(with_args(n_trees = 5.5))),
    list(case = "n_trees two values", accept = FALSE, make = function() gbm(with_args(n_trees = c(1L, 2L)))),
    list(case = "max_depth above 32", accept = FALSE, make = function() gbm(with_args(max_depth = 33L))),
    list(case = "objective outside its pattern", accept = FALSE, make = function() gbm(with_args(objective = "ranking"))),
    list(case = "num_class below 2", accept = FALSE, make = function() gbm(with_args(num_class = 1L))),
    list(case = "early_stopping_rounds at 0", accept = FALSE, make = function() gbm(with_args(early_stopping_rounds = 0L))),
    list(case = "seed not whole", accept = FALSE, make = function() gbm(with_args(seed = 1.5))),
    list(case = "verbose NA", accept = FALSE, make = function() gbm(with_args(verbose = NA))),
    list(case = "weights below 0", accept = FALSE, make = function() gbm(with_args(weights = c(-1, rep(1, 9L))))),
    list(case = "weights NA", accept = FALSE, make = function() gbm(with_args(weights = c(NA, rep(1, 9L))))),
    list(case = "feature_idx at 0", accept = FALSE, make = function() gbm(with_args(feature_idx = 0:9 + 0L))),
    list(case = "feature_idx not whole", accept = FALSE, make = function() gbm(with_args(feature_idx = c(1.5, 2:10)))),
    list(case = "weights with names", accept = FALSE, make = function() gbm(with_args(weights = stats::setNames(rep(1, 10L), letters[1:10])))),
    list(case = "rule: early stopping at n_trees", accept = FALSE, make = function() gbm(with_args(early_stopping_rounds = 500L))),
    list(case = "rule: num_class without multiclass", accept = FALSE, make = function() gbm(with_args(objective = "binary"))),
    list(case = "rule: multiclass without num_class", accept = FALSE, make = function() gbm(without("num_class"))),
    list(case = "write converts a whole double", accept = TRUE, make = function() {
      y <- write(gbm(a), "n_trees", 300)
      stopifnot(identical(get_field(y, "n_trees"), 300L))
      y
    }),
    list(case = "write n_trees at 0", accept = FALSE, make = function() write(gbm(a), "n_trees", 0L)),
    list(case = "write breaking a rule", accept = FALSE, make = function() write(gbm(a), "n_trees", 20L)),
    list(case = "a failed write keeps the value", accept = TRUE, make = function() {
      x <- gbm(a)
      # RC and R6 write in place: the failed write is on `x` itself.
      if (inherits(x, "envRefClass") || inherits(x, "R6")) {
        try(x$set_fr("n_trees", 0L), silent = TRUE)
      } else {
        try(write(x, "n_trees", 0L), silent = TRUE)
      }
      stopifnot(identical(get_field(x, "n_trees"), 500L))
      x
    }),
    list(case = "the timed expressions evaluate", accept = TRUE, make = function() {
      env <- new.env(parent = globalenv())
      env$args <- a
      env$x <- gbm(a)
      env$xw <- gbm(a)
      env$w <- Wide20()
      for (n in setdiff(names(ops), c("write_fr_fn", "dispatch_label"))) {
        if (is.language(ops[[n]])) eval(ops[[n]], env)
      }
      TRUE
    }),
    list(case = "Wide20 valid", accept = TRUE, make = function() Wide20(f1 = 0.5)),
    list(case = "Wide20 defaults", accept = TRUE, make = function() {
      w <- Wide20()
      stopifnot(identical(get_field(w, "f20"), 0.1))
      w
    }),
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

run_corpus <- function(system, ops) {
  cases <- corpus_cases(ops)
  if (identical(system, "R8 Rust")) {
    cases <- Filter(function(k) !grepl("GBMRun|Resampler|fold", k$case), cases)
  }
  do.call(rbind, lapply(cases, function(k) {
    observed <- tryCatch({ k$make(); TRUE }, error = function(e) FALSE)
    data.frame(system = system, case = k$case, expected = k$accept, observed = observed)
  }))
}

# The stored fields of the valid GBM, as JSON, the same in every system.
stored_fields <- function() {
  x <- do.call(GBM, utils::modifyList(gbm_args(), list(n_trees = 500, weights = rep(1L, 10L))))
  f <- lapply(GBM_FIELDS, function(n) {
    v <- get_field(x, n)
    if (is.null(v)) "<absent>" else list(type = typeof(v), value = as.vector(v))
  })
  as.character(jsonlite::toJSON(stats::setNames(f, GBM_FIELDS), auto_unbox = TRUE, digits = NA))
}
