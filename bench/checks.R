# checks.R
# ::r8 bench::
# 2026- EDG rtemis.org

# The field checks that the hand-written validators of S3, S4, Reference
# Classes, R6, and S7 run, with R8's semantics: a double field holds one
# finite double, and an integer converts; an integer field holds one whole
# number, and a whole double converts; NA is an error outside an option; a
# value carries no attributes beyond those of its class. Each check returns
# the value in its stored form or signals an error of class `bench_invalid`.
# Every system calls the same checks in the same order, so the benchmark
# compares where each system runs them, not how much each one checks.

invalid <- function(name, what) {
  stop(structure(
    class = c("bench_invalid", "error", "condition"),
    list(message = sprintf("`%s` %s", name, what), call = NULL)
  ))
}

plain <- function(x, name, class = NULL) {
  a <- attributes(x)
  a[["class"]] <- NULL
  if (!is.null(class)) {
    a[["levels"]] <- NULL
    a[["tzone"]] <- NULL
    a[["dim"]] <- NULL
  }
  if (length(a) > 0L) invalid(name, "carries attributes")
}

chk_double <- function(x, name, min = -Inf, max = Inf, min_open = FALSE, max_open = FALSE) {
  if (!is.numeric(x) || is.object(x)) invalid(name, "is not a number")
  if (length(x) != 1L) invalid(name, "is not one value")
  plain(x, name)
  if (is.na(x)) invalid(name, "is NA")
  if (!is.finite(x)) invalid(name, "is not finite")
  if (if (min_open) x <= min else x < min) invalid(name, "is below its minimum")
  if (if (max_open) x >= max else x > max) invalid(name, "is above its maximum")
  as.double(x)
}

chk_int <- function(x, name, min = -.Machine$integer.max, max = .Machine$integer.max) {
  if (!is.numeric(x) || is.object(x)) invalid(name, "is not a number")
  if (length(x) != 1L) invalid(name, "is not one value")
  plain(x, name)
  if (is.na(x)) invalid(name, "is NA")
  if (is.double(x) && (!is.finite(x) || x != trunc(x) || abs(x) > .Machine$integer.max)) {
    invalid(name, "is not a whole number")
  }
  if (x < min) invalid(name, "is below its minimum")
  if (x > max) invalid(name, "is above its maximum")
  as.integer(x)
}

chk_opt_int <- function(x, name, min = -.Machine$integer.max) {
  if (is.null(x)) NULL else chk_int(x, name, min = min)
}

chk_string <- function(x, name, pattern = NULL) {
  if (!is.character(x) || is.object(x)) invalid(name, "is not a string")
  if (length(x) != 1L) invalid(name, "is not one value")
  plain(x, name)
  if (is.na(x)) invalid(name, "is NA")
  if (!is.null(pattern) && !grepl(pattern, x, perl = TRUE)) invalid(name, "does not match its pattern")
  x
}

chk_flag <- function(x, name) {
  if (!is.logical(x) || is.object(x)) invalid(name, "is not a logical")
  if (length(x) != 1L) invalid(name, "is not one value")
  plain(x, name)
  if (is.na(x)) invalid(name, "is NA")
  x
}

chk_dvec <- function(x, name, min = -Inf) {
  if (!is.numeric(x) || is.object(x)) invalid(name, "is not numeric")
  plain(x, name)
  if (anyNA(x)) invalid(name, "holds NA")
  if (!all(is.finite(x))) invalid(name, "holds a value that is not finite")
  if (length(x) > 0L && min(x) < min) invalid(name, "holds a value below its minimum")
  as.double(x)
}

chk_ivec <- function(x, name, min = -.Machine$integer.max) {
  if (!is.numeric(x) || is.object(x)) invalid(name, "is not numeric")
  plain(x, name)
  if (anyNA(x)) invalid(name, "holds NA")
  if (is.double(x) && any(x != trunc(x) | abs(x) > .Machine$integer.max)) {
    invalid(name, "holds a value that is not a whole number")
  }
  if (length(x) > 0L && min(x) < min) invalid(name, "holds a value below its minimum")
  as.integer(x)
}

chk_date <- function(x, name) {
  if (!inherits(x, "Date")) invalid(name, "is not a Date")
  if (length(x) != 1L) invalid(name, "is not one value")
  plain(x, name, class = "Date")
  if (is.na(x)) invalid(name, "is NA")
  x
}

# A factor's values are among the declared levels, and it is stored with
# the declared levels in their order, as R8 stores it.
chk_factor <- function(x, name, levels) {
  if (!is.factor(x)) invalid(name, "is not a factor")
  plain(x, name, class = "factor")
  if (anyNA(x)) invalid(name, "holds NA")
  if (!all(levels(x) %in% levels)) invalid(name, "holds a value outside its levels")
  if (identical(levels(x), levels)) x else factor(as.character(x), levels = levels)
}

chk_matrix <- function(x, name, ncol) {
  if (!is.matrix(x) || !is.numeric(x)) invalid(name, "is not a numeric matrix")
  if (ncol(x) != ncol) invalid(name, "does not have its columns")
  if (anyNA(x) || !all(is.finite(x))) invalid(name, "holds a value that is not finite")
  storage.mode(x) <- "double"
  x
}

chk_object <- function(x, name, class) {
  if (!inherits(x, class)) invalid(name, paste("is not a", class))
  x
}

chk_objects <- function(x, name, class) {
  if (!is.list(x) || is.object(x)) invalid(name, "is not a list")
  for (e in x) {
    if (!inherits(e, class)) invalid(name, paste("holds an element that is not a", class))
  }
  x
}

# The rules of GBM, GBMRun, and Resampler, on their fields.
rules_gbm <- function(f) {
  if (!is.null(f$early_stopping_rounds) && !(f$early_stopping_rounds < f$n_trees)) {
    invalid("early_stopping_rounds", "is not below n_trees")
  }
  if ((f$objective == "multiclass") != !is.null(f$num_class)) {
    invalid("num_class", "is present exactly when the objective is multiclass")
  }
  invisible(TRUE)
}

rules_gbm_run <- function(f, n) {
  if (length(f$folds) != n) invalid("folds", "does not have one element per resample")
  invisible(TRUE)
}

# The checks of each type's fields, in declaration order, returning the
# fields in their stored forms. `cls` names the class of nested values in
# the system that calls them.
check_gbm <- function(f) {
  f$learning_rate <- chk_double(f$learning_rate, "learning_rate", 0, 1, min_open = TRUE)
  f$subsample <- chk_double(f$subsample, "subsample", 0, 1, min_open = TRUE)
  f$colsample <- chk_double(f$colsample, "colsample", 0, 1, min_open = TRUE)
  f$n_trees <- chk_int(f$n_trees, "n_trees", min = 1L)
  f$max_depth <- chk_int(f$max_depth, "max_depth", min = 1L, max = 32L)
  f$objective <- chk_string(f$objective, "objective", "^(regression|binary|multiclass)$")
  f["num_class"] <- list(chk_opt_int(f$num_class, "num_class", min = 2L))
  f["early_stopping_rounds"] <- list(chk_opt_int(f$early_stopping_rounds, "early_stopping_rounds", min = 1L))
  f["seed"] <- list(chk_opt_int(f$seed, "seed"))
  f$verbose <- chk_flag(f$verbose, "verbose")
  f$weights <- chk_dvec(f$weights, "weights", min = 0)
  f$feature_idx <- chk_ivec(f$feature_idx, "feature_idx", min = 1L)
  rules_gbm(f)
  f
}

check_resampler <- function(f) {
  f$type <- chk_string(f$type, "type", "^(kfold|bootstrap|stratsub)$")
  f$n <- chk_int(f$n, "n", min = 1L)
  f$train_p <- chk_double(f$train_p, "train_p", 0, 1, min_open = TRUE, max_open = TRUE)
  f$stratify <- chk_flag(f$stratify, "stratify")
  f
}

check_gbm_run <- function(f, cls_gbm, cls_resampler, n_of) {
  f$gbm <- chk_object(f$gbm, "gbm", cls_gbm)
  f$resampler <- chk_object(f$resampler, "resampler", cls_resampler)
  f$created <- chk_date(f$created, "created")
  f$outcome <- chk_factor(f$outcome, "outcome", c("control", "case"))
  f$importance <- chk_matrix(f$importance, "importance", 2L)
  f$folds <- chk_objects(f$folds, "folds", cls_resampler)
  rules_gbm_run(f, n_of(f$resampler))
  f
}

check_wide <- function(f) {
  for (i in seq_len(20L)) {
    nm <- paste0("f", i)
    f[[nm]] <- chk_double(f[[nm]], nm, 0, 1)
  }
  f
}

GBM_FIELDS <- c(
  "learning_rate", "subsample", "colsample", "n_trees", "max_depth", "objective",
  "num_class", "early_stopping_rounds", "seed", "verbose", "weights", "feature_idx"
)
RESAMPLER_FIELDS <- c("type", "n", "train_p", "stratify")
GBM_RUN_FIELDS <- c("gbm", "resampler", "created", "outcome", "importance", "folds")
WIDE_FIELDS <- paste0("f", 1:20)
