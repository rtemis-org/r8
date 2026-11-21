# checks.R
# ::r8 bench::
# 2026- EDG rtemis.org

# The field checks that the validators of S3, S4, Reference Classes, R6, and
# S7 run, with R8's semantics: a double field holds one finite double, and
# an integer converts; an integer field holds one whole number, and a whole
# double converts; NA is a problem outside an option; a value carries no
# attributes beyond those of its class; a factor's values are among its
# levels, and it is stored with the declared levels in their order. A check
# returns the value in its stored form, or a problem, a string of class
# `bench_problem`; each system signals a problem in its idiom: S3, RC, and
# R6 with stop(), an S4 validity method and an S7 validator by returning
# the message. Every system calls the same checks in the same order.
# spec: R8/class-system#benchmark

problem <- function(name, what) {
  structure(sprintf("`%s` %s", name, what), class = "bench_problem")
}

is_problem <- function(x) inherits(x, "bench_problem")

# Signals a problem as an error, the idiom of S3, RC, and R6, and returns
# any other value.
signal <- function(x) {
  if (inherits(x, "bench_problem")) stop(unclass(x), call. = FALSE)
  x
}

plain <- function(x, keep = NULL) {
  a <- attributes(x)
  length(setdiff(names(a), keep)) == 0L
}

chk_double <- function(x, name, min = -Inf, max = Inf, min_open = FALSE, max_open = FALSE) {
  if (!is.numeric(x) || is.object(x)) return(problem(name, "is not a number"))
  if (length(x) != 1L) return(problem(name, "is not one value"))
  if (!plain(x)) return(problem(name, "carries attributes"))
  if (is.na(x)) return(problem(name, "is NA"))
  if (!is.finite(x)) return(problem(name, "is not finite"))
  if (if (min_open) x <= min else x < min) return(problem(name, "is below its minimum"))
  if (if (max_open) x >= max else x > max) return(problem(name, "is above its maximum"))
  as.double(x)
}

chk_int <- function(x, name, min = -.Machine$integer.max, max = .Machine$integer.max) {
  if (!is.numeric(x) || is.object(x)) return(problem(name, "is not a number"))
  if (length(x) != 1L) return(problem(name, "is not one value"))
  if (!plain(x)) return(problem(name, "carries attributes"))
  if (is.na(x)) return(problem(name, "is NA"))
  if (is.double(x) && (!is.finite(x) || x != trunc(x) || abs(x) > .Machine$integer.max)) {
    return(problem(name, "is not a whole number"))
  }
  if (x < min) return(problem(name, "is below its minimum"))
  if (x > max) return(problem(name, "is above its maximum"))
  as.integer(x)
}

chk_opt_int <- function(x, name, min = -.Machine$integer.max) {
  if (is.null(x)) NULL else chk_int(x, name, min = min)
}

chk_string <- function(x, name, pattern = NULL) {
  if (!is.character(x) || is.object(x)) return(problem(name, "is not a string"))
  if (length(x) != 1L) return(problem(name, "is not one value"))
  if (!plain(x)) return(problem(name, "carries attributes"))
  if (is.na(x)) return(problem(name, "is NA"))
  if (!is.null(pattern) && !grepl(pattern, x, perl = TRUE)) return(problem(name, "does not match its pattern"))
  x
}

chk_flag <- function(x, name) {
  if (!is.logical(x) || is.object(x)) return(problem(name, "is not a logical"))
  if (length(x) != 1L) return(problem(name, "is not one value"))
  if (!plain(x)) return(problem(name, "carries attributes"))
  if (is.na(x)) return(problem(name, "is NA"))
  x
}

chk_dvec <- function(x, name, min = -Inf) {
  if (!is.numeric(x) || is.object(x)) return(problem(name, "is not numeric"))
  if (!plain(x)) return(problem(name, "carries attributes"))
  if (anyNA(x)) return(problem(name, "holds NA"))
  if (!all(is.finite(x))) return(problem(name, "holds a value that is not finite"))
  if (length(x) > 0L && min(x) < min) return(problem(name, "holds a value below its minimum"))
  as.double(x)
}

chk_ivec <- function(x, name, min = -.Machine$integer.max) {
  if (!is.numeric(x) || is.object(x)) return(problem(name, "is not numeric"))
  if (!plain(x)) return(problem(name, "carries attributes"))
  if (anyNA(x)) return(problem(name, "holds NA"))
  if (is.double(x) && any(x != trunc(x) | abs(x) > .Machine$integer.max)) {
    return(problem(name, "holds a value that is not a whole number"))
  }
  if (length(x) > 0L && min(x) < min) return(problem(name, "holds a value below its minimum"))
  as.integer(x)
}

chk_date <- function(x, name) {
  if (!inherits(x, "Date")) return(problem(name, "is not a Date"))
  if (length(x) != 1L) return(problem(name, "is not one value"))
  if (!plain(x, "class")) return(problem(name, "carries attributes"))
  if (is.na(x)) return(problem(name, "is NA"))
  x
}

chk_factor <- function(x, name, levels) {
  if (!is.factor(x)) return(problem(name, "is not a factor"))
  if (!plain(x, c("class", "levels"))) return(problem(name, "carries attributes"))
  if (anyNA(x)) return(problem(name, "holds NA"))
  if (!all(levels(x) %in% levels)) return(problem(name, "holds a value outside its levels"))
  if (identical(levels(x), levels)) x else factor(as.character(x), levels = levels)
}

chk_matrix <- function(x, name, ncol) {
  if (!is.matrix(x) || !is.numeric(x)) return(problem(name, "is not a numeric matrix"))
  if (ncol(x) != ncol) return(problem(name, "does not have its columns"))
  if (anyNA(x) || !all(is.finite(x))) return(problem(name, "holds a value that is not finite"))
  storage.mode(x) <- "double"
  x
}

chk_object <- function(x, name, class) {
  if (!inherits(x, class)) return(problem(name, paste("is not a", class)))
  x
}

chk_objects <- function(x, name, class) {
  if (!is.list(x) || is.object(x)) return(problem(name, "is not a list"))
  for (e in x) {
    if (!inherits(e, class)) return(problem(name, paste("holds an element that is not a", class)))
  }
  x
}

# The check of each field of each type, in declaration order.
GBM_CHECKS <- list(
  learning_rate = function(v) chk_double(v, "learning_rate", 0, 1, min_open = TRUE),
  subsample = function(v) chk_double(v, "subsample", 0, 1, min_open = TRUE),
  colsample = function(v) chk_double(v, "colsample", 0, 1, min_open = TRUE),
  n_trees = function(v) chk_int(v, "n_trees", min = 1L),
  max_depth = function(v) chk_int(v, "max_depth", min = 1L, max = 32L),
  objective = function(v) chk_string(v, "objective", "^(regression|binary|multiclass)$"),
  num_class = function(v) chk_opt_int(v, "num_class", min = 2L),
  early_stopping_rounds = function(v) chk_opt_int(v, "early_stopping_rounds", min = 1L),
  seed = function(v) chk_opt_int(v, "seed"),
  verbose = function(v) chk_flag(v, "verbose"),
  weights = function(v) chk_dvec(v, "weights", min = 0),
  feature_idx = function(v) chk_ivec(v, "feature_idx", min = 1L)
)
RESAMPLER_CHECKS <- list(
  type = function(v) chk_string(v, "type", "^(kfold|bootstrap|stratsub)$"),
  n = function(v) chk_int(v, "n", min = 1L),
  train_p = function(v) chk_double(v, "train_p", 0, 1, min_open = TRUE, max_open = TRUE),
  stratify = function(v) chk_flag(v, "stratify")
)
WIDE_CHECKS <- stats::setNames(
  lapply(paste0("f", 1:20), function(n) {
    force(n)
    function(v) chk_double(v, n, 0, 1)
  }),
  paste0("f", 1:20)
)
GBM_FIELDS <- names(GBM_CHECKS)
RESAMPLER_FIELDS <- names(RESAMPLER_CHECKS)
WIDE_FIELDS <- names(WIDE_CHECKS)
GBM_RUN_FIELDS <- c("gbm", "resampler", "created", "outcome", "importance", "folds")
WIDE_DEFAULTS <- as.list(stats::setNames(rep(0.1, 20L), WIDE_FIELDS))

# The fields `f` with each checked and in its stored form, or the first
# problem.
check_fields <- function(f, checks) {
  for (n in names(checks)) {
    v <- checks[[n]](f[[n]])
    if (inherits(v, "bench_problem")) return(v)
    f[n] <- list(v)
  }
  f
}

# The rules of GBM and GBMRun, on their fields: NULL, or a problem.
rules_gbm <- function(f) {
  if (!is.null(f$early_stopping_rounds) && !(f$early_stopping_rounds < f$n_trees)) {
    return(problem("early_stopping_rounds", "is not below n_trees"))
  }
  if ((f$objective == "multiclass") != !is.null(f$num_class)) {
    return(problem("num_class", "is present exactly when the objective is multiclass"))
  }
  NULL
}

rules_gbm_run <- function(folds, n) {
  if (length(folds) != n) problem("folds", "does not have one element per resample")
}

# Every field and the rules: the fields in their stored forms, or a problem.
check_gbm <- function(f) {
  f <- check_fields(f, GBM_CHECKS)
  if (inherits(f, "bench_problem")) return(f)
  r <- rules_gbm(f)
  if (is.null(r)) f else r
}

check_resampler <- function(f) check_fields(f, RESAMPLER_CHECKS)

check_wide <- function(f) check_fields(f, WIDE_CHECKS)

# One field replaced and checked, then the rules: the fields, or a problem.
check_gbm_field <- function(f, name, value) {
  v <- GBM_CHECKS[[name]](value)
  if (inherits(v, "bench_problem")) return(v)
  f[name] <- list(v)
  r <- rules_gbm(f)
  if (is.null(r)) f else r
}

check_wide_field <- function(f, name, value) {
  v <- WIDE_CHECKS[[name]](value)
  if (inherits(v, "bench_problem")) return(v)
  f[name] <- list(v)
  f
}

# GBMRun's fields; `cls` names the classes of nested values in the system
# that calls it, and `n_of` reads a Resampler's `n`.
check_gbm_run <- function(f, cls_gbm, cls_resampler, n_of) {
  checks <- list(
    gbm = function(v) chk_object(v, "gbm", cls_gbm),
    resampler = function(v) chk_object(v, "resampler", cls_resampler),
    created = function(v) chk_date(v, "created"),
    outcome = function(v) chk_factor(v, "outcome", c("control", "case")),
    importance = function(v) chk_matrix(v, "importance", 2L),
    folds = function(v) chk_objects(v, "folds", cls_resampler)
  )
  f <- check_fields(f, checks)
  if (inherits(f, "bench_problem")) return(f)
  r <- rules_gbm_run(f$folds, n_of(f$resampler))
  if (is.null(r)) f else r
}

# The conversion of a type's numbers before typed slots store them, as the
# checks convert: a whole double to an integer, an integer to a double.
# S4, RC, and S7 convert in their constructors, so that a whole double for
# an integer slot reaches the validity method.
as_int <- function(v) {
  if (is.double(v) && !is.object(v) && length(v) > 0L && all(!is.na(v) & v == trunc(v) & abs(v) <= .Machine$integer.max)) {
    `attributes<-`(as.integer(v), attributes(v))
  } else {
    v
  }
}

as_dbl <- function(v) {
  if (is.integer(v) && !is.object(v)) `attributes<-`(as.double(v), attributes(v)) else v
}

convert_gbm <- function(f) {
  for (n in c("n_trees", "max_depth", "num_class", "early_stopping_rounds", "seed", "feature_idx")) {
    if (!is.null(f[[n]])) f[[n]] <- as_int(f[[n]])
  }
  for (n in c("learning_rate", "subsample", "colsample", "weights")) {
    if (!is.null(f[[n]])) f[[n]] <- as_dbl(f[[n]])
  }
  f
}

convert_resampler <- function(f) {
  if (!is.null(f$n)) f$n <- as_int(f$n)
  if (!is.null(f$train_p)) f$train_p <- as_dbl(f$train_p)
  f
}

convert_wide <- function(f) {
  for (n in names(f)) f[[n]] <- as_dbl(f[[n]])
  f
}

# The fields of a call's arguments, every field named and an omitted
# option NULL.
fields_of <- function(names, args) {
  out <- stats::setNames(vector("list", length(names)), names)
  for (n in names(args)) out[n] <- list(args[[n]])
  out
}
