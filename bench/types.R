# types.R
# ::r8 bench::
# 2026- EDG rtemis.org

# The benchmark types, Wide20, GBM, Resampler, and GBMRun, in each system:
# S3, S4, Reference Classes (RC), R6, S7, R8 with R-native values, and R8
# with Rust-native values (the package r8benchR, for Wide20 and GBM).
# `declare_system(system)` declares a system's types in the global
# environment and returns its operations: the expressions a row times, each
# reading `x`, a GBM value, `w`, a Wide20 value, and `args`, the arguments
# of a GBM. Validation runs the checks of checks.R in each system's idiom:
# S3 and R6 by hand in the constructor and setters, S4 through
# `setValidity()`, RC through a `validate()` method, S7 through property
# and class validators; R8 declares the constraints and rules.
# spec: R8/class-system#benchmark

# A package of 200 types (namespace.R) holds checks.R itself, collated
# before this file.
if (!exists("check_gbm")) source(file.path(Sys.getenv("R8_BENCH_DIR", "bench"), "checks.R"))

SYSTEMS <- c("S3", "S4", "RC", "R6", "S7", "R8", "R8 Rust")

# The integer and double fields of each type, which constructors that hold
# typed slots convert before storing: a whole double to an integer, an
# integer to a double, as R8 does.
INT_FIELDS <- c("n_trees", "max_depth", "num_class", "early_stopping_rounds", "seed", "feature_idx", "n")
DBL_FIELDS <- c("learning_rate", "subsample", "colsample", "weights", "train_p", paste0("f", 1:20))

convert <- function(f) {
  for (n in intersect(names(f), INT_FIELDS)) {
    v <- f[[n]]
    if (is.double(v) && !is.object(v) && all(!is.na(v) & v == trunc(v) & abs(v) <= .Machine$integer.max)) {
      f[[n]] <- `attributes<-`(as.integer(v), attributes(v))
    }
  }
  for (n in intersect(names(f), DBL_FIELDS)) {
    v <- f[[n]]
    if (is.integer(v) && !is.object(v)) f[[n]] <- `attributes<-`(as.double(v), attributes(v))
  }
  f
}

# The fields of a call's arguments, with every field named and an omitted
# option NULL.
fields_of <- function(names, args) {
  out <- stats::setNames(vector("list", length(names)), names)
  for (n in names(args)) out[n] <- list(args[[n]])
  out
}

# %% S3 ----
declare_s3 <- function() {
  g <- globalenv()
  g$Wide20 <- function(...) {
    f <- fields_of(WIDE_FIELDS, utils::modifyList(as.list(stats::setNames(rep(0.1, 20L), WIDE_FIELDS)), list(...)))
    structure(check_wide(f), class = "s3_Wide20")
  }
  g$GBM <- function(...) structure(check_gbm(fields_of(GBM_FIELDS, list(...))), class = "s3_GBM")
  g$Resampler <- function(...) structure(check_resampler(fields_of(RESAMPLER_FIELDS, list(...))), class = "s3_Resampler")
  g$GBMRun <- function(...) {
    f <- check_gbm_run(fields_of(GBM_RUN_FIELDS, list(...)), "s3_GBM", "s3_Resampler", function(r) r$n)
    structure(f, class = "s3_GBMRun")
  }
  # A setter replaces a field and checks the value again.
  g$`$<-.s3_GBM` <- function(x, name, value) {
    f <- unclass(x)
    f[name] <- list(value)
    structure(check_gbm(f), class = "s3_GBM")
  }
  g$`$<-.s3_Wide20` <- function(x, name, value) {
    f <- unclass(x)
    f[name] <- list(value)
    structure(check_wide(f), class = "s3_Wide20")
  }
  g$validate_gbm <- function(x) invisible(check_gbm(unclass(x)))
  g$complexity <- function(x) UseMethod("complexity")
  g$complexity.s3_GBM <- function(x) x$n_trees * x$max_depth
  list(
    construct = quote(do.call(GBM, args)),
    construct_wide = quote(Wide20()),
    read = quote(x$n_trees),
    write = quote({ y <- x; y$n_trees <- 200L; y }),
    write_wide = quote({ v <- w; v$f1 <- 0.5; v }),
    validate = quote(validate_gbm(x)),
    dispatch = quote(complexity(x))
  )
}

# %% S4 ----
declare_s4 <- function() {
  g <- globalenv()
  methods::setClassUnion("intOrNULL", c("integer", "NULL"), where = g)
  slots_of <- function(object, names) stats::setNames(lapply(names, function(n) methods::slot(object, n)), names)
  validity <- function(check, names) {
    function(object) {
      tryCatch({ check(slots_of(object, names)); TRUE }, bench_invalid = conditionMessage)
    }
  }
  methods::setClass(
    "s4_Wide20",
    slots = stats::setNames(rep("numeric", 20L), WIDE_FIELDS),
    prototype = as.list(stats::setNames(rep(0.1, 20L), WIDE_FIELDS)),
    validity = validity(check_wide, WIDE_FIELDS),
    where = g
  )
  methods::setClass(
    "s4_GBM",
    slots = c(
      learning_rate = "numeric", subsample = "numeric", colsample = "numeric",
      n_trees = "integer", max_depth = "integer", objective = "character",
      num_class = "intOrNULL", early_stopping_rounds = "intOrNULL", seed = "intOrNULL",
      verbose = "logical", weights = "numeric", feature_idx = "integer"
    ),
    validity = validity(check_gbm, GBM_FIELDS),
    where = g
  )
  methods::setClass(
    "s4_Resampler",
    slots = c(type = "character", n = "integer", train_p = "numeric", stratify = "logical"),
    validity = validity(check_resampler, RESAMPLER_FIELDS),
    where = g
  )
  methods::setClass(
    "s4_GBMRun",
    slots = c(
      gbm = "s4_GBM", resampler = "s4_Resampler", created = "Date",
      outcome = "factor", importance = "matrix", folds = "list"
    ),
    validity = validity(function(f) {
      check_gbm_run(f, "s4_GBM", "s4_Resampler", function(r) r@n)
    }, GBM_RUN_FIELDS),
    where = g
  )
  # Constructors convert numbers as R8 does; new() checks the slot classes
  # and runs the validity method.
  g$Wide20 <- function(...) do.call(methods::new, c(list("s4_Wide20"), convert(list(...))))
  g$GBM <- function(...) do.call(methods::new, c(list("s4_GBM"), convert(list(...))))
  g$Resampler <- function(...) do.call(methods::new, c(list("s4_Resampler"), convert(list(...))))
  g$GBMRun <- function(...) do.call(methods::new, c(list("s4_GBMRun"), list(...)))
  methods::setGeneric("complexity", function(x) standardGeneric("complexity"), where = g)
  methods::setMethod("complexity", "s4_GBM", function(x) x@n_trees * x@max_depth, where = g)
  list(
    construct = quote(do.call(GBM, args)),
    construct_wide = quote(Wide20()),
    read = quote(x@n_trees),
    write = quote({ y <- x; y@n_trees <- 200L; methods::validObject(y); y }),
    write_wide = quote({ v <- w; v@f1 <- 0.5; methods::validObject(v); v }),
    validate = quote(methods::validObject(x)),
    dispatch = quote(complexity(x))
  )
}

# %% Reference Classes and R6 ----
# RC and R6 rebind a method's enclosing environment to the object, so a
# method reaches only fields, `self` or `.self`, and globals. The methods
# are built with the check function and the field names inlined.
WIDE_DEFAULTS <- as.list(stats::setNames(rep(0.1, 20L), WIDE_FIELDS))
check_rc_gbm_run <- function(f) check_gbm_run(f, "rc_GBM", "rc_Resampler", function(r) r$n)
check_r6_gbm_run <- function(f) check_gbm_run(f, "r6_GBM", "r6_Resampler", function(r) r$n)

declare_rc <- function() {
  g <- globalenv()
  rc_class <- function(name, fields, check, names, defaults = NULL, extra = list()) {
    init <- if (is.null(defaults)) {
      bquote(function(...) {
        f <- .(check)(convert(fields_of(.(names), list(...))))
        for (n in names(f)) field(n, f[[n]])
        invisible(.self)
      })
    } else {
      bquote(function(...) {
        f <- .(check)(convert(fields_of(.(names), utils::modifyList(.(defaults), list(...)))))
        for (n in names(f)) field(n, f[[n]])
        invisible(.self)
      })
    }
    methods::setRefClass(
      name,
      fields = fields,
      methods = c(
        list(
          initialize = eval(init),
          values = eval(bquote(function() {
            stats::setNames(lapply(.(names), function(n) field(n)), .(names))
          })),
          validate = eval(bquote(function() invisible(.(check)(values())))),
          set = eval(bquote(function(name, value) {
            f <- values()
            f[name] <- list(value)
            f <- .(check)(convert(f))
            field(name, f[[name]])
            invisible(.self)
          }))
        ),
        extra
      ),
      where = g
    )
  }
  g$rc_Wide20 <- rc_class(
    "rc_Wide20",
    stats::setNames(as.list(rep("numeric", 20L)), WIDE_FIELDS),
    quote(check_wide), quote(WIDE_FIELDS), quote(WIDE_DEFAULTS)
  )
  g$Wide20 <- function(...) rc_Wide20$new(...)
  g$rc_GBM <- rc_class(
    "rc_GBM",
    list(
      learning_rate = "numeric", subsample = "numeric", colsample = "numeric",
      n_trees = "integer", max_depth = "integer", objective = "character",
      num_class = "ANY", early_stopping_rounds = "ANY", seed = "ANY",
      verbose = "logical", weights = "numeric", feature_idx = "integer"
    ),
    quote(check_gbm), quote(GBM_FIELDS),
    extra = list(complexity = function() n_trees * max_depth)
  )
  g$GBM <- function(...) rc_GBM$new(...)
  g$rc_Resampler <- rc_class(
    "rc_Resampler",
    list(type = "character", n = "integer", train_p = "numeric", stratify = "logical"),
    quote(check_resampler), quote(RESAMPLER_FIELDS)
  )
  g$Resampler <- function(...) rc_Resampler$new(...)
  g$rc_GBMRun <- rc_class(
    "rc_GBMRun",
    list(gbm = "ANY", resampler = "ANY", created = "Date", outcome = "factor", importance = "matrix", folds = "list"),
    quote(check_rc_gbm_run), quote(GBM_RUN_FIELDS)
  )
  g$GBMRun <- function(...) rc_GBMRun$new(...)
  list(
    construct = quote(do.call(GBM, args)),
    construct_wide = quote(Wide20()),
    read = quote(x$n_trees),
    write = quote(x$set("n_trees", 200L)),
    write_wide = quote(w$set("f1", 0.5)),
    validate = quote(x$validate()),
    dispatch = quote(x$complexity())
  )
}

declare_r6 <- function() {
  g <- globalenv()
  # A field is an active binding over a private list of the checked fields;
  # assigning it checks the fields again.
  r6_class <- function(name, field_names, check, names, defaults = NULL, extra = list()) {
    active <- lapply(field_names, function(n) {
      eval(bquote(function(value) {
        if (missing(value)) {
          return(private$f[[.(n)]])
        }
        f <- private$f
        f[.(n)] <- list(value)
        private$f <- .(check)(f)
      }))
    })
    names(active) <- field_names
    init <- if (is.null(defaults)) {
      bquote(function(...) private$f <- .(check)(fields_of(.(names), list(...))))
    } else {
      bquote(function(...) {
        private$f <- .(check)(fields_of(.(names), utils::modifyList(.(defaults), list(...))))
      })
    }
    R6::R6Class(
      name,
      public = c(
        list(
          initialize = eval(init),
          validate = eval(bquote(function() invisible(.(check)(private$f))))
        ),
        extra
      ),
      private = list(f = NULL),
      active = active
    )
  }
  g$r6_Wide20 <- r6_class("r6_Wide20", WIDE_FIELDS, quote(check_wide), quote(WIDE_FIELDS), quote(WIDE_DEFAULTS))
  g$Wide20 <- function(...) r6_Wide20$new(...)
  g$r6_GBM <- r6_class(
    "r6_GBM", GBM_FIELDS, quote(check_gbm), quote(GBM_FIELDS),
    extra = list(complexity = function() self$n_trees * self$max_depth)
  )
  g$GBM <- function(...) r6_GBM$new(...)
  g$r6_Resampler <- r6_class("r6_Resampler", RESAMPLER_FIELDS, quote(check_resampler), quote(RESAMPLER_FIELDS))
  g$Resampler <- function(...) r6_Resampler$new(...)
  g$r6_GBMRun <- r6_class("r6_GBMRun", GBM_RUN_FIELDS, quote(check_r6_gbm_run), quote(GBM_RUN_FIELDS))
  g$GBMRun <- function(...) r6_GBMRun$new(...)
  list(
    construct = quote(do.call(GBM, args)),
    construct_wide = quote(Wide20()),
    read = quote(x$n_trees),
    write = quote(x$n_trees <- 200L),
    write_wide = quote(w$f1 <- 0.5),
    validate = quote(x$validate()),
    dispatch = quote(x$complexity())
  )
}

# %% S7 ----
declare_s7 <- function() {
  g <- globalenv()
  # A property validator runs a field's check; the class validator runs the
  # rules. Constructors convert numbers as R8 does.
  prop <- function(class, check, ...) {
    S7::new_property(class, validator = function(value) {
      tryCatch({ check(value, ...); NULL }, bench_invalid = conditionMessage)
    })
  }
  rules <- function(fn) {
    function(self) tryCatch({ fn(self); NULL }, bench_invalid = conditionMessage)
  }
  new_s7 <- function(class) {
    function(...) do.call(class, convert(list(...)))
  }
  wide_props <- lapply(WIDE_FIELDS, function(n) {
    S7::new_property(S7::class_double, default = 0.1, validator = function(value) {
      tryCatch({ chk_double(value, n, 0, 1); NULL }, bench_invalid = conditionMessage)
    })
  })
  names(wide_props) <- WIDE_FIELDS
  g$s7_Wide20 <- S7::new_class("s7_Wide20", properties = wide_props)
  g$Wide20 <- new_s7(g$s7_Wide20)
  int_or_null <- S7::new_union(NULL, S7::class_integer)
  g$s7_GBM <- S7::new_class(
    "s7_GBM",
    properties = list(
      learning_rate = prop(S7::class_double, chk_double, "learning_rate", 0, 1, min_open = TRUE),
      subsample = prop(S7::class_double, chk_double, "subsample", 0, 1, min_open = TRUE),
      colsample = prop(S7::class_double, chk_double, "colsample", 0, 1, min_open = TRUE),
      n_trees = prop(S7::class_integer, chk_int, "n_trees", min = 1L),
      max_depth = prop(S7::class_integer, chk_int, "max_depth", min = 1L, max = 32L),
      objective = prop(S7::class_character, chk_string, "objective", "^(regression|binary|multiclass)$"),
      num_class = prop(int_or_null, chk_opt_int, "num_class", min = 2L),
      early_stopping_rounds = prop(int_or_null, chk_opt_int, "early_stopping_rounds", min = 1L),
      seed = prop(int_or_null, chk_opt_int, "seed"),
      verbose = prop(S7::class_logical, chk_flag, "verbose"),
      weights = prop(S7::class_double, chk_dvec, "weights", min = 0),
      feature_idx = prop(S7::class_integer, chk_ivec, "feature_idx", min = 1L)
    ),
    validator = rules(function(self) {
      rules_gbm(list(
        early_stopping_rounds = self@early_stopping_rounds,
        n_trees = self@n_trees,
        objective = self@objective,
        num_class = self@num_class
      ))
    })
  )
  g$GBM <- new_s7(g$s7_GBM)
  g$s7_Resampler <- S7::new_class(
    "s7_Resampler",
    properties = list(
      type = prop(S7::class_character, chk_string, "type", "^(kfold|bootstrap|stratsub)$"),
      n = prop(S7::class_integer, chk_int, "n", min = 1L),
      train_p = prop(S7::class_double, chk_double, "train_p", 0, 1, min_open = TRUE, max_open = TRUE),
      stratify = prop(S7::class_logical, chk_flag, "stratify")
    )
  )
  g$Resampler <- new_s7(g$s7_Resampler)
  g$s7_GBMRun <- S7::new_class(
    "s7_GBMRun",
    properties = list(
      gbm = g$s7_GBM,
      resampler = g$s7_Resampler,
      created = prop(S7::class_Date, chk_date, "created"),
      outcome = prop(S7::class_factor, chk_factor, "outcome", c("control", "case")),
      importance = prop(S7::class_double, chk_matrix, "importance", 2L),
      folds = prop(S7::class_list, chk_objects, "folds", "s7_Resampler")
    ),
    validator = rules(function(self) rules_gbm_run(list(folds = self@folds), self@resampler@n))
  )
  g$GBMRun <- function(...) g$s7_GBMRun(...)
  g$complexity <- S7::new_generic("complexity", "x")
  S7::method(g$complexity, g$s7_GBM) <- function(x) x@n_trees * x@max_depth
  list(
    construct = quote(do.call(GBM, args)),
    construct_wide = quote(Wide20()),
    read = quote(x@n_trees),
    write = quote({ y <- x; y@n_trees <- 200L; y }),
    write_wide = quote({ v <- w; v@f1 <- 0.5; v }),
    validate = quote(S7::validate(x)),
    dispatch = quote(complexity(x))
  )
}

# %% R8 ----
# The R8 declarations, in the namespace `bench`. With `native = TRUE`,
# Wide20 and GBM are the declarations of r8benchR, whose values Rust holds.
r8_declarations <- function() {
  list(
    Wide20 = quote(R8::new_struct(
      "Wide20",
      fields = stats::setNames(
        lapply(1:20, function(i) R8::field(R8::type_double(min = 0, max = 1), default = 0.1)),
        WIDE_FIELDS
      )
    )),
    GBM = quote(R8::new_struct(
      "GBM",
      fields = list(
        learning_rate = R8::type_double(exclusive_min = 0, max = 1),
        subsample = R8::type_double(exclusive_min = 0, max = 1),
        colsample = R8::type_double(exclusive_min = 0, max = 1),
        n_trees = R8::type_integer(min = 1L),
        max_depth = R8::type_integer(min = 1L, max = 32L),
        objective = R8::type_character(pattern = "^(regression|binary|multiclass)$"),
        num_class = R8::option(R8::type_integer(min = 2L)),
        early_stopping_rounds = R8::option(R8::type_integer(min = 1L)),
        seed = R8::option(R8::type_integer()),
        verbose = R8::type_logical(),
        weights = R8::vec(R8::type_double(min = 0)),
        feature_idx = R8::vec(R8::type_integer(min = 1L))
      ),
      rules = list(
        R8::rule(
          "early_stop_below_trees",
          early_stopping_rounds < n_trees,
          "early_stopping_rounds is below n_trees.",
          when = !is.null(early_stopping_rounds)
        ),
        R8::rule(
          "num_class_iff_multiclass",
          (objective == "multiclass") == !is.null(num_class),
          "num_class is present exactly when the objective is multiclass."
        )
      )
    ))
  )
}

declare_r8 <- function(native = FALSE) {
  g <- globalenv()
  op <- options(r8.namespace = "bench")
  on.exit(options(op))
  if (native) {
    g$Wide20 <- getExportedValue("r8benchR", "Wide20")
    g$GBM <- getExportedValue("r8benchR", "GBM")
    g$complexity <- getExportedValue("r8benchR", "complexity")
  } else {
    for (n in names(r8_declarations())) assign(n, eval(r8_declarations()[[n]]), envir = g)
    g$Complexity <- R8::new_trait(
      "Complexity",
      methods = list(complexity = R8::trait_method(function(x) NULL))
    )
    g$complexity <- g$Complexity@complexity
    R8::impl(g$Complexity, g$GBM, methods = list(complexity = function(x) x$n_trees * x$max_depth))
    g$n_params <- R8::generic("n_params", "x")
    R8::implement(g$n_params, g$GBM) <- function(x, ...) x$n_trees * x$max_depth
    g$Resampler <- R8::new_struct(
      "Resampler",
      fields = list(
        type = R8::type_character(pattern = "^(kfold|bootstrap|stratsub)$"),
        n = R8::type_integer(min = 1L),
        train_p = R8::type_double(exclusive_min = 0, exclusive_max = 1),
        stratify = R8::type_logical()
      )
    )
    g$GBMRun <- R8::new_struct(
      "GBMRun",
      fields = list(
        gbm = g$GBM,
        resampler = g$Resampler,
        created = R8::type_date(),
        outcome = R8::type_factor(levels = c("control", "case")),
        importance = R8::type_matrix(R8::type_double(), ncol = 2L),
        folds = R8::vec(g$Resampler)
      ),
      rules = list(
        R8::rule("folds_per_resample", length(folds) == resampler$n, "There is one fold per resample.")
      )
    )
  }
  list(
    construct = quote(do.call(GBM, args)),
    construct_wide = quote(Wide20()),
    read = quote(x$n_trees),
    write = quote({ y <- x; y$n_trees <- 200L; y }),
    write_wide = quote({ v <- w; v$f1 <- 0.5; v }),
    validate = quote(R8::r8_validate(x)),
    dispatch = quote(complexity(x)),
    dispatch_generic = if (!native) quote(n_params(x))
  )
}

# %% declare_system ----
declare_system <- function(system) {
  switch(
    system,
    "S3" = declare_s3(),
    "S4" = declare_s4(),
    "RC" = declare_rc(),
    "R6" = declare_r6(),
    "S7" = declare_s7(),
    "R8" = declare_r8(native = FALSE),
    "R8 Rust" = declare_r8(native = TRUE),
    stop("unknown system ", system)
  )
}

# The arguments of a valid GBM with `n` weights and feature indices.
gbm_args <- function(n = 10L) {
  list(
    learning_rate = 0.1,
    subsample = 0.8,
    colsample = 0.9,
    n_trees = 500L,
    max_depth = 6L,
    objective = "multiclass",
    num_class = 3L,
    early_stopping_rounds = 20L,
    seed = 2026L,
    verbose = FALSE,
    weights = rep(1, n),
    feature_idx = seq_len(n)
  )
}

# %% gbm_type ----
# A GBM type named `name` in `system`, created in the environment `where`:
# the rows of memory per type and of a namespace of 200 types declare many.
# A constructor reaches the checks of checks.R, which `where` sees.
GBM_S4_SLOTS <- c(
  learning_rate = "numeric", subsample = "numeric", colsample = "numeric",
  n_trees = "integer", max_depth = "integer", objective = "character",
  num_class = "intOrNULL", early_stopping_rounds = "intOrNULL", seed = "intOrNULL",
  verbose = "logical", weights = "numeric", feature_idx = "integer"
)
GBM_RC_FIELDS <- list(
  learning_rate = "numeric", subsample = "numeric", colsample = "numeric",
  n_trees = "integer", max_depth = "integer", objective = "character",
  num_class = "ANY", early_stopping_rounds = "ANY", seed = "ANY",
  verbose = "logical", weights = "numeric", feature_idx = "integer"
)

gbm_s7_properties <- function() {
  prop <- function(class, check, ...) {
    S7::new_property(class, validator = function(value) {
      tryCatch({ check(value, ...); NULL }, bench_invalid = conditionMessage)
    })
  }
  int_or_null <- S7::new_union(NULL, S7::class_integer)
  list(
    learning_rate = prop(S7::class_double, chk_double, "learning_rate", 0, 1, min_open = TRUE),
    subsample = prop(S7::class_double, chk_double, "subsample", 0, 1, min_open = TRUE),
    colsample = prop(S7::class_double, chk_double, "colsample", 0, 1, min_open = TRUE),
    n_trees = prop(S7::class_integer, chk_int, "n_trees", min = 1L),
    max_depth = prop(S7::class_integer, chk_int, "max_depth", min = 1L, max = 32L),
    objective = prop(S7::class_character, chk_string, "objective", "^(regression|binary|multiclass)$"),
    num_class = prop(int_or_null, chk_opt_int, "num_class", min = 2L),
    early_stopping_rounds = prop(int_or_null, chk_opt_int, "early_stopping_rounds", min = 1L),
    seed = prop(int_or_null, chk_opt_int, "seed"),
    verbose = prop(S7::class_logical, chk_flag, "verbose"),
    weights = prop(S7::class_double, chk_dvec, "weights", min = 0),
    feature_idx = prop(S7::class_integer, chk_ivec, "feature_idx", min = 1L)
  )
}

gbm_s7_validator <- function(self) {
  tryCatch({
    rules_gbm(list(
      early_stopping_rounds = self@early_stopping_rounds,
      n_trees = self@n_trees,
      objective = self@objective,
      num_class = self@num_class
    ))
    NULL
  }, bench_invalid = conditionMessage)
}

gbm_type <- function(system, name, where = globalenv()) {
  switch(
    system,
    "S3" = {
      ctor <- eval(bquote(function(...) {
        structure(check_gbm(fields_of(GBM_FIELDS, list(...))), class = .(name))
      }))
      setter <- eval(bquote(function(x, name, value) {
        f <- unclass(x)
        f[name] <- list(value)
        structure(check_gbm(f), class = .(name))
      }))
      environment(ctor) <- where
      environment(setter) <- where
      assign(paste0("$<-.", name), setter, envir = where)
      ctor
    },
    "S4" = {
      if (!methods::isClass("intOrNULL", where = where)) {
        methods::setClassUnion("intOrNULL", c("integer", "NULL"), where = where)
      }
      methods::setClass(
        name,
        slots = GBM_S4_SLOTS,
        validity = function(object) {
          f <- stats::setNames(lapply(GBM_FIELDS, function(n) methods::slot(object, n)), GBM_FIELDS)
          tryCatch({ check_gbm(f); TRUE }, bench_invalid = conditionMessage)
        },
        where = where
      )
    },
    "RC" = methods::setRefClass(
      name,
      fields = GBM_RC_FIELDS,
      methods = list(
        initialize = function(...) {
          f <- check_gbm(convert(fields_of(GBM_FIELDS, list(...))))
          for (n in names(f)) field(n, f[[n]])
          invisible(.self)
        },
        values = function() stats::setNames(lapply(GBM_FIELDS, function(n) field(n)), GBM_FIELDS),
        validate = function() invisible(check_gbm(values())),
        set = function(name, value) {
          f <- values()
          f[name] <- list(value)
          f <- check_gbm(convert(f))
          field(name, f[[name]])
          invisible(.self)
        },
        complexity = function() n_trees * max_depth
      ),
      where = where
    ),
    "R6" = {
      active <- lapply(GBM_FIELDS, function(n) {
        eval(bquote(function(value) {
          if (missing(value)) {
            return(private$f[[.(n)]])
          }
          f <- private$f
          f[.(n)] <- list(value)
          private$f <- check_gbm(f)
        }))
      })
      names(active) <- GBM_FIELDS
      R6::R6Class(
        name,
        public = list(
          initialize = function(...) private$f <- check_gbm(fields_of(GBM_FIELDS, list(...))),
          validate = function() invisible(check_gbm(private$f)),
          complexity = function() self$n_trees * self$max_depth
        ),
        private = list(f = NULL),
        active = active,
        parent_env = where
      )
    },
    "S7" = S7::new_class(name, properties = gbm_s7_properties(), validator = gbm_s7_validator),
    "R8" = {
      d <- r8_declarations()[["GBM"]]
      d[[2L]] <- name
      eval(d, envir = where)
    },
    stop("unknown system ", system)
  )
}
