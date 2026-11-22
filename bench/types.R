# types.R
# ::r8 bench::
# 2026- EDG rtemis.org

# The benchmark types, Wide20, GBM, Resampler, and GBMRun, in each system:
# S3, S4, Reference Classes (RC), R6, S7, R8 with R-native values, and R8
# with Rust-native values (the package r8benchR, for Wide20 and GBM).
# `declare_system(system, where)` declares a system's types in `where`, the
# namespace of the system's package (syspkg.R) or the global environment,
# and returns the expressions the rows time, each reading `x` and `xw`, GBM
# values, `w`, a Wide20 value, and `args`, the arguments of a GBM.
#
# Every system checks the same fields with the same checks (checks.R) and
# signals in its idiom. A write comes in three variants: the system's idiom;
# the changed field and the rules, in every system; and every field checked
# again, in every system. RC and R6 values change in place; their
# `write_keep` copies first, which keeps the original as the other systems
# do. Dispatch calls a method with a constant body; RC and R6 call a method
# of the object.
# spec: R8/class-system#benchmark

# A package of the benchmark holds checks.R itself, collated before this
# file.
if (!exists("check_gbm")) source(file.path(Sys.getenv("R8_BENCH_DIR", "bench"), "checks.R"))

SYSTEMS <- c("S3", "S4", "RC", "R6", "S7", "R8", "R8 Rust")

# A function of the formals `formals` and the body `body`, enclosed by
# `env`.
fn <- function(formals, body, env) {
  f <- function() NULL
  formals(f) <- formals
  body(f) <- body
  environment(f) <- env
  f
}

# Encloses in `where` each function of `where` whose enclosure is `frame`,
# the frame of the `declare_*()` call that defined it, so that a
# constructor finds its classes in the namespace that holds them.
enclose <- function(where, frame) {
  for (n in ls(where, all.names = TRUE)) {
    f <- get(n, envir = where, inherits = FALSE)
    if (is.function(f) && identical(environment(f), frame)) {
      environment(f) <- where
      assign(n, f, envir = where)
    }
  }
}

# The formals of the GBM and Wide20 constructors: every field, options and
# Wide20's fields with defaults, as R8's constructors have them.
gbm_formals <- function() {
  f <- stats::setNames(rep(list(quote(expr = )), length(GBM_FIELDS)), GBM_FIELDS)
  f[c("num_class", "early_stopping_rounds", "seed")] <- list(NULL)
  f
}
wide_formals <- function() WIDE_DEFAULTS

# The call `list(a = a, b = b, ...)` of the fields `names`, each wrapped in
# the function `wrap` names, or `wrap(name)` names when `wrap` is a
# function; NULL wraps nothing.
field_list <- function(names, wrap = NULL) {
  args <- lapply(names, function(n) {
    w <- if (is.function(wrap)) wrap(n) else wrap
    if (is.null(w)) as.name(n) else as.call(list(as.name(w), as.name(n)))
  })
  as.call(c(quote(list), stats::setNames(args, names)))
}

GBM_INT <- c("n_trees", "max_depth", "num_class", "early_stopping_rounds", "seed", "feature_idx")
GBM_DBL <- c("learning_rate", "subsample", "colsample", "weights")
gbm_wrap <- function(n) if (n %in% GBM_INT) "as_int" else if (n %in% GBM_DBL) "as_dbl"

# The construction every row times: a call with every argument supplied.
CONSTRUCT <- quote(GBM(
  learning_rate = 0.1, subsample = 0.8, colsample = 0.9, n_trees = 500L, max_depth = 6L,
  objective = "multiclass", num_class = 3L, early_stopping_rounds = 20L, seed = 2026L,
  verbose = FALSE, weights = args$weights, feature_idx = args$feature_idx
))

# %% S3 ----
declare_s3 <- function(where = globalenv()) {
  where$GBM <- fn(gbm_formals(), bquote(structure(signal(check_gbm(.(field_list(GBM_FIELDS)))), class = "s3_GBM")), where)
  where$Wide20 <- fn(wide_formals(), bquote(structure(signal(check_wide(.(field_list(WIDE_FIELDS)))), class = "s3_Wide20")), where)
  where$Resampler <- function(...) structure(signal(check_resampler(fields_of(RESAMPLER_FIELDS, list(...)))), class = "s3_Resampler")
  where$GBMRun <- function(...) {
    f <- check_gbm_run(fields_of(GBM_RUN_FIELDS, list(...)), "s3_GBM", "s3_Resampler", function(r) r$n)
    structure(signal(f), class = "s3_GBMRun")
  }
  # The idiom: a `$<-` method that checks the value whole again.
  where$`$<-.s3_GBM` <- function(x, name, value) {
    f <- unclass(x)
    f[name] <- list(value)
    structure(signal(check_gbm(f)), class = "s3_GBM")
  }
  where$set_fr <- function(x, name, value) {
    structure(signal(check_gbm_field(unclass(x), name, value)), class = "s3_GBM")
  }
  where$set_wide_fr <- function(x, name, value) {
    structure(signal(check_wide_field(unclass(x), name, value)), class = "s3_Wide20")
  }
  where$validate_gbm <- function(x) invisible(signal(check_gbm(unclass(x))))
  where$complexity <- function(x) UseMethod("complexity")
  where$complexity.s3_GBM <- function(x) 1L
  enclose(where, environment())
  list(
    construct = CONSTRUCT,
    construct_wide = quote(Wide20(f1 = 0.5)),
    construct_wide_defaults = quote(Wide20()),
    read = quote(x$n_trees),
    write_idiom = quote({ y <- xw; y$n_trees <- 200L; y }),
    write_field_rules = quote(set_fr(xw, "n_trees", 200L)),
    write_all = quote({ y <- xw; y$n_trees <- 200L; y }),
    write_wide = quote(set_wide_fr(w, "f1", 0.5)),
    write_fr_fn = function(x, name, value) set_fr(x, name, value),
    validate = quote(validate_gbm(x)),
    dispatch = quote(complexity(x))
  )
}

# %% S4 ----
declare_s4 <- function(where = globalenv()) {
  methods::setClassUnion("intOrNULL", c("integer", "NULL"), where = where)
  slots_of <- function(object, names) stats::setNames(lapply(names, function(n) methods::slot(object, n)), names)
  validity <- function(check, names) {
    function(object) {
      r <- check(slots_of(object, names))
      if (inherits(r, "bench_problem")) unclass(r) else TRUE
    }
  }
  methods::setClass(
    "s4_Wide20",
    slots = stats::setNames(rep("numeric", 20L), WIDE_FIELDS),
    validity = validity(check_wide, WIDE_FIELDS),
    where = where
  )
  methods::setClass(
    "s4_GBM",
    slots = GBM_S4_SLOTS,
    validity = validity(check_gbm, GBM_FIELDS),
    where = where
  )
  methods::setClass(
    "s4_Resampler",
    slots = c(type = "character", n = "integer", train_p = "numeric", stratify = "logical"),
    validity = validity(check_resampler, RESAMPLER_FIELDS),
    where = where
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
    where = where
  )
  # A constructor converts numbers as R8 does; new() checks the slot
  # classes and runs the validity method.
  new_call <- function(class, names, wrap) {
    as.call(c(list(quote(methods::new), class), as.list(field_list(names, wrap))[-1L]))
  }
  where$GBM <- fn(gbm_formals(), new_call("s4_GBM", GBM_FIELDS, gbm_wrap), where)
  where$Wide20 <- fn(wide_formals(), new_call("s4_Wide20", WIDE_FIELDS, "as_dbl"), where)
  where$Resampler <- function(...) do.call(methods::new, c(list("s4_Resampler"), convert_resampler(list(...))))
  where$GBMRun <- function(...) do.call(methods::new, c(list("s4_GBMRun"), list(...)))
  # The changed field and the rules, as a function; the idiom checks the
  # whole object with validObject().
  where$set_fr <- function(x, name, value) {
    v <- signal(GBM_CHECKS[[name]](value))
    methods::slot(x, name) <- v
    signal(rules_gbm(list(
      early_stopping_rounds = x@early_stopping_rounds,
      n_trees = x@n_trees,
      objective = x@objective,
      num_class = x@num_class
    )))
    x
  }
  where$set_wide_fr <- function(x, name, value) {
    methods::slot(x, name) <- signal(WIDE_CHECKS[[name]](value))
    x
  }
  methods::setGeneric("complexity", function(x) standardGeneric("complexity"), where = where)
  methods::setMethod("complexity", "s4_GBM", function(x) 1L, where = where)
  enclose(where, environment())
  list(
    construct = CONSTRUCT,
    construct_wide = quote(Wide20(f1 = 0.5)),
    construct_wide_defaults = quote(Wide20()),
    read = quote(x@n_trees),
    write_idiom = quote({ y <- xw; y@n_trees <- 200L; methods::validObject(y); y }),
    write_field_rules = quote(set_fr(xw, "n_trees", 200L)),
    write_all = quote({ y <- xw; y@n_trees <- 200L; methods::validObject(y); y }),
    write_wide = quote(set_wide_fr(w, "f1", 0.5)),
    write_fr_fn = function(x, name, value) set_fr(x, name, value),
    validate = quote(methods::validObject(x)),
    dispatch = quote(complexity(x))
  )
}

# %% Reference Classes and R6 ----
# RC and R6 rebind a method's enclosing environment to the object, so a
# method reaches only fields, `self` or `.self`, and the global environment
# or the package's namespace; the checks are named, not captured.
check_rc_gbm_run <- function(f) check_gbm_run(f, "rc_GBM", "rc_Resampler", function(r) r$n)
check_r6_gbm_run <- function(f) check_gbm_run(f, "r6_GBM", "r6_Resampler", function(r) r$n)

RC_GBM_FIELDS <- list(
  learning_rate = "numeric", subsample = "numeric", colsample = "numeric",
  n_trees = "integer", max_depth = "integer", objective = "character",
  num_class = "ANY", early_stopping_rounds = "ANY", seed = "ANY",
  verbose = "logical", weights = "numeric", feature_idx = "integer"
)

declare_rc <- function(where = globalenv()) {
  rc_class <- function(name, fields, check, names, convert = NULL, extra = list()) {
    conv <- if (is.null(convert)) quote(f) else bquote(.(convert)(f))
    methods::setRefClass(
      name,
      fields = fields,
      methods = c(
        list(
          # copy() calls initialize() without arguments, then copies the
          # fields.
          initialize = eval(bquote(function(...) {
            if (...length() == 0L) {
              return(invisible(.self))
            }
            f <- fields_of(.(names), list(...))
            f <- signal(.(check)(.(conv)))
            for (n in names(f)) field(n, f[[n]])
            invisible(.self)
          })),
          values = eval(bquote(function() {
            stats::setNames(lapply(.(names), function(n) field(n)), .(names))
          })),
          validate = eval(bquote(function() invisible(signal(.(check)(values())))))
        ),
        extra
      ),
      where = where
    )
  }
  where$rc_Wide20 <- rc_class(
    "rc_Wide20",
    stats::setNames(as.list(rep("numeric", 20L)), WIDE_FIELDS),
    quote(check_wide), quote(WIDE_FIELDS), quote(convert_wide),
    list(set_fr = function(name, value) {
      field(name, signal(WIDE_CHECKS[[name]](value)))
      invisible(.self)
    })
  )
  where$Wide20 <- fn(wide_formals(), as.call(c(quote(rc_Wide20$new), as.list(field_list(WIDE_FIELDS))[-1L])), where)
  where$rc_GBM <- rc_class(
    "rc_GBM", RC_GBM_FIELDS, quote(check_gbm), quote(GBM_FIELDS), quote(convert_gbm),
    list(
      # The idiom: a setter that checks the object whole again.
      set = function(name, value) {
        f <- values()
        f[name] <- list(value)
        f <- signal(check_gbm(convert_gbm(f)))
        field(name, f[[name]])
        invisible(.self)
      },
      set_fr = function(name, value) {
        v <- signal(GBM_CHECKS[[name]](value))
        r <- list(early_stopping_rounds = early_stopping_rounds, n_trees = n_trees, objective = objective, num_class = num_class)
        if (name %in% names(r)) r[name] <- list(v)
        signal(rules_gbm(r))
        field(name, v)
        invisible(.self)
      },
      complexity = function() 1L
    )
  )
  where$GBM <- fn(gbm_formals(), as.call(c(quote(rc_GBM$new), as.list(field_list(GBM_FIELDS))[-1L])), where)
  where$rc_Resampler <- rc_class(
    "rc_Resampler",
    list(type = "character", n = "integer", train_p = "numeric", stratify = "logical"),
    quote(check_resampler), quote(RESAMPLER_FIELDS), quote(convert_resampler)
  )
  where$Resampler <- function(...) rc_Resampler$new(...)
  where$rc_GBMRun <- rc_class(
    "rc_GBMRun",
    list(gbm = "ANY", resampler = "ANY", created = "Date", outcome = "factor", importance = "matrix", folds = "list"),
    quote(check_rc_gbm_run), quote(GBM_RUN_FIELDS)
  )
  where$GBMRun <- function(...) rc_GBMRun$new(...)
  enclose(where, environment())
  list(
    construct = CONSTRUCT,
    construct_wide = quote(Wide20(f1 = 0.5)),
    construct_wide_defaults = quote(Wide20()),
    read = quote(x$n_trees),
    write_idiom = quote(xw$set("n_trees", 200L)),
    write_field_rules = quote(xw$set_fr("n_trees", 200L)),
    write_all = quote(xw$set("n_trees", 200L)),
    write_keep = quote({ y <- xw$copy(); y$set_fr("n_trees", 200L); y }),
    write_wide = quote(w$set_fr("f1", 0.5)),
    write_fr_fn = function(x, name, value) x$copy()$set_fr(name, value),
    validate = quote(x$validate()),
    dispatch = quote(x$complexity()),
    dispatch_label = "method call"
  )
}

declare_r6 <- function(where = globalenv()) {
  # A field is an active binding over a private list of the checked fields;
  # the idiom, assigning it, checks the fields whole again.
  r6_class <- function(name, field_names, check, names, extra = list()) {
    active <- lapply(field_names, function(n) {
      eval(bquote(function(value) {
        if (missing(value)) {
          return(private$f[[.(n)]])
        }
        f <- private$f
        f[.(n)] <- list(value)
        private$f <- signal(.(check)(f))
      }))
    })
    names(active) <- field_names
    R6::R6Class(
      name,
      public = c(
        list(
          initialize = eval(bquote(function(...) {
            private$f <- signal(.(check)(fields_of(.(names), list(...))))
          })),
          validate = eval(bquote(function() invisible(signal(.(check)(private$f)))))
        ),
        extra
      ),
      private = list(f = NULL),
      active = active,
      parent_env = where
    )
  }
  where$r6_Wide20 <- r6_class(
    "r6_Wide20", WIDE_FIELDS, quote(check_wide), quote(WIDE_FIELDS),
    list(set_fr = function(name, value) {
      private$f <- signal(check_wide_field(private$f, name, value))
      invisible(self)
    })
  )
  where$Wide20 <- fn(wide_formals(), as.call(c(quote(r6_Wide20$new), as.list(field_list(WIDE_FIELDS))[-1L])), where)
  where$r6_GBM <- r6_class(
    "r6_GBM", GBM_FIELDS, quote(check_gbm), quote(GBM_FIELDS),
    list(
      set_fr = function(name, value) {
        private$f <- signal(check_gbm_field(private$f, name, value))
        invisible(self)
      },
      complexity = function() 1L
    )
  )
  where$GBM <- fn(gbm_formals(), as.call(c(quote(r6_GBM$new), as.list(field_list(GBM_FIELDS))[-1L])), where)
  where$r6_Resampler <- r6_class("r6_Resampler", RESAMPLER_FIELDS, quote(check_resampler), quote(RESAMPLER_FIELDS))
  where$Resampler <- function(...) r6_Resampler$new(...)
  where$r6_GBMRun <- r6_class("r6_GBMRun", GBM_RUN_FIELDS, quote(check_r6_gbm_run), quote(GBM_RUN_FIELDS))
  where$GBMRun <- function(...) r6_GBMRun$new(...)
  enclose(where, environment())
  list(
    construct = CONSTRUCT,
    construct_wide = quote(Wide20(f1 = 0.5)),
    construct_wide_defaults = quote(Wide20()),
    read = quote(x$n_trees),
    write_idiom = quote(xw$n_trees <- 200L),
    write_field_rules = quote(xw$set_fr("n_trees", 200L)),
    write_all = quote(xw$n_trees <- 200L),
    write_keep = quote({ y <- xw$clone(); y$set_fr("n_trees", 200L); y }),
    write_wide = quote(w$set_fr("f1", 0.5)),
    write_fr_fn = function(x, name, value) x$clone()$set_fr(name, value),
    validate = quote(x$validate()),
    dispatch = quote(x$complexity()),
    dispatch_label = "method call"
  )
}

# %% S7 ----
# A property's setter converts a number as R8 does, its validator runs the
# field's check, and the class validator runs the rules.
s7_property <- function(name, class, check, convert = NULL, default = NULL) {
  validator <- function(value) {
    v <- check(value)
    if (inherits(v, "bench_problem")) unclass(v)
  }
  if (is.null(convert)) {
    return(S7::new_property(class, validator = validator, default = default))
  }
  S7::new_property(
    class,
    validator = validator,
    default = default,
    setter = function(self, value) {
      S7::prop(self, name, check = FALSE) <- convert(value)
      self
    }
  )
}

s7_gbm_properties <- function() {
  int_or_null <- S7::new_union(NULL, S7::class_integer)
  classes <- list(
    learning_rate = S7::class_double, subsample = S7::class_double, colsample = S7::class_double,
    n_trees = S7::class_integer, max_depth = S7::class_integer, objective = S7::class_character,
    num_class = int_or_null, early_stopping_rounds = int_or_null, seed = int_or_null,
    verbose = S7::class_logical, weights = S7::class_double, feature_idx = S7::class_integer
  )
  out <- lapply(GBM_FIELDS, function(n) {
    w <- gbm_wrap(n)
    s7_property(n, classes[[n]], GBM_CHECKS[[n]], switch(w %||% "", as_int = as_int, as_dbl = as_dbl))
  })
  stats::setNames(out, GBM_FIELDS)
}

s7_gbm_validator <- function(self) {
  r <- rules_gbm(list(
    early_stopping_rounds = self@early_stopping_rounds,
    n_trees = self@n_trees,
    objective = self@objective,
    num_class = self@num_class
  ))
  if (!is.null(r)) unclass(r)
}

declare_s7 <- function(where = globalenv()) {
  where$s7_GBM <- S7::new_class("s7_GBM", properties = s7_gbm_properties(), validator = s7_gbm_validator)
  where$GBM <- fn(gbm_formals(), as.call(c(quote(s7_GBM), as.list(field_list(GBM_FIELDS))[-1L])), where)
  wide_props <- lapply(WIDE_FIELDS, function(n) s7_property(n, S7::class_double, WIDE_CHECKS[[n]], as_dbl, 0.1))
  where$s7_Wide20 <- S7::new_class("s7_Wide20", properties = stats::setNames(wide_props, WIDE_FIELDS))
  where$Wide20 <- fn(wide_formals(), as.call(c(quote(s7_Wide20), as.list(field_list(WIDE_FIELDS))[-1L])), where)
  where$s7_Resampler <- S7::new_class(
    "s7_Resampler",
    properties = list(
      type = s7_property("type", S7::class_character, RESAMPLER_CHECKS$type),
      n = s7_property("n", S7::class_integer, RESAMPLER_CHECKS$n, as_int),
      train_p = s7_property("train_p", S7::class_double, RESAMPLER_CHECKS$train_p, as_dbl),
      stratify = s7_property("stratify", S7::class_logical, RESAMPLER_CHECKS$stratify)
    )
  )
  where$Resampler <- function(...) s7_Resampler(...)
  where$s7_GBMRun <- S7::new_class(
    "s7_GBMRun",
    properties = list(
      gbm = where$s7_GBM,
      resampler = where$s7_Resampler,
      created = s7_property("created", S7::class_Date, function(v) chk_date(v, "created")),
      outcome = s7_property("outcome", S7::class_factor, function(v) chk_factor(v, "outcome", c("control", "case"))),
      importance = s7_property("importance", S7::class_double, function(v) chk_matrix(v, "importance", 2L)),
      # A package prefixes an S7 class's name with its own; S7_inherits()
      # tests the class itself.
      folds = s7_property("folds", S7::class_list, function(v) {
        if (!all(vapply(v, function(e) S7::S7_inherits(e, where$s7_Resampler), TRUE))) {
          return(problem("folds", "holds an element that is not a s7_Resampler"))
        }
        v
      })
    ),
    validator = function(self) {
      r <- rules_gbm_run(self@folds, self@resampler@n)
      if (!is.null(r)) unclass(r)
    }
  )
  where$GBMRun <- function(...) s7_GBMRun(...)
  where$complexity <- S7::new_generic("complexity", "x")
  S7::method(where$complexity, where$s7_GBM) <- function(x) 1L
  enclose(where, environment())
  list(
    construct = CONSTRUCT,
    construct_wide = quote(Wide20(f1 = 0.5)),
    construct_wide_defaults = quote(Wide20()),
    read = quote(x@n_trees),
    write_idiom = quote({ y <- xw; y@n_trees <- 200L; y }),
    write_field_rules = quote({ y <- xw; y@n_trees <- 200L; y }),
    write_all = quote({ y <- xw; y@n_trees <- 200L; S7::validate(y); y }),
    write_wide = quote({ v <- w; v@f1 <- 0.5; v }),
    write_fr_fn = function(x, name, value) {
      S7::prop(x, name) <- value
      x
    },
    validate = quote(S7::validate(x)),
    dispatch = quote(complexity(x))
  )
}

# %% R8 ----
# The R8 declarations. With `native = TRUE`, Wide20 and GBM are the
# declarations of r8benchR, whose values Rust holds; the declarations here
# equal them (checked before timing).
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
          early_stopping_rounds < n_trees,
          when = !is.null(early_stopping_rounds),
          message = "early_stopping_rounds is below n_trees.",
          id = "early_stop_below_trees"
        ),
        R8::rule(
          (objective == "multiclass") == !is.null(num_class),
          message = "num_class is present exactly when the objective is multiclass.",
          id = "num_class_iff_multiclass"
        )
      )
    ))
  )
}

declare_r8 <- function(where = globalenv(), native = FALSE) {
  if (native) {
    where$Wide20 <- getExportedValue("r8benchR", "Wide20")
    where$GBM <- getExportedValue("r8benchR", "GBM")
    where$complexity <- getExportedValue("r8benchR", "complexity")
  } else {
    for (n in names(r8_declarations())) assign(n, eval(r8_declarations()[[n]], envir = where), envir = where)
    where$Complexity <- R8::new_trait(
      "Complexity",
      methods = list(complexity = R8::trait_method(function(x) NULL))
    )
    where$complexity <- where$Complexity@complexity
    R8::impl(where$Complexity, where$GBM, methods = list(complexity = function(x) 1L))
    where$Resampler <- R8::new_struct(
      "Resampler",
      fields = list(
        type = R8::type_character(pattern = "^(kfold|bootstrap|stratsub)$"),
        n = R8::type_integer(min = 1L),
        train_p = R8::type_double(exclusive_min = 0, exclusive_max = 1),
        stratify = R8::type_logical()
      )
    )
    where$GBMRun <- R8::new_struct(
      "GBMRun",
      fields = list(
        gbm = where$GBM,
        resampler = where$Resampler,
        created = R8::type_date(),
        outcome = R8::type_factor(levels = c("control", "case")),
        importance = R8::type_matrix(R8::type_double(), ncol = 2L),
        folds = R8::vec(where$Resampler)
      ),
      rules = list(
        R8::rule(length(folds) == resampler$n, message = "There is one fold per resample.", id = "folds_per_resample")
      )
    )
  }
  enclose(where, environment())
  list(
    construct = CONSTRUCT,
    construct_wide = quote(Wide20(f1 = 0.5)),
    construct_wide_defaults = quote(Wide20()),
    read = quote(x$n_trees),
    write_idiom = quote({ y <- xw; y$n_trees <- 200L; y }),
    write_field_rules = quote({ y <- xw; y$n_trees <- 200L; y }),
    write_all = quote({ y <- xw; y$n_trees <- 200L; R8::r8_validate(y); y }),
    write_wide = quote({ v <- w; v$f1 <- 0.5; v }),
    write_fr_fn = function(x, name, value) {
      x[[name]] <- value
      x
    },
    validate = quote(R8::r8_validate(x)),
    dispatch = quote(complexity(x))
  )
}

# %% declare_system ----
declare_system <- function(system, where = globalenv()) {
  switch(
    system,
    "S3" = declare_s3(where),
    "S4" = declare_s4(where),
    "RC" = declare_rc(where),
    "R6" = declare_r6(where),
    "S7" = declare_s7(where),
    "R8" = declare_r8(where, native = FALSE),
    "R8 Rust" = declare_r8(where, native = TRUE),
    stop("unknown system ", system)
  )
}

# The arguments of a valid GBM with `n` weights and feature indices, held in
# memory: a compact sequence such as seq_len(n) is ALTREP, which R's min()
# and anyNA() answer without a pass; `compact = TRUE` keeps it.
gbm_args <- function(n = 10L, compact = FALSE) {
  idx <- seq_len(n)
  if (!compact) idx <- idx + 0L
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
    feature_idx = idx
  )
}

# %% gbm_type ----
# A GBM type named `name` in `system`, created in the environment `where`:
# the rows of memory per type and of a namespace of 200 types declare many.
GBM_S4_SLOTS <- c(
  learning_rate = "numeric", subsample = "numeric", colsample = "numeric",
  n_trees = "integer", max_depth = "integer", objective = "character",
  num_class = "intOrNULL", early_stopping_rounds = "intOrNULL", seed = "intOrNULL",
  verbose = "logical", weights = "numeric", feature_idx = "integer"
)

gbm_type <- function(system, name, where = globalenv()) {
  switch(
    system,
    "S3" = {
      setter <- eval(bquote(function(x, name, value) {
        f <- unclass(x)
        f[name] <- list(value)
        structure(signal(check_gbm(f)), class = .(name))
      }))
      environment(setter) <- where
      assign(paste0("$<-.", name), setter, envir = where)
      fn(gbm_formals(), bquote(structure(signal(check_gbm(.(field_list(GBM_FIELDS)))), class = .(name))), where)
    },
    "S4" = {
      if (!methods::isClass("intOrNULL", where = where)) {
        methods::setClassUnion("intOrNULL", c("integer", "NULL"), where = where)
      }
      methods::setClass(
        name,
        slots = GBM_S4_SLOTS,
        validity = function(object) {
          r <- check_gbm(stats::setNames(lapply(GBM_FIELDS, function(n) methods::slot(object, n)), GBM_FIELDS))
          if (inherits(r, "bench_problem")) unclass(r) else TRUE
        },
        where = where
      )
    },
    "RC" = methods::setRefClass(
      name,
      fields = RC_GBM_FIELDS,
      methods = list(
        initialize = function(...) {
          if (...length() == 0L) {
            return(invisible(.self))
          }
          f <- signal(check_gbm(convert_gbm(fields_of(GBM_FIELDS, list(...)))))
          for (n in names(f)) field(n, f[[n]])
          invisible(.self)
        },
        values = function() stats::setNames(lapply(GBM_FIELDS, function(n) field(n)), GBM_FIELDS),
        validate = function() invisible(signal(check_gbm(values()))),
        complexity = function() 1L
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
          private$f <- signal(check_gbm(f))
        }))
      })
      names(active) <- GBM_FIELDS
      R6::R6Class(
        name,
        public = list(
          initialize = function(...) private$f <- signal(check_gbm(fields_of(GBM_FIELDS, list(...)))),
          validate = function() invisible(signal(check_gbm(private$f))),
          complexity = function() 1L
        ),
        private = list(f = NULL),
        active = active,
        parent_env = where
      )
    },
    "S7" = S7::new_class(name, properties = s7_gbm_properties(), validator = s7_gbm_validator),
    "R8" = {
      d <- r8_declarations()[["GBM"]]
      d[[2L]] <- name
      eval(d, envir = where)
    },
    stop("unknown system ", system)
  )
}

# A valid GBM of the type `ty` of `system`, as each system constructs one.
gbm_value <- function(system, ty, args = gbm_args()) {
  if (system %in% c("RC", "R6")) do.call(ty$new, args) else do.call(ty, args)
}
