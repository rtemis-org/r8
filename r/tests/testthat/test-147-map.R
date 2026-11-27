# test-147-map.R
# ::R8::
# 2026- EDG rtemis.org

# Maps of a typed function over the items of a list or a vector, with constant
# arguments, in the container of the function's return type.
# spec: R8/raven#summaries

refuses <- function(expr, text, class = "r8_value_error") {
  expect_error(expr, text, fixed = TRUE, class = class)
}

declared <- function(expr) {
  op <- options(r8.namespace = "maps")
  on.exit(options(op))
  expr
}

Fold <- declared(new_struct("Fold", fields = list(rows = vec(type_integer()))))
Model <- declared(new_struct(
  "Model",
  fields = list(n = type_integer(), m = type_double())
))
scores <- Raven(site = c("a", "a", "b", "b"), auc = c(0.81, 0.79, 0.85, 0.88))
folds <- list(one = Fold(rows = 1:2), two = Fold(rows = 3:4))
fold_auc <- declared(new_function(
  "fold_auc",
  function(fold = arg(Fold), data = arg(Raven)) mean(data$auc[fold$rows]),
  return_type = type_double()
))
fit <- declared(new_function(
  "fit",
  function(fold = arg(Fold), data = arg(Raven)) {
    Model(n = length(fold$rows), m = mean(data$auc[fold$rows]))
  },
  return_type = Model
))

test_that("a map of a list gives the container of the return type, with its names", {
  aucs <- r8map(folds, fold_auc, args = list(data = scores))
  expect_identical(aucs, c(one = 0.8, two = 0.865))
  models <- r8map(folds, fit, args = list(data = scores))
  expect_type(models, "list")
  expect_identical(names(models), c("one", "two"))
  expect_true(r8equal(models$two, Model(n = 2L, m = 0.865)))
  # An empty list gives an empty container of the type.
  expect_identical(
    r8map(list(), fold_auc, args = list(data = scores)),
    double()
  )
  expect_identical(r8map(list(), fit, args = list(data = scores)), list())
})

test_that("a map of a vector calls the function on each element", {
  sq <- declared(new_function(
    "sq",
    function(v = arg_double(), by = arg_double(default = 1)) v^2 * by,
    return_type = type_double()
  ))
  expect_identical(r8map(c(a = 1, b = 2, c = 3), sq), c(a = 1, b = 4, c = 9))
  expect_identical(r8map(1:3 + 0, sq, args = list(by = 2)), c(2, 8, 18))
  is_long <- declared(new_function(
    "is_long",
    function(s = arg_character()) nchar(s) > 3L,
    return_type = type_logical()
  ))
  expect_identical(r8map(c("ab", "abcd"), is_long), c(FALSE, TRUE))
})

test_that("the tables of groups are items of a list", {
  site_fit <- declared(new_function(
    "site_fit",
    function(d = arg(Raven)) Model(n = nrow(d), m = mean(d$auc)),
    return_type = Model
  ))
  models <- r8map(split(scores, scores$site), site_fit)
  expect_identical(names(models), c("a", "b"))
  expect_identical(models$b$m, 0.865)
  # A table's columns take a function of one value per column.
  refuses(
    r8map(scores, site_fit, by = "site"),
    "r8map(split(x, x[[\"g\"]]), f)"
  )
})

test_that("the arguments of a map are checked before its calls", {
  refuses(
    r8map(folds, fold_auc, args = list(dat = scores)),
    "has no parameter `dat`; the nearest is `data`",
    class = "r8_argument_error"
  )
  refuses(
    r8map(folds, fold_auc, args = list(fold = 1)),
    "takes each item of `x`",
    class = "r8_argument_error"
  )
  refuses(
    r8map(folds, fold_auc, args = list(scores)),
    "`args` is a named list",
    class = "r8_argument_error"
  )
  refuses(
    r8map(folds, fold_auc, columns = "auc"),
    "`columns` and `by` choose the columns and the groups of a table",
    class = "r8_argument_error"
  )
  untyped <- declared(new_function("untyped", function(v = arg_double()) v))
  refuses(r8map(1:2 + 0, untyped), "return_type", class = "r8_argument_error")
  refuses(r8map(folds, function(f) 1), "new_function()")
  refuses(
    r8map(folds, calc(mean(col()))),
    "A calc() maps over the columns of a table"
  )
})

test_that("an error of a call names its item, and the map gives nothing", {
  e <- expect_error(
    r8map(list(one = folds$one, two = 1), fold_auc, args = list(data = scores)),
    class = "r8_value_error"
  )
  expect_match(conditionMessage(e), "of item 2 (`two`)", fixed = TRUE)
  expect_s3_class(e$parent, "r8_value_error")
})

# `expr` with the option `r8.progress` set to `hook`.
with_hook <- function(hook, expr) {
  op <- options(r8.progress = hook)
  on.exit(options(op))
  expr
}

# A hook that records the events of a map's progress.
recorder <- function() {
  log <- new.env(parent = emptyenv())
  log[["events"]] <- list()
  add <- function(...) {
    log[["events"]][[length(log[["events"]]) + 1L]] <- list(...)
  }
  log[["hook"]] <- list(
    begin = function(total, label, kind) {
      add(event = "begin", total = total, label = label, kind = kind)
      "handle"
    },
    update = function(handle, current) {
      add(event = "update", handle = handle, current = current)
    },
    end = function(handle, status) add(event = "end", status = status)
  )
  log
}

slow <- declared(new_function(
  "slow",
  function(v = arg_double()) {
    Sys.sleep(0.02)
    v
  },
  return_type = type_double()
))

test_that("a long map reports its progress to the hook, a short one nothing", {
  log <- recorder()
  expect_identical(
    with_hook(log[["hook"]], r8map(as.double(1:20), slow)),
    as.double(1:20)
  )
  kinds <- vapply(log[["events"]], `[[`, "", "event")
  expect_identical(kinds[[1L]], "begin")
  expect_identical(kinds[[length(kinds)]], "end")
  expect_true("update" %in% kinds)
  begin <- log[["events"]][[1L]]
  expect_identical(
    begin[c("total", "label", "kind")],
    list(total = 20L, label = attr(slow, "r8_function"), kind = "r8map")
  )
  expect_identical(log[["events"]][[length(kinds)]][["status"]], "done")
  current <- unlist(lapply(
    Filter(function(e) e[["event"]] == "update", log[["events"]]),
    `[[`,
    "current"
  ))
  expect_true(all(diff(current) >= 0))
  expect_identical(current[[length(current)]], 20L)
  # A map that ends within 0.2 s calls no hook.
  quick <- recorder()
  with_hook(quick[["hook"]], r8map(c(1, 2, 3), slow))
  expect_length(quick[["events"]], 0L)
})

test_that("a map that fails ends its progress with the error", {
  log <- recorder()
  failing <- declared(new_function(
    "failing",
    function(v = arg_double()) {
      Sys.sleep(0.03)
      if (v > 10) {
        stop("too large")
      }
      v
    },
    return_type = type_double()
  ))
  expect_error(
    with_hook(log[["hook"]], r8map(as.double(1:12), failing)),
    "too large",
    class = "r8_value_error"
  )
  last <- log[["events"]][[length(log[["events"]])]]
  expect_identical(
    last[c("event", "status")],
    list(event = "end", status = "error")
  )
})

test_that("a map of a table's columns counts groups times columns", {
  log <- recorder()
  slow_mean <- declared(new_function(
    "slow_mean",
    function(v = arg_vec(type_double())) {
      Sys.sleep(0.02)
      mean(v)
    },
    return_type = type_double()
  ))
  x <- Raven(g = rep(1:6, each = 2L), a = runif(12L), b = runif(12L))
  with_hook(log[["hook"]], r8map(x, slow_mean, by = "g"))
  expect_identical(log[["events"]][[1L]][["total"]], 12L)
  refuses(
    with_hook(list(begin = 1), r8map(c(1, 2), slow)),
    "`r8.progress` is NULL or a list of the functions",
    class = "r8_argument_error"
  )
})

test_that("rtemis.core draws the progress of a map under its own", {
  skip_if_not_installed("rtemis.core")
  begin <- getExportedValue("rtemis.core", "progress_begin")
  update <- getExportedValue("rtemis.core", "progress_update")
  end <- getExportedValue("rtemis.core", "progress_end")
  hook <- list(
    begin = function(total, label, kind) {
      begin(total, label = label, kind = kind, output_type = "plain")
    },
    update = function(handle, current) update(handle, current = current),
    end = function(handle, status) end(handle, status = status)
  )
  drawn <- capture.output(type = "message", {
    outer <- begin(2L, label = "Outer", output_type = "plain")
    out <- with_hook(hook, r8map(as.double(1:15), slow))
    update(outer)
    end(outer)
  })
  expect_identical(out, as.double(1:15))
  expect_true(any(grepl(
    paste(attr(slow, "r8_function"), "15/15 done"),
    drawn,
    fixed = TRUE
  )))
})
