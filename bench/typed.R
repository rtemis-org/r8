# typed.R
# ::r8 bench::
# 2026- EDG rtemis.org

# The cost of the checks of a typed generic's call, and of a typed function's
# (new_function()), against the same checks in a function: hand-written, and with
# checkmate's assert_*() and qassert(); and against a struct constructor with the
# same fields. Each row reports the time
# of a call and the time beyond the same call without checks, the minimum of
# `rounds` rounds of a byte-compiled loop, all in one process. Usage:
# Rscript bench/typed.R [rounds], with the release build of R8 installed.
# spec: R8/class-system#checks-at-a-call

suppressPackageStartupMessages(library(R8))
options(r8.namespace = "benchtyped")
arg <- commandArgs(trailingOnly = TRUE)
rounds <- if (length(arg) > 0L) as.integer(arg[[1L]]) else 11L

A <- new_struct("A", fields = list(a = type_double()))
Cfg <- new_struct("Cfg", fields = list(k = type_integer()))
Cfg2 <- new_struct("Cfg2", extends = Cfg)
Tunable <- new_trait("Tunable", methods = list(tune = trait_method(params(x))))
impl(Tunable, Cfg, methods = list(tune = function(x) 1))
Args <- new_struct(
  "Args",
  fields = list(n = type_integer(min = 0L), tol = type_double(min = 0))
)

typed_generic <- function(name, p, fn, return_type = NULL) {
  g <- generic(name, p, return_type = return_type)
  implement(g, A) <- fn
  compiler::cmpfun(g)
}
one <- function(x, n, tol) 1
g_none <- typed_generic("g_none", params(x, n, tol), one)
g_scalar <- typed_generic(
  "g_scalar",
  params(x, n = R8::arg_integer(min = 0L), tol = R8::arg_double(min = 0)),
  one
)
g_struct <- typed_generic("g_struct", params(x, n = R8::arg(Cfg), tol), one)
g_dyn <- typed_generic("g_dyn", params(x, n = R8::arg(dyn(Tunable)), tol), one)
g_sub <- typed_generic("g_sub", params(x, n = R8::arg(subtype(Cfg)), tol), one)
g_vec <- typed_generic("g_vec", params(x, n = R8::arg_vec(type_double(min = 0)), tol), one)
g_out <- typed_generic("g_out", params(x, n, tol), one, type_double())
t_scalar <- compiler::cmpfun(new_function(
  "t_scalar",
  function(x, n = arg_integer(min = 0L), tol = arg_double(min = 0)) 1
))
t_struct <- compiler::cmpfun(new_function("t_struct", function(x, n = arg(Cfg), tol) 1))
t_out <- compiler::cmpfun(new_function(
  "t_out",
  function(x, n, tol) 1,
  return_type = type_double()
))
t_rule <- compiler::cmpfun(new_function(
  "t_rule",
  function(x, n = arg_integer(min = 0L), tol = arg_double(min = 0)) 1,
  rules = list(rule(n >= tol))
))

# The same checks in a function: `f_none` is their base.
f_none <- compiler::cmpfun(one)
f_hand <- compiler::cmpfun(function(x, n, tol) {
  if (!is.integer(n) || length(n) != 1L || !is.null(attributes(n)) || is.na(n) || n < 0L) {
    stop("`n` must be an integer of at least 0.")
  }
  if (!is.double(tol) || length(tol) != 1L || !is.null(attributes(tol)) || !is.finite(tol) || tol < 0) {
    stop("`tol` must be a number of at least 0.")
  }
  1
})
f_assert <- compiler::cmpfun(function(x, n, tol) {
  checkmate::assert_int(n, lower = 0L)
  checkmate::assert_number(tol, lower = 0, finite = TRUE)
  1
})
f_qassert <- compiler::cmpfun(function(x, n, tol) {
  checkmate::qassert(n, "I1[0,)")
  checkmate::qassert(tol, "N1[0,)")
  1
})
f_hand_class <- compiler::cmpfun(function(x, n, tol) {
  if (!inherits(n, "benchtyped::Cfg")) {
    stop("`n` must be a Cfg.")
  }
  1
})
f_assert_class <- compiler::cmpfun(function(x, n, tol) {
  checkmate::assert_class(n, "benchtyped::Cfg")
  1
})
f_hand_vec <- compiler::cmpfun(function(x, n, tol) {
  if (!is.double(n) || !is.null(attributes(n)) || !all(is.finite(n)) || any(n < 0)) {
    stop("`n` must be finite doubles of at least 0.")
  }
  1
})
f_assert_vec <- compiler::cmpfun(function(x, n, tol) {
  checkmate::assert_numeric(n, lower = 0, finite = TRUE, any.missing = FALSE)
  1
})
f_hand_out <- compiler::cmpfun(function(x, n, tol) {
  out <- one(x, n, tol)
  if (!is.double(out) || length(out) != 1L || !is.null(attributes(out)) || !is.finite(out)) {
    stop("The result must be a number.")
  }
  out
})
f_assert_out <- compiler::cmpfun(function(x, n, tol) {
  out <- one(x, n, tol)
  checkmate::assert_number(out, finite = TRUE)
  out
})
f_out <- compiler::cmpfun(function(x, n, tol) one(x, n, tol))

loop <- compiler::cmpfun(function(f, x, n, tol, k) {
  for (i in seq_len(k)) {
    f(x, n, tol)
  }
})
loop_args <- compiler::cmpfun(function(k) {
  for (i in seq_len(k)) {
    Args(n = 1L, tol = 1)
  }
})
time <- function(expr, k) {
  t0 <- Sys.time()
  expr
  as.numeric(Sys.time() - t0, units = "secs") / k * 1e9
}

a <- A(a = 1)
cfg <- Cfg(k = 1L)
cfg2 <- Cfg2(k = 1L)
set.seed(2026)
v <- stats::runif(1e6)
row <- function(check, how, f, base, n, k) {
  list(check = check, how = how, f = f, base = base, n = n, k = k)
}
rows <- list(
  row("integer and double", "R8", g_scalar, g_none, 1L, 1e5),
  row("integer and double", "hand-written", f_hand, f_none, 1L, 1e5),
  row("integer and double", "checkmate qassert()", f_qassert, f_none, 1L, 1e5),
  row("integer and double", "checkmate assert_*()", f_assert, f_none, 1L, 2e4),
  row("a struct", "R8", g_struct, g_none, cfg, 1e5),
  row("a struct", "hand-written", f_hand_class, f_none, cfg, 1e5),
  row("a struct", "checkmate assert_class()", f_assert_class, f_none, cfg, 2e4),
  row("subtype() of a struct", "R8", g_sub, g_none, cfg2, 1e5),
  row("dyn() of a trait", "R8", g_dyn, g_none, cfg, 1e5),
  row("10^6 doubles", "R8", g_vec, g_none, v, 100),
  row("10^6 doubles", "hand-written", f_hand_vec, f_none, v, 20),
  row("10^6 doubles", "checkmate assert_numeric()", f_assert_vec, f_none, v, 20),
  row("a result", "R8", g_out, g_none, 1L, 1e5),
  row("a result", "hand-written", f_hand_out, f_out, 1L, 1e5),
  row("a result", "checkmate assert_number()", f_assert_out, f_out, 1L, 2e4),
  row("integer and double", "R8 function", t_scalar, f_none, 1L, 1e5),
  row("a struct", "R8 function", t_struct, f_none, cfg, 1e5),
  row("a result", "R8 function", t_out, f_out, 1L, 1e5),
  row("integer, double, and a rule", "R8 function", t_rule, f_none, 1L, 2e4)
)
measure <- function(r) {
  c(time(loop(r$f, a, r$n, 1, r$k), r$k), time(loop(r$base, a, r$n, 1, r$k), r$k))
}
for (r in rows) {
  measure(r)
}
out <- lapply(rows, function(r) {
  m <- replicate(rounds, measure(r))
  data.frame(
    check = r$check,
    how = r$how,
    call_ns = round(min(m[1L, ])),
    beyond_ns = round(min(m[1L, ]) - min(m[2L, ]))
  )
})
invisible(time(loop_args(1e4), 1e4))
out <- c(
  out,
  list(data.frame(
    check = "integer and double",
    how = "R8 struct constructor",
    call_ns = round(min(replicate(rounds, time(loop_args(2e4), 2e4)))),
    beyond_ns = NA
  ))
)
print(do.call(rbind, out), row.names = FALSE)
