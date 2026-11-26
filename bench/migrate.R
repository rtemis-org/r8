# migrate.R
# ::r8 bench::
# 2026- EDG rtemis.org

# The time of a call of a typed function whose body is in R and of the same
# function whose body is in Rust, against the plain R function, over inputs of
# 1 to 10^6 elements: `clip()`, a vectorized body (`pmin(pmax(x, lo), hi)`),
# and `level()`, a body whose iterations depend on each other (a running sum
# held between `lo` and `hi`). The bodies in Rust are those of the test package
# r8pkgR, measured when the library holds it. A second table gives the parts of
# the fixed cost of a call: a vector parameter, a rule, and a vector result.
# Each row is the minimum of `rounds` rounds of a byte-compiled loop, all in one
# process.
# Usage: Rscript bench/migrate.R lib [rounds], with the release build of R8, and
# r8pkgR for the Rust rows, installed in the library `lib`.
# spec: R8/class-system#typed-functions

arg <- commandArgs(trailingOnly = TRUE)
if (length(arg) == 0L) {
  stop("Usage: Rscript bench/migrate.R lib [rounds]")
}
.libPaths(c(arg[[1L]], .libPaths()))
rounds <- if (length(arg) > 1L) as.integer(arg[[2L]]) else 7L
suppressPackageStartupMessages(library(R8))
options(r8.namespace = "benchmigrate")
cf <- compiler::cmpfun

clip_body <- quote(pmin(pmax(x, lo), hi))
level_body <- quote({
  y <- numeric(length(x))
  s <- 0
  for (i in seq_along(x)) {
    s <- s + x[[i]]
    if (s < lo) s <- lo else if (s > hi) s <- hi
    y[[i]] <- s
  }
  y
})
plain <- function(body) {
  cf(eval(call("function", as.pairlist(alist(x = , lo = , hi = )), body)))
}
typed <- function(name, body) {
  fn <- eval(call(
    "function",
    as.pairlist(alist(
      x = arg_vec(type_double()),
      lo = arg_double(),
      hi = arg_double()
    )),
    body
  ))
  cf(new_function(
    name,
    fn,
    rules = list(rule(lo <= hi)),
    return_type = vec(type_double())
  ))
}
fns <- list(
  clip = list(
    "plain R" = plain(clip_body),
    "R8, body in R" = typed("clip", clip_body)
  ),
  level = list(
    "plain R" = plain(level_body),
    "R8, body in R" = typed("level", level_body)
  )
)
rust <- requireNamespace("r8pkgR", quietly = TRUE) &&
  all(c("clip", "level") %in% getNamespaceExports("r8pkgR"))
if (rust) {
  fns$clip[["R8, body in Rust"]] <- getExportedValue("r8pkgR", "clip")
  fns$level[["R8, body in Rust"]] <- getExportedValue("r8pkgR", "level")
}

loop <- cf(function(f, x, lo, hi, k) {
  for (i in seq_len(k)) {
    f(x, lo, hi)
  }
})
per_call <- function(f, x, lo, hi, k) {
  t0 <- Sys.time()
  loop(f, x, lo, hi, k)
  as.numeric(Sys.time() - t0, units = "secs") / k * 1e9
}
# The number of calls that a round of about 50 ms holds.
calls <- function(f, x, lo, hi) {
  t <- per_call(f, x, lo, hi, 3L)
  max(3L, as.integer(5e7 / max(t, 1)))
}

set.seed(2026)
sizes <- 10^(0:6)
bounds <- list(clip = c(-1, 1), level = c(-3, 3))
rows <- list()
for (name in names(fns)) {
  lo <- bounds[[name]][[1L]]
  hi <- bounds[[name]][[2L]]
  for (n in sizes) {
    x <- stats::rnorm(n)
    want <- fns[[name]][["plain R"]](x, lo, hi)
    for (how in names(fns[[name]])) {
      f <- fns[[name]][[how]]
      stopifnot(identical(f(x, lo, hi), want))
      k <- calls(f, x, lo, hi)
      ns <- min(replicate(rounds, per_call(f, x, lo, hi, k)))
      rows[[length(rows) + 1L]] <- data.frame(
        fn = name,
        n = n,
        how = how,
        us = signif(ns / 1e3, 3)
      )
    }
  }
}
out <- do.call(rbind, rows)
wide <- reshape(
  out,
  idvar = c("fn", "n"),
  timevar = "how",
  direction = "wide"
)
names(wide) <- sub("^us\\.", "", names(wide))
cat("Time of a call, in microseconds, the minimum of", rounds, "rounds\n")
print(
  format(wide, big.mark = ",", scientific = FALSE, drop0trailing = TRUE),
  row.names = FALSE
)

# The parts of the fixed cost of a call, in nanoseconds beyond the plain
# function, each argument and the result of one element.
part <- function(name, x_type, rules = list(), return_type = NULL) {
  fn <- eval(call(
    "function",
    as.pairlist(alist(x = , lo = arg_double(), hi = arg_double())),
    quote(x)
  ))
  formals(fn)$x <- x_type
  cf(new_function(name, fn, rules = rules, return_type = return_type))
}
parts <- list(
  "a double, two doubles" = part("p_scalar", quote(arg_double())),
  "a vector, two doubles" = part("p_vec", quote(arg_vec(type_double()))),
  "a double, two doubles, a rule" = part(
    "p_scalar_rule",
    quote(arg_double()),
    rules = list(rule(lo <= hi))
  ),
  "a vector, two doubles, a rule" = part(
    "p_vec_rule",
    quote(arg_vec(type_double())),
    rules = list(rule(lo <= hi))
  ),
  "a double, two doubles, a double result" = part(
    "p_scalar_out",
    quote(arg_double()),
    return_type = type_double()
  ),
  "a vector, two doubles, a vector result" = part(
    "p_vec_out",
    quote(arg_vec(type_double())),
    return_type = vec(type_double())
  )
)
base <- cf(function(x, lo, hi) x)
k <- 2e5
for (f in c(list(base), parts)) {
  per_call(f, 0.5, -1, 1, 1e3)
}
base_ns <- min(replicate(rounds, per_call(base, 0.5, -1, 1, k)))
fixed <- data.frame(
  check = names(parts),
  beyond_ns = vapply(
    parts,
    function(f) round(min(replicate(rounds, per_call(f, 0.5, -1, 1, k))) - base_ns),
    0
  ),
  row.names = NULL
)
cat("\nThe checks of a call, in nanoseconds beyond the plain function\n")
print(fixed, row.names = FALSE)
