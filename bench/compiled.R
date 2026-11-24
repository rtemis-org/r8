# compiled.R
# ::r8 bench::
# 2026- EDG rtemis.org

# The time of calls that cross from R into an R package's Rust code through
# R8: compiled methods, values built in Rust, open fields, upcasts, and
# documents, on the types of the test packages r8pkgR and r8pkgS. Each row is
# the minimum of `rounds` rounds of a byte-compiled loop, the empty loop
# subtracted; an R expression that runs no Rust code is the base of the rest.
# Usage, with r8pkgR and r8pkgS installed in the library `lib` against the
# release build of R8: Rscript bench/compiled.R lib [rounds]. Two builds are
# compared by alternating runs against their libraries, one row per process:
# Rscript bench/compiled.R lib rounds row.
# spec: R8/class-system#rust-impls-and-rust-stored-types-rtemis-r8-api

arg <- commandArgs(trailingOnly = TRUE)
if (length(arg) == 0L) {
  stop("Usage: Rscript bench/compiled.R lib [rounds] [row]")
}
.libPaths(c(arg[[1L]], .libPaths()))
rounds <- if (length(arg) > 1L) as.integer(arg[[2L]]) else 9L
only <- if (length(arg) > 2L) arg[[3L]]
suppressPackageStartupMessages({
  library(R8)
  library(r8pkgR)
  library(r8pkgS)
})

iv <- Interval(lo = 0, hi = 4)
band <- Band(lo = 0, hi = 4, label = "b", weights = c(1, 2))
strip <- Strip(lo = 0, hi = 4, label = "s", weights = c(1, 2), number = 7L)
tile <- Tile(lo = 1, hi = 3, number = 2L)
signal <- Signal(name = "a", values = c(3, 4))
square <- Square(side = 2)
kfold <- Resampler@KFold(k = 3L, seed = 1L)
panel <- Panel(
  range = strip,
  inset = strip,
  frame = iv,
  method = kfold,
  figure = square
)
grid <- Grid(cell = iv, method = kfold, extent = band, figure = square)
iv_json <- r8_to_json(iv)
panel_json <- r8_to_json(panel)

# Each row: an expression, and the number of calls of a round.
rows <- list(
  base = list(quote(length(iv)), 1e5),
  method = list(quote(width(iv)), 5e4),
  method_descendant = list(quote(width(strip)), 5e4),
  method_typed = list(quote(shifted(iv, 1)), 5e4),
  method_struct_arg = list(quote(overlap(iv, iv)), 5e4),
  method_two_positions = list(quote(meets(iv, band, 1)), 5e4),
  method_open_args = list(quote(refit(panel, strip, square)), 2e4),
  replacement = list(quote(upper(iv) <- 9), 5e4),
  conversion = list(quote(r8_convert(signal, Interval)), 2e4),
  result_own = list(quote(middle(iv)), 5e4),
  result_other_package = list(quote(lower(tile)), 5e4),
  open_downcast = list(quote(cells(grid)), 5e4),
  open_result = list(quote(held(panel)), 5e4),
  open_replaced = list(quote(moved(panel, 1, 2)), 5e4),
  open_replaced_read = list(quote(moved(panel, 1, 2)[["range"]]), 2e4),
  nested_read = list(quote(moved(panel, 1, 2)[["frame"]]), 2e4),
  upcast = list(quote(r8_upcast(band, Interval)), 2e4),
  construct = list(quote(Interval(lo = 0, hi = 4)), 5e4),
  construct_open = list(
    quote(Panel(range = strip, frame = iv, figure = square)),
    2e4
  ),
  field_write = list(quote(band[["lo"]] <- 0), 2e4),
  serialize = list(quote(unserialize(serialize(band, NULL))), 1e4),
  serialize_open = list(quote(unserialize(serialize(panel, NULL))), 5e3),
  json_read = list(quote(r8_from_json(Interval, iv_json)), 1e4),
  json_read_open = list(quote(r8_from_json(Panel, panel_json)), 5e3),
  hash = list(quote(r8_hash(iv)), 1e4)
)
if (!is.null(only)) {
  rows <- rows[only]
}

looped <- function(expr) {
  compiler::cmpfun(eval(bquote(function(k) for (i in seq_len(k)) .(expr))))
}
empty <- looped(quote(NULL))
time <- function(f, k) {
  t0 <- Sys.time()
  f(k)
  as.numeric(Sys.time() - t0, units = "secs") / k * 1e6
}
out <- lapply(names(rows), function(n) {
  f <- looped(rows[[n]][[1L]])
  k <- rows[[n]][[2L]]
  time(f, k)
  invisible(gc())
  t <- min(replicate(rounds, time(f, k))) - min(replicate(rounds, time(empty, k)))
  data.frame(
    row = n,
    call = paste(deparse(rows[[n]][[1L]]), collapse = ""),
    us = round(t, 2)
  )
})
print(do.call(rbind, out), row.names = FALSE, right = FALSE)
