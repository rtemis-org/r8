# code.R
# ::r8 bench::
# 2026- EDG rtemis.org

# Writes `results/<commit>.code.json`: the code each row of the benchmark
# times, for the user guide's benchmark page, from the benchmark's own
# scripts so that the page shows the code that ran. For each system: the
# timed expressions, as declare_system() returns them; the objects that
# define the GBM and the helpers those expressions call, printed as R holds
# them after declaration, with the metaprogramming of types.R resolved. Also
# the source text of the hand-written checks (checks.R), which S3, S4, RC,
# R6, and S7 call, and the inputs (gbm_args()). Run from the repository
# root: `Rscript --vanilla bench/code.R <commit>`; `just bench-publish` runs
# it when the file is missing.
# spec: R8/class-system#benchmark

args <- commandArgs(trailingOnly = TRUE)
commit <- if (length(args)) args[[1L]] else system("git rev-parse --short HEAD", intern = TRUE)
suppressPackageStartupMessages({
  library(R8)
  library(S7)
  library(R6)
})
options(r8.namespace = "bench_code", width = 80)
source("bench/checks.R")
source("bench/types.R")

# The text of an object as R prints it, without its environment line.
text_of <- function(x) {
  lines <- utils::capture.output(print(x))
  lines <- lines[!grepl("^<(environment|bytecode)", lines)]
  paste(lines, collapse = "\n")
}

# The text of an expression.
expr_text <- function(x) paste(deparse(x, width.cutoff = 72L), collapse = "\n")

# The source text, comments included, of the top-level definitions of
# `names` in `file`, in the file's order.
source_text <- function(file, names) {
  exprs <- parse(file, keep.source = TRUE)
  refs <- attr(exprs, "srcref")
  keep <- vapply(exprs, function(e) {
    is.call(e) && identical(e[[1L]], as.name("<-")) && as.character(e[[2L]]) %in% names
  }, logical(1))
  paste(vapply(refs[keep], function(r) paste(as.character(r), collapse = "\n"), ""), collapse = "\n\n")
}

# The source text of the R8 declaration of GBM in r8_declarations()
# (types.R), as written there.
r8_gbm_text <- function() {
  lines <- readLines("bench/types.R")
  start <- grep("^    GBM = quote\\(R8::new_struct\\($", lines)
  end <- start + which(lines[(start + 1L):length(lines)] == "    ))")[1L]
  body <- lines[(start + 1L):(end - 1L)]
  paste(c("GBM <- R8::new_struct(", sub("^    ", "", body), ")"), collapse = "\n")
}

timed_rows <- c(
  "construct", "read", "write_idiom", "write_field_rules", "write_all",
  "write_keep", "validate", "dispatch"
)

system_code <- function(system) {
  where <- new.env()
  x <- declare_system(system, where)
  timed <- lapply(x[intersect(timed_rows, names(x))], expr_text)
  defs <- switch(
    system,
    "S3" = list(
      GBM = where$GBM,
      `$<-.s3_GBM` = where$`$<-.s3_GBM`,
      set_fr = where$set_fr,
      validate_gbm = where$validate_gbm,
      complexity = where$complexity,
      complexity.s3_GBM = where$complexity.s3_GBM
    ),
    "S4" = list(
      GBM = where$GBM,
      `validity of s4_GBM` = methods::getValidity(methods::getClass("s4_GBM", where = where)),
      set_fr = where$set_fr,
      `complexity for s4_GBM` = methods::getMethod("complexity", "s4_GBM", where = where)@.Data
    ),
    "RC" = c(
      list(GBM = where$GBM),
      lapply(
        as.list(where$rc_GBM$def@refMethods)[c("initialize", "validate", "set", "set_fr", "complexity")],
        function(m) if (is.null(m)) NULL else m@.Data
      )
    ),
    "R6" = c(
      list(GBM = where$GBM),
      where$r6_GBM$public_methods[intersect(c("initialize", "validate", "set_fr", "complexity"), names(where$r6_GBM$public_methods))],
      list(`active binding n_trees` = where$r6_GBM$active[["n_trees"]])
    ),
    "S7" = list(
      `s7_gbm_properties()` = s7_gbm_properties,
      s7_gbm_validator = s7_gbm_validator,
      GBM = where$s7_GBM,
      complexity = where$complexity
    ),
    "R8" = list(
      GBM = structure(r8_gbm_text(), class = "source_text"),
      Complexity = quote(R8::new_trait("Complexity", methods = list(complexity = R8::trait_method(R8::params(x))))),
      impl = quote(R8::impl(Complexity, GBM, methods = list(complexity = function(x) 1L)))
    )
  )
  defs <- Filter(Negate(is.null), defs)
  list(
    timed = timed,
    definitions = lapply(defs, function(d) {
      if (inherits(d, "source_text")) unclass(d) else if (is.call(d) || is.name(d)) expr_text(d) else text_of(d)
    })
  )
}

systems <- c("S3", "S4", "RC", "R6", "S7", "R8")
out <- list(
  commit = commit,
  inputs = expr_text(body(gbm_args)),
  checks = source_text(
    "bench/checks.R",
    c("chk_double", "chk_int", "chk_opt_int", "chk_string", "chk_flag", "chk_dvec", "chk_ivec",
      "GBM_CHECKS", "check_fields", "rules_gbm", "check_gbm", "check_gbm_field")
  ),
  json_read = expr_text(quote(R8::r8_from_json(GBM, json))),
  systems = stats::setNames(lapply(systems, system_code), systems)
)
dir.create("bench/results", showWarnings = FALSE)
path <- file.path("bench/results", paste0(commit, ".code.json"))
writeLines(jsonlite::toJSON(out, auto_unbox = TRUE, pretty = TRUE), path)
cat("Wrote", path, "\n")
