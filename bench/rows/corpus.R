# corpus.R
# ::r8 bench::
# 2026- EDG rtemis.org

# The acceptance gate: the number of corpus cases on which the system's
# acceptance differs from the expected one (corpus.R). run.R stops when it
# is not 0.
# spec: R8/class-system#benchmark

source(file.path(Sys.getenv("R8_BENCH_DIR", "bench"), "rows", "common.R"))
invisible(load_system())
source(file.path(BENCH_DIR, "corpus.R"))
r <- run_corpus(SYSTEM)
for (k in r$case[r$expected != r$observed]) message("mismatch: ", k)
emit("corpus", "cases", nrow(r), "")
emit("corpus", "mismatches", sum(r$expected != r$observed), "")
