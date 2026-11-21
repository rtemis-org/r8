# corpus.R
# ::r8 bench::
# 2026- EDG rtemis.org

# The acceptance gate: the number of corpus cases on which the system's
# acceptance differs from the expected one (corpus.R), and the stored
# fields of the valid GBM. run.R stops when a case differs or when the
# stored fields differ between systems.
# spec: R8/class-system#benchmark

source(file.path(Sys.getenv("R8_BENCH_DIR", "bench"), "rows", "common.R"))
ops <- load_system()
source(file.path(BENCH_DIR, "corpus.R"))
r <- run_corpus(SYSTEM, ops)
for (k in r$case[r$expected != r$observed]) message("mismatch: ", k)
emit("corpus", "cases", nrow(r), "")
emit("corpus", "mismatches", sum(r$expected != r$observed), "")
cat(jsonlite::toJSON(
  list(row = ROW, system = SYSTEM, mode = MODE, type = "corpus", metric = "stored_fields", value = stored_fields(), unit = "json"),
  auto_unbox = TRUE
), "\n", sep = "")
