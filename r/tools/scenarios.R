# scenarios.R
# ::R8::
# 2026- EDG rtemis.org

# Writes tests/scenarios.md, the list of the tests and of the scenarios, from the
# test files, and a copy to each path given: Rscript tools/scenarios.R [path ...]

dir <- file.path("tests", "testthat")
env <- new.env()
sys.source(file.path(dir, "helper-scenario.R"), env)
text <- env$scenario_text(dir)
for (path in c(file.path("tests", "scenarios.md"), commandArgs(TRUE))) {
  writeLines(text, path)
}
