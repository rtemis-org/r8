# helper-scenario.R
# ::R8::
# 2026- EDG rtemis.org

# A scenario is a test of what happens to a user: who does what, when, and
# what R8 does. Its text is written for a reader who knows R and not R8's
# internals, and tools/scenarios.R writes it into tests/scenarios.md, from
# which the paper takes it.

# Runs `code` as the test "<id> <title>".
#   id         A letter for the group, and a number: "E3".
#   title      The outcome, in one clause.
#   situation  Who does what, and when.
#   risk       What goes wrong when nothing checks it.
#   result     What R8 does.
#   how        "installed": packages installed, some of them apart, and fresh R
#              sessions. "session": one R session.
#   open       For a scenario whose result is not decided or not implemented:
#              what R8 does today. Its test is skipped.
scenario <- function(
  id,
  title,
  situation,
  risk,
  result,
  how = c("session", "installed"),
  open = NULL,
  code = {}
) {
  how <- match.arg(how)
  code <- substitute(code)
  if (!is.null(open)) {
    code <- bquote(skip(.(paste0("open: ", open))))
  } else if (identical(how, "installed")) {
    code <- bquote({
      skip_unless_packages()
      .(code)
    })
  }
  eval(
    bquote(test_that(.(paste(id, title)), .(code))),
    parent.frame()
  )
}

# Skips a test that installs packages unless R8_PACKAGE_TESTS is set.
skip_unless_packages <- function() {
  skip_if(
    !nzchar(Sys.getenv("R8_PACKAGE_TESTS")),
    "set R8_PACKAGE_TESTS to install the test packages"
  )
}

# The groups of the scenarios, in the order of the paper.
SCENARIO_GROUPS <- c(
  A = "One session: a user declares a type again at the console",
  B = "One package: it is installed, loaded, unloaded, and developed",
  C = "Values that travel: to workers, files, and other sessions",
  D = "Two packages, both current: one uses the types of the other",
  E = "Two packages, one updated: on CRAN or r-universe, or by its developer",
  F = "Damage and misuse"
)

# The scenarios of the test files of `dir`, in the order of the paper, one row
# each: the arguments of their scenario() calls, and the file.
scenario_table <- function(dir = test_path()) {
  files <- sort(list.files(dir, "^test-[0-9]+.*[.]R$", full.names = TRUE))
  rows <- lapply(files, function(f) {
    calls <- Filter(
      function(e) is.call(e) && identical(e[[1L]], quote(scenario)),
      as.list(parse(f, keep.source = FALSE))
    )
    do.call(
      rbind,
      lapply(calls, function(e) {
        a <- as.list(match.call(scenario, e))[-1L]
        data.frame(
          id = a[["id"]],
          title = a[["title"]],
          situation = a[["situation"]],
          risk = a[["risk"]],
          result = a[["result"]],
          how = if (is.null(a[["how"]])) "session" else a[["how"]],
          open = if (is.null(a[["open"]])) "" else a[["open"]],
          file = basename(f)
        )
      })
    )
  })
  out <- do.call(rbind, rows)
  out[order(substr(out$id, 1L, 1L), as.integer(substring(out$id, 2L))), ]
}

# The essential tests, one row per file: the first paragraph of its header, and
# the number of its tests.
essential_table <- function(dir = test_path()) {
  files <- sort(list.files(dir, "^test-1[0-9]+.*[.]R$", full.names = TRUE))
  do.call(
    rbind,
    lapply(files, function(f) {
      lines <- readLines(f)
      header <- lines[-(1:4)]
      end <- match(TRUE, !startsWith(header, "# "))
      data.frame(
        file = basename(f),
        text = sub(
          " *spec: .*$",
          "",
          paste(sub("^# ", "", header[seq_len(end - 1L)]), collapse = " ")
        ),
        tests = sum(grepl("^test_that\\(", lines))
      )
    })
  )
}

# The text of tests/scenarios.md.
scenario_text <- function(dir = test_path()) {
  s <- scenario_table(dir)
  e <- essential_table(dir)
  cell <- function(x) gsub("|", "\\|", x, fixed = TRUE)
  tested <- ifelse(
    nzchar(s$open),
    paste0("open: ", s$open),
    ifelse(s$how == "installed", "installed packages", "one session")
  )
  groups <- lapply(names(SCENARIO_GROUPS), function(g) {
    r <- s[substr(s$id, 1L, 1L) == g, ]
    t <- tested[substr(s$id, 1L, 1L) == g]
    c(
      paste0("### ", g, ". ", SCENARIO_GROUPS[[g]]),
      "",
      "| | What happens | The situation | The risk without a check | What R8 does | Tested with |",
      "|:--|:--|:--|:--|:--|:--|",
      sprintf(
        "| %s | %s | %s | %s | %s | %s |",
        r$id,
        cell(r$title),
        cell(r$situation),
        cell(r$risk),
        cell(r$result),
        cell(t)
      ),
      ""
    )
  })
  c(
    "# The tests of R8 in R",
    "",
    "`Rscript tools/scenarios.R` writes this file from the test files; a test compares the two.",
    "",
    "## Essentials",
    "",
    sprintf(
      "What each feature does, in %d files of %d tests. The runtime has its own tests in Rust (`rust/crates/`), and the behavior that every host language shares is in `conformance/`.",
      nrow(e),
      sum(e$tests)
    ),
    "",
    "| File | What it tests | Tests |",
    "|:--|:--|--:|",
    sprintf("| `%s` | %s | %d |", e$file, cell(e$text), e$tests),
    "",
    "## Scenarios",
    "",
    sprintf(
      "What happens to a user: %d scenarios, of which %d are open, since what R8 is to do is not decided or not implemented. Scenarios of packages install them, at several versions where a package is updated, and run in fresh R sessions.",
      nrow(s),
      sum(nzchar(s$open))
    ),
    "",
    unlist(groups)
  )
}
