# test-148-explain.R
# ::R8::
# 2026- EDG rtemis.org

# The plan of a call of a table function before it runs: its stages, where
# each runs, the typed functions it would call, and its flowchart.
# spec: R8/raven#plans

declared <- function(expr) {
  op <- options(r8.namespace = "plans")
  on.exit(options(op))
  expr
}

m <- Raven(
  combo = c(1L, 1L, 2L, 2L),
  auc = c(0.81, 0.79, 0.85, 0.88),
  brier = c(0.16, 0.18, 0.14, 0.12)
)
calls <- 0L
spread <- declared(new_function(
  "spread",
  function(x = arg_vec(type_double())) {
    calls <<- calls + 1L
    max(x) - min(x)
  },
  return_type = type_double()
))

# The qualified name of `spread`, whose namespace depends on how the tests run.
q <- attr(spread, "r8_function")
stages_of <- function(plan) as.data.frame(plan)[["stage"]]

test_that("a map's plan states its stages and calls no typed function", {
  calls <<- 0L
  plan <- r8explain(r8map(m, spread, by = "combo"))
  expect_s3_class(plan, "r8_plan")
  expect_identical(calls, 0L)
  expect_identical(
    stages_of(plan),
    c("check", "compile", "group", "call", "call", "assemble")
  )
  stages <- as.data.frame(plan)
  expect_identical(
    names(stages),
    c(
      "node_id",
      "stage",
      "label",
      "engine",
      "function",
      "language",
      "calls",
      "threads",
      "inputs",
      "status",
      "t_start",
      "t_end",
      "busy"
    )
  )
  expect_true(all(is.na(stages[c("t_start", "t_end", "busy")])))
  calling <- stages[stages[["stage"]] == "call", ]
  expect_identical(calling[["function"]], rep(q, 2L))
  expect_identical(calling[["language"]], rep("r", 2L))
  expect_identical(calling[["engine"]], rep("r", 2L))
  expect_identical(calling[["calls"]], c(2L, 2L))
  expect_identical(
    calling[["label"]],
    paste0(q, c(": auc", ": brier"))
  )
  expect_identical(calling[["inputs"]], c("s3, s4", "s3, s4"))
  expect_identical(unique(stages[["status"]]), "pending")
  expect_identical(plan$engine, "rust+r")
  expect_identical(
    plan$headline,
    paste0(
      "r8map(): ",
      q,
      " over 2 columns and 2 groups, 4 calls, in Rust, with the body of `",
      q,
      "` in R"
    )
  )
  # The run calls the function after its plan.
  r8map(m, spread, by = "combo")
  expect_identical(calls, 4L)
})

test_that("the call's node holds the call, R8's version, and the plan hash", {
  plan <- r8explain(r8map(m, spread, by = "combo"))
  root <- plan$nodes[[1L]]
  expect_identical(root$node_id, "s1")
  expect_null(root$parent_id)
  expect_identical(root$kind, "r8map")
  expect_identical(root$label, q)
  expect_identical(root$meta$call, "r8map(m, spread, by = \"combo\")")
  expect_identical(
    root$meta$r8_version,
    as.character(utils::packageVersion("R8"))
  )
  expect_identical(root$meta$platform, R.version$platform)
  expect_identical(root$meta$format, 1L)
  expect_match(root$meta$plan_hash, "^sha256:[0-9a-f]{64}$")
  expect_identical(root$meta$functions[[1L]]$name, q)
  expect_identical(root$meta$functions[[1L]]$language, "r")
  # The plan hash is that of the computation: the same for the same call, and
  # another for another expression or other keys.
  again <- r8explain(r8map(m, spread, by = "combo"))
  expect_identical(again$nodes[[1L]]$meta$plan_hash, root$meta$plan_hash)
  ungrouped <- r8explain(r8map(m, spread))
  expect_false(identical(
    ungrouped$nodes[[1L]]$meta$plan_hash,
    root$meta$plan_hash
  ))
  median_plan <- r8explain(r8map(m, calc(median(col())), by = "combo"))
  expect_false(identical(
    median_plan$nodes[[1L]]$meta$plan_hash,
    root$meta$plan_hash
  ))
})

test_that("the plan's engine is the one that the map's messages state", {
  plan <- r8explain(r8map(m, spread, by = "combo"))
  said <- r8_map_message(
    "start",
    c(list(label = q), map_place("rust", q))
  )
  expect_identical(plan$engine, said$engine)
  plan <- r8explain(r8map(m, calc(median(col()))))
  expect_identical(plan$engine, "rust")
})

test_that("a summary, a computed column, and a list have their own stages", {
  s <- r8explain(r8summarize(
    m,
    list(a = calc(mean(auc)), n = calc(n())),
    by = "combo"
  ))
  expect_identical(
    stages_of(s),
    c("check", "compile", "group", "aggregate", "assemble")
  )
  expect_identical(s$nodes[[1L]]$kind, "r8summarize")
  expect_identical(s$engine, "rust")
  r <- r8explain(r8set(m, list(ratio = calc(auc / brier))))
  expect_identical(stages_of(r), c("check", "compile", "evaluate", "assemble"))
  l <- r8explain(r8map(list(a = c(1, 2), b = c(3, 5)), spread))
  expect_identical(stages_of(l), c("check", "call", "assemble"))
  expect_identical(l$engine, "r")
  expect_identical(l$nodes[[1L]]$meta$path, "host")
  expect_identical(as.data.frame(l)[["calls"]], c(NA, 2L, NA))
})

test_that("a stage reads the stages whose results it reads", {
  r <- r8explain(r8set(m, list(share = calc(auc / sum(auc))), by = "combo"))
  stages <- as.data.frame(r)
  expect_identical(
    stages$stage,
    c("check", "compile", "group", "aggregate", "evaluate", "assemble")
  )
  expect_identical(stages$inputs[stages$stage == "evaluate"], "s3, s4, s5")
  expect_identical(stages$inputs[stages$stage == "assemble"], "s6")
  twice <- r8explain(r8map(
    m,
    calc(spread(col()) - spread(col())),
    columns = "auc"
  ))
  calls <- as.data.frame(twice)
  expect_identical(
    calls$label[calls$stage == "call"],
    paste0(q, ": auc #", 1:2)
  )
})

test_that("a call of r8set() without calc() has a plan of its checks", {
  p <- r8explain(r8set(m, list(flag = m$auc > 0.8)))
  expect_identical(stages_of(p), c("check", "compile", "assemble"))
  p <- r8explain(r8set(m, list()))
  expect_identical(stages_of(p), c("check", "compile", "assemble"))
})

test_that("the arguments run before the call, and only the call is explained", {
  calls <<- 0L
  plan <- r8explain(r8map(
    r8set(m, list(ratio = calc(auc / brier))),
    spread,
    columns = "ratio"
  ))
  expect_identical(plan$nodes[[1L]]$kind, "r8map")
  expect_identical(calls, 0L)
  expect_identical(
    as.data.frame(plan)[["label"]][[3L]],
    paste0(q, ": ratio")
  )
})

test_that("a plan writes its Mermaid flowchart", {
  plan <- r8explain(r8map(m, spread, by = "combo"))
  expect_identical(
    format(plan, "mermaid"),
    paste0(
      "flowchart LR\n",
      "  subgraph s1[\"r8map(): ",
      q,
      "\"]\n",
      "    s2[\"check\"]:::r\n",
      "    s3[\"compile, 2 expressions\"]:::rust\n",
      "    s4[\"group by combo, 2 groups\"]:::rust\n",
      "    s5[\"call ",
      q,
      ": auc, 2 calls\"]:::r\n",
      "    s6[\"call ",
      q,
      ": brier, 2 calls\"]:::r\n",
      "    s7[\"assemble\"]:::r\n",
      "  end\n",
      "  s2 --> s3\n",
      "  s2 --> s4\n",
      "  s3 --> s5\n",
      "  s4 --> s5\n",
      "  s3 --> s6\n",
      "  s4 --> s6\n",
      "  s5 --> s7\n",
      "  s6 --> s7\n"
    )
  )
  out <- capture.output(print(plan))
  expect_identical(
    out[[1L]],
    paste0("<r8 plan> ", plan$headline)
  )
  expect_true(any(grepl("r8rust_signature(spread)", out, fixed = TRUE)))
  expect_identical(format(plan), out)
})

test_that("r8explain() refuses what it cannot explain, and raises the call's errors", {
  expect_error(r8explain(r8map), "unevaluated", class = "r8_argument_error")
  expect_error(
    r8explain(mean(1:3)),
    "`mean` is another function",
    class = "r8_argument_error"
  )
  expect_error(
    r8explain(r8map(m, spread, by = "nope")),
    "`nope` is not a column",
    class = "r8_value_error"
  )
  expect_error(
    r8explain(r8map(m, spread, args = list(nope = 1))),
    "has no parameter `nope`",
    class = "r8_argument_error"
  )
  # After an error, a call runs.
  expect_false(explaining())
  expect_s3_class(r8map(m, calc(median(col()))), "r8_table")
})

test_that("a function of no package is identified by its body", {
  # The last test of the file: it declares `spread` again, with another body.
  # A test's functions belong to R8's namespace when the tests run inside it,
  # so the closures here are moved to an environment of no package.
  f <- function(plan) plan$nodes[[1L]]$meta$functions[[1L]]
  first <- r8explain(r8map(m, spread, by = "combo"))
  home <- topenv(environment(spread))
  if (isNamespace(home)) {
    expect_identical(f(first)$package, getNamespaceName(home)[["name"]])
  } else {
    expect_match(f(first)$body_hash, "^sha256:[0-9a-f]{64}$")
  }
  loose <- function(closure) {
    environment(closure) <- new.env(parent = globalenv())
    closure
  }
  identity_of <- function(plan, closure) {
    f(plan_of(
      plan$nodes,
      quote(r8map(m, spread)),
      stats::setNames(list(loose(closure)), q)
    ))
  }
  one <- identity_of(first, spread)
  expect_null(one$package)
  expect_match(one$body_hash, "^sha256:[0-9a-f]{64}$")
  other <- declared(new_function(
    "spread",
    function(x = arg_vec(type_double())) diff(range(x)),
    return_type = type_double()
  ))
  second <- r8explain(r8map(m, other, by = "combo"))
  two <- identity_of(second, other)
  expect_false(identical(two$body_hash, one$body_hash))
  # The declaration is the same, and so is the plan hash.
  expect_identical(
    second$nodes[[1L]]$meta$plan_hash,
    first$nodes[[1L]]$meta$plan_hash
  )
})
