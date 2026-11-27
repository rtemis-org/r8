# test-401-workers.R
# ::R8::
# 2026- EDG rtemis.org

# Values sent to parallel workers (mirai and future) and back.

scenario(
  "C1",
  "values cross to parallel workers and back",
  situation = "A user sends values of a package to workers started by mirai and future, and receives values that the workers build.",
  risk = "A worker has not loaded the package, and the value arrives as a plain list.",
  result = "The value and its methods work in the worker, and the value that it returns equals one built in the session.",
  how = "installed",
  code = {
    skip_if_not_installed("mirai")
    skip_if_not_installed("future")
    lib <- install_test_packages()
    out <- run_r(
      lib,
      c(
        "suppressPackageStartupMessages(library(r8pkgA))",
        "x <- Rect(w = 3, h = 2)",
        "mirai::daemons(1)",
        "# A worker that has not loaded the defining package receives a classed list.",
        "m1 <- mirai::mirai(list(loaded = 'r8pkgA' %in% loadedNamespaces(), w = unclass(x)[['w']]), x = x)",
        "r1 <- mirai::collect_mirai(m1)",
        "cat('mirai-unloaded', r1$loaded, r1$w, '\\n')",
        "# A worker that loads it dispatches and constructs; the value comes back equal.",
        "m2 <- mirai::mirai({ library(r8pkgA); list(a = area(x), v = Rect(w = 4, h = 1)) }, x = x)",
        "r2 <- mirai::collect_mirai(m2)",
        "cat('mirai-area', r2$a, '\\n')",
        "cat('mirai-value', R8::r8equal(r2$v, Rect(w = 4, h = 1)), identical(r2$v, Rect(w = 4, h = 1)), '\\n')",
        "mirai::daemons(0)",
        "future::plan(future::multisession, workers = 1)",
        "f <- future::future({ library(r8pkgA); list(a = area(x), v = Rect(w = 5, h = 5)) }, globals = list(x = x), packages = character())",
        "r3 <- future::value(f)",
        "cat('future-area', r3$a, '\\n')",
        "cat('future-value', R8::r8hash(r3$v) == R8::r8hash(Rect(w = 5, h = 5)), '\\n')",
        "future::plan(future::sequential)"
      )
    )
    info <- paste(out, collapse = "\n")
    expect_true("mirai-unloaded FALSE 3 " %in% out, info = info)
    expect_true("mirai-area 6 " %in% out, info = info)
    expect_true("mirai-value TRUE TRUE " %in% out, info = info)
    expect_true("future-area 6 " %in% out, info = info)
    expect_true("future-value TRUE " %in% out, info = info)
  }
)

scenario(
  "C2",
  "a type declared at the console works in a worker",
  situation = "A user declares a type at the console, and sends a function that builds its values to a worker.",
  risk = "The worker does not know the type.",
  result = "The type registers in the worker on its first use there.",
  how = "installed",
  code = {
    skip_if_not_installed("mirai")
    skip_if_not_installed("future")
    lib <- install_test_packages()
    out <- run_r(
      lib,
      c(
        "options(r8.namespace = 'scratch')",
        "K <- R8::new_enum('K', variants = list(Linear = R8::variant(), Rbf = R8::variant(fields = list(gamma = R8::type_double()))))",
        "S <- R8::new_struct('S', fields = list(cost = R8::type_double(min = 0), kernel = R8::field(K, default = K@Linear)))",
        "cat('record', is.raw(attr(S, 'r8_record')), is.null(attr(S, 'r8_ir')), '\n')",
        "mirai::daemons(1)",
        "# The worker has no declaration of S or K; S's record holds both.",
        "m <- mirai::mirai({ v <- S(cost = 2); list(cost = v$cost, kernel = R8::r8variant(v$kernel)) }, S = S)",
        "r <- mirai::collect_mirai(m)",
        "cat('mirai', r$cost, r$kernel, '\n')",
        "m2 <- mirai::mirai(tryCatch(S(cost = -1), r8_value_error = function(e) 'refused'), S = S)",
        "cat('mirai-invalid', mirai::collect_mirai(m2), '\n')",
        "m3 <- mirai::mirai(K@Rbf(gamma = 0.5)$gamma, K = K)",
        "cat('mirai-variant', mirai::collect_mirai(m3), '\n')",
        "mirai::daemons(0)",
        "future::plan(future::multisession, workers = 1)",
        "f <- future::future(S(cost = 3)$cost, globals = list(S = S), packages = character())",
        "cat('future', future::value(f), '\n')",
        "future::plan(future::sequential)"
      )
    )
    info <- paste(out, collapse = "\n")
    expect_true("record TRUE TRUE " %in% out, info = info)
    expect_true("mirai 2 Linear " %in% out, info = info)
    expect_true("mirai-invalid refused " %in% out, info = info)
    expect_true("mirai-variant 0.5 " %in% out, info = info)
    expect_true("future 3 " %in% out, info = info)
  }
)
