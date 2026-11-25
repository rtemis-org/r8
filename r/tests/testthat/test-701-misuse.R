# test-701-misuse.R
# ::R8::
# 2026- EDG rtemis.org

# Scenarios of misuse and damage: a value that R code takes apart, and a saved
# value whose bytes are damaged.
#
# R lets any code change the attributes of an object. These tests state what R8
# finds out, and where.

misuse_type <- function(ns) {
  declare_in(ns, {
    new_struct(
      "Point",
      fields = list(x = type_double(min = 0), y = type_double())
    )
  })
}

scenario(
  "F2",
  "a typed argument refuses a value that was taken apart and rebuilt",
  situation = "A user removes the class of a value, puts in an invalid field, and restores the class.",
  risk = "A function that takes the type receives an invalid value.",
  result = "The call is an error that says what the object is: the type and the fields of a value are in the object, where R code does not reach, and a write of an element that R8 does not make leaves a list.",
  code = {
    Point <- misuse_type("f2")
    f <- declare_in("f2", {
      area <- generic("area", params(x, corner = arg(Point)))
      implement(area, Point) <- function(x, corner) x$x * corner$y
      area
    })
    p <- Point(x = 1, y = 2)
    forged <- unclass(p)
    forged$x <- -1
    class(forged) <- class(p)
    expect_error(f(forged, p), "is not one", class = "r8_error")
    expect_error(f(p, forged), "dput", class = "r8_error")
    copied <- list(x = -1, y = 2)
    attributes(copied) <- attributes(p)
    expect_error(f(copied, p), "is not one", class = "r8_error")
    expect_error(f(p, copied), "dput", class = "r8_error")
    expect_identical(f(p, p), 2)
    # A value that was only looked at is a value.
    same <- unclass(p)
    class(same) <- class(p)
    expect_identical(f(same, same), 2)
  }
)

scenario(
  "F3",
  "a value whose fields or class R code changed is not written or compared",
  situation = "A user changes a field of a value, its class, or its attributes with R's own functions.",
  risk = "The value is written to a file or compared as a valid one, or as a value of another type.",
  result = "A list in place of the value is an error, and a class that R code gives a value changes nothing that R8 reads.",
  code = {
    Point <- misuse_type("f3")
    Other <- declare_in(
      "f3",
      new_struct("Other", fields = list(a = type_character()))
    )
    p <- Point(x = 1, y = 2)
    forged <- unclass(p)
    forged$x <- -1
    class(forged) <- class(p)
    expect_error(r8_validate(forged), class = "r8_error")
    expect_error(r8_to_json(forged), class = "r8_error")
    expect_error(r8_hash(forged), class = "r8_error")
    expect_error(forged$x <- 1, "is not one", class = "r8_error")
    expect_match(format(forged)[[1L]], "is not one")
    renamed <- p
    class(renamed) <- c("f3::Other", "r8_value")
    attr(renamed, "r8_type") <- "f3::Other"
    expect_identical(r8_to_json(renamed), r8_to_json(p))
    expect_true(r8_equal(renamed, p))
    Holder <- declare_in("f3", new_struct("Holder", fields = list(o = Other)))
    expect_error(Holder(o = renamed), class = "r8_value_error")
  }
)

scenario(
  "F4",
  "R's functions that change a vector are refused on a value",
  situation = "A user calls names<-, length<-, or an assignment by position on a value.",
  risk = "A field is renamed, dropped, or replaced without a check.",
  result = "Each is an error, and the value is as it was.",
  code = {
    p <- misuse_type("f4")(x = 1, y = 2)
    expect_error(names(p) <- c("y", "x"), class = "r8_error")
    expect_error(length(p) <- 1L, class = "r8_error")
    expect_error(p[1] <- list(5), class = "r8_error")
    expect_error(p[[1]] <- 5, class = "r8_error")
    expect_identical(as.list(p), list(x = 1, y = 2))
  }
)

scenario(
  "F5",
  "a saved value of Rust with invalid fields is not given to Rust code",
  situation = "A file that holds a value of a type whose values Rust holds is damaged, or edited, in the numbers and the texts of its fields. Damage to R's own part of a file is R's to find, and r8_read_rds() finds it.",
  risk = "The Rust code computes with a value that its type refuses.",
  result = "The fields read as the file holds them. The Rust code is given the value once its fields are checked, and an invalid one is an error.",
  how = "installed",
  code = {
    lib <- install_test_packages()
    out <- run_r(
      lib,
      c(
        "suppressPackageStartupMessages(library(r8pkgR))",
        "bytes <- function(x) writeBin(x, raw(), endian = 'little')",
        "find <- function(b, p) which(vapply(seq_len(length(b) - length(p) + 1L), function(i) all(b[seq_along(p) + i - 1L] == p), NA))",
        "edit <- function(x, from, to) { b <- serialize(x, NULL, xdr = FALSE); at <- find(b, bytes(from)); b[at + seq_along(bytes(to)) - 1L] <- bytes(to); unserialize(b) }",
        "msg <- function(x) tryCatch(x, error = conditionMessage)",
        "x <- edit(Interval(lo = 2, hi = 5), 5, 1)",
        "cat('fields', x$lo, x$hi, '\\n')",
        "cat('width', msg(width(x)), '\\n')",
        "cat('write', msg({ y <- x; y$hi <- 9; width(y) }), '\\n')",
        "cat('validate', inherits(tryCatch(R8::r8_validate(x), error = identity), 'r8_value_error'), '\\n')",
        "p <- Panel(range = Interval(lo = 3, hi = 7), frame = Interval(lo = 0, hi = 1), figure = Square(side = 1))",
        "cat('nested', msg(contents(edit(p, 7, 1))), '\\n')",
        "set.seed(2)",
        "res <- character()",
        "for (i in 1:300) {",
        "  b <- serialize(Strip(lo = 2, hi = 5, label = 'abcdef', weights = c(3, 4), number = 77L), NULL, xdr = FALSE)",
        "  part <- list(bytes(2), bytes(5), bytes(3), bytes(4), charToRaw('abcdef'), bytes(77L))[[sample(6L, 1L)]]",
        "  k <- find(b, part)[1L] + sample(length(part), sample(2L, 1L)) - 1L",
        "  b[k] <- as.raw(sample(if (length(part) == 6L) 97:122 else 0:255, length(k), TRUE))",
        "  res <- c(res, tryCatch({ x <- unserialize(b); for (f in names(x)) x[[f]]; ok <- tryCatch({ R8::r8_validate(x); TRUE }, error = function(e) FALSE); given <- tryCatch({ width(x); TRUE }, error = function(e) FALSE); if (ok == given) paste('same', ok) else 'differ' }, error = function(e) 'error'))",
        "}",
        "cat('random', all(res %in% c('same TRUE', 'same FALSE')), all(c('same TRUE', 'same FALSE') %in% res), '\\n')"
      )
    )
    info <- paste(out, collapse = "\n")
    expect_true("fields 2 1 " %in% out, info = info)
    expect_true(
      any(grepl("^width .*saved `pkgr::Interval` is invalid", out)),
      info = info
    )
    expect_true(
      "write 7 " %in% out || any(grepl("^write .*invalid", out)),
      info = info
    )
    expect_true("validate TRUE " %in% out, info = info)
    expect_true(any(grepl("^nested .*invalid", out)), info = info)
    expect_true("random TRUE TRUE " %in% out, info = info)
  }
)

scenario(
  "F13",
  "a condition on columns means the same whatever the table and the session hold",
  situation = "A user filters a table by a column, in a session that has a variable of the column's name, or a table that later gains a column of a variable's name. Or the user forgets rule().",
  risk = "The line reads a variable where the user means a column, or the other way round, and keeps other rows without an error.",
  result = "A name in rule() is a column, and val() is R's value; every other index is R's own. A condition on a column without rule() is an error that writes the rule.",
  code = {
    r <- Raven(age = c(17, 30, 80), score = c(1, 2, 3))
    age <- c(100, 100, 100)
    expect_identical(r8_filter(r, rule(age >= 18))$score, c(2, 3))
    cutoff <- 50
    older <- function(x) r8_filter(x, rule(age >= val(cutoff)))$score
    expect_identical(older(r), 3)
    r$cutoff <- c(0, 0, 0)
    expect_identical(older(r), 3)
    expect_error(r[score > 1, ], "rule(score > 1)", fixed = TRUE)
    expect_error(
      r8_filter(r, rule(age >= limit)),
      "write `val(limit)`",
      fixed = TRUE
    )
  }
)

scenario(
  "F14",
  "a missing cell decides nothing that another condition decides",
  situation = "A rule has two conditions, and a cell that one of them reads is missing. The rule checks the rows of a table, and filters another.",
  risk = "The order in which the conditions are written decides whether a row is valid, or whether it is kept.",
  result = "Conditions combine as R combines them: a false one is false with a missing one, in either order. A check fails where the rule is false, and a filter keeps the rows where it is true; a row where it is missing is neither.",
  code = {
    one <- rule(dose > 0 && arm == "a")
    two <- rule(arm == "a" && dose > 0)
    x <- Raven(dose = c(1, NA, NA, -1), arm = c("a", "a", "b", "b"), row = 1:4)
    for (r in list(one, two)) {
      expect_identical(r8_filter(x, r)$row, 1L)
      expect_identical(r8_filter(x, !r)$row, 3:4)
      Doses <- declare_in("f14", {
        new_table(
          "Doses",
          list(dose = option(type_double()), arm = type_character()),
          rules = r
        )
      })
      expect_identical(nrow(Doses(dose = c(1, NA), arm = c("a", "a"))), 2L)
      expect_error(Doses(dose = NA_real_, arm = "b"), "arm = \"b\"")
    }
  }
)

scenario(
  "F15",
  "a calculation gives values that the type of its source does not admit",
  situation = "A user standardizes a column that is declared to be at least 0, takes the logarithm of another, which gives NaN, and computes a column from two.",
  risk = "The calculation is an error that the user did not expect, or its result keeps a range, a rule, or a unit that is not its own.",
  result = "A calculation is R's, and its result keeps nothing of its source: under another name, it is a column without a range, a rule, or a unit. Where it replaces the declared column, the error says so, and gives another name and as_raven(), whose replaced column has no unit.",
  code = {
    Patients <- declare_in("f15", {
      new_table(
        "Patients",
        list(
          weight = column(type_double(exclusive_min = 0), unit = "kg"),
          height = column(type_double(exclusive_min = 0), unit = "m"),
          auc = type_double(min = 0, max = 1, nan = TRUE)
        ),
        open = TRUE,
        rules = rule(auc >= 0.5)
      )
    })
    # NaN is in no range, and a rule on it has no answer.
    p <- Patients(weight = c(60, 80), height = c(1.6, 1.8), auc = c(0.7, NaN))
    z <- (p$weight - mean(p$weight)) / sd(p$weight)
    p$weight_z <- z
    p$bmi <- p$weight / p$height^2
    p$log_dose <- suppressWarnings(log(p$weight - 70))
    expect_identical(r8_columns(p)$unit, c("kg", "m", NA, NA, NA, NA))
    expect_identical(is.nan(p$log_dose), c(TRUE, FALSE))
    expect_error(
      p$weight <- z,
      "A transformed column takes another name, or as_raven(x)",
      fixed = TRUE
    )
    expect_error(p$auc <- c(0.4, NaN), "auc >= 0.5", fixed = TRUE)
    r <- as_raven(p)
    r$weight <- z
    expect_identical(r8_columns(r)$unit[1:2], c(NA, "m"))
    # A check, and a rule, for what is new.
    expect_identical(r8_check(p$bmi, vec(type_double(min = 0))), p$bmi)
    expect_identical(nrow(r8_filter(p, rule(bmi > 24))), 1L)
  }
)
