# test-402-saved.R
# ::R8::
# 2026- EDG rtemis.org

# Scenarios of saved values: a value is written with saveRDS(), or as a JSON
# document, and read in another session.
#
# An analysis is saved today and read in a year, on another computer, or by a
# colleague. The session that reads it may not have the package, or may have
# another version of it.

scenario(
  "C3",
  "a saved value is read in a session without its package",
  situation = "A user saves a value of a package, and reads it where the package is not loaded.",
  risk = "The data are lost, or cannot be told.",
  result = "The value keeps its data and the name of its type, and is a full value once the package is loaded.",
  how = "installed",
  code = {
    first <- pa_first()
    out <- said(
      first$pa,
      c(
        sprintf("p <- readRDS(%s)", deparse(first$p_rds)),
        "say('class', class(p))",
        "say('data', unlist(unclass(p)[c('x', 'y')]))",
        "library(pa)",
        "say('then', c(mag(p), p$y))",
        "say('equal', R8::r8_equal(p, Point(x = 3, y = 2)))"
      )
    )
    expect_said(
      out,
      class = "pa::Point r8_value",
      data = "^3 2$",
      then = "^3 2$",
      equal = "^TRUE$"
    )
  }
)

scenario(
  "C4",
  "a value and its document hold the same data across sessions",
  situation = "A user saves a value with saveRDS() and as a JSON document, and reads both later.",
  risk = "One of the two changes a number, a name, or the order of the fields.",
  result = "Both are equal to the value that was saved, and have its hash.",
  how = "installed",
  code = {
    first <- pa_first()
    out <- said(
      c(first$pb, first$pa),
      c(
        "library(pb)",
        sprintf("h <- readRDS(%s)", deparse(first$h_rds)),
        sprintf(
          "j <- R8::r8_from_json(Holder, readLines(%s))",
          deparse(first$h_json)
        ),
        "n <- Holder(p = pa::Point(x = 3, y = 2), label = 'a')",
        "say('equal', c(R8::r8_equal(h, n), R8::r8_equal(j, n)))",
        "say('hash', length(unique(c(R8::r8_hash(h), R8::r8_hash(j), R8::r8_hash(n)))))"
      )
    )
    expect_said(out, equal = "^TRUE TRUE$", hash = "^1$")
  }
)

scenario(
  "C5",
  "a worker has another version of a package than the session",
  situation = "A cluster has pa 2.0.0, with the fields in another order, and the session that sends it a value has pa 1.0.0.",
  risk = "The worker reads another field, or changes one.",
  result = "The worker reads each field by its name, and refuses to use the value or to change it until r8_update() gives its value of the worker's declaration.",
  how = "installed",
  code = {
    out <- with_pa(
      "reordered",
      c(
        "# The value arrives as the bytes that mirai and future send.",
        "q <- unserialize(serialize(p, NULL))",
        "say('read', c(q$x, q$y))",
        "say('use', pa::mag(q))",
        "say('write', { q$y <- 9; q })",
        "say('updated', { u <- R8::r8_update(q); u$y <- 9; c(pa::mag(u), unlist(as.list(u))) })"
      )
    )
    expect_said(
      out,
      read = "^3 2$",
      use = "^error: .*in the order x, y, and are in the order y, x.*declaration_changed",
      write = "^error: .*declaration_changed",
      updated = "^3 9 3$"
    )
  }
)
