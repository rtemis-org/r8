# test-703-limits.R
# ::R8::
# 2026- EDG rtemis.org

# Scenarios of limits: values, declarations, and lists that nest deeper than the
# stack of a process holds, arguments that R8's own routines do not expect, and
# functions that change a vector in place.

nested_list <- function(n) {
  out <- list(1)
  for (i in seq_len(n)) {
    out <- list(out)
  }
  out
}

sharing_table <- function(ns) {
  declare_in(ns, {
    new_struct(
      "W",
      fields = list(
        tab = type_table(columns = list(a = column(type_double(min = 0))))
      )
    )
  })
}

scenario(
  "F6",
  "a value that nests too deep is an error",
  situation = "A user builds a chain of values, each a field of the next, or puts a list of many levels into a field of JSON, as a recursive function or a parser may give it.",
  risk = "Each walk of the value takes a part of the stack per level, and the session ends.",
  result = "The value is refused with the limit, which every JSON parser reads, and with what holds a longer chain. A chain within the limit is printed, hashed, compared, written, and read.",
  code = {
    Node <- declare_in(
      "f7",
      new_struct("Node", fields = list(v = type_double()))
    )
    Node <- declare_in("f7", {
      new_struct("Node", fields = list(v = type_double(), nx = option(Node)))
    })
    chain <- function(n) {
      x <- Node(v = 0)
      for (i in seq_len(n)) {
        x <- Node(v = i, nx = x)
      }
      x
    }
    x <- chain(99L)
    expect_identical(r8_validate(x), x)
    expect_length(format(x), 201L)
    expect_true(r8_equal(r8_from_json(Node, r8_to_json(x)), x))
    expect_identical(r8_hash(unserialize(serialize(x, NULL))), r8_hash(x))
    expect_error(
      chain(200L),
      "at most 100 levels deep.*vec\\(\\)",
      class = "r8_value_error"
    )
    J <- declare_in("f7", new_struct("J", fields = list(j = type_json())))
    for (n in c(200L, 1e5L)) {
      expect_error(J(j = nested_list(n)), "too_deep", class = "r8_value_error")
      expect_error(
        r8_update(nested_list(n)),
        "too_deep",
        class = "r8_value_error"
      )
      expect_error(
        r8_from_json(
          J,
          paste0(
            "{\"$type\":\"f7::J\",\"$types\":{\"f7::J\":1},\"j\":",
            strrep("[", n),
            strrep("]", n),
            "}"
          )
        ),
        class = "r8_value_error"
      )
    }
  }
)

scenario(
  "F7",
  "a declaration without end, or that nests too deep, is an error",
  situation = "A user gives a field of a recursive type a default that leaves the field to its default, or builds a type by wrapping it many times.",
  risk = "The first value, or the declaration itself, ends the session.",
  result = "The declaration is refused, with the field and what to change.",
  code = {
    Node <- declare_in(
      "f8",
      new_struct("Node", fields = list(v = type_double()))
    )
    expect_error(
      declare_in("f8", {
        new_struct(
          "Node",
          fields = list(
            v = type_double(),
            nx = field(option(Node), default = Node(v = 1))
          )
        )
      }),
      "default_recursive",
      class = "r8_declaration_error"
    )
    nest <- function(n) {
      t <- type_double()
      for (i in seq_len(n)) {
        t <- vec(t)
      }
      declare_in("f8", new_struct("Deep", fields = list(a = t)))
    }
    expect_error(
      nest(150L),
      "at most 100 levels",
      class = "r8_declaration_error"
    )
    expect_error(nest(1e4L), class = "r8_declaration_error")
  }
)

scenario(
  "F8",
  "a method that calls itself without end is an error",
  situation = "A user's format, print, or generic method calls itself on the same value.",
  risk = "The calls go through R8's compiled code, and the session ends.",
  result = "R's own error of a recursion without end, and R8 works afterwards.",
  code = {
    out <- declare_in("f8b", {
      P <- new_struct("P", fields = list(x = type_double()))
      implement(format, P) <- function(x, ...) format(x)
      again <- generic("again", params(x, y = arg(P)))
      implement(again, P) <- function(x, y) again(x, y)
      list(P = P, again = again)
    })
    p <- out$P(x = 1)
    # A handler that R calls at the end of the stack has no room to run.
    caught <- function(e) tryCatch(e, error = function(e) "error")
    expect_identical(caught(format(p)), "error")
    expect_identical(caught(out$again(p, p)), "error")
    expect_identical(p$x, 1)
    expect_identical(r8_validate(p), p)
  }
)

scenario(
  "F9",
  "a text in another encoding is translated, or refused where it cannot be",
  situation = "A user reads a file that was written in Latin-1, or cuts a string inside a character, and gives it to a field, a document, or the name of a variant.",
  risk = "The bytes are taken for UTF-8: the session ends, or the text is stored, hashed, and written with other letters.",
  result = "A text that R marks as Latin-1 is translated, and has the hash and the document of the same text in UTF-8. Bytes that are no text are an error that gives the position and what reads the file.",
  code = {
    T <- declare_in("f9", {
      new_struct(
        "T",
        fields = list(
          s = type_character(),
          j = type_json(),
          m = option(index_map(type_double())),
          f = option(type_factor())
        )
      )
    })
    E <- declare_in("f9", {
      new_enum("E", variants = list(Warm = variant(), Cold = variant()))
    })
    latin1 <- "caf\xe9"
    Encoding(latin1) <- "latin1"
    unmarked <- "caf\xe9"
    cut <- rawToChar(charToRaw("W\u00e4rme")[1:2])
    Encoding(cut) <- "UTF-8"
    bytes <- "\xff\xfe"
    Encoding(bytes) <- "bytes"
    x <- T(
      s = latin1,
      j = list(latin1),
      m = setNames(1, latin1),
      f = factor(latin1)
    )
    y <- T(
      s = "caf\u00e9",
      j = list("caf\u00e9"),
      m = c("caf\u00e9" = 1),
      f = factor("caf\u00e9")
    )
    expect_true(r8_equal(x, y))
    expect_identical(r8_hash(x), r8_hash(y))
    expect_identical(r8_to_json(x), r8_to_json(y))
    for (bad in list(cut, bytes, unmarked)) {
      text <- "valid UTF-8"
      expect_error(T(s = bad, j = 1), text, class = "r8_value_error")
      expect_error(T(s = "a", j = list(bad)), text, class = "r8_value_error")
      expect_error(
        T(s = "a", j = setNames(list(1), bad)),
        text,
        class = "r8_value_error"
      )
      expect_error(
        T(s = "a", j = 1, m = setNames(1, bad)),
        text,
        class = "r8_value_error"
      )
      expect_error(r8_is_variant(E@Warm, bad), text)
    }
    doc <- paste0(
      "{\"$type\":\"f9::T\",\"$types\":{\"f9::T\":1},\"",
      unmarked,
      "\":1}"
    )
    expect_error(
      r8_from_json(T, doc),
      "latin1.*enc2utf8",
      class = "r8_type_error"
    )
    expect_error(
      r8_from_json(T, globalenv()),
      "readLines",
      class = "r8_type_error"
    )
  }
)

scenario(
  "F10",
  "a long call ends when a time limit is reached",
  situation = "A user sets a time limit, or interrupts, while R8 checks, hashes, or writes a value of hundreds of thousands of values, or filters a table of millions of rows.",
  risk = "The call goes on to its end, whatever the user does.",
  result = "The call ends with an error that says why, and R8 works afterwards.",
  code = {
    out <- declare_in("f10", {
      P <- new_struct("P", fields = list(x = type_double()))
      list(P = P, H = new_struct("H", fields = list(ps = vec(P))))
    })
    # R looks at the time every 0.05 s.
    h <- out$H(ps = rep(list(out$P(x = 1)), 8e5))
    r <- Raven(x = runif(3e6))
    for (f in list(
      r8_validate,
      r8_hash,
      r8_to_json,
      function(h) out$H(ps = h$ps),
      function(h) {
        r8_filter(r, rule(log(x + exp(x)) > 1 && nchar(paste0("", x)) > 2))
      }
    )) {
      message <- tryCatch(
        {
          setTimeLimit(elapsed = 0.01, transient = TRUE)
          f(h)
          "done"
        },
        error = conditionMessage
      )
      setTimeLimit()
      expect_match(message, "time limit")
    }
    expect_identical(r8_validate(out$P(x = 1)), out$P(x = 1))
  }
)

scenario(
  "F11",
  "data.table changes no value, whether the table was given or read",
  situation = "A user gives a data.table to a field, and turns a table that a field gives into a data.table. The user then changes both in place.",
  risk = "The value changes without a check, and holds what its type refuses.",
  result = "The value is as it was: the columns of a data.table given are copied, and a data.table made from what a field gives has columns of its own.",
  code = {
    skip_if_not_installed("data.table")
    W <- sharing_table("f11")
    dt <- data.table::data.table(a = as.numeric(1:1000))
    w <- W(tab = dt)
    before <- r8_hash(w)
    data.table::set(dt, 1L, "a", -1)
    data.table::setnames(dt, "a", "q")
    x <- w$tab
    data.table::setDT(x)
    data.table::set(x, 1L, "a", -1)
    y <- data.table::as.data.table(w$tab)
    data.table::set(y, j = "a", value = NULL)
    expect_identical(r8_hash(w), before)
    expect_identical(r8_validate(w), w)
  }
)

scenario(
  "F12",
  "a vector that data.table changes in place is found on demand",
  situation = "A user gives a data frame to a field, turns that data frame into a data.table with setDT(), and changes a column in place. Or the user calls set() on the data frame that a field gives.",
  risk = "The value changes without a check: the vectors are shared, since a copy of each would double the memory, and data.table does not ask R before it changes one.",
  result = "The value holds the change, and r8_validate() reports it. A large data.table that is changed in place is held in a field of host_class(), which R8 does not check, and r8_validate() checks it against a type where the code asks.",
  code = {
    skip_if_not_installed("data.table")
    W <- sharing_table("f12")
    df <- data.frame(a = runif(1000))
    w <- W(tab = df)
    data.table::setDT(df)
    data.table::set(df, 1L, "a", -1)
    expect_error(r8_validate(w), "0", class = "r8_value_error")
    w <- W(tab = data.frame(a = runif(1000)))
    data.table::set(w$tab, 1L, "a", -1)
    expect_error(r8_validate(w), "0", class = "r8_value_error")
    D <- declare_in("f12b", {
      new_struct("D", fields = list(data = option(host_class("data.table"))))
    })
    d <- D(data = data.table::data.table(a = c(1, -1)))
    data.table::set(d$data, 1L, "a", 2)
    expect_identical(d$data$a, c(2, -1))
    type <- type_table(columns = list(a = column(type_double(min = 0))))
    expect_error(r8_validate(d$data, type), "0", class = "r8_value_error")
  }
)
