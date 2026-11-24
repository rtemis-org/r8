# test-703-limits.R
# ::R8::
# 2026- EDG rtemis.org

# Scenarios of limits: values, declarations, and lists that nest deeper than the
# stack of a process holds, and arguments that R8's own routines do not expect.
# Each ended the R session before it was an error.

nested_list <- function(n) {
  out <- list(1)
  for (i in seq_len(n)) {
    out <- list(out)
  }
  out
}

scenario(
  "F7",
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
  "F8",
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
  "F9",
  "a method that calls itself without end is an error",
  situation = "A user's format, print, or generic method calls itself on the same value.",
  risk = "The calls go through R8's compiled code, and the session ends.",
  result = "R's own error of a recursion without end, and R8 works afterwards.",
  code = {
    out <- declare_in("f9", {
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
