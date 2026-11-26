# test-201-session.R
# ::R8::
# 2026- EDG rtemis.org

# Scenarios of one session: a user declares a type at the console, builds
# values, and then changes the declaration.
#
# A script that is run again, and an exploration at the console, declare a type
# several times. The values of the earlier declaration are still there, and so
# are the structs that extend the type and the methods that take it.

scenario(
  "A1",
  "a type declared again unchanged changes nothing",
  situation = "A user runs the same script twice in one session.",
  risk = "The values that the first run built are refused, or differ from those of the second.",
  result = "The values are valid, and equal those of the second run.",
  code = {
    point <- function() {
      declare_in("a1", {
        new_struct("Point", fields = list(x = type_double(), y = type_double()))
      })
    }
    p <- point()(x = 1, y = 2)
    q <- point()(x = 1, y = 2)
    expect_identical(r8_validate(p), p)
    expect_true(r8_equal(p, q))
    expect_identical(r8_hash(p), r8_hash(q))
  }
)

scenario(
  "A2",
  "a value of an earlier declaration can be looked at, and is used once it is updated",
  situation = "A user declares a type again with its fields in another order, and then reads, changes, and compares a value built before.",
  risk = "A write replaces another field than the one it names, since each field has another position.",
  result = "The value prints and gives its fields by name. A write, a comparison, and a document are errors that say what differs. r8_update() gives the value under the new declaration.",
  code = {
    p <- declare_in("a2", {
      new_struct(
        "Point",
        fields = list(x = type_double(min = 0), y = type_double())
      )
    })(x = 6, y = 2)
    Point <- declare_in("a2", {
      new_struct(
        "Point",
        fields = list(y = type_double(), x = type_double(min = 0))
      )
    })
    expect_identical(p$x, 6)
    expect_identical(as.list(p), list(x = 6, y = 2))
    shown <- paste(format(p), collapse = " ")
    expect_match(shown, "x: 6")
    expect_match(shown, "in the order x, y, and are in the order y, x")
    other <- "another declaration.*in the order x, y.*r8_update\\(\\) gives"
    expect_error(p$y <- 9, other, class = "r8_value_error")
    expect_error(p["y"] <- list(9), other, class = "r8_value_error")
    expect_error(p[["y"]] <- 9, other, class = "r8_value_error")
    expect_error(
      r8_equal(p, Point(x = 6, y = 2)),
      other,
      class = "r8_other_declaration_error"
    )
    expect_error(r8_to_json(p), other, class = "r8_value_error")
    q <- r8_update(p)
    expect_true(r8_equal(q, Point(x = 6, y = 2)))
    expect_identical(q, Point(x = 6, y = 2))
    q$y <- 9
    expect_identical(as.list(q), list(y = 9, x = 6))
    expect_identical(r8_update(q), q)
  }
)

scenario(
  "A3",
  "a value that the new declaration does not admit is not updated",
  situation = "A user declares a type again with a tighter bound, or with a new field that has no default. A value built before is outside the bound, and lacks the field.",
  risk = "The value is written or hashed as one of the new declaration, or becomes a field of another value.",
  result = "Every use of the value is an error, and so is r8_update(), which names what the value lacks or violates.",
  code = {
    p <- declare_in("a3", {
      new_struct("Point", fields = list(x = type_double(min = 0)))
    })(x = 3)
    Point <- declare_in("a3", {
      new_struct(
        "Point",
        fields = list(x = type_double(min = 5), z = type_double())
      )
    })
    Holder <- declare_in("a3", {
      new_struct("Holder", fields = list(p = Point))
    })
    expect_error(
      r8_validate(p),
      "declaration_changed",
      class = "r8_value_error"
    )
    expect_error(r8_to_json(p), "declaration_changed", class = "r8_value_error")
    expect_error(r8_hash(p), "declaration_changed", class = "r8_value_error")
    expect_error(Holder(p = p), "declaration_changed", class = "r8_value_error")
    expect_error(p$x <- 6, "declaration_changed", class = "r8_value_error")
    expect_error(
      r8_update(p),
      "`z` was added without a default.*lacks a field",
      class = "r8_other_declaration_error"
    )
    declare_in("a3", {
      new_struct(
        "Point",
        fields = list(x = type_double(min = 5), z = option(type_double()))
      )
    })
    expect_error(
      r8_update(p),
      "`x` must be at least 5",
      class = "r8_value_error"
    )
  }
)

scenario(
  "A4",
  "a method refuses a value of an earlier declaration",
  situation = "A user declares a type again with a tighter bound, and passes a value built before, outside the bound, to a function that takes the type, to a generic that dispatches on it, and to a method of a base generic.",
  risk = "The function receives a value outside the bound.",
  result = "Each call is an error that says what differs.",
  code = {
    out <- declare_in("a4", {
      Dose <- new_struct("Dose", fields = list(mg = type_double(min = 0)))
      give <- generic("give", params(x, dose = arg(Dose)))
      implement(give, "numeric") <- function(x, dose) x * dose$mg
      size <- generic("size", params(x))
      implement(size, Dose) <- function(x) x$mg
      implement(summary, Dose) <- function(object, ...) object$mg
      list(Dose = Dose, give = give, size = size)
    })
    d <- out$Dose(mg = 500)
    calls <- function() {
      c(out$give(2, d), out$size(d), summary(d))
    }
    expect_identical(calls(), c(1000, 500, 500))
    declare_in("a4", {
      Dose <- new_struct(
        "Dose",
        fields = list(mg = type_double(min = 0, max = 100))
      )
    })
    other <- "was double \\[0, Inf\\), and is double \\[0, 100\\]"
    expect_error(out$give(2, d), other, class = "r8_error")
    expect_error(out$size(d), other, class = "r8_other_declaration_error")
    expect_error(summary(d), other, class = "r8_other_declaration_error")
    # A value of the new declaration, and the earlier declaration again.
    expect_error(r8_update(d), "at most 100", class = "r8_value_error")
    d2 <- out$Dose(mg = 50)
    expect_identical(
      c(out$give(2, d2), out$size(d2), summary(d2)),
      c(100, 50, 50)
    )
    declare_in("a4", {
      Dose <- new_struct("Dose", fields = list(mg = type_double(min = 0)))
    })
    expect_identical(calls(), c(1000, 500, 500))
  }
)

scenario(
  "A5",
  "a struct is refused once the struct it extends changes",
  situation = "A user declares a struct and another that extends it, builds a value of the child, and declares the parent again with another field.",
  risk = "The child keeps the fields of the earlier parent, and methods of the parent meet a value of another shape.",
  result = "The declaration warns that the child is stale. The child is refused until it is declared again.",
  code = {
    ty <- declare_in("a5", {
      Base <- new_struct("Base", fields = list(a = type_double()))
      Child <- new_struct(
        "Child",
        fields = list(b = type_double()),
        extends = Base
      )
      list(Base = Base, Child = Child)
    })
    x <- ty$Child(a = 1, b = 2)
    expect_warning(
      declare_in("a5", {
        Base <- new_struct(
          "Base",
          fields = list(a = type_double(), c = type_double())
        )
      }),
      "stale|Child",
      class = "r8_stale_warning"
    )
    expect_error(ty$Child(a = 1, b = 2), "type_stale", class = "r8_error")
    expect_error(r8_validate(x), "type_stale", class = "r8_error")
  }
)

scenario(
  "A6",
  "a value of a variant that is gone is refused where R8 checks it",
  situation = "A user declares an enum again without one of its variants. A value of that variant was built before.",
  risk = "Code that handles every variant meets one that it does not know.",
  result = "Validation, a document, and r8_update() are errors that name the variant.",
  code = {
    Kind <- declare_in("a6", {
      new_enum(
        "Kind",
        variants = list(A = variant(), B = variant(), C = variant())
      )
    })
    k <- Kind@C
    declare_in("a6", {
      new_enum("Kind", variants = list(A = variant(), B = variant()))
    })
    expect_identical(r8_variant(k), "C")
    gone <- "the variant `C` was removed"
    expect_error(r8_validate(k), gone, class = "r8_value_error")
    expect_error(r8_to_json(k), gone, class = "r8_value_error")
    expect_error(r8_update(k), gone, class = "r8_other_declaration_error")
  }
)

scenario(
  "A7",
  "a changed rule applies to a value once it is updated",
  situation = "A user declares a type again with a stricter rule, and uses values built before, of which one violates the rule.",
  risk = "The earlier rule still runs, or no rule does.",
  result = "The values are refused until r8_update(), which runs the new rule.",
  code = {
    I <- declare_in("a7", {
      new_struct("I", fields = list(lo = type_double(), hi = type_double()))
    })
    bad <- I(lo = 5, hi = 1)
    good <- I(lo = 1, hi = 5)
    declare_in("a7", {
      new_struct(
        "I",
        fields = list(lo = type_double(), hi = type_double()),
        rules = rule(lo <= hi)
      )
    })
    differ <- "the rules of `a7::I` differ"
    expect_error(r8_validate(good), differ, class = "r8_value_error")
    expect_error(good$hi <- 9, differ, class = "r8_value_error")
    expect_error(r8_update(bad), "rule_violation", class = "r8_value_error")
    x <- r8_update(list(a = good, b = 1))
    expect_identical(x$b, 1)
    expect_error(x$a$hi <- 0, "rule_violation", class = "r8_value_error")
    x$a$hi <- 9
    expect_identical(x$a$hi, 9)
  }
)

scenario(
  "A8",
  "a method or a generic declared again replaces the earlier one",
  situation = "A user corrects the body of a method and runs its declaration again, then declares the generic again and the method after it.",
  risk = "Calls run the earlier method, which the dispatch of R8 keeps for speed.",
  result = "The next call runs the method that was declared last.",
  code = {
    P <- declare_in("a8", new_struct("P", fields = list(a = type_double())))
    size <- declare_in("a8", generic("size", params(x)))
    declare_in("a8", implement(size, P) <- function(x) "first")
    expect_identical(size(P(a = 1)), "first")
    declare_in("a8", implement(size, P) <- function(x) "second")
    expect_identical(size(P(a = 1)), "second")
    size <- declare_in("a8", generic("size", params(x)))
    declare_in("a8", implement(size, P) <- function(x) "third")
    expect_identical(size(P(a = 1)), "third")
  }
)


scenario(
  "A9",
  "an agent calls a typed function from a document",
  situation = "A user declares a typed function with described parameters, gives an agent its schema, and runs the documents of a call that the agent writes.",
  risk = "A document runs a function that the user did not offer, or runs with an argument of the wrong type, or reports one problem at a time.",
  result = "The schema states each parameter with its type, default, and description. A document of a call runs only a function the user named, and a document with problems is refused with every problem, each at its path in the document, before the function runs.",
  code = {
    clip <- declare_in("a9", {
      new_function(
        "clip",
        function(
          x = arg_vec(type_double(), description = "The values."),
          lo = arg_double(default = 0, description = "The lower bound."),
          hi = arg_double(description = "The upper bound.")
        ) {
          pmin(pmax(x, lo), hi)
        },
        rules = list(order = rule(lo <= hi))
      )
    })
    schema <- gsub("[[:space:]]+", " ", r8_schema(clip))
    expect_match(
      schema,
      '"hi": { "type": "number", "description": "The upper bound." }',
      fixed = TRUE
    )
    expect_match(schema, '"required": [ "x", "hi" ]', fixed = TRUE)
    expect_identical(
      r8_call_from_json(
        clip,
        '{"$function": "a9::clip", "arguments": {"x": [-1, 2], "hi": 1}}'
      ),
      c(0, 1)
    )
    e <- expect_error(
      r8_call_from_json(
        clip,
        '{"$function": "a9::clip", "arguments": {"x": ["a"], "lo": 2, "hi": "b"}}'
      ),
      class = "r8_argument_error"
    )
    expect_setequal(
      e[["problems"]][["path"]],
      c("/arguments/x/0", "/arguments/hi")
    )
    expect_identical(
      r8_check_call(clip, '{"$function": "base::system", "arguments": {}}')[[
        "code"
      ]],
      "unknown_function"
    )
  }
)
