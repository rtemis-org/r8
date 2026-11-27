# test-104-dyn.R
# ::R8::
# 2026- EDG rtemis.org

# Fields that hold a value of any type that implements a trait, dyn(Trait),
# or of any struct that extends another, subtype(Struct).
#
# spec: R8/class-system#dyn

dyn_types <- function(ns) {
  declare_in(ns, {
    Algorithm <- new_trait(
      "Algorithm",
      methods = list(fit = trait_method(params(x))),
      tag = "algorithm"
    )
    Plain <- new_trait(
      "Plain",
      methods = list(label = trait_method(params(x)))
    )
    Glmnet <- new_struct(
      "Glmnet",
      fields = list(alpha = type_double(min = 0, max = 1))
    )
    Cart <- new_struct("Cart", fields = list(depth = type_integer()))
    Pruned <- new_struct(
      "Pruned",
      fields = list(prune = type_logical()),
      extends = Cart
    )
    Point <- new_struct("Point", fields = list(x = type_double()))
    Color <- new_enum(
      "Color",
      variants = list(Red = variant(), Blue = variant())
    )
    impl(
      Algorithm,
      Glmnet,
      methods = list(fit = function(x) "glmnet"),
      tag_value = "GLMNET"
    )
    impl(Algorithm, Cart, methods = list(fit = function(x) "cart"))
    impl(Plain, Cart, methods = list(label = function(x) "cart"))
    impl(Plain, Color, methods = list(label = function(x) "color"))
    Model <- new_struct(
      "Model",
      fields = list(
        algo = option(dyn(Algorithm)),
        plain = option(dyn(Plain)),
        base = option(subtype(Cart))
      )
    )
    environment()
  })
}

test_that("a dyn() field holds an implementor, and a subtype() field a struct or a descendant", {
  e <- dyn_types("dyn1")
  m <- e$Model(
    algo = e$Glmnet(alpha = 0.5),
    plain = e$Pruned(depth = 2L, prune = TRUE)
  )
  expect_s3_class(m$algo, "dyn1::Glmnet")
  expect_error(
    e$Model(plain = e$Point(x = 1)),
    "not_implementor|does not implement",
    class = "r8_value_error"
  )
  expect_s3_class(
    e$Model(base = e$Pruned(depth = 1L, prune = FALSE)),
    "r8_value"
  )
  expect_error(e$Model(base = e$Point(x = 1)), class = "r8_value_error")
})

test_that("a dyn() value round-trips by tag value, by $type, and as a unit-only enum", {
  e <- dyn_types("dyn2")
  m <- e$Model(
    algo = e$Glmnet(alpha = 0.5),
    plain = e$Color@Red,
    base = e$Pruned(depth = 3L, prune = TRUE)
  )
  json <- r8to_json(m)
  expect_match(json, "\"algorithm\":\"GLMNET\"", fixed = TRUE)
  expect_match(json, "\"$type\":\"dyn2::Color\",\"type\":\"Red\"", fixed = TRUE)
  expect_match(json, "\"$type\":\"dyn2::Pruned\"", fixed = TRUE)
  back <- r8from_json(e$Model, json)
  expect_true(r8equal(back, m))
  expect_identical(r8hash(back), r8hash(m))
  # A descendant without its own impl writes its type name as its tag.
  p <- e$Model(algo = e$Pruned(depth = 1L, prune = FALSE))
  expect_match(r8to_json(p), "\"algorithm\":\"Pruned\"", fixed = TRUE)
  expect_true(r8equal(r8from_json(e$Model, r8to_json(p)), p))
})

test_that("tag values are names and unique among a trait's implementors", {
  e <- dyn_types("dyn3")
  declare_in("dyn3", {
    Other <- new_struct("Other", fields = list(k = type_double()))
    expect_error(
      impl(
        e$Algorithm,
        Other,
        methods = list(fit = function(x) NULL),
        tag_value = "GLMNET"
      ),
      "duplicate_tag"
    )
    expect_error(
      impl(
        e$Algorithm,
        Other,
        methods = list(fit = function(x) NULL),
        tag_value = "a-b"
      ),
      "tag_value_invalid"
    )
    expect_error(
      impl(
        e$Plain,
        Other,
        methods = list(label = function(x) NULL),
        tag_value = "O"
      ),
      "tag_without_member"
    )
  })
})

test_that("dyn() takes a trait or a generic, and subtype() a struct", {
  e <- dyn_types("dyn4")
  expect_error(dyn(e$Color), "is an enum", class = "r8_declaration_error")
  expect_error(dyn(e$Cart), "subtype\\(Cart\\)", class = "r8_declaration_error")
  expect_error(
    subtype(e$Algorithm),
    "dyn\\(\\) of the trait",
    class = "r8_declaration_error"
  )
  expect_identical(unclass(subtype(e$Cart))[["kind"]], "subtype")
  expect_error(
    dyn("dyn4::Cart"),
    "takes a trait object",
    class = "r8_declaration_error"
  )
  g <- declare_in("dyn4", generic("g", params(x)))
  expect_identical(unclass(dyn(g))[["trait"]], "dyn4::g")
})

test_that("a document naming an unregistered type names its namespace", {
  e <- dyn_types("dyn5")
  json <- paste0(
    "{\"$type\":\"dyn5::Model\",\"$types\":{\"dyn5::Model\":1,\"nopkg::Thing\":1},",
    "\"plain\":{\"$type\":\"nopkg::Thing\",\"a\":1}}"
  )
  err <- tryCatch(r8from_json(e$Model, json), error = function(e) e)
  expect_s3_class(err, "r8_unknown_type_error")
  expect_match(
    conditionMessage(err),
    "no installed package declares namespace `nopkg`",
    fixed = TRUE
  )
  expect_error(
    r8from_json(e$Model, json, load = "no.such.package.r8"),
    "is not installed",
    class = "r8_type_error"
  )
})

test_that("r8schema() writes a dyn() field open or as its registered implementors", {
  e <- dyn_types("dyn6")
  open <- R8:::r8_json_parse(r8schema(e$Model))
  algo <- open[["properties"]][["algo"]][["anyOf"]][[1L]]
  expect_identical(algo[["x-r8"]][["dyn"]], "dyn6::Algorithm")
  closed <- R8:::r8_json_parse(r8schema(e$Model, closed = TRUE))
  alts <- closed[["properties"]][["algo"]][["anyOf"]][[1L]][["anyOf"]]
  tags <- vapply(
    alts,
    function(a) a[["properties"]][["algorithm"]][["const"]],
    ""
  )
  expect_setequal(tags, c("GLMNET", "Cart", "Pruned"))
})

test_that("a value that is not a subtype or an implementor is named by what it is", {
  t <- declare_in("dynmsg", {
    Range <- new_struct("Range", fields = list(lo = type_double()))
    Other <- new_struct("Other")
    Planar <- new_trait(
      "Planar",
      methods = list(area = trait_method(params(x)))
    )
    impl(Planar, Range, methods = list(area = function(x) x$lo))
    S <- new_struct(
      "S",
      fields = list(r = option(subtype(Range)), p = option(dyn(Planar)))
    )
    list(S = S, Other = Other)
  })
  S <- t[["S"]]
  refused <- function(expr, text) {
    expect_error(expr, text, fixed = TRUE, class = "r8_value_error")
  }
  refused(
    S(r = c(2, 5)),
    "`r` must be a value of `dynmsg::Range` or of a struct that extends it; found a numeric vector. [not_subtype]"
  )
  refused(
    S(r = t[["Other"]]()),
    "`r` must be a value of `dynmsg::Range` or of a struct that extends it; found a value of `dynmsg::Other`. [not_subtype]"
  )
  refused(
    S(p = data.frame()),
    "`p` must be a value of a type that implements `dynmsg::Planar`; found an object of class \"data.frame\". [not_implementor]"
  )
  refused(
    S(p = t[["Other"]]()),
    "found a value of `dynmsg::Other`. [not_implementor]"
  )
})
