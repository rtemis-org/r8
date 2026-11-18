# test-declare.R
# ::R8::
# 2026- EDG rtemis.org

test_that("Kernel and Svm declared in R produce the IR of the fixture", {
  types <- declare_in("rtemis", {
    Kernel <- new_enum(
      "Kernel",
      variants = list(
        Linear = variant(),
        Rbf = variant(
          fields = list(
            gamma = field(
              type_double(exclusive_min = 0),
              description = "Kernel width."
            )
          )
        )
      ),
      description = "Kernel function."
    )
    Svm <- new_struct(
      "Svm",
      fields = list(
        cost = field(
          type_double(exclusive_min = 0),
          default = 1,
          description = "Cost of a margin violation."
        ),
        kernel = field(
          Kernel,
          default = Kernel@Linear,
          description = "Kernel function."
        ),
        weights = field(
          option(vec(type_double(min = 0))),
          description = "Case weights.",
          external = TRUE
        ),
        mtry = field(
          option(type_integer(min = 1L), resolve = "runtime"),
          description = "Variables per split."
        )
      ),
      rules = list(
        rule("RBF_GAMMA", kernel$gamma <= 10, "gamma is at most 10."),
        rule(
          "RBF_COST",
          cost <= 100,
          "cost is at most 100 with the Rbf kernel.",
          when = is_variant(kernel, "Rbf")
        )
      ),
      description = "Support vector machine settings.",
      version = 1L
    )
    list(Kernel = Kernel, Svm = Svm)
  })
  expect_identical(
    R8:::r8_canonical_json(r8_declaration(types[["Kernel"]])),
    fixture_canonical("kernel.ir.json")
  )
  expect_identical(
    R8:::r8_canonical_json(r8_declaration(types[["Svm"]])),
    fixture_canonical("svm.ir.json")
  )
  expect_s3_class(types[["Svm"]], c("r8_struct_type", "r8_type"))
  expect_identical(
    names(formals(types[["Svm"]])),
    c("cost", "kernel", "weights", "mtry")
  )
})

test_that("rules compile from R expressions to JSONLogic", {
  compile <- function(e) R8:::compile_rule_expr(e)
  expect_identical(
    compile(quote(kernel$gamma <= 10)),
    list("<=" = list(list(var = "kernel.gamma"), 10))
  )
  expect_identical(
    compile(quote(a == 1L)),
    list("===" = list(list(var = "a"), 1L))
  )
  expect_identical(
    compile(quote(is.null(a) || !is.null(b))),
    list(
      or = list(
        list(is_null = list(list(var = "a"))),
        list("!" = list(list(is_null = list(list(var = "b")))))
      )
    )
  )
  expect_identical(
    compile(quote(x %in% c("a", "b"))),
    list("in" = list(list(var = "x"), list("a", "b")))
  )
  expect_identical(
    compile(quote(all(assays, nrow(.x$values) == n))),
    list(
      all = list(
        list(var = "assays"),
        list(
          "===" = list(
            list(nrow = list(list(var = ".x.values"))),
            list(var = "n")
          )
        )
      )
    )
  )
  expect_error(compile(quote(mean(x) > 1)), class = "r8_declaration_error")
  expect_error(compile(quote(x == NA)), class = "r8_declaration_error")
})

test_that("declaration errors name the problem and its location", {
  expect_error(
    declare_in("demo", new_struct("class", fields = list(x = type_double()))),
    "reserved word",
    class = "r8_declaration_error"
  )
  e <- expect_error(
    declare_in("demo", {
      K <- new_enum(
        "K",
        variants = list(
          A = variant(),
          B = variant(fields = list(g = type_double()))
        )
      )
      new_struct(
        "S",
        fields = list(k = K),
        rules = list(rule("R", k == "B", "m"))
      )
    }),
    class = "r8_declaration_error"
  )
  expect_match(conditionMessage(e), "is_variant")
  expect_match(conditionMessage(e), "rule_type")
  expect_error(
    declare_in(
      "demo",
      new_struct(
        "S2",
        fields = list(
          dose_mg = field(type_double(), unit = "mg"),
          max_g = field(type_double(), unit = "g")
        ),
        rules = list(rule("DOSE", dose_mg <= max_g, "m"))
      )
    ),
    "rule_unit",
    class = "r8_declaration_error"
  )
  expect_error(
    declare_in(
      "demo",
      new_struct(
        "S3",
        fields = list(x = field(type_integer(min = 1L), default = 0L))
      )
    ),
    "default_invalid"
  )
})

test_that("a unit variant is an R8 value, and an unknown variant is an error", {
  K <- declare_in(
    "demo",
    new_enum("Shape", variants = list(Circle = variant(), Square = variant()))
  )
  v <- K@Circle
  expect_s3_class(v, c("demo::Shape", "r8_value"))
  expect_identical(attr(v, "r8_variant"), "Circle")
  expect_error(K@Hexagon, class = "r8_value_error")
})

test_that("re-declaring a type replaces it", {
  declare_in("demo", new_struct("Again", fields = list(x = type_double())))
  T2 <- declare_in(
    "demo",
    new_struct("Again", fields = list(x = type_double(), y = type_logical()))
  )
  expect_match(r8_declaration(T2), "\"y\"")
})

test_that("a computed field compiles its expression", {
  T <- declare_in(
    "demo",
    new_struct(
      "Dose",
      fields = list(
        dose_mg = field(type_double(), unit = "mg"),
        dose_g = field(
          option(type_double(), resolve = "computed", compute = dose_mg / 1000),
          unit = "g"
        )
      )
    )
  )
  expect_match(r8_declaration(T), "\"policy\":\"computed\"")
  expect_error(
    declare_in(
      "demo",
      new_struct(
        "Dose2",
        fields = list(
          dose_mg = field(type_double(), unit = "mg"),
          dose_g = field(
            option(type_double(), resolve = "computed", compute = dose_mg + 1),
            unit = "g"
          )
        )
      )
    ),
    "rule_unit"
  )
})

test_that("every valid name can be a field, a variant, or a method", {
  T <- declare_in(
    "demo",
    new_struct(
      "Names",
      fields = list(
        n = type_integer(),
        name = type_character(),
        description = type_character(),
        version = type_integer(),
        rules = type_json()
      )
    )
  )
  x <- T(n = 1L, name = "a", description = "b", version = 2L, rules = list())
  expect_identical(x$n, 1L)
  expect_identical(x$description, "b")
  E <- declare_in(
    "demo",
    new_enum(
      "Kinds",
      variants = list(
        description = variant(),
        tag = variant(fields = list(of = type_double()))
      )
    )
  )
  expect_identical(attr(E@description, "r8_variant"), "description")
  Tr <- declare_in(
    "demo",
    new_trait("Named", methods = list(type = trait_method(function(x) NULL)))
  )
  expect_s3_class(Tr@type, "r8_generic")
})

test_that("member lists are named and their names are distinct", {
  expect_error(
    declare_in(
      "demo",
      new_struct("Dup", fields = list(a = type_double(), a = type_logical()))
    ),
    "twice",
    class = "r8_declaration_error"
  )
  expect_error(
    declare_in("demo", new_struct("Unnamed", fields = list(type_double()))),
    "named",
    class = "r8_declaration_error"
  )
  expect_error(
    declare_in(
      "demo",
      new_enum("DupV", variants = list(A = variant(), A = variant()))
    ),
    "twice",
    class = "r8_declaration_error"
  )
})

test_that("qualified rule functions compile like bare ones, and is_variant() tests R values", {
  expect_identical(
    R8:::compile_rule_expr(quote(R8::is_variant(k, "A"))),
    R8:::compile_rule_expr(quote(is_variant(k, "A")))
  )
  expect_identical(
    R8:::compile_rule_expr(quote(base::is.null(a))),
    list(is_null = list(list(var = "a")))
  )
  K <- declare_in(
    "demo",
    new_enum("Ks", variants = list(A = variant(), B = variant()))
  )
  expect_true(is_variant(K@A, "A"))
  expect_false(is_variant(K@A, "B"))
  expect_error(is_variant(1, "A"), class = "r8_type_error")
})
