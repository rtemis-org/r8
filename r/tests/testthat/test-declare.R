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

test_that("a numeric type takes one bound per side", {
  expect_error(
    type_double(min = 0, exclusive_min = 0),
    "bound the same side",
    class = "r8_declaration_error"
  )
  expect_error(
    type_double(max = 1, exclusive_max = 2),
    "bound the same side",
    class = "r8_declaration_error"
  )
  expect_error(
    type_integer(min = 1L, exclusive_min = 0L),
    "bound the same side",
    class = "r8_declaration_error"
  )
  T <- declare_in("bounds", {
    new_struct(
      "Range",
      fields = list(
        p = type_double(exclusive_min = 0, max = 1),
        n = type_integer(min = 1L, exclusive_max = 10L)
      )
    )
  })
  expect_identical(T(p = 1, n = 9L)$n, 9L)
  expect_error(T(p = 0, n = 1L), "is not above 0", class = "r8_value_error")
  expect_error(T(p = 0.5, n = 10L), "is not below 10", class = "r8_value_error")
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

test_that("enum variants are built once and rebuilt after a redeclaration", {
  E <- declare_in(
    "variants",
    new_enum(
      "E",
      variants = list(
        A = variant(),
        B = variant(fields = list(x = type_double()))
      )
    )
  )
  expect_identical(E@B, E@B)
  expect_identical(E@A, E@A)
  expect_identical(E@B(x = 1)$x, 1)
  expect_error(E@C, "not a variant", class = "r8_value_error")
  E <- declare_in(
    "variants",
    new_enum(
      "E",
      variants = list(
        A = variant(),
        C = variant(fields = list(y = type_integer()))
      )
    )
  )
  expect_identical(E@C(y = 2L)$y, 2L)
  expect_error(E@B, "not a variant", class = "r8_value_error")
})

test_that("a type object outside a package carries its record, which registers it", {
  P <- declare_in("records", new_struct("P", fields = list(x = type_double())))
  record <- attr(P, "r8_record")
  expect_true(is.raw(record))
  expect_null(attr(P, "r8_ir"))
  expect_identical(rawToChar(record[1:3]), "R8D")
  # A damaged record is refused with the source named.
  bad <- record
  bad[length(bad)] <- xor(bad[length(bad)], as.raw(1L))
  expect_error(
    R8:::r8_declare_records(list(bad), "somepkg", TRUE),
    "package 'somepkg' do not read: the record's checksum does not match its content; reinstall 'somepkg'"
  )
  # Registering the record again changes nothing.
  before <- r8_declaration(P)
  R8:::register_records(list(record))
  expect_identical(r8_declaration(P), before)
})

test_that("a restored registry takes back the declarations made after its save", {
  R8:::r8_registry_save()
  Q <- declare_in("restore", new_struct("Q", fields = list(x = type_double())))
  expect_true(R8:::r8_is_registered("restore::Q"))
  R8:::r8_registry_restore()
  expect_false(R8:::r8_is_registered("restore::Q"))
  # The type object carries its record, which registers it again.
  expect_identical(Q(x = 1)$x, 1)
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

test_that("a failed declaration leaves the registry unchanged", {
  Point <- declare_in(
    "regfail",
    new_struct("Point", fields = list(x = type_double(min = 0)))
  )
  before <- r8_declaration(Point)
  # A failed replacement keeps the registered declaration.
  expect_error(
    declare_in(
      "regfail",
      new_struct(
        "Point",
        fields = list(x = field(type_double(min = 0), default = -1))
      )
    ),
    class = "r8_declaration_error"
  )
  expect_identical(r8_declaration(Point), before)
  expect_s3_class(Point(x = 1), "regfail::Point")
  # A failed new declaration registers nothing.
  expect_error(
    declare_in(
      "regfail",
      new_struct(
        "Line",
        fields = list(n = field(type_integer(min = 1L), default = 0L))
      )
    ),
    class = "r8_declaration_error"
  )
  expect_error(R8:::r8_type_document("regfail::Line"), "not registered")
  # A failed first declaration in a namespace leaves no document for it.
  expect_error(
    declare_in(
      "regfresh",
      new_struct(
        "Bad",
        fields = list(n = field(type_integer(min = 1L), default = 0L))
      )
    ),
    class = "r8_declaration_error"
  )
  expect_error(R8:::r8_type_document("regfresh::Bad"), "not registered")
  expect_length(R8:::r8_check_namespaces("regfresh")[["code"]], 0L)
  # An identical declaration is accepted again.
  expect_no_error(declare_in(
    "regfail",
    new_struct("Point", fields = list(x = type_double(min = 0)))
  ))
  expect_identical(r8_declaration(Point), before)
})

test_that("a type and a trait of one name are refused in either order", {
  declare_in(
    "collide",
    new_trait("Size", methods = list(total = trait_method(function(x) NULL)))
  )
  expect_error(
    declare_in("collide", new_struct("Size", fields = list(a = type_double()))),
    "duplicate_type",
    class = "r8_declaration_error"
  )
  declare_in("collide", new_struct("Shape", fields = list(a = type_double())))
  expect_error(
    declare_in(
      "collide",
      new_trait("Shape", methods = list(area = trait_method(function(x) NULL)))
    ),
    "duplicate_type"
  )
})

test_that("the check of a whole namespace finds a type a replacement invalidates", {
  declare_in("replaced", {
    B <- new_struct("B", fields = list(x = type_double()))
    new_struct(
      "A",
      fields = list(b = B),
      rules = list(rule("POSITIVE", b$x > 0, "b$x is positive."))
    )
  })
  expect_length(R8:::r8_check_namespaces("replaced")[["code"]], 0L)
  # B declared again without `x`: A's rule reads a field that B lacks.
  declare_in("replaced", new_struct("B", fields = list(y = type_double())))
  p <- R8:::r8_check_namespaces("replaced")
  expect_true(length(p[["code"]]) > 0L)
  expect_true(all(startsWith(p[["path"]], "/types/1/")))
})

test_that("declaring a type takes time independent of the namespace's size", {
  declare <- function(i) {
    declare_in(
      "scaling",
      new_struct(
        paste0("T", i),
        fields = list(a = type_double(min = 0), b = vec(type_integer())),
        rules = list(rule("ORDER", a >= 0, "a is not negative."))
      )
    )
  }
  first <- system.time(
    for (i in 1:50) {
      declare(i)
    }
  )[["elapsed"]]
  for (i in 51:400) {
    declare(i)
  }
  last <- system.time(
    for (i in 401:450) {
      declare(i)
    }
  )[["elapsed"]]
  # Quadratic declaration made the last 50 about 8 times slower than the
  # first; linear, about the same.
  expect_lt(last, 3 * first + 0.05)
})
