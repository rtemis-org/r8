# Extracted from test-values.R:86

# prequel ----------------------------------------------------------------------
svm_types <- function() {
  declare_in("rtemis", {
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
        rule(
          kernel$gamma <= 10,
          message = "gamma is at most 10.",
          id = "RBF_GAMMA"
        ),
        rule(
          cost <= 100,
          when = is_variant(kernel, "Rbf"),
          message = "cost is at most 100 with the Rbf kernel.",
          id = "RBF_COST"
        )
      ),
      description = "Support vector machine settings."
    )
    list(Kernel = Kernel, Svm = Svm)
  })
}

# test -------------------------------------------------------------------------
t <- svm_types()
Svm <- t[["Svm"]]
Kernel <- t[["Kernel"]]
e <- expect_error(Svm(cost = -1, mtry = 0L), class = "r8_value_error")
expect_match(conditionMessage(e), "below_min at /cost")
