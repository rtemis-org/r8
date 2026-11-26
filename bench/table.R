# table.R
# ::r8 bench::
# 2026- EDG rtemis.org

# The comparison of `vec(S)` of 10^6 values with `type_table(S)` of 10^6
# rows in R8, for S a struct of two doubles: a list of R8 values against a
# data frame of two double columns.
# spec: R8/class-system#benchmark

table_types <- function() {
  op <- options(r8.namespace = "benchtable")
  on.exit(options(op))
  S <- R8::new_struct("S", fields = list(lower = R8::type_double(), upper = R8::type_double()))
  list(
    S = S,
    Vecs = R8::new_struct("Vecs", fields = list(items = R8::vec(S))),
    Rows = R8::new_struct("Rows", fields = list(items = R8::type_table(of = S)))
  )
}

table_input <- function(n) {
  set.seed(2026L)
  lower <- stats::runif(n)
  list(lower = lower, upper = lower + stats::runif(n))
}

table_as_vec <- function(input, types = table_types()) {
  items <- lapply(seq_along(input$lower), function(i) types$S(lower = input$lower[[i]], upper = input$upper[[i]]))
  types$Vecs(items = items)
}

table_as_table <- function(input, types = table_types()) {
  types$Rows(items = data.frame(lower = input$lower, upper = input$upper))
}
