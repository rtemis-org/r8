# r8-types.R
# ::r8benchR::
# R8::r8_document_native() writes this file from the R8 declarations in
# the package's Rust code, and replaces it when they change.

#' Wide20
#'
#' The struct `bench::Wide20`, declared in Rust.
#'
#' @param f1 The field `f1`.
#' @param f2 The field `f2`.
#' @param f3 The field `f3`.
#' @param f4 The field `f4`.
#' @param f5 The field `f5`.
#' @param f6 The field `f6`.
#' @param f7 The field `f7`.
#' @param f8 The field `f8`.
#' @param f9 The field `f9`.
#' @param f10 The field `f10`.
#' @param f11 The field `f11`.
#' @param f12 The field `f12`.
#' @param f13 The field `f13`.
#' @param f14 The field `f14`.
#' @param f15 The field `f15`.
#' @param f16 The field `f16`.
#' @param f17 The field `f17`.
#' @param f18 The field `f18`.
#' @param f19 The field `f19`.
#' @param f20 The field `f20`.
#' @return A `bench::Wide20` value.
#' @export
Wide20 <- R8::new_struct(
  "Wide20",
  fields = list(
    f1 = R8::field(R8::type_double(min = 0, max = 1), default = 0.1),
    f2 = R8::field(R8::type_double(min = 0, max = 1), default = 0.1),
    f3 = R8::field(R8::type_double(min = 0, max = 1), default = 0.1),
    f4 = R8::field(R8::type_double(min = 0, max = 1), default = 0.1),
    f5 = R8::field(R8::type_double(min = 0, max = 1), default = 0.1),
    f6 = R8::field(R8::type_double(min = 0, max = 1), default = 0.1),
    f7 = R8::field(R8::type_double(min = 0, max = 1), default = 0.1),
    f8 = R8::field(R8::type_double(min = 0, max = 1), default = 0.1),
    f9 = R8::field(R8::type_double(min = 0, max = 1), default = 0.1),
    f10 = R8::field(R8::type_double(min = 0, max = 1), default = 0.1),
    f11 = R8::field(R8::type_double(min = 0, max = 1), default = 0.1),
    f12 = R8::field(R8::type_double(min = 0, max = 1), default = 0.1),
    f13 = R8::field(R8::type_double(min = 0, max = 1), default = 0.1),
    f14 = R8::field(R8::type_double(min = 0, max = 1), default = 0.1),
    f15 = R8::field(R8::type_double(min = 0, max = 1), default = 0.1),
    f16 = R8::field(R8::type_double(min = 0, max = 1), default = 0.1),
    f17 = R8::field(R8::type_double(min = 0, max = 1), default = 0.1),
    f18 = R8::field(R8::type_double(min = 0, max = 1), default = 0.1),
    f19 = R8::field(R8::type_double(min = 0, max = 1), default = 0.1),
    f20 = R8::field(R8::type_double(min = 0, max = 1), default = 0.1)
  ),
  version = 1L,
  native = TRUE
)

#' GBM
#'
#' The struct `bench::GBM`, declared in Rust.
#'
#' @param learning_rate The field `learning_rate`.
#' @param subsample The field `subsample`.
#' @param colsample The field `colsample`.
#' @param n_trees The field `n_trees`.
#' @param max_depth The field `max_depth`.
#' @param objective The field `objective`.
#' @param num_class The field `num_class`.
#' @param early_stopping_rounds The field `early_stopping_rounds`.
#' @param seed The field `seed`.
#' @param verbose The field `verbose`.
#' @param weights The field `weights`.
#' @param feature_idx The field `feature_idx`.
#' @return A `bench::GBM` value.
#' @export
GBM <- R8::new_struct(
  "GBM",
  fields = list(
    learning_rate = R8::type_double(max = 1, exclusive_min = 0),
    subsample = R8::type_double(max = 1, exclusive_min = 0),
    colsample = R8::type_double(max = 1, exclusive_min = 0),
    n_trees = R8::type_integer(min = 1L),
    max_depth = R8::type_integer(min = 1L, max = 32L),
    objective = R8::type_character(pattern = "^(regression|binary|multiclass)$"),
    num_class = R8::option(R8::type_integer(min = 2L)),
    early_stopping_rounds = R8::option(R8::type_integer(min = 1L)),
    seed = R8::option(R8::type_integer()),
    verbose = R8::type_logical(),
    weights = R8::vec(R8::type_double(min = 0)),
    feature_idx = R8::vec(R8::type_integer(min = 1L))
  ),
  version = 1L,
  rules = list(
    R8::rule("early_stop_below_trees", early_stopping_rounds < n_trees, "early_stopping_rounds is below n_trees.", when = !is.null(early_stopping_rounds)),
    R8::rule("num_class_iff_multiclass", (objective == "multiclass") == !is.null(num_class), "num_class is present exactly when the objective is multiclass.")
  ),
  native = TRUE
)
