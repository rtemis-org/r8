//! The Rust-stored types of the R8 benchmark (`bench/` of the r8
//! repository): `bench::Wide20` and `bench::GBM`, declared as `bench/types.R`
//! declares them in R, and the impl of `bench::Complexity`, a trait declared
//! in R, for `GBM`. The structs carry plain comments, not doc comments, so
//! that their declarations, descriptions included, equal the R ones.

use rtemis_r8::R8;
use rtemis_r8_api as r8;
use rtemis_r8_api::Buffer;

// Twenty doubles in [0, 1], each 0.1 by default.
#[derive(R8, Clone)]
#[r8(namespace = "bench", version = 1, storage = "rust")]
pub struct Wide20 {
    #[r8(min = 0, max = 1, default = "0.1")]
    pub f1: f64,
    #[r8(min = 0, max = 1, default = "0.1")]
    pub f2: f64,
    #[r8(min = 0, max = 1, default = "0.1")]
    pub f3: f64,
    #[r8(min = 0, max = 1, default = "0.1")]
    pub f4: f64,
    #[r8(min = 0, max = 1, default = "0.1")]
    pub f5: f64,
    #[r8(min = 0, max = 1, default = "0.1")]
    pub f6: f64,
    #[r8(min = 0, max = 1, default = "0.1")]
    pub f7: f64,
    #[r8(min = 0, max = 1, default = "0.1")]
    pub f8: f64,
    #[r8(min = 0, max = 1, default = "0.1")]
    pub f9: f64,
    #[r8(min = 0, max = 1, default = "0.1")]
    pub f10: f64,
    #[r8(min = 0, max = 1, default = "0.1")]
    pub f11: f64,
    #[r8(min = 0, max = 1, default = "0.1")]
    pub f12: f64,
    #[r8(min = 0, max = 1, default = "0.1")]
    pub f13: f64,
    #[r8(min = 0, max = 1, default = "0.1")]
    pub f14: f64,
    #[r8(min = 0, max = 1, default = "0.1")]
    pub f15: f64,
    #[r8(min = 0, max = 1, default = "0.1")]
    pub f16: f64,
    #[r8(min = 0, max = 1, default = "0.1")]
    pub f17: f64,
    #[r8(min = 0, max = 1, default = "0.1")]
    pub f18: f64,
    #[r8(min = 0, max = 1, default = "0.1")]
    pub f19: f64,
    #[r8(min = 0, max = 1, default = "0.1")]
    pub f20: f64,
}

// The settings of a gradient-boosted tree model, with case weights and the
// indices of the features used.
#[derive(R8, Clone)]
#[r8(namespace = "bench", version = 1, storage = "rust")]
#[r8(rule(
    id = "early_stop_below_trees",
    invariant = r#"{"<": [{"var": "early_stopping_rounds"}, {"var": "n_trees"}]}"#,
    when = r#"{"!": [{"is_null": [{"var": "early_stopping_rounds"}]}]}"#,
    message = "early_stopping_rounds is below n_trees."
))]
#[r8(rule(
    id = "num_class_iff_multiclass",
    invariant = r#"{"===": [{"===": [{"var": "objective"}, "multiclass"]}, {"!": [{"is_null": [{"var": "num_class"}]}]}]}"#,
    message = "num_class is present exactly when the objective is multiclass."
))]
pub struct GBM {
    #[r8(exclusive_min = 0, max = 1)]
    pub learning_rate: f64,
    #[r8(exclusive_min = 0, max = 1)]
    pub subsample: f64,
    #[r8(exclusive_min = 0, max = 1)]
    pub colsample: f64,
    #[r8(min = 1)]
    pub n_trees: i32,
    #[r8(min = 1, max = 32)]
    pub max_depth: i32,
    #[r8(pattern = "^(regression|binary|multiclass)$")]
    pub objective: String,
    #[r8(min = 2)]
    pub num_class: Option<i32>,
    #[r8(min = 1)]
    pub early_stopping_rounds: Option<i32>,
    pub seed: Option<i32>,
    pub verbose: bool,
    #[r8(min = 0)]
    pub weights: Buffer<f64>,
    #[r8(min = 1)]
    pub feature_idx: Buffer<i32>,
}

/// The Rust side of `bench::Complexity`, declared in R with
/// `complexity(x)`.
pub trait Complexity {
    /// A constant, so that the dispatch row times dispatch alone.
    fn complexity(&self) -> i32;
}

#[r8::methods(trait = "bench::Complexity")]
impl Complexity for GBM {
    fn complexity(&self) -> i32 {
        1
    }
}

r8::exports! {
    package = "r8benchR",
    namespace = "bench",
    types = [Wide20, GBM],
    impls = [GBM: Complexity],
}
