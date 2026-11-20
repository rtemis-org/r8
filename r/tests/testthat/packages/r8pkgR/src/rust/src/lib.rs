//! The Rust code of r8pkgR: a Rust-native struct, `pkgr::Signal`, with
//! impls of `pkgr::Norm`, declared in R, and of `R8::Display`; and a
//! Rust-native enum, `pkgr::Resampler`, of every variant shape, with an impl
//! of `R8::Display`.

use rtemis_r8::R8;
use rtemis_r8_api as r8;
use rtemis_r8_api::{Buffer, Dots};

/// A named series of values.
#[derive(R8, Clone)]
#[r8(namespace = "pkgr", version = 1, native)]
pub struct Signal {
    /// The name.
    pub name: String,
    /// The values.
    pub values: Buffer<f64>,
}

/// The Rust side of `pkgr::Norm`, declared in R with `magnitude(x)` and
/// `ratio(x, by)`.
pub trait Norm {
    fn magnitude(&self) -> f64;
    fn ratio(&self, by: f64) -> r8::Result<f64>;
}

#[r8::methods(trait = "pkgr::Norm")]
impl Norm for Signal {
    fn magnitude(&self) -> f64 {
        self.values.iter().map(|v| v * v).sum::<f64>().sqrt()
    }
    fn ratio(&self, by: f64) -> r8::Result<f64> {
        if by == 0.0 {
            return Err(r8::Error::new("zero_divisor", "`by` is not 0"));
        }
        Ok(self.values.first().copied().unwrap_or(f64::NAN) / by)
    }
}

#[r8::methods(trait = "R8::Display")]
impl r8::Display for Signal {
    fn format(&self, _dots: Dots) -> String {
        format!("Signal {} ({} values)", self.name, self.values.len())
    }
}

/// A train and test split of case indices.
#[derive(R8, Clone)]
#[r8(namespace = "pkgr", version = 1, native)]
pub struct Split {
    /// The indices of the training cases.
    pub train: Buffer<i32>,
    /// The indices of the test cases.
    pub test: Buffer<i32>,
}

/// A resampling method.
#[derive(R8, Clone)]
#[r8(namespace = "pkgr", version = 1, native)]
pub enum Resampler {
    /// K-fold cross-validation.
    KFold {
        /// The number of folds.
        k: i32,
        /// The random seed.
        seed: i32,
    },
    /// Bootstrap resampling.
    Bootstrap {
        /// The number of resamples.
        n: i32,
    },
    /// A fixed set of training cases.
    Fixed {
        /// The indices of the training cases.
        train: Buffer<i32>,
    },
    /// A given split.
    Custom(Split),
    /// Leave-one-out cross-validation.
    Loocv,
}

#[r8::methods(trait = "R8::Display")]
impl r8::Display for Resampler {
    fn format(&self, _dots: Dots) -> String {
        match self {
            Resampler::KFold { k, seed } => format!("KFold (k = {k}, seed = {seed})"),
            Resampler::Bootstrap { n } => format!("Bootstrap (n = {n})"),
            Resampler::Fixed { train } => format!("Fixed ({} training cases)", train.len()),
            Resampler::Custom(s) => format!("Custom ({} train, {} test)", s.train.len(), s.test.len()),
            Resampler::Loocv => "LOOCV".to_string(),
        }
    }
}

r8::exports! {
    package = "r8pkgR",
    namespace = "pkgr",
    types = [Signal, Resampler],
    impls = [Signal: Norm, Signal: Display, Resampler: Display],
}

