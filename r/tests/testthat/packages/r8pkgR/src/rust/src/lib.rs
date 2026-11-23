//! The Rust code of r8pkgR: a Rust-stored struct, `pkgr::Signal`, with
//! impls of `pkgr::Norm`, declared in R, and of `R8::Display`; and a
//! Rust-stored enum, `pkgr::Resampler`, of every variant shape, with an impl
//! of `R8::Display`; and `pkgr::Probe`, a field of every Rust field type,
//! against which the conformance tests of `tests/testthat/conformance` run.

use rtemis_r8::R8;
use rtemis_r8_api as r8;
use rtemis_r8_api::{Buffer, Dots, Logical};

/// A named series of values.
#[derive(R8, Clone)]
#[r8(namespace = "pkgr", version = 1, storage = "rust")]
pub struct Signal {
    /// The name.
    pub name: String,
    /// The values.
    pub values: Buffer<f64>,
}

/// The Rust side of `pkgr::Norm`, declared in R with `magnitude(x)`,
/// `ratio(x, by)`, and `distance(x, y)`.
pub trait Norm {
    fn magnitude(&self) -> f64;
    fn ratio(&self, by: f64) -> r8::Result<f64>;
    fn distance(&self, y: &Signal) -> r8::Result<f64>;
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
    fn distance(&self, y: &Signal) -> r8::Result<f64> {
        if self.values.len() != y.values.len() {
            return Err(r8::Error::new(
                "length_mismatch",
                "`x` and `y` have values of the same length",
            ));
        }
        Ok(self
            .values
            .iter()
            .zip(y.values.iter())
            .map(|(a, b)| (a - b) * (a - b))
            .sum::<f64>()
            .sqrt())
    }
}

#[r8::methods(trait = "R8::Display")]
impl r8::Display for Signal {
    fn format(&self, _dots: Dots) -> String {
        format!("Signal {} ({} values)", self.name, self.values.len())
    }
}

/// A field of every Rust field type, with constraints, a default, and
/// rules.
#[derive(R8, Clone)]
#[r8(namespace = "pkgr", version = 1, storage = "rust")]
#[r8(rule(
    id = "seed_below_count",
    invariant = r#"{"<": [{"var": "seed"}, {"var": "count"}]}"#,
    when = r#"{"!==": [{"var": "seed"}, null]}"#,
    message = "The seed is below the count."
))]
#[r8(rule(
    id = "weights_match_index",
    invariant = r#"{"===": [{"length": {"var": "weights"}}, {"length": {"var": "index"}}]}"#,
    message = "There is one weight per index."
))]
pub struct Probe {
    /// A fraction.
    #[r8(min = 0, max = 1)]
    pub ratio: f64,
    /// A count.
    #[r8(min = 1, default = "10")]
    pub count: i32,
    /// A flag.
    pub flag: bool,
    /// A lowercase label.
    #[r8(pattern = "^[a-z]+$")]
    pub label: String,
    /// An optional seed.
    pub seed: Option<i32>,
    /// Weights.
    pub weights: Buffer<f64>,
    /// Case indices.
    pub index: Buffer<i32>,
    /// Bytes.
    pub bytes: Buffer<u8>,
    /// A mask.
    pub mask: Buffer<Logical>,
}

#[r8::methods(trait = "R8::Display")]
impl r8::Display for Probe {
    fn format(&self, _dots: Dots) -> String {
        format!("Probe {} ({})", self.label, self.count)
    }
}

#[r8::methods(trait = "R8::PartialOrd")]
impl r8::PartialOrd for Probe {
    fn partial_cmp(&self, other: r8::Sexp) -> Option<std::cmp::Ordering> {
        let other = unsafe { r8::Receiver::<Probe>::from_named_arg(other.0, "y") }.ok()?;
        self.ratio.partial_cmp(&other.ratio)
    }
}

#[r8::methods(trait = "R8::Shape")]
impl r8::Shape for Probe {
    fn dim(&self) -> Vec<i32> {
        vec![self.index.len() as i32, 2]
    }
}

/// A train and test split of case indices.
#[derive(R8, Clone)]
#[r8(namespace = "pkgr", version = 1, storage = "rust")]
pub struct Split {
    /// The indices of the training cases.
    pub train: Buffer<i32>,
    /// The indices of the test cases.
    pub test: Buffer<i32>,
}

/// A resampling method.
#[derive(R8, Clone)]
#[r8(namespace = "pkgr", version = 1, storage = "rust")]
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
            Resampler::Custom(s) => {
                format!("Custom ({} train, {} test)", s.train.len(), s.test.len())
            }
            Resampler::Loocv => "LOOCV".to_string(),
        }
    }
}

r8::exports! {
    package = "r8pkgR",
    namespace = "pkgr",
    types = [Signal, Resampler, Probe],
    impls = [
        Signal: Norm,
        Signal: Display,
        Resampler: Display,
        Probe: Display,
        Probe: PartialOrd,
        Probe: Shape,
    ],
}
