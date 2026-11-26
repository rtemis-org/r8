//! The Rust code of r8pkgR: a Rust-stored struct, `pkgr::Signal`, with
//! impls of `pkgr::Norm`, declared in R, and of `R8::Display`; and a
//! Rust-stored enum, `pkgr::Resampler`, of every variant shape, with an impl
//! of `R8::Display`; `pkgr::Probe`, a field of every Rust field type,
//! against which the conformance tests of `tests/testthat/conformance` run;
//! structs that extend one another (`interval`); open fields (`open`); and
//! the bodies of typed functions declared in R (`functions`), over a module
//! without R8 (`levels`).

pub mod functions;
pub mod interval;
pub mod levels;
pub mod open;

use interval::*;
use open::*;
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
#[r8::methods(trait = "pkgr::Norm")]
pub trait Norm {
    fn magnitude(&self) -> f64;
    fn ratio(&self, by: f64) -> r8::Result<f64>;
    fn distance(&self, y: &Signal) -> r8::Result<f64>;
}

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

impl r8::Display for Probe {
    fn format(&self, _dots: Dots) -> String {
        format!("Probe {} ({})", self.label, self.count)
    }
}

impl r8::PartialOrd for Probe {
    fn partial_cmp(&self, y: r8::Sexp) -> Option<std::cmp::Ordering> {
        let y = unsafe { r8::Receiver::<Probe>::from_named_arg(y.0, "y") }.ok()?;
        self.ratio.partial_cmp(&y.ratio)
    }
}

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
    types = [Signal, Resampler, Probe, Interval, Band, Strip, Square, Panel],
    traits = [
        Shifted, Meets, WithLabel, Figure, Contents, Moved, Banded, HeldRange, Refit,
    ],
    impls = [
        Signal: Norm,
        Signal: r8::Display,
        Resampler: r8::Display,
        Probe: r8::Display,
        Probe: r8::PartialOrd,
        Probe: r8::Shape,
        Interval: Width,
        Interval: Middle,
        Interval: Overlap,
        Interval: r8::Display,
        Interval: Shifted,
        Band: Shifted,
        Strip: Shifted,
        Band: Total,
        Strip: Shares,
        Interval: Meets<Interval>,
        Interval: Meets<Band>,
        Signal: Meets<r8::Any>,
        Band: WithLabel,
        Strip: WithLabel,
        Interval: WithUpper,
        Interval: r8::From<Signal>,
        Signal: r8::From<Band>,
        Square: Figure,
        Panel: Contents,
        Panel: Moved,
        Panel: Banded,
        Panel: HeldRange,
        Panel: Refit,
    ],
    functions = [
        functions::clip,
        functions::level,
        functions::tally,
        functions::fraction,
    ],
}
