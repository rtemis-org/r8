//! Structs that extend one another: `pkgr::Interval`, `pkgr::Band`, which
//! extends it, and `pkgr::Strip`, which extends `Band`; with impls on
//! `Interval`, which apply to the three, and an impl of each of the three
//! (`Shifted`), whose result keeps the type of the value. The crate of
//! r8pkgS depends on this one.

use rtemis_r8::R8;
use rtemis_r8_api as r8;
use rtemis_r8_api::{Buffer, Dots};

/// A numeric interval.
#[derive(R8, Clone)]
#[r8(namespace = "pkgr", version = 1, storage = "rust", extensible)]
#[r8(rule(
    id = "ordered",
    invariant = r#"{"<=": [{"var": "lo"}, {"var": "hi"}]}"#
))]
pub struct Interval {
    /// The lower end.
    pub lo: f64,
    /// The upper end.
    #[r8(default = "1")]
    pub hi: f64,
    /// The unit of the ends.
    pub unit: Option<String>,
}

/// A labeled interval with weights.
#[derive(R8, Clone)]
#[r8(namespace = "pkgr", version = 1, storage = "rust", extensible)]
#[r8(rule(id = "labeled", invariant = r#"{"!==": [{"var": "label"}, ""]}"#))]
pub struct Band {
    #[r8(extends, inherited(lo(default = "0"), unit(default = r#""cm""#)))]
    pub base: Interval,
    /// The label.
    pub label: String,
    /// The weights.
    pub weights: Buffer<f64>,
}

/// A numbered band without a default unit.
#[derive(R8, Clone)]
#[r8(namespace = "pkgr", version = 1, storage = "rust")]
pub struct Strip {
    #[r8(extends, inherited(unit(default = "null")))]
    pub band: Band,
    /// The number.
    #[r8(min = 1, default = "1")]
    pub number: i32,
}

/// The Rust side of the generic `pkgr::width`.
#[r8::methods(trait = "pkgr::width")]
pub trait Width {
    fn width(&self) -> f64;
}

impl Width for Interval {
    fn width(&self) -> f64 {
        self.hi - self.lo
    }
}

/// The Rust side of the generic `pkgr::middle`, whose impl for `Interval`
/// returns an `Interval` for a value of any of the three.
#[r8::methods(trait = "pkgr::middle")]
pub trait Middle {
    fn middle(&self) -> Interval;
}

impl Middle for Interval {
    fn middle(&self) -> Interval {
        let quarter = (self.hi - self.lo) / 4.0;
        Interval {
            lo: self.lo + quarter,
            hi: self.hi - quarter,
            unit: self.unit.clone(),
        }
    }
}

/// The Rust side of the generic `pkgr::shifted`, whose impl for `Interval`
/// and every struct that extends it returns a value of the same type.
#[r8::methods(trait = "pkgr::shifted")]
pub trait Shifted {
    fn shifted(&self, by: f64) -> r8::Result<Self>
    where
        Self: Sized;
}

impl<T: IntervalLike> Shifted for T {
    fn shifted(&self, by: f64) -> r8::Result<Self> {
        if !by.is_finite() {
            return Err(r8::Error::new("not_finite", "`by` is finite"));
        }
        let interval = self.to_interval();
        Ok(self.with_interval(Interval {
            lo: interval.lo + by,
            hi: interval.hi + by,
            ..interval
        }))
    }
}

/// The Rust side of the generic `pkgr::overlap`, whose `y` is an `Interval`
/// exactly.
#[r8::methods(trait = "pkgr::overlap")]
pub trait Overlap {
    fn overlap(&self, y: &Interval) -> f64;
}

impl Overlap for Interval {
    fn overlap(&self, y: &Interval) -> f64 {
        (self.hi.min(y.hi) - self.lo.max(y.lo)).max(0.0)
    }
}

/// The Rust side of the generic `pkgr::total`, which reads the fields that
/// `Band` adds and those it inherits.
#[r8::methods(trait = "pkgr::total")]
pub trait Total {
    fn total(&self) -> f64;
}

impl Total for Band {
    fn total(&self) -> f64 {
        self.lo() + self.weights().iter().sum::<f64>()
    }
}

/// The Rust side of the generic `pkgr::shares`: whether the weights of the
/// `Band` that a `Strip` holds, cloned from it, share their memory.
#[r8::methods(trait = "pkgr::shares")]
pub trait Shares {
    fn shares(&self) -> bool;
}

impl Shares for Strip {
    fn shares(&self) -> bool {
        self.to_band().weights.as_ptr() == self.weights().as_ptr()
    }
}

impl r8::Display for Interval {
    fn format(&self, _dots: Dots) -> String {
        format!("[{}, {}]", self.lo, self.hi)
    }
}
