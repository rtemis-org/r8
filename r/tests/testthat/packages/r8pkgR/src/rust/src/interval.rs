//! Structs that extend one another: `pkgr::Interval`, `pkgr::Band`, which
//! extends it, and `pkgr::Strip`, which extends `Band`; with impls on
//! `Interval`, which apply to the three.

use rtemis_r8::R8;
use rtemis_r8_api as r8;
use rtemis_r8_api::{Buffer, Dots};

/// A numeric interval.
#[derive(R8, Clone)]
#[r8(namespace = "pkgr", version = 1, storage = "rust", extensible)]
#[r8(rule(id = "ordered", invariant = r#"{"<=": [{"var": "lo"}, {"var": "hi"}]}"#))]
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
pub trait Width {
    fn width(&self) -> f64;
}

#[r8::methods(trait = "pkgr::width")]
impl Width for Interval {
    fn width(&self) -> f64 {
        self.hi - self.lo
    }
}

/// The Rust side of the generic `pkgr::shifted`.
pub trait Shifted {
    fn shifted(&self, by: f64) -> r8::Value<Interval>;
}

#[r8::methods(trait = "pkgr::shifted")]
impl Shifted for Interval {
    fn shifted(&self, by: f64) -> r8::Value<Interval> {
        r8::Value(Interval {
            lo: self.lo + by,
            hi: self.hi + by,
            unit: self.unit.clone(),
        })
    }
}

/// The Rust side of the generic `pkgr::overlap`, whose `y` is an `Interval`
/// exactly.
pub trait Overlap {
    fn overlap(&self, y: &Interval) -> f64;
}

#[r8::methods(trait = "pkgr::overlap")]
impl Overlap for Interval {
    fn overlap(&self, y: &Interval) -> f64 {
        (self.hi.min(y.hi) - self.lo.max(y.lo)).max(0.0)
    }
}

/// The Rust side of the generic `pkgr::total`, which reads the fields that
/// `Band` adds and those it inherits.
pub trait Total {
    fn total(&self) -> f64;
}

#[r8::methods(trait = "pkgr::total")]
impl Total for Band {
    fn total(&self) -> f64 {
        self.lo() + self.weights().iter().sum::<f64>()
    }
}

/// The Rust side of the generic `pkgr::shares`: whether the weights of the
/// `Band` that a `Strip` holds, cloned from it, share their memory.
pub trait Shares {
    fn shares(&self) -> bool;
}

#[r8::methods(trait = "pkgr::shares")]
impl Shares for Strip {
    fn shares(&self) -> bool {
        self.to_band().weights.as_ptr() == self.weights().as_ptr()
    }
}

#[r8::methods(trait = "R8::Display")]
impl r8::Display for Interval {
    fn format(&self, _dots: Dots) -> String {
        format!("[{}, {}]", self.lo, self.hi)
    }
}
