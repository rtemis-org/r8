//! The Rust code of r8pkgS, whose crate depends on that of r8pkgR:
//! `pkgs::Tile`, which extends `pkgr::Interval`; the impl of `pkgr::shifted`
//! that r8pkgR's crate writes for every struct that extends `Interval`,
//! registered for `Tile`; and impls of a trait of r8pkgR's crate and of this
//! one. Each shared object has its own compilation of `Interval`.

use r8pkgr::interval::{Interval, IntervalLike, Shifted, Total};
use rtemis_r8::R8;
use rtemis_r8_api as r8;

/// A numbered interval.
#[derive(R8, Clone)]
#[r8(namespace = "pkgs", version = 1, storage = "rust")]
pub struct Tile {
    #[r8(extends)]
    pub base: Interval,
    /// The number.
    #[r8(min = 1)]
    pub number: i32,
}

impl Total for Tile {
    fn total(&self) -> f64 {
        self.hi() + f64::from(self.number)
    }
}

/// The Rust side of the generic `pkgs::gap`, whose `y` is a value of
/// r8pkgR's type.
#[r8::methods(trait = "pkgs::gap")]
pub trait Gap {
    fn gap(&self, y: &Interval) -> f64;
}

impl<T: IntervalLike> Gap for T {
    fn gap(&self, y: &Interval) -> f64 {
        y.lo - self.hi()
    }
}

r8::exports! {
    package = "r8pkgS",
    namespace = "pkgs",
    types = [Tile],
    impls = [Tile: Shifted, Tile: Total, Tile: Gap],
}
