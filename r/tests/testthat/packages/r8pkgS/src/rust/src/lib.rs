//! The Rust code of r8pkgS, whose crate depends on that of r8pkgR:
//! `pkgs::Tile`, which extends `pkgr::Interval`; the impl of `pkgr::shifted`
//! that r8pkgR's crate writes for every struct that extends `Interval`,
//! registered for `Tile`; and impls of a trait of r8pkgR's crate and of this
//! one; and `pkgs::Grid`, whose fields hold values of r8pkgR's types. Each
//! shared object has its own compilation of `Interval`.

use r8pkgr::Resampler;
use r8pkgr::interval::{Band, Interval, IntervalLike, Meets, Shifted, Total};
use r8pkgr::open::Figure;
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

/// The Rust side of `pkgs::Noted`, declared in R.
#[r8::methods(trait = "pkgs::Noted", dyn)]
pub trait Noted {
    fn noted(&self) -> String;
}

impl Noted for Tile {
    fn noted(&self) -> String {
        format!("tile {}", self.number)
    }
}

/// A grid of cells.
#[derive(R8, Clone)]
#[r8(namespace = "pkgs", version = 1, storage = "rust")]
pub struct Grid {
    /// The cell, a value of r8pkgR's type.
    pub cell: Interval,
    /// The resampling method, a value of r8pkgR's enum.
    pub method: Resampler,
    /// The extent.
    pub extent: r8::SubtypeValue<Interval>,
    /// The figure.
    pub figure: Option<r8::ImplementorValue<dyn Figure>>,
    /// A value with a note.
    pub note: Option<r8::ImplementorValue<dyn Noted>>,
}

/// The number of cells in the extent, and the band that the extent is.
#[r8::methods(trait = "pkgs::cells")]
pub trait Cells {
    fn cells(&self) -> r8::Result<String>;
}

impl Cells for Grid {
    fn cells(&self) -> r8::Result<String> {
        let n = (self.extent.hi() - self.extent.lo()) / (self.cell.hi - self.cell.lo);
        let of = match self.extent.downcast_ref::<Band>()? {
            Some(b) => format!("{} {}", b.label, b.weights.len()),
            None => self.extent.type_name().to_string(),
        };
        Ok(format!("{n} {of}"))
    }
}

/// The Rust side of the generic `pkgs::lower`, whose result is a value of
/// r8pkgR's type, which this library does not build.
#[r8::methods(trait = "pkgs::lower")]
pub trait Lower {
    fn lower(&self) -> Interval;
}

impl Lower for Tile {
    fn lower(&self) -> Interval {
        let half = (self.hi() - self.lo()) / 2.0;
        Interval {
            hi: self.lo() + half,
            ..self.to_interval()
        }
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

/// An impl on this package's type and one of r8pkgR, whose value this
/// library reads by its fields.
impl Meets<Interval> for Tile {
    fn meets(&self, y: &Interval, _by: f64) -> String {
        format!("tile {} interval {}", self.number, y.hi)
    }
}

/// The impl that tells a tile and a band from the impls of (`Tile`,
/// `Interval`) and of r8pkgR's (`Interval`, `Band`).
impl Meets<Band> for Tile {
    fn meets(&self, y: &Band, _by: f64) -> String {
        format!("tile {} band {}", self.number, y.weights.len())
    }
}

impl Meets<Tile> for Tile {
    fn meets(&self, y: &Tile, _by: f64) -> String {
        format!("tile {} tile {}", self.number, y.number)
    }
}

r8::exports! {
    package = "r8pkgS",
    namespace = "pkgs",
    types = [Tile, Grid],
    traits = [Cells],
    impls = [
        Tile: Shifted,
        Tile: Total,
        Tile: Lower,
        Tile: Noted,
        Grid: Cells,
        Tile: Gap,
        Tile: Meets<Interval>,
        Tile: Meets<Band>,
        Tile: Meets<Tile>,
    ],
}
