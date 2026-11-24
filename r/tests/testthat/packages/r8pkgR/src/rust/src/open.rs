//! Open fields: `pkgr::Panel`, whose fields hold any value of a subtype of a
//! struct and any implementor of `pkgr::Figure`, a trait with a tag member,
//! beside nested Rust-stored values; and `pkgr::Square`, which implements
//! `pkgr::Figure`.

use rtemis_r8::R8;
use rtemis_r8::derive::Subtype;
use rtemis_r8_api as r8;
use rtemis_r8_api::{ImplementorValue, SubtypeValue};

use crate::Resampler;
use crate::interval::*;

/// A plane figure with a name of its own.
#[r8::methods(trait = "pkgr::Figure", dyn, tag = "figure")]
pub trait Figure {
    /// The area.
    fn area(&self) -> f64;
    /// The figure scaled by a factor.
    fn scaled(
        &self,
        /// The factor.
        #[r8(exclusive_min = 0)]
        by: f64,
    ) -> Self
    where
        Self: Sized;
    /// The name.
    #[r8(field)]
    fn name(&self) -> &String;
}

/// A square.
#[derive(R8, Clone)]
#[r8(namespace = "pkgr", version = 1, storage = "rust")]
pub struct Square {
    /// The side.
    #[r8(min = 0)]
    pub side: f64,
    /// The name.
    #[r8(default = r#""square""#)]
    pub name: String,
}

impl Figure for Square {
    fn area(&self) -> f64 {
        self.side * self.side
    }
    fn scaled(&self, by: f64) -> Self {
        Square {
            side: self.side * by,
            ..self.clone()
        }
    }
    fn name(&self) -> &String {
        &self.name
    }
}

/// A panel of a plot.
#[derive(R8, Clone)]
#[r8(namespace = "pkgr", version = 1, storage = "rust")]
pub struct Panel {
    /// The range.
    pub range: SubtypeValue<Interval>,
    /// The band of an inset.
    pub inset: Option<SubtypeValue<Band>>,
    /// The frame.
    pub frame: Interval,
    /// The resampling method.
    pub method: Option<Resampler>,
    /// The figure.
    pub figure: ImplementorValue<dyn Figure>,
}

/// The width of an interval, through its interface.
fn width(x: &dyn IntervalLike) -> f64 {
    x.hi() - x.lo()
}

/// The sum of the weights of a band and of every struct that extends it.
fn weight(x: &impl BandLike) -> f64 {
    x.weights().iter().sum()
}

/// What the fields of a panel hold.
#[r8::methods(trait = "pkgr::contents")]
pub trait Contents {
    fn contents(&self) -> r8::Result<String>;
}

impl Contents for Panel {
    fn contents(&self) -> r8::Result<String> {
        let range = match self.range.downcast_ref::<Strip>()? {
            Some(s) => format!("strip {}", s.number),
            None => self.range.type_name().to_string(),
        };
        let inset = match &self.inset {
            Some(b) => format!("{} {}", b.label(), weight(b)),
            None => "none".to_string(),
        };
        let figure = match self.figure.downcast_ref::<Square>()? {
            Some(s) => format!("{} {}", s.name(), s.area()),
            None => self.figure.type_name().to_string(),
        };
        // The fields of the interface are read on any thread.
        let spans = std::thread::scope(|s| {
            s.spawn(|| width(&self.range) + width(&self.frame))
                .join()
                .unwrap_or(f64::NAN)
        });
        Ok(format!("{range}; {inset}; {figure}; {spans}"))
    }
}

/// The panel with its range moved, which keeps the type of the range.
#[r8::methods(trait = "pkgr::moved")]
pub trait Moved {
    fn moved(
        &self,
        /// The lower end.
        lo: f64,
        /// The upper end.
        hi: f64,
    ) -> Panel;
}

impl Moved for Panel {
    fn moved(&self, lo: f64, hi: f64) -> Panel {
        let base: &Interval = self.range.upcast();
        Panel {
            range: self.range.with_interval(Interval {
                lo,
                hi,
                ..base.clone()
            }),
            ..self.clone()
        }
    }
}

/// The panel over a new band, and of a new square.
#[r8::methods(trait = "pkgr::banded")]
pub trait Banded {
    fn banded(&self, label: String) -> r8::Result<Panel>;
}

impl Banded for Panel {
    fn banded(&self, label: String) -> r8::Result<Panel> {
        Ok(Panel {
            range: SubtypeValue::new(Band {
                base: self.frame.clone(),
                label,
                weights: vec![1.0, 2.0].into(),
            })?,
            figure: ImplementorValue::new(Square {
                side: 2.0,
                name: "square".to_string(),
            })?,
            ..self.clone()
        })
    }
}

/// The value of the range.
#[r8::methods(trait = "pkgr::held")]
pub trait HeldRange {
    fn held(&self) -> SubtypeValue<Interval>;
}

/// The panel with other values of its open fields, where they are given.
#[r8::methods(trait = "pkgr::refit")]
pub trait Refit {
    fn refit(
        &self,
        /// The range.
        range: SubtypeValue<Interval>,
        /// The figure.
        figure: ImplementorValue<dyn Figure>,
        /// The frame.
        #[r8(default = "null")]
        frame: Option<&Interval>,
        /// Weights, whose sum is added to the upper end of the frame.
        #[r8(default = "null", min = 0)]
        weights: Option<r8::Buffer<f64>>,
    ) -> Panel;
}

impl Refit for Panel {
    fn refit(
        &self,
        range: SubtypeValue<Interval>,
        figure: ImplementorValue<dyn Figure>,
        frame: Option<&Interval>,
        weights: Option<r8::Buffer<f64>>,
    ) -> Panel {
        let mut frame = frame.unwrap_or(&self.frame).clone();
        frame.hi += weights.iter().flat_map(|w| w.iter()).sum::<f64>();
        Panel {
            range,
            figure,
            frame,
            ..self.clone()
        }
    }
}

impl HeldRange for Panel {
    fn held(&self) -> SubtypeValue<Interval> {
        self.range.clone()
    }
}
