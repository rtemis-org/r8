//! R8's C services for an R package's Rust code: Rust-stored types, whose
//! values R8 holds in Rust, and impls of R8 traits written in Rust.
//!
//! A package declares a struct with `#[derive(R8)]` and `#[r8(storage = "rust")]`,
//! marks the Rust trait of each R8 trait with
//! `#[r8::methods(trait = "ns::Trait")]`, implements it as any Rust trait,
//! and lists the types and the impls in `r8::exports!`, which emits
//! `r8_init_<package>()` for the package's initialization routine. An R8 trait
//! is declared in R, as for impls written in R, or in Rust.
//!
//! An impl over the interface trait of an extensible struct keeps the type of
//! the value, and each type it is registered for is a row:
//!
//! ```no_run,standalone_crate
//! use rtemis_r8::R8;
//! use rtemis_r8_api as r8;
//!
//! /// A numeric interval.
//! #[derive(R8, Clone)]
//! #[r8(namespace = "demo", version = 1, storage = "rust", extensible)]
//! pub struct Interval {
//!     pub lo: f64,
//!     pub hi: f64,
//! }
//!
//! /// A labeled interval.
//! #[derive(R8, Clone)]
//! #[r8(namespace = "demo", version = 1, storage = "rust")]
//! pub struct Band {
//!     #[r8(extends)]
//!     pub base: Interval,
//!     pub label: String,
//! }
//!
//! /// The Rust side of `demo::Shifted`, declared in R with `shifted(x, by)`.
//! #[r8::methods(trait = "demo::Shifted")]
//! pub trait Shifted {
//!     fn shifted(&self, by: f64) -> Self;
//! }
//!
//! impl<T: IntervalLike> Shifted for T {
//!     fn shifted(&self, by: f64) -> Self {
//!         self.with_interval(Interval {
//!             lo: self.lo() + by,
//!             hi: self.hi() + by,
//!         })
//!     }
//! }
//!
//! r8::exports! {
//!     package = "demo",
//!     namespace = "demo",
//!     types = [Interval, Band],
//!     impls = [Interval: Shifted, Band: Shifted],
//! }
//! # fn main() {}
//! ```
//!
//! A type parameter of the trait is a dispatched position after the first,
//! which a row names, or [`Any`]; [`From`] converts a value of another type.
//!
//! ```no_run,standalone_crate
//! use rtemis_r8::R8;
//! use rtemis_r8_api as r8;
//!
//! /// A numeric interval.
//! #[derive(R8, Clone)]
//! #[r8(namespace = "demo", version = 1, storage = "rust")]
//! pub struct Interval {
//!     pub lo: f64,
//!     pub hi: f64,
//! }
//!
//! /// A point.
//! #[derive(R8, Clone)]
//! #[r8(namespace = "demo", version = 1, storage = "rust")]
//! pub struct Point {
//!     pub at: f64,
//! }
//!
//! /// The Rust side of the generic `demo::covers(x, y)`, which dispatches on
//! /// both arguments.
//! #[r8::methods(trait = "demo::covers", dispatch(x, y))]
//! pub trait Covers<Y> {
//!     fn covers(&self, y: &Y) -> bool;
//! }
//!
//! impl Covers<Interval> for Interval {
//!     fn covers(&self, y: &Interval) -> bool {
//!         self.lo <= y.lo && y.hi <= self.hi
//!     }
//! }
//!
//! impl Covers<Point> for Interval {
//!     fn covers(&self, y: &Point) -> bool {
//!         (self.lo..=self.hi).contains(&y.at)
//!     }
//! }
//!
//! impl Covers<r8::Any> for Point {
//!     fn covers(&self, _y: &r8::Any) -> bool {
//!         false
//!     }
//! }
//!
//! impl r8::From<Point> for Interval {
//!     fn from(source: &Point) -> r8::Result<Self> {
//!         Ok(Interval { lo: source.at, hi: source.at })
//!     }
//! }
//!
//! r8::exports! {
//!     package = "demo",
//!     namespace = "demo",
//!     types = [Interval, Point],
//!     impls = [
//!         Interval: Covers<Interval>,
//!         Interval: Covers<Point>,
//!         Point: Covers<r8::Any>,
//!         Interval: r8::From<Point>,
//!     ],
//! }
//! # fn main() {}
//! ```
//!
//! `traits` lists the traits that Rust declares, in place of R: the types of
//! the parameters and of the result are those of the Rust signature, the
//! attributes of a parameter its constraints and default, and the doc comments
//! its descriptions.
//!
//! ```no_run,standalone_crate
//! use rtemis_r8::R8;
//! use rtemis_r8_api as r8;
//!
//! /// A numeric interval.
//! #[derive(R8, Clone)]
//! #[r8(namespace = "demo", version = 1, storage = "rust")]
//! pub struct Interval {
//!     pub lo: f64,
//!     pub hi: f64,
//! }
//!
//! /// A value moved along its axis.
//! #[r8::methods(trait = "demo::shifted")]
//! pub trait Shifted {
//!     fn shifted(
//!         &self,
//!         /// The distance.
//!         #[r8(default = "1")]
//!         by: f64,
//!     ) -> Self;
//! }
//!
//! impl Shifted for Interval {
//!     fn shifted(&self, by: f64) -> Self {
//!         Interval { lo: self.lo + by, hi: self.hi + by }
//!     }
//! }
//!
//! r8::exports! {
//!     package = "demo",
//!     namespace = "demo",
//!     types = [Interval],
//!     traits = [Shifted],
//!     impls = [Interval: Shifted],
//! }
//! # fn main() {}
//! ```
//!
//! A typed function that R declares with `new_function(name, function(...)
//! rust())` has its body in Rust: `#[r8::function]` marks the function, and
//! `functions` of `r8::exports!` lists it. Its parameters have the names of the
//! declaration's, in order; a slice and a string are borrowed from the R value
//! for the call, and a `Vec` result becomes an R vector.
//!
//! ```no_run,standalone_crate
//! use rtemis_r8_api as r8;
//!
//! /// Holds a running level between `lo` and `hi`.
//! #[r8::function]
//! fn level(x: &[f64], lo: f64, hi: f64) -> Vec<f64> {
//!     let mut s = 0.0;
//!     x.iter()
//!         .map(|v| {
//!             s = (s + v).clamp(lo, hi);
//!             s
//!         })
//!         .collect()
//! }
//!
//! /// A summary of a vector, with a parameter whose R name is no Rust
//! /// identifier.
//! #[r8::function]
//! fn total(x: &[f64], #[r8(name = "na.rm")] na_rm: bool, label: Option<&str>) -> String {
//!     let s: f64 = x.iter().filter(|v| !(na_rm && v.is_nan())).sum();
//!     format!("{}: {s}", label.unwrap_or("total"))
//! }
//!
//! /// A name that a module of `std` has too, and a named lifetime.
//! #[r8::function]
//! fn log<'a>(x: &'a [f64], base: Option<f64>) -> Vec<f64> {
//!     let b = base.unwrap_or(std::f64::consts::E);
//!     x.iter().map(|v| v.log(b)).collect()
//! }
//!
//! mod stats {
//!     use rtemis_r8_api as r8;
//!
//!     /// A function of a module, which `functions` lists by its path.
//!     #[r8::function]
//!     pub fn spread(x: &[f64]) -> f64 {
//!         let (lo, hi) = x.iter().fold((f64::INFINITY, f64::NEG_INFINITY), |(a, b), v| {
//!             (a.min(*v), b.max(*v))
//!         });
//!         hi - lo
//!     }
//! }
//!
//! r8::exports! {
//!     package = "demo",
//!     namespace = "demo",
//!     functions = [level, total, log, stats::spread],
//! }
//! # fn main() {}
//! ```
//!
//! A field of a Rust-stored type is a number, a logical, a string, a
//! [`Buffer`], another Rust-stored type, or an `Option` of one
//! ([`Field`]); a field of any struct that extends another is a
//! [`SubtypeValue`], and one of any implementor of a trait an
//! [`ImplementorValue`].
//!
//! A crate that depends on the crate of another package reads the values of that
//! package's types by the names of their fields, when every value that the
//! registered declaration admits is one that the compiled declaration admits; a
//! read is otherwise the error `declaration_changed`. A type that other packages
//! use changes in coordination with them, with a minimum version of the declaring
//! package in their `DESCRIPTION` and `Cargo.toml`.
//!
//! The crate reaches R through its own declarations of R's C API (`sys`)
//! and R8 through `R8Api` (`R8.h`). Every R function that can jump runs
//! inside `R8Api.protect`, so a jump skips no Rust destructor.

// The macros name this crate `::rtemis_r8_api`, in the crate itself too.
extern crate self as rtemis_r8_api;

pub mod api;
pub mod borrowed;
pub mod buffer;
pub mod call;
pub mod dispatch;
pub mod exports;
pub mod field;
pub mod keep;
pub mod open;
pub mod reader;
pub mod rust_stored;
pub mod signature;
pub mod sys;
pub mod traits;

pub use api::{Error, Jumped, Result};
pub use borrowed::Borrowed;
pub use buffer::{Buffer, Element, Logical};
pub use call::{Dots, FromR, IntoR, Receiver, Sexp};
pub use dispatch::{Any, Position, PositionEntry};
pub use exports::{FunctionRow, ImplSet, MethodRow};
pub use field::Field;
pub use open::{ImplementorValue, SubtypeValue};
pub use reader::{Node, Reader};
pub use rust_stored::{
    RustStored, RustStoredEnum, RustStoredStruct, Shared, codec, enum_codec, module,
};
pub use signature::{ParamType, TraitDecl};
pub use traits::{Debug, Display, From, PartialOrd, Shape};

#[doc(hidden)]
pub use rtemis_r8;
pub use rtemis_r8_macros::{exports, function, methods};

/// Code that the macros refuse when the crate compiles.
///
/// `#[r8::methods]` on an impl, whose Rust trait takes the attribute:
///
/// ```compile_fail
/// use rtemis_r8::R8;
/// use rtemis_r8_api as r8;
/// #[derive(R8, Clone)]
/// #[r8(namespace = "demo", version = 1, storage = "rust")]
/// struct Interval { lo: f64 }
/// trait Width { fn width(&self) -> f64; }
/// #[r8::methods(trait = "demo::Width")]
/// impl Width for Interval { fn width(&self) -> f64 { self.lo } }
/// ```
///
/// A row of a trait without the attribute, of a type without an impl of it,
/// and of a type that is not Rust-stored:
///
/// ```compile_fail,E0576
/// use rtemis_r8::R8;
/// use rtemis_r8_api as r8;
/// #[derive(R8, Clone)]
/// #[r8(namespace = "demo", version = 1, storage = "rust")]
/// struct Interval { lo: f64 }
/// trait Width { fn width(&self) -> f64; }
/// impl Width for Interval { fn width(&self) -> f64 { self.lo } }
/// r8::exports! {
///     package = "demo", namespace = "demo",
///     types = [Interval], impls = [Interval: Width],
/// }
/// fn main() {}
/// ```
///
/// ```compile_fail,E0277
/// use rtemis_r8::R8;
/// use rtemis_r8_api as r8;
/// #[derive(R8, Clone)]
/// #[r8(namespace = "demo", version = 1, storage = "rust")]
/// struct Interval { lo: f64 }
/// #[r8::methods(trait = "demo::Width")]
/// trait Width { fn width(&self) -> f64; }
/// r8::exports! {
///     package = "demo", namespace = "demo",
///     types = [Interval], impls = [Interval: Width],
/// }
/// fn main() {}
/// ```
///
/// ```compile_fail,E0277
/// use rtemis_r8::R8;
/// use rtemis_r8_api as r8;
/// #[derive(R8, Clone)]
/// #[r8(namespace = "demo", version = 1)]
/// struct Interval { lo: f64 }
/// #[r8::methods(trait = "demo::Width")]
/// trait Width { fn width(&self) -> f64; }
/// impl Width for Interval { fn width(&self) -> f64 { self.lo } }
/// r8::exports! {
///     package = "demo", namespace = "demo",
///     types = [], impls = [Interval: Width],
/// }
/// fn main() {}
/// ```
///
/// A method without `&self`, with a type parameter, with a parameter or a
/// result that R8 does not convert, and a trait without methods:
///
/// ```compile_fail
/// use rtemis_r8_api as r8;
/// #[r8::methods(trait = "demo::Width")]
/// trait Width { fn width(x: f64) -> f64; }
/// ```
///
/// ```compile_fail
/// use rtemis_r8_api as r8;
/// #[r8::methods(trait = "demo::Width")]
/// trait Width { fn width<T>(&self, by: T) -> f64; }
/// ```
///
/// ```compile_fail,E0277
/// use rtemis_r8_api as r8;
/// #[r8::methods(trait = "demo::Width")]
/// trait Width { fn width(&self, by: u64) -> f64; }
/// ```
///
/// ```compile_fail,E0277
/// use rtemis_r8_api as r8;
/// #[r8::methods(trait = "demo::Width")]
/// trait Width { fn width(&self, y: &f64) -> f64; }
/// ```
///
/// ```compile_fail,E0277
/// use rtemis_r8_api as r8;
/// #[r8::methods(trait = "demo::Width")]
/// trait Width { fn width(&self) -> u64; }
/// ```
///
/// ```compile_fail
/// use rtemis_r8_api as r8;
/// #[r8::methods(trait = "demo::Width")]
/// trait Width {}
/// ```
///
/// A type parameter that `dispatch(...)` does not name, an argument of
/// another type or name than its position's, and a position that is not a
/// Rust-stored type:
///
/// ```compile_fail
/// use rtemis_r8_api as r8;
/// #[r8::methods(trait = "demo::Meets")]
/// trait Meets<Y> { fn meets(&self, y: &Y) -> bool; }
/// ```
///
/// ```compile_fail
/// use rtemis_r8_api as r8;
/// #[r8::methods(trait = "demo::Meets", dispatch(x, y))]
/// trait Meets<Y> { fn meets(&self, y: f64) -> bool; }
/// ```
///
/// ```compile_fail
/// use rtemis_r8_api as r8;
/// #[r8::methods(trait = "demo::Meets", dispatch(x, z))]
/// trait Meets<Y> { fn meets(&self, y: &Y) -> bool; }
/// ```
///
/// ```compile_fail,E0277
/// use rtemis_r8::R8;
/// use rtemis_r8_api as r8;
/// #[derive(R8, Clone)]
/// #[r8(namespace = "demo", version = 1, storage = "rust")]
/// struct Interval { lo: f64 }
/// #[r8::methods(trait = "demo::Meets", dispatch(x, y))]
/// trait Meets<Y> { fn meets(&self, y: &Y) -> bool; }
/// impl Meets<f64> for Interval { fn meets(&self, _y: &f64) -> bool { true } }
/// r8::exports! {
///     package = "demo", namespace = "demo",
///     types = [Interval], impls = [Interval: Meets<f64>],
/// }
/// fn main() {}
/// ```
///
/// A field of a type that R8 does not convert, an open field that holds JSON,
/// a holder of a struct that is not extensible, a nested type without `Clone`,
/// and a trait object of a trait without `dyn`:
///
/// ```compile_fail,E0277
/// use rtemis_r8::R8;
/// #[derive(R8, Clone)]
/// #[r8(namespace = "demo", version = 1, storage = "rust")]
/// struct Axis { n: i64 }
/// ```
///
/// ```compile_fail,E0277
/// use rtemis_r8::R8;
/// use rtemis_r8::derive::SubtypeOf;
/// #[derive(R8, Clone)]
/// #[r8(namespace = "demo", version = 1, storage = "rust", extensible)]
/// struct Interval { lo: f64 }
/// #[derive(R8, Clone)]
/// #[r8(namespace = "demo", version = 1, storage = "rust")]
/// struct Axis { range: SubtypeOf<Interval> }
/// ```
///
/// ```compile_fail,E0277
/// use rtemis_r8::R8;
/// use rtemis_r8_api as r8;
/// #[derive(R8, Clone)]
/// #[r8(namespace = "demo", version = 1, storage = "rust")]
/// struct Interval { lo: f64 }
/// #[derive(R8, Clone)]
/// #[r8(namespace = "demo", version = 1, storage = "rust")]
/// struct Axis { range: r8::SubtypeValue<Interval> }
/// ```
///
/// ```compile_fail,E0277
/// use rtemis_r8::R8;
/// #[derive(R8)]
/// #[r8(namespace = "demo", version = 1, storage = "rust")]
/// struct Interval { lo: f64 }
/// #[derive(R8)]
/// #[r8(namespace = "demo", version = 1, storage = "rust")]
/// struct Axis { range: Interval }
/// ```
///
/// ```compile_fail,E0277
/// use rtemis_r8::R8;
/// use rtemis_r8_api as r8;
/// #[r8::methods(trait = "demo::Width")]
/// trait Width { fn width(&self) -> f64; }
/// #[derive(R8, Clone)]
/// #[r8(namespace = "demo", version = 1, storage = "rust")]
/// struct Axis { range: r8::ImplementorValue<dyn Width> }
/// ```
///
/// A trait that `traits` lists and Rust does not declare, for a parameter, a
/// result, or a replacement's result of the implementing type; one of another
/// namespace; a method's name in R that is not its identifier's; a constraint
/// on a dispatched parameter; and an accessor with a parameter:
///
/// ```compile_fail
/// use rtemis_r8_api as r8;
/// #[r8::methods(trait = "demo::near")]
/// trait Near { fn near(&self, y: &Self) -> bool; }
/// r8::exports! { package = "demo", namespace = "demo", types = [], traits = [Near], impls = [] }
/// fn main() {}
/// ```
///
/// ```compile_fail
/// use rtemis_r8_api as r8;
/// #[r8::methods(trait = "demo::near")]
/// trait Near { fn near(&self) -> Option<Self> where Self: Sized; }
/// r8::exports! { package = "demo", namespace = "demo", types = [], traits = [Near], impls = [] }
/// fn main() {}
/// ```
///
/// ```compile_fail
/// use rtemis_r8_api as r8;
/// #[r8::methods(trait = "demo::with_label")]
/// trait WithLabel { #[r8(name = "label<-")] fn with_label(&self, value: String) -> f64; }
/// r8::exports! { package = "demo", namespace = "demo", types = [], traits = [WithLabel], impls = [] }
/// fn main() {}
/// ```
///
/// ```compile_fail
/// use rtemis_r8_api as r8;
/// #[r8::methods(trait = "other::width")]
/// trait Width { fn width(&self) -> f64; }
/// r8::exports! { package = "demo", namespace = "demo", types = [], traits = [Width], impls = [] }
/// fn main() {}
/// ```
///
/// ```compile_fail
/// use rtemis_r8_api as r8;
/// #[r8::methods(trait = "demo::with_label")]
/// trait WithLabel { #[r8(name = "labels<-")] fn with_label(&self, value: String) -> Self; }
/// ```
///
/// ```compile_fail
/// use rtemis_r8_api as r8;
/// #[r8::methods(trait = "demo::meets", dispatch(x, y))]
/// trait Meets<Y> { fn meets(&self, #[r8(min = 0)] y: &Y) -> bool; }
/// ```
///
/// ```compile_fail
/// use rtemis_r8_api as r8;
/// #[r8::methods(trait = "demo::Named")]
/// trait Named { #[r8(field)] fn name(&self, n: i32) -> &String; fn f(&self) -> f64; }
/// ```
///
/// `dyn` on a trait that dispatches on two arguments, and on one that is not
/// dyn compatible:
///
/// ```compile_fail
/// use rtemis_r8_api as r8;
/// #[r8::methods(trait = "demo::Meets", dispatch(x, y), dyn)]
/// trait Meets<Y> { fn meets(&self, y: &Y) -> bool; }
/// ```
///
/// ```compile_fail,E0038
/// use rtemis_r8_api as r8;
/// #[r8::methods(trait = "demo::Shifted", dyn)]
/// trait Shifted { fn shifted(&self) -> Self; }
/// ```
///
/// `#[r8::function]` with a key reserved for a function declared in Rust,
/// with another key, on a method, on a function of a type parameter, and on an
/// `async` function:
///
/// ```compile_fail
/// use rtemis_r8_api as r8;
/// #[r8::function(declare)]
/// fn level(x: &[f64]) -> Vec<f64> { x.to_vec() }
/// ```
///
/// ```compile_fail
/// use rtemis_r8_api as r8;
/// #[r8::function(rule = "lo <= hi")]
/// fn span(lo: f64, hi: f64) -> f64 { hi - lo }
/// ```
///
/// ```compile_fail
/// use rtemis_r8_api as r8;
/// #[r8::function(namespace = "demo")]
/// fn span(lo: f64, hi: f64) -> f64 { hi - lo }
/// ```
///
/// ```compile_fail
/// use rtemis_r8_api as r8;
/// struct S;
/// impl S {
///     #[r8::function]
///     fn width(&self) -> f64 { 1.0 }
/// }
/// ```
///
/// ```compile_fail
/// use rtemis_r8_api as r8;
/// #[r8::function]
/// fn first<T: Copy>(x: &[f64]) -> f64 { x[0] }
/// ```
///
/// ```compile_fail
/// use rtemis_r8_api as r8;
/// #[r8::function]
/// async fn span(lo: f64, hi: f64) -> f64 { hi - lo }
/// ```
#[cfg(doctest)]
struct RefusedByTheMacros;
