//! R8's C services for an R package's Rust code: Rust-stored types, whose
//! values R8 holds in Rust, and impls of R8 traits written in Rust.
//!
//! A package declares a struct with `#[derive(R8)]` and `#[r8(storage = "rust")]`,
//! marks the Rust trait of each R8 trait with
//! `#[r8::methods(trait = "ns::Trait")]`, implements it as any Rust trait,
//! and lists the types and the impls in `r8::exports!`, which emits
//! `r8_init_<package>()` for the package's initialization routine. The R8
//! traits are declared in R, as for impls written in R.
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
//! The crate reaches R through its own declarations of R's C API (`sys`)
//! and R8 through `R8Api` (`R8.h`). Every R function that can jump runs
//! inside `R8Api.protect`, so a jump skips no Rust destructor.

// The macros name this crate `::rtemis_r8_api`, in the crate itself too.
extern crate self as rtemis_r8_api;

pub mod api;
pub mod buffer;
pub mod call;
pub mod exports;
pub mod field;
pub mod keep;
pub mod reader;
pub mod rust_stored;
pub mod state;
pub mod sys;
pub mod traits;

pub use api::{Error, Jumped, Result};
pub use buffer::{Buffer, Element, Logical};
pub use call::{Dots, FromR, IntoR, Receiver, Sexp};
pub use exports::{ImplSet, MethodRow};
pub use field::Field;
pub use reader::{Node, Reader};
pub use rust_stored::{
    RustStored, RustStoredEnum, RustStoredStruct, Shared, codec, enum_codec, module,
};
pub use state::{StateReader, StateWriter};
pub use traits::{Debug, Display, PartialOrd, Shape};

#[doc(hidden)]
pub use rtemis_r8;
pub use rtemis_r8_macros::{exports, methods};

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
#[cfg(doctest)]
struct RefusedByTheMacros;
