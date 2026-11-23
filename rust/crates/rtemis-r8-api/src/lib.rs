//! R8's C services for an R package's Rust code: Rust-stored types, whose
//! values R8 holds in Rust, and impls of R8 traits written in Rust.
//!
//! A package declares a struct with `#[derive(R8)]` and `#[r8(storage = "rust")]`,
//! implements its traits with `#[r8::methods(trait = "pkg::Trait")]` on an
//! ordinary Rust trait impl, and lists both in `r8::exports!`, which emits
//! `r8_init()` for the package's initialization routine. The traits are
//! declared in R, as for impls written in R.
//!
//! The crate reaches R through its own declarations of R's C API (`sys`)
//! and R8 through `R8Api` (`R8.h`). Every R function that can jump runs
//! inside `R8Api.protect`, so a jump skips no Rust destructor.

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
pub use call::{Dots, FromR, IntoR, Receiver, Sexp, Value};
pub use exports::{ImplSet, MethodRow};
pub use field::Field;
pub use reader::{Node, Reader};
pub use rust_stored::{RustStored, RustStoredEnum, RustStoredStruct, Shared, codec, enum_codec};
pub use state::{StateReader, StateWriter};
pub use traits::{Debug, Display, PartialOrd, Shape};

#[doc(hidden)]
pub use rtemis_r8;
pub use rtemis_r8_macros::{exports, methods};
