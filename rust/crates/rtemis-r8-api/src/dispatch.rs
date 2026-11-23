//! The dispatched positions of an impl: [`Position`], which every Rust-stored
//! type and [`Any`] implement.

use std::ffi::CStr;
use std::ops::Deref;

use crate::api::Result;
use crate::call::{Receiver, Sexp};
use crate::rust_stored::RustStored;
use crate::sys::SEXP;

/// What a position of an impl names.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PositionEntry {
    /// A type, by its qualified name.
    Type(&'static CStr),
    /// Any value.
    Any,
}

/// A dispatched position of a trait after the first, a type parameter of the
/// Rust trait: a Rust-stored type, whose method takes the type's values and
/// those of the structs that extend it, or [`Any`].
#[diagnostic::on_unimplemented(
    message = "`{Self}` is not a dispatched position",
    note = "a position is a Rust-stored type, declared with `#[derive(R8)]` and `#[r8(storage = \"rust\")]`, or `rtemis_r8_api::Any`"
)]
pub trait Position: Sized + 'static {
    /// What the position names.
    const ENTRY: PositionEntry;

    /// The argument of a call.
    type Arg<'a>: Deref<Target = Self>;

    /// The argument `name` of a call that dispatch selected the impl for.
    ///
    /// # Safety
    ///
    /// On R's main thread, with a valid `x` that stays protected while the
    /// argument lives.
    unsafe fn from_dispatched<'a>(x: SEXP, name: &str) -> Result<Self::Arg<'a>>;
}

impl<T: RustStored> Position for T {
    const ENTRY: PositionEntry = PositionEntry::Type(T::TYPE_NAME);

    type Arg<'a> = Receiver<'a, T>;

    unsafe fn from_dispatched<'a>(x: SEXP, name: &str) -> Result<Self::Arg<'a>> {
        unsafe { Receiver::from_dispatched_arg(x, name) }
    }
}

/// A position of any value, `impl Overlaps<r8::Any> for IRanges`: the method
/// for the values that no more specific impl takes.
#[derive(Debug, Clone, Copy)]
pub struct Any(Sexp);

impl Any {
    /// The R object, which R protects during the call.
    pub fn sexp(&self) -> Sexp {
        self.0
    }
}

/// The argument of a position of [`Any`].
#[derive(Debug, Clone, Copy)]
pub struct AnyArg(Any);

impl Deref for AnyArg {
    type Target = Any;
    fn deref(&self) -> &Any {
        &self.0
    }
}

impl Position for Any {
    const ENTRY: PositionEntry = PositionEntry::Any;

    type Arg<'a> = AnyArg;

    unsafe fn from_dispatched<'a>(x: SEXP, _name: &str) -> Result<Self::Arg<'a>> {
        Ok(AnyArg(Any(Sexp(x))))
    }
}
