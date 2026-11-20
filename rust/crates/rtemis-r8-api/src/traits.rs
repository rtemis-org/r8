//! Rust traits of R8's standard traits, for impls written in Rust with
//! `#[r8::methods(trait = "R8::Display")]` and its siblings. Each method's
//! parameters follow the R trait's: `x` is `&self`, and `...` is a
//! [`Dots`].

use crate::call::Dots;

/// `R8::Display`: the text of a value, `format(x, ...)`.
pub trait Display {
    /// The text.
    fn format(&self, dots: Dots) -> String;
}

/// `R8::Debug`: the structure of a value, `str(object, ...)`.
pub trait Debug {
    /// The structure, printed by R.
    fn str(&self, dots: Dots) -> String;
}

/// `R8::Dim`: the dimensions of a value, `dim(x)`.
pub trait Dim {
    /// The dimensions.
    fn dim(&self) -> Vec<i32>;
}

/// `R8::PartialOrd`: the order of two values, `r8_cmp(x, y)`: negative,
/// zero, or positive, or `None` when they are not ordered.
pub trait PartialOrd {
    /// The order of `self` and `other`.
    fn r8_cmp(&self, other: crate::call::Sexp) -> Option<i32>;
}
