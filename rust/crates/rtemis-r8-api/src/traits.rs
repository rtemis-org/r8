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

/// `R8::Shape`: the dimensions of a value, `dim(x)`.
pub trait Shape {
    /// The dimensions.
    fn dim(&self) -> Vec<i32>;
}

/// `R8::PartialOrd`: the order of two values, the trait's method
/// `partial_cmp(x, y)`, which `r8_partial_cmp()`, `<`, `<=`, `>`, and `>=`
/// call in R, `None` when they are not ordered, which R receives as `NA`.
/// The macro calls the method by its qualified path, so a type that also
/// implements `std::cmp::PartialOrd` keeps both.
pub trait PartialOrd {
    /// The order of `self` and `other`.
    fn partial_cmp(&self, other: crate::call::Sexp) -> Option<std::cmp::Ordering>;
}
