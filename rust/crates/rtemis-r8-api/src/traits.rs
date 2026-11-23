//! Rust traits of R8's standard traits, which a type implements as any Rust
//! trait and `r8::exports!` lists. Each method's parameters follow the R
//! trait's: the first is `&self`, and `...` is a [`Dots`].

use rtemis_r8_macros::methods;

use crate::call::{Dots, Sexp};

/// `R8::Display`: the text of a value, `format(x, ...)`.
#[methods(trait = "R8::Display")]
pub trait Display {
    /// The text.
    fn format(&self, dots: Dots) -> String;
}

/// `R8::Debug`: the structure of a value, `str(object, ...)`.
#[methods(trait = "R8::Debug")]
pub trait Debug {
    /// The structure, printed by R.
    fn str(&self, dots: Dots) -> String;
}

/// `R8::Shape`: the dimensions of a value, `dim(x)`.
#[methods(trait = "R8::Shape")]
pub trait Shape {
    /// The dimensions.
    fn dim(&self) -> Vec<i32>;
}

/// `R8::PartialOrd`: the order of two values, the trait's method
/// `partial_cmp(x, y)`, which `r8_partial_cmp()`, `<`, `<=`, `>`, and `>=`
/// call in R, `None` when they are not ordered, which R receives as `NA`.
/// R8 calls the method by its qualified path, so a type that also
/// implements `std::cmp::PartialOrd` keeps both.
#[methods(trait = "R8::PartialOrd")]
pub trait PartialOrd {
    /// The order of `self` and `y`.
    fn partial_cmp(&self, y: Sexp) -> Option<std::cmp::Ordering>;
}
