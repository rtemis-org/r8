//! Rust traits of R8's standard traits, which a type implements as any Rust
//! trait and `r8::exports!` lists. Each method's parameters follow the R
//! trait's: the first is `&self`, and `...` is a [`Dots`].

use rtemis_r8::abi;
use rtemis_r8_macros::methods;

use crate::api::Result;
use crate::call::{self, Dots, IntoR, Receiver, Sexp};
use crate::dispatch::Position;
use crate::exports::{ImplSet, MethodRow};
use crate::rust_stored::RustStored;
use crate::signature;

/// `R8::Display`: the text of a value, `format(x, ...)`.
#[methods(trait = "R8::Display", dyn)]
pub trait Display {
    /// The text.
    fn format(&self, dots: Dots) -> String;
}

/// `R8::Debug`: the structure of a value, `str(object, ...)`.
#[methods(trait = "R8::Debug", dyn)]
pub trait Debug {
    /// The structure, printed by R.
    fn str(&self, dots: Dots) -> String;
}

/// `R8::Shape`: the dimensions of a value, `dim(x)`.
#[methods(trait = "R8::Shape", dyn)]
pub trait Shape {
    /// The dimensions.
    fn dim(&self) -> Vec<i32>;
}

/// `R8::PartialOrd`: the order of two values, the trait's method
/// `partial_cmp(x, y)`, which `r8partial_cmp()`, `<`, `<=`, `>`, and `>=`
/// call in R, `None` when they are not ordered, which R receives as `NA`.
/// R8 calls the method by its qualified path, so a type that also
/// implements `std::cmp::PartialOrd` keeps both.
#[methods(trait = "R8::PartialOrd", dyn)]
pub trait PartialOrd {
    /// The order of `self` and `y`.
    fn partial_cmp(&self, y: Sexp) -> Option<std::cmp::Ordering>;
}

/// `R8::From(Source)`: the conversion of a `Source` to the type, which
/// `r8convert(x, Type)` and `Type@from(x)` call. A value of a struct that
/// extends `Source` converts through the `Source` that it holds, when its own
/// type has no conversion. It is `std::convert::From` with the source by
/// reference, as R8 lends it, and with an error.
///
/// ```no_run,standalone_crate
/// # use rtemis_r8::R8;
/// # use rtemis_r8_api as r8;
/// # #[derive(R8, Clone)]
/// # #[r8(namespace = "demo", version = 1, storage = "rust")]
/// # pub struct Celsius { pub degrees: f64 }
/// # #[derive(R8, Clone)]
/// # #[r8(namespace = "demo", version = 1, storage = "rust")]
/// # pub struct Kelvin { pub degrees: f64 }
/// impl r8::From<Celsius> for Kelvin {
///     fn from(source: &Celsius) -> r8::Result<Self> {
///         Ok(Kelvin { degrees: source.degrees + 273.15 })
///     }
/// }
/// # r8::exports! {
/// #     package = "demo", namespace = "demo",
/// #     types = [Celsius, Kelvin],
/// #     impls = [Kelvin: r8::From<Celsius>],
/// # }
/// # fn main() {}
/// ```
pub trait From<Source>: Sized {
    /// The value of the type from `source`.
    fn from(source: &Source) -> Result<Self>;

    #[doc(hidden)]
    fn __r8_methods() -> ImplSet
    where
        Self: RustStored,
        Source: RustStored,
    {
        unsafe extern "C" fn from<S: RustStored, T: From<S> + RustStored>(
            _inputs: *const abi::CallInputs,
            args: *const abi::Sexp,
            nargs: usize,
            result: *mut abi::Sexp,
            err: *mut abi::Error,
        ) -> abi::Status {
            unsafe {
                call::method(args, nargs, 1, result, err, |a| {
                    let source = Receiver::<S>::from_arg(a[0])?;
                    <T as From<S>>::from(&source).into_r()
                })
            }
        }
        ImplSet {
            trait_name: c"R8::From",
            positions: vec![<Self as Position>::ENTRY, <Source as Position>::ENTRY],
            methods: vec![MethodRow {
                name: c"from",
                call: from::<Source, Self>,
                signature: signature::method(
                    "from",
                    "",
                    vec![signature::dispatched("", false, "")],
                    None,
                ),
            }],
            tag: None,
            required_fields: Vec::new(),
            register: |set| {
                <Self as Position>::register(set);
                <Source as Position>::register(set);
            },
        }
    }
}
