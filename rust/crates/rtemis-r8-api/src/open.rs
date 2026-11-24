//! Open fields of Rust-stored types: [`SubtypeValue`], a field of any struct
//! that extends another, `subtype(P)`, and [`ImplementorValue`], a field of
//! any implementor of a trait, `dyn(Trait)`. A holder keeps the R value,
//! whether or not the package knows its type, and gives it back unchanged.
//! spec: R8/class-system#rust-impls-and-rust-stored-types-rtemis-r8-api

use std::ffi::{CString, c_void};
use std::fmt;
use std::marker::PhantomData;
use std::sync::{Arc, OnceLock};

use rtemis_r8::abi::{self, OK};
use rtemis_r8::derive::{Encode, Extensible, Subtype, TraitObject, implementor_type};
use rtemis_r8::ir::FieldType;
use rtemis_r8::rules::DocumentSet;
use serde_json::Value;

use crate::api::{self, Error, Jumped, Result};
use crate::call::{FromR, IntoR, Receiver, field_list, r_type, rust_value};
use crate::field::Field;
use crate::keep::{self, Token};
use crate::reader::{Node, Reader};
use crate::rust_stored::{RustStored, module};
use crate::signature::ParamType;
use crate::state::{StateReader, StateWriter};
use crate::sys::{self, SEXP};

/// A kept R value.
struct Held {
    token: Token,
    /// The qualified name of its type.
    type_name: String,
    /// The value's state, which R8 lends to this module, and which lives as
    /// long as the value; NULL for a value whose fields are read instead.
    state: *const c_void,
}

// The state is read only, and the value is reached on R's main thread.
unsafe impl Send for Held {}
unsafe impl Sync for Held {}

/// An error of the argument `name`.
fn named(name: &str, e: Error) -> Error {
    Error::new(e.code, format!("`{name}`: {}", e.message))
}

fn no_r(what: &str) -> Error {
    Error::new(
        "main_thread",
        format!("{what} reads the R value, on R's main thread"),
    )
}

impl Held {
    /// Keeps `x`, an R8 value, which nothing else need protect once
    /// [`keep::prepare`] has run.
    ///
    /// # Safety
    ///
    /// On R's main thread, with a valid `x`.
    unsafe fn new(x: SEXP) -> Result<Self> {
        let token = unsafe { Token::keep(x) }?;
        let type_name = unsafe { r_type(x) }?.0;
        let name =
            CString::new(type_name.as_str()).map_err(|e| Error::new("bad_name", e.to_string()))?;
        let lend = api::api()?
            .value_state
            .ok_or_else(|| Error::new("no_api", "R8's API has no `value_state`"))?;
        let mut state: *const c_void = std::ptr::null();
        let mut err = api::blank();
        if unsafe { lend(x, name.as_ptr(), module(), &mut state, &mut err) } != OK {
            let e = Error::from_abi(&err);
            // A state that is not lent: the value's fields are read, or
            // refused, when the code reads the value as a type that it names.
            if !matches!(
                e.code.as_str(),
                "not_lent" | "not_pinned" | "declaration_changed"
            ) {
                return Err(e);
            }
            state = std::ptr::null();
        }
        Ok(Held {
            token,
            type_name,
            state,
        })
    }

    fn downcast_ref<T: RustStored>(&self) -> Result<Option<Receiver<'_, T>>> {
        if self.type_name != T::NAME {
            return Ok(None);
        }
        if !self.state.is_null() {
            return Ok(Some(Receiver::Borrowed(unsafe {
                &*self.state.cast::<T>()
            })));
        }
        if !keep::on_main_thread() {
            return Err(no_r("`downcast_ref()` of a value of another package"));
        }
        unsafe { Receiver::from_named_arg(self.token.get(), "value") }.map(Some)
    }
}

/// The value of `P` that a held value of a subtype holds.
enum Base<P> {
    /// In the held value's state.
    Lent(*const P),
    /// Built from the held value's fields.
    Built(Arc<P>),
    /// In place of the held value's, whose other fields stay.
    Replaced(Arc<Replaced<P>>),
}

/// Another value of `P`, and the R value that holds it, once built.
struct Replaced<P> {
    base: P,
    held: OnceLock<Held>,
}

// The pointer is to a state that the holder's token keeps alive, and the state
// is read only.
unsafe impl<P: Send + Sync> Send for Base<P> {}
unsafe impl<P: Send + Sync> Sync for Base<P> {}

impl<P> Clone for Base<P> {
    fn clone(&self) -> Self {
        match self {
            Base::Lent(p) => Base::Lent(*p),
            Base::Built(p) => Base::Built(Arc::clone(p)),
            Base::Replaced(p) => Base::Replaced(Arc::clone(p)),
        }
    }
}

/// A field of `P` or of any struct that extends it, `subtype(P)`, in a
/// Rust-stored type; a clone shares the R value.
///
/// The fields that `P` declares are the value of `P` ([`Subtype::upcast`]),
/// so the holder implements the interface traits of `P` and its ancestors.
/// They are read on any thread, without a copy for a value whose state this
/// package's code built.
///
/// `with_p(value)` gives a holder with another value of `P`; its R value is
/// built and validated, with the rules of its own type, when it is first
/// read: by R, by [`downcast_ref`](SubtypeValue::downcast_ref), or for the
/// bytes of the state. A value that violates them is the error
/// `invalid_value` there.
///
/// ```no_run,standalone_crate
/// use rtemis_r8::R8;
/// use rtemis_r8_api as r8;
///
/// /// A numeric interval.
/// #[derive(R8, Clone)]
/// #[r8(namespace = "demo", version = 1, storage = "rust", extensible)]
/// pub struct Interval {
///     pub lo: f64,
///     pub hi: f64,
/// }
///
/// /// A labeled interval.
/// #[derive(R8, Clone)]
/// #[r8(namespace = "demo", version = 1, storage = "rust")]
/// pub struct Band {
///     #[r8(extends)]
///     pub base: Interval,
///     pub label: String,
/// }
///
/// /// An axis over any interval.
/// #[derive(R8, Clone)]
/// #[r8(namespace = "demo", version = 1, storage = "rust")]
/// pub struct Axis {
///     pub range: r8::SubtypeValue<Interval>,
/// }
///
/// #[r8::methods(trait = "demo::Title")]
/// pub trait Title {
///     fn title(&self) -> r8::Result<String>;
/// }
///
/// impl Title for Axis {
///     fn title(&self) -> r8::Result<String> {
///         Ok(match self.range.downcast_ref::<Band>()? {
///             Some(band) => band.label.clone(),
///             None => format!("{} to {}", self.range.lo(), self.range.hi()),
///         })
///     }
/// }
///
/// r8::exports! {
///     package = "demo",
///     namespace = "demo",
///     types = [Interval, Band, Axis],
///     impls = [Axis: Title],
/// }
/// # fn main() {}
/// ```
pub struct SubtypeValue<P: RustStored + Extensible> {
    held: Arc<Held>,
    base: Base<P>,
}

impl<P: RustStored + Extensible> Clone for SubtypeValue<P> {
    fn clone(&self) -> Self {
        SubtypeValue {
            held: Arc::clone(&self.held),
            base: self.base.clone(),
        }
    }
}

impl<P: RustStored + Extensible> fmt::Debug for SubtypeValue<P> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "SubtypeValue({})", self.held.type_name)
    }
}

impl<P: RustStored + Extensible> SubtypeValue<P> {
    /// The holder of `value`, as an R value; called on R's main thread.
    pub fn new<T: RustStored + Subtype<P>>(value: T) -> Result<Self> {
        unsafe { Self::of(new_r_value(value)?) }
    }

    /// The holder of `x`, a valid value of a subtype of `P`, which nothing
    /// else need protect once [`keep::prepare`] has run.
    unsafe fn of(x: SEXP) -> Result<Self> {
        let held = unsafe { Held::new(x) }?;
        let base = match unsafe { Receiver::<P>::from_dispatched_arg(x, "value") }? {
            Receiver::Borrowed(p) => Base::Lent(p),
            Receiver::Built(p) => Base::Built(Arc::new(p)),
        };
        Ok(SubtypeValue {
            held: Arc::new(held),
            base,
        })
    }

    /// The qualified name of the value's type.
    pub fn type_name(&self) -> &str {
        &self.held.type_name
    }

    /// The value as a `T`, when its type is `T` exactly: its state, borrowed,
    /// or a value built from its fields, on R's main thread.
    pub fn downcast_ref<T: RustStored + Subtype<P>>(&self) -> Result<Option<Receiver<'_, T>>> {
        if self.held.type_name != T::NAME {
            return Ok(None);
        }
        self.current()?.downcast_ref()
    }

    /// The R value, built first for a holder with another value of `P`.
    fn current(&self) -> Result<&Held> {
        let Base::Replaced(r) = &self.base else {
            return Ok(&self.held);
        };
        if let Some(held) = r.held.get() {
            return Ok(held);
        }
        if !keep::on_main_thread() {
            return Err(no_r("a holder with another value of its struct"));
        }
        let fields = field_list(Box::new(r.base.clone()))?;
        let names = fields.named()?;
        let f = api::api()?
            .with_fields
            .ok_or_else(|| Error::new("no_api", "R8's API has no `with_fields`"))?;
        let mut value: SEXP = unsafe { sys::R_NilValue };
        let mut err = api::blank();
        if unsafe { f(self.held.token.get(), names, &mut value, &mut err) } != OK {
            return Err(Error::from_abi(&err));
        }
        let held = unsafe { Held::new(value) }?;
        Ok(r.held.get_or_init(|| held))
    }
}

/// The R value of `value`, unprotected, for a holder that keeps it.
fn new_r_value<T: RustStored>(value: T) -> Result<SEXP> {
    if !keep::on_main_thread() {
        return Err(no_r("a holder of a new value"));
    }
    unsafe { keep::prepare() }?;
    rust_value(value).map_err(Jumped::into_error)
}

/// The R value of the node `n`, unprotected, for a holder that keeps it.
fn node_value<'a>(r: &Reader<'a>, n: Node<'a>, kind: u32, name: &str) -> Result<SEXP> {
    unsafe { keep::prepare() }?;
    let name = CString::new(name).map_err(|e| Error::new("bad_name", e.to_string()))?;
    r.value(n, kind, &name)
}

impl<P: Subtype<Q> + RustStored + Extensible, Q> Subtype<Q> for SubtypeValue<P> {
    fn upcast(&self) -> &Q {
        let base: &P = match &self.base {
            Base::Lent(p) => unsafe { &**p },
            Base::Built(p) => p,
            Base::Replaced(r) => &r.base,
        };
        base.upcast()
    }
    fn with_upcast(&self, value: Q) -> Self {
        let base: &P = <Self as Subtype<P>>::upcast(self);
        SubtypeValue {
            held: Arc::clone(&self.held),
            base: Base::Replaced(Arc::new(Replaced {
                base: base.with_upcast(value),
                held: OnceLock::new(),
            })),
        }
    }
}

impl<P: RustStored + Extensible> Field for SubtypeValue<P> {
    unsafe fn from_r(x: SEXP) -> Result<Self> {
        unsafe { Self::of(x) }
    }
    fn from_node<'a>(r: &Reader<'a>, n: Node<'a>) -> Result<Self> {
        let x = node_value(r, n, abi::VALUE_SUBTYPE, P::NAME)?;
        unsafe { Self::of(x) }
    }
    fn to_r(&self, _owner: SEXP) -> Result<SEXP> {
        Ok(self.current()?.token.get())
    }
    fn write(&self, out: &mut StateWriter) -> Result<()> {
        out.keep(self.current()?.token.get())
    }
    fn read(input: &mut StateReader) -> Result<Self> {
        unsafe { Self::of(input.kept()?) }
    }
    fn owned_bytes(&self) -> usize {
        let base: &P = match &self.base {
            // The R value accounts for its state.
            Base::Lent(_) => return 0,
            Base::Built(p) => p,
            Base::Replaced(r) => &r.base,
        };
        std::mem::size_of::<P>() + base.heap_bytes()
    }
}

impl<P: RustStored + Extensible> FromR for SubtypeValue<P> {
    fn param_type() -> ParamType {
        ParamType::Typed(<Self as Encode>::field_type())
    }
    fn register(set: &mut DocumentSet) {
        P::register(set);
    }
    unsafe fn from_arg(x: SEXP, name: &str) -> Result<Self> {
        unsafe { Self::of(x) }.map_err(|e| named(name, e))
    }
}

impl<P: RustStored + Extensible> IntoR for SubtypeValue<P> {
    fn return_type() -> Option<FieldType> {
        Some(<Self as Encode>::field_type())
    }
    fn register(set: &mut DocumentSet) {
        P::register(set);
    }
    fn into_r(self) -> std::result::Result<SEXP, Jumped> {
        Ok(self.current()?.token.get())
    }
}

/// The reason that a kept R value has no JSON form in Rust.
fn kept_in_r<T>(name: &str) -> std::result::Result<T, String> {
    Err(format!(
        "the value of a `{name}` is an R value, whose document R reads and writes"
    ))
}

impl<P: RustStored + Extensible> Encode for SubtypeValue<P> {
    fn field_type() -> FieldType {
        FieldType::Subtype {
            of: P::qualified_name(),
        }
    }
    fn encode(&self) -> std::result::Result<Value, String> {
        kept_in_r("SubtypeValue")
    }
    fn decode(_v: &Value) -> std::result::Result<Self, String> {
        kept_in_r("SubtypeValue")
    }
    fn register(set: &mut DocumentSet) {
        P::register(set);
    }
}

/// A field of any implementor of a trait, `dyn(Trait)`, in a Rust-stored type,
/// written `ImplementorValue<dyn Shape>`; a clone shares the R value.
///
/// The trait is one of `#[r8::methods(trait = "ns::Shape", dyn)]`, or of R8's
/// standard traits `Display`, `Debug`, `Shape`, and `PartialOrd`. R8 decides
/// which values implement it, where it validates the field; Rust code calls a
/// trait method on a value of a type that it knows
/// ([`downcast_ref`](ImplementorValue::downcast_ref)).
pub struct ImplementorValue<D: TraitObject + ?Sized> {
    held: Arc<Held>,
    of: PhantomData<fn() -> Box<D>>,
}

impl<D: TraitObject + ?Sized> Clone for ImplementorValue<D> {
    fn clone(&self) -> Self {
        ImplementorValue {
            held: Arc::clone(&self.held),
            of: PhantomData,
        }
    }
}

impl<D: TraitObject + ?Sized> fmt::Debug for ImplementorValue<D> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "ImplementorValue({})", self.held.type_name)
    }
}

impl<D: TraitObject + ?Sized> ImplementorValue<D> {
    /// The holder of `value`, as an R value; called on R's main thread.
    pub fn new<T: RustStored>(value: T) -> Result<Self> {
        unsafe { Self::of(new_r_value(value)?) }
    }

    unsafe fn of(x: SEXP) -> Result<Self> {
        Ok(ImplementorValue {
            held: Arc::new(unsafe { Held::new(x) }?),
            of: PhantomData,
        })
    }

    /// The qualified name of the value's type.
    pub fn type_name(&self) -> &str {
        &self.held.type_name
    }

    /// The value as a `T`, when its type is `T`: its state, borrowed, or a
    /// value built from its fields, on R's main thread.
    pub fn downcast_ref<T: RustStored>(&self) -> Result<Option<Receiver<'_, T>>> {
        self.held.downcast_ref()
    }
}

impl<D: TraitObject + ?Sized + 'static> Field for ImplementorValue<D> {
    unsafe fn from_r(x: SEXP) -> Result<Self> {
        unsafe { Self::of(x) }
    }
    fn from_node<'a>(r: &Reader<'a>, n: Node<'a>) -> Result<Self> {
        let x = node_value(r, n, abi::VALUE_IMPLEMENTOR, D::NAME)?;
        unsafe { Self::of(x) }
    }
    fn to_r(&self, _owner: SEXP) -> Result<SEXP> {
        Ok(self.held.token.get())
    }
    fn write(&self, out: &mut StateWriter) -> Result<()> {
        out.keep(self.held.token.get())
    }
    fn read(input: &mut StateReader) -> Result<Self> {
        unsafe { Self::of(input.kept()?) }
    }
}

impl<D: TraitObject + ?Sized> FromR for ImplementorValue<D> {
    fn param_type() -> ParamType {
        ParamType::Typed(implementor_type::<D>())
    }
    unsafe fn from_arg(x: SEXP, name: &str) -> Result<Self> {
        unsafe { Self::of(x) }.map_err(|e| named(name, e))
    }
}

impl<D: TraitObject + ?Sized> IntoR for ImplementorValue<D> {
    fn return_type() -> Option<FieldType> {
        Some(implementor_type::<D>())
    }
    fn into_r(self) -> std::result::Result<SEXP, Jumped> {
        Ok(self.held.token.get())
    }
}

impl<D: TraitObject + ?Sized> Encode for ImplementorValue<D> {
    fn field_type() -> FieldType {
        implementor_type::<D>()
    }
    fn encode(&self) -> std::result::Result<Value, String> {
        kept_in_r("ImplementorValue")
    }
    fn decode(_v: &Value) -> std::result::Result<Self, String> {
        kept_in_r("ImplementorValue")
    }
}
