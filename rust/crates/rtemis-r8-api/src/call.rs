//! Calls of impl functions: the conversion of arguments from R
//! ([`FromR`]) and of results to R ([`IntoR`]), the receiver `x`
//! ([`Receiver`]), `...` ([`Dots`]), and [`method`], which the functions
//! that `#[r8::methods]` generates call.

use std::any::TypeId;
use std::cell::Cell;
use std::collections::HashMap;
use std::ffi::{CStr, CString, c_void};
use std::ops::Deref;
use std::panic::{AssertUnwindSafe, catch_unwind};
use std::sync::Mutex;

use rtemis_r8::abi::{self, ERROR, OK, Status, UNWIND};
use rtemis_r8::derive::Encode;
use rtemis_r8::ir::FieldType;
use rtemis_r8::rules::DocumentSet;

use crate::api::{self, Error, Jumped, Result};
use crate::buffer::Buffer;
use crate::field::{ElementType, Field};
use crate::keep::{self, Token};
use crate::rust_stored::{RustStored, module, panic_text};
use crate::signature::ParamType;
use crate::sys::{self, SEXP};

/// An argument of an impl function, converted from its R value.
#[diagnostic::on_unimplemented(
    message = "`{Self}` is not a parameter of an R8 method",
    note = "a parameter is a `bool`, an `i32`, an `f64`, a `String`, a `Buffer`, a `SubtypeValue`, an `ImplementorValue`, an `Option` of one, a `Sexp`, a `Dots`, or `&T` or `Option<&T>` of a Rust-stored type `T`"
)]
pub trait FromR: Sized {
    /// The R8 type of the parameter.
    fn param_type() -> ParamType;

    /// Adds the declarations of the types that the parameter's names.
    fn register(_set: &mut DocumentSet) {}

    /// The argument from `x`; `name` names the parameter in errors.
    ///
    /// # Safety
    ///
    /// On R's main thread, with a valid `x`.
    unsafe fn from_arg(x: SEXP, name: &str) -> Result<Self>;
}

fn invalid(name: &str, e: Error) -> Error {
    Error::new("invalid_argument", format!("`{name}`: {}", e.message))
}

macro_rules! from_field {
    ($($t:ty),*) => {$(
        impl FromR for $t {
            fn param_type() -> ParamType {
                ParamType::Typed(<$t as Encode>::field_type())
            }
            unsafe fn from_arg(x: SEXP, name: &str) -> Result<Self> {
                unsafe { <$t as Field>::from_r(x) }.map_err(|e| invalid(name, e))
            }
        }
    )*};
}
from_field!(bool, i32, String);

impl FromR for f64 {
    fn param_type() -> ParamType {
        ParamType::Typed(<f64 as Encode>::field_type())
    }
    unsafe fn from_arg(x: SEXP, name: &str) -> Result<Self> {
        // An integer argument reads as a double, as R's arithmetic does.
        if unsafe { sys::TYPEOF(x) } == sys::INTSXP && unsafe { sys::Rf_xlength(x) } == 1 {
            let v = unsafe { sys::INTEGER_ELT(x, 0) };
            if v == i32::MIN {
                return Err(invalid(name, Error::new("na_value", "the integer is NA")));
            }
            return Ok(f64::from(v));
        }
        let v = unsafe { <f64 as Field>::from_r(x) }.map_err(|e| invalid(name, e))?;
        if v.to_bits() == unsafe { sys::R_NaReal }.to_bits() {
            return Err(invalid(name, Error::new("na_value", "the double is NA")));
        }
        Ok(v)
    }
}

impl<T: FromR> FromR for Option<T> {
    fn param_type() -> ParamType {
        T::param_type().optional()
    }
    fn register(set: &mut DocumentSet) {
        T::register(set);
    }
    unsafe fn from_arg(x: SEXP, name: &str) -> Result<Self> {
        if x == unsafe { sys::R_NilValue } {
            return Ok(None);
        }
        unsafe { T::from_arg(x, name) }.map(Some)
    }
}

impl<T: ElementType> FromR for Buffer<T> {
    fn param_type() -> ParamType {
        ParamType::Typed(T::buffer_type())
    }
    unsafe fn from_arg(x: SEXP, name: &str) -> Result<Self> {
        unsafe { Buffer::from_r(x) }.map_err(|e| invalid(name, e))
    }
}

/// The arguments R passed as `...`, a list.
#[derive(Debug, Clone, Copy)]
pub struct Dots(pub SEXP);

impl Dots {
    /// The number of arguments.
    pub fn len(&self) -> usize {
        unsafe { sys::Rf_xlength(self.0) as usize }
    }

    /// Whether `...` was empty.
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

impl FromR for Dots {
    fn param_type() -> ParamType {
        ParamType::Dots
    }
    unsafe fn from_arg(x: SEXP, _name: &str) -> Result<Self> {
        Ok(Dots(x))
    }
}

/// The receiver `x` of a method of a Rust-stored type, or the value of a
/// parameter `&T`: the state of a Rust-stored value, borrowed, or a value
/// built from the fields of one whose state R8 does not lend
/// ([`RustStored::from_value`]).
pub enum Receiver<'a, T> {
    /// The state of a Rust-stored value, or the ancestor's state that it
    /// embeds.
    Borrowed(&'a T),
    /// A value built from a value's fields.
    Built(T),
}

impl<T> Deref for Receiver<'_, T> {
    type Target = T;
    fn deref(&self) -> &T {
        match self {
            Receiver::Borrowed(t) => t,
            Receiver::Built(t) => t,
        }
    }
}

impl<T> Receiver<'_, T> {
    /// The receiver from `x`, a value of `T` or of a struct that extends it,
    /// to which the method for `T` applies; an object of another type returns
    /// `type_mismatch`.
    ///
    /// # Safety
    ///
    /// On R's main thread, with a valid `x` that stays protected while the
    /// receiver lives.
    pub unsafe fn from_arg(x: SEXP) -> Result<Self>
    where
        T: RustStored,
    {
        unsafe { Self::from_dispatched_arg(x, "x") }
    }

    /// The value of the dispatched argument `name`, a value of `T` or of a
    /// struct that extends it, to which the method for `T` applies; an object
    /// of another type returns `type_mismatch`, naming the argument.
    ///
    /// # Safety
    ///
    /// On R's main thread, with a valid `x` that stays protected while the
    /// value lives.
    pub unsafe fn from_dispatched_arg(x: SEXP, name: &str) -> Result<Self>
    where
        T: RustStored,
    {
        unsafe { Self::lent(x, name, api::api()?.upcast_state) }
    }

    /// The value of the argument `name`, a parameter `&T` of a method: a
    /// value of `T` exactly; an object of another type returns
    /// `type_mismatch`, naming the argument.
    ///
    /// # Safety
    ///
    /// On R's main thread, with a valid `x` that stays protected while the
    /// value lives.
    pub unsafe fn from_named_arg(x: SEXP, name: &str) -> Result<Self>
    where
        T: RustStored,
    {
        unsafe { Self::lent(x, name, api::api()?.value_state) }
    }

    /// The value of `T` in `x` through `lend`, which lends its state or
    /// reports a value whose fields are read instead.
    unsafe fn lent(x: SEXP, name: &str, lend: Option<abi::StateFn>) -> Result<Self>
    where
        T: RustStored,
    {
        let mut state: *const c_void = std::ptr::null();
        let mut err = api::blank();
        let Some(f) = lend else {
            return Err(Error::new(
                "no_api",
                "R8's API lends no state of a value; update R8",
            ));
        };
        let status = unsafe { f(x, T::TYPE_NAME.as_ptr(), module(), &mut state, &mut err) };
        if status == OK {
            return Ok(Receiver::Borrowed(unsafe { &*state.cast::<T>() }));
        }
        let mut e = Error::from_abi(&err);
        if e.code == "not_pinned" {
            pin::<T>()?;
            err = api::blank();
            unsafe { f(x, T::TYPE_NAME.as_ptr(), module(), &mut state, &mut err) };
            e = Error::from_abi(&err);
        }
        match e.code.as_str() {
            "not_lent" => {}
            "type_mismatch" => {
                return Err(Error::new(
                    "type_mismatch",
                    format!("`{name}` is not a `{}`", T::NAME),
                ));
            }
            _ => return Err(Error::new(e.code, format!("`{name}`: {}", e.message))),
        }
        unsafe { T::from_value(x) }
            .map(Receiver::Built)
            .map_err(|e| Error::new(e.code, format!("`{name}`: {}", e.message)))
    }
}

/// Gives R8 the declarations that this package was compiled against of `T` and
/// the types that it reaches, as the record of its exports does of the types
/// that its declarations and its signatures name.
fn pin<T: RustStored>() -> Result<()> {
    use rtemis_r8::record::{self, DeclarationRef};
    let f = api::api()?
        .pin
        .ok_or_else(|| Error::new("no_api", "R8's API has no `pin`"))?;
    let docs = T::declarations().into_documents();
    let bytes = record::encode(docs.iter().flat_map(|d| {
        d.types
            .iter()
            .map(|t| (&d.namespace, DeclarationRef::Type(t)))
    }));
    let mut err = api::blank();
    if unsafe { f(module(), bytes.as_ptr(), bytes.len(), &mut err) } == OK {
        Ok(())
    } else {
        Err(Error::from_abi(&err))
    }
}

/// The fields of the value `x` that `names` gives, in the order of `names`.
/// Each is looked for at its own position first, which is where a value of a
/// type that is declared as it was compiled has it.
///
/// # Safety
///
/// On R's main thread, with a valid `x`.
#[doc(hidden)]
pub unsafe fn fields_by_name(
    x: SEXP,
    names: impl FnOnce(&mut dyn FnMut(&'static CStr)),
) -> Result<Vec<SEXP>> {
    let mut wanted: Vec<&'static CStr> = Vec::new();
    names(&mut |n| wanted.push(n));
    let nil = unsafe { sys::R_NilValue };
    let out: Vec<Cell<SEXP>> = vec![Cell::new(nil); wanted.len()];
    let missing = Cell::new(None);
    // A field read of a value of another package's codec may raise an R error.
    api::protect(&|| unsafe {
        let have = sys::Rf_getAttrib(x, sys::R_NamesSymbol);
        let n = if sys::TYPEOF(have) == sys::STRSXP {
            sys::Rf_xlength(have).min(sys::Rf_xlength(x))
        } else {
            0
        };
        let at = |j: isize, name: &CStr| {
            j < n && CStr::from_ptr(sys::R_CHAR(sys::STRING_ELT(have, j))) == name
        };
        for (i, name) in wanted.iter().enumerate() {
            let found = if at(i as isize, name) {
                Some(i as isize)
            } else {
                (0..n).find(|j| at(*j, name))
            };
            match found {
                Some(j) => out[i].set(sys::VECTOR_ELT(x, j)),
                None => {
                    missing.set(Some(i));
                    break;
                }
            }
        }
        nil
    })
    .map_err(Jumped::into_error)?;
    match missing.get() {
        None => Ok(out.into_iter().map(Cell::into_inner).collect()),
        Some(i) => Err(Error::new(
            "declaration_changed",
            format!(
                "the value has no field `{}`; it was made before its type was declared again",
                wanted[i].to_string_lossy()
            ),
        )),
    }
}

/// The value of an argument `Option<&T>` of a method, `None` for NULL.
///
/// # Safety
///
/// As for [`Receiver::from_named_arg`].
#[doc(hidden)]
pub unsafe fn optional_arg<'a, T: RustStored>(
    x: SEXP,
    name: &str,
) -> Result<Option<Receiver<'a, T>>> {
    if x == unsafe { sys::R_NilValue } {
        return Ok(None);
    }
    unsafe { Receiver::from_named_arg(x, name) }.map(Some)
}

/// The variant of the enum value `x`.
///
/// # Safety
///
/// On R's main thread, with a valid `x`.
#[doc(hidden)]
pub unsafe fn r_variant(x: SEXP) -> Result<String> {
    let variant = unsafe { r_type(x) }?.1;
    variant.ok_or_else(|| Error::new("no_variant", "the value has no variant"))
}

/// The type of the value `x`, its qualified name, and the variant of an enum's
/// value.
///
/// # Safety
///
/// On R's main thread, with a valid `x`.
pub(crate) unsafe fn r_type(x: SEXP) -> Result<(String, Option<String>)> {
    let f = api::api()?
        .value_type
        .ok_or_else(|| Error::new("no_api", "R8's API has no `value_type`"))?;
    let (mut name, mut variant) = (std::ptr::null(), std::ptr::null());
    unsafe { f(x, &mut name, &mut variant) };
    let text = |p: *const std::ffi::c_char| {
        (!p.is_null()).then(|| unsafe { CStr::from_ptr(p) }.to_string_lossy().into_owned())
    };
    match text(name) {
        Some(name) => Ok((name, text(variant))),
        None => Err(Error::new("not_a_value", "the object is not an R8 value")),
    }
}

/// A result of an impl function, converted to its R value: a Rust-stored
/// value is wrapped by R8, or, for a type of another package, built from its
/// fields.
#[diagnostic::on_unimplemented(
    message = "`{Self}` is not the result of an R8 method",
    note = "a result is a `bool`, an `i32`, an `f64`, a string, a `Vec` or a `Buffer` of numbers, a Rust-stored value, a `SubtypeValue`, an `ImplementorValue`, a `Sexp`, `()`, or an `Option` or a `Result` of one"
)]
pub trait IntoR {
    /// The R8 type of the result; `None` for a value of R of any type.
    fn return_type() -> Option<FieldType> {
        None
    }

    /// Adds the declarations of the types that the result's names.
    fn register(_set: &mut DocumentSet) {}

    /// The R value; a jump out of an allocation is kept for R8 to continue.
    fn into_r(self) -> std::result::Result<SEXP, Jumped>;
}

fn alloc<F: Fn() -> SEXP>(f: &F) -> std::result::Result<SEXP, Jumped> {
    api::protect(f)
}

impl IntoR for f64 {
    fn return_type() -> Option<FieldType> {
        Some(<f64 as Encode>::field_type())
    }
    fn into_r(self) -> std::result::Result<SEXP, Jumped> {
        alloc(&|| unsafe { sys::Rf_ScalarReal(self) })
    }
}

impl IntoR for i32 {
    fn return_type() -> Option<FieldType> {
        Some(<i32 as Encode>::field_type())
    }
    fn into_r(self) -> std::result::Result<SEXP, Jumped> {
        alloc(&|| unsafe { sys::Rf_ScalarInteger(self) })
    }
}

/// The result of `R8::PartialOrd`'s `partial_cmp()`: `-1L`, `0L`, or `1L`,
/// and `NA` when the values are not ordered, as the R method returns.
impl IntoR for Option<std::cmp::Ordering> {
    fn into_r(self) -> std::result::Result<SEXP, Jumped> {
        let o = match self {
            Some(o) => o as i32,
            None => unsafe { sys::R_NaInt },
        };
        alloc(&|| unsafe { sys::Rf_ScalarInteger(o) })
    }
}

impl IntoR for bool {
    fn return_type() -> Option<FieldType> {
        Some(<bool as Encode>::field_type())
    }
    fn into_r(self) -> std::result::Result<SEXP, Jumped> {
        alloc(&|| unsafe { sys::Rf_ScalarLogical(i32::from(self)) })
    }
}

impl IntoR for String {
    fn return_type() -> Option<FieldType> {
        Some(<String as Encode>::field_type())
    }
    fn into_r(self) -> std::result::Result<SEXP, Jumped> {
        self.as_str().into_r()
    }
}

impl IntoR for &str {
    fn return_type() -> Option<FieldType> {
        Some(<String as Encode>::field_type())
    }
    fn into_r(self) -> std::result::Result<SEXP, Jumped> {
        let n = i32::try_from(self.len()).map_err(|_| {
            Jumped::Error(Error::new("too_long", "the string is longer than R allows"))
        })?;
        alloc(&|| unsafe {
            let v = sys::Rf_protect(sys::Rf_allocVector(sys::STRSXP, 1));
            sys::SET_STRING_ELT(
                v,
                0,
                sys::Rf_mkCharLenCE(self.as_ptr().cast(), n, sys::CE_UTF8),
            );
            sys::Rf_unprotect(1);
            v
        })
    }
}

impl IntoR for () {
    fn into_r(self) -> std::result::Result<SEXP, Jumped> {
        Ok(unsafe { sys::R_NilValue })
    }
}

impl<T: IntoR> IntoR for Option<T> {
    fn return_type() -> Option<FieldType> {
        T::return_type().map(|t| FieldType::Option { inner: Box::new(t) })
    }
    fn register(set: &mut DocumentSet) {
        T::register(set);
    }
    fn into_r(self) -> std::result::Result<SEXP, Jumped> {
        match self {
            Some(v) => v.into_r(),
            None => Ok(unsafe { sys::R_NilValue }),
        }
    }
}

impl<T: IntoR, E: Into<Error>> IntoR for std::result::Result<T, E> {
    fn return_type() -> Option<FieldType> {
        T::return_type()
    }
    fn register(set: &mut DocumentSet) {
        T::register(set);
    }
    fn into_r(self) -> std::result::Result<SEXP, Jumped> {
        match self {
            Ok(v) => v.into_r(),
            Err(e) => Err(Jumped::Error(e.into())),
        }
    }
}

unsafe extern "C" fn drop_boxed<T>(x: SEXP) {
    let p = unsafe { sys::R_ExternalPtrAddr(x) };
    if !p.is_null() {
        let _ = catch_unwind(AssertUnwindSafe(|| {
            drop(unsafe { Box::from_raw(p.cast::<T>()) })
        }));
        unsafe { sys::R_ClearExternalPtr(x) };
    }
}

/// An external pointer, unprotected, that owns `boxed` until R collects it,
/// with the box's address, for views of its memory.
fn owner<T>(boxed: Box<T>) -> std::result::Result<(SEXP, *const T), Jumped> {
    let boxed = Box::into_raw(boxed);
    let owner = alloc(&|| unsafe {
        let o = sys::Rf_protect(sys::R_MakeExternalPtr(
            boxed.cast(),
            sys::R_NilValue,
            sys::R_NilValue,
        ));
        sys::R_RegisterCFinalizerEx(o, drop_boxed::<T>, 1);
        sys::Rf_unprotect(1);
        o
    });
    match owner {
        Ok(o) => Ok((o, boxed)),
        Err(j) => {
            // The finalizer was not registered, so the box is still Rust's.
            drop(unsafe { Box::from_raw(boxed) });
            Err(j)
        }
    }
}

impl<T: ElementType> IntoR for Vec<T> {
    fn return_type() -> Option<FieldType> {
        Some(T::buffer_type())
    }
    /// The vector as an owned [`Buffer`], which R reads as a view: the
    /// result of `R8::Shape`'s `dim()`, among others.
    fn into_r(self) -> std::result::Result<SEXP, Jumped> {
        Buffer::from(self).into_r()
    }
}

impl<T: ElementType> IntoR for Buffer<T> {
    fn return_type() -> Option<FieldType> {
        Some(T::buffer_type())
    }
    /// A borrowed buffer returns its R vector; an owned one, a view whose
    /// owner, an external pointer, holds the buffer until R collects it.
    fn into_r(self) -> std::result::Result<SEXP, Jumped> {
        let (owner, b) = owner(Box::new(self))?;
        unsafe { &*b }.to_r(owner).map_err(Jumped::Error)
    }
}

impl<T: RustStored> IntoR for T {
    fn return_type() -> Option<FieldType> {
        Some(T::field_type())
    }
    fn register(set: &mut DocumentSet) {
        <T as Encode>::register(set);
    }
    fn into_r(self) -> std::result::Result<SEXP, Jumped> {
        rust_value(self)
    }
}

/// The R value of `value`, unprotected: its state, which R8 wraps, or, for a
/// type whose codec another package registers, a value built from its fields.
pub(crate) fn rust_value<T: RustStored>(value: T) -> std::result::Result<SEXP, Jumped> {
    let Some(f) = api::api()?.new_value else {
        return Err(Jumped::Error(Error::new(
            "no_api",
            "R8's API has no `new_value`",
        )));
    };
    let mut out: SEXP = unsafe { sys::R_NilValue };
    let mut err = api::blank();
    let state = Box::into_raw(Box::new(value));
    let status = unsafe {
        f(
            T::TYPE_NAME.as_ptr(),
            module(),
            state.cast(),
            &mut out,
            &mut err,
        )
    };
    if status == OK {
        return Ok(out);
    }
    // R8 owns the state only once it has wrapped it.
    let state = unsafe { Box::from_raw(state) };
    let e = Error::from_abi(&err);
    if e.code != "other_module" {
        return Err(Jumped::Error(e));
    }
    value_of_fields(state).map_err(Jumped::Error)
}

/// A value of a type of another package from the fields of `value`.
fn value_of_fields<T: RustStored>(value: Box<T>) -> Result<SEXP> {
    let f = api::api()?
        .value_of_fields
        .ok_or_else(|| Error::new("no_api", "R8's API has no `value_of_fields`"))?;
    let variant = value.variant_name();
    let hash = declaration_hash::<T>()?;
    let fields = field_list(value)?;
    let mut out: SEXP = unsafe { sys::R_NilValue };
    let mut err = api::blank();
    let status = unsafe {
        f(
            T::TYPE_NAME.as_ptr(),
            variant.map_or(std::ptr::null(), CStr::as_ptr),
            hash.as_ptr(),
            fields.list.get(),
            &mut out,
            &mut err,
        )
    };
    if status == OK {
        Ok(out)
    } else {
        Err(Error::from_abi(&err))
    }
}

/// The declaration hash of `T`, as this package was compiled against it,
/// computed once.
fn declaration_hash<T: RustStored>() -> Result<&'static CStr> {
    static HASHES: Mutex<Option<HashMap<TypeId, &'static CStr>>> = Mutex::new(None);
    let mut held = HASHES.lock().unwrap_or_else(|e| e.into_inner());
    let hashes = held.get_or_insert_default();
    if let Some(h) = hashes.get(&TypeId::of::<T>()) {
        return Ok(h);
    }
    let text = rtemis_r8::canonical::declaration_hash(&T::qualified_name(), &T::declarations());
    let text = CString::new(text).map_err(|e| Error::new("bad_name", e.to_string()))?;
    let hash: &'static CStr = Box::leak(text.into_boxed_c_str());
    hashes.insert(TypeId::of::<T>(), hash);
    Ok(hash)
}

/// The fields of a Rust-stored value as an R list, kept.
pub(crate) struct FieldList {
    list: Token,
    names: Vec<&'static CStr>,
}

impl FieldList {
    /// The list with its names.
    pub(crate) fn named(&self) -> Result<SEXP> {
        let list = self.list.get();
        let names = &self.names;
        api::protect(&|| unsafe {
            let v = sys::Rf_protect(sys::Rf_allocVector(sys::STRSXP, names.len() as isize));
            for (i, name) in names.iter().enumerate() {
                let n = name.to_bytes().len() as i32;
                sys::SET_STRING_ELT(
                    v,
                    i as isize,
                    sys::Rf_mkCharLenCE(name.as_ptr(), n, sys::CE_UTF8),
                );
            }
            sys::Rf_setAttrib(list, sys::R_NamesSymbol, v);
            sys::Rf_unprotect(1);
            list
        })
        .map_err(Jumped::into_error)
    }
}

/// The fields of `value`, whose memory the views among them keep alive.
pub(crate) fn field_list<T: RustStored>(value: Box<T>) -> Result<FieldList> {
    unsafe { keep::prepare() }?;
    let n = value.field_count();
    let (owner, value) = owner(value).map_err(Jumped::into_error)?;
    // The owner holds the value's memory until a view does.
    let owner = unsafe { Token::keep(owner) }?;
    let list = api::protect(&|| unsafe { sys::Rf_allocVector(sys::VECSXP, n as isize) })
        .map_err(Jumped::into_error)?;
    let list = unsafe { Token::keep(list) }?;
    let mut names = Vec::with_capacity(n);
    unsafe { &*value }.each_field(owner.get(), &mut |name, x| {
        if names.len() == n {
            return Err(Error::new(
                "arity",
                "the value gives more fields than it has",
            ));
        }
        unsafe { sys::SET_VECTOR_ELT(list.get(), names.len() as isize, x) };
        names.push(name);
        Ok(())
    })?;
    Ok(FieldList { list, names })
}

/// An R object passed through unchanged; the impl function keeps it
/// protected or returns it at once.
#[derive(Debug, Clone, Copy)]
pub struct Sexp(pub SEXP);

impl IntoR for Sexp {
    fn into_r(self) -> std::result::Result<SEXP, Jumped> {
        Ok(self.0)
    }
}

impl FromR for Sexp {
    fn param_type() -> ParamType {
        ParamType::Untyped
    }
    unsafe fn from_arg(x: SEXP, _name: &str) -> Result<Self> {
        Ok(Sexp(x))
    }
}

/// Runs an impl function for R8: checks the number of arguments, runs
/// `body`, which converts the arguments, calls the method under
/// `catch_unwind`, and converts the result, and reports `R8_OK`,
/// `R8_ERROR`, or the `R8_UNWIND` of a jump R8 continues.
///
/// # Safety
///
/// Called from an impl function with R8's arguments.
pub unsafe fn method(
    args: *const abi::Sexp,
    nargs: usize,
    n_params: usize,
    result: *mut abi::Sexp,
    err: *mut abi::Error,
    body: impl FnOnce(&[SEXP]) -> std::result::Result<SEXP, Jumped>,
) -> Status {
    keep::drain();
    if nargs != n_params {
        Error::new(
            "arity",
            format!("the method takes {n_params} arguments, and received {nargs}"),
        )
        .fill(err);
        return ERROR;
    }
    let a: &[SEXP] = if nargs == 0 {
        &[]
    } else {
        unsafe { std::slice::from_raw_parts(args, nargs) }
    };
    match catch_unwind(AssertUnwindSafe(|| body(a))) {
        Ok(Ok(v)) => {
            unsafe { *result = v };
            OK
        }
        Ok(Err(Jumped::Error(e))) => {
            e.fill(err);
            ERROR
        }
        Ok(Err(Jumped::Unwind(e))) => {
            e.fill(err);
            UNWIND
        }
        Err(p) => {
            Error::new("panic", panic_text(&*p)).fill(err);
            ERROR
        }
    }
}

impl From<Error> for Jumped {
    fn from(e: Error) -> Self {
        Jumped::Error(e)
    }
}
