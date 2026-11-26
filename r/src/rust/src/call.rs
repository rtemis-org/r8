//! Documents of a call of a typed function, and the JSON Schema of its
//! arguments, over `rtemis_r8::call`.
//!
//! spec: R8/class-system#typed-functions

use extendr_api::prelude::*;
use extendr_api::{Error, Result};
use rtemis_r8::name::QualifiedName;
use rtemis_r8::tree::TreeRef;

use crate::values::{decode_field, errors_list};

fn fail(message: impl Into<String>) -> Error {
    Error::Other(message.into())
}

fn qualified(name: &str) -> Result<QualifiedName> {
    QualifiedName::parse(name).map_err(|e| fail(e.to_string()))
}

/// The JSON Schema (draft 2020-12) of the arguments object of a registered
/// function (`rtemis_r8::call::arguments_schema()`).
///
/// @param name Character: The function's qualified name.
/// @param closed Logical: Write `dyn()` and `subtype()` types as the closed
///   lists of the registered types.
/// @return List: `schema`, the schema as indented JSON text, or
///   `undocumented`, the names of the parameters that a document cannot hold.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_function_schema(name: crate::utf8::Utf8, closed: bool) -> Result<List> {
    let name: &str = &name;
    let q = qualified(name)?;
    let docs = crate::declare::snapshot()?;
    if rtemis_r8::rules::TypeLookup::function_of(&*docs, &q).is_none() {
        return Err(fail(format!("`{name}` is not a registered function")));
    }
    Ok(
        match rtemis_r8::call::arguments_schema(&q, &*docs, closed) {
            Ok(s) => {
                list!(schema = serde_json::to_string_pretty(&s).map_err(|e| fail(e.to_string()))?)
            }
            Err(names) => list!(undocumented = names),
        },
    )
}

/// The parameters of a registered function that a document cannot hold
/// (`rtemis_r8::call::undocumented_params()`).
///
/// @param name Character: The function's qualified name.
/// @return Character: The names, or `NULL` for a function that is not
///   registered.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_function_undocumented(name: crate::utf8::Utf8) -> Result<Robj> {
    let name: &str = &name;
    let q = qualified(name)?;
    let docs = crate::declare::snapshot()?;
    Ok(
        match rtemis_r8::rules::TypeLookup::function_of(&*docs, &q) {
            Some(def) => Robj::from(rtemis_r8::call::undocumented_params(def)),
            None => ().into(),
        },
    )
}

/// R's reserved words, which a call cannot name as a function.
const RESERVED: &[&str] = &[
    "if",
    "else",
    "repeat",
    "while",
    "function",
    "for",
    "next",
    "break",
    "TRUE",
    "FALSE",
    "NULL",
    "Inf",
    "NaN",
    "NA",
    "NA_integer_",
    "NA_real_",
    "NA_character_",
    "NA_complex_",
    "in",
];

/// The call `head(a = a, b = b)` of the symbols `names`, which a frame that
/// binds them evaluates, so that the function's `sys.call()` shows the names.
fn call_of(head: &str, names: &[&str]) -> Result<Robj> {
    use extendr_ffi::{
        R_NilValue, Rf_cons, Rf_install, Rf_lcons, Rf_protect, Rf_unprotect, SET_TAG,
    };
    let symbol = |n: &str| -> Result<extendr_ffi::SEXP> {
        let c = std::ffi::CString::new(n).map_err(|e| fail(e.to_string()))?;
        Ok(unsafe { Rf_install(c.as_ptr()) })
    };
    let head = symbol(head)?;
    let symbols: Vec<extendr_ffi::SEXP> = names.iter().map(|n| symbol(n)).collect::<Result<_>>()?;
    unsafe {
        let mut args = R_NilValue;
        for s in symbols.iter().rev() {
            args = Rf_protect(Rf_cons(*s, args));
            SET_TAG(args, *s);
        }
        let call = Rf_protect(Rf_lcons(head, args));
        let out = Robj::from_sexp(call);
        Rf_unprotect(symbols.len() as std::os::raw::c_int + 1);
        Ok(out)
    }
}

/// Reads a document of a call of one of the functions `admitted`
/// (`rtemis_r8::call::read_call()`).
///
/// @param text Character: The document.
/// @param admitted Character: The qualified names of the functions the call
///   may name.
/// @return List: `errors`, the problems, for a call that does not hold; or
///   `function`, the qualified name, `arguments`, the arguments given in their
///   stored R form, by name, `unchecked`, the rules that the check did not
///   decide, `name`, the symbol under which a frame binds the function, and
///   `call`, the call of that symbol with the arguments' symbols.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_read_call(text: crate::utf8::Utf8, admitted: crate::utf8::Utf8s) -> Result<List> {
    let text: &str = &text;
    let admitted: Vec<QualifiedName> = admitted
        .to_vec()
        .iter()
        .filter_map(|n| QualifiedName::parse(n).ok())
        .collect();
    crate::interrupt::interruptible_if(text.len() > 1 << 20, || {
        let docs = crate::declare::snapshot()?;
        let read = crate::construct::with_cache(|cache| {
            rtemis_r8::call::read_call(text, &admitted, &*docs, cache)
        });
        let call = match read {
            Ok(c) => c,
            Err(errors) => return Ok(list!(errors = errors_list(&errors))),
        };
        let def = rtemis_r8::rules::TypeLookup::function_of(&*docs, &call.function)
            .ok_or_else(|| fail("the function is not registered"))?;
        let mut names = Vec::with_capacity(call.arguments.len());
        let mut values = Vec::with_capacity(call.arguments.len());
        for (k, v) in &call.arguments {
            let ty = def
                .params
                .iter()
                .find(|p| p.name() == k)
                .and_then(|p| p.ty())
                .ok_or_else(|| fail(format!("`{k}` is not a typed parameter")))?;
            values.push(decode_field(TreeRef::new(v), ty, &*docs)?);
            names.push(k.as_str());
        }
        // The function's name, unless a parameter or R's syntax takes it.
        let bare = call.function.name.as_str();
        let head = if names.contains(&bare) || RESERVED.contains(&bare) {
            ".r8_f"
        } else {
            bare
        };
        let lang = call_of(head, &names)?;
        Ok(list!(
            function = call.function.to_string(),
            arguments = List::from_names_and_values(&names, values)?,
            unchecked = errors_list(&call.unchecked),
            name = head,
            call = lang
        ))
    })
}

/// The document of a call of a registered function with the arguments
/// `values`, checked as `r8_read_call()` checks a document.
///
/// @param name Character: The function's qualified name.
/// @param values List: The arguments, by parameter name.
/// @return List: `json`, the document's text, or `errors`, the problems.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_write_call(name: crate::utf8::Utf8, values: List) -> Result<List> {
    let name: &str = &name;
    let q = qualified(name)?;
    let docs = crate::declare::snapshot()?;
    let def = rtemis_r8::rules::TypeLookup::function_of(&*docs, &q)
        .ok_or_else(|| fail(format!("`{name}` is not a registered function")))?;
    let mut args = Vec::with_capacity(values.len());
    let mut errors = Vec::new();
    for (k, x) in values.iter() {
        match def
            .params
            .iter()
            .find(|p| p.name() == k)
            .and_then(|p| p.ty())
        {
            Some(ty) => {
                let mut enc = crate::values::Encoder {
                    lookup: &*docs,
                    errors: Vec::new(),
                    host_only: Vec::new(),
                };
                let v = enc.value(&x, ty, &format!("/arguments/{k}"));
                errors.extend(enc.errors);
                args.push((k.to_string(), v));
            }
            None => args.push((k.to_string(), serde_json::Value::Null)),
        }
    }
    if !errors.is_empty() {
        return Ok(list!(errors = errors_list(&errors)));
    }
    let text = rtemis_r8::call::write_call(&q, &args, &*docs);
    let read = crate::construct::with_cache(|cache| {
        rtemis_r8::call::read_call(&text, std::slice::from_ref(&q), &*docs, cache)
    });
    Ok(match read {
        Ok(_) => list!(json = text),
        Err(errors) => list!(errors = errors_list(&errors)),
    })
}

extendr_module! {
    mod call;
    fn r8_function_schema;
    fn r8_function_undocumented;
    fn r8_read_call;
    fn r8_write_call;
}
