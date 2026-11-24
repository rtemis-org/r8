//! The differential oracle: construction and identity through the JSON form,
//! which `tests/testthat/test-construct.R` compares with in-place
//! construction and identity. Built with the Cargo feature `oracle` only, which
//! `tools/config.R` enables when the environment variable `R8_ORACLE` is
//! set. `R/extendr-wrappers.R` holds no wrapper for these functions: the
//! `document` binary builds without the feature, and the tests call the
//! registered routines by name.

use extendr_api::Result;
use extendr_api::prelude::*;
use rtemis_r8::canonical::read_document;
use rtemis_r8::name::QualifiedName;
use rtemis_r8::rules::DocumentSet;
use rtemis_r8::validate::{ValueError, validate};
use serde_json::Value;

use crate::declare::registry;
use crate::values::{Decoder, Encoder, errors_list, fail};

/// Encodes the fields `x` of a value of a named type to their JSON form and
/// validates it.
fn encode_and_validate(
    x: &Robj,
    name: &QualifiedName,
    variant: &str,
    docs: &DocumentSet,
) -> std::result::Result<Value, Vec<ValueError>> {
    encode_with_runtime(x, name, variant, docs).map(|(v, _)| v)
}

/// Encodes and validates, also returning the paths of host-only fields
/// that hold a value.
fn encode_with_runtime(
    x: &Robj,
    name: &QualifiedName,
    variant: &str,
    docs: &DocumentSet,
) -> std::result::Result<(Value, Vec<String>), Vec<ValueError>> {
    let mut enc = Encoder {
        lookup: docs,
        errors: Vec::new(),
        host_only: Vec::new(),
    };
    let v = enc.members(x, name, variant, "");
    if !enc.errors.is_empty() {
        return Err(enc.errors);
    }
    let errors = validate(&v, name, docs);
    if errors.is_empty() {
        Ok((v, enc.host_only))
    } else {
        Err(errors)
    }
}

/// The reference construction, through the JSON form: the fields are
/// encoded to JSON, validated, normalized (an absent field takes its
/// default), and decoded. The differential test compares `r8_new_value()`
/// with it.
///
/// Returns `list(value = <R8 value>)` or `list(errors = <problems>)`.
///
/// @param fields List: The fields, by name.
/// @param type_name Character: The qualified type name.
/// @param variant Character: The enum variant, or `""` for a struct.
/// @return List: The value or the problems.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_new_value_json(fields: Robj, type_name: &str, variant: &str) -> Result<List> {
    let q = QualifiedName::parse(type_name).map_err(|e| fail(e.to_string()))?;
    let docs = crate::declare::snapshot()?;
    let docs: &DocumentSet = &docs;
    match encode_and_validate(&fields, &q, variant, docs)
        .and_then(|v| rtemis_r8::canonical::normalize(&v, &q, docs))
    {
        Ok(v) => {
            let value = Decoder { lookup: docs }.named(&v, &q, Some(&fields))?;
            Ok(list!(value = value))
        }
        Err(errors) => Ok(list!(errors = errors_list(&errors))),
    }
}

/// The canonical text and content hash of a document, computed on its JSON
/// form; the differential test compares them with `r8_value_identity()`.
///
/// @param text Character: The document.
/// @param type_name Character: The qualified type name.
/// @return List: `canonical` and `hash`, or `errors`.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_document_identity(text: &str, type_name: &str) -> Result<List> {
    let q = QualifiedName::parse(type_name).map_err(|e| fail(e.to_string()))?;
    let docs = registry().snapshot();
    let docs: &DocumentSet = &docs;
    let result = read_document(text, &q, docs).and_then(|v| {
        let canonical = rtemis_r8::canonical::canonical(&v, &q, docs)?;
        let hash = rtemis_r8::canonical::content_hash(&v, &q, docs)?;
        Ok((canonical, hash))
    });
    match result {
        Ok((canonical, hash)) => Ok(list!(canonical = canonical, hash = hash)),
        Err(errors) => Ok(list!(errors = errors_list(&errors))),
    }
}

/// The declaration hash of a registered type, for the tests of the binary
/// interface, whose `value_of_fields` takes it.
///
/// @param type_name Character: The qualified type name.
/// @return Character: The hash.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_declaration_hash(type_name: &str) -> Result<String> {
    let q = QualifiedName::parse(type_name).map_err(|e| fail(e.to_string()))?;
    let docs = registry().snapshot();
    Ok(rtemis_r8::canonical::declaration_hash(&q, &*docs))
}

/// An R-stored value of any type over any fields, unchecked, as one that another
/// declaration of the type made, or one of a type whose values Rust holds.
///
/// @param fields List: The fields, by name.
/// @param type_name Character: The qualified type name.
/// @param variant Character: The enum variant, or `""` for a struct.
/// @param declaration Character: The text that names the declaration, or `""`
///   for the registered one.
/// @return The value.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_any_value(fields: List, type_name: &str, variant: &str, declaration: &str) -> Result<Robj> {
    let q = QualifiedName::parse(type_name).map_err(|e| fail(e.to_string()))?;
    let variant = (!variant.is_empty()).then_some(variant);
    let text = if declaration.is_empty() {
        crate::value::declaration(&q, &*registry().snapshot())
    } else {
        declaration.to_string()
    };
    let names: Vec<&str> = fields.names().map(Iterator::collect).unwrap_or_default();
    let class = [type_name, "r8_value"];
    let id = crate::value::other_identity(&q, variant, &names, &class, &text);
    let plain = List::from_values(fields.values());
    Ok(unsafe { Robj::from_sexp(crate::value::new(plain.get(), id)) })
}

extendr_module! {
    mod oracle;
    fn r8_any_value;
    fn r8_new_value_json;
    fn r8_document_identity;
    fn r8_declaration_hash;
}
