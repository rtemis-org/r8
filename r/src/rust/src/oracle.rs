//! The differential oracle: construction and identity through the JSON form,
//! which `tests/testthat/test-native.R` compares with native construction
//! and identity. Built with the Cargo feature `oracle` only, which
//! `tools/config.R` enables when the environment variable `R8_ORACLE` is
//! set. `R/extendr-wrappers.R` holds no wrapper for these functions: the
//! `document` binary builds without the feature, and the tests call the
//! registered routines by name.

use extendr_api::Result;
use extendr_api::prelude::*;
use rtemis_r8::codec::read_document;
use rtemis_r8::ir::FieldType;
use rtemis_r8::name::QualifiedName;
use rtemis_r8::rules::DocumentSet;
use rtemis_r8::validate::{ValueError, validate};
use serde_json::Value;

use crate::declare::registry;
use crate::values::{Decoder, Encoder, errors_list, fail};

/// Encodes an R value of a named type to its JSON form and validates it.
fn encode_and_validate(
    x: &Robj,
    name: &QualifiedName,
    docs: &DocumentSet,
) -> std::result::Result<Value, Vec<ValueError>> {
    encode_with_runtime(x, name, docs).map(|(v, _)| v)
}

/// Encodes and validates, also returning the paths of runtime-only fields
/// that hold a value.
fn encode_with_runtime(
    x: &Robj,
    name: &QualifiedName,
    docs: &DocumentSet,
) -> std::result::Result<(Value, Vec<String>), Vec<ValueError>> {
    let mut enc = Encoder {
        lookup: docs,
        errors: Vec::new(),
        runtime: Vec::new(),
    };
    let ft = FieldType::Named { name: name.clone() };
    let v = enc.value(x, &ft, "");
    if !enc.errors.is_empty() {
        return Err(enc.errors);
    }
    let errors = validate(&v, name, docs);
    if errors.is_empty() {
        Ok((v, enc.runtime))
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
    let mut input = fields.duplicate();
    input.set_class([type_name, "r8_value"])?;
    input.set_attrib("r8_type", type_name)?;
    if !variant.is_empty() {
        input.set_attrib("r8_variant", variant)?;
    }
    match encode_and_validate(&input, &q, docs)
        .and_then(|v| rtemis_r8::codec::normalize(&v, &q, docs))
    {
        Ok(v) => {
            let value = Decoder { lookup: docs }.named(&v, &q, Some(&input))?;
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
    let docs = registry()
        .lock()
        .map_err(|_| fail("the registry is unavailable"))?;
    let docs: &DocumentSet = &docs;
    let result = read_document(text, &q, docs).and_then(|v| {
        let canonical = rtemis_r8::codec::canonical(&v, &q, docs)?;
        let hash = rtemis_r8::codec::content_hash(&v, &q, docs)?;
        Ok((canonical, hash))
    });
    match result {
        Ok((canonical, hash)) => Ok(list!(canonical = canonical, hash = hash)),
        Err(errors) => Ok(list!(errors = errors_list(&errors))),
    }
}

extendr_module! {
    mod oracle;
    fn r8_new_value_json;
    fn r8_document_identity;
}
