//! The declaration of a struct or an enum, read from the objects the user
//! passes to `new_struct()` and `new_enum()`, and declared in one crossing.
//!
//! A field is read from its name and its object: an `r8_field` from
//! `field()`, an `r8_field_type`, or an R8 type object. A default becomes
//! its JSON form here, through `rtemis_r8::ir::build`. A problem of reading
//! has a code, a JSON Pointer path, and a message, as the declaration check's
//! problems do.
//! spec: R8/class-system#declaring-types

use std::collections::BTreeMap;

use extendr_api::prelude::*;
use extendr_ffi::{SEXP, SEXPTYPE, VECTOR_ELT};
use rtemis_r8::ir::build::{self, BuildError};
use rtemis_r8::ir::{
    EnumDef, FieldDef, FieldType, RuleDef, StructDef, TypeDef, VariantDef, VariantShape,
};
use rtemis_r8::name::{Name, NameError, QualifiedName};
use rtemis_r8::rules::TypeLookup;
use serde_json::Value;

use crate::fieldtype::field_type;
use crate::rview::{
    attr, attr_string, inherits, is_null, sexptype, string_elt, sym_names, sym_r8_name,
    sym_r8_type, sym_r8_variant, xlen,
};

/// A problem of reading a declaration.
pub(crate) struct Problem {
    code: &'static str,
    path: String,
    message: String,
}

type Read<T> = std::result::Result<T, Problem>;

fn problem(code: &'static str, path: &str, message: impl Into<String>) -> Problem {
    Problem {
        code,
        path: path.to_string(),
        message: message.into(),
    }
}

fn built(path: &str, e: BuildError) -> Problem {
    let code = match e {
        BuildError::NotFieldType => "field_type",
        BuildError::DefaultNotValue | BuildError::DefaultStruct => "field_default",
        BuildError::NotVariant { .. } => "variant_declaration",
        _ => "declaration_argument",
    };
    problem(code, path, e.to_string())
}

fn name(s: &str, path: &str) -> Read<Name> {
    Name::new(s).map_err(|e| {
        let code = match e {
            NameError::Reserved(_) => "name_reserved",
            _ => "name_pattern",
        };
        problem(code, path, e.to_string())
    })
}

fn read<T: serde::de::DeserializeOwned>(x: SEXP, path: &str) -> Read<T> {
    crate::irde::from_r(x).map_err(|e| problem("declaration_read", path, e.to_string()))
}

/// The member `key` of a named list, or `NULL`.
fn member(x: SEXP, key: &str) -> SEXP {
    let names = attr(x, sym_names());
    for i in 0..xlen(x) {
        if string_elt(names, i).as_deref() == Some(key) {
            return unsafe { VECTOR_ELT(x, i as isize) };
        }
    }
    crate::rview::nil()
}

/// The names and elements of a named list.
fn entries(x: SEXP) -> Vec<(String, SEXP)> {
    let names = attr(x, sym_names());
    (0..xlen(x))
        .map(|i| {
            (
                string_elt(names, i)
                    .map(|s| s.into_owned())
                    .unwrap_or_default(),
                unsafe { VECTOR_ELT(x, i as isize) },
            )
        })
        .collect()
}

fn is_true(x: SEXP) -> bool {
    sexptype(x) == SEXPTYPE::LGLSXP && xlen(x) == 1 && unsafe { *extendr_ffi::LOGICAL(x) } == 1
}

fn one_string(x: SEXP) -> Option<String> {
    (sexptype(x) == SEXPTYPE::STRSXP && xlen(x) == 1)
        .then(|| string_elt(x, 0).map(|s| s.into_owned()))
        .flatten()
}

/// The JSON form of a default `x` of a field of type `ty`.
fn default_value(x: SEXP, ty: &FieldType, lookup: &impl TypeLookup, path: &str) -> Read<Value> {
    if inherits(x, "r8_json_ir") {
        return read(x, path);
    }
    match ty {
        FieldType::Option { inner, .. } => default_value(x, inner, lookup, path),
        FieldType::Vec { element, .. } if sexptype(x) == SEXPTYPE::VECSXP => (0..xlen(x))
            .map(|i| {
                let e = unsafe { VECTOR_ELT(x, i as isize) };
                if is_null(e) {
                    Ok(Value::Null)
                } else {
                    default_value(e, element, lookup, &format!("{path}/{i}"))
                }
            })
            .collect::<Read<Vec<_>>>()
            .map(Value::Array),
        FieldType::Vec { .. } => read::<Vec<Value>>(x, path).map(Value::Array),
        FieldType::Named { .. } => {
            if !inherits(x, "r8_value") {
                return Err(built(path, BuildError::DefaultNotValue));
            }
            let variant = attr_string(x, sym_r8_variant())
                .ok_or_else(|| built(path, BuildError::DefaultStruct))?;
            let def = attr_string(x, sym_r8_type())
                .and_then(|q| QualifiedName::parse(&q).ok())
                .and_then(|q| match lookup.find(&q) {
                    Some(TypeDef::Enum(e)) => Some(e),
                    _ => None,
                })
                .ok_or_else(|| built(path, BuildError::DefaultNotValue))?;
            Ok(build::enum_default(def, &variant))
        }
        _ => read(x, path),
    }
}

/// A field from its name and the user's object.
fn field_def(n: &str, x: SEXP, lookup: &impl TypeLookup, path: &str) -> Read<FieldDef> {
    let n = name(n, &format!("{path}/name"))?;
    let type_path = format!("{path}/type");
    if !inherits(x, "r8_field") {
        return Ok(FieldDef {
            name: n,
            ty: field_type(x).map_err(|e| built(&type_path, e))?,
            default: None,
            description: "".into(),
            sensitive: false,
            unit: None,
            external: false,
            unique: false,
            optional: false,
            inherited_from: None,
            extensions: None,
        });
    }
    let ty = field_type(member(x, "type")).map_err(|e| built(&type_path, e))?;
    let d = member(x, "default");
    let default = if is_true(member(x, "has_default")) && !is_null(d) {
        Some(Box::new(default_value(
            d,
            &ty,
            lookup,
            &format!("{path}/default"),
        )?))
    } else {
        None
    };
    let ext = member(x, "extensions");
    let extensions = if is_null(ext) {
        None
    } else {
        Some(Box::new(read::<BTreeMap<String, Value>>(
            ext,
            &format!("{path}/extensions"),
        )?))
    };
    Ok(FieldDef {
        name: n,
        ty,
        default,
        description: one_string(member(x, "description"))
            .unwrap_or_default()
            .into_boxed_str(),
        sensitive: is_true(member(x, "sensitive")),
        unit: one_string(member(x, "unit")).map(String::into_boxed_str),
        external: is_true(member(x, "external")),
        unique: is_true(member(x, "unique")),
        optional: is_true(member(x, "optional")),
        inherited_from: None,
        extensions: extensions.filter(|m| !m.is_empty()),
    })
}

/// The fields of a named list of field objects.
pub(crate) fn fields(x: SEXP, lookup: &impl TypeLookup, path: &str) -> Read<Vec<FieldDef>> {
    entries(x)
        .into_iter()
        .enumerate()
        .map(|(i, (n, f))| field_def(&n, f, lookup, &format!("{path}/{i}")))
        .collect()
}

/// The rules of a list of `rule()` objects.
pub(crate) fn rules(x: SEXP, path: &str) -> Read<Vec<RuleDef>> {
    if is_null(x) || xlen(x) == 0 {
        return Ok(Vec::new());
    }
    read(x, path)
}

/// The variants of a named list of `variant()` objects.
fn variants(x: SEXP, lookup: &impl TypeLookup) -> Read<Vec<VariantDef>> {
    entries(x)
        .into_iter()
        .enumerate()
        .map(|(i, (n, v))| {
            let path = format!("/variants/{i}");
            if !inherits(v, "r8_variant") {
                return Err(built(&path, BuildError::NotVariant { name: n }));
            }
            let of = member(v, "of");
            let fs = member(v, "fields");
            let shape = if !is_null(of) {
                VariantShape::Newtype {
                    of: attr_string(of, sym_r8_name())
                        .and_then(|q| QualifiedName::parse(&q).ok())
                        .ok_or_else(|| built(&format!("{path}/of"), BuildError::NotFieldType))?,
                }
            } else if xlen(fs) == 0 {
                VariantShape::Unit
            } else {
                VariantShape::Struct {
                    fields: fields(fs, lookup, &format!("{path}/fields"))?,
                    rules: rules(member(v, "rules"), &format!("{path}/rules"))?,
                }
            };
            Ok(VariantDef {
                name: name(&n, &format!("{path}/name"))?,
                description: one_string(member(v, "description")).unwrap_or_default(),
                shape,
            })
        })
        .collect()
}

/// The problems of a declaration that does not read, in the form of the
/// declaration check's problems.
fn problems_of(p: Problem) -> List {
    list!(
        code = vec![p.code.to_string()],
        path = vec![p.path],
        message = vec![p.message]
    )
}

/// Declares a struct or an enum from the user's objects: claims the
/// namespace, reads the declaration, checks it, registers it, and writes its
/// record.
///
/// @param kind Character: `"struct"` or `"enum"`.
/// @param name Character: The type name.
/// @param description Character: The type's description.
/// @param version Integer: The type's version.
/// @param tag Character: An enum's tag member.
/// @param members List: A struct's fields or an enum's variants, as given.
/// @param rules List: A struct's rules.
/// @param namespace Character: The namespace.
/// @param package Character: The declaring package, or `""`.
/// @return List: `owner`, the package that owns the namespace when it is not
///   `package`, otherwise `""`; `problems`, as character vectors `code`,
///   `path`, and `message`; `record`, the record when the declaration
///   registers, otherwise `NULL`; and `required`, the names of a struct's
///   required fields.
/// @keywords internal
/// @noRd
#[extendr]
#[allow(clippy::too_many_arguments)]
fn r8_declare(
    kind: &str,
    name: &str,
    description: &str,
    version: i32,
    tag: &str,
    members: Robj,
    rules: Robj,
    namespace: &str,
    package: &str,
) -> extendr_api::Result<List> {
    let owner = crate::declare::claim_namespace(namespace, package)?;
    let empty = || {
        list!(
            code = Vec::<String>::new(),
            path = Vec::<String>::new(),
            message = Vec::<String>::new()
        )
    };
    if !owner.is_empty() {
        return Ok(list!(
            owner = owner,
            problems = empty(),
            record = (),
            formals = (),
            required = Vec::<bool>::new()
        ));
    }
    let snapshot = crate::declare::snapshot()?;
    let lookup = &*snapshot;
    let def = (|| -> Read<TypeDef> {
        let n = self::name(name, "/name")?;
        let version = u32::try_from(version).unwrap_or(1);
        let m = unsafe { members.get() };
        Ok(if kind == "enum" {
            TypeDef::Enum(EnumDef {
                name: n,
                description: description.to_string(),
                tag: self::name(tag, "/tag")?,
                version,
                variants: variants(m, lookup)?,
                migrations: Vec::new(),
                aliases: Vec::new(),
                extensions: BTreeMap::new(),
            })
        } else {
            TypeDef::Struct(StructDef {
                name: n,
                description: description.to_string(),
                version,
                fields: fields(m, lookup, "/fields")?,
                extends: None,
                rules: self::rules(unsafe { rules.get() }, "/rules")?,
                migrations: Vec::new(),
                aliases: Vec::new(),
                provenance: false,
                s4: false,
                extensions: BTreeMap::new(),
            })
        })
    })();
    drop(snapshot);
    let def = match def {
        Ok(d) => d,
        Err(p) => {
            return Ok(list!(
                owner = "",
                problems = problems_of(p),
                record = (),
                formals = (),
                required = Vec::<bool>::new()
            ));
        }
    };
    let (formals, required) = match &def {
        TypeDef::Struct(s) => struct_formals(unsafe { members.get() }, s),
        _ => (Robj::from(()), Vec::new()),
    };
    let q = QualifiedName {
        namespace: Name::new(namespace).map_err(|e| extendr_api::Error::Other(e.to_string()))?,
        name: def.name().clone(),
    };
    let problems = crate::declare::declare_type(def, namespace)?;
    let failed = problems.index("code").map(|c| c.len() > 0).unwrap_or(false);
    let record = if failed {
        Robj::from(())
    } else {
        Robj::from(crate::declare::type_record(&q)?.as_slice())
    };
    Ok(list!(
        owner = "",
        problems = problems,
        record = record,
        formals = formals,
        required = required
    ))
}

/// The constructor formals of a struct, from the fields as given, and which
/// fields are required. A required field has no default argument; a field
/// with a default takes it, except a `json_ir()` default, which construction
/// fills; any other field takes `NULL`.
fn struct_formals(members: SEXP, def: &StructDef) -> (Robj, Vec<bool>) {
    let mut b = crate::irser::Builder::named(def.fields.len());
    let mut required = Vec::with_capacity(def.fields.len());
    for ((n, x), f) in entries(members).iter().zip(&def.fields) {
        let r = f.is_required();
        required.push(r);
        let value = if r {
            unsafe { extendr_ffi::R_MissingArg }
        } else {
            let d = member(*x, "default");
            if inherits(*x, "r8_field")
                && is_true(member(*x, "has_default"))
                && !is_null(d)
                && !inherits(d, "r8_json_ir")
            {
                d
            } else {
                crate::rview::nil()
            }
        };
        b.object(n, value);
    }
    (b.list(), required)
}

/// `type_table(columns, open, rules)`, its columns as fields.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_ft_table(columns: Robj, open: Robj, rules: Robj) -> extendr_api::Result<Robj> {
    let snapshot = crate::declare::snapshot()?;
    let built = (|| -> Read<FieldType> {
        Ok(FieldType::Table {
            columns: fields(unsafe { columns.get() }, &*snapshot, "/columns")?.into_boxed_slice(),
            open: is_true(unsafe { open.get() }),
            rules: self::rules(unsafe { rules.get() }, "/rules")?.into_boxed_slice(),
        })
    })();
    Ok(match built {
        Ok(t) => {
            let mut out =
                crate::irser::to_r(&t).map_err(|e| extendr_api::Error::Other(e.to_string()))?;
            out.set_class(["r8_field_type"])?;
            out
        }
        Err(p) => Robj::from(format!("{} [{} at {}]", p.message, p.code, p.path)),
    })
}

/// The canonical JSON text of the default `x` of a field of type `type`, or
/// `""` when `x` is not a default of that type.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_default_canonical(x: Robj, r#type: Robj) -> extendr_api::Result<String> {
    let snapshot = crate::declare::snapshot()?;
    let Ok(ty) = crate::irde::from_r::<FieldType>(unsafe { r#type.get() }) else {
        return Ok(String::new());
    };
    Ok(default_value(unsafe { x.get() }, &ty, &*snapshot, "")
        .map(|v| rtemis_r8::codec::canonical_text(&v))
        .unwrap_or_default())
}

extendr_module! {
    mod reader;
    fn r8_declare;
    fn r8_ft_table;
    fn r8_default_canonical;
}
