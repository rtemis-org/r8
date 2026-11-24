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
use rtemis_r8::ir::build::BuildError;
use rtemis_r8::ir::{EnumDef, FieldDef, FieldType, RuleDef, StructDef, TypeDef, VariantDef};
use rtemis_r8::name::{Name, NameError, QualifiedName};
use rtemis_r8::rules::TypeLookup;
use rtemis_r8::tree::{Exact, Tree};
use serde_json::Value;

use crate::fieldtype::field_type;
use crate::rview::{
    attr, attr_string, inherits, is_null, sexptype, string_elt, sym_names, sym_r8_name, xlen,
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
        BuildError::DefaultNotValue => "field_default",
        BuildError::ColumnOutsideTable => "column_position",
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

/// The JSON form of the default `x` of `name`, a field or parameter of type
/// `ty`: an R value of the type, whose normalized form it takes, as a tree
/// whose numbers are kept as written. `json_ir()` gives the JSON form itself,
/// and `int64()` an integer by its digits. `NULL` is the default of a type
/// that admits an absent value, `option()`.
fn default_value(
    x: SEXP,
    ty: &FieldType,
    lookup: &(impl TypeLookup + Sized),
    path: &str,
    name: &str,
) -> Read<Tree> {
    if inherits(x, "r8_json_ir") || inherits(x, "r8_int64") {
        return read::<Exact>(x, path).map(|e| e.0);
    }
    let invalid = |path: &str, why: &str| {
        problem(
            "default_invalid",
            path,
            format!("the default of `{name}` is invalid: {why}"),
        )
    };
    match ty {
        FieldType::Option { .. } if is_null(x) => Ok(Tree::Null),
        FieldType::Option { inner, .. } => default_value(x, inner, lookup, path, name),
        // A list, whose items may be `json_ir()`.
        FieldType::Vec { element, .. } if sexptype(x) == SEXPTYPE::VECSXP => (0..xlen(x))
            .map(|i| {
                let e = unsafe { VECTOR_ELT(x, i as isize) };
                default_value(e, element, lookup, &format!("{path}/{i}"), name)
            })
            .collect::<Read<Vec<_>>>()
            .map(|v| Tree::Array(v.into_boxed_slice())),
        FieldType::Named { .. } if !crate::value::is_value(x) => {
            Err(built(path, BuildError::DefaultNotValue))
        }
        FieldType::HostClass { .. } => {
            Err(invalid(path, "a value of a host class has no JSON form"))
        }
        _ => {
            let lookup: &dyn TypeLookup = lookup;
            let view = crate::rview::RView::Whole(x);
            let errors = crate::construct::with_cache(|cache| {
                rtemis_r8::validate::validate_field_access(view, ty, lookup, cache)
            });
            match errors.iter().find(|e| e.code != "unknown_type") {
                Some(e) => Err(invalid(&format!("{path}{}", e.path), &e.message)),
                None => Ok(rtemis_r8::canonical::normalized_field_tree(
                    view, ty, lookup,
                )),
            }
        }
    }
}

/// A field from its name and the user's object; `column` admits a
/// `column()`, whose own members the caller reads.
fn field_def(
    n: &str,
    x: SEXP,
    lookup: &impl TypeLookup,
    path: &str,
    column: bool,
) -> Read<FieldDef> {
    if !column && inherits(x, "r8_column") {
        return Err(built(path, BuildError::ColumnOutsideTable));
    }
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
            resolve: None,
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
            n.as_str(),
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
        resolve: {
            let r = member(x, "resolve");
            if is_null(r) {
                None
            } else {
                Some(read::<rtemis_r8::ir::Resolve>(
                    r,
                    &format!("{path}/resolve"),
                )?)
            }
        },
        inherited_from: None,
        extensions: extensions.filter(|m| !m.is_empty()),
    })
}

/// The fields of a named list of field objects.
pub(crate) fn fields(x: SEXP, lookup: &impl TypeLookup, path: &str) -> Read<Vec<FieldDef>> {
    // Sized exactly: the registry keeps the vector for the session, and a
    // collect through `Result` grows it by doubling.
    let items = entries(x);
    let mut out = Vec::with_capacity(items.len());
    for (i, (n, f)) in items.into_iter().enumerate() {
        out.push(field_def(&n, f, lookup, &format!("{path}/{i}"), false)?);
    }
    Ok(out)
}

/// The columns of `type_table(columns)`: each a field type, a `field()`, or
/// a `column()`, which adds `unique` and `required`.
pub(crate) fn columns(
    x: SEXP,
    lookup: &impl TypeLookup,
    path: &str,
) -> Read<Vec<rtemis_r8::ir::ColumnDef>> {
    let items = entries(x);
    let mut out = Vec::with_capacity(items.len());
    for (i, (n, c)) in items.into_iter().enumerate() {
        let p = format!("{path}/{i}");
        let column = inherits(c, "r8_column");
        let field = field_def(&n, c, lookup, &p, column)?;
        if field.resolve.is_some() {
            return Err(problem(
                "resolve_position",
                &format!("{p}/resolve"),
                format!(
                    "column `{n}` has a fill policy, which the fields of a struct or a struct variant take"
                ),
            ));
        }
        out.push(rtemis_r8::ir::ColumnDef {
            field,
            unique: column && is_true(member(c, "unique")),
            required: !column || is_true(member(c, "required")),
        });
    }
    Ok(out)
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
    let items = entries(x);
    let n_items = items.len();
    let mut out: Vec<VariantDef> = items
        .into_iter()
        .enumerate()
        .map(|(i, (n, v))| {
            let path = format!("/variants/{i}");
            if !inherits(v, "r8_variant") {
                return Err(built(&path, BuildError::NotVariant { name: n }));
            }
            let of = member(v, "of");
            let of = if is_null(of) {
                None
            } else {
                Some(
                    attr_string(of, sym_r8_name())
                        .and_then(|q| QualifiedName::parse(&q).ok())
                        .ok_or_else(|| built(&format!("{path}/of"), BuildError::VariantOf))?,
                )
            };
            let kind = rtemis_r8::ir::build::variant_kind(
                fields(member(v, "fields"), lookup, &format!("{path}/fields"))?,
                rules(member(v, "rules"), &format!("{path}/rules"))?,
                of,
            )
            .map_err(|e| built(&path, e))?;
            Ok(VariantDef {
                name: name(&n, &format!("{path}/name"))?,
                description: one_string(member(v, "description")).unwrap_or_default(),
                kind,
            })
        })
        .collect::<Read<Vec<_>>>()?;
    out.shrink_to(n_items);
    Ok(out)
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
/// @param parent Character: The qualified name of the struct's parent, or
///   `""`.
/// @param defaults List: The child's defaults of inherited fields, by name.
/// @param parent_formals List: The parent constructor's formals.
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
    parent: &str,
    defaults: Robj,
    parent_formals: Robj,
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
                extensions: BTreeMap::new(),
            })
        })
    })();
    let def = def.and_then(|d| match d {
        TypeDef::Struct(s) if !parent.is_empty() => {
            extended(s, parent, unsafe { defaults.get() }, lookup).map(|r| r.map(TypeDef::Struct))
        }
        d => Ok(Ok::<_, Vec<rtemis_r8::ir::DeclError>>(d)),
    });
    drop(snapshot);
    let def = match def {
        Ok(Ok(d)) => d,
        Ok(Err(errors)) => {
            return Ok(list!(
                owner = "",
                problems = decl_problems(&errors),
                record = (),
                formals = (),
                required = Vec::<bool>::new()
            ));
        }
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
        TypeDef::Struct(s) => struct_formals(
            unsafe { members.get() },
            unsafe { defaults.get() },
            unsafe { parent_formals.get() },
            s,
        ),
        _ => (Robj::from(()), Vec::new()),
    };
    let q = QualifiedName {
        namespace: Name::new(namespace).map_err(|e| extendr_api::Error::Other(e.to_string()))?,
        name: def.name().clone(),
    };
    let (problems, stale) = crate::declare::declare_type(def, namespace)?;
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
        required = required,
        stale = stale
    ))
}

/// The constructor formals of a struct, from the fields as given, and which
/// fields are required. A required field has no default argument; a field
/// with a default takes it, except a `json_ir()` default, which construction
/// fills; any other field takes `NULL`. An inherited field takes the child's
/// default from `defaults`, or else the parent constructor's formal.
fn struct_formals(
    members: SEXP,
    defaults: SEXP,
    parent_formals: SEXP,
    def: &StructDef,
) -> (Robj, Vec<bool>) {
    let mut b = crate::irser::Builder::named(def.fields.len());
    let mut required = Vec::with_capacity(def.fields.len());
    let own = entries(members);
    let parent = entries(parent_formals);
    let given = entries(defaults);
    for f in &def.fields {
        let n = f.name.as_str();
        let r = f.is_required();
        required.push(r);
        let value = if r {
            unsafe { extendr_ffi::R_MissingArg }
        } else if f.inherited_from.is_some() {
            match given.iter().find(|(k, _)| k == n) {
                Some((_, d)) if !inherits(*d, "r8_json_ir") => *d,
                Some(_) => crate::rview::nil(),
                None => parent
                    .iter()
                    .find(|(k, _)| k == n)
                    .map_or_else(crate::rview::nil, |(_, d)| *d),
            }
        } else {
            let x = own
                .iter()
                .find(|(k, _)| k == n)
                .map_or_else(crate::rview::nil, |(_, x)| *x);
            let d = member(x, "default");
            if inherits(x, "r8_field")
                && is_true(member(x, "has_default"))
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

/// The child `s`, which holds the fields and rules it adds, lowered onto
/// its registered parent `parent` with the defaults `defaults` of inherited
/// fields; the problems of the lowering, at paths of the child's declaration.
fn extended(
    mut s: StructDef,
    parent: &str,
    defaults: SEXP,
    lookup: &(impl TypeLookup + Sized),
) -> Read<std::result::Result<StructDef, Vec<rtemis_r8::ir::DeclError>>> {
    let pq = QualifiedName::parse(parent)
        .map_err(|e| problem("extends_unknown", "/extends", e.to_string()))?;
    let p = match lookup.find(&pq) {
        Some(TypeDef::Struct(p)) => p,
        Some(TypeDef::Enum(_)) => {
            return Err(problem(
                "extends_not_struct",
                "/extends",
                format!("`{pq}` is an enum; `extends` names a struct"),
            ));
        }
        None => {
            return Err(problem(
                "extends_unknown",
                "/extends",
                format!("`{pq}` is not a registered type"),
            ));
        }
    };
    let mut changes = Vec::new();
    for (i, (n, x)) in entries(defaults).into_iter().enumerate() {
        let path = format!("/defaults/{i}");
        let n = name(&n, &path)?;
        let default = match p.fields.iter().find(|f| f.name == n) {
            // `NULL` removes the default, which the lowering admits for an
            // `option()` field only.
            Some(_) if is_null(x) => Some(None),
            Some(f) => Some(Some(Box::new(default_value(
                x,
                &f.ty,
                lookup,
                &path,
                f.name.as_str(),
            )?))),
            None => None,
        };
        changes.push(rtemis_r8::ir::InheritedChange {
            name: n,
            default,
            sensitive: false,
            description: None,
        });
    }
    Ok(rtemis_r8::ir::extend(&mut s, &pq, p, &changes).map(|()| s))
}

/// The problems of a declaration check, as character vectors.
fn decl_problems(errors: &[rtemis_r8::ir::DeclError]) -> List {
    list!(
        code = errors
            .iter()
            .map(|e| e.code.to_string())
            .collect::<Vec<_>>(),
        path = errors.iter().map(|e| e.path.clone()).collect::<Vec<_>>(),
        message = errors.iter().map(|e| e.message.clone()).collect::<Vec<_>>()
    )
}

/// `type_table(columns, open, rules)`, its columns as fields.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_ft_table(columns: Robj, open: Robj, rules: Robj) -> extendr_api::Result<Robj> {
    let snapshot = crate::declare::snapshot()?;
    let built = (|| -> Read<FieldType> {
        Ok(FieldType::Table {
            columns: self::columns(unsafe { columns.get() }, &*snapshot, "/columns")?
                .into_boxed_slice(),
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
    Ok(default_value(unsafe { x.get() }, &ty, &*snapshot, "", "")
        .map(|v| rtemis_r8::canonical::canonical_text(&v.to_value()))
        .unwrap_or_default())
}

/// The required fields of `new_trait(required_fields = )`: each a field type,
/// an R8 type object, or `field(type, description = )`; a default, a fill
/// policy, `sensitive`, a unit, and extensions belong to a struct's field.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_required_fields(required_fields: Robj) -> extendr_api::Result<Robj> {
    let snapshot = crate::declare::snapshot()?;
    let built = (|| -> Read<Vec<rtemis_r8::ir::RequiredField>> {
        let fs = fields(
            unsafe { required_fields.get() },
            &*snapshot,
            "/required_fields",
        )?;
        fs.into_iter()
            .enumerate()
            .map(|(i, f)| {
                if f.default.is_some()
                    || f.resolve.is_some()
                    || f.sensitive
                    || f.unit.is_some()
                    || f.extensions.is_some()
                {
                    return Err(problem(
                        "required_field_form",
                        &format!("/required_fields/{i}"),
                        format!(
                            "required field `{}` has a type and a description; its default, fill policy, sensitivity, unit, and extensions belong to a struct's field",
                            f.name
                        ),
                    ));
                }
                Ok(rtemis_r8::ir::RequiredField {
                    name: f.name,
                    ty: f.ty,
                    description: f.description,
                })
            })
            .collect()
    })();
    Ok(match built {
        Ok(r) => crate::irser::to_r(&r).map_err(|e| extendr_api::Error::Other(e.to_string()))?,
        Err(p) => Robj::from(format!("{} [{} at {}]", p.message, p.code, p.path)),
    })
}

/// Whether a call of R holds a default of type `ty` in its JSON form: a
/// value of a host class has none.
fn has_json_form(ty: &FieldType, default: SEXP) -> bool {
    match ty {
        FieldType::HostClass { .. } => false,
        FieldType::Option { inner } => is_null(default) || has_json_form(inner, default),
        _ => true,
    }
}

/// The IR parameters of a generic or trait method, from `params()`.
///
/// @param names Character: The parameters' names.
/// @param types List: Each parameter's type, a field type or an R8 type
///   object, or `NULL`.
/// @param constants List: For a default that is a syntactic constant, a list
///   of its value; otherwise `NULL`.
/// @param texts Character: Each default's source text, or `""`.
/// @param dispatch Character: The names of the dispatched parameters.
/// @return List: The IR parameters, or the message of one that does not
///   read.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_params_ir(
    names: Vec<String>,
    types: Robj,
    constants: Robj,
    texts: Vec<String>,
    dispatch: Vec<String>,
) -> extendr_api::Result<Robj> {
    use rtemis_r8::ir::{HostDefault, Param, ParamDef};
    let snapshot = crate::declare::snapshot()?;
    let lookup = &*snapshot;
    // A method that dispatches on its first parameter alone carries no mark.
    let first_only = dispatch.len() == 1 && names.first() == dispatch.first();
    let (types, constants) = unsafe { (types.get(), constants.get()) };
    let built = (|| -> Read<Vec<ParamDef>> {
        let mut out = Vec::with_capacity(names.len());
        for (i, n) in names.iter().enumerate() {
            let path = format!("/params/{i}");
            let t = unsafe { VECTOR_ELT(types, i as isize) };
            let ty = if is_null(t) {
                None
            } else {
                Some(field_type(t).map_err(|e| built(&format!("{path}/type"), e))?)
            };
            let constant = unsafe { VECTOR_ELT(constants, i as isize) };
            let text = texts.get(i).map(String::as_str).unwrap_or_default();
            let mut default = None;
            let mut host_default = None;
            if !text.is_empty() {
                let value = (!is_null(constant)).then(|| unsafe { VECTOR_ELT(constant, 0) });
                match (&ty, value) {
                    (Some(ty), Some(v)) if has_json_form(ty, v) => {
                        default = Some(Box::new(default_value(
                            v,
                            ty,
                            lookup,
                            &format!("{path}/default"),
                            n,
                        )?));
                    }
                    _ => {
                        host_default = Some(HostDefault {
                            host: "r".into(),
                            text: text.into(),
                        });
                    }
                }
            }
            out.push(ParamDef::from(Param {
                name: n.clone(),
                dispatch: !first_only && dispatch.contains(n),
                ty,
                default,
                host_default,
                description: "".into(),
            }));
        }
        Ok(out)
    })();
    Ok(match built {
        Ok(p) => crate::irser::to_r(&p).map_err(|e| extendr_api::Error::Other(e.to_string()))?,
        Err(p) => Robj::from(format!("{} [{} at {}]", p.message, p.code, p.path)),
    })
}

extendr_module! {
    mod reader;
    fn r8_params_ir;
    fn r8_required_fields;
    fn r8_declare;
    fn r8_ft_table;
    fn r8_default_canonical;
}
