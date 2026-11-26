//! Documents of a call of a typed function, and the JSON Schema of its
//! arguments.
//!
//! A document of a call is a JSON object with the members `"$function"`,
//! the function's qualified name, and `"arguments"`, an object of the
//! arguments by parameter name; `"$types"`, the versions of the named types
//! that the arguments contain, is optional and checked when present.
//! [`read_call`] checks a document against the functions a caller admits and
//! returns every problem at once: of the document's form, of each argument
//! against its parameter's type, and, when every argument holds, of the
//! function's rules. [`arguments_schema`] writes the JSON Schema of the
//! arguments object, which a tool's input schema takes.
//!
//! A function has a document form when every parameter has a type of a
//! document form: no parameter without a type, no `...`, and no
//! `host_class()` outside `option()` ([`undocumented_params`]).
//!
//! spec: R8/class-system#typed-functions

use serde_json::{Map, Value, json};

use crate::access::JsonOf;
use crate::canonical::{Envelope, contained, document_error, fill_defaults};
use crate::ir::{FieldDef, FieldType, FunctionDef, ParamDef, ReturnType};
use crate::name::QualifiedName;
use crate::problem::{Neutral, Problem, Segment};
use crate::rules::{Outcome, TypeLookup};
use crate::tree::{Tree, TreeRef};
use crate::validate::{RuleCache, ValueError, validate_field_access};

/// The parameters of `def` that a document cannot hold, by name: those
/// without a type, `...`, and those of a type `host_class()` outside
/// `option()`.
pub fn undocumented_params(def: &FunctionDef) -> Vec<String> {
    def.params
        .iter()
        .filter(|p| match p.ty() {
            None => true,
            Some(FieldType::HostClass { .. }) => true,
            Some(_) => false,
        })
        .map(|p| p.name().to_string())
        .collect()
}

/// A parameter's default: `true` when a call may omit its argument.
fn has_default(p: &ParamDef) -> bool {
    match p {
        ParamDef::Name(_) => false,
        ParamDef::Full(p) => p.default.is_some() || p.host_default.is_some(),
    }
}

/// The JSON Schema 2020-12 of the arguments object of the function `f`, or
/// the names of its parameters that a document cannot hold
/// ([`undocumented_params`]). A parameter without a default is required; one
/// of type `option()` admits `null`. A default in JSON form is `default`, and
/// one written in a host's language is the annotation `x-r8.host_default`.
/// The function's rules and return type are annotations under `x-r8`. With
/// `closed`, `dyn()` and `subtype()` types are the closed lists of
/// [`crate::value_schema::json_schema_with`].
pub fn arguments_schema(
    f: &QualifiedName,
    lookup: &dyn TypeLookup,
    closed: bool,
) -> Result<Value, Vec<String>> {
    let Some(def) = lookup.function_of(f) else {
        return Err(Vec::new());
    };
    let undocumented = undocumented_params(def);
    if !undocumented.is_empty() {
        return Err(undocumented);
    }
    let mut w = crate::value_schema::writer(lookup, closed);
    let mut props = Map::new();
    let mut required = Vec::new();
    for p in &def.params {
        let ParamDef::Full(q) = p else { continue };
        let Some(ty) = &q.ty else { continue };
        let mut s = w.ty(ty);
        if !q.description.is_empty() {
            s["description"] = json!(&*q.description);
        }
        if let Some(d) = &q.default {
            s["default"] = d.to_value();
        }
        if let Some(h) = &q.host_default {
            s["x-r8"] = json!({"host_default": {"host": &*h.host, "text": &*h.text}});
        }
        if !has_default(p) {
            required.push(Value::from(q.name.as_str()));
        }
        props.insert(q.name.clone(), s);
    }
    let mut annotations = Map::new();
    annotations.insert("function".into(), json!(f.to_string()));
    if !def.rules.is_empty() {
        annotations.insert(
            "rules".into(),
            serde_json::to_value(&def.rules).expect("rules serialize"),
        );
    }
    if let Some(ReturnType::Type(t)) = &def.return_type {
        annotations.insert("return_type".into(), w.ty(t));
    }
    let mut out = Map::new();
    out.insert(
        "$schema".into(),
        json!("https://json-schema.org/draft/2020-12/schema"),
    );
    out.insert("title".into(), json!(f.to_string()));
    if !def.description.is_empty() {
        out.insert("description".into(), json!(def.description));
    }
    out.insert("type".into(), json!("object"));
    out.insert("properties".into(), Value::Object(props));
    if !required.is_empty() {
        out.insert("required".into(), Value::Array(required));
    }
    out.insert("additionalProperties".into(), json!(false));
    out.insert("x-r8".into(), Value::Object(annotations));
    let defs = w.into_defs();
    if !defs.is_empty() {
        out.insert("$defs".into(), Value::Object(defs.into_iter().collect()));
    }
    Ok(Value::Object(out))
}

/// A document of a call that [`read_call`] admits.
#[derive(Debug)]
pub struct Call {
    /// The function's qualified name.
    pub function: QualifiedName,
    /// The arguments given, by parameter name, in the order of the
    /// parameters, each with the defaults of the fields of its values
    /// filled; an omitted argument is not among them.
    pub arguments: Vec<(String, Tree)>,
    /// The problems that the check could not decide: a rule that reads an
    /// omitted argument whose default is written in a host's language, as
    /// `rule_unchecked`.
    pub unchecked: Vec<ValueError>,
}

fn form_error(path: &str, message: impl Into<String>) -> ValueError {
    document_error("call_form", path, message.into())
}

fn pointer_token(name: &str) -> String {
    name.replace('~', "~0").replace('/', "~1")
}

/// Reads a document of a call of one of the functions `admitted` and returns
/// the call, or every problem it has: of the document's form (`document_json`,
/// `call_form`), an unknown or inadmissible function (`unknown_function`), a
/// function without a document form (`function_undocumented`), an argument
/// that names no parameter (`unknown_argument`) or that is missing
/// (`missing_argument`), each argument against its parameter's type, with
/// paths under `/arguments`, the versions of `"$types"` when present, and,
/// when every argument holds, the function's rules over the typed
/// parameters, with the defaults of omitted arguments in JSON form filled.
/// The problems of a call that does not hold include its `rule_unchecked`
/// rules.
pub fn read_call(
    text: &str,
    admitted: &[QualifiedName],
    lookup: &dyn TypeLookup,
    cache: &mut RuleCache,
) -> Result<Call, Vec<ValueError>> {
    let doc: Tree = serde_json::from_str(text).map_err(|e| {
        vec![document_error(
            "document_json",
            "",
            format!("not a JSON document: {e}"),
        )]
    })?;
    let Tree::Object(members) = doc else {
        return Err(vec![form_error(
            "",
            "a document of a call is a JSON object",
        )]);
    };
    let mut errors = Vec::new();
    let mut function = None;
    let mut arguments = None;
    let mut versions = None;
    for (k, v) in members.into_vec() {
        match &*k {
            "$function" => function = Some(v),
            "arguments" => arguments = Some(v),
            "$types" => versions = Some(v),
            other => errors.push(form_error(
                &format!("/{}", pointer_token(other)),
                format!(
                    "`{other}` is not a member of a document of a call, which holds `$function`, `arguments`, and `$types`"
                ),
            )),
        }
    }
    let name = match &function {
        Some(Tree::Str(s)) => match QualifiedName::parse(s) {
            Ok(q) if admitted.contains(&q) => Some(q),
            _ => {
                errors.push(document_error(
                    "unknown_function",
                    "/$function",
                    format!(
                        "`{s}` is not a function that this call admits; it admits {}",
                        admitted_list(admitted)
                    ),
                ));
                None
            }
        },
        _ => {
            errors.push(form_error(
                "/$function",
                "a document of a call names its function in `$function`, such as \"ns::f\"",
            ));
            None
        }
    };
    let given = match arguments {
        Some(Tree::Object(m)) => Some(m.into_vec()),
        _ => {
            errors.push(form_error(
                "/arguments",
                "a document of a call holds its arguments in `arguments`, an object",
            ));
            None
        }
    };
    let envelope = match &versions {
        None => None,
        Some(Tree::Object(m)) => Some(Envelope {
            versions: m
                .iter()
                .map(|(k, v)| {
                    let v = match v {
                        Tree::Num(n) => n.as_u64(),
                        _ => None,
                    };
                    (&**k, v)
                })
                .collect(),
            declarations: None,
            ir: None,
        }),
        Some(_) => {
            errors.push(form_error("/$types", "`$types` holds a version per type"));
            None
        }
    };
    let (Some(name), Some(given)) = (name, given) else {
        return Err(errors);
    };
    let Some(def) = lookup.function_of(&name) else {
        errors.push(document_error(
            "unknown_function",
            "/$function",
            format!("`{name}` is not registered"),
        ));
        return Err(errors);
    };
    let undocumented = undocumented_params(def);
    if !undocumented.is_empty() {
        errors.push(document_error(
            "function_undocumented",
            "/$function",
            format!(
                "`{name}()` has parameters that a document cannot hold: {}",
                undocumented
                    .iter()
                    .map(|n| format!("`{n}`"))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
        ));
        return Err(errors);
    }
    let mut args: Vec<(String, Tree)> = Vec::with_capacity(given.len());
    let names: Vec<&str> = def.params.iter().map(ParamDef::name).collect();
    for (k, v) in given {
        if names.contains(&&*k) {
            args.push((k.into(), v));
        } else {
            errors.push(ValueError {
                code: "unknown_argument",
                path: format!("/arguments/{}", pointer_token(&k)),
                message: format!(
                    "`{k}` is not a parameter of `{name}()`, whose parameters are {}",
                    names
                        .iter()
                        .map(|n| format!("`{n}`"))
                        .collect::<Vec<_>>()
                        .join(", ")
                ),
                suggestion: crate::name::nearest(&k, names.iter().copied()).map(str::to_string),
                ..Default::default()
            });
        }
    }
    let mut ordered = Vec::with_capacity(args.len());
    for p in &def.params {
        let ty = p.ty().expect("a parameter of a document form has a type");
        let at = args.iter().position(|(k, _)| k == p.name());
        let Some(at) = at else {
            if !has_default(p) {
                errors.push(ValueError {
                    code: "missing_argument",
                    path: format!("/arguments/{}", pointer_token(p.name())),
                    segments: vec![Segment::Field(p.name().to_string())],
                    problem: Some(Problem::Required),
                    ..Default::default()
                });
            }
            continue;
        };
        let (k, mut v) = args.swap_remove(at);
        let found = validate_field_access(JsonOf::of(TreeRef::new(&v)), ty, lookup, cache);
        if found.is_empty() {
            fill_defaults(&mut v, ty, lookup);
        }
        for mut e in found {
            e.path = format!("/arguments/{}{}", pointer_token(&k), e.path);
            e.segments.insert(0, Segment::Field(k.clone()));
            errors.push(e);
        }
        ordered.push((k, v));
    }
    if let Some(envelope) = &envelope {
        let mut found = std::collections::BTreeMap::new();
        for (k, v) in &ordered {
            if let Some(ty) = def
                .params
                .iter()
                .find(|p| p.name() == k)
                .and_then(ParamDef::ty)
            {
                contained(JsonOf::of(TreeRef::new(v)), ty, lookup, &mut found);
            }
        }
        for (n, current) in found {
            envelope.contained(&n, current, &mut errors);
        }
    }
    let mut unchecked = Vec::new();
    if errors.is_empty() && !def.rules.is_empty() {
        rule_problems(def, &ordered, lookup, cache, &mut errors, &mut unchecked);
    }
    for e in &mut errors {
        if let Some(p) = &e.problem {
            e.message = p.sentence(
                &e.segments,
                e.found.as_ref(),
                e.suggestion.as_deref(),
                &Neutral,
            );
        }
    }
    if errors.is_empty() {
        Ok(Call {
            function: name,
            arguments: ordered,
            unchecked,
        })
    } else {
        errors.extend(unchecked);
        Err(errors)
    }
}

fn admitted_list(admitted: &[QualifiedName]) -> String {
    if admitted.is_empty() {
        return "none".to_string();
    }
    admitted
        .iter()
        .map(|q| format!("`{q}`"))
        .collect::<Vec<_>>()
        .join(", ")
}

/// The violations of the rules of `def` by the arguments `given`, with the
/// defaults in JSON form of omitted arguments; a rule that reads an omitted
/// argument whose default is written in a host's language goes to
/// `unchecked`.
fn rule_problems(
    def: &FunctionDef,
    given: &[(String, Tree)],
    lookup: &dyn TypeLookup,
    cache: &mut RuleCache,
    errors: &mut Vec<ValueError>,
    unchecked: &mut Vec<ValueError>,
) {
    let scope: Vec<FieldDef> = def.rule_scope();
    let mut host_only = Vec::new();
    let mut values: Vec<(Box<str>, Tree)> = Vec::with_capacity(def.params.len());
    for p in &def.params {
        if let Some((_, v)) = given.iter().find(|(k, _)| k == p.name()) {
            values.push((p.name().into(), v.clone()));
            continue;
        }
        if let ParamDef::Full(q) = p {
            if let Some(d) = &q.default {
                let mut d = (**d).clone();
                if let Some(ty) = &q.ty {
                    fill_defaults(&mut d, ty, lookup);
                }
                values.push((q.name.as_str().into(), d));
            } else if q.host_default.is_some() {
                host_only.push(q.name.as_str());
            }
        }
    }
    let record = Tree::Object(values.into_boxed_slice());
    let view = JsonOf::of(TreeRef::new(&record));
    for (ri, r) in def.rules.iter().enumerate() {
        let Some(rule) = cache.compiled(r, &scope, lookup) else {
            errors.push(document_error(
                "rule_invalid",
                "/arguments",
                format!("rule {ri} does not compile; check the declaration"),
            ));
            continue;
        };
        if let Some(n) = host_only.iter().find(|n| rule.reads_field(n)) {
            unchecked.push(ValueError {
                code: "rule_unchecked",
                path: "/arguments".to_string(),
                rule: r.id.clone(),
                rule_index: Some(ri),
                message: format!(
                    "the rule reads `{n}`, whose default the function evaluates when it runs"
                ),
                ..Default::default()
            });
            continue;
        }
        match rule.check_access(view, &scope, lookup) {
            Ok(Outcome::Holds) => {}
            Ok(Outcome::Violated(paths)) => {
                for (k, p) in paths.into_iter().enumerate() {
                    let diagnosis = (k < crate::validate::DIAGNOSED).then(|| {
                        crate::validate::diagnosis(
                            r,
                            rule.diagnose_access(view, &scope, lookup, &p),
                        )
                    });
                    errors.push(ValueError {
                        code: "rule_violation",
                        path: format!("/arguments{}", crate::problem::pointer(&p)),
                        segments: p,
                        rule: r.id.clone(),
                        rule_index: Some(ri),
                        message: r.message.clone().unwrap_or_default(),
                        diagnosis,
                        ..Default::default()
                    });
                }
            }
            Err(e) => {
                let mut d =
                    crate::validate::diagnosis(r, rule.diagnose_access(view, &scope, lookup, &[]));
                d.operands.clear();
                errors.push(ValueError {
                    code: "rule_error",
                    path: "/arguments".to_string(),
                    rule: r.id.clone(),
                    rule_index: Some(ri),
                    message: e,
                    diagnosis: Some(d),
                    ..Default::default()
                });
            }
        }
    }
}

/// The text of the document of a call of `f` with `arguments`, each a
/// parameter's name and its argument's JSON form, in RFC 8785 form, with
/// `"$types"` holding the version of every named type that the arguments
/// contain. The arguments are written as given; [`read_call`] checks the
/// text.
pub fn write_call(
    f: &QualifiedName,
    arguments: &[(String, Value)],
    lookup: &dyn TypeLookup,
) -> String {
    let mut found = std::collections::BTreeMap::new();
    let mut args = Map::new();
    let def = lookup.function_of(f);
    for (k, v) in arguments {
        if let Some(ty) = def
            .and_then(|d| d.params.iter().find(|p| p.name() == k))
            .and_then(ParamDef::ty)
        {
            contained(crate::access::Json::new(v), ty, lookup, &mut found);
        }
        args.insert(k.clone(), v.clone());
    }
    let doc = json!({
        "$function": f.to_string(),
        "$types": found,
        "arguments": args,
    });
    crate::canonical::canonical_text(&doc)
}
