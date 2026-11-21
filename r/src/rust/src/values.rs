//! R-native values: the conversion between an R value and its JSON form, by
//! field type, and the construction, encoding, and decoding of values.
//!
//! A value is constructed by encoding the R input to its JSON form, validating
//! it with `rtemis-r8`, and decoding the JSON form back to R, so the stored R
//! value is the normalized one and reading its document returns it exactly.
//! A runtime-only field has no JSON form; construction carries its R value
//! over from the input.
//!
//! R forms: a scalar is a length-one atomic vector without attributes (an
//! integer converts to a double, a whole double to an integer); `vec()` of a
//! scalar is an atomic vector, otherwise an unnamed list; an absent
//! `option()` is `NULL`, and a missing element of `vec(option(T))` is `NA`; a
//! date is a `Date`, a date-time a `POSIXct`; a factor a `factor`; a matrix or
//! array an atomic vector with `dim`; a sparse matrix a `dgCMatrix`; a table
//! a `data.frame` with automatic row names (a tibble or a data.table is
//! accepted); a `hashmap()` a named atomic vector or a named list; an
//! `ordered_map()` a named list; `type_json()` nested lists; and a struct or
//! enum value an R8 value of its type.

use extendr_api::prelude::*;
use extendr_api::{Error, Result, SEXP};
use rtemis_r8::codec::{date_days, date_text, datetime_micros, datetime_text};
use rtemis_r8::ir::{FieldDef, FieldType, TypeDef, VariantShape};
use rtemis_r8::name::QualifiedName;
use rtemis_r8::rules::{DocumentSet, TypeLookup};
use rtemis_r8::validate::ValueError;
use serde_json::{Map, Value};

use crate::declare::registry;

pub(crate) fn fail(message: impl Into<String>) -> Error {
    Error::Other(message.into())
}

fn esc(s: &str) -> String {
    s.replace('~', "~0").replace('/', "~1")
}

/// The attributes of an R object, as R's `attributes()` returns them.
fn attribute_names(x: &Robj) -> Vec<String> {
    let Ok(f) = R!("base::attributes") else {
        return Vec::new();
    };
    let Some(f) = f.as_function() else {
        return Vec::new();
    };
    match f.call(pairlist!(x.clone())) {
        Ok(a) => a
            .names()
            .map(|n| n.map(str::to_string).collect())
            .unwrap_or_default(),
        Err(_) => Vec::new(),
    }
}

fn number(x: f64) -> Value {
    serde_json::Number::from_f64(if x == 0.0 { 0.0 } else { x }).map_or(Value::Null, Value::Number)
}

// ------------------------------------------------------------------ encode

pub(crate) struct Encoder<'a> {
    pub(crate) lookup: &'a dyn TypeLookup,
    pub(crate) errors: Vec<ValueError>,
    /// The paths of runtime-only fields that hold a value.
    #[cfg_attr(not(feature = "oracle"), allow(dead_code))]
    pub(crate) runtime: Vec<String>,
}

impl Encoder<'_> {
    fn err(&mut self, code: &'static str, path: &str, message: String) -> Value {
        self.errors.push(ValueError {
            code,
            path: path.to_string(),
            rule: None,
            message,
        });
        Value::Null
    }

    /// Rejects attributes other than `allowed` on a base-type input.
    fn plain(&mut self, x: &Robj, allowed: &[&str], path: &str) -> bool {
        let extra: Vec<String> = attribute_names(x)
            .into_iter()
            .filter(|a| !allowed.contains(&a.as_str()))
            .collect();
        if extra.is_empty() {
            return true;
        }
        self.err(
            "r_attributes",
            path,
            format!("the value carries the attributes {extra:?}, which this field's type does not hold; r8_conform() removes them"),
        );
        false
    }

    /// The elements of an atomic vector of a scalar type, each as JSON, with
    /// `NA` as `null`.
    fn atomic(&mut self, x: &Robj, element: &FieldType, path: &str) -> Option<Vec<Value>> {
        let (inner, _) = unwrap_option(element);
        match inner {
            FieldType::Logical => match x.as_logical_slice() {
                Some(s) => Some(
                    s.iter()
                        .map(|b| {
                            if b.is_na() {
                                Value::Null
                            } else {
                                Value::Bool(b.is_true())
                            }
                        })
                        .collect(),
                ),
                None => {
                    self.err("r_type", path, "expected a logical vector".to_string());
                    None
                }
            },
            FieldType::Integer(_) => match (x.as_integer_slice(), x.as_real_slice()) {
                (Some(s), _) => Some(
                    s.iter()
                        .map(|&i| {
                            if i == i32::MIN {
                                Value::Null
                            } else {
                                Value::from(i)
                            }
                        })
                        .collect(),
                ),
                (None, Some(s)) => {
                    let mut out = Vec::with_capacity(s.len());
                    for (k, &d) in s.iter().enumerate() {
                        if d.is_nan() {
                            out.push(Value::Null);
                        } else if d.fract() == 0.0 && d.abs() <= 2_147_483_647.0 {
                            out.push(Value::from(d as i64));
                        } else {
                            self.err(
                                "r_type",
                                &format!("{path}/{k}"),
                                format!("{d} is not a whole number in the integer range"),
                            );
                            out.push(Value::Null);
                        }
                    }
                    Some(out)
                }
                _ => {
                    self.err("r_type", path, "expected an integer vector".to_string());
                    None
                }
            },
            FieldType::Double(_) => {
                let s: Vec<f64> = match (x.as_real_slice(), x.as_integer_slice()) {
                    (Some(s), _) => s.to_vec(),
                    (None, Some(s)) => s
                        .iter()
                        .map(|&i| {
                            if i == i32::MIN {
                                f64::NAN
                            } else {
                                f64::from(i)
                            }
                        })
                        .collect(),
                    _ => {
                        self.err("r_type", path, "expected a numeric vector".to_string());
                        return None;
                    }
                };
                let na: Vec<bool> = match x.as_real_slice() {
                    Some(r) => r.iter().map(|d| d.is_na()).collect(),
                    None => x
                        .as_integer_slice()
                        .unwrap()
                        .iter()
                        .map(|&i| i == i32::MIN)
                        .collect(),
                };
                Some(
                    s.iter()
                        .zip(na)
                        .map(|(&d, is_na)| {
                            if is_na {
                                Value::Null
                            } else if d.is_nan() {
                                Value::from("NaN")
                            } else if d.is_infinite() {
                                Value::from(if d > 0.0 { "Infinity" } else { "-Infinity" })
                            } else {
                                number(d)
                            }
                        })
                        .collect(),
                )
            }
            FieldType::Character(_) => match x.as_str_iter() {
                Some(it) => Some(
                    it.map(|s| {
                        if s.is_na() {
                            Value::Null
                        } else {
                            Value::from(s)
                        }
                    })
                    .collect(),
                ),
                None => {
                    self.err("r_type", path, "expected a character vector".to_string());
                    None
                }
            },
            FieldType::Date => {
                if !x.inherits("Date") {
                    self.err("r_type", path, "expected a Date".to_string());
                    return None;
                }
                let days: Vec<Option<f64>> = match (x.as_real_slice(), x.as_integer_slice()) {
                    (Some(s), _) => s
                        .iter()
                        .map(|d| if d.is_nan() { None } else { Some(*d) })
                        .collect(),
                    (None, Some(s)) => s
                        .iter()
                        .map(|&i| {
                            if i == i32::MIN {
                                None
                            } else {
                                Some(f64::from(i))
                            }
                        })
                        .collect(),
                    _ => return None,
                };
                let mut out = Vec::with_capacity(days.len());
                for (k, d) in days.into_iter().enumerate() {
                    match d {
                        None => out.push(Value::Null),
                        Some(d) if d.fract() != 0.0 => {
                            out.push(self.err(
                                "date_fraction",
                                &format!("{path}/{k}"),
                                format!("a Date of {d} days has a fraction of a day"),
                            ));
                        }
                        Some(d) => out.push(Value::from(date_text(d as i64))),
                    }
                }
                Some(out)
            }
            FieldType::Posixct => {
                if !x.inherits("POSIXct") {
                    self.err("r_type", path, "expected a POSIXct".to_string());
                    return None;
                }
                let secs: Vec<f64> = match (x.as_real_slice(), x.as_integer_slice()) {
                    (Some(s), _) => s.to_vec(),
                    (None, Some(s)) => s
                        .iter()
                        .map(|&i| {
                            if i == i32::MIN {
                                f64::NAN
                            } else {
                                f64::from(i)
                            }
                        })
                        .collect(),
                    _ => return None,
                };
                Some(
                    secs.iter()
                        .map(|&s| {
                            if s.is_nan() {
                                Value::Null
                            } else {
                                Value::from(datetime_text((s * 1e6).round() as i64))
                            }
                        })
                        .collect(),
                )
            }
            _ => None,
        }
    }

    fn scalar(&mut self, x: &Robj, ft: &FieldType, path: &str) -> Value {
        let allowed: &[&str] = match ft {
            FieldType::Date => &["class"],
            FieldType::Posixct => &["class", "tzone"],
            _ => &[],
        };
        if !self.plain(x, allowed, path) {
            return Value::Null;
        }
        if x.len() != 1 {
            return self.err(
                "r_length",
                path,
                format!("expected one value, found {}", x.len()),
            );
        }
        match self.atomic(x, ft, path) {
            Some(mut v) => {
                let v = v.remove(0);
                if v.is_null() {
                    return self.err(
                        "na_value",
                        path,
                        "NA is not a value of this field; an absent value is declared option()"
                            .to_string(),
                    );
                }
                v
            }
            None => Value::Null,
        }
    }

    pub(crate) fn value(&mut self, x: &Robj, ft: &FieldType, path: &str) -> Value {
        match ft {
            FieldType::Option { inner, .. } => {
                if x.is_null() {
                    Value::Null
                } else {
                    self.value(x, inner, path)
                }
            }
            _ if x.is_null() => Value::Null,
            FieldType::Logical
            | FieldType::Integer(_)
            | FieldType::Double(_)
            | FieldType::Character(_)
            | FieldType::Date
            | FieldType::Posixct => self.scalar(x, ft, path),
            FieldType::Raw => match x.as_raw_slice() {
                Some(b) if self.plain(x, &[], path) => {
                    rtemis_r8::derive::Encode::encode(&rtemis_r8::derive::Bytes(b.to_vec()))
                        .unwrap_or(Value::Null)
                }
                Some(_) => Value::Null,
                None => self.err("r_type", path, "expected a raw vector".to_string()),
            },
            FieldType::Factor { levels, ordered } => {
                self.factor(x, levels.as_deref(), *ordered, path)
            }
            FieldType::Vec { element, .. } => {
                let (inner, _) = unwrap_option(element);
                if is_scalar(inner) {
                    let allowed: &[&str] = match inner {
                        FieldType::Date => &["class"],
                        FieldType::Posixct => &["class", "tzone"],
                        _ => &[],
                    };
                    if !self.plain(x, allowed, path) {
                        return Value::Null;
                    }
                    match self.atomic(x, element, path) {
                        Some(items) => Value::Array(items),
                        None => Value::Null,
                    }
                } else {
                    match x.as_list() {
                        Some(list) => {
                            if x.names().is_some() && !self.plain(x, &[], path) {
                                return Value::Null;
                            }
                            Value::Array(
                                list.values()
                                    .enumerate()
                                    .map(|(i, e)| self.value(&e, element, &format!("{path}/{i}")))
                                    .collect(),
                            )
                        }
                        None => self.err("r_type", path, "expected an unnamed list".to_string()),
                    }
                }
            }
            FieldType::Hashmap { value, .. } | FieldType::OrderedMap { value, .. } => {
                let ordered = matches!(ft, FieldType::OrderedMap { .. });
                let names: Vec<String> = x
                    .names()
                    .map(|n| n.map(str::to_string).collect())
                    .unwrap_or_default();
                if names.len() != x.len() {
                    return self.err(
                        "r_type",
                        path,
                        "a map is a named list or a named vector".to_string(),
                    );
                }
                let items: Vec<Value> = match x.as_list() {
                    Some(list) => list
                        .values()
                        .zip(&names)
                        .map(|(e, k)| self.value(&e, value, &format!("{path}/{}", esc(k))))
                        .collect(),
                    None => {
                        let (inner, _) = unwrap_option(value);
                        if !is_scalar(inner) {
                            return self.err(
                                "r_type",
                                path,
                                "a map of non-scalar values is a named list".to_string(),
                            );
                        }
                        self.atomic(x, value, path).unwrap_or_default()
                    }
                };
                if ordered {
                    Value::Array(
                        names
                            .iter()
                            .zip(items)
                            .map(|(k, v)| serde_json::json!({"name": k, "value": v}))
                            .collect(),
                    )
                } else {
                    Value::Object(names.into_iter().zip(items).collect())
                }
            }
            FieldType::Json => self.json(x, path),
            FieldType::R { .. } => {
                self.runtime.push(path.to_string());
                Value::Null
            }
            FieldType::Matrix { element, .. } | FieldType::Array { element, .. } => {
                self.dense(x, element, path)
            }
            FieldType::SparseMatrix { .. } => self.sparse(x, path),
            FieldType::Table { columns, open, .. } => self.table(x, columns, *open, path),
            FieldType::MatrixOf { of } => {
                let element = self
                    .struct_fields(of)
                    .first()
                    .map(|f| f.ty.clone())
                    .unwrap_or(FieldType::Logical);
                self.dense(x, &element, path)
            }
            FieldType::TableOf { of } => {
                let fields = self.struct_fields(of);
                self.table(x, &fields, false, path)
            }
            FieldType::Named { name } => self.named(x, name, path),
            FieldType::Dyn { .. } => self.dynamic(x, path),
        }
    }

    fn struct_fields(&self, of: &QualifiedName) -> Vec<FieldDef> {
        match self.lookup.find(of) {
            Some(TypeDef::Struct(s)) => s.fields.clone(),
            _ => Vec::new(),
        }
    }

    fn factor(
        &mut self,
        x: &Robj,
        levels: Option<&[Box<str>]>,
        ordered: bool,
        path: &str,
    ) -> Value {
        if !x.inherits("factor") {
            return self.err("r_type", path, "expected a factor".to_string());
        }
        if !self.plain(x, &["levels", "class"], path) {
            return Value::Null;
        }
        if x.inherits("ordered") != ordered {
            return self.err(
                "r_type",
                path,
                if ordered {
                    "expected an ordered factor".into()
                } else {
                    "expected an unordered factor".into()
                },
            );
        }
        let lv: Vec<Option<String>> = x
            .get_attrib("levels")
            .and_then(|l| {
                l.as_str_iter().map(|it| {
                    it.map(|s| if s.is_na() { None } else { Some(s.to_string()) })
                        .collect()
                })
            })
            .unwrap_or_default();
        if lv.iter().any(Option::is_none) {
            return self.err("r_factor", path, "a factor has no NA level".to_string());
        }
        let lv: Vec<String> = lv.into_iter().map(Option::unwrap).collect();
        let codes = x.as_integer_slice().unwrap_or_default();
        let k = lv.len();
        // A code outside 1..=k has no JSON form; the encoder reports it at
        // the path where the validator of the native form reports it.
        let cell_base = match levels {
            Some(_) => path.to_string(),
            None => format!("{path}/codes"),
        };
        let mut cells = Vec::with_capacity(codes.len());
        for (i, &c) in codes.iter().enumerate() {
            cells.push(if c == i32::MIN {
                Value::Null
            } else if (c as u32).wrapping_sub(1) < k as u32 {
                match levels {
                    Some(_) => Value::from(lv[(c - 1) as usize].as_str()),
                    None => Value::from(c),
                }
            } else {
                self.err(
                    "invalid_code",
                    &format!("{cell_base}/{i}"),
                    format!("a code is an integer from 1 to {k}"),
                )
            });
        }
        match levels {
            Some(_) => Value::Array(cells),
            None => serde_json::json!({ "levels": lv, "codes": cells }),
        }
    }

    fn dense(&mut self, x: &Robj, element: &FieldType, path: &str) -> Value {
        let Some(dim) = x.get_attrib("dim") else {
            return self.err("r_type", path, "expected a matrix or an array".to_string());
        };
        if !self.plain(x, &["dim", "dimnames"], path) {
            return Value::Null;
        }
        let shape: Vec<i64> = dim
            .as_integer_slice()
            .unwrap_or_default()
            .iter()
            .map(|&d| i64::from(d))
            .collect();
        let data = self.atomic(x, element, path).unwrap_or_default();
        let mut m = Map::new();
        m.insert("shape".into(), Value::from(shape));
        m.insert("data".into(), Value::Array(data));
        if let Some(dn) = x.get_attrib("dimnames").and_then(|d| d.as_list()) {
            let names: Vec<Value> = dn
                .values()
                .map(|n| match n.as_str_iter() {
                    Some(it) => Value::Array(it.map(Value::from).collect()),
                    None => Value::Null,
                })
                .collect();
            m.insert("dimnames".into(), Value::Array(names));
        }
        Value::Object(m)
    }

    fn sparse(&mut self, x: &Robj, path: &str) -> Value {
        if !x.inherits("dgCMatrix") {
            return self.err(
                "r_type",
                path,
                "expected a dgCMatrix (package Matrix)".to_string(),
            );
        }
        let ints = |k: &str| -> Vec<i64> {
            x.get_attrib(k)
                .and_then(|a| {
                    a.as_integer_slice()
                        .map(|s| s.iter().map(|&i| i64::from(i)).collect())
                })
                .unwrap_or_default()
        };
        let data: Vec<Value> = x
            .get_attrib("x")
            .and_then(|a| {
                a.as_real_slice()
                    .map(|s| s.iter().map(|&d| number(d)).collect())
            })
            .unwrap_or_default();
        serde_json::json!({"shape": ints("Dim"), "indptr": ints("p"), "indices": ints("i"), "data": data})
    }

    fn table(&mut self, x: &Robj, columns: &[FieldDef], open: bool, path: &str) -> Value {
        if !x.inherits("data.frame") {
            return self.err("r_type", path, "expected a data.frame".to_string());
        }
        // Automatic row names are the compact form c(NA, -n) or c(NA, n).
        let n = match x.get_attrib("row.names") {
            Some(rn) => match rn.as_integer_slice() {
                Some([na, k]) if *na == i32::MIN => i64::from(k.abs()),
                Some(s) if s.iter().enumerate().all(|(i, &v)| v == i as i32 + 1) => s.len() as i64,
                _ => {
                    return self.err(
                        "r_row_names",
                        path,
                        "a table has automatic row names; r8_conform() resets them".to_string(),
                    );
                }
            },
            None => 0,
        };
        let list = x.as_list().unwrap();
        let mut m = Map::new();
        m.insert("$length".into(), Value::from(n));
        let mut extra_types = Map::new();
        for (name, col) in list.iter() {
            let p = format!("{path}/{}", esc(name));
            let declared = columns.iter().find(|c| c.name.as_str() == name);
            let ft = match declared {
                Some(c) => c.ty.clone(),
                None if open => match infer_column(&col) {
                    Some(t) => {
                        extra_types.insert(name.to_string(), serde_json::to_value(&t).unwrap());
                        t
                    }
                    None => {
                        self.err("r_type", &p, format!("column `{name}` holds no column type: logical, integer, double, character, factor, Date, or POSIXct"));
                        continue;
                    }
                },
                None => {
                    self.err(
                        "unknown_column",
                        &p,
                        format!("`{name}` is not a column of this table"),
                    );
                    continue;
                }
            };
            let (inner, _) = unwrap_option(&ft);
            let v = if let FieldType::Factor { levels, ordered } = inner {
                self.factor(&col, levels.as_deref(), *ordered, &p)
            } else {
                Value::Array(self.atomic(&col, &ft, &p).unwrap_or_default())
            };
            m.insert(name.to_string(), v);
        }
        if !extra_types.is_empty() {
            m.insert("$columns".into(), Value::Object(extra_types));
        }
        Value::Object(m)
    }

    fn json(&mut self, x: &Robj, path: &str) -> Value {
        match x.rtype() {
            Rtype::Null => Value::Null,
            Rtype::List => {
                let list = x.as_list().unwrap();
                let names: Option<Vec<String>> = x.names().map(|n| n.map(str::to_string).collect());
                match names {
                    Some(n) => Value::Object(
                        list.values()
                            .zip(n)
                            .map(|(e, k)| {
                                let v = self.json(&e, &format!("{path}/{}", esc(&k)));
                                (k, v)
                            })
                            .collect(),
                    ),
                    None => Value::Array(
                        list.values()
                            .enumerate()
                            .map(|(i, e)| self.json(&e, &format!("{path}/{i}")))
                            .collect(),
                    ),
                }
            }
            Rtype::Logicals | Rtype::Integers | Rtype::Doubles | Rtype::Strings => {
                if x.len() != 1 {
                    return self.err(
                        "r_json",
                        path,
                        "a JSON array in R is an unnamed list; an atomic value has length one"
                            .to_string(),
                    );
                }
                let v = match x.rtype() {
                    Rtype::Logicals => x
                        .as_logical_slice()
                        .filter(|s| !s[0].is_na())
                        .map(|s| Value::Bool(s[0].is_true())),
                    Rtype::Integers => x
                        .as_integer_slice()
                        .filter(|s| s[0] != i32::MIN)
                        .map(|s| Value::from(f64::from(s[0]))),
                    Rtype::Doubles => x
                        .as_real_slice()
                        .filter(|s| s[0].is_finite())
                        .map(|s| number(s[0])),
                    _ => x
                        .as_str_iter()
                        .and_then(|mut it| it.next())
                        .filter(|s| !s.is_na())
                        .map(Value::from),
                };
                v.unwrap_or_else(|| {
                    self.err(
                        "r_json",
                        path,
                        "NA and non-finite numbers have no JSON form".to_string(),
                    )
                })
            }
            _ => self.err(
                "r_json",
                path,
                "a JSON value in R is a list, a length-one atomic vector, or NULL".to_string(),
            ),
        }
    }

    fn named(&mut self, x: &Robj, name: &QualifiedName, path: &str) -> Value {
        let qname = name.to_string();
        if !x.inherits("r8_value") || !x.inherits(&qname) {
            return self.err("r_type", path, format!("expected a value of `{qname}`"));
        }
        let Some(def) = self.lookup.find(name).cloned() else {
            return self.err(
                "unresolved_type",
                path,
                format!("`{qname}` is not registered"),
            );
        };
        let list = x.as_list();
        let get = |n: &str| -> Robj {
            list.as_ref()
                .and_then(|l| l.iter().find(|(k, _)| *k == n).map(|(_, v)| v))
                .unwrap_or(().into())
        };
        let mut m = Map::new();
        match def {
            TypeDef::Struct(s) => {
                for f in &s.fields {
                    m.insert(
                        f.name.to_string(),
                        self.value(
                            &get(f.name.as_str()),
                            &f.ty,
                            &format!("{path}/{}", esc(f.name.as_str())),
                        ),
                    );
                }
                Value::Object(m)
            }
            TypeDef::Enum(e) => {
                let variant: String = x
                    .get_attrib("r8_variant")
                    .and_then(|v| v.as_str().map(str::to_string))
                    .unwrap_or_default();
                let unit_only = e
                    .variants
                    .iter()
                    .all(|v| matches!(v.shape, VariantShape::Unit));
                if unit_only {
                    return Value::from(variant);
                }
                let fields: Vec<FieldDef> = match e
                    .variants
                    .iter()
                    .find(|v| v.name.as_str() == variant)
                    .map(|v| &v.shape)
                {
                    Some(VariantShape::Struct { fields, .. }) => fields.clone(),
                    Some(VariantShape::Newtype { of }) => self.struct_fields(of),
                    _ => Vec::new(),
                };
                for f in &fields {
                    m.insert(
                        f.name.to_string(),
                        self.value(
                            &get(f.name.as_str()),
                            &f.ty,
                            &format!("{path}/{}", esc(f.name.as_str())),
                        ),
                    );
                }
                m.insert(e.tag.to_string(), Value::from(variant));
                Value::Object(m)
            }
        }
    }

    fn dynamic(&mut self, x: &Robj, path: &str) -> Value {
        let ty = x
            .get_attrib("r8_type")
            .and_then(|t| t.as_str().map(str::to_string));
        match ty.and_then(|t| QualifiedName::parse(&t).ok()) {
            Some(q) => {
                let mut v = self.named(x, &q, path);
                if let Value::Object(o) = &mut v {
                    o.insert("$type".into(), Value::from(q.to_string()));
                }
                v
            }
            None => self.err(
                "r_type",
                path,
                "a dyn() field holds an R8 value".to_string(),
            ),
        }
    }
}

fn unwrap_option(ft: &FieldType) -> (&FieldType, bool) {
    match ft {
        FieldType::Option { inner, .. } => (inner.as_ref(), true),
        other => (other, false),
    }
}

fn is_scalar(ft: &FieldType) -> bool {
    matches!(
        ft,
        FieldType::Logical
            | FieldType::Integer(_)
            | FieldType::Double(_)
            | FieldType::Character(_)
            | FieldType::Date
            | FieldType::Posixct
    )
}

/// The column type of an extra column of an open table, from its R class;
/// cells may be missing, so the type is `option()` of the class's type.
fn infer_column(col: &Robj) -> Option<FieldType> {
    let base = if col.inherits("factor") {
        FieldType::Factor {
            levels: None,
            ordered: col.inherits("ordered"),
        }
    } else if col.inherits("Date") {
        FieldType::Date
    } else if col.inherits("POSIXct") {
        FieldType::Posixct
    } else {
        match col.rtype() {
            Rtype::Logicals => FieldType::Logical,
            Rtype::Integers => FieldType::Integer(Default::default()),
            Rtype::Doubles => FieldType::Double(Default::default()),
            Rtype::Strings => FieldType::Character(Default::default()),
            _ => return None,
        }
    };
    Some(FieldType::Option {
        inner: Box::new(base),
        resolve: Default::default(),
    })
}

// ------------------------------------------------------------------ decode

pub(crate) struct Decoder<'a> {
    pub(crate) lookup: &'a dyn TypeLookup,
}

fn na_logical() -> Rbool {
    Rbool::na()
}

impl Decoder<'_> {
    /// An atomic vector of a scalar type from JSON values, `null` as `NA`.
    fn atomic(&self, items: &[Value], element: &FieldType) -> Result<Robj> {
        let (inner, _) = unwrap_option(element);
        Ok(match inner {
            FieldType::Logical => Logicals::from_values(
                items
                    .iter()
                    .map(|v| v.as_bool().map(Rbool::from).unwrap_or_else(na_logical)),
            )
            .into(),
            FieldType::Integer(_) => Integers::from_values(items.iter().map(|v| {
                v.as_f64()
                    .map(|d| Rint::from(d as i32))
                    .unwrap_or_else(Rint::na)
            }))
            .into(),
            FieldType::Double(_) => Doubles::from_values(items.iter().map(|v| match v {
                Value::Null => Rfloat::na(),
                other => Rfloat::from(rtemis_r8::rules::number_value(other).unwrap_or(f64::NAN)),
            }))
            .into(),
            FieldType::Character(_) => {
                Strings::from_values(items.iter().map(|v| match v.as_str() {
                    Some(s) => Rstr::from(s),
                    None => Rstr::na(),
                }))
                .into()
            }
            FieldType::Date => {
                let mut r: Robj = Doubles::from_values(items.iter().map(|v| match v.as_str() {
                    Some(s) => Rfloat::from(date_days(s) as f64),
                    None => Rfloat::na(),
                }))
                .into();
                r.set_class(["Date"])?;
                r
            }
            FieldType::Posixct => {
                let mut r: Robj = Doubles::from_values(items.iter().map(|v| match v.as_str() {
                    Some(s) => Rfloat::from(datetime_micros(s) as f64 / 1e6),
                    None => Rfloat::na(),
                }))
                .into();
                r.set_class(["POSIXct", "POSIXt"])?;
                r.set_attrib("tzone", "UTC")?;
                r
            }
            _ => return Err(fail("not a scalar type")),
        })
    }

    fn value(&self, v: &Value, ft: &FieldType) -> Result<Robj> {
        match ft {
            FieldType::Option { inner, .. } => {
                if v.is_null() {
                    Ok(().into())
                } else {
                    self.value(v, inner)
                }
            }
            _ if v.is_null() => Ok(().into()),
            FieldType::Logical
            | FieldType::Integer(_)
            | FieldType::Double(_)
            | FieldType::Character(_)
            | FieldType::Date
            | FieldType::Posixct => self.atomic(std::slice::from_ref(v), ft),
            FieldType::Raw => {
                let b = <rtemis_r8::derive::Bytes as rtemis_r8::derive::Encode>::decode(v)
                    .map_err(fail)?;
                Ok(Raw::from_bytes(&b.0).into())
            }
            FieldType::Factor { levels, ordered } => self.factor(v, levels.as_deref(), *ordered),
            FieldType::Vec { element, .. } => {
                let items = v.as_array().cloned().unwrap_or_default();
                let (inner, _) = unwrap_option(element);
                if is_scalar(inner) {
                    self.atomic(&items, element)
                } else {
                    let values = items
                        .iter()
                        .map(|e| self.value(e, element))
                        .collect::<Result<Vec<_>>>()?;
                    Ok(List::from_values(values).into())
                }
            }
            FieldType::Hashmap { value, .. } | FieldType::OrderedMap { value, .. } => {
                let (keys, items): (Vec<String>, Vec<Value>) = match v {
                    Value::Object(m) => {
                        let mut pairs: Vec<(&String, &Value)> = m.iter().collect();
                        pairs.sort_by(|a, b| a.0.cmp(b.0));
                        pairs
                            .into_iter()
                            .map(|(k, x)| (k.clone(), x.clone()))
                            .unzip()
                    }
                    Value::Array(pairs) => pairs
                        .iter()
                        .map(|p| {
                            (
                                p["name"].as_str().unwrap_or_default().to_string(),
                                p["value"].clone(),
                            )
                        })
                        .unzip(),
                    _ => (Vec::new(), Vec::new()),
                };
                let (inner, _) = unwrap_option(value);
                let mut r = if is_scalar(inner) && matches!(ft, FieldType::Hashmap { .. }) {
                    self.atomic(&items, value)?
                } else {
                    let values = items
                        .iter()
                        .map(|e| self.value(e, value))
                        .collect::<Result<Vec<_>>>()?;
                    List::from_values(values).into()
                };
                r.set_names(keys)?;
                Ok(r)
            }
            FieldType::Json => self.json(v),
            FieldType::R { .. } => Ok(().into()),
            FieldType::Matrix { element, .. } | FieldType::Array { element, .. } => {
                self.dense(v, element)
            }
            FieldType::MatrixOf { of } => {
                let element = match self.lookup.find(of) {
                    Some(TypeDef::Struct(s)) => s
                        .fields
                        .first()
                        .map(|f| f.ty.clone())
                        .unwrap_or(FieldType::Logical),
                    _ => FieldType::Logical,
                };
                self.dense(v, &element)
            }
            FieldType::SparseMatrix { .. } => self.sparse(v),
            FieldType::Table { columns, .. } => self.table(v, columns),
            FieldType::TableOf { of } => {
                let fields = match self.lookup.find(of) {
                    Some(TypeDef::Struct(s)) => s.fields.clone(),
                    _ => Vec::new(),
                };
                self.table(v, &fields)
            }
            FieldType::Named { name } => self.named(v, name, None),
            FieldType::Dyn { r#trait } => {
                let (ty, member) = self.lookup.implementor(r#trait, v).map_err(fail)?;
                let mut inner = v.clone();
                if let Some(o) = inner.as_object_mut() {
                    o.remove(&member);
                }
                self.named(&inner, &ty, None)
            }
        }
    }

    fn factor(&self, v: &Value, levels: Option<&[Box<str>]>, ordered: bool) -> Result<Robj> {
        let (lv, codes): (Vec<String>, Vec<Rint>) = match (v, levels) {
            (Value::Object(o), _) => (
                o["levels"]
                    .as_array()
                    .unwrap()
                    .iter()
                    .map(|l| l.as_str().unwrap().to_string())
                    .collect(),
                o["codes"]
                    .as_array()
                    .unwrap()
                    .iter()
                    .map(|c| {
                        c.as_i64()
                            .map(|k| Rint::from(k as i32))
                            .unwrap_or_else(Rint::na)
                    })
                    .collect(),
            ),
            (Value::Array(items), Some(levels)) => (
                levels.iter().map(|l| l.to_string()).collect(),
                items
                    .iter()
                    .map(|x| {
                        match x
                            .as_str()
                            .and_then(|s| levels.iter().position(|l| &**l == s))
                        {
                            Some(i) => Rint::from(i as i32 + 1),
                            None => Rint::na(),
                        }
                    })
                    .collect(),
            ),
            _ => return Err(fail("a factor's JSON form")),
        };
        let mut r: Robj = Integers::from_values(codes).into();
        r.set_attrib("levels", lv)?;
        if ordered {
            r.set_class(["ordered", "factor"])?;
        } else {
            r.set_class(["factor"])?;
        }
        Ok(r)
    }

    fn dense(&self, v: &Value, element: &FieldType) -> Result<Robj> {
        let data = v["data"].as_array().cloned().unwrap_or_default();
        let mut r = self.atomic(&data, element)?;
        let shape: Vec<i32> = v["shape"]
            .as_array()
            .unwrap()
            .iter()
            .map(|d| d.as_i64().unwrap() as i32)
            .collect();
        r.set_attrib("dim", shape)?;
        if let Some(dn) = v.get("dimnames").and_then(Value::as_array) {
            let names: Vec<Robj> = dn
                .iter()
                .map(|d| match d.as_array() {
                    Some(a) => Strings::from_values(
                        a.iter().map(|s| Rstr::from(s.as_str().unwrap_or_default())),
                    )
                    .into(),
                    None => Robj::from(()),
                })
                .collect();
            r.set_attrib("dimnames", List::from_values(names))?;
        }
        Ok(r)
    }

    fn sparse(&self, v: &Value) -> Result<Robj> {
        let ints = |k: &str| -> Robj {
            Integers::from_values(
                v[k].as_array()
                    .unwrap()
                    .iter()
                    .map(|x| Rint::from(x.as_i64().unwrap() as i32)),
            )
            .into()
        };
        let x: Robj = Doubles::from_values(
            v["data"]
                .as_array()
                .unwrap()
                .iter()
                .map(|d| Rfloat::from(d.as_f64().unwrap_or(f64::NAN))),
        )
        .into();
        let f = R!("R8:::new_dgc")?;
        f.as_function()
            .ok_or_else(|| fail("R8:::new_dgc"))?
            .call(pairlist!(ints("indices"), ints("indptr"), x, ints("shape")))
    }

    fn table(&self, v: &Value, columns: &[FieldDef]) -> Result<Robj> {
        let m = v.as_object().cloned().unwrap_or_default();
        let n = m.get("$length").and_then(Value::as_i64).unwrap_or(0) as i32;
        let extra = m
            .get("$columns")
            .and_then(Value::as_object)
            .cloned()
            .unwrap_or_default();
        let mut names = Vec::new();
        let mut cols = Vec::new();
        for (k, cells) in m.iter().filter(|(k, _)| !k.starts_with('$')) {
            let ft: FieldType = match columns.iter().find(|c| c.name.as_str() == k) {
                Some(c) => c.ty.clone(),
                None => serde_json::from_value(extra.get(k).cloned().unwrap_or(Value::Null))
                    .map_err(|e| fail(e.to_string()))?,
            };
            let (inner, _) = unwrap_option(&ft);
            let col = match inner {
                FieldType::Factor { levels, ordered } => {
                    self.factor(cells, levels.as_deref(), *ordered)?
                }
                _ => self.atomic(cells.as_array().map(Vec::as_slice).unwrap_or_default(), &ft)?,
            };
            names.push(k.clone());
            cols.push(col);
        }
        let mut r: Robj = List::from_values(cols).into();
        r.set_names(names)?;
        r.set_attrib("row.names", vec![Rint::na(), Rint::from(-n)])?;
        r.set_class(["data.frame"])?;
        Ok(r)
    }

    fn json(&self, v: &Value) -> Result<Robj> {
        Ok(match v {
            Value::Null => ().into(),
            Value::Bool(b) => (*b).into(),
            Value::Number(n) => n.as_f64().unwrap_or(f64::NAN).into(),
            Value::String(s) => s.as_str().into(),
            Value::Array(a) => {
                List::from_values(a.iter().map(|x| self.json(x)).collect::<Result<Vec<_>>>()?)
                    .into()
            }
            Value::Object(o) => {
                let mut r: Robj = List::from_values(
                    o.values()
                        .map(|x| self.json(x))
                        .collect::<Result<Vec<_>>>()?,
                )
                .into();
                r.set_names(o.keys().cloned().collect::<Vec<_>>())?;
                r
            }
        })
    }

    /// An R8 value of a named type; `original` supplies runtime-only fields.
    pub(crate) fn named(
        &self,
        v: &Value,
        name: &QualifiedName,
        original: Option<&Robj>,
    ) -> Result<Robj> {
        let def = self
            .lookup
            .find(name)
            .cloned()
            .ok_or_else(|| fail(format!("`{name}` is not registered")))?;
        let carry = |f: &FieldDef| -> Option<Robj> {
            if !matches!(unwrap_option(&f.ty).0, FieldType::R { .. }) {
                return None;
            }
            original.and_then(|o| o.as_list()).and_then(|l| {
                l.iter()
                    .find(|(k, _)| *k == f.name.as_str())
                    .map(|(_, x)| x)
            })
        };
        let fields_of = |fields: &[FieldDef]| -> Result<(Vec<String>, Vec<Robj>)> {
            let mut names = Vec::new();
            let mut values = Vec::new();
            for f in fields {
                names.push(f.name.to_string());
                values.push(match carry(f) {
                    Some(x) => x,
                    None => self.value(v.get(f.name.as_str()).unwrap_or(&Value::Null), &f.ty)?,
                });
            }
            Ok((names, values))
        };
        let (names, values, variant, version) = match &def {
            TypeDef::Struct(s) => {
                let (n, vals) = fields_of(&s.fields)?;
                (n, vals, None, s.version)
            }
            TypeDef::Enum(e) => {
                let unit_only = e
                    .variants
                    .iter()
                    .all(|x| matches!(x.shape, VariantShape::Unit));
                let variant = if unit_only {
                    v.as_str().unwrap_or_default().to_string()
                } else {
                    v.get(e.tag.as_str())
                        .and_then(Value::as_str)
                        .unwrap_or_default()
                        .to_string()
                };
                let fields: Vec<FieldDef> = match e
                    .variants
                    .iter()
                    .find(|x| x.name.as_str() == variant)
                    .map(|x| &x.shape)
                {
                    Some(VariantShape::Struct { fields, .. }) => fields.clone(),
                    Some(VariantShape::Newtype { of }) => match self.lookup.find(of) {
                        Some(TypeDef::Struct(s)) => s.fields.clone(),
                        _ => Vec::new(),
                    },
                    _ => Vec::new(),
                };
                let (n, vals) = fields_of(&fields)?;
                (n, vals, Some(variant), e.version)
            }
        };
        let mut r: Robj = List::from_values(values).into();
        r.set_names(names)?;
        let qname = name.to_string();
        r.set_class([qname.as_str(), "r8_value"])?;
        r.set_attrib("r8_type", qname.as_str())?;
        r.set_attrib("r8_version", version as i32)?;
        if let Some(var) = variant {
            r.set_attrib("r8_variant", var)?;
        }
        Ok(r)
    }
}

// ------------------------------------------------------------- entry points

/// The JSON form of an R object of the field type `ft`, for the rule
/// evaluator and for export; `null` where the object has no JSON form.
pub(crate) fn encode_field(x: SEXP, ft: &FieldType, lookup: &dyn TypeLookup) -> Value {
    let robj = unsafe { Robj::from_sexp(x) };
    let mut enc = Encoder {
        lookup,
        errors: Vec::new(),
        runtime: Vec::new(),
    };
    enc.value(&robj, ft, "")
}

/// The stored R form of a field's value given in its JSON form, such as a
/// declared default.
pub(crate) fn decode_field(v: &Value, ft: &FieldType, lookup: &dyn TypeLookup) -> Result<Robj> {
    Decoder { lookup }.value(v, ft)
}

pub(crate) fn errors_list(errors: &[ValueError]) -> List {
    let code: Vec<String> = errors.iter().map(|e| e.code.to_string()).collect();
    let path: Vec<String> = errors.iter().map(|e| e.path.clone()).collect();
    let rule: Vec<String> = errors
        .iter()
        .map(|e| e.rule.as_ref().map(|r| r.to_string()).unwrap_or_default())
        .collect();
    let message: Vec<String> = errors.iter().map(|e| e.message.clone()).collect();
    list!(code = code, path = path, rule = rule, message = message)
}

/// The identity of an R8 value: the canonical text of its normalized form,
/// its content hash, and the paths of its runtime-only fields that hold a
/// value, written from the R objects in place.
///
/// @param x An R8 value.
/// @return List: `canonical`, `hash`, and `runtime`, or `errors`.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_value_identity(x: Robj) -> Result<List> {
    let xs = unsafe { x.get() };
    let (q, docs) = value_type(xs)?;
    let lookup: &dyn TypeLookup = &**docs;
    let v = crate::rview::RView::Whole(xs);
    let errors = crate::construct::with_cache(|cache| {
        rtemis_r8::validate::validate_access(v, &q, lookup, cache)
    });
    if !errors.is_empty() {
        return Ok(list!(errors = errors_list(&errors)));
    }
    let canonical = rtemis_r8::codec::canonical_access(v, &q, lookup);
    let hash = rtemis_r8::codec::content_hash_access(v, &q, lookup);
    let runtime = rtemis_r8::codec::runtime_paths(&q, v, lookup);
    Ok(list!(canonical = canonical, hash = hash, runtime = runtime))
}

/// The content hash of an R8 value, streamed from the R objects in place.
///
/// @param x An R8 value.
/// @return Character: The hash, or `list(errors = <problems>)`.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_value_hash(x: Robj) -> Result<Robj> {
    let xs = unsafe { x.get() };
    let (q, docs) = value_type(xs)?;
    let lookup: &dyn TypeLookup = &**docs;
    let v = crate::rview::RView::Whole(xs);
    let errors = crate::construct::with_cache(|cache| {
        rtemis_r8::validate::validate_access(v, &q, lookup, cache)
    });
    if !errors.is_empty() {
        return Ok(list!(errors = errors_list(&errors)).into());
    }
    Ok(rtemis_r8::codec::content_hash_access(v, &q, lookup).into())
}

/// Whether two R8 values of one type are equal: the same normalized form,
/// and identical runtime-only fields, compared in place.
///
/// @param x,y R8 values.
/// @return Logical: One value.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_values_equal(x: Robj, y: Robj) -> Result<Robj> {
    let (xs, ys) = unsafe { (x.get(), y.get()) };
    let (q, docs) = value_type(xs)?;
    if crate::rview::attr_string(ys, crate::rview::sym_r8_type()).as_deref() != Some(&q.to_string())
    {
        return Ok(false.into());
    }
    let lookup: &dyn TypeLookup = &**docs;
    let eq = rtemis_r8::equal::equal_access(
        &q,
        crate::rview::RView::Whole(xs),
        crate::rview::RView::Whole(ys),
        lookup,
    );
    Ok(eq.into())
}

/// The qualified type of an R8 value, with the registry locked.
fn value_type(
    x: SEXP,
) -> Result<(
    QualifiedName,
    std::sync::MutexGuard<'static, std::sync::Arc<DocumentSet>>,
)> {
    let type_name = crate::rview::attr_string(x, crate::rview::sym_r8_type())
        .ok_or_else(|| fail("not an R8 value"))?;
    let q = QualifiedName::parse(&type_name).map_err(|e| fail(e.to_string()))?;
    let docs = registry()
        .lock()
        .map_err(|_| fail("the registry is unavailable"))?;
    Ok((q, docs))
}

/// Writes an R8 value as a document, from the R objects in place.
///
/// @param x An R8 value.
/// @return List: `list(json = <text>)` or `list(errors = <problems>)`.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_value_to_document(x: Robj) -> Result<List> {
    let xs = unsafe { x.get() };
    let (q, docs) = value_type(xs)?;
    let lookup: &dyn TypeLookup = &**docs;
    let v = crate::rview::RView::Whole(xs);
    let errors = crate::construct::with_cache(|cache| {
        rtemis_r8::validate::validate_access(v, &q, lookup, cache)
    });
    if !errors.is_empty() {
        return Ok(list!(errors = errors_list(&errors)));
    }
    Ok(list!(
        json = rtemis_r8::codec::document_access(v, &q, lookup)
    ))
}

/// Reads a document of a type into an R8 value.
///
/// @param text Character: The document.
/// @param type_name Character: The qualified type name.
/// @return List: `list(value = <R8 value>)` or `list(errors = <problems>)`.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_value_from_document(text: &str, type_name: &str) -> Result<List> {
    let q = QualifiedName::parse(type_name).map_err(|e| fail(e.to_string()))?;
    let docs = crate::declare::snapshot()?;
    let v = match crate::construct::with_cache(|cache| {
        rtemis_r8::codec::read_document_cached(text, &q, &*docs, cache)
    }) {
        Ok(v) => v,
        Err(errors) => return Ok(list!(errors = errors_list(&errors))),
    };
    let lookup: &dyn TypeLookup = &*docs;
    // A Rust-native struct or enum: the codec's `decode` builds the state
    // from the normalized document, or else `from_fields` (a struct) or
    // `from_variant` (an enum) from the decoded fields.
    if let (Some(def), Some(vt)) = (lookup.find(&q), crate::exports::codec(type_name)) {
        let document =
            rtemis_r8::codec::document_access(rtemis_r8::access::Json::new(&v), &q, lookup);
        if let Some(state) = crate::native::decode(vt, &document) {
            let state = state.map_err(fail)?;
            let variant = crate::native::state_variant(vt, state);
            let Some((names, version)) =
                crate::native::value_fields(def, variant.as_deref(), lookup)
            else {
                if let Some(d) = unsafe { (*vt).drop } {
                    unsafe { d(state) };
                }
                return Err(fail(format!(
                    "the codec of `{q}` decoded a state of the undeclared variant `{}` [codec_variant]",
                    variant.unwrap_or_default()
                )));
            };
            let value = unsafe {
                let like =
                    crate::native::template(&q.to_string(), &names, version, variant.as_deref());
                let r = Robj::from_sexp(crate::native::wrap(vt, state, like));
                extendr_ffi::Rf_unprotect(1);
                r
            };
            return Ok(list!(value = value));
        }
        let fields = Decoder { lookup: &*docs }.named(&v, &q, None)?;
        let fs = unsafe { fields.get() };
        let state = match def {
            TypeDef::Struct(_) => crate::native::from_fields(vt, fs),
            TypeDef::Enum(_) => {
                let variant = fields
                    .get_attrib("r8_variant")
                    .and_then(|a| a.as_str().map(str::to_string))
                    .unwrap_or_default();
                crate::native::from_variant(vt, &variant, fs)
            }
        }
        .map_err(fail)?;
        let value = unsafe { Robj::from_sexp(crate::native::wrap(vt, state, fs)) };
        return Ok(list!(value = value));
    }
    Ok(list!(
        value = Decoder { lookup: &*docs }.named(&v, &q, None)?
    ))
}

/// Resolves a struct value: given its fields, with runtime fields filled by
/// the caller, evaluates each absent computed field in dependency order over
/// the R objects in place, then constructs the value natively.
///
/// @param fields List: The fields, by name.
/// @param type_name Character: The qualified struct name.
/// @return List: `list(value = <R8 value>)` or `list(errors = <problems>)`.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_resolve_value(fields: Robj, type_name: &str) -> Result<List> {
    use extendr_ffi::{
        R_xlen_t, Rf_allocVector, Rf_protect, Rf_unprotect, SET_VECTOR_ELT, SEXPTYPE, VECTOR_ELT,
    };
    use rtemis_r8::ir::Resolve;
    let q = QualifiedName::parse(type_name).map_err(|e| fail(e.to_string()))?;
    let docs = registry()
        .lock()
        .map_err(|_| fail("the registry is unavailable"))?;
    let lookup: &dyn TypeLookup = &**docs;
    let Some(TypeDef::Struct(def)) = lookup.find(&q) else {
        return Err(fail(format!("`{type_name}` is not a registered struct")));
    };
    // A working copy of the fields list, which the computed values fill.
    let fs = unsafe { fields.get() };
    let n = crate::rview::xlen(fs);
    let work = unsafe {
        let w = Rf_protect(Rf_allocVector(SEXPTYPE::VECSXP, n as R_xlen_t));
        for i in 0..n {
            SET_VECTOR_ELT(w, i as R_xlen_t, VECTOR_ELT(fs, i as R_xlen_t));
        }
        let r = Robj::from_sexp(w);
        Rf_unprotect(1);
        r
    };
    let ws = unsafe { work.get() };
    unsafe {
        extendr_ffi::Rf_setAttrib(
            ws,
            crate::rview::sym_names(),
            crate::rview::attr(fs, crate::rview::sym_names()),
        );
    }
    let names: Vec<String> =
        crate::rview::strings(crate::rview::attr(fs, crate::rview::sym_names()))
            .into_iter()
            .map(|k| k.map(|s| s.into_owned()).unwrap_or_default())
            .collect();
    let computed = |f: &FieldDef| match &f.ty {
        FieldType::Option {
            resolve: Resolve::Computed { expr },
            ..
        } => Some(expr.clone()),
        _ => None,
    };
    let mut pending: Vec<&FieldDef> = def
        .fields
        .iter()
        .filter(|f| computed(f).is_some())
        .filter(|f| {
            crate::rview::list_member(ws, f.name.as_str()).is_none_or(crate::rview::is_null)
        })
        .collect();
    let mut errors = Vec::new();
    let mut kept: Vec<Robj> = Vec::new();
    while !pending.is_empty() {
        let pending_names: Vec<String> = pending.iter().map(|f| f.name.to_string()).collect();
        // A field is ready when no field it reads is still pending.
        let ready = pending.iter().position(|f| {
            computed(f).is_some_and(|expr| {
                rtemis_r8::rules::reads(&expr)
                    .iter()
                    .all(|r| !pending_names.contains(r) || r == f.name.as_str())
            })
        });
        let Some(k) = ready else { break };
        let f = pending.remove(k);
        let Some(expr) = computed(f) else { continue };
        let at = format!("/{}", esc(f.name.as_str()));
        match rtemis_r8::rules::evaluate_computed_access(
            &expr,
            &def.fields,
            lookup,
            f,
            crate::rview::RView::Draft(ws, ""),
        ) {
            Ok(Some(x)) => {
                let r = match x {
                    rtemis_r8::rules::Computed::Scalar(x) => {
                        crate::construct::scalar_to_r(&x, &f.ty)?
                    }
                    rtemis_r8::rules::Computed::Value(v) => v.robj()?,
                };
                match names.iter().position(|k| k == f.name.as_str()) {
                    Some(i) => unsafe {
                        SET_VECTOR_ELT(ws, i as R_xlen_t, r.get());
                    },
                    None => {
                        errors.push(ValueError {
                            code: "computed_error",
                            path: at,
                            rule: None,
                            message: format!("the fields list has no member `{}`", f.name),
                        });
                        continue;
                    }
                }
                kept.push(r);
            }
            Ok(None) => errors.push(ValueError {
                code: "computed_absent",
                path: at,
                rule: None,
                message: format!(
                    "computed field `{}` reads a field that is absent; resolve that field first",
                    f.name
                ),
            }),
            Err(e) => errors.push(ValueError {
                code: "computed_error",
                path: at,
                rule: None,
                message: e,
            }),
        }
    }
    if !errors.is_empty() {
        return Ok(list!(errors = errors_list(&errors)));
    }
    let problems = crate::construct::with_cache(|cache| {
        rtemis_r8::validate::validate_access(crate::rview::RView::Draft(ws, ""), &q, lookup, cache)
    });
    if !problems.is_empty() {
        return Ok(list!(errors = errors_list(&problems)));
    }
    let value = crate::construct::assemble(ws, &q, "", lookup)?;
    drop(kept);
    Ok(list!(value = value))
}

/// Validates an R8 value in place.
///
/// @param x An R8 value.
/// @return `NULL`, or `list(errors = <problems>)`.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_value_check(x: Robj) -> Result<Robj> {
    let xs = unsafe { x.get() };
    let (q, docs) = value_type(xs)?;
    let lookup: &dyn TypeLookup = &**docs;
    let errors = crate::construct::with_cache(|cache| {
        rtemis_r8::validate::validate_access(crate::rview::RView::Whole(xs), &q, lookup, cache)
    });
    if errors.is_empty() {
        Ok(().into())
    } else {
        Ok(list!(errors = errors_list(&errors)).into())
    }
}

extendr_module! {
    mod values;
    fn r8_resolve_value;
    fn r8_value_identity;
    fn r8_values_equal;
    fn r8_value_hash;
    fn r8_value_check;
    fn r8_value_to_document;
    fn r8_value_from_document;
}
