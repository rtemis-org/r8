//! An [`abi::Reader`] over a parsed JSON value: the reader R8 lends a
//! Rust-native codec for a document it has read and validated, so that the
//! codec builds its state from the value in place, with no JSON text and no
//! second tree. A [`abi::Node`] is a `*const serde_json::Value` into the
//! value, valid while the value lives.
//!
//! A null reads as R's `NA` in a logical, an integer, or a double
//! ([`abi::NA_INTEGER`], [`abi::NA_DOUBLE_BITS`]), and the strings `"NaN"`,
//! `"Infinity"`, and `"-Infinity"` read as their doubles, as R8 reads a
//! document into R values.
//! spec: R8/class-system#c-interface

use std::ffi::{CStr, c_char, c_void};

use serde_json::Value;

use crate::abi::{self, Error, Node, Reader, Status};

/// The reader over parsed JSON values.
pub fn json_reader() -> Reader {
    Reader {
        size: std::mem::size_of::<Reader>() as u32,
        context: std::ptr::null_mut(),
        kind: Some(kind),
        field: Some(field),
        item: Some(item),
        length: Some(length),
        logical: Some(logical),
        number: Some(number),
        string: Some(string),
        doubles: Some(doubles),
        integers: Some(integers),
        logicals: Some(logicals),
    }
}

/// The node of `v`.
pub fn node(v: &Value) -> Node {
    (v as *const Value).cast()
}

/// The value of a node, or `None` for an absent one.
///
/// # Safety
///
/// `n` is NULL or a node of a value that is alive.
unsafe fn value<'a>(n: Node) -> Option<&'a Value> {
    unsafe { n.cast::<Value>().as_ref() }
}

/// Fills `err`, when it is writable, with `code` and `message`, truncated
/// to fit.
fn fail(err: *mut Error, code: &str, message: &str) -> Status {
    if let Some(e) = unsafe { err.as_mut() } {
        let put = |dst: &mut [c_char], s: &str| {
            let b = s.as_bytes();
            let n = b.len().min(dst.len() - 1);
            for (d, &c) in dst.iter_mut().zip(&b[..n]) {
                *d = c as c_char;
            }
            dst[n] = 0;
        };
        put(&mut e.code, code);
        put(&mut e.message, message);
    }
    abi::ERROR
}

unsafe extern "C" fn kind(_: *mut c_void, n: Node) -> u32 {
    match unsafe { value(n) } {
        None => abi::NODE_ABSENT,
        Some(Value::Null) => abi::NODE_NULL,
        Some(Value::Bool(_)) => abi::NODE_LOGICAL,
        Some(Value::Number(_)) => abi::NODE_NUMBER,
        Some(Value::String(_)) => abi::NODE_STRING,
        Some(Value::Array(_)) => abi::NODE_ARRAY,
        Some(Value::Object(_)) => abi::NODE_OBJECT,
    }
}

unsafe extern "C" fn field(_: *mut c_void, n: Node, name: *const c_char) -> Node {
    let (Some(Value::Object(m)), false) = (unsafe { value(n) }, name.is_null()) else {
        return std::ptr::null();
    };
    let Ok(name) = unsafe { CStr::from_ptr(name) }.to_str() else {
        return std::ptr::null();
    };
    m.get(name).map_or(std::ptr::null(), node)
}

unsafe extern "C" fn item(_: *mut c_void, n: Node, i: usize) -> Node {
    match unsafe { value(n) } {
        Some(Value::Array(a)) => a.get(i).map_or(std::ptr::null(), node),
        _ => std::ptr::null(),
    }
}

unsafe extern "C" fn length(_: *mut c_void, n: Node) -> usize {
    match unsafe { value(n) } {
        Some(Value::Array(a)) => a.len(),
        Some(Value::Object(m)) => m.len(),
        _ => 0,
    }
}

fn logical_of(v: &Value) -> Option<i32> {
    match v {
        Value::Bool(b) => Some(i32::from(*b)),
        Value::Null => Some(abi::NA_INTEGER),
        _ => None,
    }
}

fn number_of(v: &Value) -> Option<f64> {
    match v {
        Value::Number(x) => x.as_f64(),
        Value::Null => Some(f64::from_bits(abi::NA_DOUBLE_BITS)),
        Value::String(s) => match s.as_str() {
            "NaN" => Some(f64::NAN),
            "Infinity" => Some(f64::INFINITY),
            "-Infinity" => Some(f64::NEG_INFINITY),
            _ => None,
        },
        _ => None,
    }
}

fn integer_of(v: &Value) -> Option<i32> {
    match v {
        Value::Null => Some(abi::NA_INTEGER),
        Value::Number(x) => x
            .as_i64()
            .and_then(|i| i32::try_from(i).ok())
            .filter(|&i| i != abi::NA_INTEGER),
        _ => None,
    }
}

unsafe extern "C" fn logical(_: *mut c_void, n: Node, out: *mut i32, err: *mut Error) -> Status {
    match unsafe { value(n) }.and_then(logical_of) {
        Some(b) => {
            unsafe { *out = b };
            abi::OK
        }
        None => fail(err, "not_logical", "the node is not a logical"),
    }
}

unsafe extern "C" fn number(_: *mut c_void, n: Node, out: *mut f64, err: *mut Error) -> Status {
    match unsafe { value(n) }.and_then(number_of) {
        Some(x) => {
            unsafe { *out = x };
            abi::OK
        }
        None => fail(err, "not_number", "the node is not a number"),
    }
}

unsafe extern "C" fn string(
    _: *mut c_void,
    n: Node,
    out: *mut *const c_char,
    len: *mut usize,
    err: *mut Error,
) -> Status {
    match unsafe { value(n) } {
        Some(Value::String(s)) => {
            unsafe {
                *out = s.as_ptr().cast();
                *len = s.len();
            }
            abi::OK
        }
        _ => fail(err, "not_string", "the node is not a string"),
    }
}

/// Reads items `start..start + count` of an array through `f` into `out`.
///
/// # Safety
///
/// `n` is NULL or a live node, and `out` holds `count` writable items.
unsafe fn block<T>(
    n: Node,
    start: usize,
    count: usize,
    out: *mut T,
    err: *mut Error,
    what: &str,
    f: fn(&Value) -> Option<T>,
) -> Status {
    let Some(Value::Array(a)) = (unsafe { value(n) }) else {
        return fail(err, "not_array", "the node is not an array");
    };
    let Some(items) = start.checked_add(count).and_then(|end| a.get(start..end)) else {
        return fail(
            err,
            "out_of_range",
            "the block extends past the array's end",
        );
    };
    let out = unsafe { std::slice::from_raw_parts_mut(out, count) };
    for (k, (o, v)) in out.iter_mut().zip(items).enumerate() {
        match f(v) {
            Some(x) => *o = x,
            None => {
                return fail(
                    err,
                    "not_element",
                    &format!("item {} is not {what}", start + k),
                );
            }
        }
    }
    abi::OK
}

unsafe extern "C" fn doubles(
    _: *mut c_void,
    n: Node,
    start: usize,
    count: usize,
    out: *mut f64,
    err: *mut Error,
) -> Status {
    unsafe { block(n, start, count, out, err, "a number", number_of) }
}

unsafe extern "C" fn integers(
    _: *mut c_void,
    n: Node,
    start: usize,
    count: usize,
    out: *mut i32,
    err: *mut Error,
) -> Status {
    unsafe { block(n, start, count, out, err, "a whole number", integer_of) }
}

unsafe extern "C" fn logicals(
    _: *mut c_void,
    n: Node,
    start: usize,
    count: usize,
    out: *mut i32,
    err: *mut Error,
) -> Status {
    unsafe { block(n, start, count, out, err, "a logical", logical_of) }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn reads_a_value_through_the_table() {
        let v: Value = serde_json::json!({"a": [1.5, null, "Infinity"], "n": [1, 2, null], "s": "x", "b": true});
        let r = json_reader();
        let root = node(&v);
        unsafe {
            let ctx = r.context;
            assert_eq!(r.kind.unwrap()(ctx, root), abi::NODE_OBJECT);
            let a = r.field.unwrap()(ctx, root, c"a".as_ptr());
            assert_eq!(r.length.unwrap()(ctx, a), 3);
            let mut out = [0.0; 3];
            assert_eq!(
                r.doubles.unwrap()(ctx, a, 0, 3, out.as_mut_ptr(), std::ptr::null_mut()),
                abi::OK
            );
            assert_eq!(out[0], 1.5);
            assert_eq!(out[1].to_bits(), abi::NA_DOUBLE_BITS);
            assert_eq!(out[2], f64::INFINITY);
            let n = r.field.unwrap()(ctx, root, c"n".as_ptr());
            let mut ints = [0; 3];
            assert_eq!(
                r.integers.unwrap()(ctx, n, 0, 3, ints.as_mut_ptr(), std::ptr::null_mut()),
                abi::OK
            );
            assert_eq!(ints, [1, 2, abi::NA_INTEGER]);
            assert_eq!(
                r.doubles.unwrap()(ctx, a, 2, 2, out.as_mut_ptr(), std::ptr::null_mut()),
                abi::ERROR
            );
            assert!(r.field.unwrap()(ctx, root, c"missing".as_ptr()).is_null());
            let s = r.field.unwrap()(ctx, root, c"s".as_ptr());
            let (mut p, mut len) = (std::ptr::null(), 0);
            assert_eq!(
                r.string.unwrap()(ctx, s, &mut p, &mut len, std::ptr::null_mut()),
                abi::OK
            );
            assert_eq!(std::slice::from_raw_parts(p.cast::<u8>(), len), b"x");
        }
    }
}
