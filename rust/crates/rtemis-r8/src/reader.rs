//! An [`abi::Reader`] over the compact tree of a document read
//! ([`crate::tree::Tree`]): the reader R8 lends a Rust-stored codec for a
//! document it has read and validated, so that the codec builds its state
//! from the value in place, with no JSON text and no second tree. A
//! [`abi::Node`] is a `*const Tree` into the value, valid while the value
//! lives; an item of a run of numbers read one at a time is a node the reader
//! keeps for the call ([`TreeReader`]), and a block read of a run copies the
//! run.
//!
//! A node of a value whose type the codec does not know reads as an R value,
//! which the host gives ([`HostValues`]).
//!
//! A null reads as R's `NA` in a logical, an integer, or a double
//! ([`abi::NA_INTEGER`], [`abi::NA_DOUBLE_BITS`]), and the strings `"NaN"`,
//! `"Infinity"`, and `"-Infinity"` read as their doubles, as R8 reads a
//! document into R values.
//! spec: R8/class-system#c-interface

use std::ffi::{CStr, c_char, c_void};

use std::cell::RefCell;

use crate::abi::{self, Error, Node, Reader, Sexp, Status};
use crate::ir::{FieldType, Number};
use crate::name::QualifiedName;
use crate::tree::Tree;

/// The values of the host that reads the tree.
pub trait HostValues {
    /// The host value of `node`, a valid value of the field type `ty`; an
    /// error is a code and a message. It raises no error of the host and does
    /// not panic.
    fn value(&self, node: &Tree, ty: &FieldType) -> Result<Sexp, (&'static str, String)>;
}

/// The state of a reader for one call: the nodes of the run items it has
/// lent, freed with it, and the host's values.
pub struct TreeReader<'a> {
    // Boxed: a lent node's address stays fixed while the vector grows.
    #[allow(clippy::vec_box)]
    items: RefCell<Vec<Box<Tree>>>,
    values: &'a dyn HostValues,
}

impl<'a> TreeReader<'a> {
    /// A reader with no lent items, whose host values are `values`.
    pub fn new(values: &'a dyn HostValues) -> Self {
        TreeReader {
            items: RefCell::default(),
            values,
        }
    }

    /// The function table of the reader, whose context is `self`; valid
    /// while `self` lives.
    pub fn table(&self) -> Reader {
        reader_table((self as *const TreeReader<'a>).cast_mut().cast())
    }
}

/// `null`, the node of a run's `NA_real_` item.
static NULL: Tree = Tree::Null;

fn reader_table(context: *mut c_void) -> Reader {
    Reader {
        size: std::mem::size_of::<Reader>() as u32,
        context,
        kind: Some(kind),
        member: Some(member),
        item: Some(item),
        length: Some(length),
        logical: Some(logical),
        number: Some(number),
        string: Some(string),
        doubles: Some(doubles),
        integers: Some(integers),
        logicals: Some(logicals),
        value: Some(host_value),
    }
}

/// The node of `t`.
pub fn node(t: &Tree) -> Node {
    (t as *const Tree).cast()
}

/// The tree of a node, or `None` for an absent one.
///
/// # Safety
///
/// `n` is NULL or a node of a tree that is alive.
unsafe fn value<'a>(n: Node) -> Option<&'a Tree> {
    unsafe { n.cast::<Tree>().as_ref() }
}

fn is_na(x: f64) -> bool {
    x.to_bits() == abi::NA_DOUBLE_BITS
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
        Some(Tree::Null) => abi::NODE_NULL,
        Some(Tree::Bool(_)) => abi::NODE_LOGICAL,
        Some(Tree::Num(_)) => abi::NODE_NUMBER,
        Some(Tree::Str(_)) => abi::NODE_STRING,
        Some(Tree::Array(_) | Tree::Doubles(_)) => abi::NODE_ARRAY,
        Some(Tree::Object(_)) => abi::NODE_OBJECT,
    }
}

unsafe extern "C" fn member(_: *mut c_void, n: Node, name: *const c_char) -> Node {
    let (Some(t), false) = (unsafe { value(n) }, name.is_null()) else {
        return std::ptr::null();
    };
    let Ok(name) = unsafe { CStr::from_ptr(name) }.to_str() else {
        return std::ptr::null();
    };
    t.get(name).map_or(std::ptr::null(), node)
}

unsafe extern "C" fn item(context: *mut c_void, n: Node, i: usize) -> Node {
    match unsafe { value(n) } {
        Some(Tree::Array(a)) => a.get(i).map_or(std::ptr::null(), node),
        Some(Tree::Doubles(d)) => match d.get(i) {
            None => std::ptr::null(),
            Some(&x) if is_na(x) => node(&NULL),
            Some(&x) => {
                let Some(reader) = (unsafe { context.cast::<TreeReader>().as_ref() }) else {
                    return std::ptr::null();
                };
                let lent = Box::new(Tree::Num(Number::of_f64(x)));
                let p = node(&lent);
                reader.items.borrow_mut().push(lent);
                p
            }
        },
        _ => std::ptr::null(),
    }
}

unsafe extern "C" fn length(_: *mut c_void, n: Node) -> usize {
    match unsafe { value(n) } {
        Some(Tree::Array(a)) => a.len(),
        Some(Tree::Doubles(d)) => d.len(),
        Some(Tree::Object(m)) => m.len(),
        _ => 0,
    }
}

fn logical_of(v: &Tree) -> Option<i32> {
    match v {
        Tree::Bool(b) => Some(i32::from(*b)),
        Tree::Null => Some(abi::NA_INTEGER),
        _ => None,
    }
}

fn number_of(v: &Tree) -> Option<f64> {
    match v {
        Tree::Num(x) => Some(x.as_f64()),
        Tree::Null => Some(f64::from_bits(abi::NA_DOUBLE_BITS)),
        Tree::Str(s) => match &**s {
            "NaN" => Some(f64::NAN),
            "Infinity" => Some(f64::INFINITY),
            "-Infinity" => Some(f64::NEG_INFINITY),
            _ => None,
        },
        _ => None,
    }
}

fn integer_of(v: &Tree) -> Option<i32> {
    match v {
        Tree::Null => Some(abi::NA_INTEGER),
        Tree::Num(x) => whole_i32(x.as_f64()),
        _ => None,
    }
}

/// A whole number within R's integer range.
fn whole_i32(x: f64) -> Option<i32> {
    (x.fract() == 0.0 && (-2147483647.0..=2147483647.0).contains(&x)).then_some(x as i32)
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
        Some(Tree::Str(s)) => {
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
    f: fn(&Tree) -> Option<T>,
) -> Status {
    let Some(Tree::Array(a)) = (unsafe { value(n) }) else {
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

/// The items `start..start + count` of a run, or the error of a block past
/// its end.
fn run_part(d: &[f64], start: usize, count: usize, err: *mut Error) -> Result<&[f64], Status> {
    start
        .checked_add(count)
        .and_then(|end| d.get(start..end))
        .ok_or_else(|| {
            fail(
                err,
                "out_of_range",
                "the block extends past the array's end",
            )
        })
}

unsafe extern "C" fn doubles(
    _: *mut c_void,
    n: Node,
    start: usize,
    count: usize,
    out: *mut f64,
    err: *mut Error,
) -> Status {
    if let Some(Tree::Doubles(d)) = unsafe { value(n) } {
        // A run holds `null` as `NA_real_`, which is what `number` reads.
        return match run_part(d, start, count, err) {
            Ok(part) => {
                unsafe { std::ptr::copy_nonoverlapping(part.as_ptr(), out, count) };
                abi::OK
            }
            Err(status) => status,
        };
    }
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
    if let Some(Tree::Doubles(d)) = unsafe { value(n) } {
        let part = match run_part(d, start, count, err) {
            Ok(part) => part,
            Err(status) => return status,
        };
        let out = unsafe { std::slice::from_raw_parts_mut(out, count) };
        for (k, (o, &x)) in out.iter_mut().zip(part).enumerate() {
            match if is_na(x) {
                Some(abi::NA_INTEGER)
            } else {
                whole_i32(x)
            } {
                Some(i) => *o = i,
                None => {
                    return fail(
                        err,
                        "not_element",
                        &format!("item {} is not a whole number", start + k),
                    );
                }
            }
        }
        return abi::OK;
    }
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

unsafe extern "C" fn host_value(
    context: *mut c_void,
    n: Node,
    kind: u32,
    name: *const c_char,
    out: *mut Sexp,
    err: *mut Error,
) -> Status {
    let Some(t) = (unsafe { value(n) }) else {
        return fail(err, "absent", "the node is absent");
    };
    let Some(reader) = (unsafe { context.cast::<TreeReader>().as_ref() }) else {
        return fail(err, "reader", "the reader has no context");
    };
    if name.is_null() || out.is_null() {
        return fail(err, "invalid_argument", "`name` or `out` is NULL");
    }
    let q = match unsafe { CStr::from_ptr(name) }
        .to_str()
        .map_err(|e| e.to_string())
        .and_then(|s| QualifiedName::parse(s).map_err(|e| e.to_string()))
    {
        Ok(q) => q,
        Err(e) => return fail(err, "invalid_argument", &e),
    };
    let ty = match kind {
        abi::VALUE_TYPE => FieldType::Named { name: q },
        abi::VALUE_SUBTYPE => FieldType::Subtype { of: q },
        abi::VALUE_IMPLEMENTOR => FieldType::Dyn { r#trait: q },
        _ => return fail(err, "invalid_argument", "`kind` is no R8_VALUE_* constant"),
    };
    match reader.values.value(t, &ty) {
        Ok(v) => {
            unsafe { *out = v };
            abi::OK
        }
        Err((code, message)) => fail(err, code, &message),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A host whose value of a node is its address, for `subtype(demo::A)`.
    struct Addresses;

    impl HostValues for Addresses {
        fn value(&self, node: &Tree, ty: &FieldType) -> Result<Sexp, (&'static str, String)> {
            match ty {
                FieldType::Subtype { of } if of.to_string() == "demo::A" => {
                    Ok((node as *const Tree).cast_mut().cast())
                }
                other => Err(("other_type", format!("{other:?}"))),
            }
        }
    }

    #[test]
    fn reads_a_node_as_a_host_value() {
        let t: Tree = serde_json::from_str(r#"{"a": {"$type": "demo::A"}}"#).unwrap();
        let reader = TreeReader::new(&Addresses);
        let r = reader.table();
        unsafe {
            let a = r.member.unwrap()(r.context, node(&t), c"a".as_ptr());
            let read = |n: Node, kind: u32, name: &CStr| {
                let mut out: Sexp = std::ptr::null_mut();
                let mut e = Error {
                    code: [0; 64],
                    message: [0; 1024],
                };
                let status = r.value.unwrap()(r.context, n, kind, name.as_ptr(), &mut out, &mut e);
                let code = CStr::from_ptr(e.code.as_ptr())
                    .to_str()
                    .unwrap()
                    .to_string();
                (status, out, code)
            };
            let (status, out, _) = read(a, abi::VALUE_SUBTYPE, c"demo::A");
            assert_eq!((status, out.cast_const()), (abi::OK, a));
            for (n, kind, name, code) in [
                (a, abi::VALUE_IMPLEMENTOR, c"demo::A", "other_type"),
                (a, abi::VALUE_TYPE, c"demo::A", "other_type"),
                (a, 9, c"demo::A", "invalid_argument"),
                (a, abi::VALUE_SUBTYPE, c"A", "invalid_argument"),
                (std::ptr::null(), abi::VALUE_SUBTYPE, c"demo::A", "absent"),
            ] {
                let (status, _, found) = read(n, kind, name);
                assert_eq!((status, found.as_str()), (abi::ERROR, code));
            }
        }
    }

    #[test]
    fn reads_a_tree_through_the_table() {
        let t: Tree = serde_json::from_str(
            r#"{"a": [1.5, null, "Infinity"], "r": [1, null, 2.5], "n": [1, 2, null], "s": "x", "b": true}"#,
        )
        .unwrap();
        let reader = TreeReader::new(&Addresses);
        let r = reader.table();
        let root = node(&t);
        unsafe {
            let ctx = r.context;
            assert_eq!(r.kind.unwrap()(ctx, root), abi::NODE_OBJECT);
            let a = r.member.unwrap()(ctx, root, c"a".as_ptr());
            assert_eq!(r.length.unwrap()(ctx, a), 3);
            let mut out = [0.0; 3];
            assert_eq!(
                r.doubles.unwrap()(ctx, a, 0, 3, out.as_mut_ptr(), std::ptr::null_mut()),
                abi::OK
            );
            assert_eq!(out[0], 1.5);
            assert_eq!(out[1].to_bits(), abi::NA_DOUBLE_BITS);
            assert_eq!(out[2], f64::INFINITY);
            let run = r.member.unwrap()(ctx, root, c"r".as_ptr());
            assert_eq!(r.kind.unwrap()(ctx, run), abi::NODE_ARRAY);
            assert_eq!(
                r.doubles.unwrap()(ctx, run, 0, 3, out.as_mut_ptr(), std::ptr::null_mut()),
                abi::OK
            );
            assert_eq!((out[0], out[2]), (1.0, 2.5));
            assert_eq!(out[1].to_bits(), abi::NA_DOUBLE_BITS);
            let second = r.item.unwrap()(ctx, run, 2);
            let mut x = 0.0;
            assert_eq!(
                r.number.unwrap()(ctx, second, &mut x, std::ptr::null_mut()),
                abi::OK
            );
            assert_eq!(x, 2.5);
            assert_eq!(
                r.kind.unwrap()(ctx, r.item.unwrap()(ctx, run, 1)),
                abi::NODE_NULL
            );
            let n = r.member.unwrap()(ctx, root, c"n".as_ptr());
            let mut ints = [0; 3];
            assert_eq!(
                r.integers.unwrap()(ctx, n, 0, 3, ints.as_mut_ptr(), std::ptr::null_mut()),
                abi::OK
            );
            assert_eq!(ints, [1, 2, abi::NA_INTEGER]);
            assert_eq!(
                r.doubles.unwrap()(ctx, run, 2, 2, out.as_mut_ptr(), std::ptr::null_mut()),
                abi::ERROR
            );
            assert!(r.member.unwrap()(ctx, root, c"missing".as_ptr()).is_null());
            let s = r.member.unwrap()(ctx, root, c"s".as_ptr());
            let (mut p, mut len) = (std::ptr::null(), 0);
            assert_eq!(
                r.string.unwrap()(ctx, s, &mut p, &mut len, std::ptr::null_mut()),
                abi::OK
            );
            assert_eq!(std::slice::from_raw_parts(p.cast::<u8>(), len), b"x");
        }
    }
}
