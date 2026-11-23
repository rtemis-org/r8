//! The compact tree of a document read: a JSON value in 24 B a node, whose
//! arrays of numbers are runs of doubles, so that a read holds about the size
//! of its data.
//!
//! [`Tree`] deserializes from JSON text in one pass. An array is collected as
//! a run while its items are numbers that a double holds exactly or `null`,
//! which the run stores as R's `NA_real_` (the convention of
//! [`crate::access::Access::f64_slice`]); it falls back to nodes on any other
//! item, on an integer beyond 2^53, and when it holds no number. The strings
//! `"NaN"` and `"Infinity"` never join a run: a character vector may hold
//! them. [`TreeRef`] reads the tree as an [`access::Node`](crate::access::Node),
//! a run's item as its number and an `NA_real_` item as `null`, so the
//! value-access trait reads a tree through the same implementation as a
//! `serde_json::Value`.
//! spec: R8/class-system#documents

use std::fmt;

use serde::de::{self, Deserializer, MapAccess, SeqAccess, Visitor};
use serde::ser::{SerializeMap, SerializeSeq, Serializer};
use serde::{Deserialize, Serialize};
use serde_json::Value;

use crate::abi::NA_DOUBLE_BITS;
use crate::access::Node;
use crate::ir::Number;

/// The members of an object, in document order.
pub type Members = Box<[(Box<str>, Tree)]>;

/// A JSON value in compact form.
#[derive(Debug, Clone, PartialEq)]
pub enum Tree {
    /// `null`.
    Null,
    /// A boolean.
    Bool(bool),
    /// A number.
    Num(Number),
    /// A string.
    Str(Box<str>),
    /// An array of nodes.
    Array(Box<[Tree]>),
    /// An object, its members in document order.
    Object(Members),
    /// An array of numbers and nulls as one run of doubles, `null` as
    /// `NA_real_`.
    Doubles(Box<[f64]>),
}

/// `null`, for a run's `NA_real_` item.
static NULL: Tree = Tree::Null;

fn is_na(x: f64) -> bool {
    x.to_bits() == NA_DOUBLE_BITS
}

fn na() -> f64 {
    f64::from_bits(NA_DOUBLE_BITS)
}

impl Tree {
    /// The tree as a `serde_json::Value`.
    pub fn to_value(&self) -> Value {
        TreeRef::Node(self).to_value()
    }

    /// The member `key` of an object; the last of members of one name.
    pub fn get(&self, key: &str) -> Option<&Tree> {
        match self {
            Tree::Object(m) => m.iter().rev().find(|(k, _)| &**k == key).map(|(_, v)| v),
            _ => None,
        }
    }

    /// The members of an object, for an edit in place.
    pub fn members_mut(&mut self) -> Option<&mut Members> {
        match self {
            Tree::Object(m) => Some(m),
            _ => None,
        }
    }
}

/// A handle into a [`Tree`]: a node, or a number of a run.
#[derive(Debug, Clone, Copy)]
pub enum TreeRef<'a> {
    /// A node.
    Node(&'a Tree),
    /// An item of a run that is not `NA_real_`.
    Num(f64),
}

impl<'a> TreeRef<'a> {
    /// The handle of `t`.
    pub fn new(t: &'a Tree) -> Self {
        TreeRef::Node(t)
    }

    fn node(self) -> Option<&'a Tree> {
        match self {
            TreeRef::Node(t) => Some(t),
            TreeRef::Num(_) => None,
        }
    }
}

impl<'a> Node<'a> for TreeRef<'a> {
    fn is_null(self) -> bool {
        matches!(self, TreeRef::Node(Tree::Null))
    }
    fn as_bool(self) -> Option<bool> {
        match self {
            TreeRef::Node(Tree::Bool(b)) => Some(*b),
            _ => None,
        }
    }
    fn number(self) -> Option<f64> {
        match self {
            TreeRef::Node(Tree::Num(n)) => Some(n.as_f64()),
            TreeRef::Num(x) => Some(x),
            _ => None,
        }
    }
    fn as_u64(self) -> Option<u64> {
        match self {
            TreeRef::Node(Tree::Num(n)) => n.as_u64(),
            TreeRef::Num(x) => Number::of_f64(x).as_u64(),
            _ => None,
        }
    }
    fn as_str(self) -> Option<&'a str> {
        match self {
            TreeRef::Node(Tree::Str(s)) => Some(s),
            _ => None,
        }
    }
    fn array_len(self) -> Option<usize> {
        match self.node()? {
            Tree::Array(a) => Some(a.len()),
            Tree::Doubles(d) => Some(d.len()),
            _ => None,
        }
    }
    fn item(self, i: usize) -> Option<Self> {
        match self.node()? {
            Tree::Array(a) => a.get(i).map(TreeRef::Node),
            Tree::Doubles(d) => d.get(i).map(|&x| {
                if is_na(x) {
                    TreeRef::Node(&NULL)
                } else {
                    TreeRef::Num(x)
                }
            }),
            _ => None,
        }
    }
    fn get(self, key: &str) -> Option<Self> {
        self.node()?.get(key).map(TreeRef::Node)
    }
    fn entries(self) -> Option<Vec<(&'a str, Self)>> {
        match self.node()? {
            Tree::Object(m) => Some(m.iter().map(|(k, v)| (&**k, TreeRef::Node(v))).collect()),
            _ => None,
        }
    }
    fn object_len(self) -> Option<usize> {
        match self.node()? {
            Tree::Object(m) => Some(m.len()),
            _ => None,
        }
    }
    fn kind(self) -> &'static str {
        match self {
            TreeRef::Num(_) => "a number",
            TreeRef::Node(t) => match t {
                Tree::Null => "null",
                Tree::Bool(_) => "a logical",
                Tree::Num(_) => "a number",
                Tree::Str(_) => "a string",
                Tree::Array(_) | Tree::Doubles(_) => "an array",
                Tree::Object(_) => "an object",
            },
        }
    }
    fn f64_slice(self) -> Option<&'a [f64]> {
        match self.node()? {
            Tree::Doubles(d) => Some(d),
            _ => None,
        }
    }
    fn same(self, other: Self) -> bool {
        match (self, other) {
            (TreeRef::Node(a), TreeRef::Node(b)) => std::ptr::eq(a, b),
            _ => false,
        }
    }
    fn with_value<R>(self, f: impl FnOnce(&Value) -> R) -> R {
        f(&self.to_value())
    }
    fn to_value(self) -> Value {
        match self {
            TreeRef::Num(x) => Number::of_f64(x).into(),
            TreeRef::Node(t) => match t {
                Tree::Null => Value::Null,
                Tree::Bool(b) => Value::Bool(*b),
                Tree::Num(n) => (*n).into(),
                Tree::Str(s) => Value::String(s.to_string()),
                Tree::Array(a) => Value::Array(a.iter().map(Tree::to_value).collect()),
                Tree::Doubles(d) => Value::Array(
                    d.iter()
                        .map(|&x| {
                            if is_na(x) {
                                Value::Null
                            } else {
                                Number::of_f64(x).into()
                            }
                        })
                        .collect(),
                ),
                Tree::Object(m) => Value::Object(
                    m.iter()
                        .map(|(k, v)| (k.to_string(), v.to_value()))
                        .collect(),
                ),
            },
        }
    }
}

/// Builds a tree: with numeric runs for a document read, and with every
/// number kept as written (`exact`) for a value of the IR, such as a default,
/// whose record keeps each number's form.
struct TreeVisitor {
    exact: bool,
}

impl<'de> Visitor<'de> for TreeVisitor {
    type Value = Tree;

    fn expecting(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("a JSON value")
    }
    fn visit_unit<E: de::Error>(self) -> Result<Tree, E> {
        Ok(Tree::Null)
    }
    fn visit_none<E: de::Error>(self) -> Result<Tree, E> {
        Ok(Tree::Null)
    }
    fn visit_some<D: Deserializer<'de>>(self, d: D) -> Result<Tree, D::Error> {
        d.deserialize_any(self)
    }
    fn visit_bool<E: de::Error>(self, v: bool) -> Result<Tree, E> {
        Ok(Tree::Bool(v))
    }
    fn visit_i64<E: de::Error>(self, v: i64) -> Result<Tree, E> {
        Ok(Tree::Num(if v >= 0 {
            Number::U64(v as u64)
        } else {
            Number::I64(v)
        }))
    }
    fn visit_u64<E: de::Error>(self, v: u64) -> Result<Tree, E> {
        Ok(Tree::Num(Number::U64(v)))
    }
    fn visit_f64<E: de::Error>(self, v: f64) -> Result<Tree, E> {
        Ok(Tree::Num(Number::F64(v)))
    }
    fn visit_str<E: de::Error>(self, v: &str) -> Result<Tree, E> {
        Ok(Tree::Str(v.into()))
    }
    fn visit_string<E: de::Error>(self, v: String) -> Result<Tree, E> {
        Ok(Tree::Str(v.into_boxed_str()))
    }

    fn visit_seq<A: SeqAccess<'de>>(self, mut seq: A) -> Result<Tree, A::Error> {
        if self.exact {
            let mut items = Vec::with_capacity(seq.size_hint().unwrap_or(0));
            while let Some(x) = seq.next_element::<Exact>()? {
                items.push(x.0);
            }
            return Ok(Tree::Array(items.into_boxed_slice()));
        }
        // A run while every item is an exact number or null; nodes after the
        // first other item.
        let mut run: Vec<f64> = Vec::new();
        let mut numbers = false;
        let mut nodes: Option<Vec<Tree>> = None;
        while let Some(item) = seq.next_element::<Tree>()? {
            if let Some(v) = nodes.as_mut() {
                v.push(item);
                continue;
            }
            match item {
                Tree::Num(n) if n.is_exact_f64() => {
                    run.push(n.as_f64());
                    numbers = true;
                }
                Tree::Null => run.push(na()),
                other => {
                    let mut v: Vec<Tree> = run
                        .drain(..)
                        .map(|x| {
                            if is_na(x) {
                                Tree::Null
                            } else {
                                Tree::Num(Number::of_f64(x))
                            }
                        })
                        .collect();
                    v.push(other);
                    nodes = Some(v);
                }
            }
        }
        Ok(match nodes {
            Some(v) => Tree::Array(v.into_boxed_slice()),
            None if numbers => Tree::Doubles(run.into_boxed_slice()),
            None => Tree::Array(run.iter().map(|_| Tree::Null).collect()),
        })
    }

    fn visit_map<A: MapAccess<'de>>(self, mut map: A) -> Result<Tree, A::Error> {
        let mut members: Vec<(Box<str>, Tree)> = Vec::with_capacity(map.size_hint().unwrap_or(0));
        if self.exact {
            while let Some((k, v)) = map.next_entry::<String, Exact>()? {
                members.push((k.into_boxed_str(), v.0));
            }
        } else {
            while let Some((k, v)) = map.next_entry::<String, Tree>()? {
                members.push((k.into_boxed_str(), v));
            }
        }
        Ok(Tree::Object(members.into_boxed_slice()))
    }
}

impl<'de> Deserialize<'de> for Tree {
    fn deserialize<D: Deserializer<'de>>(d: D) -> Result<Self, D::Error> {
        d.deserialize_any(TreeVisitor { exact: false })
    }
}

/// A tree whose numbers are kept as written: no runs.
pub struct Exact(pub Tree);

impl<'de> Deserialize<'de> for Exact {
    fn deserialize<D: Deserializer<'de>>(d: D) -> Result<Self, D::Error> {
        d.deserialize_any(TreeVisitor { exact: true }).map(Exact)
    }
}

/// Deserializes an optional boxed tree with its numbers kept as written, for
/// `#[serde(deserialize_with)]` on a value of the IR.
pub fn exact_option<'de, D: Deserializer<'de>>(d: D) -> Result<Option<Box<Tree>>, D::Error> {
    Option::<Exact>::deserialize(d).map(|o| o.map(|e| Box::new(e.0)))
}

/// Deserializes a boxed tree that is present, `null` included, with its
/// numbers kept as written, for `#[serde(deserialize_with)]` on a member whose
/// absence differs from `null`.
pub fn exact_present<'de, D: Deserializer<'de>>(d: D) -> Result<Option<Box<Tree>>, D::Error> {
    Exact::deserialize(d).map(|e| Some(Box::new(e.0)))
}

impl Tree {
    /// The tree of a `serde_json::Value`, its numbers kept as written.
    pub fn from_value(v: &Value) -> Tree {
        match v {
            Value::Null => Tree::Null,
            Value::Bool(b) => Tree::Bool(*b),
            Value::Number(n) => Tree::Num(if let Some(u) = n.as_u64() {
                Number::U64(u)
            } else if let Some(i) = n.as_i64() {
                Number::I64(i)
            } else {
                Number::F64(n.as_f64().unwrap_or(f64::NAN))
            }),
            Value::String(s) => Tree::Str(s.as_str().into()),
            Value::Array(a) => Tree::Array(a.iter().map(Tree::from_value).collect()),
            Value::Object(m) => Tree::Object(
                m.iter()
                    .map(|(k, x)| (k.as_str().into(), Tree::from_value(x)))
                    .collect(),
            ),
        }
    }
}

/// Builds the [`Tree`] of the emitter's stream, its numbers as reading their
/// ECMAScript text gives them ([`Number::of_f64`]) and no runs: the
/// normalized form of a value as a tree.
#[derive(Default)]
pub struct TreeSink {
    stack: Vec<Frame>,
    /// The tree, once the outermost value is written.
    pub out: Option<Tree>,
}

enum Frame {
    Array(Vec<Tree>),
    Object(Vec<(Box<str>, Tree)>, Option<Box<str>>),
}

impl TreeSink {
    fn push(&mut self, t: Tree) {
        match self.stack.last_mut() {
            None => self.out = Some(t),
            Some(Frame::Array(items)) => items.push(t),
            Some(Frame::Object(m, key)) => {
                if let Some(k) = key.take() {
                    m.push((k, t));
                }
            }
        }
    }
}

impl crate::emit::Sink for TreeSink {
    fn sorted(&self) -> bool {
        false
    }
    fn null(&mut self) {
        self.push(Tree::Null)
    }
    fn bool(&mut self, b: bool) {
        self.push(Tree::Bool(b))
    }
    fn number(&mut self, x: f64) {
        if x.is_nan() {
            return self.string("NaN");
        }
        if x.is_infinite() {
            return self.string(if x > 0.0 { "Infinity" } else { "-Infinity" });
        }
        self.push(Tree::Num(Number::of_f64(x)))
    }
    fn string(&mut self, s: &str) {
        self.push(Tree::Str(s.into()))
    }
    fn begin_array(&mut self) {
        self.stack.push(Frame::Array(Vec::new()))
    }
    fn end_array(&mut self) {
        if let Some(Frame::Array(items)) = self.stack.pop() {
            self.push(Tree::Array(items.into_boxed_slice()))
        }
    }
    fn begin_object(&mut self) {
        self.stack.push(Frame::Object(Vec::new(), None))
    }
    fn key(&mut self, k: &str) {
        if let Some(Frame::Object(_, key)) = self.stack.last_mut() {
            *key = Some(k.into());
        }
    }
    fn end_object(&mut self) {
        if let Some(Frame::Object(m, _)) = self.stack.pop() {
            self.push(Tree::Object(m.into_boxed_slice()))
        }
    }
}

impl Serialize for Tree {
    fn serialize<S: Serializer>(&self, s: S) -> Result<S::Ok, S::Error> {
        match self {
            Tree::Null => s.serialize_unit(),
            Tree::Bool(b) => s.serialize_bool(*b),
            Tree::Num(n) => n.serialize(s),
            Tree::Str(v) => s.serialize_str(v),
            Tree::Array(a) => {
                let mut q = s.serialize_seq(Some(a.len()))?;
                for x in a.iter() {
                    q.serialize_element(x)?;
                }
                q.end()
            }
            Tree::Doubles(d) => {
                let mut q = s.serialize_seq(Some(d.len()))?;
                for &x in d.iter() {
                    if is_na(x) {
                        q.serialize_element(&())?;
                    } else {
                        q.serialize_element(&Number::of_f64(x))?;
                    }
                }
                q.end()
            }
            Tree::Object(m) => {
                let mut q = s.serialize_map(Some(m.len()))?;
                for (k, v) in m.iter() {
                    q.serialize_entry(&**k, v)?;
                }
                q.end()
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn tree(text: &str) -> Tree {
        serde_json::from_str(text).unwrap()
    }

    #[test]
    fn a_node_takes_24_bytes() {
        assert_eq!(std::mem::size_of::<Tree>(), 24);
    }

    #[test]
    fn numbers_and_nulls_form_a_run() {
        let t = tree(
            r#"{"a": [1, 2.5, null], "b": [1, "x"], "c": ["NaN", 1], "d": [null], "e": [9007199254740993, 1]}"#,
        );
        match t.get("a") {
            Some(Tree::Doubles(d)) => {
                assert_eq!(d[0], 1.0);
                assert_eq!(d[1], 2.5);
                assert!(is_na(d[2]));
            }
            other => panic!("{other:?}"),
        }
        assert!(matches!(t.get("b"), Some(Tree::Array(_))));
        assert!(matches!(t.get("c"), Some(Tree::Array(_))));
        assert!(matches!(t.get("d"), Some(Tree::Array(_))));
        assert!(matches!(t.get("e"), Some(Tree::Array(_))));
    }

    #[test]
    fn the_tree_reads_back_as_the_value() {
        let text = r#"{"x": [1, 2, null, 3.5], "y": {"z": "s", "w": [true, null]}, "big": 18446744073709551615, "neg": -3}"#;
        let v: Value = serde_json::from_str(text).unwrap();
        assert_eq!(tree(text).to_value(), v);
        let back: Value = serde_json::to_value(tree(text)).unwrap();
        assert_eq!(back, v);
    }

    #[test]
    fn a_run_reads_through_the_node_interface() {
        let t = tree("[1, null, 2.5]");
        let r = TreeRef::new(&t);
        assert_eq!(r.array_len(), Some(3));
        assert_eq!(r.item(0).and_then(Node::number), Some(1.0));
        assert!(r.item(1).is_some_and(Node::is_null));
        assert_eq!(r.item(2).and_then(Node::number), Some(2.5));
        assert_eq!(r.item(0).and_then(Node::as_u64), Some(1));
        assert_eq!(r.f64_slice().map(<[f64]>::len), Some(3));
    }
}
