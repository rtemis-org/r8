//! Expressions of the rule dialect, held in the IR.
//!
//! A rule's invariant and condition, a computed field's expression, and a
//! migration's expressions are JSONLogic expressions in the strict dialect of
//! `rtemis-schema`, extended by R8's intrinsics. The IR holds each as a typed
//! syntax tree, [`Logic`], whose serde implementations read and write the
//! JSONLogic encoding: an expression reads back to the text it was read from.

use std::fmt;

use serde::de::{self, Deserializer, MapAccess, SeqAccess, Visitor};
use serde::ser::{SerializeMap, SerializeSeq, Serializer};
use serde::{Deserialize, Serialize};
use serde_json::{Map, Number, Value};

macro_rules! operators {
    ($($(#[$doc:meta])* $v:ident = $s:literal),* $(,)?) => {
        /// An operator of the rule dialect, named in JSONLogic by its text.
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
        #[repr(u8)]
        pub enum Operator {
            $($(#[$doc])* $v),*
        }

        impl Operator {
            /// The operator named `s`, if the dialect has one.
            pub fn parse(s: &str) -> Option<Operator> {
                match s {
                    $($s => Some(Operator::$v),)*
                    _ => None,
                }
            }

            /// The operator's JSONLogic name.
            pub fn as_str(self) -> &'static str {
                match self {
                    $(Operator::$v => $s),*
                }
            }
        }
    };
}

operators! {
    /// `var`: a field, or a path into it.
    Var = "var",
    /// `===`: strict equality.
    StrictEq = "===",
    /// `!==`: strict inequality.
    StrictNe = "!==",
    /// `==`: JSONLogic's loose equality, which R8 rules reject.
    LooseEq = "==",
    /// `!=`: JSONLogic's loose inequality, which R8 rules reject.
    LooseNe = "!=",
    /// `<`.
    Lt = "<",
    /// `<=`.
    Le = "<=",
    /// `>`.
    Gt = ">",
    /// `>=`.
    Ge = ">=",
    /// `!`: negation.
    Not = "!",
    /// `+`.
    Add = "+",
    /// `-`: subtraction or negation.
    Sub = "-",
    /// `*`.
    Mul = "*",
    /// `/`.
    Div = "/",
    /// `%`.
    Mod = "%",
    /// `min`.
    Min = "min",
    /// `max`.
    Max = "max",
    /// `in`: membership in an array or a string.
    In = "in",
    /// `cat`: string concatenation.
    Cat = "cat",
    /// `length`.
    Length = "length",
    /// `subset`.
    Subset = "subset",
    /// `non_increasing`.
    NonIncreasing = "non_increasing",
    /// `non_decreasing`.
    NonDecreasing = "non_decreasing",
    /// `floor`.
    Floor = "floor",
    /// `ceiling`.
    Ceiling = "ceiling",
    /// `round`.
    Round = "round",
    /// `nrow`.
    Nrow = "nrow",
    /// `ncol`.
    Ncol = "ncol",
    /// `dim`.
    Dim = "dim",
    /// `is_null`: a presence test.
    IsNull = "is_null",
    /// `is_variant`.
    IsVariant = "is_variant",
    /// `levels`.
    Levels = "levels",
    /// `all`.
    All = "all",
    /// `any`.
    Any = "any",
    /// `if`.
    If = "if",
    /// `and`.
    And = "and",
    /// `or`.
    Or = "or",
}

/// A JSONLogic expression of the rule dialect, as declared.
///
/// An operation is an object of one member, the operator, whose value is its
/// operands: an array, or one operand written without an array, which
/// `single` records so that the expression writes back as declared. A number
/// keeps its JSON form (`1` and `1.0` differ), as `serde_json` reads it.
#[derive(Debug, Clone, PartialEq)]
pub enum Logic {
    /// `null`.
    Null,
    /// A boolean.
    Bool(bool),
    /// A number.
    Number(Number),
    /// A string.
    String(Box<str>),
    /// An array literal.
    Array(Box<[Logic]>),
    /// `{"var": "path"}`, a field or a path into it.
    Var(Box<str>),
    /// `{"op": [operands]}`, or `{"op": operand}` when `single`.
    Call {
        /// The operator.
        op: Operator,
        /// The operand was written without an array.
        single: bool,
        /// The operands.
        args: Box<[Logic]>,
    },
    /// An object that is not an operation of the dialect: an unknown
    /// operator, or not one member. The declaration check reports it with its
    /// code and path.
    Other(Box<Value>),
}

impl Logic {
    /// The string of a string literal.
    pub fn as_str(&self) -> Option<&str> {
        match self {
            Logic::String(s) => Some(s),
            _ => None,
        }
    }

    /// The expression as a JSON value.
    pub fn to_value(&self) -> Value {
        serde_json::to_value(self).expect("an expression serializes")
    }

    /// The expression of a JSON value.
    pub fn from_value(v: &Value) -> Logic {
        Logic::deserialize(v).expect("every JSON value reads as an expression")
    }
}

impl Serialize for Logic {
    fn serialize<S: Serializer>(&self, s: S) -> Result<S::Ok, S::Error> {
        match self {
            Logic::Null => s.serialize_unit(),
            Logic::Bool(b) => s.serialize_bool(*b),
            Logic::Number(n) => n.serialize(s),
            Logic::String(t) => s.serialize_str(t),
            Logic::Array(items) => {
                let mut seq = s.serialize_seq(Some(items.len()))?;
                for x in items.iter() {
                    seq.serialize_element(x)?;
                }
                seq.end()
            }
            Logic::Var(path) => {
                let mut m = s.serialize_map(Some(1))?;
                m.serialize_entry("var", &**path)?;
                m.end()
            }
            Logic::Call { op, single, args } => {
                let mut m = s.serialize_map(Some(1))?;
                match (single, &**args) {
                    (true, [one]) => m.serialize_entry(op.as_str(), one)?,
                    _ => m.serialize_entry(op.as_str(), &**args)?,
                }
                m.end()
            }
            Logic::Other(v) => v.serialize(s),
        }
    }
}

impl<'de> Deserialize<'de> for Logic {
    fn deserialize<D: Deserializer<'de>>(d: D) -> Result<Self, D::Error> {
        d.deserialize_any(LogicVisitor)
    }
}

/// Builds the tree in one pass over any self-describing input, including the
/// buffered content of an internally tagged enum.
struct LogicVisitor;

/// An object of one member whose key names no operator, kept as written.
fn other_object(key: String, value: &Logic) -> Logic {
    let mut m = Map::new();
    m.insert(key, value.to_value());
    Logic::Other(Box::new(Value::Object(m)))
}

impl<'de> Visitor<'de> for LogicVisitor {
    type Value = Logic;

    fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str("a JSONLogic expression")
    }

    fn visit_unit<E>(self) -> Result<Logic, E> {
        Ok(Logic::Null)
    }

    fn visit_none<E>(self) -> Result<Logic, E> {
        Ok(Logic::Null)
    }

    fn visit_some<D: Deserializer<'de>>(self, d: D) -> Result<Logic, D::Error> {
        Logic::deserialize(d)
    }

    fn visit_bool<E>(self, b: bool) -> Result<Logic, E> {
        Ok(Logic::Bool(b))
    }

    fn visit_u64<E>(self, n: u64) -> Result<Logic, E> {
        Ok(Logic::Number(n.into()))
    }

    fn visit_i64<E>(self, n: i64) -> Result<Logic, E> {
        Ok(Logic::Number(n.into()))
    }

    fn visit_f64<E: de::Error>(self, n: f64) -> Result<Logic, E> {
        Number::from_f64(n)
            .map(Logic::Number)
            .ok_or_else(|| E::custom("a JSONLogic number is finite"))
    }

    fn visit_str<E>(self, s: &str) -> Result<Logic, E> {
        Ok(Logic::String(s.into()))
    }

    fn visit_seq<A: SeqAccess<'de>>(self, mut seq: A) -> Result<Logic, A::Error> {
        let mut items = Vec::with_capacity(seq.size_hint().unwrap_or(0));
        while let Some(x) = seq.next_element()? {
            items.push(x);
        }
        Ok(Logic::Array(items.into_boxed_slice()))
    }

    fn visit_map<A: MapAccess<'de>>(self, mut map: A) -> Result<Logic, A::Error> {
        let Some(key) = map.next_key::<String>()? else {
            return Ok(Logic::Other(Box::new(Value::Object(Map::new()))));
        };
        let value: Logic = map.next_value()?;
        if let Some(second) = map.next_key::<String>()? {
            let mut m = Map::new();
            m.insert(key, value.to_value());
            m.insert(second, map.next_value::<Value>()?);
            while let Some((k, v)) = map.next_entry::<String, Value>()? {
                m.insert(k, v);
            }
            return Ok(Logic::Other(Box::new(Value::Object(m))));
        }
        let Some(op) = Operator::parse(&key) else {
            return Ok(other_object(key, &value));
        };
        Ok(match (op, value) {
            (Operator::Var, Logic::String(path)) => Logic::Var(path),
            (op, Logic::Array(args)) => Logic::Call {
                op,
                single: false,
                args,
            },
            (op, one) => Logic::Call {
                op,
                single: true,
                args: Box::new([one]),
            },
        })
    }
}

#[cfg(feature = "schema")]
impl schemars::JsonSchema for Logic {
    fn schema_name() -> std::borrow::Cow<'static, str> {
        "Logic".into()
    }

    fn json_schema(_: &mut schemars::SchemaGenerator) -> schemars::Schema {
        schemars::json_schema!({
            "description": "A JSONLogic expression of the rule dialect."
        })
    }
}
