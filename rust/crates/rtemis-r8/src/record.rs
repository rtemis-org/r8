//! The declaration record: a compact binary encoding of declarations, which
//! a host persists with its type objects and reads back to register them.
//!
//! A record is a header and a payload. The header holds the magic bytes
//! `R8D\0`, the format version, the version of `rtemis-r8` that wrote it,
//! and the CRC-32C of the payload, four bytes little-endian. The payload is a
//! sequence of entries, each a declaration (a type, a trait, or an impl) with
//! the namespace it belongs to.
//!
//! The writer and the reader are written by hand over the IR types, so the code
//! is the format. Every struct is destructured without `..`, every enum tag is
//! an explicit literal, and the writer's methods are typed, so a change to an
//! IR type fails to compile here until the format accounts for it. The encoder
//! writes [`FORMAT`], the one format the decoder reads, into the current IR
//! types. Optional members and booleans pack into one flag byte per node, whose
//! unused bits are rejected; varints are canonical; every length is checked
//! against the bytes that remain; trees have a depth limit; trailing bytes are
//! rejected. One declaration has one encoding. spec:
//! R8/class-system#declaration-record

use std::collections::BTreeMap;
use std::fmt;

use serde_json::{Map, Number, Value};

use crate::ir::{
    Bound, CharacterType, ClassSystem, ColumnDef, DoubleType, EnumDef, FieldDef, FieldType,
    HostDefault, ImplDef, ImplMethod, IntegerType, Logic, MethodDef, Migration, MigrationStep,
    Operator, Param, ParamDef, RequiredField, Resolve, ReturnType, RuleDef, SelfMarker,
    SplitTarget, StructDef, TraitDef, TypeDef, TypeRef, TypedImplMethod, VariantDef, VariantKind,
};
use crate::name::{MethodName, Name, QualifiedName};

/// The magic bytes that open a record.
pub const MAGIC: [u8; 4] = *b"R8D\0";

/// The format version this crate writes.
pub const FORMAT: u32 = 7;

/// The deepest nesting of field types, expressions, and JSON values a record
/// holds.
const MAX_DEPTH: u32 = 128;

/// A declaration of a record entry.
#[derive(Debug, Clone, PartialEq)]
pub enum Declaration {
    /// A struct or an enum.
    Type(TypeDef),
    /// A trait.
    Trait(TraitDef),
    /// An impl.
    Impl(ImplDef),
}

/// One entry of a record: a declaration and its namespace.
#[derive(Debug, Clone, PartialEq)]
pub struct Entry {
    /// The namespace the declaration belongs to.
    pub namespace: Name,
    /// The declaration.
    pub declaration: Declaration,
}

/// A borrowed declaration, for encoding without a copy.
#[derive(Debug, Clone, Copy)]
pub enum DeclarationRef<'a> {
    /// A struct or an enum.
    Type(&'a TypeDef),
    /// A trait.
    Trait(&'a TraitDef),
    /// An impl.
    Impl(&'a ImplDef),
}

/// Why a record could not be read.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RecordError {
    /// The bytes do not start with [`MAGIC`].
    Magic,
    /// The record's format version is another than [`FORMAT`]; `writer` is
    /// the version of `rtemis-r8` that wrote it.
    Newer {
        /// The record's format version.
        format: u32,
        /// The writing version of `rtemis-r8`.
        writer: String,
    },
    /// The payload does not match the header's CRC-32C.
    Checksum,
    /// The bytes end inside a value.
    Truncated,
    /// A varint is overlong or overflows its type.
    Varint,
    /// A tag or flag byte holds a value the format does not define.
    Tag(&'static str, u8),
    /// A string is not UTF-8.
    Utf8,
    /// A name does not satisfy the naming rules.
    Name(String),
    /// A tree nests deeper than the limit.
    Depth,
    /// An object or a map repeats a key, or a map's keys are out of order.
    Key(String),
    /// Bytes follow the last entry.
    Trailing,
}

impl fmt::Display for RecordError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RecordError::Magic => f.write_str("the bytes are not an R8 declaration record"),
            RecordError::Newer { format, writer } => write!(
                f,
                "the record has format {format}, written by rtemis-r8 {writer}; this rtemis-r8 {} reads format {FORMAT}; reinstall the package that holds the record",
                env!("CARGO_PKG_VERSION")
            ),
            RecordError::Checksum => {
                f.write_str("the record's checksum does not match its content")
            }
            RecordError::Truncated => f.write_str("the record ends inside a value"),
            RecordError::Varint => {
                f.write_str("the record holds an overlong or overflowing integer")
            }
            RecordError::Tag(what, t) => write!(f, "the record holds {t} as a {what} tag"),
            RecordError::Utf8 => f.write_str("the record holds a string that is not UTF-8"),
            RecordError::Name(s) => write!(f, "the record holds an invalid name `{s}`"),
            RecordError::Depth => write!(f, "the record nests deeper than {MAX_DEPTH} levels"),
            RecordError::Key(k) => write!(f, "the record repeats or misorders the key `{k}`"),
            RecordError::Trailing => f.write_str("bytes follow the record's last entry"),
        }
    }
}

impl std::error::Error for RecordError {}

type Res<T> = Result<T, RecordError>;

// ---------------------------------------------------------------- CRC-32C

/// The table of the reflected Castagnoli polynomial, 0x82F63B78.
const CRC_TABLE: [u32; 256] = {
    let mut t = [0u32; 256];
    let mut i = 0;
    while i < 256 {
        let mut c = i as u32;
        let mut k = 0;
        while k < 8 {
            c = if c & 1 != 0 {
                0x82F6_3B78 ^ (c >> 1)
            } else {
                c >> 1
            };
            k += 1;
        }
        t[i] = c;
        i += 1;
    }
    t
};

/// The CRC-32C (Castagnoli) of `bytes`, as iSCSI and ext4 compute it.
pub fn crc32c(bytes: &[u8]) -> u32 {
    let mut c = !0u32;
    for &b in bytes {
        c = CRC_TABLE[((c ^ u32::from(b)) & 0xff) as usize] ^ (c >> 8);
    }
    !c
}

// ---------------------------------------------------------------- writer

struct W(Vec<u8>);

impl W {
    fn u8(&mut self, b: u8) {
        self.0.push(b);
    }
    fn uv(&mut self, mut x: u64) {
        while x >= 0x80 {
            self.0.push(x as u8 | 0x80);
            x >>= 7;
        }
        self.0.push(x as u8);
    }
    fn u32(&mut self, x: u32) {
        self.uv(u64::from(x));
    }
    fn u64(&mut self, x: u64) {
        self.uv(x);
    }
    fn len(&mut self, n: usize) {
        self.uv(n as u64);
    }
    fn i64(&mut self, x: i64) {
        self.uv(((x << 1) ^ (x >> 63)) as u64);
    }
    fn i32(&mut self, x: i32) {
        self.i64(i64::from(x));
    }
    fn f64(&mut self, x: f64) {
        self.0.extend_from_slice(&x.to_bits().to_le_bytes());
    }
    fn str(&mut self, s: &str) {
        self.len(s.len());
        self.0.extend_from_slice(s.as_bytes());
    }
    fn name(&mut self, n: &Name) {
        self.str(n.as_str());
    }
    fn qn(&mut self, q: &QualifiedName) {
        let QualifiedName { namespace, name } = q;
        self.name(namespace);
        self.name(name);
    }
    fn seq<T>(&mut self, xs: &[T], mut f: impl FnMut(&mut Self, &T)) {
        self.len(xs.len());
        for x in xs {
            f(self, x);
        }
    }
}

macro_rules! operator_tags {
    ($($tag:literal $v:ident),* $(,)?) => {
        fn operator_tag(o: Operator) -> u8 {
            match o {
                $(Operator::$v => $tag),*
            }
        }
        fn operator_of(t: u8) -> Res<Operator> {
            match t {
                $($tag => Ok(Operator::$v),)*
                _ => Err(RecordError::Tag("operator", t)),
            }
        }
    };
}

operator_tags! {
    0 Var, 1 StrictEq, 2 StrictNe, 3 LooseEq, 4 LooseNe, 5 Lt, 6 Le, 7 Gt, 8 Ge, 9 Not,
    10 Add, 11 Sub, 12 Mul, 13 Div, 14 Mod, 15 Min, 16 Max, 17 In, 18 Cat, 19 Length,
    20 Subset, 21 NonIncreasing, 22 NonDecreasing, 23 Floor, 24 Ceiling, 25 Round,
    26 Nrow, 27 Ncol, 28 Dim, 29 IsNull, 30 IsVariant, 31 Levels, 32 All, 33 Any,
    34 If, 35 And, 36 Or, 37 Abs, 38 Sum, 39 Nchar,
}

/// A number at the tags `base` (a `u64`), `base + 1` (a negative `i64`), and
/// `base + 2` (an `f64`), the three forms of a `serde_json::Number`.
fn w_number(w: &mut W, base: u8, n: &Number) {
    if let Some(u) = n.as_u64() {
        w.u8(base);
        w.u64(u);
    } else if let Some(i) = n.as_i64() {
        w.u8(base + 1);
        w.i64(i);
    } else {
        w.u8(base + 2);
        w.f64(
            n.as_f64()
                .expect("a JSON number is a u64, an i64, or an f64"),
        );
    }
}

/// An IR number at the tags of [`w_number`]: a non-negative integer, a
/// negative integer, or a double.
fn w_num(w: &mut W, base: u8, n: &crate::ir::Number) {
    use crate::ir::Number as N;
    match *n {
        N::U64(u) => {
            w.u8(base);
            w.u64(u);
        }
        N::I64(i) if i >= 0 => {
            w.u8(base);
            w.u64(i as u64);
        }
        N::I64(i) => {
            w.u8(base + 1);
            w.i64(i);
        }
        N::F64(d) => {
            w.u8(base + 2);
            w.f64(d);
        }
    }
}

/// A tree at the tags of [`w_value`]; a run of doubles writes as an array of
/// its numbers, `NA_real_` as `null`.
fn w_tree(w: &mut W, t: &crate::tree::Tree) {
    use crate::tree::Tree as T;
    match t {
        T::Null => w.u8(0),
        T::Bool(false) => w.u8(1),
        T::Bool(true) => w.u8(2),
        T::Num(n) => w_num(w, 3, n),
        T::Str(s) => {
            w.u8(6);
            w.str(s);
        }
        T::Array(a) => {
            w.u8(7);
            w.seq(a, w_tree);
        }
        T::Doubles(d) => {
            w.u8(7);
            w.len(d.len());
            for &x in d.iter() {
                if x.to_bits() == crate::abi::NA_DOUBLE_BITS {
                    w.u8(0);
                } else {
                    w_num(w, 3, &crate::ir::Number::of_f64(x));
                }
            }
        }
        T::Object(m) => {
            w.u8(8);
            w.len(m.len());
            for (k, x) in m.iter() {
                w.str(k);
                w_tree(w, x);
            }
        }
    }
}

fn w_value(w: &mut W, v: &Value) {
    match v {
        Value::Null => w.u8(0),
        Value::Bool(false) => w.u8(1),
        Value::Bool(true) => w.u8(2),
        Value::Number(n) => w_number(w, 3, n),
        Value::String(s) => {
            w.u8(6);
            w.str(s);
        }
        Value::Array(a) => {
            w.u8(7);
            w.seq(a, w_value);
        }
        Value::Object(m) => {
            w.u8(8);
            w.len(m.len());
            for (k, x) in m {
                w.str(k);
                w_value(w, x);
            }
        }
    }
}

fn w_logic(w: &mut W, l: &Logic) {
    match l {
        Logic::Null => w.u8(0),
        Logic::Bool(false) => w.u8(1),
        Logic::Bool(true) => w.u8(2),
        Logic::Number(n) => w_num(w, 3, n),
        Logic::String(s) => {
            w.u8(6);
            w.str(s);
        }
        Logic::Array(a) => {
            w.u8(7);
            w.seq(a, w_logic);
        }
        Logic::Var(p) => {
            w.u8(8);
            w.str(p);
        }
        Logic::Call { op, single, args } => {
            w.u8(if *single { 10 } else { 9 });
            w.u8(operator_tag(*op));
            w.seq(args, w_logic);
        }
        Logic::Other(v) => {
            w.u8(11);
            w_tree(w, v);
        }
    }
}

fn w_map<K: AsRef<str>>(w: &mut W, m: &BTreeMap<K, Value>) {
    w.len(m.len());
    for (k, v) in m {
        w.str(k.as_ref());
        w_value(w, v);
    }
}

fn w_strs<S: AsRef<str>>(w: &mut W, xs: &[S]) {
    w.seq(xs, |w, s| w.str(s.as_ref()));
}

fn w_character(w: &mut W, c: &CharacterType) {
    let CharacterType { pattern, values } = c;
    w.u8(u8::from(pattern.is_some()) | u8::from(values.is_some()) << 1);
    if let Some(p) = pattern {
        w.str(p);
    }
    if let Some(v) = values {
        w_strs(w, v);
    }
}

/// Two bits per side: 0 absent, 1 inclusive, 3 exclusive.
fn bound_bits<T>(lower: &Option<Bound<T>>, upper: &Option<Bound<T>>) -> u8 {
    let b = |x: &Option<Bound<T>>| match x {
        None => 0,
        Some(Bound {
            exclusive: false, ..
        }) => 1,
        Some(Bound {
            exclusive: true, ..
        }) => 3,
    };
    b(lower) | b(upper) << 2
}

fn w_dims(w: &mut W, a: Option<u32>, b: Option<u32>) {
    w.u8(u8::from(a.is_some()) | u8::from(b.is_some()) << 1);
    if let Some(a) = a {
        w.u32(a);
    }
    if let Some(b) = b {
        w.u32(b);
    }
}

fn w_type(w: &mut W, t: &FieldType) {
    match t {
        FieldType::Logical => w.u8(0),
        FieldType::Integer(IntegerType {
            lower,
            upper,
            values,
        }) => {
            w.u8(1);
            w.u8(bound_bits(lower, upper) | u8::from(values.is_some()) << 4);
            if let Some(b) = lower {
                w.i32(b.value);
            }
            if let Some(b) = upper {
                w.i32(b.value);
            }
            if let Some(v) = values {
                w.seq(v, |w, x| w.i32(*x));
            }
        }
        FieldType::Double(DoubleType {
            lower,
            upper,
            finite,
            values,
        }) => {
            w.u8(2);
            w.u8(bound_bits(lower, upper)
                | u8::from(values.is_some()) << 4
                | u8::from(*finite) << 5);
            if let Some(b) = lower {
                w.f64(b.value);
            }
            if let Some(b) = upper {
                w.f64(b.value);
            }
            if let Some(v) = values {
                w.seq(v, |w, x| w.f64(*x));
            }
        }
        FieldType::Character(c) => {
            w.u8(3);
            w_character(w, c);
        }
        FieldType::Raw => w.u8(4),
        FieldType::Date => w.u8(5),
        FieldType::Posixct => w.u8(6),
        FieldType::Factor { levels, ordered } => {
            w.u8(7);
            w.u8(u8::from(levels.is_some()) | u8::from(*ordered) << 1);
            if let Some(l) = levels {
                w_strs(w, l);
            }
        }
        FieldType::Matrix {
            element,
            nrow,
            ncol,
        } => {
            w.u8(8);
            w_type(w, element);
            w_dims(w, *nrow, *ncol);
        }
        FieldType::Array { element, dim } => {
            w.u8(9);
            w_type(w, element);
            w.u8(u8::from(dim.is_some()));
            if let Some(d) = dim {
                w.seq(d, |w, x| w.u32(*x));
            }
        }
        FieldType::SparseMatrix {
            element,
            nrow,
            ncol,
        } => {
            w.u8(10);
            w_type(w, element);
            w_dims(w, *nrow, *ncol);
        }
        FieldType::MatrixOf { of } => {
            w.u8(11);
            w.qn(of);
        }
        FieldType::Table {
            columns,
            open,
            rules,
        } => {
            w.u8(12);
            w.u8(u8::from(*open));
            w.seq(columns, w_column);
            w.seq(rules, w_rule);
        }
        FieldType::TableOf { of } => {
            w.u8(13);
            w.qn(of);
        }
        FieldType::Json => w.u8(14),
        FieldType::HostClass {
            host,
            package,
            class,
            system,
        } => {
            w.u8(15);
            w.str(host);
            w.str(package);
            w.str(class);
            w_class_system(w, *system);
        }
        FieldType::Vec {
            element,
            min_length,
            max_length,
            unique,
        } => {
            w.u8(16);
            w_type(w, element);
            w.u8(u8::from(min_length.is_some())
                | u8::from(max_length.is_some()) << 1
                | u8::from(*unique) << 2);
            if let Some(n) = min_length {
                w.u64(*n);
            }
            if let Some(n) = max_length {
                w.u64(*n);
            }
        }
        FieldType::Option { inner } => {
            w.u8(17);
            w_type(w, inner);
        }
        FieldType::BtreeMap { value, keys } => {
            w.u8(18);
            w_type(w, value);
            w_character(w, keys);
        }
        FieldType::IndexMap { value, keys } => {
            w.u8(19);
            w_type(w, value);
            w_character(w, keys);
        }
        FieldType::Named { name } => {
            w.u8(20);
            w.qn(name);
        }
        FieldType::Dyn { r#trait } => {
            w.u8(21);
            w.qn(r#trait);
        }
        FieldType::Subtype { of } => {
            w.u8(22);
            w.qn(of);
        }
    }
}

fn w_field(w: &mut W, f: &FieldDef) {
    let FieldDef {
        name,
        ty,
        default,
        description,
        sensitive,
        unit,
        resolve,
        inherited_from,
        extensions,
    } = f;
    w.name(name);
    w.u8(u8::from(*sensitive)
        | u8::from(resolve.is_some()) << 1
        | u8::from(default.is_some()) << 4
        | u8::from(unit.is_some()) << 5
        | u8::from(inherited_from.is_some()) << 6
        | u8::from(extensions.is_some()) << 7);
    w_type(w, ty);
    w.str(description);
    if let Some(d) = default {
        w_tree(w, d);
    }
    if let Some(u) = unit {
        w.str(u);
    }
    match resolve {
        None => {}
        Some(Resolve::Computed { expr }) => {
            w.u8(0);
            w_logic(w, expr);
        }
        Some(Resolve::Context) => w.u8(1),
    }
    if let Some(q) = inherited_from {
        w.qn(q);
    }
    if let Some(e) = extensions {
        w_map(w, e);
    }
}

fn w_column(w: &mut W, c: &ColumnDef) {
    w_field(w, &c.field);
    w.u8(u8::from(c.unique) | u8::from(!c.required) << 1);
}

fn w_rule(w: &mut W, r: &RuleDef) {
    let RuleDef {
        invariant,
        when,
        message,
        id,
        inherited_from,
    } = r;
    w.u8(u8::from(when.is_some())
        | u8::from(message.is_some()) << 1
        | u8::from(id.is_some()) << 2
        | u8::from(inherited_from.is_some()) << 3);
    w_logic(w, invariant);
    if let Some(x) = when {
        w_logic(w, x);
    }
    if let Some(m) = message {
        w.str(m);
    }
    if let Some(i) = id {
        w.name(i);
    }
    if let Some(q) = inherited_from {
        w.qn(q);
    }
}

fn w_step(w: &mut W, s: &MigrationStep) {
    match s {
        MigrationStep::Rename { from, to } => {
            w.u8(0);
            w.name(from);
            w.name(to);
        }
        MigrationStep::Add { field, value } => {
            w.u8(1);
            w.name(field);
            w_value(w, value);
        }
        MigrationStep::Remove { field } => {
            w.u8(2);
            w.name(field);
        }
        MigrationStep::Split { field, into } => {
            w.u8(3);
            w.name(field);
            w.seq(into, |w, t| {
                let SplitTarget { field, expr } = t;
                w.name(field);
                w_logic(w, expr);
            });
        }
        MigrationStep::Merge { fields, into, expr } => {
            w.u8(4);
            w.seq(fields, |w, n| w.name(n));
            w.name(into);
            w_logic(w, expr);
        }
        MigrationStep::ChangeType { field, expr } => {
            w.u8(5);
            w.name(field);
            w_logic(w, expr);
        }
        MigrationStep::RenameVariant { from, to } => {
            w.u8(6);
            w.name(from);
            w.name(to);
        }
        MigrationStep::AddVariantField {
            variant,
            field,
            value,
        } => {
            w.u8(7);
            w.name(variant);
            w.name(field);
            w_value(w, value);
        }
        MigrationStep::UnitToStruct { variant, values } => {
            w.u8(8);
            w.name(variant);
            w.len(values.len());
            for (k, v) in values {
                w.name(k);
                w_value(w, v);
            }
        }
        MigrationStep::Nest { fields, into } => {
            w.u8(9);
            w.seq(fields, |w, n| w.name(n));
            w.name(into);
        }
        MigrationStep::ConvertUnit {
            field,
            from,
            to,
            factor,
        } => {
            w.u8(10);
            w.name(field);
            w.str(from);
            w.str(to);
            w.f64(*factor);
        }
    }
}

fn w_migration(w: &mut W, m: &Migration) {
    let Migration {
        from_version,
        steps,
    } = m;
    w.u32(*from_version);
    w.seq(steps, w_step);
}

fn w_struct(w: &mut W, s: &StructDef) {
    let StructDef {
        name,
        description,
        version,
        fields,
        extends,
        rules,
        migrations,
        aliases,
        extensions,
    } = s;
    w.name(name);
    w.str(description);
    w.u32(*version);
    w.u8(u8::from(extends.is_some()));
    if let Some(q) = extends {
        w.qn(q);
    }
    w.seq(fields, w_field);
    w.seq(rules, w_rule);
    w.seq(migrations, w_migration);
    w.seq(aliases, |w, q| w.qn(q));
    w_map(w, extensions);
}

fn w_enum(w: &mut W, e: &EnumDef) {
    let EnumDef {
        name,
        description,
        tag,
        version,
        variants,
        migrations,
        aliases,
        extensions,
    } = e;
    w.name(name);
    w.str(description);
    w.name(tag);
    w.u32(*version);
    w.seq(variants, |w, v| {
        let VariantDef {
            name,
            description,
            kind,
        } = v;
        w.name(name);
        w.str(description);
        match kind {
            VariantKind::Unit => w.u8(0),
            VariantKind::Struct { fields, rules } => {
                w.u8(1);
                w.seq(fields, w_field);
                w.seq(rules, w_rule);
            }
            VariantKind::Newtype { of } => {
                w.u8(2);
                w.qn(of);
            }
        }
    });
    w.seq(migrations, w_migration);
    w.seq(aliases, |w, q| w.qn(q));
    w_map(w, extensions);
}

fn w_type_def(w: &mut W, t: &TypeDef) {
    match t {
        TypeDef::Struct(s) => {
            w.u8(0);
            w_struct(w, s);
        }
        TypeDef::Enum(e) => {
            w.u8(1);
            w_enum(w, e);
        }
    }
}

fn w_target(w: &mut W, t: &TypeRef) {
    match t {
        TypeRef::Named { name } => {
            w.u8(0);
            w.qn(name);
        }
        TypeRef::HostClass {
            host,
            package,
            class,
            system,
        } => {
            w.u8(1);
            w.str(host);
            w.str(package);
            w.str(class);
            w_class_system(w, *system);
        }
        TypeRef::Any => w.u8(2),
    }
}

fn w_param(w: &mut W, p: &ParamDef) {
    match p {
        ParamDef::Name(n) => {
            w.u8(0);
            w.str(n);
        }
        ParamDef::Full(Param {
            name,
            dispatch,
            ty,
            default,
            host_default,
            description,
        }) => {
            w.u8(1);
            w.str(name);
            w.u8(u8::from(*dispatch)
                | u8::from(ty.is_some()) << 1
                | u8::from(default.is_some()) << 2
                | u8::from(host_default.is_some()) << 3);
            if let Some(t) = ty {
                w_type(w, t);
            }
            if let Some(d) = default {
                w_tree(w, d);
            }
            if let Some(HostDefault { host, text }) = host_default {
                w.str(host);
                w.str(text);
            }
            w.str(description);
        }
    }
}

fn w_trait(w: &mut W, t: &TraitDef) {
    let TraitDef {
        name,
        description,
        methods,
        required_fields,
        supertraits,
        tag,
        generic,
        extra_type_params,
    } = t;
    w.name(name);
    w.str(description);
    w.u8(u8::from(tag.is_some()) | u8::from(*generic) << 1);
    if let Some(t) = tag {
        w.name(t);
    }
    w.u8(*extra_type_params);
    w.seq(methods, |w, m| {
        let MethodDef {
            name,
            description,
            params,
            return_type,
            stochastic,
            has_default_impl,
        } = m;
        w.str(name.as_str());
        w.str(description);
        w.seq(params, w_param);
        w.u8(u8::from(*stochastic)
            | u8::from(*has_default_impl) << 1
            | u8::from(return_type.is_some()) << 2);
        match return_type {
            Some(ReturnType::SelfType(_)) => w.u8(0),
            Some(ReturnType::Type(t)) => {
                w.u8(1);
                w_type(w, t);
            }
            None => {}
        }
    });
    w.seq(required_fields, |w, f| {
        w.name(&f.name);
        w_type(w, &f.ty);
        w.str(&f.description);
    });
    w.seq(supertraits, |w, q| w.qn(q));
}

fn w_impl(w: &mut W, i: &ImplDef) {
    let ImplDef {
        r#trait,
        type_args,
        target,
        tag_value,
        methods,
    } = i;
    w.qn(r#trait);
    w.seq(type_args, w_target);
    w_target(w, target);
    w.u8(u8::from(tag_value.is_some()));
    if let Some(t) = tag_value {
        w.str(t);
    }
    w.seq(methods, |w, m| match m {
        ImplMethod::Name(n) => {
            w.u8(0);
            w.str(n.as_str());
        }
        ImplMethod::Typed(TypedImplMethod { name, return_type }) => {
            w.u8(1);
            w.str(name.as_str());
            w_type(w, return_type);
        }
    });
}

/// Encodes declarations as a record of the current format.
pub fn encode<'a>(entries: impl IntoIterator<Item = (&'a Name, DeclarationRef<'a>)>) -> Vec<u8> {
    let mut payload = W(Vec::with_capacity(512));
    let entries: Vec<_> = entries.into_iter().collect();
    payload.len(entries.len());
    for (namespace, d) in entries {
        payload.name(namespace);
        match d {
            DeclarationRef::Type(t) => {
                payload.u8(0);
                w_type_def(&mut payload, t);
            }
            DeclarationRef::Trait(t) => {
                payload.u8(1);
                w_trait(&mut payload, t);
            }
            DeclarationRef::Impl(i) => {
                payload.u8(2);
                w_impl(&mut payload, i);
            }
        }
    }
    let mut out = W(Vec::with_capacity(payload.0.len() + 24));
    out.0.extend_from_slice(&MAGIC);
    out.u32(FORMAT);
    out.str(env!("CARGO_PKG_VERSION"));
    out.0.extend_from_slice(&crc32c(&payload.0).to_le_bytes());
    out.0.extend_from_slice(&payload.0);
    out.0
}

// ---------------------------------------------------------------- reader

struct R<'a> {
    b: &'a [u8],
    depth: u32,
}

impl<'a> R<'a> {
    fn u8(&mut self) -> Res<u8> {
        let (&x, rest) = self.b.split_first().ok_or(RecordError::Truncated)?;
        self.b = rest;
        Ok(x)
    }
    fn uv(&mut self) -> Res<u64> {
        let mut x = 0u64;
        let mut shift = 0u32;
        loop {
            let b = self.u8()?;
            if shift == 63 && b > 1 {
                return Err(RecordError::Varint);
            }
            x |= u64::from(b & 0x7f) << shift;
            if b < 0x80 {
                // A last byte of zero after the first is an overlong form.
                if b == 0 && shift > 0 {
                    return Err(RecordError::Varint);
                }
                return Ok(x);
            }
            shift += 7;
            if shift > 63 {
                return Err(RecordError::Varint);
            }
        }
    }
    fn u32(&mut self) -> Res<u32> {
        u32::try_from(self.uv()?).map_err(|_| RecordError::Varint)
    }
    fn i64(&mut self) -> Res<i64> {
        let u = self.uv()?;
        Ok((u >> 1) as i64 ^ -((u & 1) as i64))
    }
    fn i32(&mut self) -> Res<i32> {
        i32::try_from(self.i64()?).map_err(|_| RecordError::Varint)
    }
    fn f64(&mut self) -> Res<f64> {
        let (h, rest) = self
            .b
            .split_first_chunk::<8>()
            .ok_or(RecordError::Truncated)?;
        self.b = rest;
        Ok(f64::from_bits(u64::from_le_bytes(*h)))
    }
    /// A count of elements that each take at least `min` bytes, checked
    /// against the bytes that remain before anything is allocated.
    fn len(&mut self, min: usize) -> Res<usize> {
        let n = self.uv()?;
        if n > (self.b.len() / min.max(1)) as u64 {
            return Err(RecordError::Truncated);
        }
        usize::try_from(n).map_err(|_| RecordError::Truncated)
    }
    fn str(&mut self) -> Res<&'a str> {
        let n = self.len(1)?;
        let (h, rest) = self.b.split_at(n);
        self.b = rest;
        std::str::from_utf8(h).map_err(|_| RecordError::Utf8)
    }
    fn name(&mut self) -> Res<Name> {
        let s = self.str()?;
        Name::new(s).map_err(|_| RecordError::Name(s.to_string()))
    }
    fn qn(&mut self) -> Res<QualifiedName> {
        Ok(QualifiedName::new(self.name()?, self.name()?))
    }
    fn seq<T>(&mut self, min: usize, mut f: impl FnMut(&mut Self) -> Res<T>) -> Res<Vec<T>> {
        let n = self.len(min)?;
        let mut v = Vec::with_capacity(n);
        for _ in 0..n {
            v.push(f(self)?);
        }
        Ok(v)
    }
    fn deeper<T>(&mut self, f: impl FnOnce(&mut Self) -> Res<T>) -> Res<T> {
        self.depth += 1;
        if self.depth > MAX_DEPTH {
            return Err(RecordError::Depth);
        }
        let x = f(self);
        self.depth -= 1;
        x
    }
    /// A flag byte whose bits outside `mask` are zero.
    fn flags(&mut self, mask: u8, what: &'static str) -> Res<u8> {
        let b = self.u8()?;
        if b & !mask != 0 {
            Err(RecordError::Tag(what, b))
        } else {
            Ok(b)
        }
    }
}

fn r_number(r: &mut R, k: u8) -> Res<Number> {
    Ok(match k {
        0 => r.uv()?.into(),
        1 => {
            let i = r.i64()?;
            if i >= 0 {
                return Err(RecordError::Tag("negative integer", 1));
            }
            i.into()
        }
        _ => Number::from_f64(r.f64()?).ok_or(RecordError::Tag("number", 2))?,
    })
}

/// An IR number at the tags of [`r_number`].
fn r_num(r: &mut R, k: u8) -> Res<crate::ir::Number> {
    use crate::ir::Number as N;
    Ok(match k {
        0 => N::U64(r.uv()?),
        1 => {
            let i = r.i64()?;
            if i >= 0 {
                return Err(RecordError::Tag("negative integer", 1));
            }
            N::I64(i)
        }
        _ => {
            let d = r.f64()?;
            if !d.is_finite() {
                return Err(RecordError::Tag("number", 2));
            }
            N::F64(d)
        }
    })
}

/// A tree at the tags of [`r_value`], its numbers kept as written.
fn r_tree(r: &mut R) -> Res<crate::tree::Tree> {
    use crate::tree::Tree as T;
    r.deeper(|r| {
        Ok(match r.u8()? {
            0 => T::Null,
            1 => T::Bool(false),
            2 => T::Bool(true),
            t @ 3..=5 => T::Num(r_num(r, t - 3)?),
            6 => T::Str(r.str()?.into()),
            7 => T::Array(r.seq(1, r_tree)?.into_boxed_slice()),
            8 => {
                let n = r.len(2)?;
                let mut m: Vec<(Box<str>, T)> = Vec::with_capacity(n);
                for _ in 0..n {
                    let k = r.str()?;
                    if m.iter().any(|(x, _)| &**x == k) {
                        return Err(RecordError::Key(k.to_owned()));
                    }
                    let k: Box<str> = k.into();
                    let v = r_tree(r)?;
                    m.push((k, v));
                }
                T::Object(m.into_boxed_slice())
            }
            t => return Err(RecordError::Tag("value", t)),
        })
    })
}

fn r_value(r: &mut R) -> Res<Value> {
    r.deeper(|r| {
        Ok(match r.u8()? {
            0 => Value::Null,
            1 => Value::Bool(false),
            2 => Value::Bool(true),
            t @ 3..=5 => Value::Number(r_number(r, t - 3)?),
            6 => Value::String(r.str()?.to_owned()),
            7 => Value::Array(r.seq(1, r_value)?),
            8 => {
                let n = r.len(2)?;
                let mut m = Map::with_capacity(n);
                for _ in 0..n {
                    let k = r.str()?.to_owned();
                    let v = r_value(r)?;
                    if m.contains_key(&k) {
                        return Err(RecordError::Key(k));
                    }
                    m.insert(k, v);
                }
                Value::Object(m)
            }
            t => return Err(RecordError::Tag("value", t)),
        })
    })
}

fn r_logic(r: &mut R) -> Res<Logic> {
    r.deeper(|r| {
        Ok(match r.u8()? {
            0 => Logic::Null,
            1 => Logic::Bool(false),
            2 => Logic::Bool(true),
            t @ 3..=5 => Logic::Number(r_num(r, t - 3)?),
            6 => Logic::String(r.str()?.into()),
            7 => Logic::Array(r.seq(1, r_logic)?.into_boxed_slice()),
            8 => Logic::Var(r.str()?.into()),
            t @ (9 | 10) => {
                let op = operator_of(r.u8()?)?;
                let args = r.seq(1, r_logic)?.into_boxed_slice();
                if t == 10 && args.len() != 1 {
                    return Err(RecordError::Tag("single operand", t));
                }
                Logic::Call {
                    op,
                    single: t == 10,
                    args,
                }
            }
            11 => Logic::Other(Box::new(r_tree(r)?)),
            t => return Err(RecordError::Tag("expression", t)),
        })
    })
}

/// A map whose keys are in strictly increasing order, as a `BTreeMap`
/// writes them.
fn r_map<'a, K: Ord>(
    r: &mut R<'a>,
    key: impl Fn(&mut R<'a>) -> Res<K>,
    text: impl Fn(&K) -> String,
) -> Res<BTreeMap<K, Value>> {
    let n = r.len(2)?;
    let mut m = BTreeMap::new();
    for _ in 0..n {
        let k = key(r)?;
        if m.last_key_value().is_some_and(|(last, _)| *last >= k) {
            return Err(RecordError::Key(text(&k)));
        }
        let v = r_value(r)?;
        m.insert(k, v);
    }
    Ok(m)
}

fn r_boxed_strs(r: &mut R) -> Res<Box<[Box<str>]>> {
    Ok(r.seq(1, |r| Ok(r.str()?.into()))?.into_boxed_slice())
}

fn r_character(r: &mut R) -> Res<CharacterType> {
    let f = r.flags(0b11, "character type")?;
    Ok(CharacterType {
        pattern: if f & 1 != 0 {
            Some(r.str()?.into())
        } else {
            None
        },
        values: if f & 2 != 0 {
            Some(r_boxed_strs(r)?)
        } else {
            None
        },
    })
}

fn r_bound<'a, T>(
    bits: u8,
    r: &mut R<'a>,
    read: impl Fn(&mut R<'a>) -> Res<T>,
) -> Res<Option<Bound<T>>> {
    Ok(match bits & 0b11 {
        0 => None,
        1 => Some(Bound {
            value: read(r)?,
            exclusive: false,
        }),
        3 => Some(Bound {
            value: read(r)?,
            exclusive: true,
        }),
        _ => return Err(RecordError::Tag("bound", bits)),
    })
}

fn r_dims(r: &mut R) -> Res<(Option<u32>, Option<u32>)> {
    let f = r.flags(0b11, "dimensions")?;
    Ok((
        if f & 1 != 0 { Some(r.u32()?) } else { None },
        if f & 2 != 0 { Some(r.u32()?) } else { None },
    ))
}

fn r_type(r: &mut R) -> Res<FieldType> {
    r.deeper(|r| {
        Ok(match r.u8()? {
            0 => FieldType::Logical,
            1 => {
                let f = r.flags(0x1f, "integer type")?;
                FieldType::Integer(IntegerType {
                    lower: r_bound(f, r, R::i32)?,
                    upper: r_bound(f >> 2, r, R::i32)?,
                    values: if f & 16 != 0 {
                        Some(r.seq(1, R::i32)?.into_boxed_slice())
                    } else {
                        None
                    },
                })
            }
            2 => {
                let f = r.flags(0x3f, "double type")?;
                FieldType::Double(DoubleType {
                    lower: r_bound(f, r, R::f64)?,
                    upper: r_bound(f >> 2, r, R::f64)?,
                    values: if f & 16 != 0 {
                        Some(r.seq(8, R::f64)?.into_boxed_slice())
                    } else {
                        None
                    },
                    finite: f & 32 != 0,
                })
            }
            3 => FieldType::Character(r_character(r)?),
            4 => FieldType::Raw,
            5 => FieldType::Date,
            6 => FieldType::Posixct,
            7 => {
                let f = r.flags(0b11, "factor type")?;
                FieldType::Factor {
                    levels: if f & 1 != 0 {
                        Some(r_boxed_strs(r)?)
                    } else {
                        None
                    },
                    ordered: f & 2 != 0,
                }
            }
            8 => {
                let element = Box::new(r_type(r)?);
                let (nrow, ncol) = r_dims(r)?;
                FieldType::Matrix {
                    element,
                    nrow,
                    ncol,
                }
            }
            9 => {
                let element = Box::new(r_type(r)?);
                let f = r.flags(1, "array type")?;
                FieldType::Array {
                    element,
                    dim: if f != 0 {
                        Some(r.seq(1, R::u32)?.into_boxed_slice())
                    } else {
                        None
                    },
                }
            }
            10 => {
                let element = Box::new(r_type(r)?);
                let (nrow, ncol) = r_dims(r)?;
                FieldType::SparseMatrix {
                    element,
                    nrow,
                    ncol,
                }
            }
            11 => FieldType::MatrixOf { of: r.qn()? },
            12 => {
                let open = r.flags(1, "table type")? != 0;
                FieldType::Table {
                    columns: r.seq(5, r_column)?.into_boxed_slice(),
                    open,
                    rules: r.seq(5, r_rule)?.into_boxed_slice(),
                }
            }
            13 => FieldType::TableOf { of: r.qn()? },
            14 => FieldType::Json,
            15 => FieldType::HostClass {
                host: r.str()?.into(),
                package: r.str()?.into(),
                class: r.str()?.into(),
                system: r_class_system(r)?,
            },
            16 => {
                let element = Box::new(r_type(r)?);
                let f = r.flags(0b111, "vec type")?;
                FieldType::Vec {
                    element,
                    min_length: if f & 1 != 0 { Some(r.uv()?) } else { None },
                    max_length: if f & 2 != 0 { Some(r.uv()?) } else { None },
                    unique: f & 4 != 0,
                }
            }
            17 => FieldType::Option {
                inner: Box::new(r_type(r)?),
            },
            18 => FieldType::BtreeMap {
                value: Box::new(r_type(r)?),
                keys: r_character(r)?,
            },
            19 => FieldType::IndexMap {
                value: Box::new(r_type(r)?),
                keys: r_character(r)?,
            },
            20 => FieldType::Named { name: r.qn()? },
            21 => FieldType::Dyn { r#trait: r.qn()? },
            22 => FieldType::Subtype { of: r.qn()? },
            t => return Err(RecordError::Tag("field type", t)),
        })
    })
}

fn r_field(r: &mut R) -> Res<FieldDef> {
    let name = r.name()?;
    let f = r.flags(0xf3, "field")?;
    let ty = r_type(r)?;
    let description = r.str()?.into();
    Ok(FieldDef {
        name,
        ty,
        default: if f & 16 != 0 {
            Some(Box::new(r_tree(r)?))
        } else {
            None
        },
        description,
        sensitive: f & 1 != 0,
        unit: if f & 32 != 0 {
            Some(r.str()?.into())
        } else {
            None
        },
        resolve: if f & 2 != 0 {
            Some(match r.u8()? {
                0 => Resolve::Computed { expr: r_logic(r)? },
                1 => Resolve::Context,
                t => return Err(RecordError::Tag("resolve", t)),
            })
        } else {
            None
        },
        inherited_from: if f & 64 != 0 {
            Some(Box::new(r.qn()?))
        } else {
            None
        },
        extensions: if f & 128 != 0 {
            Some(Box::new(r_map(
                r,
                |r| Ok(r.str()?.to_owned()),
                String::clone,
            )?))
        } else {
            None
        },
    })
}

fn r_column(r: &mut R) -> Res<ColumnDef> {
    let field = r_field(r)?;
    let f = r.flags(3, "column")?;
    Ok(ColumnDef {
        field,
        unique: f & 1 != 0,
        required: f & 2 == 0,
    })
}

fn r_rule(r: &mut R) -> Res<RuleDef> {
    let f = r.flags(15, "rule")?;
    let invariant = r_logic(r)?;
    let when = if f & 1 != 0 { Some(r_logic(r)?) } else { None };
    let message = if f & 2 != 0 {
        Some(r.str()?.to_owned())
    } else {
        None
    };
    let id = if f & 4 != 0 { Some(r.name()?) } else { None };
    let inherited_from = if f & 8 != 0 {
        Some(Box::new(r.qn()?))
    } else {
        None
    };
    Ok(RuleDef {
        invariant,
        when,
        message,
        id,
        inherited_from,
    })
}

fn r_step(r: &mut R) -> Res<MigrationStep> {
    Ok(match r.u8()? {
        0 => MigrationStep::Rename {
            from: r.name()?,
            to: r.name()?,
        },
        1 => MigrationStep::Add {
            field: r.name()?,
            value: r_value(r)?,
        },
        2 => MigrationStep::Remove { field: r.name()? },
        3 => MigrationStep::Split {
            field: r.name()?,
            into: r.seq(2, |r| {
                Ok(SplitTarget {
                    field: r.name()?,
                    expr: r_logic(r)?,
                })
            })?,
        },
        4 => MigrationStep::Merge {
            fields: r.seq(1, R::name)?,
            into: r.name()?,
            expr: r_logic(r)?,
        },
        5 => MigrationStep::ChangeType {
            field: r.name()?,
            expr: r_logic(r)?,
        },
        6 => MigrationStep::RenameVariant {
            from: r.name()?,
            to: r.name()?,
        },
        7 => MigrationStep::AddVariantField {
            variant: r.name()?,
            field: r.name()?,
            value: r_value(r)?,
        },
        8 => MigrationStep::UnitToStruct {
            variant: r.name()?,
            values: r_map(r, R::name, |n| n.to_string())?,
        },
        9 => MigrationStep::Nest {
            fields: r.seq(1, R::name)?,
            into: r.name()?,
        },
        10 => MigrationStep::ConvertUnit {
            field: r.name()?,
            from: r.str()?.to_owned(),
            to: r.str()?.to_owned(),
            factor: r.f64()?,
        },
        t => return Err(RecordError::Tag("migration step", t)),
    })
}

fn r_migration(r: &mut R) -> Res<Migration> {
    Ok(Migration {
        from_version: r.u32()?,
        steps: r.seq(1, r_step)?,
    })
}

fn r_struct(r: &mut R) -> Res<StructDef> {
    let name = r.name()?;
    let description = r.str()?.to_owned();
    let version = r.u32()?;
    let f = r.flags(1, "struct")?;
    let extends = if f & 1 != 0 { Some(r.qn()?) } else { None };
    Ok(StructDef {
        name,
        description,
        version,
        fields: r.seq(4, r_field)?,
        extends,
        rules: r.seq(5, r_rule)?,
        migrations: r.seq(2, r_migration)?,
        aliases: r.seq(2, R::qn)?,
        extensions: r_map(r, |r| Ok(r.str()?.to_owned()), String::clone)?,
    })
}

fn r_enum(r: &mut R) -> Res<EnumDef> {
    let name = r.name()?;
    let description = r.str()?.to_owned();
    let tag = r.name()?;
    let version = r.u32()?;
    let variants = r.seq(3, |r| {
        let name = r.name()?;
        let description = r.str()?.to_owned();
        let kind = match r.u8()? {
            0 => VariantKind::Unit,
            1 => VariantKind::Struct {
                fields: r.seq(4, r_field)?,
                rules: r.seq(5, r_rule)?,
            },
            2 => VariantKind::Newtype { of: r.qn()? },
            t => return Err(RecordError::Tag("variant kind", t)),
        };
        Ok(VariantDef {
            name,
            description,
            kind,
        })
    })?;
    Ok(EnumDef {
        name,
        description,
        tag,
        version,
        variants,
        migrations: r.seq(2, r_migration)?,
        aliases: r.seq(2, R::qn)?,
        extensions: r_map(r, |r| Ok(r.str()?.to_owned()), String::clone)?,
    })
}

fn r_type_def(r: &mut R) -> Res<TypeDef> {
    Ok(match r.u8()? {
        0 => TypeDef::Struct(r_struct(r)?),
        1 => TypeDef::Enum(r_enum(r)?),
        t => return Err(RecordError::Tag("type", t)),
    })
}

fn r_method_name(r: &mut R) -> Res<MethodName> {
    let s = r.str()?;
    MethodName::new(s).map_err(|_| RecordError::Name(s.to_string()))
}

fn w_class_system(w: &mut W, system: ClassSystem) {
    w.u8(match system {
        ClassSystem::S3 => 0,
        ClassSystem::S4 => 1,
        ClassSystem::Implicit => 2,
    });
}

fn r_class_system(r: &mut R) -> Res<ClassSystem> {
    Ok(match r.u8()? {
        0 => ClassSystem::S3,
        1 => ClassSystem::S4,
        2 => ClassSystem::Implicit,
        t => return Err(RecordError::Tag("class system", t)),
    })
}

fn r_target(r: &mut R) -> Res<TypeRef> {
    Ok(match r.u8()? {
        0 => TypeRef::Named { name: r.qn()? },
        1 => TypeRef::HostClass {
            host: r.str()?.to_owned(),
            package: r.str()?.to_owned(),
            class: r.str()?.to_owned(),
            system: r_class_system(r)?,
        },
        2 => TypeRef::Any,
        t => return Err(RecordError::Tag("impl target", t)),
    })
}

fn r_param(r: &mut R) -> Res<ParamDef> {
    Ok(match r.u8()? {
        0 => ParamDef::Name(r.str()?.to_owned()),
        1 => {
            let name = r.str()?.to_owned();
            let f = r.flags(0b1111, "parameter")?;
            let ty = if f & 2 != 0 { Some(r_type(r)?) } else { None };
            let default = if f & 4 != 0 {
                Some(Box::new(r_tree(r)?))
            } else {
                None
            };
            let host_default = if f & 8 != 0 {
                Some(HostDefault {
                    host: r.str()?.into(),
                    text: r.str()?.into(),
                })
            } else {
                None
            };
            ParamDef::Full(Param {
                name,
                dispatch: f & 1 != 0,
                ty,
                default,
                host_default,
                description: r.str()?.into(),
            })
        }
        t => return Err(RecordError::Tag("parameter", t)),
    })
}

fn r_trait(r: &mut R) -> Res<TraitDef> {
    let name = r.name()?;
    let description = r.str()?.to_owned();
    let f = r.flags(0b11, "trait")?;
    let tag = if f & 1 != 0 { Some(r.name()?) } else { None };
    let extra_type_params = r.u8()?;
    Ok(TraitDef {
        name,
        description,
        methods: r.seq(4, |r| {
            let name = r_method_name(r)?;
            let description = r.str()?.to_owned();
            let params = r.seq(2, r_param)?;
            let f = r.flags(0b111, "method")?;
            let return_type = if f & 4 != 0 {
                Some(match r.u8()? {
                    0 => ReturnType::SelfType(SelfMarker::SelfType),
                    1 => ReturnType::Type(Box::new(r_type(r)?)),
                    t => return Err(RecordError::Tag("return type", t)),
                })
            } else {
                None
            };
            Ok(MethodDef {
                name,
                description,
                params,
                return_type,
                stochastic: f & 1 != 0,
                has_default_impl: f & 2 != 0,
            })
        })?,
        required_fields: r.seq(3, |r| {
            Ok(RequiredField {
                name: r.name()?,
                ty: r_type(r)?,
                description: r.str()?.into(),
            })
        })?,
        supertraits: r.seq(2, R::qn)?,
        tag,
        generic: f & 2 != 0,
        extra_type_params,
    })
}

fn r_impl(r: &mut R) -> Res<ImplDef> {
    let r#trait = r.qn()?;
    let type_args = r.seq(1, r_target)?;
    let target = r_target(r)?;
    let f = r.flags(1, "impl")?;
    let tag_value = if f & 1 != 0 {
        Some(r.str()?.to_owned())
    } else {
        None
    };
    Ok(ImplDef {
        r#trait,
        type_args,
        target,
        tag_value,
        methods: r.seq(2, |r| {
            Ok(match r.u8()? {
                0 => ImplMethod::Name(r_method_name(r)?),
                1 => ImplMethod::Typed(TypedImplMethod {
                    name: r_method_name(r)?,
                    return_type: r_type(r)?,
                }),
                t => return Err(RecordError::Tag("impl method", t)),
            })
        })?,
    })
}

/// Reads the payload of the current format.
fn r_payload(r: &mut R) -> Res<Vec<Entry>> {
    r.seq(2, |r| {
        let namespace = r.name()?;
        let declaration = match r.u8()? {
            0 => Declaration::Type(r_type_def(r)?),
            1 => Declaration::Trait(r_trait(r)?),
            2 => Declaration::Impl(r_impl(r)?),
            t => return Err(RecordError::Tag("declaration", t)),
        };
        Ok(Entry {
            namespace,
            declaration,
        })
    })
}

/// The format version of a record and the version of `rtemis-r8` that wrote
/// it, read from the header without checking the payload.
pub fn header(bytes: &[u8]) -> Result<(u32, String), RecordError> {
    let rest = bytes.strip_prefix(&MAGIC).ok_or(RecordError::Magic)?;
    let mut r = R { b: rest, depth: 0 };
    let format = r.u32()?;
    let writer = r.str()?.to_owned();
    Ok((format, writer))
}

/// Reads a record: checks the magic bytes, the format version, and the
/// checksum, then decodes the entries into the current IR types.
pub fn decode(bytes: &[u8]) -> Result<Vec<Entry>, RecordError> {
    let rest = bytes.strip_prefix(&MAGIC).ok_or(RecordError::Magic)?;
    let mut r = R { b: rest, depth: 0 };
    let format = r.u32()?;
    let writer = r.str()?;
    if format != FORMAT {
        return Err(RecordError::Newer {
            format,
            writer: writer.to_string(),
        });
    }
    let (crc, payload) = r.b.split_first_chunk::<4>().ok_or(RecordError::Truncated)?;
    if u32::from_le_bytes(*crc) != crc32c(payload) {
        return Err(RecordError::Checksum);
    }
    let mut r = R {
        b: payload,
        depth: 0,
    };
    let entries = match format {
        FORMAT => r_payload(&mut r)?,
        _ => unreachable!("format checked above"),
    };
    if !r.b.is_empty() {
        return Err(RecordError::Trailing);
    }
    Ok(entries)
}

#[cfg(test)]
mod tests {
    use super::crc32c;

    #[test]
    fn crc32c_matches_the_check_value() {
        // The check value of CRC-32C (RFC 3720, appendix B.4).
        assert_eq!(crc32c(b"123456789"), 0xE306_9283);
        assert_eq!(crc32c(b""), 0);
    }
}
