//! R8's default layout of a value: a header line naming the type, then one
//! line per field, a nested value's fields indented under it. A vector, a
//! matrix, or a factor shows its element type, its length or shape, and the
//! head of its items, read through the range readers of [`Access`]; a
//! table shows its shape and column names, and a sparse matrix its shape
//! and the number of stored entries. No part of the layout reads a whole
//! vector.
//! spec: R8/class-system#text-output

use std::borrow::Cow;

use crate::access::Access;
use crate::ir::{FieldDef, FieldType, TypeDef, VariantKind};
use crate::name::QualifiedName;
use crate::rules::TypeLookup;
use crate::text::{Line, Role, Span, Text};

/// The settings of a layout.
#[derive(Debug, Clone)]
pub struct LayoutOptions {
    /// The number of items a vector's head shows.
    pub head: usize,
    /// Text after the top-level header, such as a Rust-stored value's size,
    /// as a `summary` span.
    pub summary: Option<String>,
    /// The number of entries a level of a [`Record`] shows.
    pub max_entries: usize,
    /// The number of lines the layout of a [`Record`] shows.
    pub max_lines: usize,
    /// The depth of the deepest entries of a [`Record`] that its layout
    /// shows.
    pub max_depth: usize,
    /// The number of characters in a line of a table.
    pub width: usize,
    /// Characters outside ASCII are written.
    pub unicode: bool,
}

impl Default for LayoutOptions {
    fn default() -> Self {
        LayoutOptions {
            head: 8,
            summary: None,
            max_entries: 32,
            max_lines: 128,
            max_depth: 3,
            width: 80,
            unicode: true,
        }
    }
}

/// The text of a nested value that its host supplies in place of the
/// default layout, such as the text of a `Display` impl: `None` for the
/// default layout.
pub type DisplayText<'d, A> = dyn FnMut(A, &QualifiedName) -> Option<Text> + 'd;

/// The layout of the value `v` of the named type `ty`.
pub fn layout<'a, A: Access<'a>>(
    v: A,
    ty: &QualifiedName,
    lookup: &dyn TypeLookup,
    options: &LayoutOptions,
) -> Text {
    layout_with(v, ty, lookup, options, &mut |_, _| None)
}

/// The layout of the value `v` of the named type `ty`, where `display`
/// supplies the text of nested values: its first line follows the field's
/// name, and its other lines follow the field, one level deeper.
pub fn layout_with<'a, A: Access<'a>>(
    v: A,
    ty: &QualifiedName,
    lookup: &dyn TypeLookup,
    options: &LayoutOptions,
    display: &mut DisplayText<'_, A>,
) -> Text {
    let mut lines = Vec::new();
    if let Some(TypeDef::Table(t)) = lookup.find(ty)
        && let Some(lines) = table_grid(ty, t, v, options)
    {
        return Text { lines };
    }
    let mut header = header_spans(v, ty, lookup);
    if let Some(s) = &options.summary {
        header.push(Span::new(" ", Role::Plain));
        header.push(Span::new(s.clone(), Role::Summary));
    }
    lines.push(Line::new(0, header));
    fields(v, ty, lookup, options, display, 1, &mut lines);
    Text { lines }
}

/// `<ns::Type>` or `<ns::Type@Variant>`: brackets, the namespace muted, the
/// type's name, and the variant.
fn header_spans<'a, A: Access<'a>>(v: A, ty: &QualifiedName, lookup: &dyn TypeLookup) -> Vec<Span> {
    let mut out = vec![
        Span::new("<", Role::Bracket),
        Span::new(format!("{}::", ty.namespace), Role::Muted),
        Span::new(ty.name.to_string(), Role::TypeName),
    ];
    if let Some(TypeDef::Enum(e)) = lookup.find(ty)
        && let Ok(variant) = v.variant(ty, e)
    {
        out.push(Span::new("@", Role::Bracket));
        out.push(Span::new(variant.into_owned(), Role::Variant));
    }
    out.push(Span::new(">", Role::Bracket));
    // A table's rows and columns.
    if let Some(TypeDef::Table(t)) = lookup.find(ty)
        && let Ok(t) = v.table(&t.columns, t.open)
    {
        let extent = format!(" {} x {}", count(t.rows), count(t.columns.len() as u64));
        out.push(Span::new(extent, Role::Summary));
    }
    out
}

/// The fields of the value of the named type `ty`, of its variant for an
/// enum, as lines indented `depth` levels. The value of a field declared
/// `sensitive` is withheld.
fn fields<'a, A: Access<'a>>(
    v: A,
    ty: &QualifiedName,
    lookup: &dyn TypeLookup,
    options: &LayoutOptions,
    display: &mut DisplayText<'_, A>,
    depth: usize,
    lines: &mut Vec<Line>,
) {
    let defs: &[FieldDef] = match lookup.find(ty) {
        Some(TypeDef::Struct(s)) => &s.fields,
        Some(TypeDef::Enum(e)) => {
            let variant = v.variant(ty, e).map(Cow::into_owned).unwrap_or_default();
            match e
                .variants
                .iter()
                .find(|x| x.name.as_str() == variant)
                .map(|x| &x.kind)
            {
                Some(VariantKind::Struct { fields, .. }) => fields,
                Some(VariantKind::Newtype { of }) => match lookup.find(of) {
                    Some(TypeDef::Struct(s)) => &s.fields,
                    _ => return,
                },
                _ => return,
            }
        }
        Some(TypeDef::Table(t)) => return table_lines(t, v, options, depth, lines),
        None => return,
    };
    for (i, f) in defs.iter().enumerate() {
        let x = v.member_at(i, f.name.as_str());
        if f.sensitive && !x.is_absent() {
            let line = vec![
                Span::new(f.name.to_string(), Role::FieldName),
                Span::new(": ", Role::Plain),
                Span::new("<sensitive>", Role::Muted),
            ];
            lines.push(Line::new(depth, line));
            continue;
        }
        field_lines(
            Span::new(f.name.to_string(), Role::FieldName),
            &f.ty,
            x,
            lookup,
            options,
            display,
            depth,
            lines,
        );
    }
}

/// The rows of a table that its grid shows at each end, and the number of rows up
/// to which it shows every one.
const END_ROWS: usize = 5;
const ALL_ROWS: usize = 12;
/// The cells up to which a grid counts the missing ones.
const COUNTED_CELLS: u64 = 250_000;
/// The characters of a cell of text.
const CELL_WIDTH: usize = 24;

/// A column of a table's grid: its header, its cells, and their widths.
struct GridColumn {
    name: String,
    ty: String,
    role: Role,
    unit: Option<String>,
    missing: u64,
    /// The text of each cell shown, `None` for a missing one.
    cells: Vec<Option<String>>,
    right: bool,
    width: usize,
}

/// The table `v` as a grid: the rows and columns it has, the names of the columns
/// with their types, units, and missing cells, the first and the last rows, and
/// what the grid leaves out.
fn table_grid<'a, A: Access<'a>>(
    ty: &QualifiedName,
    def: &crate::ir::TableDef,
    v: A,
    options: &LayoutOptions,
) -> Option<Vec<Line>> {
    let t = v.table(&def.columns, def.open).ok()?;
    let (n, k) = (t.rows as usize, t.columns.len());
    let times = if options.unicode { "\u{2a09}" } else { "x" };
    let of = |n: usize, what: &str| {
        format!(
            "{} {what}{}",
            count(n as u64),
            if n == 1 { "" } else { "s" }
        )
    };
    // `R8::Raven` is a raven, and another table is named.
    let label = if ty.namespace.as_str() == "R8" && ty.name.as_str() == "Raven" {
        vec![Span::new("raven", Role::TypeName)]
    } else {
        vec![
            Span::new(format!("{}::", ty.namespace), Role::Muted),
            Span::new(ty.name.to_string(), Role::TypeName),
        ]
    };
    let mut header = vec![Span::new("<", Role::Bracket)];
    header.extend(label);
    header.push(Span::new(" table", Role::Muted));
    header.push(Span::new(">", Role::Bracket));
    header.push(Span::new(
        format!(" {} {times} {}", of(n, "row"), of(k, "column")),
        Role::Summary,
    ));
    let mut lines = vec![Line::new(0, header)];
    if k == 0 {
        return Some(lines);
    }
    let shown: Vec<usize> = if n <= ALL_ROWS {
        (0..n).collect()
    } else {
        (0..END_ROWS).chain(n - END_ROWS..n).collect()
    };
    let counted = t.rows.saturating_mul(k as u64) <= COUNTED_CELLS;
    let numbers = count(n as u64).chars().count();
    let mut room = options.width.saturating_sub(numbers);
    let mut columns: Vec<GridColumn> = Vec::new();
    for (name, cells) in &t.columns {
        let declared = def
            .columns
            .iter()
            .find(|c| c.name.as_str() == name.as_ref());
        let (ft, unit, sensitive) = match declared {
            Some(c) => (
                c.ty.clone(),
                c.unit.as_deref().map(str::to_string),
                c.sensitive,
            ),
            None => (
                v.extra_column_type(name, *cells).ok()?,
                v.column_facts(name).unwrap_or_default().unit,
                false,
            ),
        };
        let inner = match &ft {
            FieldType::Option { inner, .. } => inner.as_ref(),
            other => other,
        };
        let c = grid_column(name, inner, unit, sensitive, *cells, &shown, n, counted);
        // Two spaces stand between columns; the first column is always shown.
        if c.width + 2 > room && !columns.is_empty() {
            break;
        }
        room = room.saturating_sub(c.width + 2);
        columns.push(c);
    }
    let margin = " ".repeat(numbers);
    // The text before a cell, and the cell; the last one ends its line.
    let aligned = |s: &str, c: &GridColumn, last: bool| {
        let pad = " ".repeat(c.width.saturating_sub(s.chars().count()));
        match (c.right, last) {
            (true, _) if s.is_empty() && last => (String::new(), String::new()),
            (true, _) => (format!("  {pad}"), s.to_string()),
            (false, true) => ("  ".to_string(), s.to_string()),
            (false, false) => ("  ".to_string(), format!("{s}{pad}")),
        }
    };
    let row = |text: &dyn Fn(&GridColumn) -> (String, Role)| {
        let mut line = vec![Span::new(margin.clone(), Role::Plain)];
        // Nothing follows the last text of the line.
        let texts: Vec<(String, Role)> = columns.iter().map(text).collect();
        let end = texts.iter().rposition(|(s, _)| !s.is_empty());
        for (i, (c, (s, role))) in columns.iter().zip(texts).enumerate() {
            if end.is_none_or(|e| i > e) {
                break;
            }
            let (pad, s) = aligned(&s, c, Some(i) == end);
            line.push(Span::new(pad, Role::Plain));
            line.push(Span::new(s, role));
        }
        Line::new(0, line)
    };
    lines.push(row(&|c| (c.name.clone(), Role::FieldName)));
    lines.push(row(&|c| (c.ty.clone(), c.role)));
    if columns.iter().any(|c| c.unit.is_some()) {
        lines.push(row(&|c| {
            let unit = c.unit.as_ref().map_or(String::new(), |u| format!("[{u}]"));
            (unit, Role::Summary)
        }));
    }
    if columns.iter().any(|c| c.missing > 0) {
        lines.push(row(&|c| {
            let missing = if c.missing > 0 {
                format!("{} NA", count(c.missing))
            } else {
                String::new()
            };
            (missing, Role::Highlight)
        }));
    }
    let hidden_rows = n - shown.len();
    for (i, &r) in shown.iter().enumerate() {
        if hidden_rows > 0 && i == END_ROWS {
            let dots = format!(
                "{}  ... {} not shown",
                " ".repeat(numbers),
                of(hidden_rows, "row")
            );
            lines.push(Line::new(0, vec![Span::new(dots, Role::Muted)]));
        }
        let number = count(r as u64 + 1);
        let pad = " ".repeat(numbers - number.chars().count());
        let mut line = vec![Span::new(format!("{pad}{number}"), Role::Muted)];
        for (j, c) in columns.iter().enumerate() {
            let (s, role) = match &c.cells[i] {
                Some(s) => (s.as_str(), Role::Plain),
                None => ("NA", Role::Highlight),
            };
            let (pad, s) = aligned(s, c, j + 1 == columns.len());
            line.push(Span::new(pad, Role::Plain));
            line.push(Span::new(s, role));
        }
        lines.push(Line::new(0, line));
    }
    let hidden = k - columns.len();
    if hidden > 0 {
        let names: Vec<String> = t.columns[columns.len()..]
            .iter()
            .map(|(name, _)| name.to_string())
            .collect();
        let mut line = format!("{} not shown: ", of(hidden, "column"));
        for (i, name) in names.iter().enumerate() {
            let sep = if i == 0 { "" } else { ", " };
            if i > 0 && line.chars().count() + name.chars().count() + 5 > options.width {
                line.push_str(", ...");
                break;
            }
            line.push_str(sep);
            line.push_str(name);
        }
        lines.push(Line::new(0, vec![Span::new(line, Role::Muted)]));
    }
    if !counted {
        let line = format!(
            "missing cells are not counted in {} cells",
            count(t.rows.saturating_mul(k as u64))
        );
        lines.push(Line::new(0, vec![Span::new(line, Role::Muted)]));
    }
    Some(lines)
}

#[allow(clippy::too_many_arguments)]
fn grid_column<'a, A: Access<'a>>(
    name: &str,
    ty: &FieldType,
    unit: Option<String>,
    sensitive: bool,
    cells: A,
    shown: &[usize],
    n: usize,
    counted: bool,
) -> GridColumn {
    let numeric = matches!(ty, FieldType::Double(_) | FieldType::Integer(_));
    let mut missing = 0u64;
    let factor = match ty {
        FieldType::Factor { levels, ordered } => cells.factor(levels.as_deref(), *ordered).ok(),
        _ => None,
    };
    if counted {
        match &factor {
            Some(f) => f.codes.each_in(0, n, |_, c| {
                missing += u64::from(c == crate::access::MISSING_CODE);
            }),
            None => {
                missing = (0..n).filter(|&i| cells.item(i).is_absent()).count() as u64;
            }
        }
    }
    let text: Vec<Option<String>> = if sensitive {
        shown
            .iter()
            .map(|_| Some("<sensitive>".to_string()))
            .collect()
    } else if let Some(f) = &factor {
        shown
            .iter()
            .map(|&i| {
                let mut code = [crate::access::MISSING_CODE];
                f.codes.read(i, &mut code);
                f.level(code[0]).map(clipped)
            })
            .collect()
    } else if matches!(ty, FieldType::Double(_)) {
        let x: Vec<Option<f64>> = shown
            .iter()
            .map(|&i| {
                let item = cells.item(i);
                if item.is_absent() {
                    None
                } else {
                    item.double().ok()
                }
            })
            .collect();
        doubles_text(&x)
    } else {
        shown
            .iter()
            .map(|&i| {
                let x = cells.item(i);
                if x.is_absent() {
                    return None;
                }
                Some(match ty {
                    FieldType::Character(_) => x.character().map_or("?".into(), |s| clipped(&s)),
                    _ => scalar_text(ty, x),
                })
            })
            .collect()
    };
    let mut label = element_label(ty);
    if let FieldType::Factor { ordered: true, .. } = ty {
        label = "ordered".into();
    }
    let chars = |s: &str| s.chars().count();
    let width = [
        chars(name),
        chars(&label),
        unit.as_ref().map_or(0, |u| chars(u) + 2),
        if missing > 0 {
            chars(&count(missing)) + 3
        } else {
            0
        },
    ]
    .into_iter()
    .chain(text.iter().map(|c| c.as_ref().map_or(2, |s| chars(s))))
    .max()
    .unwrap_or(0);
    GridColumn {
        name: name.to_string(),
        ty: label,
        role: type_role(ty),
        unit,
        missing,
        cells: text,
        right: numeric,
        width,
    }
}

/// A text of one line and at most [`CELL_WIDTH`] characters.
fn clipped(s: &str) -> String {
    let s: String = s
        .chars()
        .map(|c| if c.is_control() { ' ' } else { c })
        .collect();
    if s.chars().count() <= CELL_WIDTH {
        return s;
    }
    let head: String = s.chars().take(CELL_WIDTH - 3).collect();
    format!("{head}...")
}

/// The doubles of a column with one number of decimals: four significant digits
/// of the smallest, and no more than a value has.
fn doubles_text(x: &[Option<f64>]) -> Vec<Option<String>> {
    let finite = || x.iter().flatten().copied().filter(|v| v.is_finite());
    let top = finite().fold(0.0f64, |m, v| m.max(v.abs()));
    let low = finite()
        .filter(|v| *v != 0.0)
        .fold(f64::INFINITY, |m, v| m.min(v.abs()));
    if top >= 1e9 || (low.is_finite() && low < 1e-4) {
        return x.iter().map(|v| v.map(number)).collect();
    }
    let decimals = finite()
        .map(|v| {
            if v == 0.0 {
                return 0;
            }
            let wanted = (3 - v.abs().log10().floor() as i32).max(0) as usize;
            let text = format!("{v:.wanted$}");
            match text.split_once('.') {
                Some((_, d)) => d.trim_end_matches('0').len(),
                None => 0,
            }
        })
        .max()
        .unwrap_or(0);
    x.iter()
        .map(|v| {
            v.map(|v| {
                if v.is_finite() {
                    format!("{v:.decimals$}")
                } else {
                    number(v)
                }
            })
        })
        .collect()
}

/// The columns of a table, one line each: its name, the type of its cells, its
/// unit, and its first cells, aligned; at most `LISTED_COLUMNS` of them.
fn table_lines<'a, A: Access<'a>>(
    def: &crate::ir::TableDef,
    v: A,
    options: &LayoutOptions,
    depth: usize,
    lines: &mut Vec<Line>,
) {
    const LISTED_COLUMNS: usize = 20;
    let Ok(t) = v.table(&def.columns, def.open) else {
        return;
    };
    let total = t.columns.len();
    let shown: Vec<_> = t.columns.iter().take(LISTED_COLUMNS).collect();
    let width = shown.iter().map(|(k, _)| k.chars().count()).max();
    for (k, cells) in shown {
        let declared = def.columns.iter().find(|c| c.name.as_str() == k.as_ref());
        let (ty, unit, sensitive) = match declared {
            Some(c) => (
                c.ty.clone(),
                c.unit.as_deref().map(str::to_string),
                c.sensitive,
            ),
            None => match v.extra_column_type(k, *cells) {
                Ok(ft) => (ft, v.column_facts(k).unwrap_or_default().unit, false),
                Err(_) => continue,
            },
        };
        let pad = width.unwrap_or(0) - k.chars().count();
        let mut line = vec![
            Span::new(k.to_string(), Role::FieldName),
            Span::new(format!("{}  ", " ".repeat(pad)), Role::Plain),
        ];
        let inner = match &ty {
            FieldType::Option { inner, .. } => inner.as_ref(),
            other => other,
        };
        line.push(Span::new(element_label(inner), type_role(inner)));
        if let Some(u) = &unit {
            line.push(Span::new(format!(" [{u}]"), Role::Summary));
        }
        line.push(Span::new(": ", Role::Plain));
        if sensitive {
            line.push(Span::new("<sensitive>", Role::Muted));
        } else {
            let n = t.rows as usize;
            let first: Vec<String> = (0..n.min(options.head))
                .map(|i| cell_text(inner, *cells, i))
                .collect();
            line.push(Span::new(with_ellipsis(first, n), Role::Scalar));
        }
        lines.push(Line::new(depth, line));
    }
    if total > LISTED_COLUMNS {
        let more = count((total - LISTED_COLUMNS) as u64);
        let line = vec![Span::new(format!("{more} more columns"), Role::Muted)];
        lines.push(Line::new(depth, line));
    }
}

/// The text of cell `i` of a column of the type `ty`.
fn cell_text<'a, A: Access<'a>>(ty: &FieldType, cells: A, i: usize) -> String {
    if let FieldType::Factor { levels, ordered } = ty {
        let Ok(f) = cells.factor(levels.as_deref(), *ordered) else {
            return String::new();
        };
        let mut code = [crate::access::MISSING_CODE];
        f.codes.read(i, &mut code);
        return f.level(code[0]).unwrap_or("NA").to_string();
    }
    let x = cells.item(i);
    if x.is_absent() {
        "NA".to_string()
    } else {
        scalar_text(ty, x)
    }
}

/// The lines of the field `name` of the type `ft` whose value is `x`, at
/// `depth`: `name: value`, then the fields of a nested value one level
/// deeper, or the other lines of the text `display` supplies for it.
#[allow(clippy::too_many_arguments)]
fn field_lines<'a, A: Access<'a>>(
    name: Span,
    ft: &FieldType,
    x: A,
    lookup: &dyn TypeLookup,
    options: &LayoutOptions,
    display: &mut DisplayText<'_, A>,
    depth: usize,
    lines: &mut Vec<Line>,
) {
    let mut line = vec![name, Span::new(": ", Role::Plain)];
    let nested = value(ft, x, lookup, options, display, &mut line);
    lines.push(Line::new(depth, line));
    match nested {
        Nested::Fields(name) => {
            fields(x, &name, lookup, options, display, depth + 1, lines);
        }
        Nested::Lines(more) => lines.extend(
            more.into_iter()
                .map(|l| Line::new(depth + 1 + l.depth, l.spans)),
        ),
        Nested::None => {}
    }
}

/// The name of an entry of a [`Record`]: a key, or a position, which the
/// layout shows as `muted`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EntryName {
    /// The name's text, such as `model` or `[[2]]`.
    pub text: String,
    /// The name is a position, not a key.
    pub positional: bool,
}

impl EntryName {
    /// The name's span: `field_name` for a key, `muted` for a position.
    fn span(&self) -> Span {
        let role = if self.positional {
            Role::Muted
        } else {
            Role::FieldName
        };
        Span::new(self.text.clone(), role)
    }
}

/// An entry of a [`Record`].
pub enum Entry<'a, A: Access<'a>> {
    /// A field: `value`, of the type `ty`, which the host infers.
    Field {
        /// The entry's name.
        name: EntryName,
        /// The field type of the value.
        ty: FieldType,
        /// The value.
        value: A,
    },
    /// A nested record, shown as `name: label shape` with its entries one
    /// level deeper, such as `params: list[3]`.
    Record {
        /// The entry's name.
        name: EntryName,
        /// The record's kind, such as `list`, as `type_other`.
        label: String,
        /// The record's shape, such as `[3]`, as `summary`.
        shape: String,
        /// The record's entries.
        record: Box<dyn Record<'a, A> + 'a>,
    },
}

/// A host object without an R8 type whose entries the host classifies,
/// such as an R list, a data frame, or a Python dictionary. The layout reads
/// only the entries it shows.
pub trait Record<'a, A: Access<'a>> {
    /// The number of entries.
    fn len(&self) -> usize;
    /// Whether the record has no entries.
    fn is_empty(&self) -> bool {
        self.len() == 0
    }
    /// The entry at `index`, from 0.
    fn entry(&self, index: usize) -> Entry<'a, A>;
}

/// The layout of a record: a header, `<label>` and `summary`, then a line
/// per entry. A level shows at most `max_entries` entries, then a `muted`
/// line counting the rest; nested records at `max_depth` show their label
/// and shape alone; and the layout ends with a `muted` line `...` after
/// `max_lines` lines.
pub fn layout_record<'a, A: Access<'a>>(
    label: &str,
    summary: &str,
    record: &dyn Record<'a, A>,
    lookup: &dyn TypeLookup,
    options: &LayoutOptions,
    display: &mut DisplayText<'_, A>,
) -> Text {
    let mut lines = vec![Line::new(
        0,
        vec![
            Span::new("<", Role::Bracket),
            Span::new(label.to_string(), Role::TypeOther),
            Span::new(">", Role::Bracket),
            Span::new(" ", Role::Plain),
            Span::new(summary.to_string(), Role::Summary),
        ],
    )];
    record_lines(record, lookup, options, display, 1, &mut lines);
    Text { lines }
}

/// Ends a layout at `max_lines` lines with a `muted` line `...` at `depth`.
fn cap(lines: &mut Vec<Line>, options: &LayoutOptions, depth: usize) {
    lines.truncate(options.max_lines);
    lines.push(Line::new(depth, vec![Span::new("...", Role::Muted)]));
}

/// The lines of the entries of `record` at `depth`; `false` once the layout
/// reaches `max_lines`.
fn record_lines<'a, A: Access<'a>>(
    record: &dyn Record<'a, A>,
    lookup: &dyn TypeLookup,
    options: &LayoutOptions,
    display: &mut DisplayText<'_, A>,
    depth: usize,
    lines: &mut Vec<Line>,
) -> bool {
    let n = record.len();
    let shown = n.min(options.max_entries);
    for i in 0..shown {
        if lines.len() >= options.max_lines {
            cap(lines, options, depth);
            return false;
        }
        match record.entry(i) {
            Entry::Field { name, ty, value } => {
                field_lines(
                    name.span(),
                    &ty,
                    value,
                    lookup,
                    options,
                    display,
                    depth,
                    lines,
                );
            }
            Entry::Record {
                name,
                label,
                shape,
                record: child,
            } => {
                lines.push(Line::new(
                    depth,
                    vec![
                        name.span(),
                        Span::new(": ", Role::Plain),
                        Span::new(label, Role::TypeOther),
                        Span::new(shape, Role::Summary),
                    ],
                ));
                if depth < options.max_depth
                    && !record_lines(child.as_ref(), lookup, options, display, depth + 1, lines)
                {
                    return false;
                }
            }
        }
    }
    if lines.len() > options.max_lines {
        cap(lines, options, depth);
        return false;
    }
    if shown < n {
        lines.push(Line::new(
            depth,
            vec![Span::new(
                format!("... {} more", count((n - shown) as u64)),
                Role::Muted,
            )],
        ));
    }
    true
}

/// What follows a field's line.
enum Nested {
    /// Nothing.
    None,
    /// The fields of a nested value of the named type.
    Fields(QualifiedName),
    /// The lines of a nested value's text after its first.
    Lines(Vec<Line>),
}

/// The header of the nested value `x` of the named type `name` appended to
/// `line`, or the first line of the text `display` supplies for it.
fn nested<'a, A: Access<'a>>(
    x: A,
    name: QualifiedName,
    lookup: &dyn TypeLookup,
    display: &mut DisplayText<'_, A>,
    line: &mut Vec<Span>,
) -> Nested {
    match display(x, &name) {
        Some(t) => {
            let mut it = t.lines.into_iter();
            if let Some(first) = it.next() {
                line.extend(first.spans);
            }
            Nested::Lines(it.collect())
        }
        None => {
            line.extend(header_spans(x, &name, lookup));
            Nested::Fields(name)
        }
    }
}

/// Appends the spans of a field's value to `line`, and returns what follows
/// the line.
fn value<'a, A: Access<'a>>(
    ft: &FieldType,
    x: A,
    lookup: &dyn TypeLookup,
    options: &LayoutOptions,
    display: &mut DisplayText<'_, A>,
    line: &mut Vec<Span>,
) -> Nested {
    if x.is_absent() {
        line.push(Span::new("NULL", Role::Muted));
        return Nested::None;
    }
    let ft = match ft {
        FieldType::Option { inner, .. } => inner.as_ref(),
        other => other,
    };
    match ft {
        FieldType::Named { name } => {
            return nested(x, name.clone(), lookup, display, line);
        }
        FieldType::Dyn { .. } | FieldType::Subtype { .. } => {
            return match crate::access::concrete(x, ft, lookup).expect("a dynamic field type") {
                Ok((name, _)) => nested(x, name, lookup, display, line),
                Err(_) => {
                    line.push(Span::new(element_label(ft), Role::Muted));
                    Nested::None
                }
            };
        }
        FieldType::Logical
        | FieldType::Integer(_)
        | FieldType::Double(_)
        | FieldType::Character(_)
        | FieldType::Date
        | FieldType::Posixct => {
            line.push(Span::new(scalar_text(ft, x), Role::Scalar));
        }
        FieldType::Raw => {
            let n = x.bytes().map(|b| b.len()).unwrap_or(0);
            label(
                line,
                "raw",
                Role::TypeOther,
                format!("[{}]", count(n as u64)),
            );
        }
        FieldType::Json => line.push(Span::new("json", Role::TypeOther)),
        FieldType::HostClass { class, .. } => line.push(Span::new(class.clone(), Role::TypeOther)),
        FieldType::Vec { element, .. } => {
            let n = x.vec(element).unwrap_or(0);
            label(
                line,
                &element_label(element),
                type_role(element),
                format!("[{}]", count(n as u64)),
            );
            if is_scalar(element) && n > 0 {
                line.push(Span::new(" ", Role::Plain));
                line.push(Span::new(head(element, x, n, options.head), Role::Scalar));
            }
        }
        FieldType::Factor { levels, ordered } => match x.factor(levels.as_deref(), *ordered) {
            Ok(f) => {
                let n = f.codes.len();
                label(
                    line,
                    "factor",
                    Role::TypeCategorical,
                    format!("[{}]", count(n as u64)),
                );
                let k = n.min(options.head);
                let mut shown = Vec::with_capacity(k);
                f.codes.each_in(0, k, |_, c| {
                    shown.push(if c <= 0 {
                        "NA".to_string()
                    } else {
                        f.levels
                            .get(c as usize - 1)
                            .map(|l| l.to_string())
                            .unwrap_or_else(|| "NA".to_string())
                    });
                });
                if !shown.is_empty() {
                    line.push(Span::new(" ", Role::Plain));
                    line.push(Span::new(with_ellipsis(shown, n), Role::Scalar));
                }
                line.push(Span::new(
                    format!(" ({} levels)", count(f.levels.len() as u64)),
                    Role::Muted,
                ));
            }
            Err(_) => line.push(Span::new("factor", Role::TypeCategorical)),
        },
        FieldType::Matrix { element, .. } | FieldType::Array { element, .. } => {
            match x.dense(element) {
                Ok(d) => {
                    let shape: Vec<String> = d.shape.iter().map(|s| count(*s)).collect();
                    label(
                        line,
                        &element_label(element),
                        type_role(element),
                        format!("[{}]", shape.join(" x ")),
                    );
                    if d.len > 0 {
                        line.push(Span::new(" ", Role::Plain));
                        line.push(Span::new(
                            head(element, d.data, d.len, options.head),
                            Role::Scalar,
                        ));
                    }
                }
                Err(_) => line.push(Span::new(element_label(element), type_role(element))),
            }
        }
        FieldType::MatrixOf { of } => {
            line.push(Span::new(format!("matrix of {of}"), Role::Summary));
        }
        FieldType::SparseMatrix { element, .. } => match x.sparse(element) {
            Ok(sp) => {
                line.push(Span::new("sparse ", Role::Summary));
                label(
                    line,
                    &element_label(element),
                    type_role(element),
                    format!(
                        "[{} x {}], {} stored",
                        count(sp.shape[0]),
                        count(sp.shape[1]),
                        count(sp.len as u64)
                    ),
                );
            }
            Err(_) => line.push(Span::new("sparse matrix", Role::TypeOther)),
        },
        FieldType::Table { columns, open, .. } => match x.table(columns, *open) {
            Ok(t) => {
                let names: Vec<String> = t.columns.iter().map(|(n, _)| n.to_string()).collect();
                label(
                    line,
                    "table",
                    Role::TypeOther,
                    format!("[{} x {}]", count(t.rows), count(t.columns.len() as u64)),
                );
                if !names.is_empty() {
                    line.push(Span::new(" ", Role::Plain));
                    let k = names.len();
                    line.push(Span::new(
                        with_ellipsis(names.into_iter().take(options.head).collect(), k),
                        Role::Scalar,
                    ));
                }
            }
            Err(_) => line.push(Span::new("table", Role::TypeOther)),
        },
        FieldType::TableOf { of } => {
            line.push(Span::new(format!("table of {of}"), Role::Summary));
        }
        FieldType::BtreeMap { value: vt, .. } | FieldType::IndexMap { value: vt, .. } => {
            let ordered = matches!(ft, FieldType::IndexMap { .. });
            let n = x.map(vt, ordered).unwrap_or(0);
            label(
                line,
                if ordered { "index_map" } else { "btree_map" },
                Role::TypeOther,
                format!("[{}]", count(n as u64)),
            );
            if is_scalar(vt) && n > 0 {
                let mut shown = Vec::new();
                x.each_entry(|i, e| {
                    if i < options.head
                        && let Ok((k, item)) = e
                    {
                        shown.push(format!("{k}: {}", scalar_text(vt, item)));
                    }
                });
                line.push(Span::new(" ", Role::Plain));
                line.push(Span::new(with_ellipsis(shown, n), Role::Scalar));
            }
        }
        FieldType::Option { .. } => {}
    }
    Nested::None
}

/// Whether a type is a scalar, or `option()` of one.
fn is_scalar(ft: &FieldType) -> bool {
    match ft {
        FieldType::Option { inner, .. } => inner.is_scalar(),
        other => other.is_scalar(),
    }
}

/// Appends a base-type label: the type's name with `role`, then its shape
/// as a `summary` span.
fn label(line: &mut Vec<Span>, name: &str, role: Role, shape: String) {
    line.push(Span::new(name.to_string(), role));
    line.push(Span::new(shape, Role::Summary));
}

/// The role of the name of the base type `ft`, after rtemislive's column
/// types: `type_number` for double, `type_integer` for integer,
/// `type_categorical` for factor, `type_string` for character,
/// `type_boolean` for logical, `type_datetime` for Date or POSIXct, and
/// `type_other` for every other type.
fn type_role(ft: &FieldType) -> Role {
    match ft {
        FieldType::Option { inner, .. } => type_role(inner),
        FieldType::Double(_) => Role::TypeNumber,
        FieldType::Integer(_) => Role::TypeInteger,
        FieldType::Factor { .. } => Role::TypeCategorical,
        FieldType::Character(_) => Role::TypeString,
        FieldType::Logical => Role::TypeBoolean,
        FieldType::Date | FieldType::Posixct => Role::TypeDatetime,
        _ => Role::TypeOther,
    }
}

/// The name of an element type in a vector's summary.
fn element_label(ft: &FieldType) -> String {
    match ft {
        FieldType::Option { inner, .. } => element_label(inner),
        FieldType::Logical => "logical".into(),
        FieldType::Integer(_) => "integer".into(),
        FieldType::Double(_) => "double".into(),
        FieldType::Character(_) => "character".into(),
        FieldType::Raw => "raw".into(),
        FieldType::Date => "Date".into(),
        FieldType::Posixct => "POSIXct".into(),
        FieldType::Factor { .. } => "factor".into(),
        FieldType::Json => "json".into(),
        FieldType::Named { name } => name.to_string(),
        FieldType::Dyn { r#trait } => format!("dyn {}", r#trait),
        FieldType::Subtype { of } => format!("dyn {of}"),
        FieldType::Vec { element, .. } => format!("vec<{}>", element_label(element)),
        _ => "value".into(),
    }
}

/// The first `k` of `n` items of a sequence of a scalar type, separated by
/// spaces, with `...` when items remain.
fn head<'a, A: Access<'a>>(element: &FieldType, v: A, n: usize, k: usize) -> String {
    let k = n.min(k);
    let base = match element {
        FieldType::Option { inner, .. } => inner.as_ref(),
        other => other,
    };
    let mut shown: Vec<String> = Vec::with_capacity(k);
    let na = || "NA".to_string();
    match base {
        FieldType::Double(_) => v.each_double_in(0, k, |_, r| {
            shown.push(r.ok().flatten().map(number).unwrap_or_else(na));
        }),
        FieldType::Integer(_) => v.each_integer_in(0, k, |_, r| {
            shown.push(r.ok().flatten().map(|i| i.to_string()).unwrap_or_else(na));
        }),
        FieldType::Logical => v.each_logical_in(0, k, |_, r| {
            shown.push(
                r.ok()
                    .flatten()
                    .map(|b| if b { "TRUE" } else { "FALSE" }.to_string())
                    .unwrap_or_else(na),
            );
        }),
        FieldType::Character(_) => v.each_character_in(0, k, |_, r| {
            shown.push(r.ok().flatten().map(|s| quote(&s)).unwrap_or_else(na));
        }),
        FieldType::Date => v.each_date_in(0, k, |_, r| {
            shown.push(r.ok().flatten().map(date_text).unwrap_or_else(na));
        }),
        FieldType::Posixct => v.each_datetime_in(0, k, |_, r| {
            shown.push(r.ok().flatten().map(datetime_text).unwrap_or_else(na));
        }),
        _ => {}
    }
    with_ellipsis(shown, n)
}

/// Items separated by spaces, with `...` when they are fewer than `n`.
fn with_ellipsis(mut shown: Vec<String>, n: usize) -> String {
    if shown.len() < n {
        shown.push("...".to_string());
    }
    shown.join(" ")
}

/// The text of a scalar value.
fn scalar_text<'a, A: Access<'a>>(ft: &FieldType, x: A) -> String {
    if x.is_absent() {
        return "NA".to_string();
    }
    let ft = match ft {
        FieldType::Option { inner, .. } => inner.as_ref(),
        other => other,
    };
    let text = match ft {
        FieldType::Logical => x
            .logical()
            .map(|b| if b { "TRUE" } else { "FALSE" }.to_string()),
        FieldType::Integer(_) => x.integer().map(|i| i.to_string()),
        FieldType::Double(_) => x.double().map(number),
        FieldType::Character(_) => x.character().map(|s| quote(&s)),
        FieldType::Date => x.date().map(date_text),
        FieldType::Posixct => x.datetime().map(datetime_text),
        _ => return "?".to_string(),
    };
    text.unwrap_or_else(|_| "?".to_string())
}

/// A double as R prints it by default: up to 7 significant digits, `NA`,
/// `NaN`, `Inf`, and `-Inf`, and scientific notation for a magnitude below
/// 1e-4 or at or above 1e15.
pub fn number(x: f64) -> String {
    if x.is_nan() {
        // R's NA_real_ is the NaN whose low word is 1954.
        return if x.to_bits() & 0xFFFF_FFFF == 1954 {
            "NA".to_string()
        } else {
            "NaN".to_string()
        };
    }
    if x.is_infinite() {
        return if x > 0.0 { "Inf" } else { "-Inf" }.to_string();
    }
    if x == 0.0 {
        return "0".to_string();
    }
    let magnitude = x.abs();
    if !(1e-4..1e15).contains(&magnitude) {
        let s = format!("{x:.6e}");
        let (mantissa, exp) = s.split_once('e').unwrap_or((&s, "0"));
        let mantissa = trim_zeros(mantissa);
        let exp: i32 = exp.parse().unwrap_or(0);
        return format!(
            "{mantissa}e{}{:02}",
            if exp < 0 { "-" } else { "+" },
            exp.abs()
        );
    }
    let digits = magnitude.log10().floor() as i32;
    let decimals = (6 - digits).max(0) as usize;
    trim_zeros(&format!("{x:.decimals$}")).to_string()
}

/// A decimal without trailing zeros after its point.
fn trim_zeros(s: &str) -> &str {
    if s.contains('.') {
        s.trim_end_matches('0').trim_end_matches('.')
    } else {
        s
    }
}

/// A string in double quotes, with `"`, `\`, and control characters escaped.
fn quote(s: &str) -> String {
    let mut out = String::with_capacity(s.len() + 2);
    out.push('"');
    for c in s.chars() {
        match c {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\t' => out.push_str("\\t"),
            '\r' => out.push_str("\\r"),
            c if c.is_control() => out.push_str(&format!("\\u{{{:x}}}", c as u32)),
            c => out.push(c),
        }
    }
    out.push('"');
    out
}

/// A count with commas between thousands: `1,000,000`.
pub fn count(n: u64) -> String {
    let s = n.to_string();
    let mut out = String::with_capacity(s.len() + s.len() / 3);
    for (i, c) in s.chars().enumerate() {
        if i > 0 && (s.len() - i).is_multiple_of(3) {
            out.push(',');
        }
        out.push(c);
    }
    out
}

/// The civil date of a number of days since 1970-01-01, `YYYY-MM-DD`.
fn date_text(days: i64) -> String {
    let (y, m, d) = civil(days);
    format!("{y:04}-{m:02}-{d:02}")
}

/// A UTC instant of microseconds since 1970-01-01T00:00:00Z,
/// `YYYY-MM-DD HH:MM:SS UTC`, with the fraction of a second when it has one.
fn datetime_text(micros: i64) -> String {
    let secs = micros.div_euclid(1_000_000);
    let frac = micros.rem_euclid(1_000_000);
    let (y, m, d) = civil(secs.div_euclid(86_400));
    let s = secs.rem_euclid(86_400);
    let time = format!("{:02}:{:02}:{:02}", s / 3600, (s / 60) % 60, s % 60);
    let fraction = if frac == 0 {
        String::new()
    } else {
        format!(".{}", format!("{frac:06}").trim_end_matches('0'))
    };
    format!("{y:04}-{m:02}-{d:02} {time}{fraction} UTC")
}

/// The year, month, and day of a number of days since 1970-01-01, in the
/// proleptic Gregorian calendar (Howard Hinnant's `civil_from_days`).
fn civil(days: i64) -> (i64, u32, u32) {
    let z = days + 719_468;
    let era = z.div_euclid(146_097);
    let doe = z.rem_euclid(146_097);
    let yoe = (doe - doe / 1460 + doe / 36_524 - doe / 146_096) / 365;
    let doy = doe - (365 * yoe + yoe / 4 - yoe / 100);
    let mp = (5 * doy + 2) / 153;
    let d = (doy - (153 * mp + 2) / 5 + 1) as u32;
    let m = if mp < 10 { mp + 3 } else { mp - 9 } as u32;
    let y = yoe + era * 400 + i64::from(m <= 2);
    (y, m, d)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn numbers_print_as_r_prints_them() {
        assert_eq!(number(1.0), "1");
        assert_eq!(number(0.1), "0.1");
        assert_eq!(number(1.0 / 3.0), "0.3333333");
        assert_eq!(number(123456.789), "123456.8");
        assert_eq!(number(-2.5), "-2.5");
        assert_eq!(number(1e-5), "1e-05");
        assert_eq!(number(1.5e20), "1.5e+20");
        assert_eq!(number(f64::INFINITY), "Inf");
        assert_eq!(number(f64::from_bits(0x7FF0_0000_0000_07A2)), "NA");
        assert_eq!(number(f64::NAN), "NaN");
    }

    #[test]
    fn a_value_lays_out_with_its_header_fields_and_heads() {
        use crate::access::Json;
        use crate::ir::IrDocument;
        use crate::rules::DocumentSet;
        let doc = IrDocument::from_json(
            r#"{"$ir": 1, "namespace": "demo", "types": [
              {"kind": "enum", "name": "Kernel", "description": "", "tag": "type", "version": 1,
               "variants": [{"name": "Linear", "description": "", "kind": "unit"},
                            {"name": "Rbf", "description": "", "kind": "struct",
                             "fields": [{"name": "gamma", "type": {"kind": "double"}, "description": ""}]}]},
              {"kind": "struct", "name": "Fit", "description": "", "version": 1, "fields": [
                {"name": "name", "type": {"kind": "character"}, "description": ""},
                {"name": "coef", "type": {"kind": "vec", "element": {"kind": "double"}}, "description": ""},
                {"name": "k", "type": {"kind": "option", "inner": {"kind": "integer"}}, "description": ""},
                {"name": "token", "type": {"kind": "character"}, "description": "", "sensitive": true},
                {"name": "kernel", "type": {"kind": "named", "name": "demo::Kernel"}, "description": ""}]}]}"#,
        )
        .expect("the document reads");
        let set = DocumentSet::from(vec![doc]);
        let v: serde_json::Value = serde_json::from_str(
            r#"{"name": "a", "coef": [0.1, 0.25, 1, 2, 3, 4, 5, 6, 7, 8],
                "token": "s3cret",
                "kernel": {"type": "Rbf", "gamma": 0.5}}"#,
        )
        .expect("the value reads");
        let q = QualifiedName::parse("demo::Fit").expect("a name");
        let t = layout(
            Json::new(&v),
            &q,
            &set,
            &LayoutOptions {
                summary: Some("1.00 KiB".into()),
                ..LayoutOptions::default()
            },
        );
        assert_eq!(
            crate::text::encode_plain(&t),
            "<demo::Fit> 1.00 KiB\n  name: \"a\"\n  coef: double[10] 0.1 0.25 1 2 3 4 5 6 ...\n  k: NULL\n  token: <sensitive>\n  kernel: <demo::Kernel@Rbf>\n    gamma: 0.5"
        );
        assert_eq!(t.lines[0].spans[1], Span::new("demo::", Role::Muted));
        assert_eq!(t.lines[5].spans[5], Span::new("@", Role::Bracket));
        assert_eq!(t.lines[2].spans[2], Span::new("double", Role::TypeNumber));
        assert_eq!(t.lines[2].spans[3], Span::new("[10]", Role::Summary));
        let depths: Vec<usize> = t.lines.iter().map(|l| l.depth).collect();
        assert_eq!(depths, [0, 1, 1, 1, 1, 1, 2]);
        let mut calls = Vec::new();
        let shown = layout_with(
            Json::new(&v),
            &q,
            &set,
            &LayoutOptions::default(),
            &mut |_, name| {
                calls.push(name.to_string());
                Some(Text {
                    lines: vec![
                        vec![Span::new("RBF kernel", Role::UserText)].into(),
                        Line::new(1, vec![Span::new("gamma = 0.5", Role::UserText)]),
                    ],
                })
            },
        );
        assert_eq!(calls, ["demo::Kernel"]);
        assert_eq!(
            crate::text::encode_plain(&shown),
            "<demo::Fit>\n  name: \"a\"\n  coef: double[10] 0.1 0.25 1 2 3 4 5 6 ...\n  k: NULL\n  token: <sensitive>\n  kernel: RBF kernel\n      gamma = 0.5"
        );
    }

    /// A record over a JSON array of entries, each `[name, value]`, with
    /// `null` names as positions.
    struct JsonRecord<'a>(&'a [serde_json::Value]);

    impl<'a> Record<'a, crate::access::Json<'a>> for JsonRecord<'a> {
        fn len(&self) -> usize {
            self.0.len()
        }

        fn entry(&self, index: usize) -> Entry<'a, crate::access::Json<'a>> {
            use crate::ir::DoubleType;
            let pair = self.0[index].as_array().expect("an entry is [name, value]");
            let name = match pair[0].as_str() {
                Some(n) => EntryName {
                    text: n.to_string(),
                    positional: false,
                },
                None => EntryName {
                    text: format!("[[{}]]", index + 1),
                    positional: true,
                },
            };
            match &pair[1] {
                serde_json::Value::Array(items) if items.iter().all(|i| i.is_array()) => {
                    Entry::Record {
                        name,
                        label: "list".into(),
                        shape: format!("[{}]", items.len()),
                        record: Box::new(JsonRecord(items)),
                    }
                }
                serde_json::Value::Array(_) => Entry::Field {
                    name,
                    ty: FieldType::Vec {
                        element: Box::new(FieldType::Double(DoubleType::default())),
                        min_length: None,
                        max_length: None,
                        unique: false,
                    },
                    value: crate::access::Json::new(&pair[1]),
                },
                _ => Entry::Field {
                    name,
                    ty: FieldType::Double(DoubleType::default()),
                    value: crate::access::Json::new(&pair[1]),
                },
            }
        }
    }

    #[test]
    fn a_record_lays_out_its_entries_within_its_limits() {
        let no_types = crate::rules::DocumentSet::default();
        let v: serde_json::Value = serde_json::from_str(
            r#"[["n_trees", 500], ["auc", [0.81, 0.84, 0.79]],
                [null, [["a", 1], ["b", [["c", [["d", 4]]]]]]],
                ["x", 1], ["y", 2], ["z", 3]]"#,
        )
        .expect("the entries read");
        let entries = v.as_array().expect("an array");
        let options = LayoutOptions {
            max_entries: 5,
            ..LayoutOptions::default()
        };
        let t = layout_record(
            "list",
            "6 elements",
            &JsonRecord(entries),
            &no_types,
            &options,
            &mut |_, _| None,
        );
        assert_eq!(
            crate::text::encode_plain(&t),
            "<list> 6 elements\n  n_trees: 500\n  auc: double[3] 0.81 0.84 0.79\n  [[3]]: list[2]\n    a: 1\n    b: list[1]\n      c: list[1]\n  x: 1\n  y: 2\n  ... 1 more"
        );
        assert_eq!(t.lines[0].spans[1], Span::new("list", Role::TypeOther));
        assert_eq!(t.lines[3].spans[0], Span::new("[[3]]", Role::Muted));
        let short = layout_record(
            "list",
            "6 elements",
            &JsonRecord(entries),
            &no_types,
            &LayoutOptions {
                max_lines: 3,
                ..options
            },
            &mut |_, _| None,
        );
        assert_eq!(
            crate::text::encode_plain(&short),
            "<list> 6 elements\n  n_trees: 500\n  auc: double[3] 0.81 0.84 0.79\n  ..."
        );
    }

    #[test]
    fn counts_dates_and_strings() {
        assert_eq!(count(0), "0");
        assert_eq!(count(1_000), "1,000");
        assert_eq!(count(100_000_000), "100,000,000");
        assert_eq!(date_text(0), "1970-01-01");
        assert_eq!(date_text(19_723), "2024-01-01");
        assert_eq!(date_text(-1), "1969-12-31");
        assert_eq!(datetime_text(1_500_000), "1970-01-01 00:00:01.5 UTC");
        assert_eq!(quote("a\"b\n"), "\"a\\\"b\\n\"");
    }
}
