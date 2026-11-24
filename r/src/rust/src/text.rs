//! Text output for R: the default layout of an R8 value, the encoders, the
//! theme, and the resolution of the output type, through the core's `text`
//! and `layout` modules.
//!
//! R holds text as an `r8_text` object: a character vector of plain lines,
//! indentation included, with the spans of each role other than `plain` as
//! one-based character positions (`line`, `start`, `end`, `role`) and each
//! line's depth. The core holds a line as its depth and spans, without the
//! indentation. [`text_to_r`] and [`text_from_r`] convert between the two,
//! from characters at the R side to UTF-8 strings at the Rust side.
//! spec: R8/class-system#text-output

use std::collections::HashSet;

use extendr_api::SEXP;
use extendr_api::prelude::*;
use extendr_api::{Error, Result};
use extendr_ffi::{R_xlen_t, SEXPTYPE, VECTOR_ELT};
use rtemis_r8::ir::build::host_class;
use rtemis_r8::ir::{CharacterType, ClassSystem, DoubleType, FieldType, IntegerType};
use rtemis_r8::layout::{
    Entry, EntryName, LayoutOptions, Record, count, layout_record, layout_with,
};
use rtemis_r8::name::QualifiedName;
use rtemis_r8::rules::TypeLookup;
use rtemis_r8::text::{
    ALL_ROLES, Color, ColorDepth, Facts, HostKind, HtmlStyle, Line, OutputType, Role, Span, Style,
    Text, Theme, css, determine, encode_ansi, encode_html, encode_markdown, encode_plain,
};

use crate::rview::{
    RView, attr, attributes_checked, attributes_unchecked, inherits, is_null, nil, sexptype,
    string_elt, strings, sym_class, sym_dim, sym_names, table_rows, xlen,
};

fn fail(message: impl Into<String>) -> Error {
    Error::Other(message.into())
}

/// The R lists of an `r8_text`: `lines`, `line`, `start`, `end`, `role`,
/// and `depth`.
fn text_to_r(t: &Text) -> List {
    let mut lines = Vec::with_capacity(t.lines.len());
    let mut depth = Vec::with_capacity(t.lines.len());
    let (mut line, mut start, mut end, mut role) = (vec![], vec![], vec![], vec![]);
    for (i, l) in t.lines.iter().enumerate() {
        let mut text = l.indent();
        let mut pos = 2 * l.depth;
        for s in &l.spans {
            let p = s.plain();
            let n = p.chars().count();
            if s.role != Role::Plain && n > 0 {
                line.push((i + 1) as i32);
                start.push((pos + 1) as i32);
                end.push((pos + n) as i32);
                role.push(s.role.name());
            }
            pos += n;
            text.push_str(&p);
        }
        lines.push(text);
        depth.push(l.depth as i32);
    }
    list!(
        lines = lines,
        line = line,
        start = start,
        end = end,
        role = role,
        depth = depth
    )
}

/// The elements of an integer vector; an empty vector is read without its
/// data pointer, which R sets to 1 for a vector of length zero and
/// extendr's slices would read as unaligned.
fn int_vec(v: &Robj) -> Option<Vec<i32>> {
    match v.rtype() {
        Rtype::Integers if v.len() == 0 => Some(Vec::new()),
        Rtype::Integers => v.as_integer_vector(),
        _ => None,
    }
}

/// The elements of a character vector, read as [`int_vec`] reads integers.
pub(crate) fn str_vec(v: &Robj) -> Option<Vec<String>> {
    match v.rtype() {
        Rtype::Strings if v.len() == 0 => Some(Vec::new()),
        // Text to show: a string that is no text shows replacement characters.
        Rtype::Strings => {
            crate::utf8::all(v).map(|s| s.into_iter().map(Option::unwrap_or_default).collect())
        }
        _ => None,
    }
}

/// An integer vector of an `r8_text` list.
fn ints(x: &List, name: &str) -> Result<Vec<i32>> {
    x.dollar(name)
        .ok()
        .and_then(|v| int_vec(&v))
        .ok_or_else(|| fail(format!("an `r8_text` holds `{name}` as integers")))
}

/// The core's text of an `r8_text` list: each line without the two spaces
/// per level of its depth, its spans in order, and the text between them
/// as `plain` spans.
fn text_from_r(x: &List) -> Result<Text> {
    let lines: Vec<String> = x
        .dollar("lines")
        .ok()
        .and_then(|v| str_vec(&v))
        .ok_or_else(|| fail("an `r8_text` holds its lines as character"))?;
    let depth = ints(x, "depth")?;
    let line = ints(x, "line")?;
    let start = ints(x, "start")?;
    let end = ints(x, "end")?;
    let role: Vec<String> = x
        .dollar("role")
        .ok()
        .and_then(|v| str_vec(&v))
        .ok_or_else(|| fail("an `r8_text` holds its roles as character"))?;
    if depth.len() != lines.len() || [start.len(), end.len(), role.len()] != [line.len(); 3] {
        return Err(fail(
            "the spans and depths of an `r8_text` do not match its lines",
        ));
    }
    let mut spans: Vec<Vec<(usize, usize, Role)>> = vec![Vec::new(); lines.len()];
    for k in 0..line.len() {
        let r = Role::from_name(&role[k])
            .ok_or_else(|| fail(format!("`{}` is not a role of R8", role[k])))?;
        let i = line[k] as usize;
        if i == 0 || i > lines.len() || start[k] < 1 || end[k] < start[k] {
            return Err(fail("a span of an `r8_text` lies outside its lines"));
        }
        spans[i - 1].push((start[k] as usize - 1, end[k] as usize, r));
    }
    let mut out = Vec::with_capacity(lines.len());
    for (i, text) in lines.iter().enumerate() {
        let chars: Vec<char> = text.chars().collect();
        let d = depth[i].max(0) as usize;
        let indent = (2 * d).min(chars.len());
        let mut cursor = indent;
        let mut line_spans = Vec::new();
        let mut these = std::mem::take(&mut spans[i]);
        these.sort_by_key(|s| s.0);
        for (s, e, r) in these {
            if e > chars.len() || s < cursor {
                return Err(fail(
                    "the spans of an `r8_text` overlap or exceed its lines",
                ));
            }
            if s > cursor {
                line_spans.push(Span::new(
                    chars[cursor..s].iter().collect::<String>(),
                    Role::Plain,
                ));
            }
            line_spans.push(Span::new(chars[s..e].iter().collect::<String>(), r));
            cursor = e;
        }
        if cursor < chars.len() {
            line_spans.push(Span::new(
                chars[cursor..].iter().collect::<String>(),
                Role::Plain,
            ));
        }
        out.push(Line::new(d, line_spans));
    }
    Ok(Text { lines: out })
}

/// The qualified names of the types whose nested values show the text of
/// `display`.
fn display_set(display_types: &Robj) -> Result<HashSet<String>> {
    Ok(str_vec(display_types)
        .ok_or_else(|| fail("`display_types` is character"))?
        .into_iter()
        .collect())
}

/// The text of a nested value of the registered declaration whose type is in
/// `shown`, the lists of an
/// `r8_text` that the R function `display` returns for it; the first error
/// goes to `failure`, and the layout of every later value is the default.
fn display_text<'f, 'a>(
    shown: &'f HashSet<String>,
    display: &'f Function,
    failure: &'f mut Option<Error>,
) -> impl FnMut(RView<'a>, &QualifiedName) -> Option<Text> + 'f {
    move |v: RView<'a>, name: &QualifiedName| {
        let current = v
            .sexp()
            .and_then(crate::value::id)
            .is_some_and(crate::value::current);
        if failure.is_some() || !current || !shown.contains(&name.to_string()) {
            return None;
        }
        let result = v
            .robj()
            .and_then(|r| attributes_checked(|| display.call(pairlist!(r))))
            .and_then(|t| {
                if t.is_null() {
                    return Ok(None);
                }
                let l: List = t.try_into()?;
                text_from_r(&l).map(Some)
            });
        match result {
            Ok(t) => t,
            Err(e) => {
                *failure = Some(e);
                None
            }
        }
    }
}

/// The default layout of an R8 value, as the lists of an `r8_text`.
///
/// Nested values whose type is in `display_types` show the text that
/// `display` returns for them, the lists of an `r8_text`, or the default
/// layout when it returns `NULL`. The registry is copied before the layout
/// reads the value, so that `display` may declare and construct values.
///
/// @param x An R8 value.
/// @param display_types Character: The qualified names of the types whose
///   values show the text of `display`.
/// @param display Function: The text of a nested value.
/// @param summary Character or `NULL`: Text after the header, as a
///   `summary` span.
/// @param head Integer: The number of items a vector's head shows.
/// @return List: The lists of an `r8_text`.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_layout_value(
    x: Robj,
    display_types: Robj,
    display: Function,
    summary: Nullable<String>,
    head: i32,
) -> Result<List> {
    let xs = unsafe { x.get() };
    let type_name = crate::value::type_name(xs).ok_or_else(|| fail("`x` is not an R8 value"))?;
    let q = QualifiedName::parse(&type_name).map_err(|e| fail(e.to_string()))?;
    // A value of another declaration is shown as of its own.
    let id = crate::value::id(xs).filter(|&id| !crate::value::current(id));
    let made = id.and_then(crate::value::made_under);
    let docs = match &made {
        Some(m) => m.clone(),
        None => crate::declare::snapshot()?,
    };
    let lookup: &dyn TypeLookup = &*docs;
    let shown = display_set(&display_types)?;
    let options = LayoutOptions {
        head: head.max(0) as usize,
        summary: match summary {
            Nullable::NotNull(s) => Some(s),
            Nullable::Null => None,
        },
        ..LayoutOptions::default()
    };
    let mut failure: Option<Error> = None;
    let mut layout = || {
        layout_with(
            RView::Whole(xs),
            &q,
            lookup,
            &options,
            &mut display_text(&shown, &display, &mut failure),
        )
    };
    let text = if id.is_some() {
        crate::value::looking(layout).ok_or_else(|| fail("the fields of `x` do not read"))?
    } else {
        layout()
    };
    match failure {
        Some(e) => Err(e),
        None => Ok(text_to_r(&text)),
    }
}

/// The field type of the element `x` of a list, or of a column of a data
/// frame when `column`, inferred from its R type, class, length, and
/// attributes in constant time.
fn infer_type(x: SEXP, column: bool) -> FieldType {
    let other = |class: &str, system: ClassSystem| host_class(class, "", system);
    let vector = |element: FieldType| FieldType::Vec {
        element: Box::new(element),
        min_length: None,
        max_length: None,
        unique: false,
    };
    // A length is read only from a vector: R refuses it for a closure.
    let scalar = || !column && xlen(x) == 1;
    if is_null(x) {
        return FieldType::Json;
    }
    if let Some(t) = crate::value::type_name(x) {
        return QualifiedName::parse(&t)
            .map(|name| FieldType::Named { name })
            .unwrap_or_else(|_| other("r8_value", ClassSystem::S3));
    }
    if inherits(x, "factor") {
        return FieldType::Factor {
            levels: None,
            ordered: inherits(x, "ordered"),
        };
    }
    if inherits(x, "data.frame") {
        return FieldType::Table {
            columns: Box::default(),
            open: true,
            rules: Box::default(),
        };
    }
    if inherits(x, "dgCMatrix") {
        return FieldType::SparseMatrix {
            element: Box::new(FieldType::Double(DoubleType::default())),
            nrow: None,
            ncol: None,
        };
    }
    for (class, ft) in [("Date", FieldType::Date), ("POSIXct", FieldType::Posixct)] {
        if inherits(x, class) {
            return if scalar() { ft } else { vector(ft) };
        }
    }
    let element = match sexptype(x) {
        SEXPTYPE::LGLSXP => FieldType::Logical,
        SEXPTYPE::INTSXP => FieldType::Integer(IntegerType::default()),
        SEXPTYPE::REALSXP => FieldType::Double(DoubleType::default()),
        SEXPTYPE::STRSXP => FieldType::Character(CharacterType::default()),
        SEXPTYPE::RAWSXP if attr(x, sym_class()) == nil() => return FieldType::Raw,
        t => {
            let class = strings(attr(x, sym_class()))
                .into_iter()
                .flatten()
                .next()
                .map(|c| c.into_owned());
            return match class {
                Some(class) => other(&class, explicit_system(x)),
                None => other(implicit_class(t), ClassSystem::Implicit),
            };
        }
    };
    if attr(x, sym_class()) != nil() {
        let class = strings(attr(x, sym_class()))
            .into_iter()
            .flatten()
            .next()
            .map(|c| c.into_owned())
            .unwrap_or_default();
        return other(&class, explicit_system(x));
    }
    let dim = attr(x, sym_dim());
    if !is_null(dim) {
        return if xlen(dim) == 2 {
            FieldType::Matrix {
                element: Box::new(element),
                nrow: None,
                ncol: None,
            }
        } else {
            FieldType::Array {
                element: Box::new(element),
                dim: None,
            }
        };
    }
    if scalar() && attr(x, sym_names()) == nil() {
        element
    } else {
        vector(element)
    }
}

/// The class R reports for an object of the type `t` without a class
/// attribute, other than an atomic vector.
fn implicit_class(t: SEXPTYPE) -> &'static str {
    match t {
        SEXPTYPE::VECSXP => "list",
        SEXPTYPE::CLOSXP | SEXPTYPE::BUILTINSXP | SEXPTYPE::SPECIALSXP => "function",
        SEXPTYPE::ENVSXP => "environment",
        SEXPTYPE::SYMSXP => "name",
        SEXPTYPE::LANGSXP => "call",
        SEXPTYPE::CPLXSXP => "complex",
        SEXPTYPE::EXTPTRSXP => "externalptr",
        SEXPTYPE::OBJSXP => "S4",
        _ => "object",
    }
}

/// The class system of an object with a class attribute: S4 for an S4
/// object, S3 otherwise.
fn explicit_system(x: SEXP) -> ClassSystem {
    // SAFETY: `x` is a live, protected R object.
    if bool::from(unsafe { extendr_ffi::Rf_isS4(x) }) {
        ClassSystem::S4
    } else {
        ClassSystem::S3
    }
}

/// A list without a class attribute, which the layout shows as a nested
/// record.
fn is_plain_list(x: SEXP) -> bool {
    sexptype(x) == SEXPTYPE::VECSXP && attr(x, sym_class()) == nil()
}

/// An R list, or a data frame's columns when `columns`, as a record: each
/// element an entry, named by its name, or by its position (`[[2]]`) when
/// it has none.
struct ListRecord {
    x: SEXP,
    columns: bool,
}

impl<'a> Record<'a, RView<'a>> for ListRecord {
    fn len(&self) -> usize {
        xlen(self.x)
    }

    fn entry(&self, index: usize) -> Entry<'a, RView<'a>> {
        let elt = unsafe { VECTOR_ELT(self.x, index as R_xlen_t) };
        let names = attr(self.x, sym_names());
        let key = (sexptype(names) == SEXPTYPE::STRSXP && index < xlen(names))
            .then(|| string_elt(names, index))
            .flatten();
        let name = match key {
            Some(n) if !n.is_empty() => EntryName {
                text: n.into_owned(),
                positional: false,
            },
            _ => EntryName {
                text: format!("[[{}]]", index + 1),
                positional: true,
            },
        };
        if !self.columns && is_plain_list(elt) {
            return Entry::Record {
                name,
                label: "list".to_string(),
                shape: format!("[{}]", count(xlen(elt) as u64)),
                record: Box::new(ListRecord {
                    x: elt,
                    columns: false,
                }),
            };
        }
        let mut ty = infer_type(elt, self.columns);
        if self.columns && is_plain_list(elt) {
            ty = host_class("list", "", ClassSystem::Implicit);
        }
        Entry::Field {
            name,
            ty,
            value: RView::Whole(elt),
        }
    }
}

/// The text of a class name for a header: its characters without control
/// characters.
fn class_label(x: SEXP) -> String {
    strings(attr(x, sym_class()))
        .into_iter()
        .flatten()
        .next()
        .map(|c| c.chars().filter(|c| !c.is_control()).collect())
        .unwrap_or_else(|| "list".to_string())
}

/// The default layout of a list or a data frame, as the lists of an
/// `r8_text`: a header naming the class with the number of elements, or
/// rows x columns, then a line per element or column, its field type
/// inferred from the R object.
///
/// @param x A list or a data frame.
/// @param display_types Character: The qualified names of the types whose
///   values show the text of `display`.
/// @param display Function: The text of a nested R8 value.
/// @param head Integer: The number of items a vector's head shows.
/// @return List: The lists of an `r8_text`.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_layout_object(x: Robj, display_types: Robj, display: Function, head: i32) -> Result<List> {
    let xs = unsafe { x.get() };
    if sexptype(xs) != SEXPTYPE::VECSXP {
        return Err(fail("`x` is a list or a data frame"));
    }
    let frame = inherits(xs, "data.frame");
    let n = xlen(xs);
    let summary = if frame {
        let rows = attributes_unchecked(|| table_rows(xs)).unwrap_or(0);
        format!("{} x {}", count(rows), count(n as u64))
    } else {
        format!(
            "{} element{}",
            count(n as u64),
            if n == 1 { "" } else { "s" }
        )
    };
    let docs = crate::declare::snapshot()?;
    let lookup: &dyn TypeLookup = &*docs;
    let shown = display_set(&display_types)?;
    let options = LayoutOptions {
        head: head.max(0) as usize,
        ..LayoutOptions::default()
    };
    let record = ListRecord {
        x: xs,
        columns: frame,
    };
    let mut failure: Option<Error> = None;
    let text = attributes_unchecked(|| {
        layout_record(
            &class_label(xs),
            &summary,
            &record,
            lookup,
            &options,
            &mut display_text(&shown, &display, &mut failure),
        )
    });
    match failure {
        Some(e) => Err(e),
        None => Ok(text_to_r(&text)),
    }
}

/// The style of a role in an R theme table: `color` (a hex code or `NA`),
/// `bold`, and `italic`.
fn style_from_r(role: &str, x: &Robj) -> Result<Style> {
    let l: List = x
        .clone()
        .try_into()
        .map_err(|_| fail(format!("the style of `{role}` is a list")))?;
    let color = match l
        .dollar("color")
        .ok()
        .filter(|c| !c.is_null() && !c.is_na())
        .and_then(|c| crate::utf8::one(&c))
    {
        Some(hex) => Some(Color::from_hex(&hex).ok_or_else(|| {
            fail(format!(
                "the color of `{role}` is a hex code such as \"#6CA3A0\", not \"{hex}\""
            ))
        })?),
        None => None,
    };
    let flag = |name: &str| {
        l.dollar(name)
            .ok()
            .and_then(|v| v.as_bool())
            .unwrap_or(false)
    };
    Ok(Style {
        color,
        bold: flag("bold"),
        italic: flag("italic"),
    })
}

/// A theme from an R theme table, a named list of styles by role.
fn theme_from_r(x: &List) -> Result<Theme> {
    let mut theme = Theme::rtemis();
    for (name, style) in x.iter() {
        let role =
            Role::from_name(name).ok_or_else(|| fail(format!("`{name}` is not a role of R8")))?;
        theme = theme.with(role, style_from_r(name, &style)?);
    }
    Ok(theme)
}

/// The R theme table of a theme: a style per role, with the 256-color index
/// of each color.
fn theme_to_r(theme: &Theme) -> List {
    let roles: Vec<&str> = ALL_ROLES.iter().map(|r| r.name()).collect();
    let styles: Vec<Robj> = ALL_ROLES
        .iter()
        .map(|r| {
            let s = theme.style(*r);
            let (color, index): (Robj, Robj) = match s.color {
                Some(c) => (c.hex().into(), (c.index as i32).into()),
                None => (Rstr::na().into(), Rint::na().into()),
            };
            list!(
                color = color,
                index = index,
                bold = s.bold,
                italic = s.italic
            )
            .into()
        })
        .collect();
    let mut out = List::from_values(styles);
    out.set_names(roles).expect("one name per role");
    out
}

/// R8's theme with the styles of `overrides` in place of its own: each
/// member a role, whose `color`, `bold`, and `italic` replace the default
/// style's.
///
/// @param overrides List: Styles by role, each a list of `color`, `bold`,
///   and `italic`, any of which may be absent.
/// @return List: A style per role: `color`, `index`, `bold`, and `italic`.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_theme_table(overrides: List) -> Result<List> {
    let mut theme = Theme::rtemis();
    for (name, value) in overrides.iter() {
        let role = Role::from_name(name).ok_or_else(|| {
            fail(format!(
                "`{name}` is not a role of R8; the roles are {}",
                ALL_ROLES.map(|r| r.name()).join(", ")
            ))
        })?;
        let l: List = value
            .try_into()
            .map_err(|_| fail(format!("the style of `{name}` is a list")))?;
        let mut style = theme.style(role);
        for (member, v) in l.iter() {
            match member {
                "color" => style.color = match crate::utf8::one(&v) {
                    Some(hex) => Some(Color::from_hex(&hex).ok_or_else(|| {
                        fail(format!(
                            "the color of `{name}` is a hex code such as \"#6CA3A0\", not \"{hex}\""
                        ))
                    })?),
                    None if v.is_null() || v.is_na() => None,
                    None => return Err(fail(format!("the color of `{name}` is a hex code"))),
                },
                "bold" | "italic" => {
                    let b = v
                        .as_bool()
                        .ok_or_else(|| fail(format!("`{member}` of `{name}` is TRUE or FALSE")))?;
                    if member == "bold" {
                        style.bold = b;
                    } else {
                        style.italic = b;
                    }
                }
                other => {
                    return Err(fail(format!(
                        "`{other}` is not a member of a style; a style has `color`, `bold`, and `italic`"
                    )));
                }
            }
        }
        theme = theme.with(role, style);
    }
    Ok(theme_to_r(&theme))
}

/// The names of R8's roles.
///
/// @return Character.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_roles() -> Vec<&'static str> {
    ALL_ROLES.iter().map(|r| r.name()).collect()
}

/// The stylesheet of the html classes mode: the rules of `theme`, and
/// those of R8's light and dark themes that differ from them, for the roles
/// outside `named`.
///
/// @param theme List: A theme table, or `NULL` for R8's theme.
/// @param named Character: The roles the theme names.
/// @return Character: The stylesheet.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_css_text(theme: Nullable<List>, named: Robj) -> Result<String> {
    let base = match theme {
        Nullable::NotNull(l) => theme_from_r(&l)?,
        Nullable::Null => Theme::rtemis(),
    };
    let named = str_vec(&named).ok_or_else(|| fail("`named` is character"))?;
    let mut light = Theme::rtemis_light();
    let mut dark = Theme::rtemis_dark();
    for n in &named {
        let role = Role::from_name(n).ok_or_else(|| fail(format!("`{n}` is not a role of R8")))?;
        light = light.with(role, base.style(role));
        dark = dark.with(role, base.style(role));
    }
    Ok(css(&base, &light, &dark))
}

/// An `r8_text` encoded as one output type.
///
/// @param text List: The lists of an `r8_text`.
/// @param output_type Character: `plain`, `ansi`, `html`, or `markdown`.
/// @param theme List: A theme table, or `NULL` for R8's theme.
/// @param depth Character: The color depth of `ansi`, `256` or `24bit`.
/// @param html_classes Logical: `html` marks styles by class only.
/// @param inline Logical: `markdown` writes one line.
/// @return Character: One string, lines joined by line breaks.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_text_encode(
    text: List,
    output_type: crate::utf8::Utf8,
    theme: Nullable<List>,
    depth: crate::utf8::Utf8,
    html_classes: bool,
    inline: bool,
) -> Result<String> {
    let output_type: &str = &output_type;
    let depth: &str = &depth;
    let t = text_from_r(&text)?;
    let theme = match theme {
        Nullable::NotNull(l) => theme_from_r(&l)?,
        Nullable::Null => Theme::rtemis(),
    };
    let ty = OutputType::from_name(output_type).ok_or_else(|| {
        fail(format!(
            "`output_type` is \"plain\", \"ansi\", \"html\", or \"markdown\", not \"{output_type}\""
        ))
    })?;
    Ok(match ty {
        OutputType::Plain => encode_plain(&t),
        OutputType::Ansi => encode_ansi(
            &t,
            &theme,
            if depth == "24bit" {
                ColorDepth::TrueColor
            } else {
                ColorDepth::Colors256
            },
        ),
        OutputType::Html => encode_html(
            &t,
            &theme,
            if html_classes {
                HtmlStyle::Classes
            } else {
                HtmlStyle::Inline
            },
        ),
        OutputType::Markdown => encode_markdown(&t, inline),
    })
}

/// An output type named in a fact, or `None` for `NULL` or `""`.
fn fact_type(facts: &List, name: &str) -> Result<Option<OutputType>> {
    match facts.dollar(name).ok().and_then(|v| crate::utf8::one(&v)) {
        None => Ok(None),
        Some(s) if s.is_empty() => Ok(None),
        Some(s) => OutputType::from_name(&s).map(Some).ok_or_else(|| {
            fail(format!(
                "the output type of `{name}` is \"plain\", \"ansi\", \"html\", or \"markdown\", not \"{s}\""
            ))
        }),
    }
}

/// The output type and color depth of `print()` from the facts R gathers.
///
/// @param facts List: `explicit`, `option`, and `env` (an output type or
///   `NULL`); `no_color`, `knitr`, `terminal`, and `truecolor` (logical);
///   and `host` (`positron`, `rstudio`, `rstudio_color`, `no_ansi_gui`, or
///   `other`).
/// @return Character: The output type and the color depth, `256` or `24bit`.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_determine_output(facts: List) -> Result<Vec<String>> {
    let flag = |name: &str| {
        facts
            .dollar(name)
            .ok()
            .and_then(|v| v.as_bool())
            .unwrap_or(false)
    };
    let host = match facts.dollar("host").ok().and_then(|v| crate::utf8::one(&v)) {
        Some(h) if h == "positron" => HostKind::Positron,
        Some(h) if h == "rstudio" => HostKind::RStudio {
            console_color: false,
        },
        Some(h) if h == "rstudio_color" => HostKind::RStudio {
            console_color: true,
        },
        Some(h) if h == "no_ansi_gui" => HostKind::NoAnsiGui,
        _ => HostKind::Other,
    };
    let f = Facts {
        explicit: fact_type(&facts, "explicit")?,
        option: fact_type(&facts, "option")?,
        env: fact_type(&facts, "env")?,
        no_color: flag("no_color"),
        knitr: flag("knitr"),
        host,
        terminal: flag("terminal"),
        truecolor: flag("truecolor"),
    };
    let (ty, depth) = determine(&f);
    Ok(vec![
        ty.name().to_string(),
        match depth {
            ColorDepth::TrueColor => "24bit",
            ColorDepth::Colors256 => "256",
        }
        .to_string(),
    ])
}

extendr_module! {
    mod text;
    fn r8_layout_value;
    fn r8_layout_object;
    fn r8_theme_table;
    fn r8_roles;
    fn r8_text_encode;
    fn r8_css_text;
    fn r8_determine_output;
}
