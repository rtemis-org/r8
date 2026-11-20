//! Text output: lines of spans, each tagged with a role, which an encoder
//! writes as plain text, ANSI, HTML, or markdown; the choice of the output
//! type from the facts a host gathers; and the size of a value's memory.
//! Every interface of R8 prints through it, so a value reads the same in
//! each.
//! spec: R8/class-system#text-output

/// What a span of text is, which the theme maps to a style.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Role {
    /// The angle brackets around a type in a header.
    Bracket,
    /// A type's name.
    TypeName,
    /// An enum value's variant.
    Variant,
    /// A field's name.
    FieldName,
    /// A field's value, or the head of a vector.
    Scalar,
    /// A measurement or count, such as a size or a length.
    Summary,
    /// The name of a double type.
    TypeNumber,
    /// The name of an integer type.
    TypeInteger,
    /// The name of a factor type.
    TypeCategorical,
    /// The name of a character type.
    TypeString,
    /// The name of a logical type.
    TypeBoolean,
    /// The name of a Date or POSIXct type.
    TypeDatetime,
    /// The name of any other base type: table, list, raw, JSON, maps, and R objects.
    TypeOther,
    /// Text an impl returns, which each encoder escapes.
    UserText,
    /// Emphasis for an impl's text, strong.
    Strong,
    /// Emphasis for an impl's text.
    Emphasis,
    /// Secondary text.
    Muted,
    /// Text to draw the eye to.
    Highlight,
    /// Text without a role, such as separators and indentation.
    Plain,
}

impl Role {
    /// The role's name, as R and the encoders' class names write it.
    pub fn name(self) -> &'static str {
        match self {
            Role::Bracket => "bracket",
            Role::TypeName => "type_name",
            Role::Variant => "variant",
            Role::FieldName => "field_name",
            Role::Scalar => "scalar",
            Role::Summary => "summary",
            Role::TypeNumber => "type_number",
            Role::TypeInteger => "type_integer",
            Role::TypeCategorical => "type_categorical",
            Role::TypeString => "type_string",
            Role::TypeBoolean => "type_boolean",
            Role::TypeDatetime => "type_datetime",
            Role::TypeOther => "type_other",
            Role::UserText => "user_text",
            Role::Strong => "strong",
            Role::Emphasis => "emphasis",
            Role::Muted => "muted",
            Role::Highlight => "highlight",
            Role::Plain => "plain",
        }
    }

    /// The role of a name; `None` for an unknown name.
    pub fn from_name(name: &str) -> Option<Role> {
        Some(match name {
            "bracket" => Role::Bracket,
            "type_name" => Role::TypeName,
            "variant" => Role::Variant,
            "field_name" => Role::FieldName,
            "scalar" => Role::Scalar,
            "summary" => Role::Summary,
            "type_number" => Role::TypeNumber,
            "type_integer" => Role::TypeInteger,
            "type_categorical" => Role::TypeCategorical,
            "type_string" => Role::TypeString,
            "type_boolean" => Role::TypeBoolean,
            "type_datetime" => Role::TypeDatetime,
            "type_other" => Role::TypeOther,
            "user_text" => Role::UserText,
            "strong" => Role::Strong,
            "emphasis" => Role::Emphasis,
            "muted" => Role::Muted,
            "highlight" => Role::Highlight,
            "plain" => Role::Plain,
            _ => return None,
        })
    }
}

/// A run of text with one role.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Span {
    /// The text, without line breaks.
    pub text: String,
    /// What the text is.
    pub role: Role,
}

impl Span {
    /// A span of `text` with `role`.
    pub fn new(text: impl Into<String>, role: Role) -> Span {
        Span {
            text: text.into(),
            role,
        }
    }

    /// The span's text as plain text: user text loses control characters.
    pub fn plain(&self) -> String {
        plain_text(&self.text, self.role)
    }
}

/// A line of text: its nesting depth and its spans. Every encoder indents a
/// line by two spaces per level of depth; the html encoder also records the
/// depth in the attribute `data-depth`.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Line {
    /// The nesting depth: 0 for a value's header, 1 for its fields, and one
    /// more for the fields of each nested value.
    pub depth: usize,
    /// The spans of the line, without its indentation.
    pub spans: Vec<Span>,
}

impl Line {
    /// A line of `spans` at `depth`.
    pub fn new(depth: usize, spans: Vec<Span>) -> Self {
        Self { depth, spans }
    }

    /// The indentation of the line: two spaces per level of depth.
    pub fn indent(&self) -> String {
        "  ".repeat(self.depth)
    }

    /// The plain text of the line, its indentation included.
    pub fn plain(&self) -> String {
        let mut out = self.indent();
        for s in &self.spans {
            out.push_str(&plain_text(&s.text, s.role));
        }
        out
    }
}

impl From<Vec<Span>> for Line {
    /// A line of depth 0.
    fn from(spans: Vec<Span>) -> Self {
        Self::new(0, spans)
    }
}

/// Lines of spans: the layout of a value, or text an impl returns.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Text {
    /// The lines.
    pub lines: Vec<Line>,
}

impl Text {
    /// The plain text of each line, the reference that every encoding
    /// reduces to when its markup is removed.
    pub fn plain_lines(&self) -> Vec<String> {
        self.lines.iter().map(Line::plain).collect()
    }
}

/// The text of a span as plain text: user text loses control characters,
/// which include the escape that starts an ANSI sequence.
fn plain_text(text: &str, role: Role) -> String {
    if role == Role::UserText {
        text.chars().filter(|c| !c.is_control()).collect()
    } else {
        text.to_string()
    }
}

/// The output types of text.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OutputType {
    /// Text without markup.
    Plain,
    /// Text with ANSI escape sequences for a terminal.
    Ansi,
    /// HTML.
    Html,
    /// Markdown.
    Markdown,
}

impl OutputType {
    /// The output type of a name: `plain`, `ansi`, `html`, or `markdown`.
    pub fn from_name(name: &str) -> Option<OutputType> {
        Some(match name {
            "plain" => OutputType::Plain,
            "ansi" => OutputType::Ansi,
            "html" => OutputType::Html,
            "markdown" => OutputType::Markdown,
            _ => return None,
        })
    }

    /// The type's name.
    pub fn name(self) -> &'static str {
        match self {
            OutputType::Plain => "plain",
            OutputType::Ansi => "ansi",
            OutputType::Html => "html",
            OutputType::Markdown => "markdown",
        }
    }
}

/// The colors an ANSI encoding may use.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ColorDepth {
    /// The 256-color palette.
    Colors256,
    /// 24-bit color.
    TrueColor,
}

/// The kind of host that writes the text.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HostKind {
    /// The Positron console.
    Positron,
    /// The RStudio console, with `console_color` when `RSTUDIO_CONSOLE_COLOR`
    /// is set.
    RStudio {
        /// The console renders color.
        console_color: bool,
    },
    /// R.app on macOS (`AQUA`) or Rgui on Windows, which render no ANSI.
    NoAnsiGui,
    /// Any other host, such as `Rscript`, a shell, or a server.
    Other,
}

/// The facts from which a host resolves the output type of `print()`, in
/// the order the resolution reads them.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Facts {
    /// The output type the caller names.
    pub explicit: Option<OutputType>,
    /// The output type of the host's setting (`r8.output_type` in R).
    pub option: Option<OutputType>,
    /// The output type of the environment variable `R8_OUTPUT_TYPE`.
    pub env: Option<OutputType>,
    /// The environment variable `NO_COLOR` is set.
    pub no_color: bool,
    /// knitr is running, which prints through its own methods.
    pub knitr: bool,
    /// The kind of host.
    pub host: HostKind,
    /// Standard output is a terminal.
    pub terminal: bool,
    /// The environment variable `COLORTERM` is `truecolor` or `24bit`.
    pub truecolor: bool,
}

/// The output type and color depth of `print()` from a host's facts: the
/// explicit choice; the setting; the environment variable; plain under
/// `NO_COLOR` or knitr; ANSI in 24-bit color in Positron, in 256 colors in
/// RStudio with console color; plain in R.app and Rgui; ANSI in a
/// terminal, in 24-bit color when `COLORTERM` says so and 256 colors
/// otherwise; and plain in any other host.
pub fn resolve(facts: &Facts) -> (OutputType, ColorDepth) {
    let depth = if facts.truecolor {
        ColorDepth::TrueColor
    } else {
        ColorDepth::Colors256
    };
    if let Some(t) = facts.explicit.or(facts.option).or(facts.env) {
        return (t, depth);
    }
    if facts.no_color || facts.knitr {
        return (OutputType::Plain, depth);
    }
    match facts.host {
        HostKind::Positron => (OutputType::Ansi, ColorDepth::TrueColor),
        HostKind::RStudio {
            console_color: true,
        } => (OutputType::Ansi, ColorDepth::Colors256),
        HostKind::NoAnsiGui => (OutputType::Plain, depth),
        _ if facts.terminal => (OutputType::Ansi, depth),
        _ => (OutputType::Plain, depth),
    }
}

/// Text as plain lines joined by line breaks.
pub fn encode_plain(text: &Text) -> String {
    text.plain_lines().join("\n")
}

/// A color: its hex code and the index of the nearest color of the xterm
/// 256-color palette.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Color {
    /// Red, green, and blue.
    pub rgb: [u8; 3],
    /// The index in the 256-color palette.
    pub index: u8,
}

impl Color {
    /// The color of a hex code, `#RRGGBB`; `None` for another text.
    pub fn from_hex(hex: &str) -> Option<Color> {
        let h = hex.strip_prefix('#')?;
        if h.len() != 6 || !h.is_ascii() {
            return None;
        }
        let byte = |i: usize| u8::from_str_radix(&h[i..i + 2], 16).ok();
        let rgb = [byte(0)?, byte(2)?, byte(4)?];
        Some(Color {
            rgb,
            index: nearest_256(rgb),
        })
    }

    /// The hex code, `#RRGGBB`.
    pub fn hex(self) -> String {
        format!("#{:02X}{:02X}{:02X}", self.rgb[0], self.rgb[1], self.rgb[2])
    }
}

/// The index of the color of the xterm 256-color palette nearest to `rgb`,
/// among the 6x6x6 cube (16 to 231) and the gray ramp (232 to 255), by
/// squared distance.
fn nearest_256(rgb: [u8; 3]) -> u8 {
    const LEVELS: [i32; 6] = [0, 95, 135, 175, 215, 255];
    let distance = |c: [i32; 3]| -> i32 { (0..3).map(|k| (i32::from(rgb[k]) - c[k]).pow(2)).sum() };
    let mut best = (i32::MAX, 16u8);
    for i in 16..=255u8 {
        let c = if i < 232 {
            let j = usize::from(i - 16);
            [LEVELS[j / 36], LEVELS[(j / 6) % 6], LEVELS[j % 6]]
        } else {
            let v = 8 + 10 * i32::from(i - 232);
            [v, v, v]
        };
        let d = distance(c);
        if d < best.0 {
            best = (d, i);
        }
    }
    best.1
}

/// The style of a role: a color, or the host's default when `None`, and
/// weight and slant.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct Style {
    /// The color.
    pub color: Option<Color>,
    /// Bold.
    pub bold: bool,
    /// Italic.
    pub italic: bool,
}

impl Style {
    /// Whether the style leaves text unstyled.
    pub fn is_plain(self) -> bool {
        self.color.is_none() && !self.bold && !self.italic
    }
}

/// The styles of the roles. Encoders take a theme as an argument.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Theme {
    styles: Vec<(Role, Style)>,
}

/// rtemis teal, `#6CA3A0`.
const TEAL: Color = Color {
    rgb: [0x6C, 0xA3, 0xA0],
    index: 73,
};
/// Gray, `#808080`.
const GRAY: Color = Color {
    rgb: [0x80, 0x80, 0x80],
    index: 244,
};
/// rtemis green, `#0F6A66`.
const GREEN: Color = Color {
    rgb: [0x0F, 0x6A, 0x66],
    index: 23,
};

/// The roles of base-type names with a color, in the order of the colors
/// of [`TYPE_COLORS`], [`TYPE_COLORS_LIGHT`], and [`TYPE_COLORS_DARK`].
const TYPE_ROLES: [Role; 6] = [
    Role::TypeNumber,
    Role::TypeInteger,
    Role::TypeCategorical,
    Role::TypeString,
    Role::TypeBoolean,
    Role::TypeDatetime,
];

/// The colors of base-type names, after rtemislive's column types at OKLCH
/// lightness of about 0.6, each with WCAG contrast of 4.06 to 4.12 on white
/// and on `#1E1E1E`.
const TYPE_COLORS: [&str; 6] = [
    "#2E81CC", "#008C92", "#128F5A", "#CC566E", "#866BD8", "#C056AC",
];

/// rtemislive's colors of column types on light pages.
const TYPE_COLORS_LIGHT: [&str; 6] = [
    "#006BB9", "#00757A", "#00784A", "#7D0830", "#7550D1", "#AE379B",
];

/// rtemislive's colors of column types on dark pages.
const TYPE_COLORS_DARK: [&str; 6] = [
    "#69AAEA", "#00BDC5", "#45BF82", "#B94960", "#A696EB", "#DA81C7",
];

impl Theme {
    /// R8's theme, which reads on light and dark backgrounds: brackets,
    /// variants, and highlights in rtemis teal; type names and strong text
    /// bold; summaries and muted text in gray; emphasis in italic; the names
    /// of base types in rtemislive's colors of column types; and the default
    /// color elsewhere.
    pub fn rtemis() -> Theme {
        Theme::rtemis_base().with_type_colors(&TYPE_COLORS)
    }

    /// The theme with the base-type roles in `colors`, in the order of
    /// [`TYPE_ROLES`].
    fn with_type_colors(mut self, colors: &[&str; 6]) -> Theme {
        for (role, hex) in TYPE_ROLES.iter().zip(colors) {
            let color = Color::from_hex(hex).expect("a type color is a hex code");
            self = self.with(
                *role,
                Style {
                    color: Some(color),
                    ..Style::default()
                },
            );
        }
        self
    }

    /// R8's theme without the colors of base types.
    fn rtemis_base() -> Theme {
        let teal = Style {
            color: Some(TEAL),
            ..Style::default()
        };
        let bold = Style {
            bold: true,
            ..Style::default()
        };
        let gray = Style {
            color: Some(GRAY),
            ..Style::default()
        };
        Theme {
            styles: vec![
                (Role::Bracket, teal),
                (Role::Variant, teal),
                (Role::Highlight, Style { bold: true, ..teal }),
                (Role::TypeName, bold),
                (Role::Strong, bold),
                (Role::Summary, gray),
                (Role::Muted, gray),
                (
                    Role::Emphasis,
                    Style {
                        italic: true,
                        ..Style::default()
                    },
                ),
            ],
        }
    }

    /// R8's theme for light pages: rtemis green in place of teal, and
    /// rtemislive's colors of column types on light pages.
    pub fn rtemis_light() -> Theme {
        let mut t = Theme::rtemis_base();
        for (_, style) in &mut t.styles {
            if style.color == Some(TEAL) {
                style.color = Some(GREEN);
            }
        }
        t.with_type_colors(&TYPE_COLORS_LIGHT)
    }

    /// R8's theme for dark pages: rtemislive's colors of column types on
    /// dark pages.
    pub fn rtemis_dark() -> Theme {
        Theme::rtemis_base().with_type_colors(&TYPE_COLORS_DARK)
    }

    /// The style of a role; unstyled for a role the theme leaves out.
    pub fn style(&self, role: Role) -> Style {
        self.styles
            .iter()
            .find(|(r, _)| *r == role)
            .map(|(_, s)| *s)
            .unwrap_or_default()
    }

    /// The theme with the style of `role` replaced.
    pub fn with(mut self, role: Role, style: Style) -> Theme {
        match self.styles.iter_mut().find(|(r, _)| *r == role) {
            Some(entry) => entry.1 = style,
            None => self.styles.push((role, style)),
        }
        self
    }
}

/// Text with ANSI escape sequences: each styled span between its select
/// graphic rendition sequence and a reset; colors as 256-color indices or
/// 24-bit.
pub fn encode_ansi(text: &Text, theme: &Theme, depth: ColorDepth) -> String {
    let mut out = Vec::with_capacity(text.lines.len());
    for line in &text.lines {
        let mut l = line.indent();
        for span in &line.spans {
            let t = plain_text(&span.text, span.role);
            let style = theme.style(span.role);
            if style.is_plain() || t.is_empty() {
                l.push_str(&t);
                continue;
            }
            let mut codes: Vec<String> = Vec::new();
            if style.bold {
                codes.push("1".into());
            }
            if style.italic {
                codes.push("3".into());
            }
            if let Some(c) = style.color {
                codes.push(match depth {
                    ColorDepth::Colors256 => format!("38;5;{}", c.index),
                    ColorDepth::TrueColor => {
                        format!("38;2;{};{};{}", c.rgb[0], c.rgb[1], c.rgb[2])
                    }
                });
            }
            l.push_str(&format!("\u{1b}[{}m{}\u{1b}[0m", codes.join(";"), t));
        }
        out.push(l);
    }
    out.join("\n")
}

/// How the html encoder marks a span's style.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HtmlStyle {
    /// A class per role and an inline style through the CSS variable of the
    /// role, `style="color:var(--r8-bracket,#6CA3A0)"`, styled without a
    /// stylesheet.
    Inline,
    /// A class per role only, styled by the stylesheet of [`css`].
    Classes,
}

/// Text with HTML's `&`, `<`, `>`, and `"` escaped.
fn escape_html(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    for c in s.chars() {
        match c {
            '&' => out.push_str("&amp;"),
            '<' => out.push_str("&lt;"),
            '>' => out.push_str("&gt;"),
            '"' => out.push_str("&quot;"),
            _ => out.push(c),
        }
    }
    out
}

/// The CSS declarations of a role's style, through its variable.
fn css_declarations(role: Role, style: Style) -> String {
    let mut d = Vec::new();
    if let Some(c) = style.color {
        d.push(format!("color:var(--r8-{},{})", role.name(), c.hex()));
    }
    if style.bold {
        d.push("font-weight:bold".to_string());
    }
    if style.italic {
        d.push("font-style:italic".to_string());
    }
    d.join(";")
}

/// Text as HTML: a `pre` element of class `r8` holding a `span` element of
/// class `r8-line` per line, whose attribute `data-depth` is the line's
/// depth, and whose styled spans are `span` elements of class `r8-<role>`,
/// with inline styles or classes only; every text is escaped.
pub fn encode_html(text: &Text, theme: &Theme, mode: HtmlStyle) -> String {
    let mut lines = Vec::with_capacity(text.lines.len());
    for line in &text.lines {
        let mut l = format!(
            "<span class=\"r8-line\" data-depth=\"{}\">{}",
            line.depth,
            line.indent()
        );
        for span in &line.spans {
            let t = escape_html(&plain_text(&span.text, span.role));
            if span.role == Role::Plain {
                l.push_str(&t);
                continue;
            }
            let style = theme.style(span.role);
            let attr = match mode {
                HtmlStyle::Inline if !style.is_plain() => {
                    format!(" style=\"{}\"", css_declarations(span.role, style))
                }
                _ => String::new(),
            };
            l.push_str(&format!(
                "<span class=\"r8-{}\"{attr}>{t}</span>",
                span.role.name()
            ));
        }
        l.push_str("</span>");
        lines.push(l);
    }
    format!("<pre class=\"r8\">{}</pre>", lines.join("\n"))
}

/// The stylesheet of the html encoder's `classes` mode: a rule per styled
/// role of `theme`, then the rules of `light` that differ from it under
/// `@media (prefers-color-scheme: light)`, and those of `dark` under
/// `@media (prefers-color-scheme: dark)`.
pub fn css(theme: &Theme, light: &Theme, dark: &Theme) -> String {
    let rule = |t: &Theme, r: Role| -> Option<String> {
        let style = t.style(r);
        (!style.is_plain())
            .then(|| format!(".r8 .r8-{} {{ {} }}", r.name(), css_declarations(r, style)))
    };
    let mut out: Vec<String> = ALL_ROLES.iter().filter_map(|r| rule(theme, *r)).collect();
    for (scheme, t) in [("light", light), ("dark", dark)] {
        let differing: Vec<String> = ALL_ROLES
            .iter()
            .filter(|r| rule(t, **r) != rule(theme, **r))
            .filter_map(|r| rule(t, *r))
            .collect();
        if !differing.is_empty() {
            out.push(format!("@media (prefers-color-scheme: {scheme}) {{"));
            out.extend(differing.into_iter().map(|r| format!("  {r}")));
            out.push("}".to_string());
        }
    }
    out.join("\n")
}

/// Every role, in declaration order.
pub const ALL_ROLES: [Role; 19] = [
    Role::Bracket,
    Role::TypeName,
    Role::Variant,
    Role::FieldName,
    Role::Scalar,
    Role::Summary,
    Role::TypeNumber,
    Role::TypeInteger,
    Role::TypeCategorical,
    Role::TypeString,
    Role::TypeBoolean,
    Role::TypeDatetime,
    Role::TypeOther,
    Role::UserText,
    Role::Strong,
    Role::Emphasis,
    Role::Muted,
    Role::Highlight,
    Role::Plain,
];

/// Text with markdown's special characters escaped.
fn escape_markdown(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    for c in s.chars() {
        if matches!(
            c,
            '\\' | '`' | '*' | '_' | '[' | ']' | '<' | '>' | '#' | '|' | '{' | '}'
        ) {
            out.push('\\');
        }
        out.push(c);
    }
    out
}

/// Whether text holds a value's layout: a span of a role of the layout.
fn has_layout(text: &Text) -> bool {
    text.lines.iter().flat_map(|l| &l.spans).any(|s| {
        matches!(
            s.role,
            Role::Bracket
                | Role::TypeName
                | Role::Variant
                | Role::FieldName
                | Role::Scalar
                | Role::Summary
                | Role::TypeNumber
                | Role::TypeInteger
                | Role::TypeCategorical
                | Role::TypeString
                | Role::TypeBoolean
                | Role::TypeDatetime
                | Role::TypeOther
        )
    })
}

/// Text as markdown. A value's layout is its plain text in a code block,
/// fenced by more backticks than it holds in a row, or, when `inline`, in
/// one code span whose lines are trimmed and joined by `; `; an impl's text
/// keeps `strong` as `**` and `emphasis` as
/// `*`, with every other character that markdown reads escaped; `inline`
/// joins the lines with spaces.
pub fn encode_markdown(text: &Text, inline: bool) -> String {
    if has_layout(text) {
        let plain = text.plain_lines();
        let body = if inline {
            plain
                .iter()
                .map(|l| l.trim())
                .collect::<Vec<_>>()
                .join("; ")
        } else {
            plain.join("\n")
        };
        let longest = body.split(|c| c != '`').map(str::len).max().unwrap_or(0);
        if inline {
            let ticks = "`".repeat(longest + 1);
            let pad = if body.starts_with('`') || body.ends_with('`') {
                " "
            } else {
                ""
            };
            return format!("{ticks}{pad}{body}{pad}{ticks}");
        }
        let fence = "`".repeat((longest + 1).max(3));
        return format!("{fence}\n{body}\n{fence}");
    }
    let lines: Vec<String> = text
        .lines
        .iter()
        .map(|line| {
            let mut l = line.indent();
            for s in &line.spans {
                let t = escape_markdown(&plain_text(&s.text, s.role));
                l.push_str(&match s.role {
                    _ if t.trim().is_empty() => t,
                    Role::Strong => format!("**{t}**"),
                    Role::Emphasis => format!("*{t}*"),
                    _ => t,
                });
            }
            l
        })
        .collect();
    if inline {
        lines.join(" ")
    } else {
        lines.join("  \n")
    }
}

/// A size in bytes in IEC binary units, `B`, `KiB`, `MiB`, `GiB`, and
/// `TiB`: the exact count below 1 KiB (`120 B`), and otherwise the size in
/// the largest unit it reaches, to three significant digits (`7.63 MiB`,
/// `76.3 MiB`, `763 MiB`, `1020 KiB`).
pub fn format_size(bytes: u64) -> String {
    const UNITS: [&str; 4] = ["KiB", "MiB", "GiB", "TiB"];
    if bytes < 1024 {
        return format!("{bytes} B");
    }
    let mut value = bytes as f64 / 1024.0;
    let mut unit = 0;
    while value >= 1024.0 && unit < UNITS.len() - 1 {
        value /= 1024.0;
        unit += 1;
    }
    // Three significant digits: round at the third digit, then show as many
    // decimals as the rounded value leaves to it.
    let magnitude = value.log10().floor() as i32;
    let scale = 10f64.powi(magnitude - 2);
    let rounded = (value / scale).round() * scale;
    let decimals = (2 - rounded.log10().floor() as i32).max(0) as usize;
    format!("{rounded:.decimals$} {}", UNITS[unit])
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn roles_round_trip_through_their_names() {
        for r in [
            Role::Bracket,
            Role::TypeName,
            Role::Variant,
            Role::FieldName,
            Role::Scalar,
            Role::Summary,
            Role::UserText,
            Role::Strong,
            Role::Emphasis,
            Role::Muted,
            Role::Highlight,
            Role::Plain,
        ] {
            assert_eq!(Role::from_name(r.name()), Some(r));
        }
        assert_eq!(Role::from_name("color"), None);
    }

    #[test]
    fn plain_text_drops_control_characters_of_user_text_only() {
        let t = Text {
            lines: vec![
                vec![
                    Span::new("<", Role::Bracket),
                    Span::new("GBM", Role::TypeName),
                    Span::new(">", Role::Bracket),
                ]
                .into(),
                vec![Span::new("a\u{1b}[31mb", Role::UserText)].into(),
            ],
        };
        assert_eq!(encode_plain(&t), "<GBM>\na[31mb");
    }

    fn header() -> Text {
        Text {
            lines: vec![
                vec![
                    Span::new("<", Role::Bracket),
                    Span::new("pkgr::", Role::Muted),
                    Span::new("Resampler", Role::TypeName),
                    Span::new("@", Role::Bracket),
                    Span::new("KFold", Role::Variant),
                    Span::new(">", Role::Bracket),
                ]
                .into(),
            ],
        }
    }

    #[test]
    fn colors_take_their_nearest_256_index() {
        assert_eq!(Color::from_hex("#6CA3A0").map(|c| c.index), Some(73));
        assert_eq!(Color::from_hex("#808080").map(|c| c.index), Some(244));
        assert_eq!(Color::from_hex("#0F6A66").map(|c| c.index), Some(23));
        assert_eq!(
            Color::from_hex("#0f6a66").map(|c| c.hex()),
            Some("#0F6A66".to_string())
        );
        assert_eq!(Color::from_hex("teal"), None);
    }

    #[test]
    fn base_types_take_rtemislive_colors_by_kind() {
        let t = Theme::rtemis();
        let index = |r: Role| t.style(r).color.map(|c| c.index);
        assert_eq!(
            TYPE_ROLES.map(index),
            [Some(32), Some(30), Some(29), Some(167), Some(98), Some(133)]
        );
        assert_eq!(t.style(Role::TypeOther), Style::default());
        assert!(
            TYPE_ROLES
                .iter()
                .all(|r| !t.style(*r).bold && !t.style(*r).italic)
        );
        let sheet = css(&t, &Theme::rtemis_light(), &Theme::rtemis_dark());
        assert!(sheet.contains("@media (prefers-color-scheme: dark) {"));
        let dark_block = &sheet[sheet
            .find("(prefers-color-scheme: dark)")
            .expect("a dark block")..];
        assert!(!dark_block.contains("r8-bracket"));
        assert!(sheet.contains("  .r8 .r8-type_number { color:var(--r8-type_number,#69AAEA) }"));
        assert!(sheet.contains("  .r8 .r8-type_number { color:var(--r8-type_number,#006BB9) }"));
        assert!(sheet.contains(".r8 .r8-type_number { color:var(--r8-type_number,#2E81CC) }"));
    }

    #[test]
    fn ansi_wraps_each_styled_span_and_reduces_to_plain() {
        let t = header();
        let a = encode_ansi(&t, &Theme::rtemis(), ColorDepth::Colors256);
        assert!(a.starts_with(
            "\u{1b}[38;5;73m<\u{1b}[0m\u{1b}[38;5;244mpkgr::\u{1b}[0m\u{1b}[1mResampler\u{1b}[0m"
        ));
        let true_color = encode_ansi(&t, &Theme::rtemis(), ColorDepth::TrueColor);
        assert!(true_color.contains("\u{1b}[38;2;108;163;160m<"));
        let stripped: String = {
            let mut out = String::new();
            let mut it = a.chars().peekable();
            while let Some(c) = it.next() {
                if c == '\u{1b}' {
                    for d in it.by_ref() {
                        if d == 'm' {
                            break;
                        }
                    }
                } else {
                    out.push(c);
                }
            }
            out
        };
        assert_eq!(stripped, encode_plain(&t));
    }

    #[test]
    fn html_escapes_text_and_styles_through_variables() {
        let t = Text {
            lines: vec![
                vec![
                    Span::new("<", Role::Bracket),
                    Span::new("a<b&\"c", Role::UserText),
                ]
                .into(),
            ],
        };
        let inline = encode_html(&t, &Theme::rtemis(), HtmlStyle::Inline);
        assert_eq!(
            inline,
            "<pre class=\"r8\"><span class=\"r8-line\" data-depth=\"0\"><span class=\"r8-bracket\" style=\"color:var(--r8-bracket,#6CA3A0)\">&lt;</span><span class=\"r8-user_text\">a&lt;b&amp;&quot;c</span></span></pre>"
        );
        let classes = encode_html(&t, &Theme::rtemis(), HtmlStyle::Classes);
        assert!(classes.contains("<span class=\"r8-bracket\">&lt;</span>"));
        let sheet = css(
            &Theme::rtemis(),
            &Theme::rtemis_light(),
            &Theme::rtemis_dark(),
        );
        assert!(sheet.contains(".r8 .r8-bracket { color:var(--r8-bracket,#6CA3A0) }"));
        assert!(sheet.contains("@media (prefers-color-scheme: light) {"));
        assert!(sheet.contains("  .r8 .r8-bracket { color:var(--r8-bracket,#0F6A66) }"));
    }

    #[test]
    fn markdown_fences_layouts_and_marks_impl_text() {
        let t = Text {
            lines: vec![
                header().lines[0].clone(),
                Line::new(1, vec![Span::new("k: 5", Role::Scalar)]),
            ],
        };
        assert_eq!(
            encode_markdown(&t, false),
            "```\n<pkgr::Resampler@KFold>\n  k: 5\n```"
        );
        assert_eq!(encode_markdown(&t, true), "`<pkgr::Resampler@KFold>; k: 5`");
        let own = Text {
            lines: vec![
                vec![
                    Span::new("cost", Role::Strong),
                    Span::new(" is *high*", Role::UserText),
                ]
                .into(),
            ],
        };
        assert_eq!(encode_markdown(&own, false), "**cost** is \\*high\\*");
    }

    fn facts() -> Facts {
        Facts {
            explicit: None,
            option: None,
            env: None,
            no_color: false,
            knitr: false,
            host: HostKind::Other,
            terminal: false,
            truecolor: false,
        }
    }

    #[test]
    fn resolution_reads_the_facts_in_order() {
        use ColorDepth::*;
        use OutputType::*;
        assert_eq!(resolve(&facts()), (Plain, Colors256));
        let terminal = Facts {
            terminal: true,
            ..facts()
        };
        assert_eq!(resolve(&terminal), (Ansi, Colors256));
        assert_eq!(
            resolve(&Facts {
                truecolor: true,
                ..terminal.clone()
            }),
            (Ansi, TrueColor)
        );
        assert_eq!(
            resolve(&Facts {
                no_color: true,
                ..terminal.clone()
            }),
            (Plain, Colors256)
        );
        assert_eq!(
            resolve(&Facts {
                knitr: true,
                ..terminal.clone()
            }),
            (Plain, Colors256)
        );
        assert_eq!(
            resolve(&Facts {
                no_color: true,
                explicit: Some(Html),
                ..terminal.clone()
            }),
            (Html, Colors256)
        );
        assert_eq!(
            resolve(&Facts {
                option: Some(Markdown),
                env: Some(Ansi),
                ..facts()
            }),
            (Markdown, Colors256)
        );
        assert_eq!(
            resolve(&Facts {
                host: HostKind::Positron,
                ..facts()
            }),
            (Ansi, TrueColor)
        );
        assert_eq!(
            resolve(&Facts {
                host: HostKind::RStudio {
                    console_color: true
                },
                ..facts()
            }),
            (Ansi, Colors256)
        );
        assert_eq!(
            resolve(&Facts {
                host: HostKind::RStudio {
                    console_color: false
                },
                ..facts()
            }),
            (Plain, Colors256)
        );
        assert_eq!(
            resolve(&Facts {
                host: HostKind::NoAnsiGui,
                terminal: true,
                ..facts()
            }),
            (Plain, Colors256)
        );
    }

    #[test]
    fn sizes_take_iec_units_and_three_significant_digits() {
        assert_eq!(format_size(0), "0 B");
        assert_eq!(format_size(120), "120 B");
        assert_eq!(format_size(1023), "1023 B");
        assert_eq!(format_size(1024), "1.00 KiB");
        assert_eq!(format_size(1536), "1.50 KiB");
        assert_eq!(format_size(8_000_048), "7.63 MiB");
        assert_eq!(format_size(80_000_048), "76.3 MiB");
        assert_eq!(format_size(800_000_048), "763 MiB");
        assert_eq!(format_size(1023 * 1024 + 900), "1020 KiB");
        assert_eq!(format_size(10_239), "10.0 KiB");
        assert_eq!(format_size(5 * 1024u64.pow(4)), "5.00 TiB");
        assert_eq!(format_size(2000 * 1024u64.pow(4)), "2000 TiB");
    }
}
