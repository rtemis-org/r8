//! The messages of R8's functions, which every host writes alike: the wording
//! of each message, and the line that a host prints, `<timestamp> <text>
//! [<caller>]`, the layout of rtemis.core's `msg()`. A message is an [`Event`];
//! the host gives the timestamp, its own caller where it has one, and colors,
//! and prints the line on its thread, or hands the event to a host's function.
//! A function builds a message only at a verbosity of 1 or more.
//!
//! spec: R8/raven#summaries

/// A message: its level, its phase, `"start"` or `"end"` of a map, its
/// text, and the function that writes it.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Event {
    /// The severity, `"info"`.
    pub level: &'static str,
    /// The moment of the work the message marks, `"start"` or `"end"`.
    pub phase: &'static str,
    /// The message.
    pub text: String,
    /// The function that writes it, such as `"r8map"`.
    pub caller: String,
}

/// The code that makes the calls of a map's typed functions: R8's Rust, as for
/// a table, or a loop of the host, as for a list.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Path {
    /// R8's Rust code calls the bodies.
    Rust,
    /// A loop of the host calls them.
    Host,
}

/// A typed function that a map calls, and whether its body is compiled code.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Body {
    /// The function's qualified name.
    pub function: String,
    /// Whether a package registered its body in compiled code.
    pub in_rust: bool,
}

/// Where the work of a map runs: the code that makes its calls, and the
/// bodies of the functions it calls.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Place {
    /// The code that makes the calls.
    pub path: Path,
    /// The functions called.
    pub bodies: Vec<Body>,
}

/// The languages a map's work runs in.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Engine {
    /// Rust alone.
    Rust,
    /// The host's language alone.
    Host,
    /// Rust and the host's language.
    Both,
}

impl Engine {
    /// The engine's name for a host whose language is R: `"rust"`, `"r"`, or
    /// `"rust+r"`.
    pub fn name(self) -> &'static str {
        match self {
            Engine::Rust => "rust",
            Engine::Host => "r",
            Engine::Both => "rust+r",
        }
    }
}

impl Place {
    /// The languages the work runs in: a table's map runs in Rust, with the
    /// bodies in the host's language of the functions that have them; a list's
    /// map runs in the host, with the compiled bodies in Rust.
    pub fn engine(&self) -> Engine {
        let other = match self.path {
            Path::Rust => self.bodies.iter().any(|b| !b.in_rust),
            Path::Host => self.bodies.iter().any(|b| b.in_rust),
        };
        match (self.path, other) {
            (Path::Rust, false) => Engine::Rust,
            (Path::Host, false) => Engine::Host,
            _ => Engine::Both,
        }
    }

    /// `in Rust`, or `in Rust, with the body of `f` in R`, as `host` names the
    /// host's language.
    pub fn text(&self, host: &str) -> String {
        let (lang, other, other_lang) = match self.path {
            Path::Rust => ("Rust", false, host),
            Path::Host => (host, true, "Rust"),
        };
        let others: Vec<&str> = self
            .bodies
            .iter()
            .filter(|b| b.in_rust == other)
            .map(|b| b.function.as_str())
            .collect();
        if others.is_empty() {
            return format!("in {lang}");
        }
        format!(
            "in {lang}, with the {} of {} in {other_lang}",
            if others.len() == 1 { "body" } else { "bodies" },
            listed(&others)
        )
    }
}

/// The size of a map: its columns and groups, or its items, and its calls.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Size {
    /// The columns mapped, of a table.
    pub columns: Option<u64>,
    /// The groups of its rows.
    pub groups: Option<u64>,
    /// The items, of a list or a vector.
    pub items: Option<u64>,
    /// The calls of the typed function.
    pub calls: Option<u64>,
}

/// The text of the start of a long map of `label`:
/// `r8map(): pkgr::f over 10 columns and 100,000 groups, 1,000,000 calls, in Rust`.
pub fn map_started(label: &str, size: &Size, place: &Place, host: &str) -> String {
    started("r8map", label, size, place, host)
}

/// The text of the start of the work of `function`, as [`map_started`]
/// writes it for `r8map`.
pub(crate) fn started(
    function: &str,
    label: &str,
    size: &Size,
    place: &Place,
    host: &str,
) -> String {
    let mut parts = Vec::new();
    match (size.columns, size.groups, size.items) {
        (Some(c), Some(g), _) => parts.push(format!(
            "{} and {}",
            counted(c, "column", "columns"),
            counted(g, "group", "groups")
        )),
        (Some(c), None, _) => parts.push(counted(c, "column", "columns")),
        (None, _, Some(i)) => parts.push(counted(i, "item", "items")),
        _ => {}
    }
    let over = if parts.is_empty() {
        String::new()
    } else {
        format!(" over {}", parts.join(""))
    };
    let calls = size
        .calls
        .map(|n| format!(", {}", counted(n, "call", "calls")))
        .unwrap_or_default();
    format!("{function}(): {label}{over}{calls}, {}", place.text(host))
}

/// The text of the end of a long map: its wall time, the CPU time of the
/// process, the threads that its work may use, and where it ran:
/// `r8map(): completed in 3.3 s, CPU 3.2 s of 1 thread, in Rust`.
pub fn map_ended(wall: f64, cpu: f64, threads: u64, place: &Place, host: &str) -> String {
    format!(
        "r8map(): completed in {} s, CPU {} s of {}, {}",
        seconds(wall),
        seconds(cpu),
        counted(threads, "thread", "threads"),
        place.text(host)
    )
}

/// The line of a message, `<timestamp> <text> [<caller>]`; without a caller,
/// the timestamp and the text.
pub fn line(timestamp: &str, text: &str, caller: Option<&str>) -> String {
    match caller.filter(|c| !c.is_empty()) {
        Some(c) => format!("{timestamp} {text} [{c}]"),
        None => format!("{timestamp} {text}"),
    }
}

/// `n` with its noun, the digits grouped by thousands: `1,000,000 calls`.
pub(crate) fn counted(n: u64, one: &str, many: &str) -> String {
    format!("{} {}", grouped(n), if n == 1 { one } else { many })
}

/// The digits of `n` in groups of three, separated by commas.
fn grouped(n: u64) -> String {
    let digits = n.to_string();
    let mut out = String::with_capacity(digits.len() + digits.len() / 3);
    for (i, c) in digits.chars().enumerate() {
        if i > 0 && (digits.len() - i).is_multiple_of(3) {
            out.push(',');
        }
        out.push(c);
    }
    out
}

/// A time in seconds: two decimals below 1 s, one below 10 s, and none above.
fn seconds(x: f64) -> String {
    let x = x.max(0.0);
    if x < 1.0 {
        format!("{x:.2}")
    } else if x < 10.0 {
        format!("{x:.1}")
    } else {
        format!("{x:.0}")
    }
}

/// `` `a` ``, `` `a` and `b` ``, or `` `a`, `b`, and `c` ``.
fn listed(names: &[&str]) -> String {
    let q: Vec<String> = names.iter().map(|n| format!("`{n}`")).collect();
    match q.len() {
        0 => String::new(),
        1 => q[0].clone(),
        2 => format!("{} and {}", q[0], q[1]),
        n => format!("{}, and {}", q[..n - 1].join(", "), q[n - 1]),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn place(path: Path, bodies: &[(&str, bool)]) -> Place {
        Place {
            path,
            bodies: bodies
                .iter()
                .map(|(f, r)| Body {
                    function: f.to_string(),
                    in_rust: *r,
                })
                .collect(),
        }
    }

    #[test]
    fn a_place_names_the_bodies_in_the_other_language() {
        let p = place(Path::Rust, &[("pkgr::spread", true)]);
        assert_eq!((p.engine(), p.text("R")), (Engine::Rust, "in Rust".into()));
        let p = place(Path::Rust, &[("pkgr::a", false), ("pkgr::b", true)]);
        assert_eq!(p.engine().name(), "rust+r");
        assert_eq!(p.text("R"), "in Rust, with the body of `pkgr::a` in R");
        let p = place(Path::Host, &[("pkgr::a", true), ("pkgr::b", true)]);
        assert_eq!(
            p.text("R"),
            "in R, with the bodies of `pkgr::a` and `pkgr::b` in Rust"
        );
        assert_eq!(place(Path::Host, &[("f", false)]).engine(), Engine::Host);
    }

    #[test]
    fn the_lines_of_a_map() {
        let p = place(Path::Rust, &[("pkgr::spread", true)]);
        let size = Size {
            columns: Some(10),
            groups: Some(100_000),
            calls: Some(1_000_000),
            ..Size::default()
        };
        assert_eq!(
            map_started("pkgr::spread", &size, &p, "R"),
            "r8map(): pkgr::spread over 10 columns and 100,000 groups, 1,000,000 calls, in Rust"
        );
        assert_eq!(
            map_ended(3.31, 3.2, 1, &p, "R"),
            "r8map(): completed in 3.3 s, CPU 3.2 s of 1 thread, in Rust"
        );
        assert_eq!(
            line("2026-11-27 10:04:12", "r8map(): completed", Some("r8map")),
            "2026-11-27 10:04:12 r8map(): completed [r8map]"
        );
        assert_eq!(seconds(0.254), "0.25");
        assert_eq!(seconds(12.4), "12");
        assert_eq!(grouped(999), "999");
    }
}
