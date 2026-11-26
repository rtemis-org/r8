//! A cell as a file stores it, its canonical text, and the time zone of a clock
//! reading.
//!
//! spec: R8/io#the-crate

use std::borrow::Cow;

use rtemis_r8::canonical::{date_text, datetime_text};
use rtemis_r8::rules::Shown;

/// A cell as a file stores it: a text, as a delimited file gives it, or a typed
/// value, as Parquet gives it.
#[derive(Debug, Clone, PartialEq)]
pub enum Stored<'a> {
    /// A cell without a value: a Parquet null.
    Missing,
    /// A text. A quoted one is a text of its own, and the string of a typed format
    /// is one: in a column of text, `""` and `"NA"` are texts.
    Text {
        /// The characters.
        text: Cow<'a, str>,
        /// Whether the text is quoted.
        quoted: bool,
    },
    /// A logical.
    Logical(bool),
    /// A whole number.
    Integer(i64),
    /// A double.
    Double(f64),
    /// The days since 1970-01-01.
    Date(i32),
    /// The microseconds since 1970-01-01T00:00:00Z.
    DateTime(i64),
    /// A clock reading without a time zone, as the microseconds since 1970-01-01
    /// 00:00:00 of the same clock.
    Clock(i64),
}

impl Stored<'_> {
    /// The text that `write()` writes for the value in a delimited file, which the
    /// report lists, `recode` names, and the missing codes match; `None` for a
    /// missing cell.
    pub fn text(&self) -> Option<Cow<'_, str>> {
        Some(match self {
            Stored::Missing => return None,
            Stored::Text { text, .. } => Cow::Borrowed(text.as_ref()),
            Stored::Logical(b) => Cow::Borrowed(if *b { "TRUE" } else { "FALSE" }),
            Stored::Integer(i) => Cow::Owned(i.to_string()),
            Stored::Double(d) => Cow::Owned(number_text(*d)),
            Stored::Date(d) => Cow::Owned(date_text(i64::from(*d))),
            Stored::DateTime(m) => Cow::Owned(datetime_text(*m)),
            Stored::Clock(m) => {
                let mut t = datetime_text(*m);
                t.pop();
                Cow::Owned(t)
            }
        })
    }
}

/// A double in its shortest decimal form that reads back exactly, and `NaN`, `Inf`,
/// and `-Inf`.
pub fn number_text(x: f64) -> String {
    match Shown::number(x) {
        Shown::Number(t) => t,
        _ => x.to_string(),
    }
}

/// The time zone of the clock readings of a file: a date-time without an offset is
/// the instant at which a clock of this zone shows it.
#[derive(Debug, Clone)]
pub struct Zone {
    tz: Option<jiff::tz::TimeZone>,
}

impl Default for Zone {
    fn default() -> Self {
        Zone::utc()
    }
}

impl Zone {
    /// Coordinated Universal Time.
    pub fn utc() -> Zone {
        Zone { tz: None }
    }

    /// The zone of an IANA name, such as `America/New_York`, from the database that
    /// the crate holds; `None` for a name that it does not hold.
    pub fn named(name: &str) -> Option<Zone> {
        if name == "UTC" {
            return Some(Zone::utc());
        }
        jiff::tz::db()
            .get(name)
            .ok()
            .map(|tz| Zone { tz: Some(tz) })
    }

    /// The instant, in microseconds since 1970-01-01T00:00:00Z, at which a clock of
    /// the zone shows the reading; `None` where the clock never shows it or shows it
    /// twice, as at the changes of daylight saving time.
    pub fn instant(&self, clock: i64) -> Option<i64> {
        let Some(tz) = &self.tz else {
            return Some(clock);
        };
        let civil = jiff::Timestamp::from_microsecond(clock)
            .ok()?
            .to_zoned(jiff::tz::TimeZone::UTC)
            .datetime();
        let at = tz.to_ambiguous_timestamp(civil).unambiguous().ok()?;
        Some(at.as_microsecond())
    }
}
