//! The text of a file.

use std::borrow::Cow;
use std::fmt;

/// How the bytes of a file encode its text.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum Encoding {
    /// UTF-8, or UTF-16 where the file begins with its byte-order mark.
    #[default]
    Utf8,
    /// ISO 8859-1.
    Latin1,
    /// Windows code page 1252.
    Windows1252,
}

/// A file that is no text in the encoding.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NotText {
    /// The line, from 1.
    pub line: u64,
    /// The byte of the line, from 1.
    pub byte: u64,
    /// Whether the byte is NUL, which no text holds.
    pub nul: bool,
}

impl fmt::Display for NotText {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let what = if self.nul {
            "NUL"
        } else {
            "not valid in the encoding"
        };
        write!(f, "byte {} of line {} is {what}", self.byte, self.line)
    }
}

impl std::error::Error for NotText {}

/// The characters of Windows 1252 from 0x80 to 0x9F; U+FFFD for a byte without one.
const WINDOWS_1252: [char; 32] = [
    '\u{20AC}', '\u{FFFD}', '\u{201A}', '\u{0192}', '\u{201E}', '\u{2026}', '\u{2020}', '\u{2021}',
    '\u{02C6}', '\u{2030}', '\u{0160}', '\u{2039}', '\u{0152}', '\u{FFFD}', '\u{017D}', '\u{FFFD}',
    '\u{FFFD}', '\u{2018}', '\u{2019}', '\u{201C}', '\u{201D}', '\u{2022}', '\u{2013}', '\u{2014}',
    '\u{02DC}', '\u{2122}', '\u{0161}', '\u{203A}', '\u{0153}', '\u{FFFD}', '\u{017E}', '\u{0178}',
];

/// The text of `bytes`, without its byte-order mark. UTF-8 is not copied.
pub fn decode(bytes: &[u8], encoding: Encoding) -> Result<Cow<'_, str>, NotText> {
    let at = |k: usize, nul: bool| {
        let before = &bytes[..k];
        let start = memchr::memrchr(b'\n', before).map_or(0, |i| i + 1);
        NotText {
            line: memchr::memchr_iter(b'\n', before).count() as u64 + 1,
            byte: (k - start) as u64 + 1,
            nul,
        }
    };
    let text = match encoding {
        Encoding::Utf8 => {
            let wide = |to: fn([u8; 2]) -> u16| {
                let units: Vec<u16> = bytes[2..]
                    .chunks(2)
                    .map(|c| to([c[0], c.get(1).copied().unwrap_or(0)]))
                    .collect();
                String::from_utf16(&units).map_err(|_| at(0, false))
            };
            if bytes.starts_with(&[0xFF, 0xFE]) {
                Cow::Owned(wide(u16::from_le_bytes)?)
            } else if bytes.starts_with(&[0xFE, 0xFF]) {
                Cow::Owned(wide(u16::from_be_bytes)?)
            } else {
                let bytes = bytes.strip_prefix(b"\xEF\xBB\xBF").unwrap_or(bytes);
                match std::str::from_utf8(bytes) {
                    Ok(t) => Cow::Borrowed(t),
                    Err(e) => return Err(at(e.valid_up_to(), false)),
                }
            }
        }
        Encoding::Latin1 => Cow::Owned(bytes.iter().map(|&b| b as char).collect()),
        Encoding::Windows1252 => {
            let mut out = String::with_capacity(bytes.len());
            for (k, &b) in bytes.iter().enumerate() {
                out.push(match b {
                    0x80..=0x9F => match WINDOWS_1252[usize::from(b - 0x80)] {
                        '\u{FFFD}' => return Err(at(k, false)),
                        c => c,
                    },
                    b => b as char,
                });
            }
            Cow::Owned(out)
        }
    };
    match memchr::memchr(0, text.as_bytes()) {
        None => Ok(text),
        Some(k) => {
            let before = &text.as_bytes()[..k];
            let start = memchr::memrchr(b'\n', before).map_or(0, |i| i + 1);
            Err(NotText {
                line: memchr::memchr_iter(b'\n', before).count() as u64 + 1,
                byte: (k - start) as u64 + 1,
                nul: true,
            })
        }
    }
}
