//! The text of a scalar, in each of two grammars.
//!
//! A declared type reads every text that has one meaning as that type: `+5`, `007`,
//! and `5.` are numbers. Inference reads a text as a type where the type gives the
//! text's whole meaning back: `007` may be an identifier, and is text. Spaces around
//! a number, a logical, or a date are not part of it.

use rtemis_r8::canonical::days_from_civil;

/// The greatest integer below which a double holds every one.
const WHOLE: f64 = 9_007_199_254_740_992.0;

/// The text without the spaces and tabs around it.
pub fn trimmed(t: &str) -> &str {
    t.trim_matches([' ', '\t'])
}

/// `TRUE`, `True`, `true`, and their opposites.
pub fn logical(t: &str) -> Option<bool> {
    match t {
        "TRUE" | "True" | "true" => Some(true),
        "FALSE" | "False" | "false" => Some(false),
        _ => None,
    }
}

/// The digits of a decimal number: those before and after the point, and of the
/// exponent.
struct Decimal<'t> {
    plus: bool,
    whole: &'t str,
    point: bool,
    fraction: &'t str,
    exponent: bool,
}

fn decimal(t: &str) -> Option<Decimal<'_>> {
    let digits = |t: &str| t.bytes().take_while(u8::is_ascii_digit).count();
    let plus = t.starts_with('+');
    let t = t.strip_prefix(['-', '+']).unwrap_or(t);
    let n = digits(t);
    let (whole, mut rest) = t.split_at(n);
    let mut fraction = "";
    let point = rest.starts_with('.');
    if point {
        let after = &rest[1..];
        let k = digits(after);
        (fraction, rest) = after.split_at(k);
    }
    if whole.is_empty() && fraction.is_empty() {
        return None;
    }
    let exponent = rest.starts_with(['e', 'E']);
    if exponent {
        let after = &rest[1..];
        let after = after.strip_prefix(['-', '+']).unwrap_or(after);
        if after.is_empty() || digits(after) != after.len() {
            return None;
        }
    } else if !rest.is_empty() {
        return None;
    }
    Some(Decimal {
        plus,
        whole,
        point,
        fraction,
        exponent,
    })
}

/// A whole number in the domain of R8's integers, -2147483647 to 2147483647.
pub fn integer(t: &str, declared: bool) -> Option<i32> {
    // The common text: digits without a sign and a zero before them.
    let b = t.as_bytes();
    if (1..10).contains(&b.len())
        && (b[0] != b'0' || b.len() == 1)
        && b.iter().all(u8::is_ascii_digit)
    {
        return Some(b.iter().fold(0, |v, c| v * 10 + i32::from(c - b'0')));
    }
    let d = decimal(t)?;
    let zeros = d.fraction.bytes().all(|b| b == b'0');
    let whole = if declared {
        // `40.0` is 40.
        !d.exponent && zeros && !d.whole.is_empty()
    } else {
        !d.plus && !d.point && !d.exponent && (d.whole.len() == 1 || !d.whole.starts_with('0'))
    };
    if !whole || (!declared && t == "-0") {
        return None;
    }
    let sign = if t.starts_with('-') { -1 } else { 1 };
    let v = d.whole.parse::<i64>().ok()? * sign;
    i32::try_from(v).ok().filter(|v| *v != i32::MIN)
}

/// A double, `NaN`, or an infinite value.
pub fn double(t: &str, declared: bool) -> Option<f64> {
    match t {
        "NaN" => return Some(f64::NAN),
        "Inf" | "Infinity" => return Some(f64::INFINITY),
        "-Inf" | "-Infinity" => return Some(f64::NEG_INFINITY),
        _ => {}
    }
    let d = decimal(t)?;
    if !declared && (d.plus || (d.whole.len() > 1 && d.whole.starts_with('0'))) {
        return None;
    }
    let v: f64 = t.parse().ok().filter(|v: &f64| v.is_finite())?;
    // A long number without a point is an identifier more often than a quantity,
    // and a double does not hold its digits.
    if !declared && !d.point && !d.exponent && v.abs() > WHOLE {
        return None;
    }
    Some(v)
}

/// The days since 1970-01-01 of an ISO 8601 date, `2020-01-31`.
pub fn date(t: &str) -> Option<i32> {
    let b: &[u8; 10] = t.as_bytes().try_into().ok()?;
    if b[4] != b'-' || b[7] != b'-' {
        return None;
    }
    let mut n = [0i64; 3];
    for (v, part) in n.iter_mut().zip([&b[..4], &b[5..7], &b[8..]]) {
        for c in part {
            if !c.is_ascii_digit() {
                return None;
            }
            *v = *v * 10 + i64::from(c - b'0');
        }
    }
    let [y, m, d] = n;
    let leap = y % 4 == 0 && (y % 100 != 0 || y % 400 == 0);
    let last = match m {
        4 | 6 | 9 | 11 => 30,
        2 => 28 + i64::from(leap),
        _ => 31,
    };
    if !(1..=12).contains(&m) || !(1..=last).contains(&d) {
        return None;
    }
    i32::try_from(days_from_civil(y, m, d)).ok()
}

/// The microseconds since 1970-01-01T00:00:00Z of an RFC 3339 date-time with an
/// offset: `2020-01-31T12:00:00Z`, `2020-01-31 12:00:00.5+02:00`.
pub fn datetime(t: &str) -> Option<i64> {
    let (clock, at) = clock_part(t)?;
    let b = t.as_bytes();
    let offset = match b.get(at)? {
        b'Z' if b.len() == at + 1 => 0,
        sign @ (b'+' | b'-') if b.len() == at + 6 && b[at + 3] == b':' => {
            let (h, m) = (two(b, at + 1)?, two(b, at + 4)?);
            if h > 23 || m > 59 {
                return None;
            }
            let o = h * 3600 + m * 60;
            if *sign == b'-' { -o } else { o }
        }
        _ => return None,
    };
    Some(clock - offset * 1_000_000)
}

/// The clock reading of a date-time without an offset, `2020-01-31 12:00:00` or
/// `2020-01-31T12:00:00.5`, as the microseconds since 1970-01-01 00:00:00 of the same
/// clock.
pub fn clock(t: &str) -> Option<i64> {
    let (clock, at) = clock_part(t)?;
    (at == t.len()).then_some(clock)
}

/// Two digits at `k`.
fn two(b: &[u8], k: usize) -> Option<i64> {
    let (x, y) = (*b.get(k)?, *b.get(k + 1)?);
    (x.is_ascii_digit() && y.is_ascii_digit())
        .then(|| i64::from(x - b'0') * 10 + i64::from(y - b'0'))
}

/// The microseconds of the date and the time of day at the start of a date-time,
/// and the place after them.
fn clock_part(t: &str) -> Option<(i64, usize)> {
    let b = t.as_bytes();
    if b.len() < 19 || !matches!(b[10], b'T' | b' ') || b[13] != b':' || b[16] != b':' {
        return None;
    }
    let days = i64::from(date(t.get(..10)?)?);
    let (h, m, s) = (two(b, 11)?, two(b, 14)?, two(b, 17)?);
    if h > 23 || m > 59 || s > 59 {
        return None;
    }
    let mut at = 19;
    let mut micros = 0;
    if b.get(at) == Some(&b'.') {
        let n = b[at + 1..]
            .iter()
            .take_while(|b| b.is_ascii_digit())
            .count();
        if n == 0 || n > 6 {
            return None;
        }
        for k in 0..6 {
            let d = if k < n {
                i64::from(b[at + 1 + k] - b'0')
            } else {
                0
            };
            micros = micros * 10 + d;
        }
        at += 1 + n;
    }
    Some((
        (days * 86_400 + h * 3600 + m * 60 + s) * 1_000_000 + micros,
        at,
    ))
}
