//! The bodies of the typed functions that `R/functions.R` declares with
//! `rust()`: `clip()` and `level()`, which call `levels`, a module without
//! R8; `tally()`, of each borrowed parameter type, an R value, a default
//! written in R, and `...`; `fraction()`, which returns an error, panics,
//! and returns a result that its return type refuses; and `unwound()`, which
//! returns the error code of a jump of R without one.

use std::sync::atomic::{AtomicI32, Ordering};

use rtemis_r8_api as r8;

/// Holds each element between `lo` and `hi`.
#[r8::function]
pub fn clip(x: &[f64], lo: f64, hi: f64) -> Vec<f64> {
    crate::levels::clip(x, lo, hi)
}

/// Holds a running sum between `lo` and `hi`.
#[r8::function]
pub fn level(x: &[f64], lo: f64, hi: f64) -> Vec<f64> {
    crate::levels::level(x, lo, hi)
}

/// The arguments as text.
#[r8::function]
#[allow(clippy::too_many_arguments)]
pub fn tally(
    x: &[i32],
    label: &str,
    flags: Option<&[bool]>,
    bytes: &[u8],
    #[r8(name = "na.rm")] na_rm: bool,
    data: r8::Sexp,
    k: i32,
    dots: r8::Dots,
) -> String {
    let sum: i64 = x.iter().map(|v| i64::from(*v)).sum();
    let flags = flags.map_or_else(
        || "none".to_string(),
        |f| f.iter().filter(|b| **b).count().to_string(),
    );
    let data = if data.0 == unsafe { r8::sys::R_NilValue } {
        "NULL"
    } else {
        "an R value"
    };
    format!(
        "{label}: sum {sum}, flags {flags}, bytes {}, na.rm {na_rm}, k {k}, {} more, data {data}",
        bytes.len(),
        dots.len()
    )
}

/// `part / whole`; an error for a `whole` of 0, and a panic for a negative
/// `part`.
#[r8::function]
pub fn fraction(part: f64, whole: f64) -> r8::Result<f64> {
    if part < 0.0 {
        panic!("a negative part");
    }
    if whole == 0.0 {
        return Err(r8::Error::new("zero_whole", "`whole` is not 0"));
    }
    Ok(part / whole)
}

/// An error whose code is that of a jump of R that `R8Api.protect` caught,
/// without one.
#[r8::function]
pub fn unwound() -> r8::Result<f64> {
    Err(r8::Error::new("unwind", "no jump of R was caught"))
}

/// The largest value less the smallest; `NaN` for no values.
#[r8::function]
pub fn spread(x: &[f64]) -> f64 {
    let (lo, hi) = x
        .iter()
        .fold((f64::INFINITY, f64::NEG_INFINITY), |(lo, hi), &v| {
            (lo.min(v), hi.max(v))
        });
    if x.is_empty() { f64::NAN } else { hi - lo }
}

/// `scale` times the sum of `x` over the sum of `whole`; an error where
/// `whole` sums to 0.
#[r8::function]
pub fn portion(x: &[f64], whole: &[f64], scale: f64) -> r8::Result<f64> {
    let w: f64 = whole.iter().sum();
    if w == 0.0 {
        return Err(r8::Error::new("zero_whole", "`whole` sums to 0"));
    }
    Ok(scale * x.iter().sum::<f64>() / w)
}

/// The calls of `counted()`.
static COUNTED: AtomicI32 = AtomicI32::new(0);

/// The sum of `x`, counting its calls; an error for a negative value.
#[r8::function]
pub fn counted(x: &[f64]) -> r8::Result<f64> {
    COUNTED.fetch_add(1, Ordering::Relaxed);
    if x.iter().any(|v| *v < 0.0) {
        return Err(r8::Error::new("negative", "`x` holds a negative value"));
    }
    Ok(x.iter().sum())
}

/// The calls of `counted()` so far.
#[r8::function]
pub fn count_of() -> i32 {
    COUNTED.load(Ordering::Relaxed)
}

/// The length of `x`, after R allocates a vector of `n` doubles, inside
/// `R8Api.protect`.
#[r8::function]
pub fn filled(x: &[f64], n: i32) -> r8::Result<f64> {
    let v = vec![0.0_f64; usize::try_from(n).unwrap_or(0)];
    match r8::IntoR::into_r(v) {
        Ok(_) => Ok(x.len() as f64),
        Err(r8::Jumped::Unwind(e) | r8::Jumped::Error(e)) => Err(e),
    }
}
