//! Computations over slices, without R8: the bodies of the typed functions
//! of `functions`, which another host's binding can call the same way.

/// Each element of `x` held between `lo` and `hi`.
pub fn clip(x: &[f64], lo: f64, hi: f64) -> Vec<f64> {
    x.iter().map(|v| v.clamp(lo, hi)).collect()
}

/// The running sum of `x`, held between `lo` and `hi` after each step.
pub fn level(x: &[f64], lo: f64, hi: f64) -> Vec<f64> {
    let mut s = 0.0;
    x.iter()
        .map(|v| {
            s = (s + v).clamp(lo, hi);
            s
        })
        .collect()
}
