//! The exact sum of doubles, rounded once.
//!
//! [`ExactSum`] holds a sum as a fixed-point number that covers every double,
//! so the result is the double nearest the sum of the numbers, ties to even,
//! whatever their order and however they are divided into parts.

/// The limbs of 32 bits from 2^-1074, the least double, past 2^1024, with
/// room for the carry of 2^63 additions.
const LIMBS: usize = 70;

/// The additions after which the carries of the limbs are propagated.
const CARRY_AFTER: u32 = 1 << 30;

/// A sum of doubles.
#[derive(Clone)]
pub(crate) struct ExactSum {
    limbs: [i64; LIMBS],
    since: u32,
    /// The infinite and `NaN` numbers, added as doubles.
    special: f64,
}

impl Default for ExactSum {
    fn default() -> Self {
        ExactSum {
            limbs: [0; LIMBS],
            since: 0,
            special: 0.0,
        }
    }
}

impl ExactSum {
    /// Adds `x`.
    #[inline]
    pub fn add(&mut self, x: f64) {
        let bits = x.to_bits();
        let exponent = ((bits >> 52) & 0x7ff) as usize;
        let fraction = bits & ((1 << 52) - 1);
        if exponent == 0x7ff {
            self.special += x;
            return;
        }
        // The number is `mantissa * 2^(place - 1074)`.
        let (mantissa, place) = if exponent == 0 {
            (fraction, 0)
        } else {
            (fraction | 1 << 52, exponent - 1)
        };
        let (limb, shift) = (place / 32, place % 32);
        let wide = u128::from(mantissa) << shift;
        let sign = 1 - 2 * (bits >> 63) as i64;
        let part = |k: u32| (wide >> (32 * k)) as u32 as i64;
        self.limbs[limb] += sign * part(0);
        self.limbs[limb + 1] += sign * part(1);
        self.limbs[limb + 2] += sign * part(2);
        self.since += 1;
        if self.since == CARRY_AFTER {
            self.carry();
        }
    }

    /// Moves what a limb holds beyond 32 bits to the next one, so that every
    /// limb but the last lies in `0..2^32`.
    fn carry(&mut self) {
        let mut c = 0i64;
        for l in &mut self.limbs[..LIMBS - 1] {
            let v = *l + c;
            *l = v & 0xffff_ffff;
            c = v >> 32;
        }
        self.limbs[LIMBS - 1] += c;
        self.since = 0;
    }

    /// The double nearest the sum.
    pub fn value(&self) -> f64 {
        if self.special != 0.0 {
            return self.special;
        }
        let mut s = self.clone();
        s.carry();
        let negative = s.limbs[LIMBS - 1] < 0;
        if negative {
            s.limbs.iter_mut().for_each(|l| *l = -*l);
            s.carry();
        }
        let Some(top) = s.limbs.iter().rposition(|&l| l != 0) else {
            return 0.0;
        };
        // The three highest limbs, and whether a lower one is not zero.
        let low = top.saturating_sub(2);
        let mut head = 0u128;
        for &l in s.limbs[low..=top].iter().rev() {
            head = head << 32 | l as u128;
        }
        let rest = s.limbs[..low].iter().any(|&l| l != 0);
        let mut exponent = 32 * low as i32 - 1074;
        let bits = 128 - head.leading_zeros() as i32;
        if bits > 53 {
            let shift = bits - 53;
            let half = 1u128 << (shift - 1);
            let tail = head & ((1u128 << shift) - 1);
            head >>= shift;
            exponent += shift;
            if tail > half || (tail == half && (rest || head & 1 == 1)) {
                head += 1;
            }
        }
        // `head` has at most 53 bits, so each factor and product is exact
        // unless it overflows.
        let mut x = head as f64;
        while exponent > 0 {
            let step = exponent.min(1000);
            x *= 2f64.powi(step);
            exponent -= step;
        }
        while exponent < 0 {
            let step = exponent.max(-1000);
            x *= 2f64.powi(step);
            exponent -= step;
        }
        if negative { -x } else { x }
    }
}

#[cfg(test)]
mod tests {
    use super::ExactSum;

    fn sum(xs: &[f64]) -> f64 {
        let mut s = ExactSum::default();
        xs.iter().for_each(|&x| s.add(x));
        s.value()
    }

    #[test]
    fn the_sum_is_rounded_once() {
        assert_eq!(sum(&[]), 0.0);
        assert_eq!(sum(&[0.1, 0.2, 0.3]), 0.6);
        assert_eq!(sum(&[1e100, 1.0, -1e100]), 1.0);
        assert_eq!(sum(&[1e308, 1e308, -1e308]), 1e308);
        assert_eq!(sum(&[1e308, 1e308]), f64::INFINITY);
        assert_eq!(sum(&[-1e308, -1e308]), f64::NEG_INFINITY);
        assert_eq!(sum(&[f64::MAX]), f64::MAX);
        assert_eq!(sum(&[-5e-324, -5e-324]), -1e-323);
        assert_eq!(sum(&[f64::MIN_POSITIVE, -5e-324]), 2.225073858507201e-308);
        // A tie rounds to even, and a lower limb decides against it.
        let half = 2f64.powi(-53);
        assert_eq!(sum(&[1.0, half]), 1.0);
        assert_eq!(sum(&[1.0, half, 2f64.powi(-200)]), 1.0 + 2.0 * half);
        assert_eq!(sum(&[1.0 + 2.0 * half, half]), 1.0 + 4.0 * half);
        assert!(sum(&[f64::INFINITY, f64::NEG_INFINITY]).is_nan());
        assert!(sum(&[f64::NAN, 1.0]).is_nan());
        assert_eq!(sum(&[f64::INFINITY, 1.0]), f64::INFINITY);
    }

    #[test]
    fn the_order_of_the_numbers_does_not_matter() {
        let mut x = 0x9e37_79b9_7f4a_7c15u64;
        let mut xs = Vec::new();
        for _ in 0..5000 {
            x ^= x << 13;
            x ^= x >> 7;
            x ^= x << 17;
            let e = (x % 120) as i32 - 60;
            xs.push((x >> 12) as f64 * 2f64.powi(e) * if x & 1 == 0 { 1.0 } else { -1.0 });
        }
        let one = sum(&xs);
        xs.reverse();
        assert_eq!(one, sum(&xs));
        xs.sort_by(f64::total_cmp);
        assert_eq!(one, sum(&xs));
        let negated: Vec<f64> = xs.iter().map(|x| -x).collect();
        assert_eq!(-one, sum(&negated));
    }
}

/// A sum of doubles as a pair of doubles, with a bound on its error, which
/// gives the exact sum, rounded once, where the bound decides it.
///
/// Each addition splits into the rounded sum `hi` and its exact error, which
/// `lo` accumulates, so `hi` and the errors sum exactly to the numbers. Only
/// the additions to `lo` round, by at most `2 n u` times the sum of the
/// errors' magnitudes, `err`, for `n` additions and the unit roundoff `u`.
#[derive(Clone, Copy, Default)]
pub(crate) struct BoundedSum {
    hi: f64,
    lo: f64,
    err: f64,
    n: u64,
}

/// The unit roundoff of a double, 2^-53.
const UNIT: f64 = f64::EPSILON / 2.0;

/// `a + b` and its exact error (Knuth's TwoSum).
#[inline]
fn two_sum(a: f64, b: f64) -> (f64, f64) {
    let s = a + b;
    let bb = s - a;
    (s, (a - (s - bb)) + (b - bb))
}

impl BoundedSum {
    /// Adds `x`.
    #[inline]
    pub fn add(&mut self, x: f64) {
        let (s, e) = two_sum(self.hi, x);
        self.hi = s;
        self.lo += e;
        self.err += e.abs();
        self.n += 1;
    }

    /// The numbers added.
    pub fn count(&self) -> u64 {
        self.n
    }

    /// The exact sum, rounded once, ties to even; `None` where the bound does
    /// not decide the rounding, or a number is not finite.
    pub fn value(&self) -> Option<f64> {
        if !(self.hi.is_finite() && self.lo.is_finite() && self.err.is_finite()) {
            return None;
        }
        let (r, t) = two_sum(self.hi, self.lo);
        if !r.is_finite() {
            return None;
        }
        // Without a rounded addition to `lo`, `hi + lo` is the exact sum,
        // which the addition `r` rounds once.
        if self.err == 0.0 {
            return Some(r);
        }
        // The exact sum is `r + t + d`, with `|d|` at most `bound`; it rounds
        // to `r` where it lies strictly between the midpoints of `r` and its
        // neighbors, whose gaps differ at a power of 2. The factors of 2
        // cover the rounding of `bound` and of the margins.
        let bound = 4.0 * self.n as f64 * UNIT * self.err;
        let up = (r.next_up() - r) / 2.0 - t;
        let down = (r - r.next_down()) / 2.0 + t;
        (up > 2.0 * bound && down > 2.0 * bound).then_some(r)
    }
}

#[cfg(test)]
mod bounded_tests {
    use super::{BoundedSum, ExactSum};

    struct Random(u64);

    impl Random {
        fn next(&mut self) -> u64 {
            self.0 ^= self.0 << 13;
            self.0 ^= self.0 >> 7;
            self.0 ^= self.0 << 17;
            self.0
        }
        fn number(&mut self) -> f64 {
            let unit = (self.next() >> 11) as f64 / (1u64 << 53) as f64 - 0.5;
            let scale = 2f64.powi((self.next() % 80) as i32 - 40);
            unit * scale
        }
    }

    #[test]
    fn a_bounded_sum_is_the_exact_sum_where_it_decides() {
        let mut r = Random(0x9e37_79b9_7f4a_7c15);
        let mut decided = 0;
        for case in 0..4000usize {
            let n = 1 + (r.next() % 200) as usize;
            let mut xs: Vec<f64> = (0..n).map(|_| r.number()).collect();
            // Cancellation: a large number and its negative.
            if case % 3 == 0 {
                xs.push(1e16);
                xs.push(-1e16);
            }
            // Ties: halves of the last place.
            if case % 5 == 0 {
                xs = vec![
                    1.0,
                    f64::EPSILON / 2.0,
                    f64::EPSILON / 4.0,
                    -f64::EPSILON / 4.0,
                ];
            }
            let mut exact = ExactSum::default();
            let mut bounded = BoundedSum::default();
            for &x in &xs {
                exact.add(x);
                bounded.add(x);
            }
            // A sum that lies on a midpoint, or that cancels, is not decided.
            if let Some(v) = bounded.value() {
                assert_eq!(v.to_bits(), exact.value().to_bits(), "{xs:?}");
                decided += 1;
            }
        }
        assert!(decided > 2000, "{decided}");
    }

    #[test]
    fn a_bounded_sum_does_not_decide_what_overflows() {
        let mut b = BoundedSum::default();
        b.add(f64::MAX);
        b.add(f64::MAX);
        assert_eq!(b.value(), None);
        let mut b = BoundedSum::default();
        b.add(f64::INFINITY);
        assert_eq!(b.value(), None);
    }
}
