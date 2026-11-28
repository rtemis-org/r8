//! Work over ranges of rows on several threads, and the kernels of
//! aggregates over a column that the host holds as one array.
//!
//! The calling thread takes the first range and the host's hook
//! (`crate::progress`), which may end the work by unwinding; the others take
//! the rest and stop at the next block when it does. Each kernel keeps states
//! for each group that merge exactly, so that a result is the same for any
//! number of threads.

use std::panic::{AssertUnwindSafe, catch_unwind, resume_unwind};
use std::sync::atomic::{AtomicBool, Ordering};

use super::compile::Op;
use super::eval::extreme;
use super::exact::PairSum;

/// The rows of a range below which work stays on one thread.
const ROWS_PER_THREAD: usize = 1 << 16;

/// The rows between two checks of the hook or of the cancel flag.
const STRIDE: usize = 1 << 14;

/// The threads that work over `rows` rows takes, of at most `threads`: one
/// for each range of at least `ROWS_PER_THREAD` rows.
pub(super) fn threads_for(rows: usize, threads: usize) -> usize {
    threads.clamp(1, (rows / ROWS_PER_THREAD).max(1))
}

/// `f` of ranges of `0..rows`, in order, on at most `threads` threads.
pub(super) fn over_rows<T: Send>(
    rows: usize,
    threads: usize,
    f: impl Fn(usize, usize, &AtomicBool) -> T + Sync,
) -> Vec<T> {
    let cancel = AtomicBool::new(false);
    let threads = threads_for(rows, threads);
    if threads == 1 {
        return vec![f(0, rows, &cancel)];
    }
    let bounds: Vec<(usize, usize)> = (0..threads)
        .map(|t| (rows * t / threads, rows * (t + 1) / threads))
        .collect();
    std::thread::scope(|s| {
        let others: Vec<_> = bounds[1..]
            .iter()
            .map(|&(lo, hi)| {
                let (f, cancel) = (&f, &cancel);
                s.spawn(move || f(lo, hi, cancel))
            })
            .collect();
        let first = catch_unwind(AssertUnwindSafe(|| f(bounds[0].0, bounds[0].1, &cancel)));
        if first.is_err() {
            cancel.store(true, Ordering::Relaxed);
        }
        let rest: Vec<_> = others.into_iter().map(|h| h.join()).collect();
        let first = first.unwrap_or_else(|p| resume_unwind(p));
        let mut out = vec![first];
        for r in rest {
            out.push(r.unwrap_or_else(|p| resume_unwind(p)));
        }
        out
    })
}

/// The numbers of a column, `NaN` or `i32::MIN` missing.
#[derive(Clone, Copy)]
pub(super) enum Cells<'a> {
    Doubles(&'a [f64]),
    Integers(&'a [i32]),
}

impl Cells<'_> {
    /// The number of row `r`, `NaN` where it is missing.
    #[inline]
    pub(super) fn at(self, r: usize) -> f64 {
        match self {
            Cells::Doubles(x) => x[r],
            Cells::Integers(x) => match x[r] {
                i32::MIN => f64::NAN,
                v => f64::from(v),
            },
        }
    }
}

/// The states of an aggregate of one part of the rows, for each group.
pub(super) struct Part {
    pub sums: Vec<PairSum>,
    pub extremes: Vec<f64>,
    pub count: Vec<u64>,
    /// A group with a missing cell, which `skip` leaves out.
    pub missing: Vec<bool>,
}

impl Part {
    fn new(op: Op, g: usize) -> Part {
        let least = op == Op::Min;
        Part {
            sums: if matches!(op, Op::Sum | Op::Mean) {
                vec![PairSum::default(); g]
            } else {
                Vec::new()
            },
            extremes: if matches!(op, Op::Min | Op::Max) {
                vec![
                    if least {
                        f64::INFINITY
                    } else {
                        f64::NEG_INFINITY
                    };
                    g
                ]
            } else {
                Vec::new()
            },
            count: vec![0; g],
            missing: vec![false; g],
        }
    }

    /// Adds the states of `other`, of later rows.
    fn merge(&mut self, other: Part, least: bool) {
        for (a, b) in self.sums.iter_mut().zip(&other.sums) {
            a.merge(b);
        }
        for (a, b) in self.extremes.iter_mut().zip(&other.extremes) {
            *a = extreme(least, *a, *b);
        }
        for (a, b) in self.count.iter_mut().zip(&other.count) {
            *a += b;
        }
        for (a, b) in self.missing.iter_mut().zip(&other.missing) {
            *a |= b;
        }
    }
}

/// The lanes of a sum without groups: independent sums whose additions
/// overlap, merged at the end.
const LANES: usize = 8;

/// The states of `sum`, `mean`, `min`, or `max` of the cells `x` in the groups
/// `ids` (one group without them), over `threads` threads.
pub(super) fn column_states(
    op: Op,
    x: Cells<'_>,
    ids: Option<&[u32]>,
    g: usize,
    rows: usize,
    skip: bool,
    threads: usize,
) -> Part {
    let least = op == Op::Min;
    let parts = over_rows(rows, threads, |lo, hi, cancel| {
        // Without groups, the lanes take the rows in turn.
        let width = if ids.is_none() && matches!(op, Op::Sum | Op::Mean) {
            LANES
        } else {
            g
        };
        let mut part = Part::new(op, width);
        let mut at = lo;
        while at < hi {
            if cancel.load(Ordering::Relaxed) {
                return part;
            }
            crate::progress::tick_block();
            let end = (at + STRIDE).min(hi);
            for r in at..end {
                let k = match ids {
                    Some(ids) => ids[r] as usize,
                    None if width == LANES => r % LANES,
                    None => 0,
                };
                let v = x.at(r);
                if v.is_nan() {
                    if !skip {
                        part.missing[k] = true;
                    }
                    continue;
                }
                part.count[k] += 1;
                match op {
                    Op::Sum | Op::Mean => part.sums[k].add(v),
                    _ => part.extremes[k] = extreme(least, part.extremes[k], v),
                }
            }
            at = end;
        }
        part
    });
    let mut parts = parts.into_iter();
    let mut out = parts.next().expect("one part at least");
    for p in parts {
        out.merge(p, least);
    }
    if ids.is_none() && out.count.len() == LANES && g == 1 {
        // The lanes, merged in order.
        let mut one = Part::new(op, 1);
        for k in 0..LANES {
            one.sums[0].merge(&out.sums[k]);
            one.count[0] += out.count[k];
            one.missing[0] |= out.missing[k];
        }
        out = one;
    }
    out
}
