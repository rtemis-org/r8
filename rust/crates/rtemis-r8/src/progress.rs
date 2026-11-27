//! The hook through which a host ends a long walk of a value, such as on the
//! user's interrupt: the walks call [`tick`] per nested value and per block of
//! elements, and a hook that a host sets is called once per [`TICKS`]. A hook ends
//! the walk by unwinding. A count of steps that take any time each, such as the
//! calls of a typed function, reports at the time-gated points of a [`Stride`],
//! where the hook is called too.

use std::cell::Cell;

/// The ticks between two calls of the hook.
pub const TICKS: u32 = 1 << 14;

thread_local! {
    static HOOK: Cell<Option<fn()>> = const { Cell::new(None) };
    static COUNT: Cell<u32> = const { Cell::new(0) };
}

/// Sets the hook of this thread, and returns the earlier one.
pub fn set_hook(hook: Option<fn()>) -> Option<fn()> {
    HOOK.replace(hook)
}

/// Counts one step of a walk.
#[inline]
pub fn tick() {
    let n = COUNT.get().wrapping_add(1);
    COUNT.set(n);
    if n.is_multiple_of(TICKS) {
        call();
    }
}

/// Counts a block of elements, which is as long as many steps.
#[inline]
pub fn tick_block() {
    let n = COUNT.get().wrapping_add(64);
    COUNT.set(n);
    if n % TICKS < 64 {
        call();
    }
}

/// Calls the hook, after a step that is as long as many.
#[cold]
pub fn call() {
    if let Some(f) = HOOK.get() {
        f();
    }
}

/// The report points of a count of steps, such as the calls of a typed
/// function in a map. The clock is read only where the count reaches the next
/// point, which a stride of steps sets: a point within 0.05 s of the last
/// report doubles the stride and reports nothing; a later one reports, and
/// halves the stride when it comes after 0.2 s. Reports thus come 0.05 to about
/// 0.2 s apart, and a count that ends within 0.05 s reports nothing. At each
/// report the hook is called, so that a host ends the work on the user's
/// interrupt whatever a step takes.
#[derive(Debug)]
pub struct Stride {
    next_at: u64,
    stride: u64,
    last: std::time::Instant,
}

impl Default for Stride {
    fn default() -> Self {
        Self::new()
    }
}

impl Stride {
    /// A count whose first point is its first step, timed from its creation.
    pub fn new() -> Stride {
        Stride {
            next_at: 1,
            stride: 1,
            last: std::time::Instant::now(),
        }
    }

    /// The steps from `done` to the next point, at least 1.
    #[inline]
    pub fn remaining(&self, done: u64) -> u64 {
        self.next_at.saturating_sub(done).max(1)
    }

    /// Whether `done` steps make a report: they reach the next point, at least
    /// 0.05 s after the last report. The hook is called at a report.
    #[inline]
    pub fn reached(&mut self, done: u64) -> bool {
        if done < self.next_at {
            return false;
        }
        self.at_point(done)
    }

    #[cold]
    fn at_point(&mut self, done: u64) -> bool {
        let now = std::time::Instant::now();
        let gap = now.duration_since(self.last).as_secs_f64();
        if gap < 0.05 {
            self.stride = self.stride.saturating_mul(2);
            self.next_at = done.saturating_add(self.stride);
            return false;
        }
        if gap > 0.2 {
            self.stride = (self.stride / 2).max(1);
        }
        self.next_at = done.saturating_add(self.stride);
        self.last = now;
        call();
        true
    }
}

#[cfg(test)]
mod tests {
    use super::Stride;

    #[test]
    fn a_stride_doubles_while_points_come_quickly() {
        let mut s = Stride::new();
        let mut done = 0;
        let mut points = 0;
        for _ in 0..1_000 {
            done += 1;
            if done >= s.next_at {
                points += 1;
            }
            assert!(!s.reached(done), "a report within 0.05 s");
        }
        // Points 1, 2, 4, ... steps apart: about log2 of the steps.
        assert!(points <= 11, "{points}");
        assert!(s.remaining(done) >= 1);
    }

    #[test]
    fn a_stride_halves_when_reports_come_slowly() {
        let mut s = Stride::new();
        s.stride = 8;
        s.next_at = 8;
        s.last = std::time::Instant::now() - std::time::Duration::from_millis(300);
        assert!(s.reached(8));
        assert_eq!(s.stride, 4);
        assert_eq!(s.remaining(8), 4);
    }
}
