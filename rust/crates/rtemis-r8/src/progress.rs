//! The hook through which a host ends a long walk of a value, such as on the
//! user's interrupt: the walks call [`tick`] per nested value and per block of
//! elements, and a hook that a host sets is called once per [`TICKS`]. A hook ends
//! the walk by unwinding.

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
