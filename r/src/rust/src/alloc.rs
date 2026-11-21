//! Counts of the memory the binding allocates in Rust, for the memory rows
//! of the benchmark (`bench/rows/types.R`): with the cargo feature
//! `alloc-stats`, a global allocator wraps the system allocator and keeps
//! the live bytes and blocks; without it, `r8_alloc_stats()` returns `NULL`.
//! R's gc() does not see these allocations, the registry of declared types
//! among them.

use extendr_api::prelude::*;

#[cfg(feature = "alloc-stats")]
mod counting {
    use std::alloc::{GlobalAlloc, Layout, System};
    use std::sync::atomic::{AtomicI64, Ordering};

    pub(crate) static BYTES: AtomicI64 = AtomicI64::new(0);
    pub(crate) static BLOCKS: AtomicI64 = AtomicI64::new(0);

    /// The system allocator, with counts of the bytes and blocks it holds.
    pub(crate) struct Counting;

    // SAFETY: each method calls the system allocator with the arguments it
    // receives and returns its result; the counts are side effects.
    unsafe impl GlobalAlloc for Counting {
        unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
            let p = unsafe { System.alloc(layout) };
            if !p.is_null() {
                BYTES.fetch_add(layout.size() as i64, Ordering::Relaxed);
                BLOCKS.fetch_add(1, Ordering::Relaxed);
            }
            p
        }

        unsafe fn alloc_zeroed(&self, layout: Layout) -> *mut u8 {
            let p = unsafe { System.alloc_zeroed(layout) };
            if !p.is_null() {
                BYTES.fetch_add(layout.size() as i64, Ordering::Relaxed);
                BLOCKS.fetch_add(1, Ordering::Relaxed);
            }
            p
        }

        unsafe fn dealloc(&self, ptr: *mut u8, layout: Layout) {
            unsafe { System.dealloc(ptr, layout) };
            BYTES.fetch_sub(layout.size() as i64, Ordering::Relaxed);
            BLOCKS.fetch_sub(1, Ordering::Relaxed);
        }

        unsafe fn realloc(&self, ptr: *mut u8, layout: Layout, new_size: usize) -> *mut u8 {
            let p = unsafe { System.realloc(ptr, layout, new_size) };
            if !p.is_null() {
                BYTES.fetch_add(new_size as i64 - layout.size() as i64, Ordering::Relaxed);
            }
            p
        }
    }

    #[global_allocator]
    static ALLOCATOR: Counting = Counting;
}

/// The live bytes and blocks the binding holds in Rust, from the counting
/// allocator of the feature `alloc-stats`.
///
/// @return Double: `c(bytes, blocks)`, or `NULL` in a build without the
///   feature.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_alloc_stats() -> Robj {
    #[cfg(feature = "alloc-stats")]
    let stats: Robj = {
        use std::sync::atomic::Ordering;
        let bytes = counting::BYTES.load(Ordering::Relaxed) as f64;
        let blocks = counting::BLOCKS.load(Ordering::Relaxed) as f64;
        r!([bytes, blocks])
    };
    #[cfg(not(feature = "alloc-stats"))]
    let stats: Robj = ().into();
    stats
}

extendr_module! {
    mod alloc;
    fn r8_alloc_stats;
}
