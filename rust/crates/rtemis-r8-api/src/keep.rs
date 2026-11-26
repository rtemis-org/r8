//! Keep-alive tokens for R objects that Rust holds, such as the vector a
//! borrowed [`Buffer`](crate::Buffer) reads: one preserved, doubly linked
//! pairlist, in which a token is a cell inserted in O(1) and removed in O(1)
//! (the list's `CAR` is the object, `CDR` the next cell, and `TAG` the
//! previous one). Insertion and removal call R, so they happen on R's main
//! thread; a token dropped on another thread queues its cell, and the next
//! entry on the main thread removes it.

use std::sync::Mutex;
use std::sync::OnceLock;
use std::sync::atomic::{AtomicPtr, AtomicUsize, Ordering};
use std::thread::{self, ThreadId};

use crate::sys::{self, SEXP};

static HEAD: AtomicPtr<std::ffi::c_void> = AtomicPtr::new(std::ptr::null_mut());
static MAIN: OnceLock<ThreadId> = OnceLock::new();
static QUEUED: Mutex<Vec<usize>> = Mutex::new(Vec::new());
static LIVE: AtomicUsize = AtomicUsize::new(0);

/// Records R's main thread; `r8::exports!` calls it from the package's
/// initialization routine, which R runs on that thread.
pub fn init() {
    let _ = MAIN.set(thread::current().id());
}

/// Whether the running thread is R's main thread.
pub(crate) fn on_main_thread() -> bool {
    MAIN.get().is_some_and(|m| *m == thread::current().id())
}

/// The list's head cell, preserved on first use.
fn head() -> SEXP {
    let h = HEAD.load(Ordering::Acquire);
    if !h.is_null() {
        return h;
    }
    unsafe {
        let h = sys::Rf_cons(sys::R_NilValue, sys::R_NilValue);
        sys::R_PreserveObject(h);
        HEAD.store(h, Ordering::Release);
        h
    }
}

/// Allocates the list's head, so that [`Token::keep`] allocates one cell, which
/// protects the object it holds.
///
/// # Safety
///
/// Called on R's main thread.
pub(crate) unsafe fn prepare() -> crate::Result<()> {
    crate::api::protect(&|| head())
        .map(|_| ())
        .map_err(crate::api::Jumped::into_error)
}

/// Removes the cells that tokens dropped on other threads queued. Called on
/// the main thread at each entry of the crate's code from R.
pub(crate) fn drain() {
    let cells: Vec<usize> = match QUEUED.lock() {
        Ok(mut q) => std::mem::take(&mut *q),
        Err(_) => return,
    };
    for c in cells {
        unsafe { unlink(c as SEXP) };
    }
}

unsafe fn unlink(cell: SEXP) {
    unsafe {
        let prev = sys::TAG(cell);
        let next = sys::CDR(cell);
        sys::SETCDR(prev, next);
        if next != sys::R_NilValue {
            sys::SET_TAG(next, prev);
        }
    }
    LIVE.fetch_sub(1, Ordering::Relaxed);
}

/// A token that keeps an R object alive until it is dropped.
#[derive(Debug)]
pub struct Token(usize);

// The token's cell is only touched on the main thread: a drop elsewhere
// queues it.
unsafe impl Send for Token {}
unsafe impl Sync for Token {}

impl Token {
    /// Keeps `x` alive. The cell is allocated through `R8Api.protect`, so a
    /// failed allocation returns an error.
    ///
    /// # Safety
    ///
    /// Called on R's main thread with a valid `x`.
    pub(crate) unsafe fn keep(x: SEXP) -> crate::Result<Token> {
        let h = crate::api::protect(&|| head()).map_err(crate::api::Jumped::into_error)?;
        let cell = crate::api::protect(&|| unsafe { sys::Rf_cons(x, sys::R_NilValue) })
            .map_err(crate::api::Jumped::into_error)?;
        unsafe {
            let next = sys::CDR(h);
            sys::SETCDR(cell, next);
            sys::SET_TAG(cell, h);
            sys::SETCDR(h, cell);
            if next != sys::R_NilValue {
                sys::SET_TAG(next, cell);
            }
            LIVE.fetch_add(1, Ordering::Relaxed);
        }
        Ok(Token(cell as usize))
    }

    /// The object the token keeps alive.
    pub(crate) fn get(&self) -> SEXP {
        unsafe { sys::CAR(self.0 as SEXP) }
    }
}

impl Drop for Token {
    fn drop(&mut self) {
        if on_main_thread() {
            unsafe { unlink(self.0 as SEXP) };
        } else if let Ok(mut q) = QUEUED.lock() {
            q.push(self.0);
        }
    }
}

/// The number of R objects that tokens keep alive.
pub fn kept() -> usize {
    LIVE.load(Ordering::Relaxed)
}
