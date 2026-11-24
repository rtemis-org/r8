//! The user's interrupt and R's time limits during a long walk of a value.
//!
//! R raises both by a jump. [`interruptible`] runs a walk with the hook of
//! `rtemis_r8::progress`, which asks R in a C frame that catches the jump
//! (`r8_check_interrupt()` in `view.c`) and ends the walk by unwinding, so that no
//! jump crosses a Rust frame.

use std::panic::{AssertUnwindSafe, catch_unwind, resume_unwind};
use std::sync::atomic::{AtomicPtr, Ordering};

use extendr_api::{Error, Result, SEXP};
use extendr_ffi::{SEXPTYPE, VECTOR_ELT};

use crate::rview::{sexptype, xlen};

static CHECK: AtomicPtr<()> = AtomicPtr::new(std::ptr::null_mut());
static CONTINUE: AtomicPtr<()> = AtomicPtr::new(std::ptr::null_mut());

/// Receives `r8_check_interrupt()` and `r8_continue_unwind()` from `R_init_R8()`.
///
/// # Safety
///
/// The arguments are these functions.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn r8_set_interrupt_functions(check: *mut (), resume: *mut ()) {
    CHECK.store(check, Ordering::Release);
    CONTINUE.store(resume, Ordering::Release);
}

/// The payload of the unwinding: the user's interrupt, or a time limit.
struct Ended {
    by_user: bool,
}

fn hook() {
    let f = CHECK.load(Ordering::Acquire);
    if f.is_null() {
        return;
    }
    let f: unsafe extern "C" fn() -> i32 = unsafe { std::mem::transmute(f) };
    match unsafe { f() } {
        0 => {}
        1 => resume_unwind(Box::new(Ended { by_user: false })),
        _ => resume_unwind(Box::new(Ended { by_user: true })),
    }
}

/// Ends a long read of a vector's elements when the user or a time limit does.
pub(crate) fn tick_block() {
    rtemis_r8::progress::tick_block();
}

/// The elements of a vector above which a walk of it asks for an interrupt.
const LONG: usize = 1 << 16;

/// Whether a walk of `x`, a value, the list of its fields, or a field, may take
/// long: it holds a long vector, or a list, whose content is not looked at.
pub(crate) fn long(x: SEXP) -> bool {
    if sexptype(x) != SEXPTYPE::VECSXP {
        return xlen(x) > LONG;
    }
    let fields = crate::value::fields(x);
    if xlen(fields) > LONG || (fields == x && unsafe { extendr_ffi::ALTREP(x) } != 0) {
        return true;
    }
    (0..xlen(fields)).any(|i| {
        let f = unsafe { VECTOR_ELT(fields, i as isize) };
        sexptype(f) == SEXPTYPE::VECSXP || xlen(f) > LONG
    })
}

/// The outcome of `f`, a walk of `x`, which the user's interrupt and R's time
/// limits end when it may take long. A jump of R out of the walk, such as that of
/// another package's vector, ends at R's protection, where the hook is taken back,
/// and goes on from here.
pub(crate) fn interruptible<T>(x: SEXP, f: impl FnOnce() -> Result<T>) -> Result<T> {
    interruptible_if(long(x), f)
}

/// [`interruptible`] of a walk that may take long when `long` holds.
pub(crate) fn interruptible_if<T>(long: bool, f: impl FnOnce() -> Result<T>) -> Result<T> {
    if !long {
        return f();
    }
    let before = rtemis_r8::progress::set_hook(Some(hook));
    let mut f = Some(f);
    let out = catch_unwind(AssertUnwindSafe(|| {
        crate::rust_stored::guarded(|| f.take().map(|f| f()))
    }));
    rtemis_r8::progress::set_hook(before);
    match out {
        Ok(Some(Some(v))) => v,
        // R jumped.
        Ok(_) => {
            let f: unsafe extern "C" fn() = unsafe {
                std::mem::transmute::<*mut (), unsafe extern "C" fn()>(
                    CONTINUE.load(Ordering::Acquire),
                )
            };
            unsafe { f() };
            Err(Error::Other("R8 stopped".to_string()))
        }
        Err(p) => match p.downcast::<Ended>() {
            Ok(e) if e.by_user => Err(Error::Other("R8 stopped: interrupted".to_string())),
            Ok(_) => Err(Error::Other(
                "R8 stopped: reached a time limit of setTimeLimit() or setSessionTimeLimit()"
                    .to_string(),
            )),
            Err(p) => resume_unwind(p),
        },
    }
}
