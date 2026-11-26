// R calls R_init_R8_io when it loads the package. Forwarding the call to the Rust
// registration routine references the static library, so the linker keeps it.

#include <Rinternals.h>
#include <R_ext/Utils.h>
#include <signal.h>

void R_init_R8_io_extendr(void *dll);
void register_extendr_panic_hook(void);
void r8_io_set_interrupted(void *check);

static SEXP check_interrupt(void *unused) {
    (void) unused;
    R_CheckUserInterrupt();
    return R_NilValue;
}

static SEXP limit_reached(SEXP condition, void *reached) {
    (void) condition;
    *(int *) reached = 1;
    return R_NilValue;
}

static void check(void *reached) {
    R_tryCatchError(check_interrupt, NULL, limit_reached, reached);
}

// Whether the user interrupted R (2), a time limit of R is reached (1), or neither
// (0); R's jump ends here. The interrupt is raised again, which R takes once the
// call has returned.
static int interrupted(void) {
    int reached = 0;
    if (R_ToplevelExec(check, &reached) == FALSE) {
#ifndef _WIN32
        raise(SIGINT);
#endif
        return 2;
    }
    return reached;
}

void R_init_R8_io(void *dll) {
    register_extendr_panic_hook();
    r8_io_set_interrupted((void *) interrupted);
    R_init_R8_io_extendr(dll);
}
