// R8.h is included so that every build compiles the C interface against
// R's own headers.
#include <R8.h>

// R calls R_init_R8 when it loads the package. Forwarding the call to
// the Rust registration routine references the static library, so the linker
// keeps it.

void R_init_R8_extendr(void *dll);
void register_extendr_panic_hook(void);

// The attribute reads of attrib.c, passed to the Rust code as pointers.
R_xlen_t r8_attrib_count(SEXP x);
SEXP r8_attrib_raw(SEXP x, SEXP name);
void r8_set_attrib_functions(void *count, void *raw);

void R_init_R8(void *dll) {
    register_extendr_panic_hook();
    r8_set_attrib_functions((void *) r8_attrib_count, (void *) r8_attrib_raw);
    R_init_R8_extendr(dll);
}
