// R8.h is included so that every build compiles the C interface against
// R's own headers.
#include <R8.h>

// R calls R_init_R8 when it loads the package. Forwarding the call to
// the Rust registration routine references the static library, so the linker
// keeps it.

void R_init_R8_extendr(void *dll);
void register_extendr_panic_hook(void);

void R_init_R8(void *dll) {
    register_extendr_panic_hook();
    R_init_R8_extendr(dll);
}
