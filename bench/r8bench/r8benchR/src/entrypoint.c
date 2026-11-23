// entrypoint.c
// ::r8benchR::
// 2026- EDG rtemis.org

// R calls R_init_r8benchR when it loads the package; r8_init_r8benchR(), which
// r8::exports! emits, registers the package's exports with R8.

#include <stddef.h>
#include <R_ext/Rdynload.h>

void r8_init_r8benchR(void *dll);

void R_init_r8benchR(DllInfo *dll) {
  R_registerRoutines(dll, NULL, NULL, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  r8_init_r8benchR(dll);
}
