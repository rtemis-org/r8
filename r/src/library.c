// library.c
// ::R8::
// 2026- EDG rtemis.org

// Without R's headers, whose TRUE and FALSE are those of windows.h.

// dladdr() and RTLD_NODELETE of glibc.
#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif

#include <stddef.h>
#if defined(_WIN32)
#include <windows.h>
#elif !defined(__EMSCRIPTEN__)
#include <dlfcn.h>
#endif

// Keeps the shared library that holds `address` loaded for the session. A value
// of a Rust-stored type, a view, and R's finalizers hold addresses of the code
// and the memory of a package, which dyn.unload() would leave without them.
// Emscripten unloads no library.
void r8_pin_library(const void *address) {
#if defined(_WIN32)
  HMODULE module;
  GetModuleHandleExA(GET_MODULE_HANDLE_EX_FLAG_FROM_ADDRESS | GET_MODULE_HANDLE_EX_FLAG_PIN,
                     (LPCSTR) address, &module);
#elif !defined(__EMSCRIPTEN__)
  Dl_info info;
  if (dladdr(address, &info) != 0 && info.dli_fname != NULL) {
    dlopen(info.dli_fname, RTLD_LAZY | RTLD_NOLOAD | RTLD_NODELETE);
  }
#else
  (void) address;
#endif
}
