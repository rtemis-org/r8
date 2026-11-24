// R8.h is included so that every build compiles the C interface against
// R's own headers.
#define R8_INTERNAL_ABI
#include <R8.h>
#include <R_ext/Rdynload.h>

// R calls R_init_R8 when it loads the package. Forwarding the call to
// the Rust registration routine references the static library, so the linker
// keeps it.

void R_init_R8_extendr(void *dll);
void register_extendr_panic_hook(void);

// The attribute reads of attrib.c, passed to the Rust code as pointers.
R_xlen_t r8_attrib_count(SEXP x);
SEXP r8_attrib_raw(SEXP x, SEXP name);
SEXP r8_payload(SEXP x);
void r8_set_attrib_functions(void *count, void *raw, void *payload);

// Values (value.c), Rust-stored values (rust_stored.c), and the views and API table of R8Api
// (view.c).
void r8_init_values(DllInfo *dll);
SEXP r8_identity(SEXP type, SEXP variant, SEXP names, SEXP cls, SEXP declaration, SEXP record,
                 int registered);
SEXP r8_held_identity(SEXP type, SEXP variant, SEXP declaration);
SEXP r8_value_id(SEXP x);
SEXP r8_value_fields(SEXP x);
SEXP r8_value_new(SEXP fields, SEXP id);
void r8_set_value_functions(void *identity, void *held, void *id, void *fields, void *value);
void r8_init_rust_stored(DllInfo *dll);
void r8_init_views(DllInfo *dll);
void r8_register_api(void);
SEXP r8_rust_stored_wrap(const R8Codec *vt, void *state, SEXP id, SEXP from,
                    R_xlen_t skip);
int r8_rust_stored_build(SEXP x, R8Error *err);
const void *r8_rust_stored_state(SEXP x, const R8Codec **vt);
SEXP r8_rust_stored_parts(SEXP x);
SEXP r8_rust_stored_memory(void);
R8Status r8_protect(SEXP (*fn)(void *data), void *data, SEXP *value, R8Error *err);
SEXP r8_native_symbols(void);
void r8_set_rust_stored_functions(void *wrap, void *build, void *state, void *parts,
                                  void *memory, void *protect, void *symbols);

void R_init_R8(DllInfo *dll) {
    register_extendr_panic_hook();
    r8_set_attrib_functions((void *) r8_attrib_count, (void *) r8_attrib_raw, (void *) r8_payload);
    r8_set_rust_stored_functions((void *) r8_rust_stored_wrap, (void *) r8_rust_stored_build,
                                 (void *) r8_rust_stored_state, (void *) r8_rust_stored_parts,
                                 (void *) r8_rust_stored_memory, (void *) r8_protect,
                                 (void *) r8_native_symbols);
    r8_set_value_functions((void *) r8_identity, (void *) r8_held_identity,
                           (void *) r8_value_id, (void *) r8_value_fields,
                           (void *) r8_value_new);
    r8_init_values(dll);
    r8_init_rust_stored(dll);
    r8_init_views(dll);
    r8_register_api();
    R_init_R8_extendr(dll);
}
