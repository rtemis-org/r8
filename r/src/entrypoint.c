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
int r8_check_interrupt(void);
void r8_continue_unwind(void);
void r8_set_interrupt_functions(void *check, void *resume);
void r8_drop_unwind(void);
void r8_compiled_enter(void);
int r8_compiled_held(void);
int r8_jump_held(void);
void r8_continue_held(void);
const char *r8_unwound_text(void);
R8Status r8_package_protect(SEXP (*fn)(void *data), void *data, SEXP *value, R8Error *err);
void r8_set_compiled_functions(void *drop, void *enter, void *held, void *is_held,
                                void *resume, void *unwound, void *protect);
void r8_pin_library(const void *address);
void r8_init_values(DllInfo *dll);
SEXP r8_identity(SEXP type, SEXP variant, SEXP names, SEXP cls, SEXP declaration, SEXP record,
                 int registered);
SEXP r8_held_identity(SEXP type, SEXP variant, SEXP declaration);
SEXP r8_value_id(SEXP x);
SEXP r8_value_fields(SEXP x);
SEXP r8_value_new(SEXP fields, SEXP id);
int r8_fields_fit(SEXP fields);
void r8_set_value_functions(void *identity, void *held, void *id, void *fields, void *value,
                            void *fit, void *viewed, void *bare, void *table);
SEXP r8_table_identity(SEXP base, SEXP names, SEXP facts);
void r8_init_fields(DllInfo *dll);
SEXP r8_field_viewed(SEXP x);
SEXP r8_field_bare(SEXP x);
void r8_init_rust_stored(DllInfo *dll);
void r8_init_views(DllInfo *dll);
void r8_register_api(void);
SEXP r8_rust_stored_wrap(const R8Codec *vt, void *state, SEXP id, SEXP from,
                    R_xlen_t skip);
SEXP r8_rust_stored_fields_read(SEXP x);
void r8_rust_stored_give(SEXP x, const R8Codec *vt, void *state);
void r8_rust_stored_take(SEXP x);
const void *r8_rust_stored_state(SEXP x, const R8Codec **vt);
SEXP r8_rust_stored_parts(SEXP x);
SEXP r8_rust_stored_memory(void);
R8Status r8_protect(SEXP (*fn)(void *data), void *data, SEXP *value, R8Error *err);
SEXP r8_native_symbols(void);
void r8_set_rust_stored_functions(void *wrap, void *fields_read, void *give, void *state,
                                  void *parts, void *memory, void *protect, void *symbols,
                                  void *take);

void R_init_R8(DllInfo *dll) {
    register_extendr_panic_hook();
    r8_pin_library((const void *) R_init_R8);
    r8_set_attrib_functions((void *) r8_attrib_count, (void *) r8_attrib_raw, (void *) r8_payload);
    r8_set_rust_stored_functions((void *) r8_rust_stored_wrap,
                                 (void *) r8_rust_stored_fields_read, (void *) r8_rust_stored_give,
                                 (void *) r8_rust_stored_state, (void *) r8_rust_stored_parts,
                                 (void *) r8_rust_stored_memory, (void *) r8_protect,
                                 (void *) r8_native_symbols, (void *) r8_rust_stored_take);
    r8_set_value_functions((void *) r8_identity, (void *) r8_held_identity,
                           (void *) r8_value_id, (void *) r8_value_fields,
                           (void *) r8_value_new, (void *) r8_fields_fit,
                           (void *) r8_field_viewed, (void *) r8_field_bare,
                           (void *) r8_table_identity);
    r8_set_interrupt_functions((void *) r8_check_interrupt, (void *) r8_continue_unwind);
    r8_set_compiled_functions((void *) r8_drop_unwind, (void *) r8_compiled_enter,
                              (void *) r8_compiled_held, (void *) r8_jump_held,
                              (void *) r8_continue_held, (void *) r8_unwound_text,
                              (void *) r8_package_protect);
    r8_init_values(dll);
    r8_init_fields(dll);
    r8_init_rust_stored(dll);
    r8_init_views(dll);
    r8_register_api();
    R_init_R8_extendr(dll);
}
