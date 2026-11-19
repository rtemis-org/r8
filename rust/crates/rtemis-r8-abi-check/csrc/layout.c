/* The size of every struct and the offset of every field of R8.h, in the
 * order of `rtemis_r8::abi::layout()`. */

#include <R8.h>

#define SIZE(t) sizeof(t)
#define OFFSET(t, f) offsetof(t, f)

static const size_t layout[] = {
  SIZE(R8Error), OFFSET(R8Error, message),
  SIZE(R8Writer), OFFSET(R8Writer, write),
  SIZE(R8CallContext), OFFSET(R8CallContext, flags), OFFSET(R8CallContext, seed),
  SIZE(R8TypeVTable), OFFSET(R8TypeVTable, type_name), OFFSET(R8TypeVTable, from_fields),
  OFFSET(R8TypeVTable, decode), OFFSET(R8TypeVTable, encode),
  OFFSET(R8TypeVTable, write_state), OFFSET(R8TypeVTable, read_state),
  OFFSET(R8TypeVTable, get_field), OFFSET(R8TypeVTable, with_field),
  OFFSET(R8TypeVTable, drop), OFFSET(R8TypeVTable, bytes),
  SIZE(R8ImplEntry), OFFSET(R8ImplEntry, type_name), OFFSET(R8ImplEntry, method),
  OFFSET(R8ImplEntry, call),
  SIZE(R8Exports), OFFSET(R8Exports, size), OFFSET(R8Exports, package),
  OFFSET(R8Exports, ir_json), OFFSET(R8Exports, ir_json_len), OFFSET(R8Exports, types),
  OFFSET(R8Exports, n_types), OFFSET(R8Exports, impls), OFFSET(R8Exports, n_impls),
  SIZE(R8Api), OFFSET(R8Api, size), OFFSET(R8Api, new_value), OFFSET(R8Api, value_state),
  OFFSET(R8Api, view_doubles), OFFSET(R8Api, view_integers), OFFSET(R8Api, view_logicals),
  OFFSET(R8Api, view_bytes),
};

/* Copies the layout into `out`, up to `cap` entries, and returns the number
 * of entries. */
size_t r8_abi_layout(size_t *out, size_t cap) {
  size_t n = sizeof(layout) / sizeof(layout[0]);
  for (size_t i = 0; i < n && i < cap; i++) {
    out[i] = layout[i];
  }
  return n;
}

/* The ABI version and the context flag, so the test can compare constants. */
unsigned r8_abi_version(void) { return R8_ABI_VERSION; }
unsigned r8_abi_has_seed(void) { return R8_CONTEXT_HAS_SEED; }
