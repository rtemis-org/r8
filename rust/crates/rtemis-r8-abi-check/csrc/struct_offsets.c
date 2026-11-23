/* The size of every struct and the offset of every field of R8.h, in the
 * order of `rtemis_r8::abi::struct_offsets()`. */

#include <R8.h>

#define SIZE(t) sizeof(t)
#define OFFSET(t, f) offsetof(t, f)

static const size_t struct_offsets[] = {
  SIZE(R8Error), OFFSET(R8Error, message),
  SIZE(R8Writer), OFFSET(R8Writer, write),
  SIZE(R8CallInputs), OFFSET(R8CallInputs, flags), OFFSET(R8CallInputs, seed),
  SIZE(R8Codec), OFFSET(R8Codec, type_name), OFFSET(R8Codec, module),
  OFFSET(R8Codec, from_fields), OFFSET(R8Codec, encode),
  OFFSET(R8Codec, write_state), OFFSET(R8Codec, read_state),
  OFFSET(R8Codec, get_field), OFFSET(R8Codec, with_field),
  OFFSET(R8Codec, drop), OFFSET(R8Codec, owned_bytes),
  OFFSET(R8Codec, variant), OFFSET(R8Codec, from_variant),
  OFFSET(R8Codec, from_reader), OFFSET(R8Codec, declaration_hash),
  OFFSET(R8Codec, upcast),
  SIZE(R8Reader), OFFSET(R8Reader, context), OFFSET(R8Reader, kind), OFFSET(R8Reader, member),
  OFFSET(R8Reader, item), OFFSET(R8Reader, length), OFFSET(R8Reader, logical),
  OFFSET(R8Reader, number), OFFSET(R8Reader, string), OFFSET(R8Reader, doubles),
  OFFSET(R8Reader, integers), OFFSET(R8Reader, logicals),
  SIZE(R8ImplEntry), OFFSET(R8ImplEntry, type_name), OFFSET(R8ImplEntry, method),
  OFFSET(R8ImplEntry, call), OFFSET(R8ImplEntry, n_params),
  SIZE(R8Exports), OFFSET(R8Exports, size), OFFSET(R8Exports, package),
  OFFSET(R8Exports, ir_record), OFFSET(R8Exports, ir_record_len), OFFSET(R8Exports, types),
  OFFSET(R8Exports, n_types), OFFSET(R8Exports, impls), OFFSET(R8Exports, n_impls),
  SIZE(R8Api), OFFSET(R8Api, size), OFFSET(R8Api, new_value), OFFSET(R8Api, value_state),
  OFFSET(R8Api, upcast_state),
  OFFSET(R8Api, view_doubles), OFFSET(R8Api, view_integers), OFFSET(R8Api, view_logicals),
  OFFSET(R8Api, view_bytes), OFFSET(R8Api, protect),
};

/* Copies the sizes and offsets into `out`, up to `cap` entries, and returns the number
 * of entries. */
size_t r8_abi_struct_offsets(size_t *out, size_t cap) {
  size_t n = sizeof(struct_offsets) / sizeof(struct_offsets[0]);
  for (size_t i = 0; i < n && i < cap; i++) {
    out[i] = struct_offsets[i];
  }
  return n;
}

/* The ABI version, the context flag, and the statuses, so the test can
 * compare constants. */
unsigned r8_abi_version(void) { return R8_ABI_VERSION; }
unsigned r8_abi_has_seed(void) { return R8_CALL_HAS_SEED; }

/* The status codes R8_OK, R8_ERROR, and R8_UNWIND, as one number in base 16. */
unsigned r8_abi_statuses(void) { return R8_OK | R8_ERROR << 4 | R8_UNWIND << 8; }
