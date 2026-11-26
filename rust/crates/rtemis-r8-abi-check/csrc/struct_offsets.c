/* The size of every struct and the offset of every field of R8.h, in the
 * order of `rtemis_r8::abi::struct_offsets()`. */

#define R8_INTERNAL_ABI
#include <R8.h>

#define SIZE(t) sizeof(t)
#define OFFSET(t, f) offsetof(t, f)

static const size_t struct_offsets[] = {
  SIZE(R8Error), OFFSET(R8Error, message),
  SIZE(R8CallInputs), OFFSET(R8CallInputs, flags), OFFSET(R8CallInputs, seed),
  SIZE(R8Codec), OFFSET(R8Codec, type_name), OFFSET(R8Codec, module),
  OFFSET(R8Codec, from_fields),
  OFFSET(R8Codec, get_field), OFFSET(R8Codec, with_field),
  OFFSET(R8Codec, drop), OFFSET(R8Codec, owned_bytes),
  OFFSET(R8Codec, variant), OFFSET(R8Codec, from_variant),
  OFFSET(R8Codec, from_reader), OFFSET(R8Codec, upcast),
  SIZE(R8Reader), OFFSET(R8Reader, context), OFFSET(R8Reader, kind), OFFSET(R8Reader, member),
  OFFSET(R8Reader, item), OFFSET(R8Reader, length), OFFSET(R8Reader, logical),
  OFFSET(R8Reader, number), OFFSET(R8Reader, string), OFFSET(R8Reader, doubles),
  OFFSET(R8Reader, integers), OFFSET(R8Reader, logicals), OFFSET(R8Reader, value),
  SIZE(R8Position), OFFSET(R8Position, name), OFFSET(R8Position, package),
  OFFSET(R8Position, system),
  SIZE(R8ImplEntry), OFFSET(R8ImplEntry, positions), OFFSET(R8ImplEntry, n_positions),
  OFFSET(R8ImplEntry, method), OFFSET(R8ImplEntry, call),
  OFFSET(R8ImplEntry, signature), OFFSET(R8ImplEntry, signature_len),
  SIZE(R8Exports), OFFSET(R8Exports, size), OFFSET(R8Exports, package),
  OFFSET(R8Exports, module), OFFSET(R8Exports, ir_record), OFFSET(R8Exports, ir_record_len), OFFSET(R8Exports, types),
  OFFSET(R8Exports, n_types), OFFSET(R8Exports, impls), OFFSET(R8Exports, n_impls),
  OFFSET(R8Exports, impl_size),
  SIZE(R8Api), OFFSET(R8Api, size), OFFSET(R8Api, new_value), OFFSET(R8Api, value_state),
  OFFSET(R8Api, upcast_state),
  OFFSET(R8Api, view_doubles), OFFSET(R8Api, view_integers), OFFSET(R8Api, view_logicals),
  OFFSET(R8Api, view_bytes), OFFSET(R8Api, protect),
  OFFSET(R8Api, value_of_fields), OFFSET(R8Api, with_fields), OFFSET(R8Api, pin),
  OFFSET(R8Api, value_type),
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

/* The kinds of a position, R8_POSITION_TYPE, R8_POSITION_CLASS, and
 * R8_POSITION_ANY, as one number in base 16. */
unsigned r8_abi_positions(void) {
  return R8_POSITION_TYPE | R8_POSITION_CLASS << 4 | R8_POSITION_ANY << 8;
}

/* The relations of a value, R8_VALUE_TYPE, R8_VALUE_SUBTYPE, and
 * R8_VALUE_IMPLEMENTOR, as one number in base 16. */
unsigned r8_abi_values(void) {
  return R8_VALUE_TYPE | R8_VALUE_SUBTYPE << 4 | R8_VALUE_IMPLEMENTOR << 8;
}

/* The status codes R8_OK, R8_ERROR, and R8_UNWIND, as one number in base 16. */
unsigned r8_abi_statuses(void) { return R8_OK | R8_ERROR << 4 | R8_UNWIND << 8; }

/* The minimum size of each table, R8_*_MIN_SIZE, in the order of
 * `rtemis_r8_abi_check::c_min_sizes()`. */
void r8_abi_min_sizes(size_t *out) {
  out[0] = R8_CALL_INPUTS_MIN_SIZE;
  out[1] = R8_READER_MIN_SIZE;
  out[2] = R8_CODEC_MIN_SIZE;
  out[3] = R8_IMPL_ENTRY_MIN_SIZE;
  out[4] = R8_EXPORTS_MIN_SIZE;
  out[5] = R8_API_MIN_SIZE;
}
