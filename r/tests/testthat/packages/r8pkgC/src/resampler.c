// resampler.c
// ::r8pkgC::
// 2026- EDG rtemis.org

// The codec of pkgc::Resampler, a Rust-stored enum implemented in C, with
// the variants KFold { k } and Loocv: its state holds the variant and `k`.
// Also an exports table whose struct codec sets the enum members, which
// registration refuses.

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <R8.h>

typedef struct Resampler {
  int kfold;  // 1 for KFold, 0 for Loocv
  int k;
} Resampler;

static void set_error(R8Error *err, const char *code, const char *message) {
  snprintf(err->code, sizeof(err->code), "%s", code);
  snprintf(err->message, sizeof(err->message), "%s", message);
}

static Resampler *resampler_new(int kfold, int k) {
  Resampler *r = malloc(sizeof(Resampler));
  if (r != NULL) {
    r->kfold = kfold;
    r->k = k;
  }
  return r;
}

static const char *resampler_variant(const void *state) {
  return ((const Resampler *) state)->kfold ? "KFold" : "Loocv";
}

// Fields in the variant's declaration order: KFold has k (an integer), and
// Loocv has none.
static R8Status resampler_from_variant(const char *variant, const SEXP *fields, size_t n,
                                       void **state, R8Error *err) {
  Resampler *r;
  if (strcmp(variant, "KFold") == 0 && n == 1) {
    r = resampler_new(1, INTEGER_ELT(fields[0], 0));
  } else if (strcmp(variant, "Loocv") == 0 && n == 0) {
    r = resampler_new(0, 0);
  } else {
    set_error(err, "unknown_variant", "a Resampler is KFold { k } or Loocv");
    return R8_ERROR;
  }
  if (r == NULL) {
    set_error(err, "alloc", "out of memory");
    return R8_ERROR;
  }
  *state = r;
  return R8_OK;
}

static R8Status resampler_get_field(const void *state, SEXP owner, const char *field,
                                    SEXP *value, R8Error *err) {
  (void) owner;
  const Resampler *r = state;
  if (r->kfold && strcmp(field, "k") == 0) {
    *value = Rf_ScalarInteger(r->k);
    return R8_OK;
  }
  set_error(err, "unknown_field", "KFold has the field k, and Loocv none");
  return R8_ERROR;
}

static R8Status resampler_with_field(const void *state, const char *field, SEXP value,
                                     void **new_state, R8Error *err) {
  const Resampler *r = state;
  if (!r->kfold || strcmp(field, "k") != 0) {
    set_error(err, "unknown_field", "KFold has the field k, and Loocv none");
    return R8_ERROR;
  }
  Resampler *t = resampler_new(1, INTEGER_ELT(value, 0));
  if (t == NULL) {
    set_error(err, "alloc", "out of memory");
    return R8_ERROR;
  }
  *new_state = t;
  return R8_OK;
}

// State bytes: the variant (int32) and k (int32), in the host's byte order.
static R8Status resampler_write_state(const void *state, R8Writer *out, R8Error *err) {
  const Resampler *r = state;
  if (out->write(out->context, (const uint8_t *) &r->kfold, sizeof(int)) != R8_OK ||
      out->write(out->context, (const uint8_t *) &r->k, sizeof(int)) != R8_OK) {
    set_error(err, "write_failed", "the writer refused the state");
    return R8_ERROR;
  }
  return R8_OK;
}

static R8Status resampler_read_state(const uint8_t *bytes, size_t size, void **state,
                                     R8Error *err) {
  int kfold, k;
  if (size != 2 * sizeof(int)) {
    set_error(err, "bad_state", "the bytes are not a Resampler state");
    return R8_ERROR;
  }
  memcpy(&kfold, bytes, sizeof(int));
  memcpy(&k, bytes + sizeof(int), sizeof(int));
  Resampler *r = resampler_new(kfold, k);
  if (r == NULL) {
    set_error(err, "alloc", "out of memory");
    return R8_ERROR;
  }
  *state = r;
  return R8_OK;
}

static void resampler_drop(void *state) {
  free(state);
}

static size_t resampler_owned_bytes(const void *state) {
  (void) state;
  return sizeof(Resampler);
}

// An enum's codec: `variant` and `from_variant`, and no `from_fields`.
const R8Codec pkgc_resampler_codec = {
    .size = sizeof(R8Codec),
    .type_name = "pkgc::Resampler",
    .from_fields = NULL,
    .encode = NULL,
    .write_state = resampler_write_state,
    .read_state = resampler_read_state,
    .get_field = resampler_get_field,
    .with_field = resampler_with_field,
    .drop = resampler_drop,
    .owned_bytes = resampler_owned_bytes,
    .variant = resampler_variant,
    .from_variant = resampler_from_variant,
    // The hash of the R declaration of pkgc::Resampler (R/types.R), whose
    // variants' fields from_variant reads by position.
    .declaration_hash = "sha256:87c77663033a8e37ed10ed3ec09caa6556113a10d8d7a044099f32f7dda85cef",
};

// Codecs of pkgc::Signal that registration refuses, by `kind`: 1 sets the
// enum members, 2 pins another declaration hash, and 3 pins none.
extern const R8Codec pkgc_signal_codec;
static R8Codec misshaped;
static R8Exports bad_exports;

// An external pointer to an exports table whose one codec is the codec of
// pkgc::Signal changed as `kind` says, for the tests of registration's
// refusals.
SEXP pkgc_bad_exports(SEXP kind) {
  misshaped = pkgc_signal_codec;
  switch (Rf_asInteger(kind)) {
    case 1:
      misshaped.variant = resampler_variant;
      misshaped.from_variant = resampler_from_variant;
      break;
    case 2:
      misshaped.declaration_hash = "sha256:00";
      break;
    default:
      misshaped.declaration_hash = NULL;
  }
  bad_exports = (R8Exports) {
      .abi_version = R8_ABI_VERSION,
      .size = sizeof(R8Exports),
      .package = "r8pkgC",
      .types = &misshaped,
      .n_types = 1,
  };
  return R_MakeExternalPtr(&bad_exports, Rf_install("r8_exports"), R_NilValue);
}
