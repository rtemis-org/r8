// native.c
// ::R8::
// 2026- EDG rtemis.org

// Rust-native values: an ALTLIST over a state that a registered codec
// (R8TypeVTable) owns.
//
// data1 is an external pointer whose address is the state, whose tag is the
// field names (a character vector in declaration order), and whose
// protected value is an external pointer to the codec. Its finalizer calls
// the codec's `drop`. data2 is a list with one element per field, the field's
// R value once read and the unfilled marker before; `get_field` fills it on
// the first read of each field, with `data1` as the owner of any view.

#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <R8.h>
#include <R_ext/Altrep.h>
#include <R_ext/Rdynload.h>

static R_altrep_class_t native_class;

// The marker of a field not yet read, distinct from a NULL field.
static SEXP unfilled = NULL;

static const R8TypeVTable *native_codec(SEXP data1) {
  return (const R8TypeVTable *) R_ExternalPtrAddr(R_ExternalPtrProtected(data1));
}

// The bytes of the states that values reachable from R hold, and the
// level at which R8 requests a collection: twice the bytes live after the
// last collection R8 requested, and at least NATIVE_MIN_THRESHOLD. R does
// not see memory a codec allocates, so without the request it would collect
// as rarely as the small ALTLIST objects alone warrant.
#define NATIVE_MIN_THRESHOLD (64.0 * 1024 * 1024)
static double native_live = 0;
static double native_threshold = NATIVE_MIN_THRESHOLD;

// The bytes counted for a value, kept as the protected value of its codec
// pointer.
static double counted(SEXP data1) {
  SEXP b = R_ExternalPtrProtected(R_ExternalPtrProtected(data1));
  return b == R_NilValue ? 0 : REAL(b)[0];
}

static void count(SEXP data1, const R8TypeVTable *vt, const void *state) {
  double bytes = vt->bytes != NULL ? (double) vt->bytes(state) : 0;
  R_SetExternalPtrProtected(R_ExternalPtrProtected(data1), Rf_ScalarReal(bytes));
  native_live += bytes;
}

// Requests a collection before `bytes` more would pass the threshold.
static void reserve(double bytes) {
  if (native_live + bytes > native_threshold) {
    R_gc();
    double next = 2 * (native_live + bytes);
    native_threshold = next > NATIVE_MIN_THRESHOLD ? next : NATIVE_MIN_THRESHOLD;
  }
}

static void native_finalize(SEXP data1) {
  void *state = R_ExternalPtrAddr(data1);
  const R8TypeVTable *vt = native_codec(data1);
  if (state != NULL && vt != NULL && vt->drop != NULL) {
    native_live -= counted(data1);
    vt->drop(state);
  }
  R_ClearExternalPtr(data1);
}

// The Rust code's records of a codec (native.rs).
SEXP r8_native_meta(const char *type_name);
const R8TypeVTable *r8_native_resolve(const char *type_name, int version, const char *hash,
                                      R8Error *err);

// The variant of a state of an enum's codec; NULL for a struct's codec and
// for a table whose `size` ends before the enum members (R8 0.3.0's).
static const char *state_variant(const R8TypeVTable *vt, const void *state) {
  if (vt->size < offsetof(R8TypeVTable, from_variant) + sizeof(vt->from_variant) ||
      vt->variant == NULL) {
    return NULL;
  }
  return vt->variant(state);
}

// A value read by `Unserialize` before its codec was registered keeps the
// serialized state, list(type, version, hash, package, names, bytes), and
// for an enum value a seventh element, its variant, as the tag of its codec
// pointer, and builds its state on first entry.
static SEXP kept_state(SEXP data1) {
  return R_ExternalPtrTag(R_ExternalPtrProtected(data1));
}

static const char *kept_text(SEXP kept, int i) {
  return CHAR(STRING_ELT(VECTOR_ELT(kept, i), 0));
}

// Builds the state of a value read before its codec was registered;
// returns 0, with `err` filled, when it cannot.
static int resolve(SEXP data1, R8Error *err) {
  if (R_ExternalPtrAddr(data1) != NULL) {
    return 1;
  }
  SEXP codec = R_ExternalPtrProtected(data1);
  SEXP kept = R_ExternalPtrTag(codec);
  if (kept == R_NilValue) {
    snprintf(err->code, sizeof(err->code), "%s", "state_absent");
    snprintf(err->message, sizeof(err->message), "%s", "the value holds no state");
    return 0;
  }
  const R8TypeVTable *vt =
      r8_native_resolve(kept_text(kept, 0), Rf_asInteger(VECTOR_ELT(kept, 1)), kept_text(kept, 2), err);
  if (vt == NULL) {
    return 0;
  }
  if (vt->read_state == NULL) {
    snprintf(err->code, sizeof(err->code), "%s", "no_read_state");
    snprintf(err->message, sizeof(err->message), "the codec of `%s` has no `read_state`",
             vt->type_name);
    return 0;
  }
  SEXP bytes = VECTOR_ELT(kept, 5);
  void *state = NULL;
  if (vt->read_state(RAW_RO(bytes), (size_t) XLENGTH(bytes), &state, err) != R8_OK ||
      state == NULL) {
    return 0;
  }
  // An enum value's state has the variant the value was saved with.
  if (XLENGTH(kept) > 6) {
    const char *expected = kept_text(kept, 6);
    const char *found = state_variant(vt, state);
    if (found == NULL || strcmp(found, expected) != 0) {
      snprintf(err->code, sizeof(err->code), "%s", "bad_state");
      snprintf(err->message, sizeof(err->message),
               "`read_state` of `%s` built the variant `%s` for a value saved as `%s`",
               vt->type_name, found == NULL ? "" : found, expected);
      if (vt->drop != NULL) {
        vt->drop(state);
      }
      return 0;
    }
  }
  R_SetExternalPtrAddr(codec, (void *) vt);
  R_SetExternalPtrTag(codec, R_NilValue);
  R_SetExternalPtrAddr(data1, state);
  count(data1, vt, state);
  return 1;
}

// The state of `x`, built first for a value read before its codec was
// registered; an error names the type and the package that registers it.
static const void *native_state(SEXP x) {
  SEXP data1 = R_altrep_data1(x);
  const void *state = R_ExternalPtrAddr(data1);
  if (state != NULL) {
    return state;
  }
  SEXP kept = kept_state(data1);
  R8Error err = {{0}, {0}};
  if (resolve(data1, &err)) {
    return R_ExternalPtrAddr(data1);
  }
  if (kept != R_NilValue) {
    const char *pkg = kept_text(kept, 3);
    Rf_error("this `%s` value needs the codec of the package `%s`; load it with "
             "library(%s): %s [%s]",
             kept_text(kept, 0), pkg, pkg, err.message, err.code);
  }
  Rf_error("%s [%s]", err.message, err.code);
  return NULL;
}

static R_xlen_t native_Length(SEXP x) {
  return XLENGTH(R_altrep_data2(x));
}

static SEXP native_Elt(SEXP x, R_xlen_t i) {
  SEXP cache = R_altrep_data2(x);
  SEXP v = VECTOR_ELT(cache, i);
  if (v != unfilled) {
    return v;
  }
  SEXP data1 = R_altrep_data1(x);
  const void *state = native_state(x);
  const R8TypeVTable *vt = native_codec(data1);
  SEXP names = R_ExternalPtrTag(data1);
  R8Error err = {{0}, {0}};
  SEXP value = R_NilValue;
  R8Status status = vt->get_field(state, data1, CHAR(STRING_ELT(names, i)), &value, &err);
  if (status != R8_OK) {
    Rf_error("%s [%s]", err.message, err.code);
  }
  PROTECT(value);
  SET_VECTOR_ELT(cache, i, value);
  UNPROTECT(1);
  return value;
}

static void native_Set_elt(SEXP x, R_xlen_t i, SEXP v) {
  Rf_error("an R8 value is immutable; `$<-` returns a new value");
}

// A copy shares the state and copies the cache; R copies the attributes.
static SEXP native_Duplicate(SEXP x, Rboolean deep) {
  SEXP cache = PROTECT(Rf_shallow_duplicate(R_altrep_data2(x)));
  SEXP ans = R_new_altrep(native_class, R_altrep_data1(x), cache);
  UNPROTECT(1);
  return ans;
}

// The R8Writers of `write_state`: one counts the bytes, the other fills a
// raw vector of that size.
typedef struct Sink {
  uint8_t *bytes;
  size_t len, cap;
} Sink;

static R8Status count_write(void *context, const uint8_t *bytes, size_t len) {
  (void) bytes;
  ((Sink *) context)->len += len;
  return R8_OK;
}

static R8Status fill_write(void *context, const uint8_t *bytes, size_t len) {
  Sink *s = context;
  if (len > s->cap - s->len) {
    return R8_ERROR;
  }
  if (len > 0) {
    memcpy(s->bytes + s->len, bytes, len);
  }
  s->len += len;
  return R8_OK;
}

// list(type, version, hash, package, names, bytes), with the variant as a
// seventh element for an enum value: the state's bytes from the codec's
// `write_state`, with the records that `Unserialize` checks.
// NULL, which serializes the fields as a plain list, for a codec without
// `write_state` or a type absent from the registry.
static SEXP native_Serialized_state(SEXP x) {
  SEXP data1 = R_altrep_data1(x);
  if (R_ExternalPtrAddr(data1) == NULL && kept_state(data1) != R_NilValue) {
    return kept_state(data1);
  }
  const void *state = native_state(x);
  const R8TypeVTable *vt = native_codec(data1);
  if (vt->write_state == NULL) {
    return NULL;
  }
  SEXP meta = PROTECT(r8_native_meta(vt->type_name));
  if (meta == R_NilValue) {
    UNPROTECT(1);
    return NULL;
  }
  // Two passes, so the bytes are held once: the first counts them, the
  // second writes them into a raw vector of that size.
  R8Error err = {{0}, {0}};
  Sink counter = {NULL, 0, 0};
  R8Writer w = {&counter, count_write};
  if (vt->write_state(state, &w, &err) != R8_OK) {
    UNPROTECT(1);
    Rf_error("%s [%s]", err.message, err.code);
  }
  SEXP bytes = PROTECT(Rf_allocVector(RAWSXP, (R_xlen_t) counter.len));
  Sink sink = {RAW(bytes), 0, counter.len};
  w.context = &sink;
  w.write = fill_write;
  if (vt->write_state(state, &w, &err) != R8_OK || sink.len != counter.len) {
    UNPROTECT(2);
    if (err.code[0] == '\0') {
      Rf_error("`write_state` of `%s` wrote different bytes on its second pass",
               vt->type_name);
    }
    Rf_error("%s [%s]", err.message, err.code);
  }
  const char *variant = state_variant(vt, state);
  SEXP out = PROTECT(Rf_allocVector(VECSXP, variant == NULL ? 6 : 7));
  if (variant != NULL) {
    SET_VECTOR_ELT(out, 6, Rf_mkString(variant));
  }
  SET_VECTOR_ELT(out, 0, Rf_mkString(vt->type_name));
  SET_VECTOR_ELT(out, 1, VECTOR_ELT(meta, 0));
  SET_VECTOR_ELT(out, 2, VECTOR_ELT(meta, 1));
  SET_VECTOR_ELT(out, 3, VECTOR_ELT(meta, 2));
  SET_VECTOR_ELT(out, 4, R_ExternalPtrTag(data1));
  SET_VECTOR_ELT(out, 5, bytes);
  UNPROTECT(3);
  return out;
}

// A value over the serialized state, whose state is built at once when its
// codec is registered and on first entry otherwise. R sets the attributes.
static SEXP native_Unserialize(SEXP cls, SEXP kept) {
  SEXP names = VECTOR_ELT(kept, 4);
  SEXP codec = PROTECT(R_MakeExternalPtr(NULL, kept, R_NilValue));
  SEXP data1 = PROTECT(R_MakeExternalPtr(NULL, names, codec));
  R_RegisterCFinalizerEx(data1, native_finalize, TRUE);
  R_xlen_t n = XLENGTH(names);
  SEXP cache = PROTECT(Rf_allocVector(VECSXP, n));
  for (R_xlen_t i = 0; i < n; i++) {
    SET_VECTOR_ELT(cache, i, unfilled);
  }
  SEXP ans = R_new_altrep(native_class, data1, cache);
  R8Error err = {{0}, {0}};
  resolve(data1, &err);
  UNPROTECT(3);
  return ans;
}

static Rboolean native_Inspect(SEXP x, int pre, int deep, int pvec,
                               void (*inspect_subtree)(SEXP, int, int, int)) {
  SEXP data1 = R_altrep_data1(x);
  const R8TypeVTable *vt = native_codec(data1);
  SEXP kept = kept_state(data1);
  Rprintf(" r8 native %s (fields=%lld, state %s)\n",
          vt != NULL ? vt->type_name : (kept != R_NilValue ? kept_text(kept, 0) : "?"),
          (long long) XLENGTH(R_altrep_data2(x)),
          R_ExternalPtrAddr(data1) != NULL ? "present" : "absent");
  return TRUE;
}

void r8_init_native(DllInfo *dll) {
  unfilled = R_MakeExternalPtr(NULL, Rf_install("r8_unfilled"), R_NilValue);
  R_PreserveObject(unfilled);
  native_class = R_make_altlist_class("r8_native", "R8", dll);
  R_set_altrep_Length_method(native_class, native_Length);
  R_set_altrep_Inspect_method(native_class, native_Inspect);
  R_set_altrep_Duplicate_method(native_class, native_Duplicate);
  R_set_altrep_Serialized_state_method(native_class, native_Serialized_state);
  R_set_altrep_Unserialize_method(native_class, native_Unserialize);
  R_set_altlist_Elt_method(native_class, native_Elt);
  R_set_altlist_Set_elt_method(native_class, native_Set_elt);
}

// A value over `state` with the attributes of `like`, the assembled list of
// the value's fields, whose names are the field names; the value owns the
// state once this returns, and the caller owns it when R jumps out. When
// `from` is a Rust-native value, the new value's cache takes the fields
// `from` has read, except field `skip`.
SEXP r8_native_wrap(const R8TypeVTable *vt, void *state, SEXP like, SEXP from,
                    R_xlen_t skip) {
  double bytes = vt->bytes != NULL ? (double) vt->bytes(state) : 0;
  reserve(bytes);
  SEXP names = Rf_getAttrib(like, R_NamesSymbol);
  SEXP size = PROTECT(Rf_ScalarReal(bytes));
  SEXP codec = PROTECT(R_MakeExternalPtr((void *) vt, R_NilValue, size));
  SEXP data1 = PROTECT(R_MakeExternalPtr(state, names, codec));
  R_xlen_t n = XLENGTH(names);
  SEXP cache = PROTECT(Rf_allocVector(VECSXP, n));
  int carry = from != R_NilValue && ALTREP(from) && R_altrep_inherits(from, native_class) &&
              XLENGTH(R_altrep_data2(from)) == n;
  for (R_xlen_t i = 0; i < n; i++) {
    SET_VECTOR_ELT(cache, i,
                   carry && i != skip ? VECTOR_ELT(R_altrep_data2(from), i) : unfilled);
  }
  SEXP ans = PROTECT(R_new_altrep(native_class, data1, cache));
  // The attribute vectors are shared, as every value of the type shares
  // them (`construct::shared_attributes()`); a deep copy would allocate them
  // per value.
  SHALLOW_DUPLICATE_ATTRIB(ans, like);
  // The finalizer is registered after every other allocation: when R jumps
  // before it, the state stays the caller's to drop.
  R_RegisterCFinalizerEx(data1, native_finalize, TRUE);
  native_live += bytes;
  UNPROTECT(5);
  return ans;
}

// The state and codec of a Rust-native value, or NULL for any other object.
const void *r8_native_state(SEXP x, const R8TypeVTable **vt) {
  if (!ALTREP(x) || !R_altrep_inherits(x, native_class)) {
    return NULL;
  }
  SEXP data1 = R_altrep_data1(x);
  if (vt != NULL) {
    *vt = native_codec(data1);
  }
  return R_ExternalPtrAddr(data1);
}

int r8_is_view(SEXP x);

// The state's bytes and the cached fields that are not views, which
// r8_size() adds up: list(state = <bytes>, fields = <list>); NULL for an
// object that is not a Rust-native value.
SEXP r8_native_parts(SEXP x) {
  if (!ALTREP(x) || !R_altrep_inherits(x, native_class)) {
    return R_NilValue;
  }
  native_state(x);
  SEXP data1 = R_altrep_data1(x);
  SEXP cache = R_altrep_data2(x);
  R_xlen_t n = XLENGTH(cache), k = 0;
  for (R_xlen_t i = 0; i < n; i++) {
    SEXP v = VECTOR_ELT(cache, i);
    k += v != unfilled && !r8_is_view(v);
  }
  SEXP fields = PROTECT(Rf_allocVector(VECSXP, k));
  for (R_xlen_t i = 0, j = 0; i < n; i++) {
    SEXP v = VECTOR_ELT(cache, i);
    if (v != unfilled && !r8_is_view(v)) {
      SET_VECTOR_ELT(fields, j++, v);
    }
  }
  SEXP out = PROTECT(Rf_allocVector(VECSXP, 2));
  SET_VECTOR_ELT(out, 0, Rf_ScalarReal(counted(data1)));
  SET_VECTOR_ELT(out, 1, fields);
  SEXP names = PROTECT(Rf_allocVector(STRSXP, 2));
  SET_STRING_ELT(names, 0, Rf_mkChar("state"));
  SET_STRING_ELT(names, 1, Rf_mkChar("fields"));
  Rf_setAttrib(out, R_NamesSymbol, names);
  UNPROTECT(3);
  return out;
}

// The bytes of live states and the collection threshold:
// c(live, threshold).
SEXP r8_native_memory(void) {
  SEXP out = Rf_allocVector(REALSXP, 2);
  REAL(out)[0] = native_live;
  REAL(out)[1] = native_threshold;
  return out;
}
