// rust_stored.c
// ::R8::
// 2026- EDG rtemis.org

// Rust-stored values: an ALTLIST over a state that a registered codec
// (R8Codec) owns.
//
// data1 is an external pointer whose address is the state, whose tag is the
// identity of the value (value.c), and whose protected value is an external
// pointer to the codec. Its finalizer calls the codec's `drop`. data2 is a list
// with one element per field, the field's R value once read and the unfilled
// marker before; `get_field` fills it on the first read of each field, with
// `data1` as the owner of any view. Once an element was written, data1 is
// R_NilValue and data2 the list of the elements: the object is a list as any
// other.

#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define R8_INTERNAL_ABI
#include <R8.h>
#include <R_ext/Altrep.h>
#include <R_ext/Rdynload.h>

static R_altrep_class_t rust_stored_class;

// The identities of values (value.c), and their parts.
SEXP r8_identity_state(SEXP id);
SEXP r8_identity_of_state(SEXP state, SEXP attr, R_xlen_t n);
void r8_set_read_attributes(SEXP ans, SEXP id, SEXP attr);
SEXP r8_value_of_state(SEXP state, SEXP attr);
enum { ID_TYPE, ID_VARIANT, ID_NAMES, ID_DECLARATION, ID_LIKE };

// The marker of a field not yet read, distinct from a NULL field.
static SEXP unfilled = NULL;

static const R8Codec *rust_stored_codec(SEXP data1) {
  return (const R8Codec *) R_ExternalPtrAddr(R_ExternalPtrProtected(data1));
}

// The bytes of the states that values reachable from R hold, and the
// level at which R8 requests a collection: twice the bytes live after the
// last collection R8 requested, and at least RUST_STORED_MIN_THRESHOLD. R does
// not see memory a codec allocates, so without the request it would collect
// as rarely as the small ALTLIST objects alone warrant.
#define RUST_STORED_MIN_THRESHOLD (64.0 * 1024 * 1024)
static double rust_stored_live = 0;
static double rust_stored_threshold = RUST_STORED_MIN_THRESHOLD;

// The bytes counted for a value, kept as the protected value of its codec
// pointer.
static double counted(SEXP data1) {
  SEXP b = R_ExternalPtrProtected(R_ExternalPtrProtected(data1));
  return b == R_NilValue ? 0 : REAL(b)[0];
}

static void count(SEXP data1, const R8Codec *vt, const void *state) {
  double bytes = vt->owned_bytes != NULL ? (double) vt->owned_bytes(state) : 0;
  R_SetExternalPtrProtected(R_ExternalPtrProtected(data1), Rf_ScalarReal(bytes));
  rust_stored_live += bytes;
}

// Requests a collection before `bytes` more would pass the threshold.
static void reserve(double bytes) {
  if (rust_stored_live + bytes > rust_stored_threshold) {
    R_gc();
    double next = 2 * (rust_stored_live + bytes);
    rust_stored_threshold = next > RUST_STORED_MIN_THRESHOLD ? next : RUST_STORED_MIN_THRESHOLD;
  }
}

static void rust_stored_finalize(SEXP data1) {
  void *state = R_ExternalPtrAddr(data1);
  const R8Codec *vt = rust_stored_codec(data1);
  if (state != NULL && vt != NULL && vt->drop != NULL) {
    rust_stored_live -= counted(data1);
    vt->drop(state);
  }
  R_ClearExternalPtr(data1);
}

// The Rust code's records of a codec (rust_stored.rs).
SEXP r8_rust_stored_package(const char *type_name);
const R8Codec *r8_rust_stored_codec(const char *type_name, const char *declaration, R8Error *err);

// The variant of a state of an enum's codec; NULL for a struct's codec and
// for a table whose `size` ends before the enum members (R8 0.3.0's).
static const char *state_variant(const R8Codec *vt, const void *state) {
  if (vt->size < offsetof(R8Codec, from_variant) + sizeof(vt->from_variant) ||
      vt->variant == NULL) {
    return NULL;
  }
  return vt->variant(state);
}

// A value read by `UnserializeEX` before its codec was registered keeps the
// serialized state, list(identity, package, bytes, kept), where `identity` is
// that of `r8_identity_state()` and `kept` the list of the R values that the
// state holds, or NULL, as the tag of its codec pointer, and builds its state on
// first entry.
static SEXP kept_state(SEXP data1) {
  return R_ExternalPtrTag(R_ExternalPtrProtected(data1));
}

// The parts of the identity of a kept state, as text: 0 the type, 1 the
// variant, or NULL, and 2 the declaration.
static const char *kept_text(SEXP kept, int i) {
  SEXP part = VECTOR_ELT(VECTOR_ELT(kept, 0), i);
  return part == R_NilValue ? NULL : CHAR(PRINTNAME(part));
}

// Builds the state of a value read before its codec was registered;
// returns 0, with `err` filled, when it cannot.
static int build_state(SEXP data1, R8Error *err) {
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
  const R8Codec *vt = r8_rust_stored_codec(kept_text(kept, 0), kept_text(kept, 2), err);
  if (vt == NULL) {
    char reason[sizeof(err->message)];
    snprintf(reason, sizeof(reason), "%s", err->message);
    const char *pkg = CHAR(STRING_ELT(VECTOR_ELT(kept, 1), 0));
    if (strcmp(err->code, "codec_missing") == 0) {
      snprintf(err->message, sizeof(err->message),
               "this `%s` value needs the codec of the package `%s`; load it with library(%s): %.600s",
               kept_text(kept, 0), pkg, pkg, reason);
    } else {
      snprintf(err->message, sizeof(err->message), "the fields of a saved `%s` value do not read: %.800s",
               kept_text(kept, 0), reason);
    }
    return 0;
  }
  if (vt->read_state == NULL) {
    snprintf(err->code, sizeof(err->code), "%s", "no_read_state");
    snprintf(err->message, sizeof(err->message), "the codec of `%s` has no `read_state`",
             vt->type_name);
    return 0;
  }
  SEXP bytes = VECTOR_ELT(kept, 2);
  void *state = NULL;
  if (vt->read_state(RAW_RO(bytes), (size_t) XLENGTH(bytes), VECTOR_ELT(kept, 3), &state, err) !=
          R8_OK ||
      state == NULL) {
    return 0;
  }
  // An enum value's state has the variant the value was saved with.
  const char *expected = kept_text(kept, 1);
  if (expected != NULL) {
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
// registered.
static const void *rust_stored_state(SEXP x) {
  SEXP data1 = R_altrep_data1(x);
  const void *state = R_ExternalPtrAddr(data1);
  if (state != NULL) {
    return state;
  }
  R8Error err = {{0}, {0}};
  if (build_state(data1, &err)) {
    return R_ExternalPtrAddr(data1);
  }
  Rf_error("%s [%s]", err.message, err.code);
  return NULL;
}

static R_xlen_t rust_stored_Length(SEXP x) {
  return XLENGTH(R_altrep_data2(x));
}

static SEXP rust_stored_Elt(SEXP x, R_xlen_t i) {
  SEXP cache = R_altrep_data2(x);
  SEXP v = VECTOR_ELT(cache, i);
  if (v != unfilled) {
    return v;
  }
  SEXP data1 = R_altrep_data1(x);
  const void *state = rust_stored_state(x);
  const R8Codec *vt = rust_stored_codec(data1);
  SEXP names = VECTOR_ELT(R_ExternalPtrTag(data1), ID_NAMES);
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

// An element that is written again with itself leaves the value as it is. Any
// other element makes the object a list of its elements, without a state.
static void rust_stored_Set_elt(SEXP x, R_xlen_t i, SEXP v) {
  if (VECTOR_ELT(R_altrep_data2(x), i) == v) {
    return;
  }
  if (R_altrep_data1(x) != R_NilValue) {
    PROTECT(v);
    R_xlen_t n = XLENGTH(R_altrep_data2(x));
    SEXP list = PROTECT(Rf_allocVector(VECSXP, n));
    for (R_xlen_t k = 0; k < n; k++) {
      SET_VECTOR_ELT(list, k, rust_stored_Elt(x, k));
    }
    R_set_altrep_data2(x, list);
    R_set_altrep_data1(x, R_NilValue);
    UNPROTECT(2);
  }
  SET_VECTOR_ELT(R_altrep_data2(x), i, v);
}

// A copy shares the state and copies the cache; R copies the attributes.
static SEXP rust_stored_Duplicate(SEXP x, Rboolean deep) {
  SEXP cache = PROTECT(Rf_shallow_duplicate(R_altrep_data2(x)));
  SEXP ans = R_new_altrep(rust_stored_class, R_altrep_data1(x), cache);
  UNPROTECT(1);
  return ans;
}

// The R8Writers of `write_state`: one counts the bytes and the kept values,
// the other fills a raw vector and a list of those sizes.
typedef struct Sink {
  uint8_t *bytes;
  size_t len, cap;
  SEXP kept;
  size_t n_kept, cap_kept;
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

static R8Status count_keep(void *context, SEXP value, size_t *index) {
  (void) value;
  *index = ((Sink *) context)->n_kept++;
  return R8_OK;
}

static R8Status fill_keep(void *context, SEXP value, size_t *index) {
  Sink *s = context;
  if (s->n_kept >= s->cap_kept) {
    return R8_ERROR;
  }
  SET_VECTOR_ELT(s->kept, (R_xlen_t) s->n_kept, value);
  *index = s->n_kept++;
  return R8_OK;
}

// list(identity, package, bytes, kept): the state's bytes and the R values it
// holds from the codec's `write_state`, with the identity that `UnserializeEX`
// reads; R serializes the kept values as it does any value.
// list(identity, fields), the state of an R-stored value, for a codec without
// `write_state`. NULL, which serializes a plain list, for an object whose
// elements were written and for a type absent from the registry.
static SEXP rust_stored_Serialized_state(SEXP x) {
  SEXP data1 = R_altrep_data1(x);
  if (data1 == R_NilValue) {
    return NULL;
  }
  if (R_ExternalPtrAddr(data1) == NULL && kept_state(data1) != R_NilValue) {
    return kept_state(data1);
  }
  const void *state = rust_stored_state(x);
  const R8Codec *vt = rust_stored_codec(data1);
  if (vt->write_state == NULL) {
    R_xlen_t n = XLENGTH(R_altrep_data2(x));
    SEXP fields = PROTECT(Rf_allocVector(VECSXP, n));
    for (R_xlen_t i = 0; i < n; i++) {
      SET_VECTOR_ELT(fields, i, rust_stored_Elt(x, i));
    }
    SEXP out = PROTECT(Rf_allocVector(VECSXP, 2));
    SET_VECTOR_ELT(out, 0, r8_identity_state(R_ExternalPtrTag(data1)));
    SET_VECTOR_ELT(out, 1, fields);
    UNPROTECT(2);
    return out;
  }
  SEXP package = PROTECT(r8_rust_stored_package(vt->type_name));
  if (package == R_NilValue) {
    UNPROTECT(1);
    return NULL;
  }
  // Two passes, so the bytes are held once: the first counts them, the
  // second writes them into a raw vector of that size.
  R8Error err = {{0}, {0}};
  Sink counter = {NULL, 0, 0, R_NilValue, 0, 0};
  R8Writer w = {&counter, count_write, count_keep};
  if (vt->write_state(state, &w, &err) != R8_OK) {
    UNPROTECT(1);
    Rf_error("%s [%s]", err.message, err.code);
  }
  SEXP bytes = PROTECT(Rf_allocVector(RAWSXP, (R_xlen_t) counter.len));
  SEXP values = PROTECT(
      counter.n_kept > 0 ? Rf_allocVector(VECSXP, (R_xlen_t) counter.n_kept) : R_NilValue);
  Sink sink = {RAW(bytes), 0, counter.len, values, 0, counter.n_kept};
  w.context = &sink;
  w.write = fill_write;
  w.keep = fill_keep;
  if (vt->write_state(state, &w, &err) != R8_OK || sink.len != counter.len ||
      sink.n_kept != counter.n_kept) {
    UNPROTECT(3);
    if (err.code[0] == '\0') {
      Rf_error("`write_state` of `%s` wrote other bytes or kept other values on its second pass",
               vt->type_name);
    }
    Rf_error("%s [%s]", err.message, err.code);
  }
  SEXP out = PROTECT(Rf_allocVector(VECSXP, 4));
  SET_VECTOR_ELT(out, 0, r8_identity_state(R_ExternalPtrTag(data1)));
  SET_VECTOR_ELT(out, 1, package);
  SET_VECTOR_ELT(out, 2, bytes);
  SET_VECTOR_ELT(out, 3, values);
  UNPROTECT(4);
  return out;
}

// A value over the serialized state, an R-stored value of one of its fields,
// and otherwise one whose state is built at once when its
// codec is registered and on first entry otherwise.
static SEXP rust_stored_UnserializeEX(SEXP cls, SEXP kept, SEXP attr, int objf, int levs) {
  (void) cls;
  (void) objf;
  (void) levs;
  if (TYPEOF(kept) == VECSXP && XLENGTH(kept) == 2) {
    return r8_value_of_state(kept, attr);
  }
  if (TYPEOF(kept) != VECSXP || XLENGTH(kept) != 4 || TYPEOF(VECTOR_ELT(kept, 1)) != STRSXP ||
      TYPEOF(VECTOR_ELT(kept, 2)) != RAWSXP) {
    Rf_error("a saved R8 value is damaged");
  }
  SEXP id = PROTECT(r8_identity_of_state(VECTOR_ELT(kept, 0), attr, -1));
  SEXP codec = PROTECT(R_MakeExternalPtr(NULL, kept, R_NilValue));
  SEXP data1 = PROTECT(R_MakeExternalPtr(NULL, id, codec));
  R_RegisterCFinalizerEx(data1, rust_stored_finalize, TRUE);
  R_xlen_t n = XLENGTH(VECTOR_ELT(id, ID_NAMES));
  SEXP cache = PROTECT(Rf_allocVector(VECSXP, n));
  for (R_xlen_t i = 0; i < n; i++) {
    SET_VECTOR_ELT(cache, i, unfilled);
  }
  // Building the state allocates.
  SEXP ans = PROTECT(R_new_altrep(rust_stored_class, data1, cache));
  r8_set_read_attributes(ans, id, attr);
  R8Error err = {{0}, {0}};
  build_state(data1, &err);
  UNPROTECT(5);
  return ans;
}

static Rboolean rust_stored_Inspect(SEXP x, int pre, int deep, int pvec,
                               void (*inspect_subtree)(SEXP, int, int, int)) {
  SEXP data1 = R_altrep_data1(x);
  if (data1 == R_NilValue) {
    Rprintf(" r8 rust-stored, written, a list (fields=%lld)\n",
            (long long) XLENGTH(R_altrep_data2(x)));
    return TRUE;
  }
  const R8Codec *vt = rust_stored_codec(data1);
  SEXP kept = kept_state(data1);
  Rprintf(" r8 rust-stored %s (fields=%lld, state %s)\n",
          vt != NULL ? vt->type_name : (kept != R_NilValue ? kept_text(kept, 0) : "?"),
          (long long) XLENGTH(R_altrep_data2(x)),
          R_ExternalPtrAddr(data1) != NULL ? "present" : "absent");
  return TRUE;
}

void r8_init_rust_stored(DllInfo *dll) {
  unfilled = R_MakeExternalPtr(NULL, Rf_install("r8_unfilled"), R_NilValue);
  R_PreserveObject(unfilled);
  rust_stored_class = R_make_altlist_class("r8_rust", "R8", dll);
  R_set_altrep_Length_method(rust_stored_class, rust_stored_Length);
  R_set_altrep_Inspect_method(rust_stored_class, rust_stored_Inspect);
  R_set_altrep_Duplicate_method(rust_stored_class, rust_stored_Duplicate);
  R_set_altrep_Serialized_state_method(rust_stored_class, rust_stored_Serialized_state);
  R_set_altrep_UnserializeEX_method(rust_stored_class, rust_stored_UnserializeEX);
  R_set_altlist_Elt_method(rust_stored_class, rust_stored_Elt);
  R_set_altlist_Set_elt_method(rust_stored_class, rust_stored_Set_elt);
}

// A value of the identity `id` over `state`; the value owns the state once this
// returns, and the caller owns it when R jumps out. When `from` is a
// Rust-stored value, the new value's cache takes the fields `from` has read,
// except field `skip`.
SEXP r8_rust_stored_wrap(const R8Codec *vt, void *state, SEXP id, SEXP from, R_xlen_t skip) {
  double bytes = vt->owned_bytes != NULL ? (double) vt->owned_bytes(state) : 0;
  reserve(bytes);
  SEXP names = VECTOR_ELT(id, ID_NAMES);
  SEXP size = PROTECT(Rf_ScalarReal(bytes));
  SEXP codec = PROTECT(R_MakeExternalPtr((void *) vt, R_NilValue, size));
  SEXP data1 = PROTECT(R_MakeExternalPtr(state, id, codec));
  R_xlen_t n = XLENGTH(names);
  SEXP cache = PROTECT(Rf_allocVector(VECSXP, n));
  int carry = from != R_NilValue && ALTREP(from) && R_altrep_inherits(from, rust_stored_class) &&
              R_altrep_data1(from) != R_NilValue && XLENGTH(R_altrep_data2(from)) == n;
  for (R_xlen_t i = 0; i < n; i++) {
    SET_VECTOR_ELT(cache, i,
                   carry && i != skip ? VECTOR_ELT(R_altrep_data2(from), i) : unfilled);
  }
  SEXP ans = PROTECT(R_new_altrep(rust_stored_class, data1, cache));
  // The attribute vectors are shared, as every value of the type shares them; a
  // deep copy would allocate them per value.
  SHALLOW_DUPLICATE_ATTRIB(ans, VECTOR_ELT(id, ID_LIKE));
  // The finalizer is registered after every other allocation: when R jumps
  // before it, the state stays the caller's to drop.
  R_RegisterCFinalizerEx(data1, rust_stored_finalize, TRUE);
  rust_stored_live += bytes;
  UNPROTECT(5);
  return ans;
}

// The identity of a Rust-stored value, or R_NilValue for any other object
// and one whose elements were written.
SEXP r8_rust_stored_id(SEXP x) {
  if (!R_altrep_inherits(x, rust_stored_class) || R_altrep_data1(x) == R_NilValue) {
    return R_NilValue;
  }
  return R_ExternalPtrTag(R_altrep_data1(x));
}

// The state and codec of a Rust-stored value, or NULL for any other object.
const void *r8_rust_stored_state(SEXP x, const R8Codec **vt) {
  if (!ALTREP(x) || !R_altrep_inherits(x, rust_stored_class) ||
      R_altrep_data1(x) == R_NilValue) {
    return NULL;
  }
  SEXP data1 = R_altrep_data1(x);
  if (vt != NULL) {
    *vt = rust_stored_codec(data1);
  }
  return R_ExternalPtrAddr(data1);
}

// Builds the state of `x`, a Rust-stored value that `Unserialize` read before
// its codec was registered; returns 0, with `err` filled, when it cannot, and
// 1 for any other object.
int r8_rust_stored_build(SEXP x, R8Error *err) {
  if (!ALTREP(x) || !R_altrep_inherits(x, rust_stored_class) ||
      R_altrep_data1(x) == R_NilValue) {
    return 1;
  }
  return build_state(R_altrep_data1(x), err);
}

int r8_is_view(SEXP x);

// The state's bytes and the cached fields that are not views, which
// r8_size() adds up: list(state = <bytes>, fields = <list>); NULL for an
// object that is not a Rust-stored value.
SEXP r8_rust_stored_parts(SEXP x) {
  if (!ALTREP(x) || !R_altrep_inherits(x, rust_stored_class) ||
      R_altrep_data1(x) == R_NilValue) {
    return R_NilValue;
  }
  rust_stored_state(x);
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
SEXP r8_rust_stored_memory(void) {
  SEXP out = Rf_allocVector(REALSXP, 2);
  REAL(out)[0] = rust_stored_live;
  REAL(out)[1] = rust_stored_threshold;
  return out;
}
