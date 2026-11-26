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
// `data1` as the owner of any view. A value read from a stream has its fields in
// data2 and no state, which the codec builds from them when compiled code first
// asks for it (rust_stored.rs, `build()`). Once an element was written, data1
// is R_NilValue and data2 the list of the elements: the object is a list as any
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
SEXP r8_fields_of_state(SEXP state);
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

void r8_continue_unwind(void);

// The fields of a value without a state, which a stream gave, or NULL.
static SEXP fields_read(SEXP x) {
  SEXP data1 = R_altrep_data1(x);
  return data1 != R_NilValue && R_ExternalPtrAddr(data1) == NULL ? R_altrep_data2(x) : NULL;
}

static R_xlen_t rust_stored_Length(SEXP x) {
  return XLENGTH(R_altrep_data2(x));
}

// The field `i`, read from the state unless it was read before.
static SEXP read_field(SEXP x, R_xlen_t i) {
  SEXP v = VECTOR_ELT(R_altrep_data2(x), i);
  if (v != unfilled) {
    return v;
  }
  SEXP data1 = R_altrep_data1(x);
  const void *state = R_ExternalPtrAddr(data1);
  if (state == NULL) {
    Rf_error("the value holds no state [state_absent]");
  }
  const R8Codec *vt = rust_stored_codec(data1);
  SEXP names = VECTOR_ELT(R_ExternalPtrTag(data1), ID_NAMES);
  R8Error err = {{0}, {0}};
  SEXP value = R_NilValue;
  R8Status status = vt->get_field(state, data1, CHAR(STRING_ELT(names, i)), &value, &err);
  if (status == R8_UNWIND) {
    // A jump of R that the codec's R8Api.protect caught: an interrupt stays
    // an interrupt, and an error's message is written once.
    r8_continue_unwind();
  }
  if (status != R8_OK) {
    Rf_error("%s [%s]", err.message, err.code);
  }
  return value;
}

static SEXP rust_stored_Elt(SEXP x, R_xlen_t i) {
  SEXP value = PROTECT(read_field(x, i));
  SET_VECTOR_ELT(R_altrep_data2(x), i, value);
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

// list(identity, fields), as for an R-stored value (value.c). The fields read for
// it are not kept. NULL, which serializes a plain list, for an object whose
// elements were written.
static SEXP rust_stored_Serialized_state(SEXP x) {
  SEXP data1 = R_altrep_data1(x);
  if (data1 == R_NilValue) {
    return NULL;
  }
  R_xlen_t n = XLENGTH(R_altrep_data2(x));
  SEXP fields = PROTECT(Rf_allocVector(VECSXP, n));
  for (R_xlen_t i = 0; i < n; i++) {
    SET_VECTOR_ELT(fields, i, read_field(x, i));
  }
  SEXP out = PROTECT(Rf_allocVector(VECSXP, 2));
  SET_VECTOR_ELT(out, 0, r8_identity_state(R_ExternalPtrTag(data1)));
  SET_VECTOR_ELT(out, 1, fields);
  UNPROTECT(2);
  return out;
}

// A value over the saved fields, without a state. Its fields read as those of any
// value, without the package that declares the type.
static SEXP rust_stored_UnserializeEX(SEXP cls, SEXP saved, SEXP attr, int objf, int levs) {
  (void) cls;
  (void) objf;
  (void) levs;
  SEXP fields = r8_fields_of_state(saved);
  SEXP id = PROTECT(r8_identity_of_state(VECTOR_ELT(saved, 0), attr, XLENGTH(fields)));
  SEXP codec = PROTECT(R_MakeExternalPtr(NULL, R_NilValue, R_NilValue));
  SEXP data1 = PROTECT(R_MakeExternalPtr(NULL, id, codec));
  R_RegisterCFinalizerEx(data1, rust_stored_finalize, TRUE);
  SEXP cache = PROTECT(Rf_shallow_duplicate(fields));
  SEXP ans = PROTECT(R_new_altrep(rust_stored_class, data1, cache));
  r8_set_read_attributes(ans, id, attr);
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
  Rprintf(" r8 rust-stored %s (fields=%lld, state %s)\n",
          CHAR(STRING_ELT(VECTOR_ELT(R_ExternalPtrTag(data1), ID_TYPE), 0)),
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

// The fields of a Rust-stored value that has no state, or NULL for any other
// object.
SEXP r8_rust_stored_fields_read(SEXP x) {
  return ALTREP(x) && R_altrep_inherits(x, rust_stored_class) ? fields_read(x) : NULL;
}

// Gives a value without a state the state that `vt` built from its fields; the
// value owns it once this returns.
void r8_rust_stored_give(SEXP x, const R8Codec *vt, void *state) {
  SEXP data1 = R_altrep_data1(x);
  SEXP codec = R_ExternalPtrProtected(data1);
  SEXP bytes =
      PROTECT(Rf_ScalarReal(vt->owned_bytes != NULL ? (double) vt->owned_bytes(state) : 0));
  R_SetExternalPtrProtected(codec, bytes);
  rust_stored_live += REAL(bytes)[0];
  R_SetExternalPtrAddr(codec, (void *) vt);
  R_SetExternalPtrAddr(data1, state);
  UNPROTECT(1);
}

// Takes the state back from a value that wrapped it, so that the value's
// finalizer leaves it to the caller, which drops it: a value that Rust code
// made and that fails its type's validation.
void r8_rust_stored_take(SEXP x) {
  SEXP data1 = R_altrep_data1(x);
  rust_stored_live -= counted(data1);
  R_ClearExternalPtr(data1);
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
