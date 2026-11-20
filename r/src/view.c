// view.c
// ::R8::
// 2026- EDG rtemis.org

// The views of R8Api: ALTREP vectors over memory that another package's
// object owns, and the R8Api table that R8 registers with
// R_RegisterCCallable("R8", "r8_api").
//
// A view's data1 is an external pointer whose address is the first element,
// whose tag is the length as a double, and whose protected value is the
// owner, which keeps the memory alive. data2 is R_NilValue until R asks for
// a writable pointer; it then holds a copy, which every later read and write
// uses, so the owner's memory is only read.

#include <stdio.h>
#include <string.h>

#include <R8.h>
#include <R_ext/Altrep.h>
#include <R_ext/Rdynload.h>

static R_altrep_class_t view_double;
static R_altrep_class_t view_integer;
static R_altrep_class_t view_logical;
static R_altrep_class_t view_raw;

static R_xlen_t view_length(SEXP x) {
  return (R_xlen_t) REAL(R_ExternalPtrTag(R_altrep_data1(x)))[0];
}

static const void *view_memory(SEXP x) {
  return R_ExternalPtrAddr(R_altrep_data1(x));
}

static size_t view_width(SEXP x) {
  switch (TYPEOF(x)) {
  case REALSXP:
    return sizeof(double);
  case INTSXP:
  case LGLSXP:
    return sizeof(int);
  default:
    return 1;
  }
}

// The copy that a write goes to, made on the first request for a writable
// pointer.
static SEXP view_copy(SEXP x) {
  SEXP copy = R_altrep_data2(x);
  if (copy != R_NilValue) {
    return copy;
  }
  R_xlen_t n = view_length(x);
  copy = PROTECT(Rf_allocVector(TYPEOF(x), n));
  void *to;
  switch (TYPEOF(x)) {
  case REALSXP:
    to = REAL(copy);
    break;
  case INTSXP:
    to = INTEGER(copy);
    break;
  case LGLSXP:
    to = LOGICAL(copy);
    break;
  default:
    to = RAW(copy);
    break;
  }
  if (n > 0) {
    memcpy(to, view_memory(x), (size_t) n * view_width(x));
  }
  R_set_altrep_data2(x, copy);
  UNPROTECT(1);
  return copy;
}

static void *copy_pointer(SEXP copy) {
  switch (TYPEOF(copy)) {
  case REALSXP:
    return REAL(copy);
  case INTSXP:
    return INTEGER(copy);
  case LGLSXP:
    return LOGICAL(copy);
  default:
    return RAW(copy);
  }
}

// The elements in effect: the copy once there is one, the owner's memory
// before.
static const void *view_elements(SEXP x) {
  SEXP copy = R_altrep_data2(x);
  return copy == R_NilValue ? view_memory(x) : copy_pointer(copy);
}

static R_xlen_t view_Length(SEXP x) {
  return view_length(x);
}

static Rboolean view_Inspect(SEXP x, int pre, int deep, int pvec,
                             void (*inspect_subtree)(SEXP, int, int, int)) {
  Rprintf(" r8 view (len=%lld, %s)\n", (long long) view_length(x),
          R_altrep_data2(x) == R_NilValue ? "in place" : "copied");
  return TRUE;
}

static void *view_Dataptr(SEXP x, Rboolean writeable) {
  if (writeable) {
    return copy_pointer(view_copy(x));
  }
  // R reads through a pointer it does not write; the cast drops const for
  // the ALTREP signature only.
  return (void *) view_elements(x);
}

static const void *view_Dataptr_or_null(SEXP x) {
  return view_elements(x);
}

static R_xlen_t view_Get_region(SEXP x, R_xlen_t i, R_xlen_t n, void *buf) {
  R_xlen_t len = view_length(x);
  R_xlen_t k = i < len ? (len - i < n ? len - i : n) : 0;
  size_t w = view_width(x);
  if (k > 0) {
    memcpy(buf, (const char *) view_elements(x) + (size_t) i * w, (size_t) k * w);
  }
  return k;
}

static R_xlen_t view_real_Get_region(SEXP x, R_xlen_t i, R_xlen_t n, double *buf) {
  return view_Get_region(x, i, n, buf);
}

static R_xlen_t view_int_Get_region(SEXP x, R_xlen_t i, R_xlen_t n, int *buf) {
  return view_Get_region(x, i, n, buf);
}

static R_xlen_t view_raw_Get_region(SEXP x, R_xlen_t i, R_xlen_t n, Rbyte *buf) {
  return view_Get_region(x, i, n, buf);
}

static double view_real_Elt(SEXP x, R_xlen_t i) {
  return ((const double *) view_elements(x))[i];
}

static int view_int_Elt(SEXP x, R_xlen_t i) {
  return ((const int *) view_elements(x))[i];
}

static Rbyte view_raw_Elt(SEXP x, R_xlen_t i) {
  return ((const Rbyte *) view_elements(x))[i];
}

static void init_common(R_altrep_class_t cls) {
  R_set_altrep_Length_method(cls, view_Length);
  R_set_altrep_Inspect_method(cls, view_Inspect);
  R_set_altvec_Dataptr_method(cls, view_Dataptr);
  R_set_altvec_Dataptr_or_null_method(cls, view_Dataptr_or_null);
}

void r8_init_views(DllInfo *dll) {
  view_double = R_make_altreal_class("r8_view_double", "R8", dll);
  init_common(view_double);
  R_set_altreal_Elt_method(view_double, view_real_Elt);
  R_set_altreal_Get_region_method(view_double, view_real_Get_region);

  view_integer = R_make_altinteger_class("r8_view_integer", "R8", dll);
  init_common(view_integer);
  R_set_altinteger_Elt_method(view_integer, view_int_Elt);
  R_set_altinteger_Get_region_method(view_integer, view_int_Get_region);

  view_logical = R_make_altlogical_class("r8_view_logical", "R8", dll);
  init_common(view_logical);
  R_set_altlogical_Elt_method(view_logical, view_int_Elt);
  R_set_altlogical_Get_region_method(view_logical, view_int_Get_region);

  view_raw = R_make_altraw_class("r8_view_raw", "R8", dll);
  init_common(view_raw);
  R_set_altraw_Elt_method(view_raw, view_raw_Elt);
  R_set_altraw_Get_region_method(view_raw, view_raw_Get_region);
}

// Whether `x` is a view made by R8Api.
int r8_is_view(SEXP x) {
  return ALTREP(x) &&
         (R_altrep_inherits(x, view_double) || R_altrep_inherits(x, view_integer) ||
          R_altrep_inherits(x, view_logical) || R_altrep_inherits(x, view_raw));
}

static void set_error(R8Error *err, const char *code, const char *message) {
  if (err != NULL) {
    snprintf(err->code, sizeof(err->code), "%s", code);
    snprintf(err->message, sizeof(err->message), "%s", message);
  }
}

static R8Status make_view(R_altrep_class_t cls, SEXP owner, const void *data,
                          size_t n, SEXP *value, R8Error *err) {
  if (value == NULL) {
    set_error(err, "invalid_argument", "`value` is NULL");
    return R8_ERROR;
  }
  if (data == NULL && n > 0) {
    set_error(err, "invalid_argument", "`data` is NULL and `n` is positive");
    return R8_ERROR;
  }
  if ((double) n > (double) R_XLEN_T_MAX) {
    set_error(err, "too_long", "`n` exceeds the length of an R vector");
    return R8_ERROR;
  }
  SEXP len = PROTECT(Rf_ScalarReal((double) n));
  SEXP ptr = PROTECT(R_MakeExternalPtr((void *) data, len, owner));
  *value = R_new_altrep(cls, ptr, R_NilValue);
  UNPROTECT(2);
  return R8_OK;
}

static R8Status view_doubles(SEXP owner, const double *data, size_t n, SEXP *value,
                             R8Error *err) {
  return make_view(view_double, owner, data, n, value, err);
}

static R8Status view_integers(SEXP owner, const int32_t *data, size_t n, SEXP *value,
                              R8Error *err) {
  return make_view(view_integer, owner, data, n, value, err);
}

static R8Status view_logicals(SEXP owner, const int32_t *data, size_t n, SEXP *value,
                              R8Error *err) {
  return make_view(view_logical, owner, data, n, value, err);
}

static R8Status view_bytes(SEXP owner, const uint8_t *data, size_t n, SEXP *value,
                           R8Error *err) {
  return make_view(view_raw, owner, data, n, value, err);
}

// The value functions of R8Api, implemented in the Rust code (values of
// Rust-native types).
R8Status r8_api_new_value(const char *type_name, void *state, SEXP *value, R8Error *err);
R8Status r8_api_value_state(SEXP value, const char *type_name, const void **state,
                            R8Error *err);

static const R8Api api = {
    R8_ABI_VERSION, sizeof(R8Api), r8_api_new_value, r8_api_value_state,
    view_doubles,   view_integers, view_logicals,    view_bytes,
};

static const R8Api *r8_api(uint32_t abi_version) {
  return abi_version == R8_ABI_VERSION ? &api : NULL;
}

void r8_register_api(void) {
  R_RegisterCCallable("R8", "r8_api", (DL_FUNC) r8_api);
}
