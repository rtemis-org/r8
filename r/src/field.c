// field.c
// ::R8::
// 2026- EDG rtemis.org

// What a read of a field gives. A value shares its vectors with the code that
// reads them, and R copies a shared vector before it changes it; data.table's
// functions change one in place. A read therefore gives a vector as a view of the
// value's own: an ALTREP vector without data, whose pointer is that of the vector
// it views, so that reading copies nothing. R duplicates a view before it changes
// it, since a view is marked not mutable; data.table's setDT() copies an ALTREP
// column, and setattr() changes the attributes of the view. A small vector is
// copied at the read, which takes less than a view.
//
// data1 of a view is the vector that it views.
// spec: R8/class-system#values-in-r

#define R8_INTERNAL_ABI
#include <R8.h>
#include <R_ext/Altrep.h>

static R_altrep_class_t field_real, field_integer, field_logical, field_raw, field_complex,
    field_string;

static SEXP viewed(SEXP x) { return R_altrep_data1(x); }

static R_xlen_t field_Length(SEXP x) { return XLENGTH(viewed(x)); }

// Code asks for a writable pointer to read, too: REAL() is one, and so do base R's
// order() and lm.fit(). A copy for each would be one that nothing needs. Code that
// writes through the pointer changes a vector that R code shares, as it would
// without the view; set() of a data frame is such code.
static void *field_Dataptr(SEXP x, Rboolean writeable) {
  (void) writeable;
  return (void *) DATAPTR_RO(viewed(x));
}

static const void *field_Dataptr_or_null(SEXP x) { return DATAPTR_OR_NULL(viewed(x)); }

static Rboolean field_Inspect(SEXP x, int pre, int deep, int pvec,
                              void (*inspect_subtree)(SEXP, int, int, int)) {
  (void) pre;
  (void) deep;
  (void) pvec;
  (void) inspect_subtree;
  Rprintf(" r8 field (len=%lld)\n", (long long) XLENGTH(viewed(x)));
  return TRUE;
}

static double field_real_Elt(SEXP x, R_xlen_t i) { return REAL_ELT(viewed(x), i); }
static int field_int_Elt(SEXP x, R_xlen_t i) { return INTEGER_ELT(viewed(x), i); }
static int field_lgl_Elt(SEXP x, R_xlen_t i) { return LOGICAL_ELT(viewed(x), i); }
static Rbyte field_raw_Elt(SEXP x, R_xlen_t i) { return RAW_ELT(viewed(x), i); }
static Rcomplex field_complex_Elt(SEXP x, R_xlen_t i) { return COMPLEX_ELT(viewed(x), i); }
static SEXP field_string_Elt(SEXP x, R_xlen_t i) { return STRING_ELT(viewed(x), i); }

static void field_string_Set_elt(SEXP x, R_xlen_t i, SEXP v) {
  (void) x;
  (void) i;
  (void) v;
  Rf_error("a field of an R8 value is not changed in place; change a copy of it");
}

static R_xlen_t field_real_region(SEXP x, R_xlen_t i, R_xlen_t n, double *buf) {
  return REAL_GET_REGION(viewed(x), i, n, buf);
}
static R_xlen_t field_int_region(SEXP x, R_xlen_t i, R_xlen_t n, int *buf) {
  return INTEGER_GET_REGION(viewed(x), i, n, buf);
}
static R_xlen_t field_lgl_region(SEXP x, R_xlen_t i, R_xlen_t n, int *buf) {
  return LOGICAL_GET_REGION(viewed(x), i, n, buf);
}
static R_xlen_t field_raw_region(SEXP x, R_xlen_t i, R_xlen_t n, Rbyte *buf) {
  return RAW_GET_REGION(viewed(x), i, n, buf);
}

static int field_real_sorted(SEXP x) { return REAL_IS_SORTED(viewed(x)); }
static int field_real_no_na(SEXP x) { return REAL_NO_NA(viewed(x)); }
static int field_int_sorted(SEXP x) { return INTEGER_IS_SORTED(viewed(x)); }
static int field_int_no_na(SEXP x) { return INTEGER_NO_NA(viewed(x)); }
static int field_lgl_no_na(SEXP x) { return LOGICAL_NO_NA(viewed(x)); }
static int field_string_sorted(SEXP x) { return STRING_IS_SORTED(viewed(x)); }
static int field_string_no_na(SEXP x) { return STRING_NO_NA(viewed(x)); }

static void init_common(R_altrep_class_t cls) {
  R_set_altrep_Length_method(cls, field_Length);
  R_set_altrep_Inspect_method(cls, field_Inspect);
  R_set_altvec_Dataptr_method(cls, field_Dataptr);
  R_set_altvec_Dataptr_or_null_method(cls, field_Dataptr_or_null);
}

void r8_init_fields(DllInfo *dll) {
  field_real = R_make_altreal_class("r8_field_double", "R8", dll);
  init_common(field_real);
  R_set_altreal_Elt_method(field_real, field_real_Elt);
  R_set_altreal_Get_region_method(field_real, field_real_region);
  R_set_altreal_Is_sorted_method(field_real, field_real_sorted);
  R_set_altreal_No_NA_method(field_real, field_real_no_na);

  field_integer = R_make_altinteger_class("r8_field_integer", "R8", dll);
  init_common(field_integer);
  R_set_altinteger_Elt_method(field_integer, field_int_Elt);
  R_set_altinteger_Get_region_method(field_integer, field_int_region);
  R_set_altinteger_Is_sorted_method(field_integer, field_int_sorted);
  R_set_altinteger_No_NA_method(field_integer, field_int_no_na);

  field_logical = R_make_altlogical_class("r8_field_logical", "R8", dll);
  init_common(field_logical);
  R_set_altlogical_Elt_method(field_logical, field_lgl_Elt);
  R_set_altlogical_Get_region_method(field_logical, field_lgl_region);
  R_set_altlogical_No_NA_method(field_logical, field_lgl_no_na);

  field_raw = R_make_altraw_class("r8_field_raw", "R8", dll);
  init_common(field_raw);
  R_set_altraw_Elt_method(field_raw, field_raw_Elt);
  R_set_altraw_Get_region_method(field_raw, field_raw_region);

  field_complex = R_make_altcomplex_class("r8_field_complex", "R8", dll);
  init_common(field_complex);
  R_set_altcomplex_Elt_method(field_complex, field_complex_Elt);

  field_string = R_make_altstring_class("r8_field_character", "R8", dll);
  init_common(field_string);
  R_set_altstring_Elt_method(field_string, field_string_Elt);
  R_set_altstring_Set_elt_method(field_string, field_string_Set_elt);
  R_set_altstring_Is_sorted_method(field_string, field_string_sorted);
  R_set_altstring_No_NA_method(field_string, field_string_no_na);
}

SEXP r8_view_again(SEXP x);

// The bytes up to which a copy takes less than a view: R's small vectors.
#define SMALL_BYTES 128

// The atomic vector `v` of a value as a read gives it.
static SEXP vector_out(SEXP v) {
  R_altrep_class_t cls;
  size_t width;
  switch (TYPEOF(v)) {
  case REALSXP:
    cls = field_real, width = sizeof(double);
    break;
  case INTSXP:
    cls = field_integer, width = sizeof(int);
    break;
  case LGLSXP:
    cls = field_logical, width = sizeof(int);
    break;
  case RAWSXP:
    cls = field_raw, width = 1;
    break;
  case CPLXSXP:
    cls = field_complex, width = sizeof(Rcomplex);
    break;
  case STRSXP:
    cls = field_string, width = sizeof(SEXP);
    break;
  default:
    return v;
  }
  if ((size_t) XLENGTH(v) * width <= SMALL_BYTES) {
    return Rf_duplicate(v);
  }
  // Memory that a view reads in place belongs to Rust, which a write would
  // change under it.
  SEXP again = r8_view_again(v);
  if (again != R_NilValue) {
    return again;
  }
  SEXP x = PROTECT(R_new_altrep(cls, v, R_NilValue));
  SHALLOW_DUPLICATE_ATTRIB(x, v);
  MARK_NOT_MUTABLE(x);
  UNPROTECT(1);
  return x;
}

SEXP r8_value_id(SEXP x);

// The field `v` of a value as a read gives it: a vector, a table over such
// columns, or the object itself.
SEXP r8_field_out(SEXP v) {
  if (TYPEOF(v) != VECSXP) {
    return vector_out(v);
  }
  // A table is stored as a data frame: a data.table is an object of
  // host_class(), which the value holds by reference.
  if (ALTREP(v) || !Rf_inherits(v, "data.frame") || Rf_inherits(v, "data.table")) {
    return v;
  }
  R_xlen_t n = XLENGTH(v);
  SEXP out = PROTECT(Rf_allocVector(VECSXP, n));
  for (R_xlen_t i = 0; i < n; i++) {
    SET_VECTOR_ELT(out, i, vector_out(VECTOR_ELT(v, i)));
  }
  // set() and setnames() change the vector of names in place.
  DUPLICATE_ATTRIB(out, v);
  UNPROTECT(1);
  return out;
}

R_xlen_t r8_attrib_count(SEXP x);

// The vector that `x` views, when `x` is a view without attributes of a vector
// without attributes, so that a value made from a read holds the vector itself.
SEXP r8_field_viewed(SEXP x) {
  while (ALTREP(x) && (R_altrep_inherits(x, field_real) || R_altrep_inherits(x, field_integer) ||
                       R_altrep_inherits(x, field_logical) || R_altrep_inherits(x, field_raw) ||
                       R_altrep_inherits(x, field_complex) || R_altrep_inherits(x, field_string))) {
    SEXP v = viewed(x);
    if (r8_attrib_count(x) != 0 || r8_attrib_count(v) != 0) {
      break;
    }
    x = v;
  }
  return x;
}

// The field `name` of the value `x`, NULL for an absent one and for another name.
SEXP r8_c_get(SEXP x, SEXP name) {
  SEXP names = Rf_getAttrib(x, R_NamesSymbol);
  if (TYPEOF(x) != VECSXP || TYPEOF(names) != STRSXP || TYPEOF(name) != STRSXP ||
      XLENGTH(name) != 1) {
    return R_NilValue;
  }
  SEXP s = STRING_ELT(name, 0);
  R_xlen_t n = XLENGTH(names);
  for (R_xlen_t i = 0; i < n; i++) {
    if (STRING_ELT(names, i) == s) {
      return r8_field_out(VECTOR_ELT(x, i));
    }
  }
  return R_NilValue;
}

// The fields of the value `x` at the positions `at`, or all of them for NULL, as a
// named list.
SEXP r8_c_fields(SEXP x, SEXP at) {
  SEXP names = Rf_getAttrib(x, R_NamesSymbol);
  if (TYPEOF(x) != VECSXP || (at != R_NilValue && TYPEOF(at) != INTSXP)) {
    Rf_error("the fields of a value are read by position");
  }
  R_xlen_t n = at == R_NilValue ? XLENGTH(x) : XLENGTH(at);
  SEXP out = PROTECT(Rf_allocVector(VECSXP, n));
  SEXP to = PROTECT(Rf_allocVector(STRSXP, n));
  for (R_xlen_t i = 0; i < n; i++) {
    R_xlen_t k = at == R_NilValue ? i : INTEGER_RO(at)[i] - 1;
    if (k < 0 || k >= XLENGTH(x)) {
      Rf_error("the fields of a value are read by position");
    }
    SET_VECTOR_ELT(out, i, r8_field_out(VECTOR_ELT(x, k)));
    if (TYPEOF(names) == STRSXP && k < XLENGTH(names)) {
      SET_STRING_ELT(to, i, STRING_ELT(names, k));
    }
  }
  Rf_setAttrib(out, R_NamesSymbol, to);
  UNPROTECT(2);
  return out;
}
