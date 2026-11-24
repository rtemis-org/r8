// exports.c
// ::r8pkgC::
// 2026- EDG rtemis.org

// Impls of pkgc::Norm and R8::Display for pkgc::Point in C, exported to R8
// through the R8Exports table of R8.h.

#include <math.h>
#include <stdio.h>

#include <R8.h>

static void set_error(R8Error *err, const char *code, const char *message) {
  snprintf(err->code, sizeof(err->code), "%s", code);
  snprintf(err->message, sizeof(err->message), "%s", message);
}

// A Point value is a list of its fields in declaration order: x, y.
static double field(SEXP point, int i) {
  return REAL(VECTOR_ELT(point, i))[0];
}

static R8Status magnitude_impl(const R8CallInputs *inputs, const SEXP *args, size_t nargs,
                          SEXP *result, R8Error *err) {
  (void) inputs;
  if (nargs != 1) {
    set_error(err, "arity", "magnitude() takes one argument");
    return R8_ERROR;
  }
  double x = field(args[0], 0), y = field(args[0], 1);
  *result = Rf_ScalarReal(sqrt(x * x + y * y));
  return R8_OK;
}

// Raises an R error, which R8Api.protect catches.
static SEXP raise_na(void *data) {
  (void) data;
  Rf_error("`by` is NA");
  return R_NilValue;
}

static R8Status ratio_impl(const R8CallInputs *inputs, const SEXP *args, size_t nargs,
                           SEXP *result, R8Error *err) {
  (void) inputs;
  if (nargs != 2 || TYPEOF(args[1]) != REALSXP || XLENGTH(args[1]) != 1) {
    set_error(err, "arity", "ratio() takes a Point and one number");
    return R8_ERROR;
  }
  double by = REAL(args[1])[0];
  // An R error raised under R8Api.protect returns R8_UNWIND here, which R8
  // continues once this function returns.
  if (ISNA(by)) {
    return R8_api()->protect(raise_na, NULL, result, err);
  }
  if (by == 0) {
    set_error(err, "zero_divisor", "`by` is not 0");
    return R8_ERROR;
  }
  *result = Rf_ScalarReal(field(args[0], 0) / by);
  return R8_OK;
}

// Display::format(x, ...): the ... arrive as one list.
static R8Status format_impl(const R8CallInputs *inputs, const SEXP *args, size_t nargs,
                            SEXP *result, R8Error *err) {
  (void) inputs;
  (void) nargs;
  (void) err;
  char buf[96];
  snprintf(buf, sizeof(buf), "Point(%g, %g)", field(args[0], 0), field(args[0], 1));
  *result = Rf_mkString(buf);
  return R8_OK;
}

// Norm for a double vector, a class of R.
static R8Status vector_magnitude(const R8CallInputs *inputs, const SEXP *args, size_t nargs,
                                 SEXP *result, R8Error *err) {
  (void) inputs;
  (void) nargs;
  (void) err;
  double sum = 0;
  const double *x = REAL(args[0]);
  for (R_xlen_t i = 0; i < XLENGTH(args[0]); i++) {
    sum += x[i] * x[i];
  }
  *result = Rf_ScalarReal(sqrt(sum));
  return R8_OK;
}

static R8Status vector_ratio(const R8CallInputs *inputs, const SEXP *args, size_t nargs,
                             SEXP *result, R8Error *err) {
  (void) inputs;
  (void) nargs;
  (void) err;
  *result = Rf_ScalarReal(REAL(args[0])[0] / Rf_asReal(args[1]));
  return R8_OK;
}

// The impls of pkgc::Point and of R's class "numeric", whose declarations R8
// builds from these entries; the types are declared in R (R/types.R).
static const R8Position point = {R8_POSITION_TYPE, "pkgc::Point", NULL, NULL};

static const R8Position numeric = {R8_POSITION_CLASS, "numeric", NULL, "implicit"};

static const R8ImplEntry impls[] = {
    {"pkgc::Norm", &point, 1, "magnitude", magnitude_impl, 1},
    {"pkgc::Norm", &point, 1, "ratio", ratio_impl, 2},
    {"R8::Display", &point, 1, "format", format_impl, 2},
    {"pkgc::Norm", &numeric, 1, "magnitude", vector_magnitude, 1},
    {"pkgc::Norm", &numeric, 1, "ratio", vector_ratio, 2},
};

// The codecs of pkgc::Signal (signal.c) and pkgc::Resampler (resampler.c),
// copied into one array at initialization, since R8 reads the codec tables
// as an array.
extern const R8Codec pkgc_signal_codec;
extern const R8Codec pkgc_resampler_codec;
static R8Codec codecs[2];
SEXP pkgc_bad_exports(SEXP kind);
SEXP pkgc_live_signals(void);
SEXP pkgc_signal_peek(SEXP x);
SEXP pkgc_signal_make(SEXP name, SEXP n);
SEXP pkgc_lend(SEXP x, SEXP type, SEXP upcast);
SEXP pkgc_wrap(SEXP type);

// The value that R8Api.value_of_fields builds, or the code it reports.
static SEXP pkgc_of_fields(SEXP type, SEXP variant, SEXP hash, SEXP fields) {
  const R8Api *api = R8_api();
  R8Error err = {{0}, {0}};
  SEXP value = R_NilValue;
  // R8 takes the list.
  SEXP list = PROTECT(Rf_shallow_duplicate(fields));
  R8Status status = api->value_of_fields(
      CHAR(STRING_ELT(type, 0)), variant == R_NilValue ? NULL : CHAR(STRING_ELT(variant, 0)),
      CHAR(STRING_ELT(hash, 0)), list, &value, &err);
  UNPROTECT(1);
  return status == R8_OK ? value : Rf_mkString(err.code);
}

// The value that R8Api.with_fields builds, or the report it fills.
static SEXP pkgc_with_fields(SEXP x, SEXP fields) {
  const R8Api *api = R8_api();
  R8Error err = {{0}, {0}};
  SEXP value = R_NilValue;
  if (api->with_fields(x, fields, &value, &err) == R8_OK) {
    return value;
  }
  SEXP out = PROTECT(Rf_allocVector(STRSXP, 2));
  SET_STRING_ELT(out, 0, Rf_mkChar(err.code));
  SET_STRING_ELT(out, 1, Rf_mkChar(err.message));
  UNPROTECT(1);
  return out;
}

// Impl entries that registration refuses, by `kind`: 1 repeats a method, 2 has
// a NULL function, 3 omits a method, 4 has no position, 5 has a position of an
// unknown kind, 6 a class of an unknown system, 7 an unknown method, 8
// another number of parameters, and tables of an earlier header: 9, whose
// table is smaller, and 10, whose entries are.
static R8ImplEntry bad_rows[2];
static R8Exports bad_impl_exports;

static SEXP pkgc_bad_impls(SEXP kind) {
  static const R8Position unknown = {9, "pkgc::Point", NULL, NULL};
  static const R8Position system = {R8_POSITION_CLASS, "lm", NULL, "s5"};
  int k = Rf_asInteger(kind);
  bad_rows[0] = impls[0];
  bad_rows[1] = impls[k == 1 || k == 3 ? 0 : 1];
  if (k == 2) {
    bad_rows[1].call = NULL;
  } else if (k == 4) {
    bad_rows[1].n_positions = 0;
  } else if (k == 5 || k == 6) {
    bad_rows[1].positions = k == 5 ? &unknown : &system;
  } else if (k == 7) {
    bad_rows[1].method = "norm";
  } else if (k == 8) {
    bad_rows[1].n_params = 3;
  }
  bad_impl_exports = (R8Exports) {
      .abi_version = R8_ABI_VERSION,
      .size = k == 9 ? (uint32_t) offsetof(R8Exports, impl_size) : sizeof(R8Exports),
      .package = "r8pkgC",
      .impls = bad_rows,
      .n_impls = k == 3 ? 1 : 2,
      .impl_size = k == 10 ? offsetof(R8ImplEntry, signature) : sizeof(R8ImplEntry),
  };
  return R_MakeExternalPtr(&bad_impl_exports, Rf_install("r8_exports"), R_NilValue);
}

static const R8Exports exports = {
    .abi_version = R8_ABI_VERSION,
    .size = sizeof(R8Exports),
    .package = "r8pkgC",
    .types = codecs,
    .n_types = 2,
    .impls = impls,
    .n_impls = sizeof(impls) / sizeof(impls[0]),
    .impl_size = sizeof(R8ImplEntry),
};

// The status R8Api.protect returns for a callback that raises an R error,
// R8_UNWIND, without the jump.
static SEXP pkgc_protect_status(void) {
  SEXP value = R_NilValue;
  R8Error err = {{0}, {0}};
  return Rf_ScalarInteger(R8_api()->protect(raise_na, NULL, &value, &err));
}

// The exports table, which R8 fetches with R_GetCCallable("r8pkgC",
// "r8_exports").
static const R8Exports *r8_exports(void) {
  return &exports;
}

static const R_CallMethodDef call_methods[] = {
    {"pkgc_live_signals", (DL_FUNC) &pkgc_live_signals, 0},
    {"pkgc_signal_peek", (DL_FUNC) &pkgc_signal_peek, 1},
    {"pkgc_signal_make", (DL_FUNC) &pkgc_signal_make, 2},
    {"pkgc_lend", (DL_FUNC) &pkgc_lend, 3},
    {"pkgc_wrap", (DL_FUNC) &pkgc_wrap, 1},
    {"pkgc_of_fields", (DL_FUNC) &pkgc_of_fields, 4},
    {"pkgc_with_fields", (DL_FUNC) &pkgc_with_fields, 2},
    {"pkgc_protect_status", (DL_FUNC) &pkgc_protect_status, 0},
    {"pkgc_bad_exports", (DL_FUNC) &pkgc_bad_exports, 1},
    {"pkgc_bad_impls", (DL_FUNC) &pkgc_bad_impls, 1},
    {NULL, NULL, 0},
};

void R_init_r8pkgC(DllInfo *dll) {
  codecs[0] = pkgc_signal_codec;
  codecs[1] = pkgc_resampler_codec;
  R_registerRoutines(dll, NULL, call_methods, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_RegisterCCallable("r8pkgC", "r8_exports", (DL_FUNC) r8_exports);
}
