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

static R8Status magnitude_impl(const R8CallContext *ctx, const SEXP *args, size_t nargs,
                          SEXP *result, R8Error *err) {
  (void) ctx;
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

static R8Status ratio_impl(const R8CallContext *ctx, const SEXP *args, size_t nargs,
                           SEXP *result, R8Error *err) {
  (void) ctx;
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
static R8Status format_impl(const R8CallContext *ctx, const SEXP *args, size_t nargs,
                            SEXP *result, R8Error *err) {
  (void) ctx;
  (void) nargs;
  (void) err;
  char buf[96];
  snprintf(buf, sizeof(buf), "Point(%g, %g)", field(args[0], 0), field(args[0], 1));
  *result = Rf_mkString(buf);
  return R8_OK;
}

// pkgc::Signal and pkgc::Resampler are declared in R (R/types.R) and here,
// identically, since a codec needs a declaration in the exports' IR
// document.
static const char ir_json[] =
    "{\"$ir\":1,\"namespace\":\"pkgc\","
    "\"types\":[{\"kind\":\"struct\",\"name\":\"Signal\",\"description\":\"\",\"version\":1,"
    "\"fields\":[{\"name\":\"name\",\"type\":{\"kind\":\"character\"},\"description\":\"\"},"
    "{\"name\":\"values\",\"type\":{\"kind\":\"vec\",\"element\":{\"kind\":\"double\"}},"
    "\"description\":\"\"}]},"
    "{\"kind\":\"enum\",\"name\":\"Resampler\",\"description\":\"\",\"tag\":\"type\",\"version\":1,"
    "\"variants\":[{\"name\":\"KFold\",\"description\":\"\",\"shape\":\"struct\","
    "\"fields\":[{\"name\":\"k\",\"type\":{\"kind\":\"integer\"},\"description\":\"\"}]},"
    "{\"name\":\"Loocv\",\"description\":\"\",\"shape\":\"unit\"}]}],"
    "\"impls\":["
    "{\"trait\":\"pkgc::Norm\",\"target\":{\"kind\":\"type\",\"name\":\"pkgc::Point\"},"
    "\"methods\":[\"magnitude\",\"ratio\"]},"
    "{\"trait\":\"R8::Display\",\"target\":{\"kind\":\"type\",\"name\":\"pkgc::Point\"},"
    "\"methods\":[\"format\"]}]}";

static const R8ImplEntry impls[] = {
    {"pkgc::Norm", "pkgc::Point", "magnitude", magnitude_impl, 1},
    {"pkgc::Norm", "pkgc::Point", "ratio", ratio_impl, 2},
    {"R8::Display", "pkgc::Point", "format", format_impl, 2},
};

// The codecs of pkgc::Signal (signal.c) and pkgc::Resampler (resampler.c),
// copied into one array at initialization, since R8 reads the codec tables
// as an array.
extern const R8TypeVTable pkgc_signal_codec;
extern const R8TypeVTable pkgc_resampler_codec;
static R8TypeVTable codecs[2];
SEXP pkgc_bad_exports(void);
SEXP pkgc_live_signals(void);
SEXP pkgc_signal_peek(SEXP x);
SEXP pkgc_signal_make(SEXP name, SEXP n);

static const R8Exports exports = {
    R8_ABI_VERSION, sizeof(R8Exports), "r8pkgC", ir_json, sizeof(ir_json) - 1,
    codecs,               2,           impls,    sizeof(impls) / sizeof(impls[0]),
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
    {"pkgc_protect_status", (DL_FUNC) &pkgc_protect_status, 0},
    {"pkgc_bad_exports", (DL_FUNC) &pkgc_bad_exports, 0},
    {NULL, NULL, 0},
};

void R_init_r8pkgC(DllInfo *dll) {
  codecs[0] = pkgc_signal_codec;
  codecs[1] = pkgc_resampler_codec;
  R_registerRoutines(dll, NULL, call_methods, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_RegisterCCallable("r8pkgC", "r8_exports", (DL_FUNC) r8_exports);
}
