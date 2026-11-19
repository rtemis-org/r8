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

static R8Status ratio_impl(const R8CallContext *ctx, const SEXP *args, size_t nargs,
                           SEXP *result, R8Error *err) {
  (void) ctx;
  if (nargs != 2 || TYPEOF(args[1]) != REALSXP || XLENGTH(args[1]) != 1) {
    set_error(err, "arity", "ratio() takes a Point and one number");
    return R8_ERROR;
  }
  double by = REAL(args[1])[0];
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

static const char ir_json[] =
    "{\"$ir\":1,\"namespace\":\"pkgc\",\"impls\":["
    "{\"trait\":\"pkgc::Norm\",\"target\":{\"kind\":\"type\",\"name\":\"pkgc::Point\"},"
    "\"methods\":[\"magnitude\",\"ratio\"]},"
    "{\"trait\":\"R8::Display\",\"target\":{\"kind\":\"type\",\"name\":\"pkgc::Point\"},"
    "\"methods\":[\"format\"]}]}";

static const R8ImplEntry impls[] = {
    {"pkgc::Norm", "pkgc::Point", "magnitude", magnitude_impl},
    {"pkgc::Norm", "pkgc::Point", "ratio", ratio_impl},
    {"R8::Display", "pkgc::Point", "format", format_impl},
};

static const R8Exports exports = {
    R8_ABI_VERSION, sizeof(R8Exports), "r8pkgC", ir_json, sizeof(ir_json) - 1,
    NULL,           0,                 impls,    sizeof(impls) / sizeof(impls[0]),
};

static SEXP r8_exports(void) {
  return R_MakeExternalPtr((void *) &exports, Rf_install("r8_exports"), R_NilValue);
}

static const R_CallMethodDef call_methods[] = {
    {"r8_exports", (DL_FUNC) &r8_exports, 0},
    {NULL, NULL, 0},
};

void R_init_r8pkgC(DllInfo *dll) {
  R_registerRoutines(dll, NULL, call_methods, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
