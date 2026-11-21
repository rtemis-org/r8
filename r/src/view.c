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

#include <setjmp.h>

// A jump buffer that leaves the signal mask alone: sigsetjmp(buf, 0) saves
// no mask, where setjmp() on macOS and the BSDs saves it with a system call
// (216 ns against 2 ns per call on macOS, 2026-11-21). R's own contexts jump
// this way. Windows has no sigsetjmp(), and its setjmp() saves no mask.
#ifdef _WIN32
#define R8_JMP_BUF jmp_buf
#define R8_SETJMP(b) setjmp(b)
#define R8_LONGJMP(b, v) longjmp(b, v)
#else
#define R8_JMP_BUF sigjmp_buf
#define R8_SETJMP(b) sigsetjmp(b, 0)
#define R8_LONGJMP(b, v) siglongjmp(b, v)
#endif
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

// R8Api.protect: runs a callback under R_UnwindProtect, whose cleanup
// jumps back here when R jumps, so the callback's caller sees R8_UNWIND
// instead of the jump. The continuation token is kept for r8_invoke(), which
// continues the jump once the impl function has returned.
static SEXP continuation = NULL;

typedef struct Protected {
  SEXP (*fn)(void *);
  void *data;
  R8_JMP_BUF jump;
} Protected;

static SEXP protected_body(void *p) {
  Protected *q = p;
  return q->fn(q->data);
}

static void protected_cleanup(void *p, Rboolean jump) {
  if (jump) {
    R8_LONGJMP(((Protected *) p)->jump, 1);
  }
}

static R8Status api_protect(SEXP (*fn)(void *data), void *data, SEXP *value,
                            R8Error *err) {
  if (fn == NULL || value == NULL) {
    set_error(err, "invalid_argument", "`fn` or `value` is NULL");
    return R8_ERROR;
  }
  Protected p;
  p.fn = fn;
  p.data = data;
  if (R8_SETJMP(p.jump)) {
    set_error(err, "unwind", "R raised a condition inside R8Api.protect");
    return R8_UNWIND;
  }
  *value = R_UnwindProtect(protected_body, &p, protected_cleanup, &p, continuation);
  // R_UnwindProtect() keeps the result in the token, which R8 preserves;
  // clearing it leaves the result to the caller.
  SETCAR(continuation, R_NilValue);
  return R8_OK;
}

// Arguments of a view's construction inside api_protect().
typedef struct ViewArgs {
  R_altrep_class_t cls;
  SEXP owner;
  const void *data;
  size_t n;
} ViewArgs;

static SEXP view_body(void *p) {
  ViewArgs *a = p;
  SEXP len = PROTECT(Rf_ScalarReal((double) a->n));
  SEXP ptr = PROTECT(R_MakeExternalPtr((void *) a->data, len, a->owner));
  SEXP ans = R_new_altrep(a->cls, ptr, R_NilValue);
  UNPROTECT(2);
  return ans;
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
  // The allocations run under api_protect(), so a failed allocation returns an
  // error and the API keeps its rule that a callee never jumps.
  ViewArgs a = {cls, owner, data, n};
  R8Status status = api_protect(view_body, &a, value, err);
  if (status == R8_UNWIND) {
    set_error(err, "r_error", "R could not allocate the view");
    return R8_ERROR;
  }
  return status;
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
    R8_ABI_VERSION, sizeof(R8Api), r8_api_new_value, r8_api_value_state, view_doubles,
    view_integers,  view_logicals, view_bytes,       api_protect,
};

static const R8Api *r8_api(uint32_t abi_version) {
  return abi_version == R8_ABI_VERSION ? &api : NULL;
}

void r8_register_api(void) {
  continuation = R_MakeUnwindCont();
  R_PreserveObject(continuation);
  R_RegisterCCallable("R8", "r8_api", (DL_FUNC) r8_api);
}

// R8Api.protect for R8's own Rust code (native.rs), whose callers convert
// R8_UNWIND into R8_ERROR, since R8's API functions never jump.
R8Status r8_protect(SEXP (*fn)(void *data), void *data, SEXP *value, R8Error *err) {
  return api_protect(fn, data, value, err);
}

// Calls the impl function at the address of `fn` with the elements of the
// list `args`; R8_ERROR raises an R error of the impl's message and code,
// and R8_UNWIND continues the jump that R8Api.protect caught. R calls it
// through `.Call()` on the native symbol of r8_invoke_symbol(), so the jump
// continues from a C frame.
static SEXP r8_invoke(SEXP fn, SEXP args) {
  R8MethodFn call = (R8MethodFn) R_ExternalPtrAddrFn(fn);
  if (call == NULL) {
    Rf_error("the package of this compiled method is unloaded; load it again");
  }
  R_xlen_t n = XLENGTH(args);
  const SEXP *argv = n > 0 ? (const SEXP *) R_alloc((size_t) n, sizeof(SEXP)) : NULL;
  for (R_xlen_t i = 0; i < n; i++) {
    ((SEXP *) argv)[i] = VECTOR_ELT(args, i);
  }
  R8CallContext ctx = {sizeof(R8CallContext), 0, 0};
  SEXP result = R_NilValue;
  R8Error err = {{0}, {0}};
  R8Status status = call(&ctx, argv, (size_t) n, &result, &err);
  if (status == R8_OK) {
    return result;
  }
  if (status == R8_UNWIND) {
    R_ContinueUnwind(continuation);
  }
  if (status == R8_ERROR) {
    Rf_error("%s [%s]", err.message, err.code);
  }
  Rf_error("the impl function returned the status %d, none of R8_OK, R8_ERROR, and R8_UNWIND",
           (int) status);
  return R_NilValue;
}

// The exports table of the package `pkg`, which it registered with
// R_RegisterCCallable(pkg, "r8_exports", fn), as an external pointer tagged
// `r8_exports`. R_GetCCallable() raises an error for a package that
// registered none; register_exports() reads that as no exports.
static SEXP r8_package_exports(SEXP pkg) {
  if (TYPEOF(pkg) != STRSXP || XLENGTH(pkg) != 1) {
    Rf_error("`pkg` is one package name");
  }
  R8ExportsFn fn = (R8ExportsFn) R_GetCCallable(CHAR(STRING_ELT(pkg, 0)), "r8_exports");
  const R8Exports *e = fn == NULL ? NULL : fn();
  if (e == NULL) {
    return R_NilValue;
  }
  return R_MakeExternalPtr((void *) e, Rf_install("r8_exports"), R_NilValue);
}

// A native symbol in the form of `getNativeSymbolInfo(...)$address`: an
// external pointer tagged `native symbol`, which `.Call()` accepts, of class
// `NativeSymbol`, which `R CMD check` (tools:::checkFF) accepts as a
// registered routine.
static SEXP native_symbol(DL_FUNC f) {
  SEXP p = PROTECT(R_MakeExternalPtrFn(f, Rf_install("native symbol"), R_NilValue));
  Rf_setAttrib(p, R_ClassSymbol, Rf_mkString("NativeSymbol"));
  UNPROTECT(1);
  return p;
}

// The Rust routine behind `.Call(wrap__r8_new_value, ...)`.
SEXP wrap__r8_new_value(SEXP fields, SEXP type_name, SEXP variant);

// Constructs a value from a constructor's frame. Each field, named in
// `names` in declaration order, is its value in `env`, its promise forced
// with R's own evaluation, so an error raised while forcing propagates as R
// raised it, and a formal's default (a field's declared default) applies to
// an argument not supplied. A field whose formal has no default (`required`)
// is NULL when `missing()` holds for it, an argument not supplied or a
// missing argument passed through, which construction reports as missing.
// The forcing runs in C frames, before any Rust frame exists, and the fields
// go to the Rust constructor as one list.
static SEXP r8_construct_frame(SEXP env, SEXP names, SEXP required, SEXP type_name,
                               SEXP variant) {
  // One `missing(.)` call, kept for the session, its argument set per field.
  static SEXP test = NULL;
  if (test == NULL) {
    test = Rf_lang2(Rf_install("missing"), R_NilValue);
    R_PreserveObject(test);
  }
  R_xlen_t n = Rf_xlength(names);
  SEXP fields = PROTECT(Rf_allocVector(VECSXP, n));
  for (R_xlen_t i = 0; i < n; i++) {
    SEXP sym = Rf_installChar(STRING_ELT(names, i));
    int absent = 0;
    if (LOGICAL(required)[i] == 1) {
      SETCADR(test, sym);
      absent = Rf_asLogical(Rf_eval(test, env)) == 1;
    }
    if (!absent) {
      SET_VECTOR_ELT(fields, i, R_getVarEx(sym, env, FALSE, R_NilValue));
    }
  }
  Rf_setAttrib(fields, R_NamesSymbol, names);
  SEXP out = wrap__r8_new_value(fields, type_name, variant);
  UNPROTECT(1);
  return out;
}

// The C routines that R calls through `.Call()` on a native symbol:
// list(invoke, exports, construct). R8's `.Call` routines are the Rust code's,
// registered by extendr; these continue a jump or call R_GetCCallable() from
// C frames.
SEXP r8_native_symbols(void) {
  SEXP out = PROTECT(Rf_allocVector(VECSXP, 3));
  SET_VECTOR_ELT(out, 0, native_symbol((DL_FUNC) r8_invoke));
  SET_VECTOR_ELT(out, 1, native_symbol((DL_FUNC) r8_package_exports));
  SET_VECTOR_ELT(out, 2, native_symbol((DL_FUNC) r8_construct_frame));
  SEXP names = PROTECT(Rf_allocVector(STRSXP, 3));
  SET_STRING_ELT(names, 0, Rf_mkChar("invoke"));
  SET_STRING_ELT(names, 1, Rf_mkChar("exports"));
  SET_STRING_ELT(names, 2, Rf_mkChar("construct"));
  Rf_setAttrib(out, R_NamesSymbol, names);
  UNPROTECT(2);
  return out;
}
