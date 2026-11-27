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
#include <signal.h>

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

#define R8_INTERNAL_ABI
#include <R8.h>
#include <Rversion.h>
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

// Another view of the memory that `x` views, without attributes, so that the copy
// a write asks for and the attributes it sets are those of one read. R_NilValue
// unless `x` is a view in place.
SEXP r8_view_again(SEXP x) {
  if (!r8_is_view(x) || R_altrep_data2(x) != R_NilValue) {
    return R_NilValue;
  }
  R_altrep_class_t cls;
  switch (TYPEOF(x)) {
  case REALSXP:
    cls = view_double;
    break;
  case INTSXP:
    cls = view_integer;
    break;
  case LGLSXP:
    cls = view_logical;
    break;
  default:
    cls = view_raw;
    break;
  }
  return R_new_altrep(cls, R_altrep_data1(x), R_NilValue);
}

static void set_error(R8Error *err, const char *code, const char *message) {
  if (err != NULL) {
    snprintf(err->code, sizeof(err->code), "%s", code);
    snprintf(err->message, sizeof(err->message), "%s", message);
  }
}

// R8Api.protect: runs a callback under R_UnwindProtect, whose cleanup
// jumps back here when R jumps, so the callback's caller sees R8_UNWIND
// instead of the jump. Two continuation tokens keep the jump caught last:
// `continuation`, of R8's own protections (R8's Rust code, the checks of an
// interrupt, the construction of views), and `package_continuation`, of the
// R8Api.protect that packages call, whose jump the frame that called the
// compiled code continues once the code has returned (`r8_compiled_returned()`,
// `r8_continue_held()`). A protection of R8 that returns normally clears its
// own token, so a jump that a package's code caught stays held until R8
// continues it.
static SEXP continuation = NULL;
static SEXP package_continuation = NULL;

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

static R8Status protect_on(SEXP token, SEXP (*fn)(void *data), void *data, SEXP *value,
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
  *value = R_UnwindProtect(protected_body, &p, protected_cleanup, &p, token);
  // R_UnwindProtect() keeps the result in the token, which R8 preserves;
  // clearing it leaves the result to the caller.
  SETCAR(token, R_NilValue);
  return R8_OK;
}

// R8Api.protect of R8's own code.
static R8Status api_protect(SEXP (*fn)(void *data), void *data, SEXP *value,
                            R8Error *err) {
  return protect_on(continuation, fn, data, value, err);
}

// The number of calls of a package's compiled code from R8's C frames that
// have not returned (`r8_compiled_enter()`, `r8_compiled_returned()`).
static int depth = 0;

// Whether the R8Api.protect of a package's compiled code, which R8 called from
// a C frame, caught a jump that R8 has not yet continued or dropped. While it
// holds, the package's R8Api.protect reports the same jump without running its
// function, so the continuation keeps the jump and its value until the
// compiled code returns.
static int pending = 0;

// R8Api.protect as packages call it: api_protect(), with the jump that it
// catches inside a call from R8 pending. A package's own .Call() that calls it
// handles the status itself.
static R8Status package_protect(SEXP (*fn)(void *data), void *data, SEXP *value,
                                R8Error *err) {
  if (pending) {
    set_error(err, "unwind", "a jump of R is pending; the compiled code returns before it calls R");
    return R8_UNWIND;
  }
  R8Status status = protect_on(package_continuation, fn, data, value, err);
  if (status == R8_UNWIND && depth > 0) {
    pending = 1;
  }
  return status;
}

// R8Api.protect as packages call it, for R8's Rust code that calls R as a
// package's code does, between `r8_compiled_enter()` and `r8_compiled_held()`, so
// that a jump out of the call is held.
R8Status r8_package_protect(SEXP (*fn)(void *data), void *data, SEXP *value, R8Error *err) {
  return package_protect(fn, data, value, err);
}

// Before R8 calls a package's compiled code, from a C frame or a Rust frame.
void r8_compiled_enter(void) { depth++; }

// The error of compiled code that returned R8_UNWIND without a jump that its
// R8Api.protect caught.
const char *r8_unwound_text(void) {
  return "the compiled code returned R8_UNWIND, and R8Api.protect caught no jump of R "
         "[unwind_without_jump]";
}

// After a package's compiled code, which R8 called from a C frame, returns
// `status`: a jump of R that its R8Api.protect caught continues, whatever the
// code returns, so an interrupt stays an interrupt; R8_UNWIND without a jump is
// an error.
void r8_compiled_returned(R8Status status) {
  if (depth > 0) {
    depth--;
  }
  if (pending) {
    pending = 0;
    R_ContinueUnwind(package_continuation);
  }
  if (status == R8_UNWIND) {
    Rf_error("%s", r8_unwound_text());
  }
}

// After a package's compiled code that R8 called from a Rust frame: whether its
// R8Api.protect caught a jump of R, which stays held, so that the package's
// R8Api.protect runs nothing, until the Rust frames have returned and
// `r8_continue_held()` continues it.
int r8_compiled_held(void) {
  if (depth > 0) {
    depth--;
  }
  return pending;
}

// Whether a jump of R that a package's R8Api.protect caught is held.
int r8_jump_held(void) { return pending; }

// Continues the jump that `r8_compiled_held()` held.
void r8_continue_held(void) {
  if (pending) {
    pending = 0;
    R_ContinueUnwind(package_continuation);
  }
}

// Drops the jump that a package's R8Api.protect caught, for R8's Rust code,
// which calls a codec function from a Rust frame and reports the jump as the
// error `unwind`.
void r8_drop_unwind(void) {
  if (pending) {
    pending = 0;
    SETCAR(package_continuation, R_NilValue);
  }
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
  // The caller may hold the owner unprotected.
  PROTECT(a->owner);
  SEXP len = PROTECT(Rf_ScalarReal((double) a->n));
  SEXP ptr = PROTECT(R_MakeExternalPtr((void *) a->data, len, a->owner));
  SEXP ans = R_new_altrep(a->cls, ptr, R_NilValue);
  UNPROTECT(3);
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
// Rust-stored types).
R8Status r8_api_new_value(const char *type_name, const void *module, void *state, SEXP *value,
                          R8Error *err);
R8Status r8_api_value_state(SEXP value, const char *type_name, const void *module,
                            const void **state, R8Error *err);
R8Status r8_api_upcast_state(SEXP value, const char *type_name, const void *module,
                             const void **state, R8Error *err);
R8Status r8_api_value_of_fields(const char *type_name, const char *variant,
                                const char *declaration_hash, SEXP fields, SEXP *value,
                                R8Error *err);
R8Status r8_api_with_fields(SEXP value, SEXP fields, SEXP *new_value, R8Error *err);
R8Status r8_api_pin(const void *module, const uint8_t *record, size_t len, R8Error *err);

// The type of a value, and the variant of an enum's value, each one string, or
// R_NilValue (value.c).
SEXP r8_c_type(SEXP x);
SEXP r8_c_variant(SEXP x);
SEXP r8_c_is_value(SEXP x);
SEXP r8_c_names(SEXP x);

// The fields of a value as a read gives them (field.c).
SEXP r8_c_get(SEXP x, SEXP name);
SEXP r8_c_fields(SEXP x, SEXP at);

// The identity of a value, or R_NilValue (value.c), and whether an identity is
// of the registered declaration (value.rs).
SEXP r8_value_id(SEXP x);
int r8_identity_current(SEXP id);

static R8Status r8_api_value_type(SEXP value, const char **type_name, const char **variant) {
  SEXP type = r8_c_type(value), v = r8_c_variant(value);
  *type_name = type == R_NilValue ? NULL : CHAR(STRING_ELT(type, 0));
  *variant = v == R_NilValue ? NULL : CHAR(STRING_ELT(v, 0));
  return R8_OK;
}

static const R8Api api = {
    R8_ABI_VERSION,
    sizeof(R8Api),
    r8_api_new_value,
    r8_api_value_state,
    r8_api_upcast_state,
    view_doubles,
    view_integers,
    view_logicals,
    view_bytes,
    package_protect,
    r8_api_value_of_fields,
    r8_api_with_fields,
    r8_api_pin,
    r8_api_value_type,
};

static const R8Api *r8_api(uint32_t abi_version) {
  return abi_version == R8_ABI_VERSION ? &api : NULL;
}

void r8_register_api(void) {
  continuation = R_MakeUnwindCont();
  R_PreserveObject(continuation);
  package_continuation = R_MakeUnwindCont();
  R_PreserveObject(package_continuation);
  R_RegisterCCallable("R8", "r8_api", (DL_FUNC) r8_api);
}

// R8Api.protect for R8's own Rust code (rust_stored.rs), whose callers convert
// R8_UNWIND into R8_ERROR, since R8's API functions never jump.
R8Status r8_protect(SEXP (*fn)(void *data), void *data, SEXP *value, R8Error *err) {
  return api_protect(fn, data, value, err);
}

static SEXP check_interrupt(void *data) {
  (void) data;
  R_CheckUserInterrupt();
  return R_NilValue;
}

static SEXP limit_reached(SEXP condition, void *reached) {
  (void) condition;
  *(int *) reached = 1;
  return R_NilValue;
}

static SEXP check_both(void *reached) {
  return R_tryCatchError(check_interrupt, NULL, limit_reached, reached);
}

// Whether the user interrupted R (2), a time limit of R is reached (1), or neither
// (0); R's jump ends here. The interrupt is raised again, which R takes once
// R8's code has returned.
int r8_check_interrupt(void) {
  int reached = 0;
  SEXP value = R_NilValue;
  R8Error err = {{0}, {0}};
  if (api_protect(check_both, &reached, &value, &err) == R8_UNWIND) {
#ifndef _WIN32
    raise(SIGINT);
#endif
    return 2;
  }
  return reached;
}

// Continues the jump of R that R8Api.protect caught last.
void r8_continue_unwind(void) { R_ContinueUnwind(continuation); }

// The number of the fields of `ancestor` when the registered type
// `type_name` is a subtype of it, and -1 otherwise; and the value of a struct
// over the list of its fields (construct.rs).
int r8_inherited_fields(const char *type_name, const char *ancestor);
SEXP r8_value_of_fields(const char *type_name, SEXP fields, R8Error *err);

// `r8upcast()` for the value `x` of a descendant of the struct `type`, a
// qualified name: a value over `x`'s first fields, which precede those that
// its descendants add, read here, in a C frame, since a Rust-stored value's
// field read may raise an R error. The fields are `x`'s own objects and are
// not validated: the rules of a subtype include those of `type`, which read
// its fields only.
// spec: R8/class-system#inheritance
static SEXP r8_upcast(SEXP x, SEXP type) {
  if (TYPEOF(type) != STRSXP || XLENGTH(type) != 1) {
    Rf_error("a type is named by one string");
  }
  const char *target = CHAR(STRING_ELT(type, 0));
  SEXP own = r8_c_type(x);
  int n = own == R_NilValue || !r8_identity_current(r8_value_id(x)) ? -1 : r8_inherited_fields(CHAR(STRING_ELT(own, 0)), target);
  if (n < 0 || XLENGTH(x) < n) {
    Rf_error("`x` is not a value of a subtype of `%s`", target);
  }
  SEXP fields = PROTECT(Rf_allocVector(VECSXP, n));
  for (int i = 0; i < n; i++) {
    SET_VECTOR_ELT(fields, i, VECTOR_ELT(x, i));
  }
  R8Error err = {{0}, {0}};
  SEXP out = r8_value_of_fields(target, fields, &err);
  if (out == NULL) {
    Rf_error("%s", err.message);
  }
  UNPROTECT(1);
  return out;
}

static void r8_abort3(const char *name, SEXP a, SEXP b, SEXP c, SEXP ns);

// The call of the generic whose method's frame is running: R's
// `sys.call(sys.parent())` in that frame.
static SEXP generic_call(void) {
  SEXP parent = PROTECT(Rf_lang1(Rf_install("sys.parent")));
  SEXP call = PROTECT(Rf_lang2(Rf_install("sys.call"), parent));
  SEXP env = PROTECT(R_GetCurrentEnv());
  int failed = 0;
  SEXP out = R_tryEvalSilent(call, env, &failed);
  UNPROTECT(3);
  return failed || TYPEOF(out) != LANGSXP ? R_NilValue : out;
}

// Calls the impl function at the address of `fn` with the elements of the
// list `args`; R8_ERROR raises an R error of the impl's message and code, from
// the generic's call, and R8_UNWIND continues the jump that R8Api.protect
// caught. R calls it through `.Call()` on a native symbol, so the jump
// continues from a C frame.
static SEXP r8_invoke(SEXP fn, SEXP args) {
  if (TYPEOF(fn) != EXTPTRSXP || TYPEOF(args) != VECSXP) {
    Rf_error("a compiled method calls R8 with its function and its arguments");
  }
  R8MethodFn call = (R8MethodFn) R_ExternalPtrAddrFn(fn);
  if (call == NULL) {
    Rf_error("the package of this compiled method is unloaded; load it again");
  }
  R_xlen_t n = XLENGTH(args);
  const SEXP *argv = n > 0 ? (const SEXP *) R_alloc((size_t) n, sizeof(SEXP)) : NULL;
  for (R_xlen_t i = 0; i < n; i++) {
    ((SEXP *) argv)[i] = VECTOR_ELT(args, i);
  }
  R8CallInputs inputs = {sizeof(R8CallInputs), 0, 0};
  SEXP result = R_NilValue;
  R8Error err = {{0}, {0}};
  r8_compiled_enter();
  R8Status status = call(&inputs, argv, (size_t) n, &result, &err);
  r8_compiled_returned(status);
  if (status == R8_OK) {
    return result;
  }
  if (status == R8_ERROR) {
    // A callee may leave the buffers without an end.
    err.code[sizeof(err.code) - 1] = '\0';
    err.message[sizeof(err.message) - 1] = '\0';
    // R8's `compiled_abort()` raises the classed condition, r8_compiled_error
    // or r8_panic, from the generic's call.
    SEXP ns = PROTECT(R_FindNamespace(PROTECT(Rf_mkString("R8"))));
    // `compiled_abort()` names an empty code or message.
    SEXP code = PROTECT(Rf_mkString(err.code));
    SEXP message = PROTECT(Rf_mkString(err.message));
    r8_abort3("compiled_abort", code, message, PROTECT(generic_call()), ns);
    UNPROTECT(5);
  }
  Rf_error("the impl function returned the status %d, none of R8_OK, R8_ERROR, and R8_UNWIND",
           (int) status);
  return R_NilValue;
}

void r8_pin_library(const void *address);

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
  r8_pin_library(e);
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

// The value of `sym` in the frame of `env`, its promise forced, or `absent`
// for a symbol the frame does not bind. R 4.5.0 adds R_getVarEx() to the API;
// on earlier R, and in webR builds, evaluating the symbol in `env` after a
// test of the frame forces the promise the same way. webR builds take the
// second path because extendr-ffi 0.9.0 declares R_getVarEx() and R_getVar()
// with two arguments, and wasm-ld rejects a C object that declares either
// with R's signature ("function signature mismatch").
SEXP r8_get_var(SEXP sym, SEXP env, SEXP absent) {
#if defined(__EMSCRIPTEN__) && R_VERSION >= R_Version(4, 5, 0)
  return R_existsVarInFrame(env, sym) ? Rf_eval(sym, env) : absent;
#elif R_VERSION >= R_Version(4, 5, 0)
  return R_getVarEx(sym, env, FALSE, absent);
#else
  return Rf_findVarInFrame(env, sym) == R_UnboundValue ? absent : Rf_eval(sym, env);
#endif
}

// The Rust routine behind `.Call(wrap__r8_new_value, ...)`.
SEXP wrap__r8_new_value(SEXP fields, SEXP type_name, SEXP variant);

// Whether `x` is a value of another declaration than the registered one, or a
// list without a class that holds one, within `levels` lists.
static int holds_other(SEXP x, int levels) {
  SEXP id = r8_value_id(x);
  if (id != R_NilValue) {
    return !r8_identity_current(id);
  }
  if (TYPEOF(x) == VECSXP && !ALTREP(x) && !Rf_isObject(x) && levels > 0) {
    R_xlen_t n = XLENGTH(x);
    for (R_xlen_t i = 0; i < n; i++) {
      if (holds_other(VECTOR_ELT(x, i), levels - 1)) {
        return 1;
      }
    }
  }
  return 0;
}

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
  if (TYPEOF(env) != ENVSXP || TYPEOF(names) != STRSXP || TYPEOF(required) != LGLSXP ||
      XLENGTH(required) != XLENGTH(names)) {
    Rf_error("a constructor calls R8 with its frame and its fields");
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
      SEXP v = r8_get_var(sym, env, R_NilValue);
      SET_VECTOR_ELT(fields, i, v);
      // A default that is a value, made when the package of the constructor was
      // installed, is built from the registered declarations.
      if (LOGICAL(required)[i] != 1 && holds_other(v, 100)) {
        SETCADR(test, sym);
        if (Rf_asLogical(Rf_eval(test, env)) == 1) {
          SET_VECTOR_ELT(fields, i, R_NilValue);
        }
      }
    }
  }
  Rf_setAttrib(fields, R_NamesSymbol, names);
  SEXP out = wrap__r8_new_value(fields, type_name, variant);
  UNPROTECT(1);
  return out;
}

// The key of a value in a dispatch index, which determines the value's chain
// in a position: the type of a value of R8, of the registered declaration; for
// another object with a class
// attribute, `class:` and the whole class vector joined by the byte 0x1F,
// followed for an S4 object by 0x1F, `@`, and the class's package; for an
// object without one, `type:`, `matrix:`, or `array:` by its `dim` attribute,
// and its `typeof()`, from a table of installed symbols. R8's
// `dispatch_key()` computes the same keys. Returns R_NilValue for a language
// object, whose class depends on its head, for a key longer than 255 bytes, and
// for a value of another declaration than the registered one: such values are
// not cached.
static SEXP dispatch_key(SEXP x) {
  static SEXP package_sym = NULL;
  static SEXP implicit[3][32];
  if (package_sym == NULL) {
    package_sym = Rf_install("package");
    const char *prefix[3] = {"type:", "matrix:", "array:"};
    // The types of objects that reach a generic without a class attribute.
    const SEXPTYPE types[] = {NILSXP,     SYMSXP,  LISTSXP,    CLOSXP,
                              ENVSXP,     SPECIALSXP, BUILTINSXP, LGLSXP,
                              INTSXP,     REALSXP, CPLXSXP,    STRSXP,
                              VECSXP,     EXPRSXP, EXTPTRSXP,  WEAKREFSXP,
                              RAWSXP};
    char buf[64];
    for (int d = 0; d < 3; d++) {
      for (int t = 0; t < 32; t++) {
        implicit[d][t] = R_NilValue;
      }
      for (size_t k = 0; k < sizeof types / sizeof types[0]; k++) {
        snprintf(buf, sizeof buf, "%s%s", prefix[d], Rf_type2char(types[k]));
        implicit[d][types[k]] = Rf_install(buf);
      }
    }
  }
  SEXP type = r8_c_type(x);
  if (type != R_NilValue) {
    return r8_identity_current(r8_value_id(x)) ? Rf_installTrChar(STRING_ELT(type, 0))
                                               : R_NilValue;
  }
  SEXP cls = Rf_getAttrib(x, R_ClassSymbol);
  if (TYPEOF(cls) == STRSXP && XLENGTH(cls) >= 1) {
    char buf[256];
    size_t at = 6;
    memcpy(buf, "class:", 6);
    R_xlen_t n = XLENGTH(cls);
    for (R_xlen_t i = 0; i <= n; i++) {
      const char *c;
      if (i < n) {
        c = Rf_translateCharUTF8(STRING_ELT(cls, i));
      } else if (Rf_isS4(x)) {
        SEXP pkg = Rf_getAttrib(cls, package_sym);
        c = TYPEOF(pkg) == STRSXP && XLENGTH(pkg) == 1
              ? Rf_translateCharUTF8(STRING_ELT(pkg, 0))
              : "";
        if (at + 2 >= sizeof buf) {
          return R_NilValue;
        }
        buf[at++] = '\x1f';
        buf[at++] = '@';
      } else {
        break;
      }
      size_t len = strlen(c);
      size_t sep = i > 0 && i < n ? 1 : 0;
      if (at + sep + len >= sizeof buf) {
        return R_NilValue;
      }
      if (sep) {
        buf[at++] = '\x1f';
      }
      memcpy(buf + at, c, len);
      at += len;
    }
    buf[at] = '\0';
    return Rf_install(buf);
  }
  if (TYPEOF(x) == LANGSXP) {
    return R_NilValue;
  }
  SEXP dim = Rf_getAttrib(x, R_DimSymbol);
  int d = dim == R_NilValue ? 0 : (XLENGTH(dim) == 2 ? 1 : 2);
  return TYPEOF(x) < 32 ? implicit[d][TYPEOF(x)] : R_NilValue;
}

// A value as an argument of a call that C evaluates: evaluation would look a
// symbol up and run a language object, so each is quoted.
static SEXP quoted(SEXP x) {
  return TYPEOF(x) == SYMSXP || TYPEOF(x) == LANGSXP ? Rf_lang2(Rf_install("quote"), x) : x;
}

// R8's namespace, the parent of `.r8_dispatch`, which a reload replaces.
static SEXP r8_namespace(SEXP dispatch) {
#if R_VERSION >= R_Version(4, 5, 0)
  return R_ParentEnv(dispatch);
#else
  return ENCLOS(dispatch);
#endif
}

// The impl of the trait method `key`, a symbol "Trait|method", for the
// values `args[0..n]` of its dispatched arguments: from the method's index in
// `dispatch` (`.r8_dispatch`), one environment per position keyed by
// `dispatch_key()`, the impl at the last; on a miss, R8's
// `dispatch_select(key, trait_name, method, ...)`, called in the namespace
// that is `dispatch`'s parent, finds the impl, stores it in the index, and
// returns it, or raises the dispatch error. The routine keeps no SEXP beyond
// installed symbols.
// spec: R8/class-system#traits-equality-and-conformance-in-r
static SEXP find_n(SEXP *args, int n, SEXP key, SEXP dispatch) {
  if (TYPEOF(key) != SYMSXP || TYPEOF(dispatch) != ENVSXP ||
      strchr(CHAR(PRINTNAME(key)), '|') == NULL) {
    Rf_error("a generic calls R8 with its key and the dispatch cache");
  }
  static SEXP select_sym = NULL;
  if (select_sym == NULL) {
    select_sym = Rf_install("dispatch_select");
  }
  SEXP node = r8_get_var(key, dispatch, R_NilValue);
  for (int i = 0; i < n && TYPEOF(node) == ENVSXP; i++) {
    SEXP k = dispatch_key(args[i]);
    node = k == R_NilValue ? R_NilValue : r8_get_var(k, node, R_NilValue);
  }
  if (TYPEOF(node) == CLOSXP) {
    return node;
  }
  SEXP ns = r8_namespace(dispatch);
  // "Trait|method": a qualified trait name holds no '|'.
  const char *text = CHAR(PRINTNAME(key));
  const char *bar = strchr(text, '|');
  SEXP call = PROTECT(Rf_allocVector(LANGSXP, n + 4));
  SETCAR(call, select_sym);
  SEXP cell = CDR(call);
  SETCAR(cell, Rf_ScalarString(PRINTNAME(key)));
  cell = CDR(cell);
  SEXP trait_name = PROTECT(Rf_mkCharLenCE(text, (int) (bar - text), CE_UTF8));
  SETCAR(cell, Rf_ScalarString(trait_name));
  cell = CDR(cell);
  SETCAR(cell, Rf_mkString(bar + 1));
  for (int i = 0; i < n; i++) {
    cell = CDR(cell);
    SETCAR(cell, quoted(args[i]));
  }
  SEXP impl = Rf_eval(call, ns);
  UNPROTECT(2);
  return impl;
}

static SEXP r8_find(SEXP x, SEXP key, SEXP dispatch) {
  SEXP args[1] = {x};
  return find_n(args, 1, key, dispatch);
}

static SEXP r8_find2(SEXP x, SEXP y, SEXP key, SEXP dispatch) {
  SEXP args[2] = {x, y};
  return find_n(args, 2, key, dispatch);
}

static SEXP r8_find3(SEXP x, SEXP y, SEXP z, SEXP key, SEXP dispatch) {
  SEXP args[3] = {x, y, z};
  return find_n(args, 3, key, dispatch);
}

// The tests of a typed method's arguments and result (signature.rs), which
// neither allocate R memory nor raise an R error, and the Rust routines
// behind `.Call(wrap__r8_check_args, ...)` and `.Call(wrap__r8_check_result,
// ...)`, which validate what a test does not pass. The outcomes are the
// constants of signature.rs.
enum { R8_PASS = 0, R8_CHECK = 1, R8_TO_INTEGER = 2, R8_TO_DOUBLE = 3, R8_SELF = 4 };
int r8_signature_forced(SEXP key, SEXP *out, int capacity);
int r8_signature_test(SEXP key, const SEXP *symbols, const SEXP *values, int n,
                      int *outcomes, int *dispatched, int *n_dispatched);
int r8_signature_result(SEXP key, SEXP tuple, SEXP value);
SEXP wrap__r8_check_args(SEXP key, SEXP values);
SEXP wrap__r8_check_result(SEXP key, SEXP tuple, SEXP value);

// The value `x` of a test's outcome: `x`, or its conversion.
static SEXP converted(SEXP x, int outcome) {
  switch (outcome) {
  case R8_TO_INTEGER:
    return Rf_ScalarInteger((int) REAL(x)[0]);
  case R8_TO_DOUBLE:
    return Rf_ScalarReal((double) INTEGER(x)[0]);
  default:
    return x;
  }
}

static int is_problems(SEXP x) {
  return TYPEOF(x) == VECSXP && Rf_inherits(x, "r8_problems");
}

// Calls R8's function `name` with three values; it raises an R error.
static void r8_abort3(const char *name, SEXP a, SEXP b, SEXP c, SEXP ns) {
  a = PROTECT(quoted(a));
  b = PROTECT(quoted(b));
  c = PROTECT(quoted(c));
  SEXP call = PROTECT(Rf_lang4(Rf_install(name), a, b, c));
  Rf_eval(call, ns);
  UNPROTECT(4);
}

// The parameters a call forces that fit on the C stack.
#define R8_FORCED 32

// Checks the arguments of a call of the typed generic of the method `key`,
// whose frame is `env`, and returns the method's impl for them, as
// `find_n()` does.
//
// The typed and the dispatched parameters are forced in the order of the
// formals with R's own evaluation, so a missing argument without a default
// and an error raised while forcing propagate as R's own. Forcing runs R
// code, which may declare the method again; its signature is read again
// afterwards, and the call starts over when the parameters differ. The
// values stay protected from their forcing to the dispatch, whatever the
// frame's bindings become. A value that its type converts, such as a whole
// double for an integer, is bound in the frame in its stored form, which the
// method receives.
// spec: R8/class-system#checks-at-a-call
//
// A function of new_function() (`find` 0) has no dispatched parameter; its key
// is its qualified name, and the routine returns NULL once the arguments are
// checked. A function whose declaration is not registered, as in another
// process, registers the record that it carries (R8's `function_unregistered()`)
// and is checked once more.
static SEXP checked_call(SEXP env, SEXP key, SEXP dispatch, int find) {
  if (TYPEOF(env) != ENVSXP || TYPEOF(key) != SYMSXP || TYPEOF(dispatch) != ENVSXP) {
    Rf_error("a typed generic calls R8 with its frame, its key, and the dispatch cache");
  }
  SEXP few[R8_FORCED], few_values[R8_FORCED];
  int few_outcomes[R8_FORCED];
  int registered = 0;
  for (;;) {
    SEXP *symbols = few, *values = few_values;
    int *outcomes = few_outcomes;
    int n = r8_signature_forced(key, symbols, R8_FORCED);
    if (n > R8_FORCED) {
      symbols = (SEXP *) R_alloc((size_t) n, sizeof(SEXP));
      values = (SEXP *) R_alloc((size_t) n, sizeof(SEXP));
      outcomes = (int *) R_alloc((size_t) n, sizeof(int));
      n = r8_signature_forced(key, symbols, n);
    }
    if (n < 0 && !find && !registered) {
      r8_abort3("function_unregistered", PROTECT(Rf_ScalarString(PRINTNAME(key))), env,
                R_NilValue, r8_namespace(dispatch));
      UNPROTECT(1);
      registered = 1;
      continue;
    }
    if (n < 0) {
      r8_abort3("signature_abort", PROTECT(Rf_ScalarString(PRINTNAME(key))), R_NilValue,
                R_NilValue, r8_namespace(dispatch));
    }
    for (int i = 0; i < n; i++) {
      values[i] = PROTECT(r8_get_var(symbols[i], env, R_MissingArg));
    }
    int at[3], n_at = 0;
    int outcome =
      r8_signature_test(key, symbols, values, n, outcomes, at, &n_at);
    if (outcome < 0) {
      UNPROTECT(n);
      continue;
    }
    if (outcome == R8_CHECK) {
      SEXP list = PROTECT(Rf_allocVector(VECSXP, n));
      for (int i = 0; i < n; i++) {
        SET_VECTOR_ELT(list, i, values[i]);
      }
      SEXP checked = PROTECT(wrap__r8_check_args(key, list));
      if (is_problems(checked)) {
        r8_abort3("argument_abort", PROTECT(Rf_ScalarString(PRINTNAME(key))), checked, env,
                  r8_namespace(dispatch));
      }
      if (TYPEOF(checked) == VECSXP && XLENGTH(checked) == n) {
        for (int i = 0; i < n; i++) {
          SEXP stored = VECTOR_ELT(checked, i);
          if (stored != R_NilValue) {
            // The frame keeps the value once `checked` is released.
            Rf_defineVar(symbols[i], stored, env);
          }
        }
      }
      UNPROTECT(2);
    }
    if (outcome != R8_PASS) {
      for (int i = 0; i < n; i++) {
        if (outcomes[i] == R8_TO_INTEGER || outcomes[i] == R8_TO_DOUBLE) {
          SEXP v = PROTECT(converted(values[i], outcomes[i]));
          Rf_defineVar(symbols[i], v, env);
          UNPROTECT(1);
        }
      }
    }
    if (!find) {
      UNPROTECT(n);
      return R_NilValue;
    }
    SEXP args[3];
    for (int k = 0; k < n_at; k++) {
      args[k] = values[at[k]];
    }
    SEXP impl = find_n(args, n_at, key, dispatch);
    UNPROTECT(n);
    return impl;
  }
}

static SEXP r8_call(SEXP env, SEXP key, SEXP dispatch) {
  return checked_call(env, key, dispatch, 1);
}

// Checks the arguments of a call of a function of new_function(), whose frame is
// `env` and whose key is its qualified name; returns NULL.
// spec: R8/class-system#typed-functions
static SEXP r8_check_call(SEXP env, SEXP key, SEXP dispatch) {
  return checked_call(env, key, dispatch, 0);
}

int r8_native_function(SEXP key, R8MethodFn *call, const SEXP **params);

// The result `value` of the typed function `key`, checked against its return
// type, in its stored form; R8's `result_abort()` raises the error.
static SEXP function_result(SEXP value, SEXP key, SEXP dispatch) {
  int outcome = r8_signature_result(key, R_BlankString, value);
  if (outcome == R8_TO_INTEGER || outcome == R8_TO_DOUBLE) {
    return converted(value, outcome);
  }
  if (outcome == R8_CHECK) {
    PROTECT(value);
    SEXP text = PROTECT(Rf_mkString(""));
    SEXP checked = PROTECT(wrap__r8_check_result(key, text, value));
    if (is_problems(checked)) {
      r8_abort3("result_abort", PROTECT(Rf_ScalarString(PRINTNAME(key))), text, checked,
                r8_namespace(dispatch));
    }
    SEXP out = TYPEOF(checked) == VECSXP && XLENGTH(checked) == 1 ? VECTOR_ELT(checked, 0)
                                                                    : value;
    UNPROTECT(3);
    return out;
  }
  if (outcome < 0) {
    r8_abort3("signature_abort", PROTECT(Rf_ScalarString(PRINTNAME(key))), R_NilValue,
              R_NilValue, r8_namespace(dispatch));
  }
  return value;
}

// The body that the package of the typed function `key` registered, and the
// symbols of its parameters, copied for the call; R8's `native_abort()` raises
// the error when no package registered one.
static R8MethodFn native_body(SEXP key, SEXP dispatch, SEXP **params, int *n) {
  R8MethodFn call = NULL;
  const SEXP *held = NULL;
  *n = r8_native_function(key, &call, &held);
  if (*n < 0 || call == NULL) {
    r8_abort3("native_abort", PROTECT(Rf_ScalarString(PRINTNAME(key))), R_NilValue,
              R_NilValue, r8_namespace(dispatch));
  }
  // The registration holds the symbols, which R code that runs during the call
  // may replace by registering the package again; symbols live for the session.
  *params = *n > 0 ? (SEXP *) R_alloc((size_t) *n, sizeof(SEXP)) : NULL;
  for (int i = 0; i < *n; i++) {
    (*params)[i] = held[i];
  }
  return call;
}

// Calls the typed function `key` whose body is compiled code, from its frame
// `env`. Each parameter of the declaration is evaluated in order, `...` as a
// list, with R's own evaluation, so a missing argument without a default raises
// R's error; then the routine checks and converts the arguments as `check`
// does, reads the parameters' values from the frame, in which no R code runs
// after the check, finds the body that the function's package registered
// (`r8_native_function()`) again, passes it the values, and checks the result.
// R8_ERROR raises R8's `compiled_abort()` from the function's call, and a jump
// that R8Api.protect caught continues (`r8_compiled_returned()`).
// spec: R8/class-system#typed-functions
static SEXP r8_native(SEXP env, SEXP key, SEXP dispatch) {
  static SEXP list_fn = NULL;
  if (list_fn == NULL) {
    // base's list(), whatever `list` names in the function's frame.
    list_fn = Rf_findFun(Rf_install("list"), R_BaseEnv);
  }
  SEXP *params = NULL;
  int n = 0;
  R8MethodFn call = native_body(key, dispatch, &params, &n);
  SEXP args = PROTECT(Rf_allocVector(VECSXP, n));
  for (int i = 0; i < n; i++) {
    if (params[i] == R_DotsSymbol) {
      SEXP dots = PROTECT(Rf_lang2(list_fn, R_DotsSymbol));
      SET_VECTOR_ELT(args, i, Rf_eval(dots, env));
      UNPROTECT(1);
    } else {
      SET_VECTOR_ELT(args, i, Rf_eval(params[i], env));
    }
  }
  checked_call(env, key, dispatch, 0);
  for (int i = 0; i < n; i++) {
    if (params[i] != R_DotsSymbol) {
      // A forced parameter, bound in its stored form by the check.
      SET_VECTOR_ELT(args, i, r8_get_var(params[i], env, R_NilValue));
    }
  }
  SEXP *again = NULL;
  int m = 0;
  R8MethodFn now = native_body(key, dispatch, &again, &m);
  int same = m == n;
  for (int i = 0; same && i < n; i++) {
    same = again[i] == params[i];
  }
  if (!same) {
    r8_abort3("native_abort", PROTECT(Rf_ScalarString(PRINTNAME(key))), R_NilValue,
              R_NilValue, r8_namespace(dispatch));
  }
  call = now;
  const SEXP *argv = n > 0 ? (const SEXP *) R_alloc((size_t) n, sizeof(SEXP)) : NULL;
  for (int i = 0; i < n; i++) {
    ((SEXP *) argv)[i] = VECTOR_ELT(args, i);
  }
  R8CallInputs inputs = {sizeof(R8CallInputs), 0, 0};
  SEXP result = R_NilValue;
  R8Error err = {{0}, {0}};
  r8_compiled_enter();
  R8Status status = call(&inputs, argv, (size_t) n, &result, &err);
  r8_compiled_returned(status);
  if (status == R8_ERROR) {
    err.code[sizeof(err.code) - 1] = '\0';
    err.message[sizeof(err.message) - 1] = '\0';
    SEXP ns = r8_namespace(dispatch);
    SEXP code = PROTECT(Rf_mkString(err.code));
    SEXP message = PROTECT(Rf_mkString(err.message));
    SEXP sys_call = PROTECT(Rf_lang1(Rf_install("sys.call")));
    SEXP fcall = PROTECT(Rf_eval(sys_call, env));
    r8_abort3("compiled_abort", code, message, fcall, ns);
    UNPROTECT(4);
  }
  if (status != R8_OK) {
    Rf_error("the compiled body returned the status %d, none of R8_OK, R8_ERROR, and R8_UNWIND",
             (int) status);
  }
  PROTECT(result);
  SEXP out = function_result(result, key, dispatch);
  UNPROTECT(2);
  return out;
}

// Whether `out` is a value of the type of `x`: the same type for a value of
// R8, and an identical class vector for any other object, implicit class
// included.
static int same_type(SEXP x, SEXP out) {
  static SEXP class_sym = NULL;
  if (class_sym == NULL) {
    class_sym = Rf_install("class");
  }
  SEXP type = r8_c_type(x), got = r8_c_type(out);
  if (type != R_NilValue || got != R_NilValue) {
    return type != R_NilValue && got != R_NilValue &&
           STRING_ELT(type, 0) == STRING_ELT(got, 0);
  }
  SEXP cx = Rf_getAttrib(x, R_ClassSymbol);
  SEXP co = Rf_getAttrib(out, R_ClassSymbol);
  if (cx != R_NilValue || co != R_NilValue) {
    return R_compute_identical(cx, co, 16);
  }
  SEXP cls = PROTECT(Rf_lang2(class_sym, PROTECT(quoted(x))));
  SEXP ix = PROTECT(Rf_eval(cls, R_BaseEnv));
  SETCADR(cls, quoted(out));
  SEXP io = PROTECT(Rf_eval(cls, R_BaseEnv));
  int same = R_compute_identical(ix, io, 16);
  UNPROTECT(4);
  return same;
}

// Checks the result of a call of the method `key` from the impl `f`:
// `visible` is the list that `withVisible()` made of it, `x` the call's first
// dispatched argument. The result's stored form replaces it in the list,
// which the generic's frame alone holds. Returns whether the result is
// visible.
// spec: R8/class-system#checks-at-a-call
static SEXP r8_result(SEXP visible, SEXP f, SEXP key, SEXP x, SEXP dispatch) {
  static SEXP provider_sym = NULL;
  if (provider_sym == NULL) {
    provider_sym = Rf_install("r8_provider");
  }
  if (TYPEOF(visible) != VECSXP || XLENGTH(visible) != 2 || TYPEOF(key) != SYMSXP ||
      TYPEOF(dispatch) != ENVSXP) {
    Rf_error("a typed generic calls R8 with its result, its key, and the dispatch cache");
  }
  SEXP value = VECTOR_ELT(visible, 0);
  SEXP provider = Rf_getAttrib(f, provider_sym);
  SEXP tuple = TYPEOF(provider) == STRSXP && XLENGTH(provider) == 3
                 ? STRING_ELT(provider, 2)
                 : R_BlankString;
  int outcome = r8_signature_result(key, tuple, value);
  if (outcome == R8_TO_INTEGER || outcome == R8_TO_DOUBLE) {
    SET_VECTOR_ELT(visible, 0, converted(value, outcome));
  } else if (outcome == R8_SELF) {
    if (!same_type(x, value)) {
      SEXP out = PROTECT(Rf_allocVector(VECSXP, 2));
      SET_VECTOR_ELT(out, 0, value);
      SET_VECTOR_ELT(out, 1, provider);
      r8_abort3("self_abort", PROTECT(Rf_ScalarString(PRINTNAME(key))), x, out,
                r8_namespace(dispatch));
    }
  } else if (outcome == R8_CHECK) {
    SEXP text = PROTECT(Rf_ScalarString(tuple));
    SEXP checked = PROTECT(wrap__r8_check_result(key, text, value));
    if (is_problems(checked)) {
      r8_abort3("result_abort", PROTECT(Rf_ScalarString(PRINTNAME(key))), text, checked,
                r8_namespace(dispatch));
    }
    if (TYPEOF(checked) == VECSXP && XLENGTH(checked) == 1) {
      SET_VECTOR_ELT(visible, 0, VECTOR_ELT(checked, 0));
    }
    UNPROTECT(2);
  } else if (outcome < 0) {
    r8_abort3("signature_abort", PROTECT(Rf_ScalarString(PRINTNAME(key))), R_NilValue,
              R_NilValue, r8_namespace(dispatch));
  }
  return VECTOR_ELT(visible, 1);
}

// The result `out` of a replacement method called on `x`, through the trait
// method `key`, a symbol "Trait|method": `out` when it is a value of `x`'s
// type (`same_type()`). Otherwise R8's `replacement_abort()` raises the error.
// The routine keeps no SEXP beyond installed symbols.
// spec: R8/class-system#replacement-methods
static SEXP r8_replaced(SEXP x, SEXP out, SEXP key) {
  if (TYPEOF(key) != SYMSXP) {
    Rf_error("a replacement generic calls R8 with its key");
  }
  if (same_type(x, out)) {
    return out;
  }
  SEXP ns = PROTECT(R_FindNamespace(PROTECT(Rf_mkString("R8"))));
  r8_abort3("replacement_abort", PROTECT(Rf_ScalarString(PRINTNAME(key))), x, out, ns);
  UNPROTECT(3);
  return out;
}

// The name of the variant of the enum value `x`; R8's `variant_abort()`
// raises the error for another object.
// spec: R8/class-system#values-in-r
static SEXP r8_variant(SEXP x) {
  SEXP v = r8_c_variant(x);
  if (v == R_NilValue) {
    SEXP ns = PROTECT(R_FindNamespace(PROTECT(Rf_mkString("R8"))));
    r8_abort3("variant_abort", x, R_NilValue, R_NilValue, ns);
    UNPROTECT(2);
    return R_NilValue;
  }
  return v;
}

// Whether the variants `variants`, R8's `.r8_variants`, hold the enum of the
// value `x` with a variant `name`, a string that is neither NA nor empty.
static int declares_variant(SEXP x, SEXP name, SEXP variants) {
  SEXP type = r8_c_type(x);
  if (type == R_NilValue || TYPEOF(variants) != ENVSXP) {
    return 0;
  }
  SEXP declared = r8_get_var(Rf_installTrChar(STRING_ELT(type, 0)), variants, R_NilValue);
  return TYPEOF(declared) == ENVSXP &&
         R_existsVarInFrame(declared, Rf_installTrChar(name));
}

// Whether the enum value `x` is of the variant `variant`, one string that the
// enum declares. R8's `other_variant()` decides what the variants read so far
// do not: it reads the declaration, or raises the error for `x` or
// `variant`.
// spec: R8/class-system#values-in-r
static SEXP r8_is_variant(SEXP x, SEXP variant, SEXP variants) {
  SEXP v = r8_c_variant(x);
  if (v != R_NilValue && TYPEOF(variant) == STRSXP && XLENGTH(variant) == 1) {
    SEXP name = STRING_ELT(variant, 0);
    if (name != NA_STRING && LENGTH(name) > 0 && Rf_getCharCE(name) != CE_BYTES) {
      if (name == STRING_ELT(v, 0) ||
          strcmp(Rf_translateCharUTF8(name), Rf_translateCharUTF8(STRING_ELT(v, 0))) == 0) {
        return Rf_ScalarLogical(TRUE);
      }
      if (declares_variant(x, name, variants)) {
        return Rf_ScalarLogical(FALSE);
      }
    }
  }
  SEXP ns = PROTECT(R_FindNamespace(PROTECT(Rf_mkString("R8"))));
  SEXP call =
    PROTECT(Rf_lang3(Rf_install("other_variant"), PROTECT(quoted(x)), PROTECT(quoted(variant))));
  SEXP out = Rf_eval(call, ns);
  UNPROTECT(5);
  return out;
}

// NULL for a value of the registered declaration of its type and for any other
// object; R8's `other_abort()` raises the error for a value of another
// declaration.
static SEXP r8_require(SEXP x) {
  SEXP id = r8_value_id(x);
  if (id != R_NilValue && !r8_identity_current(id)) {
    SEXP ns = PROTECT(R_FindNamespace(PROTECT(Rf_mkString("R8"))));
    r8_abort3("other_abort", x, R_NilValue, R_NilValue, ns);
    UNPROTECT(2);
  }
  return R_NilValue;
}

// The C routines that R calls through `.Call()` on a native symbol:
// list(invoke, exports, construct, find, replaced, find2, find3, call,
// result, upcast, variant, is_variant, type, variant_of, is_value, names,
// require, get, fields, check, native). R8's `.Call` routines
// are the Rust code's, registered by extendr; these continue a jump, call R_GetCCallable(), evaluate R code, or
// read the fields of Rust-stored values from C frames.
SEXP r8_native_symbols(void) {
  SEXP out = PROTECT(Rf_allocVector(VECSXP, 21));
  SET_VECTOR_ELT(out, 0, native_symbol((DL_FUNC) r8_invoke));
  SET_VECTOR_ELT(out, 1, native_symbol((DL_FUNC) r8_package_exports));
  SET_VECTOR_ELT(out, 2, native_symbol((DL_FUNC) r8_construct_frame));
  SET_VECTOR_ELT(out, 3, native_symbol((DL_FUNC) r8_find));
  SET_VECTOR_ELT(out, 4, native_symbol((DL_FUNC) r8_replaced));
  SET_VECTOR_ELT(out, 5, native_symbol((DL_FUNC) r8_find2));
  SET_VECTOR_ELT(out, 6, native_symbol((DL_FUNC) r8_find3));
  SET_VECTOR_ELT(out, 7, native_symbol((DL_FUNC) r8_call));
  SET_VECTOR_ELT(out, 8, native_symbol((DL_FUNC) r8_result));
  SET_VECTOR_ELT(out, 9, native_symbol((DL_FUNC) r8_upcast));
  SET_VECTOR_ELT(out, 10, native_symbol((DL_FUNC) r8_variant));
  SET_VECTOR_ELT(out, 11, native_symbol((DL_FUNC) r8_is_variant));
  SET_VECTOR_ELT(out, 12, native_symbol((DL_FUNC) r8_c_type));
  SET_VECTOR_ELT(out, 13, native_symbol((DL_FUNC) r8_c_variant));
  SET_VECTOR_ELT(out, 14, native_symbol((DL_FUNC) r8_c_is_value));
  SET_VECTOR_ELT(out, 15, native_symbol((DL_FUNC) r8_c_names));
  SET_VECTOR_ELT(out, 16, native_symbol((DL_FUNC) r8_require));
  SET_VECTOR_ELT(out, 17, native_symbol((DL_FUNC) r8_c_get));
  SET_VECTOR_ELT(out, 18, native_symbol((DL_FUNC) r8_c_fields));
  SET_VECTOR_ELT(out, 19, native_symbol((DL_FUNC) r8_check_call));
  SET_VECTOR_ELT(out, 20, native_symbol((DL_FUNC) r8_native));
  SEXP names = PROTECT(Rf_allocVector(STRSXP, 21));
  SET_STRING_ELT(names, 0, Rf_mkChar("invoke"));
  SET_STRING_ELT(names, 1, Rf_mkChar("exports"));
  SET_STRING_ELT(names, 2, Rf_mkChar("construct"));
  SET_STRING_ELT(names, 3, Rf_mkChar("find"));
  SET_STRING_ELT(names, 4, Rf_mkChar("replaced"));
  SET_STRING_ELT(names, 5, Rf_mkChar("find2"));
  SET_STRING_ELT(names, 6, Rf_mkChar("find3"));
  SET_STRING_ELT(names, 7, Rf_mkChar("call"));
  SET_STRING_ELT(names, 8, Rf_mkChar("result"));
  SET_STRING_ELT(names, 9, Rf_mkChar("upcast"));
  SET_STRING_ELT(names, 10, Rf_mkChar("variant"));
  SET_STRING_ELT(names, 11, Rf_mkChar("is_variant"));
  SET_STRING_ELT(names, 12, Rf_mkChar("type"));
  SET_STRING_ELT(names, 13, Rf_mkChar("variant_of"));
  SET_STRING_ELT(names, 14, Rf_mkChar("is_value"));
  SET_STRING_ELT(names, 15, Rf_mkChar("names"));
  SET_STRING_ELT(names, 16, Rf_mkChar("require"));
  SET_STRING_ELT(names, 17, Rf_mkChar("get"));
  SET_STRING_ELT(names, 18, Rf_mkChar("fields"));
  SET_STRING_ELT(names, 19, Rf_mkChar("check"));
  SET_STRING_ELT(names, 20, Rf_mkChar("native"));
  Rf_setAttrib(out, R_NamesSymbol, names);
  UNPROTECT(2);
  return out;
}
