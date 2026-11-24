// abi.c
// ::r8pkgAbi::
// 2026- EDG rtemis.org

// Calls of the functions of R8's binary interface, from code that R8 does not
// know, and exports tables that R8 refuses, made from that of r8pkgR.

#include <string.h>

#define R8_INTERNAL_ABI
#include <R8.h>

// Identifies this shared object, whose exports R8 does not hold.
static const char abi_module = 0;

// An impl function that raises an R error, which R8Api.protect catches and R8
// continues once the function returns.
static SEXP raise_na(void *data) {
  (void) data;
  Rf_error("`by` is NA");
  return R_NilValue;
}

static R8Status raising_impl(const R8CallInputs *inputs, const SEXP *args, size_t nargs,
                             SEXP *result, R8Error *err) {
  (void) inputs;
  (void) args;
  (void) nargs;
  return R8_api()->protect(raise_na, NULL, result, err);
}

enum { MAX_IMPLS = 64, MAX_TYPES = 16 };

// The exports table of r8pkgR with one change, by `kind`: 1 repeats an entry, 2
// has a NULL function, 3 omits the methods of an impl but the first, 4 has no
// position, 5 a position of an unknown kind, 6 a class of R, 7 an unknown method, 8
// an entry without a signature, 9 a table of an earlier header, 10 entries of
// one, 11 no declaration record, 12 no module, 13 a codec of another module, 14
// a codec without `from_reader`, 15 a codec that sets an enum's members, 16 a
// codec without a declaration, and 17 an impl function that raises an R error.
static SEXP abi_table(SEXP kind) {
  static R8ImplEntry impls[MAX_IMPLS];
  static R8Codec types[MAX_TYPES];
  static R8Exports table;
  static R8Position position;
  int k = Rf_asInteger(kind);
  R8ExportsFn fn = (R8ExportsFn) (void (*)(void)) R_GetCCallable("r8pkgR", "r8_exports");
  table = *fn();
  if (table.n_impls > MAX_IMPLS || table.n_types > MAX_TYPES) {
    Rf_error("r8pkgR exports more entries than abi.c holds");
  }
  memcpy(impls, table.impls, table.n_impls * sizeof(R8ImplEntry));
  memcpy(types, table.types, table.n_types * sizeof(R8Codec));
  table.impls = impls;
  table.types = types;
  // The entries of pkgr::Norm, whose three methods come first, for Signal.
  position = *impls[0].positions;
  switch (k) {
  case 1:
    impls[1] = impls[0];
    break;
  case 2:
    impls[0].call = NULL;
    break;
  case 3:
    impls[2] = impls[0];
    table.impls = impls + 2;
    table.n_impls -= 2;
    break;
  case 4:
    impls[0].n_positions = 0;
    break;
  case 5:
    position.kind = 9;
    impls[0].positions = &position;
    break;
  case 6:
    position = (R8Position) {R8_POSITION_CLASS, "numeric", NULL, "implicit"};
    impls[0].positions = &position;
    break;
  case 7:
    impls[0].method = "norm";
    break;
  case 8:
    impls[0].signature = NULL;
    break;
  case 9:
    table.size = (uint32_t) offsetof(R8Exports, impl_size);
    break;
  case 10:
    table.impl_size = offsetof(R8ImplEntry, signature);
    break;
  case 11:
    table.ir_record = NULL;
    break;
  case 12:
    table.module = NULL;
    break;
  case 13:
    types[0].module = &abi_module;
    break;
  case 14:
    types[0].from_reader = NULL;
    break;
  case 15:
    types[0].variant = types[1].variant;
    types[0].from_variant = types[1].from_variant;
    break;
  case 16:
    types[0].type_name = "pkgr::None";
    break;
  case 17:
    impls[0].call = raising_impl;
    break;
  default:
    break;
  }
  return R_MakeExternalPtr(&table, Rf_install("r8_exports"), R_NilValue);
}

static SEXP abi_protect_status(void) {
  SEXP value = R_NilValue;
  R8Error err = {{0}, {0}};
  return Rf_ScalarInteger(R8_api()->protect(raise_na, NULL, &value, &err));
}

static SEXP abi_lend(SEXP x, SEXP type, SEXP upcast) {
  const R8Api *api = R8_api();
  const void *state = NULL;
  R8Error err = {{0}, {0}};
  const char *name = CHAR(STRING_ELT(type, 0));
  R8Status status = Rf_asLogical(upcast) == 1
                        ? api->upcast_state(x, name, &abi_module, &state, &err)
                        : api->value_state(x, name, &abi_module, &state, &err);
  return Rf_mkString(status == R8_OK ? "" : err.code);
}

static SEXP abi_wrap(SEXP type) {
  // A state that R8 refuses stays the caller's.
  static char state = 0;
  SEXP value = R_NilValue;
  R8Error err = {{0}, {0}};
  R8Status status =
      R8_api()->new_value(CHAR(STRING_ELT(type, 0)), &abi_module, &state, &value, &err);
  return Rf_mkString(status == R8_OK ? "" : err.code);
}

static SEXP abi_of_fields(SEXP type, SEXP variant, SEXP hash, SEXP fields) {
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

static SEXP abi_with_fields(SEXP x, SEXP fields) {
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

static SEXP abi_pin(SEXP record) {
  R8Error err = {{0}, {0}};
  R8Status status =
      R8_api()->pin(&abi_module, RAW(record), (size_t) XLENGTH(record), &err);
  return Rf_mkString(status == R8_OK ? "" : err.code);
}

static const R_CallMethodDef call_methods[] = {
    {"abi_table", (DL_FUNC) &abi_table, 1},
    {"abi_protect_status", (DL_FUNC) &abi_protect_status, 0},
    {"abi_lend", (DL_FUNC) &abi_lend, 3},
    {"abi_wrap", (DL_FUNC) &abi_wrap, 1},
    {"abi_of_fields", (DL_FUNC) &abi_of_fields, 4},
    {"abi_with_fields", (DL_FUNC) &abi_with_fields, 2},
    {"abi_pin", (DL_FUNC) &abi_pin, 1},
    {NULL, NULL, 0},
};

void R_init_r8pkgAbi(DllInfo *dll) {
  R_registerRoutines(dll, NULL, call_methods, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
