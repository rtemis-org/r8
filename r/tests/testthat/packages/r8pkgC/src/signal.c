// signal.c
// ::r8pkgC::
// 2026- EDG rtemis.org

// The codec of pkgc::Signal, a Rust-stored type implemented in C: its state
// holds the name and a copy of the values, and `values` reads as a view of
// the state's memory through R8Api.

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <R8.h>

typedef struct Signal {
  char *name;
  double *values;
  size_t n;
} Signal;

// The number of live states, for the tests of `drop`.
static int live = 0;

static void set_error(R8Error *err, const char *code, const char *message) {
  snprintf(err->code, sizeof(err->code), "%s", code);
  snprintf(err->message, sizeof(err->message), "%s", message);
}

static Signal *signal_new(const char *name, const double *values, size_t n) {
  Signal *s = malloc(sizeof(Signal));
  if (s == NULL) {
    return NULL;
  }
  s->name = malloc(strlen(name) + 1);
  s->values = malloc(n > 0 ? n * sizeof(double) : 1);
  if (s->name == NULL || s->values == NULL) {
    free(s->name);
    free(s->values);
    free(s);
    return NULL;
  }
  strcpy(s->name, name);
  if (n > 0) {
    memcpy(s->values, values, n * sizeof(double));
  }
  s->n = n;
  live++;
  return s;
}

static void signal_drop(void *state) {
  Signal *s = state;
  free(s->name);
  free(s->values);
  free(s);
  live--;
}

// Fields in declaration order: name (a string), values (doubles).
static R8Status signal_from_fields(const SEXP *fields, size_t n, void **state, R8Error *err) {
  if (n != 2) {
    set_error(err, "arity", "a Signal has two fields");
    return R8_ERROR;
  }
  Signal *s = signal_new(CHAR(STRING_ELT(fields[0], 0)), REAL_RO(fields[1]),
                         (size_t) XLENGTH(fields[1]));
  if (s == NULL) {
    set_error(err, "alloc", "out of memory");
    return R8_ERROR;
  }
  *state = s;
  return R8_OK;
}

static R8Status signal_get_field(const void *state, SEXP owner, const char *field,
                                 SEXP *value, R8Error *err) {
  const Signal *s = state;
  if (strcmp(field, "name") == 0) {
    *value = Rf_mkString(s->name);
    return R8_OK;
  }
  if (strcmp(field, "values") == 0) {
    const R8Api *api = R8_api();
    if (api == NULL) {
      set_error(err, "no_api", "R8 provides no API of this version");
      return R8_ERROR;
    }
    return api->view_doubles(owner, s->values, s->n, value, err);
  }
  set_error(err, "unknown_field", "a Signal has the fields name and values");
  return R8_ERROR;
}

static R8Status signal_with_field(const void *state, const char *field, SEXP value,
                                  void **new_state, R8Error *err) {
  const Signal *s = state;
  Signal *t;
  if (strcmp(field, "name") == 0) {
    t = signal_new(CHAR(STRING_ELT(value, 0)), s->values, s->n);
  } else if (strcmp(field, "values") == 0) {
    t = signal_new(s->name, REAL_RO(value), (size_t) XLENGTH(value));
  } else {
    set_error(err, "unknown_field", "a Signal has the fields name and values");
    return R8_ERROR;
  }
  if (t == NULL) {
    set_error(err, "alloc", "out of memory");
    return R8_ERROR;
  }
  *new_state = t;
  return R8_OK;
}

// State bytes: the name's length (uint64), the name, the number of values
// (uint64), and the values, in the host's byte order.
static R8Status signal_write_state(const void *state, R8Writer *out, R8Error *err) {
  const Signal *s = state;
  uint64_t len = strlen(s->name), n = s->n;
  if (out->write(out->context, (const uint8_t *) &len, sizeof(len)) != R8_OK ||
      out->write(out->context, (const uint8_t *) s->name, len) != R8_OK ||
      out->write(out->context, (const uint8_t *) &n, sizeof(n)) != R8_OK ||
      out->write(out->context, (const uint8_t *) s->values, n * sizeof(double)) != R8_OK) {
    set_error(err, "write_failed", "the writer refused the state");
    return R8_ERROR;
  }
  return R8_OK;
}

static R8Status signal_read_state(const uint8_t *bytes, size_t size, void **state,
                                  R8Error *err) {
  uint64_t len, n;
  if (size < sizeof(len)) {
    goto bad;
  }
  memcpy(&len, bytes, sizeof(len));
  if (size < sizeof(len) + len + sizeof(n)) {
    goto bad;
  }
  memcpy(&n, bytes + sizeof(len) + len, sizeof(n));
  if (size != sizeof(len) + len + sizeof(n) + n * sizeof(double)) {
    goto bad;
  }
  char *name = malloc(len + 1);
  double *values = malloc(n > 0 ? n * sizeof(double) : 1);
  if (name == NULL || values == NULL) {
    free(name);
    free(values);
    set_error(err, "alloc", "out of memory");
    return R8_ERROR;
  }
  memcpy(name, bytes + sizeof(len), len);
  name[len] = '\0';
  if (n > 0) {
    memcpy(values, bytes + sizeof(len) + len + sizeof(n), n * sizeof(double));
  }
  Signal *s = signal_new(name, values, n);
  free(name);
  free(values);
  if (s == NULL) {
    set_error(err, "alloc", "out of memory");
    return R8_ERROR;
  }
  *state = s;
  return R8_OK;
bad:
  set_error(err, "bad_state", "the bytes are not a Signal state");
  return R8_ERROR;
}

static size_t signal_bytes(const void *state) {
  const Signal *s = state;
  return sizeof(Signal) + strlen(s->name) + 1 + s->n * sizeof(double);
}

const R8TypeVTable pkgc_signal_codec = {
    .size = sizeof(R8TypeVTable),
    .type_name = "pkgc::Signal",
    .from_fields = signal_from_fields,
    .write_state = signal_write_state,
    .read_state = signal_read_state,
    .get_field = signal_get_field,
    .with_field = signal_with_field,
    .drop = signal_drop,
    .bytes = signal_bytes,
    // The hash of the R declaration of pkgc::Signal (R/types.R), whose
    // fields from_fields reads by position.
    .declaration_hash = "sha256:2f398c9789d687098a98bf034139fb1a55f7461062fb2a51a844de4cc9eef34a",
};

// The number of live Signal states.
SEXP pkgc_live_signals(void) {
  return Rf_ScalarInteger(live);
}

// The state of a Signal value, read through R8Api.value_state: its name and
// first value.
SEXP pkgc_signal_peek(SEXP x) {
  const R8Api *api = R8_api();
  const void *state = NULL;
  R8Error err = {{0}, {0}};
  if (api->value_state(x, "pkgc::Signal", &state, &err) != R8_OK) {
    Rf_error("%s [%s]", err.message, err.code);
  }
  const Signal *s = state;
  SEXP out = PROTECT(Rf_allocVector(VECSXP, 2));
  SET_VECTOR_ELT(out, 0, Rf_mkString(s->name));
  SET_VECTOR_ELT(out, 1, Rf_ScalarReal(s->n > 0 ? s->values[0] : NA_REAL));
  UNPROTECT(1);
  return out;
}

// A Signal built in C and wrapped through R8Api.new_value.
SEXP pkgc_signal_make(SEXP name, SEXP n) {
  const R8Api *api = R8_api();
  size_t k = (size_t) Rf_asInteger(n);
  double *values = malloc(k > 0 ? k * sizeof(double) : 1);
  for (size_t i = 0; i < k; i++) {
    values[i] = (double) i;
  }
  Signal *s = signal_new(CHAR(STRING_ELT(name, 0)), values, k);
  free(values);
  SEXP value = R_NilValue;
  R8Error err = {{0}, {0}};
  if (api->new_value("pkgc::Signal", s, &value, &err) != R8_OK) {
    Rf_error("%s [%s]", err.message, err.code);
  }
  return value;
}
