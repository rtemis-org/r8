// value.c
// ::R8::
// 2026- EDG rtemis.org

// R8 values. A value is an ALTLIST whose fields and identity are in its data
// slots, which R code does not reach; its attributes are `names` and `class`,
// for R's own functions.
//
// An R-stored value is of the class `r8_value`: data1 is the list of its fields,
// and data2 its identity, or R_NilValue once an element was written, which makes
// the object a list as any other. A Rust-stored value is of the class `r8_rust`
// (rust_stored.c).
//
// An identity is a list that every value of one declaration, and of one variant
// of an enum, shares, kept for the session in `identities`:
//
//   type         the qualified name, one string
//   variant      the variant of an enum, one string, or NULL
//   names        the names of the fields
//   declaration  the symbol that names the declaration
//   record       the text of the declaration, a list of symbols
//   like         a list with the attributes of a value
//   stamp        c(generation, current), what value.rs found at a generation
//                of the registry
//
// spec: R8/class-system#values-in-r

#define R8_INTERNAL_ABI
#include <R8.h>
#include <R_ext/Altrep.h>

enum {
  ID_TYPE,
  ID_VARIANT,
  ID_NAMES,
  ID_DECLARATION,
  ID_LIKE,
  ID_RECORD,
  ID_STAMP,
  ID_LENGTH
};

static R_altrep_class_t value_class;

// The identities of the session, an environment, by the text of their parts.
static SEXP identities = NULL;

// The identity in a Rust-stored value, or R_NilValue for any other object
// (rust_stored.c).
SEXP r8_rust_stored_id(SEXP x);

// The value of `sym` in `env`, or `absent` (view.c).
SEXP r8_get_var(SEXP sym, SEXP env, SEXP absent);

static int same_strings(SEXP a, SEXP b) {
  if (a == b) {
    return 1;
  }
  if (TYPEOF(a) != STRSXP || TYPEOF(b) != STRSXP || XLENGTH(a) != XLENGTH(b)) {
    return 0;
  }
  R_xlen_t n = XLENGTH(a);
  for (R_xlen_t i = 0; i < n; i++) {
    if (STRING_ELT(a, i) != STRING_ELT(b, i)) {
      return 0;
    }
  }
  return 1;
}

// A list with the attributes `names` and `cls`.
static SEXP like_list(SEXP names, SEXP cls) {
  SEXP like = PROTECT(Rf_allocVector(VECSXP, XLENGTH(names)));
  Rf_setAttrib(like, R_NamesSymbol, names);
  Rf_setAttrib(like, R_ClassSymbol, cls);
  MARK_NOT_MUTABLE(names);
  if (cls != R_NilValue) {
    MARK_NOT_MUTABLE(cls);
  }
  MARK_NOT_MUTABLE(like);
  UNPROTECT(1);
  return like;
}

// The identity that the session holds of the values of `type`, of the variant
// `variant` or R_NilValue, whose declaration is `declaration`, a symbol;
// R_NilValue when it holds none.
SEXP r8_held_identity(SEXP type, SEXP variant, SEXP declaration) {
  SEXP held = r8_get_var(declaration, identities, R_NilValue);
  R_xlen_t n = held == R_NilValue ? 0 : XLENGTH(held);
  for (R_xlen_t i = 0; i < n; i++) {
    SEXP id = VECTOR_ELT(held, i);
    if (same_strings(VECTOR_ELT(id, ID_TYPE), type) &&
        same_strings(VECTOR_ELT(id, ID_VARIANT), variant)) {
      return id;
    }
  }
  return R_NilValue;
}

// The identity of the values of `type`, and of the variant `variant` or
// R_NilValue, whose fields are `names` and whose declaration is `declaration`, a
// symbol, with the text `record`, a list of symbols: the one that the session
// holds, or a new one, which takes the vectors. `cls` is the class vector of its values; that of a registered
// declaration (`registered`) replaces another that the identity holds, which an
// object read from a stream gave it.
SEXP r8_identity(SEXP type, SEXP variant, SEXP names, SEXP cls, SEXP declaration, SEXP record,
                 int registered) {
  SEXP found = r8_held_identity(type, variant, declaration);
  if (found != R_NilValue) {
    SEXP like = VECTOR_ELT(found, ID_LIKE);
    if (registered && !same_strings(Rf_getAttrib(like, R_ClassSymbol), cls)) {
      SET_VECTOR_ELT(found, ID_LIKE, like_list(VECTOR_ELT(found, ID_NAMES), cls));
    }
    return found;
  }
  SEXP held = PROTECT(r8_get_var(declaration, identities, R_NilValue));
  R_xlen_t n = held == R_NilValue ? 0 : XLENGTH(held);
  SEXP id = PROTECT(Rf_allocVector(VECSXP, ID_LENGTH));
  SET_VECTOR_ELT(id, ID_TYPE, type);
  SET_VECTOR_ELT(id, ID_VARIANT, variant);
  SET_VECTOR_ELT(id, ID_NAMES, names);
  SET_VECTOR_ELT(id, ID_DECLARATION, declaration);
  SET_VECTOR_ELT(id, ID_RECORD, record);
  SET_VECTOR_ELT(id, ID_LIKE, like_list(names, cls));
  SEXP stamp = Rf_allocVector(REALSXP, 2);
  SET_VECTOR_ELT(id, ID_STAMP, stamp);
  REAL(stamp)[0] = -1;
  REAL(stamp)[1] = 0;
  MARK_NOT_MUTABLE(type);
  if (variant != R_NilValue) {
    MARK_NOT_MUTABLE(variant);
  }
  SEXP grown = PROTECT(Rf_allocVector(VECSXP, n + 1));
  for (R_xlen_t i = 0; i < n; i++) {
    SET_VECTOR_ELT(grown, i, VECTOR_ELT(held, i));
  }
  SET_VECTOR_ELT(grown, n, id);
  Rf_defineVar(declaration, grown, identities);
  UNPROTECT(3);
  return id;
}

// Whether the attributes of `x`, which R code can set, are those of a value of
// the identity `id`: the names of its fields, and a class vector that holds
// `r8_value`, which unclass() removes.
static int has_attributes(SEXP x, SEXP id) {
  SEXP names = Rf_getAttrib(x, R_NamesSymbol);
  SEXP own = VECTOR_ELT(id, ID_NAMES);
  if (names != own && !(names == R_NilValue ? XLENGTH(own) == 0 : same_strings(names, own))) {
    return 0;
  }
  SEXP cls = Rf_getAttrib(x, R_ClassSymbol);
  return cls == Rf_getAttrib(VECTOR_ELT(id, ID_LIKE), R_ClassSymbol) ||
         Rf_inherits(x, "r8_value");
}

// The identity of `x`, or R_NilValue for an object that is not a value: one
// of another class, one whose elements were written, and one without the names
// or the class of a value.
SEXP r8_value_id(SEXP x) {
  if (!ALTREP(x)) {
    return R_NilValue;
  }
  SEXP id = R_altrep_inherits(x, value_class) ? R_altrep_data2(x) : r8_rust_stored_id(x);
  return id != R_NilValue && has_attributes(x, id) ? id : R_NilValue;
}

// The list from which the fields of `x` are read: the fields of an R-stored
// value, and `x` itself for any other object.
SEXP r8_value_fields(SEXP x) {
  return ALTREP(x) && R_altrep_inherits(x, value_class) ? R_altrep_data1(x) : x;
}

// A value of the identity `id` over `fields`, a list without attributes that
// nothing else writes.
SEXP r8_value_new(SEXP fields, SEXP id) {
  MARK_NOT_MUTABLE(fields);
  SEXP x = PROTECT(R_new_altrep(value_class, fields, id));
  SHALLOW_DUPLICATE_ATTRIB(x, VECTOR_ELT(id, ID_LIKE));
  UNPROTECT(1);
  return x;
}

static R_xlen_t value_Length(SEXP x) { return XLENGTH(R_altrep_data1(x)); }

static SEXP value_Elt(SEXP x, R_xlen_t i) { return VECTOR_ELT(R_altrep_data1(x), i); }

// An element that is written again with itself, as data.table::copy() does,
// leaves the value as it is. Any other element gives the object a list of its
// own and takes its identity.
static void value_Set_elt(SEXP x, R_xlen_t i, SEXP v) {
  if (VECTOR_ELT(R_altrep_data1(x), i) == v) {
    return;
  }
  if (R_altrep_data2(x) != R_NilValue) {
    PROTECT(v);
    R_set_altrep_data1(x, Rf_shallow_duplicate(R_altrep_data1(x)));
    R_set_altrep_data2(x, R_NilValue);
    UNPROTECT(1);
  }
  SET_VECTOR_ELT(R_altrep_data1(x), i, v);
}

// A copy of a value shares its fields; R copies the attributes.
static SEXP value_Duplicate(SEXP x, Rboolean deep) {
  SEXP id = R_altrep_data2(x);
  if (id != R_NilValue) {
    return R_new_altrep(value_class, R_altrep_data1(x), id);
  }
  SEXP fields = PROTECT(Rf_shallow_duplicate(R_altrep_data1(x)));
  SEXP ans = R_new_altrep(value_class, fields, R_NilValue);
  UNPROTECT(1);
  return ans;
}

// The parts of an identity in a stream: symbols, which R writes once per stream
// and refers to afterwards, so a declaration is written once for any number
// of values.
static SEXP as_symbol(SEXP string) {
  return string == R_NilValue ? R_NilValue : Rf_installTrChar(STRING_ELT(string, 0));
}

// list(type, variant, declaration, record), of the identity `id`.
SEXP r8_identity_state(SEXP id) {
  SEXP out = PROTECT(Rf_allocVector(VECSXP, 4));
  SET_VECTOR_ELT(out, 0, as_symbol(VECTOR_ELT(id, ID_TYPE)));
  SET_VECTOR_ELT(out, 1, as_symbol(VECTOR_ELT(id, ID_VARIANT)));
  SET_VECTOR_ELT(out, 2, VECTOR_ELT(id, ID_DECLARATION));
  SET_VECTOR_ELT(out, 3, VECTOR_ELT(id, ID_RECORD));
  UNPROTECT(1);
  return out;
}

static SEXP as_string(SEXP sym) {
  return sym == R_NilValue ? R_NilValue : Rf_ScalarString(PRINTNAME(sym));
}

// Refuses attributes of another form than a pairlist of named elements, as those
// of a damaged stream.
static void check_attributes(SEXP attr) {
  SEXP a = attr;
  while (TYPEOF(a) == LISTSXP) {
    if (TYPEOF(TAG(a)) != SYMSXP) {
      break;
    }
    a = CDR(a);
  }
  if (a != R_NilValue) {
    Rf_error("a saved R8 value is damaged");
  }
}

// The attribute `name` of the pairlist `attr`.
static SEXP attribute(SEXP attr, SEXP name) {
  for (SEXP a = attr; a != R_NilValue; a = CDR(a)) {
    if (TAG(a) == name) {
      return CAR(a);
    }
  }
  return R_NilValue;
}

static int is_symbols(SEXP x) {
  if (x == R_NilValue) {
    return 1;
  }
  if (TYPEOF(x) != VECSXP) {
    return 0;
  }
  for (R_xlen_t i = 0; i < XLENGTH(x); i++) {
    if (TYPEOF(VECTOR_ELT(x, i)) != SYMSXP) {
      return 0;
    }
  }
  return 1;
}

// The identity that `state` names, of an object with the attributes `attr` and
// of `n` fields, or of any number where `n` is negative; an R error for a state
// of another form.
SEXP r8_identity_of_state(SEXP state, SEXP attr, R_xlen_t n) {
  check_attributes(attr);
  SEXP names = attribute(attr, R_NamesSymbol);
  SEXP cls = attribute(attr, R_ClassSymbol);
  if (TYPEOF(state) != VECSXP || XLENGTH(state) != 4 || TYPEOF(VECTOR_ELT(state, 0)) != SYMSXP ||
      TYPEOF(VECTOR_ELT(state, 2)) != SYMSXP ||
      (VECTOR_ELT(state, 1) != R_NilValue && TYPEOF(VECTOR_ELT(state, 1)) != SYMSXP) ||
      !is_symbols(VECTOR_ELT(state, 3)) || (cls != R_NilValue && TYPEOF(cls) != STRSXP) ||
      (names != R_NilValue && TYPEOF(names) != STRSXP)) {
    Rf_error("a saved R8 value is damaged");
  }
  PROTECT(names = names == R_NilValue ? Rf_allocVector(STRSXP, 0) : names);
  SEXP type = PROTECT(as_string(VECTOR_ELT(state, 0)));
  SEXP variant = PROTECT(as_string(VECTOR_ELT(state, 1)));
  SEXP id = r8_identity(type, variant, names, cls, VECTOR_ELT(state, 2), VECTOR_ELT(state, 3), 0);
  if ((n >= 0 && XLENGTH(names) != n) || !same_strings(VECTOR_ELT(id, ID_NAMES), names)) {
    Rf_error("a saved R8 value is damaged");
  }
  UNPROTECT(3);
  return id;
}

// Sets the attributes `attr` of an object read from a stream on `ans`, whose
// identity is `id`, with the vectors that its values share where they are
// equal.
void r8_set_read_attributes(SEXP ans, SEXP id, SEXP attr) {
  SEXP like = VECTOR_ELT(id, ID_LIKE);
  SHALLOW_DUPLICATE_ATTRIB(ans, like);
  SEXP cls = attribute(attr, R_ClassSymbol);
  if (!same_strings(cls, Rf_getAttrib(like, R_ClassSymbol))) {
    Rf_setAttrib(ans, R_ClassSymbol, cls);
  }
  for (SEXP a = attr; a != R_NilValue; a = CDR(a)) {
    if (TAG(a) != R_NamesSymbol && TAG(a) != R_ClassSymbol) {
      Rf_setAttrib(ans, TAG(a), CAR(a));
    }
  }
}

// list(identity, fields); NULL, which serializes a list, for an object that is
// not a value.
static SEXP value_Serialized_state(SEXP x) {
  SEXP id = r8_value_id(x);
  if (id == R_NilValue) {
    return NULL;
  }
  SEXP out = PROTECT(Rf_allocVector(VECSXP, 2));
  SET_VECTOR_ELT(out, 0, r8_identity_state(id));
  SET_VECTOR_ELT(out, 1, R_altrep_data1(x));
  UNPROTECT(1);
  return out;
}

// The R-stored value of a state list(identity, fields).
SEXP r8_value_of_state(SEXP state, SEXP attr) {
  if (TYPEOF(state) != VECSXP || XLENGTH(state) != 2 || TYPEOF(VECTOR_ELT(state, 1)) != VECSXP) {
    Rf_error("a saved R8 value is damaged");
  }
  SEXP fields = VECTOR_ELT(state, 1);
  SEXP id = PROTECT(r8_identity_of_state(VECTOR_ELT(state, 0), attr, XLENGTH(fields)));
  MARK_NOT_MUTABLE(fields);
  SEXP ans = PROTECT(R_new_altrep(value_class, fields, id));
  r8_set_read_attributes(ans, id, attr);
  UNPROTECT(2);
  return ans;
}

static SEXP value_UnserializeEX(SEXP cls, SEXP state, SEXP attr, int objf, int levs) {
  (void) cls;
  (void) objf;
  (void) levs;
  return r8_value_of_state(state, attr);
}

static Rboolean value_Inspect(SEXP x, int pre, int deep, int pvec,
                              void (*inspect_subtree)(SEXP, int, int, int)) {
  SEXP id = R_altrep_data2(x);
  Rprintf(" r8 value %s (fields=%lld)\n",
          id == R_NilValue ? "written, a list" : CHAR(STRING_ELT(VECTOR_ELT(id, ID_TYPE), 0)),
          (long long) XLENGTH(R_altrep_data1(x)));
  return TRUE;
}

void r8_init_values(DllInfo *dll) {
  identities = R_NewEnv(R_EmptyEnv, TRUE, 64);
  R_PreserveObject(identities);
  value_class = R_make_altlist_class("r8_value", "R8", dll);
  R_set_altrep_Length_method(value_class, value_Length);
  R_set_altrep_Inspect_method(value_class, value_Inspect);
  R_set_altrep_Duplicate_method(value_class, value_Duplicate);
  R_set_altrep_Serialized_state_method(value_class, value_Serialized_state);
  R_set_altrep_UnserializeEX_method(value_class, value_UnserializeEX);
  R_set_altlist_Elt_method(value_class, value_Elt);
  R_set_altlist_Set_elt_method(value_class, value_Set_elt);
}

// The qualified type name of a value, one string, the variant of an enum's
// value, and whether `x` is a value, for R.
SEXP r8_c_type(SEXP x) {
  SEXP id = r8_value_id(x);
  return id == R_NilValue ? R_NilValue : VECTOR_ELT(id, ID_TYPE);
}

SEXP r8_c_variant(SEXP x) {
  SEXP id = r8_value_id(x);
  return id == R_NilValue ? R_NilValue : VECTOR_ELT(id, ID_VARIANT);
}

SEXP r8_c_is_value(SEXP x) { return Rf_ScalarLogical(r8_value_id(x) != R_NilValue); }

// The names of the fields of a value, from its identity.
SEXP r8_c_names(SEXP x) {
  SEXP id = r8_value_id(x);
  return id == R_NilValue ? R_NilValue : VECTOR_ELT(id, ID_NAMES);
}
