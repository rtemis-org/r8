// Attribute reads and the payload of a vector for the Rust code, which
// receives these functions as pointers from R_init_R8() (entrypoint.c). From
// R 4.6.0 they use the attribute API (R_getAttribCount(), R_mapAttrib()) and
// the ALTREP class API (R_altrep_class_name(), R_altrep_class_package()); on
// earlier R they walk the attribute list.

#include <string.h>

#include <Rinternals.h>
#include <Rversion.h>

#if R_VERSION >= R_Version(4, 6, 0)

R_xlen_t r8_attrib_count(SEXP x) {
    return R_getAttribCount(x);
}

struct r8_find {
    SEXP name;
    SEXP value;
};

static SEXP r8_find_attrib(SEXP tag, SEXP value, void *data) {
    struct r8_find *f = (struct r8_find *) data;
    if (tag == f->name) {
        f->value = value;
    }
    return NULL;
}

// The attribute `name` of `x` as stored, without the expansion of compact
// row names that Rf_getAttrib() applies; R_NilValue when absent.
SEXP r8_attrib_raw(SEXP x, SEXP name) {
    struct r8_find f = {name, R_NilValue};
    R_mapAttrib(x, r8_find_attrib, &f);
    return f.value;
}

// The vector whose elements `x` holds, for bulk reads: `x` itself, or, for
// the wrapper that base R puts around an ALTREP vector given attributes
// (`dim<-`, `structure()`, the class of a factor), the vector it wraps. The
// wrapper's data pointer and region methods allocate and fill a copy of a
// wrapped compact sequence; the wrapped vector reads by region in place.
SEXP r8_payload(SEXP x) {
    while (ALTREP(x)) {
        const char *cls = CHAR(PRINTNAME(R_altrep_class_name(x)));
        const char *pkg = CHAR(PRINTNAME(R_altrep_class_package(x)));
        if (strcmp(pkg, "base") != 0 || strncmp(cls, "wrap_", 5) != 0) {
            break;
        }
        x = R_altrep_data1(x);
    }
    return x;
}

#else

R_xlen_t r8_attrib_count(SEXP x) {
    R_xlen_t n = 0;
    for (SEXP a = ATTRIB(x); a != R_NilValue; a = CDR(a)) {
        n++;
    }
    return n;
}

SEXP r8_attrib_raw(SEXP x, SEXP name) {
    for (SEXP a = ATTRIB(x); a != R_NilValue; a = CDR(a)) {
        if (TAG(a) == name) {
            return CAR(a);
        }
    }
    return R_NilValue;
}

// Without the ALTREP class API, no ALTREP vector has a payload that bulk
// reads leave unmaterialized, wrappers included: R_NilValue tells the Rust
// code to read it by element.
SEXP r8_payload(SEXP x) {
    return ALTREP(x) ? R_NilValue : x;
}

#endif
