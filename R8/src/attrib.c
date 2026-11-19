// Attribute reads for the Rust code, which receives these functions as
// pointers from R_init_R8() (entrypoint.c). From R 4.6.0 they use the
// attribute API (R_getAttribCount(), R_mapAttrib()); on earlier R they walk
// the attribute list.

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

#endif
