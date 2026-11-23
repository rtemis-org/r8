/*
 * R8.h -- the C interface between R packages and the R8 registry.
 *
 * ABI version 1. The Rust types in `rtemis_r8::abi` mirror these declarations
 * field for field; the crate `rtemis-r8-abi-check` compiles this header and asserts
 * that every size and offset matches.
 *
 * Two directions:
 *
 * 1. Exports, from a package to R8. A package with compiled exports calls
 *    `R_RegisterCCallable("<package>", "r8_exports", fn)` from its
 *    initialization routine, where `fn` is an `R8ExportsFn` returning its
 *    static `R8Exports`. `R8::r8_register()`, called from the package's
 *    `.onLoad`, fetches `fn` with `R_GetCCallable()` and registers the table:
 *    the IR document, the codecs of Rust-stored types, and the impl
 *    functions. Registering a package again replaces its entries.
 *
 * 2. The API, from R8 to a package. `R8_api()` returns R8's function table
 *    through `R_GetCCallable("R8", "r8_api")`.
 *
 * Rules for every function in this interface:
 *
 * - Every call happens on R's main thread.
 * - A callee never raises an R error and never calls `longjmp`. It returns
 *   `R8_ERROR` and fills the `R8Error` it receives; the caller raises the R
 *   condition. A Rust callee catches panics, since a panic that leaves an
 *   `extern "C"` function aborts the process. A callee calls an R function
 *   that can jump (an allocation, an interrupt check) through
 *   `R8Api.protect`; an impl function returns the `R8_UNWIND` it receives,
 *   and R8 continues the jump after the impl function has returned.
 * - An `SEXP` returned through an out-parameter is unprotected; the caller
 *   protects it before the next allocation.
 * - Each side frees only what it allocated. A type's `state` is freed only by
 *   that type's `drop`, and only code of the codec's `module` reads it;
 *   encoded JSON passes through an `R8Writer` supplied by the caller; error
 *   text is written into the fixed buffers of `R8Error`.
 * - Strings are UTF-8 and NUL-terminated.
 * - Everything an `R8Exports` points to is static: it stays valid while the
 *   package's shared object is loaded. R8 reads the declaration record and
 *   keeps the function pointers.
 * - `abi_version` is the major version: R8 rejects a table of another major
 *   version. Within a major version, fields are only appended to a struct,
 *   and `size` tells the reader which fields are present.
 */

#ifndef R8_H
#define R8_H

#include <stddef.h>
#include <stdint.h>

#include <Rinternals.h>
#include <R_ext/Rdynload.h>

#ifdef __cplusplus
extern "C" {
#endif

#define R8_ABI_VERSION 1u

/* Return status of every function that can fail. */
typedef int32_t R8Status;
#define R8_OK 0
#define R8_ERROR 1
/* R jumped (an error or an interrupt) inside `R8Api.protect`. */
#define R8_UNWIND 2

/* An error report, filled by the callee that returns R8_ERROR. `code` is a
 * stable code, such as "decode_failed"; `message` says what went wrong and
 * how to fix it. Both are truncated to fit and always NUL-terminated. */
typedef struct R8Error {
  char code[64];
  char message[1024];
} R8Error;

/* A sink for bytes, supplied by the caller of `encode`. */
typedef struct R8Writer {
  void *context;
  R8Status (*write)(void *context, const uint8_t *bytes, size_t len);
} R8Writer;

/* A part of a value that R8 has read and validated, lent to a codec for the
 * length of one call and read only through `R8Reader`. */
typedef const void *R8Node;

/* The kinds `R8Reader.kind` reports; a NULL node is absent. */
#define R8_NODE_ABSENT 0u
#define R8_NODE_NULL 1u
#define R8_NODE_LOGICAL 2u
#define R8_NODE_NUMBER 3u
#define R8_NODE_STRING 4u
#define R8_NODE_ARRAY 5u
#define R8_NODE_OBJECT 6u

/* The integer and logical a null reads as (R's NA_integer_), and the bits of
 * the double a null reads as (R's NA_real_, a NaN whose low word is 1954). */
#define R8_NA_INTEGER INT32_MIN
#define R8_NA_DOUBLE_BITS UINT64_C(0x7FF00000000007A2)

/* The functions through which a codec reads a value R8 lends it. The reader,
 * its nodes, and the strings it lends live for the call that receives them;
 * the codec writes block reads into memory it owns. */
typedef struct R8Reader {
  /* The size of this struct in bytes. */
  uint32_t size;
  /* The reader's state, passed to every function. */
  void *context;
  /* The node's kind, an R8_NODE_* constant. */
  uint32_t (*kind)(void *context, R8Node node);
  /* An object's member by name, or NULL when the object has none. */
  R8Node (*member)(void *context, R8Node node, const char *name);
  /* An array's item, or NULL past its end. */
  R8Node (*item)(void *context, R8Node node, size_t i);
  /* An array's or an object's length; 0 for any other node. */
  size_t (*length)(void *context, R8Node node);
  /* A logical: 0 or 1, and R8_NA_INTEGER for null. */
  R8Status (*logical)(void *context, R8Node node, int32_t *out, R8Error *err);
  /* A number: a non-finite double from its string form, and the double of
   * R8_NA_DOUBLE_BITS for null. */
  R8Status (*number)(void *context, R8Node node, double *out, R8Error *err);
  /* A string, UTF-8, lent with its length in bytes. */
  R8Status (*string)(void *context, R8Node node, const char **utf8, size_t *len, R8Error *err);
  /* Items start..start + count of an array of numbers, as `number` reads
   * each, into `out`. */
  R8Status (*doubles)(void *context, R8Node node, size_t start, size_t count, double *out,
                      R8Error *err);
  /* Items start..start + count of an array of whole numbers into `out`,
   * R8_NA_INTEGER for null. */
  R8Status (*integers)(void *context, R8Node node, size_t start, size_t count, int32_t *out,
                       R8Error *err);
  /* Items start..start + count of an array of logicals into `out`, as
   * `logical` reads each. */
  R8Status (*logicals)(void *context, R8Node node, size_t start, size_t count, int32_t *out,
                       R8Error *err);
} R8Reader;

/* Set in `R8CallInputs.flags` when `seed` holds a seed. */
#define R8_CALL_HAS_SEED 1u

/* Per-call inputs of an impl function. A stochastic method receives its
 * 64-bit seed here, given by the caller or derived by R8 from R's RNG
 * stream. */
typedef struct R8CallInputs {
  uint32_t size;
  uint32_t flags;
  uint64_t seed;
} R8CallInputs;

/* The codec of a Rust-stored type. `state` is the type's Rust value, owned
 * by R8 between `from_fields`, `from_reader`, or `with_field` and `drop`. A
 * package's `types` array holds codecs of one size, `size`, which is the
 * array's stride; fields are appended within a major version. */
typedef struct R8Codec {
  /* The size of this struct in bytes. */
  uint32_t size;
  /* Qualified name, "namespace::Type". */
  const char *type_name;
  /* The compiled code that builds and reads the codec's states: the address
   * of an object of the package's shared object, the same in each of its
   * codecs. A state crosses to compiled code that names the same module
   * only, since its layout is that code's. */
  const void *module;
  /* Builds a state from the type's fields, `n` R values in declaration
   * order, which R8 has validated and converted to their stored forms. */
  R8Status (*from_fields)(const SEXP *fields, size_t n, void **state, R8Error *err);
  /* Writes the state's JSON document, for export (`r8_to_json()`). */
  R8Status (*encode)(const void *state, R8Writer *out, R8Error *err);
  /* Writes the state's bytes, in a form of the package's choosing, for
   * `saveRDS()` and parallel workers. R8 calls it twice per serialization,
   * once to count the bytes and once to write them into a buffer of that
   * size, so every call writes the same bytes. */
  R8Status (*write_state)(const void *state, R8Writer *out, R8Error *err);
  /* Builds a state from bytes that `write_state` wrote. */
  R8Status (*read_state)(const uint8_t *bytes, size_t len, void **state, R8Error *err);
  /* Reads one field as an R value. `owner` is the external pointer that
   * holds `state`; a field that views the state's memory, through
   * `R8Api.view_doubles` and its siblings, passes it as the view's owner,
   * so the state lives as long as the view. R8 calls it once per field and
   * keeps the result. */
  R8Status (*get_field)(const void *state, SEXP owner, const char *field, SEXP *value, R8Error *err);
  /* Builds a new state with one field replaced by an R value that R8 has
   * validated; `state` is unchanged. The new state shares the buffers of
   * the unchanged fields, so a replacement copies no large field. */
  R8Status (*with_field)(const void *state, const char *field, SEXP value, void **new_state, R8Error *err);
  /* Frees a state. */
  void (*drop)(void *state);
  /* The bytes the state owns: its inline size and the memory it owns. */
  size_t (*owned_bytes)(const void *state);
  /* Enum codecs. An enum's codec sets `variant` and `from_variant` and
   * leaves `from_fields` NULL; a struct's codec sets `from_fields` and
   * leaves both NULL. R8 reads them when `size` covers them. */
  /* The name of the state's variant, NUL-terminated, alive as long as the
   * codec. */
  const char *(*variant)(const void *state);
  /* Builds a state of the variant named `variant` from its fields, `n` R
   * values in the variant's declaration order, which R8 has validated and
   * converted to their stored forms; a unit variant has none, and a newtype
   * variant has the fields of its struct. */
  R8Status (*from_variant)(const char *variant, const SEXP *fields, size_t n, void **state,
                           R8Error *err);
  /* Builds a state from the value R8 has read from a document and validated,
   * through the reader, from the value's root node. R8 reads a document
   * through it when `size` covers it and it is set, and through
   * `from_fields` or `from_variant` otherwise. */
  R8Status (*from_reader)(const R8Reader *reader, R8Node root, void **state, R8Error *err);
  /* The declaration hash of the type the codec was written against,
   * "sha256:<hex>": set by a codec written by hand in C, whose type has no
   * declaration in the exports' record, and NULL otherwise. Registration
   * requires it to equal the hash of the type's R declaration, since
   * `from_fields` and `from_variant` read fields by position; a mismatch
   * reports the declaration's current hash. */
  const char *declaration_hash;
  /* The state of the ancestor named `type_name` that the state embeds, which
   * lives as long as the state; NULL when it embeds none of that name. NULL
   * in the codec of a type whose states embed no ancestor. */
  const void *(*upcast)(const void *state, const char *type_name);
} R8Codec;

/* An impl function: a trait method implemented in compiled code. `args`
 * holds the call's arguments in the method's declared order. */
typedef R8Status (*R8MethodFn)(const R8CallInputs *inputs, const SEXP *args, size_t nargs,
                               SEXP *result, R8Error *err);

/* What a position of an impl names: an R8 type, a class of R, or any value. */
#define R8_POSITION_TYPE 0
#define R8_POSITION_CLASS 1
#define R8_POSITION_ANY 2

/* A position of an impl. */
typedef struct R8Position {
  uint32_t kind;
  /* The qualified name of a type, or the name of a class; NULL for any. */
  const char *name;
  /* Of a class: the package that defines it, which is NULL or empty when the
   * declaration names none. */
  const char *package;
  /* Of a class: its class system, "s3", "s4", or "implicit". */
  const char *system;
} R8Position;

/* One method of one impl. */
typedef struct R8ImplEntry {
  /* The qualified name of the trait. */
  const char *trait_name;
  /* The type the trait is implemented for, its first dispatched position, then
   * the trait's other dispatched positions in order; for R8::From, the type
   * and the source. The entries of one impl have equal positions. */
  const R8Position *positions;
  size_t n_positions;
  /* The method's name, as declared by the trait, or its identifier in the
   * languages other than R, such as `with_labels` for `labels<-`. */
  const char *method;
  R8MethodFn call;
  /* The number of the method's parameters, `x` included and `...` counted
   * as one, which registration checks against the trait's declaration. */
  size_t n_params;
} R8ImplEntry;

/* What a package registers: the declarations of its types in Rust, the
 * codecs of its Rust-stored types, and its impl functions. R8 builds each
 * impl declaration from the impl entries, grouped by trait and positions. */
typedef struct R8Exports {
  uint32_t abi_version;
  uint32_t size;
  /* The R package's name. */
  const char *package;
  /* The declarations of the package's types in Rust as a declaration record,
   * which rtemis-r8-api writes; NULL in a table written by hand in C, whose
   * types are the package's R declarations. */
  const uint8_t *ir_record;
  size_t ir_record_len;
  const R8Codec *types;
  size_t n_types;
  const R8ImplEntry *impls;
  size_t n_impls;
} R8Exports;

/* The function a package registers as "r8_exports". */
typedef const R8Exports *(*R8ExportsFn)(void);

/* R8's services to compiled code of other packages. */
typedef struct R8Api {
  uint32_t abi_version;
  uint32_t size;
  /* Wraps a state of a registered Rust-stored type, which the caller's
   * module built, as an R8 value. R8 owns the state once it returns R8_OK;
   * on an error the state stays the caller's. The code `other_module`
   * reports a type whose codec has another module. */
  R8Status (*new_value)(const char *type_name, const void *module, void *state, SEXP *value,
                        R8Error *err);
  /* Lends the state of a value of the named type to the caller's module; the
   * state stays valid while `value` is protected. The code `not_lent` reports
   * a value of the type whose state the module does not read: an R-stored
   * value, or one whose codec has another module, whose fields the caller
   * reads as R values instead; `type_mismatch` reports any other object. */
  R8Status (*value_state)(SEXP value, const char *type_name, const void *module,
                          const void **state, R8Error *err);
  /* `value_state`, and for a value of a struct that extends the named type,
   * the state of that type which the value's state embeds
   * (`R8Codec.upcast`). An impl function reads its receiver through it, since
   * an impl for a struct applies to the structs that extend it. */
  R8Status (*upcast_state)(SEXP value, const char *type_name, const void *module,
                           const void **state, R8Error *err);
  /* Views `n` elements at `data`, memory that `owner` keeps alive, as an R
   * vector, without copying. A write to the vector goes to a copy that R
   * makes; the memory is only read. */
  R8Status (*view_doubles)(SEXP owner, const double *data, size_t n, SEXP *value, R8Error *err);
  R8Status (*view_integers)(SEXP owner, const int32_t *data, size_t n, SEXP *value, R8Error *err);
  /* Logicals in R's form: 0, 1, and NA_LOGICAL as int32_t. */
  R8Status (*view_logicals)(SEXP owner, const int32_t *data, size_t n, SEXP *value, R8Error *err);
  R8Status (*view_bytes)(SEXP owner, const uint8_t *data, size_t n, SEXP *value, R8Error *err);
  /* Calls `fn(data)`, which may call R functions that jump, and stores its
   * result in `*value`. When R jumps, it returns R8_UNWIND and fills `err`;
   * R8 keeps the jump and continues it once the impl function that received
   * R8_UNWIND returns it. `fn` owns nothing that needs freeing, since a jump
   * skips the rest of it. */
  R8Status (*protect)(SEXP (*fn)(void *data), void *data, SEXP *value, R8Error *err);
} R8Api;

/* The entry point R8 registers with R_RegisterCCallable("R8", "r8_api").
 * Returns NULL when R8 does not provide `abi_version`. */
typedef const R8Api *(*R8ApiFn)(uint32_t abi_version);

/* R8's API for this header's ABI version, or NULL when the installed R8
 * does not provide it. The package imports R8, so R8 is loaded first. */
static inline const R8Api *R8_api(void) {
  /* The cast passes through `void (*)(void)`, the generic function pointer
   * type that GCC and clang accept as an intermediate between unrelated
   * function types. */
  R8ApiFn fn = (R8ApiFn) (void (*)(void)) R_GetCCallable("R8", "r8_api");
  return fn == NULL ? NULL : fn(R8_ABI_VERSION);
}

#ifdef __cplusplus
}
#endif

#endif /* R8_H */
