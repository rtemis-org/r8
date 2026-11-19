/* Stub of R's R_ext/Rdynload.h: the declarations R8.h uses, as R declares them. */
#ifndef R_EXT_DYNLOAD_H_
#define R_EXT_DYNLOAD_H_
typedef void *(*DL_FUNC)(void);
DL_FUNC R_GetCCallable(const char *package, const char *name);
#endif
