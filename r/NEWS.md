# R8 0.2.0

- `generic()` declares a generic, and `implement(generic, Type) <- f` declares its method for an R8 type; `r8_method()` returns a declared method.
- `implement()` declares methods of `format()`, `str()`, `dim()`, `r8_cmp()`, and `print()` as impls of the standard traits `Display`, `Debug`, `Dim`, `PartialOrd`, and `Print`, and methods of other S3 generics as S3 methods for the type's qualified class.
- The roxygen2 roclet `r8_rd()` documents each method of a generic in the generic's or the type's Rd file.
- Struct and enum variant constructors give a required field no default argument.
- The package builds against `rtemis-r8` 0.2.0.

# R8 0.1.0

- `r8_core_version()` returns the version of the `rtemis-r8` crate compiled into the package.
