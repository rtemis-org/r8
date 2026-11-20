# R8 0.4.0

- Rust-native enums: `#[derive(R8)]` with `#[r8(native)]` on an enum of unit, struct, and newtype variants keeps every value, unit variants included, in compiled code. `new_enum(native = TRUE)` declares one in R, and `r8_document_native()` writes it; a package with a native enum lists `R8 (>= 0.4.0)` in `Imports`.
- `R8.h`: an enum's codec sets the new members `variant` and `from_variant` of `R8TypeVTable`. The ABI version stays 1, and codecs built for R8 0.3.0 register unchanged.
- `r8_register()` refuses a codec whose shape does not match its type, and a codec whose enum declares a runtime-only field in any variant.
- `r8::exports!` declares every type of the package's namespace that its listed types reach, such as the struct of a newtype variant.
- The package builds against `rtemis-r8` 0.4.0.

# R8 0.3.0

- Rust-native types: a struct whose codec is in a package's compiled exports keeps its values in compiled code. Field reads return views without copying, `x$name <- v` replaces a field, and values persist with `saveRDS()` and cross to mirai and future workers.
- `r8_size()` reports the memory a value holds. R8 collects garbage when the memory of Rust-native values grows.
- The crate `rtemis-r8-api` and the macros `#[r8::methods]` and `r8::exports!` implement R8 traits and declare Rust-native types in a package's Rust code; `R8.h` gives C and C++ code the same services (`R8Api`), and a package registers its exports with `R_RegisterCCallable()`.
- `new_struct(native = TRUE)` declares a Rust-native type; `r8_document_native()` writes the R declarations of the types a package declares in Rust, and `r8_register()` checks that they match the compiled exports.
- `json_ir()` gives a rule or a default as JSON text, for the JSONLogic and defaults that the R forms do not express.
- Compact sequences, factor codes, and sparse matrix indices are read in place, without expansion, by construction, rules, hashing, validation, equality, and documents.
- The package builds against `rtemis-r8` 0.3.0.

# R8 0.2.0

- `generic()` declares a generic, and `implement(generic, Type) <- f` declares its method for an R8 type; `r8_method()` returns a declared method.
- `implement()` declares methods of `format()`, `str()`, `dim()`, `r8_cmp()`, and `print()` as impls of the standard traits `Display`, `Debug`, `Dim`, `PartialOrd`, and `Print`, and methods of other S3 generics as S3 methods for the type's qualified class.
- The roxygen2 roclet `r8_rd()` documents each method of a generic in the generic's or the type's Rd file.
- Struct and enum variant constructors give a required field no default argument.
- The package builds against `rtemis-r8` 0.2.0.

# R8 0.1.0

- `r8_core_version()` returns the version of the `rtemis-r8` crate compiled into the package.
