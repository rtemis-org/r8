# rtemis-r8-macros

The procedural macros of the R8 type system. `#[derive(R8)]` declares a Rust struct or enum as an R8 type: it implements `rtemis_r8::derive::Encode` and `rtemis_r8::derive::R8`, which give the type's declaration, in the same IR a declaration in R produces, and its encoding to and from the JSON form that `rtemis-r8` validates. `register()` adds the declaration, and those of the types its fields reach, to an `rtemis_r8::rules::DocumentSet`. A struct extends another by embedding it as its first field, marked `#[r8(extends)]`; the parent, declared with `#[r8(extensible)]`, has an interface trait that every struct that extends it implements.

Use the macros through `rtemis-r8` with the `macros` feature, which re-exports them. The generated code names the crate `::rtemis_r8`, so a crate that uses the macros depends on `rtemis-r8` under that name, without `package =` renaming.

## License

BSD-3-Clause.
