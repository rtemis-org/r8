# r8-derive

`#[derive(R8)]`, which declares a Rust struct or enum as an R8 type: it implements `r8_core::derive::Encode` and `r8_core::derive::R8`, which give the type's declaration, in the same IR a declaration in R produces, and its encoding to and from the JSON form that `r8-core` validates. `register()` adds the declaration, and those of the types its fields reach, to an `r8_core::rules::DocumentSet`.

Use it through `r8-core` with the `derive` feature, which re-exports the macro.

## License

BSD-3-Clause.
