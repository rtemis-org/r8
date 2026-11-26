# rtemis-r8-io

Readers of table files for the R8 type system. A cell is read as the type that its column declares, and each text that does not read is collected with its count and its first rows. A column without a declared type has the type that all its cells have, and is text otherwise.

The crate depends on no host language: it gives columns of numbers and texts, and a host makes its vectors of them.

## License

BSD-3-Clause.
