# rtemis-r8-io

Readers and writers of table files for the R8 type system: CSV and TSV, and Parquet under the feature `parquet`. A cell is read as the type that its column declares, and each text that does not read is collected with its count and its first rows. A column without a declared type has the type that all its cells have, and is text otherwise; in a Parquet file, the type that the file stores.

A cell of a typed format is a `Stored` value. One that is not of its column's declared type exactly is read by its canonical text, the text that a CSV holds for it, so that missing codes, given values of texts, and the report are the same for every format. A date-time without an offset is the clock reading of a time zone, which the crate converts with its own IANA database, the same on every platform.

The crate depends on no host language: it gives columns of numbers and texts, and a host makes its vectors of them. The R package `R8.io` is built over it.

## Features

- `parquet`: reading and writing Parquet files, with the codecs snappy, gzip, lz4, and zstd. Off by default.

## License

BSD-3-Clause.
