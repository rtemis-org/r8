# R8.io 0.6.0

- `r8_read_table()`, `r8_check_file()`, and `r8_write_table()` read and write Parquet files.
- `time_zone = "UTC"` gives the time zone of date-times without an offset, in CSV and in Parquet; a CSV column of date-times without an offset is read as date-times, where it was read as text.
- `encoding` is `NULL` by default, which is UTF-8, and is an argument of CSV and TSV files alone.

# R8.io 0.5.0

- `r8_read_table()` reads a CSV or TSV file as a value of a table type, or as a raven.
- `r8_check_file()` reports every cell of a file that does not read as its type, and every row that does not satisfy its rules.
- `r8_write_table()` writes a table, which reads back as it was.
