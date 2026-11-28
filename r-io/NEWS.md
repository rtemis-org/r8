# R8.io 0.7.0

- R8.io imports R8 0.7.0, whose `new_table_type()` declares the table types that `r8read_table()` and `r8check_file()` take.
- Without a type, a whole number beyond 2^53 without a point, such as `9007199254740993`, is read as text.
- Without a type, a column whose cells are missing but for a quoted `""` or `"NA"` is text, which holds those cells as texts.

# R8.io 0.6.0

- `r8read_table()`, `r8check_file()`, and `r8write_table()` begin with `r8` joined to their first word, as the functions of R8 do.
- `r8read_table()`, `r8check_file()`, and `r8write_table()` read and write Parquet files.
- `time_zone = "UTC"` gives the time zone of date-times without an offset, in CSV and in Parquet; a CSV column of date-times without an offset is read as date-times, where it was read as text.
- `encoding` is `NULL` by default, which is UTF-8, and is an argument of CSV and TSV files alone.

# R8.io 0.5.0

- `r8_read_table()` reads a CSV or TSV file as a value of a table type, or as a raven.
- `r8_check_file()` reports every cell of a file that does not read as its type, and every row that does not satisfy its rules.
- `r8_write_table()` writes a table, which reads back as it was.
