# R8.io

Readers and writers of table files for the types of [R8](https://github.com/rtemis-org/r8).

```r
library(R8)
library(R8.io)

Labs <- new_table("Labs", list(id = type_integer(), lab = type_double(min = 0)))
r8_check_file("labs.csv", Labs)
labs <- r8_read_table("labs.csv", Labs, recode_in = list(lab = c("<0.1" = 0.1)))
```
