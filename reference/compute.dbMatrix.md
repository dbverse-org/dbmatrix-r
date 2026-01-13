# Force computation of a dbMatrix

Explicitly compute a dbMatrix and save it to a table in the database.
This overrides the default
[`dplyr::compute`](https://dplyr.tidyverse.org/reference/compute.html)
to use a direct `CREATE TABLE AS` statement, which is more robust for
large tables in DuckDB.

## Usage

``` r
# S3 method for class 'dbMatrix'
compute(
  x,
  name = NULL,
  temporary = TRUE,
  dimnames = TRUE,
  overwrite = FALSE,
  ...
)
```

## Arguments

- x:

  A `dbMatrix` object

- name:

  Name of the table to create. If NULL, a random name is generated.

- temporary:

  Logical. If TRUE (default), create a temporary table.

- dimnames:

  default = TRUE. If TRUE, the rownames and colnames will be saved in
  the database. This allows full reconstruction of the dbMatrix object
  using
  [`dbProject::dbLoad()`](https://dbverse-org.github.io/dbproject-r/reference/dbLoad.html).

- overwrite:

  Logical. If TRUE, overwrite the table if it already exists. Default is
  FALSE.

- ...:

  Additional arguments passed to methods (ignored).

## Value

A `dbMatrix` object pointing to the new table.
