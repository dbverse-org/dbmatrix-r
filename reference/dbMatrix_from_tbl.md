# dbMatrix_from_tbl

Constructs a `dbSparseMatrix` object from a `tbl_duckdb_connection`
object.

## Usage

``` r
dbMatrix_from_tbl(
  tbl,
  rownames_colName,
  colnames_colName,
  value_colName = NULL,
  name = "dbMatrix",
  overwrite = FALSE
)
```

## Arguments

- tbl:

  `tbl_duckdb_connection` table in DuckDB database in long format

- rownames_colName:

  `character` column name of rownames in tbl `(required)`

- colnames_colName:

  `character` column name of colnames in tbl `(required)`

- value_colName:

  `character` column name containing pre-aggregated integer counts. If
  `NULL` (default), counts occurrences of each row-column pair.
  `(optional)`

- name:

  table name to assign within database `(required, default: "dbMatrix")`

- overwrite:

  whether to overwrite if table already exists in database `(required)`

- con:

  DBI or duckdb connection object `(required)`

## Value

`dbMatrix` object

## Details

The `tbl_duckdb_connection` object must contain dimension names as
columns in long format.

If `value_colName` is provided, the function uses pre-aggregated counts
from that column. This is useful when the input table already contains
aggregated counts (e.g., from a GROUP BY + SUM operation). If
`value_colName` is `NULL` (default), the function counts occurrences of
each row-column pair.
