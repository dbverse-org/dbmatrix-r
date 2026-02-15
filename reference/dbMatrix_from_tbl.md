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
  overwrite = FALSE,
  row_names = NULL,
  col_names = NULL,
  i_col = NULL,
  j_col = NULL
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

- row_names:

  `character` vector of pre-computed row names (sorted). If `NULL`
  (default), row names are extracted from the table. `(optional)`

- col_names:

  `character` vector of pre-computed column names (sorted). If `NULL`
  (default), column names are extracted from the table. `(optional)`

- i_col:

  `character` column name containing pre-computed row indices (1-based
  integers). If provided with `j_col`, skips index encoding for optimal
  performance. `(optional)`

- j_col:

  `character` column name containing pre-computed column indices
  (1-based integers). If provided with `i_col`, skips index encoding for
  optimal performance. `(optional)`

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

When `row_names` and/or `col_names` are provided, the function uses
these directly instead of querying distinct values from the table. This
can significantly improve performance when the input table is a complex
lazy query (e.g., result of spatial joins).

When `i_col` and `j_col` are provided, the function uses these
pre-computed integer indices directly, skipping expensive
string-to-index encoding. This is the fastest path.
