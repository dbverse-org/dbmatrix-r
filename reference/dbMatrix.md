# Create a `dbSparseMatrix` or `dbDenseMatrix` object

Create an S4 `dbMatrix` object in sparse or dense triplet vector format.

## Usage

``` r
dbMatrix(
  value,
  class = NULL,
  con = NULL,
  overwrite = FALSE,
  name = "dbMatrix",
  dims = NULL,
  dim_names = NULL,
  mtx_rowname_file_path,
  mtx_rowname_col_idx = 1,
  mtx_colname_file_path,
  mtx_colname_col_idx = 1,
  ...
)
```

## Arguments

- value:

  data to be added to the database. See details for supported data types
  `(required)`

- class:

  class of the dbMatrix: `dbDenseMatrix` or `dbSparseMatrix`
  `(required)`

- con:

  DBI or duckdb connection object `(required)`

- overwrite:

  whether to overwrite if table already exists in database `(required)`

- name:

  table name to assign within database `(required, default: "dbMatrix")`

- dims:

  dimensions of the matrix `(optional: [int, int])`

- dim_names:

  dimension names of the matrix `(optional: list(enum, enum))`

- mtx_rowname_file_path:

  path to .mtx rowname file to be read into `(optional)` database. by
  default, no header is assumed.

- mtx_rowname_col_idx:

  column index of row name file `(optional)`

- mtx_colname_file_path:

  path to .mtx colname file to be read into database. by default, no
  header is assumed. `(optional)`

- mtx_colname_col_idx:

  column index of column name file `(optional)`

- ...:

  additional params to pass to
  [`dplyr::copy_to`](https://dplyr.tidyverse.org/reference/copy_to.html)

## Details

This function reads in data into a pre-existing DuckDB database.
Supported `value` data types:

- [`Matrix::dgCMatrix`](https://rdrr.io/pkg/Matrix/man/dgCMatrix-class.html)
  In-memory sparse matrix from the
  [`Matrix::Matrix`](https://rdrr.io/pkg/Matrix/man/Matrix.html) package

- [`Matrix::dgTMatrix`](https://rdrr.io/pkg/Matrix/man/dgTMatrix-class.html)
  In-memory triplet vector or COO matrix

- [`matrix`](https://rdrr.io/pkg/Matrix/man/Matrix.html) In-memory dense
  matrix from base R

- `.mtx` Path to [.mtx](https://math.nist.gov/MatrixMarket/formats.html)
  file

- `.csv` Path to .csv file

- `tbl_duckdb_connection` Table in
  [`duckdb::duckdb`](https://r.duckdb.org/reference/duckdb.html)
  database in ijx format from existing `dbMatrix` object. `dims` and
  `dim_names` must be specified if `value` is `tbl_duckdb_connection`.

## Examples

``` r
dgc <- readRDS(system.file("data", "dgc.rds", package = "dbMatrix"))
con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
dbSparse <- dbMatrix(
  value = dgc,
  con = con,
  name = "sparse_matrix",
  class = "dbSparseMatrix",
  overwrite = TRUE
)
dbSparse
#> 634 x 624  dbMatrix of class "dbSparseMatrix"
#> [[ Colnames 'AAAGGGATGTAGCAAG-1', 'AAATGGCATGTCTTGT-1', 'AAATGGTCAATGTGCC-1' ... suppressing 618 ...'TTGTCGTTCAGTTACC-1', 'TTGTGGCCCTGACAGT-1', 'TTGTTCAGTGTGCTAC-1' ]]
#>                                                                          
#> Gna12         1.0000000 2.0000000 1.0000000 1.0000000 9.0000000 1.0000000
#> Ccnd2                 . 1.0000000 1.0000000         .         . 1.0000000
#> Btbd17                . 1.0000000 1.0000000 1.0000000         .         .
#> 
#> ......suppressing 614 columns and 628 rows
#> 
#> Gm19935               . 1.0000000         .         .         .         .
#> 9630013A20Rik         .         .         .         .         .         .
#> 2900040C04Rik 1.0000000         .         .         .         .         .
```
