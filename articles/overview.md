# Overview

``` r

library(dbMatrix)
```

## dbMatrix

`dbMatrix` is a core package in the
[`dbverse`](https://dbverse-org.github.io/dbverse/) library that
inherits from the `dbData` base class and consists of two subclasses,
`dbSparseMatrix` and `dbDenseMatrix`.

`dbMatrix` objects emulate in-memory dense and sparse matrices in an
embedded database powered by DuckDB.

## Minimal example

``` r

con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
#> duckdb is keeping downloaded extensions in a temporary directory:
#> ℹ /tmp/RtmpeDSeIs/duckdb/extensions
#> This is removed when the R session ends, so extensions are re-downloaded each session.
#> ℹ To keep them, point `options(duckdb.extension_directory =)` or the `DUCKDB_EXTENSION_DIRECTORY` environment variable at a permanent path.

mat <- matrix(1:9, nrow = 3, ncol = 3)
dbmat <- dbMatrix(
  value = mat,
  con = con,
  name = "overview_matrix",
  class = "dbDenseMatrix",
  overwrite = TRUE
)

dbmat
#> 3 x 3  dbMatrix of class "dbDenseMatrix"
#> [[ Colnames: 'col1', 'col2', 'col3' ]]
#>                                   
#> row1 1.0000000 4.0000000 7.0000000
#> row2 2.0000000 5.0000000 8.0000000
#> row3 3.0000000 6.0000000 9.0000000

DBI::dbDisconnect(con, shutdown = TRUE)
```

## dbSparseMatrix

![](../reference/figures/dbMatrix-overview.png)

Sparse matrices are represented as triplet vectors with `i`, `j`, and
`x` numerical vectors. `i` and `j` refer to the row and column indices,
respectively, with specific `x` values or counts. Note: The `i` and `j`
vectors are one-based following conventions from the `Matrix` R package
and the `TSparseMatrix` class.

The `dbSparseMatrix` class stores the triplet vector representation with
its associated dimension names and dimensions if available. If none are
provided, they are automatically generated as factors (enums) with
`row#` or `col#` labels for rownames and colnames, respectively. The
`dbSparseMatrix` class does not contain zeros in the triplet vector
representation. Zero values are inferred based on the provided
dimensions.

Support for compressed sparse column matrices of the `dgCMatrix` class
from the `Matrix` R package is currently implemented. For more
information about sparse matrices, see [the wiki article
here](https://en.wikipedia.org/wiki/Sparse_matrix).

## dbDenseMatrix

We use the same approach as `dbSparseMatrix` to represent a dense matrix
as a triplet `i`, `j`, and `x` vector. The only difference is that the
`dbDenseMatrix` class consists of all values in the matrix, including
zeros.
