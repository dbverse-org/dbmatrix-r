# Convert a dbSparseMatrix to dbDenseMatrix

Internal function to convert a
[`dbSparseMatrix`](https://dbverse-org.github.io/dbmatrix-r/reference/dbSparseMatrix-class.md)
to
[`dbDenseMatrix`](https://dbverse-org.github.io/dbmatrix-r/reference/dbDenseMatrix-class.md).

## Usage

``` r
.to_db_dense(x, chunk_size = NULL)
```

## Arguments

- x:

  A
  [`dbSparseMatrix`](https://dbverse-org.github.io/dbmatrix-r/reference/dbSparseMatrix-class.md)
  object

- chunk_size:

  integer. Number of columns to process per chunk during densification.
  If NULL (default), the function first checks the global option
  `dbMatrix.chunk_size`. If that is also NULL, it calculates a chunk
  size such that the estimated memory usage of each chunk does not
  exceed `dbMatrix.max_mem_convert` (default 8GB). If the total size is
  within the limit, a single chunk is used.

## Value

A
[`dbDenseMatrix`](https://dbverse-org.github.io/dbmatrix-r/reference/dbDenseMatrix-class.md)
object
