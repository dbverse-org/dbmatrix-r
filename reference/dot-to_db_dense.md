# Convert a dbSparseMatrix to dbDenseMatrix

Internal function to convert a
[`dbSparseMatrix`](https://dbverse-org.github.io/dbmatrix-r/reference/dbSparseMatrix-class.md)
to
[`dbDenseMatrix`](https://dbverse-org.github.io/dbmatrix-r/reference/dbDenseMatrix-class.md).

## Usage

``` r
.to_db_dense(x)
```

## Arguments

- x:

  A
  [`dbSparseMatrix`](https://dbverse-org.github.io/dbmatrix-r/reference/dbSparseMatrix-class.md)
  object

## Value

A
[`dbDenseMatrix`](https://dbverse-org.github.io/dbmatrix-r/reference/dbDenseMatrix-class.md)
object

## Examples

``` r
dbsm <- sim_dbSparseMatrix(10, 10)
dbdm <- toDbDense(dbsm)
#> Error in toDbDense(dbsm): could not find function "toDbDense"
```
