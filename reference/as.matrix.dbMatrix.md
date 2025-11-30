# Convert `dbMatrix` to in-memory matrix

Converts a `dbMatrix` object into an in-memory matrix or sparse matrix.

## Usage

``` r
# S3 method for class 'dbMatrix'
as.matrix(x, ..., sparse = FALSE, names = FALSE)
```

## Arguments

- x:

  A `dbMatrix` object (dbSparseMatrix or dbDenseMatrix)

- ...:

  Additional arguments (not used)

- sparse:

  Logical indicating if the output should be a sparse matrix
  `default:FALSE`

- names:

  Logical indicating if the output should have dimnames. `default:FALSE`

## Value

A
[`Matrix::dgCMatrix`](https://rdrr.io/pkg/Matrix/man/dgCMatrix-class.html)
or [`matrix`](https://rdrr.io/r/base/matrix.html)

## Details

This method converts a `dbMatrix` object into an in-memory
[`Matrix::dgCMatrix`](https://rdrr.io/pkg/Matrix/man/dgCMatrix-class.html)
(sparse = TRUE) or [`matrix()`](https://rdrr.io/r/base/matrix.html)
(default, sparse = FALSE).

**Warning: This function can cause memory issues for large `dbMatrix`
objects.**

Set `sparse = TRUE` to convert to a sparse matrix. Set `names = TRUE` to
keep dimnames.
