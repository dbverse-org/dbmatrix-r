# Row (column) sums for [`dbMatrix`](https://dbverse-org.github.io/dbmatrix-r/reference/dbMatrix.md) objects

Calculates the sum for each row (column) of a matrix-like object.

## Usage

``` r
# S4 method for class 'dbDenseMatrix'
rowSums(x, na.rm = FALSE, dims = 1, ..., memory = FALSE)

# S4 method for class 'dbSparseMatrix'
rowSums(x, na.rm = FALSE, dims = 1, ...)

# S4 method for class 'dbDenseMatrix'
colSums(x, na.rm = FALSE, dims = 1, ...)

# S4 method for class 'dbSparseMatrix'
colSums(x, na.rm = FALSE, dims = 1, ...)
```

## Arguments

- x:

  An NxK matrix-like object, a numeric data frame, or an array-like
  object of two or more dimensions.

- na.rm:

  Always TRUE for
  [`dbMatrix`](https://dbverse-org.github.io/dbmatrix-r/reference/dbMatrix.md).
  Included for compatibility with the generic.

- dims:

  Always 1 for
  [`dbMatrix`](https://dbverse-org.github.io/dbmatrix-r/reference/dbMatrix.md)
  queries. Included for compatibility with the generic.

- ...:

  Additional arguments passed to specific methods.

- memory:

  logical. If FALSE (default), results returned as dbDenseMatrix. This
  is recommended for large computations. Set to TRUE to return the
  results as a vector.

## Value

A named numeric vector containing one sum per row or column of `x`.
