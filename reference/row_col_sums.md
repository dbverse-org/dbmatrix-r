# Row (column) sums for `dbMatrix` objects

Calculates the sum for each row (column) of a matrix-like object.

## Usage

``` r
# S4 method for class 'dbDenseMatrix'
rowSums(x, ..., memory = FALSE)

# S4 method for class 'dbSparseMatrix'
rowSums(x, ..., memory = FALSE)

# S4 method for class 'dbDenseMatrix'
colSums(x, ..., memory = FALSE)

# S4 method for class 'dbSparseMatrix'
colSums(x, ..., memory = FALSE)
```

## Arguments

- x:

  An NxK matrix-like object, a numeric data frame, or an array-like
  object of two or more dimensions.

- ...:

  Additional arguments passed to specific methods.

- memory:

  logical. If FALSE (default), results returned as dbDenseMatrix. This
  is recommended for large computations. Set to TRUE to return the
  results as a vector.

- na.rm:

  Always TRUE for `dbMatrix`. Included for compatibility with the
  generic.

- dims:

  Always 1 for `dbMatrix` queries. Included for compatibility with the
  generic.
