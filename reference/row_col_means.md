# Row (column) means for [`dbMatrix`](https://dbverse-org.github.io/dbmatrix-r/reference/dbMatrix.md) objects

Calculates the mean for each row (column) of a matrix-like object.

## Usage

``` r
# S4 method for class 'dbMatrix'
rowMeans(x, na.rm = FALSE, dims = 1, ...)

# S4 method for class 'dbMatrix'
colMeans(x, na.rm = FALSE, dims = 1, ...)
```

## Arguments

- x:

  An NxK matrix-like object, a numeric data frame, or an array-like
  object of two or more dimensions.

- na.rm:

  Always TRUE for
  [`dbMatrix`](https://dbverse-org.github.io/dbmatrix-r/reference/dbMatrix.md)
  queries. Included for compatibility with the generic.

- dims:

  Always 1 for
  [`dbMatrix`](https://dbverse-org.github.io/dbmatrix-r/reference/dbMatrix.md)
  queries. Included for compatibility with the generic.

- ...:

  Additional arguments passed to specific methods.
