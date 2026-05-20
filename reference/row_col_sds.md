# Row (column) standard deviations for [`dbMatrix`](https://dbverse-org.github.io/dbmatrix-r/reference/dbMatrix.md) objects

Calculates the standard deviation for each row (column) of a matrix-like
object.

## Usage

``` r
# S4 method for class 'dbDenseMatrix'
colSds(
  x,
  rows = NULL,
  cols = NULL,
  na.rm = FALSE,
  center = NULL,
  ...,
  useNames = TRUE
)

# S4 method for class 'dbSparseMatrix'
colSds(
  x,
  rows = NULL,
  cols = NULL,
  na.rm = FALSE,
  center = NULL,
  ...,
  useNames = TRUE
)

# S4 method for class 'dbDenseMatrix'
rowSds(
  x,
  rows = NULL,
  cols = NULL,
  na.rm = TRUE,
  center = NULL,
  ...,
  useNames = TRUE
)

# S4 method for class 'dbSparseMatrix'
rowSds(
  x,
  rows = NULL,
  cols = NULL,
  na.rm = TRUE,
  center = NULL,
  ...,
  useNames = TRUE
)
```

## Arguments

- x:

  A
  [`dbMatrix`](https://dbverse-org.github.io/dbmatrix-r/reference/dbMatrix.md)
  object.

- rows:

  Always NULL for
  [`dbMatrix`](https://dbverse-org.github.io/dbmatrix-r/reference/dbMatrix.md)
  queries. TODO

- cols:

  Always NULL for
  [`dbMatrix`](https://dbverse-org.github.io/dbmatrix-r/reference/dbMatrix.md)
  queries. TODO

- na.rm:

  Always TRUE for
  [`dbMatrix`](https://dbverse-org.github.io/dbmatrix-r/reference/dbMatrix.md)
  queries. Included for compatibility with the generic.

- center:

  Always NULL for
  [`dbMatrix`](https://dbverse-org.github.io/dbmatrix-r/reference/dbMatrix.md)
  queries. Included for compatibility with the generic.

- ...:

  Additional arguments (not used, but included for compatibility with
  the generic).

- useNames:

  Always TRUE for
  [`dbMatrix`](https://dbverse-org.github.io/dbmatrix-r/reference/dbMatrix.md)
  queries. Included for compatibility with the generic.

## Value

A named numeric vector containing one sample standard deviation per row
or column of `x`.
