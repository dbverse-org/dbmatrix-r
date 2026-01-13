# Row (column) variances for [`dbMatrix`](https://dbverse-org.github.io/dbmatrix-r/reference/dbMatrix.md) objects

Calculates the variance for each row (column) of a matrix-like object.

## Usage

``` r
# S4 method for class 'dbDenseMatrix'
rowVars(
  x,
  rows = NULL,
  cols = NULL,
  na.rm = TRUE,
  center = NULL,
  ...,
  useNames = TRUE
)

# S4 method for class 'dbSparseMatrix'
rowVars(
  x,
  rows = NULL,
  cols = NULL,
  na.rm = TRUE,
  center = NULL,
  ...,
  useNames = TRUE
)

# S4 method for class 'dbDenseMatrix'
colVars(
  x,
  rows = NULL,
  cols = NULL,
  na.rm = TRUE,
  center = NULL,
  ...,
  useNames = TRUE
)

# S4 method for class 'dbSparseMatrix'
colVars(
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
  queries. Included for compatibility with the generic.

- cols:

  Always NULL for
  [`dbMatrix`](https://dbverse-org.github.io/dbmatrix-r/reference/dbMatrix.md)
  queries. Included for compatibility with the generic.

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
