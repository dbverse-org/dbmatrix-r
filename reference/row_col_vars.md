# Row (column) variances for `dbMatrix` objects

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
  memory = FALSE,
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
  memory = FALSE,
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
  memory = FALSE,
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
  memory = FALSE,
  useNames = TRUE
)
```

## Arguments

- x:

  A `dbMatrix` object.

- rows:

  Always NULL for `dbMatrix` queries. Included for compatibility with
  the generic.

- cols:

  Always NULL for `dbMatrix` queries. Included for compatibility with
  the generic.

- na.rm:

  Always TRUE for `dbMatrix` queries. Included for compatibility with
  the generic.

- center:

  Always NULL for `dbMatrix` queries. Included for compatibility with
  the generic.

- ...:

  Additional arguments (not used, but included for compatibility with
  the generic).

- memory:

  logical. If FALSE (default), results returned as dbDenseMatrix. This
  is recommended for large computations. Set to TRUE to return the
  results as a vector.

- useNames:

  Always TRUE for `dbMatrix` queries. Included for compatibility with
  the generic.
