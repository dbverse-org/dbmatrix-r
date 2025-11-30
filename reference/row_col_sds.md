# Row (column) standard deviations for `dbMatrix` objects

Calculates the standard deviation for each row (column) of a matrix-like
object.

## Usage

``` r
# S4 method for class 'dbDenseMatrix'
colSds(x, ..., memory = FALSE, useNames = TRUE)

# S4 method for class 'dbSparseMatrix'
colSds(x, ..., memory = FALSE, useNames = TRUE)

# S4 method for class 'dbDenseMatrix'
rowSds(
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
rowSds(
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

- ...:

  Additional arguments (not used, but included for compatibility with
  the generic).

- memory:

  logical. If FALSE (default), results returned as dbDenseMatrix.

- useNames:

  Always TRUE for `dbMatrix` queries. Included for compatibility with
  the generic.

- rows:

  Always NULL for `dbMatrix` queries. TODO

- cols:

  Always NULL for `dbMatrix` queries. TODO

- na.rm:

  Always TRUE for `dbMatrix` queries. Included for compatibility with
  the generic.

- center:

  Always NULL for `dbMatrix` queries. Included for compatibility with
  the generic.
