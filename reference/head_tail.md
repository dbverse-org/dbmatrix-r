# Return the First or Last Parts of an Object

Returns the first or last parts of a vector, matrix, array, table, data
frame or function. Since [`head()`](https://rdrr.io/r/utils/head.html)
and [`tail()`](https://rdrr.io/r/utils/head.html) are generic functions,
they have been extended to other classes, including
`"`[`ts`](https://rdrr.io/r/stats/ts.html)`"` from stats.

## Usage

``` r
# S4 method for class 'dbMatrix'
head(x, n = 6L, ...)

# S4 method for class 'dbMatrix'
tail(x, n = 6L, ...)
```

## Arguments

- x:

  an object

- n:

  an integer vector of length up to `dim(x)` (or 1, for non-dimensioned
  objects). A `logical` is silently coerced to integer. Values specify
  the indices to be selected in the corresponding dimension (or along
  the length) of the object. A positive value of `n[i]` includes the
  first/last `n[i]` indices in that dimension, while a negative value
  excludes the last/first `abs(n[i])`, including all remaining indices.
  `NA` or non-specified values (when `length(n) < length(dim(x))`)
  select all indices in that dimension. Must contain at least one
  non-missing value.

- ...:

  arguments to be passed to or from other methods.

## Value

A
[`dbMatrix`](https://dbverse-org.github.io/dbmatrix-r/reference/dbMatrix.md)
object containing the first or last `n` rows of `x`, with updated
dimensions and row names.
