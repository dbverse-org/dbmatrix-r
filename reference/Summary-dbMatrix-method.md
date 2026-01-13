# Summary Methods for [`dbMatrix`](https://dbverse-org.github.io/dbmatrix-r/reference/dbMatrix.md) Objects

Implements the `S4groupGeneric` group generic functions for dbMatrix
objects.

## Usage

``` r
# S4 method for class 'dbMatrix'
Summary(x, ..., na.rm = TRUE)
```

## Arguments

- x:

  A dbMatrix object.

- ...:

  Additional arguments (not used, but included for compatibility with
  the generic).

- na.rm:

  Logical. If TRUE, remove NA values before computation. Always set to
  TRUE for this implementation.

## Value

The result of applying the respective summary function to the dbMatrix
object. The type of the return value depends on the specific function
called.

## Details

This method provides implementations for the following `S4groupGeneric`
functions:

- [`max()`](https://rdrr.io/r/base/Extremes.html): Maximum value

- [`min()`](https://rdrr.io/r/base/Extremes.html): Minimum value

- [`range()`](https://rdrr.io/r/base/range.html): *Not supported*

- [`prod()`](https://rdrr.io/r/base/prod.html): Product of all values

- [`sum()`](https://rdrr.io/r/base/sum.html): Sum of all values

- [`any()`](https://rdrr.io/r/base/any.html): Returns TRUE if any value
  is TRUE

- [`all()`](https://rdrr.io/r/base/all.html): Returns TRUE if all values
  are TRUE

## Examples

``` r
mat <- matrix(1, nrow = 3, ncol = 3)
dbmat <- as.dbMatrix(mat)
max(dbmat)
#> [1] 1
min(dbmat)
#> [1] 1
prod(dbmat)
#> [1] 1
sum(dbmat)
#> [1] 9
any(dbmat > 0)
#> [1] TRUE
all(dbmat > 0)
#> [1] TRUE
```
