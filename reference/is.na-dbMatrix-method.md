# Element-wise is.na for dbMatrix

Returns a dbMatrix with numeric values indicating NA positions (1 = NA,
0 = not NA).

## Usage

``` r
# S4 method for class 'dbMatrix'
is.na(x)
```

## Arguments

- x:

  A dbMatrix object.

## Value

A dbMatrix with same dimensions, containing 1 where the original value
was NA and 0 otherwise.

## Examples

``` r
mat <- matrix(c(1, NA, 3, NA), nrow = 2)
dbmat <- as.dbMatrix(mat)
is.na(dbmat)
#> 2 x 2  dbMatrix of class "dbDenseMatrix"
#> [[ Colnames: 'col1', 'col2' ]]
#>                         
#> row1 0.0000000 0.0000000
#> row2 1.0000000 1.0000000
```
