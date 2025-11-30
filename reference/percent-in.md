# Value Matching

Implements the `%in%` operator for dbMatrix objects. This operator
checks if elements from the left operand are contained in the right
operand, returning a logical vector.

## Usage

``` r
# S4 method for class 'dbDenseMatrix,ANY'
x %in% table

# S4 method for class 'ANY,dbDenseMatrix'
x %in% table

# S4 method for class 'dbSparseMatrix,ANY'
x %in% table
```

## Arguments

- x:

  A dbMatrix object or any other object

- table:

  Any object or a dbMatrix object

## Value

A logical vector of the same length as `x`, indicating which elements of
`x` are in `table`.

## Details

This is a method for the standard `%in%` operator for dbMatrix objects.
It follows R's standard behavior for the `%in%` operator:

- When `x` is a dbDenseMatrix, it returns a logical vector with the same
  length as the total number of elements in the matrix.

- When `table` is a dbDenseMatrix, it allows checking if elements in `x`
  are in the matrix.

- For dbSparseMatrix objects, it throws an error to match the behavior
  of dgCMatrix.

## Examples

``` r
if (FALSE) { # \dontrun{
# Create a dbMatrix
mat <- matrix(1:9, nrow = 3, ncol = 3)
dbmat <- as.dbMatrix(mat)

# Check if elements in dbMatrix are in a vector
result <- dbmat %in% c(1, 3, 5, 7, 9)

# Check if elements in a vector are in dbMatrix
result <- c(1, 3, 5, 7, 9) %in% dbmat
} # }
```
