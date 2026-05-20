# Extract or replace values in database-backed matrices

Methods for subsetting and replacing values in `dbMatrix` objects.

## Usage

``` r
# S4 method for class 'dbMatrix,dbIndex,missing,ANY'
x[i, j, ..., drop = TRUE]

# S4 method for class 'dbMatrix,missing,dbIndex,ANY'
x[i, j, ..., drop = TRUE]

# S4 method for class 'dbMatrix,dbIndex,dbIndex,ANY'
x[i, j, ..., drop = FALSE]

# S4 method for class 'dbMatrix,dbMatrix,missing,ANY'
x[i, j, ..., drop = TRUE]

# S4 method for class 'dbMatrix,dbMatrix,missing,ANY'
x[i, j] <- value

# S4 method for class 'dbMatrix,dbDenseMatrix,missing,ANY'
x[i, j, ..., drop = TRUE]

# S4 method for class 'dbMatrix,missing,dbDenseMatrix,ANY'
x[i, j, ..., drop = FALSE]

# S4 method for class 'dbMatrix,dbDenseMatrix,dbDenseMatrix,ANY'
x[i, j, ..., drop = FALSE]
```

## Arguments

- x:

  A `dbMatrix` object.

- i:

  Row, logical matrix, or matrix-style index.

- j:

  Column index.

- ...:

  Additional arguments.

- drop:

  Ignored; included for matrix API compatibility.

- value:

  Replacement value.

## Value

A subsetted or modified `dbMatrix`, or an extracted vector for
matrix-style indexing.
