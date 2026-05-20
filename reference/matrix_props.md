# Retrieve and Set Row (Column) Dimension Names of dbMatrix Objects

Retrieve and Set Row (Column) Dimension Names of dbMatrix Objects

## Usage

``` r
rownames.dbMatrix(x, do.NULL = TRUE, prefix = "row")

# S3 method for class 'dbMatrix'
rownames(x) <- value

colnames.dbMatrix(x, do.NULL = TRUE, prefix = "col")

# S3 method for class 'dbMatrix'
colnames(x) <- value

# S4 method for class 'dbMatrix'
dimnames(x)

# S4 method for class 'dbMatrix,list'
dimnames(x) <- value
```

## Arguments

- x:

  a matrix-like R object, with at least two dimensions for `colnames`.

- do.NULL:

  Not used for this method. Included for compatibility with the generic.

- prefix:

  Not used for this method. Included for compatibility with the generic.

- value:

  a valid value for that component of
  [`dimnames`](https://rdrr.io/r/base/dimnames.html)`(x)`. For a matrix
  or array this is either `NULL` or a character vector of non-zero
  length equal to the appropriate dimension.

## Value

[`rownames()`](https://rdrr.io/r/base/colnames.html) and
[`colnames()`](https://rdrr.io/r/base/colnames.html) return character
vectors of dimension names.
[`dimnames()`](https://rdrr.io/r/base/dimnames.html) returns a length-2
list containing row and column name vectors. The replacement forms
return the modified
[`dbMatrix`](https://dbverse-org.github.io/dbmatrix-r/reference/dbMatrix.md)
object.
