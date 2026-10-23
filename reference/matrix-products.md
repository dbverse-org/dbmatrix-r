# Matrix products

Matrix products (`%*%`,
[`crossprod()`](https://rdrr.io/r/base/crossprod.html),
[`tcrossprod()`](https://rdrr.io/r/base/crossprod.html)) are not
supported for
[`dbMatrix`](https://dbverse-org.github.io/dbmatrix-r/reference/dbMatrix.md)
objects yet. These methods give a clear error.

## Usage

``` r
# S4 method for class 'dbMatrix,ANY'
x %*% y

# S4 method for class 'ANY,dbMatrix'
x %*% y

# S4 method for class 'dbMatrix,ANY'
crossprod(x, y = NULL, ...)

# S4 method for class 'ANY,dbMatrix'
crossprod(x, y = NULL, ...)

# S4 method for class 'dbMatrix,ANY'
tcrossprod(x, y = NULL, ...)

# S4 method for class 'ANY,dbMatrix'
tcrossprod(x, y = NULL, ...)

# S4 method for class 'dbMatrix,dbMatrix'
x %*% y

# S4 method for class 'dbMatrix,dbMatrix'
crossprod(x, y = NULL, ...)

# S4 method for class 'dbMatrix,dbMatrix'
tcrossprod(x, y = NULL, ...)
```

## Arguments

- x, y:

  [`dbMatrix`](https://dbverse-org.github.io/dbmatrix-r/reference/dbMatrix.md)
  objects or matrices.

- ...:

  Not used.

## Value

Does not return; always gives an error.
