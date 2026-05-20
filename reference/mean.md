# Arithmetic Mean for [`dbMatrix`](https://dbverse-org.github.io/dbmatrix-r/reference/dbMatrix.md) objects

Generic function for the (trimmed) arithmetic mean.

## Usage

``` r
# S4 method for class 'dbDenseMatrix'
mean(x, ...)

# S4 method for class 'dbSparseMatrix'
mean(x, ...)
```

## Arguments

- x:

  [`dbMatrix`](https://dbverse-org.github.io/dbmatrix-r/reference/dbMatrix.md)
  object

- ...:

  further arguments passed to or from other methods.

## Value

A length-one numeric vector giving the arithmetic mean of all entries in
`x`.
