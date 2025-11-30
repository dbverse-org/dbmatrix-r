# Join a `dbSparseMatrix` with a `dbMatrix` object

Join a `dbSparseMatrix` with a `dbMatrix` object

## Usage

``` r
.join_dbm_vector(dbm, dbVector, op, swap_arith_order = FALSE)
```

## Arguments

- dbm:

  A `dbSparseMatrix` object.

- dbVector:

  A `dbMatrix` object with 1D row or col.

- swap_arith_order:

  order of the arguments for the operation. default: NULL

- generic_char:

  A character string representing the operation to be performed.
