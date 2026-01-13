# Join a `dbSparseMatrix` with a `dbMatrix` object

Join a `dbSparseMatrix` with a `dbMatrix` object

## Usage

``` r
.join_dbm_vect(dbm, vec_matrix, op, swap_arith_order = FALSE)
```

## Arguments

- dbm:

  A `dbSparseMatrix` object.

- vec_matrix:

  A `dbMatrix` object with 1D row or col.

- op:

  A character string representing the operation to be performed.

- swap_arith_order:

  order of the arguments for the operation. default: NULL
