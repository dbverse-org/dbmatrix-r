# Evaluate if a dbSparseMatrix should be densified

Evaluate if a dbSparseMatrix should be densified

## Usage

``` r
.eval_op_densify(generic_char, dbVector)
```

## Arguments

- generic_char:

  A character string representing the operation to be performed.

- dbVector:

  A `dbMatrix` object with 1D row or col.

## Details

Evaluates if a `dbSparseMatrix` should be densified for `[Arith]`
operations and specific scalar values for operations in the order of
dbSparseMatrix, vector
