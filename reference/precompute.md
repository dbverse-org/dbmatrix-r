# Compute a dense COO table in a database connection

Precomputes a COO list table in a specificied database connection in
column- major order. This can speed up operations that involve breaking
sparsity of a `dbSparseMatrix`, such as in cases when performing + or -
arithmetic operations.

## Usage

``` r
precompute(conn, m, n, verbose = FALSE)
```

## Arguments

- conn:

  duckdb database connection

- m:

  number of rows of precomputed dbMatrix table

- n:

  number of columns of precomputed dbMatrix table

- verbose:

  logical, print progress messages. default: FALSE.

## Value

A `tbl_dbi` object referencing the newly created precomputed lookup
table in DuckDB.

## Details

The `m` and `n` parameters must exceed the maximum row and column
indices of the `dbMatrix` in order to be used for densifying any
`dbMatrix`. If these params are less than the maximum row and column
indices, a new precomputed table will be automatically generated with
the name 'precomp_mXn'.

In such cases, run this function again with a larger `n_rows` and
`num_cols`, or to manually remove the precomputed table set
`options(dbMatrix.precomp = NULL)` in the R console.
