# sim_dgc

Simulate a dbSparseMatrix in memory

Simulate a dbDenseMatrix in memory.

## Usage

``` r
sim_duckdb(value = datasets::iris, name = "test", con = NULL, memory = TRUE)

sim_dgc(num_rows = 50, num_cols = 50, n_vals = 50)

sim_denseMat(num_rows = 50, num_cols = 50)

sim_ijx_matrix(mat_type = NULL, num_rows = 50, num_cols = 50, seed_num = 42)

sim_dbSparseMatrix(
  num_rows = 50,
  num_cols = 50,
  seed_num = 42,
  name = "sparse_test",
  memory = FALSE
)

sim_dbDenseMatrix(
  num_rows = 50,
  num_cols = 50,
  seed_num = 42,
  name = "dense_test",
  memory = FALSE
)
```

## Arguments

- num_rows:

  The number of rows in the matrix (default: 50)

- num_cols:

  The number of columns in the matrix (default: 50)

- seed_num:

  The seed number for reproducibility (default: 42)

## Value

A dgCMatrix object

## Details

This function generates a simulated sparse matrix (dgCMatrix) with
number of rows and columns and sets n_vals random values to a non-zero
value.

This function generates a simulated dense matrix object with a specified
number of rows and columns.

This function generates an ijx representation of a simulated dgCMatrix
object with a specified number of rows and columns and sets 50 random
values to a non-zero value.

## Functions

- `sim_duckdb()`: Simulate a duckdb connection dplyr tbl_Pool in memory

- `sim_dgc()`: Simulate a dgcMatrix

- `sim_denseMat()`: Simulate a dense matrix

- `sim_ijx_matrix()`: Simulate a duckdb connection dplyr tbl_Pool in
  memory

- `sim_dbSparseMatrix()`: Simulate a dbSparseMatrix in memory

- `sim_dbDenseMatrix()`: Simulate a dbDenseMatrix in memory

## Examples

``` r
sim_ijx_matrix()
#> Error in sim_ijx_matrix(): could not find function "sim_ijx_matrix"
```
