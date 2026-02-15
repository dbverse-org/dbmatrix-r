# Convert dbMatrix to named ijx table

Converts a `dbMatrix` to a lazy long table where row and column indices
are replaced by dimension names.

## Usage

``` r
to_named_ijx_tbl(
  x,
  row_col = "row_name",
  col_col = "col_name",
  compute = FALSE
)
```

## Arguments

- x:

  A dbMatrix object (dbSparseMatrix or dbDenseMatrix)

- row_col:

  Name for the row-name column (default: "row_name")

- col_col:

  Name for the column-name column (default: "col_name")

- compute:

  Whether to materialize as temp table (default: FALSE)

## Value

A lazy tbl with columns: row_col, col_col, x
