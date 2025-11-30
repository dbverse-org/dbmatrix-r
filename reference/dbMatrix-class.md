# S4 virtual class for `dbMatrix`

Representation of sparse and dense matrices in a database. Each object
is used as a connection to a single table that exists within the
database. Inherits from `dbData`.

## Usage

``` r
dbMatrix(
  value,
  class = NULL,
  con = NULL,
  overwrite = FALSE,
  name = "dbMatrix",
  dims = NULL,
  dim_names = NULL,
  mtx_rowname_file_path,
  mtx_rowname_col_idx = 1,
  mtx_colname_file_path,
  mtx_colname_col_idx = 1,
  ...
)
```

## Slots

- `dim_names`:

  row 1 and col 2 names

- `dims`:

  dimensions of the matrix

- `init`:

  logical. Whether the object is fully initialized
