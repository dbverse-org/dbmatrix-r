# dbMatrix Package Global Options

The following global options can be modified to control the behavior of
the `dbMatrix` package.

## Details

Use [`options()`](https://rdrr.io/r/base/options.html) to set the below
options.

## Options

- `dbMatrix.dbdm_auto_compute`: logical. If `TRUE`, automatically
  computes intermediate dense COO tables that are generated from
  [`dbSparseMatrix`](https://dbverse-org.github.io/dbmatrix-r/reference/dbSparseMatrix-class.md)
  to
  [`dbDenseMatrix`](https://dbverse-org.github.io/dbmatrix-r/reference/dbDenseMatrix-class.md)
  conversion in the internal function
  [`.to_db_dense`](https://dbverse-org.github.io/dbmatrix-r/reference/dot-to_db_dense.md).
  If `FALSE` (default), this step is skipped and the dense COO table is
  lazy. Setting this to `TRUE` is recommended to avoid large
  intermediate COO tables that can trigger downstream bottlenecks or
  memory errors due to disk spilling when using DuckDB as a backend.

- `dbMatrix.digits`: integer. Number of digits to round to in the show
  function of dbMatrix objects. Default is 7.
