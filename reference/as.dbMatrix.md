# Convert [`Matrix::Matrix`](https://rdrr.io/pkg/Matrix/man/Matrix.html) to [`dbMatrix`](https://dbverse-org.github.io/dbmatrix-r/reference/dbMatrix.md)

Converts in-memory [`matrix`](https://rdrr.io/r/base/matrix.html),
[`Matrix::dgeMatrix`](https://rdrr.io/pkg/Matrix/man/dgeMatrix-class.html),
or
[`Matrix::dgCMatrix`](https://rdrr.io/pkg/Matrix/man/dgCMatrix-class.html)
into a
[`dbMatrix`](https://dbverse-org.github.io/dbmatrix-r/reference/dbMatrix.md)
object.

Generic function to convert in-memory objects to `dbMatrix` objects.

## Usage

``` r
as.dbMatrix(x, con = NULL, name = "dbMatrix", overwrite = FALSE, ...)

as.dbMatrix(x, con = NULL, name = "dbMatrix", overwrite = FALSE, ...)
```

## Arguments

- x:

  Object to convert (e.g., matrix, dgCMatrix)

- con:

  DBI or duckdb connection object

- name:

  Table name to assign within database

- overwrite:

  Whether to overwrite if table already exists

- ...:

  Additional arguments passed to methods

## Details

If no `con` is provided, a temporary in-memory database connection is
created. If no `name` is provided, a unique table name is generated.
