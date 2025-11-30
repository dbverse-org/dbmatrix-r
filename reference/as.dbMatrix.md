# Convert `Matrix` to `dbMatrix`

Converts in-memory [`matrix`](https://rdrr.io/r/base/matrix.html),
[`Matrix::dgeMatrix`](https://rdrr.io/pkg/Matrix/man/dgeMatrix-class.html),
or
[`Matrix::dgCMatrix`](https://rdrr.io/pkg/Matrix/man/dgCMatrix-class.html)
into a `dbMatrix` object.

## Usage

``` r
as.dbMatrix(x, con, name, ...)
```

## Arguments

- x:

  [`matrix`](https://rdrr.io/r/base/matrix.html),
  [`Matrix::dgeMatrix`](https://rdrr.io/pkg/Matrix/man/dgeMatrix-class.html),
  or
  [`Matrix::dgCMatrix`](https://rdrr.io/pkg/Matrix/man/dgCMatrix-class.html)
  `required`

- con:

  `tbl_duckdb_connection` `default:"memory"` Connection to DuckDB
  database connection. If not provided, a temporary in-memory DuckDB
  database is created. `':temp:'` will create a DuckDB database in the
  temporary directory. `':memory:'` will create a DuckDB database in
  memory.

- name:

  `default:"memory"` table name in the database. If not provided, a
  unique table name is generated.

- ...:

  Additional arguments passed to `dbMatrix`

## Details

If no `con` is provided, a temporary in-memory database connection is
created. If no `name` is provided, a unique table name is generated.
