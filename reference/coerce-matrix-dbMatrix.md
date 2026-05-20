# Coerce matrix to dbMatrix

Coercion methods to convert in-memory `matrix` objects to `dbMatrix`
objects. Creates a new in-memory DuckDB connection.

## Value

A database-backed matrix object. Dense inputs return a
[`dbDenseMatrix`](https://dbverse-org.github.io/dbmatrix-r/reference/dbDenseMatrix-class.md),
while sparse
[`Matrix::dgCMatrix`](https://rdrr.io/pkg/Matrix/man/dgCMatrix-class.html)
inputs return a
[`dbSparseMatrix`](https://dbverse-org.github.io/dbmatrix-r/reference/dbSparseMatrix-class.md).
