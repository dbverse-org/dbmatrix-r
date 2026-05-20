# S4 Class for dbSparseMatrix

Representation of sparse matrices using an on-disk database. Inherits
from
[dbMatrix](https://dbverse-org.github.io/dbmatrix-r/reference/dbMatrix.md).

## Value

Objects of class `dbSparseMatrix` store only non-zero matrix entries in
DuckDB. They are typically returned by
[`dbMatrix()`](https://dbverse-org.github.io/dbmatrix-r/reference/dbMatrix.md)
or
[`as.dbMatrix()`](https://dbverse-org.github.io/dbmatrix-r/reference/as.dbMatrix.md)
for sparse inputs.
