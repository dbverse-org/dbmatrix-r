# S4 Class for `dbDenseMatrix`

Representation of dense matrices using an on-disk database. Inherits
from
[dbMatrix](https://dbverse-org.github.io/dbmatrix-r/reference/dbMatrix.md).

## Value

Objects of class `dbDenseMatrix` store all matrix entries explicitly in
DuckDB. They are typically returned by
[`dbMatrix()`](https://dbverse-org.github.io/dbmatrix-r/reference/dbMatrix.md)
or
[`as.dbMatrix()`](https://dbverse-org.github.io/dbmatrix-r/reference/as.dbMatrix.md)
for dense inputs.
