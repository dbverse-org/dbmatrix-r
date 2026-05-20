# Coerce dbMatrix to dgCMatrix

Coercion methods to convert `dbMatrix` objects to in-memory `dgCMatrix`
objects. Respects `dbMatrix.max_mem_convert` option to prevent OOM
errors.

## Value

A
[`Matrix::dgCMatrix`](https://rdrr.io/pkg/Matrix/man/dgCMatrix-class.html)
object containing the collected matrix values. Dense inputs are
converted to sparse Matrix format after collection.
